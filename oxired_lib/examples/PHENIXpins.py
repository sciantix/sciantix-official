from __future__ import annotations

"""
Generate stand-alone SCIANTIX radial histories. 
It's implementation was done in parallel to the TRANSURANUS implementation of 
oxired model 4. Refer to:

  - Subroutine RadialOxygen:
      builds the local oxygen balance, updates matrix O/M up to O/M = 2,
      and stores remaining free oxygen.
  - Subroutine UpdateRadialOxygen:
      after thermodiffusion, rescales the free-oxygen shape so the integrated
      free-oxygen inventory is conserved.

The SCIANTIX output written here uses the redistributed matrix O/M profile.
"""

import os
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Callable

import numpy as np

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt

from oxired import CylinderGeometry, OxiRedCylinder, PolynomialProfile
from oxired.constants import diffusion_coefficient
from oxired.fission_yields import fission_yield_for_element


PAPER_PALETTE = [
    "#736F3F", "#BFAE56", "#B29DA6", "#D9AF32", "#A66226", "#733426",
    "#737675", "#9D6953", "#363726",  "#785C2D",
]

plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (10, 7),
    "font.family": "serif",
    "font.serif": ["Times New Roman", "Times", "Nimbus Roman", "DejaVu Serif"],
    "mathtext.fontset": "dejavuserif",
    "font.size": 20,
    "axes.labelsize": 20,
    "axes.titlesize": 20,
    "xtick.labelsize": 20,
    "ytick.labelsize": 20,
    "legend.fontsize": 20,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.5,
    "grid.linestyle": "--",
    "lines.linewidth": 3,
    "lines.markersize": 6,
    "legend.frameon": False,
})

FractionLaw = float | Callable[[float], float]

FIXED_OXYGEN_SINK_ELEMENTS = (
    "Zr",
    "Nb",
    "Sr",
    "Y",
    "La",
    "Ce",
    "Pr",
    "Nd",
    "Pm",
    "Sm",
    "Eu",
    "Gd",
    "Am+Cm",
    "Np",
)


@dataclass(frozen=True)
class RadialOxygen:

    om_before_redistribution: np.ndarray
    om_after_redistribution: np.ndarray
    free_surplus_oxygen_per_metal: np.ndarray
    released_oxygen_per_metal: np.ndarray
    fixed_sink_oxygen_per_metal: np.ndarray
    ba_sink_oxygen_per_metal: np.ndarray
    mo_sink_oxygen_per_metal: np.ndarray
    oxygen_to_matrix_per_metal: np.ndarray
    target_average_om: float
    redistributed_average_om: float


def make_case_dir_name(index: int, radius_mm: float) -> str:
    return f"point_{index:02d}_r_{radius_mm:.4f}mm".replace(".", "p")


def evaluate_fraction(value: FractionLaw, burnup_at_percent: float, name: str) -> float:
    """
    Evaluate a scalar or burnup-dependent oxide fraction. 
    No feedback from CALPHAD calculations.
    """
    fraction = float(value(burnup_at_percent) if callable(value) else value)
    if not (0.0 <= fraction <= 1.0):
        raise ValueError(f"{name} must evaluate to a value in [0, 1]")
    return fraction


def fp_atoms_per_initial_metal(element: str, burnup_at_percent: np.ndarray) -> np.ndarray:
    """
    Return FP atoms divided by initial heavy-metal atoms.

        FP / initial_metal = yield_per_fission * burnup_fraction

    """
    entry = fission_yield_for_element(element)
    return entry.yield_percent_fp_per_fission / 100.0 * burnup_at_percent / 100.0


def fixed_oxygen_sink_contributions() -> dict[str, float]:
    """Oxygen sink per at.% FIMA from the fixed-sink elements.

        yield_percent / 100 * valence / 2

    Ba and Mo are excluded because they are handled as local and variable oxygen sinks.
    """
    contributions = {}
    for element in FIXED_OXYGEN_SINK_ELEMENTS:
        entry = fission_yield_for_element(element)
        contributions[element] = entry.yield_percent_fp_per_fission / 100.0 * entry.valence / 2.0
    return contributions


def fixed_oxygen_per_at_percent() -> float:
    """Sum of the oxygen sink contributions"""
    return sum(fixed_oxygen_sink_contributions().values())


def solve_radial_profiles_from_average_om(
    solver: OxiRedCylinder,
    average_om_history: np.ndarray,
) -> np.ndarray:
    """ """
    profiles = []
    for average_om in average_om_history:
        if average_om >= 2.0 - 1.0e-10:
            profiles.append(np.full(solver.n_cells, 2.0))
        else:
            profiles.append(
                solver.solve_steady_state(
                    average_om=float(average_om),
                    mode="hypo",
                    max_iter=1000,
                ).om
            )
    return np.asarray(profiles)


def time_constant(
    edges: np.ndarray,
    radius: np.ndarray,
    temperature: np.ndarray,
) -> float:
    """"""
    temperature_average = area_average(edges, temperature)
    diffusion_average = diffusion_coefficient(temperature_average)
    return (radius[-1] ** 2 - radius[0] ** 2) / (17.2 * diffusion_average)


def solve_transient_profiles(
    edges: np.ndarray,
    radius: np.ndarray,
    temperature: np.ndarray,
    time_hours: np.ndarray,
    initial_om_profile: np.ndarray,
    steady_target_profiles: np.ndarray,
) -> tuple[np.ndarray, float]:
    """"""
    tau = time_constant(edges, radius, temperature)
    profiles = [initial_om_profile.copy()]

    for i in range(1, len(time_hours)):
        dt_seconds = (time_hours[i] - time_hours[i - 1]) * 3600.0
        factor = 0.0 if dt_seconds / tau > 60.0 else np.exp(-dt_seconds / tau)

        c_zero = 0.5 * (2.0 - profiles[-1])
        c_infinity = 0.5 * (2.0 - steady_target_profiles[i])
        c_time = c_infinity + (c_zero - c_infinity) * factor
        profiles.append(2.0 - 2.0 * c_time)

    return np.asarray(profiles), tau


def area_average(edges: np.ndarray, values: np.ndarray) -> float:
    """
    Finite-volume radial average.
    """
    ring_areas = edges[1:] ** 2 - edges[:-1] ** 2
    return float(np.sum(values * ring_areas) / np.sum(ring_areas))


def radial_burnup_profile(
    average_burnup_at_percent: float,
    radius: np.ndarray,
    r_outer: float,
    rim_to_center_factor: float,
) -> np.ndarray:
    """
    Simple normalized local-burnup shape.
    rim_to_center_factor = 1.0 gives a flat burnup history.
    """
    normalized_radius = radius / r_outer
    shape = 1.0 + (rim_to_center_factor - 1.0) * normalized_radius**2
    shape /= np.mean(shape)
    return average_burnup_at_percent * shape


def step_radial_oxygen_model(
    *,
    solver: OxiRedCylinder,
    edges: np.ndarray,
    previous_om: np.ndarray,
    previous_burnup: np.ndarray,
    current_burnup: np.ndarray,
    free_surplus_oxygen_per_metal: np.ndarray,
    mo_oxide_fraction: FractionLaw,
    mo_valence: float,
    ba_oxide_fraction: FractionLaw,
    ba_valence: float,
    fixed_oxygen_per_at_percent: float,
) -> RadialOxygen:
    """
    Mirror the RadialOxygen balance.

    Quantities are normalized per initial heavy-metal atom. 
      1. compute delta_burnup from old/new local burnup;
      2. compute oxygen released by fission;
      3. compute fixed, Ba, and Mo oxygen sinks;
      4. send positive surplus first to the fuel matrix up to O/M = 2;
      5. store remaining free surplus oxygen;
      6. use the resulting average O/M as target for radial redistribution.
    """
    # Units: at.% FIMA.  Negative increments are clipped.
    delta_burnup = np.maximum(current_burnup - previous_burnup, 0.0)

    # Oxygen released by fission and oxygen in fixed sinks in the step
    released = previous_om * delta_burnup / 100.0
    fixed_sinks = fixed_oxygen_per_at_percent * delta_burnup / 100.0

    # Barium sink
    ba_atoms_new = fp_atoms_per_initial_metal("Ba", current_burnup)
    ba_atoms_old = fp_atoms_per_initial_metal("Ba", previous_burnup)
    ba_frac_new = np.array([
        evaluate_fraction(ba_oxide_fraction, float(bu), "ba_oxide_fraction")
        for bu in current_burnup
    ])
    ba_frac_old = np.array([
        evaluate_fraction(ba_oxide_fraction, float(bu), "ba_oxide_fraction")
        for bu in previous_burnup
    ])
    ba_sink = ba_atoms_new * ba_frac_new * ba_valence / 2.0
    ba_sink -= ba_atoms_old * ba_frac_old * ba_valence / 2.0

    # Mo sink
    mo_atoms_new = fp_atoms_per_initial_metal("Mo", current_burnup)
    mo_atoms_old = fp_atoms_per_initial_metal("Mo", previous_burnup)
    mo_frac_new = np.array([
        evaluate_fraction(mo_oxide_fraction, float(bu), "mo_oxide_fraction")
        for bu in current_burnup
    ])
    mo_frac_old = np.array([
        evaluate_fraction(mo_oxide_fraction, float(bu), "mo_oxide_fraction")
        for bu in previous_burnup
    ])
    mo_sink = mo_atoms_new * mo_frac_new * mo_valence / 2.0
    mo_sink -= mo_atoms_old * mo_frac_old * mo_valence / 2.0

    # Oxygen surplus.
    # Positive values are oxygen available to fill the hypo-stoichiometric fuel
    # matrix.  Negative values mean sinks consumed more oxygen than was released
    # during this time step.
    surplus_before_matrix = released - fixed_sinks - mo_sink - ba_sink

    # Capacity of the matrix to store the oxygen surplus
    matrix_capacity = np.maximum(2.0 - previous_om, 0.0)
    oxygen_to_matrix = np.minimum(np.maximum(surplus_before_matrix, 0.0), matrix_capacity)
    free_surplus = np.maximum(
        free_surplus_oxygen_per_metal + surplus_before_matrix - oxygen_to_matrix,
        0.0,
    )

    # The Python mesh is cell-centered, so the section average is computed by
    # ring-area averaging the updated local O/M field.
    om_before_redistribution = previous_om + oxygen_to_matrix
    target_average_om = min(area_average(edges, om_before_redistribution), 2.0)

    # Hypo->hyper transition is not modeled, therefore the Python profile is capped
    # at stoichiometry if the target average reaches O/M = 2.
    if target_average_om >= 2.0 - 1.0e-10:
        om_after_redistribution = np.full_like(previous_om, 2.0)
        redistributed_average_om = 2.0
    else:
        # Solve the hypostoichiometric steady-state radial O/M profile with
        # the same target average O/M.
        redistributed = solver.solve_steady_state(
            average_om=target_average_om,
            mode="hypo",
            max_iter=1000,
        )
        om_after_redistribution = redistributed.om
        redistributed_average_om = redistributed.average_om

    return RadialOxygen(
        om_before_redistribution=om_before_redistribution,
        om_after_redistribution=om_after_redistribution,
        free_surplus_oxygen_per_metal=free_surplus,
        released_oxygen_per_metal=released,
        fixed_sink_oxygen_per_metal=fixed_sinks,
        ba_sink_oxygen_per_metal=ba_sink,
        mo_sink_oxygen_per_metal=mo_sink,
        oxygen_to_matrix_per_metal=oxygen_to_matrix,
        target_average_om=target_average_om,
        redistributed_average_om=redistributed_average_om,
    )


def write_radial_input_histories(
    output_root: Path,
    time_hours: np.ndarray,
    radius_mm: np.ndarray,
    temperature_k: np.ndarray,
    om_profiles: np.ndarray,
    fission_rate: float,
    cooldown_hours: float = 24.0,
    cooldown_temperature_k: float = 300.0,
    om_simplification_decimals: int = 3,
    max_history_gap_hours: float = 1000.0,
) -> None:
    """Write one SCIANTIX ``input_history.txt`` file per radial point.

        time[h], temperature[K], fission_rate[fiss/m3/s],
        hydrostatic_stress[MPa], pressure[Pa], O/M

    History lines are kept where the rounded O/M changes, plus a uniform
    subsampling so that no gap between consecutive lines exceeds
    ``max_history_gap_hours``: denser histories give SCIANTIX finer time
    stepping, which keeps the per-step composition jumps small.
    """
    hydrostatic_stress_mpa = 0.0
    pressure_start_pa = 1.0e5
    pressure_end_pa = 7.0e6
    pressure_pa = np.linspace(pressure_start_pa, pressure_end_pa, len(time_hours))

    output_root.mkdir(parents=True, exist_ok=True)

    for i, r_mm in enumerate(radius_mm):
        case_dir = output_root / make_case_dir_name(i + 1, float(r_mm))
        case_dir.mkdir(parents=True, exist_ok=True)

        rounded_om = np.round(om_profiles[:, i], om_simplification_decimals)
        change_indices = np.flatnonzero(rounded_om[1:] != rounded_om[:-1])
        keep_history_point = np.zeros(len(time_hours), dtype=bool)
        keep_history_point[0] = True
        keep_history_point[-1] = True
        keep_history_point[change_indices] = True
        keep_history_point[change_indices + 1] = True

        if max_history_gap_hours > 0.0:
            time_span = time_hours[-1] - time_hours[0]
            if time_span > max_history_gap_hours:
                stride = max(1, int(np.floor(
                    len(time_hours) * max_history_gap_hours / time_span
                )))
                keep_history_point[::stride] = True

        history_indices = np.flatnonzero(keep_history_point)

        lines = []
        for j in history_indices:
            t_h = time_hours[j]
            lines.append(
                f"{t_h:.4f}   {temperature_k[i]:.0f}   {fission_rate:.2e}   "
                f"{hydrostatic_stress_mpa:.2e}   {pressure_pa[j]:.2e}   "
                f"{om_profiles[j, i]:.3f}"
            )

        if cooldown_hours > 0.0:
            shutdown_time_h = time_hours[-1] + 1.0e-4
            cooldown_end_time_h = time_hours[-1] + cooldown_hours
            cooldown_points = (
                (shutdown_time_h, temperature_k[i]),
                (cooldown_end_time_h, cooldown_temperature_k),
            )
            for t_h, temp_k in cooldown_points:
                lines.append(
                    f"{t_h:.4f}   {temp_k:.0f}   {0.0:.2e}   "
                    f"{hydrostatic_stress_mpa:.2e}   {pressure_pa[-1]:.2e}   "
                    f"{om_profiles[-1, i]:.3f}"
                )

        (case_dir / "input_history.txt").write_text("\n".join(lines) + "\n")


def copy_radial_input_histories_to_regression(history_root: Path, regression_root: Path) -> int:
    """Copy generated radial histories into the matching regression point folders."""
    copied = 0
    for history_file in sorted(history_root.glob("point_*/input_history.txt")):
        target_case_dir = regression_root / history_file.parent.name
        if not target_case_dir.is_dir():
            raise FileNotFoundError(f"Missing regression case directory: {target_case_dir}")

        shutil.copy2(history_file, target_case_dir / "input_history.txt")
        copied += 1

    if copied == 0:
        raise FileNotFoundError(f"No generated input histories found in {history_root}")

    return copied

def main() -> None:
    # =========================
    # USER INPUT
    # =========================
    initial_om = 1.975
    pu_fraction = 0.22
    r_outer = 2.719e-3
    burnup_final = 13.28
    max_time_hours = 25200
    fission_rate = 3.45e19

    # Requested SCIANTIX radial histories.
    n_radial_points = 4
    n_time_points = 1000

    # Oxygen atoms consumed by fixed sinks per 100 initial metal atoms per at.% FIMA.
    # Ba and Mo are excluded and handled separately.
    fixed_sink_contributions = fixed_oxygen_sink_contributions()
    fixed_oxygen_per_at_percent_value = fixed_oxygen_per_at_percent()

    # Stand-alone assumption: no CALPHAD feedback, complete oxidation as BaO.
    ba_oxide_fraction = 1.0
    ba_valence = 2.0

    # Stand-alone assumption: no CALPHAD feedback.  The oxide fraction is a
    # fixed burnup law.  Valence +6 is consistent with Cs2MoO4 formation.
    mo_oxide_fraction = 0.6
    mo_valence = 6.0

    # Radial burnup, 1.0 means all radial points see the same local burnup.
    rim_to_center_burnup_factor = 1.0

    r_inner = 0.8e-3 # central hole (Inspyre deliverable 7.3)
    # Temperature profile used by the Python thermodiffusion solve.
    profile = PolynomialProfile(
        r_inner=r_inner,
        r_outer=r_outer,
        t_center=2200.0,
        t_surface=800.0,
        power=2.0,
    )

    # Python counterpart of the OXIRED radial mesh.  ``edges`` are ring
    # boundaries; ``radius`` are ring centers.
    geom = CylinderGeometry(r_outer=r_outer, r_inner=r_inner)
    solver = OxiRedCylinder(
        geometry=geom,
        temperature_profile=profile,
        pu_fraction=pu_fraction,
        n_cells=n_radial_points,
    )

    edges, radius = solver.mesh()
    radius_mm = radius * 1e3
    temperature = profile(radius)
    time_hours = np.linspace(0.0, max_time_hours, n_time_points)
    average_burnup = np.linspace(0.0, burnup_final, n_time_points)
    local_burnup = np.asarray([
        radial_burnup_profile(bu, radius, r_outer, rim_to_center_burnup_factor)
        for bu in average_burnup
    ])

    # Output root for generated SCIANTIX point folders and comparison plots.
    output_root = Path(__file__).resolve().parent / "PHENIXpins_history"
    output_root.mkdir(parents=True, exist_ok=True)

    print("Model-based radial O/M history")
    print("==============================")
    print("Mirrors the provisional ioxire=4 RadialOxygen balance implemeted in TU.")
    print(f"radial points:                 {n_radial_points}")
    print(f"initial O/M:                   {initial_om:.6f}")
    print(f"final average burnup:          {burnup_final:.6f} at.%")
    print(f"fission rate:                  {fission_rate:.6e} fiss/m3/s")
    print("history O/M simplification:    3 decimal places")
    print("zero-fission cooldown:         24.000000 h to 300 K")
    print(f"fixed O sink per at.%:         {fixed_oxygen_per_at_percent_value:.6f}")
    print(f"Ba oxide fraction:             {ba_oxide_fraction:.6f}")
    print(f"Ba valence:                    {ba_valence:.6f}")
    print(f"Mo valence:                    {mo_valence:.6f}")
    print(f"rim/center burnup factor:      {rim_to_center_burnup_factor:.6f}")
    print()
    print("Fixed oxygen sink contributions")
    print("-------------------------------")
    for element, contribution in fixed_sink_contributions.items():
        entry = fission_yield_for_element(element)
        print(
            f"{element:>5s}: "
            f"{entry.yield_percent_fp_per_fission:6.3f}/100 * "
            f"{entry.valence:3.1f}/2 = {contribution:.6f}"
        )
    print()
    print("Radial points [mm]:")
    print(np.round(radius_mm, 4))
    print()
    print("Temperature [K] at radial points:")
    print(np.round(temperature, 2))
    print()

    om_profile = np.full(n_radial_points, initial_om, dtype=float)
    previous_burnup = local_burnup[0].copy()
    free_surplus = np.zeros(n_radial_points, dtype=float)

    states: list[RadialOxygen] = []
    om_profiles = []
    average_oms = []
    free_surplus_profiles = []

    initial_state = RadialOxygen(
        om_before_redistribution=om_profile.copy(),
        om_after_redistribution=om_profile.copy(),
        free_surplus_oxygen_per_metal=free_surplus.copy(),
        released_oxygen_per_metal=np.zeros(n_radial_points),
        fixed_sink_oxygen_per_metal=np.zeros(n_radial_points),
        ba_sink_oxygen_per_metal=np.zeros(n_radial_points),
        mo_sink_oxygen_per_metal=np.zeros(n_radial_points),
        oxygen_to_matrix_per_metal=np.zeros(n_radial_points),
        target_average_om=area_average(edges, om_profile),
        redistributed_average_om=area_average(edges, om_profile),
    )
    states.append(initial_state)
    om_profiles.append(om_profile.copy())
    average_oms.append(initial_state.redistributed_average_om)
    free_surplus_profiles.append(free_surplus.copy())

    for j in range(1, n_time_points):
        state = step_radial_oxygen_model(
            solver=solver,
            edges=edges,
            previous_om=om_profile,
            previous_burnup=previous_burnup,
            current_burnup=local_burnup[j],
            free_surplus_oxygen_per_metal=free_surplus,
            mo_oxide_fraction=mo_oxide_fraction,
            mo_valence=mo_valence,
            ba_oxide_fraction=ba_oxide_fraction,
            ba_valence=ba_valence,
            fixed_oxygen_per_at_percent=fixed_oxygen_per_at_percent_value,
        )
        states.append(state)

        om_profile = state.om_after_redistribution.copy()
        previous_burnup = local_burnup[j].copy()
        free_surplus = state.free_surplus_oxygen_per_metal.copy()
        om_profiles.append(om_profile.copy())
        average_oms.append(state.redistributed_average_om)
        free_surplus_profiles.append(free_surplus.copy())

        print(f"Burnup = {average_burnup[j]:.2f} at.%")
        print(f"  target average O/M      = {state.target_average_om:.6f}")
        print(f"  redistributed avg O/M   = {state.redistributed_average_om:.6f}")
        print(f"  radial O/M              = {np.round(state.om_after_redistribution, 6)}")
        print(f"  free O per metal        = {np.round(state.free_surplus_oxygen_per_metal, 8)}")
        print()

    om_profiles = np.asarray(om_profiles)
    average_oms = np.asarray(average_oms)
    free_surplus_profiles = np.asarray(free_surplus_profiles)
    released_profiles = np.asarray([state.released_oxygen_per_metal for state in states])
    fixed_sink_profiles = np.asarray([state.fixed_sink_oxygen_per_metal for state in states])
    ba_sink_profiles = np.asarray([state.ba_sink_oxygen_per_metal for state in states])
    mo_sink_profiles = np.asarray([state.mo_sink_oxygen_per_metal for state in states])
    matrix_uptake_profiles = np.asarray([state.oxygen_to_matrix_per_metal for state in states])
    free_surplus_delta_profiles = np.vstack([
        free_surplus_profiles[0],
        np.diff(free_surplus_profiles, axis=0),
    ])
    
    ioxire1_target_average_oms = np.minimum(initial_om + 0.005 * average_burnup, 2.0)
    ioxire1_target_om_profiles = solve_radial_profiles_from_average_om(
        solver,
        ioxire1_target_average_oms,
    )
    ioxire1_like_om_profiles, ioxire1_like_tau = solve_transient_profiles(
        edges,
        radius,
        temperature,
        time_hours,
        om_profiles[0],
        ioxire1_target_om_profiles,
    )
    ioxire1_like_average_oms = np.asarray([
        area_average(edges, profile)
        for profile in ioxire1_like_om_profiles
    ])

    print(f"IOXIRE = 1 tau:                {ioxire1_like_tau:.6e} s")
    print(f"IOXIRE = 1 tau:                {ioxire1_like_tau / 3600.0:.6e} h")
    print()

    released_average = np.asarray([area_average(edges, row) for row in released_profiles])
    fixed_sink_average = np.asarray([area_average(edges, row) for row in fixed_sink_profiles])
    ba_sink_average = np.asarray([area_average(edges, row) for row in ba_sink_profiles])
    mo_sink_average = np.asarray([area_average(edges, row) for row in mo_sink_profiles])
    matrix_uptake_average = np.asarray([area_average(edges, row) for row in matrix_uptake_profiles])
    free_surplus_delta_average = np.asarray([
        area_average(edges, row)
        for row in free_surplus_delta_profiles
    ])

    released_cumulative_average = np.cumsum(released_average)
    fixed_sink_cumulative_average = np.cumsum(fixed_sink_average)
    ba_sink_cumulative_average = np.cumsum(ba_sink_average)
    mo_sink_cumulative_average = np.cumsum(mo_sink_average)
    matrix_uptake_cumulative_average = np.cumsum(matrix_uptake_average)
    free_surplus_cumulative_average = np.cumsum(free_surplus_delta_average)
    balance_residual_cumulative_average = released_cumulative_average - (
        fixed_sink_cumulative_average
        + ba_sink_cumulative_average
        + mo_sink_cumulative_average
        + matrix_uptake_cumulative_average
        + free_surplus_cumulative_average
    )

    write_radial_input_histories(
        output_root=output_root,
        time_hours=time_hours,
        radius_mm=radius_mm,
        temperature_k=temperature,
        om_profiles=om_profiles,
        fission_rate=fission_rate,
    )
    regression_root = Path(__file__).resolve().parents[2] / "regression" / "JOG" / "PHENIXpins"
    #copied_histories = copy_radial_input_histories_to_regression(output_root, regression_root)

    # Region colors: one per radial node, so the reader can see which shell
    # of the pellet (edges[i] to edges[i+1]) each SCIANTIX node represents.
    REGION_COLORS = ["#2a78d6", "#1baf7a", "#eda100", "#008300"]
    edges_over_ro = edges / r_outer
    n_regions = len(edges_over_ro) - 1

    fig, axis = plt.subplots(1,1, figsize=(8,5))

    for i in range(n_regions):
        axis.axvspan(
            edges_over_ro[i], edges_over_ro[i + 1],
            color=REGION_COLORS[i % len(REGION_COLORS)],
            alpha=0.15, zorder=0, linewidth=0,
        )
        axis.text(
            0.5 * (edges_over_ro[i] + edges_over_ro[i + 1]), 0.97,
            f"Point {i + 1}", ha="center", va="top", fontsize=13, color="#3a3a3a",
            transform=axis.get_xaxis_transform(),
        )

    for color_index, idx in enumerate(np.linspace(0, len(average_burnup) - 1, 5, dtype=int)):
        color = PAPER_PALETTE[color_index % len(PAPER_PALETTE)]
        # Continuous curve over the full inner-to-outer radius: the node
        # values are held flat to the domain edges and linearly interpolated
        # in between.
        radius_full = np.concatenate(([edges[0]], radius, [edges[-1]]))
        om_full = np.concatenate((
            [om_profiles[idx][0]], om_profiles[idx], [om_profiles[idx][-1]],
        ))
        axis.plot(radius_full / r_outer, om_full, color=color, linewidth=2.5, zorder=2)
        # Dots mark only the radii where a SCIANTIX calculation was performed.
        axis.scatter(
            radius_mm * 1e-3 / r_outer, om_profiles[idx],
            marker="o", s=55, color=color, zorder=3,
            label=f"Burnup = {average_burnup[idx]:.0f} at.%",
        )
    secondary_axis = axis.twinx()
    # The temperature profile is analytic, so the full inner-to-outer curve
    # is the real evaluated profile, not an extrapolation.
    temperature_full = profile(radius_full)
    secondary_axis.plot(radius_full / r_outer, temperature_full, color=PAPER_PALETTE[-1], linewidth=3, zorder=2)
    secondary_axis.scatter(
        radius_mm * 1e-3 / r_outer, temperature,
        marker="^", s=70, color=PAPER_PALETTE[-1], zorder=3,
    )
    axis.set_xlabel("R/Ro")
    axis.set_xlim(0.0-0.1, 1.0+0.1)
    axis.set_xticks(np.linspace(0.0, 1.0, 6))
    axis.set_ylim(1.90, 2.01)
    secondary_axis.set_ylim(700.0, 2300.0)
    secondary_axis.grid(False)
    axis.set_ylabel("Oxygen-to-Metal ratio (-)")
    axis.set_yticks([1.90, 1.92, 1.94, 1.96, 1.98, 2.00])
    axis.tick_params(axis="y")
    secondary_axis.tick_params(axis="y", labelcolor=PAPER_PALETTE[-1])
    secondary_axis.set_yticks(np.linspace(700.0, 2300.0, 9))
    secondary_axis.set_ylabel("Temperature (K)", color=PAPER_PALETTE[-1])
    axis.legend(loc="lower left", ncol=1, fontsize=14)
    plt.tight_layout()

    SCRIPT_DIR = Path(__file__).resolve().parent
    plt.savefig(SCRIPT_DIR.parents[3] / "OverLeaf/JOGSCIANTIX/Images/Oprofile.png")

    fig, axis = plt.subplots(1,1, figsize=(5+4,5))
    axis.plot(
        average_burnup,
        ioxire1_like_average_oms,
        label="Δ(O/M) = 0.005 %Bu",
        color=PAPER_PALETTE[0]
    )
    axis.plot(average_burnup, average_oms, label="This work", color=PAPER_PALETTE[1])
    axis.set_xlabel("Average burnup (at.%)")
    axis.set_ylabel("Average Oxygen-to-Metal ratio")
    axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
    plt.tight_layout()
    plt.savefig(output_root / "OMAverage.png")

    fig, axis = plt.subplots(1,1, figsize=(5+4,5))
    axis.plot(average_burnup, released_cumulative_average*100, label="Released", color=PAPER_PALETTE[0])
    axis.plot(average_burnup, fixed_sink_cumulative_average*100, label="Fixed sinks", color=PAPER_PALETTE[1])
    axis.plot(average_burnup, ba_sink_cumulative_average*100, label="Ba sink", color=PAPER_PALETTE[2])
    axis.plot(average_burnup, mo_sink_cumulative_average*100, label="Mo sink", color=PAPER_PALETTE[3])
    axis.plot(average_burnup, matrix_uptake_cumulative_average*100, label="Matrix uptake", color=PAPER_PALETTE[4])
    axis.plot(average_burnup, free_surplus_cumulative_average*100, label="Free O", color=PAPER_PALETTE[5])
    axis.plot(average_burnup, balance_residual_cumulative_average*100, linestyle="--", label="Residual", color=PAPER_PALETTE[6])
    axis.set_xlabel("Average burnup (at.%)")
    axis.set_ylabel("Oxygen to Initial Metal (%)")
    axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
    plt.tight_layout()
    plt.savefig(output_root / "EvolutionOxygenBalance.png")

    fig, axis = plt.subplots(1,1, figsize=(5+4,5))
    positive_terms = [
        fixed_sink_cumulative_average,
        ba_sink_cumulative_average,
        mo_sink_cumulative_average,
        matrix_uptake_cumulative_average,
        free_surplus_cumulative_average,
    ]
    labels = [
        "Fixed sinks",
        "Ba sink",
        "Mo sink",
        "Matrix uptake",
        "Free oxygen",
    ]
    bottom = np.zeros_like(average_burnup)
    for i, values, label in zip([0, 1, 2, 3, 4], positive_terms, labels):
        axis.bar(average_burnup, values, bottom=bottom, width=0.5, label=label, color=PAPER_PALETTE[i])
        bottom = bottom + values
    axis.plot(average_burnup, released_cumulative_average, color="k", label="Released")
    axis.set_xlabel("Average burnup (at.%)")
    axis.set_ylabel("Oxygen to Initial Metal (%)")
    axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
    plt.tight_layout()
    plt.savefig(output_root / "EvolutionOxygenBalanceClosure.png")

    print(f"Generated Sciantix input histories in: {output_root}")


if __name__ == "__main__":
    main()
