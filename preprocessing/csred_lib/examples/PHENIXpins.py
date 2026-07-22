from __future__ import annotations
import os

"""
Print radial Cs-production scaling factors.

The geometry and temperature choices mirror oxired_lib/examples/PHENIXpins.py.
Cs is produced locally with the burnup history and then redistributed according
to N(r) = A exp(-Q*/RT(r)).
"""

from pathlib import Path
import numpy as np

from csred import (
    CsRedCylinder,
    CylinderGeometry,
    PolynomialProfile,
    area_average,
)

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt

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

def radial_burnup_profile(
    average_burnup_at_percent: float,
    edges: np.ndarray,
    radius: np.ndarray,
    r_outer: float,
    rim_to_center_factor: float,
) -> np.ndarray:
    """Simple normalized local-burnup shape."""
    normalized_radius = radius / r_outer
    shape = 1.0 + (rim_to_center_factor - 1.0) * normalized_radius**2
    shape /= area_average(edges, shape)
    return average_burnup_at_percent * shape


def main() -> None:
    r_outer = 2.719e-3
    burnup_final = 13.28
    max_time_hours = 25200

    n_radial_points = 4
    n_time_points = 10
    # Cs production is assumed practically flat. The radial redistribution
    # factor comes from the temperature-dependent concentration profile.
    rim_to_center_burnup_factor = 1.0

    r_inner = 0.8e-3 # central hole (Inspyre deliverable 7.3)
    profile = PolynomialProfile(
        r_inner=r_inner,
        r_outer=r_outer,
        t_center=2200.0,
        t_surface=800.0,
        power=2.0,
    )

    solver = CsRedCylinder(
        geometry=CylinderGeometry(r_outer=r_outer, r_inner=r_inner),
        temperature_profile=profile,
        n_cells=n_radial_points,
    )
    edges, radius = solver.mesh()
    radius_mm = radius * 1e3
    temperature = profile(radius)
    time_hours = np.linspace(0.0, max_time_hours, n_time_points)
    average_burnup = np.linspace(0.0, burnup_final, n_time_points)
    local_burnup = np.asarray([
        radial_burnup_profile(bu, edges, radius, r_outer, rim_to_center_burnup_factor)
        for bu in average_burnup
    ])

    result = solver.solve_history(time_hours * 3600.0, local_burnup)
    source_weighted_factor = area_average(
        edges,
        result.scaling_factor * result.produced,
    ) / area_average(edges, result.produced)

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

    # Continuous curve over the full inner-to-outer radius: the node values
    # are held flat to the domain edges and linearly interpolated in between.
    radius_full = np.concatenate(([edges[0]], radius, [edges[-1]]))
    scaling_factor_full = np.concatenate((
        [result.scaling_factor[0]], result.scaling_factor, [result.scaling_factor[-1]],
    ))
    axis.plot(
        radius_full / r_outer, scaling_factor_full,
        color=PAPER_PALETTE[2], linewidth=3, zorder=2,
    )
    # Dots mark only the radii where a SCIANTIX calculation was performed.
    axis.scatter(
        radius_mm * 1e-3 / r_outer, result.scaling_factor,
        marker="o", s=70, color=PAPER_PALETTE[2], zorder=3,
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
    axis.set_ylim(0.0, 3.0)
    secondary_axis.set_ylim(700.0, 2300.0)
    secondary_axis.grid(False)
    axis.set_ylabel("Cs redistribution (-)", color=PAPER_PALETTE[2])
    axis.set_yticks(np.linspace(0.0, 3.0, 7))
    axis.tick_params(axis="y", labelcolor=PAPER_PALETTE[2])
    secondary_axis.tick_params(axis="y", labelcolor=PAPER_PALETTE[-1])
    secondary_axis.set_yticks(np.linspace(700.0, 2300.0, 9))
    secondary_axis.set_ylabel("Temperature (K)", color=PAPER_PALETTE[-1])
    plt.tight_layout()
    
    SCRIPT_DIR = Path(__file__).resolve().parent
    plt.savefig(SCRIPT_DIR.parents[4] / "OverLeaf/JOGSCIANTIX/Images/Csprofile.png")

    print("Cs radial redistribution scaling")
    print("================================")
    print(f"radial points:                 {n_radial_points}")
    print(f"final average burnup:          {burnup_final:.6f} at.%")
    print(f"effective heat of transport:   {solver.heat_of_transport:.6e} J/mol")
    print(f"rim/center burnup factor:      {rim_to_center_burnup_factor:.6f}")
    print(f"area-average Cs factor:        {result.average_scaling_factor:.6f}")
    print(f"source-weighted Cs factor:     {source_weighted_factor:.6f}")
    print()
    print("Radial Cs production scaling factors")
    print("------------------------------------")
    print("index  radius_mm  r_over_ro  temperature_K  sf_cs_production")
    for index, (r_mm, r_norm, temp, factor) in enumerate(
        zip(radius_mm, radius / r_outer, temperature, result.scaling_factor),
        start=1,
    ):
        print(
            f"{index:5d}  {r_mm:9.4f}  {r_norm:8.5f}  {temp:13.2f}  {factor:16.8e}"
        )

    print()
    print("radius_mm = " + np.array2string(radius_mm, precision=4, separator=", "))
    print(
        "sf_cs_production = "
        + np.array2string(result.scaling_factor, precision=8, separator=", ")
    )


if __name__ == "__main__":
    main()
