from __future__ import annotations

from pathlib import Path

import numpy as np
import matplotlib.pyplot as plt

from oxired import CylinderGeometry, OxiRedCylinder, PolynomialProfile, OxygenBalanceModel

plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (10, 7),
    "font.size": 12,
    "axes.labelsize": 15,
    "axes.titlesize": 12,
    "xtick.labelsize": 12,
    "ytick.labelsize": 12,
    "legend.fontsize": 12,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.5,
    "grid.linestyle": "--",
    "lines.linewidth": 2,
    "lines.markersize": 6,
    "legend.frameon": False,
})

def make_case_dir_name(index: int, radius_mm: float) -> str:
    return f"point_{index:02d}_r_{radius_mm:.4f}mm".replace(".", "p")


def write_radial_input_histories(
    output_root: Path,
    time_hours: np.ndarray,
    radius_mm: np.ndarray,
    temperature_k: np.ndarray,
    om_profiles: np.ndarray,
) -> None:
    fission_rate = 4.0e19
    hydrostatic_stress_mpa = 0.0
    pressure_start_pa = 2.0e5
    pressure_end_pa = 7.0e6
    pressure_pa = np.linspace(pressure_start_pa, pressure_end_pa, len(time_hours))

    output_root.mkdir(parents=True, exist_ok=True)

    for i, r_mm in enumerate(radius_mm):
        case_dir = output_root / make_case_dir_name(i + 1, float(r_mm))
        case_dir.mkdir(parents=True, exist_ok=True)

        lines = []
        for j, t_h in enumerate(time_hours):
            lines.append(
                f"{t_h:.4f}   {temperature_k[i]:.6e}   {fission_rate:.6e}   "
                f"{hydrostatic_stress_mpa:.6e}   {pressure_pa[j]:.6e}   {om_profiles[j, i]:.6e}"
            )

        (case_dir / "input_history.txt").write_text("\n".join(lines) + "\n")


def main() -> None:
    # =========================
    # USER INPUT
    # =========================
    initial_om = 1.975           # Initial average O/M
    pu_fraction = 0.22           # Pu fraction
    r_outer = 2.7e-3             # Outer radius [m]
    burnup_final = 13.4          # Final burnup [at.%]

    # Requested mesh: 10 radial points
    n_radial_points = 20

    # User-selected thermal profile
    profile = PolynomialProfile(
        r_inner=0.4e-3,
        r_outer=r_outer,
        t_center=1900.0,         # K
        t_surface=900.0,         # K
        power=2.0,
    )

    geom = CylinderGeometry(r_outer=r_outer)

    solver = OxiRedCylinder(
        geometry=geom,
        temperature_profile=profile,
        pu_fraction=pu_fraction,
        n_cells=n_radial_points,
    )

    # =========================
    # MODEL CHOICE FOR AVERAGE O/M SHIFT
    # =========================
    balance = OxygenBalanceModel()

    # Burnup discretization
    burnup_values = np.linspace(0.0, burnup_final, 10)

    # Radial mesh
    edges, r = solver.mesh()
    r_mm = r * 1e3
    temperature = profile(r)

    print("Radial points [mm]:")
    print(np.round(r_mm, 4))
    print()

    print("Temperature [K] at radial points:")
    print(np.round(temperature, 2))
    print()

    # Result containers
    profiles = []
    average_oms = []
    center_oms = []
    rim_oms = []
    target_average_oms = []

    # =========================
    # O/M PROFILE CALCULATION
    # =========================
    for bu in burnup_values:
        chem = balance.target_average_om(
            initial_average_om=initial_om,
            burnup_at_percent=bu
        )
        target_average_om = chem.target_average_om

        steady = solver.solve_steady_state(
            average_om=target_average_om,
            mode="auto",
            max_iter=1000,
        )

        profiles.append(steady.om.copy())
        average_oms.append(steady.average_om)
        center_oms.append(steady.om[0])
        rim_oms.append(steady.om[-1])
        target_average_oms.append(target_average_om)

        print(f"Burnup = {bu:.2f} at.%")
        print(f"  target average O/M = {target_average_om:.6f}")
        print(f"  centerline O/M     = {steady.om[0]:.6f}")
        print(f"  rim O/M            = {steady.om[-1]:.6f}")
        print(f"  radial O/M         = {np.round(steady.om, 6)}")
        print()

    profiles = np.asarray(profiles)
    average_oms = np.asarray(average_oms)
    center_oms = np.asarray(center_oms)
    rim_oms = np.asarray(rim_oms)
    target_average_oms = np.asarray(target_average_oms)

    # =========================
    # WRITE INPUT HISTORIES (JOG-style point folders)
    # Columns:
    # time[h], temperature[K], fission rate[fiss/m^3/s], hydrostress[MPa], pressure[Pa], O/M
    # =========================
    max_time_hours = 20560.0
    time_hours = np.linspace(0.0, max_time_hours, len(burnup_values))
    histories_dir = Path(__file__).resolve().parent / "radial_input_histories"
    write_radial_input_histories(
        output_root=histories_dir,
        time_hours=time_hours,
        radius_mm=r_mm,
        temperature_k=temperature,
        om_profiles=profiles,
    )
    print(f"Generated input histories in: {histories_dir}")

    # =========================
    # PLOT 1: temperature
    # =========================
    plt.figure(figsize=(7, 5))
    plt.plot(r_mm, temperature, marker="o")
    plt.xlabel("Radius (mm)")
    plt.ylabel("Temperature (K)")
    plt.title("Radial temperature profile")
    plt.grid(True)
    plt.savefig("/home/ecappellari/transparant/sciantix-official/oxired_lib/examples/radial_input_histories/Tprofile.png")
    
    # =========================
    # PLOT 2: O/M vs radius at different burnup values
    # =========================
    plt.figure(figsize=(8, 6))
    for i, bu in enumerate(burnup_values):
        plt.plot(r_mm, profiles[i], marker="o", label=f"BU = {bu:.1f} at.%")
    plt.xlabel("Radius (mm)")
    plt.ylabel("O/M")
    plt.title("Radial O/M profiles vs burnup")
    plt.legend()
    plt.grid(True)
    plt.savefig("/home/ecappellari/transparant/sciantix-official/oxired_lib/examples/radial_input_histories/OMprofile.png")

    # =========================
    # PLOT 4: average O/M vs burnup
    # =========================
    plt.figure(figsize=(7, 5))
    plt.plot(burnup_values, average_oms, marker="o", label="Redistributed average O/M")
    plt.plot(burnup_values, target_average_oms, marker="x", linestyle="--", label="Target average O/M")
    plt.xlabel("Burnup (at.%)")
    plt.ylabel("Average O/M")
    plt.title("Evolution of average O/M with burnup")
    plt.legend()
    plt.grid(True)
    plt.savefig("/home/ecappellari/transparant/sciantix-official/oxired_lib/examples/radial_input_histories/EvolutionOMaverage.png")
    plt.close()


if __name__ == "__main__":
    main()
