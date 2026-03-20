#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import os
import shutil
import subprocess
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
import numpy as np


TEST_DIR = Path(__file__).resolve().parent
BUILD_EXECUTABLE = TEST_DIR.parents[1] / "build" / "sciantix.x"
RESULTS_DIR = TEST_DIR / "sensitivity_analysis"
CASES_DIR = RESULTS_DIR / "cases"
SUMMARY_FILE = RESULTS_DIR / "jog_sensitivity_summary.tsv"
RUN_LOG = RESULTS_DIR / "run.log"
JOG_RADIUS_M = 5.0e-3
AVOGADRO_NUMBER = 6.02214076e23
CS2MOO4_LIQUID_DENSITY_KG_PER_M3 = 4380.0
ATOMIC_MASS_G_PER_MOL = {
    "Cs": 132.90545196,
    "Mo": 95.95,
    "O": 15.999,
}

INPUT_FILES = (
    "input_settings.txt",
    "input_history.txt",
    "input_initial_conditions.txt",
    "input_scaling_factors.txt",
    "input_thermochemistry.txt",
    "input_thermochemistry_settings.txt",
)

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
    "lines.markersize": 5,
    "legend.frameon": False,
})


def parse_float_list(raw_value: str) -> list[float]:
    return [float(item.strip()) for item in raw_value.split(",") if item.strip()]


def ensure_executable(path: Path) -> None:
    if not path.exists():
        raise FileNotFoundError(f"SCIANTIX executable not found: {path}")
    if not path.is_file():
        raise FileNotFoundError(f"SCIANTIX executable path is not a file: {path}")


def load_output_data(output_file: Path) -> tuple[list[str], np.ndarray]:
    with output_file.open(newline="") as handle:
        reader = csv.reader(handle, delimiter="\t")
        rows = [[cell.strip() for cell in row if cell.strip()] for row in reader]

    if len(rows) < 2:
        raise ValueError(f"Not enough rows found in {output_file}")

    headers = rows[0]
    values = np.array(rows[1:], dtype=float)

    if values.ndim != 2 or values.shape[1] != len(headers):
        raise ValueError(f"Malformed SCIANTIX output in {output_file}")

    return headers, values


def is_all_zero(series: np.ndarray) -> bool:
    return np.allclose(series, 0.0)


def read_baseline_history() -> tuple[float, float]:
    history_path = TEST_DIR / "input_history.txt"
    rows: list[list[float]] = []

    with history_path.open() as handle:
        for raw_line in handle:
            stripped = raw_line.strip()
            if not stripped or stripped.startswith("#"):
                continue
            rows.append([float(token) for token in stripped.split()])

    if not rows or len(rows[0]) < 5:
        raise ValueError(f"Unexpected history format in {history_path}")

    return rows[0][1], rows[0][4]


def read_initial_stoichiometry_deviation() -> float:
    initial_conditions_path = TEST_DIR / "input_initial_conditions.txt"
    lines = initial_conditions_path.read_text().splitlines()

    for index, raw_line in enumerate(lines):
        if "initial fuel stoichiometry deviation" not in raw_line.lower():
            continue
        if index == 0:
            break
        return float(lines[index - 1].strip().split()[0])

    raise ValueError(
        "Could not locate the initial stoichiometry deviation entry in input_initial_conditions.txt"
    )


def format_case_name(temperature_k: float, pressure_pa: float, stoich_deviation: float) -> str:
    deviation_tag = f"{stoich_deviation:+.4f}".replace("+", "p").replace("-", "m").replace(".", "p")
    return (
        f"T_{int(round(temperature_k))}K"
        f"__P_{pressure_pa:.3e}Pa"
        f"__dev_{deviation_tag}"
    )


def prepare_case_directory(case_dir: Path) -> None:
    case_dir.mkdir(parents=True, exist_ok=True)
    for filename in INPUT_FILES:
        shutil.copy2(TEST_DIR / filename, case_dir / filename)


def update_input_history(case_dir: Path, temperature_k: float, pressure_pa: float) -> None:
    history_path = case_dir / "input_history.txt"
    updated_lines: list[str] = []

    for raw_line in history_path.read_text().splitlines():
        stripped = raw_line.strip()
        if not stripped or stripped.startswith("#"):
            updated_lines.append(raw_line)
            continue

        parts = stripped.split()
        if len(parts) < 5:
            updated_lines.append(raw_line)
            continue

        parts[1] = f"{temperature_k:g}"
        parts[4] = f"{pressure_pa:g}"
        updated_lines.append("\t".join(parts))

    history_path.write_text("\n".join(updated_lines) + "\n")


def update_initial_stoichiometry_deviation(case_dir: Path, stoich_deviation: float) -> None:
    initial_conditions_path = case_dir / "input_initial_conditions.txt"
    lines = initial_conditions_path.read_text().splitlines()

    for index, raw_line in enumerate(lines):
        if "initial fuel stoichiometry deviation" not in raw_line.lower():
            continue
        if index == 0:
            break
        lines[index - 1] = f"{stoich_deviation:.8e}"
        initial_conditions_path.write_text("\n".join(lines) + "\n")
        return

    raise ValueError(
        f"Could not update initial stoichiometry deviation in {initial_conditions_path}"
    )


def run_case(case_dir: Path) -> subprocess.CompletedProcess[str]:
    completed = subprocess.run(
        [str(BUILD_EXECUTABLE)],
        cwd=case_dir,
        text=True,
        capture_output=True,
        check=False,
    )

    (case_dir / "sciantix.log").write_text(completed.stdout + completed.stderr)
    return completed


def summarize_case(
    case_dir: Path,
    temperature_k: float,
    pressure_pa: float,
    stoich_deviation: float,
) -> dict[str, float | str]:
    output_file = case_dir / "output.txt"
    headers, values = load_output_data(output_file)
    column_map = {name: idx for idx, name in enumerate(headers)}

    time_h = values[:, column_map["Time (h)"]]
    burnup = values[:, column_map["Burnup (MWd/kgUO2)"]]
    fima = values[:, column_map["FIMA (%)"]]
    jog_fraction = values[:, column_map["JOG (/)"]]

    jog_thickness_um = jog_fraction * (JOG_RADIUS_M / 2.0) * 1.0e6
    max_index = int(np.argmax(jog_thickness_um))

    return {
        "case": case_dir.name,
        "temperature_k": temperature_k,
        "pressure_pa": pressure_pa,
        "stoich_deviation": stoich_deviation,
        "ou_ratio": 2.0 + stoich_deviation,
        "final_time_h": float(time_h[-1]),
        "final_burnup_mwd_per_kguo2": float(burnup[-1]),
        "final_fima_pct": float(fima[-1]),
        "final_jog_fraction": float(jog_fraction[-1]),
        "final_jog_thickness_um": float(jog_thickness_um[-1]),
        "max_jog_fraction": float(jog_fraction[max_index]),
        "max_jog_thickness_um": float(jog_thickness_um[max_index]),
        "time_at_max_jog_h": float(time_h[max_index]),
        "burnup_at_max_jog_mwd_per_kguo2": float(burnup[max_index]),
        "fima_at_max_jog_pct": float(fima[max_index]),
    }


def build_case_plots(case_dir: Path) -> list[Path]:
    output_headers, output_values = load_output_data(case_dir / "output.txt")
    output_column_map = {name: idx for idx, name in enumerate(output_headers)}

    thermochemistry_headers, thermochemistry_values = load_output_data(case_dir / "thermochemistry_output.txt")
    thermochemistry_column_map = {name: idx for idx, name in enumerate(thermochemistry_headers)}

    time_label = "Time (h)"
    burnup_label = "Burnup (MWd/kgUO2)"
    fima_label = "FIMA (%)"
    jog_label = "JOG (/)"
    oxygen_label = "Oxygen content (mol/m3)"

    burnup = output_values[:, output_column_map[burnup_label]]
    time = output_values[:, output_column_map[time_label]]
    fima = output_values[:, output_column_map[fima_label]]
    jog_fraction = output_values[:, output_column_map[jog_label]]
    jog_thickness_um = jog_fraction * (JOG_RADIUS_M / 2.0) * 1.0e6

    thermochemistry_time = thermochemistry_values[:, thermochemistry_column_map[time_label]]
    thermochemistry_burnup = np.interp(thermochemistry_time, time, burnup)

    gb_variables = [
        header
        for header in thermochemistry_headers
        if header not in {burnup_label, time_label}
        and ", at grain boundary)" in header
        and not is_all_zero(thermochemistry_values[:, thermochemistry_column_map[header]])
    ]
    gb_sorted_variables = sorted(
        gb_variables,
        key=lambda variable: thermochemistry_values[-1, thermochemistry_column_map[variable]],
        reverse=True,
    )

    saved_paths: list[Path] = []

    if gb_sorted_variables:
        fig, axis = plt.subplots()
        gb_stacked_data = [
            thermochemistry_values[:, thermochemistry_column_map[variable]]
            for variable in gb_sorted_variables
        ]
        colors = plt.cm.nipy_spectral(np.linspace(0, 1, len(gb_sorted_variables)))
        axis.stackplot(
            thermochemistry_burnup,
            gb_stacked_data,
            labels=gb_sorted_variables,
            colors=colors,
            alpha=0.9,
        )
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("Concentration at grain boundary (mol/m3)")
        axis.legend(loc="upper left", fontsize=8)
        fig.tight_layout()
        plot_path = case_dir / "thermochemistry_fission_products_at_grain_boundary_stacked.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

    oxygen_activity = (
        output_values[:, output_column_map["Fuel oxygen partial pressure (MPa)"]] / 0.1
    ) ** 0.5
    cs = (
        output_values[:, output_column_map["Cs reacted - GB (at/m3)"]]
        + output_values[:, output_column_map["Cs at grain boundary (at/m3)"]]
    ) / AVOGADRO_NUMBER
    mo = (
        output_values[:, output_column_map["Mo reacted - GB (at/m3)"]]
        + output_values[:, output_column_map["Mo at grain boundary (at/m3)"]]
    ) / AVOGADRO_NUMBER
    o = output_values[:, output_column_map[oxygen_label]] * oxygen_activity

    liquid_cs2moo4_available = np.minimum.reduce([cs / 2.0, mo, o / 4.0])
    cs2moo4_molar_mass = (
        2.0 * ATOMIC_MASS_G_PER_MOL["Cs"]
        + ATOMIC_MASS_G_PER_MOL["Mo"]
        + 4.0 * ATOMIC_MASS_G_PER_MOL["O"]
    )

    condensed_cs2moo4_s1_label = "CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)"
    condensed_cs2moo4_s2_label = "CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)"
    temperature = thermochemistry_values[:, thermochemistry_column_map["Temperature (K)"]]
    temperature_celsius = temperature - 273.15

    a_o_ref = 0.8499e-9
    b_o_ref = 0.6551e-9
    c_o_ref = 1.1586e-9
    z_o = 4.0
    z_h = 2.0

    alpha_o = -7.12e-4 + 2.57e-5 * temperature_celsius + 4.03e-8 * temperature_celsius**2
    alpha_h = -0.0102 + 8.50e-5 * temperature_celsius - 2.13e-8 * temperature_celsius**2

    v_cell_o = (a_o_ref * b_o_ref * c_o_ref) * (1.0 + 3.0 * alpha_o)
    v_cell_h = (a_o_ref * b_o_ref * c_o_ref) * (1.0 + 3.0 * alpha_h)

    condensed_s1_density_g_per_m3 = (cs2moo4_molar_mass / AVOGADRO_NUMBER) * (z_o / v_cell_o)
    condensed_s2_density_g_per_m3 = (cs2moo4_molar_mass / AVOGADRO_NUMBER) * (z_h / v_cell_h)

    liquid_cs2moo4_volume_fraction = (
        liquid_cs2moo4_available * cs2moo4_molar_mass / (CS2MOO4_LIQUID_DENSITY_KG_PER_M3 * 1000.0)
    )

    condensed_cs2moo4_s1_volume_fraction = np.zeros_like(thermochemistry_burnup)
    if condensed_cs2moo4_s1_label in thermochemistry_column_map:
        condensed_cs2moo4_s1 = thermochemistry_values[:, thermochemistry_column_map[condensed_cs2moo4_s1_label]]
        condensed_cs2moo4_s1_volume_fraction = (
            condensed_cs2moo4_s1 * cs2moo4_molar_mass / condensed_s1_density_g_per_m3
        )

    condensed_cs2moo4_s2_volume_fraction = np.zeros_like(thermochemistry_burnup)
    if condensed_cs2moo4_s2_label in thermochemistry_column_map:
        condensed_cs2moo4_s2 = thermochemistry_values[:, thermochemistry_column_map[condensed_cs2moo4_s2_label]]
        condensed_cs2moo4_s2_volume_fraction = (
            condensed_cs2moo4_s2 * cs2moo4_molar_mass / condensed_s2_density_g_per_m3
        )

    condensed_cs2moo4_volume_fraction = (
        condensed_cs2moo4_s1_volume_fraction + condensed_cs2moo4_s2_volume_fraction
    )
    thermochemistry_jog_thickness_um = (
        liquid_cs2moo4_volume_fraction + condensed_cs2moo4_volume_fraction
    ) * (JOG_RADIUS_M / 2.0) * 1.0e6
    liquid_thickness_um = liquid_cs2moo4_volume_fraction * (JOG_RADIUS_M / 2.0) * 1.0e6
    condensed_thickness_um = condensed_cs2moo4_volume_fraction * (JOG_RADIUS_M / 2.0) * 1.0e6

    fig, axis = plt.subplots()
    axis.stackplot(
        thermochemistry_burnup,
        liquid_thickness_um,
        condensed_thickness_um,
        labels=["Available Cs2MoO4", "CALPHAD Cs2MoO4"],
        colors=["#fb923c", "#7c3aed"],
        alpha=0.75,
    )
    axis.plot(
        thermochemistry_burnup,
        thermochemistry_jog_thickness_um,
        color="#111827",
        label="Thermochemistry total thickness",
    )
    axis.plot(
        fima,
        jog_thickness_um,
        color="#059669",
        linestyle="--",
        label="SCIANTIX JOG thickness",
    )
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("JOG thickness (um)")
    axis.legend(loc="upper left")
    fig.tight_layout()
    plot_path = case_dir / "jog_thickness_comparison.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    return saved_paths


def write_summary(summary_rows: list[dict[str, float | str]]) -> None:
    RESULTS_DIR.mkdir(parents=True, exist_ok=True)
    if not summary_rows:
        raise ValueError("No sensitivity results available to write.")

    fieldnames = list(summary_rows[0].keys())
    with SUMMARY_FILE.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, delimiter="\t")
        writer.writeheader()
        writer.writerows(summary_rows)


def write_run_log(
    case_runs: list[tuple[str, subprocess.CompletedProcess[str]]]
) -> None:
    chunks: list[str] = []
    for case_name, completed in case_runs:
        chunks.append(f"===== {case_name} =====")
        chunks.append(f"returncode = {completed.returncode}")
        chunks.append(completed.stdout)
        chunks.append(completed.stderr)
        chunks.append("")

    RUN_LOG.write_text("\n".join(chunks))


def extract_series(
    summary_rows: list[dict[str, float | str]],
    x_key: str,
    y_key: str,
    baseline_temperature_k: float,
    baseline_pressure_pa: float,
    baseline_stoich_deviation: float,
) -> tuple[np.ndarray, np.ndarray]:
    selected: list[dict[str, float | str]] = []

    for row in summary_rows:
        if x_key != "temperature_k" and float(row["temperature_k"]) != baseline_temperature_k:
            continue
        if x_key != "pressure_pa" and float(row["pressure_pa"]) != baseline_pressure_pa:
            continue
        if x_key != "stoich_deviation" and float(row["stoich_deviation"]) != baseline_stoich_deviation:
            continue
        selected.append(row)

    selected.sort(key=lambda row: float(row[x_key]))
    x_values = np.array([float(row[x_key]) for row in selected], dtype=float)
    y_values = np.array([float(row[y_key]) for row in selected], dtype=float)
    return x_values, y_values


def plot_one_factor_sensitivity(
    summary_rows: list[dict[str, float | str]],
    baseline_temperature_k: float,
    baseline_pressure_pa: float,
    baseline_stoich_deviation: float,
) -> list[Path]:
    saved_paths: list[Path] = []

    plot_specs = [
        (
            "temperature_k",
            "Temperature (K)",
            "jog_sensitivity_vs_temperature.png",
            "#c2410c",
        ),
        (
            "pressure_pa",
            "Pressure (Pa)",
            "jog_sensitivity_vs_pressure.png",
            "#1d4ed8",
        ),
        (
            "stoich_deviation",
            "Initial stoichiometry deviation (/)",
            "jog_sensitivity_vs_stoich_deviation.png",
            "#0f766e",
        ),
    ]

    for x_key, xlabel, filename, color in plot_specs:
        x_values, final_thickness = extract_series(
            summary_rows,
            x_key,
            "final_jog_thickness_um",
            baseline_temperature_k,
            baseline_pressure_pa,
            baseline_stoich_deviation,
        )
        _, max_thickness = extract_series(
            summary_rows,
            x_key,
            "max_jog_thickness_um",
            baseline_temperature_k,
            baseline_pressure_pa,
            baseline_stoich_deviation,
        )

        fig, axis = plt.subplots()
        axis.plot(x_values, final_thickness, marker="o", color=color, label="Final thickness")
        axis.plot(
            x_values,
            max_thickness,
            marker="s",
            linestyle="--",
            color="#374151",
            label="Maximum thickness",
        )
        axis.set_xlabel(xlabel)
        axis.set_ylabel("JOG thickness (um)")
        axis.legend(loc="best")
        fig.tight_layout()

        plot_path = RESULTS_DIR / filename
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

    return saved_paths


def plot_jog_map(
    summary_rows: list[dict[str, float | str]],
    baseline_stoich_deviation: float,
) -> Path:
    selected = [
        row for row in summary_rows
        if float(row["stoich_deviation"]) == baseline_stoich_deviation
    ]

    temperatures = sorted({float(row["temperature_k"]) for row in selected})
    pressures = sorted({float(row["pressure_pa"]) for row in selected})
    z_values = np.full((len(temperatures), len(pressures)), np.nan, dtype=float)

    temperature_index = {value: idx for idx, value in enumerate(temperatures)}
    pressure_index = {value: idx for idx, value in enumerate(pressures)}

    for row in selected:
        i = temperature_index[float(row["temperature_k"])]
        j = pressure_index[float(row["pressure_pa"])]
        z_values[i, j] = float(row["final_jog_thickness_um"])

    fig, axis = plt.subplots()
    image = axis.imshow(z_values, aspect="auto", origin="lower", cmap="viridis")
    axis.set_xticks(range(len(pressures)), [f"{value:.1e}" for value in pressures], rotation=30, ha="right")
    axis.set_yticks(range(len(temperatures)), [f"{value:.0f}" for value in temperatures])
    axis.set_xlabel("Pressure (Pa)")
    axis.set_ylabel("Temperature (K)")
    axis.set_title(f"Final JOG thickness (um) at initial deviation = {baseline_stoich_deviation:+.4f}")
    fig.colorbar(image, ax=axis, label="Final JOG thickness (um)")
    fig.tight_layout()

    plot_path = RESULTS_DIR / "jog_thickness_temperature_pressure_map.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    return plot_path


def build_default_parser_values() -> tuple[str, str, str]:
    temperatures = list(np.arange(600.0, 2000.0 + 1.0, 200.0))
    pressures_bar = list(np.arange(1.0, 10.0 + 1.0, 1.0))
    pressures = [value * 1.0e5 for value in pressures_bar]
    deviations = list(np.arange(-0.10, 0.20 + 1.0e-12, 0.05))

    return (
        ",".join(f"{value:g}" for value in temperatures),
        ",".join(f"{value:g}" for value in pressures),
        ",".join(f"{value:g}" for value in deviations),
    )


def main() -> int:
    default_temperatures, default_pressures, default_deviations = build_default_parser_values()

    parser = argparse.ArgumentParser(
        description="Run a JOG1 sensitivity analysis versus temperature, pressure, and initial stoichiometry deviation."
    )
    parser.add_argument(
        "--temperatures",
        default=default_temperatures,
        help="Comma-separated temperatures in K.",
    )
    parser.add_argument(
        "--pressures",
        default=default_pressures,
        help="Comma-separated system pressures in Pa.",
    )
    parser.add_argument(
        "--stoich-deviations",
        default=default_deviations,
        help="Comma-separated initial stoichiometry deviations (/).",
    )
    parser.add_argument(
        "--clean",
        action="store_true",
        help="Remove the previous sensitivity_analysis directory before running.",
    )
    args = parser.parse_args()

    ensure_executable(BUILD_EXECUTABLE)

    temperatures_k = parse_float_list(args.temperatures)
    pressures_pa = parse_float_list(args.pressures)
    stoich_deviations = parse_float_list(args.stoich_deviations)

    baseline_temperature_k, baseline_pressure_pa = read_baseline_history()
    baseline_stoich_deviation = read_initial_stoichiometry_deviation()

    if args.clean and RESULTS_DIR.exists():
        shutil.rmtree(RESULTS_DIR)

    CASES_DIR.mkdir(parents=True, exist_ok=True)

    summary_rows: list[dict[str, float | str]] = []
    case_runs: list[tuple[str, subprocess.CompletedProcess[str]]] = []
    generated_case_plot_count = 0

    for temperature_k in temperatures_k:
        for pressure_pa in pressures_pa:
            for stoich_deviation in stoich_deviations:
                case_name = format_case_name(temperature_k, pressure_pa, stoich_deviation)
                case_dir = CASES_DIR / case_name

                prepare_case_directory(case_dir)
                update_input_history(case_dir, temperature_k, pressure_pa)
                update_initial_stoichiometry_deviation(case_dir, stoich_deviation)

                completed = run_case(case_dir)
                case_runs.append((case_name, completed))

                if completed.returncode != 0:
                    raise RuntimeError(f"SCIANTIX failed for {case_name}. See {case_dir / 'sciantix.log'}")

                summary_rows.append(
                    summarize_case(case_dir, temperature_k, pressure_pa, stoich_deviation)
                )
                generated_case_plot_count += len(build_case_plots(case_dir))

    write_summary(summary_rows)
    write_run_log(case_runs)

    saved_plots = plot_one_factor_sensitivity(
        summary_rows,
        baseline_temperature_k,
        baseline_pressure_pa,
        baseline_stoich_deviation,
    )
    saved_plots.append(plot_jog_map(summary_rows, baseline_stoich_deviation))

    print("Sensitivity analysis completed.")
    print(f"Summary: {SUMMARY_FILE}")
    print(f"Run log: {RUN_LOG}")
    print(f"Per-case plots generated: {generated_case_plot_count}")
    print("Plots:")
    for plot_path in saved_plots:
        print(plot_path)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
