#!/usr/bin/env python3
import argparse
import csv
import os
import subprocess
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
import numpy as np

TEST_DIR = Path(__file__).resolve().parent
LOCAL_EXECUTABLE = TEST_DIR / "sciantix.x"
BUILD_EXECUTABLE = TEST_DIR.parents[1] / "build" / "sciantix.x"
MAIN_OUTPUT_NAME = "output.txt"
THERMO_OUTPUT_NAME = "thermochemistry_output.txt"
PLOTS_DIR = TEST_DIR / "plots"
AVOGADRO_NUMBER = 6.02214076e23
PELLET_RADIUS_M = 2.7e-3

COLORS = ["#ff0000", "#ff7f00", "#00c853", "#2962ff", "#aa00ff"]
LIQUID_CONSTITUENT_COLUMNS = [
    "CS+ (liquid, at grain boundary) (mol/m3)",
    "MO+4 (liquid, at grain boundary) (mol/m3)",
    "MOO4-2 (liquid, at grain boundary) (mol/m3)",
    "O-2 (liquid, at grain boundary) (mol/m3)",
    "VA (liquid, at grain boundary) (mol/m3)",
    "MOO3 (liquid, at grain boundary) (mol/m3)",
    "CSO2 (liquid, at grain boundary) (mol/m3)",
]

plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (10, 7),
    "font.size": 12,
    "axes.labelsize": 15,
    "axes.titlesize": 12,
    "xtick.labelsize": 12,
    "ytick.labelsize": 12,
    "legend.fontsize": 10,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.5,
    "grid.linestyle": "--",
    "lines.linewidth": 2,
    "lines.markersize": 6,
    "legend.frameon": False,
})


def executable_path() -> Path:
    if LOCAL_EXECUTABLE.exists():
        return LOCAL_EXECUTABLE
    return BUILD_EXECUTABLE


def ensure_file(path: Path) -> None:
    if not path.is_file():
        raise FileNotFoundError(f"Required file not found: {path}")


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


def column_map(headers: list[str]) -> dict[str, int]:
    return {name: index for index, name in enumerate(headers)}


def series_or_zeros(values: np.ndarray, columns: dict[str, int], label: str) -> np.ndarray:
    if label not in columns:
        return np.zeros(values.shape[0], dtype=float)
    return values[:, columns[label]]


def secondary_time_axis(axis: plt.Axes, burnup: np.ndarray, time: np.ndarray, time_label: str) -> None:
    def burnup_to_time(x):
        return np.interp(x, burnup, time)

    def time_to_burnup(x):
        return np.interp(x, time, burnup)

    axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)


def save_figure(fig: plt.Figure, name: str, saved_paths: list[Path]) -> None:
    PLOTS_DIR.mkdir(parents=True, exist_ok=True)
    path = PLOTS_DIR / name
    fig.tight_layout()
    fig.savefig(path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(path)


def plot_case(saved_paths: list[Path]) -> None:
    output_file = TEST_DIR / MAIN_OUTPUT_NAME
    thermo_file = TEST_DIR / THERMO_OUTPUT_NAME
    ensure_file(output_file)
    ensure_file(thermo_file)

    headers, values = load_output_data(output_file)
    thermo_headers, thermo_values = load_output_data(thermo_file)
    columns = column_map(headers)
    thermo_columns = column_map(thermo_headers)

    burnup_label = "Burnup (MWd/kgUO2)"
    time_label = "Time (h)"
    fima_label = "FIMA (%)"
    burnup = values[:, columns[burnup_label]]
    time = values[:, columns[time_label]]
    fima = values[:, columns[fima_label]]

    jog_condensed = series_or_zeros(values, columns, "JOG from condensed (/)")
    jog_liquid = series_or_zeros(values, columns, "JOG from liquid (/)")
    jog_thickness_um = (jog_condensed + jog_liquid) * PELLET_RADIUS_M * 1.0e6

    fig, axes = plt.subplots(3, 2, figsize=(12, 14))
    axes = axes.flatten()

    axis = axes[0]
    if "O content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["O content (mol/m3)"]], color=COLORS[0], label="Total")
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Oxygen concentration (mol/m3)")
    secondary_time_axis(axis, burnup, time, time_label)
    axis.legend(loc="upper left")

    axis = axes[1]
    if "U content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["U content (mol/m3)"]], color=COLORS[0], label="Uranium")
    if "Pu content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["Pu content (mol/m3)"]], color=COLORS[1], label="Plutonium")
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Concentration (mol/m3)")
    secondary_time_axis(axis, burnup, time, time_label)
    axis.legend(loc="upper left")

    for axis, species, ylabel in [
        (axes[2], "Xe", "Xenon concentration (mol/m3)"),
        (axes[3], "Kr", "Krypton concentration (mol/m3)"),
        (axes[4], "Cs", "Cesium concentration (mol/m3)"),
        (axes[5], "Mo", "Molybdenum concentration (mol/m3)"),
    ]:
        for legend_label, color, suffix in [
            ("Produced", COLORS[0], " produced (at/m3)"),
            ("In grain", COLORS[1], " in grain (at/m3)"),
            ("At grain boundary", COLORS[2], " at grain boundary (at/m3)"),
            ("Reacted", COLORS[3], " reacted - GB (at/m3)"),
            ("Released", COLORS[4], " released (at/m3)"),
        ]:
            column_name = f"{species}{suffix}"
            if column_name in columns:
                axis.plot(burnup, values[:, columns[column_name]] / AVOGADRO_NUMBER, color=color, label=legend_label)
        axis.set_xlabel(burnup_label)
        axis.set_ylabel(ylabel)
        secondary_time_axis(axis, burnup, time, time_label)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="upper left")

    save_figure(fig, "inventory_mol_m3_overview.png", saved_paths)

    if "Fuel oxygen potential (KJ/mol)" in columns:
        fig, axis = plt.subplots()
        axis.plot(burnup, values[:, columns["Fuel oxygen potential (KJ/mol)"]], label="SCIANTIX", color=COLORS[0])
        if "Fuel oxygen potential - Kato (KJ/mol)" in columns:
            axis.plot(burnup, values[:, columns["Fuel oxygen potential - Kato (KJ/mol)"]], label="Kato", color=COLORS[1], linestyle="--")
        if "Fuel oxygen potential - CALPHAD (KJ/mol)" in columns:
            axis.plot(burnup, values[:, columns["Fuel oxygen potential - CALPHAD (KJ/mol)"]], label="CALPHAD", color=COLORS[3], linestyle="--")
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("muO2 (kJ/mol)")
        secondary_time_axis(axis, burnup, time, time_label)
        axis.legend(loc="best")
        save_figure(fig, "muo2.png", saved_paths)

    fig, axis = plt.subplots()
    stack_series = []
    stack_labels = []
    stack_colors = []
    jog_parts = [
        ("CS2MOO4_S2", "JOG from CS2MOO4_S2 (/)", "#6baed6"),
        ("CS2MOO4_S1", "JOG from CS2MOO4_S1 (/)", "#fd8d3c"),
        ("MOO2", "JOG from MOO2 (/)", "#74c476"),
        ("CS2MO3O10", "JOG from CS2MO3O10 (/)", "#9e9ac8"),
        ("CS2MO4O13", "JOG from CS2MO4O13 (/)", "#e377c2"),
        ("BCC_A2", "JOG from BCC_A2 (/)", "#8c564b"),
        ("FCC_A1", "JOG from FCC_A1 (/)", "#17becf"),
        ("HCP_A3", "JOG from HCP_A3 (/)", "#bcbd22"),
        ("LIQUID", "JOG from liquid (/)", "#f97316"),
    ]
    for label, column_name, color in jog_parts:
        if column_name in columns:
            stack_series.append(values[:, columns[column_name]] * PELLET_RADIUS_M * 1.0e6)
            stack_labels.append(label)
            stack_colors.append(color)
    if stack_series:
        axis.stackplot(fima, *stack_series, labels=stack_labels, colors=stack_colors, alpha=0.85)
        axis.plot(fima, jog_thickness_um, color="#111827", label="Total JOG", linewidth=2.3)
        axis.set_xlabel(fima_label)
        axis.set_ylabel("JOG thickness (um)")
        secondary_time_axis(axis, burnup, time, time_label)
        axis.legend(loc="upper left")
        save_figure(fig, "JOG_contributions.png", saved_paths)
    else:
        plt.close(fig)

    plot_liquid_constituent_fractions(thermo_values, thermo_columns, burnup, time, saved_paths)


def plot_liquid_constituent_fractions(
    thermo_values: np.ndarray,
    thermo_columns: dict[str, int],
    burnup: np.ndarray,
    time: np.ndarray,
    saved_paths: list[Path],
) -> None:
    time_label = "Time (h)"
    burnup_label = "Burnup (MWd/kgUO2)"
    if time_label not in thermo_columns:
        raise ValueError(f"Missing {time_label} in {THERMO_OUTPUT_NAME}")

    thermo_time = thermo_values[:, thermo_columns[time_label]]
    thermo_burnup = np.interp(thermo_time, time, burnup)
    available_columns = [
        column for column in LIQUID_CONSTITUENT_COLUMNS
        if column in thermo_columns and not np.allclose(thermo_values[:, thermo_columns[column]], 0.0)
    ]
    if not available_columns:
        print("No non-zero liquid constituent columns found; skipping liquid fraction plot.")
        return

    constituent_values = np.vstack([thermo_values[:, thermo_columns[column]] for column in available_columns])
    denominator = np.sum(constituent_values, axis=0)
    fractions = np.divide(
        constituent_values,
        denominator,
        out=np.zeros_like(constituent_values),
        where=denominator > 0.0,
    )

    labels = [column.split(" (", 1)[0] for column in available_columns]
    colors = ["#6baed6", "#fd8d3c", "#74c476", "#9e9ac8", "#e377c2", "#8c564b", "#17becf"]

    fig, axis = plt.subplots(figsize=(11, 7))
    axis.stackplot(thermo_burnup, fractions, labels=labels, colors=colors[:len(labels)], alpha=0.88)
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Fraction of tracked liquid constituents (-)")
    axis.set_ylim(0.0, 1.0)

    def burnup_to_time(x):
        return np.interp(x, burnup, time)

    def time_to_burnup(x):
        return np.interp(x, time, burnup)

    axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
    axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5))
    save_figure(fig, "liquid_constituent_fractions.png", saved_paths)


def run_case() -> None:
    executable = executable_path()
    ensure_file(executable)
    completed = subprocess.run(
        [str(executable)],
        cwd=TEST_DIR,
        text=True,
        capture_output=True,
        check=False,
    )
    (TEST_DIR / "sciantix.log").write_text(completed.stdout + completed.stderr)
    if completed.returncode != 0:
        raise RuntimeError(f"SCIANTIX failed with return code {completed.returncode}. See sciantix.log.")


def main() -> None:
    parser = argparse.ArgumentParser(description="Run and plot the JOG MOX single-point case.")
    parser.add_argument("--no-run", action="store_true", help="Plot existing output files without running SCIANTIX.")
    args = parser.parse_args()

    if not args.no_run:
        run_case()

    saved_paths: list[Path] = []
    plot_case(saved_paths)
    print("Saved plots:")
    for path in saved_paths:
        print(f"  {path.relative_to(TEST_DIR)}")


if __name__ == "__main__":
    main()
