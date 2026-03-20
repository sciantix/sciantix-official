#!/usr/bin/env python3
import argparse
import csv
import math
import os
import re
import shutil
import subprocess
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
import numpy as np


TEST_DIR = Path(__file__).resolve().parent
BUILD_EXECUTABLE = TEST_DIR.parents[1] / "build" / "sciantix.x"
RUN_LOG = TEST_DIR / "sciantix.log"
CASE_LOG_NAME = "sciantix_case.log"
RUN_SUMMARY = TEST_DIR / "run_summary.txt"
THERMO_OUTPUT_NAME = "thermochemistry_output.txt"
MAIN_OUTPUT_NAME = "output.txt"
AVOGADRO_NUMBER = 6.02214076e23
ATOMIC_MASS_G_PER_MOL = {
    "Cs": 132.90545196,
    "Mo": 95.95,
    "O": 15.999,
    "U": 238.02891,
}
CS2MOO4_LIQUID_DENSITY_KG_PER_M3 = 4380.0
CASE_DIR_PATTERN = "point_*"
PLOTS_DIR_NAME = "plots"
EXP_DATA_DIR = TEST_DIR.parent / "test_JOG" / "exp_data"
CONDENSED_TOP_SPECIES = 8
SHARED_INPUT_FILES = (
    "input_settings.txt",
    "input_initial_conditions.txt",
    "input_scaling_factors.txt",
    "input_thermochemistry.txt",
    "input_thermochemistry_settings.txt",
)
HISTORY_PLOT_COLUMNS = [
    ("Temperature (K)", "Temperature history", "Temperature (K)", "#b45309"),
    ("Fission rate (fiss / m3 s)", "Fission rate history", "Fission rate (fiss / m3 s)", "#0f766e"),
    ("System pressure (Pa)", "Gap pressure history", "Pressure (Pa)", "#1d4ed8"),
    ("O/M ratio (/)", "O/M ratio history", "O/M ratio (/)", "#7c3aed"),
]
RADIAL_SUMMARY_VARIABLES = [
    "Temperature (K)",
    "Fission rate (fiss / m3 s)",
    "Burnup (MWd/kgUO2)",
    "FIMA (%)",
    "System pressure (Pa)",
    "Gap oxygen partial pressure (MPa)",
    "Stoichiometry deviation (/)",
    "Fuel oxygen partial pressure (MPa)",
    "Fuel oxygen potential (KJ/mol)",
    "JOG (/)",
]

plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (11, 7),
    "font.size": 17,
    "axes.labelsize": 18,
    "axes.titlesize": 19,
    "axes.titleweight": "bold",
    "xtick.labelsize": 15,
    "ytick.labelsize": 15,
    "legend.fontsize": 13,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.25,
    "grid.linestyle": "--",
    "axes.spines.top": False,
    "axes.spines.right": False,
    "lines.linewidth": 2.4,
    "lines.markersize": 6,
    "legend.frameon": False,
})


def ensure_executable(path: Path) -> None:
    if not path.exists():
        raise FileNotFoundError(f"SCIANTIX executable not found: {path}")
    if not path.is_file():
        raise FileNotFoundError(f"SCIANTIX executable path is not a file: {path}")


def sanitize_filename(label: str) -> str:
    return "".join(char if char.isalnum() else "_" for char in label).strip("_").lower()


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


def load_experimental_jog_data(data_file: Path) -> tuple[np.ndarray, np.ndarray]:
    fima_values: list[float] = []
    thickness_values: list[float] = []

    with data_file.open() as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line or line.startswith("#") or line.startswith("FIMA("):
                continue

            fima_str, thickness_str = [item.strip() for item in line.split(";")]
            fima_values.append(float(fima_str))
            thickness_values.append(float(thickness_str))

    return np.array(fima_values, dtype=float), np.array(thickness_values, dtype=float)


def style_axis(axis, title: str, xlabel: str, ylabel: str) -> None:
    axis.set_title(title, pad=14)
    axis.set_xlabel(xlabel)
    axis.set_ylabel(ylabel)
    axis.grid(True, alpha=0.25)


def make_plot_dir() -> Path:
    plot_dir = TEST_DIR / PLOTS_DIR_NAME
    plot_dir.mkdir(exist_ok=True)
    return plot_dir


def append_saved_path(saved_paths: list[Path], plot_dir: Path, filename: str) -> Path:
    plot_path = plot_dir / filename
    saved_paths.append(plot_path)
    return plot_path


def parse_radius_mm(case_dir: Path) -> float:
    match = re.search(r"_r_(\d+p\d+)mm$", case_dir.name)
    if not match:
        raise ValueError(f"Could not parse radius from case directory name: {case_dir.name}")
    return float(match.group(1).replace("p", "."))


def case_label(case_dir: Path) -> str:
    return f"{parse_radius_mm(case_dir):.4f} mm"


def prepare_case_inputs(case_dir: Path) -> None:
    for filename in SHARED_INPUT_FILES:
        source = TEST_DIR / filename
        target = case_dir / filename
        shutil.copy2(source, target)


def run_sciantix_case(executable: Path, case_dir: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(executable)],
        cwd=case_dir,
        text=True,
        capture_output=True,
        check=False,
    )


def write_case_log(case_dir: Path, completed: subprocess.CompletedProcess[str]) -> None:
    log_text = completed.stdout + completed.stderr
    (case_dir / CASE_LOG_NAME).write_text(log_text)
    (TEST_DIR / f"{case_dir.name}.log").write_text(log_text)


def write_master_log(case_results: list[tuple[Path, subprocess.CompletedProcess[str]]]) -> None:
    chunks: list[str] = []
    for case_dir, completed in case_results:
        chunks.append(f"===== {case_dir.name} =====")
        chunks.append(f"returncode = {completed.returncode}")
        chunks.append(completed.stdout)
        chunks.append(completed.stderr)
        chunks.append("")
    RUN_LOG.write_text("\n".join(chunks))


def write_run_summary(case_results: list[tuple[Path, subprocess.CompletedProcess[str]]]) -> None:
    lines = ["SUPERFACT batch run summary", ""]

    for case_dir, completed in case_results:
        lines.append(f"{case_dir.name}: returncode={completed.returncode}")
        lines.append(f"  case_log = {case_dir / CASE_LOG_NAME}")
        lines.append(f"  root_log = {TEST_DIR / f'{case_dir.name}.log'}")

        output_file = case_dir / MAIN_OUTPUT_NAME
        thermo_output_file = case_dir / THERMO_OUTPUT_NAME
        lines.append(f"  output = {'yes' if output_file.exists() else 'no'}")
        lines.append(f"  thermochemistry_output = {'yes' if thermo_output_file.exists() else 'no'}")
        lines.append("")

    RUN_SUMMARY.write_text("\n".join(lines))


def atoms_per_m3_to_umol_per_mm3(series: np.ndarray) -> np.ndarray:
    return series / AVOGADRO_NUMBER * 1.0e-3


def is_all_zero(series: np.ndarray) -> bool:
    return np.allclose(series, 0.0)


def build_axis_mappers(x_primary: np.ndarray, x_secondary: np.ndarray):
    def primary_to_secondary(x):
        return np.interp(x, x_primary, x_secondary)

    def secondary_to_primary(x):
        return np.interp(x, x_secondary, x_primary)

    return primary_to_secondary, secondary_to_primary


def create_secondary_axis(axis, primary_to_secondary, secondary_to_primary, label: str) -> None:
    top_axis = axis.secondary_xaxis("top", functions=(primary_to_secondary, secondary_to_primary))
    top_axis.set_xlabel(label)


def get_inventory_groups(headers: list[str]) -> dict[str, list[str]]:
    grouped_variables: dict[str, list[str]] = {}
    inventory_markers = {"produced", "in", "at", "released", "reacted"}

    for header in headers:
        parts = header.split()
        if len(parts) < 2 or parts[1] not in inventory_markers:
            continue

        grouped_variables.setdefault(parts[0], []).append(header)

    return grouped_variables


def make_case_plot_dir(plot_dir: Path, case_dir: Path) -> Path:
    case_plot_dir = plot_dir / case_dir.name
    case_plot_dir.mkdir(exist_ok=True)
    return case_plot_dir


def plot_case_histories(case_dirs: list[Path], plot_dir: Path, saved_paths: list[Path]) -> None:
    histories = []
    for case_dir in case_dirs:
        output_file = case_dir / MAIN_OUTPUT_NAME
        headers, values = load_output_data(output_file)
        column_map = {name: idx for idx, name in enumerate(headers)}
        histories.append((case_dir, column_map, values))

    for column_name, title, ylabel, accent_color in HISTORY_PLOT_COLUMNS:
        fig, axis = plt.subplots(figsize=(13.5, 8.5))
        color_positions = np.linspace(0, 1, max(len(histories), 2))
        colors = plt.cm.viridis(color_positions)
        plotted_series: list[np.ndarray] = []

        for color, (case_dir, column_map, values) in zip(colors, histories):
            if column_name not in column_map:
                continue
            time = values[:, column_map["Time (h)"]]
            series = values[:, column_map[column_name]]
            axis.plot(time, series, label=case_label(case_dir), color=color)
            plotted_series.append(series)

        style_axis(axis, title, "Time (h)", ylabel)
        axis.axhline(0.0, color=accent_color, linewidth=1.2, alpha=0.25)

        if column_name == "O/M ratio (/)" and plotted_series:
            stacked = np.concatenate(plotted_series)
            ymin = float(np.nanmin(stacked))
            ymax = float(np.nanmax(stacked))
            padding = max((ymax - ymin) * 0.18, 1.0e-4)
            axis.set_ylim(ymin - padding, ymax + padding)

        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), title="Radius")
        fig.tight_layout()

        plot_path = append_saved_path(saved_paths, plot_dir, f"{sanitize_filename(column_name)}_history_radial.png")
        fig.savefig(plot_path, dpi=220, bbox_inches="tight")
        plt.close(fig)


def build_final_profiles(case_dirs: list[Path], output_name: str) -> tuple[np.ndarray, dict[str, np.ndarray]]:
    radii_mm: list[float] = []
    profiles: dict[str, list[float]] = {}

    for case_dir in case_dirs:
        headers, values = load_output_data(case_dir / output_name)
        final_row = values[-1]
        radii_mm.append(parse_radius_mm(case_dir))

        for index, header in enumerate(headers):
            profiles.setdefault(header, []).append(final_row[index])

    radii = np.array(radii_mm, dtype=float)
    order = np.argsort(radii)
    sorted_profiles = {
        name: np.array(series, dtype=float)[order]
        for name, series in profiles.items()
    }
    return radii[order], sorted_profiles


def build_time_histories(case_dirs: list[Path], output_name: str) -> tuple[np.ndarray, dict[str, np.ndarray]]:
    histories_by_radius: list[tuple[float, dict[str, np.ndarray]]] = []

    for case_dir in case_dirs:
        headers, values = load_output_data(case_dir / output_name)
        series_map = {
            header: values[:, index]
            for index, header in enumerate(headers)
        }
        histories_by_radius.append((parse_radius_mm(case_dir), series_map))

    histories_by_radius.sort(key=lambda item: item[0])
    radii_mm = np.array([radius for radius, _ in histories_by_radius], dtype=float)
    reference_series_map = max(histories_by_radius, key=lambda item: len(item[1]["Time (h)"]))[1]
    reference_time = reference_series_map["Time (h)"]
    common_names = set(reference_series_map.keys())
    for _, series_map in histories_by_radius[1:]:
        common_names &= set(series_map.keys())

    histories: dict[str, np.ndarray] = {}
    for name in sorted(common_names):
        aligned_series = []
        for _, series_map in histories_by_radius:
            case_time = series_map["Time (h)"]
            case_values = series_map[name]
            aligned_series.append(np.interp(reference_time, case_time, case_values))
        histories[name] = np.vstack(aligned_series)

    return radii_mm, histories


def condensed_grain_boundary_headers(headers: list[str]) -> list[str]:
    return [
        header
        for header in headers
        if "(condensed, at grain boundary)" in header
    ]


def compute_annulus_areas(radii_m: np.ndarray) -> np.ndarray:
    boundaries = np.empty(len(radii_m) + 1, dtype=float)
    boundaries[0] = 0.0
    boundaries[-1] = radii_m[-1]
    boundaries[1:-1] = 0.5 * (radii_m[:-1] + radii_m[1:])
    return math.pi * (boundaries[1:] ** 2 - boundaries[:-1] ** 2)


def plot_radial_summary(
    radii_mm: np.ndarray,
    profiles: dict[str, np.ndarray],
    plot_dir: Path,
    saved_paths: list[Path],
) -> None:
    variables = [name for name in RADIAL_SUMMARY_VARIABLES if name in profiles]
    if not variables:
        return

    ncols = 2
    nrows = math.ceil(len(variables) / ncols)
    fig, axes = plt.subplots(nrows, ncols, figsize=(17, 5.5 * nrows), sharex=True)
    axes = np.atleast_1d(axes).flatten()

    radius_fraction = radii_mm / radii_mm.max()

    for axis, variable in zip(axes, variables):
        axis.plot(radius_fraction, profiles[variable], color="#0f766e", marker="o")
        style_axis(axis, variable, "Radius / Rmax (-)", variable)

    for axis in axes[len(variables):]:
        axis.set_visible(False)

    fig.suptitle("SUPERFACT Radial Profiles at Final Time", fontsize=21, weight="bold")
    fig.tight_layout(rect=(0, 0, 1, 0.98))

    plot_path = append_saved_path(saved_paths, plot_dir, "superfact_radial_summary.png")
    fig.savefig(plot_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_inventory_radial_groups(
    radii_mm: np.ndarray,
    profiles: dict[str, np.ndarray],
    plot_dir: Path,
    saved_paths: list[Path],
) -> None:
    radius_fraction = radii_mm / radii_mm.max()
    groups = ("Xe", "Kr", "Cs", "I", "Te", "Mo")

    for group in groups:
        variables = [
            name for name in profiles
            if name.startswith(f"{group} ")
            and (
                " produced " in f" {name} "
                or " in " in f" {name} "
                or " at " in f" {name} "
                or " released " in f" {name} "
                or " reacted " in f" {name} "
            )
        ]
        if not variables:
            continue

        fig, axis = plt.subplots(figsize=(14, 8.5))
        colors = plt.cm.tab10(np.linspace(0, 1, len(variables)))
        for color, variable in zip(colors, variables):
            axis.plot(radius_fraction, profiles[variable], marker="o", label=variable.replace(f"{group} ", ""), color=color)

        style_axis(axis, f"{group} Radial Inventory at Final Time", "Radius / Rmax (-)", "Concentration (at/m3)")
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5))
        fig.tight_layout()

        plot_path = append_saved_path(saved_paths, plot_dir, f"{sanitize_filename(group)}_radial_inventory.png")
        fig.savefig(plot_path, dpi=220, bbox_inches="tight")
        plt.close(fig)

        if group in {"Xe", "Kr", "Cs"}:
            fig, axis = plt.subplots(figsize=(14, 8.5))
            for color, variable in zip(colors, variables):
                axis.plot(
                    radius_fraction,
                    atoms_per_m3_to_umol_per_mm3(profiles[variable]),
                    marker="o",
                    label=variable.replace(f"{group} ", ""),
                    color=color,
                )

            style_axis(axis, f"{group} Radial Inventory at Final Time", "Radius / Rmax (-)", "Concentration (umol/mm3)")
            axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5))
            fig.tight_layout()

            plot_path = append_saved_path(saved_paths, plot_dir, f"{sanitize_filename(group)}_radial_inventory_umol_mm3.png")
            fig.savefig(plot_path, dpi=220, bbox_inches="tight")
            plt.close(fig)


def plot_thermochemistry_radial_group(
    radii_mm: np.ndarray,
    profiles: dict[str, np.ndarray],
    location_label: str,
    filename: str,
    plot_dir: Path,
    saved_paths: list[Path],
) -> None:
    variables = [
        name for name in profiles
        if f", {location_label})" in name
        and not is_all_zero(profiles[name])
    ]
    if not variables:
        return

    radius_fraction = radii_mm / radii_mm.max()
    fig, axis = plt.subplots(figsize=(15, 9))
    colors = plt.cm.nipy_spectral(np.linspace(0, 1, len(variables)))

    for color, variable in zip(colors, variables):
        axis.plot(radius_fraction, profiles[variable], label=variable, linewidth=1.6, marker="o", color=color)

    style_axis(axis, f"Thermochemistry Radial Profile: {location_label}", "Radius / Rmax (-)", "Amount / concentration")
    axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), fontsize=8)
    fig.tight_layout()

    plot_path = append_saved_path(saved_paths, plot_dir, filename)
    fig.savefig(plot_path, dpi=200, bbox_inches="tight")
    plt.close(fig)


def extract_case_block(master_log_text: str, case_dir: Path) -> str:
    pattern = rf"===== {re.escape(case_dir.name)} =====\n(.*?)(?=\n===== |\Z)"
    match = re.search(pattern, master_log_text, flags=re.DOTALL)
    return match.group(1) if match else ""


def parse_last_jog_value(case_block: str, label: str) -> float | None:
    matches = re.findall(rf"{re.escape(label)} = ([^\s]+)", case_block)
    if not matches:
        return None

    try:
        return float(matches[-1])
    except ValueError:
        return None


def build_jog_profiles(case_dirs: list[Path], output_profiles: dict[str, np.ndarray]) -> dict[str, np.ndarray]:
    jog_profiles: dict[str, list[float]] = {}

    if "JOG (/)" in output_profiles:
        jog_profiles["JOG (/)"] = output_profiles["JOG (/)"].tolist()

    if not RUN_LOG.exists():
        return {name: np.array(values, dtype=float) for name, values in jog_profiles.items()}

    master_log_text = RUN_LOG.read_text()
    log_labels = (
        "JOG thickness from s1 - volume [m3 ompound / m3 fuel]",
        "JOG thickness from s2 - volume [m3 ompound / m3 fuel]",
        "Total JOG thickness - volume [m3 ompound / m3 fuel]",
    )

    for label in log_labels:
        values: list[float] = []
        for case_dir in case_dirs:
            case_block = extract_case_block(master_log_text, case_dir)
            value = parse_last_jog_value(case_block, label)
            if value is None:
                break
            values.append(value)
        if len(values) == len(case_dirs):
            jog_profiles[label] = values

    return {name: np.array(values, dtype=float) for name, values in jog_profiles.items()}


def plot_jog_profiles(
    radii_mm: np.ndarray,
    jog_profiles: dict[str, np.ndarray],
    plot_dir: Path,
    saved_paths: list[Path],
) -> None:
    if not jog_profiles:
        return

    radius_fraction = radii_mm / radii_mm.max()
    fig, axes = plt.subplots(1, 2, figsize=(18, 7.5), sharex=True)

    if "JOG (/)" in jog_profiles:
        axes[0].plot(radius_fraction, jog_profiles["JOG (/)"], color="#7c3aed", marker="o")
        style_axis(axes[0], "JOG Radial Profile", "Radius / Rmax (-)", "JOG (/)")
    else:
        axes[0].set_visible(False)

    thickness_labels = [
        "JOG thickness from s1 - volume [m3 ompound / m3 fuel]",
        "JOG thickness from s2 - volume [m3 ompound / m3 fuel]",
        "Total JOG thickness - volume [m3 ompound / m3 fuel]",
    ]
    thickness_series = [(label, jog_profiles[label]) for label in thickness_labels if label in jog_profiles]

    if thickness_series:
        colors = ("#0f766e", "#f97316", "#1d4ed8")
        for color, (label, series) in zip(colors, thickness_series):
            axes[1].plot(
                radius_fraction,
                series,
                marker="o",
                label=label.replace(" [m3 ompound / m3 fuel]", ""),
                color=color,
            )
        style_axis(
            axes[1],
            "JOG Thickness Radial Profile",
            "Radius / Rmax (-)",
            "Volume fraction (m3 compound / m3 fuel)",
        )
        axes[1].legend(loc="best")
    else:
        axes[1].set_visible(False)

    fig.tight_layout()

    plot_path = append_saved_path(saved_paths, plot_dir, "jog_radial_summary.png")
    fig.savefig(plot_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_jog_thickness_vs_fima(
    radii_mm: np.ndarray,
    output_histories: dict[str, np.ndarray],
    plot_dir: Path,
    saved_paths: list[Path],
) -> None:
    required_labels = ("FIMA (%)", "JOG (/)")
    if any(label not in output_histories for label in required_labels):
        return

    radii_m = radii_mm * 1.0e-3
    ring_areas = compute_annulus_areas(radii_m)
    outer_radius_m = radii_m[-1]

    fima = output_histories["FIMA (%)"][0]
    jog_fraction = output_histories["JOG (/)"]
    jog_volume_per_height = np.sum(jog_fraction * ring_areas[:, np.newaxis], axis=0)
    jog_thickness_um = jog_volume_per_height / (2.0 * math.pi * outer_radius_m) * 1.0e6

    fig, axis = plt.subplots(figsize=(13.5, 8.5))
    axis.plot(fima, jog_thickness_um, color="#7c3aed", linewidth=2.4, label="SCIANTIX integrated")

    experimental_sets = [
        ("Melis 1993", EXP_DATA_DIR / "Melis1993.txt", "#f97316", "o"),
        ("Tourasse 1992", EXP_DATA_DIR / "Tourasse1992.txt", "#0f766e", "s"),
    ]
    for label, data_path, color, marker in experimental_sets:
        if not data_path.exists():
            continue
        exp_fima, exp_thickness = load_experimental_jog_data(data_path)
        axis.scatter(exp_fima, exp_thickness, color=color, marker=marker, s=60, label=label, alpha=0.9)

    style_axis(axis, "Integrated JOG Thickness vs FIMA", "FIMA (%)", "Thickness (um)")
    axis.legend(loc="best")
    axis.text(
        0.02,
        0.02,
        "Thickness = integral(JOG dV) / outer fuel surface",
        transform=axis.transAxes,
        fontsize=11,
        color="#475569",
    )
    fig.tight_layout()

    plot_path = append_saved_path(saved_paths, plot_dir, "jog_thickness_vs_fima.png")
    fig.savefig(plot_path, dpi=220, bbox_inches="tight")
    plt.close(fig)

    table_path = plot_dir / "jog_thickness_vs_fima.tsv"
    with table_path.open("w", newline="") as handle:
        writer = csv.writer(handle, delimiter="\t")
        writer.writerow(["FIMA (%)", "Integrated JOG thickness (um)"])
        for fima_value, thickness_value in zip(fima, jog_thickness_um):
            writer.writerow([f"{fima_value:.6f}", f"{thickness_value:.6f}"])
    saved_paths.append(table_path)


def plot_condensed_grain_boundary_vs_fima(
    radii_mm: np.ndarray,
    output_histories: dict[str, np.ndarray],
    thermo_histories: dict[str, np.ndarray],
    plot_dir: Path,
    saved_paths: list[Path],
) -> None:
    if "FIMA (%)" not in output_histories:
        return

    condensed_headers = [
        header for header in condensed_grain_boundary_headers(list(thermo_histories.keys()))
        if not is_all_zero(thermo_histories[header])
    ]
    if not condensed_headers:
        return

    radii_m = radii_mm * 1.0e-3
    ring_areas = compute_annulus_areas(radii_m)
    fima = output_histories["FIMA (%)"][0]

    integrated_series = {
        header: np.sum(thermo_histories[header] * ring_areas[:, np.newaxis], axis=0)
        for header in condensed_headers
    }
    totals = {header: float(np.trapezoid(series, fima)) for header, series in integrated_series.items()}
    ranked_headers = sorted(condensed_headers, key=lambda header: totals[header], reverse=True)
    selected_headers = ranked_headers[:CONDENSED_TOP_SPECIES]
    other_headers = ranked_headers[CONDENSED_TOP_SPECIES:]

    stacked_labels = [
        header.replace(" (condensed, at grain boundary) (mol/m3)", "")
        for header in selected_headers
    ]
    stacked_series = [integrated_series[header] for header in selected_headers]

    if other_headers:
        stacked_labels.append("Other condensed")
        stacked_series.append(np.sum([integrated_series[header] for header in other_headers], axis=0))

    total_condensed = np.sum([integrated_series[header] for header in condensed_headers], axis=0)
    cs2moo4_headers = [
        header for header in condensed_headers
        if "CS2MOO4_S1" in header or "CS2MOO4_S2" in header
    ]
    cs2moo4_total = np.sum([integrated_series[header] for header in cs2moo4_headers], axis=0) if cs2moo4_headers else None

    fig, axes = plt.subplots(2, 1, figsize=(14, 13), sharex=True, height_ratios=(1.2, 1.0))

    colors = plt.cm.tab20(np.linspace(0, 1, len(stacked_series)))
    axes[0].stackplot(fima, stacked_series, labels=stacked_labels, colors=colors, alpha=0.9)
    axes[0].plot(fima, total_condensed, color="#111827", linewidth=2.2, label="Total condensed")
    if cs2moo4_total is not None:
        axes[0].plot(fima, cs2moo4_total, color="#7c3aed", linewidth=2.0, linestyle="--", label="Cs2MoO4 only")
    style_axis(axes[0], "Integrated Condensed Grain-Boundary Inventory vs FIMA", "FIMA (%)", "Integrated amount (mol/m)")
    axes[0].legend(loc="center left", bbox_to_anchor=(1.02, 0.5), fontsize=9)

    if "JOG (/)" in output_histories:
        outer_radius_m = radii_m[-1]
        jog_volume_per_height = np.sum(output_histories["JOG (/)"] * ring_areas[:, np.newaxis], axis=0)
        jog_thickness_um = jog_volume_per_height / (2.0 * math.pi * outer_radius_m) * 1.0e6
        axes[1].plot(fima, jog_thickness_um, color="#7c3aed", linewidth=2.4, label="Cs2MoO4-based JOG thickness")
        style_axis(axes[1], "Physical JOG Thickness from Cs2MoO4 vs FIMA", "FIMA (%)", "Thickness (um)")
        axes[1].legend(loc="best")
    else:
        axes[1].set_visible(False)

    fig.tight_layout()

    plot_path = append_saved_path(saved_paths, plot_dir, "condensed_grain_boundary_vs_fima.png")
    fig.savefig(plot_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_case_inventory_groups(
    case_plot_dir: Path,
    burnup: np.ndarray,
    time: np.ndarray,
    headers: list[str],
    column_map: dict[str, int],
    values: np.ndarray,
    saved_paths: list[Path],
) -> None:
    burnup_to_time, time_to_burnup = build_axis_mappers(burnup, time)
    grouped_variables = get_inventory_groups(headers)

    for group_name, variables in grouped_variables.items():
        nonzero_variables = [
            variable for variable in variables if not is_all_zero(values[:, column_map[variable]])
        ]
        if not nonzero_variables:
            continue

        fig, axis = plt.subplots(figsize=(14, 8.5))
        colors = plt.cm.tab10(np.linspace(0, 1, len(nonzero_variables)))
        for color, variable in zip(colors, nonzero_variables):
            axis.plot(
                burnup,
                values[:, column_map[variable]],
                label=variable.replace(f"{group_name} ", ""),
                linewidth=2.0,
                color=color,
            )

        style_axis(axis, f"{group_name} Inventory Evolution", "Burnup (MWd/kgUO2)", nonzero_variables[0].split("(", 1)[-1].rstrip(")"))
        create_secondary_axis(axis, burnup_to_time, time_to_burnup, "Time (h)")
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), frameon=False)
        fig.tight_layout()

        plot_path = append_saved_path(saved_paths, case_plot_dir, f"{sanitize_filename(group_name)}_inventory.png")
        fig.savefig(plot_path, dpi=220, bbox_inches="tight")
        plt.close(fig)

        if group_name in {"Xe", "Kr", "Cs"}:
            fig, axis = plt.subplots(figsize=(14, 8.5))
            colors = plt.cm.Set2(np.linspace(0, 1, len(nonzero_variables)))
            for color, variable in zip(colors, nonzero_variables):
                axis.plot(
                    burnup,
                    atoms_per_m3_to_umol_per_mm3(values[:, column_map[variable]]),
                    label=variable.replace(f"{group_name} ", ""),
                    color=color,
                )

            style_axis(axis, f"{group_name} Inventory Evolution", "Burnup (MWd/kgUO2)", "Concentration (umol/mm3)")
            create_secondary_axis(axis, burnup_to_time, time_to_burnup, "Time (h)")
            axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), frameon=False)
            fig.tight_layout()

            plot_path = append_saved_path(saved_paths, case_plot_dir, f"{sanitize_filename(group_name)}_inventory_umol_mm3.png")
            fig.savefig(plot_path, dpi=220, bbox_inches="tight")
            plt.close(fig)


def plot_case_summary_variables(
    case_plot_dir: Path,
    burnup: np.ndarray,
    time: np.ndarray,
    headers: list[str],
    column_map: dict[str, int],
    values: np.ndarray,
    saved_paths: list[Path],
) -> None:
    grouped_variable_names = {
        variable for variables in get_inventory_groups(headers).values() for variable in variables
    }
    summary_variables = [
        header
        for header in headers
        if header not in {"Burnup (MWd/kgUO2)", "Time (h)"}
        and header not in grouped_variable_names
        and not is_all_zero(values[:, column_map[header]])
    ]

    plots_per_page = 6
    ncols = 2
    nrows = 3
    burnup_to_time, time_to_burnup = build_axis_mappers(burnup, time)

    for page_index, start in enumerate(range(0, len(summary_variables), plots_per_page), start=1):
        page_variables = summary_variables[start:start + plots_per_page]
        fig, axes = plt.subplots(nrows, ncols, figsize=(18, 16), sharex=True)
        axes = axes.flatten()

        for axis, variable in zip(axes, page_variables):
            axis.plot(burnup, values[:, column_map[variable]], color="#0f766e", linewidth=2.0)
            style_axis(axis, variable, "Burnup (MWd/kgUO2)", variable)
            create_secondary_axis(axis, burnup_to_time, time_to_burnup, "Time (h)")

        for axis in axes[len(page_variables):]:
            axis.set_visible(False)

        fig.suptitle("SCIANTIX Summary Variables vs Burnup and Time", fontsize=21, weight="bold")
        fig.tight_layout(rect=(0, 0, 1, 0.98))

        plot_path = append_saved_path(saved_paths, case_plot_dir, f"summary_variables_{page_index:02d}.png")
        fig.savefig(plot_path, dpi=220, bbox_inches="tight")
        plt.close(fig)


def plot_case_oxygen_activity(
    case_plot_dir: Path,
    burnup: np.ndarray,
    time: np.ndarray,
    column_map: dict[str, int],
    values: np.ndarray,
    saved_paths: list[Path],
) -> None:
    p_o2_label = "Fuel oxygen partial pressure (MPa)"
    if p_o2_label not in column_map:
        return

    p_o2 = values[:, column_map[p_o2_label]]
    activity_o = np.sqrt(np.clip(p_o2, 0.0, None) / 0.1)
    burnup_to_time, time_to_burnup = build_axis_mappers(burnup, time)

    fig, axis = plt.subplots(figsize=(12, 7))
    axis.plot(burnup, activity_o, color="#1d4ed8", linewidth=2.2)
    style_axis(axis, "Activity of Oxygen", "Burnup (MWd/kgUO2)", "Activity (/)")
    create_secondary_axis(axis, burnup_to_time, time_to_burnup, "Time (h)")
    fig.tight_layout()

    plot_path = append_saved_path(saved_paths, case_plot_dir, "Oactivity.png")
    fig.savefig(plot_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_case_jog_vs_fima(
    case_plot_dir: Path,
    case_dir: Path,
    burnup: np.ndarray,
    fima: np.ndarray,
    time: np.ndarray,
    column_map: dict[str, int],
    values: np.ndarray,
    saved_paths: list[Path],
) -> None:
    jog_label = "JOG (/)"
    if jog_label not in column_map:
        return

    radius_m = parse_radius_mm(case_dir) * 1.0e-3
    jog_fraction = values[:, column_map[jog_label]]
    jog_thickness_um = jog_fraction * (radius_m / 2.0) * 1.0e6
    fima_to_time, time_to_fima = build_axis_mappers(fima, time)

    fig, axis = plt.subplots(figsize=(12.5, 8.5))
    axis.plot(fima, jog_thickness_um, color="#7c3aed", label="SCIANTIX")

    for label, data_path, color, marker in (
        ("Melis 1993", EXP_DATA_DIR / "Melis1993.txt", "#dc2626", "o"),
        ("Tourasse 1992", EXP_DATA_DIR / "Tourasse1992.txt", "#2563eb", "D"),
    ):
        if not data_path.exists():
            continue
        exp_fima, exp_thickness = load_experimental_jog_data(data_path)
        axis.scatter(exp_fima, exp_thickness, color=color, marker=marker, s=46, label=label, zorder=3)

    style_axis(axis, "JOG Thickness vs FIMA", "FIMA (%)", "Thickness (um)")
    create_secondary_axis(axis, fima_to_time, time_to_fima, "Time (h)")
    axis.text(
        0.02,
        0.95,
        f"Thickness = JOG * R/2,  R = {radius_m * 1.0e3:.4f} mm",
        transform=axis.transAxes,
        ha="left",
        va="top",
        fontsize=13,
        color="#4b5563",
    )
    axis.legend(loc="upper left")
    fig.tight_layout()

    plot_path = append_saved_path(saved_paths, case_plot_dir, "JOG_fraction_vs_time_and_thickness_vs_fima.png")
    fig.savefig(plot_path, dpi=240, bbox_inches="tight")
    plt.close(fig)


def plot_case_thermochemistry_group(
    case_plot_dir: Path,
    thermo_output_file: Path,
    output_file: Path,
    location_label: str,
    filename: str,
    saved_paths: list[Path],
) -> None:
    headers, values = load_output_data(thermo_output_file)
    column_map = {name: idx for idx, name in enumerate(headers)}
    reference_headers, reference_values = load_output_data(output_file)
    reference_column_map = {name: idx for idx, name in enumerate(reference_headers)}

    time_label = "Time (h)"
    burnup_label = "Burnup (MWd/kgUO2)"
    if time_label not in column_map or time_label not in reference_column_map or burnup_label not in reference_column_map:
        return

    variables = [
        header
        for header in headers
        if header not in {burnup_label, time_label}
        and f", {location_label})" in header
        and not is_all_zero(values[:, column_map[header]])
    ]
    if not variables:
        return

    time = values[:, column_map[time_label]]
    reference_time = reference_values[:, reference_column_map[time_label]]
    reference_burnup = reference_values[:, reference_column_map[burnup_label]]
    burnup = np.interp(time, reference_time, reference_burnup)
    burnup_to_time, time_to_burnup = build_axis_mappers(reference_burnup, reference_time)

    fig, axis = plt.subplots(figsize=(15, 9))
    colors = plt.cm.nipy_spectral(np.linspace(0, 1, len(variables)))
    for color, variable in zip(colors, variables):
        axis.plot(burnup, values[:, column_map[variable]], label=variable, linewidth=1.4, color=color)

    style_axis(axis, f"Thermochemistry Variables: {location_label}", burnup_label, "Amount / concentration")
    create_secondary_axis(axis, burnup_to_time, time_to_burnup, time_label)
    axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), fontsize=8, frameon=False)
    fig.tight_layout()

    plot_path = append_saved_path(saved_paths, case_plot_dir, filename)
    fig.savefig(plot_path, dpi=200, bbox_inches="tight")
    plt.close(fig)


def compute_case_thermochemistry_jog(
    output_headers: list[str],
    output_values: np.ndarray,
    thermo_headers: list[str],
    thermo_values: np.ndarray,
) -> dict[str, np.ndarray | str]:
    output_column_map = {name: idx for idx, name in enumerate(output_headers)}
    thermo_column_map = {name: idx for idx, name in enumerate(thermo_headers)}

    burnup = output_values[:, output_column_map["Burnup (MWd/kgUO2)"]]
    time = output_values[:, output_column_map["Time (h)"]]
    fima = output_values[:, output_column_map["FIMA (%)"]]
    jog_fraction = output_values[:, output_column_map["JOG (/)"]]
    thermochemistry_time = thermo_values[:, thermo_column_map["Time (h)"]]
    thermochemistry_burnup = np.interp(thermochemistry_time, time, burnup)

    liquid_cs_label = "CS (liquid, at grain boundary) (mol/m3)"
    liquid_mo_label = "MO (liquid, at grain boundary) (mol/m3)"
    liquid_o_label = "O (liquid, at grain boundary) (mol/m3)"
    condensed_cs2moo4_s1_label = "CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)"
    condensed_cs2moo4_s2_label = "CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)"

    liquid_cs = np.zeros_like(thermochemistry_burnup)
    if liquid_cs_label in thermo_column_map:
        liquid_cs = thermo_values[:, thermo_column_map[liquid_cs_label]]

    liquid_mo = np.zeros_like(thermochemistry_burnup)
    if liquid_mo_label in thermo_column_map:
        liquid_mo = thermo_values[:, thermo_column_map[liquid_mo_label]]

    liquid_o = np.zeros_like(thermochemistry_burnup)
    if liquid_o_label in thermo_column_map:
        liquid_o = thermo_values[:, thermo_column_map[liquid_o_label]]

    liquid_cs2moo4_limiters = np.vstack([
        liquid_cs / 2.0,
        liquid_mo,
        liquid_o / 4.0,
    ])
    limiter_labels = ["Cs/2", "Mo", "O/4"]
    limiter_index = np.argmin(liquid_cs2moo4_limiters, axis=0)
    final_limiter_label = limiter_labels[int(limiter_index[-1])]
    liquid_cs2moo4_available = np.min(liquid_cs2moo4_limiters, axis=0)

    cs2moo4_molar_mass = (
        2.0 * ATOMIC_MASS_G_PER_MOL["Cs"]
        + ATOMIC_MASS_G_PER_MOL["Mo"]
        + 4.0 * ATOMIC_MASS_G_PER_MOL["O"]
    )

    temperature = thermo_values[:, thermo_column_map["Temperature (K)"]]
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
    if condensed_cs2moo4_s1_label in thermo_column_map:
        condensed_cs2moo4_s1 = thermo_values[:, thermo_column_map[condensed_cs2moo4_s1_label]]
        condensed_cs2moo4_s1_volume_fraction = (
            condensed_cs2moo4_s1 * cs2moo4_molar_mass / condensed_s1_density_g_per_m3
        )

    condensed_cs2moo4_s2_volume_fraction = np.zeros_like(thermochemistry_burnup)
    if condensed_cs2moo4_s2_label in thermo_column_map:
        condensed_cs2moo4_s2 = thermo_values[:, thermo_column_map[condensed_cs2moo4_s2_label]]
        condensed_cs2moo4_s2_volume_fraction = (
            condensed_cs2moo4_s2 * cs2moo4_molar_mass / condensed_s2_density_g_per_m3
        )

    condensed_cs2moo4_volume_fraction = condensed_cs2moo4_s1_volume_fraction + condensed_cs2moo4_s2_volume_fraction
    return {
        "burnup": thermochemistry_burnup,
        "time": thermochemistry_time,
        "fima": np.interp(thermochemistry_time, time, fima),
        "model_jog_fraction": np.interp(thermochemistry_time, time, jog_fraction),
        "liquid_volume_fraction": liquid_cs2moo4_volume_fraction,
        "condensed_volume_fraction": condensed_cs2moo4_volume_fraction,
        "total_volume_fraction": liquid_cs2moo4_volume_fraction + condensed_cs2moo4_volume_fraction,
        "limiter_labels": np.array(limiter_labels, dtype=object),
        "limiter_index": limiter_index,
        "final_limiter_label": final_limiter_label,
        "liquid_limiters": liquid_cs2moo4_limiters,
    }


def plot_case_inventory_overview(
    case_plot_dir: Path,
    burnup: np.ndarray,
    time: np.ndarray,
    column_map: dict[str, int],
    values: np.ndarray,
    saved_paths: list[Path],
) -> None:
    required = [
        "Grain boundary fraction (/)",
        "Fuel oxygen partial pressure (MPa)",
        "Oxygen content (mol/m3)",
        "Uranium content (mol/m3)",
        "Xe produced (at/m3)",
        "Xe in grain (at/m3)",
        "Xe at grain boundary (at/m3)",
        "Xe released (at/m3)",
        "Kr produced (at/m3)",
        "Kr in grain (at/m3)",
        "Kr at grain boundary (at/m3)",
        "Kr released (at/m3)",
        "Cs produced (at/m3)",
        "Cs in grain (at/m3)",
        "Cs at grain boundary (at/m3)",
        "Cs reacted - GB (at/m3)",
        "Cs released (at/m3)",
        "Mo produced (at/m3)",
        "Mo in grain (at/m3)",
        "Mo at grain boundary (at/m3)",
        "Mo reacted - GB (at/m3)",
        "Mo released (at/m3)",
    ]
    if any(label not in column_map for label in required):
        return

    burnup_to_time, time_to_burnup = build_axis_mappers(burnup, time)
    grain_boundary_fraction = values[:, column_map["Grain boundary fraction (/)"]]
    oxygen_activity = np.sqrt(np.clip(values[:, column_map["Fuel oxygen partial pressure (MPa)"]], 0.0, None) / 0.1)

    fig, axes = plt.subplots(3, 2, figsize=(12, 14))
    axes = axes.flatten()

    axis = axes[0]
    axis.plot(burnup, values[:, column_map["Oxygen content (mol/m3)"]], color="C0", label="Total")
    axis.plot(burnup, values[:, column_map["Oxygen content (mol/m3)"]] * grain_boundary_fraction, color="C2", label="* at grain boundary")
    axis.plot(burnup, values[:, column_map["Oxygen content (mol/m3)"]] * oxygen_activity, color="C3", label="* activity(O)")
    axis.set_xlabel("Burnup (MWd/kgUO2)")
    axis.set_ylabel("Oxygen concentration (mol/m3)")
    axis.set_yscale("log")
    create_secondary_axis(axis, burnup_to_time, time_to_burnup, "Time (h)")
    axis.legend(loc="upper left")

    axis = axes[1]
    axis.plot(burnup, values[:, column_map["Uranium content (mol/m3)"]], color="C0", label="Total")
    axis.plot(burnup, values[:, column_map["Uranium content (mol/m3)"]] * grain_boundary_fraction, color="C2", label="At grain boundary")
    axis.set_xlabel("Burnup (MWd/kgUO2)")
    axis.set_ylabel("Uranium concentration (mol/m3)")
    axis.set_yscale("log")
    create_secondary_axis(axis, burnup_to_time, time_to_burnup, "Time (h)")
    axis.legend(loc="upper left")

    for axis, species, extras in [
        (axes[2], "Xe", ["produced", "in grain", "at grain boundary", "released"]),
        (axes[3], "Kr", ["produced", "in grain", "at grain boundary", "released"]),
        (axes[4], "Cs", ["produced", "in grain", "at grain boundary", "reacted - GB", "released"]),
        (axes[5], "Mo", ["produced", "in grain", "at grain boundary", "reacted - GB", "released"]),
    ]:
        labels = [f"{species} {item} (at/m3)" for item in extras]
        colors = ["C0", "C1", "C2", "C4", "C3"][: len(labels)]
        for color, label in zip(colors, labels):
            axis.plot(burnup, values[:, column_map[label]] / AVOGADRO_NUMBER, color=color, label=label.replace(f"{species} ", "").replace(" (at/m3)", ""))
        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel(f"{species} concentration (mol/m3)")
        create_secondary_axis(axis, burnup_to_time, time_to_burnup, "Time (h)")
        axis.legend(loc="upper left")

    fig.tight_layout()
    plot_path = append_saved_path(saved_paths, case_plot_dir, "inventory_mol_m3_overview.png")
    fig.savefig(plot_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_case_po2_and_muo2(
    case_plot_dir: Path,
    burnup: np.ndarray,
    column_map: dict[str, int],
    values: np.ndarray,
    saved_paths: list[Path],
) -> None:
    po2_labels = [
        "Fuel oxygen partial pressure (MPa)",
        "Fuel oxygen partial pressure - CALPHAD (MPa)",
        "Fuel oxygen partial pressure - Blackburn (MPa)",
    ]
    mu_labels = [
        "Fuel oxygen potential (KJ/mol)",
        "Fuel oxygen potential - CALPHAD (KJ/mol)",
        "Fuel oxygen potential - Blackburn (KJ/mol)",
    ]
    if all(label in column_map for label in po2_labels):
        fig, axis = plt.subplots()
        axis.plot(burnup, np.log10(np.clip(values[:, column_map[po2_labels[0]]], 1.0e-30, None) / 0.1), color="C0", label="SCIANTIX")
        axis.plot(burnup, np.log10(np.clip(values[:, column_map[po2_labels[1]]], 1.0e-30, None) / 0.1), color="C1", label="CALPHAD")
        axis.plot(burnup, np.log10(np.clip(values[:, column_map[po2_labels[2]]], 1.0e-30, None) / 0.1), color="C2", label="Blackburn model")
        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel(r"$\log_{10}(p_{O_2})$ (bar)")
        axis.legend()
        fig.tight_layout()
        plot_path = append_saved_path(saved_paths, case_plot_dir, "po2.png")
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)

    if all(label in column_map for label in mu_labels):
        fig, axis = plt.subplots()
        axis.plot(burnup, values[:, column_map[mu_labels[0]]], color="C0", label="SCIANTIX")
        axis.plot(burnup, values[:, column_map[mu_labels[1]]], color="C1", label="CALPHAD")
        axis.plot(burnup, values[:, column_map[mu_labels[2]]], color="C2", label="Blackburn model")
        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel("Fuel oxygen potential (kJ/mol)")
        axis.legend()
        fig.tight_layout()
        plot_path = append_saved_path(saved_paths, case_plot_dir, "muo2.png")
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)


def plot_case_thermochemistry_jog(
    case_plot_dir: Path,
    case_dir: Path,
    output_headers: list[str],
    output_values: np.ndarray,
    thermo_headers: list[str],
    thermo_values: np.ndarray,
    saved_paths: list[Path],
) -> None:
    derived = compute_case_thermochemistry_jog(output_headers, output_values, thermo_headers, thermo_values)
    radius_m = parse_radius_mm(case_dir) * 1.0e-3

    thermochemistry_jog_thickness_um = derived["total_volume_fraction"] * (radius_m / 2.0) * 1.0e6
    liquid_thickness_um = derived["liquid_volume_fraction"] * (radius_m / 2.0) * 1.0e6
    condensed_thickness_um = derived["condensed_volume_fraction"] * (radius_m / 2.0) * 1.0e6
    model_jog_thickness_um = derived["model_jog_fraction"] * (radius_m / 2.0) * 1.0e6

    fig, axis = plt.subplots()
    axis.stackplot(
        derived["burnup"],
        liquid_thickness_um,
        condensed_thickness_um,
        labels=["Liquid stoichiometric Cs2MoO4", "Condensed Cs2MoO4"],
        colors=["#fb923c", "#7c3aed"],
        alpha=0.75,
    )
    axis.plot(derived["burnup"], thermochemistry_jog_thickness_um, color="#111827", label="Total estimated thickness")
    axis.plot(derived["fima"], model_jog_thickness_um, color="#059669", linestyle="--", label="SCIANTIX JOG thickness")
    axis.text(
        0.02,
        0.98,
        f"Liquid Cs2MoO4 = min(Cs/2, Mo, O/4)\nFinal limiting factor: {derived['final_limiter_label']}",
        transform=axis.transAxes,
        ha="left",
        va="top",
    )
    axis.set_xlabel("Burnup (MWd/kgUO2)")
    axis.set_ylabel("JOG thickness (um)")
    axis.legend()
    fig.tight_layout()
    plot_path = append_saved_path(saved_paths, case_plot_dir, "JOG_thickness_from_thermochemistry.png")
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)


def build_thermochemistry_jog_profiles(case_dirs: list[Path]) -> tuple[np.ndarray, dict[str, np.ndarray]]:
    radii_mm: list[float] = []
    profiles: dict[str, list[float]] = {
        "Thermochemistry JOG total": [],
        "Thermochemistry JOG liquid": [],
        "Thermochemistry JOG condensed": [],
        "SCIANTIX JOG": [],
    }

    for case_dir in case_dirs:
        output_headers, output_values = load_output_data(case_dir / MAIN_OUTPUT_NAME)
        thermo_headers, thermo_values = load_output_data(case_dir / THERMO_OUTPUT_NAME)
        derived = compute_case_thermochemistry_jog(output_headers, output_values, thermo_headers, thermo_values)
        radii_mm.append(parse_radius_mm(case_dir))
        profiles["Thermochemistry JOG total"].append(float(derived["total_volume_fraction"][-1]))
        profiles["Thermochemistry JOG liquid"].append(float(derived["liquid_volume_fraction"][-1]))
        profiles["Thermochemistry JOG condensed"].append(float(derived["condensed_volume_fraction"][-1]))
        profiles["SCIANTIX JOG"].append(float(derived["model_jog_fraction"][-1]))

    radii = np.array(radii_mm, dtype=float)
    order = np.argsort(radii)
    return radii[order], {name: np.array(values, dtype=float)[order] for name, values in profiles.items()}


def plot_thermochemistry_jog_radial_profiles(
    radii_mm: np.ndarray,
    jog_profiles: dict[str, np.ndarray],
    plot_dir: Path,
    saved_paths: list[Path],
) -> None:
    radius_fraction = radii_mm / radii_mm.max()
    fig, axes = plt.subplots(1, 2, figsize=(18, 7.5), sharex=True)
    axes[0].plot(radius_fraction, jog_profiles["SCIANTIX JOG"], color="#059669", marker="o", label="SCIANTIX JOG")
    axes[0].plot(radius_fraction, jog_profiles["Thermochemistry JOG total"], color="#111827", marker="o", label="Thermochemistry total")
    style_axis(axes[0], "JOG Radial Profile", "Radius / Rmax (-)", "JOG (/)")
    axes[0].legend(loc="best")

    axes[1].plot(radius_fraction, jog_profiles["Thermochemistry JOG liquid"], color="#fb923c", marker="o", label="Liquid Cs2MoO4")
    axes[1].plot(radius_fraction, jog_profiles["Thermochemistry JOG condensed"], color="#7c3aed", marker="o", label="Condensed Cs2MoO4")
    axes[1].plot(radius_fraction, jog_profiles["Thermochemistry JOG total"], color="#111827", marker="o", label="Total")
    style_axis(axes[1], "Thermochemistry JOG Radial Profile", "Radius / Rmax (-)", "Volume fraction (m3 compound / m3 fuel)")
    axes[1].legend(loc="best")

    fig.tight_layout()
    plot_path = append_saved_path(saved_paths, plot_dir, "jog_thermochemistry_radial_summary.png")
    fig.savefig(plot_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_thermochemistry_jog_thickness_vs_fima(
    radii_mm: np.ndarray,
    case_dirs: list[Path],
    plot_dir: Path,
    saved_paths: list[Path],
) -> None:
    radii_m = radii_mm * 1.0e-3
    ring_areas = compute_annulus_areas(radii_m)
    outer_radius_m = radii_m[-1]

    derived_by_radius: list[tuple[float, dict[str, np.ndarray | str]]] = []

    for case_dir in case_dirs:
        output_headers, output_values = load_output_data(case_dir / MAIN_OUTPUT_NAME)
        thermo_headers, thermo_values = load_output_data(case_dir / THERMO_OUTPUT_NAME)
        derived_by_radius.append((parse_radius_mm(case_dir), compute_case_thermochemistry_jog(output_headers, output_values, thermo_headers, thermo_values)))

    derived_by_radius.sort(key=lambda item: item[0])
    reference = max(derived_by_radius, key=lambda item: len(item[1]["time"]))[1]
    reference_time = reference["time"]
    fima_reference = np.interp(reference_time, reference["time"], reference["fima"])

    total_series = []
    liquid_series = []
    condensed_series = []
    model_series = []
    for _, derived in derived_by_radius:
        derived_time = derived["time"]
        total_series.append(np.interp(reference_time, derived_time, derived["total_volume_fraction"]))
        liquid_series.append(np.interp(reference_time, derived_time, derived["liquid_volume_fraction"]))
        condensed_series.append(np.interp(reference_time, derived_time, derived["condensed_volume_fraction"]))
        model_series.append(np.interp(reference_time, derived_time, derived["model_jog_fraction"]))

    total_stack = np.vstack(total_series)
    liquid_stack = np.vstack(liquid_series)
    condensed_stack = np.vstack(condensed_series)
    model_stack = np.vstack(model_series)

    total_thickness_um = np.sum(total_stack * ring_areas[:, np.newaxis], axis=0) / (2.0 * math.pi * outer_radius_m) * 1.0e6
    liquid_thickness_um = np.sum(liquid_stack * ring_areas[:, np.newaxis], axis=0) / (2.0 * math.pi * outer_radius_m) * 1.0e6
    condensed_thickness_um = np.sum(condensed_stack * ring_areas[:, np.newaxis], axis=0) / (2.0 * math.pi * outer_radius_m) * 1.0e6
    model_thickness_um = np.sum(model_stack * ring_areas[:, np.newaxis], axis=0) / (2.0 * math.pi * outer_radius_m) * 1.0e6

    fig, axis = plt.subplots(figsize=(13.5, 8.5))
    axis.stackplot(
        fima_reference,
        liquid_thickness_um,
        condensed_thickness_um,
        labels=["Liquid stoichiometric Cs2MoO4", "Condensed Cs2MoO4"],
        colors=["#fb923c", "#7c3aed"],
        alpha=0.75,
    )
    axis.plot(fima_reference, total_thickness_um, color="#111827", linewidth=2.4, label="Thermochemistry total")
    axis.plot(fima_reference, model_thickness_um, color="#059669", linewidth=2.2, linestyle="--", label="SCIANTIX integrated")
    style_axis(axis, "Integrated Thermochemistry JOG Thickness vs FIMA", "FIMA (%)", "Thickness (um)")
    axis.legend(loc="best")
    fig.tight_layout()

    plot_path = append_saved_path(saved_paths, plot_dir, "jog_thickness_from_thermochemistry_vs_fima.png")
    fig.savefig(plot_path, dpi=220, bbox_inches="tight")
    plt.close(fig)
def plot_test_jog_style_for_case(case_dir: Path, plot_dir: Path, saved_paths: list[Path]) -> None:
    output_file = case_dir / MAIN_OUTPUT_NAME
    thermo_output_file = case_dir / THERMO_OUTPUT_NAME
    case_plot_dir = make_case_plot_dir(plot_dir, case_dir)

    headers, values = load_output_data(output_file)
    thermo_headers, thermo_values = load_output_data(thermo_output_file)
    column_map = {name: idx for idx, name in enumerate(headers)}
    burnup_label = "Burnup (MWd/kgUO2)"
    time_label = "Time (h)"
    fima_label = "FIMA (%)"
    required_labels = (burnup_label, time_label, fima_label)
    if any(label not in column_map for label in required_labels):
        return

    burnup = values[:, column_map[burnup_label]]
    time = values[:, column_map[time_label]]
    fima = values[:, column_map[fima_label]]

    plot_case_inventory_overview(case_plot_dir, burnup, time, column_map, values, saved_paths)
    plot_case_inventory_groups(case_plot_dir, burnup, time, headers, column_map, values, saved_paths)
    plot_case_summary_variables(case_plot_dir, burnup, time, headers, column_map, values, saved_paths)
    plot_case_po2_and_muo2(case_plot_dir, burnup, column_map, values, saved_paths)
    plot_case_oxygen_activity(case_plot_dir, burnup, time, column_map, values, saved_paths)
    plot_case_jog_vs_fima(case_plot_dir, case_dir, burnup, fima, time, column_map, values, saved_paths)
    plot_case_thermochemistry_jog(
        case_plot_dir,
        case_dir,
        headers,
        values,
        thermo_headers,
        thermo_values,
        saved_paths,
    )
    plot_case_thermochemistry_group(
        case_plot_dir,
        thermo_output_file,
        output_file,
        "at grain boundary",
        "thermochemistry_fission_products_at_grain_boundary.png",
        saved_paths,
    )
    plot_case_thermochemistry_group(
        case_plot_dir,
        thermo_output_file,
        output_file,
        "matrix",
        "thermochemistry_matrix.png",
        saved_paths,
    )


def validate_existing_outputs(case_dirs: list[Path]) -> None:
    missing_outputs = [case_dir / MAIN_OUTPUT_NAME for case_dir in case_dirs if not (case_dir / MAIN_OUTPUT_NAME).exists()]
    if missing_outputs:
        missing = "\n".join(str(path) for path in missing_outputs)
        raise FileNotFoundError(f"Missing existing SCIANTIX outputs:\n{missing}")

    missing_thermo = [case_dir / THERMO_OUTPUT_NAME for case_dir in case_dirs if not (case_dir / THERMO_OUTPUT_NAME).exists()]
    if missing_thermo:
        missing = "\n".join(str(path) for path in missing_thermo)
        raise FileNotFoundError(f"Missing existing thermochemistry outputs:\n{missing}")


def rerun_sciantix(case_dirs: list[Path]) -> int:
    ensure_executable(BUILD_EXECUTABLE)

    case_results: list[tuple[Path, subprocess.CompletedProcess[str]]] = []
    for case_dir in case_dirs:
        prepare_case_inputs(case_dir)
        completed = run_sciantix_case(BUILD_EXECUTABLE, case_dir)
        write_case_log(case_dir, completed)
        case_results.append((case_dir, completed))

    write_master_log(case_results)
    write_run_summary(case_results)

    failures = [case_dir.name for case_dir, completed in case_results if completed.returncode != 0]
    if failures:
        print("SCIANTIX failed for:")
        for failure in failures:
            print(f" - {failure}")
        return 1

    return 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Plot and optionally rerun SUPERFACT radial cases.")
    parser.add_argument(
        "--rerun",
        action="store_true",
        help="rerun SCIANTIX for all point_* cases before regenerating plots",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    case_dirs = sorted(path for path in TEST_DIR.glob(CASE_DIR_PATTERN) if path.is_dir())
    if not case_dirs:
        raise FileNotFoundError(f"No case directories matching {CASE_DIR_PATTERN!r} found in {TEST_DIR}")

    if args.rerun:
        rerun_return_code = rerun_sciantix(case_dirs)
        if rerun_return_code != 0:
            return rerun_return_code

    validate_existing_outputs(case_dirs)

    plot_dir = make_plot_dir()
    saved_paths: list[Path] = []
    plot_case_histories(case_dirs, plot_dir, saved_paths)

    radii_mm, output_profiles = build_final_profiles(case_dirs, MAIN_OUTPUT_NAME)
    plot_radial_summary(radii_mm, output_profiles, plot_dir, saved_paths)
    plot_inventory_radial_groups(radii_mm, output_profiles, plot_dir, saved_paths)
    plot_jog_profiles(radii_mm, build_jog_profiles(case_dirs, output_profiles), plot_dir, saved_paths)
    thermochemistry_jog_radii_mm, thermochemistry_jog_profiles = build_thermochemistry_jog_profiles(case_dirs)
    plot_thermochemistry_jog_radial_profiles(thermochemistry_jog_radii_mm, thermochemistry_jog_profiles, plot_dir, saved_paths)
    _, output_histories = build_time_histories(case_dirs, MAIN_OUTPUT_NAME)
    plot_jog_thickness_vs_fima(radii_mm, output_histories, plot_dir, saved_paths)
    plot_thermochemistry_jog_thickness_vs_fima(radii_mm, case_dirs, plot_dir, saved_paths)
    _, thermo_histories = build_time_histories(case_dirs, THERMO_OUTPUT_NAME)
    plot_condensed_grain_boundary_vs_fima(radii_mm, output_histories, thermo_histories, plot_dir, saved_paths)
    for case_dir in case_dirs:
        plot_test_jog_style_for_case(case_dir, plot_dir, saved_paths)

    _, thermo_profiles = build_final_profiles(case_dirs, THERMO_OUTPUT_NAME)
    plot_thermochemistry_radial_group(
        radii_mm,
        thermo_profiles,
        "at grain boundary",
        "thermochemistry_radial_at_grain_boundary.png",
        plot_dir,
        saved_paths,
    )
    plot_thermochemistry_radial_group(
        radii_mm,
        thermo_profiles,
        "matrix",
        "thermochemistry_radial_matrix.png",
        plot_dir,
        saved_paths,
    )

    print(f"Generated plots in {plot_dir}:")
    for path in saved_paths:
        print(path)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
