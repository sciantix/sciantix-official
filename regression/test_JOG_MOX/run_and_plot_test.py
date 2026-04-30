#!/usr/bin/env python3
import argparse
import csv
import os
import re
import shutil
import subprocess
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
import numpy as np

TEST_DIR = Path(__file__).resolve().parent
RUN_LOG = "sciantix.log"
BUILD_EXECUTABLE = TEST_DIR.parents[1] / "build" / "sciantix.x"
RUN_SUMMARY = TEST_DIR / "run_summary.txt"
MAIN_OUTPUT_NAME = "output.txt"
THERMO_OUTPUT_NAME = "thermochemistry_output.txt"
THERMOCHEMISTRY_MANIFEST_FILE = TEST_DIR / "input_thermochemistry.txt"
PLOTS_DIR = TEST_DIR / "plots"
EXP_DATA_DIR = TEST_DIR / "exp_data"
PELLET_RADIUS_M = 2.7e-3
AVOGADRO_NUMBER = 6.02214076e23
BURNUP_LABEL = "Burnup (MWd/kgUO2)"
TIME_LABEL = "Time (h)"
FIMA_LABEL = "FIMA (%)"

SHARED_INPUT_FILES = (
    "input_settings.txt",
    "input_initial_conditions.txt",
    "input_scaling_factors.txt",
    "input_thermochemistry.txt",
    "input_thermochemistry_settings.txt",
)
CASE_TEMPORARY_FILES = SHARED_INPUT_FILES + (
    "execution.txt",
    "overview.txt",
    "OCinput_matrix.OCM",
    "OCinput_grain_boundary.OCM",
    "OCoutput_matrix.DAT",
    "OCoutput_grain_boundary.DAT",
)
MAX_STACKPLOT_LEGEND_ITEMS = 50
JOG_PHASES = [
    ("CS2MOO4_S2", "JOG from CS2MOO4_S2 (/)", "CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)"),
    ("CS2MOO4_S1", "JOG from CS2MOO4_S1 (/)", "CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)"),
    ("MOO2", "JOG from MOO2 (/)", "MOO2 (condensed, at grain boundary) (mol/m3)"),
    ("CS2MO3O10", "JOG from CS2MO3O10 (/)", "CS2MO3O10 (condensed, at grain boundary) (mol/m3)"),
    ("CS2MO4O13", "JOG from CS2MO4O13 (/)", "CS2MO4O13 (condensed, at grain boundary) (mol/m3)"),
    ("BCC_A2", "JOG from BCC_A2 (/)", "BCC_A2 (condensed, at grain boundary) (mol/m3)"),
    ("FCC_A1", "JOG from FCC_A1 (/)", "FCC_A1 (condensed, at grain boundary) (mol/m3)"),
    ("HCP_A3", "JOG from HCP_A3 (/)", "HCP_A3 (condensed, at grain boundary) (mol/m3)"),
]
LIQUID_CONSTITUENT_COLORS = ["#6baed6", "#fd8d3c", "#74c476", "#9e9ac8", "#e377c2", "#8c564b", "#17becf"]
SUMMARY_STACK_COLORS = [
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
    "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78",
    "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7",
    "#dbdb8d", "#9edae5", "#393b79", "#637939", "#8c6d31", "#843c39",
]
LIQUID_TOTAL_COLUMN = "LIQUID (liquid, at grain boundary) (mol/m3)"
LIQUID_ELEMENT_SPECIES = {"Cs", "Mo", "O"}
LIQUID_CONSTITUENT_STOICHIOMETRIC_SIZE = {
    "CS+": 1.0,
    "MO+4": 1.0,
    "MOO4-2": 5.0,
    "O-2": 1.0,
    "VA": 0.0,
    "MOO3": 4.0,
    "CSO2": 3.0,
}

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

COLORS = ["#ff0000", "#ff7f00", "#00c853", "#2962ff", "#aa00ff"]
# Muted but still well-separated palette for species-consistent plots.
DISTINCT_COLOR_VALUES = [
    "#6baed6", "#fd8d3c", "#74c476", "#9e9ac8", "#e377c2", "#8c564b",
    "#17becf", "#bcbd22", "#c7a9d9", "#fdae6b", "#a1d99b", "#bcbddc",
    "#fdd0a2", "#9ecae1", "#c994c7", "#bdbdbd", "#66c2a5", "#fc8d62",
    "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3",
]


def ensure_executable(path: Path) -> None:
    if not path.exists():
        raise FileNotFoundError(f"SCIANTIX executable not found: {path}")
    if not path.is_file():
        raise FileNotFoundError(f"SCIANTIX executable path is not a file: {path}")


def ensure_output_file(path: Path) -> None:
    if not path.exists():
        raise FileNotFoundError(f"Required file not found: {path}")
    if not path.is_file():
        raise FileNotFoundError(f"Required path is not a file: {path}")


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

def save_figure(fig: plt.Figure, path: Path, saved_paths: list[Path]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(path)


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


def is_all_zero(series: np.ndarray) -> bool:
    return np.allclose(series, 0.0)


def delete_file_if_exists(path: Path) -> None:
    if path.exists() and path.is_file():
        path.unlink()


def cleanup_case_directory(case_dir: Path) -> None:
    for filename in CASE_TEMPORARY_FILES:
        delete_file_if_exists(case_dir / filename)


def add_capped_legend(axis: plt.Axes, max_items: int = MAX_STACKPLOT_LEGEND_ITEMS, **kwargs) -> None:
    handles, labels = axis.get_legend_handles_labels()
    if not handles:
        return

    if len(handles) > max_items:
        handles = handles[:max_items]
        labels = labels[:max_items]
        labels[-1] = labels[-1] + " ..."

    axis.legend(handles, labels, **kwargs)


def radial_integral_over_radius(profile: np.ndarray, radii_m_array: np.ndarray) -> np.ndarray:
    r_inner_m = radii_m_array[0]
    r_outer_m = radii_m_array[-1]
    annulus_thickness_m = r_outer_m - r_inner_m
    annulus_area_factor = (r_outer_m ** 2 - r_inner_m ** 2)
    if annulus_thickness_m <= 0.0 or annulus_area_factor <= 0.0:
        return np.zeros(profile.shape[1], dtype=float)

    # Area-average on annulus: <f> = 2/(ro^2-ri^2) * integral(f*r dr)
    # Equivalent half-thickness scaling for hollow domain: <f> * (ro-ri)/2.
    integral = np.trapezoid(profile * radii_m_array[:, np.newaxis], x=radii_m_array, axis=0)
    return integral * annulus_thickness_m / annulus_area_factor


def radial_volume_average(profile: np.ndarray, radii_m_array: np.ndarray) -> np.ndarray:
    r_min_m = radii_m_array[0]
    r_max_m = radii_m_array[-1]
    shell_edges = np.empty(len(radii_m_array) + 1, dtype=float)
    shell_edges[0] = r_min_m
    shell_edges[-1] = r_max_m
    shell_edges[1:-1] = 0.5 * (radii_m_array[:-1] + radii_m_array[1:])
    denominator = (r_max_m ** 2 - r_min_m ** 2)
    if denominator <= 0.0:
        return np.zeros(profile.shape[1], dtype=float)
    shell_weights = (shell_edges[1:] ** 2 - shell_edges[:-1] ** 2) / denominator
    return np.tensordot(shell_weights, profile, axes=(0, 0))


def radial_integral_masked_to_full_radius(
    profile: np.ndarray,
    radii_m_array: np.ndarray,
    radial_indices: list[int],
) -> np.ndarray:
    masked_profile = np.zeros_like(profile)
    masked_profile[radial_indices, :] = profile[radial_indices, :]
    return radial_integral_over_radius(masked_profile, radii_m_array)

def case_dirs() -> list[Path]:
    return sorted(path for path in TEST_DIR.glob("point_*") if path.is_dir())


def parse_radius_mm(case_dir: Path) -> float:
    match = re.search(r"_r_(\d+p\d+)mm$", case_dir.name)
    if not match:
        raise ValueError(f"Could not parse radius from case directory name: {case_dir.name}")
    return float(match.group(1).replace("p", "."))

def build_species_color_map(labels: list[str]) -> dict[str, object]:
    species_names = sorted({label.split(" (", 1)[0] for label in labels})
    if not species_names:
        return {}

    return {
        species: DISTINCT_COLOR_VALUES[index % len(DISTINCT_COLOR_VALUES)]
        for index, species in enumerate(species_names)
    }

def build_label_color_map(labels: list[str]) -> dict[str, object]:
    unique_labels = sorted(set(labels))
    if not unique_labels:
        return {}

    species_color_map = build_species_color_map(unique_labels)
    return {
        label: species_color_map[label.split(" (", 1)[0]]
        for label in unique_labels
    }


def assign_distinct_colors(labels: list[str], palette: list[str] = SUMMARY_STACK_COLORS) -> dict[str, object]:
    return {
        label: palette[index % len(palette)]
        for index, label in enumerate(labels)
    }


def grain_boundary_phase(header: str) -> str:
    match = re.search(r"\(([^,]+), at grain boundary\)", header)
    return match.group(1).strip().lower() if match else "unknown"


def grain_boundary_species(header: str) -> str:
    return header.split(" (", 1)[0]


def is_liquid_total_column(header: str) -> bool:
    return grain_boundary_phase(header) == "liquid" and grain_boundary_species(header).upper() == "LIQUID"


def is_liquid_fraction_column(header: str) -> bool:
    return grain_boundary_phase(header) == "liquid" and not is_liquid_total_column(header)


def is_liquid_element_column(header: str) -> bool:
    return is_liquid_fraction_column(header) and grain_boundary_species(header) in LIQUID_ELEMENT_SPECIES


def is_liquid_constituent_column(header: str) -> bool:
    return is_liquid_fraction_column(header) and not is_liquid_element_column(header)


def liquid_constituent_stoichiometric_size(header: str) -> float:
    return LIQUID_CONSTITUENT_STOICHIOMETRIC_SIZE.get(grain_boundary_species(header), 1.0)


def is_grain_boundary_amount_column(header: str) -> bool:
    return (
        header not in {BURNUP_LABEL, TIME_LABEL}
        and ", at grain boundary)" in header
        and not is_liquid_fraction_column(header)
    )


def build_thermochemistry_color_map(case_directories: list[Path]) -> dict[str, object]:
    gb_labels: list[str] = []

    for case_dir in case_directories:
        thermo_file = case_dir / THERMO_OUTPUT_NAME
        ensure_output_file(thermo_file)
        thermochemistry_headers, _ = load_output_data(thermo_file)

        gb_labels.extend(
            header
            for header in thermochemistry_headers
            if is_grain_boundary_amount_column(header)
        )

    return build_label_color_map(gb_labels)


def prepare_case_inputs(case_dir: Path) -> None:
    for filename in SHARED_INPUT_FILES:
        source = TEST_DIR / filename
        target = case_dir / filename
        if source.exists():
            shutil.copy2(source, target)


def run_sciantix_case(case_dir: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(BUILD_EXECUTABLE)],
        cwd=case_dir,
        text=True,
        capture_output=True,
        check=False,
    )

def plot_case(
    case_dir: Path,
    saved_paths: list[Path],
    gb_color_map: dict[str, object],
) -> None:
    output_file = case_dir / MAIN_OUTPUT_NAME
    thermo_file = case_dir / THERMO_OUTPUT_NAME
    ensure_output_file(output_file)
    ensure_output_file(thermo_file)

    headers, values = load_output_data(output_file)
    columns = column_map(headers)
    thermochemistry_headers, thermochemistry_values = load_output_data(thermo_file)
    thermochemistry_columns = column_map(thermochemistry_headers)

    burnup = values[:, columns[BURNUP_LABEL]]
    time = values[:, columns[TIME_LABEL]]

    thermochemistry_time = thermochemistry_values[:, thermochemistry_columns[TIME_LABEL]]
    thermochemistry_burnup = np.interp(thermochemistry_time, time, burnup)

    case_plot_dir = PLOTS_DIR / case_dir.name
    case_plot_dir.mkdir(parents=True, exist_ok=True)

    fig, axes = plt.subplots(3, 2, figsize=(12, 14))
    axes = axes.flatten()

    axis = axes[0]
    if "O content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["O content (mol/m3)"]], color=COLORS[0], label="Total")
    axis.set_xlabel(BURNUP_LABEL)
    axis.set_ylabel("Oxygen concentration (mol/m3)")
    axis.legend(loc="upper left")

    axis = axes[1]
    if "U content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["U content (mol/m3)"]], color=COLORS[0], label="Uranium")
    if "Pu content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["Pu content (mol/m3)"]], color=COLORS[1], label="Plutonium")
    axis.set_xlabel(BURNUP_LABEL)
    axis.set_ylabel("Concentration (mol/m3)")
    axis.legend(loc="upper left")

    for axis, species, ylabel in [
        (axes[2], "Xe", "Xenon concentration (mol/m3)"),
        (axes[3], "Kr", "Krypton concentration (mol/m3)"),
        (axes[4], "Cs", "Cesium concentration (mol/m3)"),
        (axes[5], "Mo", "Molybdenum concentration (mol/m3)"),
    ]:
        for label, color, suffix in [
            ("Produced", COLORS[0], " produced (at/m3)"),
            ("In grain", COLORS[1], " in grain (at/m3)"),
            ("At grain boundary", COLORS[2], " at grain boundary (at/m3)"),
            ("Reacted", COLORS[3], " reacted - GB (at/m3)"),
            ("Released", COLORS[4], " released (at/m3)"),
        ]:
            column_name = f"{species}{suffix}"
            if column_name in columns:
                axis.plot(burnup, values[:, columns[column_name]] / AVOGADRO_NUMBER, color=color, label=label)
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_ylabel(ylabel)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="upper left")

    save_figure(fig, case_plot_dir / "inventory_mol_m3_overview.png", saved_paths)

    summary_variables = [
        "Temperature (K)",
        "Fission gas release (/)",
        "Stoichiometry deviation (/)"
    ]
    available_summary_variables = [name for name in summary_variables if name in columns]
    for page_index, start in enumerate(range(0, len(available_summary_variables), 4), start=1):
        page_variables = available_summary_variables[start:start + 4]
        fig, axes = plt.subplots(len(page_variables), 1, figsize=(10, 3.2 * len(page_variables)), sharex=True)
        axes = np.atleast_1d(axes)
        for axis, variable in zip(axes, page_variables):
            axis.plot(time, values[:, columns[variable]], color="#0f766e")
            axis.set_ylabel(variable)
            axis.grid(True, alpha=0.3)
        axes[-1].set_xlabel(TIME_LABEL)
        save_figure(fig, case_plot_dir / f"summary_variables_{page_index:02d}.png", saved_paths)

    if "Fuel oxygen potential (KJ/mol)" in columns:
        fig, axis = plt.subplots()
        axis.plot(burnup, values[:, columns["Fuel oxygen potential (KJ/mol)"]], label="SCIANTIX", color=COLORS[0])
        if "Fuel oxygen potential - Kato (KJ/mol)" in columns:
            axis.plot(burnup, values[:, columns["Fuel oxygen potential - Kato (KJ/mol)"]], label="Kato", color=COLORS[1], linestyle="--")
        if "Fuel oxygen potential - CALPHAD (KJ/mol)" in columns:
            axis.plot(burnup, values[:, columns["Fuel oxygen potential - CALPHAD (KJ/mol)"]], label="CALPHAD", color=COLORS[3], linestyle="--")
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_ylabel("muO2 (kJ/mol)")
        axis.legend(loc="best")
        save_figure(fig, case_plot_dir / "muo2.png", saved_paths)

    gb_variables = [
        header
        for header in thermochemistry_headers
        if is_grain_boundary_amount_column(header)
        and not is_all_zero(thermochemistry_values[:, thermochemistry_columns[header]])
    ]
    gb_sorted_variables = sorted(
        gb_variables,
        key=lambda variable: thermochemistry_values[-1, thermochemistry_columns[variable]],
        reverse=True,
    )
    fig, axis = plt.subplots()
    gb_stacked_data = [thermochemistry_values[:, thermochemistry_columns[variable]] for variable in gb_sorted_variables]
    gb_colors = [gb_color_map[variable] for variable in gb_sorted_variables]
    if gb_stacked_data:
        phase_hatch = {
            "gas": "...",
            "liquid": "///",
            "ionic_liquid": "|||",
            "condensed": "xx",
            "unknown": "\\\\\\",
        }
        gb_labels = []
        gb_hatches = []
        for variable in gb_sorted_variables:
            species = grain_boundary_species(variable)
            phase = grain_boundary_phase(variable)
            gb_labels.append(f"{species} ({phase})")
            gb_hatches.append(phase_hatch.get(phase, phase_hatch["unknown"]))

        polys = axis.stackplot(
            thermochemistry_burnup,
            gb_stacked_data,
            labels=gb_labels,
            colors=gb_colors,
            alpha=0.9,
        )
        for poly, hatch in zip(polys, gb_hatches):
            poly.set_hatch(hatch)
            poly.set_edgecolor((0.1, 0.1, 0.1, 0.7))
            poly.set_linewidth(0.2)
    axis.set_xlabel(BURNUP_LABEL)
    axis.set_ylabel("Concentration at grain boundary (mol/m3)")
    add_capped_legend(axis, loc="upper left", fontsize=8)
    save_figure(fig, case_plot_dir / "thermochemistry_fission_products_at_grain_boundary.png", saved_paths)

    if TIME_LABEL not in thermochemistry_columns:
        raise ValueError(f"Missing {TIME_LABEL} in {THERMO_OUTPUT_NAME}")

    available_columns = [
        column for column in thermochemistry_headers
        if is_liquid_constituent_column(column)
        and not is_all_zero(thermochemistry_values[:, thermochemistry_columns[column]])
    ]
    if not available_columns:
        return

    constituent_formula_units = np.vstack([
        thermochemistry_values[:, thermochemistry_columns[column]]
        for column in available_columns
    ])
    constituent_atom_equivalents = np.vstack([
        thermochemistry_values[:, thermochemistry_columns[column]] * liquid_constituent_stoichiometric_size(column)
        for column in available_columns
    ])
    constituent_values = constituent_formula_units
    constituent_ylabel = "Liquid constituent inventory (mol/m3)"
    if LIQUID_TOTAL_COLUMN in thermochemistry_columns:
        total_series = thermochemistry_values[:, thermochemistry_columns[LIQUID_TOTAL_COLUMN]]
        total_norm = np.linalg.norm(total_series)
        if total_norm > 0.0:
            raw_error = np.linalg.norm(np.sum(constituent_formula_units, axis=0) - total_series) / total_norm
            atom_equivalent_error = np.linalg.norm(np.sum(constituent_atom_equivalents, axis=0) - total_series) / total_norm
            if atom_equivalent_error < raw_error:
                constituent_values = constituent_atom_equivalents
                constituent_ylabel = "Liquid constituent atom-equivalent inventory (mol/m3)"

    labels = [column.split(" (", 1)[0] for column in available_columns]
    colors = [
        LIQUID_CONSTITUENT_COLORS[index % len(LIQUID_CONSTITUENT_COLORS)]
        for index in range(len(labels))
    ]

    fig, axis = plt.subplots(figsize=(11, 7))
    axis.stackplot(
        thermochemistry_burnup,
        constituent_values,
        labels=labels,
        colors=colors,
        alpha=0.88,
    )
    axis.set_xlabel(BURNUP_LABEL)
    axis.set_ylabel(constituent_ylabel)

    if LIQUID_TOTAL_COLUMN in thermochemistry_columns:
        total_series = thermochemistry_values[:, thermochemistry_columns[LIQUID_TOTAL_COLUMN]]
        if not is_all_zero(total_series):
            axis.plot(
                thermochemistry_burnup,
                total_series,
                color="black",
                linestyle="--",
                label="LIQUID total",
            )
            handles, labels_for_legend = axis.get_legend_handles_labels()
            axis.legend(
                handles,
                labels_for_legend,
                loc="center left",
                bbox_to_anchor=(1.12, 0.5),
            )
        else:
            axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5))
    else:
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5))
    save_figure(fig, case_plot_dir / "liquid_constituent.png", saved_paths)

    fig, axis = plt.subplots(figsize=(11, 7))
    axis.stackplot(
        thermochemistry_burnup,
        constituent_formula_units,
        labels=labels,
        colors=colors,
        alpha=0.88,
    )
    axis.set_xlabel(BURNUP_LABEL)
    axis.set_ylabel("Liquid constituent formula-unit inventory (mol/m3)")
    axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5))
    save_figure(fig, case_plot_dir / "liquid_constituent_formula_units.png", saved_paths)


def plot_radial_profiles(
    case_directories: list[Path],
    saved_paths: list[Path],
    gb_color_map: dict[str, object],
) -> None:
    radii_mm: list[float] = []
    output_histories: list[dict[str, np.ndarray]] = []
    thermo_histories: list[dict[str, np.ndarray]] = []

    for case_dir in case_directories:
        headers, values = load_output_data(case_dir / MAIN_OUTPUT_NAME)
        thermo_headers, thermo_values = load_output_data(case_dir / THERMO_OUTPUT_NAME)
        radii_mm.append(parse_radius_mm(case_dir))
        output_histories.append({header: values[:, index] for index, header in enumerate(headers)})
        thermo_histories.append({header: thermo_values[:, index] for index, header in enumerate(thermo_headers)})

    radii_mm_array = np.array(radii_mm, dtype=float)
    order = np.argsort(radii_mm_array)
    radii_mm_array = radii_mm_array[order]
    radii_m_array = radii_mm_array * 1.0e-3
    output_histories = [output_histories[index] for index in order]
    thermo_histories = [thermo_histories[index] for index in order]

    reference_time = output_histories[0]["Time (h)"]
    reference_burnup = output_histories[0]["Burnup (MWd/kgUO2)"]
    reference_fima = output_histories[0]["FIMA (%)"]

    output_profiles: dict[str, np.ndarray] = {}
    for name in output_histories[0]:
        aligned_series = []
        for case_history in output_histories:
            aligned_series.append(np.interp(reference_time, case_history["Time (h)"], case_history[name]))
        output_profiles[name] = np.vstack(aligned_series)

    thermo_profiles: dict[str, np.ndarray] = {}
    for name in thermo_histories[0]:
        aligned_series = []
        for case_history in thermo_histories:
            aligned_series.append(np.interp(reference_time, case_history["Time (h)"], case_history[name]))
        thermo_profiles[name] = np.vstack(aligned_series)

    snapshot_targets = np.linspace(
        float(np.nanmin(reference_burnup)),
        float(np.nanmax(reference_burnup)),
        6,
    )
    indexes: list[int] = []
    for target in snapshot_targets:
        index = int(np.argmin(np.abs(reference_burnup - target)))
        if index not in indexes:
            indexes.append(index)
    if indexes[-1] != len(reference_time) - 1:
        indexes.append(len(reference_time) - 1)
    snapshot_colors = plt.cm.viridis(np.linspace(0, 1, len(indexes)))

    # 1) Temperature (K) vs radius (mm)
    if "Temperature (K)" in output_profiles:
        fig, axis = plt.subplots()
        for color, index in zip(snapshot_colors, indexes):
            axis.plot(
                radii_mm_array,
                output_profiles["Temperature (K)"][:, index],
                color=color,
                marker="o",
                label=f"{reference_burnup[index]:.1f} MWd/kgUO2",
            )
        axis.set_xlabel("Radius (mm)")
        axis.set_xlim([0, 3.0])
        axis.set_ylabel("Temperature (K)")
        axis.set_ylim([800, 2000])
        axis.legend(loc="best")
        save_figure(fig, PLOTS_DIR / "summary_01_temperature_vs_radius.png", saved_paths)

    # 2) System pressure (bar) vs burnup
    if "System pressure (Pa)" in output_profiles:
        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history["Burnup (MWd/kgUO2)"]
            pressure_bar = case_history["System pressure (Pa)"] * 1.0e-5
            axis.plot(case_burnup, pressure_bar, label=f"r = {radius_mm:.3f} mm", alpha=0.9)
        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylim([0,100])
        axis.set_ylabel("System pressure (bar)")
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "summary_02_pressure_vs_burnup.png", saved_paths)

    # 3) Stoichiometry deviation vs burnup
    if "Stoichiometry deviation (/)" in output_profiles:
        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history["Burnup (MWd/kgUO2)"]
            axis.plot(
                case_burnup,
                2.0 + case_history["Stoichiometry deviation (/)"],
                label=f"r = {radius_mm:.1f} mm",
                alpha=0.9,
            )
        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel("O/M (/)")
        axis.legend(loc="lower right")
        save_figure(fig, PLOTS_DIR / "summary_03_stoichiometry_vs_burnup.png", saved_paths)

        fig, axis = plt.subplots()
        for color, index in zip(snapshot_colors, indexes):
            axis.plot(
                radii_mm_array,
                2.0 + output_profiles["Stoichiometry deviation (/)"][:, index],
                color=color,
                marker="o",
                label=f"{reference_burnup[index]:.1f} MWd/kgUO2",
            )
        axis.set_xlabel("Radius (mm)")
        axis.set_xlim([0, 3.0])
        axis.set_ylabel("O/M (/)")
        axis.legend(loc="lower right")
        save_figure(fig, PLOTS_DIR / "summary_03bis_stoichiometry_vs_radius.png", saved_paths)

    if "Fission gas release (/)" in output_profiles:
        fgr_radial_average = radial_volume_average(
            output_profiles["Fission gas release (/)"],
            radii_m_array,
        )

        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history["Burnup (MWd/kgUO2)"]
            axis.plot(
                case_burnup,
                case_history["Fission gas release (/)"],
                color="#9ca3af",
                linewidth=0.8,
                alpha=0.45,
            )
        axis.plot(
            reference_burnup,
            fgr_radial_average,
            color="#111827",
            linewidth=2.5,
            label="Radial average",
        )
        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel("FGR (/)")
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "summary_03c_fgr_radial_average_vs_burnup.png", saved_paths)

    # 3b) q vs burnup
    if "q (-)" in output_profiles:
        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history["Burnup (MWd/kgUO2)"]
            axis.plot(
                case_burnup,
                case_history["q (-)"],
                label=f"r = {radius_mm:.1f} mm",
                alpha=0.9,
            )
        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel("q (-)")
        axis.set_ylim(0.20, 0.24)
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "summary_03b_q_vs_burnup.png", saved_paths)

    # 4) Chemical potential vs burnup
    if "Fuel oxygen potential (KJ/mol)" in output_profiles:
        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history["Burnup (MWd/kgUO2)"]
            axis.plot(
                case_burnup,
                case_history["Fuel oxygen potential (KJ/mol)"],
                label=f"r = {radius_mm:.1f} mm",
                alpha=0.9,
            )
        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel("Fuel oxygen potential (kJ/mol)")
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "summary_04_chemical_potential_vs_burnup.png", saved_paths)

        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history["Burnup (MWd/kgUO2)"]
            axis.plot(
                case_burnup,
                case_history["Fuel oxygen potential (KJ/mol)"] / 2.0,
                label=f"r = {radius_mm:.1f} mm",
                alpha=0.9,
            )
        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel("Chemical potential O (kJ/mol)")
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "summary_04bis_chemical_potential_vs_burnup.png", saved_paths)

    gb_variables = [
        header
        for header in thermo_profiles
        if is_grain_boundary_amount_column(header)
        and not is_all_zero(thermo_profiles[header])
    ]
    gb_sorted_variables = sorted(
        gb_variables,
        key=lambda variable: radial_volume_average(thermo_profiles[variable], radii_m_array)[-1],
        reverse=True,
    )
    species_colors: dict[str, object] = {}

    # 5-6) Grain-boundary phases: species-specific color + phase-specific hatch style.
    if gb_sorted_variables:
        species_colors = build_species_color_map(gb_sorted_variables)
        summary_entries: list[tuple[str, np.ndarray]] = []
        for variable in gb_sorted_variables:
            species = grain_boundary_species(variable)
            phase = grain_boundary_phase(variable)
            if phase not in {"condensed", "liquid"}:
                continue
            if is_liquid_total_column(variable):
                continue
            series = radial_volume_average(thermo_profiles[variable], radii_m_array)
            summary_entries.append((f"{species}", series))

        liquid_constituent_variables = [
            variable for variable in thermo_profiles
            if is_liquid_constituent_column(variable)
            and not is_all_zero(thermo_profiles[variable])
        ]
        if liquid_constituent_variables:
            liquid_formula_series = [
                radial_volume_average(thermo_profiles[variable], radii_m_array)
                for variable in liquid_constituent_variables
            ]
            liquid_atom_equivalent_series = [
                radial_volume_average(
                    thermo_profiles[variable] * liquid_constituent_stoichiometric_size(variable),
                    radii_m_array,
                )
                for variable in liquid_constituent_variables
            ]
            liquid_series = liquid_formula_series
            if LIQUID_TOTAL_COLUMN in thermo_profiles:
                liquid_total_series = radial_volume_average(thermo_profiles[LIQUID_TOTAL_COLUMN], radii_m_array)
                total_norm = np.linalg.norm(liquid_total_series)
                if total_norm > 0.0:
                    raw_error = np.linalg.norm(np.sum(np.vstack(liquid_formula_series), axis=0) - liquid_total_series) / total_norm
                    atom_equivalent_error = np.linalg.norm(np.sum(np.vstack(liquid_atom_equivalent_series), axis=0) - liquid_total_series) / total_norm
                    if atom_equivalent_error < raw_error:
                        liquid_series = liquid_atom_equivalent_series

            liquid_entries = [
                (grain_boundary_species(variable), series)
                for variable, series in zip(liquid_constituent_variables, liquid_series)
                if not is_all_zero(series)
            ]
            liquid_entries.sort(key=lambda item: item[1][-1], reverse=True)
            for species, series in liquid_entries:
                summary_entries.append((f"Liquid: {species}", series))

        fig, axis = plt.subplots()
        gb_labels = [label for label, _ in summary_entries]
        gb_radial_histories = [series for _, series in summary_entries]
        summary_colors = assign_distinct_colors(gb_labels)
        gb_colors = [summary_colors[label] for label in gb_labels]

        if gb_radial_histories:
            axis.stackplot(
                reference_burnup,
                gb_radial_histories,
                colors=gb_colors,
                labels=gb_labels,
                alpha=0.9,
            )

            cumulative_histories = np.cumsum(np.vstack(gb_radial_histories), axis=0)
            for boundary in cumulative_histories:
                axis.plot(reference_burnup, boundary, color="#111827", linewidth=0.25, alpha=0.40)

        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel("Concentration at grain boundary (mol/m3)")
        add_capped_legend(axis, loc="upper left")
        save_figure(fig, PLOTS_DIR / "summary_06_gb_noliq_phases_vs_burnup.png", saved_paths)
    
    if "JOG (/)" in output_profiles:
        jog_total_thickness_over_time_um = radial_integral_over_radius(
            output_profiles["JOG (/)"],
            radii_m_array,
        ) * 1.0e6

        jog_liquid_thickness_over_time_um = None
        if "JOG from liquid (/)" in output_profiles:
            jog_liquid_thickness_over_time_um = radial_integral_over_radius(
                output_profiles["JOG from liquid (/)"],
                radii_m_array,
            ) * 1.0e6
        melis_fima, melis_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Melis1993.txt")
        tourasse_fima, tourasse_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Tourasse1992.txt")

        def fima_to_burnup(fima_values: np.ndarray) -> np.ndarray:
            return np.interp(fima_values, reference_fima, reference_burnup)

        condensed_contribution_columns = [
            ("CS2MOO4_S2", "JOG from CS2MOO4_S2 (/)"),
            ("CS2MOO4_S1", "JOG from CS2MOO4_S1 (/)"),
            ("MOO2", "JOG from MOO2 (/)"),
            ("CS2MO3O10", "JOG from CS2MO3O10 (/)"),
            ("CS2MO4O13", "JOG from CS2MO4O13 (/)"),
            ("BCC_A2", "JOG from BCC_A2 (/)"),
            ("FCC_A1", "JOG from FCC_A1 (/)"),
            ("HCP_A3", "JOG from HCP_A3 (/)"),
        ]
        condensed_entries: list[tuple[str, np.ndarray, object]] = []
        for index, (label, column_name) in enumerate(condensed_contribution_columns):
            if column_name not in output_profiles:
                continue
            series = radial_integral_over_radius(output_profiles[column_name], radii_m_array) * 1.0e6
            if is_all_zero(series):
                continue
            condensed_entries.append((label, series, species_colors.get(label, plt.cm.tab20(index / max(1, len(condensed_contribution_columns))))))

        # Put CS2MOO4_S2 at the base inside the condensed stack ordering.
        condensed_entries.sort(key=lambda item: (item[0] != "CS2MOO4_S2", item[0]))
        gb_labels = []
        gb_radial_histories = []
        gb_colors = []

        gb_labels.extend(item[0] for item in condensed_entries)
        gb_radial_histories.extend(item[1] for item in condensed_entries)
        gb_colors.extend(item[2] for item in condensed_entries)

        # Keep liquid at the base of the stack when available.
        if jog_liquid_thickness_over_time_um is not None and not is_all_zero(jog_liquid_thickness_over_time_um):
            gb_labels.append("LIQUID")
            gb_radial_histories.append(jog_liquid_thickness_over_time_um)
            gb_colors.append("#f97316")

        fig, axis = plt.subplots()
        if gb_radial_histories:
            axis.stackplot(
                reference_burnup,
                *gb_radial_histories,
                colors=gb_colors,
                labels=gb_labels,
                alpha=0.9,
            )
            cumulative_histories = np.cumsum(np.vstack(gb_radial_histories), axis=0)
            for boundary in cumulative_histories:
                axis.plot(reference_burnup, boundary, color="#111827", linewidth=0.25, alpha=0.40)

        axis.plot(reference_burnup, jog_total_thickness_over_time_um, color="#111827", label="Total")
        axis.scatter(
            fima_to_burnup(melis_fima),
            melis_thickness,
            edgecolors="black", facecolor=None,
            marker="o",
            label="Melis et al. (1993)",
            zorder=3,
        )
        axis.scatter(
            fima_to_burnup(tourasse_fima),
            tourasse_thickness,
            edgecolors="black", facecolor=None,
            marker="D",
            label="Tourasse et al. (1992)",
            zorder=3,
        )
        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel("JOG thickness (um)")
        axis.legend(loc="upper left")
        axis.set_ylim([0,200])

        save_figure(fig, PLOTS_DIR / "summary_08_jog_contributions_and_experiments.png", saved_paths)

        outer_indices = [
            index for index, radius_mm in enumerate(radii_mm_array)
            if radius_mm >=  2.3
        ]
        if len(outer_indices) >= 2:
            outer_jog_total_thickness_over_time_um = radial_integral_masked_to_full_radius(
                output_profiles["JOG (/)"],
                radii_m_array,
                outer_indices,
            ) * 1.0e6

            outer_jog_liquid_thickness_over_time_um = None
            if "JOG from liquid (/)" in output_profiles:
                outer_jog_liquid_thickness_over_time_um = radial_integral_masked_to_full_radius(
                    output_profiles["JOG from liquid (/)"],
                    radii_m_array,
                    outer_indices,
                ) * 1.0e6

            outer_entries: list[tuple[str, np.ndarray, object]] = []
            for index, (label, column_name) in enumerate(condensed_contribution_columns):
                if column_name not in output_profiles:
                    continue
                series = radial_integral_masked_to_full_radius(
                    output_profiles[column_name],
                    radii_m_array,
                    outer_indices,
                ) * 1.0e6
                if is_all_zero(series):
                    continue
                outer_entries.append((
                    label,
                    series,
                    species_colors.get(label, plt.cm.tab20(index / max(1, len(condensed_contribution_columns)))),
                ))

            outer_entries.sort(key=lambda item: (item[0] != "CS2MOO4_S2", item[0]))
            outer_labels = [item[0] for item in outer_entries]
            outer_histories = [item[1] for item in outer_entries]
            outer_colors = [item[2] for item in outer_entries]

            if outer_jog_liquid_thickness_over_time_um is not None and not is_all_zero(outer_jog_liquid_thickness_over_time_um):
                outer_labels.append("LIQUID")
                outer_histories.append(outer_jog_liquid_thickness_over_time_um)
                outer_colors.append("#f97316")

            fig, axis = plt.subplots()
            if outer_histories:
                axis.stackplot(
                    reference_burnup,
                    *outer_histories,
                    colors=outer_colors,
                    labels=outer_labels,
                    alpha=0.9,
                )
                cumulative_histories = np.cumsum(np.vstack(outer_histories), axis=0)
                for boundary in cumulative_histories:
                    axis.plot(reference_burnup, boundary, color="#111827", linewidth=0.25, alpha=0.40)

            axis.plot(reference_burnup, outer_jog_total_thickness_over_time_um, color="#111827", label="Total")
            axis.scatter(
                fima_to_burnup(melis_fima),
                melis_thickness,
                edgecolors="black", facecolor=None,
                marker="o",
                label="Melis et al. (1993)",
                zorder=3,
            )
            axis.scatter(
                fima_to_burnup(tourasse_fima),
                tourasse_thickness,
                edgecolors="black", facecolor=None,
                marker="D",
                label="Tourasse et al. (1992)",
                zorder=3,
            )
            axis.set_xlabel("Burnup (MWd/kgUO2)")
            axis.set_ylabel("JOG thickness (um)")
            axis.legend(loc="upper left")
            axis.set_ylim([0, 200])
            save_figure(fig, PLOTS_DIR / "summary_08_jog_contributions_and_experiments_radii_18_20.png", saved_paths)

def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--plot-only",
        action="store_true",
        help="Skip the SCIANTIX runs and regenerate plots from existing point outputs.",
    )
    args = parser.parse_args()

    case_directories = case_dirs()
    if not case_directories:
        raise FileNotFoundError(f"No point_* directories found in {TEST_DIR}")

    PLOTS_DIR.mkdir(exist_ok=True)
    saved_paths: list[Path] = []
    case_results: list[tuple[Path, int]] = []

    if not args.plot_only:
        ensure_executable(BUILD_EXECUTABLE)
        delete_file_if_exists(RUN_SUMMARY)
        for case_dir in case_directories:
            cleanup_case_directory(case_dir)
            prepare_case_inputs(case_dir)
            completed = run_sciantix_case(case_dir)
            RUN_LOG_case = case_dir / RUN_LOG
            RUN_LOG_case.write_text(completed.stdout + completed.stderr)
            case_results.append((case_dir, completed.returncode))
            if completed.returncode != 0:
                cleanup_case_directory(case_dir)
                raise RuntimeError(f"SCIANTIX failed for {case_dir}")
            cleanup_case_directory(case_dir)

        summary_lines = ["SUPERFACT batch run summary", ""]
        for case_dir, returncode in case_results:
            summary_lines.append(f"{case_dir.name}: returncode={returncode}")
        RUN_SUMMARY.write_text("\n".join(summary_lines))
    else:
        for case_dir in case_directories:
            ensure_output_file(case_dir / MAIN_OUTPUT_NAME)
            ensure_output_file(case_dir / THERMO_OUTPUT_NAME)

    gb_color_map = build_thermochemistry_color_map(case_directories)

    for case_dir in case_directories:
        plot_case(case_dir, saved_paths, gb_color_map)

    plot_radial_profiles(case_directories, saved_paths, gb_color_map)

    print("Generated plots:")
    for path in saved_paths:
        print(path)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
