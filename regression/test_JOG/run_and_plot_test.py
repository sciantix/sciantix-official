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
THERMOCHEMISTRY_MANIFEST_FILE = TEST_DIR /"input_thermochemistry.txt"
PLOTS_DIR = TEST_DIR / "plots"
EXP_DATA_DIR = TEST_DIR / "exp_data"
PELLET_RADIUS_M = 2.719e-3
AVOGADRO_NUMBER = 6.02214076e23
CS2MOO4_LIQUID_DENSITY_KG_PER_M3 = 4380.0

ATOMIC_MASS_G_PER_MOL = {
    "Cs": 132.90545196,
    "Mo": 95.95,
    "O": 15.999,
    "U": 238.02891,
}
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

colors = ["#ff0000", "#ff7f00", "#00c853", "#2962ff", "#aa00ff"]
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


def load_grain_boundary_uranium_species(manifest_file: Path) -> set[str]:
    uranium_species: set[str] = set()
    known_elements = {"Cs", "Mo", "O", "U", "Pu", "I", "Te", "Cr", "Va"}

    def extract_elements_from_compound(compound: str) -> set[str]:
        elements: set[str] = set()
        index = 0
        while index < len(compound):
            if not compound[index].isalpha():
                index += 1
                continue

            matched = False
            for size in (2, 1):
                token = compound[index:index + size]
                if token in known_elements:
                    elements.add(token)
                    index += size
                    matched = True
                    break

            if not matched:
                return set()

        return elements

    with manifest_file.open() as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line or line.startswith("#"):
                continue

            parts = [part.strip() for part in line.split("|")]
            if len(parts) < 7:
                continue

            category = parts[1]
            compound = parts[3]
            location = parts[4]
            output_flag = parts[6]

            if category != "fission_products" or location != "at grain boundary" or output_flag != "1":
                continue

            elements = extract_elements_from_compound(compound)
            if ("U" in elements) and ("Mo" not in elements) and ("Cs" not in elements):
                uranium_species.add(compound)

    return uranium_species


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


def has_columns(column_map: dict[str, int], labels: list[str]) -> bool:
    return all(label in column_map for label in labels)


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


def hollow_geometry_from_case_directories(case_directories: list[Path]) -> tuple[float, float, float]:
    radii_mm = sorted(parse_radius_mm(case_dir) for case_dir in case_directories)
    if len(radii_mm) < 2:
        raise ValueError("At least two radial points are required to define hollow geometry.")
    r_inner_m = radii_mm[0] * 1.0e-3
    r_outer_m = radii_mm[-1] * 1.0e-3
    half_wall_thickness_m = 0.5 * (r_outer_m - r_inner_m)
    if half_wall_thickness_m <= 0.0:
        raise ValueError("Invalid radial geometry: outer radius must be larger than inner radius.")
    return r_inner_m, r_outer_m, half_wall_thickness_m


def case_dirs() -> list[Path]:
    return sorted(path for path in TEST_DIR.glob("point_*") if path.is_dir())


def parse_radius_mm(case_dir: Path) -> float:
    match = re.search(r"_r_(\d+p\d+)mm$", case_dir.name)
    if not match:
        raise ValueError(f"Could not parse radius from case directory name: {case_dir.name}")
    return float(match.group(1).replace("p", "."))


def case_label(case_dir: Path) -> str:
    return f"{parse_radius_mm(case_dir):.4f} mm"


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


def build_thermochemistry_color_maps(case_directories: list[Path]) -> tuple[dict[str, object], dict[str, object]]:
    uranium_species = load_grain_boundary_uranium_species(THERMOCHEMISTRY_MANIFEST_FILE)
    gb_labels: list[str] = []
    matrix_labels: list[str] = []

    for case_dir in case_directories:
        thermo_file = case_dir / THERMO_OUTPUT_NAME
        ensure_output_file(thermo_file)
        thermochemistry_headers, _ = load_output_data(thermo_file)

        gb_labels.extend(
            header
            for header in thermochemistry_headers
            if header not in {"Burnup (MWd/kgUO2)", "Time (h)"}
            and ", at grain boundary)" in header
            and header.split(" (", 1)[0] not in uranium_species
        )
        matrix_labels.extend(
            header
            for header in thermochemistry_headers
            if header not in {"Burnup (MWd/kgUO2)", "Time (h)"}
            and ", matrix)" in header
        )

    return build_label_color_map(gb_labels), build_label_color_map(matrix_labels)


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
    matrix_color_map: dict[str, object],
    half_wall_thickness_m: float,
    r_inner_m: float,
    r_outer_m: float,
) -> None:
    output_file = case_dir / MAIN_OUTPUT_NAME
    thermo_file = case_dir / THERMO_OUTPUT_NAME
    ensure_output_file(output_file)
    ensure_output_file(thermo_file)

    headers, values = load_output_data(output_file)
    column_map = {name: idx for idx, name in enumerate(headers)}
    thermochemistry_headers, thermochemistry_values = load_output_data(thermo_file)
    thermochemistry_column_map = {name: idx for idx, name in enumerate(thermochemistry_headers)}

    burnup_label = "Burnup (MWd/kgUO2)"
    time_label = "Time (h)"
    fima_label = "FIMA (%)"
    jog_label = "JOG from condensed (/)"
    oxygen_label = "O content (mol/m3)"
    uranium_label = "U content (mol/m3)"

    burnup = values[:, column_map[burnup_label]]
    time = values[:, column_map[time_label]]
    fima = values[:, column_map[fima_label]]
    jog_fraction = values[:, column_map[jog_label]]
    jog_thickness_um = jog_fraction * half_wall_thickness_m * 1.0e6
    
    def burnup_to_time(x):
        return np.interp(x, burnup, time)

    def time_to_burnup(x):
        return np.interp(x, time, burnup)

    thermochemistry_time = thermochemistry_values[:, thermochemistry_column_map[time_label]]
    thermochemistry_burnup = np.interp(thermochemistry_time, time, burnup)

    case_plot_dir = PLOTS_DIR / case_dir.name
    case_plot_dir.mkdir(parents=True, exist_ok=True)

    fig, axes = plt.subplots(3, 2, figsize=(12, 14))
    axes = axes.flatten()

    axis = axes[0]
    axis.plot(burnup, values[:, column_map[oxygen_label]], color=colors[0], label="Total")
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Oxygen concentration (mol/m3)")
    axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
    axis.legend(loc="upper left")

    axis = axes[1]
    axis.plot(burnup, values[:, column_map[uranium_label]], color=colors[0], label="Total")
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Uranium concentration (mol/m3)")
    axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
    axis.legend(loc="upper left")

    for axis, species, ylabel in [
        (axes[2], "Xe", "Xenon concentration (mol/m3)"),
        (axes[3], "Kr", "Krypton concentration (mol/m3)"),
        (axes[4], "Cs", "Cesium concentration (mol/m3)"),
        (axes[5], "Mo", "Molybdenum concentration (mol/m3)"),
    ]:
        for label, color, suffix in [
            ("Produced", colors[0], " produced (at/m3)"),
            ("In grain", colors[1], " in grain (at/m3)"),
            ("At grain boundary", colors[2], " at grain boundary (at/m3)"),
            ("Reacted", colors[3], " reacted - GB (at/m3)"),
            ("Released", colors[4], " released (at/m3)"),
        ]:
            column_name = f"{species}{suffix}"
            if column_name in column_map:
                axis.plot(burnup, values[:, column_map[column_name]] / AVOGADRO_NUMBER, color=color, label=label)
        axis.set_xlabel(burnup_label)
        axis.set_ylabel(ylabel)
        axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="upper left")

    fig.tight_layout()
    plot_path = case_plot_dir / "inventory_mol_m3_overview.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    summary_variables = [
        "Temperature (K)",
        "Fission gas release (/)",
        "Stoichiometry deviation (/)"
    ]
    available_summary_variables = [name for name in summary_variables if name in column_map]
    for page_index, start in enumerate(range(0, len(available_summary_variables), 4), start=1):
        page_variables = available_summary_variables[start:start + 4]
        fig, axes = plt.subplots(len(page_variables), 1, figsize=(10, 3.2 * len(page_variables)), sharex=True)
        axes = np.atleast_1d(axes)
        for axis, variable in zip(axes, page_variables):
            axis.plot(time, values[:, column_map[variable]], color="#0f766e")
            axis.set_ylabel(variable)
            axis.grid(True, alpha=0.3)
        axes[-1].set_xlabel(time_label)
        fig.tight_layout()
        plot_path = case_plot_dir / f"summary_variables_{page_index:02d}.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

    fig, axis = plt.subplots()
    p_o2_oc = np.log10(values[:, column_map["Fuel oxygen partial pressure - CALPHAD (MPa)"]] / 0.1)
    p_o2_bb = np.log10(values[:, column_map["Fuel oxygen partial pressure - Blackburn (MPa)"]] / 0.1)
    p_o2_final = np.log10(values[:, column_map["Fuel oxygen partial pressure (MPa)"]] / 0.1)
    axis.plot(burnup, p_o2_final, label="SCIANTIX", color=colors[0])
    axis.plot(burnup, p_o2_bb, label="Blackburn", color=colors[1], linestyle = '--')
    axis.plot(burnup, p_o2_oc, label="CALPHAD", color=colors[3], linestyle = '--')
    axis.set_xlabel(burnup_label)
    axis.set_ylabel((r"$\log_{10}(p_{O_2})$ (bar)"))
    axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
    axis.legend(loc="best")
    fig.tight_layout()
    plot_path = case_plot_dir / "po2.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    fig, axis = plt.subplots()
    axis.plot(burnup, values[:, column_map["Fuel oxygen potential (KJ/mol)"]], label="SCIANTIX", color=colors[0])
    axis.plot(burnup, values[:, column_map["Fuel oxygen potential - Blackburn (KJ/mol)"]], label="Blackburn", color=colors[1], linestyle = '--')
    axis.plot(burnup, values[:, column_map["Fuel oxygen potential - CALPHAD (KJ/mol)"]], label="CALPHAD", color=colors[3], linestyle = '--')
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("muO2 (kJ/mol)")
    axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
    axis.legend(loc="best")
    fig.tight_layout()
    plot_path = case_plot_dir / "muo2.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    melis_fima, melis_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Melis1993.txt")
    tourasse_fima, tourasse_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Tourasse1992.txt")
    fig, axis = plt.subplots()
    axis.plot(fima, jog_thickness_um, color=colors[3], label="SCIANTIX")
    axis.scatter(melis_fima, melis_thickness, edgecolors="black", facecolor=None, marker="o", label="Melis et al. (1993)", zorder=3)
    axis.scatter(tourasse_fima, tourasse_thickness, edgecolors="black", facecolor=None, marker="D", label="Tourasse et al. (1992)", zorder=3)
    axis.set_xlabel(fima_label)
    axis.set_ylabel("JOG thickness (um)")
    axis.text(
        0.98,
        0.98,
        "Thickness = JOG (/) * (Rout - Rin)/2\n"
        f"Rin = {r_inner_m * 1.0e3:.3f} mm, Rout = {r_outer_m * 1.0e3:.3f} mm",
        transform=axis.transAxes,
        ha="right",
        va="top",
    )
    axis.legend(loc="upper left")
    fig.tight_layout()
    plot_path = case_plot_dir / "JOG_vs_fima.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    jog_breakdown_labels = {
        "condensed": "JOG from condensed (/)"
    }
    jog_condensed_subvariables = [
        ("CS2MOO4_S2", "JOG from CS2MOO4_S2 (/)", "CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)"),
        ("CS2MOO4_S1", "JOG from CS2MOO4_S1 (/)", "CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)"),
        ("MOO2", "JOG from MOO2 (/)", "MOO2 (condensed, at grain boundary) (mol/m3)"),
        ("CS2MO3O10", "JOG from CS2MO3O10 (/)", "CS2MO3O10 (condensed, at grain boundary) (mol/m3)"),
        ("CS2MO4O13", "JOG from CS2MO4O13 (/)", "CS2MO4O13 (condensed, at grain boundary) (mol/m3)"),
        ("BCC_A2", "JOG from BCC_A2 (/)", "BCC_A2 (condensed, at grain boundary) (mol/m3)"),
        ("FCC_A1", "JOG from FCC_A1 (/)", "FCC_A1 (condensed, at grain boundary) (mol/m3)"),
        ("HCP_A3", "JOG from HCP_A3 (/)", "HCP_A3 (condensed, at grain boundary) (mol/m3)"),
    ]

    if has_columns(column_map, [jog_breakdown_labels["condensed"]]):
        stack_series = []
        stack_labels = []
        stack_colors = []
        for sub_label, output_label, thermo_label in jog_condensed_subvariables:
            if output_label not in column_map:
                continue
            stack_series.append(values[:, column_map[output_label]] * half_wall_thickness_m * 1.0e6)
            stack_labels.append(sub_label)
            stack_colors.append(gb_color_map.get(thermo_label))

        fig, axis = plt.subplots()
        axis.stackplot(
            fima,
            *stack_series,
            labels=stack_labels,
            colors=stack_colors,
            alpha=0.8,
        )
        axis.plot(fima, jog_thickness_um, color="#111827", label="Total JOG", linewidth=2.3)
        axis.set_ylabel("JOG thickness (um)")
        axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
        axis.set_xlabel(fima_label)
        axis.legend(loc="upper left")

        fig.tight_layout()
        plot_path = case_plot_dir / "JOG_contributions.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

    uranium_species = load_grain_boundary_uranium_species(THERMOCHEMISTRY_MANIFEST_FILE)

    gb_variables = [
        header
        for header in thermochemistry_headers
        if header not in {burnup_label, time_label}
        and ", at grain boundary)" in header
        and header.split(" (", 1)[0] not in uranium_species
        and not is_all_zero(thermochemistry_values[:, thermochemistry_column_map[header]])
    ]
    gb_sorted_variables = sorted(
        gb_variables,
        key=lambda variable: thermochemistry_values[-1, thermochemistry_column_map[variable]],
        reverse=True,
    )
    fig, axis = plt.subplots()
    gb_stacked_data = [thermochemistry_values[:, thermochemistry_column_map[variable]] for variable in gb_sorted_variables]
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
            species = variable.split(" (", 1)[0]
            match = re.search(r"\(([^,]+), at grain boundary\)", variable)
            phase = match.group(1).strip().lower() if match else "unknown"
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
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Concentration at grain boundary (mol/m3)")
    add_capped_legend(axis, loc="upper left", fontsize=8)
    fig.tight_layout()
    plot_path = case_plot_dir / "thermochemistry_fission_products_at_grain_boundary.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    liquid_phase_label = "LIQUID (liquid, at grain boundary) (mol/m3)"
    if liquid_phase_label in thermochemistry_column_map:
        liquid_cs2moo4_available = thermochemistry_values[:, thermochemistry_column_map[liquid_phase_label]]
    else:
        cs = np.zeros_like(thermochemistry_burnup)
        mo = np.zeros_like(thermochemistry_burnup)
        o = np.zeros_like(thermochemistry_burnup)
        liquid_cs_label = "CS (liquid, at grain boundary) (mol/m3)"
        if liquid_cs_label in thermochemistry_column_map:
            cs = thermochemistry_values[:, thermochemistry_column_map[liquid_cs_label]]
        liquid_mo_label = "MO (liquid, at grain boundary) (mol/m3)"
        if liquid_mo_label in thermochemistry_column_map:
            mo = thermochemistry_values[:, thermochemistry_column_map[liquid_mo_label]]
        liquid_o_label = "O (liquid, at grain boundary) (mol/m3)"
        if liquid_o_label in thermochemistry_column_map:
            o = thermochemistry_values[:, thermochemistry_column_map[liquid_o_label]]
        liquid_cs2moo4_available = np.minimum.reduce([cs / 2.0, mo, o / 4.0])
    cs2moo4_molar_mass = (
        2.0 * ATOMIC_MASS_G_PER_MOL["Cs"] + ATOMIC_MASS_G_PER_MOL["Mo"] + 4.0 * ATOMIC_MASS_G_PER_MOL["O"]
    )
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
    liquid_cs2moo4_volume_fraction = liquid_cs2moo4_available * cs2moo4_molar_mass / (CS2MOO4_LIQUID_DENSITY_KG_PER_M3 * 1000.0)
    condensed_cs2moo4_s1_volume_fraction = np.zeros_like(thermochemistry_burnup)
    condensed_cs2moo4_s1_label = "CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)"
    if condensed_cs2moo4_s1_label in thermochemistry_column_map:
        condensed_cs2moo4_s1 = thermochemistry_values[:, thermochemistry_column_map[condensed_cs2moo4_s1_label]]
        condensed_cs2moo4_s1_volume_fraction = condensed_cs2moo4_s1 * cs2moo4_molar_mass / condensed_s1_density_g_per_m3
    condensed_cs2moo4_s2_volume_fraction = np.zeros_like(thermochemistry_burnup)
    condensed_cs2moo4_s2_label = "CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)"
    if condensed_cs2moo4_s2_label in thermochemistry_column_map:
        condensed_cs2moo4_s2 = thermochemistry_values[:, thermochemistry_column_map[condensed_cs2moo4_s2_label]]
        condensed_cs2moo4_s2_volume_fraction = condensed_cs2moo4_s2 * cs2moo4_molar_mass / condensed_s2_density_g_per_m3
    condensed_cs2moo4_volume_fraction = condensed_cs2moo4_s1_volume_fraction + condensed_cs2moo4_s2_volume_fraction
    thermochemistry_jog_thickness_um = (
        liquid_cs2moo4_volume_fraction + condensed_cs2moo4_volume_fraction
    ) * half_wall_thickness_m * 1.0e6
    liquid_thickness_um = liquid_cs2moo4_volume_fraction * half_wall_thickness_m * 1.0e6
    condensed_thickness_um = condensed_cs2moo4_volume_fraction * half_wall_thickness_m * 1.0e6

    fig, axis = plt.subplots()
    axis.stackplot(
        fima,
        liquid_thickness_um,
        condensed_thickness_um,
        labels=["Available Cs2MoO4", "CALPHAD Cs2MoO4"],
        colors=["#fb923c", "#7c3aed"],
        alpha=0.75,
    )
    axis.plot(fima, thermochemistry_jog_thickness_um, color="#111827", label="Total estimated thickness")
    axis.scatter(melis_fima, melis_thickness, edgecolors="black", facecolor=None, marker="o", label="Melis et al. (1993)", zorder=3)
    axis.scatter(tourasse_fima, tourasse_thickness, edgecolors="black", facecolor=None, marker="D", label="Tourasse et al. (1992)", zorder=3)
    axis.set_xlabel(fima_label)
    axis.set_ylabel("JOG thickness (um)")
    axis.legend(loc="upper left")
    fig.tight_layout()
    plot_path = case_plot_dir / "JOG_thickness_from_thermochemistry.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

def plot_radial_profiles(
    case_directories: list[Path],
    saved_paths: list[Path],
    gb_color_map: dict[str, object],
) -> None:
    radii_mm: list[float] = []
    output_histories: list[dict[str, np.ndarray]] = []
    thermo_histories: list[dict[str, np.ndarray]] = []
    uranium_species = load_grain_boundary_uranium_species(THERMOCHEMISTRY_MANIFEST_FILE)

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
                marker="o")
            break
        axis.set_xlabel("Radius (mm)")
        axis.set_xlim([0, 3.0])
        axis.set_ylabel("Temperature (K)")
        axis.set_ylim([800, 2000])
        axis.legend(loc="best")
        fig.tight_layout()
        plot_path = PLOTS_DIR / "summary_01_temperature_vs_radius.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

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
        fig.tight_layout()
        plot_path = PLOTS_DIR / "summary_02_pressure_vs_burnup.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

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
        axis.set_ylim([1.95, 2.3])
        axis.legend(loc="upper left")
        fig.tight_layout()
        plot_path = PLOTS_DIR / "summary_03_stoichiometry_vs_burnup.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

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
        axis.set_ylim([-800, 0.0])
        axis.legend(loc="upper left")
        fig.tight_layout()
        plot_path = PLOTS_DIR / "summary_04_chemical_potential_vs_burnup.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

    gb_variables = [
        header
        for header in thermo_profiles
        if header not in {"Burnup (MWd/kgUO2)", "Time (h)"}
        and ", at grain boundary)" in header
        and header.split(" (", 1)[0] not in uranium_species
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
        phase_hatch = {
            "liquid": "///",
            "gas": "...",
            "condensed": "xx",
            "unknown": "\\\\\\",
        }
        fig, axis = plt.subplots()
        gb_radial_histories = []
        gb_colors = []
        gb_hatches = []
        gb_labels = []
        for variable in gb_sorted_variables:
            species = variable.split(" (", 1)[0]
            match = re.search(r"\(([^,]+), at grain boundary\)", variable)
            phase = match.group(1).strip().lower() if match else "unknown"
            series = radial_volume_average(thermo_profiles[variable], radii_m_array)
            color = species_colors[species]
            gb_radial_histories.append(series)
            gb_colors.append(color)
            gb_hatches.append(phase_hatch.get(phase, phase_hatch["unknown"]))
            gb_labels.append(f"{species} ({phase})")

        polys = axis.stackplot(
            reference_burnup,
            gb_radial_histories,
            colors=gb_colors,
            labels=gb_labels,
            alpha=0.9,
        )
        for poly, hatch in zip(polys, gb_hatches):
            poly.set_hatch(hatch)
            poly.set_edgecolor((0.1, 0.1, 0.1, 0.7))
            poly.set_linewidth(0.2)

        cumulative_histories = np.cumsum(np.vstack(gb_radial_histories), axis=0)
        for boundary in cumulative_histories:
            axis.plot(reference_burnup, boundary, color="#111827", linewidth=0.25, alpha=0.40)

        axis.set_xlabel("Burnup (MWd/kgUO2)")
        axis.set_ylabel("Concentration at grain boundary (mol/m3)")
        add_capped_legend(axis, loc="upper left")
        fig.tight_layout()
        plot_path = PLOTS_DIR / "summary_05_gb_phases_vs_burnup.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

        fig, axis = plt.subplots()
        gb_radial_histories = []
        gb_colors = []
        gb_hatches = []
        gb_labels = []
        for variable in gb_sorted_variables:
            species = variable.split(" (", 1)[0]
            match = re.search(r"\(([^,]+), at grain boundary\)", variable)
            phase = match.group(1).strip().lower() if match else "unknown"
            if phase != "condensed": 
                continue
            series = radial_volume_average(thermo_profiles[variable], radii_m_array)
            color = species_colors[species]
            gb_radial_histories.append(series)
            gb_colors.append(color)
            gb_hatches.append(phase_hatch.get(phase, phase_hatch["unknown"]))
            gb_labels.append(f"{species}")

        polys = axis.stackplot(
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
        axis.set_ylabel("Concentration of condensed species at grain boundary (mol/m3)")
        add_capped_legend(axis, loc="upper left")
        fig.tight_layout()
        plot_path = PLOTS_DIR / "summary_06_gb_noliq_phases_vs_burnup.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)
    
    if "JOG (/)" in output_profiles:
        jog_condensed_thickness_over_time_um = None

        if "JOG from condensed (/)" in output_profiles:
            jog_condensed_thickness_over_time_um = radial_integral_over_radius(
                output_profiles["JOG from condensed (/)"],
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
        gb_labels = [item[0] for item in condensed_entries]
        gb_radial_histories = [item[1] for item in condensed_entries]
        gb_colors = [item[2] for item in condensed_entries]
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

        axis.plot(reference_burnup, jog_condensed_thickness_over_time_um, color="#111827", label="Total")
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

        fig.tight_layout()
        plot_path = PLOTS_DIR / "summary_08_jog_contributions_and_experiments.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)


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
    r_inner_m, r_outer_m, half_wall_thickness_m = hollow_geometry_from_case_directories(case_directories)

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

    for case_dir in case_directories:
        gb_color_map, matrix_color_map = build_thermochemistry_color_maps(case_directories)
        break
    else:
        gb_color_map, matrix_color_map = {}, {}

    for case_dir in case_directories:
        plot_case(case_dir, saved_paths, gb_color_map, matrix_color_map, half_wall_thickness_m, r_inner_m, r_outer_m)

    plot_radial_profiles(case_directories, saved_paths, gb_color_map)

    print("Generated plots:")
    for path in saved_paths:
        print(path)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
