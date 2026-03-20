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
BUILD_EXECUTABLE = TEST_DIR.parents[1] / "build" / "sciantix.x"
RUN_LOG = TEST_DIR / "sciantix.log"
RUN_SUMMARY = TEST_DIR / "run_summary.txt"
CASE_LOG_NAME = "sciantix_case.log"
MAIN_OUTPUT_NAME = "output.txt"
THERMO_OUTPUT_NAME = "thermochemistry_output.txt"
PLOTS_DIR = TEST_DIR / "plots"
EXP_DATA_DIR = TEST_DIR.parent / "test_JOG" / "exp_data"
JOG_RADIUS_M = 5.0e-3
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
    "lines.markersize": 4,
    "legend.frameon": False,
})

colors = ["#ff0000", "#ff7f00", "#00c853", "#2962ff", "#aa00ff"]


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


def case_dirs() -> list[Path]:
    return sorted(path for path in TEST_DIR.glob("point_*") if path.is_dir())


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


def plot_case(case_dir: Path, saved_paths: list[Path]) -> None:
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
    jog_label = "JOG (/)"
    oxygen_label = "Oxygen content (mol/m3)"
    uranium_label = "Uranium content (mol/m3)"

    burnup = values[:, column_map[burnup_label]]
    time = values[:, column_map[time_label]]
    fima = values[:, column_map[fima_label]]
    jog_fraction = values[:, column_map[jog_label]]
    jog_thickness_um = jog_fraction * (JOG_RADIUS_M / 2.0) * 1.0e6
    grain_boundary_fraction = values[:, column_map["Grain boundary fraction (/)"]]
    oxygen_activity = (values[:, column_map["Fuel oxygen partial pressure (MPa)"]] / 0.1) ** 0.5

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
    axis.plot(burnup, values[:, column_map[oxygen_label]] * grain_boundary_fraction, color=colors[2], label="* at grain boundary")
    axis.plot(burnup, values[:, column_map[oxygen_label]] * oxygen_activity, color=colors[3], label="* activity(O)")
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Oxygen concentration (mol/m3)")
    axis.set_yscale("log")
    axis.set_ylim([1e1, 1e6])
    axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
    axis.legend(loc="upper left")

    axis = axes[1]
    axis.plot(burnup, values[:, column_map[uranium_label]], color=colors[0], label="Total")
    axis.plot(burnup, values[:, column_map[uranium_label]] * grain_boundary_fraction, color=colors[2], label="At grain boundary")
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Uranium concentration (mol/m3)")
    axis.set_yscale("log")
    axis.set_ylim([1e1, 1e6])
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
            ("Released", colors[3], " released (at/m3)"),
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
        "Grain radius (m)",
        "Fission gas release (/)",
        "Gap oxygen partial pressure (MPa)",
        "Stoichiometry deviation (/)",
        "Fuel oxygen partial pressure (MPa)",
        "Fuel oxygen potential (KJ/mol)",
        "JOG (/)",
        "Grain boundary fraction (/)",
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
    axis.plot(burnup, p_o2_bb, label="Blackburn", color=colors[1])
    axis.plot(burnup, p_o2_oc, label="CALPHAD", color=colors[3])
    axis.set_xlabel(burnup_label)
    axis.set_ylabel(r"log10(pO2/bar)")
    axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
    axis.legend(loc="best")
    fig.tight_layout()
    plot_path = case_plot_dir / "po2.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    fig, axis = plt.subplots()
    axis.plot(burnup, values[:, column_map["Fuel oxygen potential (KJ/mol)"]], label="SCIANTIX", color=colors[0])
    axis.plot(burnup, values[:, column_map["Fuel oxygen potential - Blackburn (KJ/mol)"]], label="Blackburn", color=colors[1])
    axis.plot(burnup, values[:, column_map["Fuel oxygen potential - CALPHAD (KJ/mol)"]], label="CALPHAD", color=colors[3])
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
    axis.scatter(melis_fima, melis_thickness, color=colors[1], marker="o", label="Melis et al. (1993)", zorder=3)
    axis.scatter(tourasse_fima, tourasse_thickness, color=colors[2], marker="D", label="Tourasse et al. (1992)", zorder=3)
    axis.set_xlabel(fima_label)
    axis.set_ylabel("JOG thickness (um)")
    axis.text(
        1.00,
        1.00,
        f"Thickness = JOG (/) * R/2,  R = {JOG_RADIUS_M * 1.0e3:.1f} mm",
        transform=axis.transAxes,
        ha="right",
        va="bottom",
    )
    axis.legend(loc="upper left")
    fig.tight_layout()
    plot_path = case_plot_dir / "JOG_fraction_vs_time_and_thickness_vs_fima.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

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
    fig, axis = plt.subplots()
    gb_stacked_data = [thermochemistry_values[:, thermochemistry_column_map[variable]] for variable in gb_sorted_variables]
    gb_colors = plt.cm.nipy_spectral(np.linspace(0, 1, max(len(gb_sorted_variables), 1)))
    if gb_stacked_data:
        axis.stackplot(
            thermochemistry_burnup,
            gb_stacked_data,
            labels=gb_sorted_variables,
            colors=gb_colors,
            alpha=0.9,
        )
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Concentration at grain boundary (mol/m3)")
    if axis.get_legend_handles_labels()[0]:
        axis.legend(loc="upper left", fontsize=8)
    fig.tight_layout()
    plot_path = case_plot_dir / "thermochemistry_fission_products_at_grain_boundary.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    condensed_cs2moo4_s1_label = "CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)"
    condensed_cs2moo4_s2_label = "CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)"
    cs = (values[:, column_map["Cs reacted - GB (at/m3)"]] + values[:, column_map["Cs at grain boundary (at/m3)"]]) / AVOGADRO_NUMBER
    mo = (values[:, column_map["Mo reacted - GB (at/m3)"]] + values[:, column_map["Mo at grain boundary (at/m3)"]]) / AVOGADRO_NUMBER
    o = values[:, column_map[oxygen_label]] * oxygen_activity
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
    if condensed_cs2moo4_s1_label in thermochemistry_column_map:
        condensed_cs2moo4_s1 = thermochemistry_values[:, thermochemistry_column_map[condensed_cs2moo4_s1_label]]
        condensed_cs2moo4_s1_volume_fraction = condensed_cs2moo4_s1 * cs2moo4_molar_mass / condensed_s1_density_g_per_m3
    condensed_cs2moo4_s2_volume_fraction = np.zeros_like(thermochemistry_burnup)
    if condensed_cs2moo4_s2_label in thermochemistry_column_map:
        condensed_cs2moo4_s2 = thermochemistry_values[:, thermochemistry_column_map[condensed_cs2moo4_s2_label]]
        condensed_cs2moo4_s2_volume_fraction = condensed_cs2moo4_s2 * cs2moo4_molar_mass / condensed_s2_density_g_per_m3
    condensed_cs2moo4_volume_fraction = condensed_cs2moo4_s1_volume_fraction + condensed_cs2moo4_s2_volume_fraction
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
    axis.plot(thermochemistry_burnup, thermochemistry_jog_thickness_um, color="#111827", label="Total estimated thickness")
    axis.plot(fima, jog_thickness_um, color="#059669", linestyle="--", label="SCIANTIX JOG thickness")
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("JOG thickness (um)")
    axis.legend(loc="upper left")
    fig.tight_layout()
    plot_path = case_plot_dir / "JOG_thickness_from_thermochemistry.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    matrix_variables = [
        header
        for header in thermochemistry_headers
        if header not in {burnup_label, time_label}
        and ", matrix)" in header
        and not is_all_zero(thermochemistry_values[:, thermochemistry_column_map[header]])
    ]
    matrix_sorted_variables = sorted(
        matrix_variables,
        key=lambda variable: thermochemistry_values[-1, thermochemistry_column_map[variable]],
        reverse=True,
    )
    fig, axis = plt.subplots()
    matrix_stacked_data = [thermochemistry_values[:, thermochemistry_column_map[variable]] for variable in matrix_sorted_variables]
    matrix_colors = plt.cm.nipy_spectral(np.linspace(0, 1, max(len(matrix_sorted_variables), 1)))
    if matrix_stacked_data:
        axis.stackplot(
            thermochemistry_burnup,
            matrix_stacked_data,
            labels=matrix_sorted_variables,
            colors=matrix_colors,
            alpha=0.9,
        )
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Concentration in matrix (mol/m3)")
    if axis.get_legend_handles_labels()[0]:
        axis.legend(loc="upper left", fontsize=8)
    fig.tight_layout()
    plot_path = case_plot_dir / "thermochemistry_matrix.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)


def plot_radial_profiles(case_directories: list[Path], saved_paths: list[Path]) -> None:
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
    radius_fraction = radii_mm_array / radii_mm_array.max()
    radii_m_array = radii_mm_array * 1.0e-3
    r_max_m = radii_m_array.max()
    output_histories = [output_histories[index] for index in order]
    thermo_histories = [thermo_histories[index] for index in order]

    reference_time = output_histories[0]["Time (h)"]
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

    snapshot_targets = np.arange(0.0, np.nanmax(reference_fima) + 1.0, 1.0)
    indexes: list[int] = []
    for target in snapshot_targets:
        index = int(np.argmin(np.abs(reference_fima - target)))
        if index not in indexes:
            indexes.append(index)
    if indexes[-1] != len(reference_time) - 1:
        indexes.append(len(reference_time) - 1)
    snapshot_colors = plt.cm.viridis(np.linspace(0, 1, len(indexes)))

    fig, axes = plt.subplots(3, 2, figsize=(12, 14))
    axes = axes.flatten()
    radial_summary_variables = [
        ("Temperature (K)", "Temperature (K)"),
        ("System pressure (Pa)", "Pressure (Pa)"),
        ("Stoichiometry deviation (/)", "Stoichiometry deviation (/)"),
        ("Fuel oxygen partial pressure (MPa)", "Fuel oxygen partial pressure (MPa)"),
        ("Fuel oxygen potential (KJ/mol)", "Fuel oxygen potential (KJ/mol)"),
        ("JOG (/)", "JOG (/)"),
    ]
    for axis, (variable, ylabel) in zip(axes, radial_summary_variables):
        if variable not in output_profiles:
            axis.set_visible(False)
            continue

        for color, index in zip(snapshot_colors, indexes):
            value = output_profiles[variable][:, index]
            if variable == "Fuel oxygen partial pressure (MPa)":
                value = np.log10(value/0.1)
                ylabel= r"log10(pO2/bar)"
                
            axis.plot(
                radius_fraction,
                value,
                color=color,
                marker="o",
                label=f"FIMA {reference_fima[index]:.1f}%",
            )
        axis.set_xlabel("Radius / Rmax (-)")
        axis.set_ylabel(ylabel)
        axis.grid(True, alpha=0.3)
        axis.legend(loc="upper right")
    fig.tight_layout()
    plot_path = PLOTS_DIR / "radial_profile_summary.png"
    fig.savefig(plot_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    if "JOG (/)" in output_profiles:
        jog_thickness_over_time_m = np.trapezoid(
            output_profiles["JOG (/)"] * radii_m_array[:, np.newaxis],
            x=radii_m_array,
            axis=0,
        ) / r_max_m
        jog_thickness_over_time_um = jog_thickness_over_time_m * 1.0e6

        cs_mol = (
            output_profiles["Cs reacted - GB (at/m3)"] + output_profiles["Cs at grain boundary (at/m3)"]
        ) / AVOGADRO_NUMBER
        mo_mol = (
            output_profiles["Mo reacted - GB (at/m3)"] + output_profiles["Mo at grain boundary (at/m3)"]
        ) / AVOGADRO_NUMBER
        oxygen_activity = np.sqrt(np.clip(output_profiles["Fuel oxygen partial pressure (MPa)"], 0.0, None) / 0.1)
        oxygen_available = output_profiles["Oxygen content (mol/m3)"] * oxygen_activity
        liquid_cs2moo4_available_mol = np.minimum.reduce([
            cs_mol / 2.0,
            mo_mol,
            oxygen_available / 4.0,
        ])

        cs2moo4_molar_mass = (
            2.0 * ATOMIC_MASS_G_PER_MOL["Cs"]
            + ATOMIC_MASS_G_PER_MOL["Mo"]
            + 4.0 * ATOMIC_MASS_G_PER_MOL["O"]
        )
        liquid_cs2moo4_volume_fraction = (
            liquid_cs2moo4_available_mol * cs2moo4_molar_mass / (CS2MOO4_LIQUID_DENSITY_KG_PER_M3 * 1000.0)
        )
        liquid_cs2moo4_thickness_over_time_m = np.trapezoid(
            liquid_cs2moo4_volume_fraction * radii_m_array[:, np.newaxis],
            x=radii_m_array,
            axis=0,
        ) / r_max_m
        liquid_cs2moo4_thickness_over_time_um = liquid_cs2moo4_thickness_over_time_m * 1.0e6

        condensed_cs2moo4_thickness_over_time_um = None
        if (
            "CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)" in thermo_profiles
            or "CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)" in thermo_profiles
        ):
            temperature_profiles = output_profiles["Temperature (K)"]
            temperature_celsius = temperature_profiles - 273.15
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

            condensed_s1_volume_fraction = np.zeros_like(output_profiles["JOG (/)"])
            if "CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)" in thermo_profiles:
                condensed_s1_volume_fraction = (
                    thermo_profiles["CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)"]
                    * cs2moo4_molar_mass
                    / condensed_s1_density_g_per_m3
                )

            condensed_s2_volume_fraction = np.zeros_like(output_profiles["JOG (/)"])
            if "CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)" in thermo_profiles:
                condensed_s2_volume_fraction = (
                    thermo_profiles["CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)"]
                    * cs2moo4_molar_mass
                    / condensed_s2_density_g_per_m3
                )

            condensed_cs2moo4_volume_fraction = condensed_s1_volume_fraction + condensed_s2_volume_fraction
            condensed_cs2moo4_thickness_over_time_m = np.trapezoid(
                condensed_cs2moo4_volume_fraction * radii_m_array[:, np.newaxis],
                x=radii_m_array,
                axis=0,
            ) / r_max_m
            condensed_cs2moo4_thickness_over_time_um = condensed_cs2moo4_thickness_over_time_m * 1.0e6

        fig, axis = plt.subplots()
        for color, index in zip(snapshot_colors, indexes):
            axis.plot(
                radius_fraction,
                output_profiles["JOG (/)"][:, index],
                color=color,
                marker="o",
                label=f"FIMA {reference_fima[index]:.1f}%",
            )
        axis.set_xlabel("Radius / Rmax (-)")
        axis.set_ylabel("JOG (/)")
        axis.set_title("Radial Profile of JOG")
        axis.legend()
        axis.grid(True, alpha=0.3)
        fig.tight_layout()
        plot_path = PLOTS_DIR / "radial_profile_jog.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

        fig, axis = plt.subplots()
        axis.plot(reference_time, jog_thickness_over_time_um, color="#7c3aed",label="SCIANTIX JOG thickness")
        axis.plot(reference_time, liquid_cs2moo4_thickness_over_time_um, color="#f97316", label="Available Cs2MoO4 thickness")
        if condensed_cs2moo4_thickness_over_time_um is not None:
            axis.plot(
                reference_time,
                condensed_cs2moo4_thickness_over_time_um,
                color="#1d4ed8",
                linestyle="--",
                label="CALPHAD Cs2MoO4 thickness",
            )
        axis.set_xlabel("Time (h)")
        axis.set_ylabel("JOG thickness (um)")
        axis.set_title(r"JOG thickness = $\int_0^{Rmax} JOG(r)\,r\,dr / Rmax$")
        axis.legend(loc="best")
        axis.grid(True, alpha=0.3)
        fig.tight_layout()
        plot_path = PLOTS_DIR / "jog_thickness_over_time.png"
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

    PLOTS_DIR.mkdir(exist_ok=True)
    saved_paths: list[Path] = []
    case_results: list[tuple[Path, subprocess.CompletedProcess[str]]] = []

    if not args.plot_only:
        ensure_executable(BUILD_EXECUTABLE)
        for case_dir in case_directories:
            prepare_case_inputs(case_dir)
            completed = run_sciantix_case(case_dir)
            (case_dir / CASE_LOG_NAME).write_text(completed.stdout + completed.stderr)
            case_results.append((case_dir, completed))
            if completed.returncode != 0:
                raise RuntimeError(f"SCIANTIX failed for {case_dir}")

        chunks: list[str] = []
        for case_dir, completed in case_results:
            chunks.append(f"===== {case_dir.name} =====")
            chunks.append(f"returncode = {completed.returncode}")
            chunks.append(completed.stdout)
            chunks.append(completed.stderr)
            chunks.append("")
        RUN_LOG.write_text("\n".join(chunks))

        summary_lines = ["SUPERFACT batch run summary", ""]
        for case_dir, completed in case_results:
            summary_lines.append(f"{case_dir.name}: returncode={completed.returncode}")
            summary_lines.append(f"  case_log = {case_dir / CASE_LOG_NAME}")
            summary_lines.append("")
        RUN_SUMMARY.write_text("\n".join(summary_lines))
    else:
        for case_dir in case_directories:
            ensure_output_file(case_dir / MAIN_OUTPUT_NAME)
            ensure_output_file(case_dir / THERMO_OUTPUT_NAME)

    for case_dir in case_directories:
        plot_case(case_dir, saved_paths)

    plot_radial_profiles(case_directories, saved_paths)

    print("Generated plots:")
    for path in saved_paths:
        print(path)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
