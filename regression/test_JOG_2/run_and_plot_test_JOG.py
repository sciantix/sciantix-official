#!/usr/bin/env python3
import argparse
import csv
import subprocess
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

TEST_DIR = Path(__file__).resolve().parent
BUILD_EXECUTABLE = TEST_DIR.parents[1] / "build" / "sciantix.x"
RUN_LOG = TEST_DIR / "sciantix.log"
OUTPUT_FILE = TEST_DIR / "output.txt"
THERMOCHEMISTRY_OUTPUT_FILE = TEST_DIR / "thermochemistry_output.txt"
EXP_DATA_DIR = TEST_DIR / "exp_data"
JOG_RADIUS_M = 5.0e-3
AVOGADRO_NUMBER = 6.02214076e23

ATOMIC_MASS_G_PER_MOL = {
    "Cs": 132.90545196,
    "Mo": 95.95,
    "O": 15.999,
    "U": 238.02891,
}
CS2MOO4_LIQUID_DENSITY_KG_PER_M3 = 4380.0 # Samuellsson

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

colors = ['#ff0000', '#ff7f00', '#00c853', '#2962ff', '#aa00ff']

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


def run_sciantix(executable: Path) -> subprocess.CompletedProcess[str]:
    completed = subprocess.run(
        [str(executable)],
        cwd=TEST_DIR,
        text=True,
        capture_output=True,
        check=False,
    )
    return completed


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--plot-only",
        action="store_true",
        help="Skip the SCIANTIX run and regenerate plots from existing output files.",
    )
    args = parser.parse_args()

    if args.plot_only:
        completed = subprocess.CompletedProcess(args=[str(BUILD_EXECUTABLE)], returncode=0)
        ensure_output_file(OUTPUT_FILE)
        ensure_output_file(THERMOCHEMISTRY_OUTPUT_FILE)
    else:
        ensure_executable(BUILD_EXECUTABLE)
        completed = run_sciantix(BUILD_EXECUTABLE)
        RUN_LOG.write_text(completed.stdout + completed.stderr)

    if completed.returncode == 0:
        saved_paths: list[Path] = []

        headers, values = load_output_data(OUTPUT_FILE)
        column_map = {name: idx for idx, name in enumerate(headers)}

        burnup_label = "Burnup (MWd/kgUO2)"
        time_label = "Time (h)"
        fima_label = "FIMA (%)"
        jog_label = "JOG (/)"

        burnup = values[:, column_map[burnup_label]]
        time = values[:, column_map[time_label]]

        def burnup_to_time(x):
            return np.interp(x, burnup, time)

        def time_to_burnup(x):
            return np.interp(x, time, burnup)

        inventory_markers = {"produced", "in", "at", "released", "reacted"}
        grouped_variable_names: set[str] = set()
        for header in headers:
            parts = header.split()
            if len(parts) >= 2 and parts[1] in inventory_markers:
                grouped_variable_names.add(header)

        # Plot 1: O, U and grain-boundary fission-product inventories.
        grain_boundary_fraction = values[:, column_map["Grain boundary fraction (/)"]]
        oxygen_activity = (values[:, column_map["Fuel oxygen partial pressure (MPa)"]]/0.1)**0.5
        fig, axes = plt.subplots(3, 2, figsize=(12, 14))
        axes = axes.flatten()

        axis = axes[0]
        oxygen_label = "Oxygen content (mol/m3)"
        if oxygen_label not in column_map:
            raise KeyError(f"Required inventory column is missing from {OUTPUT_FILE}: {oxygen_label}")
        axis.plot(burnup, values[:, column_map[oxygen_label]], color=colors[0], label="Total")
        axis.plot(burnup, values[:, column_map[oxygen_label]] * grain_boundary_fraction, color=colors[2], label="* at grain boundary")
        axis.plot(burnup, values[:, column_map[oxygen_label]] * oxygen_activity, color=colors[3], label="* activity(O)")
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("Oxygen concentration (mol/m3)")
        axis.set_yscale('log')
        axis.set_ylim([1e1, 1e6])
        axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
        axis.legend(loc="upper left")

        axis = axes[1]
        uranium_label = "Uranium content (mol/m3)"
        if uranium_label not in column_map:
            raise KeyError(f"Required inventory column is missing from {OUTPUT_FILE}: {uranium_label}")
        axis.plot(burnup, values[:, column_map[uranium_label]], color=colors[0], label="Total")
        axis.plot(burnup, values[:, column_map[uranium_label]] * grain_boundary_fraction, color=colors[2], label="At grain boundary")
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("Uranium concentration (mol/m3)")
        axis.set_yscale('log')
        axis.set_ylim([1e1, 1e6])
        axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
        axis.legend(loc="upper left")

        axis = axes[2]
        axis.plot(
            burnup,
            values[:, column_map["Xe produced (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[0],
            label="Produced",
        )
        axis.plot(
            burnup,
            values[:, column_map["Xe in grain (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[1],
            label="In grain",
        )
        axis.plot(
            burnup,
            values[:, column_map["Xe at grain boundary (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[2],
            label="At grain boundary",
        )
        axis.plot(
            burnup,
            values[:, column_map["Xe released (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[3],
            label="Released",
        )
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("Xenon concentration (mol/m3)")
        axis.set_ylim([0, 1300])
        axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="upper left")

        axis = axes[3]
        axis.plot(
            burnup,
            values[:, column_map["Kr produced (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[0],
            label="Produced",
        )
        axis.plot(
            burnup,
            values[:, column_map["Kr in grain (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[1],
            label="In grain",
        )
        axis.plot(
            burnup,
            values[:, column_map["Kr at grain boundary (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[2],
            label="At grain boundary",
        )
        axis.plot(
            burnup,
            values[:, column_map["Kr released (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[3],
            label="Released",
        )
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("Krypton concentration (mol/m3)")
        axis.set_ylim([0, 1300])
        axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="upper left")

        axis = axes[4]
        axis.plot(
            burnup,
            values[:, column_map["Cs produced (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[0],
            label="Produced",
        )
        axis.plot(
            burnup,
            values[:, column_map["Cs in grain (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[1],
            label="In grain",
        )
        axis.plot(
            burnup,
            values[:, column_map["Cs at grain boundary (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[2],
            label="At grain boundary",
        )
        axis.plot(
            burnup,
            values[:, column_map["Cs reacted - GB (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[4],
            label="Reacted at grain boundary",
        )
        axis.plot(
            burnup,
            values[:, column_map["Cs released (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[3],
            label="Released",
        )
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("Cesium concentration (mol/m3)")
        axis.set_ylim([0, 1300])
        axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="upper left")

        axis = axes[5]
        axis.plot(
            burnup,
            values[:, column_map["Mo produced (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[0],
            label="Produced",
        )
        axis.plot(
            burnup,
            values[:, column_map["Mo in grain (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[1],
            label="In grain",
        )
        axis.plot(
            burnup,
            values[:, column_map["Mo at grain boundary (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[2],
            label="At grain boundary",
        )
        axis.plot(
            burnup,
            values[:, column_map["Mo reacted - GB (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[4],
            label="Reacted at grain boundary",
        )
        axis.plot(
            burnup,
            values[:, column_map["Mo released (at/m3)"]] / AVOGADRO_NUMBER,
            color=colors[3],
            label="Released",
        )
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("Molybdenum concentration (mol/m3)")
        axis.set_ylim([0, 1300])
        axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="upper left")

        fig.tight_layout()
        plot_path = OUTPUT_FILE.parent / "inventory_mol_m3_overview.png"
        fig.savefig(plot_path, dpi=220, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

        # Plot 2: automatically generated pages for the remaining non-zero summary variables.
        summary_variables = [
            header
            for header in headers
            if header not in {burnup_label, time_label}
            and header not in grouped_variable_names
            and not is_all_zero(values[:, column_map[header]])
        ]

        plots_per_page = 6
        ncols = 2
        nrows = 3
        for page_index, start in enumerate(range(0, len(summary_variables), plots_per_page), start=1):
            page_variables = summary_variables[start:start + plots_per_page]
            fig, axes = plt.subplots(nrows, ncols, figsize=(12, 14))
            axes = axes.flatten()

            for axis, variable in zip(axes, page_variables):
                axis.plot(burnup, values[:, column_map[variable]], color="#0f766e")
                axis.set_xlabel(burnup_label)
                axis.set_ylabel(variable)
                
            for axis in axes[len(page_variables):]:
                axis.set_visible(False)

            fig.tight_layout()
            plot_path = OUTPUT_FILE.parent / f"summary_variables_{page_index:02d}.png"
            fig.savefig(plot_path, bbox_inches="tight")
            plt.close(fig)
            saved_paths.append(plot_path)

        # Plot 3: oxygen activity
        fig, axis = plt.subplots(1, 1)
        p_o2 = np.log10(values[:, column_map["Fuel oxygen partial pressure (MPa)"]]/0.1)
        p_o2_oc = np.log10(values[:, column_map["Fuel oxygen partial pressure - CALPHAD (MPa)"]]/0.1)
        p_o2_bb = np.log10(values[:, column_map["Fuel oxygen partial pressure - Blackburn (MPa)"]]/0.1)
    
        axis.plot(burnup, p_o2, color=colors[0], label="SCIANTIX")
        axis.plot(burnup, p_o2_oc, color=colors[1], label="CALPHAD")
        axis.plot(burnup, p_o2_bb, color=colors[2], label="Blackburn model")
        axis.set_xlabel(burnup_label)
        axis.set_ylabel(r"$\log_{10}(p_{O_2})$ (bar)")
        axis.legend()
        fig.tight_layout()
        plot_path = OUTPUT_FILE.parent / "po2.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

        fig, axis = plt.subplots(1, 1)
        p_o2 = values[:, column_map["Fuel oxygen potential (KJ/mol)"]]
        p_o2_oc = values[:, column_map["Fuel oxygen potential - CALPHAD (KJ/mol)"]]
        p_o2_bb = values[:, column_map["Fuel oxygen potential - Blackburn (KJ/mol)"]]
    
        axis.plot(burnup, p_o2, color=colors[0], label="SCIANTIX")
        axis.plot(burnup, p_o2_oc, color=colors[1], label="CALPHAD")
        axis.plot(burnup, p_o2_bb, color=colors[2], label="Blackburn model")
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("Fuel oxygen potential (kJ/mol)")
        axis.legend()
        fig.tight_layout()
        plot_path = OUTPUT_FILE.parent / "muo2.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

        # Plot 4: JOG thickness vs FIMA with experimental data
        fima = values[:, column_map[fima_label]]
        jog_fraction = values[:, column_map[jog_label]]
        jog_thickness_um = jog_fraction * (JOG_RADIUS_M / 2.0) * 1.0e6

        def fima_to_time(x):
            return np.interp(x, fima, time)

        def time_to_fima(x):
            return np.interp(x, time, fima)

        melis_fima, melis_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Melis1993.txt")
        tourasse_fima, tourasse_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Tourasse1992.txt")

        fig, axis = plt.subplots(1, 1)
        axis.plot(fima, jog_thickness_um, color=colors[3], label="SCIANTIX")
        axis.scatter(
            melis_fima,
            melis_thickness,
            color=colors[1],
            marker="o",
            label="Melis et al. (1993)",
            zorder=3,
        )
        axis.scatter(
            tourasse_fima,
            tourasse_thickness,
            color=colors[2],
            marker="D",
            label="Tourasse et al. (1992)",
            zorder=3,
        )
        axis.set_xlabel(fima_label)
        axis.set_ylabel("JOG thickness (um)")
        axis.text(
            1.00,
            1.00,
            f"Thickness = JOG (/) * R/2,  R = {JOG_RADIUS_M * 1.0e3:.1f} mm",
            transform=axis.transAxes,
            ha="right",
            va="bottom"
        )
        axis.legend(loc="upper left")
        fig.tight_layout()
        plot_path = OUTPUT_FILE.parent / "JOG_fraction_vs_time_and_thickness_vs_fima.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

        thermochemistry_headers, thermochemistry_values = load_output_data(THERMOCHEMISTRY_OUTPUT_FILE)
        thermochemistry_column_map = {name: idx for idx, name in enumerate(thermochemistry_headers)}

        thermochemistry_time = thermochemistry_values[:, thermochemistry_column_map[time_label]]
        thermochemistry_burnup = np.interp(thermochemistry_time, time, burnup)

        def thermochemistry_burnup_to_time(x):
            return np.interp(x, burnup, time)

        def thermochemistry_time_to_burnup(x):
            return np.interp(x, time, burnup)

        # Plot 5: thermochemistry variables at grain boundary.
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
        thermochemistry_colors = plt.cm.nipy_spectral(np.linspace(0, 1, len(gb_variables)))
        gb_stacked_data = [
            thermochemistry_values[:, thermochemistry_column_map[variable]]
            for variable in gb_sorted_variables
        ]
        axis.stackplot(
            thermochemistry_burnup,
            gb_stacked_data,
            labels=gb_sorted_variables,
            colors=thermochemistry_colors,
            alpha=0.9,
        )
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("Concentration at grain boundary (mol/m3)")
        if axis.get_legend_handles_labels()[0]:
            axis.legend()
        fig.tight_layout()
        plot_path = OUTPUT_FILE.parent / "thermochemistry_fission_products_at_grain_boundary.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

        # Plot 6: JOG thickness estimated from liquid + condensed thermochemistry phases.
        condensed_cs2moo4_s1_label = "CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)"
        condensed_cs2moo4_s2_label = "CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)"

        cs = (values[:, column_map["Cs reacted - GB (at/m3)"]] + values[:, column_map["Cs at grain boundary (at/m3)"]])/AVOGADRO_NUMBER
        mo = (values[:, column_map["Mo reacted - GB (at/m3)"]] + values[:, column_map["Mo at grain boundary (at/m3)"]])/AVOGADRO_NUMBER
        o = values[:, column_map[oxygen_label]] * oxygen_activity

        liquid_cs2moo4_available = np.minimum.reduce([
            cs / 2.0,
            mo,
            o / 4.0,
        ])
        liquid_cs2moo4_limiters = np.vstack([
            cs / 2.0,
            mo,
            o / 4.0,
        ])
        limiter_labels = ["Cs/2", "Mo", "O/4"]
        limiter_index = np.argmin(liquid_cs2moo4_limiters, axis=0)
        final_limiter_label = limiter_labels[int(limiter_index[-1])]
        unique_limiter_index, limiter_counts = np.unique(limiter_index, return_counts=True)

        cs2moo4_molar_mass = (
            2.0 * ATOMIC_MASS_G_PER_MOL["Cs"]
            + ATOMIC_MASS_G_PER_MOL["Mo"]
            + 4.0 * ATOMIC_MASS_G_PER_MOL["O"]
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
        axis.plot(
            thermochemistry_burnup,
            thermochemistry_jog_thickness_um,
            color="#111827",
            label="Total estimated thickness",
        )
        axis.plot(
            fima,
            jog_thickness_um,
            color="#059669",
            linestyle="--",
            label="SCIANTIX JOG thickness",
        )
        axis.text(
            0.02,
            0.98,
            f"Cs2MoO4 = min(Cs/2, Mo, O/4)\nFinal limiting factor: {final_limiter_label}",
            transform=axis.transAxes,
            ha="left",
            va="top",
        )
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("JOG thickness (um)")
        axis.legend()
        fig.tight_layout()
        plot_path = OUTPUT_FILE.parent / "JOG_thickness_from_thermochemistry.png"
        fig.savefig(plot_path, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

        print("Liquid Cs2MoO4 limiting factor:")
        print(f"  Final point: {final_limiter_label}")
        print(
            "  Final availability [mol/m3 as Cs2MoO4 precursor]: "
            f"Cs/2 = {liquid_cs2moo4_limiters[0, -1]:.6e}, "
            f"Mo = {liquid_cs2moo4_limiters[1, -1]:.6e}, "
            f"O/4 = {liquid_cs2moo4_limiters[2, -1]:.6e}"
        )
        print("  Limiting factor counts over time:")
        for idx, count in zip(unique_limiter_index, limiter_counts):
            print(f"    {limiter_labels[int(idx)]}: {count}")

        # Plot 7: thermochemistry variables in matrix.
        matrix_variables = [
            header
            for header in thermochemistry_headers
            if header not in {burnup_label, time_label}
            and ", matrix)" in header
            and not is_all_zero(thermochemistry_values[:, thermochemistry_column_map[header]])
        ]
        if not matrix_variables:
            raise ValueError("No thermochemistry variables found for matrix")
        matrix_sorted_variables = sorted(
            matrix_variables,
            key=lambda variable: thermochemistry_values[-1, thermochemistry_column_map[variable]],
            reverse=True,
        )

        fig, axis = plt.subplots()
        thermochemistry_colors = plt.cm.nipy_spectral(np.linspace(0, 1, len(matrix_variables)))
        matrix_stacked_data = [
            thermochemistry_values[:, thermochemistry_column_map[variable]]
            for variable in matrix_sorted_variables
        ]
        axis.stackplot(
            thermochemistry_burnup,
            matrix_stacked_data,
            labels=matrix_sorted_variables,
            colors=thermochemistry_colors,
            alpha=0.9,
        )
        axis.set_xlabel(burnup_label)
        axis.set_ylabel("Concentration at grain boundary (mol/m3)")
        if axis.get_legend_handles_labels()[0]:
            axis.legend()
        fig.tight_layout()
        plot_path = OUTPUT_FILE.parent / "thermochemistry_matrix.png"
        fig.savefig(plot_path, dpi=200, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

        print("Generated plots:")
        for path in saved_paths:
            print(path)

    return completed.returncode


if __name__ == "__main__":
    raise SystemExit(main())
