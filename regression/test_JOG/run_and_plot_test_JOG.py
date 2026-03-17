#!/usr/bin/env python3
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
PLOT_BASENAME = "sciantix_all_variables_vs_burnup_time"
JOG_RADIUS_M = 5.0e-3
AVOGADRO_NUMBER = 6.02214076e23

plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (11, 7),
    "font.size": 17,
    "axes.labelsize": 18,
    "axes.titlesize": 19,
    "axes.titleweight": "bold",
    "xtick.labelsize": 15,
    "ytick.labelsize": 15,
    "legend.fontsize": 14,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.25,
    "grid.linestyle": "--",
    "axes.spines.top": False,
    "axes.spines.right": False,
    "lines.linewidth": 2.4,
    "lines.markersize": 7,
    "legend.frameon": False,
})


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


def sanitize_filename(label: str) -> str:
    return "".join(char if char.isalnum() else "_" for char in label).strip("_").lower()


def build_axis_mappers(burnup: np.ndarray, time: np.ndarray):
    def burnup_to_time(x):
        return np.interp(x, burnup, time)

    def time_to_burnup(x):
        return np.interp(x, time, burnup)

    return burnup_to_time, time_to_burnup


def is_all_zero(series: np.ndarray) -> bool:
    return np.allclose(series, 0.0)


def style_axis(axis, title: str, xlabel: str, ylabel: str) -> None:
    axis.set_title(title, pad=14)
    axis.set_xlabel(xlabel)
    axis.set_ylabel(ylabel)
    axis.grid(True, alpha=0.25)


def create_secondary_time_axis(axis, burnup_to_time, time_to_burnup, time_label: str) -> None:
    top_axis = axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup))
    top_axis.set_xlabel(time_label)


def get_inventory_groups(headers: list[str]) -> dict[str, list[str]]:
    grouped_variables: dict[str, list[str]] = {}
    inventory_markers = {"produced", "in", "at", "released", "reacted"}

    for header in headers:
        parts = header.split()
        if len(parts) < 2 or parts[1] not in inventory_markers:
            continue

        grouped_variables.setdefault(parts[0], []).append(header)

    return grouped_variables


def atoms_per_m3_to_umol_per_mm3(series: np.ndarray) -> np.ndarray:
    return series / AVOGADRO_NUMBER * 1.0e-3


def plot_grouped_inventory_variables(
    output_file: Path,
    burnup: np.ndarray,
    column_map: dict[str, int],
    grouped_variables: dict[str, list[str]],
    burnup_to_time,
    time_to_burnup,
    burnup_label: str,
    time_label: str,
    values: np.ndarray,
) -> list[Path]:
    saved_paths: list[Path] = []

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

        style_axis(
            axis,
            f"{group_name} Inventory Evolution",
            burnup_label,
            nonzero_variables[0].split("(", 1)[-1].rstrip(")"),
        )
        create_secondary_time_axis(axis, burnup_to_time, time_to_burnup, time_label)
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), frameon=False)
        fig.tight_layout()

        plot_path = output_file.parent / f"{sanitize_filename(group_name)}_inventory.png"
        fig.savefig(plot_path, dpi=220, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

    return saved_paths


def plot_selected_inventory_groups_in_umol(
    output_file: Path,
    burnup: np.ndarray,
    column_map: dict[str, int],
    grouped_variables: dict[str, list[str]],
    burnup_to_time,
    time_to_burnup,
    burnup_label: str,
    time_label: str,
    values: np.ndarray,
) -> list[Path]:
    saved_paths: list[Path] = []
    selected_groups = ["Xe", "Kr", "Cs"]

    for group_name in selected_groups:
        variables = grouped_variables.get(group_name, [])
        nonzero_variables = [
            variable for variable in variables if not is_all_zero(values[:, column_map[variable]])
        ]
        if not nonzero_variables:
            continue

        fig, axis = plt.subplots(figsize=(14, 8.5))
        colors = plt.cm.Set2(np.linspace(0, 1, len(nonzero_variables)))

        for color, variable in zip(colors, nonzero_variables):
            converted_values = atoms_per_m3_to_umol_per_mm3(values[:, column_map[variable]])
            axis.plot(
                burnup,
                converted_values,
                label=variable.replace(f"{group_name} ", ""),
                color=color,
            )

        style_axis(
            axis,
            f"{group_name} Inventory Evolution",
            burnup_label,
            "Concentration (umol/mm3)",
        )
        create_secondary_time_axis(axis, burnup_to_time, time_to_burnup, time_label)
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), frameon=False)
        fig.tight_layout()

        plot_path = output_file.parent / f"{sanitize_filename(group_name)}_inventory_umol_mm3.png"
        fig.savefig(plot_path, dpi=220, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

    return saved_paths


def plot_misc_variables(
    output_file: Path,
    burnup: np.ndarray,
    column_map: dict[str, int],
    variables: list[str],
    burnup_to_time,
    time_to_burnup,
    burnup_label: str,
    time_label: str,
    values: np.ndarray,
) -> list[Path]:
    saved_paths: list[Path] = []
    plots_per_page = 6
    ncols = 2
    nrows = 3

    for page_index, start in enumerate(range(0, len(variables), plots_per_page), start=1):
        page_variables = variables[start:start + plots_per_page]
        fig, axes = plt.subplots(nrows, ncols, figsize=(18, 16), sharex=True)
        axes = axes.flatten()

        for axis, variable in zip(axes, page_variables):
            y_data = values[:, column_map[variable]]
            axis.plot(burnup, y_data, color="#0f766e", linewidth=2.0)
            style_axis(axis, variable, burnup_label, variable)
            create_secondary_time_axis(axis, burnup_to_time, time_to_burnup, time_label)

        for axis in axes[len(page_variables):]:
            axis.set_visible(False)

        fig.suptitle("SCIANTIX Summary Variables vs Burnup and Time", fontsize=21, weight="bold")
        fig.tight_layout(rect=(0, 0, 1, 0.98))

        plot_path = output_file.parent / f"summary_variables_{page_index:02d}.png"
        fig.savefig(plot_path, dpi=220, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

    return saved_paths


def plot_all_variables(output_file: Path) -> list[Path]:
    headers, values = load_output_data(output_file)
    column_map = {name: idx for idx, name in enumerate(headers)}

    burnup_label = "Burnup (MWd/kgUO2)"
    time_label = "Time (h)"

    if burnup_label not in column_map or time_label not in column_map:
        raise KeyError("Required x-axis columns are missing from output.txt")

    burnup = values[:, column_map[burnup_label]]
    time = values[:, column_map[time_label]]
    burnup_to_time, time_to_burnup = build_axis_mappers(burnup, time)
    saved_paths: list[Path] = []

    grouped_variables = get_inventory_groups(headers)
    saved_paths.extend(
        plot_grouped_inventory_variables(
            output_file,
            burnup,
            column_map,
            grouped_variables,
            burnup_to_time,
            time_to_burnup,
            burnup_label,
            time_label,
            values,
        )
    )
    saved_paths.extend(
        plot_selected_inventory_groups_in_umol(
            output_file,
            burnup,
            column_map,
            grouped_variables,
            burnup_to_time,
            time_to_burnup,
            burnup_label,
            time_label,
            values,
        )
    )

    grouped_variable_names = {
        variable for variables in grouped_variables.values() for variable in variables
    }
    summary_variables = [
        header
        for header in headers
        if header not in {burnup_label, time_label}
        and header not in grouped_variable_names
        and not is_all_zero(values[:, column_map[header]])
    ]
    saved_paths.extend(
        plot_misc_variables(
            output_file,
            burnup,
            column_map,
            summary_variables,
            burnup_to_time,
            time_to_burnup,
            burnup_label,
            time_label,
            values,
        )
    )

    fig, axis = plt.subplots(1, 1, figsize=(12, 7), sharex=True)
    p_o2 = values[:, column_map["Fuel oxygen partial pressure (MPa)"]]
    activity_o = np.sqrt(p_o2 / 0.1)
    axis.plot(burnup, activity_o, color="#1d4ed8", linewidth=2.2)
    style_axis(axis, "Activity of Oxygen", burnup_label, "Activity (/)")
    create_secondary_time_axis(axis, burnup_to_time, time_to_burnup, time_label)

    fig.tight_layout(rect=(0, 0, 1, 0.98))
    plot_path = output_file.parent / "Oactivity.png"
    fig.savefig(plot_path, dpi=220, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(plot_path)

    saved_paths.append(plot_jog_quantities(output_file, headers, values, column_map))

    return saved_paths


def plot_jog_quantities(
    output_file: Path,
    headers: list[str],
    values: np.ndarray,
    column_map: dict[str, int],
) -> Path:
    time_label = "Time (h)"
    fima_label = "FIMA (%)"
    jog_label = "JOG (/)"

    if jog_label not in column_map:
        raise KeyError(f"Required JOG column is missing from {output_file}")
    if time_label not in column_map or fima_label not in column_map:
        raise KeyError(f"Required x-axis columns are missing from {output_file}")

    fima = values[:, column_map[fima_label]]
    jog_fraction = values[:, column_map[jog_label]]
    jog_thickness_um = jog_fraction * (JOG_RADIUS_M / 2.0) * 1.0e6
    time = values[:, column_map[time_label]]
    fima_to_time, time_to_fima = build_axis_mappers(fima, time)

    melis_fima, melis_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Melis1993.txt")
    tourasse_fima, tourasse_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Tourasse1992.txt")

    fig, axis = plt.subplots(1, 1, figsize=(12.5, 8.5))

    axis.plot(fima, jog_thickness_um, color="#7c3aed", label="SCIANTIX")
    axis.scatter(
        melis_fima,
        melis_thickness,
        color="#dc2626",
        marker="o",
        s=46,
        label="Melis 1993",
        zorder=3,
    )
    axis.scatter(
        tourasse_fima,
        tourasse_thickness,
        color="#2563eb",
        marker="D",
        s=44,
        label="Tourasse 1992",
        zorder=3,
    )
    style_axis(axis, "JOG Thickness vs FIMA", fima_label, "Thickness (um)")
    top_axis_thickness = axis.secondary_xaxis("top", functions=(fima_to_time, time_to_fima))
    top_axis_thickness.set_xlabel(time_label)
    axis.text(
        0.02,
        0.95,
        f"Thickness = JOG * R/2,  R = {JOG_RADIUS_M * 1.0e3:.1f} mm",
        transform=axis.transAxes,
        ha="left",
        va="top",
        fontsize=13,
        color="#4b5563",
    )
    axis.legend(loc="upper left")

    fig.tight_layout()
    plot_path = output_file.parent / "JOG_fraction_vs_time_and_thickness_vs_fima.png"
    fig.savefig(plot_path, dpi=240, bbox_inches="tight")
    plt.close(fig)
    return plot_path


def plot_thermochemistry_group(
    output_file: Path,
    reference_output_file: Path,
    location_label: str,
    filename: str,
) -> Path:
    headers, values = load_output_data(output_file)
    column_map = {name: idx for idx, name in enumerate(headers)}
    reference_headers, reference_values = load_output_data(reference_output_file)
    reference_column_map = {name: idx for idx, name in enumerate(reference_headers)}

    burnup_label = "Burnup (MWd/kgUO2)"
    time_label = "Time (h)"
    if time_label not in column_map:
        raise KeyError(f"Required time column is missing from {output_file}")
    if burnup_label not in reference_column_map or time_label not in reference_column_map:
        raise KeyError(f"Required x-axis columns are missing from {reference_output_file}")

    variables = [
        header
        for header in headers
        if header not in {burnup_label, time_label} and f", {location_label})" in header
        and not is_all_zero(values[:, column_map[header]])
    ]
    if not variables:
        raise ValueError(f"No thermochemistry variables found for {location_label} in {output_file}")

    time = values[:, column_map[time_label]]
    reference_burnup = reference_values[:, reference_column_map[burnup_label]]
    reference_time = reference_values[:, reference_column_map[time_label]]
    burnup = np.interp(time, reference_time, reference_burnup)
    burnup_to_time, time_to_burnup = build_axis_mappers(reference_burnup, reference_time)

    fig, axis = plt.subplots(figsize=(15, 9))
    colors = plt.cm.nipy_spectral(np.linspace(0, 1, len(variables)))

    for color, variable in zip(colors, variables):
        axis.plot(burnup, values[:, column_map[variable]], label=variable, linewidth=1.4, color=color)

    style_axis(
        axis,
        f"Thermochemistry Variables: {location_label}",
        burnup_label,
        "Amount / concentration",
    )

    create_secondary_time_axis(axis, burnup_to_time, time_to_burnup, time_label)

    handles, labels = axis.get_legend_handles_labels()
    if handles:
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), fontsize=8, frameon=False)
    fig.tight_layout()

    plot_path = output_file.parent / filename
    fig.savefig(plot_path, dpi=200, bbox_inches="tight")
    plt.close(fig)
    return plot_path

def ensure_executable(path: Path) -> None:
    if not path.exists():
        raise FileNotFoundError(f"SCIANTIX executable not found: {path}")
    if not path.is_file():
        raise FileNotFoundError(f"SCIANTIX executable path is not a file: {path}")

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
    ensure_executable(BUILD_EXECUTABLE)
    completed = run_sciantix(BUILD_EXECUTABLE)
    RUN_LOG.write_text(completed.stdout + completed.stderr)

    if completed.returncode == 0:
        saved_paths = plot_all_variables(OUTPUT_FILE)
        saved_paths.append(
            plot_thermochemistry_group(
                THERMOCHEMISTRY_OUTPUT_FILE,
                OUTPUT_FILE,
                "at grain boundary",
                "thermochemistry_fission_products_at_grain_boundary.png",
            )
        )
        saved_paths.append(
            plot_thermochemistry_group(
                THERMOCHEMISTRY_OUTPUT_FILE,
                OUTPUT_FILE,
                "matrix",
                "thermochemistry_matrix.png",
            )
        )
        print("Generated plots:")
        for path in saved_paths:
            print(path)

    return completed.returncode


if __name__ == "__main__":
    raise SystemExit(main())
