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
PLOT_BASENAME = "sciantix_all_variables_vs_burnup_time"


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

    y_variables = [
        header
        for header in headers
        if header not in {burnup_label, time_label}
        and not is_all_zero(values[:, column_map[header]])
    ]

    plots_per_page = 12
    ncols = 3
    nrows = 4
    saved_paths: list[Path] = []

    for page_index, start in enumerate(range(0, len(y_variables), plots_per_page), start=1):
        page_variables = y_variables[start:start + plots_per_page]
        fig, axes = plt.subplots(nrows, ncols, figsize=(16, 18), sharex=True)
        axes = axes.flatten()

        for axis, variable in zip(axes, page_variables):
            y_data = values[:, column_map[variable]]
            axis.plot(burnup, y_data, color="tab:blue", linewidth=1.6)
            axis.set_title(variable, fontsize=10)
            axis.set_xlabel(burnup_label)
            axis.set_ylabel(variable)
            axis.grid(True, alpha=0.3)

            top_axis = axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup))
            top_axis.set_xlabel(time_label)

        for axis in axes[len(page_variables):]:
            axis.set_visible(False)

        fig.suptitle("SCIANTIX variables vs burnup/time", fontsize=16)
        fig.tight_layout(rect=(0, 0, 1, 0.98))

        plot_path = output_file.parent / f"{PLOT_BASENAME}_{page_index:02d}.png"
        fig.savefig(plot_path, dpi=200, bbox_inches="tight")
        plt.close(fig)
        saved_paths.append(plot_path)

    return saved_paths


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

    fig, axis = plt.subplots(figsize=(16, 9))
    colors = plt.cm.nipy_spectral(np.linspace(0, 1, len(variables)))

    for color, variable in zip(colors, variables):
        axis.plot(burnup, values[:, column_map[variable]], label=variable, linewidth=1.4, color=color)

    axis.set_title(f"Thermochemistry variables in one plot: {location_label}", fontsize=16)
    axis.set_xlabel(burnup_label)
    axis.set_ylabel("Amount / concentration")
    axis.grid(True, alpha=0.3)

    top_axis = axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup))
    top_axis.set_xlabel(time_label)

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
