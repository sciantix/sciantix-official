from __future__ import annotations

"""Run the MOX Kato verification sweep and collect SCIANTIX outputs.

The workflow mirrors the UO2 pO2 verification layout as closely as possible:
- one case directory per temperature and plutonium content q
- one full SCIANTIX transient per case
- one concatenated summary table for the downstream comparison script
"""

import argparse

import math
import shutil
import subprocess
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import pandas as pd

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

TEMPERATURES_K = list(range(800, 2800, 200))
REFERENCE_PRESSURE_MPA = 0.1 # 1 bar
Q_VALUES = [0.1, 0.3, 0.4]

SCRIPT_DIR = Path(__file__).resolve().parent
BUILD_BINARY = SCRIPT_DIR.parent.parent / "build" / "sciantix.x"
LOCAL_BINARY = SCRIPT_DIR / "sciantix.x"
SUMMARY_PATH = SCRIPT_DIR / "temperature_sweep_summary.tsv"
PRESSURE_PLOT_NAME = "fuel_oxygen_partial_pressures_vs_ou_ratio"
PRESSURE_PLOT_NAME_2 = "fuel_oxygen_partial_pressures_vs_ou_ratio_2"
POTENTIAL_PLOT_NAME = "fuel_oxygen_potentials_vs_ou_ratio"
COMPARE_SCRIPT = SCRIPT_DIR / "sciantix_verification" / "compare_sciantix_with_kato.py"
COMPARE_OC_SCRIPT = SCRIPT_DIR / "sciantix_verification" / "compare_sciantix_with_oc_csv.py"


def parse_args() -> argparse.Namespace:
    """Parse the command-line arguments used by the MOX verification sweep."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--plot-only", action="store_true", help="Skip case generation and rerun only the comparison plots.")
    parser.add_argument("--keep-cases", action="store_true", help="Keep generated case folders after the comparison has completed.")
    parser.add_argument("--temperatures", default=",".join(str(value) for value in TEMPERATURES_K))
    parser.add_argument("--q-values", default=",".join(f"{value:.2f}" for value in Q_VALUES))
    return parser.parse_args()


def parse_int_list(text: str) -> list[int]:
    """Parse a comma-separated list of integers."""
    return [int(item.strip()) for item in text.split(",") if item.strip()]


def parse_float_list(text: str) -> list[float]:
    """Parse a comma-separated list of floating-point values."""
    return [float(item.strip()) for item in text.split(",") if item.strip()]

def ensure_local_binary() -> None:
    """Copy the up-to-date compiled SCIANTIX executable into this folder."""
    if not BUILD_BINARY.exists():
        raise FileNotFoundError(f"Missing SCIANTIX binary: {BUILD_BINARY}")

    shutil.copy2(BUILD_BINARY, LOCAL_BINARY)

def template_input_files() -> list[Path]:
    """Return the template input files that are replicated for each case."""
    return sorted(
        path
        for path in SCRIPT_DIR.glob("input_*")
        if path.is_file()
    )

def format_q_tag(q_value: float) -> str:
    """Format a plutonium-content tag for directory names."""
    return f"{q_value:.2f}".replace(".", "p")


def q_plot_suffix(q_value: float) -> str:
    """Format the q suffix used in per-q plot filenames."""
    return f"q_{format_q_tag(q_value)}"


def case_dir_path(temperature_k: int, q_value: float) -> Path:
    """Build the directory path for one generated `(temperature, q)` case."""
    return SCRIPT_DIR / f"{temperature_k}K" / f"q_{format_q_tag(q_value)}"


def case_id(temperature_k: int, q_value: float) -> str:
    """Build a compact case identifier stored in the summary table."""
    return f"{temperature_k}K/q_{format_q_tag(q_value)}"


def prepare_case(case_dir: Path, temperature_k: int, q_value: float, input_files: list[Path]) -> None:
    """Create one generated case and copy the template inputs into it."""
    case_dir.mkdir(parents=True, exist_ok=True)

    for source in input_files:
        shutil.copy2(source, case_dir / source.name)

    history_path = case_dir / "input_history.txt"
    updated_lines = []
    for raw_line in history_path.read_text().splitlines():
        stripped = raw_line.strip()
        if not stripped:
            updated_lines.append(raw_line)
            continue

        parts = stripped.split()
        if len(parts) < 2:
            updated_lines.append(raw_line)
            continue

        parts[1] = str(temperature_k)
        updated_lines.append("\t".join(parts))

    history_path.write_text("\n".join(updated_lines) + "\n")

    initial_conditions_path = case_dir / "input_initial_conditions.txt"
    initial_text = initial_conditions_path.read_text()
    initial_text = initial_text.replace("__Q_VALUE__", f"{q_value:.5f}")
    initial_conditions_path.write_text(initial_text)


def run_case(case_dir: Path) -> None:
    """Execute SCIANTIX for one prepared case directory."""
    subprocess.run(
        [str(LOCAL_BINARY), f"{case_dir.relative_to(SCRIPT_DIR).as_posix()}/"],
        cwd=SCRIPT_DIR,
        check=True,
    )


def collect_case(case_dir: Path, temperature_k: int, q_value: float) -> pd.DataFrame:
    """Load a case output and derive the quantities used in the plots."""
    output_path = case_dir / "output.txt"
    if not output_path.exists():
        raise FileNotFoundError(f"Missing output for {case_dir.name}: {output_path}")

    frame = pd.read_csv(output_path, sep="\t")
    frame["Temperature target (K)"] = temperature_k
    frame["q target (-)"] = q_value
    frame["O/U ratio (/)"] = frame["Stoichiometry deviation (/)"] + 2.0
    frame["Case"] = case_id(temperature_k, q_value)
    
    pressure_columns = {
        "SCIANTIX + Kato model": "Fuel oxygen partial pressure - Kato (MPa)",
        "SCIANTIX + OpenCalphad": "Fuel oxygen partial pressure - CALPHAD (MPa)",
    }

    for label, column in pressure_columns.items():
        ratio = frame[column] / REFERENCE_PRESSURE_MPA
        # Log values are only defined for positive pressures.
        frame[f"log10({label} pressure / reference)"] = ratio.where(ratio > 0.0).map(
            lambda value: math.log10(value) if pd.notna(value) else math.nan
        )

    return frame


def style_maps():
    """Create consistent color and line encodings across all plots."""
    cmap = plt.get_cmap("turbo", len(TEMPERATURES_K))
    colors = {temperature_k: cmap(index) for index, temperature_k in enumerate(TEMPERATURES_K)}
    linestyles = {
        "SCIANTIX + Kato model": None,
        "SCIANTIX + OpenCalphad": None,
    }
    markers = {
        "SCIANTIX + Kato model": "^",
        "SCIANTIX + OpenCalphad": "s",
    }
    return colors, linestyles, markers

def add_legends(ax,
                colors: dict[int, object],
                linestyles: dict[str, str],
                markers: dict[str, str],
                temperatures_k: list[int]) -> None:
    """Split the legend into temperature entries and model/source entries."""
    temperature_handles = [
        Line2D([0], [0], color=colors[temperature_k], label=f"{temperature_k} K")
        for temperature_k in temperatures_k
    ]
    source_handles = [
        Line2D(
            [0],
            [0],
            color="black",
            linestyle=linestyles[label] if linestyles[label] is not None else "None",
            marker=markers[label],
            label=label,
        )
        for label in linestyles
    ]

    temperature_legend = ax.legend(
        handles=temperature_handles,
        loc="lower right",
        ncol=2,
        title="Temperature"
    )
    ax.add_artist(temperature_legend)
    ax.legend(handles=source_handles, loc="upper left", title="Model")

def make_pressure_plot(frames: list[pd.DataFrame], q_value: float) -> None:
    """Plot oxygen partial pressure versus O/U ratio for all temperatures."""
    fig, ax = plt.subplots()
    colors, linestyles, markers = style_maps()
    temperatures_k = sorted({int(frame["Temperature (K)"].iloc[0]) for frame in frames})
    pressure_columns = {
        "SCIANTIX + Kato model": "log10(SCIANTIX + Kato model pressure / reference)",
        "SCIANTIX + OpenCalphad": "log10(SCIANTIX + OpenCalphad pressure / reference)",
    }

    for frame in frames:
        temperature_k = int(frame["Temperature (K)"].iloc[0])
        for label, column in pressure_columns.items():
            valid = frame.dropna(subset=[column])
            if valid.empty:
                continue

            ax.scatter(
                valid["O/U ratio (/)"],
                valid[column],
                color=colors[temperature_k],
                marker=markers[label],
            )

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\log_{10}(p_{O_2})$ (bar)")
    ax.grid(True, alpha=0.3)
    add_legends(ax, colors, linestyles, markers, temperatures_k)
    ax.set_xlim([1.95, 2.20])
    ax.set_ylim([-30, 2])
    ax.set_yticks(range(-30, 0, 2))

    fig.tight_layout()
    fig.savefig(SCRIPT_DIR / f"{PRESSURE_PLOT_NAME}_{q_plot_suffix(q_value)}.png")
    plt.close(fig)

    fig, ax = plt.subplots()
    colors, _, _ = style_maps()

    for frame in frames:
        temperature_k = int(frame["Temperature (K)"].iloc[0])
        valid = frame.dropna(
            subset=[
                "O/U ratio (/)",
                "log10(SCIANTIX + OpenCalphad pressure / reference)",
                "log10(SCIANTIX + Kato model pressure / reference)",
            ]
        )
        if valid.empty:
            continue

        ax.scatter(
            valid["O/U ratio (/)"],
            valid["log10(SCIANTIX + Kato model pressure / reference)"] - valid["log10(SCIANTIX + OpenCalphad pressure / reference)"],
            color=colors[temperature_k],
            marker="o",
        )

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\Delta\log_{10}(p_{O_2})$ (bar)")
    ax.grid(True, alpha=0.3)
    temperature_handles = [
        Line2D([0], [0], color=colors[temperature_k], label=f"{temperature_k} K")
        for temperature_k in temperatures_k
    ]
    temperature_legend = ax.legend(
        handles=temperature_handles,
        loc="lower left",
        ncol=2,
        title="Temperature"
    )
    ax.add_artist(temperature_legend)
    ax.set_xlim([1.95, 2.20])

    fig.tight_layout()
    fig.savefig(SCRIPT_DIR / f"{PRESSURE_PLOT_NAME_2}_{q_plot_suffix(q_value)}.png")
    plt.close(fig)

def make_potential_plot(frames: list[pd.DataFrame], q_value: float) -> None:
    """Plot oxygen potential versus O/U ratio for all temperatures."""
    fig, ax = plt.subplots()
    colors, linestyles, markers = style_maps()
    temperatures_k = sorted({int(frame["Temperature (K)"].iloc[0]) for frame in frames})
    potential_columns = {
        "SCIANTIX + Kato model": "Fuel oxygen potential - Kato (KJ/mol)",
        "SCIANTIX + OpenCalphad": "Fuel oxygen potential - CALPHAD (KJ/mol)",
    }

    for frame in frames:
        temperature_k = int(frame["Temperature (K)"].iloc[0])
        for label, column in potential_columns.items():
            if column not in frame.columns:
                continue
            valid = frame.dropna(subset=[column])
            if valid.empty:
                continue

            ax.scatter(
                valid["O/U ratio (/)"],
                valid[column],
                color=colors[temperature_k],
                marker=markers[label],
            )

    ax.set_xlim([1.95, 2.20])
    ax.set_ylim([-1000, 50])
    ax.set_yticks(range(-1000, 100, 100))
    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel("Oxygen potential (kJ/mol)")
    ax.grid(True, alpha=0.3)
    add_legends(ax, colors, linestyles, markers, temperatures_k)

    fig.tight_layout()
    fig.savefig(SCRIPT_DIR / f"{POTENTIAL_PLOT_NAME}_{q_plot_suffix(q_value)}.png")
    plt.close(fig)



def run_comparison() -> None:
    """Run post-processing comparisons against Kato and standalone OpenCalphad."""
    subprocess.run(
        ["python3", str(COMPARE_SCRIPT)],
        cwd=SCRIPT_DIR,
        check=True,
    )
    if COMPARE_OC_SCRIPT.exists():
        subprocess.run(
            ["python3", str(COMPARE_OC_SCRIPT)],
            cwd=SCRIPT_DIR,
            check=True,
        )


def cleanup_generated_cases(temperatures_k: list[int]) -> None:
    """Remove generated case directories after a successful run, if requested."""
    for temperature_k in temperatures_k:
        temperature_dir = SCRIPT_DIR / f"{temperature_k}K"
        if temperature_dir.exists():
            shutil.rmtree(temperature_dir)


def cleanup_generated_artifacts() -> None:
    """Keep only compact metrics and selected verification plots."""
    verification_dir = SCRIPT_DIR / "sciantix_verification"
    plots_dir = SCRIPT_DIR / "plots"
    plots_dir.mkdir(parents=True, exist_ok=True)

    metric_files_to_keep = {
        verification_dir / "sciantix_vs_kato_residuals.tsv",
        verification_dir / "sciantix_vs_kato_summary.txt",
        verification_dir / "sciantix_vs_oc_csv_summary.tsv",
        verification_dir / "sciantix_vs_oc_csv_summary.txt",
    }
    files_to_remove = [
        verification_dir / "kato_explicit_comparison.tsv",
        verification_dir / "kato_explicit_points.tsv",
        verification_dir / "kato_explicit_residuals.tsv",
        verification_dir / "kato_explicit_summary.txt",
        verification_dir / "kato_solver_comparison.tsv",
        verification_dir / "kato_solver_residuals.tsv",
        verification_dir / "sciantix_vs_kato_comparison.tsv",
        verification_dir / "sciantix_vs_kato_points.tsv",
        verification_dir / "sciantix_vs_oc_csv_comparison.tsv",
    ]
    for path in files_to_remove:
        if path.exists() and path not in metric_files_to_keep:
            path.unlink()

    selected_plot_prefixes = (
        "fuel_oxygen_partial_pressure_vs_om_ratio_kato_q_",
        "fuel_oxygen_partial_pressure_error_vs_om_ratio_kato_q_",
        "fuel_oxygen_partial_pressure_error_absolute_vs_om_ratio_kato_q_",
        "fuel_oxygen_partial_pressure_relative_error_vs_om_ratio_kato_q_",
        "sciantix_vs_oc_csv_partial_pressure_q_",
        "sciantix_vs_oc_csv_log_pO2_error_q_",
        "sciantix_vs_oc_csv_log_pO2_error_absolute_q_",
        "sciantix_vs_oc_csv_log_pO2_error_relative_percent_q_",
    )

    for png_path in SCRIPT_DIR.glob("*.png"):
        if png_path.name.startswith(selected_plot_prefixes):
            destination = plots_dir / png_path.name
            if destination.exists():
                destination.unlink()
            shutil.move(str(png_path), str(destination))


def main() -> None:
    """Entry point for the MOX Kato verification workflow."""
    args = parse_args()
    temperatures_k = parse_int_list(args.temperatures)
    q_values = parse_float_list(args.q_values)

    collected_frames: list[pd.DataFrame] = []
    if not args.plot_only:
        ensure_local_binary()
        input_files = template_input_files()

        for temperature_k in temperatures_k:
            for q_value in q_values:
                current_case = case_dir_path(temperature_k, q_value)
                prepare_case(current_case, temperature_k, q_value, input_files)
                run_case(current_case)
                collected_frames.append(collect_case(current_case, temperature_k, q_value))
    else:
        for temperature_k in temperatures_k:
            for q_value in q_values:
                current_case = case_dir_path(temperature_k, q_value)
                collected_frames.append(collect_case(current_case, temperature_k, q_value))

    summary = pd.concat(collected_frames, ignore_index=True)
    summary.to_csv(SUMMARY_PATH, sep="\t", index=False)

    q_to_frames: dict[float, list[pd.DataFrame]] = {}
    for frame in collected_frames:
        q_case = float(frame["q target (-)"].iloc[0])
        q_to_frames.setdefault(q_case, []).append(frame)

    for q_value in sorted(q_to_frames):
        make_pressure_plot(q_to_frames[q_value], q_value)
        make_potential_plot(q_to_frames[q_value], q_value)

    run_comparison()

    if not args.plot_only and not args.keep_cases:
        cleanup_generated_cases(temperatures_k)

    cleanup_generated_artifacts()


if __name__ == "__main__":
    main()
