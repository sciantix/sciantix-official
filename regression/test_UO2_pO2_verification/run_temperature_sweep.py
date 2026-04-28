from __future__ import annotations

"""Run the UO2 oxygen-potential sweep and collect SCIANTIX outputs.

The script clones the template input files in this folder into one case
directory per temperature, updates the imposed temperature in
``input_history.txt``, runs the local ``sciantix.x`` executable, and builds:

- ``temperature_sweep_summary.tsv`` with all cases concatenated
- a partial-pressure comparison plot
- an oxygen-potential comparison plot
"""

import math
import os
import shutil
import subprocess
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
os.environ.setdefault("MPLCONFIGDIR", str(SCRIPT_DIR / ".matplotlib"))

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
BUILD_BINARY = SCRIPT_DIR.parent.parent / "build" / "sciantix.x"
LOCAL_BINARY = SCRIPT_DIR / "sciantix.x"
SUMMARY_PATH = SCRIPT_DIR / "temperature_sweep_summary.tsv"
PRESSURE_PLOT_PATH = SCRIPT_DIR / "fuel_oxygen_partial_pressures_OC_Blackburn_ML.png"
PRESSURE_PAIR_PLOT_PATHS = {
    ("OpenCalphad", "Blackburn"): SCRIPT_DIR / "fuel_oxygen_partial_pressures_OC_vs_Blackburn.png",
    ("OpenCalphad", "ML"): SCRIPT_DIR / "fuel_oxygen_partial_pressures_OC_vs_ML.png",
    ("Blackburn", "ML"): SCRIPT_DIR / "fuel_oxygen_partial_pressures_Blackburn_vs_ML.png",
}
POTENTIAL_PLOT_PATH = SCRIPT_DIR / "fuel_oxygen_potentials_vs_ou_ratio.png"

PRESSURE_MODEL_COLUMNS = {
    "Blackburn": "log10(SCIANTIX + Blackburn model pressure / reference)",
    "ML": "log10(SCIANTIX + ML model pressure / reference)",
    "OpenCalphad": "log10(SCIANTIX + OpenCalphad pressure / reference)",
}

PRESSURE_MODEL_STYLES = {
    "OpenCalphad": {"linestyle": "-", "marker": None, "linewidth": 2.4, "alpha": 0.95},
    "Blackburn": {"linestyle": "--", "marker": "^", "linewidth": 1.8, "alpha": 0.90},
    "ML": {"linestyle": ":", "marker": "D", "linewidth": 2.2, "alpha": 0.95},
}

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

def prepare_case(case_dir: Path, temperature_k: int, input_files: list[Path]) -> None:
    """Populate one temperature case and overwrite its prescribed temperature."""
    case_dir.mkdir(exist_ok=True)

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

def run_case(case_dir: Path) -> None:
    """Execute SCIANTIX for one prepared case directory."""
    try:
        subprocess.run(
            [str(LOCAL_BINARY), f"{case_dir.name}/"],
            cwd=SCRIPT_DIR,
            check=True,
        )
    finally:
        remove_execution_file(case_dir)

def remove_execution_file(case_dir: Path) -> None:
    """Remove the transient timing file so reruns do not dirty the worktree."""
    execution_path = case_dir / "execution.txt"
    if execution_path.exists():
        execution_path.unlink()
        print(f"Removed transient timing file: {execution_path}", flush=True)

def collect_case(case_dir: Path) -> pd.DataFrame:
    """Load a case output and derive the quantities used in the plots."""
    output_path = case_dir / "output.txt"
    if not output_path.exists():
        raise FileNotFoundError(f"Missing output for {case_dir.name}: {output_path}")

    frame = pd.read_csv(output_path, sep="\t")
    frame = frame.loc[:, ~frame.columns.str.startswith("Unnamed:")]
    frame["O/U ratio (/)"] = frame["Stoichiometry deviation (/)"] + 2.0

    pressure_columns = {
        "SCIANTIX + Blackburn model": "Fuel oxygen partial pressure - Blackburn (MPa)",
        "SCIANTIX + ML model": "Fuel oxygen partial pressure - ML (MPa)",
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
        "SCIANTIX + Blackburn model": None,
        "SCIANTIX + ML model": None,
        "SCIANTIX + OpenCalphad": None,
    }
    markers = {
        "SCIANTIX + Blackburn model": "^",
        "SCIANTIX + ML model": "D",
        "SCIANTIX + OpenCalphad": "s",
    }
    return colors, linestyles, markers

def add_legends(ax, colors: dict[int, object], linestyles: dict[str, str], markers: dict[str, str]) -> None:
    """Split the legend into temperature entries and model/source entries."""
    temperature_handles = [
        Line2D([0], [0], color=colors[temperature_k], label=f"{temperature_k} K")
        for temperature_k in TEMPERATURES_K
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

def add_temperature_legend(ax, colors: dict[int, object], loc: str = "lower right") -> None:
    """Add a compact temperature legend."""
    temperature_handles = [
        Line2D([0], [0], color=colors[temperature_k], lw=2, label=f"{temperature_k} K")
        for temperature_k in TEMPERATURES_K
    ]
    temperature_legend = ax.legend(handles=temperature_handles, loc=loc, ncol=2, title="Temperature")
    ax.add_artist(temperature_legend)


def add_pressure_model_legend(ax, models: tuple[str, ...], loc: str = "upper left") -> None:
    """Add a model legend using only black handles, keeping colors for temperature."""
    model_handles = [
        Line2D(
            [0],
            [0],
            color="black",
            linestyle=PRESSURE_MODEL_STYLES[model]["linestyle"],
            marker=PRESSURE_MODEL_STYLES[model]["marker"],
            linewidth=PRESSURE_MODEL_STYLES[model]["linewidth"],
            label=model,
        )
        for model in models
    ]
    ax.legend(handles=model_handles, loc=loc, title="Model")


def plot_pressure_model(ax, frame: pd.DataFrame, model: str, color: object) -> None:
    """Draw one model for one temperature."""
    column = PRESSURE_MODEL_COLUMNS[model]
    valid = frame.dropna(subset=[column]).sort_values("O/U ratio (/)")
    if valid.empty:
        return

    style = PRESSURE_MODEL_STYLES[model]
    marker = style["marker"]
    markevery = max(1, len(valid) // 12) if marker is not None else None
    ax.plot(
        valid["O/U ratio (/)"],
        valid[column],
        color=color,
        linestyle=style["linestyle"],
        marker=marker,
        markevery=markevery,
        linewidth=style["linewidth"],
        alpha=style["alpha"],
    )


def format_pressure_axis(ax, title: str) -> None:
    """Apply shared axis formatting for pO2 plots."""
    ax.set_title(title)
    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\log_{10}(p_{O_2})$ (bar)")
    ax.set_xlim([1.90, 2.20])
    ax.set_ylim([-30, 0])
    ax.set_yticks(range(-30, 2, 2))
    ax.grid(True, alpha=0.3)


def make_pressure_pair_plot(
    frames: list[pd.DataFrame],
    colors: dict[int, object],
    first_model: str,
    second_model: str,
    output_path: Path,
) -> None:
    """Plot a cleaner two-model pO2 comparison."""
    fig, ax = plt.subplots(figsize=(10, 6.5))
    models = (first_model, second_model)

    for frame in frames:
        temperature_k = int(frame["Temperature (K)"].iloc[0])
        for model in models:
            plot_pressure_model(ax, frame, model, colors[temperature_k])

    format_pressure_axis(ax, f"{first_model} vs {second_model}")
    add_temperature_legend(ax, colors, loc="lower right")
    add_pressure_model_legend(ax, models, loc="upper left")

    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)


def make_pressure_plot(frames: list[pd.DataFrame]) -> None:
    """Plot oxygen partial pressure comparisons versus O/U ratio."""
    colors, _, _ = style_maps()

    for models, output_path in PRESSURE_PAIR_PLOT_PATHS.items():
        make_pressure_pair_plot(frames, colors, models[0], models[1], output_path)

    models = ("OpenCalphad", "Blackburn", "ML")
    fig, axes = plt.subplots(len(models), 1, figsize=(10, 12), sharex=True, sharey=True)
    for ax, model in zip(axes, models):
        for frame in frames:
            temperature_k = int(frame["Temperature (K)"].iloc[0])
            plot_pressure_model(ax, frame, model, colors[temperature_k])

        format_pressure_axis(ax, model)

    axes[-1].set_xlabel("O/U ratio (-)")
    for ax in axes[:-1]:
        ax.set_xlabel("")

    temperature_handles = [
        Line2D([0], [0], color=colors[temperature_k], lw=2, label=f"{temperature_k} K")
        for temperature_k in TEMPERATURES_K
    ]
    axes[-1].legend(handles=temperature_handles, loc="lower right", ncol=2, title="Temperature")

    fig.tight_layout()
    fig.savefig(PRESSURE_PLOT_PATH)
    plt.close(fig)

def make_potential_plot(frames: list[pd.DataFrame]) -> None:
    """Plot oxygen potential versus O/U ratio for all temperatures."""
    fig, ax = plt.subplots()
    colors, linestyles, markers = style_maps()
    potential_columns = {
        "SCIANTIX + Blackburn model": "Fuel oxygen potential - Blackburn (KJ/mol)",
        "SCIANTIX + ML model": "Fuel oxygen potential - ML (KJ/mol)",
        "SCIANTIX + OpenCalphad": "Fuel oxygen potential - CALPHAD (KJ/mol)",
    }

    for frame in frames:
        temperature_k = int(frame["Temperature (K)"].iloc[0])
        for label, column in potential_columns.items():
            valid = frame.dropna(subset=[column])
            if valid.empty:
                continue

            if label == "SCIANTIX + OpenCalphad":
                ax.plot(
                    valid["O/U ratio (/)"],
                    valid[column],
                    color=colors[temperature_k],
                    linestyle="-",
                    marker=markers[label],
                )
            else:
                ax.scatter(
                    valid["O/U ratio (/)"],
                    valid[column],
                    color=colors[temperature_k],
                    marker=markers[label],
                )

    ax.set_xlim([1.90, 2.20])
    ax.set_ylim([-1000, 0])
    ax.set_yticks(range(-1000, 100, 100))
    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel("Oxygen potential (kJ/mol)")
    ax.grid(True, alpha=0.3)
    add_legends(ax, colors, linestyles, markers)

    fig.tight_layout()
    fig.savefig(POTENTIAL_PLOT_PATH)
    plt.close(fig)


def main() -> None:
    """Run the complete temperature sweep from 800 K to 2700 K."""
    ensure_local_binary()
    input_files = template_input_files()
    frames: list[pd.DataFrame] = []

    for temperature_k in TEMPERATURES_K:
        case_dir = SCRIPT_DIR / f"{temperature_k}K"
        prepare_case(case_dir, temperature_k, input_files)
        run_case(case_dir)
        frames.append(collect_case(case_dir))

    combined = pd.concat(frames, ignore_index=True)
    combined.to_csv(SUMMARY_PATH, sep="\t", index=False)
    make_pressure_plot(frames)
    make_potential_plot(frames)


if __name__ == "__main__":
    main()
