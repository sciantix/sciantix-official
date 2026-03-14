from __future__ import annotations

import math
import shutil
import subprocess
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import pandas as pd


TEMPERATURES_K = list(range(800, 2800, 100))
REFERENCE_PRESSURE_MPA = 0.1013
SCRIPT_DIR = Path(__file__).resolve().parent
BUILD_BINARY = SCRIPT_DIR.parent.parent / "build" / "sciantix.x"
LOCAL_BINARY = SCRIPT_DIR / "sciantix.x"
SUMMARY_PATH = SCRIPT_DIR / "temperature_sweep_summary.tsv"
PRESSURE_PLOT_PATH = SCRIPT_DIR / "fuel_oxygen_partial_pressures_vs_ou_ratio.png"
POTENTIAL_PLOT_PATH = SCRIPT_DIR / "fuel_oxygen_potentials_vs_ou_ratio.png"


def ensure_local_binary() -> None:
    if not BUILD_BINARY.exists():
        raise FileNotFoundError(f"Missing SCIANTIX binary: {BUILD_BINARY}")

    shutil.copy2(BUILD_BINARY, LOCAL_BINARY)


def template_input_files() -> list[Path]:
    return sorted(
        path
        for path in SCRIPT_DIR.glob("input_*")
        if path.is_file()
    )


def prepare_case(case_dir: Path, temperature_k: int, input_files: list[Path]) -> None:
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
    subprocess.run(
        [str(LOCAL_BINARY), f"{case_dir.name}/"],
        cwd=SCRIPT_DIR,
        check=True,
    )


def collect_case(case_dir: Path) -> pd.DataFrame:
    output_path = case_dir / "output.txt"
    if not output_path.exists():
        raise FileNotFoundError(f"Missing output for {case_dir.name}: {output_path}")

    frame = pd.read_csv(output_path, sep="\t")
    frame["Temperature (K)"] = frame["Temperature (K)"]
    frame["O/U ratio (/)"] = frame["Stoichiometry deviation (/)"] + 2.0

    pressure_columns = {
        "Final": "Fuel oxygen partial pressure (MPa)",
        "Blackburn": "Fuel oxygen partial pressure - Blackburn (MPa)",
        "CALPHAD": "Fuel oxygen partial pressure - CALPHAD (MPa)",
    }

    for label, column in pressure_columns.items():
        ratio = frame[column] / REFERENCE_PRESSURE_MPA
        frame[f"log10({label} pressure / reference)"] = ratio.where(ratio > 0.0).map(
            lambda value: math.log10(value) if pd.notna(value) else math.nan
        )

    print(frame)

    return frame


def style_maps():
    cmap = plt.get_cmap("viridis", len(TEMPERATURES_K))
    colors = {temperature_k: cmap(index) for index, temperature_k in enumerate(TEMPERATURES_K)}
    linestyles = {
        "Final": "-",
        "Blackburn": "--",
        "CALPHAD": ":",
    }
    markers = {
        "Final": "o",
        "Blackburn": "^",
        "CALPHAD": "s",
    }
    return colors, linestyles, markers


def add_legends(ax, colors: dict[int, object], linestyles: dict[str, str], markers: dict[str, str]) -> None:
    temperature_handles = [
        Line2D([0], [0], color=colors[temperature_k], lw=2, label=f"{temperature_k} K")
        for temperature_k in TEMPERATURES_K
    ]
    source_handles = [
        Line2D(
            [0],
            [0],
            color="black",
            lw=2,
            linestyle=linestyles[label],
            marker=markers[label],
            markersize=5,
            label=label,
        )
        for label in linestyles
    ]

    temperature_legend = ax.legend(
        handles=temperature_handles,
        loc="upper left",
        ncol=2,
        fontsize=8,
        title="Temperature",
    )
    ax.add_artist(temperature_legend)
    ax.legend(handles=source_handles, loc="lower right", title="Model")


def make_pressure_plot(frames: list[pd.DataFrame]) -> None:
    fig, ax = plt.subplots(figsize=(10, 7))
    colors, linestyles, markers = style_maps()
    pressure_columns = {
        "Final": "log10(Final pressure / reference)",
        "Blackburn": "log10(Blackburn pressure / reference)",
        "CALPHAD": "log10(CALPHAD pressure / reference)",
    }

    for frame in frames:
        temperature_k = int(frame["Temperature (K)"].iloc[0])
        for label, column in pressure_columns.items():
            valid = frame.dropna(subset=[column])
            if valid.empty:
                continue

            ax.plot(
                valid["O/U ratio (/)"],
                valid[column],
                color=colors[temperature_k],
                linestyle=linestyles[label],
                linewidth=1.8,
                marker=markers[label],
                markersize=4,
            )

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\log_{10}(p_{O_2})$ (bar)")
    ax.grid(True, alpha=0.3)
    add_legends(ax, colors, linestyles, markers)
    ax.set_ylim([-22, -2]) # Gueneau paper

    fig.tight_layout()
    fig.savefig(PRESSURE_PLOT_PATH, dpi=300)
    plt.close(fig)


def make_potential_plot(frames: list[pd.DataFrame]) -> None:
    fig, ax = plt.subplots(figsize=(10, 7))
    colors, linestyles, markers = style_maps()
    potential_columns = {
        "Final": "Fuel oxygen potential (KJ/mol)",
        "Blackburn": "Fuel oxygen potential - Blackburn (KJ/mol)",
        "CALPHAD": "Fuel oxygen potential - CALPHAD (KJ/mol)",
    }

    for frame in frames:
        temperature_k = int(frame["Temperature (K)"].iloc[0])
        for label, column in potential_columns.items():
            valid = frame.dropna(subset=[column])
            if valid.empty:
                continue

            ax.plot(
                valid["O/U ratio (/)"],
                valid[column],
                color=colors[temperature_k],
                linestyle=linestyles[label],
                linewidth=1.8,
                marker=markers[label],
                markersize=4,
            )

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel("Oxygen potential (kJ/mol)")
    ax.grid(True, alpha=0.3)
    add_legends(ax, colors, linestyles, markers)

    fig.tight_layout()
    fig.savefig(POTENTIAL_PLOT_PATH, dpi=300)
    plt.close(fig)


def main() -> None:
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
