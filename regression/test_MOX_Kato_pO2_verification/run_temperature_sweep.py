from __future__ import annotations

"""Run the MOX Kato verification sweep and collect SCIANTIX outputs.

The workflow mirrors the UO2 pO2 verification layout as closely as possible:
- one case directory per temperature and plutonium content q
- one full SCIANTIX transient per case
- one concatenated summary table for the downstream comparison script
"""

import argparse
import shutil
import subprocess
from pathlib import Path

import pandas as pd


DEFAULT_TEMPERATURES_K = [800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600]
DEFAULT_Q_VALUES = [0.10, 0.15, 0.20, 0.25, 0.30, 0.35]

SCRIPT_DIR = Path(__file__).resolve().parent
BUILD_BINARY = SCRIPT_DIR.parent.parent / "build" / "sciantix.x"
LOCAL_BINARY = SCRIPT_DIR / "sciantix.x"
SUMMARY_PATH = SCRIPT_DIR / "temperature_sweep_summary.tsv"
LEGACY_SUMMARY_PATH = SCRIPT_DIR / "kato_sweep_summary.tsv"
COMPARE_SCRIPT = SCRIPT_DIR / "sciantix_verification" / "compare_sciantix_with_kato.py"


def parse_args() -> argparse.Namespace:
    """Parse the command-line arguments used by the MOX verification sweep."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--plot-only", action="store_true", help="Skip case generation and rerun only the comparison plots.")
    parser.add_argument("--keep-cases", action="store_true", help="Keep generated case folders after the comparison has completed.")
    parser.add_argument("--temperatures", default=",".join(str(value) for value in DEFAULT_TEMPERATURES_K))
    parser.add_argument("--q-values", default=",".join(f"{value:.2f}" for value in DEFAULT_Q_VALUES))
    return parser.parse_args()


def parse_int_list(text: str) -> list[int]:
    """Parse a comma-separated list of integers."""
    return [int(item.strip()) for item in text.split(",") if item.strip()]


def parse_float_list(text: str) -> list[float]:
    """Parse a comma-separated list of floating-point values."""
    return [float(item.strip()) for item in text.split(",") if item.strip()]


def ensure_local_binary() -> None:
    """Copy the up-to-date SCIANTIX executable into the verification folder."""
    if not BUILD_BINARY.exists():
        raise FileNotFoundError(f"Missing SCIANTIX binary: {BUILD_BINARY}")

    shutil.copy2(BUILD_BINARY, LOCAL_BINARY)


def template_input_files() -> list[Path]:
    """Return the template input files replicated into each generated case."""
    return sorted(
        path
        for path in SCRIPT_DIR.glob("input_*")
        if path.is_file()
    )


def format_q_tag(q_value: float) -> str:
    """Format a plutonium-content tag for directory names."""
    return f"{q_value:.2f}".replace(".", "p")


def case_dir_path(temperature_k: int, q_value: float) -> Path:
    """Build the directory path for one generated `(temperature, q)` case."""
    return SCRIPT_DIR / f"{temperature_k}K" / f"q_{format_q_tag(q_value)}"


def case_id(temperature_k: int, q_value: float) -> str:
    """Build a compact case identifier stored in the summary table."""
    return f"{temperature_k}K/q_{format_q_tag(q_value)}"


def rewrite_case_files(case_dir: Path, temperature_k: int, q_value: float) -> None:
    """Inject the selected temperature and q value into one prepared case."""
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


def prepare_case(case_dir: Path, temperature_k: int, q_value: float, input_files: list[Path]) -> None:
    """Create one generated case and copy the template inputs into it."""
    case_dir.mkdir(parents=True, exist_ok=True)

    for source in input_files:
        shutil.copy2(source, case_dir / source.name)

    rewrite_case_files(case_dir, temperature_k, q_value)


def run_case(case_dir: Path) -> None:
    """Execute SCIANTIX for one generated case directory."""
    subprocess.run(
        [str(LOCAL_BINARY), f"{case_dir.relative_to(SCRIPT_DIR).as_posix()}/"],
        cwd=SCRIPT_DIR,
        check=True,
    )


def collect_case(case_dir: Path, temperature_k: int, q_value: float) -> pd.DataFrame:
    """Load one case output and append the sweep identifiers used downstream."""
    output_path = case_dir / "output.txt"
    if not output_path.exists():
        raise FileNotFoundError(f"Missing output for {case_dir.name}: {output_path}")

    frame = pd.read_csv(output_path, sep="\t")
    frame["Temperature target (K)"] = temperature_k
    frame["q target (-)"] = q_value
    frame["O/M target (/)"] = frame["Stoichiometry deviation (/)"] + 2.0
    frame["Case"] = case_id(temperature_k, q_value)
    return frame


def build_summary(temperatures_k: list[int], q_values: list[float]) -> None:
    """Generate all cases, run them, and concatenate their full time histories."""
    ensure_local_binary()
    input_files = template_input_files()

    collected_frames: list[pd.DataFrame] = []
    for temperature_k in temperatures_k:
        for q_value in q_values:
            current_case = case_dir_path(temperature_k, q_value)
            prepare_case(current_case, temperature_k, q_value, input_files)
            run_case(current_case)
            collected_frames.append(collect_case(current_case, temperature_k, q_value))

    summary = pd.concat(collected_frames, ignore_index=True)
    summary.to_csv(SUMMARY_PATH, sep="\t", index=False)
    summary.to_csv(LEGACY_SUMMARY_PATH, sep="\t", index=False)


def run_comparison() -> None:
    """Run the post-processing script that compares SCIANTIX with the Kato reference."""
    subprocess.run(
        ["python3", str(COMPARE_SCRIPT)],
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
    """Keep only the compact metrics and the selected signed plots."""
    verification_dir = SCRIPT_DIR / "sciantix_verification"
    plots_dir = SCRIPT_DIR / "plots"
    plots_dir.mkdir(parents=True, exist_ok=True)

    metric_files_to_keep = {
        verification_dir / "sciantix_vs_kato_residuals.tsv",
        verification_dir / "sciantix_vs_kato_summary.txt",
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
        LEGACY_SUMMARY_PATH,
    ]
    for path in files_to_remove:
        if path.exists() and path not in metric_files_to_keep:
            path.unlink()

    selected_plot_prefixes = (
        "fuel_oxygen_partial_pressure_vs_om_ratio_kato_q_",
        "fuel_oxygen_partial_pressure_error_vs_om_ratio_kato_q_",
        "fuel_oxygen_partial_pressure_relative_error_vs_om_ratio_kato_q_",
        "fuel_oxygen_potential_vs_om_ratio_kato_q_",
    )

    for png_path in SCRIPT_DIR.glob("*.png"):
        if png_path.name.startswith(selected_plot_prefixes):
            destination = plots_dir / png_path.name
            if destination.exists():
                destination.unlink()
            shutil.move(str(png_path), str(destination))
        else:
            png_path.unlink()

    for png_path in plots_dir.glob("*.png"):
        if not png_path.name.startswith(selected_plot_prefixes):
            png_path.unlink()


def main() -> None:
    """Entry point for the MOX Kato verification workflow."""
    args = parse_args()
    temperatures_k = parse_int_list(args.temperatures)
    q_values = parse_float_list(args.q_values)

    if not args.plot_only:
        build_summary(temperatures_k, q_values)

    run_comparison()

    if not args.plot_only and not args.keep_cases:
        cleanup_generated_cases(temperatures_k)

    cleanup_generated_artifacts()


if __name__ == "__main__":
    main()
