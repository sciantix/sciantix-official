from __future__ import annotations

"""Run the MOX Kato solver verification sweep and collect SCIANTIX outputs."""

import argparse
import shutil
import subprocess
from pathlib import Path

import pandas as pd


DEFAULT_TEMPERATURES_K = [800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600]
DEFAULT_Q_VALUES = [0.10, 0.15, 0.20, 0.25, 0.30, 0.35]
# Common O/M interval covered by the explicit Kato correlation for all default (T, q) combinations.
DEFAULT_OM_VALUES = [value / 1000.0 for value in range(1960, 2080, 5)]

SCRIPT_DIR = Path(__file__).resolve().parent
BUILD_BINARY = SCRIPT_DIR.parent.parent / "build" / "sciantix.x"
LOCAL_BINARY = SCRIPT_DIR / "sciantix.x"
CASE_ROOT = SCRIPT_DIR / "cases"
SUMMARY_PATH = SCRIPT_DIR / "kato_sweep_summary.tsv"
COMPARE_SCRIPT = SCRIPT_DIR / "sciantix_verification" / "compare_sciantix_with_kato.py"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--plot-only", action="store_true", help="Skip case generation and rerun only the comparison plots.")
    parser.add_argument("--keep-cases", action="store_true", help="Keep generated case folders after the comparison has completed.")
    parser.add_argument("--temperatures", default=",".join(str(value) for value in DEFAULT_TEMPERATURES_K))
    parser.add_argument("--q-values", default=",".join(f"{value:.2f}" for value in DEFAULT_Q_VALUES))
    parser.add_argument("--om-values", default=",".join(f"{value:.2f}" for value in DEFAULT_OM_VALUES))
    return parser.parse_args()


def parse_int_list(text: str) -> list[int]:
    return [int(item.strip()) for item in text.split(",") if item.strip()]


def parse_float_list(text: str) -> list[float]:
    return [float(item.strip()) for item in text.split(",") if item.strip()]


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


def format_q_tag(q_value: float) -> str:
    return f"{q_value:.2f}".replace(".", "p")


def format_om_tag(om_value: float) -> str:
    return f"{om_value:.3f}".replace(".", "p")


def case_name(temperature_k: int, q_value: float, om_value: float) -> str:
    return f"T{temperature_k}K_q{format_q_tag(q_value)}_om{format_om_tag(om_value)}"


def rewrite_case_files(case_dir: Path, temperature_k: int, q_value: float, om_value: float) -> None:
    history_path = case_dir / "input_history.txt"
    history_text = history_path.read_text()
    history_text = history_text.replace("__TEMPERATURE__", str(temperature_k))
    history_text = history_text.replace("__OM_RATIO__", f"{om_value:.5f}")
    history_path.write_text(history_text)

    initial_conditions_path = case_dir / "input_initial_conditions.txt"
    initial_text = initial_conditions_path.read_text()
    initial_text = initial_text.replace("__Q_VALUE__", f"{q_value:.5f}")
    initial_conditions_path.write_text(initial_text)


def prepare_case(case_dir: Path, temperature_k: int, q_value: float, om_value: float, input_files: list[Path]) -> None:
    case_dir.mkdir(parents=True, exist_ok=True)

    for source in input_files:
        shutil.copy2(source, case_dir / source.name)

    rewrite_case_files(case_dir, temperature_k, q_value, om_value)


def run_case(case_dir: Path) -> None:
    subprocess.run(
        [str(LOCAL_BINARY), f"{case_dir.relative_to(SCRIPT_DIR).as_posix()}/"],
        cwd=SCRIPT_DIR,
        check=True,
    )


def collect_case(case_dir: Path, temperature_k: int, q_value: float, om_value: float) -> pd.DataFrame:
    output_path = case_dir / "output.txt"
    if not output_path.exists():
        raise FileNotFoundError(f"Missing output for {case_dir.name}: {output_path}")

    frame = pd.read_csv(output_path, sep="\t")
    final_row = frame.tail(1).copy()
    final_row["Temperature target (K)"] = temperature_k
    final_row["q target (-)"] = q_value
    final_row["O/M target (/)"] = om_value
    final_row["Case"] = case_dir.name
    return final_row


def build_summary(temperatures_k: list[int], q_values: list[float], om_values: list[float]) -> None:
    ensure_local_binary()
    input_files = template_input_files()
    CASE_ROOT.mkdir(exist_ok=True)

    collected_frames: list[pd.DataFrame] = []
    for q_value in q_values:
        for temperature_k in temperatures_k:
            for om_value in om_values:
                current_case = CASE_ROOT / case_name(temperature_k, q_value, om_value)
                prepare_case(current_case, temperature_k, q_value, om_value, input_files)
                run_case(current_case)
                collected_frames.append(collect_case(current_case, temperature_k, q_value, om_value))

    summary = pd.concat(collected_frames, ignore_index=True)
    summary.to_csv(SUMMARY_PATH, sep="\t", index=False)


def run_comparison() -> None:
    subprocess.run(
        ["python3", str(COMPARE_SCRIPT)],
        cwd=SCRIPT_DIR,
        check=True,
    )


def cleanup_generated_cases() -> None:
    if CASE_ROOT.exists():
        shutil.rmtree(CASE_ROOT)


def main() -> None:
    args = parse_args()
    temperatures_k = parse_int_list(args.temperatures)
    q_values = parse_float_list(args.q_values)
    om_values = parse_float_list(args.om_values)

    if not args.plot_only:
        build_summary(temperatures_k, q_values, om_values)

    run_comparison()

    if not args.plot_only and not args.keep_cases:
        cleanup_generated_cases()

    plots = SCRIPT_DIR / "plots"
    plots.mkdir(parents=True, exist_ok=True)

    for png_path in SCRIPT_DIR.glob("*.png"):
        shutil.move(str(png_path), plots / png_path.name)


if __name__ == "__main__":
    main()
