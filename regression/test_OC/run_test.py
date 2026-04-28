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
PROJECT_DIR = TEST_DIR.parents[1]
BUILD_EXECUTABLE = PROJECT_DIR / "build" / "sciantix.x"
LOCAL_EXECUTABLE = TEST_DIR / "sciantix.x"
OUTPUT_FILE = TEST_DIR / "output.txt"
THERMOCHEMISTRY_OUTPUT_FILE = TEST_DIR / "thermochemistry_output.txt"
EXECUTION_FILE = TEST_DIR / "execution.txt"
PLOT_FILE = TEST_DIR / "po2.png"
OUTPUT_GOLD_FILE = TEST_DIR / "output_gold.txt"
THERMOCHEMISTRY_OUTPUT_GOLD_FILE = TEST_DIR / "thermochemistry_output_gold.txt"
GOLD_REL_TOL = 0
GOLD_ABS_TOL = 0


def run_command(command: list[str], cwd: Path) -> None:
    print(f"Running in {cwd}: {' '.join(command)}", flush=True)
    subprocess.run(command, cwd=cwd, check=True)


def find_allmake_script() -> Path:
    for name in ("allmake", "Allmake", "Allmake.sh"):
        candidate = PROJECT_DIR / name
        if candidate.is_file():
            return candidate

    raise FileNotFoundError(f"No allmake script found in {PROJECT_DIR}")


def run_allmake() -> None:
    allmake_script = find_allmake_script()
    run_command([f"./{allmake_script.name}"], PROJECT_DIR)


def copy_sciantix_executable() -> Path:
    if not BUILD_EXECUTABLE.is_file():
        raise FileNotFoundError(f"SCIANTIX executable not found: {BUILD_EXECUTABLE}")

    shutil.copy2(BUILD_EXECUTABLE, LOCAL_EXECUTABLE)
    LOCAL_EXECUTABLE.chmod(LOCAL_EXECUTABLE.stat().st_mode | 0o111)
    print(f"Copied: {BUILD_EXECUTABLE} -> {LOCAL_EXECUTABLE}", flush=True)
    return LOCAL_EXECUTABLE


def run_sciantix() -> None:
    if not LOCAL_EXECUTABLE.is_file():
        raise FileNotFoundError(f"Local SCIANTIX executable not found: {LOCAL_EXECUTABLE}")

    try:
        run_command([f"./{LOCAL_EXECUTABLE.name}"], TEST_DIR)
    finally:
        remove_execution_file()


def remove_execution_file() -> None:
    if EXECUTION_FILE.exists():
        EXECUTION_FILE.unlink()
        print(f"Removed transient timing file: {EXECUTION_FILE}", flush=True)


def load_columns(output_file: Path, x_label: str, y_label: str) -> tuple[np.ndarray, np.ndarray]:
    with output_file.open(newline="") as handle:
        reader = csv.reader(handle, delimiter="\t")
        rows = [[cell.strip() for cell in row if cell.strip()] for row in reader]

    if len(rows) < 2:
        raise ValueError(f"Not enough data in {output_file}")

    headers = rows[0]
    try:
        x_index = headers.index(x_label)
        y_index = headers.index(y_label)
    except ValueError as exc:
        raise ValueError(f"Required column not found in {output_file}: {exc}") from exc

    values = np.array(rows[1:], dtype=float)
    return values[:, x_index], values[:, y_index]


def load_table(output_file: Path) -> tuple[list[str], np.ndarray]:
    with output_file.open(newline="") as handle:
        reader = csv.reader(handle, delimiter="\t")
        rows = [[cell.strip() for cell in row if cell.strip()] for row in reader]

    if len(rows) < 2:
        raise ValueError(f"Not enough data in {output_file}")

    headers = rows[0]
    values = np.array(rows[1:], dtype=float)
    if values.ndim != 2 or values.shape[1] != len(headers):
        raise ValueError(f"Malformed SCIANTIX output in {output_file}")

    return headers, values


def compare_output_against_gold() -> None:
    if not OUTPUT_GOLD_FILE.is_file():
        raise FileNotFoundError(f"Missing gold file: {OUTPUT_GOLD_FILE}")

    headers, values = load_table(OUTPUT_FILE)
    gold_headers, gold_values = load_table(OUTPUT_GOLD_FILE)
    failures: list[str] = []

    if headers != gold_headers:
        failures.append("headers changed")

    if values.shape != gold_values.shape:
        failures.append(f"shape changed: output={values.shape}, gold={gold_values.shape}")

    if not failures:
        close = np.isclose(values, gold_values, rtol=GOLD_REL_TOL, atol=GOLD_ABS_TOL, equal_nan=True)
        for row_index, column_index in np.argwhere(~close)[:20]:
            column_name = headers[column_index]
            actual = values[row_index, column_index]
            expected = gold_values[row_index, column_index]
            failures.append(
                f"row {row_index + 1}, {column_name}: "
                f"actual={actual:.12e}, gold={expected:.12e}, abs_diff={abs(actual - expected):.3e}"
            )

    if failures:
        print("FAIL: output.txt gold comparison failed.")
        for failure in failures:
            print(f"  - {failure}")
        raise SystemExit(1)

    print(f"PASS: output.txt matches output_gold.txt ({values.shape[0]} rows, {values.shape[1]} columns).")


def normalized_thermochemistry_lines(path: Path) -> list[str]:
    lines = []
    date_pattern = re.compile(r"(\bOutput for equilibrium:.*)\s+\d{4}\.\d{2}\.\d{2}\s*$")
    for line in path.read_text().splitlines():
        line = date_pattern.sub(r"\1 <date>", line.rstrip())
        lines.append(line)
    return lines


def compare_thermochemistry_against_gold() -> None:
    if not THERMOCHEMISTRY_OUTPUT_GOLD_FILE.is_file():
        raise FileNotFoundError(f"Missing gold file: {THERMOCHEMISTRY_OUTPUT_GOLD_FILE}")

    current_lines = normalized_thermochemistry_lines(THERMOCHEMISTRY_OUTPUT_FILE)
    gold_lines = normalized_thermochemistry_lines(THERMOCHEMISTRY_OUTPUT_GOLD_FILE)
    if current_lines != gold_lines:
        print("FAIL: thermochemistry_output.txt gold comparison failed.")
        max_lines = min(len(current_lines), len(gold_lines))
        for index in range(max_lines):
            if current_lines[index] != gold_lines[index]:
                print(f"  - first difference at line {index + 1}")
                print(f"    output: {current_lines[index]}")
                print(f"    gold:   {gold_lines[index]}")
                break
        if len(current_lines) != len(gold_lines):
            print(f"  - line count changed: output={len(current_lines)}, gold={len(gold_lines)}")
        raise SystemExit(1)

    print("PASS: thermochemistry_output.txt matches thermochemistry_output_gold.txt.")


def plot_vs_time() -> Path:
    time_h, cm = load_columns(OUTPUT_FILE, "Time (h)", "Fuel oxygen potential (KJ/mol)")
    time_h, cm_OC = load_columns(OUTPUT_FILE, "Time (h)", "Fuel oxygen potential - CALPHAD (KJ/mol)")
    time_h, cm_BB = load_columns(OUTPUT_FILE, "Time (h)", "Fuel oxygen potential - Blackburn (KJ/mol)")
    time_h, cm_ML = load_columns(OUTPUT_FILE, "Time (h)", "Fuel oxygen potential - ML (KJ/mol)")

    fig, ax = plt.subplots(figsize=(7, 5))
    ax.scatter(time_h, cm, color="black")
    ax.scatter(time_h, cm_OC, label = "OC")
    ax.scatter(time_h, cm_BB, label = "BB")
    ax.scatter(time_h, cm_ML, label = "ML")
    ax.set_xlabel("Time (h)")
    ax.set_ylabel("Fuel oxygen potential (KJ/mol)")
    plt.legend()
    ax.grid(True, which="both", linestyle=":", linewidth=0.8)

    fig.tight_layout()
    fig.savefig(PLOT_FILE, dpi=180)
    plt.close(fig)

    return PLOT_FILE


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run the OpenCalphad smoke test and compare against gold values.")
    parser.add_argument("--no-run", action="store_true", help="Check and plot existing output.txt without rerunning SCIANTIX.")
    args = parser.parse_args()

    if not args.no_run:
        copy_sciantix_executable()
        run_sciantix()
    compare_output_against_gold()
    compare_thermochemistry_against_gold()
    saved_plot = plot_vs_time()
    print(f"Saved: {saved_plot}", flush=True)
