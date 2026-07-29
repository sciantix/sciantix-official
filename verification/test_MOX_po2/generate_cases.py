#!/usr/bin/env python3
"""
Generates the persistent verification/test_MOX_po2/T_<T>K_q_<Pu>/ case
directories (input files + output_gold.txt) for the default sweep
(8 temperatures x 3 q-values = 24 cases) used by python -m testing.runner.

Cases are generated, never edited by hand -- re-run this (with --write-gold)
if the template inputs change. Matches the T_<T>K_q_<Pu>/ naming convention
used by validation/oxygenpotential/{freshfuel,burnup}.

These same 24 cases are the only source of truth for this V&V: running them
through `python -m testing.runner --mox-po2` produces both the gold-diff and
the paper figures (see sciantix_verification/compare_sciantix_with_kato.py
and compare_sciantix_with_oc_csv.py) -- there is no separate exploratory case
set.
"""
from __future__ import annotations

import argparse
import shutil
import subprocess
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
BUILD_BINARY = SCRIPT_DIR.parent.parent / "build" / "sciantix.x"

TEMPERATURES_K = [1000, 1200, 1400, 1600, 1800, 2000, 2200, 2400]
Q_VALUES = [0.10, 0.20, 0.30]


def format_q_tag(q_value: float) -> str:
    """Format a plutonium-content tag matching oxygenpotential's T_<T>K_q_<Pu> convention."""
    return str(round(q_value * 100))


def case_dir_path(temperature_k: int, q_value: float) -> Path:
    return SCRIPT_DIR / f"T_{temperature_k}K_q_{format_q_tag(q_value)}"


def template_input_files() -> list[Path]:
    """Return the template input files that are replicated for each case.

    These live under templates/, not directly in this directory, so that
    generic_runner.discover_cases() doesn't mistake this whole group root for
    a single case (it treats any directory containing input_settings.txt as
    a case, matching the singleton-group convention used by e.g.
    test_openPorosity)."""
    return sorted(path for path in (SCRIPT_DIR / "templates").glob("input_*") if path.is_file())


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
    if not BUILD_BINARY.exists():
        raise FileNotFoundError(f"Missing SCIANTIX binary: {BUILD_BINARY}")
    subprocess.run([str(BUILD_BINARY), f"{case_dir}/"], check=True)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--write-gold", action="store_true",
                         help="Run SCIANTIX in every case and refresh output_gold.txt")
    args = parser.parse_args()

    input_files = template_input_files()
    for temperature_k in TEMPERATURES_K:
        for q_value in Q_VALUES:
            case_dir = case_dir_path(temperature_k, q_value)
            prepare_case(case_dir, temperature_k, q_value, input_files)
            if args.write_gold:
                run_case(case_dir)
                shutil.copy2(case_dir / "output.txt", case_dir / "output_gold.txt")
                print(f"wrote gold: {case_dir.name}")


if __name__ == "__main__":
    main()
