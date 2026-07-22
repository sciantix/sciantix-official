#!/usr/bin/env python3
"""One-command verification & validation of the SCIANTIX MOX oxygen potential.

Stages (each can be skipped independently):
  1. verification  - MOX pO2 sweep vs the explicit NEA Kato equation and the
                     Thermo-Calc reference tables (test_MOX_pO2_verification).
  2. validation    - regenerate all oxygen-potential cases from the experimental
                     datasets (validation_dataset/generate_cases.py --write-gold)
                     and run SCIANTIX in each of them.
  3. plots         - parity / residual / per-source figures for the fresh-fuel
                     and burnup groups (plot.py in each group directory).

The SCIANTIX binary is taken from build/sciantix.x; build it first.
"""
from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

REGRESSION_DIR = Path(__file__).resolve().parent
ROOT = REGRESSION_DIR.parent

VERIFICATION_DRIVER = REGRESSION_DIR / "test_MOX_pO2_verification" / "run_temperature_sweep.py"
CASE_GENERATOR = ROOT / "validation_dataset" / "generate_cases.py"
PLOT_SCRIPTS = [
    REGRESSION_DIR / "oxygenpotential_freshfuel" / "plot.py",
    REGRESSION_DIR / "oxygenpotential_burnup" / "plot.py",
]
BUILD_BINARY = ROOT / "build" / "sciantix.x"


def run_stage(name: str, command: list[str], cwd: Path) -> None:
    print(f"=== {name}: {' '.join(command)} (cwd={cwd}) ===", flush=True)
    subprocess.run(command, cwd=cwd, check=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--skip-verification", action="store_true", help="Skip the pO2 verification sweep.")
    parser.add_argument("--skip-validation", action="store_true", help="Skip case regeneration and SCIANTIX runs.")
    parser.add_argument("--skip-plots", action="store_true", help="Skip the validation figures.")
    args = parser.parse_args()

    if not BUILD_BINARY.exists():
        sys.exit(f"Missing SCIANTIX binary: {BUILD_BINARY} (build the project first)")

    if not args.skip_verification:
        run_stage("verification", [sys.executable, str(VERIFICATION_DRIVER)], cwd=VERIFICATION_DRIVER.parent)

    if not args.skip_validation:
        run_stage("validation", [sys.executable, str(CASE_GENERATOR), "--write-gold"], cwd=ROOT)

    if not args.skip_plots:
        for script in PLOT_SCRIPTS:
            run_stage("plots", [sys.executable, str(script)], cwd=script.parent)

    print("Oxygen-potential V&V completed.")


if __name__ == "__main__":
    main()
