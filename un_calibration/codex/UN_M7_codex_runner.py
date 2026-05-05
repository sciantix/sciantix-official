#!/usr/bin/env python3
"""Codex runner for capture_only fixed-Dv Optuna studies.

This script intentionally reuses UN_M7_optuna_calibration_v2.py without
modifying it. Each fixed-Dv study gets a separate subfolder so Optuna's
load-if-exists SQLite storage cannot mix trials from different Dv values.
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


DV_VALUES = [0.03, 0.05, 0.10, 0.15, 0.30, 0.50, 1.00]
BASE_OUT = Path("UN_M7_codex_results") / "capture_only_dv_profile"


def dv_tag(value: float) -> str:
    return f"dv_{value:.2f}".replace(".", "p")


def run_profile(n_trials: int) -> None:
    BASE_OUT.mkdir(parents=True, exist_ok=True)
    for dv in DV_VALUES:
        out_dir = BASE_OUT / dv_tag(dv)
        cmd = [
            sys.executable,
            "UN_M7_optuna_calibration_v2.py",
            "--family",
            "capture_only",
            "--dv-fixed",
            f"{dv:g}",
            "--pressure-weight",
            "0.6",
            "--pressure-free-factor",
            "3",
            "--rizk-prior-weight",
            "0.2",
            "--bulk-shape-weight",
            "0.05",
            "--n-trials",
            str(n_trials),
            "--n-top-final",
            "0",
            "--no-plots",
            "--output-dir",
            str(out_dir),
        ]
        print("\n" + "#" * 120, flush=True)
        print(f"Running capture_only fixed Dv_scale={dv:g}", flush=True)
        print(" ".join(cmd), flush=True)
        print("#" * 120, flush=True)
        subprocess.run(cmd, check=True)


def main() -> None:
    parser = argparse.ArgumentParser(description="Run capture_only fixed-Dv profile studies.")
    parser.add_argument("--n-trials", type=int, default=100)
    args = parser.parse_args()
    run_profile(args.n_trials)


if __name__ == "__main__":
    main()
