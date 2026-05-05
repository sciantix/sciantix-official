#!/usr/bin/env python3
"""Generate final diagnostic PNGs for selected UN M7 v5 candidates.

This script intentionally does not create or run an Optuna study. It reads
already selected rows from existing CSV outputs and only calls the diagnostic
plotting helpers with final numerics.
"""

from __future__ import annotations

import csv
import os
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
OUT_DIR = REPO_ROOT / "UN_M7_v5_codex_results" / "final_selected_plots"

if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

import UN_M7_optuna_calibration as base  # noqa: E402


SELECTED = [
    ("B_standard_wide", "capture_only_v5_codex_trial_00629", "B_standard_wide_trial_00629"),
    ("E_all_wide_exploratory", "capture_only_v5_codex_trial_00496", "E_all_wide_exploratory_trial_00496"),
    ("D2_Dv_coalescence_escape", "capture_only_v5_codex_trial_00235", "D2_Dv_coalescence_escape_trial_00235"),
    ("C4_microstructure_escape", "capture_only_v5_codex_trial_00196", "C4_microstructure_escape_trial_00196"),
    ("A_nominal_near", "capture_only_v5_codex_trial_00264", "A_nominal_near_trial_00264"),
]


def read_rows(path: Path) -> list[dict[str, str]]:
    with path.open("r", newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def find_row(block: str, label: str) -> dict[str, str]:
    candidates = [
        REPO_ROOT / "UN_M7_v5_codex_results" / block / "optuna_fast_trials_capture_only.csv",
        REPO_ROOT / "UN_M7_v5_codex_results" / block / "optuna_final_top_capture_only.csv",
    ]
    for path in candidates:
        if not path.exists():
            continue
        for row in read_rows(path):
            if row.get("label", "").endswith(label) or row.get("label") == label:
                return row
    raise RuntimeError(f"Could not find {label} in block {block}")


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    base.OUTPUT_DIR = str(OUT_DIR)
    base.FINAL_DT_H = 1.0
    base.FINAL_N_MODES = 40
    base.set_model_family("capture_only")

    generated: list[str] = []
    for block, label, prefix in SELECTED:
        row = find_row(block, label)
        cand = base.candidate_from_score_row(row, label_prefix=prefix)
        base.capture_diagnostic_for_candidate(cand, T=1600.0, burnup=3.2)
        base.make_diagnostics_for_candidate(cand, dt_h=1.0, n_modes=40, prefix=prefix)
        generated.extend(sorted(p.name for p in OUT_DIR.glob(f"{prefix}_*.png")))

    manifest = OUT_DIR / "MANIFEST.txt"
    with manifest.open("w", encoding="utf-8") as f:
        f.write("Final selected diagnostic PNGs\n")
        f.write("final_dt_h=1.0\n")
        f.write("final_n_modes=40\n")
        f.write("Optuna was not run by this script.\n\n")
        for name in sorted(set(generated)):
            f.write(f"{name}\n")

    print(f"Generated {len(set(generated))} PNG files in {OUT_DIR}")


if __name__ == "__main__":
    main()
