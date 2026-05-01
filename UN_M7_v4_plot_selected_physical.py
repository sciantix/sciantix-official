#!/usr/bin/env python3
"""Rerun and plot selected physical candidates from UN_M7_optuna_calibration_v4 results.

Use after a v4 run. This script does NOT run Optuna. It reads the fast-trial CSV,
selects candidates that satisfy/approach the high-T radius guard, then runs the
standard final diagnostics with dt_h=1 and n_modes=40.
"""
from __future__ import annotations

import csv
import math
import os
from typing import Dict, List

import UN_M7_optuna_calibration as m

OUT = "UN_M7_optuna_v4_results/capture_only/dv0.2-2_pW0.7_pF2_part0.8_qgb0.7_prior0.2_R800"
FAST_CSV = os.path.join(OUT, "optuna_fast_trials_capture_only.csv")
PLOT_OUT = os.path.join(OUT, "selected_physical_plots")

# Manual picks from the first 300-trial v4 run:
# - trial_00170: best compromise with Rd2000 < 1000 nm
# - trial_00150: best candidate satisfying Rd2000 < 800 nm
# - trial_00163: lower Rd2000 candidate, good pressure, useful diagnostic
MANUAL_LABELS = {
    "capture_only_v4_trial_00170",
    "capture_only_v4_trial_00150",
    "capture_only_v4_trial_00163",
}


def sf(row: Dict[str, str], key: str, default: float = math.nan) -> float:
    try:
        return float(row.get(key, default))
    except Exception:
        return default


def read_rows(path: str) -> List[Dict[str, str]]:
    with open(path, newline="") as f:
        return list(csv.DictReader(f))


def write_rows(path: str, rows: List[Dict[str, str]]) -> None:
    if not rows:
        return
    keys = list(rows[0].keys())
    with open(path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=keys)
        w.writeheader()
        w.writerows(rows)


def main() -> None:
    if not os.path.exists(FAST_CSV):
        raise FileNotFoundError(f"Missing {FAST_CSV!r}. Run v4 first or adjust OUT in this script.")
    rows = read_rows(FAST_CSV)
    rows = [r for r in rows if math.isfinite(sf(r, "score_total"))]

    selected: List[Dict[str, str]] = []
    seen = set()

    def add(row: Dict[str, str]) -> None:
        lab = row.get("label", "")
        if lab and lab not in seen:
            selected.append(row)
            seen.add(lab)

    # Manual picks.
    by_label = {r.get("label", ""): r for r in rows}
    for lab in sorted(MANUAL_LABELS):
        if lab in by_label:
            add(by_label[lab])

    # Automatic backups in case labels differ after continuation.
    constrained_800 = [r for r in rows if sf(r, "Rd_1p3_2000K") <= 800.0]
    constrained_1000 = [r for r in rows if sf(r, "Rd_1p3_2000K") <= 1000.0]
    pd_r_1000 = [r for r in rows if sf(r, "Rd_1p3_2000K") <= 1000.0 and sf(r, "p_d_over_eq_1p3_1600K") <= 2.0]

    for group in (constrained_1000, constrained_800, pd_r_1000):
        if group:
            add(sorted(group, key=lambda r: sf(r, "score_total"))[0])

    selected = sorted(selected, key=lambda r: sf(r, "score_total"))

    os.makedirs(PLOT_OUT, exist_ok=True)
    write_rows(os.path.join(PLOT_OUT, "selected_physical_candidates.csv"), selected)

    # Configure base plotting output.
    m.set_model_family("capture_only")
    m.OUTPUT_DIR = PLOT_OUT
    m.SAVE_FIGS = True
    m.SHOW_PLOTS = False

    print("Selected candidates:")
    for i, r in enumerate(selected, start=1):
        print(
            f"{i}: {r.get('label')} score={sf(r,'score_total'):.4f} "
            f"Rd2000={sf(r,'Rd_1p3_2000K'):.1f} nm "
            f"ratio={sf(r,'Rd_ratio_2000_over_1800'):.2f} "
            f"pd1600={sf(r,'p_d_over_eq_1p3_1600K'):.2f} "
            f"swD1600={sf(r,'swD_1p3_1600K'):.3f}"
        )

    for i, row in enumerate(selected, start=1):
        cand = m.candidate_from_score_row(row, label_prefix=f"selected_physical_rank{i}")
        prefix = f"selected_physical_rank{i}_{row.get('label','candidate')}"
        m.make_diagnostics_for_candidate(cand, dt_h=1.0, n_modes=40, prefix=prefix)

    print(f"\nDone. Plots written under: {PLOT_OUT}")


if __name__ == "__main__":
    main()
