#!/usr/bin/env python3
"""OAT and small 2D sensitivity around PC1 capture_only v5-B candidate using v6 physics.

Copy these files into the repository root first:
  UN_M7_optuna_calibration_v6_core.py
  UN_M7_optuna_calibration_v6.py
  UN_M7_v6_B_sensitivity.py

Run:
  python UN_M7_v6_B_sensitivity.py

Outputs:
  UN_M7_v6_B_sensitivity_results/
"""

from __future__ import annotations

import csv
import os
from dataclasses import replace
from pathlib import Path
from typing import Dict, List

import UN_M7_optuna_calibration_v6_core as m
import UN_M7_optuna_calibration_v6 as v6


OUT = Path("UN_M7_v6_B_sensitivity_results")
OUT.mkdir(parents=True, exist_ok=True)

# PC1 capture_only v5 B/trial_00629.
# In v6 we map the old common scales to split terms:
#   Dg_D1 = Dg_D3 = old Dg_scale
#   Dv_D1 = Dv_D2 = old Dv_scale
#   b_bulk = b_dislocation = old b_scale
#   gd_bubble = gd_line = old gd_scale
# and old aggregate scales are reset to 1.
B = m.Candidate(
    label="B_standard_wide_trial_00629_v6_mapped",
    f_n=2.0147e-06,
    K_d=6.5339e05,
    rho_d=1.6459e13,
    fission_rate=6.9386e19,
    Dv_scale=1.0,
    Dg_scale=1.0,
    b_scale=1.0,
    gb_scale=1.6647,
    gd_scale=1.0,
    coalescence_d_scale=0.5066,
    capture_scale=2.3875,
    D2_xe_scale=1.0,
    Dg_D1_scale=0.5578,
    Dg_D3_scale=0.5578,
    Dv_D1_scale=1.4996,
    Dv_D2_scale=1.4996,
    b_bulk_scale=0.05268,
    b_dislocation_scale=0.05268,
    gd_bubble_scale=3.7672,
    gd_line_scale=3.7672,
    gd_line_alpha=1.0,
)


def sf(x):
    try:
        return float(x)
    except Exception:
        return float("nan")


def eval_candidate(cand: m.Candidate, label: str, dt_h: float = 6.0, n_modes: int = 26) -> Dict:
    # Use v5/v6 scoring components and selected diagnostics.
    score_row = v6.score_candidate_v5(cand, dt_h=dt_h, n_modes=n_modes, use_full_exp=False)
    row = m.candidate_to_dict(cand)
    row.update(score_row)
    row["label"] = label

    # Key diagnostics.
    for T in [1600.0, 1800.0, 1900.0, 2000.0]:
        h = m.run_model_point(T, 1.3, cand, dt_h, n_modes, keep_history=False)
        row[f"Rd_1p3_{int(T)}K"] = h["Rd_nm"]
        row[f"Nd_1p3_{int(T)}K"] = h["Nd"]
        row[f"swD_1p3_{int(T)}K"] = h["swelling_d_percent"]
        row[f"p_d_over_eq_1p3_{int(T)}K"] = h["p_d_over_eq"]
        row[f"p_b_over_eq_1p3_{int(T)}K"] = h["p_b_over_eq"]
        row[f"bulk_gas_1p3_{int(T)}K"] = h["bulk_gas_percent"]
        row[f"disl_gas_1p3_{int(T)}K"] = h["dislocation_gas_percent"]
        row[f"qgb_gas_1p3_{int(T)}K"] = h["qgb_gas_percent"]

    row["Rd_inc_1800_1900_nm"] = row["Rd_1p3_1900K"] - row["Rd_1p3_1800K"]
    row["Rd_inc_1900_2000_nm"] = row["Rd_1p3_2000K"] - row["Rd_1p3_1900K"]

    h11 = m.run_model_point(1600.0, 1.1, cand, dt_h, n_modes, keep_history=False)
    row["bulk_gas_1p1_1600K"] = h11["bulk_gas_percent"]
    row["disl_gas_1p1_1600K"] = h11["dislocation_gas_percent"]
    row["qgb_gas_1p1_1600K"] = h11["qgb_gas_percent"]

    return row


def write_csv(path: Path, rows: List[Dict]) -> None:
    if not rows:
        return
    keys = []
    for r in rows:
        for k in r:
            if k not in keys:
                keys.append(k)
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=keys)
        w.writeheader()
        w.writerows(rows)


def run_oat() -> List[Dict]:
    specs = {
        "b_bulk_scale": [0.03, 0.05268, 0.1, 0.2, 0.5, 1.0],
        "b_dislocation_scale": [0.03, 0.05268, 0.1, 0.2, 0.5, 1.0],
        "coalescence_d_scale": [0.25, 0.5066, 0.75, 1.0, 2.0, 4.0],
        "capture_scale": [0.5, 1.0, 2.3875, 4.0],
        "gd_line_alpha": [0.0, 0.25, 0.5, 0.75, 1.0],
        "gd_line_scale": [0.5, 1.0, 2.0, 3.7672, 6.0],
        "gd_bubble_scale": [0.5, 1.0, 2.0, 3.7672, 6.0],
        "Dg_D1_scale": [0.3, 0.5578, 1.0, 2.0],
        "Dg_D3_scale": [0.3, 0.5578, 1.0, 2.0],
        "Dv_D1_scale": [0.75, 1.0, 1.4996, 2.0, 4.0],
        "Dv_D2_scale": [0.75, 1.0, 1.4996, 2.0, 4.0],
    }
    rows = [eval_candidate(B, "base_B_v6_mapped")]
    for param, values in specs.items():
        for value in values:
            cand = replace(B, label=f"OAT_{param}_{value:g}", **{param: value})
            rows.append(eval_candidate(cand, cand.label))
            print(f"done {cand.label}")
    write_csv(OUT / "B_oat_sensitivity.csv", rows)
    return rows


def run_2d_small() -> List[Dict]:
    rows = []
    # Keep this small: enough to see directions without becoming a full Optuna run.
    for b_d in [0.03, 0.05268, 0.1, 0.2, 0.5]:
        for coal in [0.25, 0.5066, 0.75, 1.0, 2.0]:
            cand = replace(B, label=f"2D_bd{b_d:g}_coal{coal:g}",
                           b_dislocation_scale=b_d, coalescence_d_scale=coal)
            rows.append(eval_candidate(cand, cand.label))
            print(f"done {cand.label}")

    for alpha in [0.0, 0.25, 0.5, 0.75, 1.0]:
        for gl in [0.5, 1.0, 2.0, 3.7672, 6.0]:
            cand = replace(B, label=f"2D_alpha{alpha:g}_gline{gl:g}",
                           gd_line_alpha=alpha, gd_line_scale=gl)
            rows.append(eval_candidate(cand, cand.label))
            print(f"done {cand.label}")

    for cap in [0.5, 1.0, 2.3875, 4.0]:
        for coal in [0.25, 0.5066, 0.75, 1.0, 2.0]:
            cand = replace(B, label=f"2D_cap{cap:g}_coal{coal:g}",
                           capture_scale=cap, coalescence_d_scale=coal)
            rows.append(eval_candidate(cand, cand.label))
            print(f"done {cand.label}")

    write_csv(OUT / "B_2d_small_sensitivity.csv", rows)
    return rows


def plot_best_candidates(rows: List[Dict], n: int = 6) -> None:
    # Produce detailed plots for best rows if matplotlib is available.
    try:
        import matplotlib  # noqa: F401
    except Exception:
        print("matplotlib not available: skipping detailed plots")
        return

    m.set_model_family("capture_only")
    m.OUTPUT_DIR = str(OUT / "plots")
    m.SAVE_FIGS = True
    m.SHOW_PLOTS = False
    Path(m.OUTPUT_DIR).mkdir(parents=True, exist_ok=True)

    rows = sorted(rows, key=lambda r: sf(r.get("score_total", 1e9)))[:n]
    for i, row in enumerate(rows, start=1):
        cand = m.candidate_from_score_row(row, label_prefix=f"v6_B_sens_rank{i}")
        prefix = f"v6_B_sens_rank{i}_{row['label']}"
        print(f"plotting {prefix}")
        m.make_diagnostics_for_candidate(cand, dt_h=1.0, n_modes=40, prefix=prefix)


def main():
    m.set_model_family("capture_only")
    rows = []
    rows.extend(run_oat())
    rows.extend(run_2d_small())

    combined = rows
    write_csv(OUT / "B_all_sensitivity.csv", combined)

    # Compact ranked table.
    ranked = sorted(combined, key=lambda r: sf(r.get("score_total", 1e9)))
    write_csv(OUT / "B_top20_sensitivity.csv", ranked[:20])

    plot_best_candidates(ranked, n=6)
    print(f"Done. Outputs under: {OUT}")


if __name__ == "__main__":
    main()
