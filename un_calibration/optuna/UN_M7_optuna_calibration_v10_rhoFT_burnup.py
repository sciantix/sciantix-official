#!/usr/bin/env python3
"""
UN_M7_optuna_calibration_v10_rhoFT_burnup.py

Diagnostic wrapper: v9-style balanced scoring + Ray&Blank rho_d(F,T) +
soft Rizk Fig.6 dislocation swelling-vs-burnup anchors at 1600 K.

Purpose:
  - keep the same M7/v8 physics equations;
  - use Ray&Blank rho_d(F,T) diagnostic from v8_rhoFT;
  - avoid using bulk swelling-vs-burnup as a target, because Rizk states bulk
    and intergranular curves in Fig.6 are information only, not directly
    comparable to measurements;
  - add only three soft anchors for the yellow dislocation curve in Rizk Fig.6:
        FIMA 1.1% -> S_d ~ 2.6%
        FIMA 3.2% -> S_d ~ 3.85%
        FIMA 6.0% -> S_d ~ 5.2%
  - rebalance between v8 and v9: less extreme than v9 on N_d drop, less
    plateau/pressure dominated than v8.
"""

from __future__ import annotations

import argparse
import math
import os
from typing import Dict, List, Tuple

import UN_M7_optuna_calibration_v8 as v8
import UN_M7_optuna_calibration_v8_core as m
import UN_M7_optuna_calibration_v8_rhoFT as rhoft

# Soft anchors digitized/estimated from Rizk Fig. 6 yellow dislocation curve.
# Units: burnup %FIMA, dislocation swelling % at 1600 K.
RIZK_FIG6_DISL_ANCHORS: List[Tuple[float, float]] = [
    (1.1, 2.60),
    (3.2, 3.85),
    (6.0, 5.20),
]

W_RIZK_FIG6_DISL_BURNUP = 0.45


def rizk_fig6_dislocation_burnup_score(cand, dt_h: float, n_modes: int) -> Tuple[float, Dict[str, float]]:
    errs = []
    diag: Dict[str, float] = {}
    for bu, target in RIZK_FIG6_DISL_ANCHORS:
        out = m.run_model_point(1600.0, bu, cand, dt_h, n_modes, keep_history=False)
        pred = out["swelling_d_percent"]
        # Percent swelling errors; scale by 4% so this is a shape guide, not a hard fit.
        err = (pred - target) / 4.0
        errs.append(err)
        tag = str(bu).replace('.', 'p')
        diag[f"rizk_fig6_Sd_pred_{tag}FIMA"] = pred
        diag[f"rizk_fig6_Sd_target_{tag}FIMA"] = target
        diag[f"rizk_fig6_Sd_err_scaled_{tag}FIMA"] = err
    score = math.sqrt(sum(e * e for e in errs) / len(errs)) if errs else math.inf

    # Weak monotonicity guard: the yellow curve should increase with burnup.
    preds = [diag[f"rizk_fig6_Sd_pred_{str(bu).replace('.', 'p')}FIMA"] for bu, _ in RIZK_FIG6_DISL_ANCHORS]
    mono_terms = []
    for a, b in zip(preds[:-1], preds[1:]):
        mono_terms.append(max(0.0, a - b) / 4.0)
    if mono_terms:
        score = math.sqrt(0.8 * score * score + 0.2 * sum(x*x for x in mono_terms) / len(mono_terms))
    diag["score_rizk_fig6_dislocation_burnup"] = score
    return score, diag


def score_candidate_v10(cand, dt_h: float, n_modes: int, use_full_exp: bool = True):
    # Base v8 scoring with globals modified below. m.run_model_point is patched to rhoFT.
    row = v8.score_candidate_v5(cand, dt_h, n_modes, use_full_exp=use_full_exp)
    score_fig6, fig6_diag = rizk_fig6_dislocation_burnup_score(cand, dt_h, n_modes)
    row["score_rizk_fig6_dislocation_burnup"] = score_fig6
    row.update(fig6_diag)
    row["score_total"] = row["score_total"] + W_RIZK_FIG6_DISL_BURNUP * score_fig6

    # Carry rhoFT diagnostics into the CSV for sanity checks.
    row["rhoFT_enabled"] = int(rhoft.RHOFT_ENABLED)
    row["rhoFT_strength"] = rhoft.RHOFT_STRENGTH
    row["rhoFT_clip_T"] = int(rhoft.RHOFT_CLIP_T)
    row["rhoFT_use_candidate_scale"] = int(rhoft.RHOFT_USE_CANDIDATE_SCALE)
    for bu in (1.1, 1.3, 3.2, 6.0):
        tag_bu = str(bu).replace('.', 'p')
        row[f"rhoFT_base_1025_{tag_bu}FIMA"] = rhoft.rho_burnup_1025(bu)
        for T in (1025.0, 1600.0, 2000.0):
            row[f"rhoFT_eff_{tag_bu}FIMA_{int(T)}K"] = rhoft.rho_ray_blank_eff(T, bu, cand)
    return row


def main():
    parser = argparse.ArgumentParser(description="V10: v9-balanced score + rhoFT + soft Rizk Fig.6 dislocation burnup anchors.")
    parser.add_argument("--family", choices=["M7_full", "M7_no_phi", "capture_only", "baseline"], default="capture_only")
    parser.add_argument("--n-trials", type=int, default=400)
    parser.add_argument("--output-dir", default=None)
    parser.add_argument("--fast-dt-h", type=float, default=12.0)
    parser.add_argument("--fast-n-modes", type=int, default=22)
    parser.add_argument("--final-dt-h", type=float, default=1.0)
    parser.add_argument("--final-n-modes", type=int, default=40)
    parser.add_argument("--n-top-final", type=int, default=5)
    parser.add_argument("--full-exp-fast", action="store_true")
    parser.add_argument("--no-plots", action="store_true")

    # Balanced score defaults: between v8 and v9.
    parser.add_argument("--exp-swelling-weight", type=float, default=1.15)
    parser.add_argument("--exp-rd-weight", type=float, default=0.90)
    parser.add_argument("--exp-nd-level-weight", type=float, default=0.85)
    parser.add_argument("--base-nd-drop-weight", type=float, default=0.80)
    parser.add_argument("--nd-drop-target-weight", type=float, default=0.70)

    parser.add_argument("--pressure-weight", type=float, default=0.30)
    parser.add_argument("--pressure-free-factor", type=float, default=3.0)
    parser.add_argument("--highT-pressure-weight", type=float, default=0.30)

    parser.add_argument("--partition-weight", type=float, default=0.40)
    parser.add_argument("--qgb-weight", type=float, default=0.35)
    parser.add_argument("--bulk-plateau-weight", type=float, default=0.10)
    parser.add_argument("--rizk-prior-weight", type=float, default=0.10)

    parser.add_argument("--radius-guard-weight", type=float, default=1.10)
    parser.add_argument("--radius-saturation-weight", type=float, default=0.0)
    parser.add_argument("--radius-band-weight", type=float, default=0.0)
    parser.add_argument("--rd2000-max-nm", type=float, default=700.0)
    parser.add_argument("--rd-ratio-max", type=float, default=10.0)
    parser.add_argument("--rd1800-soft-max-nm", type=float, default=450.0)
    parser.add_argument("--rd1900-soft-max-nm", type=float, default=600.0)
    parser.add_argument("--rd-post1800-delta-max-nm", type=float, default=9999.0)
    parser.add_argument("--rd-last-increment-factor-max", type=float, default=9999.0)
    parser.add_argument("--rd-1900-2000-ratio-max", type=float, default=9999.0)

    parser.add_argument("--fig6-burnup-weight", type=float, default=0.45)

    # rhoFT controls.
    parser.add_argument("--rhoFT-disable", action="store_true")
    parser.add_argument("--rhoFT-strength", type=float, default=1.0)
    parser.add_argument("--rhoFT-no-clip-T", action="store_true")
    parser.add_argument("--rhoFT-no-candidate-scale", action="store_true")
    parser.add_argument("--rhoFT-fab", type=float, default=3.0e13)

    args = parser.parse_args()

    global W_RIZK_FIG6_DISL_BURNUP
    W_RIZK_FIG6_DISL_BURNUP = args.fig6_burnup_weight

    # Direct experimental weights.
    m.W_SWELLING = args.exp_swelling_weight
    m.W_RD = args.exp_rd_weight
    m.W_ND_LEVEL = args.exp_nd_level_weight
    m.W_ND_DROP = args.base_nd_drop_weight

    m.W_PRESSURE = args.pressure_weight
    m.PRESSURE_FREE_FACTOR = args.pressure_free_factor

    # v8 wrapper globals.
    v8.W_RIZK_PRIOR = args.rizk_prior_weight
    v8.W_PARTITION = args.partition_weight
    v8.W_QGB = args.qgb_weight
    v8.W_RADIUS_GUARD = args.radius_guard_weight
    v8.W_RADIUS_SATURATION = args.radius_saturation_weight
    v8.W_ND_DROP_TARGET = args.nd_drop_target_weight
    v8.W_BULK_PLATEAU = args.bulk_plateau_weight
    v8.W_HIGHT_PRESSURE = args.highT_pressure_weight
    v8.W_RADIUS_BAND = args.radius_band_weight
    v8.RD2000_MAX_NM = args.rd2000_max_nm
    v8.RD_RATIO_MAX = args.rd_ratio_max
    v8.RD1800_SOFT_MAX_NM = args.rd1800_soft_max_nm
    v8.RD1900_SOFT_MAX_NM = args.rd1900_soft_max_nm
    v8.RD_POST1800_DELTA_MAX_NM = args.rd_post1800_delta_max_nm
    v8.RD_LAST_INCREMENT_FACTOR_MAX = args.rd_last_increment_factor_max
    v8.RD_1900_2000_RATIO_MAX = args.rd_1900_2000_ratio_max

    # rhoFT globals.
    rhoft.RHOFT_ENABLED = not args.rhoFT_disable and args.rhoFT_strength != 0.0
    rhoft.RHOFT_STRENGTH = float(args.rhoFT_strength)
    rhoft.RHOFT_CLIP_T = not args.rhoFT_no_clip_T
    rhoft.RHOFT_USE_CANDIDATE_SCALE = not args.rhoFT_no_candidate_scale
    rhoft.RHO_FAB = float(args.rhoFT_fab)

    # Monkey-patch run and scoring.
    m.run_model_point = rhoft.run_model_point_rhoFT
    m.optuna_candidate_from_trial = v8.optuna_candidate_from_trial_v5
    m.score_candidate = score_candidate_v10
    m.enqueue_known_good_trials = v8.enqueue_known_good_trials_v5

    if args.output_dir:
        out = args.output_dir
    else:
        scale_tag = "scaled" if rhoft.RHOFT_USE_CANDIDATE_SCALE else "absolute"
        clip_tag = "clipT" if rhoft.RHOFT_CLIP_T else "extrapT"
        rho_tag = "rhoFT_off" if not rhoft.RHOFT_ENABLED else f"rhoFT_{scale_tag}_{clip_tag}_s{rhoft.RHOFT_STRENGTH:g}"
        out = os.path.join(
            "UN_M7_optuna_v10_rhoFT_burnup_results",
            args.family,
            f"{rho_tag}_swd{m.W_SWELLING:g}_rd{m.W_RD:g}_nd{m.W_ND_LEVEL:g}_drop{m.W_ND_DROP:g}"
            f"_p{m.W_PRESSURE:g}_part{v8.W_PARTITION:g}_fig6{W_RIZK_FIG6_DISL_BURNUP:g}_R{v8.RD2000_MAX_NM:g}",
        )

    print("#" * 120)
    print("V10 rhoFT + Fig6 dislocation-burnup settings")
    print(f"family                         = {args.family}")
    print(f"rhoFT enabled/strength/clip     = {rhoft.RHOFT_ENABLED} / {rhoft.RHOFT_STRENGTH} / {rhoft.RHOFT_CLIP_T}")
    print(f"direct exp weights              = sw {m.W_SWELLING}, Rd {m.W_RD}, Nd {m.W_ND_LEVEL}, Nd-drop {m.W_ND_DROP}")
    print(f"partition/qgb/bulk weights      = {v8.W_PARTITION} / {v8.W_QGB} / {v8.W_BULK_PLATEAU}")
    print(f"pressure/highT pressure         = {m.W_PRESSURE} / {v8.W_HIGHT_PRESSURE}, free factor {m.PRESSURE_FREE_FACTOR}")
    print(f"Nd-shape/radius guard           = {v8.W_ND_DROP_TARGET} / {v8.W_RADIUS_GUARD}")
    print(f"Fig6 dislocation burnup anchors = {RIZK_FIG6_DISL_ANCHORS}, weight {W_RIZK_FIG6_DISL_BURNUP}")
    print(f"output dir                      = {out}")
    print("#" * 120)

    m.run_optuna_calibration(
        family=args.family,
        n_trials=args.n_trials,
        output_dir=out,
        fast_dt_h=args.fast_dt_h,
        fast_n_modes=args.fast_n_modes,
        final_dt_h=args.final_dt_h,
        final_n_modes=args.final_n_modes,
        n_top_final=args.n_top_final,
        use_full_exp_fast=args.full_exp_fast,
        make_plots=not args.no_plots,
    )


if __name__ == "__main__":
    main()
