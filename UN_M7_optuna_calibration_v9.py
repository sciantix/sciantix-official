#!/usr/bin/env python3
"""
UN_M7_optuna_calibration_v9.py

Score-only wrapper around UN_M7_optuna_calibration_v8.py / v8_core.

Purpose of v9:
  - keep the physical equations unchanged relative to v8;
  - keep capture_only physics unchanged;
  - reduce the importance of Rizk-like bulk plateau / gas partition;
  - reduce pressure penalties to sanity-guard level;
  - increase the importance of direct experimental fit: P2 swelling, R_d, N_d;
  - increase the penalty for insufficient N_d decrease / coalescence shape;
  - keep a strong upper guard on high-T R_d;
  - disable explicit high-T radius saturation/flattening and radius-band forcing.

Recommended run:
  python UN_M7_optuna_calibration_v9.py --family capture_only --n-trials 400 \
    --full-exp-fast --n-top-final 5 --no-plots

Then rerun same folder with plots:
  python UN_M7_optuna_calibration_v9.py --family capture_only --n-trials 0 \
    --full-exp-fast --n-top-final 5
"""

from __future__ import annotations

import argparse
import os

import UN_M7_optuna_calibration_v8 as v8
import UN_M7_optuna_calibration_v8_core as m


def main():
    parser = argparse.ArgumentParser(
        description="V9 score-only wrapper: stronger experimental fit + Nd drop + radius max, weaker plateau/pressure."
    )
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

    # v9 default score philosophy.
    parser.add_argument("--exp-swelling-weight", type=float, default=1.45)
    parser.add_argument("--exp-rd-weight", type=float, default=1.05)
    parser.add_argument("--exp-nd-level-weight", type=float, default=0.95)
    parser.add_argument("--base-nd-drop-weight", type=float, default=1.25)

    parser.add_argument("--pressure-weight", type=float, default=0.18)
    parser.add_argument("--pressure-free-factor", type=float, default=3.5)
    parser.add_argument("--highT-pressure-weight", type=float, default=0.12)

    parser.add_argument("--partition-weight", type=float, default=0.20)
    parser.add_argument("--qgb-weight", type=float, default=0.20)
    parser.add_argument("--bulk-plateau-weight", type=float, default=0.10)
    parser.add_argument("--rizk-prior-weight", type=float, default=0.10)

    parser.add_argument("--radius-guard-weight", type=float, default=1.10)
    parser.add_argument("--radius-saturation-weight", type=float, default=0.0)
    parser.add_argument("--radius-band-weight", type=float, default=0.0)
    parser.add_argument("--nd-drop-target-weight", type=float, default=1.15)

    parser.add_argument("--rd2000-max-nm", type=float, default=700.0)
    parser.add_argument("--rd-ratio-max", type=float, default=10.0)  # mostly inactive because saturation weight = 0
    parser.add_argument("--rd1800-soft-max-nm", type=float, default=450.0)
    parser.add_argument("--rd1900-soft-max-nm", type=float, default=600.0)
    parser.add_argument("--rd-post1800-delta-max-nm", type=float, default=9999.0)  # inactive with saturation weight = 0
    parser.add_argument("--rd-last-increment-factor-max", type=float, default=9999.0)  # inactive with saturation weight = 0
    parser.add_argument("--rd-1900-2000-ratio-max", type=float, default=9999.0)  # inactive with saturation weight = 0

    args = parser.parse_args()

    # Direct experimental weights from the core score.
    m.W_SWELLING = args.exp_swelling_weight
    m.W_RD = args.exp_rd_weight
    m.W_ND_LEVEL = args.exp_nd_level_weight
    m.W_ND_DROP = args.base_nd_drop_weight

    # Pressure becomes a weak sanity guard, not a fit target.
    m.W_PRESSURE = args.pressure_weight
    m.PRESSURE_FREE_FACTOR = args.pressure_free_factor

    # v8 wrapper weights reused by v8.score_candidate_v5.
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

    # Monkey-patch core module with v8 Optuna machinery and v9 scoring weights.
    m.optuna_candidate_from_trial = v8.optuna_candidate_from_trial_v5
    m.score_candidate = v8.score_candidate_v5
    m.enqueue_known_good_trials = v8.enqueue_known_good_trials_v5

    if args.output_dir:
        out = args.output_dir
    else:
        out = os.path.join(
            "UN_M7_optuna_v9_results",
            args.family,
            f"expfit_swd{m.W_SWELLING:g}_rd{m.W_RD:g}_nd{m.W_ND_LEVEL:g}_drop{m.W_ND_DROP:g}"
            f"_p{m.W_PRESSURE:g}_part{v8.W_PARTITION:g}_bulk{v8.W_BULK_PLATEAU:g}"
            f"_Ndshape{v8.W_ND_DROP_TARGET:g}_Rmax{v8.RD2000_MAX_NM:g}",
        )

    print("#" * 120)
    print("V9 wrapper settings: SCORE ONLY, PHYSICS UNCHANGED FROM V8")
    print(f"family                     = {args.family}")
    print(f"direct exp weights          = swelling {m.W_SWELLING}, Rd {m.W_RD}, Nd {m.W_ND_LEVEL}, base Nd-drop {m.W_ND_DROP}")
    print(f"pressure weight/free        = {m.W_PRESSURE} / {m.PRESSURE_FREE_FACTOR}")
    print(f"highT pressure weight       = {v8.W_HIGHT_PRESSURE}")
    print(f"partition/qgb/bulk weights  = {v8.W_PARTITION} / {v8.W_QGB} / {v8.W_BULK_PLATEAU}")
    print(f"Rizk prior weight           = {v8.W_RIZK_PRIOR}")
    print(f"Nd coalescence-shape weight = {v8.W_ND_DROP_TARGET}")
    print(f"radius guard/saturation/band= {v8.W_RADIUS_GUARD} / {v8.W_RADIUS_SATURATION} / {v8.W_RADIUS_BAND}")
    print(f"Rd soft max 1800/1900/2000  = {v8.RD1800_SOFT_MAX_NM} / {v8.RD1900_SOFT_MAX_NM} / {v8.RD2000_MAX_NM} nm")
    print(f"full-exp-fast               = {args.full_exp_fast}")
    print(f"output dir                  = {out}")
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
