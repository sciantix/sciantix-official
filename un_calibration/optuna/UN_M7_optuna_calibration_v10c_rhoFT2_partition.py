#!/usr/bin/env python3
"""
UN_M7_optuna_calibration_v10c_rhoFT2_partition.py

V10c diagnostic wrapper:
  - same M7/v8 physics equations;
  - v10 balanced scoring + optional soft Rizk Fig.6 dislocation swelling anchors;
  - Ray & Blank rho_d(F,T) with TWO Optuna scaling factors:
        rho_low_scale  : multiplier for the Ray-Blank part up to 1300 K
        rho_high_scale : high-T plateau multiplier reached after a linear ramp
  - no discontinuity at 1300 K: the high-T branch starts from the 1300 K value
    and linearly connects to rho(1300)*rho_high_scale at T_sat.

If you want to leave the documented Blank range unchanged, run with:
  --rho-low-min 1.0 --rho-low-max 1.0

Default below does that: rho_low_scale fixed at 1.0, rho_high_scale sampled.
"""

from __future__ import annotations

import argparse
import math
import os
from dataclasses import replace
from typing import Dict, List, Tuple

import UN_M7_optuna_calibration_v8 as v8
import UN_M7_optuna_calibration_v8_core as m

_ORIGINAL_RUN_MODEL_POINT = m.run_model_point
_ORIGINAL_CANDIDATE_FROM_SCORE_ROW = m.candidate_from_score_row

# Soft anchors digitized/estimated from Rizk Fig. 6 yellow dislocation curve.
RIZK_FIG6_DISL_ANCHORS: List[Tuple[float, float]] = [
    (1.1, 2.60),
    (3.2, 3.85),
    (6.0, 5.20),
]
W_RIZK_FIG6_DISL_BURNUP = 0.05

# Ray & Blank rho_d(F,T) constants.
RHO_FAB = 3.0e13        # m^-2
C1_RB = 1.6e14          # m^-2 per a/o, from 1.6e10 cm/cm3/a/o
F0_RB = 2.4             # a/o
TREF_RB = 1025.0
TLOW_RB = 940.0
THIGH_RB = 1300.0
RHO10_SLOPE = 0.005966
RHO10_INTERCEPT = 1.022292

# rhoFT2 controls / ranges. Values set by CLI in main().
RHOFT2_ENABLED = True
RHO_LOW_MIN = 1.0
RHO_LOW_MAX = 1.0
RHO_HIGH_MIN = 0.7
RHO_HIGH_MAX = 6.0
RHO_TRANSITION_END_K = 1600.0
RHO_D_REFERENCE = m.RHO_D_NOMINAL
K_D_MIN = 3.0e5
K_D_MAX = 8.0e5
SAMPLE_RHO_D_REFERENCE = False
RHO_D_REF_MIN = 1.0e13
RHO_D_REF_MAX = 8.0e13

# label -> (rho_low_scale, rho_high_scale). Needed because Candidate dataclass has no extra fields.
RHO_SCALE_BY_LABEL: Dict[str, Tuple[float, float]] = {}


def _suggest_float_maybe_fixed(trial, name: str, low: float, high: float, *, log: bool = False) -> float:
    low = float(low)
    high = float(high)
    if abs(high - low) <= 0.0:
        return low
    return trial.suggest_float(name, low, high, log=log)


def rho10_fit(T: float) -> float:
    return RHO10_SLOPE * float(T) + RHO10_INTERCEPT


def rho_blank_factor_raw(T: float) -> float:
    """Ray & Blank weak T multiplier normalized at 1025 K, no high-T extension."""
    T_eval = min(max(float(T), TLOW_RB), THIGH_RB)
    return rho10_fit(T_eval) / rho10_fit(TREF_RB)


def rho_burnup_1025(F_a_o: float) -> float:
    rho_bu = C1_RB * max(float(F_a_o) - F0_RB, 0.0)
    return max(RHO_FAB, rho_bu)


def _scales_for_candidate(cand) -> Tuple[float, float]:
    return RHO_SCALE_BY_LABEL.get(cand.label, (1.0, 1.0))


def rhoFT2_factor(T: float, cand) -> float:
    """Temperature multiplier with low/high scaling and continuous high-T ramp."""
    if not RHOFT2_ENABLED:
        return 1.0

    rho_low_scale, rho_high_scale = _scales_for_candidate(cand)
    T = float(T)
    f1300 = rho_blank_factor_raw(THIGH_RB)

    if T <= THIGH_RB:
        # Documented Ray-Blank shape. If rho_low_scale fixed to 1, this is unchanged.
        return max(0.05, rho_low_scale * rho_blank_factor_raw(T))

    T_sat = max(float(RHO_TRANSITION_END_K), THIGH_RB + 1.0)
    f_start = rho_low_scale * f1300
    f_end = rho_high_scale * f1300

    if T < T_sat:
        x = (T - THIGH_RB) / (T_sat - THIGH_RB)
        return max(0.05, f_start + x * (f_end - f_start))

    return max(0.05, f_end)


def rho_ray_blank_eff_FT2(T: float, burnup: float, cand) -> float:
    rho_rb = rho_burnup_1025(float(burnup)) * rhoFT2_factor(float(T), cand)
    # Optional old-style candidate rho_d scale. Off by default to avoid degeneracy with rho_low/high.
    if SAMPLE_RHO_D_REFERENCE:
        rho_rb *= cand.rho_d / RHO_D_REFERENCE
    return max(rho_rb, 1.0e10)


def run_model_point_rhoFT2(T, burnup, cand, dt_h, n_modes, keep_history=False):
    rho_eff = rho_ray_blank_eff_FT2(float(T), float(burnup), cand)
    cand_eff = replace(cand, rho_d=rho_eff, label=f"{cand.label}_rhoFT2")
    return _ORIGINAL_RUN_MODEL_POINT(T, burnup, cand_eff, dt_h, n_modes, keep_history)


def optuna_candidate_from_trial_v10c(trial, family: str):
    # Candidate physics parameters mostly from v8/v10, but K_d is narrowed by default.
    rho_low_scale = _suggest_float_maybe_fixed(trial, "rho_low_scale", RHO_LOW_MIN, RHO_LOW_MAX, log=True)
    rho_high_scale = _suggest_float_maybe_fixed(trial, "rho_high_scale", RHO_HIGH_MIN, RHO_HIGH_MAX, log=True)

    rho_d_ref = (
        trial.suggest_float("rho_d", RHO_D_REF_MIN, RHO_D_REF_MAX, log=True)
        if SAMPLE_RHO_D_REFERENCE else RHO_D_REFERENCE
    )

    vals = {
        "f_n": trial.suggest_float("f_n", 1.0e-10, 3.0e-6, log=True),
        "K_d": trial.suggest_float("K_d", K_D_MIN, K_D_MAX, log=True),
        "rho_d": rho_d_ref,
        "fission_rate": trial.suggest_float("fission_rate", 4.0e19, 8.0e19, log=False),

        "Dv_scale": 1.0,
        "Dg_scale": 1.0,
        "b_scale": 1.0,
        "gd_scale": 1.0,
        "D2_xe_scale": 1.0,

        "Dg_D1_scale": trial.suggest_float("Dg_D1_scale", 0.15, 4.0, log=True),
        "Dg_D3_scale": trial.suggest_float("Dg_D3_scale", 0.15, 4.0, log=True),
        "Dv_D1_scale": trial.suggest_float("Dv_D1_scale", 0.2, 4.0, log=True),
        "Dv_D2_scale": trial.suggest_float("Dv_D2_scale", 0.2, 4.0, log=True),

        "b_bulk_scale": trial.suggest_float("b_bulk_scale", 0.03, 4.0, log=True),
        "b_dislocation_scale": trial.suggest_float("b_dislocation_scale", 0.03, 4.0, log=True),

        "gb_scale": trial.suggest_float("gb_scale", 0.05, 8.0, log=True),
        "gd_bubble_scale": trial.suggest_float("gd_bubble_scale", 0.05, 10.0, log=True),
        "gd_line_scale": trial.suggest_float("gd_line_scale", 0.05, 10.0, log=True),
        "gd_line_alpha": trial.suggest_float("gd_line_alpha", 0.0, 1.0, log=False),

        "coalescence_d_scale": trial.suggest_float("coalescence_d_scale", 0.1, 20.0, log=True),
        "capture_scale": trial.suggest_float("capture_scale", 0.01, 8.0, log=True),
    }
    if family == "baseline":
        vals["capture_scale"] = 0.0

    cand = m.Candidate(label=f"{family}_v10c_trial_{trial.number:05d}", **vals)
    RHO_SCALE_BY_LABEL[cand.label] = (rho_low_scale, rho_high_scale)
    return cand


def candidate_from_score_row_v10c(row: Dict, label_prefix: str = "best_M7"):
    cand = _ORIGINAL_CANDIDATE_FROM_SCORE_ROW(row, label_prefix=label_prefix)
    rho_low_scale = m.safe_float(row.get("rho_low_scale"), 1.0)
    rho_high_scale = m.safe_float(row.get("rho_high_scale"), 1.0)
    RHO_SCALE_BY_LABEL[cand.label] = (rho_low_scale, rho_high_scale)
    return cand


def enqueue_known_good_trials_v10c(study):
    # Existing seeds plus neutral rho scales. They guide only; Optuna explores around them.
    try:
        v8.enqueue_known_good_trials_v5(study)
    except Exception:
        pass
    for k in [
        {"rho_low_scale": 1.0, "rho_high_scale": 1.0, "K_d": 5.0e5},
        {"rho_low_scale": 1.0, "rho_high_scale": 2.0, "K_d": 5.0e5},
        {"rho_low_scale": 1.0, "rho_high_scale": 4.0, "K_d": 5.0e5},
    ]:
        try:
            study.enqueue_trial(k, skip_if_exists=True)
        except TypeError:
            try:
                study.enqueue_trial(k)
            except Exception:
                pass


def rizk_fig6_dislocation_burnup_score(cand, dt_h: float, n_modes: int) -> Tuple[float, Dict[str, float]]:
    errs = []
    diag: Dict[str, float] = {}
    for bu, target in RIZK_FIG6_DISL_ANCHORS:
        out = m.run_model_point(1600.0, bu, cand, dt_h, n_modes, keep_history=False)
        pred = out["swelling_d_percent"]
        err = (pred - target) / 4.0
        errs.append(err)
        tag = str(bu).replace('.', 'p')
        diag[f"rizk_fig6_Sd_pred_{tag}FIMA"] = pred
        diag[f"rizk_fig6_Sd_target_{tag}FIMA"] = target
        diag[f"rizk_fig6_Sd_err_scaled_{tag}FIMA"] = err
    score = math.sqrt(sum(e * e for e in errs) / len(errs)) if errs else math.inf
    preds = [diag[f"rizk_fig6_Sd_pred_{str(bu).replace('.', 'p')}FIMA"] for bu, _ in RIZK_FIG6_DISL_ANCHORS]
    mono_terms = [max(0.0, a - b) / 4.0 for a, b in zip(preds[:-1], preds[1:])]
    if mono_terms:
        score = math.sqrt(0.8 * score * score + 0.2 * sum(x*x for x in mono_terms) / len(mono_terms))
    diag["score_rizk_fig6_dislocation_burnup"] = score
    return score, diag


def score_candidate_v10c(cand, dt_h: float, n_modes: int, use_full_exp: bool = True):
    row = v8.score_candidate_v5(cand, dt_h, n_modes, use_full_exp=use_full_exp)
    score_fig6, fig6_diag = rizk_fig6_dislocation_burnup_score(cand, dt_h, n_modes)
    row["score_rizk_fig6_dislocation_burnup"] = score_fig6
    row.update(fig6_diag)
    row["score_total"] = row["score_total"] + W_RIZK_FIG6_DISL_BURNUP * score_fig6

    rho_low_scale, rho_high_scale = _scales_for_candidate(cand)
    row["rhoFT2_enabled"] = int(RHOFT2_ENABLED)
    row["rho_low_scale"] = rho_low_scale
    row["rho_high_scale"] = rho_high_scale
    row["rho_transition_end_K"] = RHO_TRANSITION_END_K
    row["rho_D_reference"] = RHO_D_REFERENCE
    row["rhoFT2_sample_rhoD_reference"] = int(SAMPLE_RHO_D_REFERENCE)
    for bu in (1.1, 1.3, 3.2, 6.0):
        tag_bu = str(bu).replace('.', 'p')
        row[f"rhoFT2_base_1025_{tag_bu}FIMA"] = rho_burnup_1025(bu)
        for T in (1025.0, 1300.0, 1600.0, 1800.0, 2000.0):
            row[f"rhoFT2_factor_{int(T)}K"] = rhoFT2_factor(T, cand)
            row[f"rhoFT2_eff_{tag_bu}FIMA_{int(T)}K"] = rho_ray_blank_eff_FT2(T, bu, cand)
    return row


def main():
    parser = argparse.ArgumentParser(description="V10c: v10 scoring + rho_d(F,T) with two low/high scaling factors and continuous high-T ramp.")
    parser.add_argument("--family", choices=["M7_full", "M7_no_phi", "capture_only", "baseline"], default="capture_only")
    parser.add_argument("--n-trials", type=int, default=500)
    parser.add_argument("--output-dir", default=None)
    parser.add_argument("--fast-dt-h", type=float, default=12.0)
    parser.add_argument("--fast-n-modes", type=int, default=22)
    parser.add_argument("--final-dt-h", type=float, default=1.0)
    parser.add_argument("--final-n-modes", type=int, default=40)
    parser.add_argument("--n-top-final", type=int, default=5)
    parser.add_argument("--full-exp-fast", action="store_true")
    parser.add_argument("--no-plots", action="store_true")

    # Balanced score controls.
    parser.add_argument("--exp-swelling-weight", type=float, default=1.15)
    parser.add_argument("--exp-rd-weight", type=float, default=0.90)
    parser.add_argument("--exp-nd-level-weight", type=float, default=0.85)
    parser.add_argument("--base-nd-drop-weight", type=float, default=0.85)
    parser.add_argument("--nd-drop-target-weight", type=float, default=0.75)

    parser.add_argument("--pressure-weight", type=float, default=0.25)
    parser.add_argument("--pressure-free-factor", type=float, default=3.0)
    parser.add_argument("--highT-pressure-weight", type=float, default=0.25)

    parser.add_argument("--partition-weight", type=float, default=0.75)
    parser.add_argument("--qgb-weight", type=float, default=0.85)
    parser.add_argument("--bulk-plateau-weight", type=float, default=0.15)
    parser.add_argument("--rizk-prior-weight", type=float, default=0.10)

    parser.add_argument("--radius-guard-weight", type=float, default=0.55)
    parser.add_argument("--radius-saturation-weight", type=float, default=0.0)
    parser.add_argument("--radius-band-weight", type=float, default=0.0)
    parser.add_argument("--rd2000-max-nm", type=float, default=1200.0)
    parser.add_argument("--rd-ratio-max", type=float, default=10.0)
    parser.add_argument("--rd1800-soft-max-nm", type=float, default=900.0)
    parser.add_argument("--rd1900-soft-max-nm", type=float, default=1100.0)
    parser.add_argument("--rd-post1800-delta-max-nm", type=float, default=9999.0)
    parser.add_argument("--rd-last-increment-factor-max", type=float, default=9999.0)
    parser.add_argument("--rd-1900-2000-ratio-max", type=float, default=9999.0)

    parser.add_argument("--fig6-burnup-weight", type=float, default=0.05)

    # rhoFT2 controls.
    parser.add_argument("--rhoFT2-disable", action="store_true")
    parser.add_argument("--rho-low-min", type=float, default=1.0)
    parser.add_argument("--rho-low-max", type=float, default=1.0)
    parser.add_argument("--rho-high-min", type=float, default=0.7)
    parser.add_argument("--rho-high-max", type=float, default=6.0)
    parser.add_argument("--rho-transition-end-K", type=float, default=1600.0)
    parser.add_argument("--rhoFT-fab", type=float, default=3.0e13)
    parser.add_argument("--Kd-min", type=float, default=3.0e5)
    parser.add_argument("--Kd-max", type=float, default=8.0e5)
    parser.add_argument("--sample-rhoD-reference", action="store_true", help="also sample old candidate rho_d as a global scale; off by default to avoid degeneracy")
    parser.add_argument("--rhoD-ref-min", type=float, default=1.0e13)
    parser.add_argument("--rhoD-ref-max", type=float, default=8.0e13)

    args = parser.parse_args()

    global W_RIZK_FIG6_DISL_BURNUP
    global RHOFT2_ENABLED, RHO_LOW_MIN, RHO_LOW_MAX, RHO_HIGH_MIN, RHO_HIGH_MAX, RHO_TRANSITION_END_K, RHO_FAB
    global K_D_MIN, K_D_MAX, SAMPLE_RHO_D_REFERENCE, RHO_D_REF_MIN, RHO_D_REF_MAX

    W_RIZK_FIG6_DISL_BURNUP = args.fig6_burnup_weight

    RHOFT2_ENABLED = not args.rhoFT2_disable
    RHO_LOW_MIN = args.rho_low_min
    RHO_LOW_MAX = args.rho_low_max
    RHO_HIGH_MIN = args.rho_high_min
    RHO_HIGH_MAX = args.rho_high_max
    RHO_TRANSITION_END_K = args.rho_transition_end_K
    RHO_FAB = args.rhoFT_fab
    K_D_MIN = args.Kd_min
    K_D_MAX = args.Kd_max
    SAMPLE_RHO_D_REFERENCE = args.sample_rhoD_reference
    RHO_D_REF_MIN = args.rhoD_ref_min
    RHO_D_REF_MAX = args.rhoD_ref_max

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

    # Monkey-patch model and Optuna hooks.
    m.run_model_point = run_model_point_rhoFT2
    m.optuna_candidate_from_trial = optuna_candidate_from_trial_v10c
    m.candidate_from_score_row = candidate_from_score_row_v10c
    m.score_candidate = score_candidate_v10c
    m.enqueue_known_good_trials = enqueue_known_good_trials_v10c

    if args.output_dir:
        out = args.output_dir
    else:
        out = os.path.join(
            "UN_M7_optuna_v10c_rhoFT2_partition_results",
            args.family,
            f"rhoLow{RHO_LOW_MIN:g}-{RHO_LOW_MAX:g}_rhoHigh{RHO_HIGH_MIN:g}-{RHO_HIGH_MAX:g}_Tsat{RHO_TRANSITION_END_K:g}"
            f"_Kd{K_D_MIN:g}-{K_D_MAX:g}_part{v8.W_PARTITION:g}_qgb{v8.W_QGB:g}_fig6{W_RIZK_FIG6_DISL_BURNUP:g}",
        )

    print("#" * 120)
    print("V10c rhoFT2 + partition settings")
    print(f"family                         = {args.family}")
    print(f"rhoFT2 enabled                 = {RHOFT2_ENABLED}")
    print(f"rho_low_scale range             = {RHO_LOW_MIN:g} -- {RHO_LOW_MAX:g}")
    print(f"rho_high_scale range            = {RHO_HIGH_MIN:g} -- {RHO_HIGH_MAX:g}")
    print(f"rho high-T linear ramp          = 1300 K -> {RHO_TRANSITION_END_K:g} K")
    print(f"K_d range                       = {K_D_MIN:g} -- {K_D_MAX:g}")
    print(f"sample old rho_d reference      = {SAMPLE_RHO_D_REFERENCE}")
    print(f"Blank factors: 1025={rho_blank_factor_raw(1025):.4g}, 1300={rho_blank_factor_raw(1300):.4g}")
    print(f"direct exp weights              = sw {m.W_SWELLING}, Rd {m.W_RD}, Nd {m.W_ND_LEVEL}, Nd-drop {m.W_ND_DROP}")
    print(f"partition/qgb/bulk weights      = {v8.W_PARTITION} / {v8.W_QGB} / {v8.W_BULK_PLATEAU}")
    print(f"pressure/highT pressure         = {m.W_PRESSURE} / {v8.W_HIGHT_PRESSURE}, free factor {m.PRESSURE_FREE_FACTOR}")
    print(f"Fig6 dislocation burnup weight  = {W_RIZK_FIG6_DISL_BURNUP}")
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
