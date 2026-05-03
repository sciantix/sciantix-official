#!/usr/bin/env python3
"""
UN_M7_optuna_calibration_v8_rhoFT.py

Diagnostic wrapper around UN_M7_optuna_calibration_v8.py.

Purpose:
  - keep v8 physics/scoring unchanged;
  - replace the constant candidate rho_d by a Ray & Blank based effective
    dislocation density depending on burnup F and weakly on temperature T.

Ray & Blank basis:
  1) At 1025 K, sodium-bonded mixed carbide data in the range 3--11 a/o
     approximately follow:
        rho_d(F,1025 K) = C1 * (F - F0)
        C1 = 1.6e10 cm/cm3/a/o, F0 = 2.4 a/o
     Convert: 1 cm/cm3 = 1e4 m^-2, so C1 = 1.6e14 m^-2/a/o.
  2) A fixed-burnup C3/1 series at F=6.8 a/o shows only a slight increase
     from 940 to 1300 K:
        T [K]           = 940, 990, 1015, 1100, 1300
        rho [1e10 cm/cm3] = 6.4, 7.0, 7.0, 8.0, 8.6
     We fit a weak linear multiplier normalized to 1025 K and clip outside
     the measured range by default.
  3) For F < F0, the linear burnup law would give zero.  Ray & Blank report
     as-fabricated dislocation densities around 1e9--5e9 cm/cm3, i.e.
     1e13--5e13 m^-2, so a fabrication floor is used.

Important:
  This is NOT an accepted UN law. It is a documented diagnostic. The old
  Optuna candidate.rho_d is retained as a multiplicative scale around the
  nominal Rizk value 3e13 m^-2, so Optuna can still compensate uncertainty:

      rho_eff(F,T) = rho_RB(F,T) * (candidate.rho_d / 3e13)

  Use --rhoFT-no-candidate-scale to use the absolute Ray & Blank estimate.
"""

from __future__ import annotations

import argparse
import math
import os
from dataclasses import replace
from typing import Dict

import UN_M7_optuna_calibration_v8 as v8
import UN_M7_optuna_calibration_v8_core as m

_ORIGINAL_RUN_MODEL_POINT = m.run_model_point

# Ray & Blank parameters
RHO_FAB = 3.0e13              # m^-2; mid/floor from 1e13--5e13 m^-2 fabrication range
C1_RB = 1.6e14                # m^-2 per a/o, converted from 1.6e10 cm/cm3/a/o
F0_RB = 2.4                   # a/o/FIMA threshold in Ray & Blank fit
TREF_RB = 1025.0
TLOW_RB = 940.0
THIGH_RB = 1300.0
# Linear fit from C3/1 fixed-burnup points, expressed as rho_10 = rho/(1e10 cm/cm3)
# Fit performed on: (940,6.4), (990,7.0), (1015,7.0), (1100,8.0), (1300,8.6)
RHO10_SLOPE = 0.005966
RHO10_INTERCEPT = 1.022292

RHOFT_ENABLED = True
RHOFT_STRENGTH = 1.0
RHOFT_CLIP_T = True
RHOFT_USE_CANDIDATE_SCALE = True


def rho10_fit(T: float) -> float:
    return RHO10_SLOPE * float(T) + RHO10_INTERCEPT


def rhoT_factor(T: float) -> float:
    """Weak T multiplier from Ray & Blank C3/1, normalized at 1025 K."""
    if not RHOFT_ENABLED:
        return 1.0
    T_eval = float(T)
    if RHOFT_CLIP_T:
        T_eval = min(max(T_eval, TLOW_RB), THIGH_RB)
    raw = rho10_fit(T_eval) / rho10_fit(TREF_RB)
    fac = 1.0 + RHOFT_STRENGTH * (raw - 1.0)
    return max(0.05, fac)


def rho_burnup_1025(F_a_o: float) -> float:
    """Ray & Blank burnup law at 1025 K with fabrication floor, in m^-2."""
    rho_bu = C1_RB * max(float(F_a_o) - F0_RB, 0.0)
    return max(RHO_FAB, rho_bu)


def rho_ray_blank_eff(T: float, burnup: float, cand) -> float:
    """Effective rho_d used in the model point."""
    rho_rb = rho_burnup_1025(float(burnup)) * rhoT_factor(float(T))
    if RHOFT_USE_CANDIDATE_SCALE:
        rho_scale = cand.rho_d / m.RHO_D_NOMINAL
        rho_rb *= rho_scale
    return max(rho_rb, 1.0e10)


def run_model_point_rhoFT(T, burnup, cand, dt_h, n_modes, keep_history=False):
    rho_eff = rho_ray_blank_eff(float(T), float(burnup), cand)
    cand_eff = replace(cand, rho_d=rho_eff, label=f"{cand.label}_rhoFT")
    return _ORIGINAL_RUN_MODEL_POINT(T, burnup, cand_eff, dt_h, n_modes, keep_history)


def score_candidate_v8_rhoFT(cand, dt_h: float, n_modes: int, use_full_exp: bool = True) -> Dict:
    row = v8.score_candidate_v5(cand, dt_h, n_modes, use_full_exp=use_full_exp)
    row["rhoFT_enabled"] = int(RHOFT_ENABLED)
    row["rhoFT_strength"] = RHOFT_STRENGTH
    row["rhoFT_clip_T"] = int(RHOFT_CLIP_T)
    row["rhoFT_use_candidate_scale"] = int(RHOFT_USE_CANDIDATE_SCALE)
    for bu in (1.1, 1.3, 3.2):
        row[f"rhoFT_base_1025_{str(bu).replace('.', 'p')}FIMA"] = rho_burnup_1025(bu)
        for T in (940.0, 1025.0, 1300.0, 1600.0, 2000.0):
            row[f"rhoFT_eff_{str(bu).replace('.', 'p')}FIMA_{int(T)}K"] = rho_ray_blank_eff(T, bu, cand)
    return row


def enqueue_known_good_trials_v8_rhoFT(study):
    return v8.enqueue_known_good_trials_v5(study)


def main():
    parser = argparse.ArgumentParser(description="V8 Optuna wrapper + Ray&Blank rho_d(F,T) diagnostic.")
    parser.add_argument("--family", choices=["M7_full", "M7_no_phi", "capture_only", "baseline"], default="capture_only")
    parser.add_argument("--n-trials", type=int, default=200)
    parser.add_argument("--output-dir", default=None)
    parser.add_argument("--fast-dt-h", type=float, default=12.0)
    parser.add_argument("--fast-n-modes", type=int, default=22)
    parser.add_argument("--final-dt-h", type=float, default=1.0)
    parser.add_argument("--final-n-modes", type=int, default=40)
    parser.add_argument("--n-top-final", type=int, default=5)
    parser.add_argument("--full-exp-fast", action="store_true")
    parser.add_argument("--no-plots", action="store_true")

    # Original v8 controls.
    parser.add_argument("--dv-fixed", type=float, default=None)
    parser.add_argument("--dv-min", type=float, default=0.20)
    parser.add_argument("--dv-max", type=float, default=2.00)
    parser.add_argument("--pressure-weight", type=float, default=0.70)
    parser.add_argument("--pressure-free-factor", type=float, default=2.0)
    parser.add_argument("--rizk-prior-weight", type=float, default=0.20)
    parser.add_argument("--partition-weight", type=float, default=0.80)
    parser.add_argument("--qgb-weight", type=float, default=0.70)
    parser.add_argument("--radius-guard-weight", type=float, default=0.55)
    parser.add_argument("--radius-saturation-weight", type=float, default=0.75)
    parser.add_argument("--nd-drop-target-weight", type=float, default=0.55)
    parser.add_argument("--bulk-plateau-weight", type=float, default=0.55)
    parser.add_argument("--highT-pressure-weight", type=float, default=0.45)
    parser.add_argument("--radius-band-weight", type=float, default=0.75)
    parser.add_argument("--rd2000-max-nm", type=float, default=800.0)
    parser.add_argument("--rd-ratio-max", type=float, default=2.8)
    parser.add_argument("--rd1800-soft-max-nm", type=float, default=600.0)
    parser.add_argument("--rd1900-soft-max-nm", type=float, default=700.0)
    parser.add_argument("--rd-post1800-delta-max-nm", type=float, default=350.0)
    parser.add_argument("--rd-last-increment-factor-max", type=float, default=0.75)
    parser.add_argument("--rd-1900-2000-ratio-max", type=float, default=1.35)

    # rhoFT controls
    parser.add_argument("--rhoFT-disable", action="store_true")
    parser.add_argument("--rhoFT-strength", type=float, default=1.0, help="0 disables T correction; 1 is Ray&Blank fit; >1 amplified diagnostic")
    parser.add_argument("--rhoFT-no-clip-T", action="store_true", help="dangerous extrapolation beyond 940--1300 K")
    parser.add_argument("--rhoFT-no-candidate-scale", action="store_true", help="use absolute Ray&Blank rho_d(F,T), ignore Optuna rho_d scale")
    parser.add_argument("--rhoFT-fab", type=float, default=3.0e13)

    args = parser.parse_args()

    global RHOFT_ENABLED, RHOFT_STRENGTH, RHOFT_CLIP_T, RHOFT_USE_CANDIDATE_SCALE, RHO_FAB
    RHOFT_ENABLED = not args.rhoFT_disable and args.rhoFT_strength != 0.0
    RHOFT_STRENGTH = float(args.rhoFT_strength)
    RHOFT_CLIP_T = not args.rhoFT_no_clip_T
    RHOFT_USE_CANDIDATE_SCALE = not args.rhoFT_no_candidate_scale
    RHO_FAB = float(args.rhoFT_fab)

    # Copy v8 CLI settings into v8 globals.
    v8.DV_FIXED = args.dv_fixed
    v8.DV_MIN = args.dv_min
    v8.DV_MAX = args.dv_max
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

    m.W_PRESSURE = args.pressure_weight
    m.PRESSURE_FREE_FACTOR = args.pressure_free_factor

    # Monkey-patch evaluator and objective functions.
    m.run_model_point = run_model_point_rhoFT
    m.optuna_candidate_from_trial = v8.optuna_candidate_from_trial_v5
    m.score_candidate = score_candidate_v8_rhoFT
    m.enqueue_known_good_trials = enqueue_known_good_trials_v8_rhoFT

    if args.output_dir:
        out = args.output_dir
    else:
        dv_tag = f"dv{v8.DV_FIXED:g}" if v8.DV_FIXED is not None else f"dv{v8.DV_MIN:g}-{v8.DV_MAX:g}"
        scale_tag = "scaled" if RHOFT_USE_CANDIDATE_SCALE else "absolute"
        clip_tag = "clipT" if RHOFT_CLIP_T else "extrapT"
        rho_tag = "rhoFT_off" if not RHOFT_ENABLED else f"rhoFT_{scale_tag}_{clip_tag}_s{RHOFT_STRENGTH:g}_fab{RHO_FAB:.1e}"
        out = os.path.join(
            "UN_M7_optuna_v8_rhoFT_results",
            args.family,
            f"{rho_tag}_{dv_tag}_pW{m.W_PRESSURE:g}_pF{m.PRESSURE_FREE_FACTOR:g}_part{v8.W_PARTITION:g}_qgb{v8.W_QGB:g}_prior{v8.W_RIZK_PRIOR:g}_R{v8.RD2000_MAX_NM:g}_sat",
        )

    print("#" * 120)
    print("V8 rhoFT diagnostic wrapper settings")
    print(f"family                    = {args.family}")
    print(f"rhoFT enabled/strength     = {RHOFT_ENABLED} / {RHOFT_STRENGTH}")
    print(f"rhoFT candidate scale      = {RHOFT_USE_CANDIDATE_SCALE}")
    print(f"rhoFT T clipped            = {RHOFT_CLIP_T}")
    print(f"rhoFT fabrication floor    = {RHO_FAB:g} m^-2")
    print(f"rhoT factors               = 940K {rhoT_factor(940):.4g}, 1025K {rhoT_factor(1025):.4g}, 1300K {rhoT_factor(1300):.4g}, 1600K {rhoT_factor(1600):.4g}, 2000K {rhoT_factor(2000):.4g}")
    for bu in (1.1, 1.3, 3.2):
        print(f"rho_RB base/effective {bu:g}% FIMA: 1025K {rho_burnup_1025(bu):.4e} m^-2, 1600K(no cand scale) {rho_burnup_1025(bu)*rhoT_factor(1600):.4e} m^-2")
    print("source                    = Ray & Blank: rho_d=C1(F-F0) at 1025 K + weak fixed-burnup T trend; no needles added")
    print(f"output dir                = {out}")
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
