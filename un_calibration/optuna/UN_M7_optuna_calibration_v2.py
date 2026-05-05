#!/usr/bin/env python3
"""
UN_M7_optuna_calibration_v2.py
Wrapper around UN_M7_optuna_calibration.py.
Adds:
  - fixed/ranged Dv_scale for Dv-profile studies
  - stronger pressure scoring options
  - optional Rizk-prior penalty
  - optional weak bulk-swelling-shape penalty

Put this file in the same folder as UN_M7_optuna_calibration.py and run it from there.
"""
import argparse
import math
import os

import UN_M7_optuna_calibration as m

DV_FIXED = None
DV_MIN = 0.03
DV_MAX = 3.0
W_RIZK_PRIOR = 0.0
W_BULK_SHAPE = 0.0


def optuna_candidate_from_trial_v2(trial, family: str):
    vals = {
        "f_n": trial.suggest_float("f_n", 1.0e-8, 3.0e-7, log=True),
        "K_d": trial.suggest_float("K_d", 1.5e5, 9.0e5, log=True),
        "rho_d": trial.suggest_float("rho_d", 1.0e13, 5.0e13, log=True),
        # fission_rate is treated as power/case scaling, not as a material constant.
        "fission_rate": trial.suggest_float("fission_rate", 4.0e19, 8.0e19, log=False),
        "Dv_scale": float(DV_FIXED) if DV_FIXED is not None else trial.suggest_float("Dv_scale", DV_MIN, DV_MAX, log=True),
        "Dg_scale": trial.suggest_float("Dg_scale", 0.25, 2.5, log=True),
        "D2_xe_scale": trial.suggest_float("D2_xe_scale", 0.1, 10.0, log=True),
        "b_scale": trial.suggest_float("b_scale", 0.15, 5.0, log=True),
        "gb_scale": trial.suggest_float("gb_scale", 0.2, 5.0, log=True),
        "gd_scale": trial.suggest_float("gd_scale", 0.15, 5.0, log=True),
        "coalescence_d_scale": trial.suggest_float("coalescence_d_scale", 0.3, 15.0, log=True),
        "capture_scale": trial.suggest_float("capture_scale", 0.05, 5.0, log=True),
    }
    if family == "baseline":
        vals["capture_scale"] = 0.0
    return m.Candidate(label=f"{family}_v2_trial_{trial.number:05d}", **vals)


def bulk_shape_score(cand, dt_h: float, n_modes: int) -> float:
    # Weak qualitative Rizk-inspired diagnostic. Do not force exact bulk curve.
    vals = {T: m.run_model_point(T, 1.3, cand, dt_h, n_modes, keep_history=False)
            for T in [1200.0, 1500.0, 1700.0, 1800.0, 2000.0]}
    swB1200 = vals[1200.0]["swelling_b_percent"]
    swB1500 = vals[1500.0]["swelling_b_percent"]
    swB1700 = vals[1700.0]["swelling_b_percent"]
    swB1800 = vals[1800.0]["swelling_b_percent"]
    swB2000 = vals[2000.0]["swelling_b_percent"]
    terms = []
    # Bulk should not be totally absent at intermediate T if we want Rizk-like partition.
    terms.append(max(0.0, 0.30 - swB1500) / 2.0)
    # Rizk-like high-T shape: bulk should decrease after peak/high-T transition.
    terms.append(max(0.0, swB2000 - swB1800) / max(1.0, swB1800))
    # Some pre-peak increase.
    terms.append(max(0.0, swB1200 - swB1700) / max(1.0, swB1200))
    # Guard against absurd bulk swelling.
    terms.append(max(0.0, swB1800 - 8.0) / 8.0)
    return m.rmse(terms)


def rizk_prior_score(cand) -> float:
    # Log-distance from nominal/reference values. Weights encode confidence.
    items = [
        (cand.Dg_scale, 1.0, 0.9),
        (cand.b_scale, 1.0, 0.9),
        (cand.gb_scale, 1.0, 0.7),
        (cand.gd_scale, 1.0, 0.7),
        (cand.K_d, m.K_D_NOMINAL, 0.45),
        (cand.rho_d, m.RHO_D_NOMINAL, 0.45),
        (cand.fission_rate, m.FISSION_RATE_NOMINAL, 0.25),
        (cand.Dv_scale, 1.0, 0.25),       # Dv is suspicious, so weak prior.
        (cand.f_n, m.F_N_NOMINAL, 0.12),  # f_n is inherited closure, very weak prior.
        (cand.capture_scale if cand.capture_scale > 0 else 1.0, 1.0, 0.20),
        (cand.coalescence_d_scale, 1.0, 0.20),
    ]
    terms = []
    for value, ref, w in items:
        if value > 0.0 and ref > 0.0 and math.isfinite(value):
            terms.append(w * math.log10(value / ref))
    return m.rmse(terms)


def score_candidate_v2(cand, dt_h: float, n_modes: int, use_full_exp: bool = True):
    # This mirrors base score_candidate but adds optional bulk-shape and Rizk-prior terms.
    if use_full_exp:
        sw_points = [p for p in m.EXP_SWELLING_T if p["T"] <= 1700.0]
        nd_points = m.EXP_ND_T_13
        rd_points = m.EXP_RD_T_13
    else:
        sw_points, nd_points, rd_points = m.fast_subset_points()

    sw_errs = []
    sw_ig_errs = []
    for exp in sw_points:
        out = m.run_model_point(exp["T"], exp["burnup"], cand, dt_h, n_modes, keep_history=False)
        sw_errs.append(out["swelling_d_percent"] - exp["swelling"])
        sw_ig_errs.append(out["swelling_ig_percent"] - exp["swelling"])

    score_swd = m.rmse(sw_errs)
    score_swig_diag = m.rmse(sw_ig_errs)

    n_pairs = []
    for exp in nd_points:
        out = m.run_model_point(exp["T"], 1.3, cand, dt_h, n_modes, keep_history=False)
        n_pairs.append((out["Nd"], exp["N"]))
    score_Nd = m.log10_rmse(n_pairs)

    r_pairs = []
    for exp in rd_points:
        out = m.run_model_point(exp["T"], 1.3, cand, dt_h, n_modes, keep_history=False)
        r_pairs.append((out["Rd_nm"], exp["R_nm"]))
    score_Rd = m.log10_rmse(r_pairs)

    score_Nd_drop = m.nd_drop_score(cand, dt_h, n_modes)
    score_pressure = m.pressure_score(cand, dt_h, n_modes)
    score_rizk_gas_shape = m.rizk_gas_shape_score(cand, dt_h, n_modes)
    score_highT_guard = m.highT_guard_score(cand, dt_h, n_modes)
    score_fdot_prior = abs(math.log10(cand.fission_rate / m.FISSION_RATE_NOMINAL))
    score_bulk_shape = bulk_shape_score(cand, dt_h, n_modes)
    score_rizk_prior = rizk_prior_score(cand)

    total = (
        m.W_SWELLING * score_swd
        + m.W_RD * score_Rd
        + m.W_ND_LEVEL * score_Nd
        + m.W_ND_DROP * score_Nd_drop
        + m.W_PRESSURE * score_pressure
        + m.W_FDOT_PRIOR * score_fdot_prior
        + m.W_RIZK_GAS_SHAPE * score_rizk_gas_shape
        + m.W_HIGHT_GUARD * score_highT_guard
        + W_BULK_SHAPE * score_bulk_shape
        + W_RIZK_PRIOR * score_rizk_prior
    )

    out1600 = m.run_model_point(1600.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    out32_1600 = m.run_model_point(1600.0, 3.2, cand, dt_h, n_modes, keep_history=False)
    outNd1400 = m.run_model_point(1400.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    outNd1725 = m.run_model_point(1725.0, 1.3, cand, dt_h, n_modes, keep_history=False)

    result = {
        **m.candidate_to_dict(cand),
        "dt_h": dt_h,
        "n_modes": n_modes,
        "use_full_exp": int(use_full_exp),
        "score_total": total,
        "score_swd": score_swd,
        "score_swig_diag": score_swig_diag,
        "score_Nd": score_Nd,
        "score_Rd": score_Rd,
        "score_Nd_drop": score_Nd_drop,
        "score_pressure": score_pressure,
        "score_fdot_prior": score_fdot_prior,
        "score_rizk_gas_shape": score_rizk_gas_shape,
        "score_highT_guard": score_highT_guard,
        "score_bulk_shape": score_bulk_shape,
        "score_rizk_prior": score_rizk_prior,
        "swD_1p3_1600K": out1600["swelling_d_percent"],
        "swB_1p3_1600K": out1600["swelling_b_percent"],
        "Rd_1p3_1600K": out1600["Rd_nm"],
        "Nd_1p3_1600K": out1600["Nd"],
        "Nd_1p3_1400K": outNd1400["Nd"],
        "Nd_1p3_1725K": outNd1725["Nd"],
        "Nd_drop_1725_over_1400_log10": m.safe_log10_ratio(outNd1725["Nd"], outNd1400["Nd"]),
        "swD_3p2_1600K": out32_1600["swelling_d_percent"],
        "swB_3p2_1600K": out32_1600["swelling_b_percent"],
        "max_f_cap_step_3p2_1600K": out32_1600["max_f_cap_step"],
        "capture_fraction_sum_3p2_1600K": out32_1600["capture_fraction_sum"],
        "capture_raw_sum_3p2_1600K": out32_1600["capture_raw_sum"],
    }
    return result


def main():
    parser = argparse.ArgumentParser(description="V2 Optuna calibration wrapper for UN M7 model families.")
    parser.add_argument("--family", choices=["M7_full", "M7_no_phi", "capture_only", "baseline"], required=True)
    parser.add_argument("--n-trials", type=int, default=150)
    parser.add_argument("--output-dir", default=None)
    parser.add_argument("--fast-dt-h", type=float, default=12.0)
    parser.add_argument("--fast-n-modes", type=int, default=22)
    parser.add_argument("--final-dt-h", type=float, default=1.0)
    parser.add_argument("--final-n-modes", type=int, default=40)
    parser.add_argument("--n-top-final", type=int, default=0)
    parser.add_argument("--full-exp-fast", action="store_true")
    parser.add_argument("--no-plots", action="store_true")
    parser.add_argument("--dv-fixed", type=float, default=None)
    parser.add_argument("--dv-min", type=float, default=0.03)
    parser.add_argument("--dv-max", type=float, default=3.0)
    parser.add_argument("--pressure-weight", type=float, default=None)
    parser.add_argument("--pressure-free-factor", type=float, default=None)
    parser.add_argument("--rizk-prior-weight", type=float, default=0.0)
    parser.add_argument("--bulk-shape-weight", type=float, default=0.0)
    args = parser.parse_args()

    global DV_FIXED, DV_MIN, DV_MAX, W_RIZK_PRIOR, W_BULK_SHAPE
    DV_FIXED = args.dv_fixed
    DV_MIN = args.dv_min
    DV_MAX = args.dv_max
    W_RIZK_PRIOR = args.rizk_prior_weight
    W_BULK_SHAPE = args.bulk_shape_weight

    if args.pressure_weight is not None:
        m.W_PRESSURE = args.pressure_weight
    if args.pressure_free_factor is not None:
        m.PRESSURE_FREE_FACTOR = args.pressure_free_factor

    # Monkey-patch base module.
    m.optuna_candidate_from_trial = optuna_candidate_from_trial_v2
    m.score_candidate = score_candidate_v2

    if args.output_dir:
        out = args.output_dir
    else:
        dv_tag = f"dv{DV_FIXED:g}" if DV_FIXED is not None else f"dv{DV_MIN:g}-{DV_MAX:g}"
        out = os.path.join(
            "UN_M7_optuna_v2_results",
            args.family,
            f"{dv_tag}_pW{m.W_PRESSURE:g}_pF{m.PRESSURE_FREE_FACTOR:g}_prior{W_RIZK_PRIOR:g}_bulk{W_BULK_SHAPE:g}",
        )

    print("#" * 120)
    print("V2 wrapper settings")
    print(f"family               = {args.family}")
    print(f"Dv fixed/range        = {DV_FIXED} / [{DV_MIN}, {DV_MAX}]")
    print(f"pressure weight/free  = {m.W_PRESSURE} / {m.PRESSURE_FREE_FACTOR}")
    print(f"Rizk prior weight     = {W_RIZK_PRIOR}")
    print(f"Bulk shape weight     = {W_BULK_SHAPE}")
    print(f"output dir            = {out}")
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
