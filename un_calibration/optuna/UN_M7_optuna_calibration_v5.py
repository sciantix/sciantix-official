#!/usr/bin/env python3
"""
UN_M7_optuna_calibration_v5.py

Wrapper around UN_M7_optuna_calibration.py.

Purpose of v5:
  - keep the physical equations in the base module unchanged;
  - keep bulk -> dislocation capture active for capture_only;
  - add explicit gas-partition scoring using q_gb as
        gas at grain faces ~= grain-boundary bubbles + fission gas release;
  - keep pressure and Rizk-prior penalties;
  - add gas-partition/q_gb scoring as in v4;
  - strengthen the high-temperature radius guard;
  - add an explicit saturation/flattening guard so R_d(T) cannot keep accelerating after ~1800 K.

Recommended first use:
  python UN_M7_optuna_calibration_v5.py --family capture_only --dv-min 0.2 --dv-max 2.0 \
    --partition-weight 0.8 --qgb-weight 0.7 --pressure-weight 0.7 --pressure-free-factor 2 \
    --rizk-prior-weight 0.2 --radius-guard-weight 0.55 --radius-saturation-weight 0.75 --rd2000-max-nm 800 \
    --n-trials 500 --n-top-final 5 --no-plots

Then rerun/continue same folder with plots:
  python UN_M7_optuna_calibration_v5.py --family capture_only --dv-min 0.2 --dv-max 2.0 \
    --partition-weight 0.8 --qgb-weight 0.7 --pressure-weight 0.7 --pressure-free-factor 2 \
    --rizk-prior-weight 0.2 --radius-guard-weight 0.55 --radius-saturation-weight 0.75 --rd2000-max-nm 800 \
    --n-trials 0 --n-top-final 5
"""

from __future__ import annotations

import argparse
import math
import os
from typing import Dict, Iterable, List, Tuple

import UN_M7_optuna_calibration as m


DV_FIXED = None
DV_MIN = 0.20
DV_MAX = 2.00

W_RIZK_PRIOR = 0.20
W_PARTITION = 0.80
W_QGB = 0.70
W_RADIUS_GUARD = 0.45
RD2000_MAX_NM = 800.0
RD_RATIO_MAX = 2.8
RD1800_SOFT_MAX_NM = 600.0
# V5: explicit saturation/flattening constraints for R_d(T) after ~1800 K.
W_RADIUS_SATURATION = 0.75
RD1900_SOFT_MAX_NM = 700.0
RD_POST1800_DELTA_MAX_NM = 350.0
RD_LAST_INCREMENT_FACTOR_MAX = 0.75
RD_1900_2000_RATIO_MAX = 1.35

# The previous v2 bulk shape penalty is intentionally not reused as a strong target.
# In v5 gas partition is the cleaner way to constrain bulk/dislocation/q_gb behaviour.
W_BULK_SHAPE = 0.0


# Codex-selected capture_only candidates. They are enqueued to ensure Optuna starts
# from the known useful regions, then it is free to move away.
CODEX_CAPTURE_ONLY_SEEDS: List[Dict[str, float]] = [
    # best_overall_score, good scalar fit but bad pressure
    {
        "f_n": 1.3333e-08, "K_d": 7.1430e05, "rho_d": 4.6135e13,
        "fission_rate": 4.9558e19, "Dv_scale": 0.05, "Dg_scale": 2.012,
        "D2_xe_scale": 1.0, "b_scale": 1.181, "gb_scale": 0.8994,
        "gd_scale": 3.641, "coalescence_d_scale": 10.99, "capture_scale": 3.247,
    },
    # best_balanced, current main capture_only candidate
    {
        "f_n": 2.5466e-07, "K_d": 6.4846e05, "rho_d": 3.5350e13,
        "fission_rate": 7.4799e19, "Dv_scale": 1.0, "Dg_scale": 0.5333,
        "D2_xe_scale": 1.0, "b_scale": 3.066, "gb_scale": 0.4625,
        "gd_scale": 4.507, "coalescence_d_scale": 6.997, "capture_scale": 0.1401,
    },
    # best_pressure_friendly, only diagnostic: pressure improves slightly but fit/partition worsen
    {
        "f_n": 1.9043e-07, "K_d": 6.3368e05, "rho_d": 4.8099e13,
        "fission_rate": 6.8347e19, "Dv_scale": 1.0, "Dg_scale": 0.3921,
        "D2_xe_scale": 1.0, "b_scale": 3.704, "gb_scale": 0.3881,
        "gd_scale": 3.375, "coalescence_d_scale": 7.005, "capture_scale": 0.0608,
    },
    # best_rizk_near
    {
        "f_n": 1.3237e-08, "K_d": 5.5892e05, "rho_d": 2.4815e13,
        "fission_rate": 7.6621e19, "Dv_scale": 0.5, "Dg_scale": 1.307,
        "D2_xe_scale": 1.0, "b_scale": 1.296, "gb_scale": 0.4151,
        "gd_scale": 1.963, "coalescence_d_scale": 9.772, "capture_scale": 0.1557,
    },

    # v4 physical candidate: Rd(2000 K) below ~1000 nm, useful compromise
    {
        "f_n": 2.940436e-07, "K_d": 1.033314e06, "rho_d": 1.205949e13,
        "fission_rate": 6.730293e19, "Dv_scale": 0.227099, "Dg_scale": 2.163517,
        "D2_xe_scale": 0.112489, "b_scale": 0.099290, "gb_scale": 1.445601,
        "gd_scale": 4.396731, "coalescence_d_scale": 3.880728, "capture_scale": 0.024136,
    },
    # v4 physical candidate: Rd(2000 K) below 800 nm
    {
        "f_n": 5.964780e-07, "K_d": 1.198283e06, "rho_d": 9.976134e12,
        "fission_rate": 6.685431e19, "Dv_scale": 0.239374, "Dg_scale": 2.580361,
        "D2_xe_scale": 0.119911, "b_scale": 0.115218, "gb_scale": 1.009685,
        "gd_scale": 4.737461, "coalescence_d_scale": 2.916475, "capture_scale": 0.027787,
    },
    # v4 physical candidate: lowest Rd(2000 K) among the inspected physical set
    {
        "f_n": 7.392651e-07, "K_d": 1.166892e06, "rho_d": 1.180353e13,
        "fission_rate": 6.574543e19, "Dv_scale": 0.221547, "Dg_scale": 2.170761,
        "D2_xe_scale": 0.149876, "b_scale": 0.126114, "gb_scale": 1.304950,
        "gd_scale": 5.541092, "coalescence_d_scale": 2.726375, "capture_scale": 0.023283,
    },
]


def _finite(x: float) -> bool:
    return isinstance(x, (int, float)) and math.isfinite(float(x))


def _rmse(vals: Iterable[float]) -> float:
    vals = [float(v) for v in vals if _finite(v)]
    if not vals:
        return math.inf
    return math.sqrt(sum(v * v for v in vals) / len(vals))


def _below(value: float, target_min: float, scale: float = 100.0) -> float:
    return max(0.0, target_min - value) / scale


def _above(value: float, target_max: float, scale: float = 100.0) -> float:
    return max(0.0, value - target_max) / scale


def optuna_candidate_from_trial_v5(trial, family: str):
    vals = {
        # f_n is treated as a closure parameter of the single-size model, so the range is wider
        # than v2 and the prior on it is weak.
        "f_n": trial.suggest_float("f_n", 1.0e-10, 3.0e-6, log=True),
        "K_d": trial.suggest_float("K_d", 1.0e5, 1.5e6, log=True),
        "rho_d": trial.suggest_float("rho_d", 5.0e12, 8.0e13, log=True),
        "fission_rate": trial.suggest_float("fission_rate", 4.0e19, 8.0e19, log=False),
        "Dv_scale": float(DV_FIXED) if DV_FIXED is not None else trial.suggest_float("Dv_scale", DV_MIN, DV_MAX, log=True),
        "Dg_scale": trial.suggest_float("Dg_scale", 0.15, 4.0, log=True),
        "D2_xe_scale": trial.suggest_float("D2_xe_scale", 0.05, 20.0, log=True),
        "b_scale": trial.suggest_float("b_scale", 0.05, 8.0, log=True),
        "gb_scale": trial.suggest_float("gb_scale", 0.05, 8.0, log=True),
        "gd_scale": trial.suggest_float("gd_scale", 0.05, 10.0, log=True),
        # coalescence remains active; only its scale is calibrated.
        "coalescence_d_scale": trial.suggest_float("coalescence_d_scale", 0.1, 20.0, log=True),
        # capture is kept active for capture_only/M7_no_phi; do not allow exactly zero.
        "capture_scale": trial.suggest_float("capture_scale", 0.01, 8.0, log=True),
    }
    if family == "baseline":
        vals["capture_scale"] = 0.0
    return m.Candidate(label=f"{family}_v5_trial_{trial.number:05d}", **vals)


def rizk_prior_score_v5(cand) -> float:
    # Weak/medium log-distance from reference. f_n and Dv are deliberately weak.
    items = [
        (cand.Dg_scale, 1.0, 0.7),
        (cand.b_scale, 1.0, 0.7),
        (cand.gb_scale, 1.0, 0.45),
        (cand.gd_scale, 1.0, 0.45),
        (cand.K_d, m.K_D_NOMINAL, 0.35),
        (cand.rho_d, m.RHO_D_NOMINAL, 0.35),
        (cand.fission_rate, m.FISSION_RATE_NOMINAL, 0.25),
        (cand.Dv_scale, 1.0, 0.18),
        (cand.f_n, m.F_N_NOMINAL, 0.08),
        (cand.coalescence_d_scale, 1.0, 0.18),
        (cand.capture_scale if cand.capture_scale > 0 else 1.0, 1.0, 0.16),
    ]
    terms = []
    for value, ref, w in items:
        if value > 0.0 and ref > 0.0 and math.isfinite(value):
            terms.append(w * math.log10(value / ref))
    return _rmse(terms)


def gas_partition_score_v5(cand, dt_h: float, n_modes: int) -> Tuple[float, float, Dict[str, float]]:
    """Return (partition_score, qgb_score, diagnostics).

    q_gb in this reduced model is interpreted as gas reaching grain faces, i.e.
    approximately grain-boundary bubbles + fission gas release in Rizk Fig. 9.
    We therefore constrain q_gb to stay small at low/intermediate T rather than
    pretending it is only released gas.
    """
    diag: Dict[str, float] = {}
    part_terms: List[float] = []
    qgb_terms: List[float] = []

    # Soft anchors from Rizk Fig. 9, deliberately not an exact curve fit.
    # Low/intermediate T: bulk retains most generated gas, q_gb small.
    # High T: dislocation bubbles dominate generated gas, q_gb still not huge.
    anchors = [
        # burnup, T, min bulk, min disl, max bulk, max qgb, label
        # Low/intermediate-T bulk fraction is strengthened relative to v4 because
        # v4 physical candidates still depleted the bulk gas too early (~60% instead of ~75-80%).
        (1.1, 1200.0, 74.0, None, None, 8.0, "1p1_1200"),
        (1.1, 1500.0, 72.0, None, None, 10.0, "1p1_1500"),
        (1.1, 1600.0, 60.0, None, None, 12.0, "1p1_1600"),
        (1.1, 2000.0, None, 85.0, 15.0, 15.0, "1p1_2000"),
        (1.1, 2200.0, None, 90.0, 10.0, 18.0, "1p1_2200"),
        (3.2, 1200.0, 74.0, None, None, 8.0, "3p2_1200"),
        (3.2, 1500.0, 72.0, None, None, 10.0, "3p2_1500"),
        (3.2, 1800.0, 48.0, 35.0, None, 15.0, "3p2_1800"),
        (3.2, 2000.0, None, 82.0, 18.0, 15.0, "3p2_2000"),
        (3.2, 2200.0, None, 88.0, 12.0, 18.0, "3p2_2200"),
    ]

    for bu, T, min_bulk, min_disl, max_bulk, max_qgb, tag in anchors:
        out = m.run_model_point(T, bu, cand, dt_h, n_modes, keep_history=False)
        bulk = out["bulk_gas_percent"]
        disl = out["dislocation_gas_percent"]
        qgb = out["qgb_gas_percent"]
        matrix = out["matrix_gas_percent"]
        diag[f"bulk_gas_{tag}"] = bulk
        diag[f"disl_gas_{tag}"] = disl
        diag[f"qgb_gas_{tag}"] = qgb
        diag[f"matrix_gas_{tag}"] = matrix

        if min_bulk is not None:
            part_terms.append(_below(bulk, min_bulk, scale=70.0))
        if min_disl is not None:
            part_terms.append(_below(disl, min_disl, scale=70.0))
        if max_bulk is not None:
            part_terms.append(_above(bulk, max_bulk, scale=70.0))
        # Matrix gas should not dominate after the transition either.
        if T >= 1800.0:
            part_terms.append(_above(matrix, 10.0, scale=70.0))
        if max_qgb is not None:
            # qgb is a separate term because it is the main new physics sanity check.
            qgb_terms.append(_above(qgb, max_qgb, scale=50.0))

    # Shape anchors: bulk should generally decrease from mid T to high T; dislocation gas should increase.
    for bu in (1.1, 3.2):
        out1500 = m.run_model_point(1500.0, bu, cand, dt_h, n_modes, keep_history=False)
        out2000 = m.run_model_point(2000.0, bu, cand, dt_h, n_modes, keep_history=False)
        part_terms.append(max(0.0, out2000["bulk_gas_percent"] - out1500["bulk_gas_percent"]) / 70.0)
        part_terms.append(max(0.0, out1500["dislocation_gas_percent"] - out2000["dislocation_gas_percent"]) / 70.0)

    return _rmse(part_terms), _rmse(qgb_terms), diag


def highT_radius_guard_score_v5(cand, dt_h: float, n_modes: int) -> Tuple[float, float, Dict[str, float]]:
    """Return (radius_guard_score, saturation_score, diagnostics).

    V5 keeps the v4 ceiling on the single-size average dislocation-bubble radius
    and adds an explicit *shape* guard: after ~1800 K, R_d(T) should start
    flattening rather than accelerating.  This is not a hard fit to the Rizk
    model curve or to Ronchi data beyond the validated experimental range; it is
    a guardrail against using a non-saturating, micrometric single-size radius as
    a hidden compensation mechanism.
    """
    out1600 = m.run_model_point(1600.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    out1700 = m.run_model_point(1700.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    out1800 = m.run_model_point(1800.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    out1900 = m.run_model_point(1900.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    out2000 = m.run_model_point(2000.0, 1.3, cand, dt_h, n_modes, keep_history=False)

    r1600 = out1600["Rd_nm"]
    r1700 = out1700["Rd_nm"]
    r1800 = out1800["Rd_nm"]
    r1900 = out1900["Rd_nm"]
    r2000 = out2000["Rd_nm"]

    ratio_20_18 = r2000 / r1800 if r1800 > 0 else math.inf
    ratio_20_17 = r2000 / r1700 if r1700 > 0 else math.inf
    ratio_20_19 = r2000 / r1900 if r1900 > 0 else math.inf

    inc_16_17 = max(0.0, r1700 - r1600)
    inc_17_18 = max(0.0, r1800 - r1700)
    inc_18_19 = max(0.0, r1900 - r1800)
    inc_19_20 = max(0.0, r2000 - r1900)
    inc_18_20 = max(0.0, r2000 - r1800)

    # Absolute size guard: single-size mean radius should remain sub-micrometric.
    radius_terms = [
        max(0.0, r2000 - RD2000_MAX_NM) / RD2000_MAX_NM,
        max(0.0, r1900 - RD1900_SOFT_MAX_NM) / RD1900_SOFT_MAX_NM,
        0.6 * max(0.0, r1800 - RD1800_SOFT_MAX_NM) / RD1800_SOFT_MAX_NM,
        0.5 * max(0.0, ratio_20_17 - 4.0) / 4.0,
    ]

    # Saturation/flattening guard.  We do not force a perfectly flat curve, but
    # penalize continued acceleration after 1900 K and an excessive 1800->2000 jump.
    sat_terms = [
        max(0.0, ratio_20_18 - RD_RATIO_MAX) / RD_RATIO_MAX,
        max(0.0, ratio_20_19 - RD_1900_2000_RATIO_MAX) / RD_1900_2000_RATIO_MAX,
        max(0.0, inc_18_20 - RD_POST1800_DELTA_MAX_NM) / max(RD_POST1800_DELTA_MAX_NM, 1.0),
        max(0.0, inc_19_20 - RD_LAST_INCREMENT_FACTOR_MAX * max(inc_18_19, 1.0)) / max(RD_POST1800_DELTA_MAX_NM, 1.0),
        0.5 * max(0.0, inc_18_19 - 1.25 * max(inc_17_18, 1.0)) / max(RD_POST1800_DELTA_MAX_NM, 1.0),
    ]

    return _rmse(radius_terms), _rmse(sat_terms), {
        "Rd_1p3_1600K": r1600,
        "Rd_1p3_1700K": r1700,
        "Rd_1p3_1800K": r1800,
        "Rd_1p3_1900K": r1900,
        "Rd_1p3_2000K": r2000,
        "Rd_ratio_2000_over_1800": ratio_20_18,
        "Rd_ratio_2000_over_1700": ratio_20_17,
        "Rd_ratio_2000_over_1900": ratio_20_19,
        "Rd_inc_1600_1700_nm": inc_16_17,
        "Rd_inc_1700_1800_nm": inc_17_18,
        "Rd_inc_1800_1900_nm": inc_18_19,
        "Rd_inc_1900_2000_nm": inc_19_20,
        "Rd_inc_1800_2000_nm": inc_18_20,
        "Rd2000_max_nm": RD2000_MAX_NM,
        "Rd1900_soft_max_nm": RD1900_SOFT_MAX_NM,
        "Rd_ratio_max": RD_RATIO_MAX,
        "Rd_post1800_delta_max_nm": RD_POST1800_DELTA_MAX_NM,
        "Rd_last_increment_factor_max": RD_LAST_INCREMENT_FACTOR_MAX,
        "Rd_1900_2000_ratio_max": RD_1900_2000_RATIO_MAX,
    }

def score_candidate_v5(cand, dt_h: float, n_modes: int, use_full_exp: bool = True):
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
    score_fdot_prior = abs(math.log10(cand.fission_rate / m.FISSION_RATE_NOMINAL))
    score_rizk_prior = rizk_prior_score_v5(cand)
    score_partition, score_qgb, part_diag = gas_partition_score_v5(cand, dt_h, n_modes)
    score_radius_guard, score_radius_saturation, radius_diag = highT_radius_guard_score_v5(cand, dt_h, n_modes)

    # Keep the base high-T guard weak as well; it catches extreme divergence.
    score_base_highT_guard = m.highT_guard_score(cand, dt_h, n_modes)

    total = (
        m.W_SWELLING * score_swd
        + m.W_RD * score_Rd
        + m.W_ND_LEVEL * score_Nd
        + m.W_ND_DROP * score_Nd_drop
        + m.W_PRESSURE * score_pressure
        + m.W_FDOT_PRIOR * score_fdot_prior
        + W_RIZK_PRIOR * score_rizk_prior
        + W_PARTITION * score_partition
        + W_QGB * score_qgb
        + W_RADIUS_GUARD * score_radius_guard
        + W_RADIUS_SATURATION * score_radius_saturation
        + 0.04 * score_base_highT_guard
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
        "score_rizk_prior": score_rizk_prior,
        "score_partition": score_partition,
        "score_qgb": score_qgb,
        "score_radius_guard": score_radius_guard,
        "score_radius_saturation": score_radius_saturation,
        "score_base_highT_guard": score_base_highT_guard,
        "swD_1p3_1600K": out1600["swelling_d_percent"],
        "swB_1p3_1600K": out1600["swelling_b_percent"],
        "Rd_1p3_1600K": out1600["Rd_nm"],
        "Nd_1p3_1600K": out1600["Nd"],
        "Nd_1p3_1400K": outNd1400["Nd"],
        "Nd_1p3_1725K": outNd1725["Nd"],
        "Nd_drop_1725_over_1400_log10": m.safe_log10_ratio(outNd1725["Nd"], outNd1400["Nd"]),
        "p_d_over_eq_1p3_1600K": out1600["p_d_over_eq"],
        "p_b_over_eq_1p3_1600K": out1600["p_b_over_eq"],
        "swD_3p2_1600K": out32_1600["swelling_d_percent"],
        "swB_3p2_1600K": out32_1600["swelling_b_percent"],
        "bulk_gas_1p3_1600K": out1600["bulk_gas_percent"],
        "disl_gas_1p3_1600K": out1600["dislocation_gas_percent"],
        "qgb_gas_1p3_1600K": out1600["qgb_gas_percent"],
        "bulk_gas_3p2_1600K": out32_1600["bulk_gas_percent"],
        "disl_gas_3p2_1600K": out32_1600["dislocation_gas_percent"],
        "qgb_gas_3p2_1600K": out32_1600["qgb_gas_percent"],
        "max_f_cap_step_3p2_1600K": out32_1600["max_f_cap_step"],
        "capture_fraction_sum_3p2_1600K": out32_1600["capture_fraction_sum"],
        "capture_raw_sum_3p2_1600K": out32_1600["capture_raw_sum"],
        **part_diag,
        **radius_diag,
    }
    return result


ORIGINAL_ENQUEUE_KNOWN_GOOD_TRIALS = m.enqueue_known_good_trials

def enqueue_known_good_trials_v5(study):
    # Keep base anchors, then add Codex capture_only anchors.
    try:
        ORIGINAL_ENQUEUE_KNOWN_GOOD_TRIALS(study)
    except Exception:
        pass
    for seed in CODEX_CAPTURE_ONLY_SEEDS:
        try:
            study.enqueue_trial(seed, skip_if_exists=True)
        except TypeError:
            study.enqueue_trial(seed)


def main():
    parser = argparse.ArgumentParser(description="V5 Optuna calibration wrapper with gas partition/q_gb score and explicit high-T radius saturation guard.")
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
    parser.add_argument("--rd2000-max-nm", type=float, default=800.0)
    parser.add_argument("--rd-ratio-max", type=float, default=2.8)
    parser.add_argument("--rd1800-soft-max-nm", type=float, default=600.0)
    parser.add_argument("--rd1900-soft-max-nm", type=float, default=700.0)
    parser.add_argument("--rd-post1800-delta-max-nm", type=float, default=350.0)
    parser.add_argument("--rd-last-increment-factor-max", type=float, default=0.75)
    parser.add_argument("--rd-1900-2000-ratio-max", type=float, default=1.35)
    args = parser.parse_args()

    global DV_FIXED, DV_MIN, DV_MAX, W_RIZK_PRIOR, W_PARTITION, W_QGB, W_RADIUS_GUARD, W_RADIUS_SATURATION
    global RD2000_MAX_NM, RD_RATIO_MAX, RD1800_SOFT_MAX_NM, RD1900_SOFT_MAX_NM
    global RD_POST1800_DELTA_MAX_NM, RD_LAST_INCREMENT_FACTOR_MAX, RD_1900_2000_RATIO_MAX
    DV_FIXED = args.dv_fixed
    DV_MIN = args.dv_min
    DV_MAX = args.dv_max
    W_RIZK_PRIOR = args.rizk_prior_weight
    W_PARTITION = args.partition_weight
    W_QGB = args.qgb_weight
    W_RADIUS_GUARD = args.radius_guard_weight
    W_RADIUS_SATURATION = args.radius_saturation_weight
    RD2000_MAX_NM = args.rd2000_max_nm
    RD_RATIO_MAX = args.rd_ratio_max
    RD1800_SOFT_MAX_NM = args.rd1800_soft_max_nm
    RD1900_SOFT_MAX_NM = args.rd1900_soft_max_nm
    RD_POST1800_DELTA_MAX_NM = args.rd_post1800_delta_max_nm
    RD_LAST_INCREMENT_FACTOR_MAX = args.rd_last_increment_factor_max
    RD_1900_2000_RATIO_MAX = args.rd_1900_2000_ratio_max

    m.W_PRESSURE = args.pressure_weight
    m.PRESSURE_FREE_FACTOR = args.pressure_free_factor

    # Monkey-patch base module.
    m.optuna_candidate_from_trial = optuna_candidate_from_trial_v5
    m.score_candidate = score_candidate_v5
    m.enqueue_known_good_trials = enqueue_known_good_trials_v5

    if args.output_dir:
        out = args.output_dir
    else:
        dv_tag = f"dv{DV_FIXED:g}" if DV_FIXED is not None else f"dv{DV_MIN:g}-{DV_MAX:g}"
        out = os.path.join(
            "UN_M7_optuna_v5_results",
            args.family,
            f"{dv_tag}_pW{m.W_PRESSURE:g}_pF{m.PRESSURE_FREE_FACTOR:g}_part{W_PARTITION:g}_qgb{W_QGB:g}_prior{W_RIZK_PRIOR:g}_R{RD2000_MAX_NM:g}_sat",
        )

    print("#" * 120)
    print("V5 wrapper settings")
    print(f"family                 = {args.family}")
    print(f"Dv fixed/range          = {DV_FIXED} / [{DV_MIN}, {DV_MAX}]")
    print(f"pressure weight/free    = {m.W_PRESSURE} / {m.PRESSURE_FREE_FACTOR}")
    print(f"partition/qgb weights   = {W_PARTITION} / {W_QGB}")
    print(f"Rizk prior weight       = {W_RIZK_PRIOR}")
    print(f"radius guard weight     = {W_RADIUS_GUARD}")
    print(f"radius saturation weight= {W_RADIUS_SATURATION}")
    print(f"Rd2000/Rratio guard      = {RD2000_MAX_NM} nm / {RD_RATIO_MAX}")
    print(f"Rd1900/delta/slope guard = {RD1900_SOFT_MAX_NM} nm / {RD_POST1800_DELTA_MAX_NM} nm / {RD_LAST_INCREMENT_FACTOR_MAX}")
    print(f"output dir              = {out}")
    print("q_gb interpretation     = grain-boundary bubbles + fission gas release")
    print("capture law             = unchanged; active for capture_only and M7_no_phi")
    print("nucleation mass coupling= depends on family switch in base module")
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
