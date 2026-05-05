#!/usr/bin/env python3
"""
UN_M7_optuna_calibration_v11_rhoFT2_partition.py

V11 diagnostic wrapper:
  - uses the v10c rho_d(F,T) law with two scaling factors:
      rho_low_scale  : factor in the Ray-Blank documented range up to 1300 K
      rho_high_scale : high-T plateau factor reached after a linear ramp
  - keeps the documented Blank branch unchanged by default:
      --rho-low-min 1 --rho-low-max 1
  - changes the gas-partition score relative to v8/v10:
      * low-T bulk/dislocation split is mostly free;
      * q_gb / gas-to-grain-face is constrained to stay low and nearly flat;
      * high-T dislocation gas is constrained to become dominant and plateau-like;
  - uses manually digitized Rizk Fig. 8 radius anchors, especially R_d(T).

Run this file in the same folder as:
  UN_M7_optuna_calibration_v8.py
  UN_M7_optuna_calibration_v8_core.py
  UN_M7_optuna_calibration_v10c_rhoFT2_partition.py
"""

from __future__ import annotations

import argparse
import math
import os
from typing import Dict, List, Tuple

import UN_M7_optuna_calibration_v8 as v8
import UN_M7_optuna_calibration_v8_core as m
import UN_M7_optuna_calibration_v10c_rhoFT2_partition as v10c

# --------------------------------------------------------------------------------------
# Manually digitized Rizk Fig. 8 radius anchors, 1.3% FIMA.
# Use mainly dislocation; bulk optional/weak. Grain-boundary not used because reduced model
# has no explicit grain-boundary bubble radius variable.
# --------------------------------------------------------------------------------------

RIZK_FIG8_DISLOCATION_RADIUS: List[Tuple[float, float]] = [
    (899.999999999998, 43.17789221929055),
    (952.777777777776, 49.37019516057693),
    (1033.333333333333, 54.90035634986601),
    (1097.222222222222, 54.80542241133718),
    (1177.777777777778, 59.315737511601846),
    (1244.444444444443, 59.20871297878608),
    (1333.333333333335, 62.35471248401371),
    (1413.888888888889, 60.55595572051611),
    (1494.444444444443, 65.55952175249142),
    (1588.888888888887, 79.02146884249838),
    (1666.666666666665, 95.31977456143832),
    (1741.666666666667, 128.1481728359008),
    (1791.666666666665, 186.9950306266442),
    (1836.111111111109, 288.0994861369096),
    (1900.0, 387.4383570341449),
    (1961.111111111111, 408.3316765847392),
]

RIZK_FIG8_BULK_RADIUS: List[Tuple[float, float]] = [
    (916.6666666666666, 6.65733871278919),
    (986.111111111111, 7.014764119470463),
    (1061.111111111111, 7.390267027041024),
    (1150.0, 7.574938042366936),
    (1261.111111111111, 7.552172400073921),
    (1402.777777777778, 7.942087006896324),
    (1538.888888888889, 8.818447970098383),
    (1650.0, 12.84667832357808),
    (1725.0, 15.923045781997049),
    (1800.0, 21.99481236607679),
    (1852.777777777778, 32.092624062460324),
    (1927.777777777778, 43.14541399238171),
    (1988.888888888891, 52.06772800089737),
]

# To keep runtime acceptable, use a reduced but representative subset of anchors.
RIZK_FIG8_DISLOCATION_RADIUS_SCORE_POINTS = [
    (1033.333333333333, 54.90035634986601),
    (1333.333333333335, 62.35471248401371),
    (1588.888888888887, 79.02146884249838),
    (1741.666666666667, 128.1481728359008),
    (1836.111111111109, 288.0994861369096),
    (1900.0, 387.4383570341449),
    (1961.111111111111, 408.3316765847392),
]

RIZK_FIG8_BULK_RADIUS_SCORE_POINTS = [
    (1061.111111111111, 7.390267027041024),
    (1538.888888888889, 8.818447970098383),
    (1800.0, 21.99481236607679),
    (1988.888888888891, 52.06772800089737),
]

# --------------------------------------------------------------------------------------
# Weights specific to v11. CLI overrides in main().
# --------------------------------------------------------------------------------------

W_RIZK_FIG8_DISL_RADIUS = 0.35
W_RIZK_FIG8_BULK_RADIUS = 0.06
W_RIZK_FIG6_DISL_BURNUP = 0.02
W_V11_PARTITION = 0.85
W_V11_QGB = 1.10

# q_gb constraints. These are deliberately looser than an exact Rizk fit, but strong enough
# to avoid the v10 failure mode q_gb ~60-70% at 1600 K.
QGB_MAX_LOW_MID = 18.0
QGB_MAX_HIGH = 22.0
QGB_FLATNESS_SCALE = 25.0

# High-T dislocation gas plateau anchors. Low-T bulk/dislocation split is not constrained.
# tuple: burnup, T, min dislocation gas %, tag
DISL_GAS_HIGHT_ANCHORS = [
    (1.1, 1800.0, 55.0, "1p1_1800"),
    (1.1, 1900.0, 78.0, "1p1_1900"),
    (1.1, 2000.0, 84.0, "1p1_2000"),
    (3.2, 1800.0, 45.0, "3p2_1800"),
    (3.2, 1900.0, 70.0, "3p2_1900"),
    (3.2, 2000.0, 80.0, "3p2_2000"),
]

QGB_ANCHORS = [
    (1.1, 1200.0, QGB_MAX_LOW_MID, "1p1_1200"),
    (1.1, 1500.0, QGB_MAX_LOW_MID, "1p1_1500"),
    (1.1, 1600.0, QGB_MAX_LOW_MID, "1p1_1600"),
    (1.1, 1800.0, QGB_MAX_HIGH, "1p1_1800"),
    (1.1, 2000.0, QGB_MAX_HIGH, "1p1_2000"),
    (3.2, 1200.0, QGB_MAX_LOW_MID, "3p2_1200"),
    (3.2, 1500.0, QGB_MAX_LOW_MID, "3p2_1500"),
    (3.2, 1600.0, QGB_MAX_LOW_MID, "3p2_1600"),
    (3.2, 1800.0, QGB_MAX_HIGH, "3p2_1800"),
    (3.2, 2000.0, QGB_MAX_HIGH, "3p2_2000"),
]


def _rmse(vals: List[float]) -> float:
    vals = [v for v in vals if math.isfinite(v)]
    if not vals:
        return 0.0
    return math.sqrt(sum(v * v for v in vals) / len(vals))


def _above(value: float, max_allowed: float, scale: float) -> float:
    return max(0.0, float(value) - float(max_allowed)) / float(scale)


def _below(value: float, min_allowed: float, scale: float) -> float:
    return max(0.0, float(min_allowed) - float(value)) / float(scale)


def _log_ratio_error(pred: float, target: float) -> float:
    if pred <= 0.0 or target <= 0.0 or not math.isfinite(pred) or not math.isfinite(target):
        return 3.0
    return math.log10(pred / target)


def rizk_fig8_radius_score(cand, dt_h: float, n_modes: int):
    """Soft score against manually digitized Rizk Fig. 8 radius curves."""
    diag: Dict[str, float] = {}
    disl_terms: List[float] = []
    bulk_terms: List[float] = []

    for T, target in RIZK_FIG8_DISLOCATION_RADIUS_SCORE_POINTS:
        out = m.run_model_point(T, 1.3, cand, dt_h, n_modes, keep_history=False)
        pred = out["Rd_nm"]
        e = _log_ratio_error(pred, target)
        disl_terms.append(e)
        tag = str(int(round(T)))
        diag[f"rizk_fig8_Rd_pred_{tag}K"] = pred
        diag[f"rizk_fig8_Rd_target_{tag}K"] = target
        diag[f"rizk_fig8_Rd_logerr_{tag}K"] = e

    for T, target in RIZK_FIG8_BULK_RADIUS_SCORE_POINTS:
        out = m.run_model_point(T, 1.3, cand, dt_h, n_modes, keep_history=False)
        pred = out["Rb_nm"]
        e = _log_ratio_error(pred, target)
        bulk_terms.append(e)
        tag = str(int(round(T)))
        diag[f"rizk_fig8_Rb_pred_{tag}K"] = pred
        diag[f"rizk_fig8_Rb_target_{tag}K"] = target
        diag[f"rizk_fig8_Rb_logerr_{tag}K"] = e

    score_d = _rmse(disl_terms)
    score_b = _rmse(bulk_terms)
    diag["score_rizk_fig8_dislocation_radius"] = score_d
    diag["score_rizk_fig8_bulk_radius"] = score_b
    return score_d, score_b, diag


def gas_partition_score_v11(cand, dt_h: float, n_modes: int):
    """V11 gas partition score.

    Philosophy:
      - Do NOT force low-T bulk vs dislocation partition, because Blank suggests even P1
        can be defect-associated and because Rizk bulk inventory is not a direct P1 datum.
      - Strongly prevent premature loss to q_gb/gas-to-grain-face.
      - At high T, require dislocation gas to dominate and approach a plateau.
    """
    diag: Dict[str, float] = {}
    part_terms: List[float] = []
    qgb_terms: List[float] = []

    cache: Dict[Tuple[float, float], Dict[str, float]] = {}

    def get(bu: float, T: float):
        key = (float(bu), float(T))
        if key not in cache:
            cache[key] = m.run_model_point(T, bu, cand, dt_h, n_modes, keep_history=False)
        return cache[key]

    # q_gb magnitude: should not become the hidden gas sink.
    for bu, T, max_qgb, tag in QGB_ANCHORS:
        out = get(bu, T)
        bulk = out["bulk_gas_percent"]
        disl = out["dislocation_gas_percent"]
        qgb = out["qgb_gas_percent"]
        matrix = out["matrix_gas_percent"]
        diag[f"v11_bulk_gas_{tag}"] = bulk
        diag[f"v11_disl_gas_{tag}"] = disl
        diag[f"v11_qgb_gas_{tag}"] = qgb
        diag[f"v11_matrix_gas_{tag}"] = matrix
        qgb_terms.append(_above(qgb, max_qgb, scale=35.0))
        # Matrix should not dominate after transition; keep this weak.
        if T >= 1800.0:
            part_terms.append(0.5 * _above(matrix, 12.0, scale=60.0))

    # q_gb flatness: penalize a strong rise from mid-T to high-T.
    for bu in (1.1, 3.2):
        q1200 = get(bu, 1200.0)["qgb_gas_percent"]
        q1600 = get(bu, 1600.0)["qgb_gas_percent"]
        q2000 = get(bu, 2000.0)["qgb_gas_percent"]
        tag = str(bu).replace('.', 'p')
        diag[f"v11_qgb_delta_1600_1200_{tag}"] = q1600 - q1200
        diag[f"v11_qgb_delta_2000_1600_{tag}"] = q2000 - q1600
        qgb_terms.append(max(0.0, (q1600 - q1200) - 8.0) / QGB_FLATNESS_SCALE)
        qgb_terms.append(max(0.0, (q2000 - q1600) - 8.0) / QGB_FLATNESS_SCALE)

    # High-T dislocation dominance / plateau.
    for bu, T, min_disl, tag in DISL_GAS_HIGHT_ANCHORS:
        out = get(bu, T)
        disl = out["dislocation_gas_percent"]
        part_terms.append(_below(disl, min_disl, scale=70.0))
        diag[f"v11_disl_min_target_{tag}"] = min_disl
        diag[f"v11_disl_min_err_{tag}"] = _below(disl, min_disl, scale=70.0)

    # Plateau shape: after 1900 K dislocation fraction should not decrease strongly.
    for bu in (1.1, 3.2):
        d1800 = get(bu, 1800.0)["dislocation_gas_percent"]
        d1900 = get(bu, 1900.0)["dislocation_gas_percent"]
        d2000 = get(bu, 2000.0)["dislocation_gas_percent"]
        tag = str(bu).replace('.', 'p')
        diag[f"v11_disl_delta_1900_1800_{tag}"] = d1900 - d1800
        diag[f"v11_disl_delta_2000_1900_{tag}"] = d2000 - d1900
        part_terms.append(max(0.0, d1800 - d1900) / 60.0)
        part_terms.append(max(0.0, d1900 - d2000) / 60.0)
        # Avoid absurd non-plateau overshoot, but weakly: a large positive delta after 1900 is not wanted.
        part_terms.append(0.5 * max(0.0, (d2000 - d1900) - 18.0) / 60.0)

    return _rmse(part_terms), _rmse(qgb_terms), diag


def score_candidate_v11(cand, dt_h: float, n_modes: int, use_full_exp: bool = True):
    # Direct experimental data from core/v8.
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
    score_Rd_exp = m.log10_rmse(r_pairs)

    score_Nd_drop = m.nd_drop_score(cand, dt_h, n_modes)
    score_pressure = m.pressure_score(cand, dt_h, n_modes)
    score_fdot_prior = abs(math.log10(cand.fission_rate / m.FISSION_RATE_NOMINAL))
    score_rizk_prior = v8.rizk_prior_score_v5(cand)
    score_partition, score_qgb, part_diag = gas_partition_score_v11(cand, dt_h, n_modes)
    score_radius_guard, score_radius_saturation, radius_guard_diag = v8.highT_radius_guard_score_v5(cand, dt_h, n_modes)
    score_Nd_coalescence_shape, nd_shape_diag = v8.nd_coalescence_shape_score_v7(cand, dt_h, n_modes)
    score_highT_pressure, highT_pressure_diag = v8.highT_pressure_score_v7(cand, dt_h, n_modes)
    score_fig8_Rd, score_fig8_Rb, fig8_diag = rizk_fig8_radius_score(cand, dt_h, n_modes)
    score_fig6, fig6_diag = v10c.rizk_fig6_dislocation_burnup_score(cand, dt_h, n_modes)

    # Bulk plateau and radius band are intentionally not used in v11 total by default.
    # They are either not directly experimental (bulk partition) or too restrictive on high-T R_d.
    score_base_highT_guard = m.highT_guard_score(cand, dt_h, n_modes)

    total = (
        m.W_SWELLING * score_swd
        + m.W_RD * score_Rd_exp
        + m.W_ND_LEVEL * score_Nd
        + m.W_ND_DROP * score_Nd_drop
        + m.W_PRESSURE * score_pressure
        + m.W_FDOT_PRIOR * score_fdot_prior
        + v8.W_RIZK_PRIOR * score_rizk_prior
        + W_V11_PARTITION * score_partition
        + W_V11_QGB * score_qgb
        + v8.W_RADIUS_GUARD * score_radius_guard
        + v8.W_ND_DROP_TARGET * score_Nd_coalescence_shape
        + v8.W_HIGHT_PRESSURE * score_highT_pressure
        + W_RIZK_FIG8_DISL_RADIUS * score_fig8_Rd
        + W_RIZK_FIG8_BULK_RADIUS * score_fig8_Rb
        + W_RIZK_FIG6_DISL_BURNUP * score_fig6
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
        "score_Rd_exp": score_Rd_exp,
        "score_Rd": score_Rd_exp,
        "score_Nd_drop": score_Nd_drop,
        "score_pressure": score_pressure,
        "score_fdot_prior": score_fdot_prior,
        "score_rizk_prior": score_rizk_prior,
        "score_partition": score_partition,
        "score_qgb": score_qgb,
        "score_radius_guard": score_radius_guard,
        "score_radius_saturation": score_radius_saturation,
        "score_Nd_coalescence_shape": score_Nd_coalescence_shape,
        "score_highT_pressure": score_highT_pressure,
        "score_rizk_fig8_dislocation_radius": score_fig8_Rd,
        "score_rizk_fig8_bulk_radius": score_fig8_Rb,
        "score_rizk_fig6_dislocation_burnup": score_fig6,
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
        **radius_guard_diag,
        **nd_shape_diag,
        **highT_pressure_diag,
        **fig8_diag,
        **fig6_diag,
    }

    rho_low_scale, rho_high_scale = v10c._scales_for_candidate(cand)
    result["rhoFT2_enabled"] = int(v10c.RHOFT2_ENABLED)
    result["rho_low_scale"] = rho_low_scale
    result["rho_high_scale"] = rho_high_scale
    result["rho_transition_end_K"] = v10c.RHO_TRANSITION_END_K
    for bu in (1.1, 1.3, 3.2, 6.0):
        tag_bu = str(bu).replace('.', 'p')
        result[f"rhoFT2_base_1025_{tag_bu}FIMA"] = v10c.rho_burnup_1025(bu)
        for T in (1025.0, 1300.0, 1600.0, 1800.0, 2000.0):
            result[f"rhoFT2_factor_{int(T)}K"] = v10c.rhoFT2_factor(T, cand)
            result[f"rhoFT2_eff_{tag_bu}FIMA_{int(T)}K"] = v10c.rho_ray_blank_eff_FT2(T, bu, cand)

    return result


def main():
    parser = argparse.ArgumentParser(description="V11: rhoFT2 + gas-to-grain-face constrained + high-T dislocation gas plateau + Rizk Fig.8 radius anchors.")
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

    # Direct experimental weights.
    parser.add_argument("--exp-swelling-weight", type=float, default=1.10)
    parser.add_argument("--exp-rd-weight", type=float, default=0.85)
    parser.add_argument("--exp-nd-level-weight", type=float, default=0.85)
    parser.add_argument("--base-nd-drop-weight", type=float, default=0.80)
    parser.add_argument("--nd-drop-target-weight", type=float, default=0.75)

    # Physical/diagnostic weights.
    parser.add_argument("--pressure-weight", type=float, default=0.22)
    parser.add_argument("--pressure-free-factor", type=float, default=3.0)
    parser.add_argument("--highT-pressure-weight", type=float, default=0.22)
    parser.add_argument("--rizk-prior-weight", type=float, default=0.08)
    parser.add_argument("--radius-guard-weight", type=float, default=0.30)
    parser.add_argument("--rd2000-max-nm", type=float, default=1400.0)
    parser.add_argument("--rd-ratio-max", type=float, default=12.0)
    parser.add_argument("--rd1800-soft-max-nm", type=float, default=1100.0)
    parser.add_argument("--rd1900-soft-max-nm", type=float, default=1300.0)

    # V11 specific weights.
    parser.add_argument("--v11-partition-weight", type=float, default=0.85)
    parser.add_argument("--v11-qgb-weight", type=float, default=1.10)
    parser.add_argument("--fig8-disl-radius-weight", type=float, default=0.35)
    parser.add_argument("--fig8-bulk-radius-weight", type=float, default=0.06)
    parser.add_argument("--fig6-burnup-weight", type=float, default=0.02)
    parser.add_argument("--qgb-max-low-mid", type=float, default=18.0)
    parser.add_argument("--qgb-max-high", type=float, default=22.0)

    # rhoFT2 controls.
    parser.add_argument("--rhoFT2-disable", action="store_true")
    parser.add_argument("--rho-low-min", type=float, default=1.0)
    parser.add_argument("--rho-low-max", type=float, default=1.0)
    parser.add_argument("--rho-high-min", type=float, default=0.7)
    parser.add_argument("--rho-high-max", type=float, default=8.0)
    parser.add_argument("--rho-transition-end-K", type=float, default=1600.0)
    parser.add_argument("--rhoFT-fab", type=float, default=3.0e13)
    parser.add_argument("--Kd-min", type=float, default=3.0e5)
    parser.add_argument("--Kd-max", type=float, default=8.0e5)
    parser.add_argument("--sample-rhoD-reference", action="store_true", help="also sample old candidate rho_d as global scale; off by default")
    parser.add_argument("--rhoD-ref-min", type=float, default=1.0e13)
    parser.add_argument("--rhoD-ref-max", type=float, default=8.0e13)

    args = parser.parse_args()

    global W_RIZK_FIG8_DISL_RADIUS, W_RIZK_FIG8_BULK_RADIUS, W_RIZK_FIG6_DISL_BURNUP, W_V11_PARTITION, W_V11_QGB
    global QGB_MAX_LOW_MID, QGB_MAX_HIGH, QGB_ANCHORS

    W_RIZK_FIG8_DISL_RADIUS = args.fig8_disl_radius_weight
    W_RIZK_FIG8_BULK_RADIUS = args.fig8_bulk_radius_weight
    W_RIZK_FIG6_DISL_BURNUP = args.fig6_burnup_weight
    W_V11_PARTITION = args.v11_partition_weight
    W_V11_QGB = args.v11_qgb_weight
    QGB_MAX_LOW_MID = args.qgb_max_low_mid
    QGB_MAX_HIGH = args.qgb_max_high
    QGB_ANCHORS = [
        (1.1, 1200.0, QGB_MAX_LOW_MID, "1p1_1200"),
        (1.1, 1500.0, QGB_MAX_LOW_MID, "1p1_1500"),
        (1.1, 1600.0, QGB_MAX_LOW_MID, "1p1_1600"),
        (1.1, 1800.0, QGB_MAX_HIGH, "1p1_1800"),
        (1.1, 2000.0, QGB_MAX_HIGH, "1p1_2000"),
        (3.2, 1200.0, QGB_MAX_LOW_MID, "3p2_1200"),
        (3.2, 1500.0, QGB_MAX_LOW_MID, "3p2_1500"),
        (3.2, 1600.0, QGB_MAX_LOW_MID, "3p2_1600"),
        (3.2, 1800.0, QGB_MAX_HIGH, "3p2_1800"),
        (3.2, 2000.0, QGB_MAX_HIGH, "3p2_2000"),
    ]

    # Direct experimental weights.
    m.W_SWELLING = args.exp_swelling_weight
    m.W_RD = args.exp_rd_weight
    m.W_ND_LEVEL = args.exp_nd_level_weight
    m.W_ND_DROP = args.base_nd_drop_weight
    m.W_PRESSURE = args.pressure_weight
    m.PRESSURE_FREE_FACTOR = args.pressure_free_factor

    # v8 guard/prior globals reused by v11.
    v8.W_RIZK_PRIOR = args.rizk_prior_weight
    v8.W_RADIUS_GUARD = args.radius_guard_weight
    v8.W_ND_DROP_TARGET = args.nd_drop_target_weight
    v8.W_HIGHT_PRESSURE = args.highT_pressure_weight
    v8.RD2000_MAX_NM = args.rd2000_max_nm
    v8.RD_RATIO_MAX = args.rd_ratio_max
    v8.RD1800_SOFT_MAX_NM = args.rd1800_soft_max_nm
    v8.RD1900_SOFT_MAX_NM = args.rd1900_soft_max_nm
    v8.W_RADIUS_SATURATION = 0.0
    v8.W_RADIUS_BAND = 0.0
    v8.W_BULK_PLATEAU = 0.0

    # v10c rhoFT2 globals used by its candidate generator and run_model_point wrapper.
    v10c.W_RIZK_FIG6_DISL_BURNUP = W_RIZK_FIG6_DISL_BURNUP
    v10c.RHOFT2_ENABLED = not args.rhoFT2_disable
    v10c.RHO_LOW_MIN = args.rho_low_min
    v10c.RHO_LOW_MAX = args.rho_low_max
    v10c.RHO_HIGH_MIN = args.rho_high_min
    v10c.RHO_HIGH_MAX = args.rho_high_max
    v10c.RHO_TRANSITION_END_K = args.rho_transition_end_K
    v10c.RHO_FAB = args.rhoFT_fab
    v10c.K_D_MIN = args.Kd_min
    v10c.K_D_MAX = args.Kd_max
    v10c.SAMPLE_RHO_D_REFERENCE = args.sample_rhoD_reference
    v10c.RHO_D_REF_MIN = args.rhoD_ref_min
    v10c.RHO_D_REF_MAX = args.rhoD_ref_max

    # Monkey-patch core hooks.
    m.run_model_point = v10c.run_model_point_rhoFT2
    m.optuna_candidate_from_trial = v10c.optuna_candidate_from_trial_v10c
    m.candidate_from_score_row = v10c.candidate_from_score_row_v10c
    m.score_candidate = score_candidate_v11
    m.enqueue_known_good_trials = v10c.enqueue_known_good_trials_v10c

    if args.output_dir:
        out = args.output_dir
    else:
        out = os.path.join(
            "UN_M7_optuna_v11_rhoFT2_partition_results",
            args.family,
            f"rhoHigh{v10c.RHO_HIGH_MIN:g}-{v10c.RHO_HIGH_MAX:g}_Tsat{v10c.RHO_TRANSITION_END_K:g}"
            f"_qgb{W_V11_QGB:g}_part{W_V11_PARTITION:g}_Rfig8{W_RIZK_FIG8_DISL_RADIUS:g}",
        )

    print("#" * 120)
    print("V11 rhoFT2 + gas-to-grain-face + high-T dislocation plateau settings")
    print(f"family                         = {args.family}")
    print(f"rhoFT2 enabled                 = {v10c.RHOFT2_ENABLED}")
    print(f"rho_low_scale range             = {v10c.RHO_LOW_MIN:g} -- {v10c.RHO_LOW_MAX:g}")
    print(f"rho_high_scale range            = {v10c.RHO_HIGH_MIN:g} -- {v10c.RHO_HIGH_MAX:g}")
    print(f"rho high-T linear ramp          = 1300 K -> {v10c.RHO_TRANSITION_END_K:g} K")
    print(f"K_d range                       = {v10c.K_D_MIN:g} -- {v10c.K_D_MAX:g}")
    print(f"direct exp weights              = sw {m.W_SWELLING}, Rd {m.W_RD}, Nd {m.W_ND_LEVEL}, Nd-drop {m.W_ND_DROP}")
    print(f"v11 partition/qgb weights       = {W_V11_PARTITION} / {W_V11_QGB}")
    print(f"qgb max low-mid/high            = {QGB_MAX_LOW_MID} / {QGB_MAX_HIGH}")
    print(f"Fig8 disl/bulk radius weights   = {W_RIZK_FIG8_DISL_RADIUS} / {W_RIZK_FIG8_BULK_RADIUS}")
    print(f"Fig6 burnup weight              = {W_RIZK_FIG6_DISL_BURNUP}")
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
