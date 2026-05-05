#!/usr/bin/env python3
"""
UN_M7_optuna_calibration_v14_rhoSat_qgbStrict_NdAnchors_STANDALONE.py

V14 diagnostic wrapper:
  - same M7/v8 physics equations;
  - Ray & Blank rho_d(F,T) with a saturating temperature shape and one Optuna scale factor:
        rho_d(F,T) = rho_scale * rho_d0(F) * f_T_sat(T)
  - f_T_sat(T) is normalized at 1025 K and saturates at high T, avoiding the
    v11 artifact where only the high-T plateau could be lowered;
  - gas-to-grain-face/q_gb remains a strong guardrail;
  - point-by-point Nd matching is weakened, while an explicit high-T Nd drop target is added;
  - Rizk Fig. 8 dislocation radius anchors are strengthened.

Run this file in the same folder as:
  UN_M7_optuna_calibration_v8.py
  UN_M7_optuna_calibration_v8_core.py
"""

from __future__ import annotations

# --- path setup (added by tools/fix_paths.py after un_calibration/ reorg) ---
import sys as _sys
from pathlib import Path as _Path
_sys.path.insert(0, str(_Path(__file__).resolve().parent.parent))
import _pathsetup  # noqa: F401
# --- end path setup ---


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
RHO_SCALE_MIN = 0.5
RHO_SCALE_MAX = 2.0
# Saturating Ray & Blank T-shape parameters; rho units are 1e10 cm/cm^3.
RHO_SAT_RHO940 = 6.3571
RHO_SAT_RHOINF = 9.1036
RHO_SAT_TAU_K = 203.76
RHO_D_REFERENCE = m.RHO_D_NOMINAL
K_D_MIN = 3.0e5
K_D_MAX = 8.0e5
SAMPLE_RHO_D_REFERENCE = False
RHO_D_REF_MIN = 1.0e13
RHO_D_REF_MAX = 8.0e13

# label -> rho_scale. Needed because Candidate dataclass has no extra fields.
RHO_SCALE_BY_LABEL: Dict[str, float] = {}


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


def _rho_scale_for_candidate(cand) -> float:
    return RHO_SCALE_BY_LABEL.get(cand.label, 1.0)


def rho_sat_shape10(T: float) -> float:
    """Saturating fit to Ray & Blank Table 3 at 6.8 a/o.

    rho_shape(T) = rho_inf - (rho_inf-rho_940)*exp[-(T-940)/tau]
    Returned units are 1e10 cm/cm^3; only the normalized shape is used.
    """
    T = float(T)
    return RHO_SAT_RHOINF - (RHO_SAT_RHOINF - RHO_SAT_RHO940) * math.exp(-(T - 940.0) / RHO_SAT_TAU_K)


def rho_sat_factor_raw(T: float) -> float:
    return rho_sat_shape10(float(T)) / rho_sat_shape10(TREF_RB)


def rhoFT2_factor(T: float, cand) -> float:
    """V13 temperature multiplier.

    Uses a monotonic concave saturating shape from Ray & Blank data, normalized at
    1025 K, multiplied by a single global scale. This removes the v11/v12 freedom
    to alter only the high-T plateau or the slope.
    """
    if not RHOFT2_ENABLED:
        return 1.0
    rho_scale = _rho_scale_for_candidate(cand)
    f_shape = rho_sat_factor_raw(float(T))
    return max(0.05, rho_scale * f_shape)


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
    rho_scale = _suggest_float_maybe_fixed(trial, "rho_scale", RHO_SCALE_MIN, RHO_SCALE_MAX, log=True)

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

    cand = m.Candidate(label=f"{family}_v13_trial_{trial.number:05d}", **vals)
    RHO_SCALE_BY_LABEL[cand.label] = rho_scale
    return cand


def candidate_from_score_row_v10c(row: Dict, label_prefix: str = "best_M7"):
    cand = _ORIGINAL_CANDIDATE_FROM_SCORE_ROW(row, label_prefix=label_prefix)
    rho_scale = m.safe_float(row.get("rho_scale", row.get("rho_global_scale", 1.0)), 1.0)
    RHO_SCALE_BY_LABEL[cand.label] = rho_scale
    return cand


def enqueue_known_good_trials_v10c(study):
    # Existing seeds plus neutral rho scales. They guide only; Optuna explores around them.
    try:
        v8.enqueue_known_good_trials_v5(study)
    except Exception:
        pass
    for k in [
        {"rho_scale": 1.0, "K_d": 5.0e5},
        {"rho_scale": 0.75, "K_d": 3.5e5},
        {"rho_scale": 1.25, "K_d": 5.0e5},
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

    rho_scale = _rho_scale_for_candidate(cand)
    row["rhoFT2_enabled"] = int(RHOFT2_ENABLED)
    row["rho_scale"] = rho_scale
    row["rho_shape"] = "saturating_RayBlank"
    row["rho_D_reference"] = RHO_D_REFERENCE
    row["rhoFT2_sample_rhoD_reference"] = int(SAMPLE_RHO_D_REFERENCE)
    for bu in (1.1, 1.3, 3.2, 6.0):
        tag_bu = str(bu).replace('.', 'p')
        row[f"rhoFT2_base_1025_{tag_bu}FIMA"] = rho_burnup_1025(bu)
        for T in (1025.0, 1300.0, 1600.0, 1800.0, 2000.0):
            row[f"rhoFT2_factor_{int(T)}K"] = rhoFT2_factor(T, cand)
            row[f"rhoFT2_eff_{tag_bu}FIMA_{int(T)}K"] = rho_ray_blank_eff_FT2(T, bu, cand)
    return row


# --------------------------------------------------------------------------------------
# V13b code starts here.
# --------------------------------------------------------------------------------------

"""
UN_M7_optuna_calibration_v14_rhoSat_qgbStrict_NdAnchors_STANDALONE.py

V14 diagnostic wrapper:
  - uses a Ray-Blank rho_d(F,T) law with a saturating temperature shape and one global scale:
      rho_scale : multiplies the whole dislocation-density correlation
  - no independent high-T plateau or slope factor is allowed; the T-dependence is monotonic, concave and saturating.
  - changes the gas-partition score relative to v8/v10:
      * low-T bulk/dislocation split is mostly free;
      * q_gb / gas-to-grain-face is constrained to stay low and nearly flat;
      * high-T dislocation gas is constrained to become dominant and plateau-like;
  - uses manually digitized Rizk Fig. 8 radius anchors, especially R_d(T).

Run this file in the same folder as:
  UN_M7_optuna_calibration_v8.py
  UN_M7_optuna_calibration_v8_core.py
  UN_M7_optuna_calibration_v14_rhoSat_qgbStrict_NdAnchors_STANDALONE.py
"""

import argparse
import math
import os
from typing import Dict, List, Tuple

import UN_M7_optuna_calibration_v8 as v8
import UN_M7_optuna_calibration_v8_core as m

class _V10cProxy:
    def __getattr__(self, name):
        return globals()[name]
    def __setattr__(self, name, value):
        globals()[name] = value

v10c = _V10cProxy()

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
# Weights specific to v13. CLI overrides in main().
# --------------------------------------------------------------------------------------

W_RIZK_FIG8_DISL_RADIUS = 0.75
W_RIZK_FIG8_BULK_RADIUS = 0.04
W_RIZK_FIG6_DISL_BURNUP = 0.02
W_V13_PARTITION = 0.95
W_V13_QGB = 2.50
W_ND_HIGHT_LOW_TARGET = 0.80

# V14 Nd anchor weights.
# Instead of relying on log-drop ratios, v14 enforces:
#   (i) early/mid-T N_d close to the experimental high-density region;
#   (ii) absolute high-T upper bounds for N_d.
W_ND_EARLY_EXP = 1.20
W_ND_HIGHT_ANCHOR_ABS = 1.40
ND_EARLY_MAX_T = 1507.5
ND_ANCHOR_1800 = 8.0e18
ND_ANCHOR_1900 = 4.0e18
ND_ANCHOR_2000 = 1.5e18

# q_gb constraints. These are deliberately looser than an exact Rizk fit, but strong enough
# to avoid the v10 failure mode q_gb ~60-70% at 1600 K.
QGB_MAX_LOW_MID = 18.0
QGB_MAX_HIGH = 22.0
QGB_FLATNESS_SCALE = 25.0
# V13b: q_gb limits are burnup-dependent. Rizk Fig. 9 suggests q_gb+FGR
# remains very small at 1.1% FIMA even at high T, while 3.2% FIMA can tolerate
# somewhat larger intergranular/FGR inventory.
QGB_1P1_LOW_MID = 5.0
QGB_1P1_HIGH = 5.0
QGB_3P2_LOW_MID = 8.0
QGB_3P2_HIGH = 12.0
QGB_PENALTY_SCALE = 25.0

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
    (1.1, 1200.0, QGB_1P1_LOW_MID, "1p1_1200"),
    (1.1, 1500.0, QGB_1P1_LOW_MID, "1p1_1500"),
    (1.1, 1600.0, QGB_1P1_LOW_MID, "1p1_1600"),
    (1.1, 1800.0, QGB_1P1_HIGH, "1p1_1800"),
    (1.1, 1900.0, QGB_1P1_HIGH, "1p1_1900"),
    (1.1, 2000.0, QGB_1P1_HIGH, "1p1_2000"),
    (3.2, 1200.0, QGB_3P2_LOW_MID, "3p2_1200"),
    (3.2, 1500.0, QGB_3P2_LOW_MID, "3p2_1500"),
    (3.2, 1600.0, QGB_3P2_LOW_MID, "3p2_1600"),
    (3.2, 1800.0, QGB_3P2_HIGH, "3p2_1800"),
    (3.2, 1900.0, QGB_3P2_HIGH, "3p2_1900"),
    (3.2, 2000.0, QGB_3P2_HIGH, "3p2_2000"),
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


def gas_partition_score_v13(cand, dt_h: float, n_modes: int):
    """V13 gas partition score.

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
        diag[f"v13_bulk_gas_{tag}"] = bulk
        diag[f"v13_disl_gas_{tag}"] = disl
        diag[f"v13_qgb_gas_{tag}"] = qgb
        diag[f"v13_matrix_gas_{tag}"] = matrix
        qgb_terms.append(_above(qgb, max_qgb, scale=QGB_PENALTY_SCALE))
        # Matrix should not dominate after transition; keep this weak.
        if T >= 1800.0:
            part_terms.append(0.5 * _above(matrix, 12.0, scale=60.0))

    # q_gb flatness: penalize a strong rise from mid-T to high-T.
    for bu in (1.1, 3.2):
        q1200 = get(bu, 1200.0)["qgb_gas_percent"]
        q1600 = get(bu, 1600.0)["qgb_gas_percent"]
        q2000 = get(bu, 2000.0)["qgb_gas_percent"]
        tag = str(bu).replace('.', 'p')
        diag[f"v13_qgb_delta_1600_1200_{tag}"] = q1600 - q1200
        diag[f"v13_qgb_delta_2000_1600_{tag}"] = q2000 - q1600
        qgb_terms.append(max(0.0, (q1600 - q1200) - 8.0) / QGB_FLATNESS_SCALE)
        qgb_terms.append(max(0.0, (q2000 - q1600) - 8.0) / QGB_FLATNESS_SCALE)

    # High-T dislocation dominance / plateau.
    for bu, T, min_disl, tag in DISL_GAS_HIGHT_ANCHORS:
        out = get(bu, T)
        disl = out["dislocation_gas_percent"]
        part_terms.append(_below(disl, min_disl, scale=70.0))
        diag[f"v13_disl_min_target_{tag}"] = min_disl
        diag[f"v13_disl_min_err_{tag}"] = _below(disl, min_disl, scale=70.0)

    # Plateau shape: after 1900 K dislocation fraction should not decrease strongly.
    for bu in (1.1, 3.2):
        d1800 = get(bu, 1800.0)["dislocation_gas_percent"]
        d1900 = get(bu, 1900.0)["dislocation_gas_percent"]
        d2000 = get(bu, 2000.0)["dislocation_gas_percent"]
        tag = str(bu).replace('.', 'p')
        diag[f"v13_disl_delta_1900_1800_{tag}"] = d1900 - d1800
        diag[f"v13_disl_delta_2000_1900_{tag}"] = d2000 - d1900
        part_terms.append(max(0.0, d1800 - d1900) / 60.0)
        part_terms.append(max(0.0, d1900 - d2000) / 60.0)
        # Avoid absurd non-plateau overshoot, but weakly: a large positive delta after 1900 is not wanted.
        part_terms.append(0.5 * max(0.0, (d2000 - d1900) - 18.0) / 60.0)

    return _rmse(part_terms), _rmse(qgb_terms), diag




def nd_early_exp_score_v14(cand, dt_h: float, n_modes: int):
    """Fit only the early/mid-temperature experimental N_d points.

    Motivation: in v13/v13b the high-T drop/ratio terms allowed N_d to start too low.
    Here the high-density part of the experimental cloud is used as an anchor, while
    the later high-T collapse is handled by separate absolute upper bounds.
    """
    diag: Dict[str, float] = {}
    terms: List[float] = []
    n_used = 0
    for exp in m.EXP_ND_T_13:
        T = float(exp["T"])
        if T <= ND_EARLY_MAX_T:
            out = m.run_model_point(T, 1.3, cand, dt_h, n_modes, keep_history=False)
            pred = max(out["Nd"], 1.0)
            target = max(float(exp["N"]), 1.0)
            terms.append(_log_ratio_error(pred, target))
            n_used += 1
    score = _rmse(terms)
    diag["score_Nd_early_exp"] = score
    diag["Nd_early_exp_max_T"] = ND_EARLY_MAX_T
    diag["Nd_early_exp_n_points"] = n_used

    # Additional diagnostic anchors at useful temperatures.
    for T in (1150.0, 1200.0, 1300.0, 1400.0, 1500.0):
        out = m.run_model_point(T, 1.3, cand, dt_h, n_modes, keep_history=False)
        diag[f"Nd_v14_diag_{int(T)}K"] = out["Nd"]
    return score, diag


def nd_highT_abs_anchor_score_v14(cand, dt_h: float, n_modes: int):
    """Absolute high-temperature N_d upper-bound anchors.

    This replaces the old ratio/drop target. The intention is not to punish whether
    the drop occurs slightly earlier or later, but to require that by 1800-2000 K
    the dislocation-bubble concentration has actually decreased.
    """
    diag: Dict[str, float] = {}
    anchors = [
        (1800.0, ND_ANCHOR_1800, "1800"),
        (1900.0, ND_ANCHOR_1900, "1900"),
        (2000.0, ND_ANCHOR_2000, "2000"),
    ]
    terms: List[float] = []
    for T, maxN, tag in anchors:
        out = m.run_model_point(T, 1.3, cand, dt_h, n_modes, keep_history=False)
        nd = max(out["Nd"], 1.0)
        # Penalize only if N_d is above the high-T upper-bound anchor.
        err = max(0.0, math.log10(nd / maxN))
        terms.append(err)
        diag[f"Nd_v14_anchor_{tag}K"] = nd
        diag[f"Nd_v14_anchor_{tag}K_max"] = maxN
        diag[f"Nd_v14_anchor_{tag}K_err"] = err
    score = _rmse(terms)
    diag["score_Nd_highT_abs_anchor"] = score
    return score, diag


def nd_highT_low_target_score_v13(cand, dt_h: float, n_modes: int):
    """High-temperature Nd-shape target.

    This intentionally reduces the weight of point-by-point Nd matching and instead
    asks for a visible high-T drop, even if the drop occurs later than the
    experimental/Rizk data points. Targets are in log10 ratios relative to 1400 K.
    """
    diag: Dict[str, float] = {}
    out1400 = m.run_model_point(1400.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    out1900 = m.run_model_point(1900.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    out2000 = m.run_model_point(2000.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    n1400 = max(out1400["Nd"], 1.0)
    n1900 = max(out1900["Nd"], 1.0)
    n2000 = max(out2000["Nd"], 1.0)
    lr1900 = math.log10(n1900 / n1400)
    lr2000 = math.log10(n2000 / n1400)
    target1900 = -0.20
    target2000 = -0.35
    # Penalize only if the drop is too small; stronger drop is accepted.
    e1900 = max(0.0, lr1900 - target1900)
    e2000 = max(0.0, lr2000 - target2000)
    diag["Nd_1400_1p3"] = n1400
    diag["Nd_1900_1p3"] = n1900
    diag["Nd_2000_1p3"] = n2000
    diag["Nd_logdrop_1900_over_1400"] = lr1900
    diag["Nd_logdrop_2000_over_1400"] = lr2000
    diag["Nd_logdrop_target_1900_over_1400"] = target1900
    diag["Nd_logdrop_target_2000_over_1400"] = target2000
    diag["score_Nd_highT_low_target"] = _rmse([e1900, e2000])
    return diag["score_Nd_highT_low_target"], diag


def score_candidate_v13(cand, dt_h: float, n_modes: int, use_full_exp: bool = True):
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
    score_partition, score_qgb, part_diag = gas_partition_score_v13(cand, dt_h, n_modes)
    score_radius_guard, score_radius_saturation, radius_guard_diag = v8.highT_radius_guard_score_v5(cand, dt_h, n_modes)
    score_Nd_coalescence_shape, nd_shape_diag = v8.nd_coalescence_shape_score_v7(cand, dt_h, n_modes)
    score_Nd_highT_low_target, nd_highT_diag = nd_highT_low_target_score_v13(cand, dt_h, n_modes)
    score_Nd_early_exp, nd_early_diag = nd_early_exp_score_v14(cand, dt_h, n_modes)
    score_Nd_highT_abs_anchor, nd_highT_abs_diag = nd_highT_abs_anchor_score_v14(cand, dt_h, n_modes)
    score_highT_pressure, highT_pressure_diag = v8.highT_pressure_score_v7(cand, dt_h, n_modes)
    score_fig8_Rd, score_fig8_Rb, fig8_diag = rizk_fig8_radius_score(cand, dt_h, n_modes)
    score_fig6, fig6_diag = v10c.rizk_fig6_dislocation_burnup_score(cand, dt_h, n_modes)

    # Bulk plateau and radius band are intentionally not used in v13 total by default.
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
        + W_V13_PARTITION * score_partition
        + W_V13_QGB * score_qgb
        + v8.W_RADIUS_GUARD * score_radius_guard
        + v8.W_ND_DROP_TARGET * score_Nd_coalescence_shape
        + W_ND_HIGHT_LOW_TARGET * score_Nd_highT_low_target
        + W_ND_EARLY_EXP * score_Nd_early_exp
        + W_ND_HIGHT_ANCHOR_ABS * score_Nd_highT_abs_anchor
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
        "score_Nd_highT_low_target": score_Nd_highT_low_target,
        "score_Nd_early_exp": score_Nd_early_exp,
        "score_Nd_highT_abs_anchor": score_Nd_highT_abs_anchor,
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
        **nd_highT_diag,
        **highT_pressure_diag,
        **fig8_diag,
        **fig6_diag,
    }

    rho_scale = v10c._rho_scale_for_candidate(cand)
    result["rhoFT2_enabled"] = int(v10c.RHOFT2_ENABLED)
    result["rho_scale"] = rho_scale
    result["rho_shape"] = "saturating_RayBlank"
    for bu in (1.1, 1.3, 3.2, 6.0):
        tag_bu = str(bu).replace('.', 'p')
        result[f"rhoFT2_base_1025_{tag_bu}FIMA"] = v10c.rho_burnup_1025(bu)
        for T in (1025.0, 1300.0, 1600.0, 1800.0, 2000.0):
            result[f"rhoFT2_factor_{int(T)}K"] = v10c.rhoFT2_factor(T, cand)
            result[f"rhoFT2_eff_{tag_bu}FIMA_{int(T)}K"] = v10c.rho_ray_blank_eff_FT2(T, bu, cand)

    return result


def main():
    parser = argparse.ArgumentParser(description="V13b: rhoSat + stricter burnup-dependent q_gb anchors + high-T dislocation gas plateau + Rizk Fig.8 radius anchors.")
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
    parser.add_argument("--exp-swelling-weight", type=float, default=1.00)
    parser.add_argument("--exp-rd-weight", type=float, default=0.90)
    parser.add_argument("--exp-nd-level-weight", type=float, default=0.40)
    parser.add_argument("--base-nd-drop-weight", type=float, default=1.00)
    parser.add_argument("--nd-drop-target-weight", type=float, default=1.10)
    parser.add_argument("--nd-highT-target-weight", type=float, default=0.80)
    parser.add_argument("--nd-early-exp-weight", type=float, default=1.20)
    parser.add_argument("--nd-highT-anchor-weight", type=float, default=1.40)
    parser.add_argument("--nd-early-max-T", type=float, default=1507.5)
    parser.add_argument("--nd-anchor-1800", type=float, default=8.0e18)
    parser.add_argument("--nd-anchor-1900", type=float, default=4.0e18)
    parser.add_argument("--nd-anchor-2000", type=float, default=1.5e18)

    # Physical/diagnostic weights.
    parser.add_argument("--pressure-weight", type=float, default=0.22)
    parser.add_argument("--pressure-free-factor", type=float, default=3.0)
    parser.add_argument("--highT-pressure-weight", type=float, default=0.22)
    parser.add_argument("--rizk-prior-weight", type=float, default=0.08)
    parser.add_argument("--radius-guard-weight", type=float, default=0.25)
    parser.add_argument("--rd2000-max-nm", type=float, default=1400.0)
    parser.add_argument("--rd-ratio-max", type=float, default=12.0)
    parser.add_argument("--rd1800-soft-max-nm", type=float, default=1100.0)
    parser.add_argument("--rd1900-soft-max-nm", type=float, default=1300.0)

    # V13 specific weights.
    parser.add_argument("--v13-partition-weight", type=float, default=0.95)
    parser.add_argument("--v13-qgb-weight", type=float, default=1.20)
    parser.add_argument("--fig8-disl-radius-weight", type=float, default=0.75)
    parser.add_argument("--fig8-bulk-radius-weight", type=float, default=0.04)
    parser.add_argument("--fig6-burnup-weight", type=float, default=0.02)
    # Legacy global qgb args kept for compatibility, but v13b uses burnup-dependent anchors below.
    parser.add_argument("--qgb-max-low-mid", type=float, default=18.0)
    parser.add_argument("--qgb-max-high", type=float, default=22.0)
    parser.add_argument("--qgb-1p1-low-mid", type=float, default=5.0)
    parser.add_argument("--qgb-1p1-high", type=float, default=5.0)
    parser.add_argument("--qgb-3p2-low-mid", type=float, default=8.0)
    parser.add_argument("--qgb-3p2-high", type=float, default=12.0)
    parser.add_argument("--qgb-penalty-scale", type=float, default=25.0)

    # rhoFT2 controls.
    parser.add_argument("--rhoFT2-disable", action="store_true")
    parser.add_argument("--rho-scale-min", type=float, default=0.5)
    parser.add_argument("--rho-scale-max", type=float, default=2.0)
    parser.add_argument("--rhoFT-fab", type=float, default=3.0e13)
    parser.add_argument("--Kd-min", type=float, default=3.0e5)
    parser.add_argument("--Kd-max", type=float, default=8.0e5)
    parser.add_argument("--sample-rhoD-reference", action="store_true", help="also sample old candidate rho_d as global scale; off by default")
    parser.add_argument("--rhoD-ref-min", type=float, default=1.0e13)
    parser.add_argument("--rhoD-ref-max", type=float, default=8.0e13)

    args = parser.parse_args()

    global W_RIZK_FIG8_DISL_RADIUS, W_RIZK_FIG8_BULK_RADIUS, W_RIZK_FIG6_DISL_BURNUP, W_V13_PARTITION, W_V13_QGB, W_ND_HIGHT_LOW_TARGET, W_ND_EARLY_EXP, W_ND_HIGHT_ANCHOR_ABS, ND_EARLY_MAX_T, ND_ANCHOR_1800, ND_ANCHOR_1900, ND_ANCHOR_2000
    global QGB_MAX_LOW_MID, QGB_MAX_HIGH, QGB_1P1_LOW_MID, QGB_1P1_HIGH, QGB_3P2_LOW_MID, QGB_3P2_HIGH, QGB_PENALTY_SCALE, QGB_ANCHORS

    W_RIZK_FIG8_DISL_RADIUS = args.fig8_disl_radius_weight
    W_RIZK_FIG8_BULK_RADIUS = args.fig8_bulk_radius_weight
    W_RIZK_FIG6_DISL_BURNUP = args.fig6_burnup_weight
    W_V13_PARTITION = args.v13_partition_weight
    W_V13_QGB = args.v13_qgb_weight
    W_ND_HIGHT_LOW_TARGET = args.nd_highT_target_weight
    W_ND_EARLY_EXP = args.nd_early_exp_weight
    W_ND_HIGHT_ANCHOR_ABS = args.nd_highT_anchor_weight
    ND_EARLY_MAX_T = args.nd_early_max_T
    ND_ANCHOR_1800 = args.nd_anchor_1800
    ND_ANCHOR_1900 = args.nd_anchor_1900
    ND_ANCHOR_2000 = args.nd_anchor_2000
    QGB_MAX_LOW_MID = args.qgb_max_low_mid
    QGB_MAX_HIGH = args.qgb_max_high
    QGB_1P1_LOW_MID = args.qgb_1p1_low_mid
    QGB_1P1_HIGH = args.qgb_1p1_high
    QGB_3P2_LOW_MID = args.qgb_3p2_low_mid
    QGB_3P2_HIGH = args.qgb_3p2_high
    QGB_PENALTY_SCALE = args.qgb_penalty_scale
    QGB_ANCHORS = [
        (1.1, 1200.0, QGB_1P1_LOW_MID, "1p1_1200"),
        (1.1, 1500.0, QGB_1P1_LOW_MID, "1p1_1500"),
        (1.1, 1600.0, QGB_1P1_LOW_MID, "1p1_1600"),
        (1.1, 1800.0, QGB_1P1_HIGH, "1p1_1800"),
        (1.1, 1900.0, QGB_1P1_HIGH, "1p1_1900"),
        (1.1, 2000.0, QGB_1P1_HIGH, "1p1_2000"),
        (3.2, 1200.0, QGB_3P2_LOW_MID, "3p2_1200"),
        (3.2, 1500.0, QGB_3P2_LOW_MID, "3p2_1500"),
        (3.2, 1600.0, QGB_3P2_LOW_MID, "3p2_1600"),
        (3.2, 1800.0, QGB_3P2_HIGH, "3p2_1800"),
        (3.2, 1900.0, QGB_3P2_HIGH, "3p2_1900"),
        (3.2, 2000.0, QGB_3P2_HIGH, "3p2_2000"),
    ]

    # Direct experimental weights.
    m.W_SWELLING = args.exp_swelling_weight
    m.W_RD = args.exp_rd_weight
    m.W_ND_LEVEL = args.exp_nd_level_weight
    m.W_ND_DROP = args.base_nd_drop_weight
    m.W_PRESSURE = args.pressure_weight
    m.PRESSURE_FREE_FACTOR = args.pressure_free_factor

    # v8 guard/prior globals reused by v13.
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
    v10c.RHO_SCALE_MIN = args.rho_scale_min
    v10c.RHO_SCALE_MAX = args.rho_scale_max
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
    m.score_candidate = score_candidate_v13
    m.enqueue_known_good_trials = v10c.enqueue_known_good_trials_v10c

    if args.output_dir:
        out = args.output_dir
    else:
        out = os.path.join(
            "results/UN_M7_optuna_v13b_rhoSat_qgbStrict_results",
            args.family,
            f"rhoScale{v10c.RHO_SCALE_MIN:g}-{v10c.RHO_SCALE_MAX:g}"
            f"_qgb{W_V13_QGB:g}_part{W_V13_PARTITION:g}_Rfig8{W_RIZK_FIG8_DISL_RADIUS:g}",
        )

    print("#" * 120)
    print("V13b rhoSat + STRICT gas-to-grain-face + high-T dislocation plateau settings")
    print(f"family                         = {args.family}")
    print(f"rhoFT2 enabled                 = {v10c.RHOFT2_ENABLED}")
    print(f"rho_scale range                 = {v10c.RHO_SCALE_MIN:g} -- {v10c.RHO_SCALE_MAX:g}")
    print("rho T shape                     = saturating Ray-Blank fit normalized at 1025 K")
    print(f"K_d range                       = {v10c.K_D_MIN:g} -- {v10c.K_D_MAX:g}")
    print(f"direct exp weights              = sw {m.W_SWELLING}, Rd {m.W_RD}, Nd {m.W_ND_LEVEL}, Nd-drop {m.W_ND_DROP}")
    print(f"v13 partition/qgb weights       = {W_V13_PARTITION} / {W_V13_QGB}")
    print(f"v13 Nd highT target weight      = {W_ND_HIGHT_LOW_TARGET}")
    print(f"v14 Nd early/highT anchor weights = {W_ND_EARLY_EXP} / {W_ND_HIGHT_ANCHOR_ABS}")
    print(f"v14 Nd early max T / highT anchors = {ND_EARLY_MAX_T} K / {ND_ANCHOR_1800:.3e}, {ND_ANCHOR_1900:.3e}, {ND_ANCHOR_2000:.3e} m^-3")
    print(f"qgb 1.1% low-mid/high           = {QGB_1P1_LOW_MID} / {QGB_1P1_HIGH}")
    print(f"qgb 3.2% low-mid/high           = {QGB_3P2_LOW_MID} / {QGB_3P2_HIGH}")
    print(f"qgb penalty scale               = {QGB_PENALTY_SCALE}")
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
