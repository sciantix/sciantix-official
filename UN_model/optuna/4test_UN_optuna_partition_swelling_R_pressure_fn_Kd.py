#!/usr/bin/env python3
"""
Optuna sweep for 4test_UN: gas partition + swelling(T) + radius(T) + pressure equilibrium.

This script DOES NOT modify the notebook. It loads the model definitions from a
4test_UN*.ipynb notebook and keeps the physics/equations unchanged.

Optimized parameters in this version:

    - Dg_dislocation_scale : Xe/gas diffusivity multiplier in the dislocation line sink
    - Dv_dislocation_scale : U-vacancy diffusivity multiplier for dislocation bubble growth
    - f_n                  : homogeneous bulk nucleation prefactor
    - K_d                  : dislocation bubble density parameter [bubble/m]

Important constraints:

    Dg_dislocation_scale >= 1
    Dv_dislocation_scale >= 1

Default bounds:

    Dg_dislocation_scale : 1 -> 20
    Dv_dislocation_scale : 1 -> 20
    f_n                  : 1e-7 -> 1e-5
    K_d                  : 1e5 -> 8e5 bubble/m

Main score:

    1. gas partition targets;
    2. experimental P2/dislocation swelling vs T;
    3. experimental large-bubble radius R_d vs T at ~1.3% FIMA;
    4. pressure equilibrium p_d/p_eq close to 1 up to 2000 K.

No N_d target is included in the total score. N_d is only saved in outputs.

Example from WSL:

    cd /home/destro/sciantix-official/UN_model/optuna

    /home/destro/sciantix-official/.venv/bin/python \
      4test_UN_optuna_partition_swelling_R_pressure_fn_Kd.py \
      --notebook /home/destro/sciantix-official/UN_model/notebooks/4test_UN_rhoRizk2023.ipynb \
      --n-trials 160 \
      --dt-h 12 \
      --n-modes 22 \
      --dscale-min 1 --dscale-max 20 \
      --vscale-min 1 --vscale-max 20 \
      --fn-min 1e-7 --fn-max 1e-5 \
      --kd-min 1e5 --kd-max 8e5
"""

from __future__ import annotations

import argparse
import json
import math
import sys
import traceback
import types
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Tuple

import nbformat
import numpy as np
import pandas as pd

try:
    import optuna
except ImportError as exc:  # pragma: no cover
    raise SystemExit("Optuna is not installed. Install with: python -m pip install optuna nbformat pandas numpy") from exc


# =============================================================================
# Scoring configuration
# =============================================================================

# Visible weights. These are intentionally simple and easy to edit.
WEIGHT_GAS_PARTITION = 1.30
WEIGHT_SWELLING_D = 1.15
WEIGHT_RD = 0.85
WEIGHT_PRESSURE_EQ = 0.75
WEIGHT_GUARD = 0.60

DEFAULT_TARGET_T_MAX = 2000.0

FAST_SWELLING_MAX_POINTS_PER_BURNUP = 10
FAST_RD_MAX_POINTS = 12

# Gas partition targets: percent of total generated gas.
# They are soft targets; tolerances are set by command-line options.
# Rationale: matrix low but nonzero at low T, bulk important at low/mid T,
# dislocation gas increases with T, qgb/release path stays moderate because
# the notebook still does not include a full grain-boundary release model.
PARTITION_TARGETS = [
    # burnup, T, matrix, bulk, dislocation, qgb
    (1.1, 1200.0, 7.0, 58.0, 27.0, 8.0),
    (1.1, 1400.0, 5.0, 53.0, 32.0, 10.0),
    (1.1, 1600.0, 2.0, 43.0, 44.0, 11.0),
    (1.1, 1800.0, 0.7, 24.0, 62.0, 13.3),
    (1.1, 2000.0, 0.2, 7.0, 78.0, 14.8),
    (1.3, 1200.0, 6.0, 58.0, 27.0, 9.0),
    (1.3, 1600.0, 1.5, 42.0, 45.0, 11.5),
    (1.3, 2000.0, 0.2, 7.0, 78.0, 14.8),
]

# Pressure equilibrium points up to 2000 K. This is a true score term, not just a guard.
PRESSURE_EQ_POINTS = [
    (1.1, 1200.0), (1.1, 1400.0), (1.1, 1600.0), (1.1, 1800.0), (1.1, 2000.0),
    (1.3, 1200.0), (1.3, 1400.0), (1.3, 1600.0), (1.3, 1800.0), (1.3, 2000.0),
    (3.2, 1200.0), (3.2, 1400.0), (3.2, 1600.0), (3.2, 1800.0), (3.2, 2000.0),
]

# Guard only prevents catastrophic early runaway. It is deliberately not a high-T fit.
GUARD_POINTS = [
    (1.1, 1800.0), (1.1, 2000.0),
    (1.3, 1800.0), (1.3, 2000.0),
    (3.2, 1700.0), (3.2, 1800.0), (3.2, 1900.0), (3.2, 2000.0),
]


# =============================================================================
# Notebook loader
# =============================================================================

def load_notebook_model(notebook_path: Path) -> Dict[str, Any]:
    """Execute only definition cells from the 4test_UN notebook into a namespace."""
    with open(notebook_path, "r", encoding="utf-8") as f:
        nb = nbformat.read(f, as_version=4)

    module_name = "__4test_un_model__"
    module = types.ModuleType(module_name)
    module.__file__ = str(notebook_path)
    sys.modules[module_name] = module
    ns: Dict[str, Any] = module.__dict__

    # Current 4test_UN layout:
    #   cell 1: settings
    #   cell 3: experimental data
    #   cell 5: solver core
    #   cell 7: runner functions, run_model_point, simulate_grid, plots
    wanted_indices = {1, 3, 5, 7}
    for i, cell in enumerate(nb.cells):
        if i not in wanted_indices or cell.cell_type != "code":
            continue
        exec(compile(cell.source, f"{notebook_path.name}:cell-{i}", "exec"), ns)

    required = [
        "Candidate", "MANUAL_PARAMS", "run_model_point", "temperature_grid",
        "EXP_SWELLING_T", "EXP_RD_T_13",
    ]
    missing = [k for k in required if k not in ns]
    if missing:
        raise RuntimeError(f"Notebook model load failed; missing symbols: {missing}")
    return ns


# =============================================================================
# Error functions
# =============================================================================

def _mean(values: Iterable[float]) -> float:
    vals = [float(v) for v in values if math.isfinite(float(v))]
    if not vals:
        return 1e6
    return float(sum(vals) / len(vals))


def _log_factor_error(model: float, target: float, floor: float = 1e-30) -> float:
    if not (math.isfinite(model) and math.isfinite(target)):
        return 1e6
    model = max(float(model), floor)
    target = max(float(target), floor)
    return float(math.log(model / target) ** 2)


def _relative_error(model: float, target: float, scale_floor: float = 0.25) -> float:
    if not (math.isfinite(model) and math.isfinite(target)):
        return 1e6
    denom = max(abs(float(target)), scale_floor)
    return float(((float(model) - float(target)) / denom) ** 2)


def _absolute_percent_error(model_percent: float, target_percent: float, tol_percent: float) -> float:
    if not (math.isfinite(model_percent) and math.isfinite(target_percent)):
        return 1e6
    return float(((float(model_percent) - float(target_percent)) / tol_percent) ** 2)


def _thin_points(points: List[Dict[str, Any]], max_points: int, x_key: str = "T") -> List[Dict[str, Any]]:
    pts = sorted(points, key=lambda x: float(x[x_key]))
    if len(pts) <= max_points:
        return pts
    idx = np.linspace(0, len(pts) - 1, max_points).round().astype(int)
    return [pts[int(i)] for i in idx]


def select_swelling_targets(exp_swelling: List[Dict[str, Any]], density: str, t_max: float) -> List[Dict[str, Any]]:
    pts_all = [p for p in exp_swelling if float(p["T"]) <= t_max]
    if density == "full":
        return pts_all
    selected: List[Dict[str, Any]] = []
    burnups = sorted({round(float(x["burnup"]), 3) for x in pts_all})
    for bu in burnups:
        pts = [x for x in pts_all if abs(float(x["burnup"]) - bu) < 1e-6]
        selected.extend(_thin_points(pts, FAST_SWELLING_MAX_POINTS_PER_BURNUP))
    return selected


def select_rd_targets(exp_rd: List[Dict[str, Any]], density: str, t_max: float) -> List[Dict[str, Any]]:
    pts = [p for p in exp_rd if float(p["T"]) <= t_max]
    return pts if density == "full" else _thin_points(pts, FAST_RD_MAX_POINTS)


# =============================================================================
# Candidate construction and scoring
# =============================================================================

def candidate_from_trial(ns: Dict[str, Any], trial: optuna.Trial, args: argparse.Namespace):
    Candidate = ns["Candidate"]
    base = dict(ns["MANUAL_PARAMS"])

    # These are the only optimized variables in this run.
    base["Dg_dislocation_scale"] = trial.suggest_float(
        "Dg_dislocation_scale", args.dscale_min, args.dscale_max, log=True
    )
    base["Dv_dislocation_scale"] = trial.suggest_float(
        "Dv_dislocation_scale", args.vscale_min, args.vscale_max, log=True
    )

    base["f_n"] = trial.suggest_float("f_n", args.fn_min, args.fn_max, log=True)
    if args.kd_log:
        base["K_d"] = trial.suggest_float("K_d", args.kd_min, args.kd_max, log=True)
    else:
        base["K_d"] = trial.suggest_float("K_d", args.kd_min, args.kd_max)

    return Candidate(label=f"optuna_part_sw_R_p_fn_Kd_{trial.number:04d}", **base)


def evaluate_candidate(ns: Dict[str, Any], cand: Any, args: argparse.Namespace) -> Tuple[float, Dict[str, float]]:
    run_model_point = ns["run_model_point"]

    if "RUN_CACHE" in ns:
        ns["RUN_CACHE"].clear()

    point_cache: Dict[Tuple[float, float], Dict[str, Any]] = {}

    def run(T: float, bu: float) -> Dict[str, Any]:
        key = (float(T), float(bu))
        if key not in point_cache:
            point_cache[key] = run_model_point(float(T), float(bu), cand, args.dt_h, args.n_modes, keep_history=False)
        return point_cache[key]

    # 1) Gas partition score.
    partition_errs = []
    for bu, T, matrix_t, bulk_t, disl_t, qgb_t in PARTITION_TARGETS:
        out = run(T, bu)
        partition_errs.append(_absolute_percent_error(out.get("matrix_gas_percent", math.nan), matrix_t, args.partition_matrix_tol))
        partition_errs.append(_absolute_percent_error(out.get("bulk_gas_percent", math.nan), bulk_t, args.partition_bulk_tol))
        partition_errs.append(_absolute_percent_error(out.get("dislocation_gas_percent", math.nan), disl_t, args.partition_disl_tol))
        partition_errs.append(_absolute_percent_error(out.get("qgb_gas_percent", math.nan), qgb_t, args.partition_qgb_tol))
    score_partition = _mean(partition_errs)

    # 2) Experimental P2/dislocation swelling vs T.
    swelling_targets = select_swelling_targets(ns["EXP_SWELLING_T"], args.score_density, args.target_t_max)
    sw_errs = []
    for pt in swelling_targets:
        out = run(float(pt["T"]), float(pt["burnup"]))
        sw_errs.append(_relative_error(out.get("swelling_d_percent", math.nan), float(pt["swelling"]), scale_floor=args.swelling_scale_floor))
    score_sw = _mean(sw_errs)

    # 3) Experimental large-bubble radius at 1.3% FIMA.
    rd_targets = select_rd_targets(ns["EXP_RD_T_13"], args.score_density, args.target_t_max)
    rd_errs = []
    for pt in rd_targets:
        out = run(float(pt["T"]), 1.3)
        rd_errs.append(_log_factor_error(out.get("Rd_nm", math.nan), float(pt["R_nm"]), floor=1e-9))
    score_rd = _mean(rd_errs)

    # 4) Pressure equilibrium up to 2000 K: target log(p/p_eq) = 0.
    pressure_errs = []
    for bu, T in PRESSURE_EQ_POINTS:
        out = run(T, bu)
        ratio = float(out.get("p_d_over_eq", math.nan))
        if not math.isfinite(ratio) or ratio <= 0:
            pressure_errs.append(25.0)
        else:
            # tolerance is a multiplicative factor: pressure_eq_factor_tol=2 means
            # ratio=2 or 0.5 gives error 1 before weighting.
            pressure_errs.append((math.log(ratio) / math.log(args.pressure_eq_factor_tol)) ** 2)
    score_pressure = _mean(pressure_errs)

    # 5) Guard against catastrophic runaway before 2000 K.
    guard_penalties = []
    for bu, T in GUARD_POINTS:
        out = run(T, bu)
        pen = 0.0
        valid = bool(out.get("valid_single_size", False))
        if not valid:
            pen += 1.0
        xi_d = float(out.get("porosity_d", math.nan))
        psi_d = float(out.get("psi_d", math.nan))
        Rd_nm = float(out.get("Rd_nm", math.nan))
        Nd = float(out.get("Nd", math.nan))
        sw_d = float(out.get("swelling_d_percent", math.nan))

        if not math.isfinite(xi_d) or xi_d > args.guard_xi_d:
            pen += 2.0 + (0.0 if not math.isfinite(xi_d) else max(0.0, xi_d - args.guard_xi_d) * 5.0)
        if not math.isfinite(psi_d) or psi_d > args.guard_psi_d:
            pen += 1.0
        if not math.isfinite(Rd_nm) or Rd_nm > args.guard_rd_nm:
            pen += 1.0 + (0.0 if not math.isfinite(Rd_nm) else min(10.0, max(0.0, Rd_nm - args.guard_rd_nm) / args.guard_rd_nm))
        if not math.isfinite(Nd) or Nd < args.guard_nd_min:
            pen += 1.0
        if math.isfinite(sw_d) and sw_d > args.guard_swelling_d_percent:
            pen += min(10.0, (sw_d - args.guard_swelling_d_percent) / args.guard_swelling_d_percent)
        guard_penalties.append(pen)
    score_guard = _mean(guard_penalties)

    total = (
        WEIGHT_GAS_PARTITION * score_partition
        + WEIGHT_SWELLING_D * score_sw
        + WEIGHT_RD * score_rd
        + WEIGHT_PRESSURE_EQ * score_pressure
        + WEIGHT_GUARD * score_guard
    )

    details = {
        "score_total": total,
        "score_gas_partition": score_partition,
        "score_swelling_d": score_sw,
        "score_Rd": score_rd,
        "score_pressure_eq": score_pressure,
        "score_guard": score_guard,
        "Dg_dislocation_scale": float(cand.Dg_dislocation_scale),
        "Dv_dislocation_scale": float(cand.Dv_dislocation_scale),
        "f_n": float(cand.f_n),
        "K_d": float(cand.K_d),
    }
    return float(total), details


def make_objective(ns: Dict[str, Any], args: argparse.Namespace):
    def objective(trial: optuna.Trial) -> float:
        cand = candidate_from_trial(ns, trial, args)
        try:
            score, details = evaluate_candidate(ns, cand, args)
        except Exception as exc:
            trial.set_user_attr("exception", repr(exc))
            trial.set_user_attr("traceback", traceback.format_exc(limit=8))
            return 1e9
        for k, v in details.items():
            trial.set_user_attr(k, float(v) if isinstance(v, (int, float, np.floating)) else v)
        return score
    return objective


# =============================================================================
# Export
# =============================================================================

def export_best(ns: Dict[str, Any], study: optuna.Study, args: argparse.Namespace, out_dir: Path) -> None:
    Candidate = ns["Candidate"]
    run_model_point = ns["run_model_point"]
    temperature_grid = ns["temperature_grid"]

    base = dict(ns["MANUAL_PARAMS"])
    base["Dg_dislocation_scale"] = float(study.best_params["Dg_dislocation_scale"])
    base["Dv_dislocation_scale"] = float(study.best_params["Dv_dislocation_scale"])
    base["f_n"] = float(study.best_params["f_n"])
    base["K_d"] = float(study.best_params["K_d"])

    cand = Candidate(label="optuna_best_partition_swelling_R_pressure_fn_Kd", **base)

    best_payload = {
        "best_value": float(study.best_value),
        "best_params": dict(study.best_params),
        "optimized_parameters": ["Dg_dislocation_scale", "Dv_dislocation_scale", "f_n", "K_d"],
        "fixed_manual_params_except_optimized": {
            k: v for k, v in ns["MANUAL_PARAMS"].items()
            if k not in ["Dg_dislocation_scale", "Dv_dislocation_scale", "f_n", "K_d"]
        },
        "notebook_settings_used": {
            "XE_DIFFUSIVITY_MODE": ns.get("XE_DIFFUSIVITY_MODE"),
            "VU_DIFFUSIVITY_MODE": ns.get("VU_DIFFUSIVITY_MODE"),
            "RHO_MODE": ns.get("RHO_MODE"),
            "USE_PHI_GAS_RESOLUTION": ns.get("USE_PHI_GAS_RESOLUTION"),
            "USE_NUCLEATION_MASS_COUPLING": ns.get("USE_NUCLEATION_MASS_COUPLING"),
            "USE_BULK_DISLOCATION_CAPTURE": ns.get("USE_BULK_DISLOCATION_CAPTURE"),
            "DT_H": args.dt_h,
            "N_MODES": args.n_modes,
            "target_t_max": args.target_t_max,
            "Dg_scale_bounds": [args.dscale_min, args.dscale_max],
            "Dv_scale_bounds": [args.vscale_min, args.vscale_max],
            "fn_bounds": [args.fn_min, args.fn_max],
            "Kd_bounds": [args.kd_min, args.kd_max],
        },
        "score_weights": {
            "gas_partition": WEIGHT_GAS_PARTITION,
            "swelling_d": WEIGHT_SWELLING_D,
            "Rd": WEIGHT_RD,
            "pressure_eq": WEIGHT_PRESSURE_EQ,
            "guard": WEIGHT_GUARD,
        },
        "partition_targets": PARTITION_TARGETS,
        "pressure_eq_points": PRESSURE_EQ_POINTS,
    }
    (out_dir / "best_params.json").write_text(json.dumps(best_payload, indent=2), encoding="utf-8")

    if "RUN_CACHE" in ns:
        ns["RUN_CACHE"].clear()

    # Full diagnostic grid for plotting/analysis.
    rows = []
    burnups = list(ns.get("BURNUPS", [1.1, 1.3, 3.2]))
    tmax = float(ns.get("T_MAX_DIAGNOSTIC", 2600.0))
    for bu in burnups:
        for T in temperature_grid(tmax):
            out = run_model_point(float(T), float(bu), cand, args.dt_h, args.n_modes, keep_history=False)
            rows.append({k: v for k, v in out.items() if k not in ("hist", "rates")})
    pd.DataFrame(rows).to_csv(out_dir / "best_grid.csv", index=False)

    # Compact summary at important points.
    summary_points = []
    key_Ts = [900.0, 1000.0, 1100.0, 1200.0, 1300.0, 1400.0, 1500.0, 1600.0, 1700.0, 1800.0, 1900.0, 2000.0]
    for bu in burnups:
        for T in key_Ts:
            out = run_model_point(float(T), float(bu), cand, args.dt_h, args.n_modes, keep_history=False)
            summary_points.append({k: v for k, v in out.items() if k not in ("hist", "rates")})
    pd.DataFrame(summary_points).to_csv(out_dir / "best_key_points.csv", index=False)

    # Side-by-side target comparison for the best trial.
    comp_rows = []
    for bu, T, matrix_t, bulk_t, disl_t, qgb_t in PARTITION_TARGETS:
        out = run_model_point(float(T), float(bu), cand, args.dt_h, args.n_modes, keep_history=False)
        comp_rows.append({
            "target_type": "gas_partition",
            "burnup": bu,
            "T": T,
            "matrix_target_percent": matrix_t,
            "matrix_model_percent": out.get("matrix_gas_percent"),
            "bulk_target_percent": bulk_t,
            "bulk_model_percent": out.get("bulk_gas_percent"),
            "dislocation_target_percent": disl_t,
            "dislocation_model_percent": out.get("dislocation_gas_percent"),
            "qgb_target_percent": qgb_t,
            "qgb_model_percent": out.get("qgb_gas_percent"),
        })
    for pt in select_swelling_targets(ns["EXP_SWELLING_T"], "full", args.target_t_max):
        out = run_model_point(float(pt["T"]), float(pt["burnup"]), cand, args.dt_h, args.n_modes, keep_history=False)
        comp_rows.append({
            "target_type": "swelling_d",
            "burnup": float(pt["burnup"]),
            "T": float(pt["T"]),
            "swelling_exp_percent": float(pt["swelling"]),
            "swelling_model_percent": out.get("swelling_d_percent"),
        })
    for pt in select_rd_targets(ns["EXP_RD_T_13"], "full", args.target_t_max):
        out = run_model_point(float(pt["T"]), 1.3, cand, args.dt_h, args.n_modes, keep_history=False)
        comp_rows.append({
            "target_type": "Rd",
            "burnup": 1.3,
            "T": float(pt["T"]),
            "Rd_exp_nm": float(pt["R_nm"]),
            "Rd_model_nm": out.get("Rd_nm"),
        })
    for bu, T in PRESSURE_EQ_POINTS:
        out = run_model_point(float(T), float(bu), cand, args.dt_h, args.n_modes, keep_history=False)
        comp_rows.append({
            "target_type": "pressure_eq",
            "burnup": bu,
            "T": T,
            "pd_over_peq_target": 1.0,
            "pd_over_peq_model": out.get("p_d_over_eq"),
        })
    pd.DataFrame(comp_rows).to_csv(out_dir / "best_target_comparison.csv", index=False)


# =============================================================================
# CLI
# =============================================================================

def parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--notebook", type=Path, default=Path("4test_UN_rhoRizk2023.ipynb"), help="Input notebook to load definitions from.")
    parser.add_argument("--output-dir", type=Path, default=Path("4test_UN_optuna_partition_swelling_R_pressure_fn_Kd_results"), help="Output directory.")
    parser.add_argument("--n-trials", type=int, default=120, help="Number of Optuna trials.")
    parser.add_argument("--seed", type=int, default=23, help="Optuna sampler seed.")
    parser.add_argument("--dt-h", type=float, default=12.0, help="Time step in hours during trials.")
    parser.add_argument("--n-modes", type=int, default=22, help="Number of spectral modes during trials.")

    parser.add_argument("--dscale-min", type=float, default=1.0, help="Lower bound for Dg_dislocation_scale. Must be >=1.")
    parser.add_argument("--dscale-max", type=float, default=20.0, help="Upper bound for Dg_dislocation_scale.")
    parser.add_argument("--vscale-min", type=float, default=1.0, help="Lower bound for Dv_dislocation_scale. Must be >=1.")
    parser.add_argument("--vscale-max", type=float, default=20.0, help="Upper bound for Dv_dislocation_scale.")

    parser.add_argument("--fn-min", type=float, default=1e-7, help="Lower bound for f_n. Default spans one decade below Rizk nominal 1e-6.")
    parser.add_argument("--fn-max", type=float, default=1e-5, help="Upper bound for f_n. Default spans one decade above Rizk nominal 1e-6.")
    parser.add_argument("--kd-min", type=float, default=1e5, help="Lower bound for K_d [bubble/m].")
    parser.add_argument("--kd-max", type=float, default=8e5, help="Upper bound for K_d [bubble/m].")
    parser.add_argument("--kd-log", action="store_true", default=False, help="Use log sampling for K_d. Default is linear sampling.")
    parser.add_argument("--kd-linear", action="store_false", dest="kd_log", help="Use linear sampling for K_d.")

    parser.add_argument("--score-density", choices=["fast", "full"], default="fast", help="fast uses representative targets; full uses all targets.")
    parser.add_argument("--target-t-max", type=float, default=DEFAULT_TARGET_T_MAX, help="Max T used for swelling/Rd target score.")
    parser.add_argument("--swelling-scale-floor", type=float, default=0.35, help="Relative-error denominator floor for swelling percent.")

    parser.add_argument("--partition-matrix-tol", type=float, default=5.0)
    parser.add_argument("--partition-bulk-tol", type=float, default=18.0)
    parser.add_argument("--partition-disl-tol", type=float, default=18.0)
    parser.add_argument("--partition-qgb-tol", type=float, default=7.0)
    parser.add_argument("--pressure-eq-factor-tol", type=float, default=2.0, help="Multiplicative pressure tolerance: 2 means ratio 2 or 0.5 gives unit error.")

    parser.add_argument("--guard-xi-d", type=float, default=0.55)
    parser.add_argument("--guard-psi-d", type=float, default=0.85)
    parser.add_argument("--guard-rd-nm", type=float, default=1000.0)
    parser.add_argument("--guard-nd-min", type=float, default=1e17)
    parser.add_argument("--guard-swelling-d-percent", type=float, default=35.0)

    parser.add_argument("--study-name", default="4test_UN_partition_swelling_R_pressure_fn_Kd")
    parser.add_argument("--storage", default=None, help="Optional Optuna storage URL, e.g. sqlite:///study.db")
    parser.add_argument("--rho-mode", default=None, choices=["constant", "rhoSat_RayBlank", "rizk2023"], help="Override RHO_MODE without editing notebook.")
    parser.add_argument("--xe-mode", default=None, help="Override XE_DIFFUSIVITY_MODE without editing notebook.")
    parser.add_argument("--vu-mode", default=None, help="Override VU_DIFFUSIVITY_MODE without editing notebook.")
    parser.add_argument("--enqueue-baseline", action="store_true", default=True, help="Evaluate notebook baseline as an initial trial. Default: on.")
    parser.add_argument("--no-enqueue-baseline", action="store_false", dest="enqueue_baseline", help="Disable initial baseline trial.")
    return parser.parse_args(argv)


def main(argv: Optional[List[str]] = None) -> int:
    args = parse_args(argv)
    args.notebook = args.notebook.resolve()
    args.output_dir.mkdir(parents=True, exist_ok=True)

    if args.dscale_min < 1.0 or args.vscale_min < 1.0:
        raise SystemExit("Dislocation scale factors must be >= 1. Use dscale-min/vscale-min >= 1.")
    if args.fn_min <= 0 or args.fn_max <= args.fn_min:
        raise SystemExit("Invalid f_n bounds: require 0 < fn-min < fn-max.")
    if args.kd_min <= 0 or args.kd_max <= args.kd_min:
        raise SystemExit("Invalid K_d bounds: require 0 < kd-min < kd-max.")
    if args.kd_max < 1e3:
        raise SystemExit("K_d bounds look like f_n. Use e.g. --kd-min 1e5 --kd-max 8e5.")

    ns = load_notebook_model(args.notebook)

    # Optional overrides in this script only; notebook file is not modified.
    if args.rho_mode is not None:
        ns["RHO_MODE"] = args.rho_mode
    if args.xe_mode is not None:
        ns["XE_DIFFUSIVITY_MODE"] = args.xe_mode
    if args.vu_mode is not None:
        ns["VU_DIFFUSIVITY_MODE"] = args.vu_mode

    ns["DT_H"] = float(args.dt_h)
    ns["N_MODES"] = int(args.n_modes)
    ns["OUTPUT_DIR"] = str(args.output_dir)
    ns["SHOW_PLOTS"] = False

    print("Loaded notebook:", args.notebook)
    print("Output dir:", args.output_dir)
    print("Fixed settings:")
    print("  XE_DIFFUSIVITY_MODE =", ns.get("XE_DIFFUSIVITY_MODE"))
    print("  VU_DIFFUSIVITY_MODE =", ns.get("VU_DIFFUSIVITY_MODE"))
    print("  RHO_MODE            =", ns.get("RHO_MODE"))
    print("  dt_h, n_modes       =", args.dt_h, args.n_modes)
    print("Score targets:")
    print("  gas partition, swelling_d(T), Rd(T), pressure p_d/p_eq to 2000 K")
    print("Optimized parameters:")
    print(f"  Dg_dislocation_scale in [{args.dscale_min}, {args.dscale_max}] log")
    print(f"  Dv_dislocation_scale in [{args.vscale_min}, {args.vscale_max}] log")
    print(f"  f_n in [{args.fn_min}, {args.fn_max}] log")
    print(f"  K_d in [{args.kd_min}, {args.kd_max}] {'log' if args.kd_log else 'linear'}")

    sampler = optuna.samplers.TPESampler(seed=args.seed, multivariate=True, group=True)
    study = optuna.create_study(
        study_name=args.study_name,
        storage=args.storage,
        direction="minimize",
        sampler=sampler,
        load_if_exists=True,
    )

    if args.enqueue_baseline:
        base_trial = {
            "Dg_dislocation_scale": float(ns["MANUAL_PARAMS"].get("Dg_dislocation_scale", 10.0)),
            "Dv_dislocation_scale": float(ns["MANUAL_PARAMS"].get("Dv_dislocation_scale", 10.0)),
        }
        base_trial["f_n"] = min(max(float(ns["MANUAL_PARAMS"].get("f_n", args.fn_min)), args.fn_min), args.fn_max)
        base_trial["K_d"] = min(max(float(ns["MANUAL_PARAMS"].get("K_d", args.kd_min)), args.kd_min), args.kd_max)
        try:
            study.enqueue_trial(base_trial)
        except Exception:
            pass

    study.optimize(make_objective(ns, args), n_trials=args.n_trials, show_progress_bar=True)

    trials_df = study.trials_dataframe(attrs=("number", "value", "params", "user_attrs", "state"))
    trials_csv = args.output_dir / "optuna_trials.csv"
    trials_df.to_csv(trials_csv, index=False)

    export_best(ns, study, args, args.output_dir)

    print("\nBest result")
    print("  value:", study.best_value)
    print("  params:", study.best_params)
    print("Saved:")
    print(" ", trials_csv)
    print(" ", args.output_dir / "best_params.json")
    print(" ", args.output_dir / "best_grid.csv")
    print(" ", args.output_dir / "best_key_points.csv")
    print(" ", args.output_dir / "best_target_comparison.csv")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
