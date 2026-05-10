#!/usr/bin/env python3
"""
Optuna sweep for 4test_UN: gas-partition + low-temperature P2 fit.

This script DOES NOT modify the notebook. It loads the model definitions from a
4test_UN*.ipynb notebook and keeps every model/scaling parameter fixed except:

    - Dg_dislocation_scale : Xe/gas diffusivity multiplier in the dislocation line sink
    - Dv_dislocation_scale : U-vacancy diffusivity multiplier for dislocation bubble growth

Important physics constraint for this run:

    Dg_dislocation_scale >= 1
    Dv_dislocation_scale >= 1

because diffusivity along/near dislocations should not be lower than the bulk value.

Main scoring ideas requested for this run:

    1. Gas partition matters.
    2. Add an explicit low-temperature target: ~7% gas in matrix for T < 1400 K
       at 1.1% FIMA.
    3. Fit P2/dislocation swelling, large-bubble radius Rd and number density Nd
       only in the low/mid-temperature range by default (T <= 1700 K).
    4. Keep pressure and high-temperature runaway as guardrails, not as the main fit.

Example:

    cd /home/destro/sciantix-official/UN_model/optuna

    /home/destro/sciantix-official/.venv/bin/python \
      4test_UN_optuna_partition_lowT_dislocation_scales.py \
      --notebook /home/destro/sciantix-official/UN_model/notebooks/4test_UN_rhoRizk2023.ipynb \
      --n-trials 120 \
      --dt-h 12 \
      --n-modes 22

Optional overrides without editing the notebook:

    --rho-mode constant|rhoSat_RayBlank|rizk2023
    --xe-mode rizk2025_refit_plot
    --vu-mode rizk2025_refit_full
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
    raise SystemExit("Optuna is not installed. Install with: python -m pip install optuna nbformat") from exc


# =============================================================================
# Scoring configuration
# =============================================================================

# Main weights. Keep visible and simple.
WEIGHT_GAS_PARTITION = 1.35
WEIGHT_MATRIX_LOW_T = 1.60
WEIGHT_SWELLING_D_LOW_T = 1.00
WEIGHT_RD_LOW_T = 0.45
WEIGHT_ND_LOW_T = 0.30
WEIGHT_PRESSURE_LOW_T = 0.15
WEIGHT_GUARD = 1.20

# Low-temperature fitting range for experimental curves.
DEFAULT_TARGET_T_MAX = 1700.0

# Fast scoring keeps representative points only.
FAST_SWELLING_MAX_POINTS_PER_BURNUP = 9
FAST_ND_MAX_POINTS = 10
FAST_RD_MAX_POINTS = 10

# Explicit gas-in-matrix target requested by user.
# T values are in K; target is percent of generated gas.
MATRIX_LOW_T_TARGETS = [
    (1.1, 900.0, 7.0),
    (1.1, 1000.0, 7.0),
    (1.1, 1100.0, 7.0),
    (1.1, 1200.0, 7.0),
    (1.1, 1300.0, 7.0),
]

# Gas partition targets are deliberately weak/soft. They encode the qualitative
# behavior we have been asking for: small but nonzero matrix fraction at low T,
# qgb not dominating, dislocation gas rising with T.
# Fractions are percent of generated gas: matrix + bulk + dislocation + qgb.
# You can edit these numbers later, but the model equations are untouched.
PARTITION_TARGETS = [
    # burnup, T, matrix, bulk, dislocation, qgb
    (1.1, 1200.0, 7.0, 58.0, 27.0, 8.0),
    (1.1, 1400.0, 4.0, 52.0, 35.0, 9.0),
    (1.1, 1600.0, 2.0, 42.0, 46.0, 10.0),
    (1.1, 1800.0, 1.0, 22.0, 65.0, 12.0),
    (1.1, 2000.0, 0.5, 6.0, 81.5, 12.0),
]

# Pressure is a guard, not a dominant target.
PRESSURE_POINTS = [
    (1.1, 1200.0), (1.1, 1400.0), (1.1, 1600.0),
    (1.3, 1200.0), (1.3, 1400.0), (1.3, 1600.0),
    (3.2, 1200.0), (3.2, 1400.0), (3.2, 1600.0),
]

# High-T guard does not try to fit 2300+ K. It only prevents very early runaway.
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
        code = cell.source
        exec(compile(code, f"{notebook_path.name}:cell-{i}", "exec"), ns)

    required = [
        "Candidate", "MANUAL_PARAMS", "run_model_point",
        "EXP_SWELLING_T", "EXP_ND_T_13", "EXP_RD_T_13",
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
    """Squared error normalized by an absolute percentage tolerance."""
    if not (math.isfinite(model_percent) and math.isfinite(target_percent)):
        return 1e6
    return float(((float(model_percent) - float(target_percent)) / tol_percent) ** 2)


def _range_penalty(value: float, lo: float, hi: float, scale: float) -> float:
    """Zero inside [lo, hi], quadratic outside. Useful for guardrails."""
    if not math.isfinite(value):
        return 1e6
    if value < lo:
        return float(((value - lo) / scale) ** 2)
    if value > hi:
        return float(((value - hi) / scale) ** 2)
    return 0.0


# =============================================================================
# Target selection
# =============================================================================

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


def select_nd_targets(exp_nd: List[Dict[str, Any]], density: str, t_max: float) -> List[Dict[str, Any]]:
    pts = [p for p in exp_nd if float(p["T"]) <= t_max]
    return pts if density == "full" else _thin_points(pts, FAST_ND_MAX_POINTS)


# =============================================================================
# Candidate construction and scoring
# =============================================================================

def candidate_from_trial(ns: Dict[str, Any], trial: optuna.Trial, args: argparse.Namespace):
    Candidate = ns["Candidate"]
    base = dict(ns["MANUAL_PARAMS"])

    # These are the ONLY two model parameters changed by this Optuna run.
    # Lower bounds default to 1.0: dislocation diffusion is not allowed to be below bulk.
    base["Dg_dislocation_scale"] = trial.suggest_float(
        "Dg_dislocation_scale", args.dscale_min, args.dscale_max, log=True
    )
    base["Dv_dislocation_scale"] = trial.suggest_float(
        "Dv_dislocation_scale", args.vscale_min, args.vscale_max, log=True
    )

    return Candidate(label=f"optuna_partition_lowT_{trial.number:04d}", **base)


def evaluate_candidate(ns: Dict[str, Any], cand: Any, args: argparse.Namespace) -> Tuple[float, Dict[str, float]]:
    run_model_point = ns["run_model_point"]

    if "RUN_CACHE" in ns:
        ns["RUN_CACHE"].clear()

    # Cache outputs inside a trial because many score terms reuse points.
    point_cache: Dict[Tuple[float, float], Dict[str, Any]] = {}

    def run(T: float, bu: float) -> Dict[str, Any]:
        key = (float(T), float(bu))
        if key not in point_cache:
            point_cache[key] = run_model_point(float(T), float(bu), cand, args.dt_h, args.n_modes, keep_history=False)
        return point_cache[key]

    # 1) Requested explicit target: matrix gas ~7% at T<1400 K, 1.1% FIMA.
    matrix_lowT_errs = []
    for bu, T, target in MATRIX_LOW_T_TARGETS:
        out = run(T, bu)
        matrix_lowT_errs.append(_absolute_percent_error(out.get("matrix_gas_percent", math.nan), target, args.matrix_tol_percent))
    score_matrix_lowT = _mean(matrix_lowT_errs)

    # 2) Gas partition targets. Soft; normalized by configurable tolerances.
    partition_errs = []
    for bu, T, matrix_t, bulk_t, disl_t, qgb_t in PARTITION_TARGETS:
        out = run(T, bu)
        partition_errs.append(_absolute_percent_error(out.get("matrix_gas_percent", math.nan), matrix_t, args.partition_matrix_tol))
        partition_errs.append(_absolute_percent_error(out.get("bulk_gas_percent", math.nan), bulk_t, args.partition_bulk_tol))
        partition_errs.append(_absolute_percent_error(out.get("dislocation_gas_percent", math.nan), disl_t, args.partition_disl_tol))
        partition_errs.append(_absolute_percent_error(out.get("qgb_gas_percent", math.nan), qgb_t, args.partition_qgb_tol))
    score_partition = _mean(partition_errs)

    # 3) Low/mid-T swelling P2 / dislocation swelling vs data.
    swelling_targets = select_swelling_targets(ns["EXP_SWELLING_T"], args.score_density, args.target_t_max)
    sw_errs = []
    for pt in swelling_targets:
        out = run(float(pt["T"]), float(pt["burnup"]))
        sw_errs.append(_relative_error(out.get("swelling_d_percent", math.nan), float(pt["swelling"]), scale_floor=0.35))
    score_sw = _mean(sw_errs)

    # 4) Low/mid-T radius and concentration for 1.3% FIMA.
    rd_targets = select_rd_targets(ns["EXP_RD_T_13"], args.score_density, args.target_t_max)
    rd_errs = []
    for pt in rd_targets:
        out = run(float(pt["T"]), 1.3)
        rd_errs.append(_log_factor_error(out.get("Rd_nm", math.nan), float(pt["R_nm"]), floor=1e-9))
    score_rd = _mean(rd_errs)

    nd_targets = select_nd_targets(ns["EXP_ND_T_13"], args.score_density, args.target_t_max)
    nd_errs = []
    for pt in nd_targets:
        out = run(float(pt["T"]), 1.3)
        nd_errs.append(_log_factor_error(out.get("Nd", math.nan), float(pt["N"]), floor=1e6))
    score_nd = _mean(nd_errs)

    # 5) Pressure guard in useful range. Free band 0.5..3.0.
    pressure_penalties = []
    for bu, T in PRESSURE_POINTS:
        out = run(T, bu)
        ratio = float(out.get("p_d_over_eq", math.nan))
        if not math.isfinite(ratio) or ratio <= 0:
            pressure_penalties.append(25.0)
        elif ratio < 0.5:
            pressure_penalties.append(math.log(ratio / 0.5) ** 2)
        elif ratio > 3.0:
            pressure_penalties.append(math.log(ratio / 3.0) ** 2)
        else:
            pressure_penalties.append(0.0)
    score_pressure = _mean(pressure_penalties)

    # 6) High-T guard: only prevent early nonphysical runaway, do not fit >2000 K.
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
        pd_ratio = float(out.get("p_d_over_eq", math.nan))
        sw_d = float(out.get("swelling_d_percent", math.nan))

        if not math.isfinite(xi_d) or xi_d > 0.55:
            pen += 2.0 + (0.0 if not math.isfinite(xi_d) else max(0.0, xi_d - 0.55) * 5.0)
        if not math.isfinite(psi_d) or psi_d > 0.85:
            pen += 1.0
        if not math.isfinite(Rd_nm) or Rd_nm > args.guard_rd_nm:
            pen += 1.0 + (0.0 if not math.isfinite(Rd_nm) else min(10.0, max(0.0, Rd_nm - args.guard_rd_nm) / args.guard_rd_nm))
        if not math.isfinite(Nd) or Nd < args.guard_nd_min:
            pen += 1.0
        if not math.isfinite(pd_ratio) or pd_ratio > 10.0:
            pen += 1.0
        if math.isfinite(sw_d) and sw_d > args.guard_swelling_d_percent:
            pen += min(10.0, (sw_d - args.guard_swelling_d_percent) / args.guard_swelling_d_percent)
        guard_penalties.append(pen)
    score_guard = _mean(guard_penalties)

    total = (
        WEIGHT_MATRIX_LOW_T * score_matrix_lowT
        + WEIGHT_GAS_PARTITION * score_partition
        + WEIGHT_SWELLING_D_LOW_T * score_sw
        + WEIGHT_RD_LOW_T * score_rd
        + WEIGHT_ND_LOW_T * score_nd
        + WEIGHT_PRESSURE_LOW_T * score_pressure
        + WEIGHT_GUARD * score_guard
    )

    details = {
        "score_total": total,
        "score_matrix_lowT": score_matrix_lowT,
        "score_gas_partition": score_partition,
        "score_swelling_d_lowT": score_sw,
        "score_Rd_lowT": score_rd,
        "score_Nd_lowT": score_nd,
        "score_pressure_lowT": score_pressure,
        "score_guard": score_guard,
        "Dg_dislocation_scale": float(cand.Dg_dislocation_scale),
        "Dv_dislocation_scale": float(cand.Dv_dislocation_scale),
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
    cand = Candidate(label="optuna_best_partition_lowT", **base)

    best_payload = {
        "best_value": float(study.best_value),
        "best_params": dict(study.best_params),
        "fixed_manual_params_except_two": {
            k: v for k, v in ns["MANUAL_PARAMS"].items()
            if k not in ["Dg_dislocation_scale", "Dv_dislocation_scale"]
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
        },
        "score_weights": {
            "matrix_lowT": WEIGHT_MATRIX_LOW_T,
            "gas_partition": WEIGHT_GAS_PARTITION,
            "swelling_d_lowT": WEIGHT_SWELLING_D_LOW_T,
            "Rd_lowT": WEIGHT_RD_LOW_T,
            "Nd_lowT": WEIGHT_ND_LOW_T,
            "pressure_lowT": WEIGHT_PRESSURE_LOW_T,
            "guard": WEIGHT_GUARD,
        },
        "matrix_lowT_targets": MATRIX_LOW_T_TARGETS,
        "partition_targets": PARTITION_TARGETS,
    }
    (out_dir / "best_params.json").write_text(json.dumps(best_payload, indent=2), encoding="utf-8")

    # Full diagnostic grid for plotting/analysis. This can still go to 2600 K if notebook says so;
    # scoring itself does not try to optimize that whole range.
    rows = []
    if "RUN_CACHE" in ns:
        ns["RUN_CACHE"].clear()
    burnups = list(ns.get("BURNUPS", [1.1, 1.3, 3.2]))
    tmax = float(ns.get("T_MAX_DIAGNOSTIC", 2600.0))
    for bu in burnups:
        for T in temperature_grid(tmax):
            out = run_model_point(float(T), float(bu), cand, args.dt_h, args.n_modes, keep_history=False)
            rows.append({k: v for k, v in out.items() if k not in ("hist", "rates")})
    pd.DataFrame(rows).to_csv(out_dir / "best_grid.csv", index=False)

    # Compact summary at important points.
    summary_points = []
    for bu in burnups:
        for T in [900.0, 1000.0, 1200.0, 1300.0, 1400.0, 1600.0, 1700.0, 1800.0, 2000.0]:
            out = run_model_point(float(T), float(bu), cand, args.dt_h, args.n_modes, keep_history=False)
            summary_points.append({k: v for k, v in out.items() if k not in ("hist", "rates")})
    pd.DataFrame(summary_points).to_csv(out_dir / "best_key_points.csv", index=False)


# =============================================================================
# CLI
# =============================================================================

def parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--notebook", type=Path, default=Path("4test_UN_rhoRizk2023.ipynb"), help="Input notebook to load definitions from.")
    parser.add_argument("--output-dir", type=Path, default=Path("4test_UN_optuna_partition_lowT_results"), help="Output directory.")
    parser.add_argument("--n-trials", type=int, default=120, help="Number of Optuna trials.")
    parser.add_argument("--seed", type=int, default=17, help="Optuna sampler seed.")
    parser.add_argument("--dt-h", type=float, default=12.0, help="Time step in hours during trials.")
    parser.add_argument("--n-modes", type=int, default=22, help="Number of spectral modes during trials.")
    parser.add_argument("--dscale-min", type=float, default=1.0, help="Lower bound for Dg_dislocation_scale. Default 1: not below bulk.")
    parser.add_argument("--dscale-max", type=float, default=100.0, help="Upper bound for Dg_dislocation_scale.")
    parser.add_argument("--vscale-min", type=float, default=1.0, help="Lower bound for Dv_dislocation_scale. Default 1: not below bulk.")
    parser.add_argument("--vscale-max", type=float, default=100.0, help="Upper bound for Dv_dislocation_scale.")
    parser.add_argument("--score-density", choices=["fast", "full"], default="fast", help="fast uses representative targets; full uses all low-T targets.")
    parser.add_argument("--target-t-max", type=float, default=DEFAULT_TARGET_T_MAX, help="Max T used for swelling/Rd/Nd target score.")
    parser.add_argument("--matrix-tol-percent", type=float, default=3.0, help="Tolerance for explicit 7% low-T matrix gas target.")
    parser.add_argument("--partition-matrix-tol", type=float, default=5.0)
    parser.add_argument("--partition-bulk-tol", type=float, default=20.0)
    parser.add_argument("--partition-disl-tol", type=float, default=20.0)
    parser.add_argument("--partition-qgb-tol", type=float, default=8.0)
    parser.add_argument("--guard-rd-nm", type=float, default=1000.0)
    parser.add_argument("--guard-nd-min", type=float, default=1e17)
    parser.add_argument("--guard-swelling-d-percent", type=float, default=30.0)
    parser.add_argument("--study-name", default="4test_UN_partition_lowT_dislocation_scales")
    parser.add_argument("--storage", default=None, help="Optional Optuna storage URL, e.g. sqlite:///study.db")
    parser.add_argument("--rho-mode", default=None, choices=["constant", "rhoSat_RayBlank", "rizk2023"], help="Override RHO_MODE without editing notebook.")
    parser.add_argument("--xe-mode", default=None, help="Override XE_DIFFUSIVITY_MODE without editing notebook.")
    parser.add_argument("--vu-mode", default=None, help="Override VU_DIFFUSIVITY_MODE without editing notebook.")
    parser.add_argument("--enqueue-10-10", action="store_true", default=True, help="Evaluate Dg=10,Dv=10 as an initial trial. Default: on.")
    parser.add_argument("--no-enqueue-10-10", action="store_false", dest="enqueue_10_10", help="Disable initial 10,10 trial.")
    return parser.parse_args(argv)


def main(argv: Optional[List[str]] = None) -> int:
    args = parse_args(argv)
    args.notebook = args.notebook.resolve()
    args.output_dir.mkdir(parents=True, exist_ok=True)

    if args.dscale_min < 1.0 or args.vscale_min < 1.0:
        raise SystemExit("For this run, dislocation scale factors must be >= 1. Use dscale-min/vscale-min >= 1.")

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
    print("Low-T target max T    =", args.target_t_max)
    print("Optimized parameters, constrained >= 1:")
    print(f"  Dg_dislocation_scale in [{args.dscale_min}, {args.dscale_max}] log")
    print(f"  Dv_dislocation_scale in [{args.vscale_min}, {args.vscale_max}] log")

    sampler = optuna.samplers.TPESampler(seed=args.seed, multivariate=True, group=True)
    study = optuna.create_study(
        study_name=args.study_name,
        storage=args.storage,
        direction="minimize",
        sampler=sampler,
        load_if_exists=True,
    )

    if args.enqueue_10_10:
        # The current manual value 10,10 is explicitly evaluated, because the user noted
        # that gas partition there is not bad.
        try:
            study.enqueue_trial({"Dg_dislocation_scale": 10.0, "Dv_dislocation_scale": 10.0})
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
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
