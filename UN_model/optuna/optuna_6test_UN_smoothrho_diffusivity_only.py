#!/usr/bin/env python3
"""
Optuna calibration runner for 6test_UN.ipynb — fixed smooth rho_d(F,T), NO bulk-dislocation capture.

Variant: R_d(T) and swelling_d(T) prioritized with robust log-Huber losses.

This script DOES NOT modify the notebook. It loads selected notebook cells in memory,
then runs Optuna by calling the embedded solver functions.

Free parameters in this calibration, and only these:
  - Dg_scale                  in [0.2, 5]
  - Dv_scale                  in [0.2, 5]
  - Dg_dislocation_scale      default [0.5, 30]
  - Dv_dislocation_scale      default [0.5, 30]

Fixed physics/parameters:
  - RHO_MODE = "rizk2023_smooth" with the smooth rho settings already in 6test_UN.ipynb
  - K_d, f_n, first-gas pressure factor, b_scale, coalescence_d_scale fixed from MANUAL_PARAMS/settings
  - USE_BULK_DISLOCATION_CAPTURE = False and capture_scale = 0.0
  - gamma, Xe yield, b0(R), solver equations unchanged
"""

from __future__ import annotations

import argparse
import json
import math
import os
import sys
import traceback
from pathlib import Path
from typing import Any, Dict, Iterable, List, Tuple

import nbformat
import numpy as np
import pandas as pd


# =============================================================================
# Configuration: scoring targets
# =============================================================================

GAS_PARTITION_TARGETS: Dict[float, Dict[float, Dict[str, Tuple[float, float]]]] = {
    1.1: {
        1200.0: {"bulk": (65, 85), "dislocation": (5, 25), "qgb": (0, 5), "matrix": (4, 6)},
        1400.0: {"bulk": (65, 85), "dislocation": (5, 25), "qgb": (0, 5), "matrix": (4, 6)},
        1600.0: {"bulk": (55, 80), "dislocation": (10, 35), "qgb": (2, 8), "matrix": (2, 5)},
        1800.0: {"bulk": (30, 55), "dislocation": (35, 60), "qgb": (5, 10), "matrix": (0.5, 3)},
        1900.0: {"bulk": (10, 35), "dislocation": (55, 80), "qgb": (8, 12), "matrix": (0, 2)},
        2000.0: {"bulk": (0, 15), "dislocation": (75, 88), "qgb": (10, 15), "matrix": (0, 1)},
        2200.0: {"bulk": (0, 5), "dislocation": (78, 90), "qgb": (10, 18), "matrix": (0, 0.5)},
    },
    3.2: {
        1200.0: {"bulk": (80, 90), "dislocation": (3, 12), "qgb": (0, 5), "matrix": (4, 6)},
        1400.0: {"bulk": (80, 90), "dislocation": (3, 12), "qgb": (0, 5), "matrix": (4, 6)},
        1600.0: {"bulk": (75, 88), "dislocation": (5, 20), "qgb": (2, 8), "matrix": (2, 5)},
        1800.0: {"bulk": (60, 80), "dislocation": (15, 30), "qgb": (5, 10), "matrix": (0.5, 3)},
        1900.0: {"bulk": (35, 55), "dislocation": (35, 55), "qgb": (8, 12), "matrix": (0, 2)},
        2000.0: {"bulk": (10, 25), "dislocation": (60, 78), "qgb": (10, 15), "matrix": (0, 1)},
        2200.0: {"bulk": (0, 8), "dislocation": (75, 90), "qgb": (10, 18), "matrix": (0, 0.5)},
    },
}

PRESSURE_BURNUPS = [1.1, 1.3, 3.2]
PRESSURE_TEMPS = [1200.0, 1400.0, 1600.0, 1800.0, 1900.0, 2000.0, 2200.0]

ND_DROP_BURNUPS = [1.1, 1.3, 3.2]
ND_DROP_TMID = 1400.0
ND_DROP_TARGETS = [(1800.0, 0.8), (2000.0, 0.5)]
ND_MIN = 1.0e16

# Prioritized score: R_d(T) and swelling_d(T) are the main experimental targets.
# Gas partition stays important, but cannot compensate a bad radius/swelling fit.
# Swelling vs burnup at 1600 K is kept weak because those points are less robust.
WEIGHTS = {
    "swelling_T": 20.0,
    "radius_T": 30.0,
    "partition": 3.0,
    "swelling_burnup": 1.0,
    "Nd_drop": 0.5,
    "pressure": 1.0,
    "guards": 5.0,
}


# =============================================================================
# Generic helpers
# =============================================================================

def finite(x: Any) -> bool:
    try:
        return math.isfinite(float(x))
    except Exception:
        return False


def positive_finite(x: Any) -> bool:
    return finite(x) and float(x) > 0.0


def safe_float(x: Any, default: float = math.nan) -> float:
    try:
        x = float(x)
        return x if math.isfinite(x) else default
    except Exception:
        return default


def logerr(model: float, target: float, eps: float = 1.0e-30) -> float:
    if not finite(model) or not finite(target):
        return 1.0e6
    model = max(float(model), eps)
    target = max(float(target), eps)
    return math.log(model / target) ** 2


def log_huber_err(model: float, target: float, delta: float = math.log(1.5), eps: float = 1.0e-30) -> float:
    """Robust loss on log(model/target).

    Below a factor ~1.5 error it behaves quadratically.
    Above that it grows linearly, so a single uncertain experimental point
    cannot dominate the entire objective. Systematic radius/swelling errors
    are still penalized strongly because many points contribute.
    """
    if not finite(model) or not finite(target):
        return 1.0e6
    model = max(float(model), eps)
    target = max(float(target), eps)
    e = abs(math.log(model / target))
    if e <= delta:
        return e * e
    return 2.0 * delta * e - delta * delta


def band_penalty(x: float, lo: float, hi: float) -> float:
    if not finite(x):
        return 1.0e6
    x = float(x)
    lo = float(lo)
    hi = float(hi)
    width = max(hi - lo, 1.0e-12)
    if lo <= x <= hi:
        return 0.0
    if x < lo:
        return ((x - lo) / width) ** 2
    return ((x - hi) / width) ** 2


def upper_soft_penalty(x: float, threshold: float, scale: float) -> float:
    if not finite(x):
        return 1.0e6
    return max(0.0, (float(x) - threshold) / scale) ** 2


def row_key(T: float, bu: float) -> Tuple[float, float]:
    # round avoids accidental duplicate keys from 1600 vs 1600.0
    return (round(float(T), 6), round(float(bu), 6))


# =============================================================================
# Notebook loading
# =============================================================================

def load_notebook_model(notebook_path: Path, output_dir: Path, dt_h: float, n_modes: int) -> Dict[str, Any]:
    """Load selected notebook cells into a namespace. Does not run the notebook's main() cell."""
    if not notebook_path.exists():
        raise FileNotFoundError(f"Notebook not found: {notebook_path}")

    nb = nbformat.read(notebook_path, as_version=4)
    import types
    module_name = "__optuna_loaded_notebook__"
    module = types.ModuleType(module_name)
    sys.modules[module_name] = module
    ns: Dict[str, Any] = module.__dict__
    ns["__name__"] = module_name

    # Required code cells in 6test_UN.ipynb:
    # 1 = settings, 4 = experimental data, 6 = solver core, 8 = manual runner/plot functions.
    for idx in [1, 4, 6, 8]:
        exec(nb.cells[idx].source, ns)

    # Override runtime settings in memory only.
    ns["OUTPUT_DIR"] = str(output_dir)
    ns["SHOW_PLOTS"] = False
    ns["DT_H"] = float(dt_h)
    ns["N_MODES"] = int(n_modes)

    # Force the new baseline in memory only; the notebook file is not modified.
    ns["USE_BULK_DISLOCATION_CAPTURE"] = False
    ns["RHO_MODE"] = "rizk2023_smooth"

    return ns


# =============================================================================
# rho_d models
# =============================================================================

def make_effective_rho_function(ns: Dict[str, Any], rho_cfg: Dict[str, Any], rho_fab_base: float, rho_max: float):
    """Return a monkey-patched effective_rho_d(T, burnup, cand) function."""
    mode = rho_cfg["rho_mode"]

    def rho_burnup_blank(F_percent: float) -> float:
        C1_RB = float(ns.get("C1_RB", 1.6e14))
        F0_RB = float(ns.get("F0_RB", 2.4))
        return max(float(rho_fab_base), C1_RB * max(float(F_percent) - F0_RB, 0.0))

    def rho_sat_shape_deformed(T: float, shift_K: float, tau_scale: float) -> float:
        # shift_K < 0 means the T-dependence rises earlier:
        # evaluate the saturating Ray/Blank curve at T_eff = T - shift_K.
        T_eff = float(T) - float(shift_K)
        rho940 = float(ns.get("RHO_SAT_RHO940", 6.3571))
        rhoinf = float(ns.get("RHO_SAT_RHOINF", 9.1036))
        tau = float(ns.get("RHO_SAT_TAU_K", 203.76)) * float(tau_scale)
        tau = max(tau, 1.0)
        return rhoinf - (rhoinf - rho940) * math.exp(-(T_eff - 940.0) / tau)

    def fT_deformed(T: float, shift_K: float, tau_scale: float, amp_scale: float) -> float:
        tref = float(ns.get("TREF_RB", 1025.0))
        raw = rho_sat_shape_deformed(T, shift_K, tau_scale) / rho_sat_shape_deformed(tref, shift_K, tau_scale)
        # Amp changes only the deviation from 1.
        val = 1.0 + float(amp_scale) * (raw - 1.0)
        return max(val, 0.05)

    def effective_rho_d(T: float, burnup: float, cand) -> float:
        if mode == "constant":
            rho = float(cand.rho_d)
        elif mode == "blank_sat":
            scale = float(rho_cfg["rho_blank_scale"])
            rho = rho_burnup_blank(burnup) * scale * float(ns["rho_sat_factor_raw"](T))
        elif mode == "blank_sat_deformed":
            scale = float(rho_cfg["rho_blank_def_scale"])
            shift = float(rho_cfg["rho_T_shift"])
            tau_scale = float(rho_cfg["rho_T_tau_scale"])
            amp_scale = float(rho_cfg["rho_T_amp_scale"])
            rho = rho_burnup_blank(burnup) * scale * fT_deformed(T, shift, tau_scale, amp_scale)
        elif mode == "rizk2023_fit":
            scale = float(rho_cfg["rho_rizk2023_fit_scale"])
            variant = str(rho_cfg["rho_rizk2023_fit_variant"])
            cap = float(rho_cfg["rho_rizk2023_fit_cap"])
            rho = scale * float(ns["rho_d_rizk2023_centipede"](
                F_percent=burnup,
                T_K=T,
                variant=variant,
                rho_cap=cap,
            ))
        else:
            raise ValueError(f"Unknown rho_mode={mode!r}")

        return min(max(rho, 1.0e10), float(rho_cfg.get("rho_rizk2023_fit_cap", rho_max)) if mode == "rizk2023_fit" else float(rho_max))

    return effective_rho_d


# =============================================================================
# Optuna candidate construction
# =============================================================================

def suggest_trial(trial, ns: Dict[str, Any], args) -> Tuple[Any, Dict[str, Any]]:
    """Build candidate for 6test: fixed smooth rho, only diffusivity scales sampled."""
    # Fixed rho_d(F,T) mode from 6test_UN. No rho shape sampling here.
    rho_cfg: Dict[str, Any] = {
        "rho_mode": "rizk2023_smooth_fixed",
        "RHO_RIZK2023_SMOOTH_SCALE": float(ns.get("RHO_RIZK2023_SMOOTH_SCALE", math.nan)),
        "RHO_RIZK2023_SMOOTH_RHO_FAB": float(ns.get("RHO_RIZK2023_SMOOTH_RHO_FAB", math.nan)),
        "RHO_RIZK2023_SMOOTH_RHO_AMP": float(ns.get("RHO_RIZK2023_SMOOTH_RHO_AMP", math.nan)),
        "RHO_RIZK2023_SMOOTH_FC_PERCENT": float(ns.get("RHO_RIZK2023_SMOOTH_FC_PERCENT", math.nan)),
        "RHO_RIZK2023_SMOOTH_T_HALF": float(ns.get("RHO_RIZK2023_SMOOTH_T_HALF", math.nan)),
        "RHO_RIZK2023_SMOOTH_WIDTH": float(ns.get("RHO_RIZK2023_SMOOTH_WIDTH", math.nan)),
        "RHO_RIZK2023_SMOOTH_F_MIN": float(ns.get("RHO_RIZK2023_SMOOTH_F_MIN", math.nan)),
        "RHO_RIZK2023_SMOOTH_CAP": float(ns.get("RHO_RIZK2023_SMOOTH_CAP", math.nan)),
    }

    # Only sampled parameters.
    Dg_scale = trial.suggest_float("Dg_scale", args.dg_scale_min, args.dg_scale_max, log=True)
    Dv_scale = trial.suggest_float("Dv_scale", args.dv_scale_min, args.dv_scale_max, log=True)
    Dg_dislocation_scale = trial.suggest_float(
        "Dg_dislocation_scale",
        args.dg_dislocation_scale_min,
        args.dg_dislocation_scale_max,
        log=True,
    )
    Dv_dislocation_scale = trial.suggest_float(
        "Dv_dislocation_scale",
        args.dv_dislocation_scale_min,
        args.dv_dislocation_scale_max,
        log=True,
    )

    # Fixed settings from the notebook.
    ns["RHO_MODE"] = "rizk2023_smooth"
    ns["USE_FIRST_GAS_PRESSURE_SEED"] = True
    ns["FIRST_GAS_PRESSURE_FACTOR"] = float(ns.get("FIRST_GAS_PRESSURE_FACTOR", 1.0 / 3.0))
    ns["SEED_BULK_ON_FIRST_GAS"] = True
    ns["SEED_DISLOCATION_ON_FIRST_GAS"] = True
    ns["USE_BULK_DISLOCATION_CAPTURE"] = False

    params = dict(ns["MANUAL_PARAMS"])
    params.update(
        {
            # Keep nominal/manual values fixed.
            "f_n": float(params.get("f_n", ns["F_N_NOMINAL"])),
            "K_d": float(params.get("K_d", ns["K_D_NOMINAL"])),
            "rho_d": float(params.get("rho_d", ns["RHO_D_NOMINAL"])),
            "fission_rate": float(params.get("fission_rate", ns["FISSION_RATE_NOMINAL"])),

            # Sampled diffusivity scales.
            "Dv_scale": Dv_scale,
            "Dv_D1_scale": 1.0,
            "Dv_D2_scale": 1.0,
            "Dv_dislocation_scale": Dv_dislocation_scale,

            "Dg_scale": Dg_scale,
            "Dg_D1_scale": 1.0,
            "Dg_D3_scale": 1.0,
            "Dg_dislocation_scale": Dg_dislocation_scale,

            # Fixed rest.
            "b_scale": 1.0,
            "b_bulk_scale": 1.0,
            "b_dislocation_scale": 1.0,

            "gb_scale": 1.0,
            "gd_scale": 1.0,
            "gd_bubble_scale": 1.0,
            "gd_line_scale": 1.0,
            "gd_line_alpha": 1.0,

            "coalescence_d_scale": 1.0,
            "capture_scale": 0.0,
        }
    )

    Candidate = ns["Candidate"]
    cand = Candidate(label=f"trial_{trial.number:05d}", **params)

    # Use notebook's own effective_rho_d for smooth rho.
    ns["RHO_MODE"] = "rizk2023_smooth"
    ns["RUN_CACHE"].clear()
    return cand, rho_cfg


# =============================================================================
# Scoring
# =============================================================================

class TrialRunner:
    def __init__(self, ns: Dict[str, Any], cand, args):
        self.ns = ns
        self.cand = cand
        self.args = args
        self.rows: Dict[Tuple[float, float], Dict[str, Any]] = {}

    def row(self, T: float, bu: float) -> Dict[str, Any]:
        key = row_key(T, bu)
        if key not in self.rows:
            r = self.ns["run_model_point"](float(T), float(bu), self.cand, self.args.dt_h, self.args.n_modes, keep_history=False)
            self.rows[key] = r
        return self.rows[key]

    def safe_row(self, T: float, bu: float) -> Dict[str, Any]:
        r = self.row(T, bu)
        # Basic sanity check on the most important quantities.
        required = ["swelling_d_percent", "Rd_nm", "Nd", "p_d", "p_d_eq"]
        for k in required:
            if not finite(r.get(k, math.nan)):
                raise FloatingPointError(f"Non-finite {k} at T={T}, bu={bu}: {r.get(k)}")
        return r


def score_swelling_T(ns: Dict[str, Any], runner: TrialRunner) -> float:
    vals = []
    for p in ns["EXP_SWELLING_T"]:
        # Experimental fit only where points exist; no high-T extrapolated fit.
        T = float(p["T"])
        bu = float(p["burnup"])
        target = float(p["swelling"])
        r = runner.safe_row(T, bu)
        vals.append(log_huber_err(r["swelling_d_percent"], target))
    return float(np.mean(vals)) if vals else 0.0


def score_radius_T(ns: Dict[str, Any], runner: TrialRunner) -> float:
    vals = []
    for p in ns["EXP_RD_T_13"]:
        T = float(p["T"])
        target = float(p["R_nm"])
        r = runner.safe_row(T, 1.3)
        vals.append(log_huber_err(r["Rd_nm"], target))
    return float(np.mean(vals)) if vals else 0.0


def score_swelling_burnup_1600(ns: Dict[str, Any], runner: TrialRunner) -> float:
    vals = []
    for p in ns["EXP_SWELLING_BURNUP_1600"]:
        bu = float(p["burnup"])
        target = float(p["swelling"])
        r = runner.safe_row(1600.0, bu)
        vals.append(log_huber_err(r["swelling_d_percent"], target))
    return float(np.mean(vals)) if vals else 0.0


def score_partition(runner: TrialRunner) -> float:
    vals = []
    key_map = {
        "bulk": "bulk_gas_percent",
        "dislocation": "dislocation_gas_percent",
        "qgb": "qgb_gas_percent",
        "matrix": "matrix_gas_percent",
    }
    for bu, by_T in GAS_PARTITION_TARGETS.items():
        for T, targets in by_T.items():
            r = runner.safe_row(T, bu)
            for name, (lo, hi) in targets.items():
                vals.append(band_penalty(r.get(key_map[name], math.nan), lo, hi))
    return float(np.mean(vals)) if vals else 0.0


def pressure_score_one_ratio(ratio: float) -> float:
    if not positive_finite(ratio):
        return 0.0
    # Penalize only p > p_eq.
    return max(0.0, math.log10(float(ratio))) ** 2


def score_pressure(runner: TrialRunner) -> float:
    vals_d = []
    vals_b = []
    for bu in PRESSURE_BURNUPS:
        for T in PRESSURE_TEMPS:
            r = runner.safe_row(T, bu)
            vals_d.append(pressure_score_one_ratio(r.get("p_d_over_eq", math.nan)))

            # Bulk excluded if population effectively absent.
            if positive_finite(r.get("Nb", math.nan)) and positive_finite(r.get("Rb_nm", math.nan)) and positive_finite(r.get("mb", math.nan)):
                vals_b.append(pressure_score_one_ratio(r.get("p_b_over_eq", math.nan)))

    jd = float(np.mean(vals_d)) if vals_d else 0.0
    jb = float(np.mean(vals_b)) if vals_b else 0.0
    return 0.7 * jd + 0.3 * jb


def score_Nd_drop(runner: TrialRunner) -> float:
    vals = []
    for bu in ND_DROP_BURNUPS:
        r_mid = runner.safe_row(ND_DROP_TMID, bu)
        N_mid = safe_float(r_mid.get("Nd", math.nan))
        if not positive_finite(N_mid):
            vals.append(1.0e4)
            continue
        for T_high, ratio_max in ND_DROP_TARGETS:
            r_hi = runner.safe_row(T_high, bu)
            N_hi = safe_float(r_hi.get("Nd", math.nan))
            if not positive_finite(N_hi):
                vals.append(1.0e4)
                continue
            vals.append(max(0.0, math.log(N_hi / (ratio_max * N_mid))) ** 2)

            # Do not allow "good drop" to mean numerical collapse.
            if N_hi < ND_MIN:
                vals.append((math.log(max(ND_MIN, 1.0) / max(N_hi, 1.0))) ** 2)
    return float(np.mean(vals)) if vals else 0.0


def score_guards(runner: TrialRunner) -> float:
    vals = []
    # Use the same grid as pressure + gas partition for validity.
    points = {(T, bu) for bu in PRESSURE_BURNUPS for T in PRESSURE_TEMPS}
    for bu, by_T in GAS_PARTITION_TARGETS.items():
        for T in by_T:
            points.add((T, bu))

    for T, bu in sorted(points):
        r = runner.safe_row(T, bu)

        Rd = safe_float(r.get("Rd_nm", math.nan))
        # Soft anti-runaway on dislocation radius.
        vals.append(upper_soft_penalty(Rd, 800.0, 400.0))
        vals.append(upper_soft_penalty(Rd, 1500.0, 250.0) * 4.0)

        psi_d = safe_float(r.get("psi_d", math.nan))
        vals.append(upper_soft_penalty(psi_d, 0.8, 0.2))
        vals.append(upper_soft_penalty(psi_d, 0.95, 0.05) * 4.0)

        xi_d = safe_float(r.get("porosity_d", math.nan))
        vals.append(upper_soft_penalty(xi_d, 0.5, 0.3))
        vals.append(upper_soft_penalty(xi_d, 0.8, 0.1) * 4.0)

        lam = safe_float(r.get("lambda_d", math.nan))
        if positive_finite(lam):
            vals.append(max(0.0, math.log10(lam / 1.0e2)) ** 2)
            vals.append(max(0.0, math.log10(lam / 1.0e4)) ** 2 * 4.0)

        fcap = safe_float(r.get("max_f_cap_step", math.nan))
        vals.append(max(0.0, (fcap - 0.1) / 0.9) ** 2)
        if finite(fcap) and fcap >= 0.999:
            vals.append(25.0)

    return float(np.mean(vals)) if vals else 0.0


def compute_score(ns: Dict[str, Any], cand, args) -> Tuple[float, Dict[str, float], Dict[Tuple[float, float], Dict[str, Any]]]:
    runner = TrialRunner(ns, cand, args)

    components = {
        "swelling_T": score_swelling_T(ns, runner),
        "radius_T": score_radius_T(ns, runner),
        "partition": score_partition(runner),
        "swelling_burnup": score_swelling_burnup_1600(ns, runner),
        "Nd_drop": score_Nd_drop(runner),
        "pressure": score_pressure(runner),
        "guards": score_guards(runner),
    }

    total = sum(WEIGHTS[k] * components[k] for k in components)
    return float(total), components, runner.rows


# =============================================================================
# Main
# =============================================================================

def make_study(args):
    try:
        import optuna
    except ImportError as exc:
        print("ERROR: optuna is not installed. Install it with:", file=sys.stderr)
        print("  pip install optuna", file=sys.stderr)
        raise exc

    sampler = optuna.samplers.TPESampler(seed=args.seed, multivariate=True)
    pruner = optuna.pruners.MedianPruner(n_startup_trials=max(20, args.n_trials // 10))

    storage = f"sqlite:///{args.output_dir / 'optuna_4test.db'}"
    study = optuna.create_study(
        direction="minimize",
        study_name=args.study_name,
        storage=storage,
        load_if_exists=True,
        sampler=sampler,
        pruner=pruner,
    )
    return study


def objective_factory(ns: Dict[str, Any], args):
    def objective(trial):
        try:
            cand, rho_cfg = suggest_trial(trial, ns, args)
            total, components, rows = compute_score(ns, cand, args)

            for k, v in components.items():
                trial.set_user_attr(f"score_{k}", float(v))
            trial.set_user_attr("rho_cfg", json.dumps(rho_cfg))
            trial.set_user_attr("first_gas_pressure_factor", float(ns.get("FIRST_GAS_PRESSURE_FACTOR", math.nan)))

            # Useful diagnostics from all computed rows.
            all_rows = list(rows.values())
            if all_rows:
                trial.set_user_attr("max_Rd_nm", max(safe_float(r.get("Rd_nm", 0.0), 0.0) for r in all_rows))
                trial.set_user_attr("max_f_cap_step", max(safe_float(r.get("max_f_cap_step", 0.0), 0.0) for r in all_rows))
                trial.set_user_attr("max_psi_d", max(safe_float(r.get("psi_d", 0.0), 0.0) for r in all_rows))
                trial.set_user_attr("min_Nd", min(safe_float(r.get("Nd", 1.0e300), 1.0e300) for r in all_rows))
            return total

        except Exception as exc:
            trial.set_user_attr("failed", True)
            trial.set_user_attr("error", repr(exc))
            if args.verbose_failures:
                traceback.print_exc()
            return 1.0e9
    return objective


def trial_to_candidate_and_rho(ns: Dict[str, Any], trial, args, label: str):
    """Recreate candidate/rho_cfg from a completed 6test trial."""
    p = trial.params
    rho_cfg: Dict[str, Any] = {"rho_mode": "rizk2023_smooth_fixed"}

    ns["RHO_MODE"] = "rizk2023_smooth"
    ns["USE_FIRST_GAS_PRESSURE_SEED"] = True
    ns["FIRST_GAS_PRESSURE_FACTOR"] = float(ns.get("FIRST_GAS_PRESSURE_FACTOR", 1.0 / 3.0))
    ns["SEED_BULK_ON_FIRST_GAS"] = True
    ns["SEED_DISLOCATION_ON_FIRST_GAS"] = True
    ns["USE_BULK_DISLOCATION_CAPTURE"] = False

    params = dict(ns["MANUAL_PARAMS"])
    params.update(
        {
            "f_n": float(params.get("f_n", ns["F_N_NOMINAL"])),
            "K_d": float(params.get("K_d", ns["K_D_NOMINAL"])),
            "rho_d": float(params.get("rho_d", ns["RHO_D_NOMINAL"])),
            "fission_rate": float(params.get("fission_rate", ns["FISSION_RATE_NOMINAL"])),

            "Dv_scale": float(p["Dv_scale"]),
            "Dv_D1_scale": 1.0,
            "Dv_D2_scale": 1.0,
            "Dv_dislocation_scale": float(p["Dv_dislocation_scale"]),

            "Dg_scale": float(p["Dg_scale"]),
            "Dg_D1_scale": 1.0,
            "Dg_D3_scale": 1.0,
            "Dg_dislocation_scale": float(p["Dg_dislocation_scale"]),

            "b_scale": 1.0,
            "b_bulk_scale": 1.0,
            "b_dislocation_scale": 1.0,
            "gb_scale": 1.0,
            "gd_scale": 1.0,
            "gd_bubble_scale": 1.0,
            "gd_line_scale": 1.0,
            "gd_line_alpha": 1.0,
            "coalescence_d_scale": 1.0,
            "capture_scale": 0.0,
        }
    )

    Candidate = ns["Candidate"]
    cand = Candidate(label=label, **params)
    ns["RUN_CACHE"].clear()
    return cand, rho_cfg


def write_trial_outputs(study, ns: Dict[str, Any], args):
    args.output_dir.mkdir(parents=True, exist_ok=True)

    # Full Optuna dataframe.
    df = study.trials_dataframe(attrs=("number", "value", "params", "user_attrs", "state"))
    df.to_csv(args.output_dir / "trials.csv", index=False)

    best = study.best_trial
    with open(args.output_dir / "best_params.json", "w", encoding="utf-8") as f:
        json.dump(
            {
                "number": best.number,
                "value": best.value,
                "params": best.params,
                "user_attrs": best.user_attrs,
            },
            f,
            indent=2,
        )

    # Human-readable summary and notebook paste block.
    lines = []
    lines.append("# Optuna 6test_UN smooth-rho diffusivity-only calibration summary\n")
    lines.append(f"- Best trial: `{best.number}`\n")
    lines.append(f"- Best objective: `{best.value:.8g}`\n")
    lines.append("\n## Best parameters\n")
    for k, v in best.params.items():
        lines.append(f"- `{k}` = `{v}`\n")

    lines.append("\n## Best component scores\n")
    for k, v in best.user_attrs.items():
        if k.startswith("score_"):
            lines.append(f"- `{k}` = `{v}`\n")

    lines.append("\n## Fixed parameters in this run\n")
    lines.append("- `gb_scale = 1.0`\n")
    lines.append("- `gd_scale = 1.0`\n")
    lines.append("- `b_scale = 1.0`\n")
    lines.append("- `coalescence_d_scale = 1.0`\n")
    lines.append("- `USE_BULK_DISLOCATION_CAPTURE = False`\n")
    lines.append("- `capture_scale = 0.0`\n")

    lines.append("\n## Notebook paste block for manual rerun\n")
    lines.append("```python\n")
    lines.append(f'CASE_LABEL = "optuna_best_{best.number:05d}"\n')
    lines.append('RHO_MODE = "rizk2023_smooth"\n')
    lines.append('USE_BULK_DISLOCATION_CAPTURE = False\n')
    lines.append("MANUAL_PARAMS.update({\n")
    lines.append(f'    "Dg_scale": {best.params["Dg_scale"]:.12g},\n')
    lines.append(f'    "Dv_scale": {best.params["Dv_scale"]:.12g},\n')
    lines.append(f'    "Dg_dislocation_scale": {best.params["Dg_dislocation_scale"]:.12g},\n')
    lines.append(f'    "Dv_dislocation_scale": {best.params["Dv_dislocation_scale"]:.12g},\n')
    lines.append('    "gb_scale": 1.0,\n')
    lines.append('    "gd_scale": 1.0,\n')
    lines.append('    "b_scale": 1.0,\n')
    lines.append('    "coalescence_d_scale": 1.0,\n')
    lines.append('    "capture_scale": 0.0,\n')
    lines.append("})\n")
    lines.append("```\n")
    lines.append("\n**Nota:** la dislocation density è fissa: `RHO_MODE = rizk2023_smooth`, con i parametri della prima cella di `6test_UN.ipynb`.\n")

    (args.output_dir / "best_summary.md").write_text("".join(lines), encoding="utf-8")


def rerun_best_plots(study, ns: Dict[str, Any], args):
    """Rerun top N trials with full notebook grid and plots."""
    completed = [t for t in study.trials if t.value is not None and math.isfinite(t.value)]
    completed = sorted(completed, key=lambda t: t.value)[: args.n_final]

    if not completed:
        return

    for rank, trial in enumerate(completed, start=1):
        label = f"rank{rank:02d}_trial{trial.number:05d}"
        final_dir = args.output_dir / "final_reruns" / label
        final_dir.mkdir(parents=True, exist_ok=True)

        ns["OUTPUT_DIR"] = str(final_dir)
        ns["SHOW_PLOTS"] = False
        ns["DT_H"] = float(args.final_dt_h if args.final_dt_h is not None else args.dt_h)
        ns["N_MODES"] = int(args.final_n_modes if args.final_n_modes is not None else args.n_modes)

        cand, rho_cfg = trial_to_candidate_and_rho(ns, trial, args, label)

        try:
            rows = ns["simulate_grid"](cand)
            csv_path, df = ns["write_csv"](rows, cand)
            saved = ns["make_all_plots"](rows, cand)

            with open(final_dir / "rerun_meta.json", "w", encoding="utf-8") as f:
                json.dump(
                    {
                        "rank": rank,
                        "trial_number": trial.number,
                        "objective": trial.value,
                        "params": trial.params,
                        "rho_cfg": rho_cfg,
                        "csv_path": str(csv_path),
                        "plots": [str(p) for p in saved],
                    },
                    f,
                    indent=2,
                )
        except Exception as exc:
            (final_dir / "FAILED.txt").write_text(
                f"Final rerun failed for trial {trial.number}:\n{repr(exc)}\n\n{traceback.format_exc()}",
                encoding="utf-8",
            )


def parse_args(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--notebook", type=Path, default=Path("6test_UN.ipynb"))
    parser.add_argument("--output-dir", type=Path, default=Path("optuna_6test_UN_results_smoothrho_diffusivity_only"))
    parser.add_argument("--study-name", type=str, default="optuna_6test_UN_smoothrho_diffusivity_only")
    parser.add_argument("--n-trials", type=int, default=200)
    parser.add_argument("--seed", type=int, default=12345)

    parser.add_argument("--dt-h", type=float, default=12.0)
    parser.add_argument("--n-modes", type=int, default=22)

    parser.add_argument("--dg-scale-min", type=float, default=0.2)
    parser.add_argument("--dg-scale-max", type=float, default=5.0)
    parser.add_argument("--dv-scale-min", type=float, default=0.2)
    parser.add_argument("--dv-scale-max", type=float, default=5.0)
    parser.add_argument("--dg-dislocation-scale-min", type=float, default=0.5)
    parser.add_argument("--dg-dislocation-scale-max", type=float, default=30.0)
    parser.add_argument("--dv-dislocation-scale-min", type=float, default=0.5)
    parser.add_argument("--dv-dislocation-scale-max", type=float, default=30.0)

    parser.add_argument("--n-final", type=int, default=5)
    parser.add_argument("--final-dt-h", type=float, default=None)
    parser.add_argument("--final-n-modes", type=int, default=None)
    parser.add_argument("--no-final-plots", action="store_true")

    parser.add_argument("--verbose-failures", action="store_true")
    return parser.parse_args(argv)


def main(argv=None):
    args = parse_args(argv)
    args.output_dir.mkdir(parents=True, exist_ok=True)

    ns = load_notebook_model(args.notebook, args.output_dir, args.dt_h, args.n_modes)

    study = make_study(args)
    objective = objective_factory(ns, args)

    print(f"Loaded notebook: {args.notebook}")
    print(f"Output directory: {args.output_dir}")
    print(f"Starting 6test smooth-rho NO-CAPTURE diffusivity-only Optuna: n_trials={args.n_trials}, dt_h={args.dt_h}, n_modes={args.n_modes}")

    study.optimize(objective, n_trials=args.n_trials, gc_after_trial=True)

    write_trial_outputs(study, ns, args)

    if not args.no_final_plots and args.n_final > 0:
        rerun_best_plots(study, ns, args)

    print("\nBest trial:")
    print(f"  number = {study.best_trial.number}")
    print(f"  value  = {study.best_trial.value:.8g}")
    print("  params:")
    for k, v in study.best_trial.params.items():
        print(f"    {k}: {v}")
    print(f"\nWrote results to: {args.output_dir}")


if __name__ == "__main__":
    main()
