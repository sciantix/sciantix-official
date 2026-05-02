#!/usr/bin/env python3
"""Structured no-Codex v5 calibration for the Rizk-base / no-capture family.

This script is meant for PC2 when Codex is not used.

It runs several independent Optuna blocks with:
  family = baseline
which in the current model switches means:
  - phi gas re-solution OFF
  - nucleation mass coupling OFF
  - bulk -> dislocation capture OFF
  - dislocation-bubble coalescence still ON

All studies are separated into different output folders so Optuna cannot
over-exploit one non-physical basin.  After the fast blocks, the script combines
the CSVs, selects candidates by category, and optionally makes final plots.

Run from the repository root:
  python UN_M7_v5_rizk_base_nocodex_runner.py --full
"""

from __future__ import annotations

import argparse
import csv
import math
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Dict, Iterable, List, Tuple

import UN_M7_optuna_calibration as m
import UN_M7_optuna_calibration_v5 as v5


BASE_ROOT = Path("UN_M7_optuna_v5_results/baseline_no_capture_structured")
RESULTS_ROOT = Path("UN_M7_v5_rizk_base_nocodex_results")
WORKLOG = RESULTS_ROOT / "WORKLOG.md"

FAMILY = "baseline"

# v5 scoring settings, same philosophy as capture_only v5.
V5_SETTINGS = {
    "pressure_weight": 0.70,
    "pressure_free_factor": 2.0,
    "rizk_prior_weight": 0.20,
    "partition_weight": 0.80,
    "qgb_weight": 0.70,
    "radius_guard_weight": 0.55,
    "radius_saturation_weight": 0.75,
    "rd2000_max_nm": 800.0,
    "rd_ratio_max": 2.8,
    "rd1800_soft_max_nm": 600.0,
    "rd1900_soft_max_nm": 700.0,
    "rd_post1800_delta_max_nm": 350.0,
    "rd_last_increment_factor_max": 0.75,
    "rd_1900_2000_ratio_max": 1.35,
}

# Parameter range format:
# name: (lo, hi, log)
# capture_scale is always forced to 0.0 for the baseline/no-capture family.
MODERATE = {
    "f_n": (1.0e-9, 1.0e-6, True),
    "K_d": (2.0e5, 8.0e5, True),
    "rho_d": (1.0e13, 6.0e13, True),
    "fission_rate": (4.5e19, 7.5e19, False),
    "Dv_scale": (0.5, 1.5, True),
    "Dg_scale": (0.5, 2.0, True),
    "D2_xe_scale": (0.3, 3.0, True),
    "b_scale": (0.3, 3.0, True),
    "gb_scale": (0.3, 3.0, True),
    "gd_scale": (0.5, 3.0, True),
    "coalescence_d_scale": (0.5, 5.0, True),
}

NOMINAL_NEAR = {
    **MODERATE,
    "Dv_scale": (0.7, 1.3, True),
    "Dg_scale": (0.7, 1.5, True),
    "D2_xe_scale": (0.5, 2.0, True),
    "b_scale": (0.5, 2.0, True),
    "gb_scale": (0.5, 2.0, True),
    "gd_scale": (0.7, 2.0, True),
    "coalescence_d_scale": (0.7, 3.0, True),
}

WIDE = {
    "f_n": (1.0e-10, 3.0e-6, True),
    "K_d": (1.0e5, 1.5e6, True),
    "rho_d": (5.0e12, 8.0e13, True),
    "fission_rate": (4.0e19, 8.0e19, False),
    "Dv_scale": (0.2, 2.0, True),
    "Dg_scale": (0.15, 4.0, True),
    "D2_xe_scale": (0.05, 20.0, True),
    "b_scale": (0.05, 8.0, True),
    "gb_scale": (0.05, 8.0, True),
    "gd_scale": (0.05, 10.0, True),
    "coalescence_d_scale": (0.1, 20.0, True),
}

ALL_WIDE = {
    "f_n": (1.0e-11, 1.0e-5, True),
    "K_d": (5.0e4, 2.0e6, True),
    "rho_d": (2.0e12, 1.0e14, True),
    "fission_rate": (4.0e19, 8.0e19, False),
    "Dv_scale": (0.1, 3.0, True),
    "Dg_scale": (0.08, 6.0, True),
    "D2_xe_scale": (0.01, 50.0, True),
    "b_scale": (0.02, 12.0, True),
    "gb_scale": (0.02, 12.0, True),
    "gd_scale": (0.02, 15.0, True),
    "coalescence_d_scale": (0.03, 40.0, True),
}


def with_overrides(base: Dict[str, Tuple[float, float, bool]], **kwargs):
    out = dict(base)
    out.update(kwargs)
    return out


BLOCKS = [
    {
        "name": "A_nominal_near",
        "n_trials": 300,
        "ranges": NOMINAL_NEAR,
        "purpose": "Rizk-near pilot: can the no-capture baseline work without moving far from nominal parameters?",
    },
    {
        "name": "B_standard_wide",
        "n_trials": 700,
        "ranges": WIDE,
        "purpose": "Main wide v5 baseline/no-capture exploration.",
    },
    {
        "name": "C1_diffusion_escape",
        "n_trials": 200,
        "ranges": with_overrides(MODERATE,
            Dv_scale=(0.1, 3.0, True),
            Dg_scale=(0.08, 6.0, True),
            D2_xe_scale=(0.01, 50.0, True),
        ),
        "purpose": "One-family escape: diffusivity scales wide; other families moderate.",
    },
    {
        "name": "C2_resolution_escape",
        "n_trials": 200,
        "ranges": with_overrides(MODERATE, b_scale=(0.02, 12.0, True)),
        "purpose": "One-family escape: re-solution scale wide.",
    },
    {
        "name": "C3_trapping_escape",
        "n_trials": 200,
        "ranges": with_overrides(MODERATE,
            gb_scale=(0.02, 12.0, True),
            gd_scale=(0.02, 15.0, True),
        ),
        "purpose": "One-family escape: bulk/dislocation trapping scales wide.",
    },
    {
        "name": "C4_microstructure_escape",
        "n_trials": 200,
        "ranges": with_overrides(MODERATE,
            f_n=(1.0e-11, 1.0e-5, True),
            K_d=(5.0e4, 2.0e6, True),
            rho_d=(2.0e12, 1.0e14, True),
        ),
        "purpose": "One-family escape: f_n, K_d, rho_d wide.",
    },
    {
        "name": "C5_coalescence_escape",
        "n_trials": 200,
        "ranges": with_overrides(MODERATE, coalescence_d_scale=(0.03, 40.0, True)),
        "purpose": "One-family escape: dislocation coalescence scale wide; capture remains disabled.",
    },
    {
        "name": "D1_resolution_trapping_pair",
        "n_trials": 200,
        "ranges": with_overrides(MODERATE,
            b_scale=(0.02, 12.0, True),
            gd_scale=(0.02, 15.0, True),
            gb_scale=(0.05, 8.0, True),
        ),
        "purpose": "Two-family escape: b_scale + trapping.",
    },
    {
        "name": "D2_Dv_coalescence_pair",
        "n_trials": 200,
        "ranges": with_overrides(MODERATE,
            Dv_scale=(0.1, 3.0, True),
            coalescence_d_scale=(0.03, 40.0, True),
        ),
        "purpose": "Two-family escape: vacancy diffusivity + dislocation coalescence.",
    },
    {
        "name": "D3_microstructure_coalescence_pair",
        "n_trials": 200,
        "ranges": with_overrides(MODERATE,
            f_n=(1.0e-11, 1.0e-5, True),
            K_d=(5.0e4, 2.0e6, True),
            rho_d=(2.0e12, 1.0e14, True),
            coalescence_d_scale=(0.03, 40.0, True),
        ),
        "purpose": "Two-family escape: microstructure + coalescence.",
    },
    {
        "name": "D4_diffusion_resolution_pair",
        "n_trials": 200,
        "ranges": with_overrides(MODERATE,
            Dv_scale=(0.1, 3.0, True),
            Dg_scale=(0.08, 6.0, True),
            D2_xe_scale=(0.01, 50.0, True),
            b_scale=(0.02, 12.0, True),
        ),
        "purpose": "Two-family escape: diffusion + resolution.",
    },
    {
        "name": "E_all_wide",
        "n_trials": 500,
        "ranges": ALL_WIDE,
        "purpose": "All-wide exploratory baseline/no-capture study with nonzero prior.",
    },
]


BASELINE_SEEDS = [
    {
        "f_n": 1.0e-6, "K_d": 5.0e5, "rho_d": 3.0e13, "fission_rate": 5.0e19,
        "Dv_scale": 1.0, "Dg_scale": 1.0, "D2_xe_scale": 1.0,
        "b_scale": 1.0, "gb_scale": 1.0, "gd_scale": 1.0,
        "coalescence_d_scale": 1.0, "capture_scale": 0.0,
    },
    {
        "f_n": 1.0e-7, "K_d": 3.0e5, "rho_d": 3.0e13, "fission_rate": 5.0e19,
        "Dv_scale": 0.5, "Dg_scale": 1.0, "D2_xe_scale": 1.0,
        "b_scale": 1.0, "gb_scale": 1.0, "gd_scale": 2.5,
        "coalescence_d_scale": 4.0, "capture_scale": 0.0,
    },
]


def ensure_dirs() -> None:
    RESULTS_ROOT.mkdir(parents=True, exist_ok=True)
    BASE_ROOT.mkdir(parents=True, exist_ok=True)


def run_cmd(cmd: List[str]) -> str:
    try:
        return subprocess.check_output(cmd, text=True, stderr=subprocess.STDOUT).strip()
    except Exception as exc:
        return f"ERROR running {' '.join(cmd)}: {exc}"


def append_worklog(text: str) -> None:
    ensure_dirs()
    with WORKLOG.open("a", encoding="utf-8") as f:
        f.write(text.rstrip() + "\n\n")


def setup_v5_globals() -> None:
    v5.W_RIZK_PRIOR = V5_SETTINGS["rizk_prior_weight"]
    v5.W_PARTITION = V5_SETTINGS["partition_weight"]
    v5.W_QGB = V5_SETTINGS["qgb_weight"]
    v5.W_RADIUS_GUARD = V5_SETTINGS["radius_guard_weight"]
    v5.W_RADIUS_SATURATION = V5_SETTINGS["radius_saturation_weight"]
    v5.RD2000_MAX_NM = V5_SETTINGS["rd2000_max_nm"]
    v5.RD_RATIO_MAX = V5_SETTINGS["rd_ratio_max"]
    v5.RD1800_SOFT_MAX_NM = V5_SETTINGS["rd1800_soft_max_nm"]
    v5.RD1900_SOFT_MAX_NM = V5_SETTINGS["rd1900_soft_max_nm"]
    v5.RD_POST1800_DELTA_MAX_NM = V5_SETTINGS["rd_post1800_delta_max_nm"]
    v5.RD_LAST_INCREMENT_FACTOR_MAX = V5_SETTINGS["rd_last_increment_factor_max"]
    v5.RD_1900_2000_RATIO_MAX = V5_SETTINGS["rd_1900_2000_ratio_max"]
    m.W_PRESSURE = V5_SETTINGS["pressure_weight"]
    m.PRESSURE_FREE_FACTOR = V5_SETTINGS["pressure_free_factor"]


def sample_float(trial, name: str, lo: float, hi: float, log: bool) -> float:
    if lo == hi:
        return float(lo)
    return float(trial.suggest_float(name, lo, hi, log=log))


def make_sampler(block_name: str, ranges: Dict[str, Tuple[float, float, bool]]):
    def sampler(trial, family: str):
        vals = {
            "f_n": sample_float(trial, "f_n", *ranges["f_n"]),
            "K_d": sample_float(trial, "K_d", *ranges["K_d"]),
            "rho_d": sample_float(trial, "rho_d", *ranges["rho_d"]),
            "fission_rate": sample_float(trial, "fission_rate", *ranges["fission_rate"]),
            "Dv_scale": sample_float(trial, "Dv_scale", *ranges["Dv_scale"]),
            "Dg_scale": sample_float(trial, "Dg_scale", *ranges["Dg_scale"]),
            "D2_xe_scale": sample_float(trial, "D2_xe_scale", *ranges["D2_xe_scale"]),
            "b_scale": sample_float(trial, "b_scale", *ranges["b_scale"]),
            "gb_scale": sample_float(trial, "gb_scale", *ranges["gb_scale"]),
            "gd_scale": sample_float(trial, "gd_scale", *ranges["gd_scale"]),
            "coalescence_d_scale": sample_float(trial, "coalescence_d_scale", *ranges["coalescence_d_scale"]),
            "capture_scale": 0.0,
        }
        return m.Candidate(label=f"baseline_v5_{block_name}_trial_{trial.number:05d}", **vals)
    return sampler


def seed_is_inside(seed: Dict[str, float], ranges: Dict[str, Tuple[float, float, bool]]) -> bool:
    for k, (lo, hi, _log) in ranges.items():
        if k in seed and not (lo <= float(seed[k]) <= hi):
            return False
    return True


def make_enqueue(ranges: Dict[str, Tuple[float, float, bool]]):
    def enqueue(study):
        for seed in BASELINE_SEEDS:
            if seed_is_inside(seed, ranges):
                try:
                    study.enqueue_trial(seed, skip_if_exists=True)
                except TypeError:
                    study.enqueue_trial(seed)
    return enqueue


def csv_path_for(out_dir: Path) -> Path:
    return out_dir / f"optuna_fast_trials_{FAMILY}.csv"


def read_csv(path: Path) -> List[Dict[str, str]]:
    if not path.exists():
        return []
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def write_csv(path: Path, rows: List[Dict[str, object]]) -> None:
    if not rows:
        return
    keys = list(rows[0].keys())
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=keys)
        w.writeheader()
        w.writerows(rows)


def sf(row: Dict[str, object], key: str, default: float = math.inf) -> float:
    try:
        val = row.get(key, default)
        if val is None or val == "":
            return default
        return float(val)
    except Exception:
        return default


def row_count(path: Path) -> int:
    return len(read_csv(path))


def physical_metric(row: Dict[str, object]) -> float:
    # Lower is better. This is deliberately not just score_total.
    score = sf(row, "score_total", 999.0)
    rd2000 = sf(row, "Rd_1p3_2000K", 9999.0)
    ratio = sf(row, "Rd_ratio_2000_over_1800", 99.0)
    pd = sf(row, "p_d_over_eq_1p3_1600K", 99.0)
    bulk1600 = sf(row, "bulk_gas_1p3_1600K", 0.0)
    qgb1600 = sf(row, "qgb_gas_1p3_1600K", 99.0)
    penalties = 0.0
    penalties += max(0.0, rd2000 - 1000.0) / 500.0
    penalties += max(0.0, ratio - 3.0) / 2.0
    penalties += max(0.0, pd - 3.0) / 2.0
    penalties += max(0.0, 35.0 - bulk1600) / 35.0
    penalties += max(0.0, qgb1600 - 20.0) / 20.0
    return score + penalties


def classify_rows(rows: List[Dict[str, object]], block_name: str = "") -> List[Dict[str, object]]:
    valid = [r for r in rows if math.isfinite(sf(r, "score_total"))]
    if not valid:
        return []

    selected: List[Tuple[str, Dict[str, object]]] = []

    def add(category: str, candidates: List[Dict[str, object]], keyfunc) -> None:
        if not candidates:
            return
        row = min(candidates, key=keyfunc)
        selected.append((category, row))

    add("best_score_total", valid, lambda r: sf(r, "score_total"))
    add("best_physical_metric", valid, physical_metric)
    add("best_radius_saturated", valid, lambda r: sf(r, "score_radius_guard") + sf(r, "score_radius_saturation"))
    add("best_partition_qgb", valid, lambda r: sf(r, "score_partition") + sf(r, "score_qgb"))
    add("best_pressure", valid, lambda r: sf(r, "score_pressure"))
    add("best_rizk_near", valid, lambda r: sf(r, "score_rizk_prior"))

    phys = [
        r for r in valid
        if sf(r, "Rd_1p3_2000K") <= 1000.0
        and sf(r, "p_d_over_eq_1p3_1600K") <= 3.0
        and sf(r, "qgb_gas_1p3_1600K") <= 25.0
    ]
    add("best_hard_physical_filter", phys, lambda r: sf(r, "score_total"))

    out = []
    seen = set()
    for category, row in selected:
        label = str(row.get("label", ""))
        key = (category, label)
        if key in seen:
            continue
        seen.add(key)
        new = dict(row)
        new["selection_category"] = category
        new["block"] = block_name
        new["physical_metric"] = physical_metric(row)
        out.append(new)
    return out


def summarize_block(block: Dict[str, object], out_dir: Path) -> None:
    rows = read_csv(csv_path_for(out_dir))
    selected = classify_rows(rows, str(block["name"]))
    if selected:
        write_csv(out_dir / "block_selected_candidates.csv", selected)
        # Small terminal/worklog table.
        lines = [
            f"## Block {block['name']} completed",
            f"Purpose: {block['purpose']}",
            f"Rows: {len(rows)}",
            "",
            "| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |",
            "|---|---|---:|---:|---:|---:|---:|---:|",
        ]
        for r in selected:
            lines.append(
                f"| {r.get('selection_category')} | {r.get('label')} | "
                f"{sf(r,'score_total'):.4g} | {sf(r,'Rd_1p3_2000K'):.4g} | "
                f"{sf(r,'Rd_ratio_2000_over_1800'):.4g} | {sf(r,'p_d_over_eq_1p3_1600K'):.4g} | "
                f"{sf(r,'bulk_gas_1p3_1600K'):.4g} | {sf(r,'qgb_gas_1p3_1600K'):.4g} |"
            )
        append_worklog("\n".join(lines))


def run_block(block: Dict[str, object], trial_scale: float, skip_completed: bool) -> None:
    name = str(block["name"])
    n_trials = max(1, int(round(int(block["n_trials"]) * trial_scale)))
    ranges = block["ranges"]
    out_dir = BASE_ROOT / name
    out_dir.mkdir(parents=True, exist_ok=True)
    existing_rows = row_count(csv_path_for(out_dir))

    if skip_completed and existing_rows >= n_trials:
        print(f"[SKIP] {name}: existing rows {existing_rows} >= requested {n_trials}")
        summarize_block(block, out_dir)
        return

    setup_v5_globals()
    m.optuna_candidate_from_trial = make_sampler(name, ranges)
    m.score_candidate = v5.score_candidate_v5
    m.enqueue_known_good_trials = make_enqueue(ranges)

    append_worklog(
        f"## Starting block {name}\n\n"
        f"Purpose: {block['purpose']}\n\n"
        f"Requested trials this run: {n_trials}\n\n"
        f"Output: `{out_dir}`"
    )

    print("=" * 120)
    print(f"RUNNING BLOCK {name}")
    print(f"Purpose: {block['purpose']}")
    print(f"Trials: {n_trials}")
    print(f"Output: {out_dir}")
    print("=" * 120)

    m.run_optuna_calibration(
        family=FAMILY,
        n_trials=n_trials,
        output_dir=str(out_dir),
        fast_dt_h=12.0,
        fast_n_modes=22,
        final_dt_h=1.0,
        final_n_modes=40,
        n_top_final=0,
        use_full_exp_fast=False,
        make_plots=False,
    )

    summarize_block(block, out_dir)


def combine_all() -> List[Dict[str, object]]:
    combined: List[Dict[str, object]] = []
    for block in BLOCKS:
        name = str(block["name"])
        out_dir = BASE_ROOT / name
        for r in read_csv(csv_path_for(out_dir)):
            rr = dict(r)
            rr["block"] = name
            rr["physical_metric"] = physical_metric(rr)
            combined.append(rr)

    if combined:
        write_csv(RESULTS_ROOT / "all_blocks_fast_trials.csv", combined)
        selected = classify_rows(combined, "all_blocks")
        write_csv(RESULTS_ROOT / "top_by_category.csv", selected)

        # Also save top 50 by physical metric and score.
        top_physical = sorted(combined, key=physical_metric)[:50]
        top_score = sorted(combined, key=lambda r: sf(r, "score_total"))[:50]
        write_csv(RESULTS_ROOT / "top50_physical_metric.csv", top_physical)
        write_csv(RESULTS_ROOT / "top50_score_total.csv", top_score)

        # Per-block best score and physical.
        per_block = []
        for block in BLOCKS:
            name = str(block["name"])
            rows = [r for r in combined if r.get("block") == name]
            if rows:
                best_s = min(rows, key=lambda r: sf(r, "score_total"))
                best_p = min(rows, key=physical_metric)
                per_block.append({
                    "block": name,
                    "n_rows": len(rows),
                    "best_score_label": best_s.get("label"),
                    "best_score_total": sf(best_s, "score_total"),
                    "best_score_Rd2000": sf(best_s, "Rd_1p3_2000K"),
                    "best_score_pd1600": sf(best_s, "p_d_over_eq_1p3_1600K"),
                    "best_physical_label": best_p.get("label"),
                    "best_physical_metric": physical_metric(best_p),
                    "best_physical_score_total": sf(best_p, "score_total"),
                    "best_physical_Rd2000": sf(best_p, "Rd_1p3_2000K"),
                    "best_physical_pd1600": sf(best_p, "p_d_over_eq_1p3_1600K"),
                })
        write_csv(RESULTS_ROOT / "block_overview.csv", per_block)

    return combined


def make_report(combined: List[Dict[str, object]]) -> None:
    selected = read_csv(RESULTS_ROOT / "top_by_category.csv")
    lines = [
        "# UN M7 v5 Rizk-base/no-capture structured run report",
        "",
        "Family used: `baseline`.",
        "",
        "Physics switches expected for `baseline`: phi gas re-solution OFF, nucleation mass coupling OFF, bulk→dislocation capture OFF. Dislocation coalescence remains controlled by `coalescence_d_scale` and is not disabled.",
        "",
        f"Total fast rows: {len(combined)}",
        "",
        "## Selected candidates by category",
        "",
        "| category | block | label | score | physical_metric | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |",
        "|---|---|---|---:|---:|---:|---:|---:|---:|---:|",
    ]
    for r in selected:
        lines.append(
            f"| {r.get('selection_category')} | {r.get('block')} | {r.get('label')} | "
            f"{sf(r,'score_total'):.4g} | {sf(r,'physical_metric'):.4g} | "
            f"{sf(r,'Rd_1p3_2000K'):.4g} | {sf(r,'Rd_ratio_2000_over_1800'):.4g} | "
            f"{sf(r,'p_d_over_eq_1p3_1600K'):.4g} | {sf(r,'bulk_gas_1p3_1600K'):.4g} | "
            f"{sf(r,'qgb_gas_1p3_1600K'):.4g} |"
        )
    lines += [
        "",
        "## Files",
        "",
        "- `all_blocks_fast_trials.csv`",
        "- `top_by_category.csv`",
        "- `top50_physical_metric.csv`",
        "- `top50_score_total.csv`",
        "- `block_overview.csv`",
        "- optional final plots under `final_selected/`",
        "",
        "## Reminder",
        "",
        "This is an automatic no-Codex run. Final scientific selection should still be done by inspecting plots, especially radius, pressure ratio, gas partition, and concentration.",
    ]
    (RESULTS_ROOT / "UN_M7_v5_rizk_base_nocodex_report.md").write_text("\n".join(lines) + "\n", encoding="utf-8")


def candidate_from_row(row: Dict[str, object], label_prefix: str) -> m.Candidate:
    # Base helper works with string rows.
    return m.candidate_from_score_row(row, label_prefix=label_prefix)


def make_final_plots(max_candidates: int = 10) -> None:
    selected = read_csv(RESULTS_ROOT / "top_by_category.csv")
    if not selected:
        return

    final_dir = RESULTS_ROOT / "final_selected"
    final_dir.mkdir(parents=True, exist_ok=True)

    # Deduplicate by label while preserving category order.
    picked = []
    seen = set()
    for row in selected:
        lab = str(row.get("label", ""))
        if lab and lab not in seen:
            picked.append(row)
            seen.add(lab)
        if len(picked) >= max_candidates:
            break

    write_csv(final_dir / "final_selected_candidates.csv", picked)

    m.set_model_family(FAMILY)
    m.OUTPUT_DIR = str(final_dir)
    m.SAVE_FIGS = True
    m.SHOW_PLOTS = False

    append_worklog(f"## Starting final plots\n\nCandidates: {len(picked)}\n\nOutput: `{final_dir}`")

    for i, row in enumerate(picked, start=1):
        label = str(row.get("label", f"candidate_{i}"))
        category = str(row.get("selection_category", "selected"))
        cand = candidate_from_row(row, label_prefix=f"rizk_base_final_{i}")
        prefix = f"rizk_base_final_{i}_{category}_{label}"
        print(f"[FINAL] {i}/{len(picked)} {category} {label}")
        m.make_diagnostics_for_candidate(cand, dt_h=1.0, n_modes=40, prefix=prefix)

    append_worklog(f"## Final plots completed\n\nOutput: `{final_dir}`")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--full", action="store_true", help="Run full overnight plan, about 3300 trials.")
    parser.add_argument("--quick", action="store_true", help="Run a very small smoke test, about 5% of full.")
    parser.add_argument("--trial-scale", type=float, default=None, help="Multiply the trial count of every block.")
    parser.add_argument("--only", default="", help="Comma-separated block names to run.")
    parser.add_argument("--skip-completed", action="store_true", help="Skip a block if its CSV already has at least planned rows.")
    parser.add_argument("--no-final-plots", action="store_true")
    parser.add_argument("--max-final-candidates", type=int, default=10)
    args = parser.parse_args()

    ensure_dirs()

    if args.trial_scale is not None:
        trial_scale = args.trial_scale
    elif args.quick:
        trial_scale = 0.05
    else:
        trial_scale = 1.0

    selected_names = {s.strip() for s in args.only.split(",") if s.strip()}

    git_status = run_cmd(["git", "status", "--short"])
    append_worklog(
        "# WORKLOG — v5 Rizk-base/no-capture structured run\n\n"
        f"Initial git status:\n\n```text\n{git_status}\n```\n\n"
        f"Trial scale: {trial_scale}\n\n"
        f"Family: {FAMILY}\n\n"
        "This run uses separate blocks, not one blind continuous Optuna study."
    )

    for block in BLOCKS:
        if selected_names and str(block["name"]) not in selected_names:
            continue
        run_block(block, trial_scale=trial_scale, skip_completed=args.skip_completed)

    combined = combine_all()
    make_report(combined)

    if not args.no_final_plots and combined:
        make_final_plots(max_candidates=args.max_final_candidates)

    git_status_end = run_cmd(["git", "status", "--short"])
    append_worklog(
        "## Run completed\n\n"
        f"Final git status:\n\n```text\n{git_status_end}\n```\n"
    )

    print("\nDONE.")
    print(f"Combined results: {RESULTS_ROOT / 'all_blocks_fast_trials.csv'}")
    print(f"Category summary: {RESULTS_ROOT / 'top_by_category.csv'}")
    print(f"Report: {RESULTS_ROOT / 'UN_M7_v5_rizk_base_nocodex_report.md'}")
    print(f"Worklog: {WORKLOG}")


if __name__ == "__main__":
    main()
