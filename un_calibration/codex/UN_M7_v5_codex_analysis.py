#!/usr/bin/env python3
"""Analyze and classify UN M7 v5 Codex calibration blocks."""

from __future__ import annotations

import argparse
import csv
import glob
import math
import os
from typing import Dict, Iterable, List


ROOT = "UN_M7_v5_codex_results"
FAMILY = "capture_only"
PARAMS = [
    "f_n", "K_d", "rho_d", "fission_rate", "Dv_scale", "Dg_scale", "D2_xe_scale",
    "b_scale", "gb_scale", "gd_scale", "coalescence_d_scale", "capture_scale",
]
SCORES = [
    "score_total", "score_swd", "score_Rd", "score_Nd", "score_Nd_drop",
    "score_pressure", "score_partition", "score_qgb", "score_radius_guard",
    "score_radius_saturation", "score_rizk_prior",
]


def f(row: Dict[str, str], key: str, default: float = math.inf) -> float:
    try:
        value = row.get(key, "")
        if value == "":
            return default
        out = float(value)
        return out if math.isfinite(out) else default
    except Exception:
        return default


def read_rows(path: str) -> List[Dict[str, str]]:
    if not os.path.exists(path):
        return []
    with open(path, newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def write_rows(path: str, rows: List[Dict[str, object]], fieldnames: Iterable[str] | None = None) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    if fieldnames is None:
        keys = []
        for row in rows:
            for key in row:
                if key not in keys:
                    keys.append(key)
        fieldnames = keys
    with open(path, "w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(fieldnames))
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def physical_flags(row: Dict[str, str]) -> Dict[str, bool]:
    rd2000 = f(row, "Rd_1p3_2000K")
    rd1900 = f(row, "Rd_1p3_1900K")
    rd1800 = f(row, "Rd_1p3_1800K")
    inc1819 = f(row, "Rd_inc_1800_1900_nm", 0.0)
    inc1920 = f(row, "Rd_inc_1900_2000_nm", 0.0)
    p1600 = f(row, "p_d_over_eq_1p3_1600K")
    bulk1600 = f(row, "bulk_gas_1p3_1600K", 0.0)
    qgb1600 = f(row, "qgb_gas_1p3_1600K")
    swd = f(row, "score_swd")
    rd_score = f(row, "score_Rd")
    nd = f(row, "score_Nd")
    drift = f(row, "score_rizk_prior")
    return {
        "radius_ok": rd2000 <= 1000.0 and rd1900 <= 1000.0 and rd1800 <= 800.0,
        "radius_strict": rd2000 <= 800.0 and inc1920 <= max(inc1819, 1.0) * 0.85,
        "radius_decelerates": inc1920 <= max(inc1819, 1.0),
        "pressure_ok": 0.2 <= p1600 <= 5.0,
        "partition_ok": bulk1600 >= 45.0 and qgb1600 <= 20.0,
        "fit_ok": swd <= 1.6 and rd_score <= 0.35 and nd <= 0.7,
        "drift_ok": drift <= 0.45,
    }


def reject_reason(row: Dict[str, str]) -> str:
    flags = physical_flags(row)
    if not flags["radius_ok"]:
        return "rejected_by_radius"
    if not flags["pressure_ok"]:
        return "rejected_by_pressure"
    if not flags["partition_ok"]:
        return "rejected_by_partition"
    if f(row, "score_rizk_prior") > 0.9:
        return "rejected_by_absurd_parameters"
    return ""


def best_by(rows: List[Dict[str, str]], key: str, candidates: List[Dict[str, str]] | None = None) -> Dict[str, str] | None:
    pool = candidates if candidates is not None else rows
    pool = [r for r in pool if math.isfinite(f(r, key))]
    if not pool:
        return None
    return min(pool, key=lambda r: f(r, key))


def classify(rows: List[Dict[str, str]], block: str) -> List[Dict[str, object]]:
    complete = [r for r in rows if math.isfinite(f(r, "score_total"))]
    if not complete:
        return []
    physical = []
    saturated = []
    partition = []
    pressure = []
    rizk = []
    for row in complete:
        flags = physical_flags(row)
        if flags["radius_ok"] and flags["pressure_ok"] and flags["partition_ok"] and flags["fit_ok"]:
            physical.append(row)
        if flags["radius_strict"]:
            saturated.append(row)
        if flags["partition_ok"]:
            partition.append(row)
        if flags["pressure_ok"]:
            pressure.append(row)
        if flags["drift_ok"]:
            rizk.append(row)

    picks = {
        "best_score_total": best_by(complete, "score_total"),
        "best_physical": best_by(complete, "score_total", physical),
        "best_radius_saturated": best_by(complete, "score_total", saturated),
        "best_partition": best_by(complete, "score_partition", partition),
        "best_pressure": best_by(complete, "score_pressure", pressure),
        "best_rizk_near": best_by(complete, "score_rizk_prior", rizk),
        "best_low_parameter_drift": best_by(complete, "score_rizk_prior"),
    }

    rejected = {
        "rejected_by_radius": [r for r in complete if reject_reason(r) == "rejected_by_radius"],
        "rejected_by_pressure": [r for r in complete if reject_reason(r) == "rejected_by_pressure"],
        "rejected_by_partition": [r for r in complete if reject_reason(r) == "rejected_by_partition"],
        "rejected_by_absurd_parameters": [r for r in complete if reject_reason(r) == "rejected_by_absurd_parameters"],
    }
    for key, pool in rejected.items():
        picks[key] = best_by(pool, "score_total")

    out = []
    for category, row in picks.items():
        if row is None:
            continue
        flags = physical_flags(row)
        rec: Dict[str, object] = {
            "block": block,
            "category": category,
            "label": row.get("label", ""),
            "is_physical": flags["radius_ok"] and flags["pressure_ok"] and flags["partition_ok"] and flags["fit_ok"],
            "reject_reason": reject_reason(row),
        }
        for key in SCORES + PARAMS:
            rec[key] = row.get(key, "")
        for key in [
            "Rd_1p3_1800K", "Rd_1p3_1900K", "Rd_1p3_2000K",
            "Rd_inc_1800_1900_nm", "Rd_inc_1900_2000_nm",
            "p_d_over_eq_1p3_1600K", "p_b_over_eq_1p3_1600K",
            "bulk_gas_1p3_1600K", "disl_gas_1p3_1600K", "qgb_gas_1p3_1600K",
            "swD_1p3_1600K", "Rd_1p3_1600K", "Nd_1p3_1600K",
        ]:
            rec[key] = row.get(key, "")
        out.append(rec)
    return out


def quantile(vals: List[float], q: float) -> float:
    vals = sorted(v for v in vals if math.isfinite(v))
    if not vals:
        return math.inf
    return vals[min(len(vals) - 1, max(0, round(q * (len(vals) - 1))))]


def summarize_scores(rows: List[Dict[str, str]], block: str) -> List[Dict[str, object]]:
    out = []
    for key in SCORES:
        vals = [f(r, key) for r in rows]
        vals = [v for v in vals if math.isfinite(v)]
        if not vals:
            continue
        out.append({
            "block": block,
            "component": key,
            "n": len(vals),
            "min": min(vals),
            "p10": quantile(vals, 0.10),
            "median": quantile(vals, 0.50),
            "p90": quantile(vals, 0.90),
            "max": max(vals),
        })
    return out


def summarize_params(rows: List[Dict[str, str]], block: str, top_n: int = 25) -> List[Dict[str, object]]:
    top = sorted([r for r in rows if math.isfinite(f(r, "score_total"))], key=lambda r: f(r, "score_total"))[:top_n]
    out = []
    for key in PARAMS:
        vals = [f(r, key) for r in top]
        vals = [v for v in vals if math.isfinite(v)]
        if not vals:
            continue
        out.append({
            "block": block,
            "parameter": key,
            "top_n": len(vals),
            "min": min(vals),
            "median": quantile(vals, 0.50),
            "max": max(vals),
            "direction_vs_1": "low" if quantile(vals, 0.50) < 0.7 and key.endswith("_scale") else ("high" if quantile(vals, 0.50) > 1.4 and key.endswith("_scale") else "near/mixed"),
        })
    return out


def analyze_block(block_dir: str) -> None:
    block = os.path.basename(block_dir.rstrip(os.sep))
    fast = os.path.join(block_dir, f"optuna_fast_trials_{FAMILY}.csv")
    rows = read_rows(fast)
    rows = sorted(rows, key=lambda r: f(r, "score_total"))
    top = rows[:50]
    cats = classify(rows, block)
    score_summary = summarize_scores(rows, block)
    param_summary = summarize_params(rows, block)

    write_rows(os.path.join(block_dir, "top_candidates.csv"), top)
    write_rows(os.path.join(block_dir, "top_by_category.csv"), cats)
    write_rows(os.path.join(block_dir, "score_component_summary.csv"), score_summary)
    write_rows(os.path.join(block_dir, "parameter_direction_summary.csv"), param_summary)
    write_rows(os.path.join(block_dir, "block_summary.csv"), [{
        "block": block,
        "n_trials": len(rows),
        "best_score_total": f(rows[0], "score_total") if rows else "",
        "best_label": rows[0].get("label", "") if rows else "",
        "n_physical_top50": sum(1 for r in top if all(physical_flags(r)[k] for k in ("radius_ok", "pressure_ok", "partition_ok", "fit_ok"))),
        "n_radius_ok_top50": sum(1 for r in top if physical_flags(r)["radius_ok"]),
        "n_partition_ok_top50": sum(1 for r in top if physical_flags(r)["partition_ok"]),
        "n_pressure_ok_top50": sum(1 for r in top if physical_flags(r)["pressure_ok"]),
    }])

    note = make_note(block, rows, cats, param_summary)
    with open(os.path.join(ROOT, "WORKLOG.md"), "a", encoding="utf-8") as handle:
        handle.write(note)


def make_note(block: str, rows: List[Dict[str, str]], cats: List[Dict[str, object]], param_summary: List[Dict[str, object]]) -> str:
    if not rows:
        return f"\n## Analysis {block}\n\n- No rows found; block did not produce analyzable fast trials.\n"
    best = rows[0]
    catmap = {str(r["category"]): r for r in cats}
    physical = catmap.get("best_physical")
    directions = ", ".join(
        f"{r['parameter']}={float(r['median']):.3g}" for r in param_summary
        if str(r.get("direction_vs_1")) != "near/mixed" and str(r["parameter"]).endswith("_scale")
    )
    if not directions:
        directions = "no strong scale drift in top candidates"
    phys_txt = "yes" if physical else "no"
    next_txt = "continue only if radius/partition/pressure trade-off improved; otherwise change strategy"
    return (
        f"\n## Analysis {block}\n\n"
        f"- Best score: `{best.get('label','')}` with `score_total={f(best, 'score_total'):.5g}`.\n"
        f"- Physical candidate found by current classifier: {phys_txt}.\n"
        f"- Best score diagnostics: `Rd2000={f(best, 'Rd_1p3_2000K'):.4g} nm`, "
        f"`p_d/p_eq1600={f(best, 'p_d_over_eq_1p3_1600K'):.4g}`, "
        f"`bulk1600={f(best, 'bulk_gas_1p3_1600K'):.4g}%`, "
        f"`qgb1600={f(best, 'qgb_gas_1p3_1600K'):.4g}%`.\n"
        f"- Parameter directions in top set: {directions}.\n"
        f"- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: {next_txt}.\n"
    )


def aggregate() -> None:
    block_dirs = [d for d in glob.glob(os.path.join(ROOT, "*")) if os.path.isdir(d)]
    all_block = []
    all_cats = []
    all_params = []
    for block_dir in sorted(block_dirs):
        all_block.extend(read_rows(os.path.join(block_dir, "block_summary.csv")))
        all_cats.extend(read_rows(os.path.join(block_dir, "top_by_category.csv")))
        all_params.extend(read_rows(os.path.join(block_dir, "parameter_direction_summary.csv")))
    write_rows(os.path.join(ROOT, "all_blocks_summary.csv"), all_block)
    write_rows(os.path.join(ROOT, "top_by_category.csv"), all_cats)
    write_rows(os.path.join(ROOT, "parameter_escape_summary.csv"), all_params)
    physical = [r for r in all_cats if str(r.get("is_physical", "")).lower() == "true"]
    physical = sorted(physical, key=lambda r: f(r, "score_total"))
    write_rows(os.path.join(ROOT, "top_physical_candidates.csv"), physical)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--block-dir")
    parser.add_argument("--aggregate", action="store_true")
    args = parser.parse_args()
    if args.block_dir:
        analyze_block(args.block_dir)
    if args.aggregate:
        aggregate()


if __name__ == "__main__":
    main()
