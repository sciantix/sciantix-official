#!/usr/bin/env python3
"""
UN_M7_rerun_codex_selected_exact.py

Rerun the 4 Codex-selected capture_only candidates with the full M7 diagnostic
plot set, without rerunning Optuna and without modifying the original model files.

Put this file in ~/sciantix-official, next to:
  - UN_M7_optuna_calibration.py
  - UN_M7_optuna_calibration_v2.py   (not required for the rerun, but it is the wrapper that generated the candidates)

Main output:
  UN_M7_codex_results/final_exact_full_plots/

The script first tries to read:
  UN_M7_codex_results/codex_final_selected_candidates.csv
If that CSV is present, its parameter values are used. If a column is missing
(e.g. D2_xe_scale), a safe fallback is used and written to README_RUN.md.
If the CSV is not present, the four candidates pasted in chat are used.
"""

from __future__ import annotations

import csv
import math
import os
import re
import sys
from pathlib import Path
from typing import Any, Dict, Iterable, List

# The base model/diagnostics module. UN_M7_optuna_calibration_v2.py is a wrapper
# around this file for Optuna/fixed-Dv studies; exact rerun diagnostics use the
# full model functions from here.
try:
    import UN_M7_optuna_calibration as m
except Exception as exc:
    raise SystemExit(
        "Could not import UN_M7_optuna_calibration.py.\n"
        "Run this script from ~/sciantix-official, next to UN_M7_optuna_calibration.py.\n"
        f"Original error: {exc!r}"
    )

ROOT = Path("UN_M7_codex_results")
SELECTED_CSV = ROOT / "codex_final_selected_candidates.csv"
OUT_ROOT = ROOT / "final_exact_full_plots"

FINAL_DT_H = 1.0
FINAL_N_MODES = 40

# Fallback values from the table pasted in chat / Codex report.
# If codex_final_selected_candidates.csv exists, it takes priority.
FALLBACK_CANDIDATES: List[Dict[str, Any]] = [
    {
        "selection_category": "best_overall_score",
        "score_total": 1.286,
        "score_pressure": 0.4584,
        "score_rizk_prior": 0.2037,
        "Dv_scale": 0.05,
        "f_n": 1.3333e-08,
        "K_d": 7.1430e05,
        "rho_d": 4.6135e13,
        "fission_rate": 4.9558e19,
        "Dg_scale": 2.012,
        "b_scale": 1.181,
        "gb_scale": 0.8994,
        "gd_scale": 3.641,
        "coalescence_d_scale": 10.99,
        "capture_scale": 3.247,
        "D2_xe_scale": 1.0,
    },
    {
        "selection_category": "best_balanced",
        "score_total": 1.337,
        "score_pressure": 0.005634,
        "score_rizk_prior": 0.2306,
        "Dv_scale": 1.0,
        "f_n": 2.5466e-07,
        "K_d": 6.4846e05,
        "rho_d": 3.5350e13,
        "fission_rate": 7.4799e19,
        "Dg_scale": 0.5333,
        "b_scale": 3.066,
        "gb_scale": 0.4625,
        "gd_scale": 4.507,
        "coalescence_d_scale": 6.997,
        "capture_scale": 0.1401,
        "D2_xe_scale": 1.0,
    },
    {
        "selection_category": "best_pressure_friendly",
        "score_total": 1.398,
        "score_pressure": 0.0,
        "score_rizk_prior": 0.2563,
        "Dv_scale": 1.0,
        "f_n": 1.9043e-07,
        "K_d": 6.3368e05,
        "rho_d": 4.8099e13,
        "fission_rate": 6.8347e19,
        "Dg_scale": 0.3921,
        "b_scale": 3.704,
        "gb_scale": 0.3881,
        "gd_scale": 3.375,
        "coalescence_d_scale": 7.005,
        "capture_scale": 0.0608,
        "D2_xe_scale": 1.0,
    },
    {
        "selection_category": "best_rizk_near",
        "score_total": 1.464,
        "score_pressure": 0.08804,
        "score_rizk_prior": 0.1539,
        "Dv_scale": 0.5,
        "f_n": 1.3237e-08,
        "K_d": 5.5892e05,
        "rho_d": 2.4815e13,
        "fission_rate": 7.6621e19,
        "Dg_scale": 1.307,
        "b_scale": 1.296,
        "gb_scale": 0.4151,
        "gd_scale": 1.963,
        "coalescence_d_scale": 9.772,
        "capture_scale": 0.1557,
        "D2_xe_scale": 1.0,
    },
]

REQUIRED_PARAMS = [
    "f_n",
    "K_d",
    "rho_d",
    "fission_rate",
    "Dv_scale",
    "Dg_scale",
    "b_scale",
    "gb_scale",
    "gd_scale",
    "coalescence_d_scale",
    "capture_scale",
]

OPTIONAL_PARAMS_WITH_DEFAULTS = {
    "D2_xe_scale": 1.0,
}

PLOT_BURNUPS = [1.1, 1.3, 3.2]
TEMP_MAIN = [float(T) for T in range(900, 2001, 50)]
TEMP_GAS_PARTITION = [float(T) for T in range(900, 2601, 100)]
BURNUP_GRID = [0.2, 0.5, 0.8, 1.1, 1.3, 1.6, 2.0, 2.5, 3.2, 4.0, 5.0, 6.0]


def slugify(text: str) -> str:
    text = str(text).strip().lower()
    text = text.replace(".", "p")
    text = re.sub(r"[^a-z0-9_+.-]+", "_", text)
    text = re.sub(r"_+", "_", text).strip("_")
    return text or "candidate"


def parse_float(value: Any, default: float = math.nan) -> float:
    if value is None:
        return default
    if isinstance(value, (int, float)):
        return float(value)
    text = str(value).strip()
    if text == "" or text.upper() == "NA":
        return default
    try:
        return float(text)
    except ValueError:
        return default


def read_selected_candidates() -> tuple[List[Dict[str, Any]], List[str]]:
    warnings: List[str] = []
    if not SELECTED_CSV.exists():
        warnings.append(f"{SELECTED_CSV} not found: using hard-coded fallback candidates from chat/report.")
        return [dict(row) for row in FALLBACK_CANDIDATES], warnings

    rows: List[Dict[str, Any]] = []
    with SELECTED_CSV.open("r", newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        for raw in reader:
            row: Dict[str, Any] = dict(raw)
            for key in REQUIRED_PARAMS:
                val = parse_float(row.get(key))
                if not math.isfinite(val):
                    raise SystemExit(f"Missing or invalid required parameter {key!r} in {SELECTED_CSV}: row={raw}")
                row[key] = val
            for key, default in OPTIONAL_PARAMS_WITH_DEFAULTS.items():
                val = parse_float(row.get(key), default)
                if not math.isfinite(val):
                    val = default
                if key not in raw or str(raw.get(key, "")).strip() in ("", "NA"):
                    warnings.append(f"Column {key!r} missing/empty for {row.get('selection_category', row.get('label', 'row'))}; using {default:g}.")
                row[key] = val
            if not row.get("selection_category"):
                row["selection_category"] = row.get("label") or f"candidate_{len(rows)+1}"
            rows.append(row)

    if not rows:
        warnings.append(f"{SELECTED_CSV} is empty: using hard-coded fallback candidates from chat/report.")
        return [dict(row) for row in FALLBACK_CANDIDATES], warnings

    return rows, warnings


def make_candidate(row: Dict[str, Any]) -> Any:
    label = str(row.get("selection_category") or row.get("label") or "candidate")
    return m.Candidate(
        label=label,
        f_n=float(row["f_n"]),
        K_d=float(row["K_d"]),
        rho_d=float(row["rho_d"]),
        fission_rate=float(row["fission_rate"]),
        Dv_scale=float(row["Dv_scale"]),
        Dg_scale=float(row["Dg_scale"]),
        b_scale=float(row["b_scale"]),
        gb_scale=float(row["gb_scale"]),
        gd_scale=float(row["gd_scale"]),
        coalescence_d_scale=float(row["coalescence_d_scale"]),
        capture_scale=float(row["capture_scale"]),
        D2_xe_scale=float(row.get("D2_xe_scale", 1.0)),
    )


def write_dict_rows(path: Path, rows: Iterable[Dict[str, Any]]) -> None:
    rows = list(rows)
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        path.write_text("", encoding="utf-8")
        return
    keys: List[str] = []
    for row in rows:
        for key in row.keys():
            if key not in keys:
                keys.append(key)
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=keys, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def strip_heavy(row: Dict[str, Any]) -> Dict[str, Any]:
    return {k: v for k, v in row.items() if k not in ("hist", "rates")}


def export_candidate_tables(cand: Any, out_dir: Path) -> Dict[str, Any]:
    """Export full numeric tables so we can inspect values even without opening PNGs."""
    point_rows: List[Dict[str, Any]] = []
    for burnup in PLOT_BURNUPS:
        for T in TEMP_MAIN:
            out = m.run_model_point(T, burnup, cand, FINAL_DT_H, FINAL_N_MODES, keep_history=False)
            row = strip_heavy(out)
            row["candidate"] = cand.label
            row["grid"] = "TEMP_MAIN"
            point_rows.append(row)
    write_dict_rows(out_dir / "point_diagnostics_T900_2000.csv", point_rows)

    gas_rows: List[Dict[str, Any]] = []
    for burnup in [1.1, 3.2]:
        for T in TEMP_GAS_PARTITION:
            out = m.run_model_point(T, burnup, cand, FINAL_DT_H, FINAL_N_MODES, keep_history=False)
            row = strip_heavy(out)
            row["candidate"] = cand.label
            row["grid"] = "GAS_PARTITION"
            gas_rows.append(row)
    write_dict_rows(out_dir / "gas_partition_T900_2600.csv", gas_rows)

    burnup_rows: List[Dict[str, Any]] = []
    for burnup in BURNUP_GRID:
        out = m.run_model_point(1600.0, burnup, cand, FINAL_DT_H, FINAL_N_MODES, keep_history=False)
        row = strip_heavy(out)
        row["candidate"] = cand.label
        row["grid"] = "BURNUP_1600K"
        burnup_rows.append(row)
    write_dict_rows(out_dir / "burnup_scan_1600K.csv", burnup_rows)

    # Histories at important points; useful for checking capture/vacancy transients.
    for T, burnup in [(1600.0, 1.3), (1600.0, 3.2), (2000.0, 1.1), (2000.0, 3.2)]:
        out = m.run_model_point(T, burnup, cand, FINAL_DT_H, FINAL_N_MODES, keep_history=True)
        hist = out.get("hist") or {}
        if hist:
            n = len(next(iter(hist.values())))
            rows = []
            for i in range(n):
                rows.append({k: v[i] for k, v in hist.items() if isinstance(v, list) and len(v) == n})
            write_dict_rows(out_dir / f"history_T{int(T)}_bu{str(burnup).replace('.', 'p')}.csv", rows)

    # Summary/check row at 1600K, 1.3% FIMA.
    probe = strip_heavy(m.run_model_point(1600.0, 1.3, cand, FINAL_DT_H, FINAL_N_MODES, keep_history=False))
    return probe


def write_readme(candidates: List[Dict[str, Any]], warnings: List[str]) -> None:
    OUT_ROOT.mkdir(parents=True, exist_ok=True)
    lines: List[str] = []
    lines.append("# Exact full-plot rerun of Codex-selected capture_only candidates")
    lines.append("")
    lines.append("This folder was generated by `UN_M7_rerun_codex_selected_exact.py`.")
    lines.append("")
    lines.append("## Model / numerical settings")
    lines.append("")
    lines.append("- Family: `capture_only`")
    lines.append(f"- final_dt_h: `{FINAL_DT_H}`")
    lines.append(f"- final_n_modes: `{FINAL_N_MODES}`")
    lines.append("- No Optuna trials are run here; candidates are rerun directly from selected parameter values.")
    lines.append("")
    lines.append("## Warnings / assumptions")
    lines.append("")
    if warnings:
        for w in warnings:
            lines.append(f"- {w}")
    else:
        lines.append("- None.")
    lines.append("")
    lines.append("## Candidates")
    lines.append("")
    header = ["selection_category", "score_total", *REQUIRED_PARAMS, "D2_xe_scale"]
    lines.append("| " + " | ".join(header) + " |")
    lines.append("|" + "|".join(["---"] * len(header)) + "|")
    for r in candidates:
        lines.append("| " + " | ".join(str(r.get(k, "")) for k in header) + " |")
    lines.append("")
    lines.append("## Files per candidate folder")
    lines.append("")
    lines.append("- PNGs produced by `make_diagnostics_for_candidate`: swelling, radius, concentration, pressure, pressure ratio, gas partition, capture diagnostic.")
    lines.append("- `point_diagnostics_T900_2000.csv`: numerical values for burnups 1.1, 1.3, 3.2% FIMA.")
    lines.append("- `gas_partition_T900_2600.csv`: gas partition values at 1.1 and 3.2% FIMA.")
    lines.append("- `burnup_scan_1600K.csv`: burnup scan at 1600 K.")
    lines.append("- `history_*.csv`: time histories for selected points.")
    (OUT_ROOT / "README_RUN.md").write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> None:
    OUT_ROOT.mkdir(parents=True, exist_ok=True)

    # Configure exact family used by Codex.
    m.set_model_family("capture_only")
    m.SAVE_FIGS = True
    m.SHOW_PLOTS = False

    # Make plots possible if matplotlib is installed.
    if getattr(m, "plt", None) is None:
        print("WARNING: matplotlib was not available when UN_M7_optuna_calibration.py was imported.")
        print("Install it with: pip install matplotlib pandas numpy")
        print("The CSV exports will still be produced, but base PNG plots may be skipped.")

    candidates, warnings = read_selected_candidates()
    write_readme(candidates, warnings)
    write_dict_rows(OUT_ROOT / "selected_candidates_used.csv", candidates)

    all_summary_rows: List[Dict[str, Any]] = []

    print("=" * 120)
    print("Exact full-plot rerun of Codex capture_only selections")
    print("=" * 120)
    print(f"Selected candidates source: {'CSV' if SELECTED_CSV.exists() else 'fallback table'}")
    print(f"Output root: {OUT_ROOT}")
    if warnings:
        print("Warnings:")
        for w in warnings:
            print(f"  - {w}")
    print("=" * 120)

    for idx, row in enumerate(candidates, start=1):
        cand = make_candidate(row)
        folder_name = f"{idx}_{slugify(row.get('selection_category', cand.label))}_dv{str(row.get('Dv_scale')).replace('.', 'p')}"
        out_dir = OUT_ROOT / folder_name
        out_dir.mkdir(parents=True, exist_ok=True)

        print("\n" + "#" * 120)
        print(f"Candidate {idx}/{len(candidates)}: {cand.label}")
        print(f"Folder: {out_dir}")
        print("#" * 120)

        # Important: make_diagnostics writes into m.OUTPUT_DIR.
        m.OUTPUT_DIR = str(out_dir)
        m.os.makedirs(m.OUTPUT_DIR, exist_ok=True)

        # Full standard plot set from the model script.
        m.make_diagnostics_for_candidate(cand, dt_h=FINAL_DT_H, n_modes=FINAL_N_MODES, prefix=slugify(cand.label))

        # Extra CSV exports with numeric values for inspection.
        probe = export_candidate_tables(cand, out_dir)

        summary = dict(row)
        summary.update({f"probe_{k}": v for k, v in probe.items() if k not in ("hist", "rates")})
        summary["candidate_folder"] = str(out_dir)
        all_summary_rows.append(summary)

        # Keep memory/cache from growing too much.
        if hasattr(m, "_RUN_CACHE"):
            m._RUN_CACHE.clear()

    write_dict_rows(OUT_ROOT / "exact_rerun_summary.csv", all_summary_rows)

    print("\nDONE")
    print(f"Results written under: {OUT_ROOT}")
    print(f"Summary CSV: {OUT_ROOT / 'exact_rerun_summary.csv'}")
    print(f"README: {OUT_ROOT / 'README_RUN.md'}")
    print("Open from Windows with:")
    print(f"  explorer.exe {OUT_ROOT}")


if __name__ == "__main__":
    main()
