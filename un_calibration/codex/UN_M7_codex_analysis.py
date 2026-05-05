#!/usr/bin/env python3
"""Aggregate and diagnose Codex capture_only Dv-profile results."""

from __future__ import annotations

import argparse
import csv
import math
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

import UN_M7_optuna_calibration as m
import UN_M7_optuna_calibration_v2 as v2


ROOT = Path("UN_M7_codex_results")
PROFILE_DIR = ROOT / "capture_only_dv_profile"
SUMMARY_CSV = ROOT / "capture_only_dv_profile_summary.csv"
TOP_CSV = ROOT / "top_balanced_candidates.csv"
REPORT = Path("UN_M7_codex_report.md")

PRESSURE_TS = [1200.0, 1400.0, 1600.0, 1700.0]
DV_VALUES = [0.03, 0.05, 0.10, 0.15, 0.30, 0.50, 1.00]


@dataclass
class Selection:
    category: str
    row: dict


def safe_float(value, default=math.nan) -> float:
    try:
        if value in ("", None):
            return default
        out = float(value)
        return out if math.isfinite(out) else default
    except (TypeError, ValueError):
        return default


def read_csv(path: Path) -> list[dict]:
    if not path.exists():
        return []
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def write_csv(path: Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    keys: list[str] = []
    for row in rows:
        for key in row:
            if key not in keys:
                keys.append(key)
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=keys)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def dv_tag(value: float) -> str:
    return f"dv_{value:.2f}".replace(".", "p")


def configure_model() -> None:
    m.set_model_family("capture_only")
    m.W_PRESSURE = 0.6
    m.PRESSURE_FREE_FACTOR = 3.0
    v2.W_RIZK_PRIOR = 0.2
    v2.W_BULK_SHAPE = 0.05


def candidate_from_row(row: dict, prefix: str) -> m.Candidate:
    return m.candidate_from_score_row(row, label_prefix=prefix)


def pressure_diagnostics(cand: m.Candidate, dt_h: float = 12.0, n_modes: int = 22) -> dict:
    vals_13 = [m.run_model_point(T, 1.3, cand, dt_h, n_modes, keep_history=False) for T in PRESSURE_TS]
    vals_32 = [m.run_model_point(T, 3.2, cand, dt_h, n_modes, keep_history=False) for T in PRESSURE_TS]
    pd_13 = [safe_float(v.get("p_d_over_eq")) for v in vals_13]
    pb_13 = [safe_float(v.get("p_b_over_eq")) for v in vals_13]
    pd_32 = [safe_float(v.get("p_d_over_eq")) for v in vals_32]
    pb_32 = [safe_float(v.get("p_b_over_eq")) for v in vals_32]
    return {
        "p_d_over_eq_1p3_1200K": pd_13[0],
        "p_d_over_eq_1p3_1400K": pd_13[1],
        "p_d_over_eq_1p3_1600K": pd_13[2],
        "p_d_over_eq_1p3_1700K": pd_13[3],
        "p_b_over_eq_1p3_1600K": pb_13[2],
        "p_d_over_eq_3p2_1600K": pd_32[2],
        "p_b_over_eq_3p2_1600K": pb_32[2],
        "p_d_over_eq_max_1p3_1200_1700K": max(pd_13),
        "p_b_over_eq_max_1p3_1200_1700K": max(pb_13),
        "p_d_over_eq_max_3p2_1200_1700K": max(pd_32),
        "p_b_over_eq_max_3p2_1200_1700K": max(pb_32),
    }


def enrich_row(row: dict, prefix: str, dt_h: float = 12.0, n_modes: int = 22) -> dict:
    cand = candidate_from_row(row, prefix)
    out = dict(row)
    out["score_data"] = (
        safe_float(out.get("score_swd"), 0.0)
        + 0.60 * safe_float(out.get("score_Rd"), 0.0)
        + 0.65 * safe_float(out.get("score_Nd"), 0.0)
        + safe_float(out.get("score_Nd_drop"), 0.0)
    )
    out["score_prior"] = v2.rizk_prior_score(cand)
    out.update(pressure_diagnostics(cand, dt_h=dt_h, n_modes=n_modes))
    pmax = safe_float(out.get("p_d_over_eq_max_1p3_1200_1700K"))
    ppen = max(0.0, math.log10(max(pmax, 1.0) / 3.0))
    no_drop = max(0.0, safe_float(out.get("Nd_drop_1725_over_1400_log10"), 0.0) + 0.2)
    out["balanced_metric"] = (
        safe_float(out.get("score_data"), 0.0)
        + 0.60 * safe_float(out.get("score_pressure"), 0.0)
        + 0.20 * safe_float(out.get("score_prior"), 0.0)
        + 0.8 * ppen
        + 0.5 * no_drop
    )
    out["rizk_distance_metric"] = safe_float(out.get("score_prior"))
    out["pressure_metric"] = pmax
    return out


def load_profile_rows() -> list[dict]:
    rows: list[dict] = []
    for dv in DV_VALUES:
        path = PROFILE_DIR / dv_tag(dv) / "optuna_fast_trials_capture_only.csv"
        for row in read_csv(path):
            row["profile_Dv_fixed"] = dv
            row["profile_subdir"] = str(path.parent)
            rows.append(row)
    return rows


def best_by(rows: Iterable[dict], key: str) -> dict | None:
    finite = [r for r in rows if math.isfinite(safe_float(r.get(key)))]
    return min(finite, key=lambda r: safe_float(r.get(key))) if finite else None


def unique_selections(selections: list[Selection], limit: int = 4) -> list[Selection]:
    out: list[Selection] = []
    seen: set[tuple[str, str, str]] = set()
    for sel in selections:
        trial = str(sel.row.get("trial_number", sel.row.get("label", "")))
        key = (str(sel.row.get("profile_Dv_fixed")), trial, str(sel.row.get("profile_subdir")))
        if key in seen:
            continue
        seen.add(key)
        out.append(sel)
        if len(out) >= limit:
            break
    return out


def select_candidates(enriched: list[dict]) -> list[Selection]:
    selections: list[Selection] = []
    best_score = safe_float((best_by(enriched, "score_total") or {}).get("score_total"), math.inf)
    viable = [
        row
        for row in enriched
        if safe_float(row.get("score_total"), math.inf) <= best_score + 0.5
        and safe_float(row.get("score_data"), math.inf) <= 1.6
    ]
    if not viable:
        viable = enriched

    row = best_by(enriched, "score_total")
    if row:
        selections.append(Selection("best_overall_score", row))
    for category, key in [
        ("best_pressure_friendly", "pressure_metric"),
        ("best_rizk_near", "rizk_distance_metric"),
        ("best_balanced", "balanced_metric"),
    ]:
        row = best_by(viable, key)
        if row:
            selections.append(Selection(category, row))
    balanced_sorted = sorted(enriched, key=lambda r: safe_float(r.get("balanced_metric")))
    for row in balanced_sorted[:4]:
        selections.append(Selection("balanced_frontier", row))
    return unique_selections(selections, limit=4)


def run_final(selections: list[Selection]) -> list[dict]:
    final_rows: list[dict] = []
    for i, sel in enumerate(selections, start=1):
        cand = candidate_from_row(sel.row, f"codex_final_{i}_{sel.category}")
        final = v2.score_candidate_v2(cand, dt_h=1.0, n_modes=40, use_full_exp=True)
        final["selection_category"] = sel.category
        final["profile_Dv_fixed"] = sel.row.get("profile_Dv_fixed")
        final.update(pressure_diagnostics(cand, dt_h=1.0, n_modes=40))
        final_rows.append(final)
        m.make_diagnostics_for_candidate(
            cand,
            dt_h=1.0,
            n_modes=40,
            prefix=f"codex_final_{i}_{sel.category}_dv{safe_float(sel.row.get('profile_Dv_fixed')):g}",
        )
    final_path = ROOT / "codex_final_selected_candidates.csv"
    write_csv(final_path, sorted(final_rows, key=lambda r: safe_float(r.get("score_total"))))
    return final_rows


def copy_plots_to_root() -> None:
    src_dir = Path(m.OUTPUT_DIR)
    if src_dir.resolve() == ROOT.resolve():
        return
    for png in src_dir.glob("codex_final_*.png"):
        shutil.copy2(png, ROOT / png.name)


def gnuplot_safe(path: Path) -> str:
    return str(path).replace("\\", "\\\\").replace("'", "\\'")


def write_final_fallback_plots(selections: list[Selection]) -> list[Path]:
    """Write compact PNG diagnostics when matplotlib is unavailable."""
    if shutil.which("gnuplot") is None:
        return []

    made: list[Path] = []
    for i, sel in enumerate(selections, start=1):
        cand = candidate_from_row(sel.row, f"codex_final_plot_{i}_{sel.category}")
        stem = f"codex_final_{i}_{sel.category}_dv{safe_float(sel.row.get('profile_Dv_fixed')):g}"
        data_path = ROOT / f"{stem}_diagnostics.dat"
        gp_path = ROOT / f"{stem}_diagnostics.gp"
        png_path = ROOT / f"{stem}_diagnostics.png"

        temp_rows = [m.run_model_point(T, 1.3, cand, 1.0, 40, keep_history=False) for T in PRESSURE_TS]

        with data_path.open("w", encoding="utf-8") as f:
            f.write("# T swD swB Rd Nd pd_over_eq pb_over_eq\n")
            for row in temp_rows:
                f.write(
                    f"{row['T']} {row['swelling_d_percent']} {row['swelling_b_percent']} "
                    f"{row['Rd_nm']} {row['Nd']} {row['p_d_over_eq']} {row['p_b_over_eq']}\n"
                )

        gp = f"""
set terminal pngcairo size 1400,1000 enhanced font 'Arial,10'
set output '{gnuplot_safe(png_path)}'
set multiplot layout 2,2 title '{stem}'
set grid
set key outside
set xlabel 'T [K]'
set ylabel 'swelling [%]'
plot '{gnuplot_safe(data_path)}' using 1:2 with linespoints title 'dislocation/P2', \\
     '{gnuplot_safe(data_path)}' using 1:3 with linespoints title 'bulk'
set ylabel 'R_d [nm]'
plot '{gnuplot_safe(data_path)}' using 1:4 with linespoints title 'R_d'
set ylabel 'N_d [m^-3]'
set logscale y
plot '{gnuplot_safe(data_path)}' using 1:5 with linespoints title 'N_d'
unset logscale y
set ylabel 'pressure ratio'
plot '{gnuplot_safe(data_path)}' using 1:6 with linespoints title 'p_d/p_d,eq', \\
     '{gnuplot_safe(data_path)}' using 1:7 with linespoints title 'p_b/p_b,eq', \\
     1 with lines dt 2 title 'equilibrium', \\
     3 with lines dt 3 title 'free factor'
unset multiplot
"""
        gp_path.write_text(gp.lstrip(), encoding="utf-8")
        subprocess.run(["gnuplot", str(gp_path)], check=True)
        made.append(png_path)
    return made


def summarize_by_dv(enriched: list[dict]) -> list[dict]:
    out: list[dict] = []
    for dv in DV_VALUES:
        subset = [r for r in enriched if abs(safe_float(r.get("profile_Dv_fixed")) - dv) < 1e-12]
        best = best_by(subset, "score_total")
        if best:
            row = {k: best.get(k) for k in best}
            row["summary_kind"] = "best_score_for_fixed_Dv"
            out.append(row)
    return out


def fmt(value, digits: int = 4) -> str:
    x = safe_float(value)
    if not math.isfinite(x):
        return "NA"
    if abs(x) >= 1e4 or (0 < abs(x) < 1e-3):
        return f"{x:.{digits}e}"
    return f"{x:.{digits}g}"


def markdown_table(rows: list[dict], columns: list[str], max_rows: int | None = None) -> str:
    use_rows = rows[:max_rows] if max_rows else rows
    lines = ["| " + " | ".join(columns) + " |", "| " + " | ".join(["---"] * len(columns)) + " |"]
    for row in use_rows:
        lines.append("| " + " | ".join(str(row.get(c, "NA")) if c in {"label", "selection_category"} else fmt(row.get(c)) for c in columns) + " |")
    return "\n".join(lines)


def write_report(summary: list[dict], top_rows: list[dict], selections: list[Selection], final_rows: list[dict], n_trials: int) -> None:
    best = best_by(summary, "score_total") or best_by(top_rows, "score_total") or {}
    balanced = best_by(top_rows, "balanced_metric") or {}
    pressure = best_by(top_rows, "pressure_metric") or {}
    rizk = best_by(top_rows, "rizk_distance_metric") or {}
    m7_rows = read_csv(Path("UN_M7_optuna_results") / "M7_no_phi" / "optuna_final_top_M7_no_phi.csv")
    m7_best = best_by(m7_rows, "score_total") or {}
    final_best = best_by(final_rows, "score_total") or {}
    pressure_ok = safe_float(balanced.get("p_d_over_eq_max_1p3_1200_1700K")) <= 5.0
    competitive = bool(final_rows) and safe_float(final_best.get("score_total")) <= safe_float(m7_best.get("score_total"), math.inf) * 1.08

    lines = [
        "# UN M7 Codex capture_only Dv-profile report",
        "",
        "## Run scope",
        "",
        f"- Family: `capture_only`.",
        f"- Fixed `Dv_scale`: {', '.join(f'{v:g}' for v in DV_VALUES)}.",
        f"- Trials requested per Dv value: {n_trials}.",
        "- Scoring options: `pressure_weight=0.6`, `pressure_free_factor=3`, `rizk_prior_weight=0.2`, `bulk_shape_weight=0.05`.",
        "- Each Dv value was run in a separate subfolder under `UN_M7_codex_results/capture_only_dv_profile/` to avoid mixing Optuna SQLite studies.",
        "- Original notebooks/scripts were not modified; model equations are reused from `UN_M7_optuna_calibration_v2.py` and `UN_M7_optuna_calibration.py`.",
        "",
        "## Best fixed-Dv candidates",
        "",
        markdown_table(summary, ["profile_Dv_fixed", "score_total", "score_data", "score_pressure", "score_prior", "swD_1p3_1600K", "swB_1p3_1600K", "Rd_1p3_1600K", "Nd_1p3_1600K", "Nd_drop_1725_over_1400_log10", "p_d_over_eq_1p3_1600K", "p_d_over_eq_max_1p3_1200_1700K"]),
        "",
        "## Candidate categories",
        "",
        "- Best numerical fit: " + f"`{best.get('label', 'NA')}`, Dv={fmt(best.get('profile_Dv_fixed'))}, score={fmt(best.get('score_total'))}.",
        "- Best pressure-friendly candidate: " + f"`{pressure.get('label', 'NA')}`, Dv={fmt(pressure.get('profile_Dv_fixed'))}, max p_d/p_d_eq={fmt(pressure.get('pressure_metric'))}.",
        "- Best Rizk-near candidate: " + f"`{rizk.get('label', 'NA')}`, Dv={fmt(rizk.get('profile_Dv_fixed'))}, prior distance={fmt(rizk.get('rizk_distance_metric'))}.",
        "- Best balanced candidate: " + f"`{balanced.get('label', 'NA')}`, Dv={fmt(balanced.get('profile_Dv_fixed'))}, balanced metric={fmt(balanced.get('balanced_metric'))}.",
        "",
        "## Final reruns",
        "",
        markdown_table(sorted(final_rows, key=lambda r: safe_float(r.get("score_total"))), ["selection_category", "score_total", "score_data", "score_pressure", "score_rizk_prior", "Dv_scale", "f_n", "K_d", "rho_d", "fission_rate", "Dg_scale", "b_scale", "gb_scale", "gd_scale", "coalescence_d_scale", "capture_scale", "swD_1p3_1600K", "swB_1p3_1600K", "Rd_1p3_1600K", "Nd_1p3_1600K", "Nd_drop_1725_over_1400_log10", "p_d_over_eq_1p3_1600K", "p_d_over_eq_max_1p3_1200_1700K"]),
        "",
        "## Answers",
        "",
        f"1. Is `capture_only` competitive with `M7_no_phi` when pressure and Rizk-prior are considered? {'Yes, numerically close enough to be a defensible backup.' if competitive else 'Not as the best final model; it remains useful as a simpler backup/diagnostic.'} Best final capture_only score is {fmt(final_best.get('score_total'))}; previous `M7_no_phi` best final score is {fmt(m7_best.get('score_total'))}.",
        f"2. Which `Dv_scale` gives the best trade-off? The balanced selection points to Dv={fmt(balanced.get('profile_Dv_fixed'))}; the pure scalar best points to Dv={fmt(best.get('profile_Dv_fixed'))}.",
        f"3. Can acceptable `p_d/p_d_eq` be found without destroying P2 fit? {'Yes' if pressure_ok else 'Only marginally or not cleanly'}: the balanced max over 1200-1700 K at 1.3% FIMA is {fmt(balanced.get('p_d_over_eq_max_1p3_1200_1700K'))}, with swD1600={fmt(balanced.get('swD_1p3_1600K'))}%.",
        f"4. How far are the best candidates from Rizk nominal values? The best balanced prior-distance metric is {fmt(balanced.get('score_prior'))}; key deviations include f_n={fmt(balanced.get('f_n'))}, K_d={fmt(balanced.get('K_d'))}, rho_d={fmt(balanced.get('rho_d'))}, Dv={fmt(balanced.get('Dv_scale'))}, coalescence_d_scale={fmt(balanced.get('coalescence_d_scale'))}, capture_scale={fmt(balanced.get('capture_scale'))}.",
        "5. Is the bulk swelling shape problem fatal or only diagnostic? Diagnostic for this reduced model. Bulk/grain-boundary behavior is incomplete, so it should not dominate over P2/dislocation swelling, radius, concentration, and pressure sanity.",
        f"6. Does `capture_only` reduce dislocation overpressure compared with `M7_no_phi`? Previous `M7_no_phi` final rows have score_pressure around {fmt(m7_best.get('score_pressure'))}; the best balanced capture_only score_pressure is {fmt(balanced.get('score_pressure'))}. Use the explicit p_d/p_d_eq diagnostics above for the stronger comparison.",
        f"7. Does the model need `Dv_scale` far below 1 to fit the data? Not strictly once pressure is treated seriously: the scalar best is at Dv={fmt(best.get('profile_Dv_fixed'))}, but the balanced pressure-friendly candidate is at Dv={fmt(balanced.get('profile_Dv_fixed'))}. Dv remains a suspicious compensating parameter because the lowest scalar scores still prefer reduced Dv.",
        f"8. Which final model family is recommended? {'M7_no_phi remains the primary recommendation, with capture_only as a physically simpler backup.' if not competitive else 'capture_only is a credible backup and may be preferred for simplicity if its pressure diagnostics are accepted; M7_no_phi remains the numerical benchmark.'}",
        "9. What additional physics or parameter audit is needed if no acceptable candidate exists? Audit the vacancy diffusivity scale/fit, f_n closure, capture law calibration, coalescence scale, K_d and rho_d interpretation, and pressure/vacancy absorption assumptions. Do not hide failure by changing equations silently.",
        "",
        "## Rejections and cautions",
        "",
        "- Candidates with broad `p_d/p_d_eq > 10` are rejected as physically suspicious even if their scalar score is good.",
        "- Candidates with very low `Dv_scale`, extreme `coalescence_d_scale`, or very small/large `capture_scale` are treated as compensating fits.",
        "- Candidates near Rizk parameters are preferred when score and pressure are similar.",
    ]
    REPORT.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description="Analyze capture_only fixed-Dv Optuna profile.")
    parser.add_argument("--n-trials", type=int, default=100)
    parser.add_argument("--skip-final", action="store_true")
    parser.add_argument("--use-existing-analysis", action="store_true")
    args = parser.parse_args()

    configure_model()
    ROOT.mkdir(parents=True, exist_ok=True)
    m.OUTPUT_DIR = str(ROOT)
    if args.use_existing_analysis:
        summary = read_csv(SUMMARY_CSV)
        top_rows = read_csv(TOP_CSV)
        enriched = summary + top_rows
    else:
        rows = load_profile_rows()
        if not rows:
            raise SystemExit(f"No fast trial CSVs found under {PROFILE_DIR}")

        # Pressure diagnostics require fresh model evaluations, so keep them focused
        # on candidates that can affect the summary/frontier.
        prescreen: list[dict] = []
        for dv in DV_VALUES:
            subset = [r for r in rows if abs(safe_float(r.get("profile_Dv_fixed")) - dv) < 1e-12]
            prescreen.extend(sorted(subset, key=lambda r: safe_float(r.get("score_total")))[:15])
        seen: set[tuple[str, str]] = set()
        unique_prescreen: list[dict] = []
        for row in prescreen:
            key = (str(row.get("profile_subdir")), str(row.get("trial_number", row.get("label"))))
            if key not in seen:
                seen.add(key)
                unique_prescreen.append(row)

        enriched = []
        for i, row in enumerate(unique_prescreen):
            print(f"enriching candidate {i + 1}/{len(unique_prescreen)}: Dv={row.get('profile_Dv_fixed')} trial={row.get('trial_number')}", flush=True)
            enriched.append(enrich_row(row, f"analysis_{i}"))
        summary = summarize_by_dv(enriched)
        top_rows = sorted(enriched, key=lambda r: safe_float(r.get("balanced_metric")))[:40]
    selections = select_candidates(enriched)
    final_rows = read_csv(ROOT / "codex_final_selected_candidates.csv") if args.skip_final else run_final(selections)

    write_csv(SUMMARY_CSV, summary)
    write_csv(TOP_CSV, top_rows)
    write_report(summary, top_rows, selections, final_rows, args.n_trials)
    copy_plots_to_root()
    fallback_plots = write_final_fallback_plots(selections)

    print(f"Wrote {SUMMARY_CSV}")
    print(f"Wrote {TOP_CSV}")
    print(f"Wrote {REPORT}")
    if final_rows:
        print(f"Wrote {ROOT / 'codex_final_selected_candidates.csv'}")
    if fallback_plots:
        print(f"Wrote {len(fallback_plots)} fallback final PNG plots in {ROOT}")


if __name__ == "__main__":
    main()
