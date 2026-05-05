#!/usr/bin/env python3
"""
UN_M7_dislocation_vacancy_relaxation_diagnostics.py

Diagnostic-only script for the UN/M7 reduced model.

Purpose
-------
Use one representative candidate only:
    M7_no_phi, Dv_scale = 0.5, PC1 balanced candidate.

The previous boundary-sink diagnostic showed a trade-off:
  - weak grain-face sink gives Rizk-like bulk/dislocation gas partition;
  - but dislocation bubbles become strongly overpressurized.

This script tests whether stronger vacancy absorption/pressure relaxation ONLY for
DISLOCATION bubbles can reduce p_d/p_eq while keeping a weaker grain-face sink.

Important
---------
This is NOT a proposed final model and NOT a calibration parameter yet.
It is a diagnostic perturbation. It does not modify the original model files.

It applies two diagnostic multipliers:
  1) boundary_sink_scale:
     multiplies only the spectral diffusion-to-boundary eigenvalue term.
  2) dislocation_vacancy_scale:
     multiplies Dv only in the vacancy absorption step of dislocation bubbles.

Outputs
-------
UN_M7_dislocation_vacancy_relaxation_diagnostics/
  candidate_parameters.csv
  point_diagnostics.csv
  combo_summary.csv
  diagnostic_summary.md

Usage
-----
cd ~/sciantix-official
source .venv/bin/activate
python UN_M7_dislocation_vacancy_relaxation_diagnostics.py

Faster quick test:
python UN_M7_dislocation_vacancy_relaxation_diagnostics.py --quick
"""

from __future__ import annotations

import argparse
import csv
import math
import os
from dataclasses import asdict
from typing import Dict, List, Sequence

import UN_M7_optuna_calibration as m

DEFAULT_CANDIDATE = dict(
    label="M7_no_phi_Dv05_trial_00097_balanced",
    f_n=1.1149e-08,
    K_d=5.34290727568e5,
    rho_d=2.7788924849394254e13,
    fission_rate=6.930584923894175e19,
    Dv_scale=0.50,
    Dg_scale=1.315673,
    D2_xe_scale=1.361065,
    b_scale=2.578705,
    gb_scale=2.010723,
    gd_scale=4.852245,
    coalescence_d_scale=6.966378,
    capture_scale=2.482966,
)

DEFAULT_BOUNDARY_SCALES = [0.0, 0.25, 0.5, 1.0]
DEFAULT_DISL_VAC_SCALES = [1.0, 2.0, 5.0, 10.0, 20.0]
DEFAULT_TEMPS = [1600.0, 2200.0]
DEFAULT_BURNUPS = [1.1, 1.3, 3.2]

_ORIGINAL_EXCHANGE_STEP = m.sciantix_3x3_exchange_step
_ORIGINAL_VAC_STEP = m.vacancy_concentration_implicit_step


def safe_float(x, default=math.nan):
    try:
        if x is None or x == "":
            return default
        return float(x)
    except Exception:
        return default


def candidate_from_csv(path: str, row_index: int = 0) -> m.Candidate:
    with open(path, newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))
    if not rows:
        raise ValueError(f"CSV has no rows: {path}")
    r = rows[row_index]
    return m.Candidate(
        label=str(r.get("label", f"csv_row_{row_index}")),
        f_n=safe_float(r.get("f_n")),
        K_d=safe_float(r.get("K_d")),
        rho_d=safe_float(r.get("rho_d")),
        fission_rate=safe_float(r.get("fission_rate")),
        Dv_scale=safe_float(r.get("Dv_scale")),
        Dg_scale=safe_float(r.get("Dg_scale")),
        D2_xe_scale=safe_float(r.get("D2_xe_scale"), 1.0),
        b_scale=safe_float(r.get("b_scale")),
        gb_scale=safe_float(r.get("gb_scale")),
        gd_scale=safe_float(r.get("gd_scale")),
        coalescence_d_scale=safe_float(r.get("coalescence_d_scale")),
        capture_scale=safe_float(r.get("capture_scale")),
    )


def default_candidate() -> m.Candidate:
    return m.Candidate(**DEFAULT_CANDIDATE)


def write_csv(path: str, rows: List[Dict]):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    if not rows:
        with open(path, "w", encoding="utf-8") as f:
            f.write("")
        return
    keys, seen = [], set()
    for row in rows:
        for k in row:
            if k not in seen:
                seen.add(k)
                keys.append(k)
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=keys)
        w.writeheader()
        for row in rows:
            w.writerow(row)


def make_params(cand: m.Candidate, T: float, burnup: float, dt_h: float, n_modes: int) -> m.UNParameters:
    return m.UNParameters(
        temperature=float(T),
        fission_rate=cand.fission_rate,
        grain_radius=m.GRAIN_RADIUS,
        target_burnup_percent_fima=float(burnup),
        dt=float(dt_h) * 3600.0,
        n_modes=int(n_modes),
        xe_yield=m.XE_YIELD,
        f_n=cand.f_n,
        K_d=cand.K_d,
        rho_d=cand.rho_d,
        Dv_scale=cand.Dv_scale,
        Dg_extra_scale=cand.Dg_scale,
        D2_xe_scale=cand.D2_xe_scale,
        b_scale=cand.b_scale,
        gb_scale=cand.gb_scale,
        gd_scale=cand.gd_scale,
        coalescence_d_scale=cand.coalescence_d_scale,
        capture_scale=cand.capture_scale,
        bulk_seed_radius_nm=0.0,
    )


def patch_boundary_sink(boundary_sink_scale: float):
    scale = float(boundary_sink_scale)

    def scaled_exchange_step(
        modes_c,
        modes_mb,
        modes_md,
        Dg: float,
        R_grain: float,
        source_c: float,
        source_mb: float,
        source_md: float,
        g_b: float,
        g_d: float,
        b_b_gas: float,
        b_d_gas: float,
        dt: float,
    ):
        projection_coeff = -2.0 * math.sqrt(2.0 / math.pi)
        diffusion_rate_coeff = scale * math.pi**2 * Dg / R_grain**2

        for i in range(len(modes_c)):
            n = i + 1
            n_coeff = (-1.0) ** n / n
            diffusion_rate = diffusion_rate_coeff * n**2
            src_c = projection_coeff * source_c * n_coeff
            src_mb = projection_coeff * source_mb * n_coeff
            src_md = projection_coeff * source_md * n_coeff
            A = [
                [1.0 + (diffusion_rate + g_b + g_d) * dt, -b_b_gas * dt, -b_d_gas * dt],
                [-g_b * dt, 1.0 + b_b_gas * dt, 0.0],
                [-g_d * dt, 0.0, 1.0 + b_d_gas * dt],
            ]
            rhs = [
                modes_c[i] + src_c * dt,
                modes_mb[i] + src_mb * dt,
                modes_md[i] + src_md * dt,
            ]
            modes_c[i], modes_mb[i], modes_md[i] = m.solve3x3_cramer(A, rhs)

        return m.reconstruct_average(modes_c), m.reconstruct_average(modes_mb), m.reconstruct_average(modes_md)

    m.sciantix_3x3_exchange_step = scaled_exchange_step


def patch_dislocation_vacancy_absorption(dislocation_vacancy_scale: float):
    """Patch vacancy step so that every second call in each time step (dislocation) uses Dv*scale.

    In the original solve_UN_M7 loop, vacancy_concentration_implicit_step is called in this order:
      1) bulk bubbles
      2) dislocation bubbles
    for each time step. This diagnostic relies on that order.
    """
    scale = float(dislocation_vacancy_scale)
    state = {"call_index": 0, "bulk_calls": 0, "disl_calls": 0}

    def wrapped(p: m.UNParameters, Dv: float, R: float, N: float, m_gas: float, n_old: float, dt: float):
        idx = state["call_index"]
        state["call_index"] += 1
        is_dislocation_call = (idx % 2 == 1)
        if is_dislocation_call:
            state["disl_calls"] += 1
            return _ORIGINAL_VAC_STEP(p, Dv * scale, R, N, m_gas, n_old, dt)
        state["bulk_calls"] += 1
        return _ORIGINAL_VAC_STEP(p, Dv, R, N, m_gas, n_old, dt)

    m.vacancy_concentration_implicit_step = wrapped
    return state


def reset_patches():
    m.sciantix_3x3_exchange_step = _ORIGINAL_EXCHANGE_STEP
    m.vacancy_concentration_implicit_step = _ORIGINAL_VAC_STEP


def row_from_hist(hist: Dict, rates: Dict, p: m.UNParameters, cand: m.Candidate, boundary_scale: float, disl_vac_scale: float, T: float, burnup: float, dt_h: float, n_modes: int, call_state: Dict) -> Dict:
    def h(key, default=math.nan):
        arr = hist.get(key, [])
        if not arr:
            return default
        return arr[-1]

    generated = h("generated", 0.0)
    c = h("c", 0.0)
    mb = h("mb", 0.0)
    md = h("md", 0.0)
    q = h("q_gb", 0.0)

    def pct(x):
        return 100.0 * x / generated if generated > 0.0 else 0.0

    # diagnostic anchors, not exact digitized targets
    partition_penalty = 0.0
    if abs(T - 1600.0) < 1.0 and abs(burnup - 3.2) < 1e-6:
        # user/Rizk-inspired low/intermediate T target: bulk around 78%
        partition_penalty += ((pct(mb) / 100.0 - 0.78) / 0.15) ** 2
        # q_gb should not dominate at low/intermediate T
        partition_penalty += ((pct(q) / 100.0 - 0.05) / 0.10) ** 2
    if abs(T - 2200.0) < 1.0 and abs(burnup - 3.2) < 1e-6:
        # high T target: dislocation around 95%, q_gb only few percent
        partition_penalty += ((pct(md) / 100.0 - 0.95) / 0.10) ** 2
        partition_penalty += ((pct(q) / 100.0 - 0.05) / 0.07) ** 2

    pd_eq = h("p_d_eq", math.nan)
    pb_eq = h("p_b_eq", math.nan)
    pd_over = h("p_d", 0.0) / pd_eq if pd_eq > 0.0 else math.nan
    pb_over = h("p_b", 0.0) / pb_eq if pb_eq > 0.0 else math.nan
    pressure_penalty = 0.0
    if math.isfinite(pd_over) and pd_over > 3.0:
        pressure_penalty = math.log10(pd_over / 3.0) ** 2

    return {
        "boundary_sink_scale": boundary_scale,
        "dislocation_vacancy_scale": disl_vac_scale,
        "candidate_label": cand.label,
        "family": m.MODEL_FAMILY,
        "T_K": T,
        "burnup_FIMA_percent": burnup,
        "dt_h": dt_h,
        "n_modes": n_modes,
        "bulk_vacancy_calls": call_state.get("bulk_calls", math.nan),
        "dislocation_vacancy_calls": call_state.get("disl_calls", math.nan),
        "generated": generated,
        "matrix_percent": pct(c),
        "bulk_percent": pct(mb),
        "dislocation_percent": pct(md),
        "qgb_percent": pct(q),
        "balance_sum_percent": pct(c) + pct(mb) + pct(md) + pct(q),
        "gas_balance_rel_error": (c + mb + md + q - generated) / generated if generated > 0 else math.nan,
        "swelling_b_percent": 100.0 * h("swelling_b", 0.0),
        "swelling_d_percent": 100.0 * h("swelling_d", 0.0),
        "swelling_ig_percent": 100.0 * h("swelling_ig", 0.0),
        "Rb_nm": h("Rb", 0.0) * 1.0e9,
        "Rd_nm": h("Rd", 0.0) * 1.0e9,
        "Nb": h("Nb", 0.0),
        "Nd": h("Nd", 0.0),
        "p_b_over_eq": pb_over,
        "p_d_over_eq": pd_over,
        "max_f_cap_step": h("max_f_cap_step", math.nan),
        "capture_fraction_sum": h("capture_fraction_sum", math.nan),
        "Dg": rates.get("Dg", math.nan),
        "Dv": rates.get("Dv", math.nan),
        "g_b": rates.get("g_b", math.nan),
        "g_d": rates.get("g_d", math.nan),
        "b_b": rates.get("b_b", math.nan),
        "b_d": rates.get("b_d", math.nan),
        "dnvd_dt_final": rates.get("dnvd_dt", math.nan),
        "dnvb_dt_final": rates.get("dnvb_dt", math.nan),
        "partition_anchor_penalty": partition_penalty,
        "pressure_penalty": pressure_penalty,
    }


def run_case(cand: m.Candidate, boundary_scale: float, disl_vac_scale: float, T: float, burnup: float, dt_h: float, n_modes: int):
    reset_patches()
    patch_boundary_sink(boundary_scale)
    call_state = patch_dislocation_vacancy_absorption(disl_vac_scale)
    p = make_params(cand, T, burnup, dt_h, n_modes)
    hist, rates = m.solve_UN_M7(p, keep_history=False)
    row = row_from_hist(hist, rates, p, cand, boundary_scale, disl_vac_scale, T, burnup, dt_h, n_modes, call_state)
    reset_patches()
    return row


def combo_summary(point_rows: List[Dict]) -> List[Dict]:
    grouped: Dict[tuple, List[Dict]] = {}
    for r in point_rows:
        key = (r["boundary_sink_scale"], r["dislocation_vacancy_scale"])
        grouped.setdefault(key, []).append(r)

    out = []
    for (bs, dvs), rows in grouped.items():
        def pick(T, bu):
            for r in rows:
                if abs(r["T_K"] - T) < 1.0 and abs(r["burnup_FIMA_percent"] - bu) < 1e-6:
                    return r
            return {}
        r1600_32 = pick(1600.0, 3.2)
        r2200_32 = pick(2200.0, 3.2)
        r1600_13 = pick(1600.0, 1.3)
        pmax = max([r.get("p_d_over_eq", 0.0) for r in rows if math.isfinite(r.get("p_d_over_eq", math.nan))] or [math.nan])
        qgb_mean = sum(r.get("qgb_percent", 0.0) for r in rows) / len(rows)
        partition_penalty = sum(r.get("partition_anchor_penalty", 0.0) for r in rows)
        pressure_penalty = sum(r.get("pressure_penalty", 0.0) for r in rows) / max(len(rows), 1)
        # A rough combined diagnostic, not a calibration score.
        combined = partition_penalty + 2.0 * pressure_penalty
        out.append({
            "boundary_sink_scale": bs,
            "dislocation_vacancy_scale": dvs,
            "diagnostic_combined_score": combined,
            "partition_anchor_penalty_sum": partition_penalty,
            "pressure_penalty_mean": pressure_penalty,
            "p_d_over_eq_max": pmax,
            "qgb_percent_mean": qgb_mean,
            "bulk_percent_1600_3p2": r1600_32.get("bulk_percent", math.nan),
            "dislocation_percent_1600_3p2": r1600_32.get("dislocation_percent", math.nan),
            "qgb_percent_1600_3p2": r1600_32.get("qgb_percent", math.nan),
            "p_d_over_eq_1600_3p2": r1600_32.get("p_d_over_eq", math.nan),
            "dislocation_percent_2200_3p2": r2200_32.get("dislocation_percent", math.nan),
            "qgb_percent_2200_3p2": r2200_32.get("qgb_percent", math.nan),
            "p_d_over_eq_2200_3p2": r2200_32.get("p_d_over_eq", math.nan),
            "swelling_d_1600_1p3": r1600_13.get("swelling_d_percent", math.nan),
            "Rd_nm_1600_1p3": r1600_13.get("Rd_nm", math.nan),
            "Nd_1600_1p3": r1600_13.get("Nd", math.nan),
        })
    out.sort(key=lambda r: r["diagnostic_combined_score"])
    return out


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--family", default="M7_no_phi", choices=["M7_full", "M7_no_phi", "capture_only", "baseline"])
    parser.add_argument("--output-dir", default="UN_M7_dislocation_vacancy_relaxation_diagnostics")
    parser.add_argument("--from-csv", default=None)
    parser.add_argument("--row", type=int, default=0)
    parser.add_argument("--dt-h", type=float, default=1.0)
    parser.add_argument("--n-modes", type=int, default=40)
    parser.add_argument("--temps", nargs="*", type=float, default=DEFAULT_TEMPS)
    parser.add_argument("--burnups", nargs="*", type=float, default=DEFAULT_BURNUPS)
    parser.add_argument("--boundary-scales", nargs="*", type=float, default=DEFAULT_BOUNDARY_SCALES)
    parser.add_argument("--disl-vac-scales", nargs="*", type=float, default=DEFAULT_DISL_VAC_SCALES)
    parser.add_argument("--quick", action="store_true", help="Use a smaller grid for a fast smoke test.")
    args = parser.parse_args()

    if args.quick:
        args.boundary_scales = [0.0, 0.5, 1.0]
        args.disl_vac_scales = [1.0, 5.0, 20.0]
        args.temps = [1600.0, 2200.0]
        args.burnups = [3.2]

    os.makedirs(args.output_dir, exist_ok=True)
    m.set_model_family(args.family)
    cand = candidate_from_csv(args.from_csv, args.row) if args.from_csv else default_candidate()
    write_csv(os.path.join(args.output_dir, "candidate_parameters.csv"), [asdict(cand)])

    point_rows: List[Dict] = []
    total = len(args.boundary_scales) * len(args.disl_vac_scales) * len(args.temps) * len(args.burnups)
    count = 0
    for bs in args.boundary_scales:
        for dvs in args.disl_vac_scales:
            print(f"boundary_sink_scale={bs:g}, dislocation_vacancy_scale={dvs:g}")
            for bu in args.burnups:
                for T in args.temps:
                    count += 1
                    print(f"  [{count}/{total}] T={T:g} K, burnup={bu:g}%")
                    row = run_case(cand, bs, dvs, T, bu, args.dt_h, args.n_modes)
                    point_rows.append(row)

    combos = combo_summary(point_rows)
    write_csv(os.path.join(args.output_dir, "point_diagnostics.csv"), point_rows)
    write_csv(os.path.join(args.output_dir, "combo_summary.csv"), combos)

    report_path = os.path.join(args.output_dir, "diagnostic_summary.md")
    with open(report_path, "w", encoding="utf-8") as f:
        f.write("# UN M7 dislocation vacancy relaxation diagnostic\n\n")
        f.write("This is diagnostic only. It tests whether stronger vacancy absorption in dislocation bubbles can relieve overpressure when the grain-face sink is weakened.\n\n")
        f.write("## Candidate\n\n")
        for k, v in asdict(cand).items():
            f.write(f"- `{k}` = {v}\n")
        f.write(f"\nFamily: `{m.MODEL_FAMILY}`\n\n")
        f.write("## Best diagnostic combinations\n\n")
        if combos:
            f.write("| rank | boundary_sink_scale | dislocation_vacancy_scale | combined | p_d/p_eq max | bulk1600 3.2 | disl2200 3.2 | qgb2200 3.2 |\n")
            f.write("|---:|---:|---:|---:|---:|---:|---:|---:|\n")
            for i, r in enumerate(combos[:10], 1):
                f.write(
                    f"| {i} | {r['boundary_sink_scale']:.3g} | {r['dislocation_vacancy_scale']:.3g} | "
                    f"{r['diagnostic_combined_score']:.4g} | {r['p_d_over_eq_max']:.4g} | "
                    f"{r['bulk_percent_1600_3p2']:.3g} | {r['dislocation_percent_2200_3p2']:.3g} | {r['qgb_percent_2200_3p2']:.3g} |\n"
                )
        f.write("\n## Interpretation guide\n\n")
        f.write("- If low boundary_sink_scale plus high dislocation_vacancy_scale gives Rizk-like partition and acceptable p_d/p_eq, then the missing/underestimated mechanism is likely pressure relaxation/vacancy absorption at dislocation bubbles.\n")
        f.write("- If pressure remains high even with large dislocation_vacancy_scale, then the issue is not only vacancy absorption; look at EOS, coalescence/Nd, or missing microstructural evolution.\n")
        f.write("- If partition only becomes Rizk-like at boundary_sink_scale near zero, then the reduced grain-face sink remains a major structural limitation.\n")
        f.write("\n## Output files\n\n")
        f.write("- `candidate_parameters.csv`\n- `point_diagnostics.csv`\n- `combo_summary.csv`\n")

    print(f"Wrote: {args.output_dir}")
    print(f"Summary: {report_path}")


if __name__ == "__main__":
    main()
