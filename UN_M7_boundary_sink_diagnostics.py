#!/usr/bin/env python3
"""
UN_M7_boundary_sink_diagnostics.py

Diagnostic-only script for the UN/M7 reduced model.

Purpose
-------
Test whether the large gas fraction going to `q_gb` is mainly caused by the
spectral diffusion sink to the grain face. This is NOT a new physical model and
NOT a calibration to be used as final. It only scales the eigenvalue loss term
D_g*pi^2*n^2/R_grain^2 inside the gas diffusion step to see how sensitive the
partition is to the grain-face sink.

It uses one representative candidate by default:
    M7_no_phi, Dv_scale = 0.5, PC1 final balanced candidate.

Outputs:
    UN_M7_boundary_sink_diagnostics/
        candidate_parameters.csv
        diffusion_timescales.csv
        boundary_sink_scale_summary.csv
        boundary_sink_scale_history.csv  (only if --with-history)
        diagnostic_summary.md

Usage
-----
cd ~/sciantix-official
source .venv/bin/activate
python UN_M7_boundary_sink_diagnostics.py

Optional:
python UN_M7_boundary_sink_diagnostics.py --from-csv path/to/optuna_final_top_M7_no_phi.csv --row 0
python UN_M7_boundary_sink_diagnostics.py --scales 0 0.25 0.5 0.75 1.0
python UN_M7_boundary_sink_diagnostics.py --with-history

Important
---------
`boundary_sink_scale` multiplies ONLY the diffusion-to-boundary eigenvalue term:
    diffusion_rate = boundary_sink_scale * (pi^2 * D_g / R_grain^2) * n^2
It does NOT scale D_g in trapping, nucleation, resolution, D_v, capture, or
coalescence. This lets us isolate whether q_gb is controlled by the spectral
boundary sink.
"""

from __future__ import annotations

import argparse
import csv
import math
import os
from dataclasses import asdict
from typing import Dict, List, Sequence, Optional

import UN_M7_optuna_calibration as m

# Balanced M7_no_phi, Dv=0.5 candidate from PC1 final rerun.
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

DEFAULT_TEMPERATURES = [1400.0, 1600.0, 1700.0, 1800.0, 2000.0, 2200.0]
DEFAULT_BURNUPS = [1.1, 3.2]
DEFAULT_SCALES = [0.0, 0.25, 0.5, 0.75, 1.0]


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
    if row_index < 0 or row_index >= len(rows):
        raise IndexError(f"row {row_index} out of range for {path}; n={len(rows)}")
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
        for k in row.keys():
            if k not in seen:
                seen.add(k)
                keys.append(k)
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=keys)
        w.writeheader()
        for r in rows:
            w.writerow(r)


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
    """Monkey-patch only the spectral diffusion-to-boundary rate."""
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


def row_from_hist(hist: Dict, rates: Dict, p: m.UNParameters, cand: m.Candidate, scale: float, T: float, burnup: float, dt_h: float, n_modes: int, idx: int = -1) -> Dict:
    def h(key, default=math.nan):
        arr = hist.get(key, [])
        if not arr:
            return default
        try:
            return arr[idx]
        except Exception:
            return default

    generated = h("generated", 0.0)
    c = h("c", 0.0)
    mb = h("mb", 0.0)
    md = h("md", 0.0)
    q = h("q_gb", 0.0)
    intra = c + mb + md

    def pct(x):
        return 100.0 * x / generated if generated > 0.0 else 0.0

    # Rizk-like soft anchors from Fig. 9 description, expressed as fractions of generated gas.
    # These are not exact digitization; they are diagnostic anchor points.
    bulk_anchor_penalty = 0.0
    disl_anchor_penalty = 0.0
    qgb_anchor_penalty = 0.0
    if T <= 1700.0:
        # User-provided qualitative target: bulk around 78% up to ~1700 K.
        bulk_anchor_penalty = ((pct(mb) / 100.0 - 0.78) / 0.15) ** 2
    if T >= 2000.0:
        # User-provided qualitative target: dislocation around 95% above ~2000 K.
        disl_anchor_penalty = ((pct(md) / 100.0 - 0.95) / 0.10) ** 2
        # If q_gb represents GB+FGR, Rizk suggests only a small few percent locally.
        qgb_anchor_penalty = ((pct(q) / 100.0 - 0.05) / 0.07) ** 2

    Dg = rates.get("Dg", math.nan)
    tau_s = p.grain_radius**2 / (math.pi**2 * Dg) if Dg > 0 else math.inf
    tau_scaled_s = p.grain_radius**2 / (scale * math.pi**2 * Dg) if Dg > 0 and scale > 0 else math.inf

    return {
        "boundary_sink_scale": scale,
        "candidate_label": cand.label,
        "family": m.MODEL_FAMILY,
        "T_K": T,
        "burnup_FIMA_percent": burnup,
        "dt_h": dt_h,
        "n_modes": n_modes,
        "time_s": h("time", 0.0),
        "burnup_from_time_percent": h("burnup_percent_fima", math.nan),
        "Dg": Dg,
        "D1_Xe": rates.get("D1_Xe", math.nan),
        "D2_Xe_scaled": rates.get("D2_Xe_scaled", math.nan),
        "D3_Xe": rates.get("D3_Xe", math.nan),
        "tau_boundary_unscaled_h": tau_s / 3600.0 if math.isfinite(tau_s) else math.inf,
        "tau_boundary_scaled_h": tau_scaled_s / 3600.0 if math.isfinite(tau_scaled_s) else math.inf,
        "generated": generated,
        "matrix_percent": pct(c),
        "bulk_percent": pct(mb),
        "dislocation_percent": pct(md),
        "qgb_percent": pct(q),
        "balance_sum_percent": pct(c) + pct(mb) + pct(md) + pct(q),
        "gas_balance_rel_error": (c + mb + md + q - generated) / generated if generated > 0 else math.nan,
        "intra_bulk_fraction_no_qgb": mb / intra if intra > 0 else math.nan,
        "intra_dislocation_fraction_no_qgb": md / intra if intra > 0 else math.nan,
        "swelling_b_percent": 100.0 * h("swelling_b", 0.0),
        "swelling_d_percent": 100.0 * h("swelling_d", 0.0),
        "swelling_ig_percent": 100.0 * h("swelling_ig", 0.0),
        "Rb_nm": h("Rb", 0.0) * 1.0e9,
        "Rd_nm": h("Rd", 0.0) * 1.0e9,
        "Nb": h("Nb", 0.0),
        "Nd": h("Nd", 0.0),
        "p_b_over_eq": h("p_b", 0.0) / h("p_b_eq", math.nan) if h("p_b_eq", 0.0) > 0 else math.nan,
        "p_d_over_eq": h("p_d", 0.0) / h("p_d_eq", math.nan) if h("p_d_eq", 0.0) > 0 else math.nan,
        "max_f_cap_step": h("max_f_cap_step", math.nan),
        "capture_fraction_sum": h("capture_fraction_sum", math.nan),
        "bulk_anchor_penalty": bulk_anchor_penalty,
        "disl_anchor_penalty": disl_anchor_penalty,
        "qgb_anchor_penalty": qgb_anchor_penalty,
        "partition_anchor_penalty": bulk_anchor_penalty + disl_anchor_penalty + qgb_anchor_penalty,
    }


def run_case(cand: m.Candidate, scale: float, T: float, burnup: float, dt_h: float, n_modes: int, keep_history: bool):
    patch_boundary_sink(scale)
    p = make_params(cand, T, burnup, dt_h, n_modes)
    hist, rates = m.solve_UN_M7(p, keep_history=keep_history)
    return hist, rates, p


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--family", default="M7_no_phi", choices=["M7_full", "M7_no_phi", "capture_only", "baseline"])
    parser.add_argument("--output-dir", default="UN_M7_boundary_sink_diagnostics")
    parser.add_argument("--from-csv", default=None)
    parser.add_argument("--row", type=int, default=0)
    parser.add_argument("--dt-h", type=float, default=1.0)
    parser.add_argument("--n-modes", type=int, default=40)
    parser.add_argument("--temps", nargs="*", type=float, default=DEFAULT_TEMPERATURES)
    parser.add_argument("--burnups", nargs="*", type=float, default=DEFAULT_BURNUPS)
    parser.add_argument("--scales", nargs="*", type=float, default=DEFAULT_SCALES)
    parser.add_argument("--with-history", action="store_true")
    args = parser.parse_args()

    os.makedirs(args.output_dir, exist_ok=True)
    m.set_model_family(args.family)
    cand = candidate_from_csv(args.from_csv, args.row) if args.from_csv else default_candidate()
    write_csv(os.path.join(args.output_dir, "candidate_parameters.csv"), [asdict(cand)])

    all_rows = []
    history_rows = []
    timescale_rows = []

    # Dg/tau with no model run side effects.
    for T in args.temps:
        p_tau = make_params(cand, T, args.burnups[0], args.dt_h, args.n_modes)
        Dg, parts = m.xe_diffusivity_UN(p_tau)
        tau_s = p_tau.grain_radius**2 / (math.pi**2 * Dg) if Dg > 0 else math.inf
        timescale_rows.append({
            "T_K": T,
            "Dg": Dg,
            "D1_Xe": parts.get("D1_Xe", math.nan),
            "D2_Xe_scaled": parts.get("D2_Xe_scaled", math.nan),
            "D3_Xe": parts.get("D3_Xe", math.nan),
            "tau_boundary_unscaled_h": tau_s / 3600.0 if math.isfinite(tau_s) else math.inf,
            "tau_boundary_unscaled_days": tau_s / (3600.0 * 24.0) if math.isfinite(tau_s) else math.inf,
            "irradiation_time_1p1_FIMA_days": m.burnup_percent_to_time(1.1, cand.fission_rate, m.LATTICE_PARAMETER) / (3600.0 * 24.0),
            "irradiation_time_3p2_FIMA_days": m.burnup_percent_to_time(3.2, cand.fission_rate, m.LATTICE_PARAMETER) / (3600.0 * 24.0),
        })

    for scale in args.scales:
        print(f"Running boundary_sink_scale={scale:g} ...")
        for burnup in args.burnups:
            for T in args.temps:
                keep = bool(args.with_history and scale in (0.0, 0.5, 1.0) and T in (1600.0, 2200.0) and burnup in (1.1, 3.2))
                hist, rates, p = run_case(cand, scale, T, burnup, args.dt_h, args.n_modes, keep_history=keep)
                all_rows.append(row_from_hist(hist, rates, p, cand, scale, T, burnup, args.dt_h, args.n_modes, idx=-1))
                if keep:
                    n = len(hist.get("time", []))
                    if n > 0:
                        idxs = sorted(set([0, min(1, n-1), min(5, n-1), min(20, n-1), n//4, n//2, (3*n)//4, n-1]))
                        for idx in idxs:
                            r = row_from_hist(hist, rates, p, cand, scale, T, burnup, args.dt_h, args.n_modes, idx=idx)
                            r["history_index"] = idx
                            history_rows.append(r)

    write_csv(os.path.join(args.output_dir, "diffusion_timescales.csv"), timescale_rows)
    write_csv(os.path.join(args.output_dir, "boundary_sink_scale_summary.csv"), all_rows)
    write_csv(os.path.join(args.output_dir, "boundary_sink_scale_history.csv"), history_rows)

    # Simple markdown report.
    report_path = os.path.join(args.output_dir, "diagnostic_summary.md")
    with open(report_path, "w", encoding="utf-8") as f:
        f.write("# UN M7 boundary sink diagnostics\n\n")
        f.write("## Candidate\n\n")
        for k, v in asdict(cand).items():
            f.write(f"- `{k}` = {v}\n")
        f.write(f"\nFamily: `{m.MODEL_FAMILY}`\n\n")
        f.write("## Meaning of boundary_sink_scale\n\n")
        f.write("This is a diagnostic-only multiplier on the spectral diffusion-to-boundary eigenvalue term. It is not a Rizk parameter and not a proposed final calibration parameter.\n\n")
        f.write("## Quick flags\n\n")
        # Highlight rows where qgb is high at low T and dislocation target is missed at high T.
        flags = []
        for r in all_rows:
            if r["T_K"] <= 1700 and r["qgb_percent"] > 15:
                flags.append(f"scale={r['boundary_sink_scale']}: q_gb={r['qgb_percent']:.1f}% at T={r['T_K']:.0f} K, burnup={r['burnup_FIMA_percent']}%")
            if r["T_K"] >= 2000 and r["dislocation_percent"] < 85:
                flags.append(f"scale={r['boundary_sink_scale']}: dislocation gas={r['dislocation_percent']:.1f}% at T={r['T_K']:.0f} K, burnup={r['burnup_FIMA_percent']}%")
            if r["p_d_over_eq"] > 10:
                flags.append(f"scale={r['boundary_sink_scale']}: p_d/p_eq={r['p_d_over_eq']:.1f} at T={r['T_K']:.0f} K, burnup={r['burnup_FIMA_percent']}%")
        if flags:
            for fl in flags[:100]:
                f.write(f"- {fl}\n")
            if len(flags) > 100:
                f.write(f"- ... {len(flags)-100} more flags omitted.\n")
        else:
            f.write("No quick flags.\n")
        f.write("\n## Output files\n\n")
        f.write("- `candidate_parameters.csv`\n")
        f.write("- `diffusion_timescales.csv`\n")
        f.write("- `boundary_sink_scale_summary.csv`\n")
        f.write("- `boundary_sink_scale_history.csv` if --with-history was used\n")

    print(f"Wrote: {args.output_dir}")
    print(f"Summary: {report_path}")


if __name__ == "__main__":
    main()
