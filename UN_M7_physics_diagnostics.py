#!/usr/bin/env python3
"""
UN_M7_physics_diagnostics.py

Diagnostic-only script for the UN/M7 reduced model.

Purpose
-------
Run ONE representative calibrated candidate (default: M7_no_phi, Dv_scale=0.5)
and check whether the remaining problems are likely due to:
  1) pressure/free-volume formulation,
  2) initial bubble/vacancy initialization,
  3) gas drainage to the grain face q_gb,
  4) numerical resolution dt/n_modes.

It does NOT modify equations and does NOT run Optuna.
It imports UN_M7_optuna_calibration.py from the current folder.

Outputs are written to UN_M7_physics_diagnostics/ by default:
  - diagnostic_summary.md
  - candidate_parameters.csv
  - point_diagnostics.csv
  - history_diagnostics.csv
  - initialization_sensitivity.csv
  - numerical_convergence_check.csv

Usage
-----
cd ~/sciantix-official
source .venv/bin/activate
python UN_M7_physics_diagnostics.py

Optional examples:
python UN_M7_physics_diagnostics.py --from-csv UN_M7_optuna_v2_results/M7_no_phi/dv0.5_pW0.6_pF3_prior0.2_bulk0.05/optuna_final_top_M7_no_phi.csv --row 0
python UN_M7_physics_diagnostics.py --no-history
"""

from __future__ import annotations

import argparse
import csv
import math
import os
from dataclasses import asdict
from typing import Dict, List, Optional, Tuple

import UN_M7_optuna_calibration as m

# Candidate taken from PC1 M7_no_phi Dv=0.5 final rerun, best final row.
DEFAULT_CANDIDATE = dict(
    label="diagnostic_M7_no_phi_Dv05_trial_00097",
    f_n=1.1149e-08,                 # rounded from CSV; enough for diagnostics
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

TEMPERATURES = [1200.0, 1400.0, 1600.0, 1700.0, 1800.0, 2000.0, 2200.0]
BURNUPS = [1.1, 3.2]

# Seed radii for sensitivity of initial dislocation bubble volume/vacancies.
# Default model has R_d0=0 and nvd0 from default initialization (=0).
INIT_SCENARIOS_SAFE = [
    {"name": "default_Rd0_0", "R_d_nm": 0.0, "nvd0": None},
    {"name": "seed_Rd0_0p5nm", "R_d_nm": 0.5, "nvd0": None},
]

# Larger initial dislocation-bubble radii can make some high-burnup cases very slow or unstable.
# Use only with --wide-init if you explicitly want to stress-test initialization.
INIT_SCENARIOS_WIDE = INIT_SCENARIOS_SAFE + [
    {"name": "seed_Rd0_1nm", "R_d_nm": 1.0, "nvd0": None},
    {"name": "seed_Rd0_2nm", "R_d_nm": 2.0, "nvd0": None},
]


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


def apply_family(family: str):
    m.set_model_family(family)


def make_params(
    cand: m.Candidate,
    T: float,
    burnup: float,
    dt_h: float,
    n_modes: int,
    init: Optional[Dict] = None,
) -> m.UNParameters:
    init = init or INIT_SCENARIOS_SAFE[0]
    p = m.UNParameters(
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
    # Initial dislocation bubble seed radius diagnostic.
    # This tests whether zero-volume/zero-vacancy initial sites are causing numerical artifacts.
    R_d_nm = float(init.get("R_d_nm", 0.0) or 0.0)
    if R_d_nm > 0.0:
        p.R_d = R_d_nm * 1.0e-9
        # Leave nvd0=None so the base initialization fills the seed volume with vacancies.
        # If nvd0 is explicitly supplied, use it instead.
    if init.get("nvd0", None) is not None:
        p.nvd0 = float(init["nvd0"])
    return p


def final_row_from_hist(hist: Dict, rates: Dict, p: m.UNParameters, cand: m.Candidate, scenario: str, T: float, burnup: float, dt_h: float, n_modes: int) -> Dict:
    i = -1
    return diagnostic_row_from_index(hist, rates, p, cand, scenario, T, burnup, dt_h, n_modes, i)


def diagnostic_row_from_index(hist: Dict, rates: Dict, p: m.UNParameters, cand: m.Candidate, scenario: str, T: float, burnup: float, dt_h: float, n_modes: int, i: int) -> Dict:
    def h(key, default=math.nan):
        arr = hist.get(key, [])
        if not arr:
            return default
        try:
            return arr[i]
        except Exception:
            return default

    c = h("c", 0.0)
    mb = h("mb", 0.0)
    md = h("md", 0.0)
    Nb = h("Nb", 0.0)
    Nd = h("Nd", 0.0)
    Vb = h("Vb", 0.0)
    Vd = h("Vd", 0.0)
    nvb = h("nvb", 0.0)
    nvd = h("nvd", 0.0)
    generated = h("generated", 0.0)
    q_gb = h("q_gb", 0.0)
    p_b = h("p_b", 0.0)
    p_d = h("p_d", 0.0)
    p_b_eq = h("p_b_eq", math.nan)
    p_d_eq = h("p_d_eq", math.nan)

    def percent(x):
        return 100.0 * x / generated if generated > 0.0 else 0.0

    def p_alt_total(mgas, N, V):
        # Ideal gas pressure using total bubble volume, not only vacancy/free volume.
        # p = kT * (gas atoms per bubble) / V = kT * mgas / (N V)
        if mgas <= 0.0 or N <= 0.0 or V <= 0.0:
            return 0.0
        return p.kB_J * p.temperature * mgas / (N * V)

    p_b_alt = p_alt_total(mb, Nb, Vb)
    p_d_alt = p_alt_total(md, Nd, Vd)

    def gas_volume_fraction(mgas, N, V):
        if mgas <= 0.0 or N <= 0.0 or V <= 0.0:
            return 0.0
        return p.omega_fg * mgas / (N * V)

    gb_vfrac = gas_volume_fraction(mb, Nb, Vb)
    gd_vfrac = gas_volume_fraction(md, Nd, Vd)

    retained = c + mb + md
    balance_sum_pct = percent(c) + percent(mb) + percent(md) + percent(q_gb)
    balance_abs_error = (c + mb + md + q_gb) - generated

    return {
        "scenario": scenario,
        "candidate_label": cand.label,
        "family": m.MODEL_FAMILY,
        "T_K": T,
        "burnup_FIMA_percent": burnup,
        "dt_h": dt_h,
        "n_modes": n_modes,
        "time_s": h("time", 0.0),
        "burnup_from_time_percent": h("burnup_percent_fima", math.nan),
        "generated": generated,
        "retained": retained,
        "q_gb": q_gb,
        "gas_balance_abs_error": balance_abs_error,
        "gas_balance_rel_error": balance_abs_error / generated if generated > 0.0 else math.nan,
        "balance_sum_percent": balance_sum_pct,
        "matrix_percent": percent(c),
        "bulk_percent": percent(mb),
        "dislocation_percent": percent(md),
        "qgb_percent": percent(q_gb),
        "intragranular_bulk_fraction_no_qgb": mb / (c + mb + md) if (c + mb + md) > 0.0 else math.nan,
        "intragranular_dislocation_fraction_no_qgb": md / (c + mb + md) if (c + mb + md) > 0.0 else math.nan,
        "swelling_b_percent": 100.0 * h("swelling_b", 0.0),
        "swelling_d_percent": 100.0 * h("swelling_d", 0.0),
        "swelling_ig_percent": 100.0 * h("swelling_ig", 0.0),
        "Nb": Nb,
        "Nd": Nd,
        "Rb_nm": h("Rb", 0.0) * 1.0e9,
        "Rd_nm": h("Rd", 0.0) * 1.0e9,
        "Vb": Vb,
        "Vd": Vd,
        "nvb": nvb,
        "nvd": nvd,
        "p_b": p_b,
        "p_b_eq": p_b_eq,
        "p_b_over_eq": p_b / p_b_eq if p_b_eq > 0.0 else math.nan,
        "p_b_alt_total_volume": p_b_alt,
        "p_b_alt_total_over_eq": p_b_alt / p_b_eq if p_b_eq > 0.0 else math.nan,
        "p_b_current_over_alt_total": p_b / p_b_alt if p_b_alt > 0.0 else math.nan,
        "p_d": p_d,
        "p_d_eq": p_d_eq,
        "p_d_over_eq": p_d / p_d_eq if p_d_eq > 0.0 else math.nan,
        "p_d_alt_total_volume": p_d_alt,
        "p_d_alt_total_over_eq": p_d_alt / p_d_eq if p_d_eq > 0.0 else math.nan,
        "p_d_current_over_alt_total": p_d / p_d_alt if p_d_alt > 0.0 else math.nan,
        "bulk_gas_volume_fraction_omega_fg": gb_vfrac,
        "dislocation_gas_volume_fraction_omega_fg": gd_vfrac,
        "lambda_d": h("lambda_d", math.nan),
        "nu_b": h("nu_b", math.nan),
        "phi_b": h("phi_b", math.nan),
        "phi_d": h("phi_d", math.nan),
        "f_cap_step": h("f_cap_step", math.nan),
        "max_f_cap_step": h("max_f_cap_step", math.nan),
        "capture_fraction_sum": h("capture_fraction_sum", math.nan),
        "capture_raw_sum": h("capture_raw_sum", math.nan),
        "Dg_final": rates.get("Dg", math.nan),
        "Dv_final": rates.get("Dv", math.nan),
        "g_b_final": rates.get("g_b", math.nan),
        "g_d_final": rates.get("g_d", math.nan),
        "b_b_final": rates.get("b_b", math.nan),
        "b_d_final": rates.get("b_d", math.nan),
        "dnvb_dt_final": rates.get("dnvb_dt", math.nan),
        "dnvd_dt_final": rates.get("dnvd_dt", math.nan),
        "term_dislocation_final": rates.get("term_dislocation", math.nan),
        "term_bubbles_final": rates.get("term_bubbles", math.nan),
        "free_dislocation_final": rates.get("free_dislocation", math.nan),
    }


def run_case(cand: m.Candidate, scenario: Dict, T: float, burnup: float, dt_h: float, n_modes: int, keep_history: bool) -> Tuple[Dict, Dict, m.UNParameters]:
    p = make_params(cand, T, burnup, dt_h, n_modes, scenario)
    hist, rates = m.solve_UN_M7(p, keep_history=keep_history)
    return hist, rates, p


def write_csv(path: str, rows: List[Dict]):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    if not rows:
        with open(path, "w", encoding="utf-8") as f:
            f.write("")
        return
    keys = []
    seen = set()
    for row in rows:
        for k in row.keys():
            if k not in seen:
                seen.add(k)
                keys.append(k)
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=keys)
        w.writeheader()
        for row in rows:
            w.writerow(row)


def summarize_history(hist: Dict, rates: Dict, p: m.UNParameters, cand: m.Candidate, scenario: str, T: float, burnup: float, dt_h: float, n_modes: int) -> Dict:
    rows = []
    n = len(hist.get("time", []))
    if n == 0:
        return {}
    # Key instants: initial, early, mid, final, max p_d ratio, max qgb.
    idxs = {0, min(1, n - 1), min(5, n - 1), min(20, n - 1), n - 1}
    pdratios = []
    qgbpcts = []
    for i in range(n):
        pd = hist.get("p_d", [math.nan] * n)[i]
        peq = hist.get("p_d_eq", [math.nan] * n)[i]
        pdratios.append(pd / peq if peq and peq > 0 else math.nan)
        gen = hist.get("generated", [0.0] * n)[i]
        q = hist.get("q_gb", [0.0] * n)[i]
        qgbpcts.append(100.0 * q / gen if gen > 0 else 0.0)
    finite_pd = [(i, v) for i, v in enumerate(pdratios) if math.isfinite(v)]
    if finite_pd:
        idxs.add(max(finite_pd, key=lambda x: x[1])[0])
    idxs.add(max(range(n), key=lambda i: qgbpcts[i]))

    sampled = []
    for i in sorted(idxs):
        r = diagnostic_row_from_index(hist, rates, p, cand, scenario, T, burnup, dt_h, n_modes, i)
        r["history_index"] = i
        sampled.append(r)

    # Compact summary row for history behavior.
    def first_cross(values, threshold):
        for i, v in enumerate(values):
            if math.isfinite(v) and v >= threshold:
                return hist["burnup_percent_fima"][i], hist["time"][i]
        return math.nan, math.nan

    q10_bu, q10_time = first_cross(qgbpcts, 10.0)
    q20_bu, q20_time = first_cross(qgbpcts, 20.0)
    pd10_bu, pd10_time = first_cross(pdratios, 10.0)
    return {
        "sampled_rows": sampled,
        "summary": {
            "scenario": scenario,
            "T_K": T,
            "burnup_FIMA_percent": burnup,
            "dt_h": dt_h,
            "n_modes": n_modes,
            "max_p_d_over_eq": max([v for v in pdratios if math.isfinite(v)], default=math.nan),
            "final_p_d_over_eq": pdratios[-1],
            "max_qgb_percent": max(qgbpcts) if qgbpcts else math.nan,
            "final_qgb_percent": qgbpcts[-1] if qgbpcts else math.nan,
            "first_qgb_ge_10pct_burnup": q10_bu,
            "first_qgb_ge_20pct_burnup": q20_bu,
            "first_pd_ratio_ge_10_burnup": pd10_bu,
        },
    }


def relative_change(a, b):
    if not math.isfinite(a) or not math.isfinite(b):
        return math.nan
    scale = max(abs(a), abs(b), 1.0e-300)
    return abs(a - b) / scale


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--family", default="M7_no_phi", choices=["M7_full", "M7_no_phi", "capture_only", "baseline"])
    parser.add_argument("--output-dir", default="UN_M7_physics_diagnostics")
    parser.add_argument("--from-csv", default=None, help="Optional final_top CSV; row 0 by default.")
    parser.add_argument("--row", type=int, default=0)
    parser.add_argument("--full", action="store_true", help="Run full temperature grid and all initialization points; slower.")
    parser.add_argument("--with-history", action="store_true", help="Run keep_history diagnostics; slower.")
    parser.add_argument("--with-convergence", action="store_true", help="Run dt/modes convergence check; slower.")
    parser.add_argument("--with-init", action="store_true", help="Run limited initialization sensitivity; moderately slower.")
    parser.add_argument("--wide-init", action="store_true", help="Also test 1 and 2 nm seed radii; can be slow/unstable.")
    args = parser.parse_args()

    apply_family(args.family)
    outdir = args.output_dir
    os.makedirs(outdir, exist_ok=True)

    cand = candidate_from_csv(args.from_csv, args.row) if args.from_csv else default_candidate()

    write_csv(os.path.join(outdir, "candidate_parameters.csv"), [asdict(cand)])

    # Default is intentionally compact. Use --full / --with-history / --with-convergence for deeper checks.
    temps = TEMPERATURES if args.full else [1600.0, 1800.0, 2200.0]
    burnups = [1.1, 3.2]

    point_rows = []
    init_rows = []
    history_sample_rows = []
    history_summary_rows = []
    conv_rows = []

    # Base point diagnostics: default initialization only, production-quality final numerics.
    for bu in burnups:
        for T in temps:
            hist, rates, p = run_case(cand, INIT_SCENARIOS_SAFE[0], T, bu, dt_h=1.0, n_modes=40, keep_history=False)
            point_rows.append(final_row_from_hist(hist, rates, p, cand, INIT_SCENARIOS_SAFE[0]["name"], T, bu, 1.0, 40))

    # Optional initialization sensitivity at key conditions.
    # Default script does not run this because some large seed radii can be very slow at high burnup.
    if args.with_init:
        init_scenarios = INIT_SCENARIOS_WIDE if args.wide_init else INIT_SCENARIOS_SAFE
        init_Ts = [1600.0, 2200.0] if not args.full else [1400.0, 1600.0, 1800.0, 2200.0]
        init_bus = [1.1] if not args.full else burnups
        for bu in init_bus:
            for T in init_Ts:
                for init in init_scenarios:
                    hist, rates, p = run_case(cand, init, T, bu, dt_h=1.0, n_modes=40, keep_history=False)
                    init_rows.append(final_row_from_hist(hist, rates, p, cand, init["name"], T, bu, 1.0, 40))

    # History diagnostics for selected points.
    if args.with_history:
        for bu, T in [(1.1, 1600.0), (1.1, 1800.0), (3.2, 1600.0), (3.2, 2200.0)]:
            hist, rates, p = run_case(cand, INIT_SCENARIOS_SAFE[0], T, bu, dt_h=1.0, n_modes=40, keep_history=True)
            hsum = summarize_history(hist, rates, p, cand, INIT_SCENARIOS_SAFE[0]["name"], T, bu, 1.0, 40)
            if hsum:
                history_sample_rows.extend(hsum["sampled_rows"])
                history_summary_rows.append(hsum["summary"])

    # Optional numerical convergence: compare 1h/40 vs 0.5h/80.
    if args.with_convergence:
        compare_keys = [
            "swelling_b_percent", "swelling_d_percent", "Rd_nm", "Nd",
            "bulk_percent", "dislocation_percent", "qgb_percent", "p_d_over_eq",
            "p_d_current_over_alt_total", "capture_fraction_sum", "max_f_cap_step",
        ]
        for bu in burnups:
            for T in [1600.0, 1800.0, 2200.0]:
                hist1, rates1, p1 = run_case(cand, INIT_SCENARIOS_SAFE[0], T, bu, dt_h=1.0, n_modes=40, keep_history=False)
                row1 = final_row_from_hist(hist1, rates1, p1, cand, "dt1h_modes40", T, bu, 1.0, 40)
                hist2, rates2, p2 = run_case(cand, INIT_SCENARIOS_SAFE[0], T, bu, dt_h=0.5, n_modes=80, keep_history=False)
                row2 = final_row_from_hist(hist2, rates2, p2, cand, "dt0p5h_modes80", T, bu, 0.5, 80)
                cr = {"T_K": T, "burnup_FIMA_percent": bu}
                for k in compare_keys:
                    cr[f"{k}_1h40"] = row1.get(k, math.nan)
                    cr[f"{k}_0p5h80"] = row2.get(k, math.nan)
                    cr[f"{k}_rel_change"] = relative_change(row1.get(k, math.nan), row2.get(k, math.nan))
                conv_rows.append(cr)

    write_csv(os.path.join(outdir, "point_diagnostics.csv"), point_rows)
    write_csv(os.path.join(outdir, "initialization_sensitivity.csv"), init_rows)
    write_csv(os.path.join(outdir, "history_sample_diagnostics.csv"), history_sample_rows)
    write_csv(os.path.join(outdir, "history_diagnostics.csv"), history_summary_rows)
    write_csv(os.path.join(outdir, "numerical_convergence_check.csv"), conv_rows)

    # Simple report with automatic flags.
    flags = []
    for r in point_rows:
        if r["p_d_over_eq"] > 10:
            flags.append(f"High p_d/p_eq={r['p_d_over_eq']:.2g} at T={r['T_K']:.0f} K, burnup={r['burnup_FIMA_percent']}%.")
        if r["qgb_percent"] > 15 and r["T_K"] <= 1700:
            flags.append(f"High early q_gb={r['qgb_percent']:.1f}% at T={r['T_K']:.0f} K, burnup={r['burnup_FIMA_percent']}%.")
        if r["p_d_current_over_alt_total"] > 5:
            flags.append(f"Pressure formula sensitivity: p_d_current/p_d_alt_total={r['p_d_current_over_alt_total']:.1f} at T={r['T_K']:.0f} K, burnup={r['burnup_FIMA_percent']}%.")
    for r in conv_rows:
        worst = max([v for k, v in r.items() if k.endswith("_rel_change") and math.isfinite(v)], default=0.0)
        if worst > 0.10:
            flags.append(f"Numerical sensitivity >10% at T={r['T_K']:.0f} K, burnup={r['burnup_FIMA_percent']}%: max rel change {worst:.2g}.")

    report = os.path.join(outdir, "diagnostic_summary.md")
    with open(report, "w", encoding="utf-8") as f:
        f.write("# UN M7 physics diagnostics\n\n")
        f.write("## Candidate\n\n")
        for k, v in asdict(cand).items():
            f.write(f"- `{k}` = {v}\n")
        f.write(f"\nFamily: `{m.MODEL_FAMILY}`\n\n")
        f.write("## What this script checks\n\n")
        f.write("- gas conservation / partition including q_gb;\n")
        f.write("- current pressure formula vs ideal pressure using total bubble volume;\n")
        f.write("- sensitivity to initial dislocation-bubble seed radius, if --with-init is used;\n")
        f.write("- numerical convergence from dt=1 h, modes=40 to dt=0.5 h, modes=80, if --with-convergence is used.\n\n")
        f.write("## Automatic flags\n\n")
        if flags:
            for fl in flags[:80]:
                f.write(f"- {fl}\n")
            if len(flags) > 80:
                f.write(f"- ... {len(flags)-80} additional flags omitted.\n")
        else:
            f.write("No automatic flags triggered with current thresholds.\n")
        f.write("\n## Output files\n\n")
        for name in [
            "candidate_parameters.csv",
            "point_diagnostics.csv",
            "initialization_sensitivity.csv",
            "history_sample_diagnostics.csv",
            "history_diagnostics.csv",
            "numerical_convergence_check.csv",
        ]:
            f.write(f"- `{name}`\n")

    print(f"Wrote diagnostics to: {outdir}")
    print(f"Summary: {report}")
    if flags:
        print("Top flags:")
        for fl in flags[:10]:
            print(" -", fl)


if __name__ == "__main__":
    main()
