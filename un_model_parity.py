"""Parity check: un_model.py (reference) vs un_model_fast.py (vectorised).

Runs a small grid of (T, burnup, f_n, K_d, g_d_scale) through both
implementations and asserts that all numeric outputs agree to within
a relative tolerance of 1e-4 (defaults). Differences come from
np.linalg.solve (LAPACK LU) versus hand-rolled Cramer's rule in the
reference; they accumulate over thousands of timesteps but stay tiny.
"""

from __future__ import annotations

import math
import time

import un_model as base
import un_model_fast as fast

REL_TOL = 1.0e-4
ABS_TOL = 1.0e-9

# Small but representative grid. Add more rows to widen coverage.
GRID = [
    # (T, burnup, f_n, K_d, g_d_scale, n_modes, dt_h)
    (1200.0, 1.1, 1.0e-6, 5.0e5, 1.0, 25, 12.0),
    (1500.0, 1.3, 1.0e-7, 5.0e5, 1.0, 25, 12.0),
    (1600.0, 1.3, 1.0e-6, 5.0e5, 1.0, 25, 12.0),
    (1800.0, 1.3, 1.0e-6, 5.0e5, 1.0, 25, 12.0),
    (1600.0, 3.2, 1.0e-6, 5.0e5, 1.0, 25, 12.0),
    (2000.0, 1.3, 1.0e-7, 5.0e5, 2.0, 25, 12.0),
    # Tighter discretisation, slow but stricter parity test
    (1600.0, 1.3, 1.0e-6, 5.0e5, 1.0, 40, 1.0),
]

# Numeric output keys to compare. Skip "T", "burnup", and the boolean/string fields.
NUMERIC_KEYS = [
    "swelling_b_percent",
    "swelling_d_percent",
    "swelling_ig_percent",
    "Nb",
    "Nd",
    "Rb_nm",
    "Rd_nm",
    "p_b",
    "p_b_eq",
    "p_d",
    "p_d_eq",
    "p_b_over_eq",
    "p_d_over_eq",
    "matrix_gas_percent",
    "bulk_gas_percent",
    "dislocation_gas_percent",
    "qgb_gas_percent",
    "Dg",
    "Dv",
    "g_b",
    "g_d",
    "g_d_unscaled",
    "b_b_gas",
    "b_d_gas",
]


def relative_diff(a: float, b: float) -> float:
    if math.isnan(a) and math.isnan(b):
        return 0.0
    if math.isinf(a) and math.isinf(b) and (a * b > 0):
        return 0.0
    denom = max(abs(a), abs(b), ABS_TOL)
    return abs(a - b) / denom


def run_one(T, burnup, f_n, K_d, g_d_scale, n_modes, dt_h):
    base.clear_run_cache()
    fast.clear_run_cache()

    t0 = time.perf_counter()
    ref = base.run_model_point(
        T, burnup, f_n=f_n, K_d=K_d, g_d_scale=g_d_scale,
        dt_h=dt_h, n_modes=n_modes,
    )
    t_ref = time.perf_counter() - t0

    t0 = time.perf_counter()
    new = fast.run_model_point(
        T, burnup, f_n=f_n, K_d=K_d, g_d_scale=g_d_scale,
        dt_h=dt_h, n_modes=n_modes,
    )
    t_new = time.perf_counter() - t0

    diffs = [(k, ref[k], new[k], relative_diff(ref[k], new[k])) for k in NUMERIC_KEYS]
    max_field = max(diffs, key=lambda x: x[3])
    return ref, new, diffs, max_field, t_ref, t_new


def main():
    print(f"Parity check: un_model (reference) vs un_model_fast (vectorised)")
    print(f"Tolerance: rel <= {REL_TOL:.1e}, abs <= {ABS_TOL:.1e}")
    print()
    print(f"{'case':>4s} {'T':>6s} {'bu':>5s} {'f_n':>9s} {'n_m':>4s} {'dt_h':>5s} "
          f"{'max_rel':>10s} {'field':>20s} {'t_ref(s)':>10s} {'t_new(s)':>10s} {'speedup':>8s}")
    failures = 0
    total_ref = 0.0
    total_new = 0.0
    for i, args in enumerate(GRID):
        T, burnup, f_n, K_d, g_d_scale, n_modes, dt_h = args
        ref, new, diffs, max_field, t_ref, t_new = run_one(*args)
        max_rel = max_field[3]
        speedup = t_ref / t_new if t_new > 0 else float("inf")
        total_ref += t_ref
        total_new += t_new
        ok = "OK " if max_rel < REL_TOL else "FAIL"
        if max_rel >= REL_TOL:
            failures += 1
        print(f"{i:>4d} {T:>6.0f} {burnup:>5.1f} {f_n:>9.1e} {n_modes:>4d} {dt_h:>5.1f} "
              f"{max_rel:>10.2e} {max_field[0]:>20s} {t_ref:>10.3f} {t_new:>10.3f} {speedup:>7.2f}x  {ok}")
    print()
    print(f"Total: {len(GRID)} cases, {failures} failures, "
          f"sum t_ref={total_ref:.2f}s, sum t_new={total_new:.2f}s, "
          f"avg speedup={total_ref/total_new:.2f}x")
    return 0 if failures == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
