"""joblib-parallel sweep driver — demonstrates change (1) on the vectorised model.

Sweeps a (T x burnup x f_n) grid through `un_model_fast.run_model_point`,
first sequentially, then with `joblib.Parallel(n_jobs=-1)`. Prints both
timings and the speedup. Drop-in pattern that can replace any of the
single-cell Cartesian-product sweeps in the notebooks.

Run:
    python run_sweep_parallel.py            # default 24-point grid
    python run_sweep_parallel.py --n-jobs 4 # cap workers
"""

from __future__ import annotations

# --- path setup (added by tools/fix_paths.py after un_calibration/ reorg) ---
import sys as _sys
from pathlib import Path as _Path
_sys.path.insert(0, str(_Path(__file__).resolve().parent.parent))
import _pathsetup  # noqa: F401
# --- end path setup ---


import argparse
import time
from itertools import product

from joblib import Parallel, delayed

import un_model_fast as fast


def evaluate(T, burnup, f_n, *, dt_h, n_modes):
    fast.clear_run_cache()  # avoid hot-cache hits dominating timings
    return fast.run_model_point(T, burnup, f_n=f_n, dt_h=dt_h, n_modes=n_modes)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--n-jobs", type=int, default=-1,
                        help="Number of joblib workers (default -1 = all cores)")
    parser.add_argument("--n-modes", type=int, default=25)
    parser.add_argument("--dt-h", type=float, default=12.0)
    args = parser.parse_args()

    Ts = [float(T) for T in range(900, 2001, 100)]   # 12 T points
    burnups = [1.1, 1.3, 3.2]
    f_ns = [1.0e-8, 1.0e-7, 1.0e-6, 1.0e-5]
    grid = list(product(Ts, burnups, f_ns))           # 12 * 3 * 4 = 144 points

    print(f"Sweep: {len(grid)} points = {len(Ts)} T x {len(burnups)} bu x {len(f_ns)} f_n")
    print(f"Numerics: dt_h={args.dt_h}, n_modes={args.n_modes}")

    print("\n[1/2] sequential...")
    t0 = time.perf_counter()
    seq = [evaluate(T, bu, f_n, dt_h=args.dt_h, n_modes=args.n_modes)
           for (T, bu, f_n) in grid]
    t_seq = time.perf_counter() - t0
    print(f"  done in {t_seq:.2f}s")

    print(f"\n[2/2] joblib.Parallel(n_jobs={args.n_jobs})...")
    t0 = time.perf_counter()
    par = Parallel(n_jobs=args.n_jobs, backend="loky")(
        delayed(evaluate)(T, bu, f_n, dt_h=args.dt_h, n_modes=args.n_modes)
        for (T, bu, f_n) in grid
    )
    t_par = time.perf_counter() - t0
    print(f"  done in {t_par:.2f}s")

    speedup = t_seq / t_par if t_par > 0 else float("inf")
    print(f"\nspeedup vs sequential: {speedup:.2f}x")

    # Quick consistency check: top-level outputs should match across runs.
    mismatches = 0
    for s, p in zip(seq, par):
        for k in ("swelling_d_percent", "Rd_nm", "Nd"):
            if abs(s[k] - p[k]) > 1.0e-9 * max(abs(s[k]), abs(p[k]), 1.0):
                mismatches += 1
    print(f"sequential vs parallel mismatches across key outputs: {mismatches}")

    print("\nfirst 3 rows (T, burnup, f_n -> swelling_d_percent, Rd_nm, Nd):")
    for (T, bu, f_n), row in list(zip(grid, par))[:3]:
        print(f"  T={T:>6.0f} bu={bu:>4.1f} f_n={f_n:.1e} -> "
              f"swD={row['swelling_d_percent']:.3f}% "
              f"Rd={row['Rd_nm']:.1f}nm "
              f"Nd={row['Nd']:.2e}")


if __name__ == "__main__":
    main()
