#!/usr/bin/env python3
"""
bias.py — parameter selection guided by sensitivity analysis on the White (2004)
intergranular-swelling validation set.

Sweeps one or more scaling factors over a grid (the Cartesian product of the
per-factor value lists), re-runs every ``test_White*`` case for every
combination, and reports how the parity statistics (BIAS / RMSE / MAD of
calculated vs experimental swelling) respond. This supports the selection of an
effective set of scaling-factor values.

It is a *deterministic grid sweep*, complementary to (not a replacement for) the
random-sampling sensitivity ranking in
``utilities/regression_scripts/globalSensitivityAnalysis.py``.

It produces a single parity figure in which the **gold** points stay fixed (the
reference ``output_gold.txt`` swelling) and each **test** point carries a vertical
bar spanning the range of calculated swelling produced across the whole sweep,
with the marker at the nominal point (all swept factors = 1.0). A BIAS/RMSE/MAD
table is printed per combination.

The White cases ship without an ``input_scaling_factors.txt`` (so every factor
defaults to 1.0). For each combination this script writes a temporary
``input_scaling_factors.txt`` into each case, runs SCIANTIX, reads the result,
and restores the case byte-for-byte afterwards (output.txt snapshot + removal of
the temporary scaling file and the stray run artifacts).

The BIAS / RMSE / MAD definitions match those printed by ``parity_plot.py`` so
the numbers are directly comparable.

Requires a built executable at ``build/sciantix.x`` (run ``./Allmake.sh`` first).

author: Giovanni Zullo
"""

import itertools
import os
import sys

import numpy as np
import matplotlib.pyplot as plt

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from regression.core.common import run_sciantix, load_output, load_gold

# ------------------------------------------------------------
# configuration
# ------------------------------------------------------------
# Factors to bias, and the values to test for each. The sweep runs the Cartesian
# product of these lists: one entry gives a 1-D sweep, two (or more) bias several
# factors at a time. Keep 1.0 in each list so the nominal point is included (it
# is used as the central marker of the parity figure).
FACTORS = {
    "diffusivity":     [0.5, 0.75, 1.0, 1.25, 1.5],
    "resolution rate": [0.5, 1.0, 2.0],
    "nucleation rate":   [0.2, 0.5, 1.0],
}
# NOTE: the number of runs is (product of grid sizes) x (number of cases), so
# keep the grids small when biasing several factors at once.

COL_SWELL = "Intergranular gas swelling (/)"   # output column, converted to %
EXP_FILE = "ig_swelling.txt"                   # experimental data in data/

# Full ordered list of scaling factors. Must match the layout written by
# utilities/inputExample/print_input_scaling_factors.py and read by
# src/file_manager/InputReading.C.
SF_NAMES = [
    "resolution rate",
    "trapping rate",
    "nucleation rate",
    "diffusivity",
    "temperature",
    "fission rate",
    "diffusion-based release",
    "helium production rate",
    "dummy",
]

SF_FILENAME = "input_scaling_factors.txt"


# ------------------------------------------------------------
# helpers
# ------------------------------------------------------------
def load_experimental(basename):
    """Load experimental swelling as {test_name: value}. Format: name<ws>value."""
    fpath = os.path.join(os.path.dirname(__file__), "data", basename)
    exp = {}
    with open(fpath, "r") as f:
        for line in f:
            if not line.strip():
                continue
            parts = line.split()
            exp[parts[0]] = float(parts[1])
    return exp


def write_scaling_factors(case_dir, overrides):
    """Write input_scaling_factors.txt with `overrides` applied, the rest at 1.0.

    overrides: dict {factor_name: value}.
    """
    with open(os.path.join(case_dir, SF_FILENAME), "w") as f:
        for name in SF_NAMES:
            f.write(f"{overrides.get(name, 1.0)}\n")
            f.write(f"# scaling factor - {name}\n")


def restore_case(case_dir, original_output):
    """Leave a case byte-for-byte as it was before the sweep.

    Restores output.txt from its snapshot (or removes it if it did not exist),
    deletes the temporary input_scaling_factors.txt, and removes the stray run
    artifacts SCIANTIX writes (execution.txt, overview.txt, input_check.txt).
    """
    out_path = os.path.join(case_dir, "output.txt")
    if original_output is None:
        if os.path.isfile(out_path):
            os.remove(out_path)
    else:
        with open(out_path, "wb") as f:
            f.write(original_output)

    for name in (SF_FILENAME, "execution.txt", "overview.txt", "input_check.txt"):
        path = os.path.join(case_dir, name)
        if os.path.isfile(path):
            os.remove(path)


def parity_stats(test_arr, exp_arr):
    """BIAS / RMSE / MAD of (test - exp), matching parity_plot.py definitions."""
    err = test_arr - exp_arr
    return {
        "BIAS": np.median(err),
        "RMSE": np.sqrt(np.mean(err ** 2)),
        "MAD": np.median(np.abs(err)),
    }


def combo_label(factor_names, combo):
    """Human-readable 'factor=value, factor=value' label for a sweep point."""
    return ", ".join(f"{n}={v}" for n, v in zip(factor_names, combo))


# ------------------------------------------------------------
# main
# ------------------------------------------------------------
def main():
    factor_names = list(FACTORS.keys())
    unknown = [f for f in factor_names if f not in SF_NAMES]
    if unknown:
        raise ValueError(f"Unknown scaling factor(s) {unknown}. Choose from: {SF_NAMES}")

    white_root = os.path.dirname(os.path.abspath(__file__))
    outdir = os.path.join(white_root, "figures")
    os.makedirs(outdir, exist_ok=True)

    exp = load_experimental(EXP_FILE)

    # Ordered list of (case_name, case_dir, exp_value) for cases with experimental data.
    cases = []
    for name in sorted(os.listdir(white_root)):
        case_dir = os.path.join(white_root, name)
        if not (name.startswith("test_White") and os.path.isdir(case_dir)):
            continue
        if name not in exp:
            print(f"[WARNING] No experimental swelling for {name}; skipping.")
            continue
        cases.append((name, case_dir, exp[name]))

    exp_arr = np.array([c[2] for c in cases])

    # Static reference: gold swelling from output_gold.txt (independent of the
    # scaling factors). These points stay fixed across the whole sweep.
    gold_arr = np.array([load_gold(c[1]).get_last(COL_SWELL) * 100.0 for c in cases])

    # Cartesian product of the per-factor grids; each combo is a tuple aligned
    # with factor_names.
    combos = list(itertools.product(*FACTORS.values()))
    sweep_label = " x ".join(factor_names)
    print(f"\nSweeping [{sweep_label}] over {len(combos)} combinations "
          f"on {len(cases)} White cases.\n")

    # Snapshot each case's original output.txt so the directories can be restored
    # exactly after the sweep (output.txt is a tracked file in the suite).
    snapshots = {}
    for name, case_dir, _ in cases:
        out_path = os.path.join(case_dir, "output.txt")
        snapshots[case_dir] = open(out_path, "rb").read() if os.path.isfile(out_path) else None

    results = []           # one dict per combination
    test_by_combo = {}     # combo -> array of calculated swelling (%) per case
    try:
        for combo in combos:
            overrides = dict(zip(factor_names, combo))
            test_vals = []
            for name, case_dir, _ in cases:
                write_scaling_factors(case_dir, overrides)
                run_sciantix(case_dir)
                out = load_output(case_dir)
                test_vals.append(out.get_last(COL_SWELL) * 100.0)

            test_arr = np.array(test_vals)
            test_by_combo[combo] = test_arr
            stats = parity_stats(test_arr, exp_arr)
            stats["combo"] = combo
            results.append(stats)
            print(f"{combo_label(factor_names, combo):<48} | "
                  f"BIAS = {stats['BIAS']:+.4f} | "
                  f"RMSE = {stats['RMSE']:.4f} | "
                  f"MAD = {stats['MAD']:.4f}")
    finally:
        # Always leave the cases pristine, even on interruption/error.
        for _, case_dir, _ in cases:
            restore_case(case_dir, snapshots[case_dir])

    # ----------------------------------------------------------------
    # summary
    # ----------------------------------------------------------------
    bias = np.array([r["BIAS"] for r in results])
    rmse = np.array([r["RMSE"] for r in results])

    best_bias = results[int(np.argmin(np.abs(bias)))]
    best_rmse = results[int(np.argmin(rmse))]

    print("\n" + "=" * 64)
    print("PARAMETER SELECTION SUMMARY")
    print("=" * 64)
    print(f"Smallest |BIAS|: {combo_label(factor_names, best_bias['combo'])} "
          f"(BIAS = {best_bias['BIAS']:+.4f}%)")
    print(f"Smallest RMSE:   {combo_label(factor_names, best_rmse['combo'])} "
          f"(RMSE = {best_rmse['RMSE']:.4f}%)")
    print("=" * 64 + "\n")

    # ----------------------------------------------------------------
    # single parity figure: gold points static, test points shown as the
    # range (vertical bars) spanned by the whole multi-factor sweep.
    # ----------------------------------------------------------------
    test_matrix = np.array([test_by_combo[c] for c in combos])  # (n_combos, n_cases)
    low = test_matrix.min(axis=0)
    high = test_matrix.max(axis=0)
    # central marker = nominal point (all swept factors = 1.0) if present,
    # otherwise the per-case median across the sweep
    nominal = tuple(1.0 for _ in factor_names)
    center = test_by_combo[nominal] if nominal in test_by_combo else np.median(test_matrix, axis=0)

    plt.figure(figsize=(6, 5))

    # gold (static reference)
    plt.scatter(exp_arr, gold_arr, facecolors="none", edgecolors="brown",
                marker="^", s=40, label="gold", zorder=3)

    # test: vertical bar over the swept range, marker at the nominal point
    plt.errorbar(exp_arr, center,
                 yerr=[center - low, high - center],
                 fmt="o", color="green", ecolor="green",
                 elinewidth=1.2, capsize=3, markersize=5, alpha=0.85,
                 label=f"test ({sweep_label} sweep)", zorder=2)

    # 1:1 and factor-2 guide lines
    finite = np.concatenate([exp_arr, gold_arr, low, high])
    finite = finite[np.isfinite(finite) & (finite > 0)]
    xmin, xmax = 0.5 * finite.min(), 2.0 * finite.max()
    xline = np.logspace(np.log10(xmin), np.log10(xmax), 200)
    plt.plot(xline, xline, "-", color="#777777")
    plt.plot(xline, 2 * xline, "--", color="#777777")
    plt.plot(xline, 0.5 * xline, "--", color="#777777")

    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel("experimental")
    plt.ylabel("calculated")
    plt.title(f"White – intergranular swelling (%), {sweep_label} sweep")
    plt.grid(True, which="both", ls=":")
    plt.legend()
    plt.tight_layout()

    tag = "_".join(f.replace(" ", "-") for f in factor_names)
    figpath = os.path.join(outdir, f"parity_white_swelling_{tag}_bars.png")
    plt.savefig(figpath, dpi=180)
    plt.close()
    print(f"Figure saved to {figpath}\n")


if __name__ == "__main__":
    main()
