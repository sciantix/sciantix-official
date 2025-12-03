#!/usr/bin/env python3
import os
import sys
import numpy as np
import matplotlib.pyplot as plt
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from regression.core.common import load_output, load_gold


# ------------------------------------------------------------
# utils
# ------------------------------------------------------------
def extract_last(output, column):
    """Return the last timestep value of a given column."""
    header = [h.strip() for h in output.header]
    if column not in header:
        raise KeyError(f"Column '{column}' not found in output.header")
    idx = header.index(column)
    return output.data[-1, idx]


def load_experimental(basename):
    """
    Load experimental data from regression/gpr/data/ (T, value).
    """
    root = os.path.dirname(__file__)
    fpath = os.path.join(root, "data", basename)
    if not os.path.isfile(fpath):
        raise FileNotFoundError(f"Experimental data not found: {fpath}")
    arr = np.loadtxt(fpath)
    return arr[:, 0], arr[:, 1]


# ------------------------------------------------------------
# parity plot
# ------------------------------------------------------------
def parity_plot(exp, calc_gold, calc_test, calc_extra, quantity, title, outdir):
    plt.figure(figsize=(6, 5))

    # GOLD
    plt.scatter(exp, calc_gold,
                facecolors="none", edgecolors="brown",
                marker="^", s=40, label="gold")

    # TEST
    plt.scatter(exp, calc_test,
                facecolors="green", edgecolors="none",
                marker="o", s=35, alpha=0.8, label="test (w/ gpr)")

    # EXTRA DATASET (third dataset)
    if calc_extra is not None:
        plt.scatter(exp, calc_extra,
                    facecolors="blue", edgecolors="none",
                    marker="s", s=35, alpha=0.8, label="(w/o gpr)")

    # 1:1 lines
    xmin = 0.5 * min(exp.min(),
                     calc_gold.min(),
                     calc_test.min(),
                     calc_extra.min() if calc_extra is not None else np.inf)

    xmax = 2 * max(exp.max(),
                   calc_gold.max(),
                   calc_test.max(),
                   calc_extra.max() if calc_extra is not None else -np.inf)

    xline = np.logspace(np.log10(xmin), np.log10(xmax), 200)
    plt.plot(xline, xline, "-", color="#777777")
    plt.plot(xline, 2*xline, "--", color="#777777")
    plt.plot(xline, 0.5*xline, "--", color="#777777")

    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel("experimental")
    plt.ylabel("calculated")
    plt.title(title)

    plt.grid(True, which="both", ls=":")
    plt.legend()

    out = os.path.join(outdir, f"parity_{quantity}.png")
    plt.tight_layout()
    plt.savefig(out, dpi=180)
    plt.close()
    print("Saved:", out)


# ------------------------------------------------------------
# main
# ------------------------------------------------------------
def main():
    root = os.path.dirname(__file__)
    gpr_root = os.path.abspath(os.path.join(root, "..", "gpr"))
    outdir = os.path.join(root, "figures")
    os.makedirs(outdir, exist_ok=True)

    # columns in sciantix output
    col_swell   = "Intragranular gas bubble swelling (/)"
    col_radius  = "Intragranular bubble radius (m)"
    col_density = "Intragranular bubble concentration (bub/m3)"

    # load experimental
    T_swell, exp_swell     = load_experimental("ig_swelling.txt")
    T_swell_noGPR, exp_swell_noGPR = load_experimental("ig_swelling_no_gpr.txt")
    T_radius, exp_radius   = load_experimental("ig_radius.txt")
    T_density, exp_density = load_experimental("ig_density.txt")

    # arrays to fill
    exp_s, gold_s, test_s = [], [], []
    exp_r, gold_r, test_r = [], [], []
    exp_d, gold_d, test_d = [], [], []

    # loop over all gpr cases
    for name in sorted(os.listdir(gpr_root)):
        if not name.startswith("test_GPR"):
            continue

        case = os.path.join(gpr_root, name)
        if not os.path.isdir(case):
            continue

        T = float(name.split("_")[-1].replace("K", ""))

        # load
        out = load_output(case)
        gold = load_gold(case)

        # extract
        swell_test   = extract_last(out, col_swell)   * 100
        swell_gold   = extract_last(gold, col_swell)  * 100

        radius_test  = extract_last(out, col_radius)
        radius_gold  = extract_last(gold, col_radius)

        density_test = extract_last(out, col_density)
        density_gold = extract_last(gold, col_density)

        # match experimental based on temperature
        i_s = np.where(T_swell == T)[0]
        i_r = np.where(T_radius == T)[0]
        i_d = np.where(T_density == T)[0]

        if len(i_s) == 0:
            print(f"Missing experimental swelling for T={T}")
            continue

        # fill master arrays
        exp_s.append(exp_swell[i_s][0])
        gold_s.append(swell_gold)
        test_s.append(swell_test)

        exp_r.append(exp_radius[i_r][0])
        gold_r.append(radius_gold)
        test_r.append(radius_test)

        exp_d.append(exp_density[i_d][0])
        gold_d.append(density_gold)
        test_d.append(density_test)

    # convert to arrays
    exp_s, gold_s, test_s = map(np.array, (exp_s, gold_s, test_s))
    exp_r, gold_r, test_r = map(np.array, (exp_r, gold_r, test_r))
    exp_d, gold_d, test_d = map(np.array, (exp_d, gold_d, test_d))

    # EXTRAS
    # swelling third dataset (experimental no GPR)
    extra_s = []

    for T in T_swell:
        i_extra = np.where(T_swell_noGPR == T)[0]
        if len(i_extra) > 0:
            extra_s.append(exp_swell_noGPR[i_extra][0])
        else:
            extra_s.append(np.nan)   # or continue

    extra_s = np.array(extra_s)

    # for radius and density you can add similar logic if needed
    extra_r = None
    extra_d = None

    # PLOTS
    parity_plot(exp_s, gold_s, test_s, extra_s,
                "swelling", "Intra-granular gaseous swelling (%)", outdir)

    parity_plot(exp_r, gold_r, test_r, extra_r,
                "radius", "Intra-granular bubble radius (m)", outdir)

    parity_plot(exp_d, gold_d, test_d, extra_d,
                "density", "Intra-granular bubble density (bub/m3)", outdir)

if __name__ == "__main__":
    main()
