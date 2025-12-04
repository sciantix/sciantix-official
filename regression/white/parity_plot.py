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
    Load experimental data for White.
    Format: test_name   value
    """
    root = os.path.dirname(__file__)
    fpath = os.path.join(root, "data", basename)

    names = []
    values = []

    with open(fpath, "r") as f:
        for line in f:
            if not line.strip():
                continue
            parts = line.split()
            names.append(parts[0])
            values.append(float(parts[1]))

    return np.array(names), np.array(values)


# ------------------------------------------------------------
# parity plot
# ------------------------------------------------------------
def parity_plot(exp, calc_gold, calc_test, outdir):

    plt.figure(figsize=(6, 5))

    # scatter
    plt.scatter(exp, calc_gold,
                facecolors="none", edgecolors="brown",
                marker="^", s=40, label="gold")

    plt.scatter(exp, calc_test,
                facecolors="green", edgecolors="none",
                marker="o", s=35, alpha=0.8, label="test")

    # 1:1 lines (log scale)
    xmin = min(exp.min(), calc_gold.min(), calc_test.min())
    xmax = max(exp.max(), calc_gold.max(), calc_test.max())

    xmin = max(xmin, 1e-30)
    xmax = max(xmax, 1e-30)

    xmin *= 0.8
    xmax *= 1.2

    xline = np.logspace(np.log10(xmin), np.log10(xmax), 200)
    plt.plot(xline, xline, "-",  color="#777777")
    plt.plot(xline, 2*xline, "--", color="#777777")
    plt.plot(xline, 0.5*xline, "--", color="#777777")

    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel("experimental")
    plt.ylabel("calculated")
    plt.title("White – intergranular swelling (%)")
    plt.grid(True, which="both", ls=":")
    plt.legend()

    out = os.path.join(outdir, "parity_white_swelling.png")
    plt.tight_layout()
    plt.savefig(out, dpi=180)
    plt.close()
    print("Saved:", out)


# ------------------------------------------------------------
# main
# ------------------------------------------------------------
def main():

    root = os.path.dirname(__file__)
    white_root = os.path.abspath(os.path.join(root, "..", "white"))
    outdir = os.path.join(root, "figures")
    os.makedirs(outdir, exist_ok=True)

    col_swell = "Intergranular gas swelling (/)"

    # load experimental swelling
    exp_names, exp_values = load_experimental("ig_swelling.txt")

    exp_list, gold_list, test_list = [], [], []

    # loop over test directories
    for name in sorted(os.listdir(white_root)):
        if not name.startswith("test_White"):
            continue

        case = os.path.join(white_root, name)
        if not os.path.isdir(case):
            continue

        test_name = name  # matching by string

        # find experimental value
        idx = np.where(exp_names == test_name)[0]
        if len(idx) == 0:
            print(f"[WARNING] No experimental swelling for {test_name}")
            continue

        exp_val = exp_values[idx][0]

        # load sciantix outputs
        out = load_output(case)
        gold = load_gold(case)

        swell_test = extract_last(out, col_swell) * 100
        swell_gold = extract_last(gold, col_swell) * 100

        exp_list.append(exp_val)
        gold_list.append(swell_gold)
        test_list.append(swell_test)

    # convert to arrays
    exp_arr = np.array(exp_list)
    gold_arr = np.array(gold_list)
    test_arr = np.array(test_list)

    # plot
    parity_plot(exp_arr, gold_arr, test_arr, outdir)


if __name__ == "__main__":
    main()
