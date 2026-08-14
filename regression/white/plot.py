#!/usr/bin/env python3
import os
import sys
import numpy as np
import matplotlib.pyplot as plt
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from regression.core.common import load_output, load_gold
from regression.core.plot import parity_plot


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
    parity_plot(exp_arr, gold_arr, test_arr, "white_swelling", "White – intergranular swelling (%)", outdir)

    # --------------------
    # Statistical analysis
    # --------------------
    print("\n" + "="*50)
    print("STATISTICAL ANALYSIS")
    print("="*50)

    error_test = test_arr - exp_arr
    error_gold = gold_arr - exp_arr

    # Experimental data
    print(f"Experimental data - mean:   {np.mean(exp_arr):.4f}")
    print(f"Experimental data - median: {np.median(exp_arr):.4f}")
    print(f"Experimental data - Q1:     {np.percentile(exp_arr, 25, method='midpoint'):.4f}")
    print(f"Experimental data - Q3:     {np.percentile(exp_arr, 75, method='midpoint'):.4f}")
    print("-" * 30)

    # Current results (test)
    print(f"Current SCIANTIX  - mean:   {np.mean(test_arr):.4f}")
    print(f"Current SCIANTIX  - median: {np.median(test_arr):.4f}")
    print(f"Current SCIANTIX  - Q1:     {np.percentile(test_arr, 25, method='midpoint'):.4f}")
    print(f"Current SCIANTIX  - Q3:     {np.percentile(test_arr, 75, method='midpoint'):.4f}")
    print(f"Current SCIANTIX  - BIAS:   {np.median(error_test):.4f}")
    print(f"Current SCIANTIX  - RMSE:   {np.sqrt(np.mean(error_test**2)):.4f}")
    print(f"Current SCIANTIX  - MAD:    {np.median(np.abs(error_test)):.4f}")
    print("-" * 30)

    # Gold results
    print(f"Gold (reference)  - mean:   {np.mean(gold_arr):.4f}")
    print(f"Gold (reference)  - median: {np.median(gold_arr):.4f}")
    print(f"Gold (reference)  - MAD:    {np.median(np.abs(error_gold)):.4f}")
    print(f"Gold (reference)  - RMSE:   {np.sqrt(np.mean(error_gold**2)):.4f}")
    print("="*50 + "\n")


if __name__ == "__main__":
    main()
