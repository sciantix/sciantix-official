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
    Load experimental data from regression/baker/data/ (T, value).
    """
    root = os.path.dirname(__file__)
    fpath = os.path.join(root, "data", basename)
    if not os.path.isfile(fpath):
        raise FileNotFoundError(f"Experimental data not found: {fpath}")
    arr = np.loadtxt(fpath)
    return arr[:, 0], arr[:, 1]


# ------------------------------------------------------------
# main
# ------------------------------------------------------------
def main():
    root = os.path.dirname(__file__)
    baker_root = os.path.abspath(os.path.join(root, "..", "baker"))
    outdir = os.path.join(root, "figures")
    os.makedirs(outdir, exist_ok=True)

    # columns in sciantix output
    col_swell   = "Intragranular gas bubble swelling (/)"
    col_radius  = "Intragranular bubble radius (m)"
    col_density = "Intragranular bubble concentration (bub/m3)"

    # load experimental
    T_swell, exp_swell     = load_experimental("ig_swelling.txt")
    T_radius, exp_radius   = load_experimental("ig_radius.txt")
    T_density, exp_density = load_experimental("ig_density.txt")

    # arrays to fill
    exp_s, gold_s, test_s = [], [], []
    exp_r, gold_r, test_r = [], [], []
    exp_d, gold_d, test_d = [], [], []

    # loop over all Baker cases
    for name in sorted(os.listdir(baker_root)):
        if not name.startswith("test_Baker"):
            continue

        case = os.path.join(baker_root, name)
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

    # one plot per quantity
    parity_plot(exp_s, gold_s, test_s, "swelling", "Intra-granular gaseous swelling (%)", outdir)
    parity_plot(exp_r, gold_r, test_r, "radius", "Intra-granular bubble radius (m)", outdir)
    parity_plot(exp_d, gold_d, test_d, "density", "Intra-granular bubble density (bub/m3)", outdir)


if __name__ == "__main__":
    main()
