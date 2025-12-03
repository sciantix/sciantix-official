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


# ------------------------------------------------------------
# load experimental data
# ------------------------------------------------------------
def load_experimental(basename):
    """
    Load experimental data stored inside regression/baker/data/.
    Files contain two columns: temperature(K), value.
    """

    root = os.path.dirname(__file__)
    fpath = os.path.join(root, "data", basename)

    if not os.path.isfile(fpath):
        raise FileNotFoundError(f"Experimental data file not found: {fpath}")

    arr = np.loadtxt(fpath)
    temperature = arr[:, 0]
    value = arr[:, 1]
    return temperature, value


# ------------------------------------------------------------
# parity plot function
# ------------------------------------------------------------
def parity_plot(exp, calc_gold, calc_test, label, outdir):
    """Generate one parity plot."""

    xmin = ymin = 1e-3
    xmax = ymax = 1e2

    plt.figure(figsize=(6, 5))
    plt.scatter(exp, calc_gold, facecolors='none', edgecolors='brown',
                marker='^', s=40, label="gold")
    plt.scatter(exp, calc_test, facecolors='green', edgecolors='none',
                marker='o', s=35, alpha=0.8, label="test")

    # 1:1 diagonal + factor bands
    xline = np.logspace(-3, 2, 200)
    plt.plot(xline, xline, '-', color='#777777')
    plt.plot(xline, 2 * xline, '--', color='#777777')
    plt.plot(xline, 0.5 * xline, '--', color='#777777')

    plt.xscale("log")
    plt.yscale("log")
    plt.xlim(xmin, xmax)
    plt.ylim(ymin, ymax)
    plt.xlabel("experimental")
    plt.ylabel("calculated")
    plt.title(label)
    plt.legend()
    plt.grid(True, which="both", ls=":")

    outfile = os.path.join(outdir, f"parity_{label}.png")
    plt.tight_layout()
    plt.savefig(outfile, dpi=180)
    plt.close()
    print(f"Saved: {outfile}")


# ------------------------------------------------------------
# main
# ------------------------------------------------------------
def main():

    # where tests live
    root = os.path.dirname(__file__)
    baker_root = os.path.join(root, "..", "baker")
    baker_root = os.path.abspath(baker_root)

    # where to save plots
    outdir = os.path.join(root, "figures")
    os.makedirs(outdir, exist_ok=True)

    # which columns we want
    col_swell = "Intragranular gas bubble swelling (/)"
    col_radius = "Intragranular bubble radius (m)"
    col_density = "Intragranular bubble concentration (bub/m3)"

    # experimental data
    exp_T_swell, exp_swell = load_experimental("ig_swelling.txt")
    exp_T_radius, exp_radius = load_experimental("ig_radius.txt")
    exp_T_density, exp_density = load_experimental("ig_density.txt")

    # loop over all Baker test folders
    for name in sorted(os.listdir(baker_root)):
        if not name.startswith("test_Baker"):
            continue

        case = os.path.join(baker_root, name)
        if not os.path.isdir(case):
            continue

        print(f"Processing {name}")

        # load sciantix results
        out = load_output(case)
        gold = load_gold(case)

        # extract last-step values
        swell_test = extract_last(out, col_swell) * 100.0
        swell_gold = extract_last(gold, col_swell) * 100.0

        radius_test = extract_last(out, col_radius)
        radius_gold = extract_last(gold, col_radius)

        density_test = extract_last(out, col_density)
        density_gold = extract_last(gold, col_density)

        # match experimental point by temperature
        Tcase = float(name.split("_")[-1].replace("K", ""))

        # experimental match
        swell_exp = exp_swell[exp_T_swell == Tcase]
        radius_exp = exp_radius[exp_T_radius == Tcase]
        density_exp = exp_density[exp_T_density == Tcase]

        if not len(swell_exp):
            print(f"No experimental data for T={Tcase} K → skipping")
            continue

        # produce parity plots
        parity_plot(swell_exp, swell_gold, swell_test,
                    f"swell_{Tcase}K", outdir)
        parity_plot(radius_exp, radius_gold, radius_test,
                    f"radius_{Tcase}K", outdir)
        parity_plot(density_exp, density_gold, density_test,
                    f"density_{Tcase}K", outdir)


if __name__ == "__main__":
    main()
