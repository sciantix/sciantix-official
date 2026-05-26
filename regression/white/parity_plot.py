#!/usr/bin/env python3
"""White regression parity plots for bubble size, density, and swelling."""

import os
import sys

import numpy as np

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from regression.core.common import load_gold, load_output
from regression.core.plot import parity_plot


# COARSENING: include White intragranular metrics used to benchmark the coarsening work against legacy SCIANTIX.
METRICS = {
    "ig_radius": {
        "data": "ig_radius.txt",
        "data_factor": 1.0e-9,
        "column": "Intragranular coarsened bubble radius (m)",
        "output_factor": 1.0,
        "title": "White - intragranular bubble radius (m)",
    },
    "ig_density": {
        "data": "ig_density.txt",
        "data_factor": 1.0e19,
        "column": "Intragranular coarsened bubble concentration (bub/m3)",
        "output_factor": 1.0,
        "title": "White - intragranular bubble concentration (bub/m3)",
    },
    "ig_swelling": {
        "data": "ig_intragranular_swelling.txt",
        "data_factor": 1.0,
        "column": "Intragranular coarsened gas bubble swelling (/)",
        "output_factor": 100.0,
        "title": "White - intragranular bubble swelling (%)",
    },
    "intergranular_swelling": {
        "data": "ig_swelling.txt",
        "data_factor": 1.0,
        "column": "Intergranular gas swelling (/)",
        "output_factor": 100.0,
        "title": "White - intergranular swelling (%)",
    },
}


def extract_last(output, column):
    """Return the last timestep value of a given column."""
    header = [h.strip() for h in output.header]
    if column not in header:
        raise KeyError(f"Column '{column}' not found in output.header")
    idx = header.index(column)
    return output.data[-1, idx]


def load_experimental(basename, factor=1.0):
    """Load White experimental data as case-name/value pairs."""
    root = os.path.dirname(__file__)
    fpath = os.path.join(root, "data", basename)

    names = []
    values = []

    with open(fpath, "r") as stream:
        for line in stream:
            stripped = line.strip()
            if not stripped or stripped.startswith("#"):
                continue
            parts = stripped.split()
            names.append(parts[0])
            values.append(float(parts[1]) * factor)

    return np.array(names), np.array(values)


def collect_metric(white_root, config):
    """Collect experimental, gold, and current SCIANTIX values for one White metric."""
    # COARSENING: all White plots follow the same experimental/gold/current parity workflow.
    exp_names, exp_values = load_experimental(config["data"], config["data_factor"])
    exp_list, gold_list, test_list = [], [], []

    for name in sorted(os.listdir(white_root)):
        if not name.startswith("test_White"):
            continue

        case = os.path.join(white_root, name)
        if not os.path.isdir(case):
            continue

        idx = np.where(exp_names == name)[0]
        if len(idx) == 0:
            print(f"[WARNING] No experimental value for {name} in {config['data']}")
            continue

        out = load_output(case)
        gold = load_gold(case)

        exp_list.append(exp_values[idx][0])
        gold_list.append(extract_last(gold, config["column"]) * config["output_factor"])
        test_list.append(extract_last(out, config["column"]) * config["output_factor"])

    return np.array(exp_list), np.array(gold_list), np.array(test_list)


def print_statistics(quantity, exp_arr, gold_arr, test_arr):
    """Print compact diagnostics for each White parity plot."""
    error_test = test_arr - exp_arr
    error_gold = gold_arr - exp_arr

    print("\n" + "=" * 50)
    print(quantity)
    print("=" * 50)
    print(f"Experimental mean:   {np.mean(exp_arr):.6g}")
    print(f"Current SCIANTIX mean: {np.mean(test_arr):.6g}")
    print(f"Current SCIANTIX bias: {np.median(error_test):.6g}")
    print(f"Current SCIANTIX RMSE: {np.sqrt(np.mean(error_test**2)):.6g}")
    print(f"Gold RMSE:            {np.sqrt(np.mean(error_gold**2)):.6g}")


def main():
    root = os.path.dirname(__file__)
    white_root = os.path.abspath(os.path.join(root, "..", "white"))
    outdir = os.path.join(root, "figures")
    os.makedirs(outdir, exist_ok=True)

    for quantity, config in METRICS.items():
        exp_arr, gold_arr, test_arr = collect_metric(white_root, config)
        if exp_arr.size == 0:
            print(f"[WARNING] No data available for {quantity}")
            continue

        parity_plot(exp_arr, gold_arr, test_arr, f"white_{quantity}", config["title"], outdir)
        print_statistics(config["title"], exp_arr, gold_arr, test_arr)


if __name__ == "__main__":
    main()
