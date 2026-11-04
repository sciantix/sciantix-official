#!/usr/bin/env python3
"""Nominal vs calibrated two-zone FMP model against the TEM data, from results/best.txt (EC November 2026)."""

import os
import re
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.ticker

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
from plot import C, POSITIONS  # noqa: E402

LINE = re.compile(r"(r\d{3}): R_intra\s+([\d.]+) \(\s*([\d.]+)\) nm, R_GB\s+([\d.]+) \(\s*([\d.]+)\) nm, "
                  r"A_intra\s+([\d.]+) \(\s*([\d.]+)\) %, A_GB\s+([\d.]+) \(\s*([\d.]+)\) %")


def read_blocks(path):
    blocks, label = {}, None
    with open(path) as f:
        for line in f:
            if not line.startswith(" ") and ": J4 =" in line:
                label = line.split(":")[0]
                blocks[label] = {"J4": float(line.split("J4 = ")[1].split(",")[0]),
                                 "J3": float(line.split("J3 = ")[1])}
            match = LINE.search(line)
            if match and label:
                values = [float(v) for v in match.groups()[1:]]
                blocks[label][match.group(1)] = {
                    "model": dict(zip(("R_intra", "R_GB", "A_intra", "A_GB"), values[0::2])),
                    "exp": dict(zip(("R_intra", "R_GB", "A_intra", "A_GB"), values[1::2])),
                }
    return blocks


def main():
    blocks = read_blocks(os.path.join(HERE, "results", "best.txt"))
    nominal = next(v for k, v in blocks.items() if k.startswith("Nominal"))
    refined = next(v for k, v in blocks.items() if k.startswith("Refined"))
    x = [r for _, r, _ in POSITIONS]
    names = [n for n, _, _ in POSITIONS]

    fig, axes = plt.subplots(1, 2, figsize=(10, 3.9))
    for ax, (quantity_intra, quantity_gb, label) in zip(
            axes, [("R_intra", "R_GB", "Mean particle radius (nm)"), ("A_intra", "A_GB", "Projected area fraction (%)")]):
        for quantity, colour, marker, population in [(quantity_intra, C[0], "o", "intragranular"),
                                                      (quantity_gb, C[1], "s", "grain boundary")]:
            ax.plot(x, [nominal[n]["exp"][quantity] for n in names], marker, color=colour, linestyle="none",
                    label=f"TEM, {population}")
            ax.plot(x, [nominal[n]["model"][quantity] for n in names], ":", color=colour, linewidth=1.2,
                    label=f"nominal, {population}")
            ax.plot(x, [refined[n]["model"][quantity] for n in names], "-", color=colour,
                    label=f"calibrated, {population}")
        ax.set(xlabel="r/R (-)", ylabel=label)
        if quantity_intra == "A_intra":
            ax.set_yscale("log")
            ax.set_yticks([0.2, 0.5, 1, 2, 5, 10])
            ax.yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(lambda v, _: f"{v:g}"))
            ax.yaxis.set_minor_formatter(matplotlib.ticker.NullFormatter())
        else:
            ax.set_ylim(bottom=0)
    axes[0].legend(fontsize=7.5, ncol=2, loc="upper right")
    axes[0].set_ylim(top=22)
    fig.suptitle(f"Two-zone FMP model, 54.1 MWd/kgUO2: nominal J4 = {nominal['J4']:.3f} (J3 = {nominal['J3']:.3f}), "
                 f"calibrated J4 = {refined['J4']:.3f} (J3 = {refined['J3']:.3f})", fontsize=10)
    fig.tight_layout()
    path = os.path.join(HERE, "results", "calibration_two_zones.png")
    fig.savefig(path, dpi=170, bbox_inches="tight", facecolor="white")
    print(f"Figure: {path}")


if __name__ == "__main__":
    main()
