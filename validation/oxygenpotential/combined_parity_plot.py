#!/usr/bin/env python3
"""Combined fresh-fuel + irradiated/burnup oxygen-potential parity plot.

Merges the per-group `oxygen_potential_plot_data.tsv` files already written by
`freshfuel/plot.py` and `burnup/plot.py` (run those first) into one parity
plot per route (Kato, OpenCalphad), marker-coded by group and colour-coded by
temperature -- one figure per route, so both routes and both fuel conditions
are visible together.
"""
from __future__ import annotations

import csv
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

PAPER_PALETTE = [
    "#736F3F", "#BFAE56", "#B29DA6", "#D9AF32", "#A66226", "#733426",
    "#737675", "#9D6953", "#363726", "#785C2D",
]

plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (10, 7),
    "font.family": "serif",
    "font.serif": ["Times New Roman", "Times", "Nimbus Roman", "DejaVu Serif"],
    "mathtext.fontset": "dejavuserif",
    "font.size": 20,
    "axes.labelsize": 20,
    "axes.titlesize": 20,
    "xtick.labelsize": 20,
    "ytick.labelsize": 20,
    "legend.fontsize": 17,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.5,
    "grid.linestyle": "--",
    "lines.linewidth": 3,
    "lines.markersize": 6,
    "legend.frameon": False,
    "axes.prop_cycle": plt.cycler(color=PAPER_PALETTE),
})

SCRIPT_DIR = Path(__file__).resolve().parent
GROUPS = [("freshfuel", "Fresh fuel", "o"), ("burnup", "Irradiated fuel", "^")]
ROUTES = [("Kato", "figures_Kato", "Kato correlation"), ("OC", "figures_OC", "OpenCalphad + U-Pu-O database from TAF-ID")]
OUT_DIR = SCRIPT_DIR / "figures_combined"
MARKER_SIZE = 55


def read_points(path: Path) -> list[dict[str, float | str]]:
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream, delimiter="\t"))


def plot_route(route_label: str, subdir: str, title: str) -> None:
    lower, upper = -800.0, 0.0
    margin = 0.05 * (upper - lower)
    limits = [lower - margin, upper + margin]

    fig, ax = plt.subplots()
    vmin, vmax = 800.0, 3000.0
    handles = []
    for group_dir, group_label, marker in GROUPS:
        points = read_points(SCRIPT_DIR / group_dir / subdir / "oxygen_potential_plot_data.tsv")
        exp = np.array([float(p["exp_mu"]) for p in points])
        sci = np.array([float(p["sci_mu"]) for p in points])
        temperature = np.array([float(p["temperature"]) for p in points])
        scatter = ax.scatter(
            exp, sci, c=temperature, cmap="viridis", vmin=vmin, vmax=vmax,
            marker=marker, s=MARKER_SIZE, alpha=0.75, edgecolors="none",
        )
        handles.append(
            plt.Line2D([0], [0], marker=marker, color="0.3", linestyle="none",
                       markersize=12, label=group_label)
        )

    ax.plot(limits, limits, color="0.25", linewidth=1.4, label="1:1")
    ax.set_xlim(limits)
    ax.set_ylim(limits)
    ax.set_title(title)
    ax.set_xlabel("Experimental oxygen potential (kJ/mol)")
    ax.set_ylabel("Calculated oxygen potential (kJ/mol)")
    ax.grid(True, linestyle=":", alpha=0.6)
    handles.append(plt.Line2D([0], [0], color="0.25", linewidth=1.4, label="1:1"))
    ax.legend(handles=handles, loc="upper left")
    colorbar = fig.colorbar(scatter, ax=ax)
    colorbar.set_label("Temperature (K)")
    fig.tight_layout()

    OUT_DIR.mkdir(exist_ok=True)
    out_path = OUT_DIR / f"parity_combined_{route_label}.png"
    fig.savefig(out_path, dpi=220)
    plt.close(fig)
    print(f"Saved {out_path}")


def main() -> None:
    for route_label, subdir, title in ROUTES:
        plot_route(route_label, subdir, title)


if __name__ == "__main__":
    main()
