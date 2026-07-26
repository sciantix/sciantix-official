"""
sciantix -- parametric studies for the generalized spectral diffusion (NUS) solver
author: Giovanni Zullo

Influence of the grain-boundary resolution layer depth on xenon behaviour. Reproduces
Figures 4 and 5 of

    A. Zayat, G. Zullo, L. Luzzi, D. Pizzocri, "A generalized spectral algorithm for
    fission gas diffusion: Implementation and verification in SCIANTIX".

The total source is split as in the paper: a uniform fission source S1 over the grain,
plus a resolution term S2 confined to the outer shell of width lambda. lambda = 0 is the
conventional formulation with no grain-boundary resolution; 10 nm is the baseline the
paper takes from SRIM and Loesoenen, 100 nm the upper case.

Caveat on S2. The paper describes it as proportional to the fission rate and developing
as gas accumulates at the grain boundary. The solver does not compute it: it reads it
from non_uniform_source.txt, so its magnitude is prescribed here as a ramp rather than
derived from the grain-boundary inventory. The lambda dependence and the direction of
the effect are therefore meaningful, the absolute size of the shift is only as good as
that prescription. Wiring S2 to c_GB would remove the caveat.

    python3 utilities/NUS_studies/fig05_gb_resolution.py
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from _common import (
    DURATION,
    FISSION_RATE,
    GRAIN_RADIUS,
    I_GRAIN_BOUNDARY_RESOLUTION,
    build_and_run,
    figure_path,
    release_onset,
    shell_source,
)

STUDY = "fig05"

# Resolution layer depths, in metres. The paper's three cases.
LAMBDAS = [0.0, 10e-9, 100e-9]

# Prescribed growth of the resolution term over the irradiation, as a multiple of the
# fission source. See the caveat in the module docstring.
S2_END_FACTOR = 3.5

QUANTITIES = [
    ("Xe in grain (at/m3)", "Xe in grain (at/m$^3$)"),
    ("Xe at grain boundary (at/m3)", "Xe at grain boundary (at/m$^3$)"),
    ("Xe released (at/m3)", "Xe released (at/m$^3$)"),
]


def label_for(lam):
    """Plot label, with the symbol typeset."""
    return rf"$\lambda$ = {lam * 1e9:.0f} nm"


def name_for(lam):
    """Console label, readable in a terminal."""
    return f"lambda = {lam * 1e9:.0f} nm"


def main():
    print(f"Fission source {FISSION_RATE:.3e} fiss/m3s on a {GRAIN_RADIUS * 1e6:g} um grain")
    print(f"Resolution term ramping to {S2_END_FACTOR:g} x the fission source\n")

    outputs, labels = [], []
    for lam in LAMBDAS:
        name = f"lambda_{lam * 1e9:03.0f}nm"
        sources = [
            shell_source(0.0, FISSION_RATE, FISSION_RATE, lam),
            shell_source(DURATION, FISSION_RATE, S2_END_FACTOR * FISSION_RATE, lam),
        ]
        # With no shell there is nothing to re-solve, which is the conventional case.
        settings = {I_GRAIN_BOUNDARY_RESOLUTION: 1 if lam > 0.0 else 0}
        out = build_and_run(STUDY, name, sources, settings)
        if out is None:
            print(f"Aborting: {name} did not run")
            return 1
        outputs.append(out)
        labels.append(label_for(lam))

    print("\n  Release onset:")
    for lam, out in zip(LAMBDAS, outputs):
        onset = release_onset(out)
        print(f"    {name_for(lam):<20}{onset:.0f} h" if onset is not None else f"    {name_for(lam):<20}no release")

    reference = outputs[0]
    print("\n  Final inventories, relative to no grain-boundary resolution:")
    for lam, out in zip(LAMBDAS[1:], outputs[1:]):
        print(f"    {name_for(lam)}")
        for column, _ in QUANTITIES:
            ref, value = reference.get_last(column), out.get_last(column)
            delta = 100 * (value - ref) / ref if ref != 0.0 else float("nan")
            print(f"      {column:<32}{value:>13.4e}{delta:>10.4f} %")

    plot(outputs, labels)
    return 0


def plot(outputs, labels):
    colors = ["black", "tab:blue", "crimson"]
    n_rows = 1 + 2 * len(QUANTITIES)
    fig, axes = plt.subplots(n_rows, 1, figsize=(7.0, 2.6 + 2.0 * (n_rows - 1)))

    # Source decomposition: the shells are a couple of percent of the radius, so the
    # panel is drawn against r/a and the shell edges marked, rather than trying to make
    # a 100 nm step visible on a 5 um axis.
    for lam, color in zip(LAMBDAS, colors):
        if lam == 0.0:
            axes[0].plot([0.0, 1.0], [FISSION_RATE] * 2, "-", color=color, linewidth=1.8, label=label_for(lam))
            continue
        start = 1.0 - lam / GRAIN_RADIUS
        axes[0].plot([0.0, start], [FISSION_RATE] * 2, "-", color=color, linewidth=1.8, label=label_for(lam))
        axes[0].plot([start, 1.0], [S2_END_FACTOR * FISSION_RATE] * 2, "-", color=color, linewidth=1.8)
        axes[0].plot([start, start], [FISSION_RATE, S2_END_FACTOR * FISSION_RATE], "--", color=color, linewidth=1.0)
    axes[0].set_xlim(0.90, 1.005)
    axes[0].set_xlabel("r / a")
    axes[0].set_ylabel("S(r) (at/m$^3$s)")
    axes[0].set_title("Source decomposition at end of irradiation (outer 10% of the grain)")
    axes[0].grid(True, ls=":")
    axes[0].legend(fontsize=8)

    time = outputs[0].get_all("Time (h)")
    for index, (column, ylabel) in enumerate(QUANTITIES):
        ax = axes[1 + 2 * index]
        ax_diff = axes[2 + 2 * index]
        reference = outputs[0].get_all(column)

        for out, label, color in zip(outputs, labels, colors):
            ax.plot(out.get_all("Time (h)"), out.get_all(column), "-", color=color, linewidth=1.6, label=label)
        ax.set_ylabel(ylabel, fontsize=9)
        ax.grid(True, ls=":")
        ax.legend(fontsize=8)

        for out, label, color in zip(outputs[1:], labels[1:], colors[1:]):
            ax_diff.plot(time, out.get_all(column) - reference, "-", color=color, linewidth=1.4, label=label)
        ax_diff.axhline(0.0, color="grey", ls=":", linewidth=1.0)
        ax_diff.set_ylabel(r"$\Delta$ vs $\lambda$=0", fontsize=8)
        ax_diff.tick_params(labelsize=8)
        ax_diff.grid(True, ls=":")

    axes[-1].set_xlabel("Time (h)")
    fig.suptitle("Influence of the grain-boundary resolution layer depth", fontsize=11)
    fig.tight_layout(rect=(0, 0, 1, 0.98))

    outpath = figure_path(STUDY, "gb_resolution")
    fig.savefig(outpath, dpi=160)
    plt.close(fig)
    print("\nSaved:", outpath)


if __name__ == "__main__":
    sys.exit(main())
