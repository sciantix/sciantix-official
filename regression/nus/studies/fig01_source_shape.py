"""
sciantix -- parametric studies for the generalized spectral diffusion (NUS) solver
author: Giovanni Zullo

Effect of the radial source distribution on xenon transport, at a fixed volume-averaged
production rate. Reproduces Figure 1 of

    A. Zayat, G. Zullo, L. Luzzi, D. Pizzocri, "A generalized spectral algorithm for
    fission gas diffusion: Implementation and verification in SCIANTIX".

Five linear sources S(r) = A r + B, all constrained to the same volume average, so any
difference in the results comes from where the gas is born and not from how much of it
there is. The paper describes them qualitatively -- one uniform, two falling towards the
boundary with the first steeper, two rising -- without giving the slopes; the five here
span the range over which S(r) stays non-negative across the grain, so they bracket what
a linear profile can physically do rather than sampling it arbitrarily.

    python3 regression/nus/studies/fig01_source_shape.py
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
    admissible_slope_range,
    build_and_run,
    figure_path,
    intercept_for,
    linear_source,
    release_onset,
    report_volume_average,
)

STUDY = "fig01"

QUANTITIES = [
    ("Xe in grain (at/m3)", "Xe in grain (at/m$^3$)"),
    ("Xe at grain boundary (at/m3)", "Xe at grain boundary (at/m$^3$)"),
    ("Xe released (at/m3)", "Xe released (at/m$^3$)"),
]


def variants():
    """
    The five profiles, as (name, label, slope).

    Fractions of the admissible limits rather than round numbers: 0.9 takes the profile
    close to running dry at one end, 0.45 sits halfway, and 0 is the uniform reference.
    """
    a_min, a_max = admissible_slope_range()
    return [
        ("S1", r"$S_1$  steeply falling", 0.90 * a_min),
        ("S2", r"$S_2$  falling", 0.45 * a_min),
        ("S0", r"$S_0$  uniform", 0.0),
        ("S3", r"$S_3$  rising", 0.45 * a_max),
        ("S4", r"$S_4$  steeply rising", 0.90 * a_max),
    ]


def main():
    a_min, a_max = admissible_slope_range()
    print(f"Volume average held at {FISSION_RATE:.3e} fiss/m3s on a {GRAIN_RADIUS * 1e6:g} um grain")
    print(f"Admissible slopes for S(r) >= 0: [{a_min:.3e}, {a_max:.3e}] at/m4s\n")

    cases = variants()
    outputs, labels, slopes = [], [], []

    for name, label, slope in cases:
        # The source is held constant in time; two records are enough to span the history.
        sources = [linear_source(0.0, slope), linear_source(DURATION, slope)]
        out = build_and_run(STUDY, name, sources)
        if out is None:
            print(f"Aborting: {name} did not run")
            return 1
        outputs.append(out)
        labels.append(label)
        slopes.append(slope)

    report_volume_average(outputs, [c[0] for c in cases])

    print("\n  Release onset and final inventories:")
    print(f"    {'case':<8}{'S(0)':>12}{'S(a)':>12}{'onset (h)':>12}{'in grain':>14}{'released':>14}")
    reference = None
    for (name, _, slope), out in zip(cases, outputs):
        b = intercept_for(slope)
        onset = release_onset(out)
        in_grain = out.get_last("Xe in grain (at/m3)")
        released = out.get_last("Xe released (at/m3)")
        if name == "S0":
            reference = (in_grain, released)
        print(
            f"    {name:<8}{b:>12.3e}{slope * GRAIN_RADIUS + b:>12.3e}"
            f"{(onset if onset is not None else float('nan')):>12.0f}"
            f"{in_grain:>14.4e}{released:>14.4e}"
        )

    if reference is not None:
        print("\n  Relative to the uniform source:")
        for (name, _, _), out in zip(cases, outputs):
            if name == "S0":
                continue
            in_grain = out.get_last("Xe in grain (at/m3)")
            released = out.get_last("Xe released (at/m3)")
            print(
                f"    {name:<8}in grain {100 * (in_grain - reference[0]) / reference[0]:+8.3f} %"
                f"    released {100 * (released - reference[1]) / reference[1]:+8.3f} %"
            )

    plot(cases, outputs, labels)
    return 0


def plot(cases, outputs, labels):
    colors = list(matplotlib.colormaps["coolwarm"](np.linspace(0.0, 1.0, len(cases))))
    # The middle of coolwarm is near-white; the uniform case is the reference every other
    # curve is read against, so it gets to be visible.
    colors[[c[0] for c in cases].index("S0")] = (0.0, 0.0, 0.0, 1.0)

    fig, axes = plt.subplots(1 + len(QUANTITIES), 1, figsize=(7.0, 3.0 + 2.4 * len(QUANTITIES)))

    r = np.linspace(0.0, GRAIN_RADIUS, 200)
    for (_, _, slope), label, color in zip(cases, labels, colors):
        axes[0].plot(r * 1e6, slope * r + intercept_for(slope), "-", color=color, linewidth=1.8, label=label)
    axes[0].axhline(FISSION_RATE, color="grey", ls=":", linewidth=1.0)
    axes[0].set_xlabel(r"r ($\mu$m)")
    axes[0].set_ylabel("S(r) (at/m$^3$s)")
    axes[0].set_title("Radial source profiles at equal volume average")
    axes[0].grid(True, ls=":")
    axes[0].legend(fontsize=8)

    for ax, (column, ylabel) in zip(axes[1:], QUANTITIES):
        for out, label, color in zip(outputs, labels, colors):
            ax.plot(out.get_all("Time (h)"), out.get_all(column), "-", color=color, linewidth=1.6, label=label)
        ax.set_ylabel(ylabel, fontsize=9)
        ax.grid(True, ls=":")
        ax.legend(fontsize=8)
    axes[-1].set_xlabel("Time (h)")

    fig.suptitle("Effect of source spatial distribution on xenon transport", fontsize=11)
    fig.tight_layout(rect=(0, 0, 1, 0.98))

    outpath = figure_path(STUDY, "source_shape")
    fig.savefig(outpath, dpi=160)
    plt.close(fig)
    print("\nSaved:", outpath)


if __name__ == "__main__":
    sys.exit(main())
