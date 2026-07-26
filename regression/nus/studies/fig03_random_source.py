"""
sciantix -- parametric studies for the generalized spectral diffusion (NUS) solver
author: Giovanni Zullo

Effect of a time-dependent, spatially non-uniform source on xenon transport, against a
uniform reference at the same volume-averaged production. Corresponds to Figures 2 and 3
of

    A. Zayat, G. Zullo, L. Luzzi, D. Pizzocri, "A generalized spectral algorithm for
    fission gas diffusion: Implementation and verification in SCIANTIX".

IMPORTANT -- this does not reproduce the published curves. The paper specifies neither
the distribution the random source is drawn from, nor its seed, nor any constraint
beyond the preserved volume average, so the profiles here cannot be the ones plotted in
the article. What is reproduced is the mechanism and the direction of the effect, not
the numbers. To recover the published figure, drop the source file actually used into
this directory and read it instead of generating one.

The rule used here, fixed seed so that reruns agree: on a uniform time grid, draw the
slope A uniformly over the range that keeps S(r) non-negative across the grain, then set
the intercept B from the volume-average constraint. Every instant therefore produces gas
at the same rate as the uniform reference, and only the radial distribution varies.

    python3 regression/nus/studies/fig03_random_source.py
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

STUDY = "fig03"

SEED = 20250726
N_INTERVALS = 40  # source records over the irradiation

QUANTITIES = [
    ("Xe in grain (at/m3)", "Xe in grain (at/m$^3$)"),
    ("Xe at grain boundary (at/m3)", "Xe at grain boundary (at/m$^3$)"),
    ("Xe released (at/m3)", "Xe released (at/m$^3$)"),
]


def random_sources():
    """Time-dependent profiles, each holding the volume average of the uniform case."""
    rng = np.random.default_rng(SEED)
    a_min, a_max = admissible_slope_range()

    times = np.linspace(0.0, DURATION, N_INTERVALS)
    slopes = rng.uniform(a_min, a_max, size=times.size)
    return [linear_source(t, s) for t, s in zip(times, slopes)], times, slopes


def main():
    print(f"Volume average held at {FISSION_RATE:.3e} fiss/m3s, seed {SEED}\n")

    uniform = build_and_run(
        STUDY, "uniform", [linear_source(0.0, 0.0), linear_source(DURATION, 0.0)]
    )
    sources, times, slopes = random_sources()
    random = build_and_run(STUDY, "random", sources)

    if uniform is None or random is None:
        print("Aborting: a variant did not run")
        return 1

    report_volume_average([uniform, random], ["uniform", "random"])

    print("\n  Release onset:")
    for label, out in (("uniform", uniform), ("random", random)):
        onset = release_onset(out)
        print(f"    {label:<10}{onset:.0f} h" if onset is not None else f"    {label:<10}no release")

    print("\n  Final inventories, random relative to uniform:")
    for column in ("Xe in grain (at/m3)", "Xe at grain boundary (at/m3)", "Xe released (at/m3)"):
        u, r = uniform.get_last(column), random.get_last(column)
        delta = 100 * (r - u) / u if u != 0.0 else float("nan")
        print(f"    {column:<32}{u:>13.4e}{r:>13.4e}{delta:>10.3f} %")

    plot(uniform, random, times, slopes)
    return 0


def plot(uniform, random, times, slopes):
    fig, axes = plt.subplots(1 + len(QUANTITIES), 1, figsize=(7.0, 3.0 + 2.4 * len(QUANTITIES)))

    # Snapshots of the random profile, at the instants the paper shows.
    r = np.linspace(0.0, GRAIN_RADIUS, 200)
    wanted = [0.0, 2000.0, 4000.0, DURATION]
    colors = matplotlib.colormaps["viridis"](np.linspace(0.15, 0.85, len(wanted)))
    for target, color in zip(wanted, colors):
        index = int(np.argmin(np.abs(times - target)))
        slope = slopes[index]
        axes[0].plot(
            r * 1e6,
            slope * r + intercept_for(slope),
            "-",
            color=color,
            linewidth=1.6,
            label=f"t = {times[index]:.0f} h",
        )
    axes[0].axhline(FISSION_RATE, color="black", ls=":", linewidth=1.2, label="uniform")
    axes[0].set_xlabel(r"r ($\mu$m)")
    axes[0].set_ylabel("S(r) (at/m$^3$s)")
    axes[0].set_title("Random source, snapshots at equal volume average")
    axes[0].grid(True, ls=":")
    axes[0].legend(fontsize=8)

    for ax, (column, ylabel) in zip(axes[1:], QUANTITIES):
        ax.plot(uniform.get_all("Time (h)"), uniform.get_all(column), "-", color="black", linewidth=1.8, label="uniform")
        ax.plot(random.get_all("Time (h)"), random.get_all(column), "-", color="crimson", linewidth=1.6, label="random")
        ax.set_ylabel(ylabel, fontsize=9)
        ax.grid(True, ls=":")
        ax.legend(fontsize=8)
    axes[-1].set_xlabel("Time (h)")

    fig.suptitle("Time-dependent source heterogeneity  (illustrative, not the published profiles)", fontsize=10)
    fig.tight_layout(rect=(0, 0, 1, 0.98))

    outpath = figure_path(STUDY, "random_source")
    fig.savefig(outpath, dpi=160)
    plt.close(fig)
    print("\nSaved:", outpath)


if __name__ == "__main__":
    sys.exit(main())
