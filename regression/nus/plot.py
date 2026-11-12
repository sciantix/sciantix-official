"""
sciantix regression suite
author: Giovanni Zullo

Figures for the non-uniform-source (NUS) cases.

One figure per case: the radial source profile S(r) that drives the case, next to the
xenon transport histories it produces. The gold curve is overlaid when a gold file sits
beside the output, and a residual strip is added underneath a panel only when that
quantity actually falls outside the regression tolerance -- so a passing case gives a
clean figure and a failing one shows where and by how much it drifted.

Usage:
    python3 regression/nus/plot.py                 # every case under regression/nus
    python3 regression/nus/plot.py <case_dir>      # one case, anywhere on disk
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")  # figures are written to file, never displayed
import matplotlib.pyplot as plt

# Add the project root to path so we can import regression.core
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from regression.core.compare import compare_arrays
from regression.core.parser import SciantixOutput
from regression.nus.sources import MODE_BLOCKS, load_sources

# Where the gas sits and how much of it got out -- the quantities a non-uniform radial
# profile actually moves. Per species, since a NUS case may be driven by fission
# (xenon, krypton) or by implanted helium.
SPECIES_PANELS = {
    "Xe": [
        ("Xe in grain (at/m3)", "Xe in grain (at/m$^3$)"),
        ("Xe at grain boundary (at/m3)", "Xe at grain boundary (at/m$^3$)"),
        ("Xe released (at/m3)", "Xe released (at/m$^3$)"),
        ("Fission gas release (/)", "Fission gas release (/)"),
    ],
    "Kr": [
        ("Kr in grain (at/m3)", "Kr in grain (at/m$^3$)"),
        ("Kr at grain boundary (at/m3)", "Kr at grain boundary (at/m$^3$)"),
        ("Kr released (at/m3)", "Kr released (at/m$^3$)"),
        ("Fission gas release (/)", "Fission gas release (/)"),
    ],
    "He": [
        ("He in grain (at/m3)", "He in grain (at/m$^3$)"),
        ("He at grain boundary (at/m3)", "He at grain boundary (at/m$^3$)"),
        ("He released (at/m3)", "He released (at/m$^3$)"),
        ("He fractional release (/)", "He fractional release (/)"),
    ],
}

TIME = "Time (h)"
GRAIN_RADIUS = "Grain radius (m)"

ABS_TOL = 1e-8
REL_TOL = 1e-6


def _grain_radius(out):
    """Initial grain radius in metres, or None if the column is absent."""
    if GRAIN_RADIUS not in out.colmap:
        return None
    values = out.get_all(GRAIN_RADIUS)
    return float(values[0]) if values.size else None


def _select_species(out):
    """
    The species the case is actually about: the one holding the most gas in the grain.
    A helium implantation case has no fission at all, so keying off xenon would draw
    four flat zero lines.
    """
    best, best_inventory = None, 0.0
    for species in SPECIES_PANELS:
        column = f"{species} in grain (at/m3)"
        if column not in out.colmap:
            continue
        inventory = float(np.nanmax(np.abs(out.get_all(column))))
        if inventory > best_inventory:
            best, best_inventory = species, inventory

    # Nothing anywhere: fall back to xenon so the figure still shows the case ran.
    return best or "Xe"


def _radial_profiles(case_dir):
    """
    The radial profile that explains the case, as (records, labels, title).

    A case may be driven by a non-uniform source or by a non-uniform initial condition --
    the helium implantation case has an all-zero source and puts everything in
    initial_distribution.txt. Prefer whichever is not identically zero.
    """
    sources = load_sources(os.path.join(case_dir, "non_uniform_source.txt"))
    live = [s for s in sources if not s.is_null]
    if live:
        return live, [f"t = {s.time:g} h" for s in live], "Radial source profile"

    ics = load_sources(os.path.join(case_dir, "initial_distribution.txt"))
    # Records map to spectral mode blocks, not to instants; only the non-zero ones say
    # anything, and their index names the quantity they initialise.
    live, labels = [], []
    for index, ic in enumerate(ics):
        if ic.is_null:
            continue
        live.append(ic)
        labels.append(MODE_BLOCKS[index] if index < len(MODE_BLOCKS) else f"block {index}")
    if live:
        return live, labels, "Radial initial condition"

    return [], [], ""


def _outer_region_start(sources):
    """
    Normalised radius where the outermost region begins, when every record agrees on a
    narrow outer shell. None when there is no such shell to zoom into.
    """
    starts = {
        round(float(source.domain[-2]), 12)
        for source in sources
        if source.n_regions > 1 and source.domain.size >= 2
    }
    if len(starts) != 1:
        return None

    start = starts.pop()
    # Only worth an inset while the shell stays a thin sliver of the grain.
    return start if start >= 0.8 else None


def _plot_radial(ax, sources, labels, title, a):
    """Radial profile, one curve per record."""
    colors = matplotlib.colormaps["viridis"](np.linspace(0.15, 0.85, max(len(sources), 1)))
    curves = []

    for source, label, color in zip(sources, labels, colors):
        r, s = source.evaluate(a)
        if r.size == 0:
            continue
        curves.append((r, s, color))
        ax.plot(r * 1e6, s, "-", color=color, linewidth=1.8, label=label)

    is_source = title.endswith("source profile")
    ax.set_xlabel(r"r ($\mu$m)")
    ax.set_ylabel("S(r) (at/m$^3$s)" if is_source else "c(r, 0) (at/m$^3$)")
    ax.set_title(title)
    ax.grid(True, ls=":")
    if len(sources) <= 6:
        ax.legend(fontsize=8, loc="center left")

    # The grain-boundary resolution shell is a couple of percent of the radius, so on the
    # full-grain axis it collapses into the frame. Zoom it, or the one feature that makes
    # the profile non-uniform cannot be read at all.
    start = _outer_region_start(sources)
    if start is None or not curves:
        return

    inset = ax.inset_axes([0.55, 0.45, 0.42, 0.5])
    r_min = start * a * 1e6
    r_max = a * 1e6
    for r, s, color in curves:
        inset.plot(r * 1e6, s, "-", color=color, linewidth=1.5)

    shell_m = (1.0 - start) * a
    shell = f"{shell_m * 1e9:.0f} nm" if shell_m < 1e-6 else f"{shell_m * 1e6:.3g} $\\mu$m"
    inset.set_xlim(r_min - 0.02 * (r_max - r_min), r_max)
    inset.set_title(f"outer {shell}", fontsize=8)
    inset.tick_params(labelsize=7)
    inset.grid(True, ls=":")


def _plot_quantity(ax, time, test, gold, ylabel):
    """One history panel. Returns the bad-element mask, or None when there is no gold."""
    ax.plot(time, test, "-", color="black", linewidth=1.8, label="test")

    bad_mask = None
    if gold is not None and gold.shape == test.shape:
        ax.plot(time, gold, "--", color="brown", linewidth=1.4, label="gold")
        _, _, bad_mask = compare_arrays(test, gold, abs_tol=ABS_TOL, rel_tol=REL_TOL)

    ax.set_ylabel(ylabel, fontsize=9)
    ax.grid(True, ls=":")
    ax.legend(fontsize=8)
    return bad_mask


def _plot_residual(ax, time, test, gold, bad_mask):
    """Relative difference against the gold, drawn only for a quantity that failed."""
    with np.errstate(divide="ignore", invalid="ignore"):
        rel = np.abs(test - gold) / np.maximum(ABS_TOL, np.abs(gold))

    ax.semilogy(time, rel, "-", color="crimson", linewidth=1.2)
    ax.axhline(REL_TOL, color="grey", ls="--", linewidth=1.0)
    if np.any(bad_mask):
        ax.semilogy(time[bad_mask], rel[bad_mask], "o", color="crimson", markersize=3)

    ax.set_ylabel("rel. diff.", fontsize=8)
    ax.grid(True, ls=":", which="both")
    ax.tick_params(labelsize=8)


def plot_case(case_dir, outdir=None):
    """
    Build the figure for one case. Returns the path written, or None if the case could
    not be plotted.
    """
    name = os.path.basename(os.path.normpath(case_dir))

    output_path = os.path.join(case_dir, "output.txt")
    if not os.path.isfile(output_path):
        print(f"Skipping {name}: no output.txt")
        return None

    out = SciantixOutput(output_path)
    if TIME not in out.colmap:
        print(f"Skipping {name}: no '{TIME}' column")
        return None
    time = out.get_all(TIME)

    gold_path = os.path.join(case_dir, "output_gold.txt")
    gold = SciantixOutput(gold_path) if os.path.isfile(gold_path) else None

    a = _grain_radius(out)
    radial, radial_labels, radial_title = _radial_profiles(case_dir)
    # A case running a uniform solver carries no radial profile; its panel is dropped.
    show_radial = bool(radial) and a is not None and a > 0.0

    species = _select_species(out)
    expected = SPECIES_PANELS[species]
    quantities = [(col, label) for col, label in expected if col in out.colmap]
    for col, _ in expected:
        if col not in out.colmap:
            print(f"Warning: {name} has no '{col}' column, panel skipped")
    if not quantities:
        print(f"Skipping {name}: none of the expected columns are present")
        return None

    # Resolve every panel before laying the figure out, so that the residual strips can
    # be given their own rows only where they are actually needed.
    panels = []
    for col, label in quantities:
        test = out.get_all(col)
        gold_values = None
        bad_mask = None
        if gold is not None and col in gold.colmap:
            gold_values = gold.get_all(col)
            if gold_values.shape == test.shape:
                ok, _, bad_mask = compare_arrays(test, gold_values, abs_tol=ABS_TOL, rel_tol=REL_TOL)
                if ok:
                    bad_mask = None
            else:
                print(f"Warning: {name} '{col}' has {test.size} rows against {gold_values.size} in gold")
                gold_values = None
        panels.append((col, label, test, gold_values, bad_mask))

    n_rows = len(panels) + (1 if show_radial else 0)
    height_ratios = ([1.0] if show_radial else []) + [
        1.0 if mask is None else 1.35 for *_, mask in panels
    ]

    fig, axes = plt.subplots(
        n_rows,
        1,
        figsize=(7.0, 2.3 * n_rows),
        gridspec_kw={"height_ratios": height_ratios},
        squeeze=False,
    )
    axes = axes[:, 0]

    row = 0
    bottom_ax = None
    if show_radial:
        _plot_radial(axes[row], radial, radial_labels, radial_title, a)
        row += 1

    for col, label, test, gold_values, bad_mask in panels:
        ax = axes[row]
        if bad_mask is None:
            _plot_quantity(ax, time, test, gold_values, label)
        else:
            # Split this row in two: history on top, relative difference underneath.
            divider = ax.get_subplotspec().subgridspec(2, 1, height_ratios=[2.2, 1.0], hspace=0.05)
            ax.remove()
            ax_hist = fig.add_subplot(divider[0])
            ax_res = fig.add_subplot(divider[1], sharex=ax_hist)
            _plot_quantity(ax_hist, time, test, gold_values, label)
            plt.setp(ax_hist.get_xticklabels(), visible=False)
            _plot_residual(ax_res, time, test, gold_values, bad_mask)
            ax = ax_res
        bottom_ax = ax
        row += 1

    # Splitting a row appends axes out of order, so the bottom panel has to be tracked
    # rather than read off the end of fig.axes.
    if bottom_ax is not None:
        bottom_ax.set_xlabel(TIME)

    status = "" if all(mask is None for *_, mask in panels) else "  [MISMATCH vs gold]"
    fig.suptitle(f"{name}  --  {species}{status}", fontsize=11)
    fig.tight_layout(rect=(0, 0, 1, 0.98))

    if outdir is None:
        outdir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "figures")
    os.makedirs(outdir, exist_ok=True)
    outpath = os.path.join(outdir, f"{name}.png")
    fig.savefig(outpath, dpi=160)
    plt.close(fig)

    print("Saved:", outpath)
    return outpath


def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))

    if len(sys.argv) > 1:
        case_dir = os.path.abspath(sys.argv[1])
        if not os.path.isdir(case_dir):
            print(f"Error: {case_dir} is not a directory")
            return 1
        # A case outside the suite gets its figure next to itself, not in the tree.
        inside = os.path.commonpath([case_dir, script_dir]) == script_dir
        plot_case(case_dir, outdir=None if inside else case_dir)
        return 0

    cases = sorted(
        os.path.join(script_dir, entry)
        for entry in os.listdir(script_dir)
        if entry.startswith("test_") and os.path.isdir(os.path.join(script_dir, entry))
    )
    if not cases:
        print(f"No test_* case found under {script_dir}")
        return 1

    for case_dir in cases:
        plot_case(case_dir)
    return 0


if __name__ == "__main__":
    sys.exit(main())
