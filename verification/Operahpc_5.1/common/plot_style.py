"""
Shared matplotlib style for verification-case plots, matched to the actual
figures in OperaHPC WP5-D5.1 (Figures 15-19: plain matplotlib defaults --
sans-serif font, tab10 color cycle, full box spines, visible grid, plain
non-bold titles -- rather than a custom "publication" theme).
"""

import matplotlib as mpl

# matplotlib's default tab10 cycle, used directly so plots read the same as
# the report's (which use un-styled default matplotlib).
TAB10 = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
         "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"]

MANUFACTURED = TAB10[0]   # blue, matches the report's "Manufactured solution"
NUMERICAL = TAB10[3]      # red, matches the report's "SCIANTIX solution"
LINE_CYCLE = TAB10

# Matches Figure 18's legend order/colors: average error (blue), RMSE
# (orange), max error (green), final point error (red).
ERROR_STYLE = {
    "L1": dict(color=TAB10[0], marker="o"),
    "L2": dict(color=TAB10[1], marker="s"),
    "Linf": dict(color=TAB10[2], marker="D"),
    "final": dict(color=TAB10[3], marker="x"),
}


def apply():
    """Reset to matplotlib's own defaults, with only a grid and figure/save
    DPI added -- deliberately not a custom theme, to match the report."""
    mpl.rcdefaults()
    mpl.rcParams.update(
        {
            "figure.dpi": 130,
            "savefig.dpi": 300,
            "axes.titlesize": 11,
            "axes.titleweight": "normal",
            "axes.grid": True,
            "grid.alpha": 0.6,
            "axes.prop_cycle": mpl.cycler(color=LINE_CYCLE),
            "savefig.bbox": "tight",
        }
    )


def save(fig, path_without_ext):
    """Save a figure as PNG."""
    fig.savefig(path_without_ext + ".png")


def clean_log_ticks(ax, values, axis="x"):
    """
    Force a log axis to show exactly one tick per swept value, plainly
    formatted (e.g. "2.5", "80"), instead of matplotlib's automatic minor
    ticks -- which, for a sweep spanning less than ~2 decades (typical for
    this suite's dt/mode/N sweeps), pack in enough "2x10^1, 3x10^1, 4x10^1,
    6x10^1"-style labels to visibly overlap.
    """
    values = sorted(set(values))
    labels = [f"{v:g}" for v in values]
    target = ax.xaxis if axis == "x" else ax.yaxis
    target.set_ticks(values)
    target.set_ticklabels(labels)
    target.set_ticks([], minor=True)


def inset_legend(ax, **kwargs):
    """
    A legend meant to sit inside busy axes (as opposed to one placed outside
    via bbox_to_anchor): opaque white backing so it stays readable over a
    data curve regardless of exactly where matplotlib's `loc="best"` puts it.
    """
    kwargs.setdefault("frameon", True)
    kwargs.setdefault("facecolor", "white")
    kwargs.setdefault("framealpha", 0.92)
    kwargs.setdefault("edgecolor", "none")
    return ax.legend(**kwargs)
