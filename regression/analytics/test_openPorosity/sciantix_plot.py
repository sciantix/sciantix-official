"""
sciantix regression suite
author: Giovanni Zullo

Open-porosity / porosity-evolution plots (Baker 1977, 1273 K conditions).

This case exercises three coupled models:
    iDensification        = 1   Van Uffelen (2002) fit -> fuel densification
    iGrainBoundaryVenting = 3   Pagani et al. (2025), athermal ML release
    (porosity evolution as the resulting output)

The figures are built to make those choices legible:
    * porosity_densification.png  densification factor driving the porosity
                                  evolution down to the residual floor.
    * grainboundary_venting.png   athermal ML release: FGR alongside the
                                  intergranular venting probability.
    * overview.png                both stories side by side (for reports).

Run from the repository root:
    python3 -m regression.analytics.test_openPorosity.sciantix_plot

or directly inside this folder:
    python3 sciantix_plot.py

Execution and gold comparison are handled by the regression runner; this
script only reads output.txt / output_gold.txt and draws the figures.
"""

import os
import sys
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))

from regression.core.common import load_output, load_gold

# active model settings, annotated on the figures
SETTINGS_NOTE = (
    "iDensification = 1  (Van Uffelen, 2002)\n"
    "iGrainBoundaryVenting = 3  (athermal ML, Pagani et al. 2025)"
)

X_COL = "Burnup (MWd/kgUO2)"

# legend proxy: dotted grey line standing for every gold (reference) overlay
GOLD_PROXY = Line2D([0], [0], color="#777777", linestyle=":", lw=1.5, label="Gold (reference)")


# ------------------------------------------------------------
# utils
# ------------------------------------------------------------
def series(output, column, factor=1.0):
    """Return column * factor if the column exists, else None."""
    if column in output.colmap:
        return output.get_all(column) * factor
    return None


def line(ax, x, y, color, style, label, lw=2.0):
    """Plot only if both x and y are available."""
    if x is not None and y is not None:
        ax.plot(x, y, style, color=color, linewidth=lw, label=label)


def annotate_settings(ax):
    ax.text(
        0.02, 0.02, SETTINGS_NOTE,
        transform=ax.transAxes, fontsize=8, va="bottom", ha="left",
        bbox=dict(boxstyle="round", facecolor="#f3f3f3", edgecolor="#cccccc"),
    )


# ------------------------------------------------------------
# individual figures
# ------------------------------------------------------------
def plot_porosity_densification(ax, out, gold):
    """Porosity components (left) and densification factor (right)."""
    x = series(out, X_COL)
    xg = series(gold, X_COL)

    # left axis: porosity components (%)
    line(ax, x, series(out, "Porosity (/)", 100), "#1f77b4", "-", "Total porosity")
    line(ax, x, series(out, "Residual porosity (/)", 100), "#2ca02c", "-", "Residual porosity")
    line(ax, x, series(out, "Open porosity (/)", 1000), "#9467bd", "-", "Open porosity ×10")
    # gold overlay
    line(ax, xg, series(gold, "Porosity (/)", 100), "#1f77b4", ":", None, lw=1.5)
    line(ax, xg, series(gold, "Residual porosity (/)", 100), "#2ca02c", ":", None, lw=1.5)
    line(ax, xg, series(gold, "Open porosity (/)", 1000), "#9467bd", ":", None, lw=1.5)

    ax.set_xlabel(X_COL)
    ax.set_ylabel("Porosity (%)")
    ax.set_title("Porosity evolution & densification")
    ax.grid(True, ls=":", alpha=0.6)

    # right axis: densification factor (%)
    ax2 = ax.twinx()
    line(ax2, x, series(out, "Densification factor (/)", 100), "#d62728", "-", "Densification factor")
    line(ax2, xg, series(gold, "Densification factor (/)", 100), "#d62728", ":", None, lw=1.5)
    ax2.set_ylabel("Densification factor (%)", color="#d62728")
    ax2.tick_params(axis="y", labelcolor="#d62728")

    # merged legend (current work only; gold shown dotted)
    h1, l1 = ax.get_legend_handles_labels()
    h2, l2 = ax2.get_legend_handles_labels()
    ax.legend(h1 + h2 + [GOLD_PROXY], l1 + l2 + [GOLD_PROXY.get_label()],
              loc="center right", fontsize=8)
    annotate_settings(ax)


def plot_grainboundary_venting(ax, out, gold):
    """FGR (left) and intergranular venting probability (right)."""
    x = series(out, X_COL)
    xg = series(gold, X_COL)

    # left axis: FGR (%)
    line(ax, x, series(out, "Fission gas release (/)", 100), "#ff7f0e", "-", "Fission gas release")
    line(ax, xg, series(gold, "Fission gas release (/)", 100), "#ff7f0e", ":", None, lw=1.5)
    ax.set_xlabel(X_COL)
    ax.set_ylabel("Fission gas release (%)", color="#ff7f0e")
    ax.tick_params(axis="y", labelcolor="#ff7f0e")
    ax.set_title("Athermal ML grain-boundary release")
    ax.grid(True, ls=":", alpha=0.6)

    # right axis: venting probability (%)
    ax2 = ax.twinx()
    line(ax2, x, series(out, "Intergranular venting probability (/)", 100), "#17becf", "-",
         "Venting probability")
    line(ax2, xg, series(gold, "Intergranular venting probability (/)", 100), "#17becf", ":",
         None, lw=1.5)
    ax2.set_ylabel("Intergranular venting probability (%)", color="#17becf")
    ax2.tick_params(axis="y", labelcolor="#17becf")

    h1, l1 = ax.get_legend_handles_labels()
    h2, l2 = ax2.get_legend_handles_labels()
    ax.legend(h1 + h2 + [GOLD_PROXY], l1 + l2 + [GOLD_PROXY.get_label()],
              loc="center right", fontsize=8)
    annotate_settings(ax)


# ------------------------------------------------------------
# main
# ------------------------------------------------------------
def main():
    case = os.path.dirname(__file__)
    outdir = os.path.join(case, "figures")
    os.makedirs(outdir, exist_ok=True)

    out = load_output(case)
    gold = load_gold(case)

    # --- individual figures ---
    fig, ax = plt.subplots(figsize=(8, 6))
    plot_porosity_densification(ax, out, gold)
    fig.tight_layout()
    fig.savefig(os.path.join(outdir, "porosity_densification.png"), dpi=180)

    fig, ax = plt.subplots(figsize=(8, 6))
    plot_grainboundary_venting(ax, out, gold)
    fig.tight_layout()
    fig.savefig(os.path.join(outdir, "grainboundary_venting.png"), dpi=180)

    # --- combined overview for reports ---
    fig, (axl, axr) = plt.subplots(1, 2, figsize=(15, 6))
    plot_porosity_densification(axl, out, gold)
    plot_grainboundary_venting(axr, out, gold)
    fig.suptitle("Open porosity — evolution, densification and athermal ML venting (1273 K)",
                 fontsize=12)
    fig.tight_layout(rect=(0, 0, 1, 0.96))
    fig.savefig(os.path.join(outdir, "overview.png"), dpi=180)

    plt.show()


if __name__ == "__main__":
    main()
