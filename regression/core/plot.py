"""
sciantix regression suite
author: Giovanni Zullo
"""

import os
import numpy as np
import matplotlib.pyplot as plt

def parity_plot(exp, calc_gold, calc_test, quantity, title, outdir, calc_extra=None, label_extra=None):
    """
    Generates a parity plot comparing Experimental, Gold, and Test results.
    
    Args:
        exp: Experimental values (x-axis)
        calc_gold: Gold standard values
        calc_test: Current test values
        quantity: Name of quantity for filename (e.g. 'swelling')
        title: Plot title
        outdir: Directory to save the plot
        calc_extra: Optional third dataset (e.g. 'no GPR')
        label_extra: Label for third dataset
    """
    plt.figure(figsize=(6, 5))

    # Scatter Gold
    plt.scatter(exp, calc_gold, 
                facecolors="none", edgecolors="brown",
                marker="^", s=40, label="gold")

    # Scatter Test
    plt.scatter(exp, calc_test,
                facecolors="green", edgecolors="none",
                marker="o", s=35, alpha=0.8, label="test")
    
    # Scatter Extra (Optional)
    if calc_extra is not None:
        lbl = label_extra if label_extra else "extra"
        plt.scatter(exp, calc_extra,
                    facecolors="blue", edgecolors="none",
                    marker="s", s=35, alpha=0.8, label=lbl)

    # Determine plot limits for 1:1 lines
    arrays = [exp, calc_gold, calc_test]
    if calc_extra is not None:
        arrays.append(calc_extra)

    # Filter valid values for min/max
    vmin = min([np.min(a[np.isfinite(a)]) for a in arrays if a.size > 0])
    vmax = max([np.max(a[np.isfinite(a)]) for a in arrays if a.size > 0])
    
    # Handling log scale issues (values <= 0)
    if vmin <= 0:
        # heuristic: uses smallest positive value or a default lower bound
        pos_vals = []
        for a in arrays:
            pos_vals.extend(a[a > 0])
        if pos_vals:
            vmin = min(pos_vals)
        else:
            vmin = 1e-3 # fallback

    xmin = 0.5 * vmin
    xmax = 2.0 * vmax
    
    xline = np.logspace(np.log10(xmin), np.log10(xmax), 200)
    
    # 1:1 and factor 2 lines
    plt.plot(xline, xline, "-", color="#777777")
    plt.plot(xline, 2*xline, "--", color="#777777")
    plt.plot(xline, 0.5*xline, "--", color="#777777")

    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel("experimental")
    plt.ylabel("calculated")
    plt.title(title)

    plt.grid(True, which="both", ls=":")
    plt.legend()

    out = os.path.join(outdir, f"parity_{quantity}.png")
    plt.tight_layout()
    plt.savefig(out, dpi=180)
    plt.close()
    print("Saved:", out)


def history_plot(exp_x, exp_y, calc_x, calc_y, xlabel, ylabel, title, outpath):
    """
    Plot history (Quantity vs Time/Burnup) comparison.
    exp_x, exp_y: Experimental data vectors
    calc_x, calc_y: Calculated data vectors
    """
    plt.figure(figsize=(6, 5))

    plt.plot(calc_x, calc_y, "-", color="black", linewidth=2, label="SCIANTIX")
    plt.scatter(exp_x, exp_y, color="red", marker="o", s=40, label="Experimental", zorder=3)

    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title)
    plt.grid(True, which="both", ls=":")
    plt.legend()

    plt.tight_layout()
    plt.savefig(outpath, dpi=180)
    plt.close()
    print("Saved:", outpath)
