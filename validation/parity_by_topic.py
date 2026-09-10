"""
Cross-group parity plots, one figure per physical phenomenon rather than per
experimental campaign: how well the model holds on a given scale across
independent datasets.

Experimental values live in <group>/data/<file>, keyed either by case name
(white, kashibe) or by temperature (baker, cornell); both are resolved by
substring match against the case directory names.

author: Giovanni Zullo
"""
import os
import glob
import sys
import numpy as np
import matplotlib.pyplot as plt

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from testing.core.common import load_output

ROOT = os.path.dirname(os.path.abspath(__file__))

# quantity -> (output column, factor applied to the calculated value, axis label)
QUANTITY = {
    "intragranular radius":   ("Intragranular bubble radius (m)", 1.0, "bubble radius (m)"),
    "intragranular density":  ("Intragranular bubble concentration (bub/m3)", 1.0, "bubble density (bub/m$^3$)"),
    "intragranular swelling": ("Intragranular gas bubble swelling (/)", 100.0, "gaseous swelling (%)"),
    "intergranular swelling": ("Intergranular gas swelling (/)", 100.0, "gaseous swelling (%)"),
}

# topic -> quantity -> [(group, data file)]
TOPICS = {
    "intragranular": {
        "intragranular radius":   [("baker", "ig_radius.txt"), ("cornell", "ig_radius.txt"),
                                   ("kashibe", "intragranular_radius.txt")],
        "intragranular density":  [("baker", "ig_density.txt"), ("cornell", "ig_density.txt"),
                                   ("kashibe", "intragranular_density.txt")],
        "intragranular swelling": [("baker", "ig_swelling.txt")],
    },
    "intergranular": {
        "intergranular swelling": [("white", "ig_swelling.txt"),
                                   ("kashibe", "intergranular_swelling.txt")],
    },
}

COLOR = {"baker": "tab:blue", "cornell": "tab:orange", "kashibe": "tab:green", "white": "tab:red"}


def read_data(group, basename):
    """Read '<key> <value>' pairs, skipping comments and blank lines."""
    pairs = []
    with open(os.path.join(ROOT, group, "data", basename)) as f:
        for line in f:
            if line.strip() and not line.lstrip().startswith("#"):
                key, value = line.split()[:2]
                pairs.append((key, float(value)))
    return pairs


def collect(group, basename, column, factor):
    """Pair each experimental value with the calculated one from its case."""
    cases = sorted(glob.glob(os.path.join(ROOT, group, "test_*")))
    exp, calc = [], []
    for key, value in read_data(group, basename):
        match = [c for c in cases if key in os.path.basename(c)]
        if len(match) != 1:
            print(f"  [skip] {group}/{key}: {len(match)} matching cases")
            continue
        output = load_output(match[0])
        header = [h.strip() for h in output.header]
        if column not in header:
            print(f"  [skip] {group}/{key}: no column '{column}'")
            continue
        exp.append(value)
        calc.append(output.data[-1, header.index(column)] * factor)
    return np.array(exp), np.array(calc)


def panel(ax, quantity, sources):
    column, factor, label = QUANTITY[quantity]
    lo, hi = np.inf, -np.inf

    for group, basename in sources:
        exp, calc = collect(group, basename, column, factor)
        keep = (exp > 0) & (calc > 0)
        exp, calc = exp[keep], calc[keep]
        if not exp.size:
            continue
        mad = 100 * np.median(np.abs(calc / exp - 1.0))
        ax.plot(exp, calc, "o", ms=5, color=COLOR[group],
                label=f"{group} ({exp.size}), MAD {mad:.0f}%")
        lo, hi = min(lo, exp.min(), calc.min()), max(hi, exp.max(), calc.max())

    lo, hi = 0.5 * lo, 2.0 * hi
    line = np.array([lo, hi])
    ax.plot(line, line, "-", color="#777777", lw=0.9)
    ax.plot(line, 2 * line, "--", color="#777777", lw=0.7)
    ax.plot(line, 0.5 * line, "--", color="#777777", lw=0.7)
    ax.set(xscale="log", yscale="log", xlim=(lo, hi), ylim=(lo, hi),
           xlabel=f"experimental {label}", ylabel=f"calculated {label}")
    ax.set_aspect("equal")
    ax.grid(True, which="both", ls=":", alpha=0.4)
    ax.legend(fontsize=8, loc="upper left")


def main():
    outdir = os.path.join(ROOT, "figures")
    os.makedirs(outdir, exist_ok=True)

    for topic, quantities in TOPICS.items():
        print(f"== {topic}")
        fig, axes = plt.subplots(1, len(quantities), figsize=(5.2 * len(quantities), 5.2), squeeze=False)
        for ax, quantity in zip(axes[0], quantities):
            panel(ax, quantity, quantities[quantity])
        fig.tight_layout()
        path = os.path.join(outdir, f"parity_{topic}.png")
        fig.savefig(path, dpi=200)
        plt.close(fig)
        print(f"Saved: {path}")


if __name__ == "__main__":
    main()
