"""
White (2004): swelling parity plots split by ramp type, as in
Cappellari et al., JNM 617 (2025) 156116, Fig. 4 and Fig. 10.

Groups are derived from the thermal histories: the max heating rate is
strongly bimodal (>7 K/s vs <0.6 K/s). Long-hold specimens are named in
the paper (Sec. 3.1) and override the rate-based split.
author: Giovanni Zullo
"""
import os
import re
import glob
import numpy as np
import matplotlib.pyplot as plt

from parity_plot import extract_last, load_experimental
import sys
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from testing.core.common import load_output, load_gold

LONG_HOLD = {"4135", "4136", "4140"}          # Cappellari et al. (2025), Sec. 3.1
FAST_THRESHOLD = 1.0                          # K/s, sits in the bimodal gap
COL = "Intergranular gas swelling (/)"
STYLE = {"Fast ramp": "tab:blue", "Slow ramp": "tab:green", "Long hold": "tab:red"}


def ramp_group(case):
    if re.search(r"_(\d+)-", case).group(1) in LONG_HOLD:
        return "Long hold"
    d = np.loadtxt(os.path.join(case, "input_history.txt"))
    rate = np.diff(d[:, 1]) / np.maximum(np.diff(d[:, 0] * 3600), 1e-9)
    return "Fast ramp" if rate.max() > FAST_THRESHOLD else "Slow ramp"


def diagonals(ax, lo, hi):
    x = np.array([lo, hi])
    ax.plot(x, x, "k-", lw=0.8)
    for f, lab in ((2.0, "x2"), (0.5, "/2")):
        ax.plot(x, f * x, "k--", lw=0.6, alpha=0.5)
    ax.set(xscale="log", yscale="log", xlim=(lo, hi), ylim=(lo, hi))
    ax.set_aspect("equal")
    ax.grid(True, which="both", alpha=0.15)


def main():
    root = os.path.dirname(os.path.abspath(__file__))
    outdir = os.path.join(root, "figures")
    os.makedirs(outdir, exist_ok=True)

    names, values = load_experimental("ig_swelling.txt")
    exp, new, old, grp = [], [], [], []

    for case in sorted(glob.glob(os.path.join(root, "test_White2004_*"))):
        idx = np.where(names == os.path.basename(case))[0]
        if not len(idx):
            print(f"[WARNING] no experimental swelling for {os.path.basename(case)}")
            continue
        exp.append(values[idx][0])
        new.append(extract_last(load_output(case), COL) * 100)
        old.append(extract_last(load_gold(case), COL) * 100)
        grp.append(ramp_group(case))

    exp, new, old, grp = map(np.array, (exp, new, old, grp))
    mad = lambda y: np.median(np.abs(y - exp))

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(11, 5.2))

    for g, c in STYLE.items():
        m = grp == g
        ax1.plot(exp[m], new[m], "o", ms=5, color=c, label=f"{g} ({m.sum()})")
    diagonals(ax1, 0.1, 10)
    ax1.set(xlabel="Experimental swelling (%)", ylabel="Calculated swelling (%)")
    ax1.legend(loc="upper left", fontsize=8)

    ax2.plot(exp, old, "^", ms=5, mfc="none", color="k",
             label=f"SCIANTIX 2.0 (MAD {mad(old):.2f}), semi-empirical burst release")
    ax2.plot(exp, new, "o", ms=5, color="k",
             label=f"SCIANTIX 2.5 (MAD {mad(new):.2f}), physics-based burst release")
    diagonals(ax2, 0.1, 10)
    ax2.set(xlabel="Measured swelling (%)", ylabel="Calculated swelling (%)")
    ax2.legend(loc="upper left", fontsize=8)

    fig.tight_layout()
    path = os.path.join(outdir, "white_ramp_groups.png")
    fig.savefig(path, dpi=200)
    print(f"Saved: {path}")

    print(f"\n{'group':<12}{'n':>4}{'MAD':>8}{'bias':>8}")
    for g in STYLE:
        m = grp == g
        e = new[m] - exp[m]
        print(f"{g:<12}{m.sum():>4}{np.median(np.abs(e)):>8.3f}{np.median(e):>8.3f}")
    print(f"{'all':<12}{len(exp):>4}{mad(new):>8.3f}{np.median(new - exp):>8.3f}")
    print(f"{'SCIANTIX2.0':<12}{len(exp):>4}{mad(old):>8.3f}{np.median(old - exp):>8.3f}")


if __name__ == "__main__":
    main()
