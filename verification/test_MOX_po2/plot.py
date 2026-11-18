#!/usr/bin/env python3
"""
sciantix testing suite
author: Elisa Cappellari

Reads the committed verification/test_MOX_po2/T_<T>K_q_<Pu>/output.txt cases
(see generate_cases.py) and draws one figure per q-value -- SCIANTIX-Kato vs
SCIANTIX-CALPHAD oxygen potential against O/U ratio, all three temperatures
overlaid. Does not run SCIANTIX and does not regenerate the sweep -- matches
every other group's plot.py convention (reads outputs python -m testing.runner
already produced).
"""

import glob
import os

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
import pandas as pd

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
FIGURES_DIR = os.path.join(SCRIPT_DIR, "figures")

TEMPERATURE_COLORS = {1400: "#1f77b4", 1800: "#ff7f0e", 2200: "#2ca02c"}


def load_case(case_dir):
    df = pd.read_csv(os.path.join(case_dir, "output.txt"), sep="\t")
    df["O/U ratio (/)"] = df["Stoichiometry deviation (/)"] + 2.0
    return df


def discover_cases():
    """Group each T_<T>K_q_<Pu>/ case's output by q-value, then temperature."""
    by_q = {}
    for case_dir in sorted(glob.glob(os.path.join(SCRIPT_DIR, "T_*_q_*"))):
        name = os.path.basename(case_dir)
        _, temp_part, _, q_part = name.split("_")
        temperature_k = int(temp_part.rstrip("K"))
        q_value = int(q_part) / 100.0
        by_q.setdefault(q_value, {})[temperature_k] = load_case(case_dir)
    return by_q


def make_potential_plot(q_value, cases):
    """Oxygen potential (Kato + CALPHAD) vs O/U ratio, all temperatures overlaid."""
    fig, ax = plt.subplots(figsize=(8, 6))

    for temperature_k, df in sorted(cases.items()):
        color = TEMPERATURE_COLORS.get(temperature_k, "gray")
        ax.plot(df["O/U ratio (/)"], df["Fuel oxygen potential - Kato (KJ/mol)"],
                "^-", color=color, markersize=4, linewidth=1.5, label=f"{temperature_k} K (Kato)")
        ax.plot(df["O/U ratio (/)"], df["Fuel oxygen potential - CALPHAD (KJ/mol)"],
                "s--", color=color, markersize=4, linewidth=1.5, label=f"{temperature_k} K (CALPHAD)")

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel("Oxygen potential (kJ/mol)")
    ax.set_title(f"MOX pO2 verification — q = {q_value:.2f}")
    ax.grid(True, ls=":", alpha=0.6)
    ax.legend(fontsize=8)
    fig.tight_layout()

    out_path = os.path.join(FIGURES_DIR, f"oxygen_potential_q_{int(round(q_value * 100))}.png")
    fig.savefig(out_path, dpi=180)
    plt.close(fig)
    return out_path


def main():
    os.makedirs(FIGURES_DIR, exist_ok=True)
    for png_path in glob.glob(os.path.join(FIGURES_DIR, "*.png")):
        os.remove(png_path)

    for q_value, cases in sorted(discover_cases().items()):
        out_path = make_potential_plot(q_value, cases)
        print(f"Saved: {out_path}")


if __name__ == "__main__":
    main()
