#!/usr/bin/env python3

import argparse
import re
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd


FIVE_METALS = ["MO", "PD", "RH", "RU", "TC"]


def parse_args():
    parser = argparse.ArgumentParser(
        description="Plot five-metals composition vs time from thermochemistry_output.txt"
    )
    parser.add_argument(
        "--input",
        default="thermochemistry_output.txt",
        help="Path to thermochemistry output table (default: thermochemistry_output.txt)",
    )
    parser.add_argument(
        "--location",
        default="at grain boundary",
        choices=["in grain", "at grain boundary", "in the gap", "matrix"],
        help="Location tag to extract from thermochemistry output columns",
    )
    parser.add_argument(
        "--output",
        default="five_metals_composition_vs_time.png",
        help="Output figure path",
    )
    return parser.parse_args()


def build_element_column_map(columns, location):
    pattern = re.compile(
        r"^\s*([A-Za-z0-9:+_-]+)\s*\(([^,]+),\s*([^)]+)\)\s*\(mol/m3\)\s*$",
        re.IGNORECASE,
    )

    element_cols = {metal: [] for metal in FIVE_METALS}
    for col in columns:
        match = pattern.match(col.strip())
        if not match:
            continue

        species = match.group(1).strip().upper()
        col_location = match.group(3).strip().lower()
        if col_location != location.lower():
            continue

        if species in element_cols:
            element_cols[species].append(col)

    return element_cols


def main():
    args = parse_args()
    input_path = Path(args.input)
    if not input_path.exists():
        raise FileNotFoundError(f"Cannot find input file: {input_path}")

    df = pd.read_csv(input_path, sep="\t", low_memory=False)
    df.columns = df.columns.str.strip()

    if "Time (h)" not in df.columns:
        raise KeyError("Column 'Time (h)' not found in thermochemistry output.")

    for col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")

    df = df.dropna(subset=["Time (h)"]).copy()
    df = df.sort_values("Time (h)")

    element_cols = build_element_column_map(df.columns, args.location)

    element_moles = pd.DataFrame(index=df.index)
    for metal in FIVE_METALS:
        cols = element_cols[metal]
        if cols:
            element_moles[metal] = df[cols].sum(axis=1)
        else:
            element_moles[metal] = 0.0

    total = element_moles.sum(axis=1)
    fractions = element_moles.div(total.where(total > 0), axis=0).fillna(0.0)

    time_h = df["Time (h)"]

    fig, (ax_abs, ax_frac) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)

    for metal in FIVE_METALS:
        ax_abs.plot(time_h, element_moles[metal], linewidth=2, label=metal)
        ax_frac.plot(time_h, fractions[metal], linewidth=2, label=metal)

    ax_abs.set_ylabel("Moles (mol/m3)")
    ax_abs.set_title(f"Five-metals amount vs time ({args.location})")
    ax_abs.grid(True, linestyle="--", alpha=0.5)
    ax_abs.legend(ncol=5, fontsize=9)

    ax_frac.set_xlabel("Time (h)")
    ax_frac.set_ylabel("Mole fraction (-)")
    ax_frac.set_title(f"Five-metals composition vs time ({args.location})")
    ax_frac.set_ylim(0.0, 1.0)
    ax_frac.grid(True, linestyle="--", alpha=0.5)

    plt.tight_layout()
    plt.savefig(args.output, dpi=200)
    print(f"Saved plot: {args.output}")


if __name__ == "__main__":
    main()
