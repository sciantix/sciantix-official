#!/usr/bin/env python3

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd


PHASES = ["bcc_a2", "hcp_a3", "fcc_a1", "sigma", "liquid"]
METALS = ["MO", "PD", "RH", "RU", "TC"]


def parse_args():
    parser = argparse.ArgumentParser(
        description="Plot five-metals total amount by phase vs time."
    )
    parser.add_argument("--input", default="thermochemistry_output.txt")
    parser.add_argument(
        "--location",
        default="at grain boundary",
        choices=["in grain", "at grain boundary", "in the gap", "matrix"],
    )
    parser.add_argument("--output", default="five_metals_phase_totals_vs_time.png")
    return parser.parse_args()


def main():
    args = parse_args()
    input_path = Path(args.input)
    if not input_path.exists():
        raise FileNotFoundError(f"Input not found: {input_path}")

    df = pd.read_csv(input_path, sep="\t", low_memory=False)
    df.columns = df.columns.str.strip()
    for c in df.columns:
        df[c] = pd.to_numeric(df[c], errors="coerce")
    df = df.dropna(subset=["Time (h)"]).sort_values("Time (h)")

    phase_totals = {}
    for phase in PHASES:
        cols = []
        for m in METALS:
            col = f"{m} ({phase}, {args.location}) (mol/m3)"
            if col in df.columns:
                cols.append(col)
        phase_totals[phase] = df[cols].sum(axis=1) if cols else 0.0

    plt.figure(figsize=(10, 6))
    for phase in PHASES:
        plt.plot(df["Time (h)"], phase_totals[phase], label=phase, linewidth=2)

    plt.xlabel("Time (h)")
    plt.ylabel("Total five-metals moles in phase (mol/m3)")
    plt.title(f"Five-metals phase totals vs time ({args.location})")
    plt.grid(True, linestyle="--", alpha=0.5)
    plt.legend()
    plt.tight_layout()
    plt.savefig(args.output, dpi=200)
    print(f"Saved plot: {args.output}")
    print("Final values by phase:")
    for phase in PHASES:
        val = phase_totals[phase].iloc[-1] if hasattr(phase_totals[phase], "iloc") else 0.0
        print(f"  {phase}: {val:.6g} mol/m3")
    print("Peak values by phase:")
    for phase in PHASES:
        series = phase_totals[phase] if hasattr(phase_totals[phase], "idxmax") else pd.Series([0.0])
        idx = series.idxmax()
        peak = float(series.max())
        t_peak = float(df.loc[idx, "Time (h)"]) if idx in df.index else 0.0
        print(f"  {phase}: max={peak:.6g} mol/m3 at t={t_peak:.6g} h")


if __name__ == "__main__":
    main()
