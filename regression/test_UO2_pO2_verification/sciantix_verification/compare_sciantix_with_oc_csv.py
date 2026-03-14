#!/usr/bin/env python3

from __future__ import annotations

import math
import os
import re
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp")
os.environ.setdefault("XDG_CACHE_HOME", "/tmp")

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import numpy as np
import pandas as pd


REFERENCE_PRESSURE_PA = 1e5
REFERENCE_PRESSURE_MPA = REFERENCE_PRESSURE_PA / 1.0e6
GAS_CONSTANT = 8.31446261815324

SCRIPT_DIR = Path(__file__).resolve().parent
SCIANTIX_SWEEP_PATH = Path(
    "/home/ecappellari/transparant/sciantix-official/regression/test_UO2_pO2_verification/temperature_sweep_summary.tsv"
)

MERGED_OUTPUT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv.tsv"
SUMMARY_OUTPUT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_summary.tsv"

POTENTIAL_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_potential.png"
PO2_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_pO2_error_percent.png"
POTENTIAL_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_potential_error_percent.png"


def load_sciantix_data() -> pd.DataFrame:
    frame = pd.read_csv(SCIANTIX_SWEEP_PATH, sep="\t")
    frame = frame.loc[:, ~frame.columns.str.startswith("Unnamed:")].copy()
    frame["Temperature (K)"] = frame["Temperature (K)"].astype(float)
    frame["O/U ratio (/)"] = frame["O/U ratio (/)"].astype(float)
    frame["System pressure (Pa)"] = frame["System pressure (Pa)"].astype(float)
    return frame


def load_oc_csv_data() -> pd.DataFrame:
    rows: list[pd.DataFrame] = []

    for path in sorted(SCRIPT_DIR.glob("test_*K_*.csv")):
        match = re.fullmatch(r"test_(\d+)K_(ipo|iper)\.csv", path.name)
        if not match:
            continue

        temperature_k = float(match.group(1))
        region = match.group(2)

        frame = pd.read_csv(path).rename(columns=lambda name: name.strip().strip('"'))
        frame = frame.rename(columns={"N(O)": "O/U ratio (/)", "AC(O)": "OC oxygen activity"})
        frame = frame[["O/U ratio (/)", "OC oxygen activity"]].copy()
        frame["Temperature (K)"] = temperature_k
        frame["Region"] = region
        rows.append(frame)

    if not rows:
        raise RuntimeError(f"No standalone OpenCalphad CSV files found in {SCRIPT_DIR}")

    frame = pd.concat(rows, ignore_index=True)
    frame["O/U ratio (/)"] = frame["O/U ratio (/)"].astype(float)
    frame["OC oxygen activity"] = frame["OC oxygen activity"].astype(float)
    frame = frame.sort_values(["Temperature (K)", "O/U ratio (/)", "Region"]).reset_index(drop=True)

    frame["OC pO2 (MPa)"] = (frame["OC oxygen activity"] ** 2) * REFERENCE_PRESSURE_PA / 1.0e6
    frame["OC oxygen potential (KJ/mol)"] = (
        2.0
        * GAS_CONSTANT
        * frame["Temperature (K)"]
        * frame["OC oxygen activity"].map(lambda value: math.log(value))
        / 1000.0
    )
    return frame


def add_legends(ax, temperatures: list[float], colors: dict[float, object]) -> None:
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], lw=2, label=f"{int(temp)} K")
        for temp in temperatures
    ]
    model_handles = [
        Line2D([0], [0], color="black", lw=1.8, linestyle="-", marker="s", markersize=5, label="SCIANTIX Final"),
        Line2D([0], [0], color="black", lw=1.8, linestyle=":", marker="^", markersize=5, label="SCIANTIX CALPHAD"),
        Line2D([0], [0], color="black", marker="o", linestyle="None", markersize=6, label="OpenCalphad CSV"),
    ]
    first = ax.legend(handles=temperature_handles, loc="upper left", ncol=2, fontsize=8, title="Temperature")
    ax.add_artist(first)
    ax.legend(handles=model_handles, loc="lower right", fontsize=8, title="Series")


def plot_partial_pressure(frame1: pd.DataFrame, frame2: pd.DataFrame) -> None:
    styles = {
        "Fuel oxygen partial pressure (MPa)": ("-", "s"),
        "Fuel oxygen partial pressure - CALPHAD (MPa)": (":", "^"),
    }
    temperatures = sorted(frame1["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots(figsize=(12, 8))
    for temperature, group in frame1.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        for column, (linestyle, marker) in styles.items():
            valid = group[group[column] > 0.0]
            if valid.empty:
                continue
            value = np.log10(valid[column] / REFERENCE_PRESSURE_MPA)
            ax.plot(
                valid["O/U ratio (/)"],
                value,
                color=colors[temperature],
                linestyle=linestyle,
                linewidth=1.5,
                marker=marker,
                markersize=4.0,
                markerfacecolor="white",
                markeredgewidth=0.9,
            )

    for temperature, group in frame2.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        valid = group[group["OC pO2 (MPa)"] > 0.0]
        if valid.empty:
            continue
        value = np.log10(valid["OC pO2 (MPa)"] / REFERENCE_PRESSURE_MPA)
        ax.plot(
            valid["O/U ratio (/)"],
            value,
            color=colors[temperature],
            marker="o",
            linestyle="None",
            markersize=6.0,
        )

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\log_{10}(p_{O_2} / p_{ref})$")
    ax.set_title("SCIANTIX vs standalone OpenCalphad")
    ax.grid(True, alpha=0.3)
    add_legends(ax, temperatures, colors)
    fig.tight_layout()
    fig.savefig( SCRIPT_DIR / "sciantix_vs_oc_csv_pO2.png", dpi=300)
    plt.close(fig)

    # as Gueneau
    fig, ax = plt.subplots(figsize=(12, 8))
    for temperature, group in frame1.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        for column, (linestyle, marker) in styles.items():
            valid = group[group[column] > 0.0]
            if valid.empty:
                continue
            value = np.log10(valid[column] / REFERENCE_PRESSURE_MPA)
            ax.plot(
                valid["O/U ratio (/)"],
                value,
                color=colors[temperature],
                linestyle=linestyle,
                linewidth=1.5,
                marker=marker,
                markersize=4.0,
                markerfacecolor="white",
                markeredgewidth=0.9,
            )

    for temperature, group in frame2.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        valid = group[group["OC pO2 (MPa)"] > 0.0]
        if valid.empty:
            continue
        value = np.log10(valid["OC pO2 (MPa)"] / REFERENCE_PRESSURE_MPA)
        ax.plot(
            valid["O/U ratio (/)"],
            value,
            color=colors[temperature],
            marker="o",
            linestyle="None",
            markersize=6.0,
        )

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\log_{10}(p_{O_2} / p_{ref})$")
    ax.set_ylim([-22, -2])
    ax.set_yticks(np.linspace(-22, -2, 11))
    ax.set_title("SCIANTIX vs standalone OpenCalphad")
    ax.grid(True, alpha=0.3)
    add_legends(ax, temperatures, colors)
    fig.tight_layout()
    fig.savefig( SCRIPT_DIR / "sciantix_vs_oc_csv_pO2_2.png", dpi=300)
    plt.close(fig)


def main() -> None:
    sciantix_frame = load_sciantix_data()
    oc_frame = load_oc_csv_data()
    plot_partial_pressure(sciantix_frame, oc_frame)

if __name__ == "__main__":
    main()
