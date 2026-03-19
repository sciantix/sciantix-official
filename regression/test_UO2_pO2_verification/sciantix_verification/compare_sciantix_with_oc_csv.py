#!/usr/bin/env python3

from __future__ import annotations

"""Compare SCIANTIX sweep results against standalone OpenCalphad CSV exports.

The script loads the concatenated SCIANTIX sweep produced in the parent folder,
parses OpenCalphad CSV files named ``test_<temperature>K_<region>.csv``, and
creates overlay plots of oxygen partial pressure as a function of O/U ratio.

Only plotting is performed at the moment; the output/summary path constants are
kept because they may be useful for a future merged-data export.
"""

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
SIGNED_LOG_PO2_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_log_pO2_error.png"
ABSOLUTE_LOG_PO2_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_log_pO2_error_absolute.png"
RELATIVE_LOG_PO2_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_log_pO2_error_relative_percent.png"
POTENTIAL_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_potential_error_percent.png"


def load_sciantix_data() -> pd.DataFrame:
    """Load the parent-folder sweep summary and normalize key column types."""
    frame = pd.read_csv(SCIANTIX_SWEEP_PATH, sep="\t")
    frame = frame.loc[:, ~frame.columns.str.startswith("Unnamed:")].copy()
    frame["Temperature (K)"] = frame["Temperature (K)"].astype(float)
    frame["O/U ratio (/)"] = frame["O/U ratio (/)"].astype(float)
    frame["System pressure (Pa)"] = frame["System pressure (Pa)"].astype(float)
    return frame


def load_oc_csv_data() -> pd.DataFrame:
    """Load all standalone OpenCalphad CSV files and derive thermochemical fields."""
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

    # OpenCalphad provides oxygen activity; derive pO2 and oxygen potential from it.
    frame["OC pO2 (MPa)"] = (frame["OC oxygen activity"] ** 2) * REFERENCE_PRESSURE_MPA
    return frame


def add_legends(ax, temperatures: list[float], colors: dict[float, object]) -> None:
    """Add separate legends for temperatures and data sources."""
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], lw=2, label=f"{int(temp)} K")
        for temp in temperatures
    ]
    model_handles = [
        Line2D([0], [0], color="black", lw=1.5, linestyle="-",  marker="s", markersize=2 ,label="SCIANTIX + OpenCalphad"),
        Line2D([0], [0], color="black", marker="o", linestyle="None", markersize=6, label="OpenCalphad alone"),
    ]
    first = ax.legend(handles=temperature_handles, loc="upper left", ncol=2, fontsize=8, title="Temperature")
    ax.add_artist(first)
    ax.legend(handles=model_handles, loc="lower right", fontsize=8, title="Series")


def plot_partial_pressure(frame1: pd.DataFrame, frame2: pd.DataFrame) -> None:
    """Overlay SCIANTIX and OpenCalphad pO2 trends, with two y-axis styles."""
    styles = {
        "Fuel oxygen partial pressure (MPa)": ("-", "s"),
        "Fuel oxygen partial pressure - CALPHAD (MPa)": (":", "^"),
    }
    temperatures = sorted(frame1["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    # Duplicate the figure with the Gueneau-style y-range used in the report plots.
    fig, ax = plt.subplots(figsize=(12, 8))
    for temperature, group in frame1.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        for column, (linestyle, marker) in styles.items():
            valid = group[group[column] > 0.0]
            if valid.empty:
                continue
            value = np.log10(valid[column] / REFERENCE_PRESSURE_MPA)
            if column == "Fuel oxygen partial pressure (MPa)":
                ax.plot(
                    valid["O/U ratio (/)"],
                    value,
                    color=colors[temperature],
                    linestyle=linestyle,
                    linewidth=1.5,
                    marker=marker,
                    markersize=2
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
    ax.set_ylabel(r"$\log_{10}(p_{O_2})$ (bar)")
    ax.set_xlim([1.90, 2.20])
    ax.set_ylim([-22, -2])
    ax.set_yticks(range(-22, 0, 2))
    ax.set_title("SCIANTIX vs standalone OpenCalphad")
    ax.grid(True, alpha=0.3)
    add_legends(ax, temperatures, colors)
    fig.tight_layout()
    fig.savefig(SCRIPT_DIR / "sciantix_vs_oc_csv_pO2.png", dpi=300)
    plt.close(fig)


def interpolate_sciantix_to_oc_points(
    sciantix_frame: pd.DataFrame,
    oc_frame: pd.DataFrame,
) -> pd.DataFrame:
    """Interpolate SCIANTIX final pO2 onto the OpenCalphad O/U grid."""
    rows: list[pd.DataFrame] = []

    for temperature, oc_group in oc_frame.groupby("Temperature (K)", sort=True):
        sci_group = sciantix_frame[
            sciantix_frame["Temperature (K)"] == temperature
        ].sort_values("O/U ratio (/)")
        sci_group = sci_group[sci_group["Fuel oxygen partial pressure (MPa)"] > 0.0].copy()
        if len(sci_group) < 2:
            continue

        oc_group = oc_group.sort_values("O/U ratio (/)")
        oc_group = (
            oc_group.groupby(["Temperature (K)", "O/U ratio (/)"], as_index=False)["OC pO2 (MPa)"]
            .mean()
            .sort_values("O/U ratio (/)")
        )
        oc_group = oc_group[oc_group["OC pO2 (MPa)"] > 0.0].copy()
        if oc_group.empty:
            continue

        sci_x = sci_group["O/U ratio (/)"].to_numpy()
        sci_log_p = np.log10(sci_group["Fuel oxygen partial pressure (MPa)"].to_numpy() / REFERENCE_PRESSURE_MPA)

        within_range = oc_group["O/U ratio (/)"].between(sci_x.min(), sci_x.max())
        oc_valid = oc_group.loc[within_range].copy()
        if oc_valid.empty:
            continue

        oc_x = oc_valid["O/U ratio (/)"].to_numpy()
        interpolated_log_p = np.interp(oc_x, sci_x, sci_log_p)
        oc_valid["Interpolated SCIANTIX log10(pO2/p_ref)"] = interpolated_log_p
        oc_valid["Interpolated SCIANTIX pO2 (MPa)"] = REFERENCE_PRESSURE_MPA * (10.0 ** interpolated_log_p)
        oc_valid["OpenCalphad log10(pO2/p_ref)"] = np.log10(oc_valid["OC pO2 (MPa)"] / REFERENCE_PRESSURE_MPA)
        oc_valid["log10(pO2/p_ref) error"] = (
            oc_valid["Interpolated SCIANTIX log10(pO2/p_ref)"]
            - oc_valid["OpenCalphad log10(pO2/p_ref)"]
        )
        oc_valid["Absolute log10(pO2/p_ref) error"] = oc_valid["log10(pO2/p_ref) error"].abs()
        denominator = oc_valid["OpenCalphad log10(pO2/p_ref)"].abs()
        oc_valid["Relative log10(pO2/p_ref) error (%)"] = np.where(
            denominator > 0.0,
            oc_valid["Absolute log10(pO2/p_ref) error"] / denominator * 100.0,
            np.nan,
        )
        rows.append(oc_valid)

    if not rows:
        raise RuntimeError("No overlapping temperature/O-U ranges found for SCIANTIX interpolation.")

    aligned = pd.concat(rows, ignore_index=True)
    return aligned.sort_values(["Temperature (K)", "O/U ratio (/)"]).reset_index(drop=True)


def _rounded_upper_limit(values: pd.Series, step: float, floor: float) -> float:
    """Round an upper bound upward to a clean plotting limit."""
    finite_values = values.replace([np.inf, -np.inf], np.nan).dropna()
    if finite_values.empty:
        return floor
    value_max = finite_values.max()
    return max(floor, math.ceil(value_max / step) * step)


def plot_signed_log_partial_pressure_error(frame: pd.DataFrame) -> None:
    """Plot the signed SCIANTIX-versus-OpenCalphad log10(pO2/p_ref) error."""
    temperatures = sorted(frame["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots(figsize=(12, 8))
    for temperature, group in frame.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        ax.plot(
            group["O/U ratio (/)"],
            group["log10(pO2/p_ref) error"],
            color=colors[temperature],
            linewidth=1.5,
            marker="o",
            markersize=3.0,
        )

    ax.axhline(0.0, color="black", linestyle="--", linewidth=1.0)
    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\Delta \log_{10}(p_{O_2}/p_{\mathrm{ref}})$ (-)")
    ax.set_xlim([1.90, 2.20])
    error_max = frame["log10(pO2/p_ref) error"].abs().max()
    y_limit = max(0.1, math.ceil(error_max / 0.02) * 0.02)
    ax.set_ylim([-y_limit, y_limit])
    ax.set_title("Interpolated SCIANTIX log(pO2) error vs standalone OpenCalphad")
    ax.grid(True, alpha=0.3)
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], lw=2, label=f"{int(temp)} K")
        for temp in temperatures
    ]
    ax.legend(handles=temperature_handles, loc="best", ncol=2, fontsize=8, title="Temperature")
    fig.tight_layout()
    fig.savefig(SIGNED_LOG_PO2_ERROR_PLOT_PATH, dpi=300)
    plt.close(fig)


def plot_absolute_log_partial_pressure_error(frame: pd.DataFrame) -> None:
    """Plot the absolute SCIANTIX-versus-OpenCalphad log10(pO2/p_ref) error."""
    temperatures = sorted(frame["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots(figsize=(12, 8))
    for temperature, group in frame.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        ax.plot(
            group["O/U ratio (/)"],
            group["Absolute log10(pO2/p_ref) error"],
            color=colors[temperature],
            linewidth=1.5,
            marker="o",
            markersize=3.0,
        )

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$|\Delta \log_{10}(p_{O_2}/p_{\mathrm{ref}})|$ (-)")
    ax.set_xlim([1.90, 2.20])
    y_limit = _rounded_upper_limit(frame["Absolute log10(pO2/p_ref) error"], step=0.02, floor=0.1)
    ax.set_ylim([0.0, y_limit])
    ax.set_title("Absolute SCIANTIX log(pO2) error vs standalone OpenCalphad")
    ax.grid(True, alpha=0.3)
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], lw=2, label=f"{int(temp)} K")
        for temp in temperatures
    ]
    ax.legend(handles=temperature_handles, loc="best", ncol=2, fontsize=8, title="Temperature")
    fig.tight_layout()
    fig.savefig(ABSOLUTE_LOG_PO2_ERROR_PLOT_PATH, dpi=300)
    plt.close(fig)


def plot_relative_log_partial_pressure_error(frame: pd.DataFrame) -> None:
    """Plot the relative SCIANTIX-versus-OpenCalphad log10(pO2/p_ref) error."""
    temperatures = sorted(frame["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots(figsize=(12, 8))
    valid_frame = frame.dropna(subset=["Relative log10(pO2/p_ref) error (%)"])
    for temperature, group in valid_frame.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        ax.plot(
            group["O/U ratio (/)"],
            group["Relative log10(pO2/p_ref) error (%)"],
            color=colors[temperature],
            linewidth=1.5,
            marker="o",
            markersize=3.0,
        )

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"Relative $|\Delta \log_{10}(p_{O_2}/p_{\mathrm{ref}})|$ (%)")
    ax.set_xlim([1.90, 2.20])
    y_limit = _rounded_upper_limit(valid_frame["Relative log10(pO2/p_ref) error (%)"], step=1.0, floor=5.0)
    ax.set_ylim([0.0, y_limit])
    ax.set_title("Relative SCIANTIX log(pO2) error vs standalone OpenCalphad")
    ax.grid(True, alpha=0.3)
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], lw=2, label=f"{int(temp)} K")
        for temp in temperatures
    ]
    ax.legend(handles=temperature_handles, loc="best", ncol=2, fontsize=8, title="Temperature")
    fig.tight_layout()
    fig.savefig(RELATIVE_LOG_PO2_ERROR_PLOT_PATH, dpi=300)
    plt.close(fig)


def main() -> None:
    """Generate the SCIANTIX versus OpenCalphad partial-pressure comparison plots."""
    sciantix_frame = load_sciantix_data()
    oc_frame = load_oc_csv_data()
    plot_partial_pressure(sciantix_frame, oc_frame)
    aligned_frame = interpolate_sciantix_to_oc_points(sciantix_frame, oc_frame)
    aligned_frame.to_csv(MERGED_OUTPUT_PATH, sep="\t", index=False)
    summary = aligned_frame.groupby("Temperature (K)", as_index=False).agg(
        count=("log10(pO2/p_ref) error", "count"),
        signed_mean=("log10(pO2/p_ref) error", "mean"),
        signed_median=("log10(pO2/p_ref) error", "median"),
        signed_min=("log10(pO2/p_ref) error", "min"),
        signed_max=("log10(pO2/p_ref) error", "max"),
        absolute_mean=("Absolute log10(pO2/p_ref) error", "mean"),
        absolute_median=("Absolute log10(pO2/p_ref) error", "median"),
        absolute_min=("Absolute log10(pO2/p_ref) error", "min"),
        absolute_max=("Absolute log10(pO2/p_ref) error", "max"),
        relative_mean_percent=("Relative log10(pO2/p_ref) error (%)", "mean"),
        relative_median_percent=("Relative log10(pO2/p_ref) error (%)", "median"),
        relative_min_percent=("Relative log10(pO2/p_ref) error (%)", "min"),
        relative_max_percent=("Relative log10(pO2/p_ref) error (%)", "max"),
    )
    summary.to_csv(SUMMARY_OUTPUT_PATH, sep="\t", index=False)
    plot_signed_log_partial_pressure_error(aligned_frame)
    plot_absolute_log_partial_pressure_error(aligned_frame)
    plot_relative_log_partial_pressure_error(aligned_frame)


if __name__ == "__main__":
    main()
