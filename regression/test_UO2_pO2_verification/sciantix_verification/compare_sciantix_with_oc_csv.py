#!/usr/bin/env python3

from __future__ import annotations

"""Compare SCIANTIX UO2 verification results against external reference data.

This script performs two complementary checks for the UO2 oxygen-potential
verification:

1. SCIANTIX versus Thermo-Calc CSV export data.
2. SCIANTIX Blackburn output versus the analytical Blackburn formula.

For each comparison it writes merged tables, summary TSV files, human-readable
text reports, and the verification plots used in the folder documentation.
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

plt.rcParams.update({
    "figure.figsize": (10, 7),
    "font.size": 12,
    "axes.labelsize": 15,
    "axes.titlesize": 12,
    "xtick.labelsize": 12,
    "ytick.labelsize": 12,
    "legend.fontsize": 12,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.5,
    "grid.linestyle": "--",
    "lines.linewidth": 2,
    "lines.markersize": 4,
    "legend.frameon": False,
})

REFERENCE_PRESSURE_PA = 1e5
REFERENCE_PRESSURE_MPA = REFERENCE_PRESSURE_PA / 1.0e6
GAS_CONSTANT = 8.31446261815324
OU_EXCLUDED_VALUE = 2.0
OU_EXCLUDED_TOLERANCE = 1.0e-3
OU_MIN = 1.90
OU_MAX = 2.20

SCRIPT_DIR = Path(__file__).resolve().parent
THERMOCALC_TEMPERATURES_DIR = SCRIPT_DIR.parent / "TEMPERATURES_THERMOCALC"
SCIANTIX_SWEEP_PATH = Path(
    "/home/ecappellari/transparant/sciantix-official/regression/test_UO2_pO2_verification/temperature_sweep_summary.tsv"
)

MERGED_OUTPUT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv.tsv"
SUMMARY_OUTPUT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_summary.tsv"
SUMMARY_REPORT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_summary.txt"

POTENTIAL_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_potential.png"
SIGNED_LOG_PO2_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_log_pO2_error.png"
ABSOLUTE_LOG_PO2_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_log_pO2_error_absolute.png"
RELATIVE_LOG_PO2_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_log_pO2_error_relative_percent.png"
POTENTIAL_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_oc_csv_potential_error_percent.png"

BLACKBURN_MERGED_OUTPUT_PATH = SCRIPT_DIR / "sciantix_vs_blackburn_formula.tsv"
BLACKBURN_SUMMARY_OUTPUT_PATH = SCRIPT_DIR / "sciantix_vs_blackburn_formula_summary.tsv"
BLACKBURN_SUMMARY_REPORT_PATH = SCRIPT_DIR / "sciantix_vs_blackburn_formula_summary.txt"
BLACKBURN_PO2_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_blackburn_formula_pO2.png"
BLACKBURN_SIGNED_LOG_PO2_ERROR_PLOT_PATH = SCRIPT_DIR / "sciantix_vs_blackburn_formula_log_pO2_error.png"
BLACKBURN_ABSOLUTE_LOG_PO2_ERROR_PLOT_PATH = (
    SCRIPT_DIR / "sciantix_vs_blackburn_formula_log_pO2_error_absolute.png"
)
BLACKBURN_RELATIVE_LOG_PO2_ERROR_PLOT_PATH = (
    SCRIPT_DIR / "sciantix_vs_blackburn_formula_log_pO2_error_relative_percent.png"
)


def load_sciantix_data() -> pd.DataFrame:
    """Load the parent-folder sweep summary and normalize key column types."""
    frame = pd.read_csv(SCIANTIX_SWEEP_PATH, sep="\t")
    frame = frame.loc[:, ~frame.columns.str.startswith("Unnamed:")].copy()
    frame["Temperature (K)"] = frame["Temperature (K)"].astype(float)
    frame["O/U ratio (/)"] = frame["O/U ratio (/)"].astype(float)
    frame["System pressure (Pa)"] = frame["System pressure (Pa)"].astype(float)
    frame["Stoichiometry deviation (/)"] = frame["Stoichiometry deviation (/)"].astype(float)
    return frame


def load_oc_csv_data() -> pd.DataFrame:
    """Load reference files from TEMPERATURES_THERMOCALC and derive thermochemical fields."""
    rows: list[pd.DataFrame] = []

    for path in sorted(THERMOCALC_TEMPERATURES_DIR.glob("*.csv")):
        if not re.fullmatch(r"\d+\.csv", path.name):
            continue

        frame = pd.read_csv(path, sep="\t").rename(columns=lambda name: name.strip().strip('"'))
        expected_columns = {"Temperature [K]", "Mole percent O", "Mole percent U", "Activity of O"}
        missing_columns = expected_columns.difference(frame.columns)
        if missing_columns:
            missing_sorted = ", ".join(sorted(missing_columns))
            raise RuntimeError(f"{path.name} is missing required columns: {missing_sorted}")

        frame = frame.rename(
            columns={
                "Temperature [K]": "Temperature (K)",
                "Mole percent O": "Mole percent O",
                "Mole percent U": "Mole percent U",
                "Activity of O": "OC oxygen activity",
            }
        )
        frame["Temperature (K)"] = frame["Temperature (K)"].astype(float)
        frame["Mole percent O"] = frame["Mole percent O"].astype(float)
        frame["Mole percent U"] = frame["Mole percent U"].astype(float)
        frame["OC oxygen activity"] = frame["OC oxygen activity"].astype(float)

        # O/U = n_O / n_U and mole percents are proportional to n_O and n_U.
        frame["O/U ratio (/)"] = np.where(
            frame["Mole percent U"] > 0.0,
            frame["Mole percent O"] / frame["Mole percent U"],
            np.nan,
        )

        # OpenCalphad/Thermo-Calc provides oxygen activity; derive pO2 from it.
        frame["OC pO2 (MPa)"] = (frame["OC oxygen activity"] ** 2) * REFERENCE_PRESSURE_MPA
        rows.append(frame[["Temperature (K)", "O/U ratio (/)", "OC oxygen activity", "OC pO2 (MPa)"]])

    if not rows:
        raise RuntimeError(
            "No reference CSV files matching '<temperature>.csv' found in "
            f"{THERMOCALC_TEMPERATURES_DIR}"
        )

    merged = pd.concat(rows, ignore_index=True)
    merged = merged.loc[
        ~np.isclose(merged["O/U ratio (/)"], OU_EXCLUDED_VALUE, atol=OU_EXCLUDED_TOLERANCE, rtol=0.0)
    ].copy()
    merged = merged.sort_values(["Temperature (K)", "O/U ratio (/)"]).reset_index(drop=True)
    return merged


def add_legends(ax, temperatures: list[float], colors: dict[float, object]) -> None:
    """Add separate legends for temperatures and data sources."""
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], lw=2, label=f"{int(temp)} K")
        for temp in temperatures
    ]
    model_handles = [
        Line2D([0], [0], color="black", linestyle="-", label="SCIANTIX + OpenCalphad"),
        Line2D([0], [0], color="black", marker="o", linestyle="None", label="Thermo-Calc"),
    ]
    first = ax.legend(handles=temperature_handles, loc="lower right", ncol=2, title="Temperature")
    ax.add_artist(first)
    ax.legend(handles=model_handles, loc="upper left", title="Series")


def plot_partial_pressure(frame1: pd.DataFrame, frame2: pd.DataFrame) -> None:
    """Overlay SCIANTIX and Thermo-Calc pO2 trends, with two y-axis styles."""
    styles = {
        "Fuel oxygen partial pressure (MPa)": ("-", "s"),
        "Fuel oxygen partial pressure - CALPHAD (MPa)": (":", "^"),
    }
    temperatures = sorted(frame1["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    # Duplicate the figure with the Gueneau-style y-range used in the report plots.
    fig, ax = plt.subplots()
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
                    linestyle=linestyle
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
        )

    sci_log = np.log10(
        frame1.loc[frame1["Fuel oxygen partial pressure (MPa)"] > 0.0, "Fuel oxygen partial pressure (MPa)"]
        / REFERENCE_PRESSURE_MPA
    )
    ref_log = np.log10(frame2.loc[frame2["OC pO2 (MPa)"] > 0.0, "OC pO2 (MPa)"] / REFERENCE_PRESSURE_MPA)
    y_limits = _padded_limits(pd.concat([sci_log, ref_log], ignore_index=True), fallback=(-30.0, 0.0))

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\log_{10}(p_{O_2})$ (bar)")
    ax.set_xlim([OU_MIN, OU_MAX])
    ax.set_ylim(list(y_limits))
    ax.grid(True, alpha=0.3)
    add_legends(ax, temperatures, colors)
    fig.tight_layout()
    fig.savefig(SCRIPT_DIR / "sciantix_vs_oc_csv_pO2.png")
    plt.close(fig)


def compute_blackburn_formula(frame: pd.DataFrame) -> pd.DataFrame:
    """Recompute Blackburn pO2 from the analytical expression."""
    blackburn = frame.copy()
    x = blackburn["Stoichiometry deviation (/)"].to_numpy(dtype=float)
    temperature = blackburn["Temperature (K)"].to_numpy(dtype=float)
    ratio_term = x * (2.0 + x) / (1.0 - x)

    analytical_pressure = np.full(len(blackburn), np.nan, dtype=float)
    valid = (temperature > 0.0) & (ratio_term > 0.0)
    ln_p = (
        2.0 * np.log(ratio_term[valid])
        + 108.0 * np.square(x[valid])
        - 32700.0 / temperature[valid]
        + 9.92
    )
    analytical_pressure[valid] = 0.1013 * np.exp(ln_p)

    blackburn["Blackburn analytical pO2 (MPa)"] = analytical_pressure
    blackburn["Blackburn analytical log10(pO2/p_ref)"] = np.where(
        analytical_pressure > 0.0,
        np.log10(analytical_pressure / 0.1013),
        np.nan,
    )
    return blackburn


def plot_blackburn_partial_pressure(frame: pd.DataFrame) -> None:
    """Overlay SCIANTIX Blackburn output with the reconstructed Blackburn formula."""
    temperatures = sorted(frame["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots()
    for temperature, group in frame.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        sciantix_valid = group[group["Fuel oxygen partial pressure - Blackburn (MPa)"] > 0.0]
        if not sciantix_valid.empty:
            ax.plot(
                sciantix_valid["O/U ratio (/)"],
                np.log10(sciantix_valid["Fuel oxygen partial pressure - Blackburn (MPa)"] / REFERENCE_PRESSURE_MPA),
                color=colors[temperature],
                linestyle="-",
            )

        analytical_valid = group[group["Blackburn analytical pO2 (MPa)"] > 0.0]
        if not analytical_valid.empty:
            ax.plot(
                analytical_valid["O/U ratio (/)"],
                analytical_valid["Blackburn analytical log10(pO2/p_ref)"],
                color=colors[temperature],
                marker="o",
                linestyle="None",
            )

    sciantix_log = np.log10(
        frame.loc[
            frame["Fuel oxygen partial pressure - Blackburn (MPa)"] > 0.0,
            "Fuel oxygen partial pressure - Blackburn (MPa)",
        ]
        / REFERENCE_PRESSURE_MPA
    )
    analytical_log = frame.loc[
        frame["Blackburn analytical pO2 (MPa)"] > 0.0,
        "Blackburn analytical log10(pO2/p_ref)",
    ]
    y_limits = _padded_limits(pd.concat([sciantix_log, analytical_log], ignore_index=True), fallback=(-30.0, 0.0))

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\log_{10}(p_{O_2})$ (bar)")
    ax.set_xlim([OU_MIN, OU_MAX])
    ax.set_ylim(list(y_limits))
    ax.grid(True, alpha=0.3)

    temperature_handles = [
        Line2D([0], [0], color=colors[temp], lw=2, label=f"{int(temp)} K")
        for temp in temperatures
    ]
    model_handles = [
        Line2D([0], [0], color="black", linestyle="-", label="SCIANTIX + Blackburn model"),
        Line2D([0], [0], color="black", marker="o", linestyle="None", label="Analytical Blackburn formula"),
    ]
    first = ax.legend(handles=temperature_handles, loc="lower right", ncol=2, title="Temperature")
    ax.add_artist(first)
    ax.legend(handles=model_handles, loc="upper left", title="Series")

    fig.tight_layout()
    fig.savefig(BLACKBURN_PO2_PLOT_PATH)
    plt.close(fig)


def interpolate_sciantix_to_oc_points(
    sciantix_frame: pd.DataFrame,
    oc_frame: pd.DataFrame,
) -> pd.DataFrame:
    """Interpolate SCIANTIX final pO2 onto the Thermo-Calc O/U grid."""
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
        oc_valid["pO2 error (MPa)"] = oc_valid["Interpolated SCIANTIX pO2 (MPa)"] - oc_valid["OC pO2 (MPa)"]
        oc_valid["Absolute pO2 error (MPa)"] = oc_valid["pO2 error (MPa)"].abs()
        oc_valid["Relative pO2 error (%)"] = np.where(
            oc_valid["OC pO2 (MPa)"] > 0.0,
            oc_valid["Absolute pO2 error (MPa)"] / oc_valid["OC pO2 (MPa)"] * 100.0,
            np.nan,
        )
        rows.append(oc_valid)

    if not rows:
        raise RuntimeError("No overlapping temperature/O-U ranges found for SCIANTIX interpolation.")

    aligned = pd.concat(rows, ignore_index=True)
    return aligned.sort_values(["Temperature (K)", "O/U ratio (/)"]).reset_index(drop=True)


def compare_blackburn_formula(frame: pd.DataFrame) -> pd.DataFrame:
    """Compare SCIANTIX Blackburn output against the reconstructed analytical formula."""
    aligned = frame.copy()
    valid = (
        (aligned["Fuel oxygen partial pressure - Blackburn (MPa)"] > 0.0)
        & (aligned["Blackburn analytical pO2 (MPa)"] > 0.0)
    )
    aligned = aligned.loc[valid].copy()
    if aligned.empty:
        raise RuntimeError("No valid Blackburn verification points were found in the SCIANTIX sweep.")

    aligned["SCIANTIX Blackburn log10(pO2/p_ref)"] = np.log10(
        aligned["Fuel oxygen partial pressure - Blackburn (MPa)"] / REFERENCE_PRESSURE_MPA
    )
    aligned["Blackburn formula log10(pO2/p_ref)"] = aligned["Blackburn analytical log10(pO2/p_ref)"]
    aligned["log10(pO2/p_ref) error"] = (
        aligned["SCIANTIX Blackburn log10(pO2/p_ref)"]
        - aligned["Blackburn formula log10(pO2/p_ref)"]
    )
    aligned["Absolute log10(pO2/p_ref) error"] = aligned["log10(pO2/p_ref) error"].abs()
    denominator = aligned["Blackburn formula log10(pO2/p_ref)"].abs()
    aligned["Relative log10(pO2/p_ref) error (%)"] = np.where(
        denominator > 0.0,
        aligned["Absolute log10(pO2/p_ref) error"] / denominator * 100.0,
        np.nan,
    )
    aligned["pO2 error (MPa)"] = (
        aligned["Fuel oxygen partial pressure - Blackburn (MPa)"] - aligned["Blackburn analytical pO2 (MPa)"]
    )
    aligned["Absolute pO2 error (MPa)"] = aligned["pO2 error (MPa)"].abs()
    aligned["Relative pO2 error (%)"] = np.where(
        aligned["Blackburn analytical pO2 (MPa)"] > 0.0,
        aligned["Absolute pO2 error (MPa)"] / aligned["Blackburn analytical pO2 (MPa)"] * 100.0,
        np.nan,
    )
    return aligned.sort_values(["Temperature (K)", "O/U ratio (/)"]).reset_index(drop=True)


def _rounded_upper_limit(values: pd.Series, step: float, floor: float) -> float:
    """Round an upper bound upward to a clean plotting limit."""
    finite_values = values.replace([np.inf, -np.inf], np.nan).dropna()
    if finite_values.empty:
        return floor
    value_max = finite_values.max()
    return max(floor, math.ceil(value_max / step) * step)


def _series_min_max(values: pd.Series) -> tuple[float, float] | None:
    """Return finite min/max, or None when no finite values exist."""
    finite_values = values.replace([np.inf, -np.inf], np.nan).dropna()
    if finite_values.empty:
        return None
    return float(finite_values.min()), float(finite_values.max())


def _padded_limits(values: pd.Series, fallback: tuple[float, float], pad_fraction: float = 0.05) -> tuple[float, float]:
    """Build y-limits with a small padding, guarding against zero-width ranges."""
    bounds = _series_min_max(values)
    if bounds is None:
        return fallback

    value_min, value_max = bounds
    span = value_max - value_min
    if span <= 0.0:
        epsilon = max(abs(value_max) * 0.05, 1.0e-12)
        return value_min - epsilon, value_max + epsilon

    pad = span * pad_fraction
    return value_min - pad, value_max + pad


def build_metric_summary(frame: pd.DataFrame, temperature_column: str = "Temperature (K)") -> pd.DataFrame:
    """Aggregate standard signed/absolute/relative pO2 error metrics by temperature."""
    return frame.groupby(temperature_column, as_index=False).agg(
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


def write_summary_report(title: str, frame: pd.DataFrame, summary: pd.DataFrame, output_path: Path) -> None:
    """Write a short text report with headline verification metrics."""
    overall_count = len(frame)
    signed_mean = frame["log10(pO2/p_ref) error"].mean()
    signed_max_abs = frame["log10(pO2/p_ref) error"].abs().max()
    absolute_mean = frame["Absolute log10(pO2/p_ref) error"].mean()
    relative_mean = frame["Relative log10(pO2/p_ref) error (%)"].mean()
    relative_max = frame["Relative log10(pO2/p_ref) error (%)"].max()

    lines = [
        title,
        "=" * len(title),
        "",
        f"Compared points: {overall_count}",
        f"Mean signed log10(pO2/p_ref) error: {signed_mean:.6e}",
        f"Mean absolute log10(pO2/p_ref) error: {absolute_mean:.6e}",
        f"Maximum absolute log10(pO2/p_ref) error: {signed_max_abs:.6e}",
        f"Mean relative log10(pO2/p_ref) error (%): {relative_mean:.6e}",
        f"Maximum relative log10(pO2/p_ref) error (%): {relative_max:.6e}",
        "",
        "Per-temperature summary:",
        summary.to_string(index=False),
        "",
    ]
    output_path.write_text("\n".join(lines))


def plot_signed_log_partial_pressure_error(frame: pd.DataFrame) -> None:
    """Plot the signed SCIANTIX-versus-Thermo-Calc log10(pO2/p_ref) error."""
    temperatures = sorted(frame["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots()
    for temperature, group in frame.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        ax.plot(
            group["O/U ratio (/)"],
            group["log10(pO2/p_ref) error"],
            color=colors[temperature],
            marker="o",
        )

    ax.axhline(0.0, color="black", linestyle="--")
    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\Delta \log_{10}(p_{O_2}/p_{\mathrm{ref}})$ (-)")
    ax.set_xlim([OU_MIN, OU_MAX])
    ax.set_ylim([-1, 1])
    ax.grid(True, alpha=0.3)
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], label=f"{int(temp)} K")
        for temp in temperatures
    ]
    ax.legend(handles=temperature_handles, loc="best", ncol=2, title="Temperature")
    fig.tight_layout()
    fig.savefig(SIGNED_LOG_PO2_ERROR_PLOT_PATH)
    plt.close(fig)


def plot_blackburn_signed_log_partial_pressure_error(frame: pd.DataFrame) -> None:
    """Plot the signed SCIANTIX-versus-Blackburn log10(pO2/p_ref) error."""
    temperatures = sorted(frame["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots()
    for temperature, group in frame.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        ax.plot(
            group["O/U ratio (/)"],
            group["log10(pO2/p_ref) error"],
            color=colors[temperature],
            marker="o",
        )

    ax.axhline(0.0, color="black", linestyle="--")
    signed_values = frame["log10(pO2/p_ref) error"].replace([np.inf, -np.inf], np.nan).dropna()
    y_limit = float(signed_values.abs().max()) if not signed_values.empty else 0.15
    y_limit = max(y_limit * 1.05, 1.0e-6)

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$\Delta \log_{10}(p_{O_2}/p_{\mathrm{ref}})$ (-)")
    ax.set_xlim([OU_MIN, OU_MAX])
    ax.set_ylim([-1, 1])
    ax.grid(True, alpha=0.3)
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], label=f"{int(temp)} K")
        for temp in temperatures
    ]
    ax.legend(handles=temperature_handles, loc="best", ncol=2, title="Temperature")
    fig.tight_layout()
    fig.savefig(BLACKBURN_SIGNED_LOG_PO2_ERROR_PLOT_PATH)
    plt.close(fig)


def plot_absolute_log_partial_pressure_error(frame: pd.DataFrame) -> None:
    """Plot the absolute SCIANTIX-versus-Thermo-Calc log10(pO2/p_ref) error."""
    temperatures = sorted(frame["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots()
    for temperature, group in frame.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        ax.plot(
            group["O/U ratio (/)"],
            group["Absolute log10(pO2/p_ref) error"],
            color=colors[temperature],
            marker="o",
        )


    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$|\Delta \log_{10}(p_{O_2}/p_{\mathrm{ref}})|$ (-)")
    ax.set_xlim([OU_MIN, OU_MAX])
    ax.set_ylim([0.0, 1.0])
    ax.grid(True, alpha=0.3)
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], lw=2, label=f"{int(temp)} K")
        for temp in temperatures
    ]
    ax.legend(handles=temperature_handles, loc="best", ncol=2, title="Temperature")
    fig.tight_layout()
    fig.savefig(ABSOLUTE_LOG_PO2_ERROR_PLOT_PATH)
    plt.close(fig)


def plot_blackburn_absolute_log_partial_pressure_error(frame: pd.DataFrame) -> None:
    """Plot the absolute SCIANTIX-versus-Blackburn log10(pO2/p_ref) error."""
    temperatures = sorted(frame["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots()
    for temperature, group in frame.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        ax.plot(
            group["O/U ratio (/)"],
            group["Absolute log10(pO2/p_ref) error"],
            color=colors[temperature],
            marker="o",
        )


    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"$|\Delta \log_{10}(p_{O_2}/p_{\mathrm{ref}})|$ (-)")
    ax.set_xlim([OU_MIN, OU_MAX])
    ax.set_ylim([0.0, 1.0])
    ax.grid(True, alpha=0.3)
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], label=f"{int(temp)} K")
        for temp in temperatures
    ]
    ax.legend(handles=temperature_handles, loc="best", ncol=2, title="Temperature")
    fig.tight_layout()
    fig.savefig(BLACKBURN_ABSOLUTE_LOG_PO2_ERROR_PLOT_PATH)
    plt.close(fig)


def plot_relative_log_partial_pressure_error(frame: pd.DataFrame) -> None:
    """Plot the relative SCIANTIX-versus-Thermo-Calc log10(pO2/p_ref) error."""
    temperatures = sorted(frame["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots()
    valid_frame = frame.dropna(subset=["Relative log10(pO2/p_ref) error (%)"])
    for temperature, group in valid_frame.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        ax.plot(
            group["O/U ratio (/)"],
            group["Relative log10(pO2/p_ref) error (%)"],
            color=colors[temperature],
            marker="o"
        )

    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"Relative $|\Delta \log_{10}(p_{O_2}/p_{\mathrm{ref}})|$ (%)")
    ax.set_xlim([OU_MIN, OU_MAX])
    ax.set_ylim([0.0, 100])
    ax.grid(True, alpha=0.3)
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], lw=2, label=f"{int(temp)} K")
        for temp in temperatures
    ]
    ax.legend(handles=temperature_handles, loc="best", ncol=2, title="Temperature")
    fig.tight_layout()
    fig.savefig(RELATIVE_LOG_PO2_ERROR_PLOT_PATH)
    plt.close(fig)


def plot_blackburn_relative_log_partial_pressure_error(frame: pd.DataFrame) -> None:
    """Plot the relative SCIANTIX-versus-Blackburn log10(pO2/p_ref) error."""
    temperatures = sorted(frame["Temperature (K)"].dropna().unique())
    cmap = plt.get_cmap("turbo", len(temperatures))
    colors = {temp: cmap(i) for i, temp in enumerate(temperatures)}

    fig, ax = plt.subplots()
    valid_frame = frame.dropna(subset=["Relative log10(pO2/p_ref) error (%)"])
    for temperature, group in valid_frame.groupby("Temperature (K)", sort=True):
        group = group.sort_values("O/U ratio (/)")
        ax.plot(
            group["O/U ratio (/)"],
            group["Relative log10(pO2/p_ref) error (%)"],
            color=colors[temperature],
            marker="o",
        )
    ax.set_xlabel("O/U ratio (-)")
    ax.set_ylabel(r"Relative $|\Delta \log_{10}(p_{O_2}/p_{\mathrm{ref}})|$ (%)")
    ax.set_xlim([OU_MIN, OU_MAX])
    ax.set_ylim([0.0, 100])
    ax.grid(True, alpha=0.3)
    temperature_handles = [
        Line2D([0], [0], color=colors[temp], label=f"{int(temp)} K")
        for temp in temperatures
    ]
    ax.legend(handles=temperature_handles, loc="best", ncol=2, title="Temperature")
    fig.tight_layout()
    fig.savefig(BLACKBURN_RELATIVE_LOG_PO2_ERROR_PLOT_PATH)
    plt.close(fig)

def main() -> None:
    """Generate tables, text summaries, and plots for the UO2 verification."""
    sciantix_frame = load_sciantix_data()
    oc_frame = load_oc_csv_data()
    oc_temperatures = set(oc_frame["Temperature (K)"].dropna().unique())
    sciantix_temperatures = set(sciantix_frame["Temperature (K)"].dropna().unique())
    common_temperatures = sorted(oc_temperatures.intersection(sciantix_temperatures))
    if not common_temperatures:
        raise RuntimeError(
            "No overlapping temperatures between SCIANTIX sweep and TEMPERATURES_THERMOCALC reference files."
        )

    sciantix_frame = sciantix_frame[sciantix_frame["Temperature (K)"].isin(common_temperatures)].copy()
    oc_frame = oc_frame[oc_frame["Temperature (K)"].isin(common_temperatures)].copy()
    sciantix_frame = sciantix_frame[sciantix_frame["O/U ratio (/)"].between(OU_MIN, OU_MAX)].copy()
    oc_frame = oc_frame[oc_frame["O/U ratio (/)"].between(OU_MIN, OU_MAX)].copy()
    plot_partial_pressure(sciantix_frame, oc_frame)
    aligned_frame = interpolate_sciantix_to_oc_points(sciantix_frame, oc_frame)
    aligned_frame.to_csv(MERGED_OUTPUT_PATH, sep="\t", index=False)
    summary = build_metric_summary(aligned_frame)
    summary.to_csv(SUMMARY_OUTPUT_PATH, sep="\t", index=False)
    write_summary_report(
        "SCIANTIX vs Thermo-Calc CSV",
        aligned_frame,
        summary,
        SUMMARY_REPORT_PATH,
    )
    plot_signed_log_partial_pressure_error(aligned_frame)
    plot_absolute_log_partial_pressure_error(aligned_frame)
    plot_relative_log_partial_pressure_error(aligned_frame)

    blackburn_frame = compute_blackburn_formula(sciantix_frame)
    plot_blackburn_partial_pressure(blackburn_frame)
    blackburn_aligned = compare_blackburn_formula(blackburn_frame)
    blackburn_aligned.to_csv(BLACKBURN_MERGED_OUTPUT_PATH, sep="\t", index=False)
    blackburn_summary = build_metric_summary(blackburn_aligned)
    blackburn_summary.to_csv(BLACKBURN_SUMMARY_OUTPUT_PATH, sep="\t", index=False)
    write_summary_report(
        "SCIANTIX Blackburn output vs analytical Blackburn formula",
        blackburn_aligned,
        blackburn_summary,
        BLACKBURN_SUMMARY_REPORT_PATH,
    )
    plot_blackburn_signed_log_partial_pressure_error(blackburn_aligned)
    plot_blackburn_absolute_log_partial_pressure_error(blackburn_aligned)
    plot_blackburn_relative_log_partial_pressure_error(blackburn_aligned)


if __name__ == "__main__":
    main()
