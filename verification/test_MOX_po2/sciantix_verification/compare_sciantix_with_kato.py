from __future__ import annotations

"""Compare SCIANTIX MOX pO2 results with explicit NEA Kato-equation points.

Source-based implementation from:
NEA/NSC/R(2024)1, Recommendations on Fuel Properties for Fuel Performance Codes,
Section 8.

The script writes:
- a merged SCIANTIX-versus-Kato table
- residual summary TSV files
- a compact text report with headline error metrics
- the verification plots stored in the parent folder
"""

import math
import os
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
os.environ.setdefault("MPLCONFIGDIR", str(SCRIPT_DIR / ".matplotlib"))

import matplotlib
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import matplotlib.colors as mcolors
import numpy as np
import pandas as pd


REFERENCE_PRESSURE_MPA = 0.1013
GAS_CONSTANT = 8.314
TEMPERATURE_COMPARE_COL = "Temperature compared (K)"
Q_COMPARE_COL = "q compared (-)"
TEMPERATURE_MIN_K = 753.0
TEMPERATURE_MAX_K = 2550.0
Q_MIN = 0.10
Q_MAX = 0.32
Q_TOLERANCE = 1.0e-3
OM_MIN = 1.92
OM_MAX = 2.08

ROOT_DIR = SCRIPT_DIR.parent
PLOTS_DIR = ROOT_DIR / "plots"
SUMMARY_PATH = ROOT_DIR / "temperature_sweep_summary.tsv"
LEGACY_SUMMARY_PATH = ROOT_DIR / "kato_sweep_summary.tsv"
COMPARISON_PATH = SCRIPT_DIR / "sciantix_vs_kato_comparison.tsv"
LEGACY_COMPARISON_PATH = SCRIPT_DIR / "kato_explicit_comparison.tsv"
RESIDUALS_PATH = SCRIPT_DIR / "sciantix_vs_kato_residuals.tsv"
LEGACY_RESIDUALS_PATH = SCRIPT_DIR / "kato_explicit_residuals.tsv"
SUMMARY_REPORT_PATH = SCRIPT_DIR / "sciantix_vs_kato_summary.txt"
LEGACY_SUMMARY_REPORT_PATH = SCRIPT_DIR / "kato_explicit_summary.txt"
EXPLICIT_POINTS_PATH = SCRIPT_DIR / "sciantix_vs_kato_points.tsv"
LEGACY_EXPLICIT_POINTS_PATH = SCRIPT_DIR / "kato_explicit_points.tsv"


PAPER_PALETTE = [
    "#736F3F", "#BFAE56", "#B29DA6", "#D9AF32", "#A66226", "#733426",
    "#737675", "#9D6953", "#363726", "#785C2D",
]

plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (10, 7),
    "font.family": "serif",
    "font.serif": ["Times New Roman", "Times", "Nimbus Roman", "DejaVu Serif"],
    "mathtext.fontset": "dejavuserif",
    "font.size": 20,
    "axes.labelsize": 20,
    "axes.titlesize": 20,
    "xtick.labelsize": 20,
    "ytick.labelsize": 20,
    "legend.fontsize": 20,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.5,
    "grid.linestyle": "--",
    "lines.linewidth": 3,
    "lines.markersize": 6,
    "legend.frameon": False,
    "axes.prop_cycle": plt.cycler(color=PAPER_PALETTE),
})
PLOT_MARKER_SIZE = 4

# Fixed normalization range shared across every MOXSCIANTIX verification plot so a
# given temperature always renders as the same viridis color in every figure.
VERIFICATION_TEMPERATURE_MIN_K = 1000.0
VERIFICATION_TEMPERATURE_MAX_K = 2400.0
SCIANTIX_KATO_MARKER = "^"
MODEL_CURVE_MARKEVERY = 0.06


def effective_pu_content(q_pu: float, q_am: float = 0.0, q_np: float = 0.0) -> float:
    """NEA Eq. (8.6): CPuMA = CPu + 2.5 CAm + 0.5 CNp."""
    return q_pu + 2.5 * q_am + 0.5 * q_np


def adjusted_temperature(
    temperature_actual: float,
    q_pu: float,
    q_am: float = 0.0,
    q_np: float = 0.0,
) -> float:
    """Apply the NEA temperature adjustment used with the Kato correlation."""
    q_eff = effective_pu_content(q_pu, q_am=q_am, q_np=q_np)
    exponent = -300.0 * (0.335 - q_eff)
    sigmoid_s = 1.0 / (1.0 + math.exp(exponent))
    return temperature_actual + (0.16 * sigmoid_s) * (1773.0 - temperature_actual)


def explicit_kato_om_from_pressure_ratio(
    temperature_actual: float,
    q_pu: float,
    pressure_ratio: float,
    q_am: float = 0.0,
    q_np: float = 0.0,
) -> float:
    """NEA Eq. (8.2) written explicitly as O/M = f(pO2, T, q)."""
    if pressure_ratio <= 0.0:
        return math.nan

    q_eff = effective_pu_content(q_pu, q_am=q_am, q_np=q_np)
    temperature_adjusted = adjusted_temperature(temperature_actual, q_pu, q_am=q_am, q_np=q_np)
    ln_po2 = math.log(pressure_ratio)

    log_v1 = -5.0 * (
        (44.0 + 55.8 * q_eff) / GAS_CONSTANT
        - 376000.0 / (GAS_CONSTANT * temperature_adjusted)
        - 0.5 * ln_po2
    )
    log_v2 = -5.0 * (
        0.5 * (68.8 + 131.3 * q_eff) / GAS_CONSTANT
        - 0.5 * 515000.0 / (GAS_CONSTANT * temperature_adjusted)
        - 0.25 * ln_po2
    )
    log_v3 = -5.0 * (
        (
            math.log(2.0)
            + (153.5 - 96.5 * q_eff + 331.0 * q_eff ** 2) / GAS_CONSTANT
            - 891000.0 / (GAS_CONSTANT * temperature_adjusted)
        ) / 3.0
        - ln_po2 / 3.0
    )
    log_v4 = -5.0 * math.log(0.5 * q_eff)

    max_log = max(log_v1, log_v2, log_v3, log_v4)
    sum_exp = (
        math.exp(log_v1 - max_log) +
        math.exp(log_v2 - max_log) +
        math.exp(log_v3 - max_log) +
        math.exp(log_v4 - max_log)
    )
    s_term = math.exp(-0.2 * (max_log + math.log(sum_exp)))
    term5 = math.exp(
        (-22.8 - 84.5 * q_eff) / GAS_CONSTANT
        + 105000.0 / (GAS_CONSTANT * temperature_adjusted)
        + 0.5 * ln_po2
    )
    return 2.0 - s_term + term5


def safe_log10(value: float) -> float:
    """Return log10(value) for positive values and NaN otherwise."""
    return math.log10(value) if value > 0.0 else math.nan


def safe_log(value: float) -> float:
    """Return ln(value) for positive values and NaN otherwise."""
    return math.log(value) if value > 0.0 else math.nan


def build_explicit_points(frame: pd.DataFrame) -> pd.DataFrame:
    """Sample the explicit Kato equation over pressure ratio for each (T, q)."""
    rows: list[pd.DataFrame] = []

    grouped = frame.groupby([TEMPERATURE_COMPARE_COL, Q_COMPARE_COL], as_index=False)
    for (temperature_k, q_value), group in grouped:
        sci_log_values = group["Kato log10(p/reference)"].replace([np.inf, -np.inf], np.nan).dropna()
        if sci_log_values.empty:
            log_min = -30.0
            log_max = 2.0
        else:
            log_min = min(-30.0, math.floor(float(sci_log_values.min())) - 2.0)
            log_max = max(2.0, math.ceil(float(sci_log_values.max())) + 2.0)

        log_grid = np.linspace(log_min, log_max, 4000)
        pressure_ratio = np.power(10.0, log_grid)
        om_values = np.array(
            [explicit_kato_om_from_pressure_ratio(temperature_k, q_value, ratio) for ratio in pressure_ratio],
            dtype=float,
        )
        potential_values = GAS_CONSTANT * 1.0e-3 * temperature_k * np.log(pressure_ratio)

        explicit_frame = pd.DataFrame({
            TEMPERATURE_COMPARE_COL: temperature_k,
            Q_COMPARE_COL: q_value,
            "Explicit log10(p/reference)": log_grid,
            "Explicit pressure ratio (-)": pressure_ratio,
            "Explicit oxygen partial pressure (MPa)": REFERENCE_PRESSURE_MPA * pressure_ratio,
            "Explicit O/M ratio (/)": om_values,
            "Explicit oxygen potential (KJ/mol)": potential_values,
        })
        explicit_frame = explicit_frame.replace([np.inf, -np.inf], np.nan).dropna(subset=["Explicit O/M ratio (/)"])
        rows.append(explicit_frame)

    explicit_points = pd.concat(rows, ignore_index=True)
    explicit_points.to_csv(EXPLICIT_POINTS_PATH, sep="\t", index=False)
    return explicit_points


def prepare_dataframe() -> pd.DataFrame:
    """Align SCIANTIX trajectory points with explicit NEA Kato reference points."""
    frame = pd.read_csv(SUMMARY_PATH, sep="\t")
    source_points = len(frame)
    # De-fragment blocks before adding many derived columns.
    frame = frame.copy()
    temperature_source = "Temperature (K)" if "Temperature (K)" in frame.columns else "Temperature target (K)"
    q_source = "q (-)" if "q (-)" in frame.columns else "q target (-)"
    frame[TEMPERATURE_COMPARE_COL] = pd.to_numeric(frame[temperature_source], errors="coerce")
    frame[Q_COMPARE_COL] = pd.to_numeric(frame[q_source], errors="coerce")

    frame["O/M ratio (/)"] = frame["Stoichiometry deviation (/)"] + 2.0
    in_comparison_range = (
        frame[TEMPERATURE_COMPARE_COL].between(TEMPERATURE_MIN_K, TEMPERATURE_MAX_K)
        & frame[Q_COMPARE_COL].between(Q_MIN - Q_TOLERANCE, Q_MAX + Q_TOLERANCE)
        & frame["O/M ratio (/)"].between(OM_MIN, OM_MAX)
    )
    frame = frame.loc[in_comparison_range].copy()
    comparison_range_points = len(frame)
    frame["Kato log10(p/reference)"] = (frame["Fuel oxygen partial pressure - Kato (MPa)"] / REFERENCE_PRESSURE_MPA).map(safe_log10)
    explicit_points = build_explicit_points(frame)

    aligned_rows: list[pd.DataFrame] = []
    for (temperature_k, q_value), group in frame.groupby([TEMPERATURE_COMPARE_COL, Q_COMPARE_COL], as_index=False):
        explicit_group = explicit_points[
            (explicit_points[TEMPERATURE_COMPARE_COL] == temperature_k) &
            (explicit_points[Q_COMPARE_COL] == q_value)
        ].sort_values("Explicit O/M ratio (/)")

        explicit_group = explicit_group.drop_duplicates(subset=["Explicit O/M ratio (/)"], keep="first")
        if explicit_group.empty:
            continue

        x_formula = explicit_group["Explicit O/M ratio (/)"].to_numpy(dtype=float)
        y_logp = explicit_group["Explicit log10(p/reference)"].to_numpy(dtype=float)
        y_pressure = explicit_group["Explicit oxygen partial pressure (MPa)"].to_numpy(dtype=float)
        y_potential = explicit_group["Explicit oxygen potential (KJ/mol)"].to_numpy(dtype=float)

        group = group.sort_values("O/M ratio (/)").copy()
        x_sci = group["O/M ratio (/)"].to_numpy(dtype=float)
        in_range = (x_sci >= x_formula.min()) & (x_sci <= x_formula.max())
        group = group.loc[in_range].copy()
        if group.empty:
            continue

        x_sci = group["O/M ratio (/)"].to_numpy(dtype=float)
        group["Explicit Kato log10(p/reference)"] = np.interp(x_sci, x_formula, y_logp)
        group["Explicit Kato oxygen partial pressure (MPa)"] = np.interp(x_sci, x_formula, y_pressure)
        group["Explicit Kato oxygen potential (KJ/mol)"] = np.interp(x_sci, x_formula, y_potential)
        aligned_rows.append(group)

    frame = pd.concat(aligned_rows, ignore_index=True).copy()
    frame.attrs["source_points"] = source_points
    frame.attrs["comparison_range_excluded_points"] = source_points - comparison_range_points
    frame.attrs["explicit_range_excluded_points"] = comparison_range_points - len(frame)

    frame["Delta log10(p/reference)"] = frame["Kato log10(p/reference)"] - frame["Explicit Kato log10(p/reference)"]
    frame["Absolute delta log10(p/reference)"] = frame["Delta log10(p/reference)"].abs()
    denominator_log_pressure = frame["Explicit Kato log10(p/reference)"].abs()
    frame["Relative delta log10(p/reference) (%)"] = np.where(
        denominator_log_pressure > 0.0,
        frame["Delta log10(p/reference)"] / denominator_log_pressure * 100.0,
        np.nan,
    )
    frame["Absolute relative delta log10(p/reference) (%)"] = frame["Relative delta log10(p/reference) (%)"].abs()
    frame["Delta oxygen potential (KJ/mol)"] = (
        frame["Fuel oxygen potential - Kato (KJ/mol)"] - frame["Explicit Kato oxygen potential (KJ/mol)"]
    )
    frame["Absolute delta oxygen potential (KJ/mol)"] = frame["Delta oxygen potential (KJ/mol)"].abs()
    denominator_potential = frame["Explicit Kato oxygen potential (KJ/mol)"].abs()
    frame["Relative delta oxygen potential (%)"] = np.where(
        denominator_potential > 0.0,
        frame["Delta oxygen potential (KJ/mol)"] / denominator_potential * 100.0,
        np.nan,
    )
    frame["Absolute relative delta oxygen potential (%)"] = frame["Relative delta oxygen potential (%)"].abs()

    frame.to_csv(COMPARISON_PATH, sep="\t", index=False)

    residuals = (
        frame.groupby([Q_COMPARE_COL, TEMPERATURE_COMPARE_COL], as_index=False)
        .agg({
            "Delta log10(p/reference)": lambda series: series.abs().max(),
            "Delta oxygen potential (KJ/mol)": lambda series: series.abs().max(),
        })
        .rename(columns={
            "Delta log10(p/reference)": "Max abs delta log10(p/reference)",
            "Delta oxygen potential (KJ/mol)": "Max abs delta oxygen potential (KJ/mol)",
        })
    )
    residuals.to_csv(RESIDUALS_PATH, sep="\t", index=False)
    return frame


def write_summary_report(frame: pd.DataFrame) -> None:
    """Write a short human-readable report with global and grouped MOX metrics."""
    overall_count = len(frame)
    source_count = int(frame.attrs.get("source_points", overall_count))
    comparison_range_excluded_count = int(frame.attrs.get("comparison_range_excluded_points", 0))
    explicit_range_excluded_count = int(frame.attrs.get("explicit_range_excluded_points", 0))
    exact_stoichiometry_points = int(np.isclose(frame["O/M ratio (/)"], 2.0, atol=1.0e-12, rtol=0.0).sum())
    signed_mean_potential = frame["Delta oxygen potential (KJ/mol)"].mean()
    mean_abs_potential = frame["Absolute delta oxygen potential (KJ/mol)"].mean()
    max_abs_potential = frame["Absolute delta oxygen potential (KJ/mol)"].max()
    mean_rel_potential = frame["Absolute relative delta oxygen potential (%)"].mean()
    max_rel_potential = frame["Absolute relative delta oxygen potential (%)"].max()
    grouped = (
        frame.groupby([Q_COMPARE_COL, TEMPERATURE_COMPARE_COL], as_index=False)
        .agg(
            count=("Delta oxygen potential (KJ/mol)", "count"),
            mean_abs_potential_error=("Absolute delta oxygen potential (KJ/mol)", "mean"),
            max_abs_potential_error=("Absolute delta oxygen potential (KJ/mol)", "max"),
        )
    )
    lines = [
        "SCIANTIX MOX verification vs explicit NEA Kato equation",
        "======================================================",
        "",
        f"Comparison ranges: T = {TEMPERATURE_MIN_K:g}-{TEMPERATURE_MAX_K:g} K, "
        f"Pu/M = {Q_MIN:.2f}-{Q_MAX:.2f}, O/M = {OM_MIN:.2f}-{OM_MAX:.2f}",
        f"Source points: {source_count}",
        f"Compared points: {overall_count}",
        f"Excluded points outside comparison ranges: {comparison_range_excluded_count}",
        f"Excluded points outside explicit Kato O/M interpolation range: {explicit_range_excluded_count}",
        f"Exact O/M = 2.0 compared points: {exact_stoichiometry_points}",
        f"Mean signed oxygen-potential error (KJ/mol): {signed_mean_potential:.6e}",
        f"Mean absolute oxygen-potential error (KJ/mol): {mean_abs_potential:.6e}",
        f"Maximum absolute oxygen-potential error (KJ/mol): {max_abs_potential:.6e}",
        f"Mean relative oxygen-potential error (%): {mean_rel_potential:.6e}",
        f"Max relative oxygen-potential error (%): {max_rel_potential:.6e}",
        "",
        "Per-(q, temperature) summary:",
        grouped.to_string(index=False),
        "",
    ]
    report_text = "\n".join(lines)
    SUMMARY_REPORT_PATH.write_text(report_text)


def temperature_color_map(temperatures_k: list[int]) -> dict[int, object]:
    """Assign a viridis color to each temperature, shared across the paper's figures."""
    norm = mcolors.Normalize(vmin=VERIFICATION_TEMPERATURE_MIN_K, vmax=VERIFICATION_TEMPERATURE_MAX_K)
    cmap = matplotlib.colormaps["viridis"]
    return {temperature_k: cmap(norm(temperature_k)) for temperature_k in temperatures_k}


def q_marker_map(q_values: list[float]) -> dict[float, str]:
    """Assign one marker style to each plutonium content q."""
    markers = ["o", "s", "^", "D", "v", "P", "X", "*"]
    return {q_value: markers[index % len(markers)] for index, q_value in enumerate(q_values)}


def q_tag(q_value: float) -> str:
    """Convert a q value into the filename tag used by the saved figures."""
    return f"{q_value:.2f}".replace(".", "p")

def add_model_legends(
    ax,
    temperatures_k: list[int],
    temperature_colors: dict[int, object],
    reference_marker: str = "o",
) -> None:
    """Add separate legends for temperatures and for SCIANTIX/reference sources."""
    temperature_handles = [
        Line2D([0], [0], color=temperature_colors[temperature_k], label=f"{int(round(temperature_k))} K")
        for temperature_k in temperatures_k
    ]
    source_handles = [
        Line2D([0], [0], color="black", linestyle="-", marker=SCIANTIX_KATO_MARKER, label="SCIANTIX + Kato model"),
        Line2D([0], [0], color="black", linestyle="None", marker=reference_marker, label="Analytical Kato formula"),
    ]

    temperature_legend = ax.legend(
        handles=temperature_handles,
        loc="lower right",
        ncol=2,
        title="Temperature",
    )
    ax.add_artist(temperature_legend)
    ax.legend(handles=source_handles, loc="upper left", title="Model")


def add_temperature_q_legends(ax, temperatures_k: list[int], temperature_colors: dict[int, object]) -> None:
    """Add the temperature legend used on the per-q error figures."""
    temperature_handles = [
        Line2D([0], [0], color=temperature_colors[temperature_k], label=f"{int(round(temperature_k))} K")
        for temperature_k in temperatures_k
    ]
    ax.legend(handles=temperature_handles, loc="upper left", ncol=2, title="Temperature")


def make_combined_potential_error_plot(frame: pd.DataFrame) -> None:
    """Absolute and relative oxygen-potential error, stacked vertically, dots only (no connecting line)."""
    q_values = sorted(frame[Q_COMPARE_COL].dropna().unique())
    temperatures_k = sorted(frame[TEMPERATURE_COMPARE_COL].dropna().unique())
    temperature_colors = temperature_color_map(temperatures_k)

    for q_value in q_values:
        fig, (ax_abs, ax_rel) = plt.subplots(2, 1, figsize=(8, 7), sharex=True)
        q_frame = frame[frame[Q_COMPARE_COL] == q_value]

        for temperature_k in temperatures_k:
            subset = q_frame[q_frame[TEMPERATURE_COMPARE_COL] == temperature_k].sort_values("O/M ratio (/)")

            abs_subset = subset.dropna(subset=["Absolute delta oxygen potential (KJ/mol)"])
            if not abs_subset.empty:
                ax_abs.scatter(
                    abs_subset["O/M ratio (/)"],
                    abs_subset["Absolute delta oxygen potential (KJ/mol)"],
                    color=temperature_colors[temperature_k],
                    marker="o",
                    s=10,
                )

            rel_subset = subset.dropna(subset=["Absolute relative delta oxygen potential (%)"])
            if not rel_subset.empty:
                ax_rel.scatter(
                    rel_subset["O/M ratio (/)"],
                    rel_subset["Absolute relative delta oxygen potential (%)"],
                    color=temperature_colors[temperature_k],
                    marker="o",
                    s=10,
                )

        ax_abs.set_title(f"q = {q_value:.2f} -- absolute error")
        ax_abs.set_ylabel(r"$|\Delta \bar{G}_{O_2}|$ (kJ/mol)")
        ax_abs.set_xlim([OM_MIN, OM_MAX])
        ax_abs.grid(True, alpha=0.3)
        add_temperature_q_legends(ax_abs, temperatures_k, temperature_colors)

        ax_rel.set_title(f"q = {q_value:.2f} -- relative error")
        ax_rel.set_xlabel("O/M ratio (-)")
        ax_rel.set_ylabel(r"Relative $|\Delta \bar{G}_{O_2}|$ (%)")
        ax_rel.set_xlim([OM_MIN, OM_MAX])
        ax_rel.grid(True, alpha=0.3)

        fig.tight_layout()
        fig.savefig(PLOTS_DIR / f"sciantix_vs_kato_potential_error_combined_q_{q_tag(q_value)}.png")
        plt.close(fig)


def make_potential_plot(frame: pd.DataFrame) -> None:
    """Create one oxygen-potential overlay plot per plutonium content q."""
    q_values = sorted(frame[Q_COMPARE_COL].dropna().unique())
    temperatures_k = sorted(frame[TEMPERATURE_COMPARE_COL].dropna().unique())
    temperature_colors = temperature_color_map(temperatures_k)
    q_markers = q_marker_map(q_values)

    for q_value in q_values:
        fig, ax = plt.subplots()
        q_frame = frame[frame[Q_COMPARE_COL] == q_value]

        for temperature_k in temperatures_k:
            subset = q_frame[q_frame[TEMPERATURE_COMPARE_COL] == temperature_k].sort_values("O/M ratio (/)")
            if subset.empty:
                continue

            ax.plot(
                subset["O/M ratio (/)"],
                subset["Fuel oxygen potential - Kato (KJ/mol)"],
                color=temperature_colors[temperature_k],
                linestyle="-",
                marker=SCIANTIX_KATO_MARKER,
                markevery=MODEL_CURVE_MARKEVERY,
                markersize=6,
            )
            ax.scatter(
                subset["O/M ratio (/)"],
                subset["Explicit Kato oxygen potential (KJ/mol)"],
                color=temperature_colors[temperature_k],
                marker=q_markers[q_value],
                s=PLOT_MARKER_SIZE,
            )

        ax.set_title(f"q = {q_value:.2f}")
        ax.set_xlabel("O/M ratio (-)")
        ax.set_ylabel("Oxygen potential (kJ/mol)")
        ax.set_xlim([OM_MIN, OM_MAX])
        ax.grid(True, alpha=0.3)
        add_model_legends(ax, temperatures_k, temperature_colors, reference_marker=q_markers[q_value])
        fig.tight_layout()
        fig.savefig(PLOTS_DIR / f"fuel_oxygen_potential_vs_om_ratio_kato_q_{q_tag(q_value)}.png")
        plt.close(fig)


def main() -> None:
    """Generate the MOX verification tables, text report, and plots."""
    PLOTS_DIR.mkdir(parents=True, exist_ok=True)
    frame = prepare_dataframe()
    write_summary_report(frame)
    make_potential_plot(frame)
    make_combined_potential_error_plot(frame)


if __name__ == "__main__":
    main()
