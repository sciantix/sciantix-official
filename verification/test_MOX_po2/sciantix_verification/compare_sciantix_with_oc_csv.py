#!/usr/bin/env python3
from __future__ import annotations

import math
import os
import re
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
ROOT_DIR = SCRIPT_DIR.parent
SUMMARY_PATH = ROOT_DIR / 'temperature_sweep_summary.tsv'
os.environ.setdefault('MPLCONFIGDIR', str(SCRIPT_DIR / '.matplotlib'))

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.ticker import MaxNLocator
import matplotlib.colors as mcolors
import numpy as np
import pandas as pd

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

REFERENCE_PRESSURE_PA = 1e5
REFERENCE_PRESSURE_MPA = REFERENCE_PRESSURE_PA / 1.0e6
GAS_CONSTANT = 8.31446261815324
THERMOCALC_Q_DIR_GLOB = "TEMPERATURES_THERMOCALC_Q_*"
TEMPERATURE_MIN_K = 753.0
TEMPERATURE_MAX_K = 2550.0
Q_MIN = 0.10
Q_MAX = 0.32
Q_TOLERANCE = 1.0e-3
OM_MIN = 1.92
OM_MAX = 2.08
NEAR_STOICHIOMETRY_TOLERANCE = 1.0e-3
PLOT_MARKER_SIZE = 4
THERMOCALC_MARKER = 'D'
THERMOCALC_MARKER_SIZE = 20

# Fixed normalization range shared across every MOXSCIANTIX verification plot so a
# given temperature always renders as the same viridis color in every figure.
VERIFICATION_TEMPERATURE_MIN_K = 1000.0
VERIFICATION_TEMPERATURE_MAX_K = 2400.0
SCIANTIX_OC_MARKER = "s"
MODEL_CURVE_MARKEVERY = 0.06

COMPARISON_PATH = SCRIPT_DIR / 'sciantix_vs_oc_csv_comparison.tsv'
SUMMARY_OUTPUT_PATH = SCRIPT_DIR / 'sciantix_vs_oc_csv_summary.tsv'
SUMMARY_REPORT_PATH = SCRIPT_DIR / 'sciantix_vs_oc_csv_summary.txt'
PLOTS_DIR = ROOT_DIR / 'plots'

TEMPERATURE_KEY_COL = 'Temperature key (K)'
Q_KEY_COL = 'q key (-)'

def q_tag(q_value: float) -> str:
    return f'{q_value:.2f}'.replace('.', 'p')


def positive_log10_ratio(values: pd.Series, reference: float) -> pd.Series:
    result = pd.Series(np.nan, index=values.index, dtype=float)
    ratio = pd.to_numeric(values, errors='coerce') / reference
    positive = ratio > 0.0
    result.loc[positive] = np.log10(ratio.loc[positive])
    return result


def positive_ln_ratio(values: pd.Series, reference: float) -> pd.Series:
    result = pd.Series(np.nan, index=values.index, dtype=float)
    ratio = pd.to_numeric(values, errors='coerce') / reference
    positive = ratio > 0.0
    result.loc[positive] = np.log(ratio.loc[positive])
    return result


def load_sciantix_data() -> pd.DataFrame:
    """Load the parent-folder sweep summary and normalize key column types."""
    frame = pd.read_csv(SUMMARY_PATH, sep='\t')
    frame = frame.loc[:, ~frame.columns.str.startswith('Unnamed:')].copy()
    source_points = len(frame)

    # temperature_col = 'Temperature (K)' if 'Temperature (K)' in frame.columns else 'Temperature target (K)'
    # q_col = 'q (-)' if 'q (-)' in frame.columns else 'q target (-)'

    frame["Temperature (K)"] = frame["Temperature (K)"].astype(float)
    frame["q (-)"] = frame["q (-)"].astype(float)
    frame['O/M ratio (/)'] = frame['Stoichiometry deviation (/)'].astype(float) + 2.0
    frame['SCIANTIX CALPHAD pO2 (MPa)'] = frame['Fuel oxygen partial pressure - CALPHAD (MPa)'].astype(float)
    frame['SCIANTIX CALPHAD oxygen potential (KJ/mol)'] = frame['Fuel oxygen potential - CALPHAD (KJ/mol)'].astype(float)
    frame['SCIANTIX CALPHAD log10(pO2/p_ref)'] = positive_log10_ratio(
        frame['SCIANTIX CALPHAD pO2 (MPa)'],
        REFERENCE_PRESSURE_MPA,
    )

    frame[TEMPERATURE_KEY_COL] = frame["Temperature (K)"].round().astype('Int64')
    frame[Q_KEY_COL] = frame["q (-)"].round(2)
    in_comparison_range = (
        frame["Temperature (K)"].between(TEMPERATURE_MIN_K, TEMPERATURE_MAX_K)
        & frame["q (-)"].between(Q_MIN - Q_TOLERANCE, Q_MAX + Q_TOLERANCE)
        & frame["O/M ratio (/)"].between(OM_MIN, OM_MAX)
    )
    frame = frame.loc[in_comparison_range].copy()
    # Exact-stoichiometry points are part of the verification; do not exclude O/M = 2.0.
    frame.attrs["source_points"] = source_points
    frame.attrs["comparison_range_excluded_points"] = source_points - len(frame)
    return frame


def load_oc_csv_data() -> pd.DataFrame:
    """Load Thermo-Calc reference CSV files and derive thermochemical fields."""
    rows: list[pd.DataFrame] = []

    # Preferred source: folders like TEMPERATURES_THERMOCALC_Q_10/800.csv
    q_folder_pattern = re.compile(r"TEMPERATURES_THERMOCALC_Q_(.+)")
    q_from_folder_pattern = re.compile(r"^(\d+)(?:[pP](\d+))?$")
    temperature_file_pattern = re.compile(r"^\d+\.csv$")
    q_folders = sorted(path for path in ROOT_DIR.glob(THERMOCALC_Q_DIR_GLOB) if path.is_dir())

    for folder in q_folders:
        folder_match = q_folder_pattern.fullmatch(folder.name)
        if not folder_match:
            continue
        q_token = folder_match.group(1)
        q_value: float
        q_match = q_from_folder_pattern.fullmatch(q_token)
        if q_match:
            integer_part = q_match.group(1)
            decimal_part = q_match.group(2)
            if decimal_part is None:
                q_value = float(integer_part) / 100.0
            else:
                q_value = float(f"{integer_part}.{decimal_part}")
        else:
            # Generic fallback for uncommon names (e.g., 0p10, 0.10)
            q_value = float(q_token.replace("p", ".").replace("P", "."))

        for path in sorted(folder.glob("*.csv")):
            if not temperature_file_pattern.fullmatch(path.name):
                continue

            frame = pd.read_csv(path, sep="\t").rename(columns=lambda name: name.strip().strip('"'))
            expected_columns = {"Temperature [K]", "Mole percent O", "Mole percent U", "Activity of O"}
            missing_columns = expected_columns.difference(frame.columns)
            if missing_columns:
                missing_sorted = ", ".join(sorted(missing_columns))
                raise RuntimeError(f"{path} is missing required columns: {missing_sorted}")

            frame["Temperature [K]"] = pd.to_numeric(frame["Temperature [K]"], errors="coerce")
            frame["Mole percent O"] = pd.to_numeric(frame["Mole percent O"], errors="coerce")
            frame["Mole percent U"] = pd.to_numeric(frame["Mole percent U"], errors="coerce")
            frame["Activity of O"] = pd.to_numeric(frame["Activity of O"], errors="coerce")
            if "Mole percent Pu" in frame.columns:
                frame["Mole percent Pu"] = pd.to_numeric(frame["Mole percent Pu"], errors="coerce").fillna(0.0)
            else:
                frame["Mole percent Pu"] = 0.0

            metal_mole_percent = frame["Mole percent U"] + frame["Mole percent Pu"]
            frame["O/M ratio (/)"] = np.where(metal_mole_percent > 0.0, frame["Mole percent O"] / metal_mole_percent, np.nan)
            frame["OC oxygen activity"] = frame["Activity of O"]
            frame[TEMPERATURE_KEY_COL] = frame["Temperature [K]"].round().astype("Int64")
            frame = frame.dropna(subset=["O/M ratio (/)", "OC oxygen activity", TEMPERATURE_KEY_COL])
            frame[Q_KEY_COL] = q_value
            frame["Region"] = "single"
            rows.append(frame[["O/M ratio (/)", "OC oxygen activity", TEMPERATURE_KEY_COL, Q_KEY_COL, "Region"]])

    # Fallback: legacy test_* exports in sciantix_verification
    if not rows:
        new_pattern = re.compile(r'test__q_(\d+p\d+)(?:\.csv)?$')
        legacy_pattern = re.compile(r'test_(\d+)K_q_(\d+p\d+)(?:_(ipo|iper|ipe))?(?:\.csv)?$')
        new_paths = sorted(SCRIPT_DIR.glob('test__q_*'))
        legacy_paths = sorted(SCRIPT_DIR.glob('test_*K_q_*'))
        selected_paths = new_paths if new_paths else legacy_paths

        for path in selected_paths:
            temperature_k: int | None = None
            q_value: float
            region: str

            new_match = new_pattern.fullmatch(path.name)
            if new_match:
                q_value = float(new_match.group(1).replace('p', '.'))
                region = 'single'
            else:
                legacy_match = legacy_pattern.fullmatch(path.name)
                if not legacy_match:
                    continue
                temperature_k = int(legacy_match.group(1))
                q_value = float(legacy_match.group(2).replace('p', '.'))
                region = legacy_match.group(3) if legacy_match.group(3) is not None else 'single'

            frame = pd.read_csv(path, sep=',', engine='python').rename(columns=lambda name: name.strip().strip('"'))
            if 'N(O)' not in frame.columns or 'AC(O)' not in frame.columns:
                continue

            if temperature_k is None:
                temperature_col = next(
                    (
                        col for col in frame.columns
                        if col.lower().strip() in {'t', 'temp', 'temperature', 't(k)', 'temperature(k)', 'temperature (k)'}
                    ),
                    None,
                )
                if temperature_col is None:
                    raise RuntimeError(
                        f'Missing temperature column in {path.name}. '
                        'Use `l ex t n(o) ac(o) ...` in the OCM export.'
                    )
                temperature_values = pd.to_numeric(frame[temperature_col], errors='coerce')
            else:
                temperature_values = pd.Series(float(temperature_k), index=frame.index)

            frame = frame[['N(O)', 'AC(O)']].copy()
            frame = frame.rename(columns={'N(O)': 'O/M ratio (/)', 'AC(O)': 'OC oxygen activity'})
            frame['O/M ratio (/)'] = pd.to_numeric(frame['O/M ratio (/)'], errors='coerce')
            frame['OC oxygen activity'] = pd.to_numeric(frame['OC oxygen activity'], errors='coerce')
            frame[TEMPERATURE_KEY_COL] = temperature_values.round().astype('Int64')
            frame = frame.dropna(subset=['O/M ratio (/)', 'OC oxygen activity', TEMPERATURE_KEY_COL])

            frame[Q_KEY_COL] = q_value
            frame['Region'] = region
            rows.append(frame)

    if not rows:
        raise RuntimeError(
            "No reference files found. Expected folders matching "
            f"{THERMOCALC_Q_DIR_GLOB} in {ROOT_DIR} or legacy test_* exports in {SCRIPT_DIR}."
        )

    frame = pd.concat(rows, ignore_index=True)
    source_points = len(frame)
    frame = frame.sort_values([TEMPERATURE_KEY_COL, Q_KEY_COL, 'O/M ratio (/)', 'Region']).reset_index(drop=True)
    in_comparison_range = (
        frame[TEMPERATURE_KEY_COL].astype(float).between(TEMPERATURE_MIN_K, TEMPERATURE_MAX_K)
        & frame[Q_KEY_COL].astype(float).between(Q_MIN - Q_TOLERANCE, Q_MAX + Q_TOLERANCE)
        & frame["O/M ratio (/)"].between(OM_MIN, OM_MAX)
    )
    frame = frame.loc[in_comparison_range].copy()
    # Exact-stoichiometry points are part of the verification; do not exclude O/M = 2.0.
    frame.attrs["source_points"] = source_points
    frame.attrs["comparison_range_excluded_points"] = source_points - len(frame)

    frame['OC pO2 (MPa)'] = (frame['OC oxygen activity'] ** 2) * REFERENCE_PRESSURE_MPA
    frame['OC log10(pO2/p_ref)'] = positive_log10_ratio(frame['OC pO2 (MPa)'], REFERENCE_PRESSURE_MPA)
    frame['OC oxygen potential (KJ/mol)'] = (
        GAS_CONSTANT * 1.0e-3
        * frame[TEMPERATURE_KEY_COL].astype(float)
        * positive_ln_ratio(frame['OC pO2 (MPa)'], REFERENCE_PRESSURE_MPA)
    )
    return frame


def interpolate_sciantix_to_oc_points(
    sciantix_frame: pd.DataFrame,
    oc_frame: pd.DataFrame,
) -> pd.DataFrame:
    """Interpolate SCIANTIX final pO2 onto the Thermo-Calc O/M grid."""
    rows: list[pd.DataFrame] = []

    grouped = oc_frame.groupby([TEMPERATURE_KEY_COL, Q_KEY_COL], as_index=False)
    for (temperature_k, q_key), oc_group in grouped:
        sci_group = sciantix_frame[
            (sciantix_frame[TEMPERATURE_KEY_COL] == temperature_k)
            & (sciantix_frame[Q_KEY_COL] == q_key)
        ].sort_values('O/M ratio (/)')
        sci_group = sci_group[sci_group['SCIANTIX CALPHAD pO2 (MPa)'] > 0.0].copy()
        if len(sci_group) < 2:
            continue

        oc_group = oc_group.sort_values('O/M ratio (/)').copy()
        oc_group = (
            oc_group.groupby([TEMPERATURE_KEY_COL, Q_KEY_COL, 'O/M ratio (/)'], as_index=False)['OC pO2 (MPa)']
            .mean()
            .sort_values('O/M ratio (/)')
        )
        oc_group['OC log10(pO2/p_ref)'] = positive_log10_ratio(oc_group['OC pO2 (MPa)'], REFERENCE_PRESSURE_MPA)
        oc_group['OC oxygen potential (KJ/mol)'] = (
            GAS_CONSTANT * 1.0e-3
            * float(temperature_k)
            * positive_ln_ratio(oc_group['OC pO2 (MPa)'], REFERENCE_PRESSURE_MPA)
        )
        oc_group = oc_group[oc_group['OC pO2 (MPa)'] > 0.0].copy()
        if oc_group.empty:
            continue

        sci_x = sci_group['O/M ratio (/)'].to_numpy(dtype=float)
        sci_log_p = np.log10(sci_group['SCIANTIX CALPHAD pO2 (MPa)'].to_numpy(dtype=float) / REFERENCE_PRESSURE_MPA)

        within_range = oc_group['O/M ratio (/)'].between(sci_x.min(), sci_x.max())
        oc_valid = oc_group.loc[within_range].copy()
        if oc_valid.empty:
            continue

        oc_x = oc_valid['O/M ratio (/)'].to_numpy(dtype=float)
        interpolated_log_p = np.interp(oc_x, sci_x, sci_log_p)
        oc_valid['Interpolated SCIANTIX CALPHAD log10(pO2/p_ref)'] = interpolated_log_p
        oc_valid['Interpolated SCIANTIX CALPHAD pO2 (MPa)'] = REFERENCE_PRESSURE_MPA * (10.0 ** interpolated_log_p)
        oc_valid['Delta log10(pO2/p_ref)'] = (
            oc_valid['Interpolated SCIANTIX CALPHAD log10(pO2/p_ref)'] - oc_valid['OC log10(pO2/p_ref)']
        )
        oc_valid['Absolute delta log10(pO2/p_ref)'] = oc_valid['Delta log10(pO2/p_ref)'].abs()

        denominator_log = oc_valid['OC log10(pO2/p_ref)'].abs()
        oc_valid['Relative delta log10(pO2/p_ref) (%)'] = np.where(
            denominator_log > 0.0,
            oc_valid['Absolute delta log10(pO2/p_ref)'] / denominator_log * 100.0,
            np.nan,
        )

        oc_valid['Interpolated SCIANTIX CALPHAD oxygen potential (KJ/mol)'] = (
            GAS_CONSTANT * 1.0e-3 * float(temperature_k) * np.log(10.0 ** interpolated_log_p)
        )
        oc_valid['Delta oxygen potential (KJ/mol)'] = (
            oc_valid['Interpolated SCIANTIX CALPHAD oxygen potential (KJ/mol)'] - oc_valid['OC oxygen potential (KJ/mol)']
        )
        oc_valid['Absolute delta oxygen potential (KJ/mol)'] = oc_valid['Delta oxygen potential (KJ/mol)'].abs()

        denominator_potential = oc_valid['OC oxygen potential (KJ/mol)'].abs()
        oc_valid['Relative delta oxygen potential (%)'] = np.where(
            denominator_potential > 0.0,
            oc_valid['Absolute delta oxygen potential (KJ/mol)'] / denominator_potential * 100.0,
            np.nan,
        )
        rows.append(oc_valid)

    if not rows:
        raise RuntimeError('No overlapping (T,q,O/M) ranges found for SCIANTIX-CALPHAD interpolation.')

    aligned = pd.concat(rows, ignore_index=True)
    aligned.attrs["sciantix_source_points"] = sciantix_frame.attrs.get("source_points", len(sciantix_frame))
    aligned.attrs["sciantix_comparison_range_excluded_points"] = sciantix_frame.attrs.get("comparison_range_excluded_points", 0)
    aligned.attrs["oc_source_points"] = oc_frame.attrs.get("source_points", len(oc_frame))
    aligned.attrs["oc_comparison_range_excluded_points"] = oc_frame.attrs.get("comparison_range_excluded_points", 0)
    aligned.attrs["oc_overlap_excluded_points"] = len(oc_frame) - len(aligned)
    result = aligned.sort_values([TEMPERATURE_KEY_COL, Q_KEY_COL, 'O/M ratio (/)']).reset_index(drop=True)
    result.attrs.update(aligned.attrs)
    return result


def build_metric_summary(frame: pd.DataFrame) -> pd.DataFrame:
    return frame.groupby([Q_KEY_COL, TEMPERATURE_KEY_COL], as_index=False).agg(
        count=('Delta log10(pO2/p_ref)', 'count'),
        mean_abs_log_error=('Absolute delta log10(pO2/p_ref)', 'mean'),
        max_abs_log_error=('Absolute delta log10(pO2/p_ref)', 'max'),
        mean_abs_potential_error=('Absolute delta oxygen potential (KJ/mol)', 'mean'),
        max_abs_potential_error=('Absolute delta oxygen potential (KJ/mol)', 'max'),
    )


def write_summary_report(frame: pd.DataFrame, summary: pd.DataFrame) -> None:
    overall_count = len(frame)
    near_stoichiometry_points = int(np.isclose(
        frame['O/M ratio (/)'],
        2.0,
        atol=NEAR_STOICHIOMETRY_TOLERANCE,
        rtol=0.0,
    ).sum())
    mean_abs_potential = frame['Absolute delta oxygen potential (KJ/mol)'].mean()
    max_abs_potential = frame['Absolute delta oxygen potential (KJ/mol)'].max()
    mean_rel_potential = frame['Relative delta oxygen potential (%)'].mean()
    max_rel_potential = frame['Relative delta oxygen potential (%)'].max()

    lines = [
        'SCIANTIX-CALPHAD vs Thermo-Calc (MOX)',
        '===============================================',
        '',
        f"Comparison ranges: T = {TEMPERATURE_MIN_K:g}-{TEMPERATURE_MAX_K:g} K, "
        f"Pu/M = {Q_MIN:.2f}-{Q_MAX:.2f}, O/M = {OM_MIN:.2f}-{OM_MAX:.2f}",
        f"Sciantix source points: {frame.attrs.get('sciantix_source_points', 'unknown')}",
        f"Sciantix points outside comparison ranges: {frame.attrs.get('sciantix_comparison_range_excluded_points', 'unknown')}",
        f"OC source points: {frame.attrs.get('oc_source_points', 'unknown')}",
        f"OC points outside comparison ranges: {frame.attrs.get('oc_comparison_range_excluded_points', 'unknown')}",
        f"OC points outside SCIANTIX interpolation overlap: {frame.attrs.get('oc_overlap_excluded_points', 'unknown')}",
        f'Compared points: {overall_count}',
        f'Near O/M = 2.0 compared points: {near_stoichiometry_points}',
        f'Mean absolute oxygen-potential error (KJ/mol): {mean_abs_potential:.6e}',
        f'Max absolute oxygen-potential error (KJ/mol): {max_abs_potential:.6e}',
        f'Mean relative oxygen-potential error (%): {mean_rel_potential:.6e}',
        f'Max relative oxygen-potential error (%): {max_rel_potential:.6e}',
        '',
        'Per-(q, temperature) summary:',
        summary.to_string(index=False),
        '',
    ]
    SUMMARY_REPORT_PATH.write_text('\n'.join(lines))


DISPLAY_TEMPERATURES_K = {1000, 1200, 1400, 1600, 1800, 2000, 2200, 2400}


def select_display_temperatures(temperatures_k: list[int]) -> list[int]:
    """Curated subset shown in multi-temperature curve plots, to keep them readable.

    The underlying statistics (aligned_frame / build_metric_summary) are unaffected:
    this only trims which temperature curves are drawn.
    """
    filtered = [t for t in temperatures_k if t in DISPLAY_TEMPERATURES_K]
    return filtered if filtered else temperatures_k


def temperature_color_map(temperatures_k: list[int]) -> dict[int, object]:
    """Assign a viridis color to each temperature, shared across the paper's figures."""
    norm = mcolors.Normalize(vmin=VERIFICATION_TEMPERATURE_MIN_K, vmax=VERIFICATION_TEMPERATURE_MAX_K)
    cmap = matplotlib.colormaps["viridis"]
    return {temperature_k: cmap(norm(temperature_k)) for temperature_k in temperatures_k}


def q_marker_map(q_values: list[float]) -> dict[float, str]:
    markers = ['o', 's', '^', 'D', 'v', 'P', 'X', '*']
    return {q_value: markers[index % len(markers)] for index, q_value in enumerate(q_values)}


def add_model_legends(
    ax,
    temperatures_k: list[int],
    temperature_colors: dict[int, object],
    reference_marker: str = 'o',
) -> None:
    temperature_handles = [
        Line2D([0], [0], color=temperature_colors[temp], label=f'{int(round(temp))} K')
        for temp in temperatures_k
    ]
    model_handles = [
        Line2D([0], [0], color='black', linestyle='-', label='SCIANTIX + OpenCalphad'),
        Line2D([0], [0], color='black', marker=reference_marker, linestyle='None', label='Thermo-Calc'),
    ]
    first = ax.legend(handles=temperature_handles, loc='lower right', ncol=2, title='Temperature')
    ax.add_artist(first)
    ax.legend(handles=model_handles, loc='upper left', title='Series')


def add_temperature_legend(ax, temperatures_k: list[int], temperature_colors: dict[int, object]) -> None:
    handles = [
        Line2D([0], [0], color=temperature_colors[temp], label=f'{round(temp)} K')
        for temp in temperatures_k
    ]
    ax.legend(handles=handles, loc='upper left', ncol=2, title='Temperature')



def make_potential_plot(sciantix_frame: pd.DataFrame, oc_frame: pd.DataFrame) -> None:
    q_values = sorted(oc_frame[Q_KEY_COL].dropna().unique())
    temperatures_k = sorted(oc_frame[TEMPERATURE_KEY_COL].dropna().unique())
    temperatures_k = select_display_temperatures(temperatures_k)
    temperature_colors = temperature_color_map(temperatures_k)
    q_markers = q_marker_map(q_values)

    for q_value in q_values:
        fig, ax = plt.subplots()
        sci_q_frame = sciantix_frame[sciantix_frame[Q_KEY_COL] == q_value]
        oc_q_frame = oc_frame[oc_frame[Q_KEY_COL] == q_value]

        for temperature_k in temperatures_k:
            sci_subset = sci_q_frame[sci_q_frame[TEMPERATURE_KEY_COL] == temperature_k].sort_values('O/M ratio (/)')
            sci_subset = sci_subset.dropna(subset=['SCIANTIX CALPHAD oxygen potential (KJ/mol)'])
            # A potential of exactly 0 means "not computed at this step" (unconverged
            # OpenCalphad equilibrium), not a real 0 kJ/mol -- same convention as
            # validation/oxygenpotential/*/plot.py's read_output_curve.
            sci_subset = sci_subset[sci_subset['SCIANTIX CALPHAD oxygen potential (KJ/mol)'] != 0.0]
            if not sci_subset.empty:
                ax.plot(
                    sci_subset['O/M ratio (/)'],
                    sci_subset['SCIANTIX CALPHAD oxygen potential (KJ/mol)'],
                    color=temperature_colors[temperature_k],
                    linestyle='-',
                    zorder=2,
                )

            oc_subset = oc_q_frame[oc_q_frame[TEMPERATURE_KEY_COL] == temperature_k].sort_values('O/M ratio (/)')
            oc_subset = oc_subset.dropna(subset=['OC oxygen potential (KJ/mol)'])
            if oc_subset.empty:
                continue

            ax.scatter(
                oc_subset['O/M ratio (/)'],
                oc_subset['OC oxygen potential (KJ/mol)'],
                facecolors='white',
                edgecolors=temperature_colors[temperature_k],
                linewidths=1.4,
                marker=THERMOCALC_MARKER,
                s=THERMOCALC_MARKER_SIZE,
                zorder=3,
            )

        ax.set_title(f'q = {q_value:.2f}')
        ax.set_xlabel('O/M ratio (-)')
        ax.set_ylabel('Oxygen potential (kJ/mol)')

        y_values = pd.concat(
            [
                sci_q_frame['SCIANTIX CALPHAD oxygen potential (KJ/mol)'],
                oc_q_frame['OC oxygen potential (KJ/mol)'],
            ],
            ignore_index=True,
        ).dropna()

        ax.set_xlim([OM_MIN, OM_MAX])

        if not y_values.empty:
            y_min = float(y_values.min())
            y_max = float(y_values.max())
            y_span = max(y_max - y_min, 1.0e-6)
            y_pad = 0.06 * y_span
            ax.set_ylim([y_min - y_pad, y_max + y_pad])

        ax.grid(True, alpha=0.3)
        add_model_legends(ax, temperatures_k, temperature_colors, reference_marker=THERMOCALC_MARKER)
        fig.tight_layout()
        fig.savefig(PLOTS_DIR / f'fuel_oxygen_potential_vs_om_ratio_oc_csv_q_{q_tag(q_value)}.png')
        plt.close(fig)


def make_signed_potential_error_plot(frame: pd.DataFrame) -> None:
    q_values = sorted(frame[Q_KEY_COL].dropna().unique())
    temperatures_k = sorted(frame[TEMPERATURE_KEY_COL].dropna().unique())
    temperatures_k = select_display_temperatures(temperatures_k)
    temperature_colors = temperature_color_map(temperatures_k)

    for q_value in q_values:
        fig, ax = plt.subplots()
        q_frame = frame[frame[Q_KEY_COL] == q_value]

        for temperature_k in temperatures_k:
            subset = q_frame[q_frame[TEMPERATURE_KEY_COL] == temperature_k].sort_values('O/M ratio (/)')
            subset = subset.dropna(subset=['Delta oxygen potential (KJ/mol)'])
            if subset.empty:
                continue

            ax.scatter(
                subset['O/M ratio (/)'],
                subset['Delta oxygen potential (KJ/mol)'],
                color=temperature_colors[temperature_k],
                marker='o',
                s=10,
            )

        ax.axhline(0.0, color='black', linestyle='--')
        ax.set_title(f'q = {q_value:.2f}')
        ax.set_xlabel('O/M ratio (-)')
        ax.set_ylabel(r'$\Delta \bar{G}_{O_2}$ (kJ/mol)')
        ax.set_xlim([OM_MIN, OM_MAX])
        ax.grid(True, alpha=0.3)
        add_temperature_legend(ax, temperatures_k, temperature_colors)
        fig.tight_layout()
        fig.savefig(PLOTS_DIR / f'sciantix_vs_oc_csv_potential_error_q_{q_tag(q_value)}.png')
        plt.close(fig)


def make_combined_error_plot(frame: pd.DataFrame) -> None:
    """Absolute and relative oxygen-potential error, stacked vertically, dots only (no connecting line).

    Reported in kJ/mol (and %), matching the units used for the code's oxygen
    potential itself and for the experimental validation, rather than log10(pO2).
    """
    q_values = sorted(frame[Q_KEY_COL].dropna().unique())
    temperatures_k = sorted(frame[TEMPERATURE_KEY_COL].dropna().unique())
    temperatures_k = select_display_temperatures(temperatures_k)
    temperature_colors = temperature_color_map(temperatures_k)

    for q_value in q_values:
        fig, (ax_abs, ax_rel) = plt.subplots(
            2, 1, figsize=(8, 8), sharex=True, gridspec_kw={'height_ratios': [1.15, 1]},
        )
        q_frame = frame[frame[Q_KEY_COL] == q_value]

        for temperature_k in temperatures_k:
            subset = q_frame[q_frame[TEMPERATURE_KEY_COL] == temperature_k].sort_values('O/M ratio (/)')

            abs_subset = subset.dropna(subset=['Absolute delta oxygen potential (KJ/mol)'])
            if not abs_subset.empty:
                ax_abs.scatter(
                    abs_subset['O/M ratio (/)'],
                    abs_subset['Absolute delta oxygen potential (KJ/mol)'],
                    color=temperature_colors[temperature_k],
                    marker='s',
                    s=15,
                )

            rel_subset = subset.dropna(subset=['Relative delta oxygen potential (%)'])
            if not rel_subset.empty:
                ax_rel.scatter(
                    rel_subset['O/M ratio (/)'],
                    rel_subset['Relative delta oxygen potential (%)'],
                    color=temperature_colors[temperature_k],
                    marker='s',
                    s=15,
                )

        ax_abs.set_title(f'q = {q_value:.2f},  absolute error')
        ax_abs.set_ylabel(r'$|\Delta G_{O_2}|$ (kJ/mol)')
        ax_abs.set_xlim([OM_MIN, OM_MAX])
        ax_abs.yaxis.set_major_locator(MaxNLocator(nbins=9, integer=False))
        ax_abs.grid(True, alpha=0.3)

        ax_rel.set_title(f'q = {q_value:.2f}, relative error')
        ax_rel.set_xlabel('O/M ratio (-)')
        ax_rel.set_ylabel(r'Relative $|\Delta G_{O_2}|$ (%)')
        ax_rel.set_xlim([OM_MIN, OM_MAX])
        ax_rel.yaxis.set_major_locator(MaxNLocator(nbins=8, integer=False))
        ax_rel.grid(True, alpha=0.3)

        # Single legend for the whole figure, outside both axes on the right -- avoids
        # covering data points in the crowded low-error band where most points sit.
        handles = [
            Line2D([0], [0], color=temperature_colors[temp], marker='s', linestyle='None',
                   markersize=8, label=f'{round(temp)} K')
            for temp in temperatures_k
        ]
        fig.legend(handles=handles, loc='center left', bbox_to_anchor=(1.0, 0.5),
                   ncol=1, title='Temperature', frameon=False)

        fig.tight_layout()
        fig.savefig(PLOTS_DIR / f'sciantix_vs_oc_csv_potential_error_combined_q_{q_tag(q_value)}.png',
                    bbox_inches='tight')
        plt.close(fig)


def main() -> None:
    PLOTS_DIR.mkdir(parents=True, exist_ok=True)
    sciantix_frame = load_sciantix_data()
    oc_frame = load_oc_csv_data()
    aligned_frame = interpolate_sciantix_to_oc_points(sciantix_frame, oc_frame)

    aligned_frame.to_csv(COMPARISON_PATH, sep='\t', index=False)
    summary = build_metric_summary(aligned_frame)
    summary.to_csv(SUMMARY_OUTPUT_PATH, sep='\t', index=False)
    write_summary_report(aligned_frame, summary)

    make_potential_plot(sciantix_frame, oc_frame)
    make_signed_potential_error_plot(aligned_frame)
    make_combined_error_plot(aligned_frame)


if __name__ == '__main__':
    main()
