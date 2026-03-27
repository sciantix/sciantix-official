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
import numpy as np
import pandas as pd

REFERENCE_PRESSURE_PA = 1.0e5
REFERENCE_PRESSURE_MPA = REFERENCE_PRESSURE_PA / 1.0e6
GAS_CONSTANT = 8.31446261815324

COMPARISON_PATH = SCRIPT_DIR / 'sciantix_vs_oc_csv_comparison.tsv'
SUMMARY_OUTPUT_PATH = SCRIPT_DIR / 'sciantix_vs_oc_csv_summary.tsv'
SUMMARY_REPORT_PATH = SCRIPT_DIR / 'sciantix_vs_oc_csv_summary.txt'

TEMPERATURE_KEY_COL = 'Temperature key (K)'
Q_KEY_COL = 'q key (-)'

plt.rcParams.update({
    'figure.figsize': (10, 7),
    'font.size': 12,
    'axes.labelsize': 15,
    'axes.titlesize': 12,
    'xtick.labelsize': 12,
    'ytick.labelsize': 12,
    'legend.fontsize': 12,
    'figure.dpi': 300,
    'axes.grid': True,
    'grid.alpha': 0.5,
    'grid.linestyle': '--',
    'lines.linewidth': 2,
    'lines.markersize': 4,
    'legend.frameon': False,
})


def q_tag(q_value: float) -> str:
    return f'{q_value:.2f}'.replace('.', 'p')


def load_sciantix_data() -> pd.DataFrame:
    frame = pd.read_csv(SUMMARY_PATH, sep='\t')
    frame = frame.loc[:, ~frame.columns.str.startswith('Unnamed:')].copy()

    temperature_col = 'Temperature (K)' if 'Temperature (K)' in frame.columns else 'Temperature target (K)'
    q_col = 'q (-)' if 'q (-)' in frame.columns else 'q target (-)'

    frame[temperature_col] = pd.to_numeric(frame[temperature_col], errors='coerce')
    frame[q_col] = pd.to_numeric(frame[q_col], errors='coerce')
    frame['O/M ratio (/)'] = pd.to_numeric(frame['Stoichiometry deviation (/)'], errors='coerce') + 2.0
    frame['SCIANTIX CALPHAD pO2 (MPa)'] = pd.to_numeric(
        frame['Fuel oxygen partial pressure - CALPHAD (MPa)'],
        errors='coerce',
    )

    frame[TEMPERATURE_KEY_COL] = frame[temperature_col].round().astype('Int64')
    frame[Q_KEY_COL] = frame[q_col].round(2)
    return frame


def load_oc_csv_data() -> pd.DataFrame:
    rows: list[pd.DataFrame] = []
    pattern = re.compile(r'test_(\d+)K_q_(\d+p\d+)_(ipo|iper|ipe)\.?$')

    for path in sorted(SCRIPT_DIR.glob('test_*K_q_*_ip*')):
        match = pattern.fullmatch(path.name)
        if not match:
            continue

        temperature_k = int(match.group(1))
        q_value = float(match.group(2).replace('p', '.'))
        region = match.group(3)

        frame = pd.read_csv(path, sep=',', engine='python').rename(columns=lambda name: name.strip().strip('"'))
        if 'N(O)' not in frame.columns or 'AC(O)' not in frame.columns:
            continue

        frame = frame[['N(O)', 'AC(O)']].copy()
        frame = frame.rename(columns={'N(O)': 'O/M ratio (/)', 'AC(O)': 'OC oxygen activity'})
        frame['O/M ratio (/)'] = pd.to_numeric(frame['O/M ratio (/)'], errors='coerce')
        frame['OC oxygen activity'] = pd.to_numeric(frame['OC oxygen activity'], errors='coerce')
        frame = frame.dropna(subset=['O/M ratio (/)', 'OC oxygen activity'])

        frame[TEMPERATURE_KEY_COL] = temperature_k
        frame[Q_KEY_COL] = q_value
        frame['Region'] = region
        rows.append(frame)

    if not rows:
        raise RuntimeError(f'No OpenCalphad export files found in {SCRIPT_DIR}')

    frame = pd.concat(rows, ignore_index=True)
    frame = (
        frame.groupby([TEMPERATURE_KEY_COL, Q_KEY_COL, 'O/M ratio (/)'], as_index=False)
        .agg({'OC oxygen activity': 'mean'})
        .sort_values([TEMPERATURE_KEY_COL, Q_KEY_COL, 'O/M ratio (/)'])
        .reset_index(drop=True)
    )

    frame['OC pO2 (MPa)'] = (frame['OC oxygen activity'] ** 2) * REFERENCE_PRESSURE_MPA
    frame['OC log10(pO2/p_ref)'] = np.where(
        frame['OC pO2 (MPa)'] > 0.0,
        np.log10(frame['OC pO2 (MPa)'] / REFERENCE_PRESSURE_MPA),
        np.nan,
    )
    frame['OC oxygen potential (KJ/mol)'] = (
        GAS_CONSTANT * 1.0e-3 * frame[TEMPERATURE_KEY_COL].astype(float) * np.log(frame['OC pO2 (MPa)'] / REFERENCE_PRESSURE_MPA)
    )
    return frame


def interpolate_sciantix_to_oc_points(sciantix_frame: pd.DataFrame, oc_frame: pd.DataFrame) -> pd.DataFrame:
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

        sci_x = sci_group['O/M ratio (/)'].to_numpy(dtype=float)
        sci_log_p = np.log10(sci_group['SCIANTIX CALPHAD pO2 (MPa)'].to_numpy(dtype=float) / REFERENCE_PRESSURE_MPA)

        oc_group = oc_group.sort_values('O/M ratio (/)').copy()
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
    return aligned.sort_values([TEMPERATURE_KEY_COL, Q_KEY_COL, 'O/M ratio (/)']).reset_index(drop=True)


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
    max_abs_log = frame['Absolute delta log10(pO2/p_ref)'].max()
    mean_abs_log = frame['Absolute delta log10(pO2/p_ref)'].mean()
    mean_rel_log = frame['Relative delta log10(pO2/p_ref) (%)'].mean()
    max_rel_log = frame['Relative delta log10(pO2/p_ref) (%)'].max()

    lines = [
        'SCIANTIX-CALPHAD vs standalone OpenCalphad (MOX)',
        '===============================================',
        '',
        f'Compared points: {overall_count}',
        f'Mean absolute log10(pO2/p_ref) error: {mean_abs_log:.6e}',
        f'Max absolute log10(pO2/p_ref) error: {max_abs_log:.6e}',
        f'Mean relative log10(pO2/p_ref) error (%): {mean_rel_log:.6e}',
        f'Max relative log10(pO2/p_ref) error (%): {max_rel_log:.6e}',
        '',
        'Per-(q, temperature) summary:',
        summary.to_string(index=False),
        '',
    ]
    SUMMARY_REPORT_PATH.write_text('\n'.join(lines))


def temperature_color_map(temperatures_k: list[int]) -> dict[int, object]:
    cmap = plt.get_cmap('turbo', len(temperatures_k))
    return {temperature_k: cmap(index) for index, temperature_k in enumerate(temperatures_k)}


def q_marker_map(q_values: list[float]) -> dict[float, str]:
    markers = ['o', 's', '^', 'D', 'v', 'P', 'X', '*']
    return {q_value: markers[index % len(markers)] for index, q_value in enumerate(q_values)}


def add_model_legends(ax, temperatures_k: list[int], temperature_colors: dict[int, object]) -> None:
    temperature_handles = [
        Line2D([0], [0], color=temperature_colors[temp], label=f'{int(round(temp))} K')
        for temp in temperatures_k
    ]
    model_handles = [
        Line2D([0], [0], color='black', linestyle='-', label='SCIANTIX + OpenCalphad'),
        Line2D([0], [0], color='black', marker='o', linestyle='None', label='OpenCalphad alone'),
    ]
    first = ax.legend(handles=temperature_handles, loc='lower right', ncol=2, title='Temperature')
    ax.add_artist(first)
    ax.legend(handles=model_handles, loc='upper left', title='Series')


def add_temperature_legend(ax, temperatures_k: list[int], temperature_colors: dict[int, object]) -> None:
    handles = [
        Line2D([0], [0], color=temperature_colors[temp], label=f'{int(round(temp))} K')
        for temp in temperatures_k
    ]
    ax.legend(handles=handles, loc='upper left', ncol=2, title='Temperature')


def make_pressure_plot(frame: pd.DataFrame) -> None:
    q_values = sorted(frame[Q_KEY_COL].dropna().unique())
    temperatures_k = sorted(frame[TEMPERATURE_KEY_COL].dropna().unique())
    temperature_colors = temperature_color_map(temperatures_k)
    q_markers = q_marker_map(q_values)

    for q_value in q_values:
        fig, ax = plt.subplots()
        q_frame = frame[frame[Q_KEY_COL] == q_value]

        for temperature_k in temperatures_k:
            subset = q_frame[q_frame[TEMPERATURE_KEY_COL] == temperature_k].sort_values('O/M ratio (/)')
            if subset.empty:
                continue

            ax.plot(
                subset['O/M ratio (/)'],
                subset['Interpolated SCIANTIX CALPHAD log10(pO2/p_ref)'],
                color=temperature_colors[temperature_k],
                linestyle='-',
            )
            ax.scatter(
                subset['O/M ratio (/)'],
                subset['OC log10(pO2/p_ref)'],
                color=temperature_colors[temperature_k],
                marker=q_markers[q_value],
                s=22,
            )

        ax.set_title(f'q = {q_value:.2f}')
        ax.set_xlabel('O/M ratio (-)')
        ax.set_ylabel(r'$\log_{10}(p_{O_2}/p_{ref})$ (-)')
        ax.set_xlim([1.90, 2.20])
        ax.grid(True, alpha=0.3)
        add_model_legends(ax, temperatures_k, temperature_colors)
        fig.tight_layout()
        fig.savefig(ROOT_DIR / f'sciantix_vs_oc_csv_partial_pressure_q_{q_tag(q_value)}.png')
        plt.close(fig)


def make_signed_log_error_plot(frame: pd.DataFrame) -> None:
    q_values = sorted(frame[Q_KEY_COL].dropna().unique())
    temperatures_k = sorted(frame[TEMPERATURE_KEY_COL].dropna().unique())
    temperature_colors = temperature_color_map(temperatures_k)

    for q_value in q_values:
        fig, ax = plt.subplots()
        q_frame = frame[frame[Q_KEY_COL] == q_value]

        for temperature_k in temperatures_k:
            subset = q_frame[q_frame[TEMPERATURE_KEY_COL] == temperature_k].sort_values('O/M ratio (/)')
            subset = subset.dropna(subset=['Delta log10(pO2/p_ref)'])
            if subset.empty:
                continue

            ax.plot(
                subset['O/M ratio (/)'],
                subset['Delta log10(pO2/p_ref)'],
                color=temperature_colors[temperature_k],
                marker='o',
            )

        ax.axhline(0.0, color='black', linestyle='--')
        ax.set_title(f'q = {q_value:.2f}')
        ax.set_xlabel('O/M ratio (-)')
        ax.set_ylabel(r'$\Delta\log_{10}(p_{O_2}/p_{ref})$ (-)')
        ax.set_xlim([1.90, 2.20])
        ax.grid(True, alpha=0.3)
        add_temperature_legend(ax, temperatures_k, temperature_colors)
        fig.tight_layout()
        fig.savefig(ROOT_DIR / f'sciantix_vs_oc_csv_log_pO2_error_q_{q_tag(q_value)}.png')
        plt.close(fig)


def make_potential_plot(frame: pd.DataFrame) -> None:
    q_values = sorted(frame[Q_KEY_COL].dropna().unique())
    temperatures_k = sorted(frame[TEMPERATURE_KEY_COL].dropna().unique())
    temperature_colors = temperature_color_map(temperatures_k)
    q_markers = q_marker_map(q_values)

    for q_value in q_values:
        fig, ax = plt.subplots()
        q_frame = frame[frame[Q_KEY_COL] == q_value]

        for temperature_k in temperatures_k:
            subset = q_frame[q_frame[TEMPERATURE_KEY_COL] == temperature_k].sort_values('O/M ratio (/)')
            if subset.empty:
                continue

            ax.plot(
                subset['O/M ratio (/)'],
                subset['Interpolated SCIANTIX CALPHAD oxygen potential (KJ/mol)'],
                color=temperature_colors[temperature_k],
                linestyle='-',
            )
            ax.scatter(
                subset['O/M ratio (/)'],
                subset['OC oxygen potential (KJ/mol)'],
                color=temperature_colors[temperature_k],
                marker=q_markers[q_value],
                s=22,
            )

        ax.set_title(f'q = {q_value:.2f}')
        ax.set_xlabel('O/M ratio (-)')
        ax.set_ylabel('Oxygen potential (kJ/mol)')
        ax.set_xlim([1.90, 2.20])
        ax.grid(True, alpha=0.3)
        add_model_legends(ax, temperatures_k, temperature_colors)
        fig.tight_layout()
        fig.savefig(ROOT_DIR / f'sciantix_vs_oc_csv_potential_q_{q_tag(q_value)}.png')
        plt.close(fig)


def main() -> None:
    sciantix_frame = load_sciantix_data()
    oc_frame = load_oc_csv_data()
    aligned_frame = interpolate_sciantix_to_oc_points(sciantix_frame, oc_frame)

    aligned_frame.to_csv(COMPARISON_PATH, sep='\t', index=False)
    summary = build_metric_summary(aligned_frame)
    summary.to_csv(SUMMARY_OUTPUT_PATH, sep='\t', index=False)
    write_summary_report(aligned_frame, summary)

    make_pressure_plot(aligned_frame)
    make_signed_log_error_plot(aligned_frame)
    make_potential_plot(aligned_frame)


if __name__ == '__main__':
    main()
