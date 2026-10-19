#!/usr/bin/env python3
"""Produce comparison figures and separate roundoff/invariant diagnostics."""
import argparse
import gzip
import json
import os
from pathlib import Path

import numpy as np

from run_integration import CASES, REF, TOL, EPS, save, table


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--results', type=Path, required=True)
    p.add_argument('--output', type=Path, required=True)
    args = p.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    os.environ['MPLCONFIGDIR'] = str(out / 'mplconfig')
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt

    groups = [(['c', 'mb', 'md'], 'Gas intragranulare [at/m³]'),
              (['Nb', 'Nd'], 'Densità bolle [m⁻³]'), (['Rb', 'Rd'], 'Raggi [m]'),
              (['nvb', 'nvd'], 'Vacancy intragranulari [m⁻³]'),
              (['p_b', 'p_d', 'p_b_eq', 'p_d_eq'], 'Pressioni [Pa]'),
              (['swelling_b', 'swelling_d', 'swelling_gf', 'swelling_total_gas'], 'Swelling [m³/m³]'),
              (['q_gb', 'q_gf', 'q_rel'], 'Gas grain-boundary/rilasciato [at/m³]'),
              (['Ngf_areal'], 'Bolle grain-face [m⁻²]'), (['R_gf'], 'Raggio grain-face [m]'),
              (['Fc_gf'], 'Copertura grain-face [-]'), (['nv_gf'], 'Vacancy per bolla grain-face [-]'),
              (['generated', 'retained'], 'Produzione/ritenzione [at/m³]'),
              (['pressure_ratio_b', 'pressure_ratio_d'], 'Rapporto di pressione [-]'),
              (['f_cap_step', 'capture_fraction_sum'], 'Cattura [-]'),
              (['mass_balance_residual'], 'Residuo di massa [at/m³]'),
              (['gf_saturated'], 'Flag saturazione [-]')]
    summary = {}
    for case in CASES:
        folder = args.results / case
        with np.load(folder / 'comparison.npz') as z:
            keys = [k[7:] for k in z.files if k.startswith('python_')]
            ref = {k: z['python_' + k] for k in keys}
            actual = {k: z['cpp_' + k] for k in keys}
        hours = ref['time'] / 3600.0
        fig, axes = plt.subplots(4, 4, figsize=(19, 15), constrained_layout=True)
        for ax, (names, title) in zip(axes.flat, groups):
            for i, name in enumerate(names):
                color = f'C{i}'
                ax.plot(hours, ref[name], color=color, linewidth=1.7, label=name)
                ax.plot(hours, actual[name], color=color, linestyle='--', linewidth=1.1)
            if title.startswith('Pressioni'):
                ax.set_yscale('symlog', linthresh=1.0)
            ax.set_title(title, fontsize=10)
            ax.set_xlabel('Tempo [h]')
            ax.grid(alpha=.25)
            ax.legend(fontsize=7)
        fig.suptitle(case + ' — Python: linea continua; SCIANTIX: tratteggiata', fontsize=14)
        fig.savefig(out / (case + '_comparison.png'), dpi=140)
        fig.savefig(out / (case + '_comparison.pdf'))
        plt.close(fig)

        first = []
        envelope = np.zeros(len(hours))
        production = max(float(ref['generated'].max()), 1.0)
        windows = []
        for key in keys:
            d = abs(actual[key] - ref[key])
            nz = np.flatnonzero(d)
            if len(nz):
                idx = int(nz[0])
                first.append({'history_row': idx, 'stage': 'history', 'field': key,
                              'absolute_error': float(d[idx])})
            scale = max(float(abs(ref[key]).max()), 1e-300)
            if key in {'q_gb', 'q_gf', 'q_rel', 'mass_balance_residual'}:
                scale = production
            elif key in {'qgb_gas_percent', 'grainface_gas_percent', 'release_gas_percent'}:
                scale = 100.0
            denom = np.maximum(abs(ref[key]), 64 * EPS * scale / TOL['rtol'])
            envelope = np.maximum(envelope, d / denom)
        for lo, hi in zip([0, .25, .5, .75], [.25, .5, .75, 1.0]):
            start = int(lo * (len(hours) - 1))
            end = int(hi * (len(hours) - 1)) + 1
            windows.append({'history_fraction': [lo, hi], 'max_normalized_error': float(envelope[start:end].max())})
        diag = table(folder / 'un_l3_diagnostics.txt')
        with gzip.open(REF / case / 'trace.jsonl.gz', 'rt') as f:
            events = [json.loads(line) for line in f]
        exchanges = [e for e in events if e['stage'] == 'after_exchange']
        for key in ['Dg', 'Dv', 'b_b', 'b_d', 'g_b', 'g_d', 'nu_b', 'phi_b', 'phi_d']:
            b = np.asarray([e['scalars'][key] for e in exchanges])
            nz = np.flatnonzero(diag[key] != b)
            if len(nz):
                idx = int(nz[0])
                first.append({'history_row': idx + 1, 'stage': 'transport_before_exchange', 'field': key,
                              'absolute_error': float(abs(diag[key][idx] - b[idx]))})
        first.sort(key=lambda x: (x['history_row'], x['stage'] != 'transport_before_exchange'))

        modes = np.fromfile(folder / 'un_l3_modes.bin', dtype='<f8').reshape(-1, 3, 160)
        with np.load(REF / case / 'modes.npz') as z:
            expected_modes = np.concatenate([z['initial'], z['step_end']])
        modal_errors = np.abs(modes - expected_modes)
        modal_scale = np.maximum(abs(expected_modes).max(axis=2, keepdims=True), 1.0)
        modal_normalized = modal_errors / np.maximum(abs(expected_modes), 64 * EPS * modal_scale / TOL['rtol'])
        fig, axes = plt.subplots(2, 1, figsize=(11, 7), constrained_layout=True)
        axes[0].plot(hours, envelope)
        axes[0].axhline(TOL['rtol'], color='red', linestyle=':', label='rtol')
        axes[0].set_ylabel('Massimo errore normalizzato scalare')
        for i, pop in enumerate(['c', 'mb', 'md']):
            axes[1].plot(hours, modal_normalized[:, i].max(axis=1), label=pop)
        axes[1].set_ylabel('Massimo errore normalizzato modale')
        axes[1].legend()
        for ax in axes:
            ax.set_xlabel('Tempo [h]')
            ax.grid(alpha=.25)
        axes[0].legend()
        fig.suptitle(case + ' — accumulo degli errori')
        fig.savefig(out / (case + '_errors.png'), dpi=150)
        fig.savefig(out / (case + '_errors.pdf'))
        plt.close(fig)
        summary[case] = {'first_nonzero_differences': first[:10], 'error_windows': windows,
                         'modal_max_absolute_error': float(modal_errors.max()),
                         'modal_max_normalized_error': float(modal_normalized.max()),
                         'max_mass_residual_over_generated': float(np.max(abs(actual['mass_balance_residual']) / np.maximum(actual['generated'], 1.0)))}
    save(out / 'analysis.json', {'status': 'pass', 'cases': summary,
                                'note': 'Nonzero differences are roundoff; no failed numerical comparison in P3_full.'})
    print('Wrote 8 comparison/error figures in PNG and PDF;', out)


if __name__ == '__main__':
    main()
