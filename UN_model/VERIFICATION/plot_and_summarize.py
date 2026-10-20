#!/usr/bin/env python3
"""Figures, measured costs and arithmetic LHS budget proposal; no sampling."""
import argparse
import csv
import json
import os
from pathlib import Path
import numpy as np
from run_verification import HERE, save


def rows(path):
    with open(path) as f:
        return list(csv.DictReader(f))


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--pilots', type=Path, required=True)
    p.add_argument('--output', type=Path, required=True)
    args = p.parse_args()
    out = args.output.resolve()
    assert out.is_relative_to(HERE)
    out.mkdir(parents=True, exist_ok=False)
    os.environ['MPLCONFIGDIR'] = str(out / 'mplconfig')
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    temporal = rows(args.pilots / 'temporal_errors.csv')
    spatial = rows(args.pilots / 'spatial_errors.csv')
    cases = json.loads((args.pilots / 'cases.json').read_text())
    refs = json.loads((args.pilots / 'reference_convergence.json').read_text())
    benchmark = json.loads((HERE / 'results/speed_initial/results.json').read_text())

    def store(fig, name):
        fig.savefig(out / (name + '.png'), dpi=160)
        fig.savefig(out / (name + '.pdf'))
        plt.close(fig)

    fig, axes = plt.subplots(1, 2, figsize=(13, 5), constrained_layout=True)
    for i, case in enumerate(cases):
        rs = [r for r in temporal if r['case'] == case['name']]
        dt = np.array([float(r['dt_s']) for r in rs])
        for ax, metric in zip(axes, ['mean_normalized_error', 'modal_l2_normalized_error']):
            ax.loglog(dt, [float(r[metric]) for r in rs], 'o-', label=case['name'])
    guide_dt = np.array([225., 3600.])
    axes[0].loglog(guide_dt, guide_dt * 1e-6, 'k:', label='pendenza 1')
    for ax, title in zip(axes, ['Medie: massimo sui tempi comuni', 'Modi: norma L2 sui tempi comuni']):
        ax.set_title(title);ax.set_xlabel('Δt [s]');ax.set_ylabel('Errore normalizzato [-]');ax.grid(True, which='both', alpha=.25)
        ax.legend(fontsize=7)
    fig.suptitle('Convergenza temporale — N=40, tempo finale fisso per caso')
    store(fig, 'temporal_convergence')

    fig, axes = plt.subplots(1, 2, figsize=(13, 5), constrained_layout=True)
    for case in cases:
        rs = [r for r in temporal if r['case'] == case['name'] and r['observed_mean_order']]
        if not rs:
            continue
        for ax, metric in zip(axes, ['observed_mean_order', 'observed_modal_order']):
            ax.semilogx([float(r['dt_s']) for r in rs], [float(r[metric]) for r in rs], 'o-', label=case['name'])
    for ax, title in zip(axes, ['Ordine sulle medie', 'Ordine sui modi']):
        ax.axhline(1, color='k', linestyle=':');ax.set_title(title);ax.set_xlabel('Δt [s]');ax.set_ylabel('p osservato')
        ax.grid(alpha=.25);ax.legend(fontsize=7)
    fig.suptitle('Source-only escluso dall’ordine: errore a livello di arrotondamento')
    store(fig, 'temporal_orders')

    fig, axes = plt.subplots(1, 2, figsize=(13, 5), constrained_layout=True)
    for case in cases:
        rs = [r for r in spatial if r['case'] == case['name']]
        n = [int(r['n']) for r in rs]
        for ax, metric in zip(axes, ['mean_total_normalized_error', 'profile_l2_normalized_error']):
            ax.loglog(n, [float(r[metric]) for r in rs], 'o-', label=case['name'])
    ng = np.array([10., 160.])
    axes[0].loglog(ng, .3 / ng**3, 'k:', label='N⁻³');axes[0].loglog(ng, .6 / ng, 'k--', label='N⁻¹')
    axes[1].loglog(ng, .3 / ng**2.5, 'k:', label='N⁻²·⁵');axes[1].loglog(ng, .7 / ng**.5, 'k--', label='N⁻⁰·⁵')
    for ax, title in zip(axes, ['Medie di volume', 'Profilo sferico, norma L2']):
        ax.set_title(title);ax.set_xlabel('N modi');ax.set_ylabel('Errore normalizzato [-]');ax.grid(True, which='both', alpha=.25)
        ax.legend(fontsize=7)
    fig.suptitle('Convergenza spaziale allo stato finale lungo — T e Δt fissi per caso')
    store(fig, 'spatial_convergence')

    fig, ax = plt.subplots(figsize=(10, 5), constrained_layout=True)
    for case in cases:
        rs = [r for r in refs if r['case'] == case['name']]
        x, raw, corrected = [], [], []
        for previous, current in zip(rs, rs[1:]):
            scale = max(np.max(np.abs(current['reference_mean'])), 1.)
            x.append(current['n'])
            raw.append(float(np.max(abs(np.array(current['raw_mean']) - previous['raw_mean'])) / scale))
            corrected.append(current['relative_successive_gap'])
        if case['D'] == 0:
            ax.loglog(x, raw, 'x--', label=case['name'] + ' raw')
        values = np.array(corrected, dtype=float)
        ax.loglog(x, np.where(values > 0, values, np.nan), 'o-', label=case['name'] + (' coda esatta' if case['D'] == 0 else ''))
    ax.set_xlabel('N reference più alto');ax.set_ylabel('Differenza relativa fra reference successive')
    ax.set_title('Controllo della reference spaziale (zeri esatti omessi)');ax.grid(True, which='both', alpha=.25);ax.legend(fontsize=7)
    store(fig, 'reference_convergence')

    fig, ax = plt.subplots(figsize=(11, 5), constrained_layout=True)
    for i, case in enumerate(cases):
        times = case['characteristics']['1'][0]['finite_times_s']
        if times:
            ax.scatter(times, [i] * len(times), color='C0', marker='o')
        ax.scatter(case['temporal_end_s'], i, color='C1', marker='x')
        ax.scatter(case['spatial_end_s'], i, color='C2', marker='s')
    ax.scatter([], [], color='C0', label='τ modale, n=1');ax.scatter([], [], color='C1', marker='x', label='T temporale pilota')
    ax.scatter([], [], color='C2', marker='s', label='T spaziale matematico')
    ax.set_yticks(range(len(cases)), [c['name'] for c in cases]);ax.set_xscale('log')
    ax.set_xlabel('Tempo [s]');ax.grid(alpha=.25);ax.legend(fontsize=8)
    ax.set_title('Tempi caratteristici: coefficienti congelati, non durate del modello fisico completo')
    store(fig, 'characteristic_times')

    fig, ax = plt.subplots(figsize=(9, 5), constrained_layout=True)
    labels = ['Compilazione', 'Processo C++\n(include avvio/I/O)', 'Solo solver C++', 'Analitica Python\n+ confronto']
    vals = [benchmark['compile']['seconds'], benchmark['cpp']['process_seconds'], benchmark['cpp']['solver_seconds'], benchmark['analytic_python_seconds']]
    ax.bar(labels, vals);ax.set_yscale('log');ax.set_ylabel('Tempo wall [s]');ax.set_title('Primo benchmark: N=40, Δt=1 h, T=24 h')
    for i, v in enumerate(vals):ax.text(i, v * 1.1, f'{v:.3g}', ha='center')
    store(fig, 'initial_speed')

    # Only arithmetic budget estimates; no LHS points or random design exist.
    costs = np.array([float(r['process_seconds']) + float(r['analytic_seconds']) for r in temporal])
    median, p95 = float(np.median(costs)), float(np.percentile(costs, 95))
    budgets = [{'samples_proposed': n, 'median_single_configuration_s': median,
                'p95_single_configuration_s': p95, 'five_dt_sweeps_median_s': 5 * n * median,
                'five_dt_sweeps_p95_with_3x_margin_s': 15 * n * p95} for n in [32, 64, 128, 256, 512]]
    with open(out / 'lhs_budget_proposal.csv', 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(budgets[0]));w.writeheader();w.writerows(budgets)
    summary = {'status': 'pass', 'figures': 6, 'formats': ['PNG','PDF'], 'lhs_sampling_executed': False,
        'temporal_solver_seconds_total': sum(float(r['solver_seconds']) for r in temporal),
        'temporal_analytic_seconds_total': sum(float(r['analytic_seconds']) for r in temporal),
        'spatial_solver_seconds_total': sum(float(r['solver_seconds']) for r in spatial),
        'reference_analytic_seconds_total': sum(r['analytic_seconds'] for r in refs),
        'lhs_cost_basis': {'median_s': median, 'p95_s': p95}, 'lhs_budget_proposal': budgets}
    save(out / 'summary.json', summary)
    print('6 figures in PNG/PDF; budget proposals only, no LHS sampling')


if __name__ == '__main__':
    main()
