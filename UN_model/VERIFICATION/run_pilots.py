#!/usr/bin/env python3
"""Five pilot cases; temporal IVP convergence and final-state spatial convergence."""
import argparse
import csv
import json
from pathlib import Path
import time

import numpy as np

from analytic import matrices, projections, solution, zero_diffusion_continuum, VOLUME
from run_verification import HERE, build, physical_case, run_cpp, save


def characteristics(case, n):
    j = matrices(case, n)
    records = []
    for idx in sorted(set([0, n - 1])):
        eig = np.linalg.eigvals(j[idx])
        assert abs(eig.imag).max() < 1e-10
        rates = -eig.real
        threshold = 100 * np.finfo(float).eps * max(np.linalg.norm(j[idx]), 1e-300)
        assert rates.min() > -threshold
        positive = rates[rates > threshold]
        records.append({'mode': idx + 1, 'decay_rates_s-1': rates.tolist(),
                        'zero_eigenvalues': int((abs(rates) <= threshold).sum()),
                        'finite_times_s': sorted((1 / positive).tolist())})
    return records


def make_cases():
    ordinary = physical_case('AP3p4_1600K')
    ordinary.update(name='ordinary_L3_1600K', temporal_end_s=172800.)
    stiff = physical_case('AP3p4_1800K')
    stiff.update(name='stiff_L3_1800K', temporal_end_s=21600.)
    pure = {**stiff, 'name': 'pure_diffusion', 'gb': 0., 'gd': 0., 'bb': 0., 'bd': 0., 'temporal_end_s': 64800.}
    exchange = {**ordinary, 'name': 'exchange_only', 'D': 0., 'gb': ordinary['gb'] * .01,
                'gd': ordinary['gd'] * .01, 'initial': [1e23, 2e22, 1e22], 'temporal_end_s': 172800.}
    source = {**ordinary, 'name': 'source_only', 'D': 0., 'gb': 0., 'gd': 0., 'bb': 0., 'bd': 0., 'temporal_end_s': 43200.}
    cases = [ordinary, stiff, pure, exchange, source]
    for case in cases:
        case['characteristics'] = {str(n): characteristics(case, n) for n in [1, 40, 160]}
        times = case['characteristics']['1'][0]['finite_times_s']
        slow = max(times) if times else 1e24 / max(case['source'])
        case['spatial_end_s'] = 32 * slow
        case['spatial_dt_start_s'] = slow / 128
        case['sampling_proposal_s'] = {'early': [min(times) * .01, min(times) * 5] if times else None,
                                      'intermediate': [min(times) if times else slow, slow], 'long_time': [slow, 10 * slow]}
    return cases


def error_row(case, n, dt, end, actual, exact, modes, exact_modes, timing, analytic_seconds):
    scale = max(float(abs(exact).max()), 1.)
    modal_scale = max(float(np.linalg.norm(exact_modes, axis=(1, 2)).max()), 1.)
    delta = abs(actual - exact)
    raw = np.divide(delta, abs(exact), out=np.zeros_like(delta), where=exact != 0)
    return {'case': case['name'], 'n': n, 'dt_s': dt, 'end_s': end, 'steps': timing['steps'],
            'mean_max_absolute_error': float(delta.max()), 'mean_max_relative_nonzero': float(raw.max()),
            'mean_normalized_error': float(delta.max() / scale),
            'modal_l2_normalized_error': float(np.linalg.norm(modes - exact_modes, axis=(1, 2)).max() / modal_scale),
            'solver_seconds': timing['solver_seconds'], 'process_seconds': timing['process_seconds'],
            'analytic_seconds': analytic_seconds, 'observed_mean_order': None, 'observed_modal_order': None}


def write_csv(path, rows):
    with open(path, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)


def run(out, speed):
    # The initial benchmark must already have completed, before the many tests.
    assert speed.exists() and json.loads(speed.read_text())['cpp']['steps'] == 24
    compilation = build(out)
    cases = make_cases()
    save(out / 'cases.json', cases)
    support = []
    for n in [10, 20, 40, 80, 160]:
        t, a, modes, timing = run_cpp(out / 'support' / str(n), cases[0], n, 3600., 3600.)
        assert np.isfinite(modes).all() and np.isfinite(a).all()
        assert abs(timing['mode_initialization_mean'] - 1) < 1e-13
        support.append({'n': n, 'status': 'pass', **timing})
    save(out / 'supported_modes.json', {'status': 'pass', 'tests': support})
    temporal, per_variable, spatial, refs, attempts, stable_checks = [], [], [], [], [], []
    for case in cases:
        name, end = case['name'], case['temporal_end_s']
        for dt in [3600., 1800., 900., 450., 225.]:
            folder = out / 'temporal' / name / str(int(dt))
            t, a, modes, timing = run_cpp(folder, case, 40, dt, end)
            start = time.perf_counter()
            exact, em = solution(case, 40, t)
            analytic_seconds = time.perf_counter() - start
            row = error_row(case, 40, dt, end, a, exact, modes, em, timing, analytic_seconds)
            previous = [r for r in temporal if r['case'] == name]
            for metric, order in [('mean_normalized_error', 'observed_mean_order'), ('modal_l2_normalized_error', 'observed_modal_order')]:
                if previous and min(row[metric], previous[-1][metric]) > 1e-12:
                    row[order] = float(np.log2(previous[-1][metric] / row[metric]))
            temporal.append(row)
            for i, pop in enumerate(['c', 'mb', 'md']):
                characteristic = max(float(abs(exact[:, i]).max()), 1.)
                per_variable.append({'case': name, 'dt_s': dt, 'population': pop,
                    'max_abs_error': float(abs(a[:, i] - exact[:, i]).max()),
                    'normalized_error': float(abs(a[:, i] - exact[:, i]).max() / characteristic)})
            np.savez_compressed(folder / 'comparison.npz', time=t, cpp=a, analytical=exact)
        # Cross-check two independent expm formulations before using the more
        # stable equilibrium-shift form at very long physical final times.
        formulation = 'equilibrium_shift' if case['D'] > 0 and case['bb'] > 0 and case['bd'] > 0 else 'augmented'
        if formulation == 'equilibrium_shift':
            aa, _ = solution(case, 40, [end])
            bb, _ = solution(case, 40, [end], formulation)
            gap = float(abs(aa - bb).max() / max(abs(aa).max(), 1))
            assert gap < 1e-9
            stable_checks.append({'case': name, 'augmented_vs_shifted_relative': gap})
        end = case['spatial_end_s']
        previous = None
        reference = None
        for high_n in [320, 640, 1280, 2560, 5120]:
            start = time.perf_counter()
            raw, high_modes = solution(case, high_n, [end], formulation)
            corrected = raw.copy()
            if case['D'] == 0:
                # For D=0 all modes have the same 3x3 matrix. The exact missing
                # Parseval tail is known: sum(q_n^2)=4*pi/3, no fit/extrapolation.
                corrected /= float(np.sum(projections(high_n)**2) / VOLUME)
            low, _ = solution(case, 160, [end], formulation)
            scale = max(float(abs(corrected).max()), 1.)
            smallest_mean = float(abs(low - corrected).max() / scale)
            gap = None if previous is None else float(abs(corrected - previous).max() / scale)
            refs.append({'case': name, 'n': high_n, 'raw_mean': raw[0].tolist(), 'reference_mean': corrected[0].tolist(),
                         'relative_successive_gap': gap, 'smallest_pilot_mean_truncation_error': smallest_mean,
                         'analytic_seconds': time.perf_counter() - start, 'exact_zero_D_tail': case['D'] == 0})
            if gap is not None and gap < min(1e-8, .01 * smallest_mean):
                reference = corrected
                break
            previous = corrected
        assert reference is not None, (name, 'high-N reference failed to converge')
        np.savez_compressed(out / (name + '_spatial_reference.npz'), means=reference, modes=high_modes)
        # Select dt at N=160 by a measured temporal/spatial mean-error ratio.
        # Long-time spatial tests examine the FINAL state, not the inaccurate
        # coarse-time transients of this deliberately separate experiment.
        dt = case['spatial_dt_start_s']
        for attempt in range(9):
            t, a, modes, timing = run_cpp(out / 'spatial_dt_selection' / name / str(attempt), case, 160, dt, end, end)
            exact, em = solution(case, 160, [end], formulation)
            temporal_abs = float(abs(a[-1:] - exact).max())
            truncation_abs = float(abs(exact - reference).max())
            ratio = temporal_abs / truncation_abs
            attempts.append({'case': name, 'attempt': attempt, 'dt_s': dt, 'steps': timing['steps'],
                             'temporal_over_spatial_mean_error': ratio, 'solver_seconds': timing['solver_seconds']})
            if ratio <= .05:
                break
            dt /= 2
        assert ratio <= .05, (name, 'could not isolate spatial error safely', ratio)
        for n in [10, 20, 40, 80, 160]:
            t, a, modes, timing = run_cpp(out / 'spatial' / name / str(n), case, n, dt, end, end)
            exact, em = solution(case, n, [end], formulation)
            total = float(abs(a[-1:] - reference).max() / scale)
            trunc = float(abs(exact - reference).max() / scale)
            time_error = float(abs(a[-1:] - exact).max() / scale)
            if case['D'] == 0:
                continuum = zero_diffusion_continuum(case, [end])[0]
                exact_first_modes = projections(n)[None, :] * continuum[:, None]
                tail_squared = np.sum(continuum**2) * max(VOLUME - np.sum(projections(n)**2), 0.)
                profile_error = np.sqrt(np.sum((modes[-1] - exact_first_modes)**2) + tail_squared)
                profile_scale = max(np.linalg.norm(continuum) * np.sqrt(VOLUME), 1.)
            else:
                profile_error = np.sqrt(np.sum((modes[-1] - high_modes[0, :, :n])**2) + np.sum(high_modes[0, :, n:]**2))
                profile_scale = max(np.linalg.norm(high_modes[0]), 1.)
            row = {'case': name, 'n': n, 'dt_s': dt, 'end_s': end, 'mean_total_normalized_error': total,
                   'mean_analytic_truncation_error': trunc, 'mean_temporal_normalized_error': time_error,
                   'profile_l2_normalized_error': float(profile_error / profile_scale),
                   'reference_n': high_n, 'solver_seconds': timing['solver_seconds'], 'observed_mean_order': None,
                   'observed_profile_order': None}
            previous_rows = [r for r in spatial if r['case'] == name]
            if previous_rows:
                for metric, key in [('mean_total_normalized_error', 'observed_mean_order'), ('profile_l2_normalized_error', 'observed_profile_order')]:
                    row[key] = float(np.log2(previous_rows[-1][metric] / row[metric]))
            spatial.append(row)
        # Preserve a coherent partial checkpoint after every complete case.
        write_csv(out / 'temporal_errors.csv', temporal)
        write_csv(out / 'temporal_errors_per_population.csv', per_variable)
        write_csv(out / 'spatial_errors.csv', spatial)
        save(out / 'reference_convergence.json', refs)
        save(out / 'spatial_dt_selection.json', attempts)
        save(out / 'formulation_checks.json', stable_checks)
        print(name, 'PASS; temporal finest error', temporal[-1]['mean_normalized_error'],
              'spatial dt', dt, 'ref N', high_n, flush=True)
    save(out / 'results.json', {'status': 'pass', 'cases': len(cases), 'temporal_runs': len(temporal),
          'spatial_runs': len(spatial), 'compilation': compilation, 'support_tests': len(support),
          'note': 'Spatial long-time final states and temporal early/intermediate trajectories are separate fixed-end experiments.'})


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--output', type=Path, required=True)
    p.add_argument('--benchmark', type=Path, default=HERE / 'results/speed_initial/results.json')
    args = p.parse_args()
    out = args.output.resolve()
    assert out.is_relative_to(HERE)
    out.mkdir(parents=True, exist_ok=False)
    run(out, args.benchmark.resolve())


if __name__ == '__main__':
    main()
