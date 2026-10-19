#!/usr/bin/env python3
"""Run the real SCIANTIX executable; compare only against frozen Phase 1 files."""
import argparse
import csv
import gzip
import hashlib
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time

import numpy as np

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
REF = ROOT / 'UN_model/regression_reference/results/full'
CASES = ['AP3p2_900K', 'AP3p4_1600K', 'AP3p4_1800K', 'ANP6_1600K']
DEFAULT_CMAKE = '/tmp/sciantix-un-l3-phase1-tools/cmake/data/bin/cmake'
TOL = json.loads((HERE / 'tolerances.json').read_text())
EPS = TOL['float64_epsilon']


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def save(path, value):
    Path(path).write_text(json.dumps(value, indent=2, allow_nan=False) + '\n')


def table(path):
    text = '\n'.join(line.rstrip() for line in Path(path).read_text().splitlines())
    return np.genfromtxt(io.StringIO(text), names=True, delimiter='\t', deletechars='', autostrip=True)


def compare(actual, expected, case, stage, field, scale=None, discrete=False):
    a, b = np.asarray(actual), np.asarray(expected)
    assert a.shape == b.shape, (case, stage, field, a.shape, b.shape)
    absolute = np.abs(a - b)
    if scale is None:
        scale = max(float(np.max(np.abs(b))), 1e-300)
    allowance = TOL['rtol'] * np.abs(b) + TOL['roundoff_multiplier'] * EPS * scale
    bad = ~np.isfinite(a) | ~np.isfinite(b) | ((absolute != 0) if discrete else (absolute > allowance))
    normalized = absolute / np.maximum(np.abs(b), np.asarray(scale) * TOL['roundoff_multiplier'] * EPS / TOL['rtol'])
    flat = int(np.argmax(normalized))
    index = tuple(int(i) for i in np.unravel_index(flat, a.shape))
    failures = np.argwhere(bad)
    rel = np.divide(absolute, np.abs(b), out=np.zeros_like(absolute), where=b != 0)
    return {'case': case, 'stage': stage, 'field': field, 'count': a.size,
            'failures': int(bad.sum()), 'max_absolute_error': float(absolute.max()),
            'max_relative_error_nonzero': float(rel.max()), 'max_normalized_error': float(normalized.max()),
            'max_index': index, 'first_failure': failures[0].tolist() if len(failures) else None,
            'cpp_at_max': float(a[index]), 'python_at_max': float(b[index]), 'discrete': discrete}


def compare_case(folder, case, steps):
    metrics = []
    actual = table(folder / 'output.txt')
    diag = table(folder / 'un_l3_diagnostics.txt')
    assert len(actual) == steps + 1 and len(diag) == steps
    with np.load(REF / case / 'history.npz') as z:
        expected = {key: z[key][:steps + 1] for key in z.files}
    production = max(float(expected['generated'].max()), 1.0)
    cancellation = {'q_gb', 'q_gf', 'q_rel', 'mass_balance_residual'}
    for key, b in expected.items():
        scale = production if key in cancellation else None
        if key in {'qgb_gas_percent', 'grainface_gas_percent', 'release_gas_percent'}:
            scale = 100.0
        metrics.append(compare(actual[key], b, case, 'history', key, scale, key == 'gf_saturated'))
    with gzip.open(REF / case / 'trace.jsonl.gz', 'rt') as f:
        events = [json.loads(line) for line in f]
    by_stage = {}
    for e in events:
        if e['step'] < steps:
            by_stage.setdefault(e['stage'], []).append(e)
    capture = by_stage['after_capture']
    exchange = by_stage['after_exchange']
    end = by_stage['step_end']
    seed = by_stage['after_seed']
    vacancy = by_stage['after_vacancies']
    for key in ['bulk_first_gas_seed_done', 'dislocation_first_gas_seed_done']:
        b = np.r_[0.0, [float(e['scalars'][key]) for e in end]]
        metrics.append(compare(actual[key], b, case, 'state', key, discrete=True))
    metrics.append(compare(actual['qv_gf'][1:], [e['gf']['qv_gf'] for e in end], case, 'state', 'qv_gf'))
    for key in diag.dtype.names:
        b = []
        for i in range(steps):
            v = {**capture[i].get('Dv_parts', {}), **capture[i]['scalars']}
            if key in {'c_new_exchange', 'mb_new_exchange', 'md_new_exchange'}:
                value = exchange[i]['scalars'][key.replace('_exchange', '')]
            elif key in {'nvb_after_seed', 'nvd_after_seed'}:
                value = seed[i]['scalars'][key[:3]]
            elif key in {'nvb_after_vacancy', 'nvd_after_vacancy'}:
                value = vacancy[i]['scalars'][key[:3]]
            elif key in {'dq_to_gb', 'q_gb_total_target'}:
                value = end[i]['scalars'][key]
            elif key == 'negative_reset':
                value = float(any(exchange[i]['scalars'][k] < 0.0 for k in ['c_new', 'mb_new', 'md_new']))
            elif key == 'captured_bubbles':
                value = v.get(key, 0.0) if v['f_cap'] > 0.0 else 0.0
            else:
                value = v[key]
            b.append(float(value))
        metrics.append(compare(diag[key], b, case, 'intermediate', key,
                               production if key in {'dq_to_gb', 'q_gb_total_target'} else None))
    modes = np.fromfile(folder / 'un_l3_modes.bin', dtype='<f8').reshape(steps + 1, 3, 160)
    exchanged = np.fromfile(folder / 'un_l3_exchange_modes.bin', dtype='<f8').reshape(steps, 3, 160)
    with np.load(REF / case / 'modes.npz') as z:
        for stage, a, b in [('initial', modes[:1], z['initial']),
                            ('after_exchange', exchanged, z['after_exchange'][:steps]),
                            ('after_capture', modes[1:], z['after_capture'][:steps]),
                            ('step_end', modes[1:], z['step_end'][:steps])]:
            for pop, name in enumerate(['c', 'mb', 'md']):
                scale = np.maximum(np.abs(b[:, pop]).max(axis=1, keepdims=True), 1.0)
                metrics.append(compare(a[:, pop], b[:, pop], case, stage, 'modes_' + name, scale))

    # Separate structural checks: every real temporal call advances exactly once.
    assert np.array_equal(actual['step_count'], np.arange(steps + 1))
    assert np.all(actual['initialization_count'] == 1)
    assert np.all(np.diff(actual['generated']) > 0.0)
    assert np.all(actual['dt'][1:] > 0.0) and np.all(actual['dt'][1:] <= 10800.0)
    assert np.array_equal(actual['time'][1:] - actual['time'][:-1], actual['dt'][1:])
    mass = np.abs(actual['mass_balance_residual']) / np.maximum(actual['generated'], 1.0)
    assert mass.max() < 1e-11
    sat = np.flatnonzero(actual['gf_saturated'])
    ref_sat = np.flatnonzero(expected['gf_saturated'])
    assert np.array_equal(sat, ref_sat)
    first_sat = int(sat[0]) if len(sat) else None
    selected = {'initial': 0, 'first': 1, 'third': 3, 'last': steps}
    if first_sat is not None:
        selected.update({'before_saturation': first_sat - 1, 'first_saturation': first_sat})
    windows = {}
    for name, idx in selected.items():
        windows[name] = {k: {'cpp': float(actual[k][idx]), 'python': float(b[idx]),
                            'absolute_error': float(abs(actual[k][idx] - b[idx]))} for k, b in expected.items()}
    save(folder / 'boundary_checks.json', {'steps': steps, 'first_saturation_history_row': first_sat,
         'max_mass_residual_over_generated': float(mass.max()), 'last_dt_s': float(actual['dt'][-1]),
         'single_initialization': True, 'exact_step_count': True, 'checks': windows})
    np.savez_compressed(folder / 'comparison.npz', **{'cpp_' + k: actual[k] for k in expected},
                        **{'python_' + k: v for k, v in expected.items()})
    return metrics


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--output', type=Path, required=True)
    p.add_argument('--suite', choices=['diagnostic', 'full'], required=True)
    p.add_argument('--cmake', default=DEFAULT_CMAKE)
    p.add_argument('--sanitize', action='store_true')
    a = p.parse_args()
    out = a.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    snapshot = out / 'snapshot'
    snapshot.mkdir()
    for name in ['src', 'include']:
        shutil.copytree(ROOT / name, snapshot / name)
    shutil.copy2(ROOT / 'CMakeLists.txt', snapshot)
    env = {**os.environ, 'ASAN_OPTIONS': 'detect_leaks=0'}
    manifest = {'suite': a.suite, 'status': 'running', 'commands': [], 'tolerances': TOL,
                'tolerances_sha256': sha(HERE / 'tolerances.json'), 'runner_sha256': sha(__file__),
                'head': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                'compiler': subprocess.check_output(['g++', '--version'], text=True),
                'source_sha256': {str(f.relative_to(snapshot)): sha(f) for f in snapshot.rglob('*') if f.is_file()}}

    def run(name, cmd, cwd=out):
        start = time.perf_counter()
        with open(out / (name + '.log'), 'x') as f:
            r = subprocess.run([str(x) for x in cmd], cwd=cwd, stdout=f, stderr=subprocess.STDOUT, env=env)
        manifest['commands'].append({'name': name, 'argv': [str(x) for x in cmd], 'returncode': r.returncode,
                                     'wall_seconds': time.perf_counter() - start})
        save(out / 'manifest.json', manifest)
        if r.returncode:
            raise RuntimeError(f'{name} failed: see {out / (name + ".log")}')

    build = snapshot / 'build'
    configure = [a.cmake, '-S', snapshot, '-B', build,
                 '-DCMAKE_BUILD_TYPE=' + ('Debug' if a.sanitize else 'Release')]
    if a.sanitize:
        configure += ['-DCMAKE_CXX_FLAGS=-O1 -g -fsanitize=address,undefined -fno-omit-frame-pointer',
                      '-DCMAKE_EXE_LINKER_FLAGS=-fsanitize=address,undefined']
    run('configure', configure)
    run('build', [a.cmake, '--build', build, '-j', '2'])
    binary = build / 'sciantix.x'
    manifest['binary_sha256'] = sha(binary)
    manifest['cases'] = {}
    metrics = []
    template = (ROOT / 'regression/baker/test_Baker1977__1273K/input_initial_conditions.txt').read_text()
    for case in CASES:
        folder = out / case
        folder.mkdir()
        ref_manifest = json.loads((REF / case / 'manifest.json').read_text())
        for name, h in ref_manifest['output_sha256'].items():
            assert sha(REF / case / name) == h, (case, name)
        params = ref_manifest['parameters']
        steps = 3 if a.suite == 'diagnostic' else ref_manifest['steps']
        final = 32400.0 if a.suite == 'diagnostic' else ref_manifest['final_time_s']
        settings = [0] * 28
        settings[7], settings[11], settings[25], settings[26], settings[27] = 1, 3, 1, 2, 1
        (folder / 'input_settings.txt').write_text(''.join(f'{v} # option {i}\n' for i, v in enumerate(settings)))
        (folder / 'input_initial_conditions.txt').write_text(format(params['grain_radius'], '.17g') + '\n' + template.split('\n', 1)[1])
        (folder / 'input_history.txt').write_text(''.join(
            f"{t / 3600.0:.17g} {params['temperature']:.17g} {params['fission_rate']:.17g} {params['hydrostatic_stress']:.17g}\n"
            for t in [0.0, final]))
        run(case, [binary, folder], folder)
        manifest['cases'][case] = {'steps': steps, 'reference_manifest_sha256': sha(REF / case / 'manifest.json'),
                                  'input_sha256': {f.name: sha(f) for f in folder.glob('input_*.txt')},
                                  'output_sha256': {f.name: sha(f) for f in folder.iterdir() if f.is_file()}}
        metrics.extend(compare_case(folder, case, steps))
    failures = sum(m['failures'] for m in metrics)
    result = {'status': 'pass' if not failures else 'fail', 'suite': a.suite,
              'values_compared': sum(m['count'] for m in metrics), 'failures': failures,
              'max_normalized_error': max(m['max_normalized_error'] for m in metrics),
              'metrics': metrics, 'tolerances': TOL}
    manifest['status'] = result['status']
    save(out / 'results.json', result)
    save(out / 'manifest.json', manifest)
    with open(out / 'errors.csv', 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(metrics[0]))
        w.writeheader()
        w.writerows(metrics)
    print(result['status'], result['values_compared'], 'values, failures', failures,
          'max normalized', result['max_normalized_error'])
    if failures:
        print('First failing field:', next(m for m in metrics if m['failures']))
    return bool(failures)


if __name__ == '__main__':
    raise SystemExit(main())
