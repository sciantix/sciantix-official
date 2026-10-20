#!/usr/bin/env python3
"""Speed benchmark and pilot convergence using unchanged production Solver.C."""
import argparse
import csv
import gzip
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import time

from analytic import matrices, projections, solution, VOLUME, zero_diffusion_continuum
import numpy as np
import scipy

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
BUILD = HERE / 'build'


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def save(path, obj):
    Path(path).write_text(json.dumps(obj, indent=2, allow_nan=False) + '\n')


def physical_case(case, step=100):
    path = ROOT / 'UN_model/regression_reference/results/full' / case / 'trace.jsonl.gz'
    with gzip.open(path, 'rt') as f:
        for line in f:
            event = json.loads(line)
            if event['stage'] == 'after_exchange' and event['step'] == step:
                s = event['scalars']
                return {'name': case, 'D': s['Dg'], 'R': 6e-6, 'gb': s['g_b'], 'gd': s['g_d'],
                        'bb': s['b_b'], 'bd': s['b_d'], 'source': [s['beta'], 0., 0.], 'initial': [0., 0., 0.],
                        'provenance': {'file': str(path.relative_to(ROOT)), 'sha256': sha(path), 'frozen_step': step,
                                       'restriction': 'fixed linear coefficients; nucleation sink/source and microstructure updates excluded'}}
    raise RuntimeError('Missing frozen coefficients')


def build(out):
    BUILD.mkdir(exist_ok=True)
    command = ['g++', '-std=c++11', '-O2', '-Wall', '-Wextra', '-I' + str(ROOT / 'include/classes'),
               str(HERE / 'solver_harness.cpp'), str(ROOT / 'src/classes/Solver.C'), '-o', str(BUILD / 'solver_harness.x')]
    start = time.perf_counter()
    with open(out / 'build.log', 'x') as f:
        subprocess.run(command, stdout=f, stderr=subprocess.STDOUT, check=True)
    return {'seconds': time.perf_counter() - start, 'command': command,
            'solver_sha256': sha(ROOT / 'src/classes/Solver.C'), 'harness_sha256': sha(HERE / 'solver_harness.cpp'),
            'binary_sha256': sha(BUILD / 'solver_harness.x')}


def run_cpp(folder, case, n, dt, end, record_interval=3600.):
    folder.mkdir(parents=True, exist_ok=False)
    stride = max(1, int(round(record_interval / dt)))
    fields = [n, dt, end, stride, case['D'], case['R'], case['gb'], case['gd'], case['bb'], case['bd'],
              *case['source'], *case['initial']]
    (folder / 'input.txt').write_text(' '.join(format(v, '.17g') for v in fields) + '\n')
    start = time.perf_counter()
    with open(folder / 'run.log', 'x') as f:
        subprocess.run([str(BUILD / 'solver_harness.x'), str(folder / 'input.txt'),
                        str(folder / 'output.bin'), str(folder / 'timing.json')], stdout=f, stderr=subprocess.STDOUT, check=True)
    process_seconds = time.perf_counter() - start
    a = np.fromfile(folder / 'output.bin', dtype='<f8').reshape(-1, 4 + 3 * n)
    timing = json.loads((folder / 'timing.json').read_text())
    timing['process_seconds'] = process_seconds
    return a[:, 0], a[:, 1:4], a[:, 4:].reshape(-1, 3, n), timing


def benchmark(out):
    case = physical_case('AP3p4_1600K')
    save(out / 'case.json', case)
    compilation = build(out)
    t, means, modes, cpp = run_cpp(out / 'cpp', case, 40, 3600., 86400.)
    start = time.perf_counter()
    exact, exact_modes = solution(case, 40, t)
    error = np.abs(means - exact)
    scale = max(np.abs(exact).max(), 1.0)
    analytic_seconds = time.perf_counter() - start
    np.savez_compressed(out / 'comparison.npz', time=t, cpp=means, analytic=exact, cpp_modes=modes, analytic_modes=exact_modes)
    result = {'status': 'completed', 'n': 40, 'dt_s': 3600., 'end_s': 86400.,
              'compile': compilation, 'cpp': cpp, 'analytic_python_seconds': analytic_seconds,
              'max_mean_error_over_global_scale': float(error.max() / scale),
              'note': '24 h is only an initial speed trial; BE discretization error is expected, not a regression failure.',
              'scipy': scipy.__version__, 'numpy': np.__version__, 'head': subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()}
    save(out / 'results.json', result)
    print(json.dumps(result, indent=2), flush=True)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('task', choices=['benchmark'])
    p.add_argument('--output', type=Path, required=True)
    a = p.parse_args()
    out = a.output.resolve()
    assert out.is_relative_to(HERE), 'All verification output must be below UN_model/VERIFICATION'
    out.mkdir(parents=True, exist_ok=False)
    benchmark(out)


if __name__ == '__main__':
    main()
