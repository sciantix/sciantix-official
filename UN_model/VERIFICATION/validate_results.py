#!/usr/bin/env python3
"""Independent scalar limits, discrete balances, completed studies and protection."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import numpy as np

from analytic import projections, solution, VOLUME
from run_verification import HERE, ROOT, save, sha


def read_cpp(path, n):
    a = np.fromfile(path, dtype='<f8').reshape(-1, 4 + 3 * n)
    return a[:, 0], a[:, 1:4], a[:, 4:].reshape(-1, 3, n)


def main():
    from scipy.special import zeta
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--pilots', type=Path, required=True)
    args = p.parse_args()
    folder = args.pilots.resolve()
    assert folder.is_relative_to(HERE)
    studies = json.loads((folder / 'results.json').read_text())
    assert studies['status'] == 'pass' and studies['cases'] == 5
    cases = {c['name']: c for c in json.loads((folder / 'cases.json').read_text())}
    basis = json.loads((HERE / 'basis_checks.json').read_text())
    assert basis['status'] == 'pass'
    checks = {}
    for name in ['source_only', 'pure_diffusion']:
        case = cases[name]
        t, means, modes = read_cpp(folder / 'temporal' / name / '225/output.bin', 40)
        exact, em = solution(case, 40, t)
        q = projections(40)
        if name == 'source_only':
            closed_modes = t[:, None, None] * np.asarray(case['source'])[None, :, None] * q[None, None, :]
        else:
            loss = case['D'] * (np.pi * np.arange(1, 41) / case['R'])**2
            closed_modes = np.zeros_like(em)
            closed_modes[:, 0] = -np.expm1(-t[:, None] * loss) / loss * case['source'][0] * q
        gap = float(abs(em - closed_modes).max() / max(abs(closed_modes).max(), 1.))
        assert gap < 1e-12
        checks[name + '_expm_vs_closed_form'] = gap

    for name in ['exchange_only', 'source_only']:
        case = cases[name]
        fraction = np.sum(projections(40)**2) / VOLUME
        t, means, modes = read_cpp(folder / 'temporal' / name / '225/output.bin', 40)
        total = fraction * (sum(case['initial']) + sum(case['source']) * t)
        residual = float(abs(means.sum(axis=1) - total).max() / max(abs(total).max(), 1.))
        assert residual < 1e-11
        checks[name + '_zero_D_mass_balance'] = residual

    # At the benchmark's dt every step was recorded: test the discrete BE
    # flux balance with the *new* dissolved modes, not a continuous surrogate.
    bench = HERE / 'results/speed_initial'
    case = json.loads((bench / 'case.json').read_text())
    t, means, modes = read_cpp(bench / 'cpp/output.bin', 40)
    weights = projections(40) / VOLUME
    loss = case['D'] * (np.pi * np.arange(1, 41) / case['R'])**2
    flux = (modes[1:, 0] * weights * loss).sum(axis=1)
    rhs = np.diff(t) * (sum(case['source']) * (projections(40)**2).sum() / VOLUME - flux)
    residual = float(abs(np.diff(means.sum(axis=1)) - rhs).max() / max(abs(rhs).max(), 1.))
    assert residual < 1e-11
    checks['benchmark_discrete_flux_balance'] = residual

    # Long-time reference also agrees with the continuum parabolic stationary
    # solution, independently of expm and of the production BE update.
    for name in ['ordinary_L3_1600K', 'stiff_L3_1800K', 'pure_diffusion']:
        case = cases[name]
        cbar = case['source'][0] * case['R']**2 / (15 * case['D'])
        exact = np.array([cbar, case['gb'] / case['bb'] * cbar if case['bb'] else 0.,
                          case['gd'] / case['bd'] * cbar if case['bd'] else 0.])
        with np.load(folder / (name + '_spatial_reference.npz')) as z:
            gap = float(abs(z['means'][0] - exact).max() / max(abs(exact).max(), 1.))
            high_n = z['modes'].shape[-1]
        assert gap < 1e-8
        checks[name + '_long_time_vs_parabolic_steady_mean'] = gap
        # Stationary mode coefficients scale as n^-3. The exact remaining
        # profile-energy tail is proportional to Hurwitz zeta(6,N+1).
        tail_ratio = float(np.sqrt(zeta(6, high_n + 1) / zeta(6, 161)))
        assert tail_ratio < .01
        checks[name + '_reference_L2_tail_over_N160_tail'] = tail_ratio

    before = json.loads((HERE / 'preservation_before.json').read_text())
    for name, h in before['files'].items():
        assert (ROOT / name).is_file() and sha(ROOT / name) == h, name
    for label, argv in [('head', ['rev-parse', 'HEAD']), ('branch', ['branch', '--show-current']),
                        ('origin', ['rev-parse', 'origin/development/nitride'])]:
        assert subprocess.check_output(['git', *argv], cwd=ROOT, text=True).strip() == before[label]
    assert hashlib.sha256(subprocess.check_output(['git','diff','--cached','--binary'],cwd=ROOT)).hexdigest() == before['staged_diff_sha256']
    changed = subprocess.check_output(['git','status','--porcelain=v1','-uall'],cwd=ROOT,text=True).splitlines()
    assert all(row[3:].startswith('UN_model/VERIFICATION/') for row in changed), changed
    save(HERE / 'validation.json', {'status': 'pass', 'checks': checks, 'protected_files': len(before['files']),
         'head_branch_origin_and_index_unchanged': True, 'all_new_files_inside_VERIFICATION': True,
         'scope': 'Phase 4A pilot verification only; no LHS and no reorganization performed'})
    print('PASS: basis, scalar limits, discrete balances, reference steady states;',len(before['files']),'protected files unchanged')


if __name__ == '__main__':
    main()
