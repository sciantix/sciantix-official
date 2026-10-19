#!/usr/bin/env python3
"""Verify local merge ancestry, file preservation and new numerical runs."""
import hashlib
import json
from pathlib import Path
import subprocess
import sys

import numpy as np

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT / 'UN_model/phase2d3'))
from run_integration import CASES, REF, save, sha, table


def git(*args):
    return subprocess.check_output(['git', *args], cwd=ROOT, text=True).strip()


def main():
    before = json.loads((HERE / 'audit_before.json').read_text())
    assert git('branch', '--show-current') == 'development/nitride'
    assert git('rev-parse', 'origin/development/nitride') == before['origin_development_start']
    merge = git('log', '--merges', '--first-parent', '-1', '--format=%H')
    parents = git('show', '-s', '--format=%P', merge).split()
    feature = git('rev-parse', 'feature/un-l3-porting')
    assert parents == [before['development_start'], feature]
    assert git('rev-parse', merge + '^{tree}') == git('rev-parse', feature + '^{tree}')
    assert subprocess.run(['git', 'merge-base', '--is-ancestor', before['feature_start'], feature], cwd=ROOT).returncode == 0
    for name, h in before['file_sha256'].items():
        assert (ROOT / name).is_file() and sha(ROOT / name) == h, name

    results = HERE / 'results'
    full = json.loads((results / 'full/results.json').read_text())
    solver = json.loads((results / 'solver_state/results.json').read_text())
    grainface = json.loads((results / 'grainface_3x3/results.json').read_text())
    assert grainface['status'] == 'pass' and grainface['values'] == 9036
    assert all(m['failures'] == 0 and m['max_normalized_error'] == 0 for m in grainface['metrics'])
    old = ROOT / 'UN_model/phase2d3/results'
    old_full = json.loads((old / 'P3_full/results.json').read_text())
    assert full['status'] == solver['status'] == 'pass'
    assert full['failures'] == solver['failures'] == 0
    assert full['metrics'] == old_full['metrics']
    assert full['values_compared'] == old_full['values_compared'] == 4637652
    assert solver['values_compared'] == 5829810 and solver['max_normalized_error'] == 0
    manifest = json.loads((results / 'full/manifest.json').read_text())
    assert manifest['head'] == merge
    for name, h in manifest['source_sha256'].items():
        assert sha(ROOT / name) == h, name
    cases = {}
    for case in CASES:
        folder = results / 'full' / case
        names = ['output.txt', 'un_l3_diagnostics.txt', 'un_l3_modes.bin', 'un_l3_exchange_modes.bin']
        for name in names:
            assert (folder / name).read_bytes() == (old / 'P3_full' / case / name).read_bytes(), (case, name)
        a = table(folder / 'output.txt')
        d = table(folder / 'un_l3_diagnostics.txt')
        r = json.loads((REF / case / 'manifest.json').read_text())
        assert len(a) == r['steps'] + 1
        assert np.array_equal(a['step_count'], np.arange(len(a)))
        assert np.all(a['initialization_count'] == 1)
        assert a['dt'][-1] == r['last_dt_s'] and a['time'][-1] == r['final_time_s']
        for flag in ['bulk_first_gas_seed_done', 'dislocation_first_gas_seed_done', 'gf_saturated']:
            assert np.all(np.diff(a[flag]) >= 0)
        gf = a['q_gf'][1:] + a['q_rel'][1:] - (a['q_gf'][:-1] + a['q_rel'][:-1] + d['dq_to_gb'])
        gf_error = float((abs(gf) / np.maximum(a['generated'][1:], 1)).max())
        mass_error = float((abs(a['mass_balance_residual']) / np.maximum(a['generated'], 1)).max())
        assert gf_error < 1e-11 and mass_error < 1e-11
        b = json.loads((folder / 'boundary_checks.json').read_text())
        cases[case] = {k: v for k, v in b.items() if k != 'checks'}
        cases[case]['grainface_balance_max_normalized'] = gf_error
        cases[case]['outputs_identical_to_phase3_bytes'] = names

    historical = results / 'historical'
    audit = json.loads((historical / 'audit.json').read_text())
    assert audit['standard_all_counts'] == {'total': 109, 'passed': 109, 'failed': 0}
    assert audit['supplemental_counts'] == {'total': 1, 'passed': 0, 'failed': 1}
    assert audit['legacy_un']['counts'] == {'PASS': 91} and audit['gold_unchanged']
    paths = []
    for row in audit['named_column_audit']:
        name = row['case']
        relative = Path('snapshot/regression') / name / 'output.txt'
        assert (historical / relative).read_bytes() == (old / 'P3_nonregression' / relative).read_bytes(), name
        paths.append(name)
    assert len(paths) == 110
    assert (historical / 'legacy_un_notebook8_summary.csv').read_bytes() == (old / 'P3_nonregression/legacy_un_notebook8_summary.csv').read_bytes()
    summary = {'status': 'pass', 'merge_commit': merge, 'merge_parents': parents,
               'feature_backup': feature, 'origin_development_unchanged': git('rev-parse', 'origin/development/nitride'),
               'preserved_files': len(before['file_sha256']), 'full_values_compared': full['values_compared'],
               'full_max_normalized_error': full['max_normalized_error'], 'cases': cases,
               'solver_values_compared': solver['values_compared'], 'solver_max_error': 0,
               'grainface_3x3_values_compared': grainface['values'], 'grainface_3x3_max_error': 0,
               'historical_standard': audit['standard_all_counts'], 'preexisting_supplemental': audit['supplemental_counts'],
               'legacy_un': audit['legacy_un']['counts'], 'historical_outputs_identical_bytes': len(paths),
               'metadata_note': 'Old HEAD/branch/path hashes are historical provenance; original manifests were not rewritten.',
               'phase4_started': False}
    save(results / 'verification.json', summary)
    print('PASS: merge ancestry,', len(before['file_sha256']), 'preserved files, full L3/solver/persistence/mass and 110 unchanged historical outputs')


if __name__ == '__main__':
    main()
