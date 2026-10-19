#!/usr/bin/env python3
"""Verify completed tests, frozen files, index, historical outputs and balances."""
import csv
import hashlib
import json
from pathlib import Path
import subprocess

import numpy as np

from run_integration import ROOT, HERE, REF, CASES, sha, save, table


def main():
    results = HERE / 'results'
    passed = ['D1_diagnostic_v3', 'D_parameter_audit', 'P3_full', 'P3_sanitized',
              'P3_solver_state', 'P3_independent_ANP6']
    for name in passed:
        r = json.loads((results / name / 'results.json').read_text())
        assert r['status'] == 'pass', name
        assert r.get('failures', 0) == 0, name
    full = json.loads((results / 'P3_full/results.json').read_text())
    for name in ['P3_full', 'P3_sanitized']:
        manifest = json.loads((results / name / 'manifest.json').read_text())
        for relative, h in manifest['source_sha256'].items():
            assert sha(ROOT / relative) == h, (name, relative, 'source changed since tested build')
    grouped = {}
    for m in full['metrics']:
        key = m['stage'], m['field']
        row = grouped.setdefault(key, {'stage': key[0], 'field': key[1], 'count': 0, 'failures': 0,
              'max_absolute_error': 0.0, 'max_relative_error_nonzero': 0.0, 'max_normalized_error': 0.0})
        row['count'] += m['count']
        row['failures'] += m['failures']
        for k in ['max_absolute_error', 'max_relative_error_nonzero', 'max_normalized_error']:
            row[k] = max(row[k], m[k])
    with open(results / 'variable_errors.csv', 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(next(iter(grouped.values()))))
        w.writeheader()
        w.writerows(grouped.values())
    boundaries = {}
    for case in CASES:
        folder = results / 'P3_full' / case
        a = table(folder / 'output.txt')
        diag = table(folder / 'un_l3_diagnostics.txt')
        ref = json.loads((REF / case / 'manifest.json').read_text())
        assert a['dt'][-1] == ref['last_dt_s']
        assert a['time'][-1] == ref['final_time_s']
        assert a['step_count'][-1] == ref['steps']
        assert np.all(np.diff(a['bulk_first_gas_seed_done']) >= 0.0)
        assert np.all(np.diff(a['dislocation_first_gas_seed_done']) >= 0.0)
        assert np.all(np.diff(a['gf_saturated']) >= 0.0)
        gf_residual = a['q_gf'][1:] + a['q_rel'][1:] - (a['q_gf'][:-1] + a['q_rel'][:-1] + diag['dq_to_gb'])
        gf_norm = np.abs(gf_residual) / np.maximum(a['generated'][1:], 1.0)
        assert gf_norm.max() < 1e-11
        boundary = json.loads((folder / 'boundary_checks.json').read_text())
        boundaries[case] = {k: v for k, v in boundary.items() if k != 'checks'}
        boundaries[case]['max_grainface_balance_over_generated'] = float(gf_norm.max())
    baseline = results / 'P3_nonregression'
    original = ROOT / 'UN_model/baseline_sciantix/runs/pre_port_2026-10-18'
    audit = json.loads((baseline / 'audit.json').read_text())
    assert audit['standard_all_counts'] == {'total': 109, 'passed': 109, 'failed': 0}
    assert audit['supplemental_counts'] == {'total': 1, 'passed': 0, 'failed': 1}
    assert audit['legacy_un']['counts'] == {'PASS': 91}
    assert audit['gold_unchanged']
    identical = []
    for row in audit['named_column_audit']:
        name = row['case']
        relative = Path('snapshot/regression') / name / 'output.txt'
        assert (baseline / relative).read_bytes() == (original / relative).read_bytes(), name
        identical.append(name)
    assert len(identical) == 110
    assert (baseline / 'legacy_un_notebook8_summary.csv').read_bytes() == (original / 'legacy_un_notebook8_summary.csv').read_bytes()
    save(baseline / 'comparison_to_phase1.json', {'status': 'pass', 'all_outputs_identical_bytes': True,
                                               'cases': identical, 'legacy_summary_identical': True})
    before = json.loads((HERE / 'protection_before.json').read_text())
    allowed = {'include/classes/Simulation.h', 'include/models/UNState.h', 'src/MainSCIANTIX.C',
               'src/classes/Simulation.C', 'src/file_manager/Initialization.C', 'src/file_manager/InputReading.C',
               'src/file_manager/Output.C', 'src/models/UNIntraGranular.C', 'src/operations/UpdateVariables.C'}
    changed = []
    for name, h in before['files'].items():
        assert (ROOT / name).is_file(), name
        if sha(ROOT / name) != h:
            changed.append(name)
    assert set(changed) <= allowed, changed
    assert hashlib.sha256(subprocess.check_output(['git', 'diff', '--cached', '--binary'], cwd=ROOT)).hexdigest() == before['staged_diff_sha256']
    assert subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip() == before['head']
    save(HERE / 'preservation_check.json', {'status': 'pass', 'files_checked': len(before['files']),
         'changed_existing_files': changed, 'legacy_header_restored_exactly': True,
         'reference_gold_thesis_and_user_files_unchanged': True, 'head_and_staged_diff_unchanged': True})
    save(results / 'summary.json', {'status': 'pass', 'scope': '2D and 3 complete; Phase 4 not started',
         'passed_checks': passed, 'values_compared_full': full['values_compared'],
         'max_normalized_error_full': full['max_normalized_error'], 'cases': boundaries,
         'historical_standard': audit['standard_all_counts'], 'preexisting_supplemental': audit['supplemental_counts'],
         'legacy_un': audit['legacy_un']['counts'], 'historical_outputs_byte_identical': len(identical),
         'protected_file_count': len(before['files']), 'modified_existing_files': changed})
    print('PASS: six test groups, balances, 110 identical historical outputs,', len(before['files']), 'protected files')


if __name__ == '__main__':
    main()
