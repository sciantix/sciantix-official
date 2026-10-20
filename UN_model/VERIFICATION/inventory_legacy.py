#!/usr/bin/env python3
"""Read-only inventory for an archive proposal. Never moves or deletes files."""
import csv
from pathlib import Path
import json
from run_verification import HERE, ROOT, save


def classify(path):
    parts = path.parts
    if '__pycache__' in parts or 'mplconfig' in parts or path.suffix in {'.pyc', '.o', '.x'}:
        return 'regenerable_build_or_cache'
    if 'snapshot' in parts:
        if 'build' in parts or 'obj' in parts:
            return 'regenerable_build_or_cache'
        if 'regression' in parts:
            return 'snapshot_regression_inputs_outputs_needed_for_existing_checks'
        return 'historical_source_snapshot'
    if 'checkpoints' in parts:
        return 'immutable_checkpoint'
    if 'results' in parts or 'runs' in parts:
        return 'frozen_reference' if 'regression_reference' in parts else 'historical_test_evidence'
    if path.suffix in {'.py', '.cpp'}:
        return 'reproducible_test_or_runner_code'
    return 'documentation_provenance_or_configuration'


def main():
    names = ['phase2a', 'phase2bc', 'phase2d3', 'baseline_sciantix', 'regression_reference', 'local_merge_2026-10-19']
    rows, totals = [], {}
    for name in names:
        counts = {}
        for path in sorted((ROOT / 'UN_model' / name).rglob('*')):
            if not path.is_file():
                continue
            group = classify(path)
            size = path.stat().st_size
            row = {'directory': name, 'category': group, 'bytes': size, 'path': str(path.relative_to(ROOT))}
            rows.append(row)
            d = counts.setdefault(group, {'files': 0, 'bytes': 0})
            d['files'] += 1
            d['bytes'] += size
        totals[name] = counts
    with open(HERE / 'legacy_inventory.csv', 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
    save(HERE / 'legacy_inventory_summary.json', totals)
    print('Read-only inventory:',len(rows),'files; no file moved, renamed or deleted')


if __name__ == '__main__':
    main()
