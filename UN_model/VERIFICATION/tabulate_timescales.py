#!/usr/bin/env python3
"""Tabulate dimensional coefficient and modal times without new simulations."""
import argparse
import csv
import json
from pathlib import Path
import numpy as np
from run_verification import HERE


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--cases', type=Path, required=True)
    args = p.parse_args()
    rows = []
    for case in json.loads(args.cases.read_text()):
        for n_string, data in case['characteristics'].items():
            for mode in data:
                diffusion = case['D'] * (np.pi * mode['mode'] / case['R'])**2
                times = mode['finite_times_s']
                row = {'case': case['name'], 'N': int(n_string), 'mode': mode['mode'],
                       'tau_diffusion_s': 1 / diffusion if diffusion else None}
                row.update({'tau_' + k + '_s': 1 / case[k] if case[k] else None for k in ['gb','gd','bb','bd']})
                row.update({'tau_eigen_fast_s': min(times) if times else None,
                            'tau_eigen_slow_s': max(times) if times else None,
                            'eigen_stiffness_ratio': max(times) / min(times) if times else None,
                            'zero_eigenvalues': mode['zero_eigenvalues'],
                            'accumulation_time_for_1e24_s': 1e24 / max(case['source']),
                            'temporal_pilot_end_s': case['temporal_end_s'], 'spatial_pilot_end_s': case['spatial_end_s']})
                rows.append(row)
    with open(HERE / 'characteristic_times.csv', 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
    print('Wrote',len(rows),'coefficient/modal timescale rows')


if __name__ == '__main__':
    main()
