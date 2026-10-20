"""Phase 4B setup; reuse Phase 4A unchanged, with a separate build directory."""
import sys
sys.dont_write_bytecode = True
from pathlib import Path
import csv
import json
import time

HERE = Path(__file__).resolve().parent
PARENT = HERE.parent
ROOT = HERE.parents[2]
sys.path.insert(0, str(PARENT))
import run_verification as rv
from analytic import matrices, projections, solution, VOLUME
import numpy as np
from scipy.optimize import brentq
from scipy.linalg import expm

rv.BUILD = HERE / 'build'
BOUNDS = {'D': [6.860e-20, 2.814e-16], 'gb': [0., 2.042], 'gd': [7.230e-5, 3.086e-1],
          'bb': [1.024e-6, 2.049e-4], 'bd': [3.790e-7, 2.049e-4]}
END = 4000 * 3600.
SOURCE = [1.7995675661038836e19, 0., 0.]


def gate_reference(case, n, times):
    """Reuse 4A, reducing only the exactly inactive bulk subspace at gb=0.

    The full 4x4 expm can inject roundoff into a mathematically zero bulk
    component. Solve the invariant c/md subsystem instead of clipping values.
    The original 4A code and all benchmark outputs remain unchanged.
    """
    if case['gb'] != 0 or case['initial'][1] != 0 or case['source'][1] != 0:
        return solution(case,n,times)
    scale=max(max(np.abs(case['initial'])),max(np.abs(case['source']))*max(times),1.)
    j=matrices(case,n)
    block=np.zeros((n,3,3));idx=[0,2]
    block[:,:2,:2]=j[:,idx][:,:,idx]
    block[:,:2,2]=np.asarray(case['source'])[idx]/scale
    y0=np.r_[np.asarray(case['initial'])[idx]/scale,1.]
    q=projections(n);modes=np.zeros((len(times),3,n))
    for i,t in enumerate(times):
        y=expm(block*t)@y0
        modes[i,idx,:]=(y[:,:2]*scale*q[:,None]).T
    means=np.einsum('tpn,n->tp',modes,q/VOLUME)
    return means,modes


def write_csv(path, rows):
    with open(path, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader(); w.writerows(rows)


def decay_rates(case, mode):
    """Stable slow root of the secular equation, then quadratic deflation.

    All 4B cases have D,gd,bb,bd>0. A generic eigensolver can round away
    the slow eigenvalue in the extreme trapping cases. No physical update
    or production numerical solve is performed here.
    """
    k = case['D'] * (np.pi * mode / case['R'])**2
    pole = min(case['bb'], case['bd']) if case['gb'] else case['bd']
    def f(z):
        rate = pole * z
        factor = 1 + case['gd'] / (case['bd'] - rate)
        if case['gb']:
            factor += case['gb'] / (case['bb'] - rate)
        return rate * factor / k - 1
    z = brentq(f, 0., np.nextafter(1., 0.), xtol=np.nextafter(0., 1.), rtol=8*np.finfo(float).eps)
    slow_root = z * pole
    if case['gb'] == 0:
        rates = [slow_root, case['bb'], k + case['gd'] + case['bd'] - slow_root]
    else:
        total = k + case['gb'] + case['gd'] + case['bb'] + case['bd'] - slow_root
        product = k * case['bb'] * case['bd'] / slow_root
        fast = .5 * (total + np.sqrt(max(total * total - 4 * product, 0.)))
        rates = [slow_root, product / fast, fast]
    rates = np.sort(rates)
    assert np.all(rates > 0) and np.isfinite(rates).all()
    return rates


def make_cases():
    cases = []
    for i, label in enumerate(['AP3p2_900K', 'AP3p4_1600K', 'AP3p4_1800K', 'ANP6_1600K'], 1):
        c = rv.physical_case(label, 100)
        original_source = c['source'][0]
        c.update(name=f'R{i}_{label}', kind='frozen_realistic', original_source_c=original_source,
                 source=SOURCE.copy(), initial=[0., 0., 0.], provenance_step=100,
                 provenance_start_s=100*10800., provenance_end_s=101*10800.)
        cases.append(c)
    definitions = [
        ('E1_minD_weak_trapping_weak_resolution', [0,0,0,0,0]),
        ('E2_maxD_zero_bulk_strong_resolution', [1,0,0,1,1]),
        ('E3_minD_bulk_strong_mixed_resolution', [0,1,0,0,1]),
        ('E4_maxD_dislocation_strong_mixed_resolution', [1,0,1,1,0]),
        ('E5_maxD_strong_trapping_weak_resolution', [1,1,1,0,0]),
        ('E6_minD_strong_trapping_strong_resolution', [0,1,1,1,1])]
    for name, choices in definitions:
        c = {key: BOUNDS[key][flag] for key, flag in zip(BOUNDS, choices)}
        c.update(name=name, kind='deterministic_extreme', R=6e-6, source=SOURCE.copy(), initial=[0.,0.,0.],
                 original_source_c=None, provenance_step=None, provenance_start_s=None, provenance_end_s=None,
                 provenance={'table':'Appendix B Table B.1 supplied verbatim in REQUEST.md'})
        cases.append(c)
    for c in cases:
        assert c['R'] == 6e-6
        for key, (low, high) in BOUNDS.items():
            assert low <= c[key] <= high, (c['name'],key,c[key])
        c['characteristics'] = {str(n): {'rates_s-1': decay_rates(c,n).tolist(),
              'tau_s': (1/decay_rates(c,n)).tolist()} for n in [1,40,320]}
    return cases


def save_cases(cases):
    rv.save(HERE/'cases.json', cases)
    rv.save(HERE/'bounds.json', {'source':'Table B.1 in user request','bounds':BOUNDS,'R':6e-6,
        'source_c_common':SOURCE[0],'initial':[0.,0.,0.], 'end_s':END})
    rows=[]
    for c in cases:
        row = {k:c[k] for k in ['name','kind','D','R','gb','gd','bb','bd']}
        row.update(source_c=SOURCE[0],source_mb=0.,source_md=0.,initial_c=0.,initial_mb=0.,initial_md=0.,
          original_frozen_source_c=c['original_source_c'], provenance_step_zero_based=c['provenance_step'],
          provenance_start_s=c['provenance_start_s'],provenance_end_s=c['provenance_end_s'],
          provenance=c['provenance'].get('file',c['provenance'].get('table')),
          provenance_sha256=c['provenance'].get('sha256',''))
        for n in [1,40,320]:
            ts=c['characteristics'][str(n)]['tau_s']
            row.update({f'tau_fast_n{n}_s':min(ts),f'tau_slow_n{n}_s':max(ts),f'stiffness_n{n}':max(ts)/min(ts)})
        for key in ['gb','gd','bb','bd']:
            row['tau_'+key+'_s']=1/c[key] if c[key] else None
        rows.append(row)
    write_csv(HERE/'parameters_10.csv', rows)
