#!/usr/bin/env python3
"""Approved 50+60 deterministic configurations and separate fast transients."""
import sys
sys.dont_write_bytecode = True
from pathlib import Path
import argparse
import json
import subprocess
import time
from reference import HERE, OLD, ROOT, rv, np, hourly, high_reference, direct_modes, average, OBS, HOURS
from common import decay_rates, write_csv

END = 14400000.
DTS = [3600., 1800., 900., 450., 225.]
NS = [10, 20, 40, 80, 160, 320]
CACHE = HERE/'cache'
RESULTS = HERE/'results'
rv.BUILD = HERE/'build'


def configuration():
    cases = json.loads((OLD/'cases.json').read_text())
    assert len(cases) == 10
    config = {'frozen_cases_sha256': rv.sha(OLD/'cases.json'), 'bounds_sha256': rv.sha(OLD/'bounds.json'),
              'temporal': {'N': 40, 'dt_s': DTS, 'end_s': END, 'observations_s': 3600},
              'spatial': {'N': NS, 'dt_s': 225., 'end_s': END, 'decomposition_hours': (OBS/3600).tolist()},
              'decomposition': 'Cpp-highN = (Cpp-sameN) + (sameN-highN); signed vectors before norms',
              'fast': {'selection': 'tau_fast(N40)<225 s', 'N': [40,320], 'end_tau': 20,
                       'steps_per_tau': [10,20,40,80,160], 'observation_interval_tau': .5},
              'reuse': 'matching case/N/dt/end/observations and immutable output SHA256',
              'no_lhs': True}
    path = HERE/'configuration.json'
    if path.exists(): assert json.loads(path.read_text()) == config
    else: rv.save(path, config)
    return cases


def load_cpp(folder, n):
    data = np.fromfile(folder/'output.bin', dtype='<f8').reshape(-1, 4+3*n)
    timing = json.loads((folder/'timing.json').read_text())
    return data[:,0], data[:,1:4], data[:,4:].reshape(-1,3,n), timing


def compatible(folder, case, n, dt, end):
    f = np.loadtxt(folder/'input.txt')
    expected = [n, dt, end, max(1, round(3600/dt)), case['D'], case['R'], case['gb'], case['gd'],
                case['bb'], case['bd'], *case['source'], *case['initial']]
    return np.array_equal(f, expected)


def cpp(case, n, dt, stage):
    """Reuse identical main configurations, keeping a distinct 110-row ledger."""
    folder = RESULTS/stage/case['name']/(f'N{n}_dt{dt:g}')
    folder.mkdir(parents=True, exist_ok=True)
    manifest = folder/'cpp.json'
    if manifest.exists():
        record = json.loads(manifest.read_text()); source = ROOT/record['source']
        assert rv.sha(source/'output.bin') == record['output_sha256']
        t,means,modes,timing = load_cpp(source,n)
        return t,means,modes,record
    candidates = []
    if stage == 'spatial' and n == 40:
        other = RESULTS/'temporal'/case['name']/f'N40_dt{dt:g}'/'cpp.json'
        if other.exists(): candidates.append(ROOT/json.loads(other.read_text())['source'])
    candidates += [OLD/'results/benchmark_4000h'/case['name']]
    if case['name'].startswith('E2_') and n == 320 and dt == 225:
        candidates += [OLD/'results/cost_accuracy_gate/cpp_225.0']
    source = next((p for p in candidates if (p/'output.bin').exists() and compatible(p,case,n,dt,END)), None)
    start = time.perf_counter()
    if source is not None:
        t,means,modes,timing = load_cpp(source,n); reused = True
        process = None
    else:
        source = folder/'cpp'
        if source.exists() and (source/'timing.json').exists() and (source/'output.bin').exists():
            assert compatible(source,case,n,dt,END)
            t,means,modes,timing = load_cpp(source,n); reused = True; process = None
        else:
            t,means,modes,timing = rv.run_cpp(source,case,n,dt,END)
            reused = False; process = timing['process_seconds']
    assert np.array_equal(t,HOURS), (case['name'],n,dt,len(t))
    assert timing['steps'] == round(END/dt)
    assert np.isfinite(means).all() and np.isfinite(modes).all()
    record = {'case':case['name'],'stage':stage,'N':n,'dt_s':dt,'end_s':END,
              'steps':timing['steps'],'source':str(source.relative_to(ROOT)), 'reused':reused,
              'output_sha256':rv.sha(source/'output.bin'), 'solver_seconds_original':timing['solver_seconds'],
              'new_process_seconds':process, 'acquire_seconds':time.perf_counter()-start}
    rv.save(manifest,record)
    print('CPP',stage,case['name'],n,dt,'REUSE' if reused else 'NEW',flush=True)
    return t,means,modes,record


def build():
    RESULTS.mkdir(exist_ok=True); CACHE.mkdir(exist_ok=True)
    path=HERE/'build_manifest.json'
    if path.exists():
        old=json.loads(path.read_text())
        assert old['solver_sha256']==rv.sha(ROOT/'src/classes/Solver.C')
        assert old['binary_sha256']==rv.sha(rv.BUILD/'solver_harness.x')
    else: rv.save(path,rv.build(HERE))


def preflight(cases):
    build();rows=[]
    for case in cases:
        _,_,meta=hourly(case,40,CACHE)
        rows.append({'case':case['name'],**meta})
        rv.save(HERE/'reference_preflight.json',rows)
        print('PREFLIGHT',case['name'],meta,flush=True)
    rv.save(HERE/'checkpoints/preflight.json',{'status':'pass','sets':10,'reference_N':40,
        'solver_sha256':rv.sha(ROOT/'src/classes/Solver.C'),'checks':rows})


def main_runs(cases):
    ledger=[]
    for case in cases:
        exact,em,meta=hourly(case,40,CACHE)
        for dt in DTS:
            _,_,_,record=cpp(case,40,dt,'temporal');ledger.append(record)
            rv.save(HERE/'main_progress.json',ledger)
    rv.save(HERE/'checkpoints/temporal_cpp.json',{'status':'pass','configurations':50,'ledger':ledger})
    for case in cases:
        for n in NS:
            _,_,_,record=cpp(case,n,225.,'spatial');ledger.append(record)
            rv.save(HERE/'main_progress.json',ledger)
            # Cached once per N; reused by temporal/spatial analysis.
            hourly(case,n,CACHE)
        high_reference(case,CACHE)
        rv.save(HERE/'checkpoints'/f'spatial_{case["name"]}.json',{'status':'pass','N':NS,
            'high_reference':json.loads((CACHE/f'{case["name"]}_high_reference.json').read_text())})
    rv.save(HERE/'main_ledger.json',ledger)


def rapid(cases):
    ledger=[]
    for case in cases:
        if min(case['characteristics']['40']['tau_s']) >= 225: continue
        for n in [40,320]:
            tau = float(1/max(decay_rates(case,n))); end = 20*tau
            times = np.arange(41)*tau/2
            exact = direct_modes(case,n,times)
            ref_folder = CACHE/f'rapid_{case["name"]}_N{n}.npz'
            if not ref_folder.exists():
                np.savez_compressed(ref_folder,time=times,modes=exact,means=average(exact))
            for resolution in [10,20,40,80,160]:
                folder=RESULTS/'rapid'/case['name']/f'N{n}_tau{resolution}'
                manifest=folder/'rapid.json';dt=tau/resolution
                if manifest.exists(): record=json.loads(manifest.read_text())
                else:
                    # Fixed physical end and observation grid within each N;
                    # the harness may append an epsilon-sized partial step.
                    t,means,modes,timing=rv.run_cpp(folder/'cpp',case,n,dt,end,record_interval=tau/2)
                    ids=np.array([int(np.argmin(abs(t-x))) for x in times])
                    assert np.max(abs(t[ids]-times)) < end*2e-12
                    assert np.isfinite(modes).all()
                    np.savez_compressed(folder/'sampled.npz',time=times,cpp=means[ids],modes=modes[ids],actual_time=t[ids])
                    record={'case':case['name'],'N':n,'tau_fast_s':tau,'dt_s':dt,'end_s':end,
                         'resolution':resolution,'steps':timing['steps'], 'observations':len(times),
                         'solver_seconds':timing['solver_seconds'],'process_seconds':timing['process_seconds'],
                         'source':str((folder/'cpp').relative_to(ROOT)), 'output_sha256':rv.sha(folder/'cpp/output.bin'),
                         'initial':case['initial'],'source_vector':case['source'],
                         'note':'short forced transient; same frozen coefficients, initial and source, no microstructure'}
                    rv.save(manifest,record)
                ledger.append(record);rv.save(HERE/'rapid_progress.json',ledger)
                print('RAPID',case['name'],n,resolution,flush=True)
    rv.save(HERE/'rapid_ledger.json',ledger)


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('task',choices=['preflight','main','rapid','all'])
    a=p.parse_args();cases=configuration()
    (HERE/'checkpoints').mkdir(exist_ok=True)
    start=time.perf_counter()
    try:
        if a.task in ['preflight','all']:preflight(cases)
        if a.task in ['main','all']:build();main_runs(cases)
        if a.task in ['rapid','all']:build();rapid(cases)
    except Exception as e:
        rv.save(HERE/f'failure_{time.time_ns()}.json',{'task':a.task,'error':repr(e),'seconds':time.perf_counter()-start})
        raise
    rv.save(HERE/f'timing_{a.task}_{time.time_ns()}.json',{'task':a.task,'wall_seconds':time.perf_counter()-start})


if __name__=='__main__':main()
