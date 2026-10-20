#!/usr/bin/env python3
"""Additional modal orders, checks across N, and measured cost tables."""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import csv
import json
import time
from reference import HERE, ROOT, rv, np, hourly, average
from campaign import CACHE, RESULTS, load_cpp, configuration
from analyze import discrete_be, order
from common import write_csv

OUT=HERE/'audit'
DATA=HERE/'analysis_v2'


def read(name):
    with open(DATA/name,newline='') as f:return list(csv.DictReader(f))


def main():
    OUT.mkdir(exist_ok=False);start=time.perf_counter();cases=configuration()
    source=read('temporal_modal_errors.csv');orders=[]
    for case in cases:
        _,rm,meta=hourly(case,40,CACHE)
        uncertainty=np.maximum(meta['modal_direct_gap_normalized'],meta['split_gap_normalized'])
        global_scale=np.maximum(np.linalg.norm(rm,axis=2).max(axis=0),1.)
        for p,pop in enumerate(['c','mb','md']):
            for k in range(40):
                subset=[r for r in source if r['case']==case['name'] and r['population']==pop and int(r['mode'])==k+1]
                for a,b in zip(subset,subset[1:]):
                    floor=max(100*np.finfo(float).eps,20*uncertainty[p]*global_scale[p]/float(a['scale']))
                    observed,status=order(float(a['max_normalized']),float(b['max_normalized']),floor=floor)
                    orders.append({'case':case['name'],'population':pop,'mode':k+1,
                        'coarse_dt_s':a['dt_s'],'fine_dt_s':b['dt_s'],'coarse_error':a['max_normalized'],
                        'fine_error':b['max_normalized'],'order':observed,'status':status,'reference_screening_floor':floor})
    write_csv(OUT/'temporal_orders_per_mode.csv',orders)
    checks=[]
    # N40 is already covered at all five dt. Extend the independent BE check
    # only to the other five N; do not repeat completed checks unnecessarily.
    for case in cases:
        for n in [10,20,80,160,320]:
            record=json.loads((RESULTS/'spatial'/case['name']/f'N{n}_dt225'/'cpp.json').read_text())
            t,cpp,cm,_=load_cpp(ROOT/record['source'],n)
            be,bem=discrete_be(case,n,225.)
            scales=np.maximum(abs(be).max(axis=0),1.)
            mscales=np.maximum(np.linalg.norm(bem,axis=2).max(axis=0),1.)
            err=abs(cpp-be).max(axis=0)/scales
            merr=np.linalg.norm(cm-bem,axis=2).max(axis=0)/mscales
            reconstruction=float(abs(cpp-average(cm)).max()/max(abs(cpp).max(),1.))
            assert max(err.max(),merr.max())<1e-8
            assert reconstruction<5e-14
            assert np.all(cpp[0]==0) and np.all(cm[0]==0)
            signed=cm*np.sign(np.asarray([(-1.)**k for k in range(n)]))[None,None,:]
            assert signed.min()>=0, ('positivity after factoring modal projection',case['name'],n)
            checks.append({'case':case['name'],'N':n,'dt_s':225,
              'max_BE_mean_gap_normalized':float(err.max()),'max_BE_modal_L2_gap_normalized':float(merr.max()),
              'mean_reconstruction_gap':reconstruction,'all_initial_modes_zero':True,'projection_factored_positivity':True})
        print('AUDIT_N',case['name'],flush=True)
    write_csv(OUT/'implementation_checks_other_N.csv',checks)
    temporal=read('temporal_summary.csv');spatial=read('spatial_errors.csv')
    case_table=[]
    for case in cases:
        name=case['name']
        row={'case':name}
        for dt in [3600,225]:
            for pop in ['c','mb','md']:
                r=next(r for r in temporal if r['case']==name and float(r['dt_s'])==dt and r['population']==pop)
                row[f'{pop}_max_temporal_mean_dt{dt}']=r['max_mean_normalized']
                row[f'{pop}_max_temporal_L2_dt{dt}']=r['max_modal_L2_normalized']
        for n in [40,320]:
            subset=[r for r in spatial if r['case']==name and int(r['N'])==n]
            row[f'max_spatial_mean_N{n}']=max(float(r['spatial_mean_normalized']) for r in subset)
            row[f'max_spatial_L2_N{n}']=max(float(r['spatial_modal_L2_normalized']) for r in subset)
        case_table.append(row)
    write_csv(OUT/'case_findings.csv',case_table)
    ledger=json.loads((HERE/'main_ledger.json').read_text())
    rapid=json.loads((HERE/'rapid_ledger.json').read_text())
    costs=[]
    for case in cases:
        name=case['name'];records=[r for r in ledger if r['case']==name]
        unique={r['source']:r for r in records}
        high=json.loads((CACHE/f'{name}_high_reference.json').read_text())
        ref_seconds=sum(json.loads(str(np.load(CACHE/f'{name}_N{n}_hourly.npz')['metadata']))['seconds'] for n in [10,20,40,80,160,320])
        short=[r for r in rapid if r['case']==name]
        costs.append({'case':name,'configurations':len(records),'unique_Cpp_configurations':len(unique),
             'solver_original_unique_seconds':sum(r['solver_seconds_original'] for r in unique.values()),
             'new_solver_seconds':sum(r['solver_seconds_original'] for r in records if not r['reused']),
             'new_process_seconds':sum(r['new_process_seconds'] or 0 for r in records),
             'analytic_hourly_cache_seconds':ref_seconds,'analytic_highN_seconds':high['seconds'],
             'high_N':high['N'],'rapid_runs':len(short),
             'rapid_solver_seconds':sum(r['solver_seconds'] for r in short),
             'rapid_process_seconds':sum(r['process_seconds'] for r in short)})
    write_csv(OUT/'costs_per_case.csv',costs)
    # Projections only, no LHS points constructed. Scale the actual per-set
    # cost of all 11 labels (10 unique C++ runs) plus both analytical caches.
    per_case=np.array([r['solver_original_unique_seconds']+r['analytic_hourly_cache_seconds']+r['analytic_highN_seconds'] for r in costs])
    proposals=[]
    for count in [32,128,256]:
        proposals.append({'proposed_samples':count,'mean_compute_projection_s':float(per_case.mean()*count),
           'observed_case_envelope_low_s':float(per_case.min()*count),'observed_case_envelope_high_s':float(per_case.max()*count),
           'basis':'10 unique C++ grids plus cached same-N and high-N analytical checks per set; excludes I/O, analysis, plots, fast tests',
           'status':'proposal_only_no_sampling','not_guaranteed':'new coefficients or accuracy targets may require finer grids and cost'})
    write_csv(OUT/'lhs_budget_proposal_only.csv',proposals)
    summary={'status':'completed','extra_BE_checks':len(checks),'total_unique_main_BE_checked':100,
       'max_other_N_BE_gap':max(r['max_BE_modal_L2_gap_normalized'] for r in checks),
       'modal_order_rows':len(orders),'audit_seconds':time.perf_counter()-start,
       'hourly_reference_seconds':sum(r['analytic_hourly_cache_seconds'] for r in costs),
       'highN_reference_seconds':sum(r['analytic_highN_seconds'] for r in costs),'LHS_samples':0}
    rv.save(OUT/'summary.json',summary);print(json.dumps(summary,indent=2))


if __name__=='__main__':main()
