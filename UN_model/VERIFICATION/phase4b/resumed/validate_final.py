#!/usr/bin/env python3
"""Validate the completed session and seal a non-overwriting local checkpoint."""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import ast
import csv
import gzip
import hashlib
import json
import re
import subprocess
import time
import zipfile
from reference import HERE, OLD, ROOT, rv, np
from campaign import configuration, compatible, load_cpp, RESULTS, NS, DTS


def git(*args):return subprocess.check_output(['git',*args],cwd=ROOT)


def read_csv(path):
    with open(path,newline='') as f:return list(csv.DictReader(f))


def main():
    start=time.perf_counter();before=json.loads((HERE/'preservation_before.json').read_text())
    assert git('branch','--show-current').decode().strip()==before['branch']=='development/nitride'
    assert git('rev-parse','HEAD').decode().strip()==before['head']
    assert git('rev-parse','origin/development/nitride').decode().strip()==before['origin']
    assert hashlib.sha256(git('diff','--cached','--binary')).hexdigest()==before['staged_diff_sha256']
    assert len(git('diff','--cached','--name-only','-z').split(b'\0'))-1==before['staged_files']==321
    assert git('diff','--name-only')==b''
    for name,digest in before['files'].items():assert (ROOT/name).is_file() and rv.sha(ROOT/name)==digest,name
    assert rv.sha(OLD/'cases.json')==before['cases_sha256']
    assert rv.sha(OLD/'bounds.json')==before['bounds_sha256']
    cases=configuration();byname={c['name']:c for c in cases}
    main_records=json.loads((HERE/'main_ledger.json').read_text())
    assert len(main_records)==110
    assert {(r['case'],r['dt_s']) for r in main_records if r['stage']=='temporal'}=={(c['name'],dt) for c in cases for dt in DTS}
    assert {(r['case'],r['N']) for r in main_records if r['stage']=='spatial'}=={(c['name'],n) for c in cases for n in NS}
    unique={}
    for record in main_records:
        source=ROOT/record['source'];case=byname[record['case']]
        assert compatible(source,case,record['N'],record['dt_s'],14400000.)
        assert rv.sha(source/'output.bin')==record['output_sha256']
        unique.setdefault(record['source'],record)
    assert len(unique)==100 and sum(not r['reused'] for r in main_records)==94
    assert sum(r['reused'] for r in main_records)==16
    for source,record in unique.items():
        t,means,modes,timing=load_cpp(ROOT/source,record['N'])
        assert len(t)==4001 and np.array_equal(t,np.arange(4001)*3600.)
        assert timing['steps']==round(14400000/record['dt_s'])
        assert np.isfinite(modes).all() and np.isfinite(means).all()
        assert np.all(modes[0]==0) and np.all(means[0]==0)
        if byname[record['case']]['gb']==0:assert np.all(modes[:,1,:]==0) and np.all(means[:,1]==0)
    rapid=json.loads((HERE/'rapid_ledger.json').read_text());assert len(rapid)==60
    assert len({r['case'] for r in rapid})==6
    for record in rapid:
        source=ROOT/record['source'];case=byname[record['case']]
        assert rv.sha(source/'output.bin')==record['output_sha256']
        config=np.loadtxt(source/'input.txt')
        assert np.array_equal(config[4:],[case[k] for k in ['D','R','gb','gd','bb','bd']]+case['source']+case['initial'])
        t,means,modes,timing=load_cpp(source,record['N'])
        assert t[-1]==record['end_s']
        assert np.isfinite(modes).all() and np.isfinite(means).all()
        assert np.all(modes[0]==0) and np.all(means[0]==0)
    assert len(list(RESULTS.rglob('input.txt')))==154
    data=HERE/'analysis_v2'
    assert len(read_csv(data/'temporal_summary.csv'))==150
    spatial=read_csv(data/'spatial_errors.csv');assert len(spatial)==1980
    max_closure=0.;max_L2_closure=0.
    for r in spatial:
        error=float(r['total_signed_mean_error'])-float(r['temporal_signed_mean_error'])-float(r['spatial_signed_mean_error'])
        max_closure=max(max_closure,abs(error)/float(r['mean_scale']))
        total=float(r['total_modal_L2_absolute']);temporal=float(r['temporal_modal_L2_absolute']);tail=float(r['spatial_modal_L2_absolute'])
        max_L2_closure=max(max_L2_closure,abs(total*total-temporal*temporal-tail*tail)/max(total*total,1.))
    assert max_closure<32*np.finfo(float).eps
    assert max_L2_closure<32*np.finfo(float).eps
    assert len(read_csv(data/'temporal_modal_errors.csv'))==6000
    assert len(read_csv(HERE/'audit/temporal_orders_per_mode.csv'))==4800
    assert len(read_csv(data/'rapid_summary.csv'))==180
    assert len(read_csv(HERE/'rapid_modal/errors_per_mode.csv'))==32400
    assert len(read_csv(HERE/'rapid_modal/orders_per_mode.csv'))==25920
    gzip_counts={}
    for name,count in [('temporal_history.csv.gz',600150),('spatial_retained_modal_decomposition.csv.gz',207900),('rapid_history.csv.gz',7380)]:
        with gzip.open(data/name,'rt') as f:actual=sum(1 for _ in f)-1
        assert actual==count,(name,actual);gzip_counts[name]=actual
    refchecks=[]
    for case in cases:
        r=json.loads((HERE/'cache'/f'{case["name"]}_high_reference.json').read_text())
        assert r['records'][-1]['max_mean_increment_over_N320_spatial']<=.01
        assert r['records'][-1]['max_tail_energy_relative_increment']<=.01
        refchecks.append({'case':case['name'],'N_high':r['N'],'convergence':'pass'})
    summary=json.loads((data/'summary.json').read_text());audit=json.loads((HERE/'audit/summary.json').read_text())
    assert summary['LHS']==audit['LHS_samples']==0
    assert audit['total_unique_main_BE_checked']==100
    assert max(audit['max_other_N_BE_gap'],summary['max_BE_implementation_gap_normalized'])<1e-8
    assert len(list((HERE/'figures').glob('*.png')))==8
    assert len(list((HERE/'figures').glob('*.pdf')))==8
    assert (HERE/'rapid_modal/fastest_mode_history.pdf').exists()
    for path in HERE.glob('*.py'):ast.parse(path.read_text(),filename=str(path))
    checkpoint=HERE/'checkpoints/completed'
    assert not checkpoint.exists()
    # The two final links are written just below; all other documentation
    # links must already resolve. Never silently leave broken artifact links.
    for path in HERE.glob('*.md'):
        for link in re.findall(r'\]\(([^)]+)\)',path.read_text()):
            target=(path.parent/link.split('#')[0]).resolve()
            if target in [HERE/'validation.json',checkpoint/'manifest.json']:continue
            assert target.exists(),(path.name,link)
    status={'status':'completed','checks':'pass','main_configurations':110,'temporal':50,'spatial':60,
       'main_unique_cpp_configurations':100,'main_new_executions':94,'main_reused_configurations':16,
       'rapid_new_executions':60,'total_new_cpp_processes':154,'LHS_samples':0,
       'protected_files_unchanged':len(before['files']),'preexisting_staged_files_unchanged':321,
       'branch_head_origin_unchanged':True,'no_unstaged_tracked_edits':True,
       'frozen_cases_bounds_unchanged':True,'BE_checks_unique_main_configurations':100,
       'max_BE_modal_gap_normalized':max(audit['max_other_N_BE_gap'],summary['max_BE_implementation_gap_normalized']),
       'max_mean_decomposition_residual_normalized':max_closure,'max_L2_squared_identity_relative_residual':max_L2_closure,
       'high_reference_checks':refchecks,'gzip_CSV_rows':gzip_counts,
       'main_png_pdf_pairs':8,'additional_fast_mode_png_pdf_pairs':1,
       'validation_seconds':time.perf_counter()-start,
       'limits':'Convergence and implementation verification passed; no universal application accuracy target specified.'}
    rv.save(HERE/'validation.json',status)
    checkpoint.mkdir()
    artifacts=[p for p in HERE.rglob('*') if p.is_file() and not p.is_relative_to(checkpoint)
               and not any(part in {'mplconfig','__pycache__'} for part in p.relative_to(HERE).parts)]
    with zipfile.ZipFile(checkpoint/'source.zip','x',zipfile.ZIP_DEFLATED) as z:
        for path in sorted(HERE.iterdir()):
            if path.is_file():z.write(path,path.relative_to(HERE))
    rv.save(checkpoint/'manifest.json',{'status':'completed','head':before['head'],'branch':before['branch'],
        'checks':status,'production_solver_sha256':rv.sha(ROOT/'src/classes/Solver.C'),
        'source_4A_hashes':{n:rv.sha(OLD.parent/n) for n in ['analytic.py','solver_harness.cpp','run_verification.py','basis_checks.json']},
        'frozen_cases_sha256':rv.sha(OLD/'cases.json'),'bounds_sha256':rv.sha(OLD/'bounds.json'),
        'source_zip_sha256':rv.sha(checkpoint/'source.zip'),
        'artifact_sha256':{str(p.relative_to(HERE)):rv.sha(p) for p in sorted(artifacts)},
        'original_cpp_sources_sha256':{str((ROOT/p)/'output.bin'):r['output_sha256'] for p,r in unique.items()},
        'preservation_snapshot_sha256':rv.sha(HERE/'preservation_before.json'),
        'Git_operations':'read-only; no commit, push, branch change, merge or reorganization'})
    print('PASS: 110 main configurations, 60 rapid runs, 100 independent BE checks; 10008 prior files and 321 staged preserved',flush=True)


if __name__=='__main__':main()
