#!/usr/bin/env python3
"""Validate existing campaign results, preserve Git/files, package compact CSV."""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import ast
import csv
import gzip
import hashlib
import json
import subprocess
import time
import argparse
import zipfile
from collections import Counter
from campaign_core import HERE, ROOT, OLD, rv, np, average, fingerprint, OBS_HOURS, HOURS, NS, POPS, result_file
from scipy.stats import qmc


def load(path):
    with open(path,newline='') as f:return list(csv.DictReader(f))


def close(a,b,scale=1.):
    assert abs(float(a)-float(b))<=64*np.finfo(float).eps*max(abs(float(a)),abs(float(b)),scale),(a,b)


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--input-directory',type=Path,default=HERE/'deliverables')
    out=parser.parse_args().input_directory.resolve();assert out.is_relative_to(HERE)
    start=time.perf_counter()
    assert not (HERE/'checkpoints/final').exists()
    before=json.loads((HERE/'preservation_before.json').read_text())
    cases=json.loads((HERE/'cases.json').read_text());config=json.loads((HERE/'configuration.json').read_text())
    summary=json.loads((HERE/'summary.json').read_text())
    assert len(cases)==160 and rv.sha(HERE/'cases.json')==config['cases_sha256']
    assert rv.sha(HERE/'combinations_160.csv')==config['combinations_csv_sha256']
    assert len({fingerprint(c) for c in cases})==160
    assert sum(c['group']=='LHS' for c in cases)==128
    unit=np.array([[float(r[k]) for k in ['D','gb','gd','bb','bd']] for r in load(HERE/'inputs/lhs_unit.csv')])
    reproduced=qmc.LatinHypercube(d=5,scramble=True,rng=np.random.default_rng(20261020)).random(128)
    assert np.array_equal(unit,reproduced)
    assert all(np.array_equal(np.sort(np.floor(unit[:,j]*128).astype(int)),np.arange(128)) for j in range(5))
    for i,c in enumerate(cases[:128]):
        for j,k in enumerate(['D','gb','gd','bb','bd']):
            lo,hi=config['bounds'][k]
            v=float(lo+unit[i,j]*(hi-lo)) if k=='gb' else float(np.exp(np.log(lo)+unit[i,j]*np.log(hi/lo)))
            assert c[k]==v and lo<=v<=hi
    full=load(HERE/'inputs/history_coefficients_complete.csv')
    selected=load(HERE/'inputs/history_selection_32.csv');assert len(full)==3012 and len(selected)==32
    for label in ['AP3p2_900K','AP3p4_1600K','AP3p4_1800K','ANP6_1600K']:
        rows=[r for r in full if r['history']==label]
        picks=[r for r in selected if r['history']==label];assert len(picks)==8
        assert len({tuple(float(r[k]) for k in ['D','gb','gd','bb','bd']) for r in picks})==8
        assert min(int(r['step']) for r in picks)==0
        assert max(int(r['step']) for r in picks)==max(int(r['step']) for r in rows)
        path=ROOT/rows[0]['source_file'];digest=rv.sha(path)
        assert all(r['source_sha256']==digest for r in rows)
        actual=[]
        with gzip.open(path,'rt') as f:
            for line_no,line in enumerate(f,1):
                e=json.loads(line)
                if e['stage']=='after_exchange':actual.append((e,line_no))
        assert len(actual)==len(rows)
        for saved,(e,line_no) in zip(rows,actual):
            s=e['scalars'];assert int(saved['step'])==e['step'] and int(saved['source_line'])==line_no
            for k,v in zip(['D','gb','gd','bb','bd'],['Dg','g_b','g_d','b_b','b_d']):assert float(saved[k])==s[v]
            assert float(saved['source_time_s'])==s['t'] and float(saved['source_interval_end_s'])==s['t']+s['dt']
        for c in [c for c in cases[128:] if c['provenance']['history']==label]:
            pick=next(r for r in picks if r['case']==c['name'])
            for k in ['D','gb','gd','bb','bd']:assert c[k]==float(pick[k])
    records=[];executions=0;modal_arrays=0;max_identity=0.;raw_cpp_hashes={}
    for c in cases:
        base=HERE/'raw'/c['name'];done=base/'completed.json'
        if not done.exists():continue
        record=json.loads(result_file(c['name'],'completed.json').read_text());records.append(record)
        ref=json.loads((base/'reference_manifest.json').read_text())
        assert ref['parameter_fingerprint']==list(fingerprint(c))
        hp=ROOT/ref['hourly_file'];highpath=ROOT/ref['high_file']
        assert rv.sha(hp)==ref['hourly_sha256'] and rv.sha(highpath)==ref['high_sha256']
        with np.load(hp) as z:exact=z['means'];em=z['modes'];assert np.array_equal(z['time'],HOURS)
        with np.load(highpath) as z:high=z['means'];hm=z['modes']
        assert np.isfinite(hm).all() and hm.shape==(11,3,ref['N_high'])
        assert ref['high_convergence']['max_mean_increment_over_N320_spatial']<=.01
        assert ref['high_convergence']['max_tail_energy_relative_increment']<=.01
        manifests=list(base.glob('dt_*/cpp_manifest.json'))
        assert len(manifests)==5+record['extra_temporal_refinements']
        for path in manifests:
            m=json.loads(path.read_text());source=ROOT/m['source'];dt=m['dt_s'];executions+=1
            assert rv.sha(source/'output.bin')==m['output_sha256']
            raw_cpp_hashes[str(source.relative_to(ROOT))]=m['output_sha256']
            cfg=np.loadtxt(source/'input.txt')
            assert np.array_equal(cfg,[40,dt,14400000,round(3600/dt),c['D'],c['R'],c['gb'],c['gd'],c['bb'],c['bd'],*c['source'],*c['initial']])
            data=np.fromfile(source/'output.bin',dtype='<f8').reshape(-1,124)
            assert np.array_equal(data[:,0],HOURS) and np.isfinite(data).all()
            modes=data[:,4:].reshape(-1,3,40);means=data[:,1:4]
            assert np.all(modes[0]==0) and np.all(means[0]==0)
            assert json.loads((source/'timing.json').read_text())['steps']==round(14400000/dt)
            if c['gb']==0:assert np.all(modes[:,1]==0) and np.all(means[:,1]==0)
            with np.load(base/f'modal_and_mean_errors_dt{dt:g}.npz') as z:
                assert np.array_equal(z['time'],HOURS)
                assert np.array_equal(z['mean_error'],means-exact)
                assert np.array_equal(z['modal_error'],modes-em)
                modal_arrays+=1
        for r in load(result_file(c['name'],'temporal_errors.csv')):
            err=abs(float(r['signed_error']));close(r['absolute_error'],err)
            if r['relative_defined']=='True':close(r['relative_instantaneous'],err/abs(float(r['analytic_sameN_mean'])))
            else:assert r['relative_instantaneous']==''
        for r in load(result_file(c['name'],'temporal_orders.csv')):
            ea=float(r['coarse_absolute_error']);eb=float(r['fine_absolute_error'])
            if ea>0 and eb>0:close(r['p_log2_errors'],np.log2(ea/eb))
            else:assert r['p_log2_errors']==''
            assert float(r['coarse_dt_s'])==2*float(r['fine_dt_s'])
        spatial=load(base/'spatial_errors.csv');assert len(spatial)==198
        for n in NS:
            low=average(hm[:,:,:n]);tail=np.sqrt(np.sum(hm[:,:,n:]**2,axis=2))
            for r in [r for r in spatial if int(r['N'])==n]:
                i=OBS_HOURS.index(int(float(r['hour'])));p=POPS.index(r['population'])
                close(r['analytic_N_mean'],low[i,p]);close(r['signed_mean_error'],low[i,p]-high[i,p])
                close(r['field_L2_error'],tail[i,p]);assert r['method']=='analytic_N_minus_analytic_highN'
        for r in load(result_file(c['name'],'decomposition_N40.csv')):
            total=float(r['total_signed']);te=float(r['temporal_implementation_signed']);se=float(r['pure_spatial_signed'])
            residual=abs(total-te-se)/max(abs(high).max(),1.)
            max_identity=max(max_identity,residual);assert residual<32*np.finfo(float).eps
        assert record['max_BE_gap_normalized']<1e-8
        print('VALIDATED',c['name'],flush=True)
    assert len(records)==summary['completed_cases']
    assert executions==modal_arrays==summary['total_temporal_cpp_configurations']
    if summary['status']=='completed':assert len(records)==160
    else:assert summary['status']=='budget_stop_partial'
    counts={}
    for name,expected in summary['csv_row_counts'].items():
        rows=load(out/name);counts[name]=len(rows);assert len(rows)==expected
    main_rows=[r for r in load(out/'temporal_errors.csv') if r['role']=='main']
    assert len(main_rows)==len(records)*5*11*3
    assert len(load(out/'spatial_errors.csv'))==len(records)*6*11*3
    # Reconstruct/check spatial p directly from exported errors as well.
    for r in load(out/'spatial_orders.csv'):
        e1=float(r['coarse_error']);e2=float(r['fine_error'])
        if e1>0 and e2>0:close(r['order'],np.log2(e1/e2))
        else:assert r['order']==''
    for name,digest in before['files'].items():assert (ROOT/name).is_file() and rv.sha(ROOT/name)==digest,name
    for key,args in [('head',['rev-parse','HEAD']),('branch',['branch','--show-current']),('origin',['rev-parse','origin/development/nitride'])]:
        assert subprocess.check_output(['git',*args],cwd=ROOT,text=True).strip()==before[key]
    assert hashlib.sha256(subprocess.check_output(['git','diff','--cached','--binary'],cwd=ROOT)).hexdigest()==before['staged_diff_sha256']
    assert subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True)==before['unstaged']
    assert len(subprocess.check_output(['git','diff','--cached','--name-only','-z'],cwd=ROOT).split(b'\0'))-1==before['staged_files']
    for path in HERE.glob('*.py'):ast.parse(path.read_text(),filename=str(path))
    assert not list(HERE.rglob('*.png')) and not list(HERE.rglob('*.pdf'))
    budget=json.loads((HERE/'budget.json').read_text())
    valid={'checks':'pass','campaign_status':summary['status'],'completed_cases':len(records),
        'main_temporal_configurations':len(records)*5,'spatial_analytic_configurations':len(records)*6,
        'extra_temporal_refinements':summary['additional_temporal_refinements'],'checked_cpp_outputs':executions,
        'checked_full_hourly_modal_error_arrays':modal_arrays,'observations_per_raw_run':4001,
        'LHS_seed_repeatability_and_stratification':'pass','complete_UN_history_rows_rechecked':3012,
        'selected_real_states_exactly_preserved':32,'selected_real_states_outside_TableB1':config['real_states_outside_bounds'],
        'complete_history_states_outside_TableB1_count':summary['complete_history_states_outside_TableB1_count'],
        'all_highN_references_converged':True,'max_BE_gap_normalized':summary['max_BE_implementation_gap_normalized'],
        'max_decomposition_residual_normalized':max_identity,'CSV_errors_orders_recomputed':'pass',
        'protected_files_unchanged':len(before['files']),'staged_files_at_start_and_end':before['staged_files'],
        'previous_321_staged_files':'already committed before this session; tracked contents protected',
        'branch_head_origin_and_index_unchanged':True,'no_unstaged_tracked_edits':True,
        'no_plots_thesis_or_accuracy_decision':True,'validation_seconds':time.perf_counter()-start,
        'elapsed_since_budget_start_seconds':time.time()-budget['start_epoch'],
        'operational_limit_seconds':5400,'limits':'Only the declared fixed times are in compact tables; complete hourly raw data retained. Precision-limited orders are unconfirmed.'}
    rv.save(out/'validation.json',valid)
    # External raw index keeps the upload auditable without shipping binaries.
    raw_index=[]
    for path in sorted(HERE.rglob('*')):
        if path.is_file() and not path.is_relative_to(out) and not any(k in {'__pycache__','final'} for k in path.relative_to(HERE).parts):
            raw_index.append({'path':str(path.relative_to(HERE)),'bytes':path.stat().st_size,'sha256':rv.sha(path)})
    with open(out/'external_raw_index.csv','x',newline='') as f:
        w=csv.DictWriter(f,fieldnames=['path','bytes','sha256']);w.writeheader();w.writerows(raw_index)
    for p in [HERE/'configuration.json',HERE/'execution_protocol.json',HERE/'benchmark.json',HERE/'build_manifest.json',HERE/'environment.json']:
        (out/p.name).write_bytes(p.read_bytes())
    for p in [HERE/'targeted_extension_protocol.json',HERE/'extensions_manifest.json']:
        if p.exists():(out/p.name).write_bytes(p.read_bytes())
    inputs=out/'inputs';inputs.mkdir()
    for p in sorted((HERE/'inputs').iterdir()):
        if p.is_file():(inputs/p.name).write_bytes(p.read_bytes())
    bundle=[p for p in sorted(out.rglob('*')) if p.is_file()]
    rv.save(out/'SHA256SUMS.json',{str(p.relative_to(out)):rv.sha(p) for p in bundle})
    bundle.append(out/'SHA256SUMS.json')
    archive=HERE/'extended_convergence_160_compact.zip'
    with zipfile.ZipFile(archive,'x',zipfile.ZIP_DEFLATED,compresslevel=9) as z:
        for p in bundle:z.write(p,p.relative_to(out))
    with zipfile.ZipFile(archive) as z:
        assert z.testzip() is None
        assert all(Path(n).suffix in ['.csv','.json','.md'] for n in z.namelist())
    package={'file':archive.name,'bytes':archive.stat().st_size,'sha256':rv.sha(archive),
        'compact_bundle_files':len(bundle),'contains_large_raw_binaries':False,
        'external_raw_files':len(raw_index),'external_raw_bytes':sum(r['bytes'] for r in raw_index),
        'elapsed_since_budget_start_seconds':time.time()-budget['start_epoch'],
        'validation_and_packaging_seconds':time.perf_counter()-start}
    rv.save(HERE/'package_info.json',package)
    final=HERE/'checkpoints/final';final.mkdir()
    rv.save(final/'manifest.json',{'status':summary['status'],'validation':valid,'package':package,
        'preservation_snapshot_sha256':rv.sha(HERE/'preservation_before.json'),
        'deliverable_sha256':{str(p.relative_to(HERE)):rv.sha(p) for p in bundle},
        'external_raw_index_sha256':rv.sha(out/'external_raw_index.csv'),
        'cases_sha256':rv.sha(HERE/'cases.json'),'solver_sha256':rv.sha(ROOT/'src/classes/Solver.C'),
        'read_only_Git':True,'stopped_after_campaign':True})
    print(json.dumps({'validation':valid,'package':package},indent=2),flush=True)


if __name__=='__main__':main()
