#!/usr/bin/env python3
"""Validate the cost-stop checkpoint; never launches or marks the 110 runs done."""
import sys
sys.dont_write_bytecode=True
import hashlib
import json
import subprocess
import zipfile
from pathlib import Path
from common import HERE, ROOT, rv, np, BOUNDS, gate_reference, solution


def main():
    cases=json.loads((HERE/'cases.json').read_text());assert len(cases)==10
    assert sum(c['kind']=='frozen_realistic' for c in cases)==4
    for c in cases:
        assert c['R']==6e-6 and c['initial']==[0.,0.,0.]
        for k,(low,high) in BOUNDS.items():assert low<=c[k]<=high
        if c['kind']=='frozen_realistic':
            f=rv.physical_case(c['name'].split('_',1)[1],100)
            for k in ['D','gb','gd','bb','bd']:assert c[k]==f[k],(c['name'],k)
    base=HERE/'results/benchmark_4000h'
    bench=json.loads((base/'results.json').read_text());assert bench['benchmarks']==5
    for r in bench['rows']:
        assert r['steps']==64000 and r['N']==40 and r['dt_s']==225 and r['end_s']==14400000
        x=np.fromfile(base/r['case']/'output.bin',dtype='<f8').reshape(-1,124)
        assert len(x)==4001 and np.isfinite(x).all() and x[-1,0]==14400000
    gate=HERE/'results/cost_accuracy_gate_v2'
    decision=json.loads((gate/'decision.json').read_text())
    assert decision['status']=='cost_gate_stop' and decision['main_campaign_started'] is False
    assert decision['reference_N']==5120
    assert len(list((HERE/'results').rglob('input.txt')))==7
    # Structural zero and active-subspace cross-check; no clipping is applied.
    c=next(c for c in cases if c['name'].startswith('E2_'))
    t=np.array([3600.,86400.,360000.,3600000.,14400000.])
    full,_=solution(c,40,t);reduced,_=gate_reference(c,40,t)
    assert np.all(reduced[:,1]==0)
    active_gap=float(abs(full[:,[0,2]]-reduced[:,[0,2]]).max()/max(abs(reduced).max(),1.))
    assert active_gap<1e-12
    full_bulk_noise=float(abs(full[:,1]).max())
    with np.load(gate/'analytic_reference.npz') as z:
        high=z['highN'];same=z['sameN']
    steady=c['source'][0]*c['R']**2/(15*c['D'])
    steady_gap=float(abs(high[-1,0]-steady)/steady)
    assert steady_gap<1e-9
    for dt in ['225.0','112.5']:
        reuse=json.loads((gate/('reused_cpp_'+dt+'.json')).read_text())
        source=Path(reuse['source'])
        assert rv.sha(source/'output.bin')==reuse['output_sha256']
        data=np.fromfile(source/'output.bin',dtype='<f8').reshape(-1,964)
        assert np.isfinite(data).all() and data[-1,0]==14400000
        modes=data[:,4:].reshape(-1,3,320)
        assert np.all(data[:,2]==0) and np.all(modes[:,1]==0)
    before=json.loads((HERE/'preservation_before.json').read_text())
    for name,h in before['files'].items():assert (ROOT/name).is_file() and rv.sha(ROOT/name)==h,name
    for key,argv in [('head',['rev-parse','HEAD']),('branch',['branch','--show-current']),('origin',['rev-parse','origin/development/nitride'])]:
        assert subprocess.check_output(['git',*argv],cwd=ROOT,text=True).strip()==before[key]
    assert hashlib.sha256(subprocess.check_output(['git','diff','--cached','--binary'],cwd=ROOT)).hexdigest()==before['staged_diff_sha256']
    assert subprocess.check_output(['git','diff','--name-only'],cwd=ROOT)==b''
    untracked=subprocess.check_output(['git','ls-files','--others','--exclude-standard','-z'],cwd=ROOT)
    assert all(p.decode().startswith('UN_model/VERIFICATION/phase4b/') for p in untracked.split(b'\0') if p)
    status={'status':'cost_gate_stop','verification_checks':'pass','completed_cpp_simulations':7,
      'benchmarks':5,'accuracy_prechecks':2,'main_temporal_runs':0,'main_spatial_runs':0,'fast_transient_runs':0,
      'lhs_samples':0,'protected_files':len(before['files']),'preexisting_staged_4A_unchanged':True,
      'head_branch_origin_unchanged':True,'all_new_files_inside_phase4b':True,
      'reference_active_subspace_relative_gap':active_gap,'full_4A_expm_inactive_bulk_noise_absolute':full_bulk_noise,
      'E2_reference_stationary_mean_relative_error':steady_gap,
      'note':'Phase 4B is not complete; stopped at the explicitly requested cost gate.'}
    rv.save(HERE/'validation.json',status)
    out=HERE/'checkpoints/cost_gate_stop';out.mkdir(parents=True,exist_ok=False)
    files=[p for p in HERE.rglob('*') if p.is_file() and not any(k in {'build','mplconfig','__pycache__','checkpoints'} for k in p.relative_to(HERE).parts)]
    with zipfile.ZipFile(out/'source.zip','x',zipfile.ZIP_DEFLATED) as z:
        for p in files:
            if p.parent==HERE:z.write(p,p.relative_to(HERE))
    rv.save(out/'manifest.json',{'status':'cost_gate_stop','checks':'pass','head':before['head'],
      'artifact_sha256':{str(p.relative_to(HERE)):rv.sha(p) for p in sorted(files)},
      'source_zip_sha256':rv.sha(out/'source.zip'),'production_solver_sha256':rv.sha(ROOT/'src/classes/Solver.C'),
      'source_4A_sha256':{p.name:rv.sha(p) for p in [HERE.parent/'analytic.py',HERE.parent/'run_verification.py',HERE.parent/'solver_harness.cpp']},
      'counts':status,'no_Git_operations':True})
    print('PASS cost-stop checkpoint:',len(before['files']),'preserved files; staged 4A intact; 7 prechecks, 0/110 main runs')


if __name__=='__main__':main()
