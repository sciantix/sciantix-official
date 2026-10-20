#!/usr/bin/env python3
"""Check retained evidence, ignore coverage and a tiny rebuild smoke test."""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import ast,csv,hashlib,json,math,subprocess,tempfile,zipfile
from cleanup_files import HERE,ROOT,PLAN,inventory,sha,save


def main():
    rows=inventory();kept=[r for r in rows if r['action']=='keep'];deleted=[r for r in rows if r['action']=='delete']
    for r in kept:assert sha(ROOT/r['path'])==r['sha256'],r['path']
    for r in deleted:assert not (ROOT/r['path']).exists()
    # Every removed path is ignored, including names still in the old index.
    names='\0'.join(r['path'] for r in deleted)+'\0'
    p=subprocess.run(['git','check-ignore','--no-index','-z','--stdin'],cwd=ROOT,
        input=names.encode(),stdout=subprocess.PIPE,check=True)
    assert set(p.stdout.decode().strip('\0').split('\0'))=={r['path'] for r in deleted}
    protected=[r['path'] for r in kept if Path(r['path']).suffix in {'.py','.csv','.zip','.json','.md','.txt'}]
    q=subprocess.run(['git','check-ignore','--no-index','-z','--stdin'],cwd=ROOT,
        input=('\0'.join(protected)+'\0').encode(),stdout=subprocess.PIPE)
    assert q.returncode==1 and not q.stdout, 'A required retained file is ignored'
    scopes=[str(Path('UN_model/VERIFICATION')/d) for d in PLAN['scope']]
    original_scripts=subprocess.check_output(['git','ls-tree','-r','--name-only',
        json.loads((HERE/'state_before.json').read_text())['head'],'--',*scopes],cwd=ROOT,text=True).splitlines()
    original_scripts=[s for s in original_scripts if s.endswith('.py')]
    for name in original_scripts:
        p=ROOT/name;assert p.is_file();ast.parse(p.read_text(),filename=name)
    ext=ROOT/scopes[0];acc=ROOT/scopes[1]
    for path in [ext/'extended_convergence_160_compact.zip',acc/'accuracy_N40_dt3600_compact.zip']:
        with zipfile.ZipFile(path) as z:assert z.testzip() is None
    # Check fixed inputs/seed without regenerating any simulation or calibration.
    # 4A vendors SciPy in its existing verification-only dependency folder.
    sys.path.insert(0,str(HERE.parent/'dependencies'))
    import numpy as np
    from scipy.stats import qmc
    with (ext/'inputs/lhs_unit.csv').open() as f:unit=list(csv.DictReader(f))
    actual=np.array([[float(r[k]) for k in ['D','gb','gd','bb','bd']] for r in unit])
    assert np.array_equal(actual,qmc.LatinHypercube(d=5,scramble=True,rng=np.random.default_rng(20261020)).random(128))
    cases=json.loads((ext/'cases.json').read_text());assert len(cases)==160
    configs=json.loads((ext/'configuration.json').read_text())
    assert sha(ext/'cases.json')==configs['cases_sha256']
    assert sha(ext/'combinations_160.csv')==configs['combinations_csv_sha256']
    input_files=list(ext.glob('raw/**/input.txt'))+list(acc.glob('targeted/**/input.txt'))+list(acc.glob('rapid/**/input.txt'))
    assert len(input_files)==911
    for path in input_files:
        v=np.loadtxt(path);assert v.shape==(16,) and np.isfinite(v).all()
    orders={}
    for kind,name,coarse,fine,value in [
        ('temporal','temporal_orders.csv','coarse_absolute_error','fine_absolute_error','p_log2_errors'),
        ('spatial','spatial_orders.csv','coarse_error','fine_error','order')]:
        total=defined=0
        with (ext/'deliverables_final'/name).open() as f:
            for row in csv.DictReader(f):
                total+=1;a=float(row[coarse]);b=float(row[fine])
                if a>0 and b>0:
                    expected=math.log2(a/b);assert abs(float(row[value])-expected)<=1e-12*max(1.,abs(expected));defined+=1
                else:assert row[value]==''
        orders[kind]={'rows':total,'defined_orders_rechecked':defined}
    # Compile and run the unchanged production solver, using retained frozen
    # cases, with only three updates and a last partial step in /tmp.
    sys.path.insert(0,str(HERE.parent));import run_verification as rv
    from analytic import matrices,projections,VOLUME
    work=Path(tempfile.mkdtemp(prefix='sciantix-cleanup-smoke-'))
    rv.BUILD=work/'build';built=rv.build(work);checks=[]
    for case in [cases[0],cases[128]]:
        for n in [40,160]:
            times,means,modes,timing=rv.run_cpp(work/f'{case["name"]}_N{n}',case,n,3600.,7323.,3600.)
            state=np.zeros((n,3));source=np.asarray(case['source'])[None,:]*projections(n)[:,None]
            predicted=[state.T.copy()]
            for h in np.diff(times):
                state=np.linalg.solve(np.eye(3)[None]-h*matrices(case,n),(state+h*source)[...,None])[...,0]
                predicted.append(state.T.copy())
            predicted=np.array(predicted);scale=np.maximum(np.linalg.norm(predicted,axis=2).max(axis=0),1.)
            gap=float((np.linalg.norm(modes-predicted,axis=2)/scale).max());assert gap<1e-8
            reconstructed=np.einsum('tpn,n->tp',modes,projections(n)/VOLUME)
            mean_gap=float((abs(means-reconstructed)/np.maximum(abs(means).max(axis=0),1.)).max())
            assert mean_gap<5e-14 and np.array_equal(times,[0.,3600.,7200.,7323.])
            checks.append({'case':case['name'],'N':n,'steps':timing['steps'],'last_partial_step_s':123,
                'max_modal_BE_gap_normalized':gap,'max_mean_reconstruction_gap':mean_gap})
    before=json.loads((HERE/'state_before.json').read_text())
    for name in ['src/classes/Solver.C','UN_model/VERIFICATION/solver_harness.cpp',
        'UN_model/VERIFICATION/analytic.py','UN_model/VERIFICATION/phase4b/resumed/reference.py']:
        old=subprocess.check_output(['git','show',before['head']+':'+name],cwd=ROOT)
        assert hashlib.sha256(old).hexdigest()==sha(ROOT/name)
    assert subprocess.check_output(['git','rev-parse',PLAN['backup_branch']],cwd=ROOT,text=True).strip()==before['head']
    assert subprocess.check_output(['git','rev-parse','origin/development/nitride'],cwd=ROOT,text=True).strip()==before['origin']
    result={'status':'PASS','kept_preexisting_files_byte_identical':len(kept),'deleted_paths_absent_and_ignored':len(deleted),
        'retained_files_accidentally_ignored':0,'all_original_python_scripts_retained':len(original_scripts),
        'retained_cpp_input_files':len(input_files),'frozen_cases':160,'LHS_seed_and_latent_coordinates_rechecked':True,
        'order_checks':orders,'compact_archive_CRC_checks':2,'build':built,'smoke_tests':checks,
        'smoke_artifacts_outside_repository':str(work),'complete_campaign_repeated':False,
        'original_production_solver_and_analytical_helpers_byte_identical':True,
        'old_commit_backup_branch':PLAN['backup_branch'],'old_commit':before['head'],
        'origin_tracking_reference_unchanged':before['origin'],'no_push_or_GC':True}
    save(HERE/'validation_after.json',result);print(json.dumps(result,indent=2),flush=True)


if __name__=='__main__':main()
