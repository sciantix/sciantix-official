"""Read-only reuse of validated 4B helpers and a separate campaign budget/build."""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import csv
import hashlib
import json
import time
import subprocess
import zipfile
sys.path.insert(0,str(Path(__file__).resolve().parent.parent/'phase4b/resumed'))
import reference as ref
from reference import rv, np, ROOT, OBS, OBS_HOURS, HOURS, average, direct_modes, matrices, projections, VOLUME
from common import decay_rates

HERE=Path(__file__).resolve().parent
OLD=HERE.parent/'phase4b/resumed'
END=14400000.
DTS=[3600.,1800.,900.,450.,225.]
NS=[10,20,40,80,160,320]
POPS=['c','mb','md']
rv.BUILD=HERE/'build'


class BudgetStop(Exception):pass


def guard(reserve=60.):
    path=HERE/'budget.json'
    if not path.exists():rv.save(path,{'start_epoch':time.time(),'limit_seconds':5400,'deadline_epoch':time.time()+5400})
    b=json.loads(path.read_text())
    remaining=b['deadline_epoch']-time.time()
    if remaining<reserve:raise BudgetStop(f'Only {remaining:.1f}s left; reserve={reserve}s')
    return remaining


_unwrapped_expm=ref.expm
def _budgeted_expm(*args,**kwargs):
    # The validated numerical routine is unchanged; check the campaign budget
    # before each batch so a high-N expansion can stop at a safe boundary.
    guard(60.)
    return _unwrapped_expm(*args,**kwargs)
ref.expm=_budgeted_expm


def result_file(case_name,filename):
    manifest=HERE/'extensions_manifest.json'
    if manifest.exists():
        table_path=json.loads(manifest.read_text())['case_tables'].get(case_name)
        if table_path and (HERE/table_path/filename).exists():return HERE/table_path/filename
    return HERE/'raw'/case_name/filename


def write_csv(path,rows):
    with open(path,'w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)


def fingerprint(case):
    return tuple(case[k] for k in ['D','R','gb','gd','bb','bd'])+tuple(case['source'])+tuple(case['initial'])


OLD_CASES=json.loads((OLD.parent/'cases.json').read_text())
OLD_MATCH={fingerprint(c):c for c in OLD_CASES}
OLD_LEDGER=json.loads((OLD/'main_ledger.json').read_text())


def build():
    (HERE/'raw').mkdir(exist_ok=True)
    if not (HERE/'build_manifest.json').exists():rv.save(HERE/'build_manifest.json',rv.build(HERE))
    b=json.loads((HERE/'build_manifest.json').read_text())
    assert b['solver_sha256']==rv.sha(ROOT/'src/classes/Solver.C')
    assert b['binary_sha256']==rv.sha(rv.BUILD/'solver_harness.x')
    return b


def cpp(case,dt):
    guard()
    folder=HERE/'raw'/case['name']/f'dt_{dt:g}'
    manifest=folder/'cpp_manifest.json'
    if manifest.exists():
        record=json.loads(manifest.read_text());source=ROOT/record['source']
        assert rv.sha(source/'output.bin')==record['output_sha256']
    else:
        folder.mkdir(parents=True,exist_ok=True)
        old=OLD_MATCH.get(fingerprint(case));source=None
        if old:
            candidates=[r for r in OLD_LEDGER if r['case']==old['name'] and r['N']==40 and r['dt_s']==dt]
            if candidates:
                record=candidates[0]
                assert json.loads((OLD/'build_manifest.json').read_text())['solver_sha256']==rv.sha(ROOT/'src/classes/Solver.C')
                source=ROOT/record['source'];assert rv.sha(source/'output.bin')==record['output_sha256']
        reused=source is not None
        if source is None:
            source=folder/'cpp'
            # Preserve a process interrupted during file creation. Retry only
            # in a fresh directory; never remove or truncate its raw output.
            attempt=0
            while source.exists():
                try:
                    complete=((source/'output.bin').stat().st_size==4001*124*8 and
                        json.loads((source/'timing.json').read_text())['steps']==round(END/dt))
                except (FileNotFoundError,json.JSONDecodeError,KeyError):complete=False
                if complete:break
                attempt+=1;source=folder/f'cpp_attempt_{attempt}'
            if not (source/'timing.json').exists():
                t,means,modes,timing=rv.run_cpp(source,case,40,dt,END)
                process=timing['process_seconds']
            else:process=None
        else:process=None
        timing=json.loads((source/'timing.json').read_text())
        record={'case':case['name'],'N':40,'dt_s':dt,'end_s':END,'steps':timing['steps'],
            'reused_from_4B':reused,'source':str(source.relative_to(ROOT)),
            'output_sha256':rv.sha(source/'output.bin'),'solver_seconds':timing['solver_seconds'],
            'new_process_seconds':process,'parameter_fingerprint':list(fingerprint(case))}
        rv.save(manifest,record)
    a=np.fromfile(source/'output.bin',dtype='<f8').reshape(-1,124)
    assert np.array_equal(a[:,0],HOURS) and np.isfinite(a).all()
    assert record['steps']==round(END/dt)
    return a[:,1:4],a[:,4:].reshape(-1,3,40),record


def reference(case):
    guard()
    folder=HERE/'raw'/case['name']/'cache';folder.mkdir(parents=True,exist_ok=True)
    old=OLD_MATCH.get(fingerprint(case))
    start=time.perf_counter()
    if old:
        hp=OLD/'cache'/f'{old["name"]}_N40_hourly.npz'
        with np.load(hp) as z:means=z['means'];modes=z['modes'];meta=json.loads(str(z['metadata']))
        hmeta=json.loads((OLD/'cache'/f'{old["name"]}_high_reference.json').read_text())
        highpath=OLD/'cache'/hmeta['file']
        with np.load(highpath) as z:high=z['means'];highm=z['modes']
    else:
        attempt=0
        while True:
            try:
                means,modes,meta=ref.hourly(case,40,folder)
                hp=folder/f'{case["name"]}_N40_hourly.npz'
                high,highm,hmeta=ref.high_reference(case,folder)
                highpath=folder/hmeta['file']
                break
            except (zipfile.BadZipFile,EOFError,json.JSONDecodeError):
                # An interrupted cache write is retained for inspection.
                # Numerical/reference assertions are deliberately not caught.
                attempt+=1;folder=HERE/'raw'/case['name']/f'cache_attempt_{attempt}'
                folder.mkdir(exist_ok=True)
    # Bind numerical caches to these actual coefficients. The reused helper's
    # legacy case_sha256 field identifies its 4B dependency, not this sample.
    binding={'case':case['name'],'parameter_fingerprint':list(fingerprint(case)),
        'hourly_file':str(hp.relative_to(ROOT)),'hourly_sha256':rv.sha(hp),
        'high_file':str(highpath.relative_to(ROOT)),'high_sha256':rv.sha(highpath),
        'N_high':hmeta['N'],'high_convergence':hmeta['records'][-1],
        'hourly_validation':meta,'reused_from_4B':bool(old),'acquire_seconds':time.perf_counter()-start,
        'legacy_metadata_note':'hourly_validation.case_sha256 is the unchanged 4B helper dependency; actual inputs bound by parameter_fingerprint'}
    rv.save(HERE/'raw'/case['name']/'reference_manifest.json',binding)
    return means,modes,high,highm,binding


def discrete_fixed(case,dt):
    """Independent BE diagnostic at fixed times, never the analytical IVP."""
    j=matrices(case,40);a=np.eye(3)[None]-dt*j
    r=np.linalg.solve(a,np.broadcast_to(np.eye(3),a.shape))
    scale=max(case['source'])*END
    b=np.linalg.solve(a,np.broadcast_to(np.asarray(case['source'])*dt/scale,(40,3))[...,None])[...,0]
    block=np.zeros((40,4,4));block[:,:3,:3]=r;block[:,:3,3]=b;block[:,3,3]=1
    block=np.asarray(block,dtype=np.longdouble)
    states=[]
    for t in OBS:
        y=np.linalg.matrix_power(block,round(t/dt))[:,:3,3]
        states.append(np.asarray((y*(scale*projections(40)[:,None])).T,dtype=float))
    modes=np.asarray(states)
    return average(modes),modes
