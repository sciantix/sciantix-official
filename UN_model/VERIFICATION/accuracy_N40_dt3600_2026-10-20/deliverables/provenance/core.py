"""Read-only source access and shared accuracy metrics; all writes local."""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import csv,json,time,hashlib,subprocess
sys.path.insert(0,str(Path(__file__).resolve().parent.parent/'phase4b/resumed'))
import reference as ref
from reference import rv,np,ROOT,OBS,OBS_HOURS,HOURS,average,matrices,projections,VOLUME
from common import decay_rates
HERE=Path(__file__).resolve().parent
SOURCE=HERE.parent/'extended_convergence_2026-10-20'
END=14400000.
POPS=['c','mb','md']
CASES=json.loads((SOURCE/'cases.json').read_text())
rv.BUILD=HERE/'build'


def guard(reserve=60.):
    b=json.loads((HERE/'budget.json').read_text());left=b['deadline_epoch']-time.time()
    if left<reserve:raise RuntimeError(f'BUDGET_STOP: {left:.1f}s remaining')
    return left


def table(path,rows):
    with open(path,'w',newline='') as f:
        fields=list(dict.fromkeys(k for row in rows for k in row))
        w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(rows)


def load_csv(path):
    with open(path,newline='') as f:return list(csv.DictReader(f))


def old_data(case):
    guard();base=SOURCE/'raw'/case['name']
    m=json.loads((base/'reference_manifest.json').read_text())
    for key,digest in [('hourly_file','hourly_sha256'),('high_file','high_sha256')]:
        assert rv.sha(ROOT/m[key])==m[digest]
    with np.load(ROOT/m['hourly_file']) as z:low=z['means'];meta=json.loads(str(z['metadata']))
    with np.load(ROOT/m['high_file']) as z:high=z['means'];hm=z['modes']
    direct40=average(hm[:,:,:40])
    scales=np.maximum(abs(low).max(axis=0),1.)
    uncertainty=np.maximum(abs(direct40-low[np.array(OBS_HOURS)]).max(axis=0),
                            np.asarray(meta['split_gap_normalized'])*scales)
    uncertainty=np.maximum(uncertainty,100*np.finfo(float).eps*scales)
    return low,high,hm,m,uncertainty


def old_cpp(case,n,dt):
    """Return only exact matches, including source and initialization."""
    if n!=40:return None
    path=SOURCE/'raw'/case['name']/f'dt_{dt:g}'/'cpp_manifest.json'
    if not path.exists():return None
    m=json.loads(path.read_text());folder=ROOT/m['source']
    expected=[n,dt,END,round(3600/dt),case['D'],case['R'],case['gb'],case['gd'],case['bb'],case['bd'],*case['source'],*case['initial']]
    assert np.array_equal(np.loadtxt(folder/'input.txt'),expected)
    assert rv.sha(folder/'output.bin')==m['output_sha256']
    a=np.fromfile(folder/'output.bin',dtype='<f8').reshape(-1,4+3*n)
    assert np.array_equal(a[:,0],HOURS) and np.isfinite(a).all()
    timing=json.loads((folder/'timing.json').read_text())
    return a[:,1:4],{'case':case['name'],'N':n,'dt_s':dt,'source':str(folder.relative_to(ROOT)),
        'output_sha256':m['output_sha256'],'reused':True,'solver_seconds':timing['solver_seconds'],
        'new_process_seconds':0.,'original_process_seconds':m.get('new_process_seconds')}


def metric_vectors(numerical,analytical,scale,uncertainty):
    signed=numerical-analytical;absolute=abs(signed)
    valid=abs(analytical)>np.maximum(1.,20*np.asarray(uncertainty))
    relative=np.divide(absolute,abs(analytical),out=np.full_like(absolute,np.nan),where=valid)
    normal=absolute/np.maximum(scale,1.)
    return signed,absolute,relative,normal,valid


def maxima(case,pop,kind,times,numerical,analytic,scale,unc,scope):
    signed,ab,rel,normal,valid=metric_vectors(numerical,analytic,scale,unc)
    rows=[]
    active=bool(np.any(abs(analytic)>0))
    for name,values in [('absolute',ab),('relative_instantaneous',rel),('normalized_population_scale',normal)]:
        finite=np.flatnonzero(np.isfinite(values))
        i=int(finite[np.argmax(values[finite])]) if len(finite) else None
        rows.append({'case':case['name'],'group':case['group'],'population':pop,'error_kind':kind,
            'metric':name,'maximum_observed':float(values[i]) if i is not None else None,
            't_s':float(times[i]) if i is not None else None,'hour':float(times[i]/3600) if i is not None else None,
            'signed_error_at_maximum':float(signed[i]) if i is not None else None,
            'absolute_error_at_maximum':float(ab[i]) if i is not None else None,
            'instantaneous_relative_at_maximum':float(rel[i]) if i is not None and valid[i] else None,
            'normalized_at_maximum':float(normal[i]) if i is not None else None,
            'analytic_at_maximum':float(analytic[i]) if i is not None else None,
            'numerical_at_maximum':float(numerical[i]) if i is not None else None,
            'population_scale':float(scale),'sampling_scope':scope,'observed_times':len(times),
            'max_sampling_gap_s':float(np.diff(times).max()) if len(times)>1 else 0,
            'status':'inactive_population' if not active else ('relative_undefined' if i is None else 'observed_discrete_maximum'),
            'undefined_relative_rows':int((~valid).sum()),'continuous_maximum_claim':False})
    return rows


def evaluated_modes(case,n,times,folder):
    """Direct expm with the original 4000 h scaling; cache by N and time."""
    guard();folder.mkdir(parents=True,exist_ok=True)
    times=np.asarray(times,dtype=float)
    digest=hashlib.sha256(times.astype('<f8').tobytes()).hexdigest()[:16]
    path=folder/f'N{n}_{digest}.npz'
    if path.exists():
        with np.load(path) as z:assert np.array_equal(z['time'],times);return z['modes']
    active,scale,b,y0=ref.system(case,n,duration=END)
    q=projections(n);modes=np.zeros((len(times),3,n))
    for i,t in enumerate(times):
        guard();y=np.broadcast_to(y0,(n,len(y0))) if t==0 else ref.expm(b*t)@y0
        modes[i,active,:]=(y[:,:-1]*(scale*q[:,None])).T
    assert np.isfinite(modes).all()
    np.savez_compressed(path,time=times,modes=modes)
    return modes


def build():
    path=HERE/'build_manifest.json'
    if not path.exists():rv.save(path,rv.build(HERE))
    b=json.loads(path.read_text());assert b['solver_sha256']==rv.sha(ROOT/'src/classes/Solver.C')
    return b


def new_cpp(case,n,dt,end,folder,record_interval=3600.):
    guard();folder.mkdir(parents=True,exist_ok=True);p=folder/'manifest.json'
    if p.exists():
        m=json.loads(p.read_text());assert rv.sha(folder/'cpp/output.bin')==m['output_sha256']
        a=np.fromfile(folder/'cpp/output.bin',dtype='<f8').reshape(-1,4+3*n)
        return a[:,0],a[:,1:4],a[:,4:].reshape(-1,3,n),m
    t,c,modes,timing=rv.run_cpp(folder/'cpp',case,n,dt,end,record_interval)
    assert np.isfinite(c).all() and t[-1]==end
    m={'case':case['name'],'N':n,'dt_s':dt,'end_s':end,'steps':timing['steps'],
        'source':str((folder/'cpp').relative_to(ROOT)),'output_sha256':rv.sha(folder/'cpp/output.bin'),
        'reused':False,'solver_seconds':timing['solver_seconds'],'new_process_seconds':timing['process_seconds']}
    rv.save(p,m);return t,c,modes,m
