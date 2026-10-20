#!/usr/bin/env python3
"""Extract solver-call coefficients; no fitting, comparisons or physics changes."""
import sys
sys.dont_write_bytecode = True
from pathlib import Path
import ast, csv, dataclasses, gzip, hashlib, inspect, json, math, os, subprocess, time, types

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
os.environ['MPLCONFIGDIR'] = str(HERE/'matplotlib_cache')
os.environ['MPLBACKEND'] = 'Agg'
BASE = ROOT/'UN_model/notebooks/BaseRizk2025.ipynb'
M7 = ROOT/'UN_model/optuna/M7_v14/UN_M7_optuna_calibration_v14_rhoSat_qgbStrict_NdAnchors_STANDALONE.py'
FIXED = ROOT/'UN_model/optuna/M7_v14/fixed_parameter_plots/M7_v14_fixed_model_plots.ipynb'
CASES = [('AP3p2_900K','AP3.2',900.),('AP3p4_1600K','AP3.4',1600.),
         ('AP3p4_1800K','AP3.4',1800.),('ANP6_1600K','ANP6',1600.)]
FIELDS = ['modello_variante','caso','tempo_s','Dg','gb','gd','bb','bd','file_sorgente',
          'tempo_fine_s','dt_s','fission_rate_m3_s','n_modes','source_sha256']


def sha(p):
    return hashlib.sha256(Path(p).read_bytes()).hexdigest()


def save(p,v):
    p.write_text(json.dumps(v,indent=2,allow_nan=False)+'\n')


def csv_write(path,rows):
    with path.open('w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=FIELDS);w.writeheader();w.writerows(rows)


def module(name):
    m=types.ModuleType(name);sys.modules[name]=m;return m


def notebook_module(path,cells,name):
    m=module(name);book=json.loads(path.read_text())
    if path==FIXED:
        # The initialization cell also creates plot directories; only its
        # untouched imports are needed by the standalone model definitions.
        init=ast.parse(''.join(book['cells'][1]['source']))
        init.body=[n for n in init.body if isinstance(n,(ast.Import,ast.ImportFrom))]
        exec(compile(init,f'{path}:cell1-imports','exec'),m.__dict__)
    for i in cells:
        source=''.join(book['cells'][i]['source'])
        # Only relocate the output setting; no solver function is edited.
        tree=ast.parse(source,filename=f'{path}:cell{i}')
        for node in tree.body:
            if isinstance(node,ast.Assign) and any(isinstance(t,ast.Name) and t.id=='OUTPUT_DIR' for t in node.targets):
                node.value=ast.Constant(str(HERE/'unused_model_output'))
        exec(compile(ast.fix_missing_locations(tree),f'{path}:cell{i}','exec'),m.__dict__)
    return m


def m7_module():
    tree=ast.parse(M7.read_text());m=module('extracted_original_m7');m.__dict__['__file__']=str(M7)
    for n in tree.body:
        if isinstance(n,ast.Assign) and any(isinstance(t,ast.Name) and t.id=='OUTPUT_DIR' for t in n.targets):
            n.value=ast.Constant(str(HERE/'unused_model_output'))
    # Original RUN_AUTOMATIC=False and __name__ != '__main__' prevent sweeps.
    exec(compile(ast.fix_missing_locations(tree),str(M7),'exec'),m.__dict__)
    assert m.RUN_AUTOMATIC is False
    return m


def observe(m,variant,case,path,run):
    folder=HERE/'raw'/variant/case;folder.mkdir(parents=True,exist_ok=True)
    cached=folder/'coefficients.csv';manifest=folder/'manifest.json'
    if cached.exists() and manifest.exists():
        meta=json.loads(manifest.read_text());assert sha(cached)==meta['output_sha256']
        with cached.open() as f:return list(csv.DictReader(f)),meta
    rows=[];original=m.sciantix_3x3_exchange_step;signature=inspect.signature(original)
    parameter_records=[]
    source=str(path.relative_to(ROOT));source_hash=sha(path)
    def wrapper(*args,**kwargs):
        bound=signature.bind(*args,**kwargs).arguments
        caller=inspect.currentframe().f_back
        loc=caller.f_locals;p=loc['p'];t=float(loc['t']);dt=float(bound['dt'])
        if not parameter_records:parameter_records.append(dataclasses.asdict(p))
        row=dict(zip(FIELDS,[variant,case,t,bound['Dg'],bound['g_b'],bound['g_d'],
            bound['b_b_gas'],bound['b_d_gas'],source,t+dt,dt,p.fission_rate,p.n_modes,source_hash]))
        assert all(math.isfinite(float(row[k])) and float(row[k])>=0 for k in ['Dg','gb','gd','bb','bd'])
        # Observations neither alter input vectors nor replace any arithmetic.
        answer=original(*args,**kwargs)
        rows.append(row)
        return answer
    m.sciantix_3x3_exchange_step=wrapper;start=time.perf_counter()
    try:run()
    finally:m.sciantix_3x3_exchange_step=original
    assert rows and rows[0]['tempo_s']==0
    p=parameter_records[0]
    assert abs(rows[-1]['tempo_fine_s']-p['final_time'])<1e-8*max(1.,p['final_time'])
    for a,b in zip(rows,rows[1:]):assert abs(a['tempo_fine_s']-b['tempo_s'])<1e-7
    csv_write(cached,rows)
    meta={'variant':variant,'case':case,'status':'ok','rows':len(rows),'seconds':time.perf_counter()-start,
          'source':source,'source_sha256':source_hash,'parameters':p,'output_sha256':sha(cached),
          'bb_bd_definition':'b_b_gas and b_d_gas arguments actually passed to sciantix_3x3_exchange_step',
          'time_definition':'tempo_s=start of interval; tempo_fine_s=actual end; final partial step retained',
          'calibration_performed':False}
    save(manifest,meta);print(variant,case,len(rows),round(meta['seconds'],2),flush=True)
    return rows,meta


def frozen(case):
    path=ROOT/'UN_model/regression_reference/results/full'/case/'trace.jsonl.gz'
    manifest=json.loads((path.parent/'manifest.json').read_text());p=manifest['parameters'];rows=[]
    source=str(path.relative_to(ROOT));source_hash=sha(path)
    with gzip.open(path,'rt') as f:
        for line in f:
            e=json.loads(line)
            if e['stage']!='after_exchange':continue
            s=e['scalars'];t=s['t'];dt=s['dt']
            rows.append(dict(zip(FIELDS,['UN_L3_frozen',case,t,s['Dg'],s['g_b'],s['g_d'],
                s['b_b_gas'],s['b_d_gas'],source,t+dt,dt,p['fission_rate'],p['n_modes'],source_hash])))
    assert len(rows)==manifest['steps']
    assert abs(rows[-1]['tempo_fine_s']-manifest['final_time_s'])<1e-6
    return rows,{'variant':'UN_L3_frozen','case':case,'rows':len(rows),'source':source,
                 'source_sha256':source_hash,'parameters':p,'new_execution':False}


def main():
    started=time.perf_counter();rows=[];records=[]
    base=notebook_module(BASE,[1,2,6,8,10],'extracted_base_rizk2025')
    for case,cid,temp in CASES:
        data,meta=observe(base,'BaseRizk2025_uncalibrated',case,BASE,
            lambda cid=cid,temp=temp:base.run_model_point_case(temp,cid,base.make_candidate(),base.DT_H,base.N_MODES,False))
        rows.extend(data);records.append(meta)
    m7=m7_module()
    # Existing nominal dataclass and existing family selector; only the requested
    # benchmark T, burnup and case Fdot are supplied, without any fitted values.
    for family in ['M7_no_phi','M7_full']:
        m7.set_model_family(family)
        for case,cid,temp in CASES:
            cfg=base.EXPERIMENT_CASES[cid]
            p=m7.UNParameters(temperature=temp,fission_rate=cfg['fission_rate'],
                target_burnup_percent_fima=cfg['burnup'],dt=m7.FINAL_DT_H*3600,n_modes=m7.FINAL_N_MODES)
            data,meta=observe(m7,family+'_nominal',case,M7,lambda p=p:m7.solve_UN_M7(p,False))
            rows.extend(data);records.append(meta)
    fixed=notebook_module(FIXED,[2,3],'extracted_fixed_m7_v14')
    for case,cid,temp in CASES:
        # Preserve fixed candidate Fdot and the original rhoFT2 wrapper.
        # The manifest discloses this calibrated Fdot for every case.
        burnup=base.EXPERIMENT_CASES[cid]['burnup']
        data,meta=observe(fixed,'M7_v14_capture_only_fixed_rank1',case,FIXED,
            lambda temp=temp,burnup=burnup:fixed.run_model_point_rhoFT2(temp,burnup,fixed.candidate,fixed.DT_H,fixed.N_MODES,False))
        rows.extend(data);records.append(meta)
    for case,_,_ in CASES:
        data,meta=frozen(case);rows.extend(data);records.append(meta)
    out=HERE/'UN_coefficients_histories.csv';csv_write(out,rows)
    before=json.loads((HERE/'preservation_before.json').read_text())
    for name,value in before['files'].items():assert sha(ROOT/name)==value
    assert subprocess.check_output(['git','branch','--show-current'],text=True).strip()==before['branch']
    assert subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()==before['head']
    assert hashlib.sha256(subprocess.check_output(['git','diff','--cached','--binary'])).hexdigest()==before['index_sha256']
    assert subprocess.check_output(['git','diff','--binary'],text=True)==before['unstaged']
    with out.open() as f:exported=list(csv.DictReader(f))
    assert len(exported)==len(rows)
    for a,b in zip(rows,exported):
        for k in ['tempo_s','tempo_fine_s','Dg','gb','gd','bb','bd']:
            assert float(a[k])==float(b[k])
    save(HERE/'manifest.json',{'status':'PASS','rows':len(rows),'CSV_sha256':sha(out),
        'seconds':time.perf_counter()-started,'series':records,'preservation_check_passed':True,
        'M7_named_families':'nominal parameter dataclass, existing set_model_family; no saved calibrated M7_no_phi/full candidates found',
        'fixed_v14_fission_rate':'unchanged calibrated candidate Fdot, disclosed in CSV; not overwritten with case Fdot',
        'no_scientific_comparison_performed':True})
    print('CSV',out,len(rows),sha(out),flush=True)


if __name__=='__main__':main()
