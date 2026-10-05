#!/usr/bin/env python3
"""Exactly 60 fixed NSGA-II multiobjective trials, then three diverse full-resolution reruns.
Run: .venv/bin/python UN_model/optuna/Optuna_rho_constant_Rizk.py
Resume: same command. Offline rescore: same command --rescore (no solver).
"""
import os
os.environ['MPLBACKEND'] = 'Agg'
os.environ.setdefault('MPLCONFIGDIR', '/tmp/Optuna_rho_constant_Rizk_matplotlib')
os.environ['OPENBLAS_NUM_THREADS'] = '1'
os.environ['OMP_NUM_THREADS'] = '1'
import argparse, hashlib, json, pickle, time, traceback, shutil
from pathlib import Path
from dataclasses import asdict
from concurrent.futures import ProcessPoolExecutor, as_completed
import multiprocessing as mp
import numpy as np
import pandas as pd
import optuna
from optuna.trial import TrialState
ROOT = Path(__file__).resolve().parent
OUT = ROOT/'Optuna_rho_constant_Rizk_results'
SOURCE = ROOT.parent/'notebooks/finalRizkUN.ipynb'
NAME = 'Optuna_rho_constant_Rizk'
SEED = 20251005
POPULATION = 12
TOTAL = 60
WORKERS = min(8, os.cpu_count() or 1)
RANGES = {'f_n':(1e-7,1e-2),'K_d':(1e5,2e6),'rho_d':(1e13,7e13),
          'Dv_dislocation_scale':(1,30),'Dg_dislocation_scale':(1,30),'NGF_AREAL_0':(2e12,2e14)}
BASELINE = dict(f_n=5.5e-4,K_d=3e5,rho_d=3.5e13,Dv_dislocation_scale=10.,Dg_dislocation_scale=15.,NGF_AREAL_0=1e13)

def digest(path): return hashlib.sha256(path.read_bytes()).hexdigest()
def write_json(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix+'.tmp'); tmp.write_text(json.dumps(data,indent=2,ensure_ascii=False,default=lambda x: x.tolist() if isinstance(x,np.ndarray) else x.item() if isinstance(x,np.generic) else float(x))); tmp.replace(path)
def write_csv(path, data):
    tmp = path.with_suffix('.csv.tmp'); data.to_csv(tmp,index=False); tmp.replace(path)

def prepare(require_source_match=True):
    OUT.mkdir(exist_ok=True)
    snapshot = OUT/'source_notebook.ipynb'
    if not snapshot.exists(): shutil.copyfile(SOURCE,snapshot)
    if require_source_match:
        assert digest(SOURCE)==digest(snapshot), 'Source changed since campaign start; cannot mix physics.'
    global model, evaluate, OBJECTIVES
    import Optuna_rho_constant_Rizk_model as model
    from Optuna_rho_constant_Rizk_metrics import evaluate, OBJECTIVES
    assert asdict(model.calibration_candidate(BASELINE,'CURRENT_BASELINE')) == asdict(model.make_candidate('CURRENT_BASELINE'))|{'ngf_areal_0':BASELINE['NGF_AREAL_0']}
    manifest = dict(study=NAME,source=str(SOURCE),source_sha256=digest(snapshot),seed=SEED,
                    sampler='NSGA-II',population_size=POPULATION,total_trials=TOTAL,search_ranges_log=RANGES,
                    baseline=BASELINE,screening_T_K=list(range(900,1801,50)),full_T_K=list(range(900,1801,25)),
                    dt_h=model.DT_H,n_modes=model.N_MODES,cases=model.EXPERIMENT_CASES,adaptations=model.ADAPTATIONS,
                    objectives=OBJECTIVES,swelling='mean of four per-pin RMSE/max(experiment), equal pin weights; 899 K excluded',
                    micro='0.6 relative Rd RMSE + 0.4 log10 Nd RMSE, experimental AP3.4 900<=T<=1600 only',
                    FGR='RMSE pp: equal burnup weights; at 1.1% average AP3.2/AP3.8 squared errors; reference native 25 K grid',
                    gas='same burnup weighting as FGR; matrix/bulk/dislocation RMS diagnostic only',
                    failure_policy='FAIL for solver/nonfinite core/negative inventory/mass-balance>0.1pp/total bubble volume>=100%; no objective penalties',
                    numeric_diagnostics='Final states at every temperature; original numerical clipping/guards unchanged. Geometry/pressure/order warnings only.',
                    units={'T':'K','burnup':'% FIMA','fission_rate':'fissions m^-3 s^-1','Rd/Rb/Rgf':'m',
                           'Nd/Nb/Ngf':'m^-3 (Ngf uses Ngf_vol)','Ngf_areal/NGF_AREAL_0':'m^-2',
                           'swelling_d/swelling_bulk/swelling_gf':'percentage points','gas/FGR':'percent of generated gas','pressure':'Pa'},
                    optuna_version=optuna.__version__,workers=WORKERS,
                    scheduling='sequential ask/tell, only material points parallel; sampler pickled after sampling and completion',
                    frozen_settings={k:v for k,v in vars(model).items() if k.isupper() and isinstance(v,(str,int,float,bool,dict,list,tuple)) and k not in ['RUN_CACHE','RIZK2025_GAS_PARTITION','SCHNEIDER_FIG7_EXP_POINTS']})
    path=OUT/'manifest.json'
    if path.exists():
        old=json.loads(path.read_text()); assert old['source_sha256']==manifest['source_sha256'] and old['search_ranges_log']==json.loads(json.dumps(RANGES))
        assert old['seed']==SEED and old['population_size']==POPULATION and old['total_trials']==TOTAL
    else: write_json(path,manifest)
    write_csv(OUT/'experimental_swelling.csv',pd.DataFrame(model.EXP_SWELLING_T))
    write_csv(OUT/'experimental_Rd.csv',pd.DataFrame(model.EXP_RD_T_13))
    write_csv(OUT/'experimental_Nd.csv',pd.DataFrame(model.EXP_ND_T_13))
    write_csv(OUT/'experimental_swelling_1600.csv',pd.DataFrame(model.EXP_SWELLING_BURNUP_1600))
    refs=[]
    for bu,ref in model.RIZK2025_GAS_PARTITION.items():
        for i,T in enumerate(ref['T_K']):
            if 900<=T<=1800: refs.append({'burnup':bu,'T':T,**{k:float(v[i]) for k,v in ref.items() if k!='T_K'}})
    write_csv(OUT/'Rizk_Fig9_benchmark.csv',pd.DataFrame(refs))
    return manifest

def point_chunk(trial, parameters, label, case, temperatures):
    import Optuna_rho_constant_Rizk_model as m
    m.RUN_CACHE.clear()
    cand=m.calibration_candidate(parameters,label)
    rows=[]
    for T in temperatures:
        row={'trial':trial,'trial_label':label,'case':case,'T':T,'burnup':m.EXPERIMENT_CASES[case]['burnup'],
             'fission_rate':m.EXPERIMENT_CASES[case]['fission_rate'],**parameters,'DT_H':m.DT_H,'N_MODES':m.N_MODES}
        try:
            result=m.run_model_point_case(T,case,cand,m.DT_H,m.N_MODES,keep_history=False)
            row.update({k:v for k,v in result.items() if k not in ('hist','rates')})
            for output,original in {'swelling_d':'swelling_d_percent','swelling_bulk':'swelling_b_percent','swelling_gf':'swelling_gf_percent',
                                    'matrix_gas':'matrix_gas_percent','bulk_gas':'bulk_gas_percent','dislocation_gas':'dislocation_gas_percent',
                                    'grainface_gas':'grainface_gas_percent','FGR':'release_gas_percent', 'Ngf':'Ngf_vol'}.items(): row[output]=row[original]
            # Radius values saved in SI, plus public notebook nm columns.
            for output,original in [('Rd','Rd_nm'),('Rb','Rb_nm'),('Rgf','Rgf_nm')]: row[output]=row[original]*1e-9
            row.update(status='ok',error='')
        except Exception as exc:
            row.update(status='failed',error=f'{type(exc).__name__}: {exc}',traceback=traceback.format_exc())
        rows.append(row)
    return rows

def simulate(pool, number, parameters, label, step, checkpoint):
    checkpoint.mkdir(parents=True,exist_ok=True)
    rows=[]; futures={}
    for case in model.EXPERIMENT_CASE_ORDER:
        grid=np.arange(900.,1800.+step/2,step)
        for chunk,ts in enumerate(np.array_split(grid,2)):
            path=checkpoint/f'{case.replace(".","p")}_{chunk}.json'
            if path.exists():
                data=json.loads(path.read_text()); assert data['parameters']==parameters and data['temperatures']==list(ts)
                rows.extend(data['rows']); continue
            future=pool.submit(point_chunk,number,parameters,label,case,list(ts))
            futures[future]=(path,list(ts))
    for future in as_completed(futures):
        path,ts=futures[future]; part=future.result()
        write_json(path,{'parameters':parameters,'temperatures':ts,'rows':part}); rows.extend(part)
        write_json(OUT/'progress.json',{'phase':'screening' if step==50 else 'full_rerun','trial':number,'label':label,'points_this_configuration':len(rows),'expected':4*len(np.arange(900,1801,step)),'updated_utc':__import__('datetime').datetime.now(__import__('datetime').timezone.utc).isoformat()})
    frame=pd.DataFrame(rows).sort_values(['case','T']).reset_index(drop=True)
    assert len(frame)==4*len(np.arange(900,1801,step)) and not frame.duplicated(['case','T']).any()
    return frame

def save_sampler(sampler):
    path=OUT/'sampler.pkl'; tmp=path.with_suffix('.pkl.tmp'); tmp.write_bytes(pickle.dumps(sampler)); tmp.replace(path)

def export(study):
    records=[]; curves=[]
    for trial in study.get_trials(deepcopy=False):
        record={'trial':trial.number,'state':trial.state.name,'trial_label':trial.user_attrs.get('label','OPTUNA'),**trial.params}
        path=OUT/'trial_checkpoints'/f'trial_{trial.number:03d}'/'metrics.json'
        if path.exists(): record.update(json.loads(path.read_text()))
        if trial.values is not None: record.update(dict(zip(OBJECTIVES,trial.values)))
        record['pareto']=trial.number in {t.number for t in study.best_trials}
        records.append(record)
        curve=path.with_name('curves.csv')
        if curve.exists():
            sub=pd.read_csv(curve); sub['trial_state']=trial.state.name; curves.append(sub)
    table=pd.DataFrame(records)
    write_csv(OUT/'trial_metrics.csv',table)
    # Full ledger retains diagnostics plus all objective values and parameters.
    write_csv(OUT/'trials.csv',table)
    write_csv(OUT/'pareto_front.csv',table.loc[table.pareto])
    if curves: write_csv(OUT/'all_trial_curves.csv',pd.concat(curves,ignore_index=True).sort_values(['trial','case','T']))
    return table

def screen(pool):
    sampler=pickle.loads((OUT/'sampler.pkl').read_bytes()) if (OUT/'sampler.pkl').exists() else optuna.samplers.NSGAIISampler(seed=SEED,population_size=POPULATION)
    optuna.logging.set_verbosity(optuna.logging.WARNING)
    study=optuna.create_study(study_name=NAME,storage=f'sqlite:///{OUT/"study.db"}',directions=['minimize']*3,sampler=sampler,load_if_exists=True)
    if not study.trials: study.enqueue_trial(BASELINE,user_attrs={'label':'CURRENT_BASELINE'})
    assert len(study.trials)<=TOTAL
    while True:
        running=[t for t in study.trials if t.state==TrialState.RUNNING]
        if running:
            assert len(running)==1
            trial=optuna.trial.Trial(study,running[0]._trial_id)
        elif sum(t.state.is_finished() for t in study.trials)>=TOTAL: break
        else:
            trial=study.ask()
        parameters={k:trial.suggest_float(k,*bounds,log=True) for k,bounds in RANGES.items()}
        label='CURRENT_BASELINE' if trial.number==0 else f'TRIAL_{trial.number:03d}'
        if trial.number==0: assert parameters==BASELINE
        trial.set_user_attr('label',label); save_sampler(study.sampler)
        checkpoint=OUT/'trial_checkpoints'/f'trial_{trial.number:03d}'
        print(f'Start {trial.number+1}/{TOTAL} {label}: {json.dumps(parameters)}',flush=True)
        started=time.monotonic(); frame=simulate(pool,trial.number,parameters,label,50,checkpoint)
        metrics=evaluate(frame); metrics['elapsed_seconds']=time.monotonic()-started
        write_csv(checkpoint/'curves.csv',frame); write_json(checkpoint/'metrics.json',metrics)
        for key in OBJECTIVES+['numeric_valid','gas_partition_core_RMS','warnings']:
            if key in metrics: trial.set_user_attr(key,metrics[key])
        if metrics['numeric_valid']: study.tell(trial,[metrics[o] for o in OBJECTIVES])
        else: study.tell(trial,state=TrialState.FAIL)
        save_sampler(study.sampler); table=export(study)
        print(f'Finished {trial.number+1}/{TOTAL}: '+json.dumps({o:metrics[o] for o in OBJECTIVES})+f'; core={metrics.get("gas_partition_core_RMS")}; {metrics["elapsed_seconds"]:.1f}s',flush=True)
    assert len(study.trials)==TOTAL and all(t.state.is_finished() for t in study.trials)
    return study,export(study)

def normalized(table):
    return np.column_stack([(np.log10(table[k].to_numpy(float))-np.log10(lo))/(np.log10(hi)-np.log10(lo)) for k,(lo,hi) in RANGES.items()])

def select(table):
    # Separate objective gates, no scalarized objective or gas-partition exclusion.
    valid=table[table.state.eq('COMPLETE') & table.numeric_valid.eq(True)].copy()
    best=float(valid.J_swelling.min()); baseline=valid.loc[valid.trial.eq(0)].iloc[0]
    swlimit=max(best*1.6, best+0.04)
    micro_limit=max(float(baseline.J_micro),float(valid.J_micro.quantile(.6)))
    fgr_limit=max(float(baseline.J_FGR)*1.5,float(valid.J_FGR.quantile(.6)))
    pool=valid[(valid.J_swelling<=swlimit)&(valid.J_micro<=micro_limit)&(valid.J_FGR<=fgr_limit)].copy()
    if len(pool)<3:
        # Transparent fallback: widen only selection pool, never optimizer/scoring.
        pool=valid.nsmallest(max(3,min(12,len(valid))),'J_swelling').copy()
    assert len(pool)>=3
    # Balanced representative: choose best swelling among pool's lower half in microstructure error.
    representatives=pool[pool.J_micro<=pool.J_micro.median()]
    a=representatives.sort_values(['J_swelling','J_micro','J_FGR','trial']).iloc[0]
    coords=normalized(pool); trialnums=pool.trial.to_numpy(int)
    chosen=[int(a.trial)]
    for _ in range(2):
        selcoords=normalized(valid[valid.trial.isin(chosen)])
        distance=np.min(np.linalg.norm(coords[:,None,:]-selcoords[None,:,:],axis=2),axis=1)
        distance[np.isin(trialnums,chosen)]=-1
        chosen.append(int(trialnums[np.argmax(distance)]))
    selected=valid.set_index('trial').loc[chosen].reset_index()
    d=np.linalg.norm(normalized(selected)[:,None,:]-normalized(selected)[None,:,:],axis=2)
    metadata={'selection_policy':'separate swelling/micro/FGR competitive gates; no partition filter; A best swelling in lower-half micro pool, B farthest from A, C max-min separation',
              'swelling_limit':swlimit,'micro_limit':micro_limit,'FGR_limit_pp':fgr_limit,
              'competitive_trial_numbers':list(map(int,pool.trial)), 'selected_trial_numbers':chosen,'normalized_log_parameter_distances':d.tolist()}
    write_json(OUT/'selection.json',metadata); write_csv(OUT/'selected_candidates.csv',selected)
    return selected,metadata

def rescore():
    # This branch never creates an executor, asks Optuna, imports plotting, or calls the solver.
    curves=pd.read_csv(OUT/'all_trial_curves.csv'); ledger=pd.read_csv(OUT/'trials.csv'); rows=[]
    for number,sub in curves.groupby('trial'):
        original=ledger.loc[ledger.trial.eq(number)].iloc[0]
        rows.append({'trial':number,'state':original.state,'trial_label':original.trial_label,**{k:float(original[k]) for k in RANGES},**evaluate(sub)})
    frame=pd.DataFrame(rows)
    vals=frame[OBJECTIVES].to_numpy(); eligible=vals[frame.numeric_valid & np.isfinite(vals).all(axis=1)]
    dominated=np.array([any(np.all(v<=vals[i]) and np.any(v<vals[i]) for v in eligible) for i in range(len(vals))])
    frame['pareto']=~dominated&frame.numeric_valid
    write_csv(OUT/'offline_rescored_metrics.csv',frame); write_csv(OUT/'offline_rescored_pareto.csv',frame[frame.pareto])
    print('Offline scores saved; no simulations.',flush=True)

def finish(pool,table,manifest):
    selected,selection=select(table)
    from Optuna_rho_constant_Rizk_plots import create_plots, report
    for letter,row in zip('ABC',selected.to_dict('records')):
        label=f'CANDIDATE_{letter}'; directory=OUT/'final_candidates'/label; directory.mkdir(parents=True,exist_ok=True)
        parameters={k:float(row[k]) for k in RANGES}
        write_json(directory/'parameters.json',{'label':label,'source_trial':int(row['trial']),'parameters':parameters,
                    'full_candidate':asdict(model.calibration_candidate(parameters,label)), 'selection':selection,'screening_metrics':{k:row[k] for k in OBJECTIVES+['E_R','E_N','gas_partition_core_RMS']}})
        print(f'Full-resolution rerun {label}, trial={row["trial"]}, 148 points, 25 K',flush=True)
        frame=simulate(pool,int(row['trial']),parameters,label,25,directory/'checkpoints')
        write_csv(directory/'full_results.csv',frame)
        metrics=evaluate(frame); metrics.update(source_trial=int(row['trial']),label=label,resolution_K=25)
        write_csv(directory/'metrics.csv',pd.DataFrame([metrics]))
        assert metrics['numeric_valid'], f'{label} invalid at full resolution; retain artifacts for inspection'
        plot_files=create_plots(frame,parameters,label,directory/'plots')
        write_json(directory/'plot_manifest.json',plot_files)
    assert digest(SOURCE)==manifest['source_sha256'], 'Original notebook changed during run'
    report(table,selected,selection,manifest,OUT)
    write_json(OUT/'completion.json',{'total_trials':60,'complete':int(table.state.eq('COMPLETE').sum()),'failed':int(table.state.eq('FAIL').sum()),
        'screening_points':60*76,'full_rerun_candidates':3,'full_rerun_points':3*148,'source_unchanged':True,'source_sha256':digest(SOURCE),'stopped':True})
    print('COMPLETE: exactly 60 trials + 3 full-resolution candidates. Stopped.',flush=True)

def main():
    parser=argparse.ArgumentParser(); parser.add_argument('--rescore',action='store_true'); args=parser.parse_args()
    manifest=prepare(require_source_match=not args.rescore)
    if args.rescore: rescore(); return
    start=time.monotonic()
    with ProcessPoolExecutor(max_workers=WORKERS,mp_context=mp.get_context('fork')) as pool:
        study,table=screen(pool)
        finish(pool,table,manifest)
    print(f'Total invocation elapsed: {time.monotonic()-start:.1f}s',flush=True)
if __name__=='__main__': main()
