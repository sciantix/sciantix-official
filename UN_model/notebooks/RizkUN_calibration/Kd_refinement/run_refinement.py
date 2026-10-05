"""Explicitly authorized one-dimensional K_d refinement: three fixed levels only."""
from pathlib import Path
import os
os.environ.setdefault('MPLBACKEND', 'Agg')
os.environ.setdefault('MPLCONFIGDIR', '/tmp/rizkun_calibration_matplotlib')
os.environ.setdefault('OPENBLAS_NUM_THREADS', '1')
os.environ.setdefault('OMP_NUM_THREADS', '1')
import json, hashlib, time, traceback
from dataclasses import asdict, replace
from concurrent.futures import ProcessPoolExecutor, as_completed
import multiprocessing as mp
import numpy as np
import pandas as pd

KD_LEVELS = (1.5e5, 2.0e5, 2.5e5)
REFINEMENT_DIR = Path(__file__).resolve().parent
OAT_DIR = REFINEMENT_DIR.parent
NOTEBOOK_PATH = OAT_DIR.parent / 'RizkUN_calibration.ipynb'
SOURCE_CELL_INDEXES = (1, 2, 4, 5, 6)

def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def load_frozen_reference():
    notebook = json.loads(NOTEBOOK_PATH.read_text())
    reference_hashes = {}
    for index in SOURCE_CELL_INDEXES:
        source = ''.join(notebook['cells'][index]['source'])
        reference_hashes[str(index)] = hashlib.sha256(source.encode()).hexdigest()
        exec(compile(source, f'RizkUN_calibration:reference_cell{index}', 'exec'), globals())
    recorded = notebook['metadata']['reassessment']['reference_code_sha256']
    assert reference_hashes == recorded, 'Frozen reference code differs from recorded OAT physics'
    oat_manifest = json.loads((OAT_DIR / 'manifest.json').read_text())
    assert (DT_H, N_MODES, T_MIN, T_STEP, T_MAX_DIAGNOSTIC) == (1.0, 40, 900, 50, 1800)
    assert (XE_DIFFUSIVITY_MODE, VU_DIFFUSIVITY_MODE, DV_GB_MODE, RHO_MODE) == ('rizk2025_refit_plot','rizk2025_refit_full','rizk_legacy_1e6_Dv1','constant')
    assert EXPERIMENT_CASES == oat_manifest['cases']
    for key, value in oat_manifest['settings'].items():
        assert globals()[key] == value, f'Frozen setting changed: {key}'
    baseline = Candidate(**oat_manifest['baseline'])
    assert asdict(replace(make_candidate(), label='baseline')) == asdict(baseline)
    candidates = [replace(baseline, label=f'K_d_{int(level)}', K_d=level) for level in KD_LEVELS]
    for candidate in candidates:
        differences = [key for key,value in asdict(baseline).items() if key!='label' and asdict(candidate)[key]!=value]
        assert differences == ['K_d']
    manifest = {
        'method':'fixed one-dimensional K_d refinement; no optimizer or combined parameters',
        'K_d_levels':list(KD_LEVELS), 'new_candidate_design':[asdict(c) for c in candidates],
        'baseline':asdict(baseline), 'cases':EXPERIMENT_CASES, 'dt_h':DT_H, 'n_modes':N_MODES,
        'grid_K':temperature_grid(1800), 'settings':oat_manifest['settings'],
        'reference_code_sha256':reference_hashes, 'original_source_sha256':oat_manifest['source_sha256'],
        'comparison_only_reused_candidates':['baseline','K_d_low'],
        'new_requested_points':228, 'two_parameter_combinations_executed':0,
        'evaluation':'separate swelling/Rd<=1600/model gas; highT Rd and Nd diagnostic only; no combined score'
    }
    signature=hashlib.sha256(json.dumps(manifest,sort_keys=True).encode()).hexdigest()
    (REFINEMENT_DIR/'manifest.json').write_text(json.dumps(manifest,indent=2,ensure_ascii=False))
    pd.DataFrame(manifest['new_candidate_design']).to_csv(REFINEMENT_DIR/'candidate_design.csv',index=False)
    return candidates, manifest, signature

def case_task(candidate, case_id):
    RUN_CACHE.clear()
    start=time.monotonic(); rows=[]
    for T in temperature_grid(1800):
        row={'candidate':candidate.label, 'varied_parameter':'K_d', 'level':'refinement',
             'experiment_case':case_id, 'T':T, 'burnup':EXPERIMENT_CASES[case_id]['burnup'],
             'fission_rate_case':EXPERIMENT_CASES[case_id]['fission_rate'],
             'f_n':candidate.f_n,'K_d':candidate.K_d,'rho_d':candidate.rho_d,
             'Dv_dislocation_scale':candidate.Dv_dislocation_scale,'Dg_dislocation_scale':candidate.Dg_dislocation_scale,
             'N_gf0_factor':candidate.ngf0_factor,'ngf0_factor':candidate.ngf0_factor,
             'N_gf0_areal':NGF_AREAL_0*candidate.ngf0_factor,
             'N_gf0_vol':3*NGF_AREAL_0*candidate.ngf0_factor/(2*GRAIN_RADIUS),
             'DT_H':DT_H,'N_MODES':N_MODES,'rho_mode':RHO_MODE,'dv_gb_mode':DV_GB_MODE}
        try:
            result=run_model_point_case(T,case_id,candidate,DT_H,N_MODES,keep_history=False)
            row.update({key:value for key,value in result.items() if key not in ('hist','rates')})
            row['rates_json']=json.dumps(result['rates'],sort_keys=True)
            row.update(status='ok',error='')
        except Exception as exc:
            row.update(status='failed',error=f'{type(exc).__name__}: {exc}',traceback=traceback.format_exc())
        rows.append(row)
    return rows,time.monotonic()-start

def run_refinement():
    candidates,manifest,signature=load_frozen_reference()
    # Protect the completed OAT physical outputs, evaluations and original notebook.
    frozen_paths=sorted(p for p in OAT_DIR.rglob('*') if p.is_file() and REFINEMENT_DIR not in p.parents)
    original=OAT_DIR.parent/'finalRizkUN.ipynb'
    if original.exists():frozen_paths.append(original)
    frozen={str(p):sha256(p) for p in frozen_paths}
    checkpoint_dir=REFINEMENT_DIR/'checkpoints';checkpoint_dir.mkdir(exist_ok=True)
    pending=[];rows=[]
    for candidate in candidates:
        for case_id in EXPERIMENT_CASE_ORDER:
            path=checkpoint_dir/f'{candidate.label}_{case_id.replace(".","p")}.json'
            if path.exists():
                saved=json.loads(path.read_text())
                if saved.get('signature')==signature and len(saved.get('rows',[]))==19:
                    rows.extend(saved['rows']);continue
            pending.append((candidate,case_id,path))
    start=time.monotonic(); resumed=len(rows)
    workers=min(8,os.cpu_count() or 1)
    print(f'K_d only: {list(KD_LEVELS)}. 228 points; reused {resumed}; pending tasks {len(pending)}; workers={workers}',flush=True)
    with ProcessPoolExecutor(max_workers=workers,mp_context=mp.get_context('fork')) as pool:
        futures={pool.submit(case_task,c,case):(c,case,path) for c,case,path in pending}
        for future in as_completed(futures):
            candidate,case_id,path=futures[future]
            part,elapsed=future.result();rows.extend(part)
            path.write_text(json.dumps({'signature':signature,'rows':part},indent=2))
            pd.DataFrame(rows).to_csv(REFINEMENT_DIR/'all_Kd_refinement_runs.csv',index=False)
            print(f'{len(rows):3d}/228 {candidate.label} {case_id}: {elapsed:.1f}s; failed={sum(r["status"]!="ok" for r in part)}',flush=True)
    df=pd.DataFrame(rows)
    df['candidate_order']=df.candidate.map({c.label:i for i,c in enumerate(candidates)})
    df['case_order']=df.experiment_case.map({case:i for i,case in enumerate(EXPERIMENT_CASE_ORDER)})
    df=df.sort_values(['candidate_order','case_order','T']).drop(columns=['candidate_order','case_order']).reset_index(drop=True)
    assert len(df)==228 and not df.duplicated(['candidate','experiment_case','T']).any()
    assert df.groupby(['candidate','experiment_case']).size().eq(19).all()
    assert df.K_d.isin(KD_LEVELS).all()
    for key,value in [('f_n',5.5e-4),('rho_d',3e13),('Dv_dislocation_scale',10),('Dg_dislocation_scale',13),('N_gf0_factor',1),('DT_H',1),('N_MODES',40)]:
        assert df[key].eq(value).all(),key
    assert all(sha256(Path(p))==digest for p,digest in frozen.items())
    df.to_csv(REFINEMENT_DIR/'all_Kd_refinement_runs.csv',index=False)
    candidate_dir=REFINEMENT_DIR/'candidates';candidate_dir.mkdir(exist_ok=True)
    for name,sub in df.groupby('candidate',sort=False):sub.to_csv(candidate_dir/f'{name}.csv',index=False)
    record={'requested_new_points':228,'new_solver_points_this_execution':228-resumed,'resumed_points':resumed,'successful':int(df.status.eq('ok').sum()),
            'failed':int(df.status.ne('ok').sum()),'elapsed_seconds':time.monotonic()-start,
            'only_Kd_varied':True,'two_parameter_combinations_executed':0,'baseline_and_Kd1e5_reused_without_rerun':True,
            'existing_OAT_artifacts_unchanged':True,'protected_artifact_sha256':frozen,'reference_code_sha256':manifest['reference_code_sha256']}
    (REFINEMENT_DIR/'simulation_validation.json').write_text(json.dumps(record,indent=2))
    print(f'Completed: {record["successful"]}/228 successful; old artifacts unchanged; 0 combined configurations.',flush=True)
    return df

if __name__=='__main__':run_refinement()
