from pathlib import Path
import json, hashlib, io, contextlib
import numpy as np
import pandas as pd
import nbformat

out=Path(__file__).resolve().parent;parent=out.parent
manifest=json.loads((out/'manifest.json').read_text());simulation=json.loads((out/'simulation_validation.json').read_text())
new=pd.read_csv(out/'all_Kd_refinement_runs.csv');summary=pd.read_csv(out/'comparison_summary.csv');runs=pd.read_csv(out/'comparison_runs.csv')
points=pd.read_csv(out/'comparison_points.csv');guard=pd.read_csv(out/'ordering_diagnostics.csv')
assert len(new)==228 and new.status.eq('ok').all()
assert new.groupby(['candidate','experiment_case']).size().eq(19).all()
assert set(new.K_d)=={1.5e5,2e5,2.5e5}
assert set(new['T'])==set(range(900,1801,50))
for k,v in [('f_n',5.5e-4),('rho_d',3e13),('Dv_dislocation_scale',10),('Dg_dislocation_scale',13),('N_gf0_factor',1),('DT_H',1),('N_MODES',40)]:assert new[k].eq(v).all(),k
for candidate in manifest['new_candidate_design']:
 changes=[k for k,v in manifest['baseline'].items() if k!='label' and candidate[k]!=v]
 assert changes==['K_d']
for case,sub in new.groupby('experiment_case'):
 assert np.allclose(sub.fission_rate_case,manifest['cases'][case]['fission_rate'],rtol=1e-15,atol=0)
 assert sub.burnup.eq(manifest['cases'][case]['burnup']).all()
assert len(runs)==380 and runs.run_origin.eq('reused_OAT').sum()==152
assert len(summary)==5 and len(pd.read_csv(out/'candidate_summary.csv'))==3
assert not runs.nonfinite_or_negative_final_state.any()
assert runs.gas_balance_error_pp.abs().max()<1e-10
for candidate in summary.candidate:
 q=points[points.candidate.eq(candidate)&points.metric.eq('R_d')&points.included_in_experimental_selection]
 assert len(q)==5 and q.T_K.le(1600).all()
assert not points[points.metric.eq('N_d')].included_in_experimental_selection.any()
assert not points[points.metric.eq('R_d')&points.T_K.gt(1600)].included_in_experimental_selection.any()
assert len(list((out/'plots').glob('*.png')))==8
assert not guard[guard.candidate.isin(['K_d_150000','K_d_200000','K_d_250000'])&guard.ordering.eq('Ngf_vol<Nd<Nb')].n_violations.any()
# Preserve all original scientific outputs. Reports/scripts may reflect the current stage.
science_names=['all_OAT_runs.csv','comparison_points.csv','model_benchmark_summary.csv','candidate_summary.csv','manifest.json','candidate_design.csv','ordering_diagnostics.csv','pressure_diagnostics.csv','baseline_reproduction_check.csv','pareto_comparison.csv']
protected=[]
for path,digest in simulation['protected_artifact_sha256'].items():
 p=Path(path)
 if p.name in science_names or any(part in ['checkpoints','candidates','plots'] for part in p.relative_to(parent).parts) if p.is_relative_to(parent) else p.name=='finalRizkUN.ipynb':
  assert hashlib.sha256(p.read_bytes()).hexdigest()==digest,p
  protected.append(path)
book_path=parent.parent/'RizkUN_calibration.ipynb';book=nbformat.read(book_path,as_version=4);nbformat.validate(book)
for index in [1,2,4,5,6]:
 assert hashlib.sha256(book.cells[index].source.encode()).hexdigest()==manifest['reference_code_sha256'][str(index)]
for cell in book.cells:
 if cell.cell_type=='code':compile(cell.source,'RizkUN_calibration.ipynb','exec')
# Exercise current default data-loading cells with model calls trapped.
def forbidden(*args,**kwargs):raise AssertionError('Unexpected model call during default loading')
namespace={'__name__':'verification'}
for name in ['solve_UN','run_model_point','run_model_point_case','simulate_grid','run_oat_screening','run_refinement']:namespace[name]=forbidden
capture=io.StringIO()
with contextlib.redirect_stdout(capture):
 for index in [7,8,9,10,13]:exec(book.cells[index].source,namespace)
assert len(namespace['kd_comparison'])==5
assert len(pd.read_csv(parent/'next_round_proposals.csv'))==0 and json.loads((parent/'next_round_proposals.json').read_text())==[]
result={'new_model_points':simulation['new_solver_points_this_execution'],'successful_new_points':228,'only_Kd_changed':True,'reused_OAT_control_points':152,
        'comparison_candidates':5,'experimental_Rd_selection_points_per_candidate':5,'highT_Rd_and_Nd_no_selection_weight':True,
        'frozen_physics_reference_cells_unchanged':True,'original_scientific_artifacts_unchanged':True,'protected_scientific_artifact_count':len(protected),
        'new_combinations_proposed':0,'new_combinations_executed':0,'previous_unapproved_pairs_suspended':True,
        'notebook_default_loading_no_model_calls':True,'new_figures_from_saved_data':8,'max_abs_gas_balance_error_pp':float(runs.gas_balance_error_pp.abs().max())}
(out/'validation.json').write_text(json.dumps(result,indent=2));print(json.dumps(result,indent=2))
