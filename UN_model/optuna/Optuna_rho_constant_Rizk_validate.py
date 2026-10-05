"""Read-only scientific audit plus report regeneration; never calls the solver."""
import json
import numpy as np
import pandas as pd
import optuna
import Optuna_rho_constant_Rizk as campaign
from Optuna_rho_constant_Rizk_metrics import OBJECTIVES
from Optuna_rho_constant_Rizk_plots import report
from pathlib import Path
out=campaign.OUT; manifest=json.loads((out/'manifest.json').read_text())
table=pd.read_csv(out/'trial_metrics.csv'); curves=pd.read_csv(out/'all_trial_curves.csv'); offline=pd.read_csv(out/'offline_rescored_metrics.csv')
study=optuna.load_study(study_name=campaign.NAME,storage=f'sqlite:///{out/"study.db"}')
assert len(study.trials)==60 and len(table)==60 and len(curves)==4560
assert study.trials[0].params==campaign.BASELINE and table.iloc[0].trial_label=='CURRENT_BASELINE'
assert sum(t.state.name=='COMPLETE' for t in study.trials)==59 and sum(t.state.name=='FAIL' for t in study.trials)==1
assert not curves.duplicated(['trial','case','T']).any()
assert curves.groupby(['trial','case']).size().eq(19).all()
assert set(curves.case)=={'AP3.2','AP3.8','AP3.4','ANP6'} and set(curves['T'])==set(range(900,1801,50))
required=['trial','case','T','burnup','fission_rate',*campaign.RANGES,'swelling_d','swelling_bulk','swelling_gf','Rd','Nd','Rb','Nb','Rgf','Ngf','matrix_gas','bulk_gas','dislocation_gas','grainface_gas','FGR','p_d_over_eq','p_b_over_eq','p_gf_over_eq']
assert set(required).issubset(curves.columns)
assert (curves.DT_H==1).all() and (curves.N_MODES==40).all() and (curves.rho_d_eff==curves.rho_d).all()
for key,(lo,hi) in campaign.RANGES.items(): assert curves[key].between(lo,hi).all()
merged=table[table.state.eq('COMPLETE')].merge(offline,on='trial',suffixes=('_original','_offline'))
errors={o:float((merged[o+'_original']-merged[o+'_offline']).abs().max()) for o in OBJECTIVES}
assert all(v<1e-12 for v in errors.values())
assert set(table.loc[table.pareto,'trial'])==set(offline.loc[offline.pareto,'trial'])=={t.number for t in study.best_trials}
assert campaign.digest(campaign.SOURCE)==manifest['source_sha256']
source_grid=campaign.SOURCE.parent/'RizkUN/RizkUN_grid.csv'; equivalence=json.loads((out/'baseline_equivalence_validation.json').read_text()); assert campaign.digest(source_grid)==equivalence['reference_sha256']
plots={}; full_validation={}
for letter in 'ABC':
 directory=out/'final_candidates'/f'CANDIDATE_{letter}'; full=pd.read_csv(directory/'full_results.csv'); metrics=pd.read_csv(directory/'metrics.csv').iloc[0]; parameters=json.loads((directory/'parameters.json').read_text())
 assert len(full)==148 and not full.duplicated(['case','T']).any() and full.groupby('case').size().eq(37).all()
 assert set(full['T'])==set(range(900,1801,25)) and metrics.numeric_valid
 for key,value in parameters['parameters'].items(): assert np.allclose(full[key],value,rtol=1e-14,atol=0), key
 files=list((directory/'plots').glob('*.png')); assert len(files)==32 and all(p.stat().st_size>10000 for p in files)
 pm=json.loads((directory/'plot_manifest.json').read_text()); assert pm['standard_count']==27 and pm['supplemental_count']==5
 plots[letter]=len(files); full_validation[letter]={'trial':int(metrics.source_trial),'points':148,'Rgf_max_um':float(full.Rgf.max()*1e6),'gas_balance_max_abs_pp':float(metrics.gas_balance_max_abs_pp)}
selection=json.loads((out/'selection.json').read_text()); distances=np.array(selection['normalized_log_parameter_distances']); assert distances[np.triu_indices(3,1)].min()>.7
report(table,pd.read_csv(out/'selected_candidates.csv'),selection,manifest,out)
record={'total_trials':60,'complete':59,'rejected_gross_nonphysical':1,'solver_failures':0,'screening_points':4560,'full_points':444,'standard_plus_supplemental_plots':plots,'offline_objective_max_absolute_error':errors,'offline_pareto_matches_sqlite':True,'source_notebook_unchanged':True,'existing_source_grid_unchanged':True,'full_candidates':full_validation,'python_sha256':{p.name:campaign.digest(p) for p in campaign.ROOT.glob('Optuna_rho_constant_Rizk*.py')}}
(out/'final_validation.json').write_text(json.dumps(record,indent=2)); print(json.dumps(record,indent=2))
