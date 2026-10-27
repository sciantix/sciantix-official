"""Verify provenance, fixed physics, coverage, conservation, and artifact completeness."""
import hashlib
import json
from pathlib import Path
import numpy as np
import pandas as pd
import model as m
from evaluate import BASE, PARAMS

root=m.ROOT
parent=root.parent
hashes=json.loads((root/'reports/original_sha256.json').read_text())
for name,digest in hashes.items():
    assert hashlib.sha256((parent/name).read_bytes()).hexdigest()==digest,name
    assert hashlib.sha256((root/'runs/baseline/originals'/name).read_bytes()).hexdigest()==digest,name
nb=json.loads(next((root/'runs/baseline/originals').glob('*.ipynb')).read_text())
for i in (1,2,6,8,10,16,20):
    assert (root/f'scripts/cell_{i:02d}.py').read_text()==''.join(nb['cells'][i]['source']),i
assert json.loads((root/'runs/baseline/reproduction.json').read_text())['all_columns_match']
base_config=json.loads((root/'runs/baseline/configuration.json').read_text())
specs=[]
batches=sorted((root/'scripts').glob('batch_*.json'))
assert len(batches)<=5
for path in batches:
    batch=json.loads(path.read_text());assert 4<=len(batch)<=8
    specs+=batch
assert len({s['id'] for s in specs})==len(specs)
records=[]
for s in specs:
    p=root/'runs'/s['id']
    for file in ['parameters.json','configuration.json','results.csv','metrics.csv','residuals.csv',
                 'diagnostics.json','history_audit.csv','temperature_summary.csv']:
        assert (p/file).is_file(),(s['id'],file)
    assert (root/'plots'/f"{s['id']}_contact_sheet.png").is_file()
    params=json.loads((p/'parameters.json').read_text())
    assert {k:v for k,v in params.items() if v!=BASE[k]}==s['changes'],s['id']
    assert set(s['changes'])<=set(PARAMS)
    config=json.loads((p/'configuration.json').read_text())
    for key,value in base_config.items():
        if key not in ('MANUAL_PARAMS','NGF_AREAL_0'):assert config[key]==value,(s['id'],key)
    df=pd.read_csv(p/'results.csv',float_precision='round_trip')
    assert len(df)==148
    for case in m.EXPERIMENT_CASE_ORDER:
        d=df[df.experiment_case==case]
        assert len(d)==37
        assert np.array_equal(d['T'].to_numpy(),np.arange(900,1801,25))
        assert (d.fission_rate_case==m.EXPERIMENT_CASES[case]['fission_rate']).all()
        assert (d.burnup==m.EXPERIMENT_CASES[case]['burnup']).all()
    diag=json.loads((p/'diagnostics.json').read_text())
    for key in ['negative_required','nonfinite_required','invalid_single_size_points',
                'negative_exchange_steps','nonfinite_exchange_steps','negative_release_increments']:
        assert diag[key]==0,(s['id'],key,diag[key])
    assert diag['max_history_relative_mass_residual']<1e-10
    assert diag['max_history_raw_capture']<1
    assert diag['max_single_size_identity_relative']<1e-12
    audit=pd.read_csv(p/'history_audit.csv')
    assert len(audit)==148
    assert (audit.max_history_zeta_guard==0).all()
    assert len(list(p.glob('history_*.npz')))==12
    records.append(dict(candidate=s['id'],rows=len(df),mass_residual=diag['max_history_relative_mass_residual'],
                        max_psi_d=diag['max_psi_d'],max_raw_capture=diag['max_history_raw_capture']))
pd.DataFrame(records).to_csv(root/'reports/verification_by_candidate.csv',index=False)
report=dict(original_hashes_unchanged=True,source_cells_verbatim=True,
            fixed_physics_and_numerics=True,case_specific_fission_rates_unchanged=True,
            batches=len(batches),candidates=len(specs),candidate_material_points=148*len(specs),
            all_passed=True,max_history_relative_mass_residual=max(r['mass_residual'] for r in records))
(root/'reports/verification.json').write_text(json.dumps(report,indent=2))
print(json.dumps(report,indent=2))
