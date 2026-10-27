"""Audit original baseline time histories at representative temperatures."""
import json
import numpy as np
import pandas as pd
import model as m

out=m.ROOT/'runs/baseline'
ref=pd.read_csv(out/'RizkUN_L3_unificato/L3_unificato_grid.csv')
exchange=m.sciantix_3x3_exchange_step
counts={}
def monitored_exchange(*args,**kwargs):
    v=exchange(*args,**kwargs)
    counts['negative_exchange_steps']+=int(any(x<0 for x in v))
    counts['nonfinite_exchange_steps']+=int(any(not np.isfinite(x) for x in v))
    return v
m.sciantix_3x3_exchange_step=monitored_exchange
audits=[]
for case in m.EXPERIMENT_CASE_ORDER:
    for T in (900,1200,1600,1800):
        counts.update(negative_exchange_steps=0,nonfinite_exchange_steps=0)
        row=m.run_model_point_case(T,case,m.make_candidate(),keep_history=True)
        h=pd.DataFrame(row['hist'])
        np.savez_compressed(out/f"history_{case.replace('.','p')}_{T}K.npz",**{k:np.asarray(v) for k,v in row['hist'].items()})
        mask=h.generated>0
        residual=(h.c+h.mb+h.md+h.q_gf+h.q_rel-h.generated)[mask]/h.generated[mask]
        rr=ref[(ref.experiment_case==case)&(ref['T']==T)].iloc[0]
        matched=all(np.isclose(row[k],rr[k],rtol=1e-12,atol=0,equal_nan=True)
                    for k in ref.select_dtypes(include=np.number).columns)
        audits.append(dict(case=case,T=T,history_matches_original_csv=matched,
                           max_history_relative_mass_residual=float(abs(residual).max()),
                           min_history_gas=float(h[['c','mb','md','q_gf','q_rel']].min().min()),
                           max_raw_capture=float(h.cap_raw_step.max()),
                           max_Fc=float(h.Fc_gf.max()),**counts))
audit=pd.DataFrame(audits)
audit.to_csv(out/'history_audit.csv',index=False)
assert audit.history_matches_original_csv.all()
print(audit.to_string(index=False),flush=True)
