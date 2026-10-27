"""Single-size consistency diagnostic; never used for candidate selection."""
import numpy as np
import pandas as pd
import model as m

sw=m.experimental_swelling_points_for_case('AP3.4')
baseline=pd.read_csv(m.ROOT/'runs/baseline/RizkUN_L3_unificato/L3_unificato_grid.csv')
baseline=baseline[baseline.experiment_case=='AP3.4'].sort_values('T')
rows=[]
for rd in m.EXP_RD_T_13:
    s=min(sw,key=lambda p:abs(p['T']-rd['T']))
    assert abs(s['T']-rd['T']) < 5
    implied=(s['swelling']/100)/(4*np.pi/3*(rd['R_nm']*1e-9)**3)
    nd=float(np.interp(rd['T'],baseline['T'],baseline.Nd))
    rows.append(dict(T_Rd=rd['T'],T_Sd=s['T'],Sd_observed_percent=s['swelling'],
                     Rd_observed_nm=rd['R_nm'],Nd_single_size_implied=implied,
                     Nd_baseline=nd,implied_to_baseline_ratio=implied/nd))
pd.DataFrame(rows).to_csv(m.ROOT/'reports/experimental_single_size_consistency.csv',index=False)
print(pd.DataFrame(rows).to_string(index=False))
