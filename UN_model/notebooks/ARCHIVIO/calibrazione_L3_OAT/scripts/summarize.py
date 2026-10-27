"""Export separate metrics for all completed candidates and OAT response maps."""
import json
import numpy as np
import pandas as pd
import model as m
from evaluate import BASE, PARAMS, GAS

ROOT=m.ROOT
def collect():
    rows=[]
    allmetrics=[]
    for p in [ROOT/'runs/baseline', *sorted((ROOT/'runs').glob('B*'))]:
        if not (p/'metrics.csv').exists():continue
        name='L3' if p.name=='baseline' else p.name
        params=BASE if name=='L3' else json.loads((p/'parameters.json').read_text())
        diag=json.loads((p/'diagnostics.json').read_text())
        metrics=pd.read_csv(p/'metrics.csv')
        allmetrics.append(metrics.assign(candidate=name))
        row=dict(candidate=name,**{k:params[k] for k in PARAMS},D_cal=diag['D_cal'])
        for _,r in metrics.iterrows():
            if r['case']=='AP3.2':continue
            for stat in ('RMSE','MAPE','bias'):
                row[f"{r['case']}_{r.observable}_{r.region}_{stat}"]=r[stat]
        temp=pd.read_csv(p/'temperature_summary.csv').set_index('case')
        row['ANP6_S1800']=temp.loc['ANP6','S_1800']
        row['ANP6_growth1600_1800']=temp.loc['ANP6','S_growth_1600_1800']
        for k in ['max_relative_mass_residual','negative_required','nonfinite_required','invalid_single_size_points']:
            row[k]=diag[k]
        rows.append(row)
    table=pd.DataFrame(rows)
    table.to_csv(ROOT/'reports/all_candidates_comparison.csv',index=False)
    pd.concat(allmetrics,ignore_index=True).to_csv(ROOT/'reports/all_metrics_long.csv',index=False)
    cols=['candidate',*PARAMS[:3],'Dg_dislocation_scale',
          'AP3.8_Sd_percent_all_RMSE','AP3.4_Sd_percent_all_RMSE','ANP6_Sd_percent_all_RMSE',
          'AP3.4_Rd_nm_low_le1500_RMSE','AP3.4_Rd_nm_high_gt1500_RMSE','ANP6_S1800','D_cal']
    print(table[cols].round(4).to_string(index=False))
    return table

def sensitivities():
    ref=pd.read_csv(ROOT/'runs/baseline/RizkUN_L3_unificato/L3_unificato_grid.csv').set_index(['experiment_case','T'])
    responses=[]
    outputs=['swelling_d_percent','Rd_nm','Nd',*GAS,'Rgf_nm','Ngf_areal']
    specs=[]
    for path in sorted((ROOT/'scripts').glob('batch_*.json')):
        specs.extend(s for s in json.loads(path.read_text()) if len(s['changes'])==1)
    for spec in specs:
        path=ROOT/'runs'/spec['id']/'results.csv'
        if not path.exists():continue
        key=next(iter(spec['changes']))
        dp=spec['changes'][key]/BASE[key]-1
        frame=pd.read_csv(path).set_index(['experiment_case','T'])
        for col in outputs:
            for (case,T),v in frame[col].items():
                base=ref.loc[(case,T),col]
                relative=(v/base-1) if abs(base)>1e-12 else np.nan
                responses.append(dict(candidate=spec['id'],parameter=key,parameter_fraction=dp,
                                      case=case,T=T,output=col,baseline=base,value=v,
                                      absolute_change=v-base,relative_change=relative,
                                      elasticity=relative/dp))
    if not responses:return
    response=pd.DataFrame(responses)
    response.to_csv(ROOT/'reports/OAT_response_by_temperature.csv',index=False)
    response['region']=np.where(response['T']<=1500,'900-1500K','1525-1800K')
    bands=response.groupby(['parameter','case','output','region']).agg(
        elasticity_mean=('elasticity','mean'),elasticity_min=('elasticity','min'),
        elasticity_max=('elasticity','max'),mean_signed_change=('absolute_change','mean'),
        max_absolute_change=('absolute_change',lambda x: abs(x).max())).reset_index()
    def direction(r):
        if pd.isna(r.elasticity_mean):return 'non identificabile (baseline nulla)'
        if r.elasticity_min < -.05 and r.elasticity_max > .05:return 'misto / dipendente da T e verso'
        a=abs(r.elasticity_mean)
        return ('~' if a<.05 else ('↑' if r.elasticity_mean>0 else '↓'))+(' debole' if a<.3 else ' medio' if a<1 else ' forte')
    bands['effect_on_increase']=bands.apply(direction,axis=1)
    bands.to_csv(ROOT/'reports/OAT_directional_table.csv',index=False)
    show=bands[(bands['case']=='AP3.4')]
    lines=['# Sensitività locali attorno a L3','',
           'Risposta per incremento del parametro. Elasticità = (Δy/y)/(Δp/p); media delle perturbazioni disponibili e delle temperature della banda. Debole: |E|<0.3; medio: 0.3–1; forte: ≥1; trascurabile: <0.05. I valori min/max e tutte le temperature sono nei CSV. Le medie non sono indici globali né punteggi di fit.','',
           '|Parametro|Output AP3.4|Banda|Effetto|Elasticità media|', '|---|---|---|---|---:|']
    for _,r in show.iterrows():lines.append(f"|{r.parameter}|{r.output}|{r.region}|{r.effect_on_increase}|{r.elasticity_mean:.4g}|")
    lines+=['','Per swelling di tutti i quattro casi vedere `OAT_directional_table.csv`. Dv: perturbazioni ±20% dopo il batch 3; rho: unilaterale -10%; N_gf,0: unilaterale +20% dopo il batch 3. Le righe vengono prodotte soltanto per simulazioni completate. Le elasticità FGR su baseline quasi nulla sono grandi anche per piccole variazioni assolute: consultare anche `max_absolute_change` (punti percentuali per il gas).']
    (ROOT/'reports/OAT_sensitivities.md').write_text('\n'.join(lines)+'\n')

if __name__=='__main__':
    collect()
    sensitivities()
