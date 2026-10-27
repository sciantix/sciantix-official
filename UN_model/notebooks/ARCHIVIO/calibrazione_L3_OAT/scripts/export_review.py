"""Package explicitly chosen review candidates; selection is not automated."""
import argparse
import json
import shutil
import numpy as np
import pandas as pd
import model as m
from evaluate import BASE, PARAMS, GAS, contact_sheet
from diagnostic_plots import plot

def main():
    parser=argparse.ArgumentParser()
    parser.add_argument('best',help='Manually reviewed candidate for the final comparison')
    parser.add_argument('conservative',help='Manually reviewed candidate with fewer parameter changes')
    args=parser.parse_args()
    root=m.ROOT
    names=['L3',args.best,args.conservative,'B3_K070_Dg0875','B3_K080_Dg0923','B2_K085_fn125']
    names=list(dict.fromkeys(names))
    table=pd.read_csv(root/'reports/all_candidates_comparison.csv')
    short=table.set_index('candidate').loc[names].reset_index()
    short.to_csv(root/'reports/best_candidates.csv',index=False)
    allmetrics=pd.read_csv(root/'reports/all_metrics_long.csv')
    allmetrics[allmetrics.candidate.isin(names)].to_csv(root/'reports/best_candidates_metrics_long.csv',index=False)
    ref=pd.read_csv(root/'runs/baseline/RizkUN_L3_unificato/L3_unificato_grid.csv')
    best=pd.read_csv(root/'runs'/args.best/'results.csv')
    contact_sheet(best,ref,args.best,root/'reports/FINAL_L3_vs_candidate.png')
    for name in (args.best,args.conservative):plot(name)
    changes=[]
    for _,r in table.iterrows():
        for key in PARAMS:
            ratio=r[key]/BASE[key]
            percentage=(ratio-1)*100
            a=abs(percentage)
            category=('invariato' if a<1e-10 else 'molto forte' if ratio>2 or ratio<.5 else
                      'forte' if a>=50-1e-10 else 'moderata' if a>=20-1e-10 else 'lieve')
            changes.append(dict(candidate=r.candidate,parameter=key,baseline=BASE[key],value=r[key],
                                ratio=ratio,percent_change=percentage,log10_ratio=np.log10(ratio),category=category))
    pd.DataFrame(changes).to_csv(root/'reports/parameter_distances.csv',index=False)
    delta=[]
    cols=['swelling_d_percent','Rd_nm','Nd','Rb_nm','Nb','Rgf_nm','Ngf_areal',*GAS,'psi_b','psi_d','p_d_over_eq','p_b_over_eq']
    a=ref.set_index(['experiment_case','T'])
    for name in names[1:]:
        b=pd.read_csv(root/'runs'/name/'results.csv').set_index(['experiment_case','T'])
        for col in cols:
            for index,value in b[col].items():
                base=a.loc[index,col]
                delta.append(dict(candidate=name,case=index[0],T=index[1],output=col,baseline=base,
                                  candidate_value=value,absolute_change=value-base,
                                  relative_change=(value/base-1) if abs(base)>1e-14 else np.nan))
    pd.DataFrame(delta).to_csv(root/'reports/shortlist_all_output_changes.csv',index=False)
    lines=['# Candidati da esaminare','',
           f'Candidato per il confronto finale: **{args.best}**. Alternativa più conservativa: **{args.conservative}**. Scelta esplicita dopo il confronto dei batch, senza score aggregato. I CSV lunghi conservano tutte le regioni, MAPE, bias e il controllo AP3.2.','',
           '|Candidato|f_n|K_d|rho_d|Dv_d|Dg_d|Ngf0|D_cal|', '|---|---:|---:|---:|---:|---:|---:|---:|']
    for _,r in short.iterrows():
        lines.append(f'|{r.candidate}|{r.f_n:.4g}|{r.K_d:.5g}|{r.rho_d:.4g}|{r.Dv_dislocation_scale:g}|{r.Dg_dislocation_scale:g}|{r.NGF_AREAL_0:.3g}|{r.D_cal:.5f}|')
    lines+=['','RMSE swelling in punti percentuali; MAPE in %. Errori raggio in nm.','',
            '|Candidato|AP3.8 RMSE / MAPE|AP3.4 RMSE / MAPE|ANP6 RMSE / MAPE|R_d ≤1500 RMSE|R_d >1500 RMSE|',
            '|---|---:|---:|---:|---:|---:|']
    for _,r in short.iterrows():
        values=[f"{r[f'{c}_Sd_percent_all_RMSE']:.4f} / {r[f'{c}_Sd_percent_all_MAPE']:.2f}" for c in ['AP3.8','AP3.4','ANP6']]
        lines.append('|'+r.candidate+'|'+'|'.join(values)+f"|{r['AP3.4_Rd_nm_low_le1500_RMSE']:.3f}|{r['AP3.4_Rd_nm_high_gt1500_RMSE']:.3f}|")
    lines+=['','|Candidato|S_ANP6(1800) [%]|Incremento S_ANP6 1600→1800 [pp]|', '|---|---:|---:|']
    for _,r in short.iterrows():lines.append(f'|{r.candidate}|{r.ANP6_S1800:.4f}|{r.ANP6_growth1600_1800:.4f}|')
    (root/'reports/best_candidates.md').write_text('\n'.join(lines)+'\n')
    (root/'reports/review_selection.json').write_text(json.dumps(dict(review_candidate=args.best,
         conservative_alternative=args.conservative,shortlist=names),indent=2))

if __name__=='__main__':main()
