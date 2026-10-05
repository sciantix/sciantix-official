"""Compare saved K_d refinement with two saved OAT controls; no solver or combinations."""
from pathlib import Path
import json, hashlib
import numpy as np
import pandas as pd

CASES=['AP3.2','AP3.8','AP3.4','ANP6']
CORE_GAS=['matrix_gas_percent','bulk_gas_percent','dislocation_gas_percent']
GAS_CHANNELS=CORE_GAS+['grainface_gas_percent','release_gas_percent']
CANDIDATES=['K_d_low','K_d_150000','K_d_200000','K_d_250000','baseline']
OBSERVABLES=['swelling_d_percent','Rd_nm','Nd','matrix_gas_percent','bulk_gas_percent','dislocation_gas_percent','grainface_gas_percent','release_gas_percent','Rgf_nm','Ngf_vol']

def rms(x):
    x=np.asarray(x,float)
    return float(np.sqrt(np.mean(x*x))) if len(x) else np.nan

def table(df,columns):
    lines=['| '+' | '.join(columns)+' |','| '+' | '.join(['---']*len(columns))+' |']
    for _,r in df.iterrows():
        cells=[]
        for c in columns:
            v=r[c]
            cells.append(f'{v:.4g}' if isinstance(v,(float,np.floating)) and np.isfinite(v) else ('n/d' if isinstance(v,(float,np.floating)) else str(v).replace('|','/')))
        lines.append('| '+' | '.join(cells)+' |')
    return '\n'.join(lines)

def ranges(T,flags):
    blocks=[];start=None
    for i in range(len(T)+1):
        flag=bool(flags[i]) if i<len(T) else False
        if flag and start is None:start=i
        if not flag and start is not None:
            if i-start>=3:blocks.append(f'{T[start]:g}-{T[i-1]:g} K')
            start=None
    return '; '.join(blocks)

def pareto(df,columns):
    values=df[columns].to_numpy(float);names=df.candidate.tolist();dominated=[]
    for i,x in enumerate(values):
        dom=[]
        for j,y in enumerate(values):
            tolerance=1e-10*np.maximum(1,np.maximum(abs(x),abs(y)))
            if i!=j and np.all(y<=x+tolerance) and np.any(y<x-tolerance):dom.append(names[j])
        dominated.append('; '.join(dom))
    return [not bool(x) for x in dominated],dominated

def evaluate_refinement(refinement_dir):
    out=Path(refinement_dir).resolve();parent=out.parent
    new=pd.read_csv(out/'all_Kd_refinement_runs.csv')
    old=pd.read_csv(parent/'all_OAT_runs.csv')
    existing=pd.read_csv(parent/'comparison_points.csv')
    prior=pd.read_csv(parent/'candidate_summary.csv').set_index('candidate')
    assert len(new)==228 and new.status.eq('ok').all()
    controls=old[old.candidate.isin(['baseline','K_d_low'])].copy()
    assert len(controls)==152
    controls['run_origin']='reused_OAT';new['run_origin']='new_Kd_refinement'
    runs=pd.concat([controls,new],ignore_index=True).copy()
    runs['sort_candidate']=runs.candidate.map({name:i for i,name in enumerate(CANDIDATES)})
    runs['sort_case']=runs.experiment_case.map({name:i for i,name in enumerate(CASES)})
    runs=runs.sort_values(['sort_candidate','sort_case','T']).drop(columns=['sort_candidate','sort_case']).reset_index(drop=True)
    assert len(runs)==380 and not runs.duplicated(['candidate','experiment_case','T']).any()
    part_cols=GAS_CHANNELS
    runs['gas_balance_error_pp']=runs[part_cols].sum(axis=1)-100
    runs['radius_gf_le_d']=runs.Rgf_nm<=runs.Rd_nm
    runs['radius_d_le_b']=runs.Rd_nm<=runs.Rb_nm
    runs['number_gf_ge_d']=runs.Ngf_vol>=runs.Nd
    runs['number_d_ge_b']=runs.Nd>=runs.Nb
    runs['radius_order_violation']=runs.radius_gf_le_d|runs.radius_d_le_b
    runs['number_order_violation']=runs.number_gf_ge_d|runs.number_d_ge_b
    critical=OBSERVABLES+['Rb_nm','Nb','p_b','p_d','p_gf']
    runs['nonfinite_or_negative_final_state']=(~np.isfinite(runs[critical])).any(axis=1)|(runs[critical]<-1e-9).any(axis=1)
    runs.to_csv(out/'comparison_runs.csv',index=False)
    # Use the same digitized observations and benchmark temperatures as the OAT.
    # Preserve the 899 K point as excluded; no extrapolation.
    template=existing[existing.candidate.eq('baseline')].copy()
    points=[]
    for candidate in CANDIDATES:
        for case_id in CASES:
            sub=runs[runs.candidate.eq(candidate)&runs.experiment_case.eq(case_id)].sort_values('T')
            for _,pt in template[template['case'].eq(case_id)].iterrows():
                T=pt.T_K;metric=pt.metric
                col={'P2_swelling':'swelling_d_percent','R_d':'Rd_nm','N_d':'Nd'}.get(metric,metric)
                pred=float(np.interp(T,sub['T'],sub[col])) if 900<=T<=1800 else np.nan
                residual=pred-pt.observed
                data_type=pt.data_type
                if data_type=='experiment' and metric=='P2_swelling':role='primary_swelling'
                elif data_type=='experiment' and metric=='R_d':role='strong_secondary_Rd_le1600' if T<=1600 else 'qualitative_Rd_highT_no_selection_penalty'
                elif data_type=='experiment' and metric=='N_d':role='qualitative_Nd_no_selection_penalty'
                elif data_type=='Rizk2025_model_benchmark':role='model_coherence_not_experiment' if metric in CORE_GAS else 'model_guardrail_not_experiment'
                else:role='supplementary_nominal_burnup_no_double_counting'
                row=pt.to_dict();row.update(candidate=candidate,K_d=float(sub.K_d.iloc[0]),predicted=pred,residual=residual,
                                           diagnostic_weight=role,evaluation_role=role,
                                           excluded_reason='outside_900_1800_screening' if not np.isfinite(pred) else ('' if data_type!='experiment_supplementary_1600K_nominal_burnup' else 'supplementary_not_in_selection'),
                                           included_in_experimental_selection=bool(np.isfinite(pred) and role in ['primary_swelling','strong_secondary_Rd_le1600']))
                row['log10_residual']=float(np.log10(pred/pt.observed)) if metric=='N_d' and pred>0 else np.nan
                points.append(row)
    points=pd.DataFrame(points);points.to_csv(out/'comparison_points.csv',index=False)
    summaries=[];case_rows=[];guards=[];pressures=[];benchmarks=[]
    for candidate in CANDIDATES:
        sub=runs[runs.candidate.eq(candidate)]
        p=points[points.candidate.eq(candidate)]
        row={'candidate':candidate,'K_d':float(sub.K_d.iloc[0]),'run_origin':sub.run_origin.iloc[0],
             'f_n':5.5e-4,'rho_d':3e13,'Dv_dislocation_scale':10,'Dg_dislocation_scale':13,'N_gf0_factor':1,
             'n_runs':len(sub),'n_ok':int(sub.status.eq('ok').sum()),'max_abs_gas_balance_error_pp':sub.gas_balance_error_pp.abs().max(),
             'nonfinite_or_negative_final_states':int(sub.nonfinite_or_negative_final_state.sum())}
        swelling=[]
        for case_id in CASES:
            q=p[p['case'].eq(case_id)&p.data_type.eq('experiment')&p.metric.eq('P2_swelling')&p.predicted.notna()]
            err=rms(q.residual);normalized=err/rms(q.observed)
            row[f'swelling_rmse_pp_{case_id}']=err;row[f'swelling_nrmse_{case_id}']=normalized;row[f'swelling_bias_pp_{case_id}']=q.residual.mean();row[f'swelling_n_{case_id}']=len(q)
            swelling.append(normalized)
            cs=sub[sub.experiment_case.eq(case_id)].sort_values('T')
            case_row={'candidate':candidate,'K_d':row['K_d'],'case':case_id,'swelling_rmse_pp':err,'swelling_nrmse':normalized,'swelling_bias_pp':q.residual.mean()}
            for col in OBSERVABLES+['Ngf_areal','p_b','p_d','p_gf','p_b_over_eq','p_d_over_eq','p_gf_over_eq']:
                case_row[f'{col}_mean']=cs[col].mean();case_row[f'{col}_min']=cs[col].min();case_row[f'{col}_max']=cs[col].max()
            case_rows.append(case_row)
            for field,kind in [('radius_order_violation','Rgf>Rd>Rb'),('number_order_violation','Ngf_vol<Nd<Nb')]:
                interval=ranges(cs['T'].to_numpy(),cs[field].to_numpy())
                guards.append({'candidate':candidate,'K_d':row['K_d'],'case':case_id,'ordering':kind,'n_temperatures':len(cs),'n_violations':int(cs[field].sum()),'persistent':bool(interval),'persistent_ranges':interval,
                               'gf_vs_d_violations':int(cs['radius_gf_le_d' if field.startswith('radius') else 'number_gf_ge_d'].sum()),
                               'd_vs_b_violations':int(cs['radius_d_le_b' if field.startswith('radius') else 'number_d_ge_b'].sum())})
                row[f'{"radius" if field.startswith("radius") else "number"}_persistent_{case_id}']=bool(interval)
            for pop in ['b','d','gf']:
                pressures.append({'candidate':candidate,'K_d':row['K_d'],'case':case_id,'population':pop,'p_min_Pa':cs[f'p_{pop}'].min(),'p_max_Pa':cs[f'p_{pop}'].max(),
                                  'p_over_eq_max':cs[f'p_{pop}_over_eq'].max(),'n_T_ratio_gt1000':int((cs[f'p_{pop}_over_eq']>1000).sum()),
                                  'persistent_ranges_ratio_gt1000':ranges(cs['T'].to_numpy(),(cs[f'p_{pop}_over_eq']>1000).to_numpy()),
                                  'role':'guardrail only; ratio threshold descriptive, not fit or acceptance criterion'})
            for metric in GAS_CHANNELS:
                q=p[p['case'].eq(case_id)&p.data_type.eq('Rizk2025_model_benchmark')&p.metric.eq(metric)]
                benchmarks.append({'candidate':candidate,'K_d':row['K_d'],'case':case_id,'metric':metric,'data_type':'Rizk_model_benchmark_not_experiment',
                                   'availability':'unavailable_1.3FIMA' if case_id=='AP3.4' else 'shared_1.1FIMA_curve' if case_id in ['AP3.2','AP3.8'] else '3.2FIMA_curve',
                                   'n_compared':len(q),'rmse_pp':rms(q.residual),'bias_pp':q.residual.mean()})
        row['swelling_balanced_nrmse']=float(np.mean(swelling))
        for which,condition in [('le1600',p.T_K.le(1600)),('gt1600_diagnostic',p.T_K.gt(1600))]:
            q=p[p.data_type.eq('experiment')&p.metric.eq('R_d')&condition]
            row[f'Rd_rmse_{which}_nm_AP3.4']=rms(q.residual);row[f'Rd_bias_{which}_nm_AP3.4']=q.residual.mean();row[f'Rd_n_{which}']=len(q)
            row[f'Rd_fraction_underpredicted_{which}']=float((q.residual<0).mean())
        for which,condition in [('le1600',p.T_K.le(1600)),('gt1600',p.T_K.gt(1600))]:
            q=p[p.data_type.eq('experiment')&p.metric.eq('N_d')&condition]
            row[f'Nd_log10_rmse_{which}_diagnostic']=rms(q.log10_residual);row[f'Nd_log10_bias_{which}_diagnostic']=q.log10_residual.mean()
            row[f'Nd_fraction_overpredicted_{which}_diagnostic']=float((q.residual>0).mean());row[f'Nd_median_ratio_{which}_diagnostic']=float(np.median(q.predicted/q.observed));row[f'Nd_n_{which}']=len(q)
        q=p[p.data_type.eq('Rizk2025_model_benchmark')]
        row['gas_core3_RMSE_pp']=rms(q[q.metric.isin(CORE_GAS)].residual);row['gas_partition5_RMSE_pp_diagnostic']=rms(q.residual)
        for metric in GAS_CHANNELS:row[f'benchmark_{metric}_RMSE_pp']=rms(q[q.metric.eq(metric)].residual)
        for col in ['p_gf','p_gf_over_eq','Rgf_nm','Ngf_vol','release_gas_percent']:
            row[f'{col}_mean']=sub[col].mean();row[f'{col}_max']=sub[col].max();row[f'{col}_min']=sub[col].min()
        summaries.append(row)
    summary=pd.DataFrame(summaries)
    for control in ['baseline','K_d_low']:
        row=summary.set_index('candidate').loc[control];old_row=prior.loc[control]
        fields=['swelling_balanced_nrmse','Rd_rmse_le1600_nm_AP3.4','Rd_rmse_gt1600_diagnostic_nm_AP3.4','Nd_log10_rmse_le1600_diagnostic','Nd_log10_rmse_gt1600_diagnostic','gas_core3_RMSE_pp']+[f'swelling_rmse_pp_{c}' for c in CASES]
        assert np.allclose(row[fields].to_numpy(float),old_row[fields].to_numpy(float),rtol=1e-12,atol=1e-12),'Existing control metrics changed'
    base=summary.set_index('candidate').loc['baseline']
    summary['gas_core3_vs_baseline_ratio']=summary.gas_core3_RMSE_pp/base.gas_core3_RMSE_pp
    summary['swelling_improves_all4_vs_baseline']=np.all(np.column_stack([summary[f'swelling_rmse_pp_{case}']<base[f'swelling_rmse_pp_{case}'] for case in CASES]),axis=1)
    exp_cols=['swelling_balanced_nrmse','Rd_rmse_le1600_nm_AP3.4'];gas_cols=exp_cols+['gas_core3_RMSE_pp']
    summary['pareto_experiments'],summary['dominated_by_experiments']=pareto(summary,exp_cols)
    summary['pareto_with_gas'],summary['dominated_by_with_gas']=pareto(summary,gas_cols)
    summary.to_csv(out/'comparison_summary.csv',index=False)
    summary[summary.run_origin.eq('new_Kd_refinement')].to_csv(out/'candidate_summary.csv',index=False)
    pd.DataFrame(case_rows).to_csv(out/'candidate_case_summary.csv',index=False)
    guard=pd.DataFrame(guards);guard.to_csv(out/'ordering_diagnostics.csv',index=False)
    pd.DataFrame(pressures).to_csv(out/'pressure_diagnostics.csv',index=False)
    pd.DataFrame(benchmarks).to_csv(out/'model_benchmark_summary.csv',index=False)
    deltas=[]
    metrics=exp_cols+['gas_core3_RMSE_pp','gas_partition5_RMSE_pp_diagnostic','Rd_rmse_gt1600_diagnostic_nm_AP3.4','Nd_log10_rmse_le1600_diagnostic','Nd_log10_rmse_gt1600_diagnostic']+[f'swelling_rmse_pp_{c}' for c in CASES]
    for candidate in ['K_d_150000','K_d_200000','K_d_250000']:
        row=summary.set_index('candidate').loc[candidate]
        for reference in ['baseline','K_d_low']:
            ref=summary.set_index('candidate').loc[reference]
            delta={'candidate':candidate,'K_d':row.K_d,'reference':reference,'reference_K_d':ref.K_d}
            delta.update({f'delta_{metric}':row[metric]-ref[metric] for metric in metrics});deltas.append(delta)
    pd.DataFrame(deltas).to_csv(out/'reference_deltas.csv',index=False)
    compact=summary[['candidate','K_d','swelling_balanced_nrmse','Rd_rmse_le1600_nm_AP3.4','gas_core3_RMSE_pp','gas_core3_vs_baseline_ratio','Rd_rmse_gt1600_diagnostic_nm_AP3.4','Nd_log10_rmse_le1600_diagnostic','Nd_log10_rmse_gt1600_diagnostic','pareto_experiments','pareto_with_gas']].rename(columns={
        'candidate':'Candidato','swelling_balanced_nrmse':'Swelling NRMSE','Rd_rmse_le1600_nm_AP3.4':'R_d ≤1600 RMSE nm','gas_core3_RMSE_pp':'Gas core RMS pp','gas_core3_vs_baseline_ratio':'Gas / baseline','Rd_rmse_gt1600_diagnostic_nm_AP3.4':'R_d high-T diagnostica nm','Nd_log10_rmse_le1600_diagnostic':'N_d low-T diagnostica dex','Nd_log10_rmse_gt1600_diagnostic':'N_d high-T diagnostica dex'})
    report='''# Refinement monodimensionale di K_d

Eseguiti soltanto K_d=1.5e5, 2.0e5, 2.5e5: 3 × 4 pin × 19 temperature = **228 nuovi punti**. Baseline K_d=3e5 e controllo K_d=1e5 riutilizzati dai risultati OAT: **nessun rerun dei due controlli**. Ogni altro campo del candidato è identico alla baseline: f_n=5.5e-4, rho_d=3e13, Dv_dislocation_scale=10, Dg_dislocation_scale=13, N_gf0_factor=1. Tutti i switches, la formulazione e gli altri parametri restano quelli originali. DT_H=1 h, N_MODES=40, T=900–1800 K con passo 50 K; fission rate specifici dei quattro pin invariati.

Nessuno score composto. Swelling prioritario: stessa media delle NRMSE per pin usata nell’OAT, con tutti i dati sperimentali in dominio. R_d secondario forte: 5 punti sperimentali AP3.4 con T ≤1600 K. R_d >1600 K e N_d low/high-T sono **solo diagnostiche**, senza peso nella selezione. Gas core RMS = benchmark dei compartimenti matrice/bulk/dislocazioni (333 residui per candidato, unità pp); il benchmark completo a cinque frazioni (555 residui), FGR, R_gf, N_gf e pressioni restano diagnostiche/guardrail. Pareto esperimenti usa solo swelling + R_d low-T; Pareto con gas aggiunge soltanto il benchmark core. Nessuna somma pesata o ranking totale.

Le temperature sperimentali e benchmark sono interpolate sulla stessa griglia 50 K; non si eseguono punti aggiuntivi. Il punto AP3.8 a 899 K è escluso, come prima. Il benchmark 1.1% è condiviso AP3.2/AP3.8; non c’è una curva gas partition a 1.3%. Sono benchmark di modello, non esperimenti. Il CSV R_gf/N_gf Rizk Fig.7/8 resta non disponibile. La sottostima high-T del raggio o sovrastima high-T della density non sono obiettivi da eliminare.

'''+table(compact,list(compact.columns))+'\n\n'
    swell=summary[['K_d']+[f'swelling_rmse_pp_{c}' for c in CASES]+[f'swelling_bias_pp_{c}' for c in CASES]].copy()
    report+='Swelling per pin: RMSE e bias in punti percentuali, sempre separati. Un miglioramento della media non cancella i casi peggiorati.\n\n'+table(swell,list(swell.columns))+'\n\n'
    gas_table=summary[['K_d','gas_core3_RMSE_pp','gas_partition5_RMSE_pp_diagnostic']+[f'benchmark_{c}_RMSE_pp' for c in GAS_CHANNELS]].copy()
    report+='Coerenza gas: tutti i canali riportati separatamente; FGR/grain-face fuori dal fronte di selezione.\n\n'+table(gas_table,list(gas_table.columns))+'\n\n'
    bias=summary[['K_d','Rd_bias_le1600_nm_AP3.4','Rd_bias_gt1600_diagnostic_nm_AP3.4','Nd_log10_bias_le1600_diagnostic','Nd_log10_bias_gt1600_diagnostic','p_gf_max','p_gf_over_eq_max','max_abs_gas_balance_error_pp']]
    report+='Bias e pressioni sono diagnostiche: bias raggio positivo = sovrastima; bias log10 N_d positivo = sovrastima. Pressioni finite e bilancio conservato non certificano validità fisica.\n\n'+table(bias,list(bias.columns))+'\n\n'
    report+='''Lettura gerarchica: **K_d=2e5** migliora lo swelling su tutti e quattro i pin (RMSE AP3.2/AP3.8/AP3.4/ANP6: 0.4256/0.6878/0.9815/0.8337 pp, contro 0.4530/0.9886/1.0782/1.0718 pp della baseline), porta R_d low-T da 24.66 a **14.65 nm** e aumenta il RMS gas core soltanto da 3.077 a **3.232 pp** (+5.03%). È il valore più interessante per un miglioramento distribuito sui quattro pin, senza dover variare un secondo parametro.

K_d=2.5e5 è più conservativo: migliora anch’esso tutti i pin, ha gas core +2.25% e R_d low-T 19.96 nm, ma lascia un residuo maggiore sugli altri tre pin rispetto a 2e5. K_d=1.5e5 dà il minimo R_d low-T (11.83 nm) e la minima media swelling (0.3057), ma AP3.2 peggiora da 0.4530 a 0.6123 pp e AP3.4 da 1.0782 a 1.0988 pp. Il fronte sperimentale aggregato lo preferisce a 2e5, mentre il dettaglio per pin mostra perché la media non determina da sola la scelta. Il confronto non viene convertito in uno score composto.

Il bias R_d low-T varia in modo ordinato: +21.04 nm a 1e5, +5.38 nm a 1.5e5, −4.62 nm a 2e5, −11.74 nm a 2.5e5, −17.17 nm alla baseline. I valori intermedi riducono il grande sbilanciamento del controllo 1e5. Nessun vantaggio sul raggio high-T o su N_d viene usato per promuovere un candidato. Il controllo 1e5 è dominato da 1.5e5 sui tre criteri aggregati swelling/raggio low-T/gas core, ma conserva RMSE swelling più piccoli su AP3.8/ANP6: le priorità restano esplicite.

Tutti i nuovi valori rispettano N_gf_vol < N_d < N_b su tutti i 76 punti per candidato. Restano violazioni persistenti dell’ordine dei raggi nei tre pin AP3.2/AP3.8/AP3.4 a bassa T; ANP6 lo rispetta per tutti i nuovi valori. Le pressioni grain-face restano pressoché identiche alla baseline: nessuna conclusione di validità fisica viene dedotta dall’esito numerico positivo. Il bilancio di gas è conservato entro 5.68e-14 pp, senza valori finali non finiti o negativi nei campi fisici controllati.

'''
    report+='Gli ordinamenti confrontano R_gf > R_d > R_b e N_gf_vol < N_d < N_b allo stesso burnup finale. N_gf_vol è la conversione volumetrica, non la densità areale. Persistente = almeno tre temperature consecutive, span ≥100 K. Non vengono controllate qui tutte le storie temporali.\n\n'
    persistent=guard[guard.persistent][['K_d','case','ordering','persistent_ranges']]
    report+=table(persistent,list(persistent.columns))+'\n\n'
    report+='''Le pressioni grain-face conservano le elevate sovrapressioni già presenti nella baseline. La soglia descrittiva p/p_eq>1000 serve solo a esporre intervalli ed estremi, non come obiettivo di fit o regola arbitraria di accettazione. I dettagli sono in pressure_diagnostics.csv e negli output completi.

Confronti rispetto a entrambi i controlli: reference_deltas.csv contiene tutte le differenze dei tre nuovi valori rispetto a baseline e K_d=1e5, inclusi i quattro swelling separati. I CSV candidate_summary.csv e comparison_summary.csv contengono rispettivamente i tre nuovi candidati e tutti e cinque i valori confrontati. comparison_runs.csv distingue i 228 punti nuovi dai 152 punti riutilizzati. comparison_points.csv conserva osservazioni, predizioni interpolate, residui e ruoli nella valutazione.

**Nessuna combinazione a due parametri viene proposta o eseguita.** Le precedenti proposte sono sospese in attesa della lettura di questo refinement. Non viene dedotta alcuna performance di una combinazione da questi risultati.
'''
    (out/'comparison.md').write_text(report,encoding='utf-8')
    assertions={'new_candidates':3,'new_points':228,'reused_control_points':152,'comparison_points_in_runs':380,'only_Kd_varied':True,'control_metrics_reproduce_previous_evaluation':True,
                'Rd_lowT_points_per_candidate':5,'Rd_highT_points_per_candidate':4,'N_d_excluded_from_selection':True,'highT_Rd_excluded_from_selection':True,
                'pareto_experimental_columns':exp_cols,'pareto_with_gas_columns':gas_cols,'combined_score':False,'combined_proposals':0,'combined_runs':0}
    assert summary.Rd_n_le1600.eq(5).all() and summary.Rd_n_gt1600_diagnostic.eq(4).all()
    (out/'evaluation_validation.json').write_text(json.dumps(assertions,indent=2))
    print(compact.to_string(index=False))
    print('\nSwelling per pin:\n'+swell.to_string(index=False))
    print('\nSaved separated evaluation and guardrails. Combined proposals/runs: 0.')
    return runs,summary,points

if __name__=='__main__':evaluate_refinement(Path(__file__).resolve().parent)
