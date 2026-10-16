"""Reassess saved OAT outputs only. This module contains no solver calls."""
from pathlib import Path
import hashlib, json, shutil
import numpy as np
import pandas as pd

CORE_GAS = ['matrix_gas_percent', 'bulk_gas_percent', 'dislocation_gas_percent']
GAS_CHANNELS = CORE_GAS + ['grainface_gas_percent', 'release_gas_percent']
CASES = ['AP3.2', 'AP3.8', 'AP3.4', 'ANP6']
PARAMS = ['f_n', 'K_d', 'rho_d', 'Dv_dislocation_scale', 'Dg_dislocation_scale', 'N_gf0_factor']

def _hash(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def _rmse(values):
    a = np.asarray(values, dtype=float)
    return float(np.sqrt(np.mean(a*a))) if len(a) else np.nan

def _table(frame, columns):
    lines = ['| '+' | '.join(columns)+' |', '| '+' | '.join(['---']*len(columns))+' |']
    for _, row in frame.iterrows():
        cells=[]
        for column in columns:
            value=row[column]
            if isinstance(value,(float,np.floating)):
                cells.append(f'{value:.4g}' if np.isfinite(value) else 'n/d')
            else:
                cells.append(str(value).replace('|','/'))
        lines.append('| '+' | '.join(cells)+' |')
    return '\n'.join(lines)

def _pareto(frame, columns):
    # All quantities are minimized. Tolerance handles exact OAT ties.
    a=frame[columns].to_numpy(float)
    labels=frame.candidate.tolist()
    dominated_by=[]
    for i in range(len(a)):
        dominant=[]
        for j in range(len(a)):
            if i==j: continue
            tolerance=1e-10*np.maximum(1.0,np.maximum(np.abs(a[i]),np.abs(a[j])))
            if np.all(a[j]<=a[i]+tolerance) and np.any(a[j]<a[i]-tolerance):
                dominant.append(labels[j])
        dominated_by.append('; '.join(dominant))
    return [not bool(x) for x in dominated_by], dominated_by

def recalculate_evaluation(output_dir):
    out=Path(output_dir).resolve()
    archive=out/'evaluation_archive'/'original_hierarchy'
    archive.mkdir(parents=True,exist_ok=True)
    for name in ['candidate_summary.csv','findings.md','direction_table.csv','direction_table.md','next_round_proposals.csv','next_round_proposals.json']:
        target=archive/name
        if not target.exists(): shutil.copy2(out/name,target)
    # Frozen data include all original physical outputs and comparison residuals.
    frozen_files=[out/'all_OAT_runs.csv',out/'comparison_points.csv',out/'model_benchmark_summary.csv',out/'manifest.json',out/'candidate_design.csv',out/'ordering_diagnostics.csv',out/'pressure_diagnostics.csv',out/'baseline_reproduction_check.csv']
    frozen_files+=sorted((out/'checkpoints').glob('*.json'))+sorted((out/'candidates').glob('*.csv'))+sorted((out/'plots').glob('*.png'))
    before={str(p.relative_to(out)):_hash(p) for p in frozen_files}
    raw=pd.read_csv(out/'all_OAT_runs.csv')
    points=pd.read_csv(out/'comparison_points.csv')
    summary=pd.read_csv(archive/'candidate_summary.csv')
    benchmarks=pd.read_csv(out/'model_benchmark_summary.csv')
    assert len(raw)==988 and raw.status.eq('ok').all()
    assert len(summary)==13 and not raw.duplicated(['candidate','experiment_case','T']).any()
    assert set(raw.candidate)==set(summary.candidate)
    valid_points=points.predicted.notna()&points.observed.notna()
    data_type=points.data_type.eq('experiment')
    rd=points[data_type&points.metric.eq('R_d')&valid_points].copy()
    nd=points[data_type&points.metric.eq('N_d')&valid_points].copy()
    gas=points[points.data_type.eq('Rizk2025_model_benchmark')&points.metric.isin(GAS_CHANNELS)&valid_points].copy()
    rd['temperature_group']=np.where(rd.T_K<=1600,'le1600','gt1600')
    nd['temperature_group']=np.where(nd.T_K<=1600,'le1600','gt1600')
    assert rd.groupby(['candidate','temperature_group']).size().unstack().eq([4,5]).all().all() # columns gt1600,le1600
    assert gas.groupby(['candidate','case','metric']).size().eq(37).all()
    # Recompute swelling from saved experimental residuals and verify exact invariance.
    for candidate in summary.candidate:
        old=summary.loc[summary.candidate.eq(candidate)].iloc[0]
        nrmse=[]
        for case in CASES:
            p=points[points.candidate.eq(candidate)&points['case'].eq(case)&data_type&points.metric.eq('P2_swelling')&valid_points]
            score=_rmse(p.residual)/_rmse(p.observed)
            assert np.isclose(_rmse(p.residual),old[f'swelling_rmse_pp_{case}'],rtol=1e-12,atol=1e-12)
            assert np.isclose(score,old[f'swelling_nrmse_{case}'],rtol=1e-12,atol=1e-12)
            nrmse.append(score)
        assert np.isclose(np.mean(nrmse),old.swelling_balanced_nrmse,rtol=1e-12,atol=1e-12)
    # Preserve the old all-T radius metric explicitly as historical diagnostic.
    summary=summary.rename(columns={'Rd_rmse_nm_AP3.4':'Rd_rmse_fullT_nm_AP3.4_legacy_diagnostic','Rd_bias_nm_AP3.4':'Rd_bias_fullT_nm_AP3.4_legacy_diagnostic'})
    summary=summary.drop(columns=['Rd_rank_secondary'],errors='ignore')
    new_rows=[]
    for candidate in summary.candidate:
        row={'candidate':candidate}
        for group,label in [('le1600','le1600'),('gt1600','gt1600_diagnostic')]:
            q=rd[rd.candidate.eq(candidate)&rd.temperature_group.eq(group)]
            row[f'Rd_rmse_{label}_nm_AP3.4']=_rmse(q.residual)
            row[f'Rd_bias_{label}_nm_AP3.4']=float(q.residual.mean())
            row[f'Rd_n_{label}_AP3.4']=len(q)
            row[f'Rd_fraction_underpredicted_{label}_AP3.4']=float((q.residual<0).mean())
        for group,label in [('le1600','le1600_diagnostic'),('gt1600','gt1600_diagnostic')]:
            q=nd[nd.candidate.eq(candidate)&nd.temperature_group.eq(group)]
            row[f'Nd_log10_rmse_{label}']=_rmse(q.log10_residual)
            row[f'Nd_log10_bias_{label}']=float(q.log10_residual.mean())
            row[f'Nd_median_ratio_{label}']=float(np.median(q.predicted/q.observed))
            row[f'Nd_fraction_overpredicted_{label}']=float((q.predicted>q.observed).mean())
            row[f'Nd_n_{label}']=len(q)
        p=gas[gas.candidate.eq(candidate)]
        # RMS of benchmark residuals only, with identical units and equal point counts.
        # This is NOT combined with experimental swelling or radius.
        row['gas_partition_RMSE_pp']=_rmse(p.residual)
        row['gas_core3_RMSE_pp']=_rmse(p[p.metric.isin(CORE_GAS)].residual)
        row['gas_benchmark_n_points']=len(p)
        for channel in GAS_CHANNELS:
            row[f'benchmark_{channel}_RMSE_pp']=_rmse(p[p.metric.eq(channel)].residual)
        for case in ['AP3.2','AP3.8','ANP6']:
            row[f'gas_partition_RMSE_pp_{case}']=_rmse(p[p['case'].eq(case)].residual)
        new_rows.append(row)
    extra=pd.DataFrame(new_rows)
    # Existing N_d diagnostics are retained numerically; duplicate names are verified then replaced.
    for column in set(extra.columns)&set(summary.columns)-{'candidate'}:
        left=summary.set_index('candidate')[column];right=extra.set_index('candidate')[column]
        assert np.allclose(left,right.reindex(left.index),rtol=1e-12,atol=1e-12)
    summary=summary.drop(columns=[c for c in extra.columns if c!='candidate' and c in summary.columns]).merge(extra,on='candidate',validate='one_to_one',sort=False)
    baseline=summary.set_index('candidate').loc['baseline']
    summary=summary.copy()
    summary['gas_partition_vs_baseline_ratio']=summary.gas_partition_RMSE_pp/baseline.gas_partition_RMSE_pp
    summary['gas_core3_vs_baseline_ratio']=summary.gas_core3_RMSE_pp/baseline.gas_core3_RMSE_pp
    summary['Rd_le1600_rank_secondary']=summary['Rd_rmse_le1600_nm_AP3.4'].rank(method='min').astype(int)
    exp_columns=['swelling_balanced_nrmse','Rd_rmse_le1600_nm_AP3.4']
    gas_columns=exp_columns+['gas_core3_RMSE_pp']
    summary['pareto_experimental_2D'],summary['dominated_by_experimental_2D']=_pareto(summary,exp_columns)
    summary['pareto_with_gas_3D'],summary['dominated_by_with_gas_3D']=_pareto(summary,gas_columns)
    # No high-T radius, density, pressure, FGR or ordering metric enters dominance or experimental ranks.
    summary.to_csv(out/'candidate_summary.csv',index=False)
    comparison_cols=['candidate','swelling_balanced_nrmse']+[f'swelling_rmse_pp_{c}' for c in CASES]+['Rd_rmse_le1600_nm_AP3.4','Rd_bias_le1600_nm_AP3.4','gas_partition_RMSE_pp','gas_core3_RMSE_pp','gas_partition_vs_baseline_ratio','gas_core3_vs_baseline_ratio','Rd_rmse_gt1600_diagnostic_nm_AP3.4','Rd_bias_gt1600_diagnostic_nm_AP3.4','Nd_log10_rmse_le1600_diagnostic','Nd_log10_rmse_gt1600_diagnostic','Nd_log10_bias_le1600_diagnostic','Nd_log10_bias_gt1600_diagnostic','pareto_experimental_2D','pareto_with_gas_3D','dominated_by_experimental_2D','dominated_by_with_gas_3D']
    for channel in GAS_CHANNELS: comparison_cols.append(f'benchmark_{channel}_RMSE_pp')
    comparison=summary[comparison_cols].copy()
    comparison.to_csv(out/'pareto_comparison.csv',index=False)
    gas_columns_display=['candidate','gas_core3_RMSE_pp','gas_core3_vs_baseline_ratio','gas_partition_RMSE_pp','gas_partition_vs_baseline_ratio']+[f'benchmark_{c}_RMSE_pp' for c in GAS_CHANNELS]+[f'gas_partition_RMSE_pp_{c}' for c in ['AP3.2','AP3.8','ANP6']]
    gas_detail=summary[gas_columns_display]
    gas_detail.to_csv(out/'gas_partition_comparison.csv',index=False)
    gas_md=gas_detail.rename(columns={'candidate':'Candidato','gas_core3_RMSE_pp':'RMS matrice/bulk/disl pp','gas_core3_vs_baseline_ratio':'Compartimenti / baseline','gas_partition_RMSE_pp':'RMS cinque canali diagnostica pp','gas_partition_vs_baseline_ratio':'Cinque canali / baseline'})
    (out/'gas_partition_comparison.md').write_text('# Benchmark gas partition separato\n\nBenchmark Rizk di modello, non esperimenti. Il fronte con gas usa soltanto la RMS dei compartimenti matrice/bulk/dislocazioni. RMS completa, grain-face e FGR restano diagnostiche. Nessuna aggregazione con swelling o R_d. La curva 1.1% FIMA è condivisa AP3.2/AP3.8; a 1.3% non è disponibile. Le metriche aggregate derivano dai residui salvati, con conteggi uguali per pin e canale. Dettagli per caso/canale in model_benchmark_summary.csv.\n\n'+_table(gas_md,list(gas_md.columns))+'\n',encoding='utf-8')
    display=comparison[['candidate','swelling_balanced_nrmse','Rd_rmse_le1600_nm_AP3.4','gas_core3_RMSE_pp','gas_core3_vs_baseline_ratio','Rd_rmse_gt1600_diagnostic_nm_AP3.4','Nd_log10_rmse_le1600_diagnostic','Nd_log10_rmse_gt1600_diagnostic','pareto_experimental_2D','pareto_with_gas_3D']].rename(columns={
        'candidate':'Candidato','swelling_balanced_nrmse':'Swelling NRMSE','Rd_rmse_le1600_nm_AP3.4':'R_d ≤1600 K RMSE nm','gas_core3_RMSE_pp':'Gas matrice/bulk/disl RMS pp','gas_core3_vs_baseline_ratio':'Gas core / baseline',
        'Rd_rmse_gt1600_diagnostic_nm_AP3.4':'R_d >1600 K diagnostica nm','Nd_log10_rmse_le1600_diagnostic':'N_d ≤1600 K diagnostica dex','Nd_log10_rmse_gt1600_diagnostic':'N_d >1600 K diagnostica dex',
        'pareto_experimental_2D':'Pareto esperimenti','pareto_with_gas_3D':'Pareto con gas'})
    criteria='''Swelling: stesso score precedente, media delle NRMSE separate sui quattro pin. R_d secondario forte: RMSE su **5 punti sperimentali AP3.4 con T ≤1600 K**. R_d high-T: **4 punti**, sola diagnostica, peso zero nella selezione. N_d low/high-T: diagnostiche qualitative con RMSE log10 e bias separati, senza peso di selezione. La sottostima high-T di R_d e la sovrastima high-T di N_d sono trattate come comportamento della formulazione accettabile secondo il criterio interpretativo fornito dall’utente per la Fig.4.7 Matthews 2025; non si tenta di eliminarle.

Gas partition: RMS degli scarti dei **tre compartimenti matrice/bulk/dislocazioni** come asse separato di coerenza, senza combinazione con gli errori sperimentali. RMS delle cinque frazioni completa, canale grain-face e FGR sono riportati separatamente come diagnostiche e non entrano nel fronte di selezione. Sono 333 confronti core per candidato (3 pin × 3 canali × 37 temperature già interpolate), e 555 nella diagnostica completa a cinque canali. Stesse unità (pp), stesso numero di punti per canale/pin, nessun peso di fit. Il CSV riporta anche RMS completo a cinque frazioni, tutti i canali separati e i rapporti alla baseline. Questi benchmark sono di modello, non dati sperimentali. Le curve 1.1% FIMA sono condivise da AP3.2/AP3.8; non sono disponibili a 1.3% FIMA. Nessuna soglia arbitraria di accettazione gas viene applicata: i peggioramenti sono esposti numericamente.

Pareto esperimenti: minimizzazione delle sole colonne swelling e R_d ≤1600 K. Pareto con gas: le stesse due colonne più RMS dei tre compartimenti matrice/bulk/dislocazioni, con dominanza componente per componente e nessuna somma pesata. Nessun ordinamento totale è imposto. Un candidato è dominato se un altro non peggiora nessuna colonna e ne migliora almeno una (tolleranza relativa 1e-10). **N_d, R_d high-T, pressioni e altri guardrail non entrano nei fronti né nei ranghi.** Un punto sul fronte può avere swelling o gas partition inaccettabilmente peggiori: essere non dominato non significa essere raccomandato.
'''
    comparison_md='# Confronto separato e Pareto — valutazione corretta\n\n'+criteria+'\n'+_table(display,list(display.columns))+'\n'
    (out/'pareto_comparison.md').write_text(comparison_md,encoding='utf-8')
    selected_points=points.copy()
    selected_points['evaluation_role']='diagnostic_or_supplementary'
    selected_points.loc[data_type&points.metric.eq('P2_swelling'),'evaluation_role']='primary_swelling'
    selected_points.loc[data_type&points.metric.eq('R_d')&points.T_K.le(1600),'evaluation_role']='strong_secondary_Rd_le1600'
    selected_points.loc[data_type&points.metric.eq('R_d')&points.T_K.gt(1600),'evaluation_role']='qualitative_highT_Rd_no_selection_penalty'
    selected_points.loc[data_type&points.metric.eq('N_d'),'evaluation_role']='qualitative_Nd_no_selection_penalty'
    selected_points.loc[points.data_type.eq('Rizk2025_model_benchmark'),'evaluation_role']='model_benchmark_coherence_not_experiment'
    selected_points['included_in_experimental_selection']=valid_points & selected_points.evaluation_role.isin(['primary_swelling','strong_secondary_Rd_le1600'])
    selected_points['diagnostic_weight']=selected_points.evaluation_role
    selected_points.to_csv(out/'evaluation_comparison_points.csv',index=False)
    # The physical OAT directions do not change when the evaluation hierarchy changes.
    direction=pd.read_csv(archive/'direction_table.csv')
    def role(r):
        if r.metric=='swelling_d_percent':return 'primary_observable; selection evaluated at experimental points'
        if r.metric=='Rd_nm':return 'strong_secondary_lowT' if r.T_band_K=='900-1600' else ('qualitative_highT_no_selection_penalty' if r.T_band_K=='1650-1800' else 'mixed_low_high_not_selection_metric')
        if r.metric=='Nd':return 'qualitative_only_no_selection_penalty'
        if r.metric in CORE_GAS:return 'important_model_benchmark_coherence_not_experiment'
        return 'guardrail_or_model_benchmark_not_experiment'
    direction['evaluation_role']=direction.apply(role,axis=1)
    direction.to_csv(out/'direction_table.csv',index=False)
    source=summary.set_index('candidate')
    effects=[]
    for parameter in PARAMS:
        baseline_row=source.loc['baseline']
        for level in ['low','high']:
            row=source.loc[parameter+'_'+level]
            entry={'parameter':parameter,'level':level,'parameter_value':row[parameter], 'swelling_NRMSE':row.swelling_balanced_nrmse,
                   'delta_swelling_NRMSE_vs_baseline':row.swelling_balanced_nrmse-baseline_row.swelling_balanced_nrmse,
                   'Rd_le1600_RMSE_nm':row['Rd_rmse_le1600_nm_AP3.4'],'delta_Rd_le1600_RMSE_nm_vs_baseline':row['Rd_rmse_le1600_nm_AP3.4']-baseline_row['Rd_rmse_le1600_nm_AP3.4'],
                   'Rd_le1600_bias_nm':row['Rd_bias_le1600_nm_AP3.4'],'gas_partition_RMSE_pp':row.gas_partition_RMSE_pp,
                   'delta_gas_partition_RMSE_pp_vs_baseline':row.gas_partition_RMSE_pp-baseline_row.gas_partition_RMSE_pp,'gas_partition_vs_baseline_ratio':row.gas_partition_vs_baseline_ratio,
                   'gas_core3_RMSE_pp':row.gas_core3_RMSE_pp,'gas_core3_vs_baseline_ratio':row.gas_core3_vs_baseline_ratio,
                   'Rd_gt1600_diagnostic_RMSE_nm':row['Rd_rmse_gt1600_diagnostic_nm_AP3.4'],
                   'Nd_le1600_diagnostic_dex':row.Nd_log10_rmse_le1600_diagnostic,'Nd_gt1600_diagnostic_dex':row.Nd_log10_rmse_gt1600_diagnostic}
            for case in CASES:entry[f'delta_swelling_RMSE_pp_{case}']=row[f'swelling_rmse_pp_{case}']-baseline_row[f'swelling_rmse_pp_{case}']
            effects.append(entry)
    effects=pd.DataFrame(effects)
    effects.to_csv(out/'evaluation_directions.csv',index=False)
    effects_display=effects[['parameter','level','parameter_value','delta_swelling_NRMSE_vs_baseline','delta_Rd_le1600_RMSE_nm_vs_baseline','delta_gas_partition_RMSE_pp_vs_baseline','gas_core3_vs_baseline_ratio']].rename(columns={
        'parameter':'Parametro','level':'Livello','parameter_value':'Valore','delta_swelling_NRMSE_vs_baseline':'Δ swelling NRMSE','delta_Rd_le1600_RMSE_nm_vs_baseline':'Δ R_d ≤1600 RMSE nm','delta_gas_partition_RMSE_pp_vs_baseline':'Δ gas RMS pp','gas_core3_vs_baseline_ratio':'Gas core / baseline'})
    effects_md='# Direzioni della valutazione OAT aggiornata\n\nVariazioni rispetto alla baseline: Δ negativo significa minore errore, non necessariamente miglioramento di tutti i pin. Ogni prova varia un solo parametro; non viene stimata la risposta delle combinazioni. Le direzioni fisiche restano nelle 720 righe di direction_table.csv.\n\n'+_table(effects_display,list(effects_display.columns))+'\n\n'
    (out/'evaluation_directions.md').write_text(effects_md,encoding='utf-8')
    prefix='''> **Gerarchia aggiornata:** swelling sui quattro pin prioritario; R_d ≤1600 K secondario forte; R_d >1600 K e N_d solo diagnostiche, fuori dai ranking. Gas partition Rizk come coerenza di modello. Le frecce seguenti descrivono la risposta fisica e non uno score di fit. Le variazioni degli errori sono in [evaluation_directions.md](evaluation_directions.md); il confronto separato in [pareto_comparison.md](pareto_comparison.md).

'''
    (out/'direction_table.md').write_text(prefix+(archive/'direction_table.md').read_text(),encoding='utf-8')
    # Conservative hypotheses for the NEXT round. Never instantiate or execute these.
    defaults={'f_n':5.5e-4,'K_d':3e5,'rho_d':3e13,'Dv_dislocation_scale':10,'Dg_dislocation_scale':13,'N_gf0_factor':1}
    proposals=[
        {'proposal':'C1','changes':{'K_d':2e5,'Dv_dislocation_scale':30},'tradeoff':'Ridurre K_d moderatamente per migliorare R_d fino a 1600 K e aumentare lo swelling nei pin sottostimati; Dv=30 ha un contributo piccolo e mantiene la gas partition vicina alla baseline.','risk':'AP3.2 è già vicino alla baseline; l’aumento di swelling può peggiorarlo. Nessun tentativo di eliminare il bias high-T del raggio.'},
        {'proposal':'C2','changes':{'K_d':1.5e5,'f_n':7e-4},'tradeoff':'Ridurre K_d per il raggio low-T e alzare f_n di poco per limitare swelling eccessivo e compensare parzialmente lo spostamento di gas dal bulk alle dislocazioni.','risk':'L’aumento di f_n può ridurre anche R_d utile e swelling AP3.8/ANP6. L’OAT a f_n=1e-2 è lontano: non prova la risposta locale a 7e-4.'},
        {'proposal':'C3','changes':{'K_d':2.5e5,'Dv_dislocation_scale':30},'tradeoff':'Versione più conservativa di C1: cercare un piccolo beneficio di R_d low-T preservando swelling AP3.2 e gas partition più vicini alla baseline.','risk':'Il miglioramento può essere troppo piccolo per AP3.8/AP3.4/ANP6. Serve a misurare il compromesso, non a inseguire il raggio high-T.'},
    ]
    proposal_rows=[]
    for p in proposals:
        proposal_rows.append({'proposal':p['proposal'],**defaults,**p['changes'],'executed':False,'approved':False,'tradeoff':p['tradeoff'],'risk':p['risk'],'basis':'swelling unchanged score + experimental Rd<=1600 + gas benchmark coherence; highT Rd/Nd no selection penalty'})
    pd.DataFrame(proposal_rows).to_csv(out/'next_round_proposals.csv',index=False)
    (out/'next_round_proposals.json').write_text(json.dumps(proposal_rows,indent=2,ensure_ascii=False))
    recommendation_lines=[]
    for p in proposals:
        changes=', '.join(f'`{k}={v:g}`' for k,v in p['changes'].items())
        recommendation_lines.append(f"{p['proposal']}. {changes}. {p['tradeoff']} {p['risk']}")
    report='''# RizkUN — rivalutazione dei 13 candidati dai soli risultati OAT salvati

**Nuove simulazioni: 0. Nuove combinazioni eseguite: 0.** Tutti i valori previsti derivano dai CSV già presenti: nessun solver è chiamato e nessuna interpolazione o nuova predizione di modello è introdotta. Le metriche sono ricalcolate dai residui sperimentali e benchmark già salvati. Gli output fisici delle 988 prove, checkpoint, grafici, design e manifest restano immutati, verificati tramite SHA-256. La valutazione precedente è conservata in `evaluation_archive/original_hierarchy/`.

'''+criteria+'\n'+_table(display,list(display.columns))+'\n\n'
    report+='''**La precedente enfasi sul raggio high-T viene rimossa.** Per K_d=1e5, il beneficio sull’RMSE di R_d pertinente alla selezione è **24.6645 → 21.8902 nm**, circa **11.25%**, mentre la precedente metrica su tutte le temperature indicava 43.602 → 21.868 nm. Quella riduzione di circa metà non è più una ragione di selezione. Inoltre il bias low-T cambia da −17.171 a **+21.041 nm**: il candidato passa dalla sottostima alla sovrastima, e un K_d intermedio è un’ipotesi più prudente.

Lo swelling rimane identico alla valutazione precedente: K_d basso migliora AP3.8/ANP6, ma peggiora AP3.2/AP3.4. L’RMSE swelling per pin baseline → K_d=1e5 è 0.453 → 1.067, 0.989 → 0.269, 1.078 → 1.593, 1.072 → 0.423 pp, nell’ordine AP3.2/AP3.8/AP3.4/ANP6. La media NRMSE 0.3836 → 0.3685 non autorizza a ignorare i pin peggiorati. Il benchmark gas cambia moderatamente (RMS cinque canali 2.998 → 3.255 pp, +8.55%; compartimenti matrice/bulk/dislocazioni 3.077 → 3.492 pp, +13.49%). È un compromesso da esplorare, non un candidato con accordo simultaneo.

Il fronte Pareto delle sole metriche sperimentali contiene K_d=1e5, rho_d=6e13 e Dg_dislocation_scale=30. Gli ultimi due hanno però RMS gas cinque canali rispettivamente **14.039 e 15.135 pp**, contro **2.998 pp** di baseline (4.68× e 5.05×). I soli compartimenti matrice/bulk/dislocazioni peggiorano a **18.005 e 19.431 pp**, contro **3.077 pp** (5.85× e 6.32×). Il raggio low-T di Dg=30 è migliore (9.359 nm), ma lo swelling peggiora a NRMSE 1.2337 e la redistribuzione bulk/dislocazioni si allontana fortemente dal benchmark: non viene promosso per inseguire R_d. Non serve penalizzare N_d o il raggio high-T per identificare questo problema.

Aumentare Dv_dislocation_scale da 10 a 30 porta swelling NRMSE 0.3836 → 0.3805 e R_d low-T 24.6645 → 24.5133 nm, con gas RMS 2.998 → 3.015 pp. È un effetto piccolo e coerente, non una soluzione dominante su tutti i pin. Dv=30 domina la baseline sulle sole due metriche sperimentali, ma peggiora lievemente gas partition e AP3.2; il fronte tridimensionale lo rende visibile. Il confronto resta gerarchico e separato.

Le perturbazioni estreme f_n=1e-7, Dg=1 e rho_d=1e13 peggiorano swelling e/o raggio low-T e gas partition. L’estremo f_n=1e-2 riporta più gas nel bulk ma peggiora swelling, raggio e benchmark; non è una soluzione di coerenza. Le proposte a f_n moderatamente più alto sono soltanto un’ipotesi per compensare K_d basso: la risposta locale e l’interazione non sono note dall’OAT.

N_gf0_factor=0.1 o 10 lascia esattamente invariati swelling e R_d low-T. Cambia FGR e grain-face: non deve essere usato per correggere una diagnostica high-T intragranulare. Il fattore 10 ha RMS gas a cinque canali lievemente più basso, ma il benchmark dei tre compartimenti di selezione è identico alla baseline; inoltre viola persistentemente gli ordinamenti delle densità in tutti i pin e quello dei raggi in tre pin lungo tutta la griglia. Le tre metriche del fronte con gas sono identiche per baseline e per entrambi i fattori N_gf0; questo non supera i guardrail e non giustifica preferire un fattore. Il fattore 0.1 favorisce gli ordinamenti ma accentua le sovrapressioni grain-face. Nessun nuovo fattore grain-face viene proposto in questo round.

Le sottostime di R_d sopra 1600 K e le sovrastime di N_d high-T sono riportate con RMSE, bias, frazioni di punti sotto/sovrastimati e rapporti mediani, senza penalità nella selezione. Non si reinterpreta la correzione della gerarchia come una necessità di aumentare K_d per far coincidere N_d. Il caso K_d=1e6 può avere una density low-T più vicina ma swelling/raggio low-T peggiori; l’accordo di N_d non lo promuove.

Gli ordinamenti, FGR, R_gf, N_gf e le pressioni conservano le diagnostiche originali. La baseline ha violazioni persistenti dell’ordine dei raggi a bassa T su AP3.2/AP3.8/AP3.4, mentre rispetta l’ordine delle densità; K_d=1e5 introduce violazioni persistenti delle densità in tutti i pin. Le pressioni grain-face sono molto elevate già alla baseline (p_gf fino a 8.87e11 Pa, p_gf/p_eq fino a 2.47e4). Questi segnali restano guardrail di plausibilità, separati dai punteggi sperimentali; la corretta tolleranza del bias high-T non li cancella. Non ci sono nuovi controlli dinamici o nuove simulazioni. Il confronto numerico R_gf/N_gf Rizk resta non disponibile perché manca il CSV digitalizzato Fig.7/8 citato nel notebook precedente.

Le direzioni fisiche OAT non cambiano: aumentare f_n riduce swelling/R_d e sposta gas verso il bulk; diminuire K_d aumenta R_d e spesso swelling ma può eccedere anche a bassa T; aumentare rho_d o Dg aumenta gas dislocazioni e swelling con forti scostamenti dal benchmark agli estremi; Dv ha effetti più piccoli. La nuova gerarchia cambia la scelta delle direzioni da esplorare: **K_d intermedio e Dv moderato/alto**, con una sola proposta di compensazione tramite f_n, senza aumenti di rho_d o Dg finalizzati al raggio high-T. `evaluation_directions.csv/.md` distingue gli scarti dagli errori fisici e riporta Δ per ciascun pin.

**Proposte per il secondo round: tre combinazioni, tutte non approvate e non eseguite.** Tutti gli altri parametri e tutta la formulazione rimangono alla baseline. I valori intermedi non sono risultati di fit, interpolazioni di performance o ottimi dedotti dai 13 punti: sono ipotesi per un round da approvare.

'''
    report+='\n\n'.join(recommendation_lines)+'\n\n'
    report+='''Non sono più riproposti gli aumenti combinati di rho_d o Dg: i risultati OAT mostrano che il miglioramento del raggio può costare molto in swelling/gas partition. N_gf0 resta un guardrail separato, dato che non migliora i criteri intragranulari e mancano benchmark numerici di R_gf/N_gf. Il bias high-T accettabile non viene corretto con nuove combinazioni.

File di valutazione: [pareto_comparison.csv](pareto_comparison.csv), [pareto_comparison.md](pareto_comparison.md), [candidate_summary.csv](candidate_summary.csv), [evaluation_comparison_points.csv](evaluation_comparison_points.csv), [evaluation_directions.csv](evaluation_directions.csv), [evaluation_directions.md](evaluation_directions.md), [next_round_proposals.csv](next_round_proposals.csv). I benchmark per caso/canale restano in [model_benchmark_summary.csv](model_benchmark_summary.csv). La convalida è in `reassessment_validation.json`.
'''
    (out/'findings.md').write_text(report,encoding='utf-8')
    after={str(p.relative_to(out)):_hash(p) for p in frozen_files}
    assert before==after
    assert summary['Rd_n_le1600_AP3.4'].eq(5).all() and summary['Rd_n_gt1600_diagnostic_AP3.4'].eq(4).all()
    old_sw= pd.read_csv(archive/'candidate_summary.csv').set_index('candidate')
    new_sw=summary.set_index('candidate')
    sw_cols=[c for c in old_sw.columns if c.startswith('swelling_')]
    assert np.allclose(new_sw.loc[old_sw.index,sw_cols].to_numpy(float),old_sw[sw_cols].to_numpy(float),rtol=0,atol=0)
    record={'new_solver_runs':0,'new_combination_runs':0,'saved_OAT_rows':len(raw),'candidates':len(summary),'swelling_scores_and_fields_unchanged_exactly':True,
            'Rd_lowT_experimental_points_per_candidate':5,'Rd_highT_diagnostic_points_per_candidate':4,
            'Nd_lowT_points_per_candidate':int(summary.Nd_n_le1600_diagnostic.iloc[0]),'Nd_highT_points_per_candidate':int(summary.Nd_n_gt1600_diagnostic.iloc[0]),
            'gas_benchmark_points_per_candidate':int(summary.gas_benchmark_n_points.iloc[0]),'gas_core_selection_benchmark_points_per_candidate':333,'FGR_grainface_excluded_from_pareto_axis':True,
            'pareto_experimental_columns':exp_columns,'pareto_with_gas_columns':gas_columns,
            'highT_Rd_Nd_pressure_excluded_from_selection':True,'combined_selection_score_created':False,
            'frozen_artifact_count':len(before),'frozen_artifact_sha256':before,'frozen_artifacts_unchanged':before==after,'proposals':len(proposals),'proposals_executed':0}
    (out/'reassessment_validation.json').write_text(json.dumps(record,indent=2,ensure_ascii=False))
    print(display.to_string(index=False))
    print(f'\nReassessment complete: 0 solver runs; {len(before)} frozen artifacts unchanged; 3 proposals not executed.')
    return summary,comparison,effects

if __name__=='__main__':
    recalculate_evaluation(Path(__file__).resolve().parent)
