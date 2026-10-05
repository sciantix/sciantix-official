"""Original notebook plot suite, supplementary grain-face/FGR panels and final report."""
from pathlib import Path
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import Optuna_rho_constant_Rizk_model as model
from Optuna_rho_constant_Rizk_metrics import OBJECTIVES

def markdown(frame, columns=None):
    sub=frame if columns is None else frame[columns]
    def cell(v):
        if isinstance(v,(float,np.floating)): return f'{v:.6g}'
        return str(v).replace('|','/').replace('\n',' ')
    return '| '+' | '.join(sub.columns)+' |\n| '+' | '.join(['---']*len(sub.columns))+' |\n'+'\n'.join('| '+' | '.join(cell(v) for v in row)+' |' for row in sub.itertuples(index=False,name=None))

def create_plots(frame, parameters, label, directory):
    directory.mkdir(parents=True,exist_ok=True)
    model.OUTPUT_DIR=str(directory)
    original_savefig=model.savefig
    def save_limited(name):
        for ax in plt.gcf().axes:
            if 'Temperature' in ax.get_xlabel() and not hasattr(ax,'zaxis'): ax.set_xlim(900,1800)
        return original_savefig(name)
    model.savefig=save_limited
    try:
        rows=frame.to_dict('records'); candidate=model.calibration_candidate(parameters,label)
        standard=model.make_all_plots(rows,candidate)
        supplemental=[]
        for kind in ['radii','densities','grainface','FGR','grainface_areal_density']:
            fig,axes=plt.subplots(2,2,figsize=(13,9),sharex=True)
            for case,ax in zip(model.EXPERIMENT_CASE_ORDER,axes.ravel()):
                sub=frame[frame.case.eq(case)].sort_values('T'); T=sub['T']
                if kind=='radii':
                    for column,name in [('Rb','bulk'),('Rd','dislocation'),('Rgf','grain-face')]: ax.semilogy(T,sub[column]*1e9,label=name)
                    if case=='AP3.4':
                        exp=pd.DataFrame(model.EXP_RD_T_13); low=exp['T']<=1600
                        ax.scatter(exp.loc[low,'T'],exp.loc[low,'R_nm'],marker='x',c='k',label='P2 experiment <=1600 K')
                        ax.scatter(exp.loc[~low,'T'],exp.loc[~low,'R_nm'],marker='+',c='0.5',label='P2 high-T diagnostic')
                    ax.set_ylabel('Radius [nm]')
                elif kind=='densities':
                    for column,name in [('Nb','bulk'),('Nd','dislocation'),('Ngf','grain-face (volume equivalent)')]: ax.semilogy(T,sub[column],label=name)
                    if case=='AP3.4':
                        exp=pd.DataFrame(model.EXP_ND_T_13); low=exp['T']<=1600
                        ax.scatter(exp.loc[low,'T'],exp.loc[low,'N'],marker='x',c='k',label='P2 experiment <=1600 K')
                        ax.scatter(exp.loc[~low,'T'],exp.loc[~low,'N'],marker='+',c='0.5',label='P2 high-T diagnostic')
                    ax.set_ylabel('Number density [m^-3]')
                elif kind=='grainface':
                    ax.plot(T,sub.swelling_gf,label='grain-face swelling [%]')
                    ax.plot(T,sub.Fc_gf,label='coverage Fc [-]')
                    ax.axhline(model.FC_SAT,color='0.5',ls=':',label='saturation coverage')
                    ax.plot(T,sub.grainface_gas,label='grain-face gas [% generated]')
                    ax.set_ylabel('Swelling / gas [%]; coverage [-]')
                elif kind=='grainface_areal_density':
                    ax.semilogy(T,sub.Ngf_areal,label='grain-face areal density')
                    ax.axhline(parameters['NGF_AREAL_0'],color='0.5',ls=':',label='initial NGF_AREAL_0')
                    ax.set_ylabel('Grain-face number density [m^-2]')
                else:
                    ax.plot(T,sub.FGR,label='Model FGR')
                    ref=model.rizk2025_partition_reference_for_burnup(float(sub.burnup.iloc[0]))
                    if ref is not None:
                        mask=(ref['T_K']>=900)&(ref['T_K']<=1800)
                        ax.plot(ref['T_K'][mask],ref['FGR'][mask],ls='--',label='Rizk Fig.9 model benchmark')
                    ax.set_ylabel('FGR [% generated]')
                ax.set_title(f'{case}: {sub.burnup.iloc[0]:.1f}% FIMA'); ax.set_xlabel('Temperature [K]')
                ax.set_xlim(900,1800); ax.grid(True,alpha=.3); ax.legend(fontsize=7)
            fig.suptitle(f'{label} — {kind}'); fig.tight_layout()
            path=directory/f'{label}_{kind}_all_cases.png'; fig.savefig(path,dpi=180); plt.close(fig); supplemental.append(str(path))
        return {'standard_notebook_plots':[str(p) for p in standard], 'supplemental_plots':supplemental,
                'standard_count':len(standard),'supplemental_count':len(supplemental),
                'GF_benchmark':'No experimental/digitized Rgf/Ngf curves embedded in current source; diagnostics only.'}
    finally: model.savefig=original_savefig

def report(table,selected,selection,manifest,out):
    valid=table[table.state.eq('COMPLETE')].copy(); baseline=valid[valid.trial.eq(0)]
    parameters=list(manifest['search_ranges_log'])
    parts=['# Optuna_rho_constant_Rizk — calibrazione esplorativa',
           'Campagna conclusa al limite autorizzato di 60 trial. Tre strategie distinte sono disponibili per ispezione; non viene scelto un modello finale.',
           '## Configurazione e riproducibilità',
           f"Notebook corrente congelato (SHA-256 `{manifest['source_sha256']}`), conservato in `source_notebook.ipynb`; originale verificato invariato. Optuna {manifest['optuna_version']}, NSGA-II, seed {manifest['seed']}, popolazione {manifest['population_size']}, SQLite `study.db`. Ask/tell sequenziale; solo i punti fisici vengono distribuiti su {manifest['workers']} processi. Stato del sampler in `sampler.pkl`, checkpoint per trial e per mezzo pin. Lo stesso comando riprende lo studio senza superare 60 trial.",
           'Screening: 900–1800 K, passo 50 K, quattro pin; rerun dei soli A/B/C: passo normale 25 K. In entrambi dt=1 h, N_MODES=40, solver e tolleranze originali. rho_d è costante per tutta la simulazione e per ogni T/burnup. Nessuno switch, scala o legge fisica aggiuntiva.',
           'Verifica della baseline: i 76 punti comuni coincidono con il CSV esistente del notebook corrente su tutte le 129 colonne numeriche confrontate (`baseline_equivalence_validation.json`). Il ricalcolo indipendente e l’esclusione dei dati high-T dagli obiettivi sono verificati in `scoring_validation.json`.',
           'L’adattamento dell’interfaccia aggiunge la densità iniziale grain-face a Candidate/UNParameters e la inoltra all’inizializzazione esistente: nessuna equazione modificata. Gli altri adattamenti riguardano solo esportazione e directory/limiti dei grafici.',
           markdown(pd.DataFrame([{'parametro':k,'min':v[0],'max':v[1],'CURRENT_BASELINE':manifest['baseline'][k],'campionamento':'log'} for k,v in manifest['search_ranges_log'].items()])),
           markdown(pd.DataFrame([{'case':k,**v} for k,v in manifest['cases'].items()]),['case','burnup','fission_rate','linear_power_kW_m','fuel_diameter_mm']),
           f"Trial totali: {len(table)}; completati: {len(valid)}; falliti: {int(table.state.eq('FAIL').sum())}. Curve screening salvate: {len(pd.read_csv(out/'all_trial_curves.csv'))}; rerun: 3 × 148 punti.",
           '## Trial esclusi',
           markdown(table.loc[table.state.eq('FAIL')],['trial','solver_failures','nonfinite_values','negative_inventory_values','gas_balance_failures','gross_nonphysical_points']),
           'Il trial 26 è escluso per ANP6 a 1800 K: swelling totale 105.35%, con grain-face bubble radius di circa 10 µm superiore al raggio del grano 6 µm. Il solver termina e il gas balance è conservato, ma questo stato è palesemente fuori dalla geometria del modello. Le sue curve restano nel CSV; gli obiettivi ricalcolabili sono diagnostici e non lo rendono eleggibile.',
           '## Obiettivi fissi e dati',
           'J_swelling = media dei quattro RMSE P2 in punti percentuali, ciascuno normalizzato dal massimo sperimentale del proprio pin. Non si mescolano i punti di pin diversi. Il punto AP3.8 a 899 K è escluso perché esterno al dominio; restano 10/9/9/10 punti. Gli anchor vicino a 1600 K sono mostrati nei grafici standard, senza duplicarli nello score.',
           'E_R = RMSE dell’errore relativo del raggio; E_N = RMSE dell’errore log10 della densità. J_micro = 0.6 E_R + 0.4 E_N. Solo AP3.4 e T<=1600 K: 5 punti R, 17 punti N. I 4 punti R e 11 punti N sopra 1600 K sono diagnostiche separate e non entrano negli obiettivi.',
           'J_FGR in punti percentuali: media degli errori quadratici dei due pin a 1.1% FIMA, poi media con quelli ANP6 a 3.2%, infine radice. Le curve Rizk digitalizzate native a 25 K vengono confrontate mediante interpolazione del modello solo a 900–1800 K. Stessa ponderazione per gli errori separati delle cinque gas partition. Core = radice della media dei quadrati dei tre RMSE matrix/bulk/dislocation. FGR e partition Rizk sono benchmark di modello, non dati sperimentali. AP3.4 non ha benchmark Fig.9 a 1.3% e non viene confrontato con un burnup inventato.',
           'Non esiste uno score totale. I warning partition >10 pp e strong >20 pp, pressione, ordinamenti e geometria non modificano i tre obiettivi. FAIL è riservato a solver failure, valori principali non finiti, inventari negativi, bilancio gas oltre 0.1 pp o volume totale delle bolle >=100%. La geometria single-size resta un warning. Le verifiche numeriche utilizzano gli stati finali a ogni T; i guard/clipping interni del solver sono quelli originali.',
           '## CURRENT_BASELINE',
           markdown(baseline,['trial',*OBJECTIVES,'E_R','E_N','gas_partition_core_RMS','Rd_highT_RMSE_nm','Nd_highT_log10_RMSE','warnings']),
           '## Minimi separati e Pareto front']
    bestrows=[]
    for objective in OBJECTIVES:
        r=valid.loc[valid[objective].idxmin()]
        bestrows.append({'obiettivo_minimizzato':objective,'trial':int(r.trial),**{k:r[k] for k in OBJECTIVES+['gas_partition_core_RMS']}})
    parts.append(markdown(pd.DataFrame(bestrows)))
    pareto=valid[valid.pareto.eq(True)]
    parts.extend([f'Pareto front: {len(pareto)} trial non dominati. La dominanza usa soltanto i tre obiettivi, senza partition/pressione/ordering.',markdown(pareto,['trial',*OBJECTIVES,'E_R','E_N','gas_partition_core_RMS']),
                  '## Selezione di strategie diverse',
                  f"Pool competitivo: J_swelling <= {selection['swelling_limit']:.6g}, J_micro <= {selection['micro_limit']:.6g}, J_FGR <= {selection['FGR_limit_pp']:.6g} pp. Sono soglie separate di selezione, applicate dopo lo screening; nessuna modifica allo scoring. Pool: {selection['competitive_trial_numbers']}. Comprende punti Pareto e competitivi dominati. Nessun filtro sulla gas partition.",
                  'A rappresenta un buon compromesso: migliore swelling nel sottoinsieme con J_micro sotto la mediana del pool. B massimizza la distanza da A; C massimizza la distanza minima da A/B. La distanza euclidea usa tutte e sei le coordinate log10 normalizzate sui rispettivi intervalli [0,1].',
                  markdown(selected,['trial',*parameters,*OBJECTIVES,'E_R','E_N','gas_partition_core_RMS','pareto'])])
    distance=np.array(selection['normalized_log_parameter_distances'])
    for i,(letter,r) in enumerate(zip('ABC',selected.to_dict('records'))):
        positions={k:(np.log10(r[k])-np.log10(bounds[0]))/(np.log10(bounds[1])-np.log10(bounds[0])) for k,bounds in manifest['search_ranges_log'].items()}
        low=[k for k,z in positions.items() if z<.35]; high=[k for k,z in positions.items() if z>.65]
        strengths=[o for o in OBJECTIVES if r[o]<float(baseline.iloc[0][o])]
        tradeoffs=[o for o in OBJECTIVES if r[o]>=float(baseline.iloc[0][o])]
        parts.append(f"### CANDIDATE_{letter}: trial {r['trial']}")
        parts.append(f"Parametri relativamente bassi nel range logaritmico: {', '.join(low) or 'nessuno'}; alti: {', '.join(high) or 'nessuno'}. Migliora rispetto alla baseline: {', '.join(strengths) or 'nessun obiettivo'}; trade-off rispetto alla baseline: {', '.join(tradeoffs) or 'nessun obiettivo'}. Core partition {r['gas_partition_core_RMS']:.3f} pp. Selezione: {'rappresentante competitivo con buon compromesso microstrutturale' if i==0 else ('massima distanza da A nel pool competitivo' if i==1 else 'massima separazione minima da A/B nel pool competitivo')}.")
        parts.append('Distanze: '+', '.join(f"{letter}–{other}: {distance[i,j]:.3f}" for j,other in enumerate('ABC') if j!=i)+'.')
        component_improvements=[k for k in ['swelling_error_AP3p2','swelling_error_AP3p8','swelling_error_AP3p4','swelling_error_ANP6','E_R','E_N'] if r[k]<float(baseline.iloc[0][k])]
        parts.append('Miglioramenti di componenti specifiche rispetto alla baseline: '+(', '.join(component_improvements) or 'baseline di riferimento')+'.')
        if not r['pareto']:
            parts.append('Questo candidato è dominato negli obiettivi aggregati e viene mantenuto come alternativa fisica competitiva, non come miglioramento globale. Migliora il pin AP3.2 e la componente N_d low-T, ma paga soprattutto R_d low-T e gli altri pin. La distanza nel parameter space motiva l’ispezione visiva; non implica superiorità del fit.')
        full=pd.read_csv(out/'final_candidates'/f'CANDIDATE_{letter}'/'metrics.csv')
        parts.append(markdown(full,['label','source_trial',*OBJECTIVES,'E_R','E_N','gas_partition_core_RMS','Rd_highT_RMSE_nm','Nd_highT_log10_RMSE','gas_balance_max_abs_pp','p_gf_over_eq_max','warnings']))
        curves_full=pd.read_csv(out/'final_candidates'/f'CANDIDATE_{letter}'/'full_results.csv')
        parts.append(f'Raggio grain-face massimo: {curves_full.Rgf.max()*1e6:.3f} µm, rispetto al raggio del grano congelato {model.GRAIN_RADIUS*1e6:.1f} µm. Anche la scala geometrica, soprattutto C vicino a tale dimensione, resta da ispezionare.')
        parts.append(markdown(full,['label','swelling_error_AP3p2','swelling_error_AP3p8','swelling_error_AP3p4','swelling_error_ANP6','Rd_lowT_RMSE_nm','Nd_lowT_log10_RMSE','gas_FGR_RMSE_1p1FIMA_pp','gas_FGR_RMSE_3p2FIMA_pp']))
        parts.append(f"Grafici: [plots CANDIDATE_{letter}](final_candidates/CANDIDATE_{letter}/plots/). Risultati completi e parametri salvati nella stessa cartella. Le tre popolazioni R/N sono confrontate in unità coerenti (Ngf volumetrica); Ngf areale è conservata a parte.")
    if len(selected)==3:
        b=selected.iloc[1]; c=selected.iloc[2]
        parts.append(f'B e C hanno densità iniziali dislocation simili: K_d × rho_d = {b.K_d*b.rho_d:.4g} e {c.K_d*c.rho_d:.4g} m^-3. Differiscono però di {c.Dv_dislocation_scale/b.Dv_dislocation_scale:.2f} volte nella scala di trasporto delle vacanze sulla dislocation e di {b.NGF_AREAL_0/c.NGF_AREAL_0:.2f} volte nella densità grain-face iniziale. A usa K_d molto più basso e rho_d più alta, con f_n maggiore. È quindi un confronto fra strategie di densità iniziale, crescita e allocazione grain-face, non fra tre copie di uno stesso optimum.')
    parts.extend(['## Correlazioni, compensazioni e limiti dei range',
                  'Correlazioni di Spearman descrittive sui trial completati: non dimostrano causalità, dato che NSGA-II modifica più parametri contemporaneamente. Con soli 60 trial le degenerazioni sono indicazioni, non identificazioni univoche.'])
    logs=valid[parameters].apply(np.log10)
    correlations=[]
    for p in parameters:
        for o in OBJECTIVES+['E_R','E_N','gas_partition_core_RMS']:
            corr=logs[p].rank().corr(valid[o].rank())
            correlations.append({'parametro':p,'metrica':o,'rho_Spearman':corr})
    corr=pd.DataFrame(correlations); corr['abs']=corr.rho_Spearman.abs()
    writepath=out/'parameter_metric_correlations.csv'; corr.drop(columns='abs').to_csv(writepath,index=False)
    parts.append(markdown(corr.nlargest(12,'abs').drop(columns='abs')))
    pairrows=[]
    for p,q in [('K_d','rho_d'),('Dv_dislocation_scale','Dg_dislocation_scale'),('f_n','Dg_dislocation_scale')]:
        pool=valid[valid.trial.isin(selection['competitive_trial_numbers'])]
        pairrows.append({'coppia':f'{p} / {q}','Spearman_tutti':logs[p].rank().corr(logs[q].rank()),
                         'Spearman_pool_competitivo':np.log10(pool[p]).rank().corr(np.log10(pool[q]).rank())})
    parts.append(markdown(pd.DataFrame(pairrows)))
    parts.append('K_d × rho_d imposta la densità iniziale delle dislocation bubbles: diversi prodotti e diverse scale di trasporto possono distribuire il gas in modi differenti. Dv_dislocation controlla la crescita tramite vacanze; Dg_dislocation modifica il trasporto sulla linea. f_n altera nucleazione bulk e competizione per il gas. Le correlazioni campionate sopra segnalano le possibili compensazioni, senza attribuirle automaticamente a un unico meccanismo.')
    boundaries=[]
    for p,(lo,hi) in manifest['search_ranges_log'].items():
        z=(np.log10(valid[p])-np.log10(lo))/(np.log10(hi)-np.log10(lo))
        boundaries.append({'parametro':p,'entro_10%_log_limite_basso':int((z<=.1).sum()),'entro_10%_log_limite_alto':int((z>=.9).sum()),'min_campionato':valid[p].min(),'max_campionato':valid[p].max()})
    parts.append(markdown(pd.DataFrame(boundaries)))
    radius_points=pd.DataFrame([p for p in model.EXP_RD_T_13 if p['T']<=1600])
    density_points=pd.DataFrame([p for p in model.EXP_ND_T_13 if p['T']<=1600])
    swelling_points=pd.DataFrame(model.experimental_swelling_points_for_case('AP3.4'))
    consistency=density_points[(density_points['T']>=radius_points['T'].min()) & (density_points['T']<=radius_points['T'].max())].copy()
    consistency['interpolated_R_exp_nm']=np.interp(consistency['T'],radius_points['T'],radius_points.R_nm)
    consistency['interpolated_swelling_exp_pp']=np.interp(consistency['T'],swelling_points['T'],swelling_points.swelling)
    consistency['single_size_spherical_proxy_swelling_pp']=100*consistency.N*4*np.pi/3*(consistency.interpolated_R_exp_nm*1e-9)**3
    consistency['proxy_over_experimental_swelling']=consistency.single_size_spherical_proxy_swelling_pp/consistency.interpolated_swelling_exp_pp
    consistency.to_csv(out/'experimental_micro_swelling_consistency.csv',index=False)
    parts.extend(['## Tensione tra swelling e microstruttura',
        f'Una ricostruzione diagnostica con la relazione sferica single-size già usata dal modello, 100 × N_exp × (4π/3) × R_exp³, usando R interpolato solo entro il dominio dei dati <=1600 K, dà swelling da {consistency.proxy_over_experimental_swelling.min():.2f} a {consistency.proxy_over_experimental_swelling.max():.2f} volte la curva P2 sperimentale interpolata. Si veda experimental_micro_swelling_consistency.csv. Non è un nuovo obiettivo né una nuova equazione nel solver. È un indizio di tensione fra target quando rappresentati con una popolazione single-size, non una prova di inconsistenza sperimentale: punti non coincidenti, definizioni statistiche del raggio/densità e distribuzioni reali possono differire. Questo aiuta a interpretare il trade-off osservato: migliorare R/N non garantisce migliorare lo swelling.'])
    parts.extend(['## Gas partition, FGR e diagnostiche',
                  f"Core partition: min {valid.gas_partition_core_RMS.min():.3f}, mediana {valid.gas_partition_core_RMS.median():.3f}, max {valid.gas_partition_core_RMS.max():.3f} pp. Warning >10: {int(valid.partition_warning.sum())}; strong >20: {int(valid.strong_partition_warning.sum())}. Nessuno di questi warning ha causato esclusione automatica. FGR RMSE: min {valid.J_FGR.min():.3f}, mediana {valid.J_FGR.median():.3f}, max {valid.J_FGR.max():.3f} pp.",
                  markdown(valid[['gas_matrix_RMSE_pp','gas_bulk_RMSE_pp','gas_dislocation_RMSE_pp','gas_grainface_RMSE_pp','gas_FGR_RMSE_pp']].agg(['min','median','max']).reset_index().rename(columns={'index':'statistica'})),
                  f"Bilancio massimo assoluto sui trial completi: {valid.gas_balance_max_abs_pp.max():.6g} pp. Errore relativo massimo della rho costante: {valid.rho_constant_max_relative_error.max():.6g}. Valori non finiti principali: {int(valid.nonfinite_values.sum())}; inventari negativi: {int(valid.negative_inventory_values.sum())}.",
                  f"High-T R_d sottostimato in media in {int((valid.Rd_highT_mean_signed_nm<0).sum())}/{len(valid)} trial; high-T N_d sovrastimata in media in {int((valid.Nd_highT_mean_log10_bias>0).sum())}/{len(valid)} trial. Le curve non vengono forzate a eliminare questo comportamento strutturale. La dispersione di N_d sperimentale e l’evoluzione high-T non sono un quarto obiettivo.",
                  f"p_gf/p_eq massimo: mediana dei massimi {valid.p_gf_over_eq_max.median():.6g}, massimo {valid.p_gf_over_eq_max.max():.6g}; baseline {float(baseline.iloc[0].p_gf_over_eq_max):.6g}. Questi valori sono segnalati come limite del modello e non usati per scegliere il fit. Le pressioni assolute e i rapporti sono salvati per tutti i punti."])
    ordering=[]
    for case in model.EXPERIMENT_CASE_ORDER:
        key=case.replace('.','p')
        ordering.append({'case':case,'trial_con_Rgf>Rd>Rb_violato_persistente':int((valid[f'radius_order_longest_run_{key}']>=3).sum()),
                         'trial_con_Ngf<Nd<Nb_violato_persistente':int((valid[f'density_order_longest_run_{key}']>=3).sum())})
    parts.append(markdown(pd.DataFrame(ordering)))
    parts.extend(['Persistente significa almeno tre temperature consecutive nello screening. Ngf < Nd < Nb usa tutte densità volumetriche. Violazioni dei raggi a basse T e pressioni grain-face elevate possono persistere anche nella baseline: restano guardrail per l’ispezione fisica. Non esistono curve numeriche Rgf/Ngf sperimentali o Rizk integrate nel notebook corrente, quindi non sono inventati RMSE per tali popolazioni.',
                  '## Artefatti e ricalcolo offline',
                  '`all_trial_curves.csv` contiene tutti i parametri, stati finali, gas, pressioni, numeri/raggi, swelling e diagnostiche per ogni trial/pin/T. `trial_metrics.csv`, `trials.csv` e `pareto_front.csv` conservano metriche separate. Gli esperimenti e la Fig.9 limitata al dominio sono esportati in CSV. `manifest.json` documenta formule, unità, pesi, impostazioni congelate e versione; i moduli ausiliari contengono scorer e plotting.',
                  'Ricalcolo senza solver: `.venv/bin/python UN_model/optuna/Optuna_rho_constant_Rizk.py --rescore`. Produce `offline_rescored_metrics.csv` e `offline_rescored_pareto.csv` senza sovrascrivere la campagna. Il normale comando riprende checkpoint/studio e resta limitato a 60 trial. La selezione e i rerun esistenti vengono riutilizzati.',
                  'Lo studio si ferma qui. Range, fisica e scoring restano quelli dichiarati. Nessun refinement, secondo studio o scelta definitiva viene eseguito.'])
    curves=pd.read_csv(out/'all_trial_curves.csv')
    inactive=['Dg_lookup_base','Dv_lookup_base','Dv_nonthermal_effective','A20_vU_active']
    diagnostic_rows=[]
    for number, sub in curves.groupby('trial'):
        columns=[c for c in sub.select_dtypes('number') if c not in ['error','traceback']]
        values=sub[columns].to_numpy(float)
        nan_columns=[c for c in columns if sub[c].isna().any()]
        inf_columns=[c for c in columns if np.isinf(sub[c].to_numpy(float)).any()]
        diagnostic_rows.append({'trial':int(number),'all_numeric_nan_count':int(np.isnan(values).sum()),
            'all_numeric_inf_count':int(np.isinf(values).sum()),'nan_columns':';'.join(nan_columns),
            'inactive_nan_columns':';'.join(c for c in nan_columns if c in inactive),
            'unexpected_nan_columns':';'.join(c for c in nan_columns if c not in inactive),
            'inf_columns':';'.join(inf_columns)})
    diagnostics=pd.DataFrame(diagnostic_rows); diagnostics.to_csv(out/'numerical_diagnostics.csv',index=False)
    # Keep finite objective diagnostics even for a physically rejected trial; never change SQLite status.
    from Optuna_rho_constant_Rizk_metrics import evaluate as fixed_evaluate
    rejected_diagnostics={}
    for number in table.loc[table.state.eq('FAIL'),'trial']:
        sub=curves[curves.trial.eq(number)]
        rejected_diagnostics[int(number)]=fixed_evaluate(sub)
    if rejected_diagnostics:
        pd.DataFrame([{'trial':n,**m} for n,m in rejected_diagnostics.items()]).to_csv(out/'failed_trial_diagnostic_metrics.csv',index=False)
    parts.append('I trial FAIL restano esclusi da Pareto/selezione. Per quelli con curve finite, le stesse formule degli obiettivi e dei benchmark vengono comunque riportate a scopo diagnostico (failed_trial_diagnostic_metrics.csv); lo stato e i valori Optuna non vengono modificati.')
    for file in ['trial_metrics.csv','trials.csv','pareto_front.csv']:
        ledger=pd.read_csv(out/file)
        for number, metric in rejected_diagnostics.items():
            for key,value in metric.items():
                if key not in ledger: ledger[key]=np.nan if not isinstance(value,str) else ''
                ledger.loc[ledger.trial.eq(number),key]=value
        ledger=ledger.drop(columns=[c for c in diagnostics.columns if c!='trial' and c in ledger])
        ledger=ledger.merge(diagnostics,on='trial',how='left')
        ledger.to_csv(out/file,index=False)
    parts.append('I conteggi NaN/inf di tutte le colonne numeriche sono salvati per ciascun trial in `numerical_diagnostics.csv` e nelle tabelle dei trial. I quattro campi Dg_lookup_base, Dv_lookup_base, Dv_nonthermal_effective e A20_vU_active sono placeholder NaN per rami di diffusività inattivi, già presenti nella baseline originale; non rappresentano un fallimento del solver. Le colonne vuote error/traceback non sono inventari numerici.')
    parts.append(f'Inf in tutte le colonne numeriche: {int(diagnostics.all_numeric_inf_count.sum())}; trial con NaN inattesi: {int(diagnostics.unexpected_nan_columns.ne("").sum())}.')
    (out/'findings.md').write_text('\n\n'.join(parts)+'\n')
