#!/usr/bin/env python3
"""Collect accuracy data and descriptive comparisons; no acceptance thresholds."""
import sys
sys.dont_write_bytecode=True
import csv,gzip,json,time,shutil
from collections import Counter
from core import *
from scipy.stats import spearmanr


def append_csv(paths,destination,compressed=False):
    opener=gzip.open if compressed else open
    with opener(destination,'wt',newline='') as out:
        first=True
        for path in paths:
            read=gzip.open if path.suffix=='.gz' else open
            with read(path,'rt',newline='') as src:
                header=src.readline()
                if first:out.write(header);first=False
                shutil.copyfileobj(src,out)


def main():
    guard();start=time.perf_counter();out=HERE/'deliverables';out.mkdir(exist_ok=False)
    selection=json.loads((HERE/'selection.json').read_text())['cases']
    byname={c['name']:c for c in CASES};peaks=[];combined=[];scales=[];timings=[];coverage=[];regimes=[]
    append_csv([HERE/'baseline'/c['name']/'temporal_errors_hourly.csv.gz' for c in CASES],out/'temporal_errors_hourly.csv.gz',True)
    append_csv([HERE/'baseline'/c['name']/'temporal_hourly_lossless.csv' for c in CASES],out/'temporal_hourly_lossless.csv')
    append_csv([HERE/'baseline'/c['name']/'temporal_selected.csv' for c in CASES],out/'temporal_errors_common11.csv')
    for c in CASES:
        folder=HERE/'baseline'/c['name'];m=json.loads((folder/'completed.json').read_text())
        peaks+=load_csv(folder/'maxima.csv');combined+=load_csv(folder/'spatial_total_errors.csv')
        for j,pop in enumerate(POPS):scales.append({'case':c['name'],'group':c['group'],'population':pop,
            'temporal_N40_scale':m['temporal_scale'][j],'highN_scale':m['highN_scale'][j],
            'reference_uncertainty_estimate':m['reference_uncertainty_estimate'][j],
            'relative_defined_rule':'abs(reference)>max(1,20*uncertainty)','scale_units':'at/m3'})
        r1=decay_rates(c,1);r40=decay_rates(c,40);k1=c['D']*(np.pi/c['R'])**2
        regimes.append({'case':c['name'],'group':c['group'],**{k:c[k] for k in ['D','gb','gd','bb','bd','R']},
            'tau_fast_N40_s':float(1/r40[-1]),'tau_mid_N1_s':float(1/r1[1]),'tau_slow_N1_s':float(1/r1[0]),
            'hour_over_tau_fast_N40':float(3600*r40[-1]),'hour_over_tau_mid_N1':float(3600*r1[1]),
            'trapping_over_diffusion_N1':float((c['gb']+c['gd'])/k1),'bulk_equilibrium_ratio':c['gb']/c['bb'],
            'dislocation_equilibrium_ratio':c['gd']/c['bd'],
            'source_history':c['provenance'].get('history',''),'source_step':c['provenance'].get('step','')})
        timings.append({'case':c['name'],'stage':'baseline_analysis','wall_seconds':m['wall_seconds'],
            'new_cpp_runs':0,'original_solver_seconds':m['cpp']['solver_seconds'],
            'note':'original candidate solver timing; source reused, no new simulation'})
        n_new=0
        if c['name'] in selection:
            f=HERE/'targeted'/c['name'];peaks+=load_csv(f/'candidate_enriched_maxima.csv')
            combined+=load_csv(f/'candidate_enriched_errors.csv');r=json.loads((f/'completed.json').read_text())
            n_new=r['new_high_observations'];timings.append({'case':c['name'],'stage':'targeted','wall_seconds':r['wall_seconds'],
                'new_cpp_runs':r['new_cpp_runs'],'original_solver_seconds':'','note':'pilot may reuse completed raw numerical work after CSV export fix'})
        coverage.append({'case':c['name'],'group':c['group'],'temporal_hours':4001,'spatial_total_common_times':11,
            'additional_spatial_total_integer_hours':n_new,'candidate_values_inside_hour_assigned':False,
            'temporal_grid':'every integer hour 0..4000','common_grid_hours':';'.join(map(str,OBS_HOURS)),
            'enriched_grid_file':f'targeted/{c["name"]}/high_reference/convergence.json' if n_new else '',
            'continuous_maximum_proved':False})
    table(out/'observed_maxima.csv',peaks);table(out/'decomposition_errors.csv',combined)
    table(out/'spatial_errors.csv',[r for r in combined if r['error_kind']=='spatial'])
    table(out/'total_errors.csv',[r for r in combined if r['error_kind']=='total'])
    table(out/'normalization_scales.csv',scales);table(out/'sampling_coverage.csv',coverage)
    table(out/'coefficient_regimes.csv',regimes)
    append_csv([HERE/'targeted'/n/'refinement_maxima.csv' for n in selection],out/'targeted_refinement_maxima.csv')
    append_csv([HERE/'targeted'/n/'refinement_errors.csv' for n in selection],out/'targeted_refinement_errors.csv')
    costs=[]
    for name in selection:costs+=load_csv(HERE/'targeted'/name/'costs.csv')
    table(out/'targeted_costs.csv',costs)
    rapid=json.loads((HERE/'rapid_selection.json').read_text())['cases']
    for file in ['short_errors.csv','short_maxima.csv','first_hour_comparison.csv']:
        append_csv([HERE/'rapid'/n/file for n in rapid],out/('rapid_'+file))
    for name in rapid:
        r=json.loads((HERE/'rapid'/name/'completed.json').read_text())
        timings.append({'case':name,'stage':'rapid_diagnostics','wall_seconds':r['wall_seconds'],
            'new_cpp_runs':r['new_cpp_runs'],'original_solver_seconds':'','note':'three short windows and one fine integration to 3600 s'})
    table(out/'timings.csv',timings)
    # Fair group statistics retain the common grid for all 160 cases.
    fair=[r for r in peaks if r['sampling_scope'] in ['all_4001_hourly','common_11_verified']]
    worst=[];stats=[];regby={r['case']:r for r in regimes}
    for group in ['LHS','UN_history']:
        for pop in POPS:
            for kind in ['temporal','spatial','total']:
                for metric in ['absolute','relative_instantaneous','normalized_population_scale']:
                    rows=[r for r in fair if r['group']==group and r['population']==pop and r['error_kind']==kind
                         and r['metric']==metric and r['status']=='observed_discrete_maximum' and r['maximum_observed'] not in ['',None]]
                    rows=sorted(rows,key=lambda r:float(r['maximum_observed']),reverse=True)
                    values=np.array([float(r['maximum_observed']) for r in rows])
                    stats.append({'group':group,'population':pop,'error_kind':kind,'metric':metric,'active_cases':len(rows),
                        'median_of_case_maxima':float(np.median(values)),'p90_of_case_maxima':float(np.quantile(values,.9)),
                        'p95_of_case_maxima':float(np.quantile(values,.95)),'maximum':float(values.max()),
                        'sampling_scope':rows[0]['sampling_scope']})
                    for rank,row in enumerate(rows[:3],1):worst.append({**row,'rank_within_group_population':rank,**regby[row['case']]})
    table(out/'worst_cases_common_sampling.csv',worst);table(out/'group_statistics_common_sampling.csv',stats)
    # Include the best observed total after densification, clearly labelled:
    # sampling is now denser only in the preselected critical subset.
    best=[]
    for group in ['LHS','UN_history']:
        for pop in POPS:
            for metric in ['absolute','relative_instantaneous','normalized_population_scale']:
                rows=[r for r in peaks if r['group']==group and r['population']==pop and r['error_kind']=='total'
                     and r['metric']==metric and r['status']=='observed_discrete_maximum' and r['maximum_observed'] not in ['',None]]
                row=max(rows,key=lambda r:float(r['maximum_observed']))
                best.append({**row,**regby[row['case']],'ranking_limitation':'observed only; densification restricted to critical subset'})
    table(out/'worst_total_after_targeted_sampling.csv',best)
    associations=[]
    features=['D','gb','gd','bb','bd','tau_fast_N40_s','tau_mid_N1_s','trapping_over_diffusion_N1',
              'bulk_equilibrium_ratio','dislocation_equilibrium_ratio']
    for group in ['LHS','UN_history']:
        for pop in POPS:
            for kind in ['temporal','spatial','total']:
                for metric in ['relative_instantaneous','normalized_population_scale']:
                    rows=[r for r in fair if r['group']==group and r['population']==pop and r['error_kind']==kind
                         and r['metric']==metric and r['status']=='observed_discrete_maximum' and r['maximum_observed'] not in ['',None]]
                    y=np.array([float(r['maximum_observed']) for r in rows])
                    for feature in features:
                        x=np.array([regby[r['case']][feature] for r in rows])
                        rho=float(spearmanr(x,y).statistic) if np.ptp(x)>0 and np.ptp(y)>0 else None
                        associations.append({'group':group,'population':pop,'error_kind':kind,'metric':metric,
                            'coefficient_or_timescale':feature,'cases':len(rows),'spearman_rho':rho,
                            'interpretation':'descriptive association, not causal or independent parameter attribution'})
    table(out/'coefficient_associations.csv',associations)
    candidate=(SOURCE/'combinations_160.csv').read_bytes();(out/'combinations_160.csv').write_bytes(candidate)
    # Descriptive report tables; no 1%/3% acceptance classification.
    lines=[]
    for group in ['LHS','UN_history']:
        for pop in POPS:
            row=next(r for r in best if r['group']==group and r['population']==pop and r['metric']=='relative_instantaneous')
            lines.append(f'| {group} | {pop} | {row["case"]} | {100*float(row["maximum_observed"]):.5g}% | {float(row["hour"]):g} | {row["sampling_scope"]} |')
    refine=load_csv(out/'targeted_refinement_maxima.csv')
    example=[]
    for name,pop in [('LHS_055','md'),('UN_AP3p2_900K_S3','c')]:
        for r in refine:
            if r['case']==name and r['population']==pop and r['error_kind']=='total' and r['metric']=='relative_instantaneous':
                example.append(f'| {name} | {pop} | {r["N"]} | {r["dt_s"]} | {100*float(r["maximum_observed"]):.5g}% | {float(r["hour"]):g} | {float(r["improvement_factor"]):.4g} |')
    mismatches=[r for r in combined if abs(float(r['identity_residual']))>0]
    firsthour=load_csv(out/'rapid_first_hour_comparison.csv')
    report='''# Accuratezza della configurazione N=40, Δt=3600 s

Analizzati i 160 casi congelati (128 LHS e 32 stati UN), con sorgente, iniziali, coefficienti costanti e durata 4000 h invariati. La campagna estesa non è stata ripetuta. Per il temporale sono usate tutte le 4001 osservazioni orarie; spazio e totale usano inizialmente gli undici tempi verificati e, nei nove casi critici, ore intermedie aggiuntive.

## Massimi relativi totali osservati

| Gruppo | Popolazione | Caso | Errore relativo istantaneo | Tempo [h] | Griglia |
|---|---|---|---:|---:|---|
'''+ '\n'.join(lines)+'''

Questi sono massimi discreti osservati, non massimi continui rigorosi. Le griglie sono più dense solo nei casi presele­zionati; i confronti statistici uniformi sui 160 casi restano separati in worst_cases_common_sampling.csv e group_statistics_common_sampling.csv. Nessuna soglia automatica dell'1% o del 3% è stata applicata.

## Separazione degli errori

La decomposizione con segno è `Cpp − highN = (Cpp − exact_N) + (exact_N − highN)`. Il temporale orario usa la cache expm N40 validata; nei confronti condivisi si usa il prefisso N40 della stessa reference diretta high-N, come nella 4B. La differenza di formulazione, di solo roundoff, è riportata esplicitamente come reference_form_gap e non è interpretata come nuova fisica. Tutte le reference originali restano immutate. L'errore spaziale confronta esclusivamente soluzioni analitiche. La dominanza è valutata sulle componenti assolute nelle stesse unità, prima della normalizzazione.

Le percentuali istantanee hanno per denominatore il riferimento della popolazione allo stesso tempo; sono diverse dagli errori sulla scala massima di storia. Percentuali non definite sono vuote e segnalate, mentre gli assoluti rimangono disponibili. Le scale e la stima di sensibilità numerica sono in normalization_scales.csv. Le otto popolazioni bulk strutturalmente inattive non ricevono percentuali fittizie.

## Raffinamenti mirati: esempi sul totale

| Caso | Pop. | N | Δt [s] | Max relativo osservato | Tempo [h] | Fattore miglioramento |
|---|---|---:|---:|---:|---:|---:|
'''+ '\n'.join(example)+'''

Tutte le configurazioni dello stesso caso sono confrontate sulla stessa griglia high-N arricchita; i temporali sono valutati separatamente sulle 4001 ore. Il fattore è errore massimo candidato / errore massimo raffinato sulla stessa griglia e con lo stesso denominatore. Un valore inferiore a uno segnala un totale maggiore, che può derivare dalla rimozione di una compensazione fra errore temporale e spaziale di segno opposto. Non è un motivo per alterare dati o fisica. Aumentare N agisce sul troncamento; dimezzare Δt sul Backward Euler. I residui a N160 sono conservati senza estendere i test oltre N160 e senza dichiarare sufficienza in assenza di una soglia concordata.

## Coefficienti, regimi e campionamento

worst_cases_common_sampling.csv riporta i coefficienti effettivi dei casi peggiori, distinti per gruppo e popolazione. coefficient_regimes.csv aggiunge scale rapide, intermedie e lente, rapporti di trapping/risoluzione e diffusione. coefficient_associations.csv fornisce correlazioni di rango descrittive; i 32 stati UN sono correlati lungo le storie e non permettono attribuzioni causali indipendenti ai singoli coefficienti. Le associazioni non vengono trasformate in una scelta definitiva di N o Δt.

LHS_055/md evidenzia un errore temporale positivo parzialmente compensato dal troncamento negativo: più modi possono quindi aumentare l'errore totale mentre ne diminuiscono la componente spaziale. I casi UN critici mostrano che una percentuale iniziale elevata può essere poco visibile nella normalizzazione sulla concentrazione massima a 4000 h. Il tempo rapido spettrale da solo non descrive tutte le popolazioni: sono presenti anche induzioni e scale intermedie dell'exchange.

## Transitori rapidi

Sono eseguite prove brevi su tre casi esistenti: il peggiore relativo temporale del gruppo veloce LHS, il peggiore del gruppo veloce UN e il caso con τ più rapido. Si usa T=20τ con dt=τ/10,/20,/40 e una integrazione risolta separata fino a 3600 s con dt=τ/20. Le prove brevi e quelle di 4000 h hanno scale di normalizzazione distinte. La selezione operativa veloce è 3600/τ≥20, senza usarla come soglia di accettazione dell'accuratezza.

Non si attribuisce alcun valore candidato dentro il primo timestep: il candidato ha output a 0 e 3600 s, non nei tempi interni delle prove brevi. Le concentrazioni risolte e quelle analitiche mostrano l'evoluzione non campionata dal candidato, ma non definiscono un suo errore continuo. Il confronto quantitativo del primo passo è fatto a 3600 s per entrambe le soluzioni. Gli scarti di campionamento float delle prove brevi sono controllati entro 2e-12 della loro durata, come nel protocollo 4B.

## Riproducibilità e anomalie operative

Gli output N40 a 3600/1800/900 s sono riutilizzati; sono nuovi soltanto N80/N160 a 3600 s sui nove casi e le dodici prove rapide/di prima ora. I numeri di modi non sono estesi oltre 160 nei confronti C++. Le reference high-N aggiuntive sono calcolate solo ai tempi nuovi e controllate con i due criteri 4B sui raddoppi dei prefissi modali. Non sono rigenerate tutte le reference high-N della campagna precedente.

Il primo benchmark mirato si è interrotto nell'esportazione dei metadati CSV, perché i record nuovi e quelli riutilizzati avevano colonne diverse. È stata corretta l'unione delle colonne e riutilizzato ogni output numerico già completo, senza ripetere C++. I tempi originali delle componenti restano conservati. Nessun problema scientifico ha richiesto modifiche di equazioni o coefficienti.

I CSV temporal_errors_hourly.csv.gz conservano l'analisi completa a ogni ora; per lo ZIP la stessa informazione numerica è resa compatta in temporal_hourly_lossless.csv con riferimenti ed errori con segno delle tre popolazioni e maschera dei relativi definiti. I massimi, le scale e le formule permettono di ricostruire esattamente assoluti, relativi e normalizzati senza i binari. La compattezza non elimina osservazioni orarie. I nuovi grezzi e tutte le reference restano separati. Nessun grafico o conclusione definitiva sulla scelta N/dt è prodotto.
'''
    (out/'REPORT_ACCURATEZZA.md').write_text(report)
    result={'status':'completed','cases':160,'candidate_N':40,'candidate_dt_s':3600,'main_end_s':END,
        'source_cases_sha256':rv.sha(SOURCE/'cases.json'),'source_combinations_sha256':rv.sha(SOURCE/'combinations_160.csv'),
        'temporal_hourly_observations':160*4001*3,'lossless_wide_hourly_rows':160*4001,
        'critical_cases':selection,'rapid_cases':rapid,'new_cpp_4000h_runs':2*len(selection),'new_short_or_bridge_cpp_runs':4*len(rapid),
        'unique_existing_main_cpp_reused':160+2*len(selection),'no_new_samples':True,
        'baseline_analysis_seconds':sum(json.loads((HERE/'baseline'/c['name']/'completed.json').read_text())['wall_seconds'] for c in CASES),
        'targeted_component_timings_note':'pilot export was retried with cached raw outputs; original component timings retained',
        'collection_seconds':time.perf_counter()-start,'elapsed_budget_seconds':time.time()-json.loads((HERE/'budget.json').read_text())['start_epoch'],
        'max_reference_form_gap_normalized':max(json.loads((HERE/'baseline'/c['name']/'completed.json').read_text())['max_reference_form_gap_normalized'] for c in CASES),
        'full_hourly_long_CSV_local':'temporal_errors_hourly.csv.gz','compact_ZIP_full_hourly_data':'temporal_hourly_lossless.csv',
        'relative_rule':'abs(analytic)>max(1,20*estimated reference uncertainty); otherwise no ratio, absolute kept',
        'reference_normalizations':'temporal candidate original N40 history maximum; spatial/total candidate high-N scale; refinement comparisons common candidate high-N scale',
        'no_acceptance_thresholds_1pct_or_3pct':True,'no_continuous_maximum_claim':True,'no_candidate_subhour_interpolation':True}
    rv.save(out/'metadata.json',result);rv.save(HERE/'summary.json',result)
    print(json.dumps(result,indent=2),flush=True)


if __name__=='__main__':main()
