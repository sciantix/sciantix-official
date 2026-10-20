#!/usr/bin/env python3
"""Collect compact CSV tables and a brief technical record, without plots."""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import csv
import json
import time
import argparse
from collections import Counter
from campaign_core import HERE, ROOT, OLD, rv, np, OBS_HOURS, DTS, NS, POPS, result_file


def load(path):
    with open(path,newline='') as f:return list(csv.DictReader(f))


def table(path,rows):
    if not rows:return
    with open(path,'x',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--output',type=Path,default=HERE/'deliverables')
    out=parser.parse_args().output.resolve();assert out.is_relative_to(HERE)
    start=time.perf_counter();cases=json.loads((HERE/'cases.json').read_text())
    completed=[c for c in cases if (HERE/'raw'/c['name']/'completed.json').exists()]
    progress=json.loads((HERE/'progress.json').read_text())
    if len(completed)<160:assert progress['status']=='budget_stop','Do not collect a running campaign'
    out.mkdir(exist_ok=False)
    totals={};records=[];references=[];timings=[]
    names=['temporal_errors.csv','temporal_orders.csv','spatial_errors.csv','spatial_orders.csv',
           'implementation_checks.csv','decomposition_N40.csv','temporal_hourly_summary.csv']
    for name in names:
        with open(out/name,'x',newline='') as dest:
            writer=None;count=0
            for c in completed:
                path=result_file(c['name'],name)
                with open(path,newline='') as src:
                    reader=csv.DictReader(src)
                    if writer is None:writer=csv.DictWriter(dest,fieldnames=reader.fieldnames);writer.writeheader()
                    for r in reader:writer.writerow(r);count+=1
            totals[name]=count
    reviews=[];population=[];anomalies=[]
    order_counts=Counter()
    for c in completed:
        base=HERE/'raw'/c['name'];record=json.loads(result_file(c['name'],'completed.json').read_text());records.append(record)
        timing={k:record[k] for k in ['case','group','wall_seconds','main_temporal_runs','spatial_analytic_configurations',
            'extra_temporal_refinements','finest_dt_s','solver_seconds_new','cpp_process_seconds_new','reference_acquire_seconds',
            'reference_N','cpp_reused_4B','reference_reused_4B','max_BE_gap_normalized']}
        timings.append(timing)
        ref=json.loads((base/'reference_manifest.json').read_text())
        references.append({'case':c['name'],'N_ref':ref['N_high'],**ref['high_convergence'],
            'hourly_file':ref['hourly_file'],'hourly_sha256':ref['hourly_sha256'],
            'high_file':ref['high_file'],'high_sha256':ref['high_sha256'],
            'reused_4B':ref['reused_from_4B']})
        orders=load(result_file(c['name'],'temporal_orders.csv'))
        for pop in POPS:
            rows=[]
            for hour in OBS_HOURS[1:]:
                a=next(r for r in orders if r['population']==pop and float(r['hour'])==hour and float(r['fine_dt_s'])==225.)
                b=next(r for r in orders if r['population']==pop and float(r['hour'])==hour and float(r['fine_dt_s'])==record['finest_dt_s'])
                review={'case':c['name'],'group':c['group'],'population':pop,'hour':hour,
                    'main_p_450_to225':a['p_log2_errors'],'main_status':a['status'],
                    'final_coarse_dt_s':b['coarse_dt_s'],'final_fine_dt_s':b['fine_dt_s'],
                    'final_p':b['p_log2_errors'],'final_status':b['status'],
                    'extra_halvings':record['extra_temporal_refinements'],
                    'final_coarse_error':b['coarse_absolute_error'],'final_fine_error':b['fine_absolute_error'],
                    'precision_screen_absolute':b['precision_screen_absolute'],
                    'final_coarse_signed_error':b['coarse_signed_error'],'final_fine_signed_error':b['fine_signed_error'],
                    'final_fine_dt_over_tau_fast':b['fine_dt_over_tau_fast']}
                reviews.append(review);rows.append(review);order_counts[b['status']]+=1
                if a['status'] not in ['first_order_observed','inactive_population'] or b['status'] not in ['first_order_observed','inactive_population']:
                    anomalies.append(review)
            counts=Counter(r['final_status'] for r in rows)
            if counts['inactive_population']==len(rows):status='inactive_population'
            elif counts['first_order_observed'] and sum(counts[s] for s in ['first_order_observed','precision_limit'])==len(rows):
                status='first_order_on_all_numerically_resolvable_times'
            elif counts['precision_limit']==len(rows):status='precision_limited_order_not_confirmed'
            else:status='not_first_order_at_all_resolvable_fixed_times'
            population.append({'case':c['name'],'group':c['group'],'population':pop,'assessment':status,
                'fixed_positive_times':len(rows),'first_order_times':counts['first_order_observed'],
                'precision_limited_times':counts['precision_limit'],'inactive_times':counts['inactive_population'],
                'cancellation_times':counts['cancellation_or_error_competition'],
                'preasymptotic_or_unresolved_times':sum(counts[s] for s in
                    ['unresolved_transient_or_preasymptotic','preasymptotic_not_yet_first_order']),
                'extra_halvings':record['extra_temporal_refinements'],'finest_dt_s':record['finest_dt_s']})
    table(out/'temporal_order_review.csv',reviews)
    table(out/'temporal_population_assessment.csv',population)
    table(out/'temporal_anomalies.csv',anomalies)
    table(out/'reference_validation.csv',references)
    table(out/'timings_per_case.csv',timings)
    table(out/'combinations_160.csv',load(HERE/'combinations_160.csv'))
    main_errors=[r for r in load(out/'temporal_errors.csv') if r['role']=='main']
    extra=sum(r['extra_temporal_refinements'] for r in records)
    budget=json.loads((HERE/'budget.json').read_text())
    full_history=load(HERE/'inputs/history_coefficients_complete.csv')
    fixed_config=json.loads((HERE/'configuration.json').read_text())
    complete_history_outside=sum(any(not lo<=float(row[k])<=hi for k,(lo,hi) in fixed_config['bounds'].items()) for row in full_history)
    meta={'status':'completed' if len(completed)==160 else 'budget_stop_partial',
        'requested_cases':160,'completed_cases':len(completed),'missing_cases':[c['name'] for c in cases if c not in completed],
        'lhs_cases':sum(c['group']=='LHS' for c in completed),'history_cases':sum(c['group']=='UN_history' for c in completed),
        'main_temporal_configurations':len(completed)*5,'spatial_analytic_configurations':len(completed)*6,
        'additional_temporal_refinements':extra,'total_temporal_cpp_configurations':len(completed)*5+extra,
        'cpp_4B_reuses':sum(r['cpp_reused_4B'] for r in records),'reference_4B_reuses':sum(r['reference_reused_4B'] for r in records),
        'cpp_solver_seconds_new':sum(r['solver_seconds_new'] for r in records),
        'cpp_process_seconds_new':sum(r['cpp_process_seconds_new'] for r in records),
        'sum_case_wall_seconds':sum(r['wall_seconds'] for r in records),
        'reference_acquire_seconds':sum(r['reference_acquire_seconds'] for r in records),
        'budget_limit_seconds':5400,'elapsed_since_first_benchmark_guard_s':time.time()-budget['start_epoch'],
        'max_BE_implementation_gap_normalized':max(r['max_BE_gap_normalized'] for r in records),
        'reference_N_counts':dict(Counter(r['reference_N'] for r in records)),
        'finest_fixed_time_order_status_counts':dict(order_counts),
        'population_assessment_counts':dict(Counter(r['assessment'] for r in population)),
        'csv_row_counts':totals,'primary_main_temporal_rows':len(main_errors),
        'first_order_screening_interval':[.9,1.1],'fixed_observation_hours':OBS_HOURS,
        'raw_temporal_observations_per_run':4001,'compact_bundle_scope':'fixed physical observations plus hourly maxima, all cases; full raw hourly means/modes/errors kept separately',
        'undefined_relative_errors':'blank when abs(analytic)<=max(1,20*estimated reference uncertainty); no division by zero',
        'temporal_errors':'C++ production BE minus same-N scipy.linalg.expm semigroup; absolute errors define p at fixed physical t',
        'spatial_errors':'analytical N minus analytical converged high-N; no temporal contamination',
        'modal_norm':'Parseval L2 on dimensionless unit sphere; physical L2 multiplies by R^(3/2)',
        'raw_modal_errors':'raw/<case>/modal_and_mean_errors_dt*.npz: all 4001 hourly times, 3 populations and 40 modes',
        'normalization':'history max of each population; modal L2 own history scale; instantaneous relative ratios separate',
        'no_plots':True,'no_thesis_text':True,'no_final_N_dt_decision':True,
        'seed':20261020,'source_common':[1.7995675661038836e19,0,0],'initial_common':[0,0,0],'R':6e-6,
        'coefficients_constant_per_case':True,'frozen_cases_sha256':rv.sha(HERE/'cases.json'),
        'solver_sha256':rv.sha(ROOT/'src/classes/Solver.C'),'source_branch':'development/nitride',
        'head':json.loads((HERE/'preservation_before.json').read_text())['head'],
        'real_states_outside_bounds':json.loads((HERE/'configuration.json').read_text())['real_states_outside_bounds'],
        'complete_history_states_outside_TableB1_count':complete_history_outside,
        'validated_dependencies_sha256':{str(p.relative_to(ROOT)):rv.sha(p) for p in
            [OLD/'reference.py',OLD/'analyze.py',OLD.parent/'common.py',OLD.parent.parent/'analytic.py',
             OLD.parent.parent/'solver_harness.cpp',OLD.parent.parent/'run_verification.py']},
        'collection_seconds':time.perf_counter()-start}
    if (HERE/'extensions_manifest.json').exists():
        meta['targeted_extension']=json.loads((HERE/'targeted_extension_protocol.json').read_text())
        meta['first_pass_tables_preserved']='deliverables/'
    rv.save(HERE/'summary.json',meta);rv.save(out/'metadata.json',meta)
    rv.save(HERE/'progress.json',{**progress,'status':meta['status'],'completed_total':len(completed),'tables_collected':True})
    assessments='; '.join(f'{k}: {v}' for k,v in meta['population_assessment_counts'].items())
    report=f'''# Campagna estesa di convergenza — 20 ottobre 2026

Esito: **{meta['status']}**, {len(completed)}/160 combinazioni: {meta['lhs_cases']} LHS e {meta['history_cases']} stati UN. Coefficienti costanti in ogni IVP, R=6e-6 m, iniziali nulle e sorgente comune come nella 4B. Seed LHS 20261020, coordinate log per Dg/gd/bb/bd e lineari per gb; campioni congelati in CSV. Recuperati tutti i 3012 aggiornamenti delle quattro reference; selezionati otto stati distinti per storia, inclusi il primo e l'ultimo intervallo effettivamente registrato, quattro tempi distribuiti e due variazioni importanti. Nessun valore reale è stato forzato; stati selezionati fuori Table B.1: {len(meta['real_states_outside_bounds'])}.

Completate **{meta['main_temporal_configurations']} configurazioni temporali principali** (4000 h, N40, cinque dt) e **{meta['spatial_analytic_configurations']} configurazioni spaziali analitiche** (4000 h, N10–320). Ulteriori raffinamenti temporali mirati: **{extra}**. Le reference high-N seguono il controllo 4B (incremento media/errore N320 e incremento relativo dell'energia della coda entrambi ≤1%): N finali e controlli per ogni caso sono in reference_validation.csv. Nessun errore C++ grezzo è spacciato per errore spaziale puro; decomposition_N40.csv conserva l'identità firmata dove è disponibile il C++.

Gli errori e gli ordini sono valutati agli stessi undici istanti fisici 0,1,2,4,8,24,100,500,1000,2000,4000 h. Ogni file grezzo C++ e ogni array di errore modale conserva anche tutte le 4001 osservazioni orarie. Le percentuali istantanee non sono sostituite da rapporti su una scala globale: sono riportate separatamente e lasciate vuote quando non numericamente definite. Il confronto indipendente con l'aggiornamento BE ha scarto massimo normalizzato **{meta['max_BE_implementation_gap_normalized']:.6g}**.

Classificazione per popolazione dopo i raffinamenti: {assessments}. Non si forza p=1 nelle sequenze dominate da precisione o cancellazione. Tutti i p grezzi, inclusi quelli negativi o molto lontani da 1, restano nei CSV con errori con segno, floor numerico, dt/τ e classificazione. temporal_order_review.csv confronta la griglia principale e il raffinamento finale; temporal_anomalies.csv include anche le anomalie della griglia principale risolte successivamente. Le categorie sono controlli operativi espliciti, non una prova di uniformità dell'ordine a ogni istante né soglie di accuratezza applicativa.

Benchmark iniziale: 18,07 s per tre casi completi; previsione media 963,43 s e inviluppo basato sul caso più lento 1495,36 s per 160 casi. Costi effettivi: somma dei tempi dei casi **{meta['sum_case_wall_seconds']:.2f} s**, kernel C++ nuovi **{meta['cpp_solver_seconds_new']:.2f} s**, processi C++ con I/O **{meta['cpp_process_seconds_new']:.2f} s**. Tempo trascorso dal primo controllo del budget alla raccolta: {meta['elapsed_since_first_benchmark_guard_s']:.2f} s, entro il limite operativo di 5400 s. Compilazione, cache, I/O e checkpoint sono documentati separatamente; i dati del benchmark sono riutilizzati. Riusi di esecuzioni 4B identiche: {meta['cpp_4B_reuses']}; reference 4B identiche: {meta['reference_4B_reuses']}.

Branch development/nitride, HEAD {meta['head']}. All'avvio l'index era vuoto: i precedenti 321 file staged risultavano già salvati nella storia Git. La preservazione viene verificata rispetto a questa fotografia iniziale e a tutti i file preesistenti, senza ricreare uno staging precedente. Il solver di produzione è invariato dalla 4B. Nessun commit, push, cambio branch, riordino, grafico, testo di tesi o studio decisionale sulla coppia N/dt. Consultare validation.json per i controlli finali e metadata.json per unità, limiti e conteggi. Lo ZIP contiene CSV, report e metadati; i grandi binari restano in raw/.
'''
    if 'targeted_extension' in meta:
        report+='\nEstensione mirata documentata in targeted_extension_protocol.json: due sequenze md a 2 h ancora preasintotiche dopo cinque dimezzamenti hanno ricevuto un sesto dimezzamento, dt=3,515625 s (4.096.000 passi, supportati dal driver). Gli ordini osservati sono diventati 0,94843 e 0,94428, senza cambiare coefficienti, reference o criteri. La prima raccolta deliverables/ e le tabelle originali per caso restano conservate; questa è la raccolta finale aggiornata.\n'
    (out/'REPORT_BREVE.md').write_text(report)
    (out/'LEGGIMI_DATI.md').write_text('''# Dati per analisi indipendente

CSV UTF-8 con separatore virgola e punto decimale. Casi identificati dalla colonna case. combinations_160.csv conserva coefficienti, origine, seed, tempi e hash; inputs/history_coefficients_complete.csv conserva le quattro storie complete di coefficienti. Il tempo source_time_s appartiene allo stato da cui sono stati calcolati i coefficienti effettivamente usati nell'intervallo; non è il tempo del gas dopo il passo. L'ultimo stato selezionato è l'ultimo intervallo disponibile, inclusa l'eventuale durata parziale.

temporal_errors.csv: medie c/mb/md, errori assoluti [at/m³], relativi istantanei adimensionali e normalizzati separati; role distingue le cinque griglie main dai raffinamenti. temporal_orders.csv: p=log2(E_coarse/E_fine) agli stessi tempi, su errori assoluti, con p grezzo e classificazione. temporal_order_review.csv confronta il risultato 450→225 s con l'ultimo raffinamento; temporal_population_assessment.csv riassume le tre popolazioni di ogni caso senza dichiarare verificati gli ordini invisibili al livello di precisione. temporal_anomalies.csv comprende anomalie iniziali anche se poi risolte. temporal_hourly_summary.csv conserva i massimi sulle 4001 osservazioni, non massimi continui rigorosi.

spatial_errors.csv: confronto esclusivamente analitico N contro N_ref; errore della media e norma del campo L2, con rapporti istantanei. spatial_orders.csv: ordini fra N consecutivi, distinti per osservabile e tempo. Nessun ordine universale è assunto. La norma L2 dei coefficienti è quella sulla sfera unitaria; sul dominio fisico si moltiplica per R^(3/2). I coefficienti mantenuti sono indipendenti da N: il troncamento del campo deriva dalla coda omessa. decomposition_N40.csv contiene errori con segno; le norme non vanno sommate come gli errori scalari.

reference_validation.csv documenta il doppio criterio 4B, max su popolazioni attive e tempi osservati; i raddoppi sono evidenze di convergenza, non limiti rigorosi della coda infinita. implementation_checks.csv contiene un confronto BE indipendente allo stesso dt e la ricostruzione delle medie. I floor di precisione sono stime operative da differenze fra formulazioni expm e dal controllo BE, non tolleranze fisiche. status=precision_limit indica ordine non confermato con questa sensibilità numerica, non errore nullo.

Per ricostruire il riferimento: J_n=[[-D(n*pi/R)^2-gb-gd,bb,bd],[gb,-bb,0],[gd,0,-bd]], q_n=sqrt(8/pi)*(-1)^(n+1)/n, V=4*pi/3. I vettori modali risolvono x_n'=J_n*x_n+q_n*source, x_n(0)=q_n*initial. La media è sum(q_n*x_n)/V. La soluzione continua usa scipy.linalg.expm sul sistema aumentato affine; il C++ usa Backward Euler. Gli input sono constant-coefficient linear benchmarks, non nuove storie microstrutturali nonlineari.

I grandi output C++, le cache expm e gli errori modali a tutte le ore sono conservati separatamente in raw/, con percorsi e SHA-256 nei manifest. Lo ZIP non contiene binari o cache grandi. Gli undici tempi nei CSV sono quelli osservati per la convergenza principale; i dati orari integrali possono essere riesportati dai grezzi senza rieseguire i solver. Nessun grafico o raccomandazione finale N/dt è incluso.
''')
    print(json.dumps({k:meta[k] for k in ['status','completed_cases','additional_temporal_refinements','max_BE_implementation_gap_normalized','population_assessment_counts','reference_N_counts','sum_case_wall_seconds']},indent=2),flush=True)


if __name__=='__main__':main()
