#!/usr/bin/env python3
"""Checkpointed extended convergence campaign; no plots or accuracy decisions."""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import argparse
import json
import time
from campaign_core import *


def assess(case,series,reference_mean,reference_modes,high_modes,meta):
    """Keep raw fixed-time p; classify numerical visibility, not accuracy."""
    rows=[];ids=np.array(OBS_HOURS);scales=np.maximum(abs(reference_mean).max(axis=0),1.)
    direct=average(high_modes[:,:,:40])
    reference_gap=abs(direct-reference_mean[ids])
    split=np.array(meta['hourly_validation']['split_gap_normalized'])*scales
    tau=float(1/max(decay_rates(case,40)))
    dts=sorted(series,reverse=True)
    for dt1,dt2 in zip(dts,dts[1:]):
        a=series[dt1];b=series[dt2]
        for i,hour in enumerate(OBS_HOURS):
            for p,pop in enumerate(POPS):
                e1=float(a['delta'][i,p]);e2=float(b['delta'][i,p]);ea=abs(e1);eb=abs(e2)
                uncertainty=max(reference_gap[i,p],split[p],a['be_gap'][i,p],b['be_gap'][i,p])
                floor=max(100*np.finfo(float).eps*scales[p],20*uncertainty)
                observed=float(np.log2(ea/eb)) if ea>0 and eb>0 else None
                active=bool(abs(reference_mean[:,p]).max()>0)
                if not active:status='inactive_population'
                elif hour==0:status='initial_zero_error'
                elif min(ea,eb)<=floor:status='precision_limit'
                elif observed is not None and .9<=observed<=1.1:status='first_order_observed'
                elif e1*e2<0 or eb>ea:status='cancellation_or_error_competition'
                elif dt2/tau>.1:status='unresolved_transient_or_preasymptotic'
                else:status='preasymptotic_not_yet_first_order'
                rows.append({'case':case['name'],'population':pop,'hour':hour,'t_s':hour*3600,
                    'coarse_dt_s':dt1,'fine_dt_s':dt2,'coarse_signed_error':e1,'fine_signed_error':e2,
                    'coarse_absolute_error':ea,'fine_absolute_error':eb,'p_log2_errors':observed,
                    'status':status,'precision_screen_absolute':floor,'tau_fast_N40_s':tau,
                    'fine_dt_over_tau_fast':dt2/tau,'pair_role':'main' if dt2>=225 else 'refinement'})
    return rows


def process_case(case):
    guard(90.)
    base=HERE/'raw'/case['name'];done=base/'completed.json'
    if done.exists():return json.loads(done.read_text())
    begin=time.perf_counter()
    exact,em,high,hm,refmeta=reference(case)
    ids=np.array(OBS_HOURS);scales=np.maximum(abs(exact).max(axis=0),1.)
    mscales=np.maximum(np.linalg.norm(em,axis=2).max(axis=0),1.)
    direct=average(hm[:,:,:40]);series={};records=[];impl=[]
    def add(dt):
        means,modes,record=cpp(case,dt);records.append(record)
        be,bem=discrete_fixed(case,dt)
        be_gap=abs(means[ids]-be)
        modal_gap=np.linalg.norm(modes[ids]-bem,axis=2)/mscales
        maxgap=max(float((be_gap/scales).max()),float(modal_gap.max()))
        assert maxgap<1e-8,('Independent BE guard',case['name'],dt,maxgap)
        reconstruction=float(abs(means-average(modes)).max()/max(abs(means).max(),1.))
        assert reconstruction<5e-14 and np.all(modes[0]==0)
        errorpath=base/f'modal_and_mean_errors_dt{dt:g}.npz'
        delta=means-exact;mdelta=modes-em
        if not errorpath.exists():np.savez_compressed(errorpath,time=HOURS,mean_error=delta,modal_error=mdelta)
        series[dt]={'delta':delta[ids],'cpp':means[ids],'modal_norm':np.linalg.norm(mdelta[ids],axis=2),
                    'be_gap':be_gap,'modes':modes[ids],
                    'mean_max_over_hourly':abs(delta).max(axis=0),
                    'modal_max_over_hourly':np.linalg.norm(mdelta,axis=2).max(axis=0)}
        impl.append({'case':case['name'],'dt_s':dt,'role':'main' if dt>=225 else 'refinement',
           'max_BE_gap_normalized':maxgap,'mean_reconstruction_gap_normalized':reconstruction,
           'cpp_output_sha256':record['output_sha256'],'cpp_solver_seconds':record['solver_seconds'],
           'new_process_seconds':record['new_process_seconds'],'reused_4B':record['reused_from_4B']})
    for dt in DTS:add(dt)
    initial_orders=assess(case,series,exact,em,hm,refmeta)
    refinement_reasons=[]
    for level in range(5):
        orders=assess(case,series,exact,em,hm,refmeta);finest=min(series)
        unresolved=[r for r in orders if r['fine_dt_s']==finest and r['status'] in
            ['cancellation_or_error_competition','unresolved_transient_or_preasymptotic','preasymptotic_not_yet_first_order']]
        if not unresolved:break
        remaining=guard(120.)
        # A sustainable refinement costs milliseconds to seconds per case;
        # do not use the remaining budget on an unbounded refinement loop.
        dt=finest/2
        if END/dt>5000000:break
        refinement_reasons.append({'dt_s':dt,'unresolved_fixed_time_sequences':len(unresolved),
            'remaining_budget_s':remaining,'reasons':sorted(set(r['status'] for r in unresolved))})
        add(dt)
    orders=assess(case,series,exact,em,hm,refmeta)
    temporal=[];modal=[];decomp=[];summaries=[]
    point_ref_gap=abs(direct-exact[ids]);split=np.array(refmeta['hourly_validation']['split_gap_normalized'])*scales
    for dt,item in sorted(series.items(),reverse=True):
        for i,hour in enumerate(OBS_HOURS):
            for p,pop in enumerate(POPS):
                value=exact[ids[i],p];err=item['delta'][i,p]
                uncertainty=max(point_ref_gap[i,p],split[p])
                relative_defined=bool(abs(value)>max(1.,20*uncertainty))
                temporal.append({'case':case['name'],'group':case['group'],'population':pop,'N':40,
                    'dt_s':dt,'role':'main' if dt>=225 else 'refinement','hour':hour,'t_s':hour*3600,
                    'cpp_mean':item['cpp'][i,p],'analytic_sameN_mean':value,'signed_error':err,
                    'absolute_error':abs(err),'relative_instantaneous':abs(err/value) if relative_defined else None,
                    'relative_defined':relative_defined,'normalized_by_history_scale':abs(err)/scales[p],
                    'history_scale':scales[p],'reference_uncertainty_estimate':uncertainty,
                    'modal_L2_error':item['modal_norm'][i,p],'modal_L2_normalized':item['modal_norm'][i,p]/mscales[p]})
                total=item['cpp'][i,p]-high[i,p]
                te=item['cpp'][i,p]-direct[i,p];se=direct[i,p]-high[i,p]
                assert abs(total-te-se)/max(abs(high).max(),1.)<32*np.finfo(float).eps
                decomp.append({'case':case['name'],'population':pop,'N':40,'dt_s':dt,'hour':hour,
                     'total_signed':total,'temporal_implementation_signed':te,'pure_spatial_signed':se,
                     'reference_form_difference':direct[i,p]-value,'identity_residual':total-te-se})
        for p,pop in enumerate(POPS):
            summaries.append({'case':case['name'],'population':pop,'dt_s':dt,'role':'main' if dt>=225 else 'refinement',
                'max_mean_absolute_over_4001_hourly_rows':item['mean_max_over_hourly'][p],
                'max_mean_normalized':item['mean_max_over_hourly'][p]/scales[p],
                'max_modal_L2_absolute_over_4001_hourly_rows':item['modal_max_over_hourly'][p],
                'max_modal_L2_normalized':item['modal_max_over_hourly'][p]/mscales[p],
                'mean_scale':scales[p],'modal_L2_scale':mscales[p]})
    spatial=[];space_orders=[];spatial_index={}
    highnorm=np.linalg.norm(hm,axis=2);highscales=np.maximum(abs(high).max(axis=0),1.)
    for n in NS:
        low=average(hm[:,:,:n]);se=low-high;tail=np.sqrt(np.sum(hm[:,:,n:]**2,axis=2))
        for i,hour in enumerate(OBS_HOURS):
            for p,pop in enumerate(POPS):
                active=bool(abs(high[:,p]).max()>0)
                row={'case':case['name'],'group':case['group'],'population':pop,'N':n,'N_ref':refmeta['N_high'],
                    'hour':hour,'t_s':hour*3600,'active':active,'analytic_N_mean':low[i,p],
                    'analytic_highN_mean':high[i,p],'signed_mean_error':se[i,p],'absolute_mean_error':abs(se[i,p]),
                    'relative_instantaneous_mean_error':abs(se[i,p]/high[i,p]) if high[i,p] else None,
                    'mean_error_normalized_by_history':abs(se[i,p])/highscales[p],
                    'field_L2_error':tail[i,p],'field_L2_relative_instantaneous':tail[i,p]/highnorm[i,p] if highnorm[i,p] else None,
                    'retained_coefficient_spatial_error':0.,'method':'analytic_N_minus_analytic_highN'}
                spatial.append(row);spatial_index[n,i,p]=row
    for n1,n2 in zip(NS,NS[1:]):
        for i,hour in enumerate(OBS_HOURS):
            for p,pop in enumerate(POPS):
                a=spatial_index[n1,i,p];b=spatial_index[n2,i,p]
                for quantity in ['absolute_mean_error','field_L2_error']:
                    e1=a[quantity];e2=b[quantity]
                    space_orders.append({'case':case['name'],'population':pop,'hour':hour,'t_s':hour*3600,
                        'coarse_N':n1,'fine_N':n2,'N_ref':refmeta['N_high'],'quantity':quantity,
                        'coarse_error':e1,'fine_error':e2,'order':float(np.log2(e1/e2)) if e1>0 and e2>0 else None,
                        'status':'observed' if e1>0 and e2>0 else 'initial_or_inactive'})
    anomalies=[];finest=min(series)
    final_orders=[r for r in orders if r['fine_dt_s']==finest]
    for r in final_orders:
        if r['status'] not in ['first_order_observed','inactive_population','initial_zero_error']:
            anomalies.append({**r,'extra_halvings':len(refinement_reasons),'classification_scope':'finest tested adjacent pair, not universal method order'})
    for name,rows in [('temporal_errors.csv',temporal),('temporal_orders.csv',orders),
                     ('spatial_errors.csv',spatial),('spatial_orders.csv',space_orders),
                     ('implementation_checks.csv',impl),('decomposition_N40.csv',decomp),
                     ('temporal_hourly_summary.csv',summaries)]:write_csv(base/name,rows)
    # Always write a header, including when a case has no unresolved sequence.
    if anomalies:write_csv(base/'temporal_anomalies.csv',anomalies)
    result={'case':case['name'],'group':case['group'],'status':'completed','main_temporal_runs':5,
       'spatial_analytic_configurations':6,'extra_temporal_refinements':len(refinement_reasons),
       'finest_dt_s':finest,'refinement_reasons':refinement_reasons,'remaining_anomaly_rows':len(anomalies),
       'anomaly_categories':{s:sum(r['status']==s for r in anomalies) for s in sorted(set(r['status'] for r in anomalies))},
       'main_unclear_sequences_before_refinement':sum(r['fine_dt_s']==225 and r['status'] not in
           ['first_order_observed','inactive_population','initial_zero_error'] for r in initial_orders),
       'reference_N':refmeta['N_high'],'reference_acquire_seconds':refmeta['acquire_seconds'],
       'reference_reused_4B':refmeta['reused_from_4B'],'cpp_reused_4B':sum(r['reused_from_4B'] for r in records),
       'solver_seconds_new':sum(r['solver_seconds'] for r in records if not r['reused_from_4B']),
       'cpp_process_seconds_new':sum(r['new_process_seconds'] or 0 for r in records),
       'max_BE_gap_normalized':max(r['max_BE_gap_normalized'] for r in impl),
       'wall_seconds':time.perf_counter()-begin,'completed_epoch':time.time()}
    rv.save(done,result)
    (HERE/'checkpoints').mkdir(exist_ok=True)
    rv.save(HERE/'checkpoints'/f'{case["name"]}.json',result)
    print('COMPLETED',case['name'],'wall',round(result['wall_seconds'],3),'extra',len(refinement_reasons),
          'Nref',refmeta['N_high'],'remaining',round(guard(0.)),flush=True)
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('task',choices=['benchmark','run'])
    a=parser.parse_args();cases=json.loads((HERE/'cases.json').read_text())
    build();guard()
    if not (HERE/'execution_protocol.json').exists():
        rv.save(HERE/'execution_protocol.json',{'base':'configuration.json','pointwise_precision_screen':
            'max(100 eps*population history scale,20*direct-vs-semigroup absolute gap,20*split gap*scale,20*independent BE point gap)',
            'BE_gap_guard':1e-8,'reconstruction_guard':5e-14,'no_plots':True,
            'spatial_campaign':'960 analytical configurations; no unnecessary spatial C++ runs',
            'max_new_campaign_seconds':5400,'budget_includes':'elapsed time from first benchmark guard through campaign, with 60-120 s orderly-stop reserve'})
    if a.task=='benchmark':
        lhs=cases[:128]
        selected=[min(lhs,key=lambda c:c['D']),max(lhs,key=lambda c:c['D']),cases[128]]
    else:
        assert (HERE/'benchmark.json').exists(),'Benchmark must precede full campaign'
        selected=cases
    start=time.perf_counter();records=[];status='completed'
    try:
        for c in selected:
            records.append(process_case(c))
            rv.save(HERE/'progress.json',{'status':'running','completed_total':len(list((HERE/'checkpoints').glob('*.json'))),
                'last_case':c['name'],'budget_remaining_s':guard(0.)})
    except BudgetStop as e:
        status='budget_stop';rv.save(HERE/'progress.json',{'status':status,'reason':str(e),
            'completed_total':len(list((HERE/'checkpoints').glob('*.json')))})
    except Exception as e:
        rv.save(HERE/f'failure_{time.time_ns()}.json',{'status':'scientific_or_implementation_stop','case':c['name'],'error':repr(e)})
        raise
    elapsed=time.perf_counter()-start
    if a.task=='benchmark':
        values=[r['wall_seconds'] for r in records]
        estimate=float(np.mean(values)*160) if values else None
        rv.save(HERE/'benchmark.json',{'status':status,'cases':[r['case'] for r in records],
            'completed_cases_including_all_five_dt_and_spatial_reference':len(records),'wall_seconds':elapsed,
            'case_wall_seconds':values,'estimated_160_cases_mean_s':estimate,
            'estimated_160_cases_at_slowest_benchmark_s':max(values)*160 if values else None,
            'compile_seconds':json.loads((HERE/'build_manifest.json').read_text())['seconds'],
            'includes':'reference, main C++, necessary extra refinements, CSV and raw I/O; three pilot cases reused by main run'})
    else:
        rv.save(HERE/f'run_timing_{time.time_ns()}.json',{'task':a.task,'status':status,'wall_seconds':elapsed})
        rv.save(HERE/'progress.json',{'status':status,'completed_total':len(list((HERE/'checkpoints').glob('*.json'))),
            'budget_remaining_s':guard(0.)})
    print(a.task,status,'seconds',elapsed,flush=True)


if __name__=='__main__':main()
