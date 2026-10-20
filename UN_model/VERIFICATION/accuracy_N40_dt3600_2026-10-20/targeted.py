#!/usr/bin/env python3
"""Add only critical-case references, missing C++ refinements and fast probes."""
import sys
sys.dont_write_bytecode=True
import argparse,json,time
from core import *
from baseline import combined_rows


def schedule(case):
    hours=set(OBS_HOURS)|set(range(1,25))
    for row in load_csv(HERE/'baseline'/case['name']/'maxima.csv'):
        if row['status']!='observed_discrete_maximum' or not row['hour']:continue
        h=int(float(row['hour']))
        for offset in [-2,-1,0,1,2,-max(1,round(h*.1)),max(1,round(h*.1))]:
            if 1<=h+offset<=4000:hours.add(h+offset)
    return sorted(hours)


def enriched_reference(case,hm,meta,folder):
    if (folder/'convergence.json').exists() and (folder/'combined_reference.npz').exists():
        record=json.loads((folder/'convergence.json').read_text())
        with np.load(folder/'combined_reference.npz') as z:
            return z['time'],z['high'],{n:z[f'N{n}'] for n in [40,80,160]},record
    hours=schedule(case);new=[h for h in hours if h not in OBS_HOURS]
    n=meta['N_high'];start=time.perf_counter();checks=[]
    for attempt in range(3):
        modes=evaluated_modes(case,n,np.array(new)*3600,folder)
        means=average(modes);low320=average(modes[:,:,:320]);half=average(modes[:,:,:n//2])
        spatial=abs(means-low320);gap=abs(means-half)
        ratios=np.divide(gap,spatial,out=np.zeros_like(gap),where=spatial>0)
        energy=np.sum(modes[:,:,320:]**2,axis=2);prev=np.sum(modes[:,:,320:n//2]**2,axis=2)
        er=np.divide(abs(energy-prev),energy,out=np.zeros_like(energy),where=energy>0)
        active=abs(means)>0
        row={'N':n,'max_mean_increment_over_N320_spatial':float(ratios[active].max()),
             'max_tail_energy_relative_increment':float(er[active].max()),'new_observations':len(new)}
        checks.append(row)
        if row['max_mean_increment_over_N320_spatial']<=.01 and row['max_tail_energy_relative_increment']<=.01:break
        n*=2
    else:raise RuntimeError('High-N convergence not controlled on enriched grid: '+case['name'])
    # Preserve original verified values at original times. Both C++ and exact
    # solutions are compared only at the union of actual hourly output times.
    high=np.zeros((len(hours),3));low={k:np.zeros((len(hours),3)) for k in [40,80,160]}
    ref_n=[]
    for i,h in enumerate(hours):
        source=hm[OBS_HOURS.index(h)] if h in OBS_HOURS else modes[new.index(h)]
        high[i]=average(source[None])[0]
        for k in low:low[k][i]=average(source[None,:,:k])[0]
        ref_n.append(source.shape[-1])
    record={'case':case['name'],'hours':hours,'new_hours':new,'high_N_by_time':ref_n,'checks':checks,
            'seconds':time.perf_counter()-start,'sameN_prefixes_identical_by_independent_modal_equations':True,
            'new_evaluation_scaling_s':END,'original_verified_values_unchanged':True}
    np.savez_compressed(folder/'combined_reference.npz',time=np.array(hours)*3600,high=high,
                        N40=low[40],N80=low[80],N160=low[160],reference_N=ref_n)
    rv.save(folder/'convergence.json',record)
    return np.array(hours)*3600,high,low,record


def critical_case(case):
    folder=HERE/'targeted'/case['name'];done=folder/'completed.json'
    if done.exists():return json.loads(done.read_text())
    folder.mkdir(parents=True,exist_ok=True);begin=time.perf_counter()
    original,high0,hm,meta,unc=old_data(case)
    scales=np.maximum(abs(original).max(axis=0),1.);highscales=np.maximum(abs(high0).max(axis=0),1.)
    times,high,low,validation=enriched_reference(case,hm,meta,folder/'high_reference')
    ids=(times/3600).astype(int);c0,_=old_cpp(case,40,3600.)
    maxima_rows=[]
    for j,pop in enumerate(POPS):
        maxima_rows+=maxima(case,pop,'spatial',times,low[40][:,j],high[:,j],highscales[j],unc[j],'enriched_hourly_subset')
        maxima_rows+=maxima(case,pop,'total',times,c0[ids,j],high[:,j],highscales[j],unc[j],'enriched_hourly_subset')
    rows=combined_rows(case,times,c0[ids],low[40],high,scales,highscales,unc,'enriched_hourly_subset',original[ids])
    table(folder/'candidate_enriched_errors.csv',rows);table(folder/'candidate_enriched_maxima.csv',maxima_rows)
    comparison=[];costs=[];hist=[];low_reference_seconds=0.
    for n,dt in [(40,3600.),(40,1800.),(40,900.),(80,3600.),(160,3600.)]:
        guard()
        reused=old_cpp(case,n,dt)
        if reused is not None:cpp,cost=reused
        else:
            t,cpp,cm,cost=new_cpp(case,n,dt,END,folder/f'cpp_N{n}_dt{dt:g}')
            assert np.array_equal(t,HOURS)
        costs.append(cost)
        if n==40:exact=original
        else:
            (folder/'lowN_cache').mkdir(exist_ok=True)
            exact,_,refcheck=ref.hourly(case,n,folder/'lowN_cache')
            low_reference_seconds+=refcheck['seconds']
        # Shared baseline high-N scales make improvement ratios comparable.
        for j,pop in enumerate(POPS):
            for kind,ts,a,b,scope in [('temporal',HOURS,cpp[:,j],exact[:,j],'all_4001_hourly'),
                ('spatial',times,low[n][:,j],high[:,j],'shared_enriched_grid'),
                ('total',times,cpp[ids,j],high[:,j],'shared_enriched_grid')]:
                for peak in maxima(case,pop,kind,ts,a,b,highscales[j],unc[j],scope):
                    comparison.append({**peak,'N':n,'dt_s':dt,'normalization_anchor':'candidate high-N population scale',
                        'work_factor_vs_candidate':n/40*3600/dt,'solver_seconds_original':cost['solver_seconds'],
                        'new_process_seconds':cost['new_process_seconds'],'reused_output':cost['reused']})
        point_rows=combined_rows(case,times,cpp[ids],low[n],high,highscales,highscales,unc,'shared_enriched_grid',exact[ids])
        hist.extend({**r,'N':n,'dt_s':dt} for r in point_rows)
    candidate={(r['population'],r['error_kind'],r['metric']):r for r in comparison if r['N']==40 and r['dt_s']==3600}
    candidate_cost=costs[0]['solver_seconds']
    for row in comparison:
        a=candidate[row['population'],row['error_kind'],row['metric']]['maximum_observed'];b=row['maximum_observed']
        row['candidate_peak_on_same_grid']=a
        row['improvement_factor']=float(a/b) if a is not None and b is not None and b>0 else None
        row['measured_solver_cost_ratio']=row['solver_seconds_original']/candidate_cost
    table(folder/'refinement_maxima.csv',comparison);table(folder/'refinement_errors.csv',hist);table(folder/'costs.csv',costs)
    result={'case':case['name'],'status':'completed','new_cpp_runs':sum(not r['reused'] for r in costs),
            'reused_cpp_runs':sum(r['reused'] for r in costs),'new_high_observations':len(validation['new_hours']),
            'high_N_max':max(validation['high_N_by_time']),'wall_seconds':time.perf_counter()-begin,
            'high_reference_original_seconds':validation['seconds'],'low_reference_original_seconds':low_reference_seconds,
            'new_solver_seconds':sum(r['solver_seconds'] for r in costs if not r['reused']),
            'new_process_seconds':sum(r['new_process_seconds'] for r in costs)}
    rv.save(done,result);print('TARGETED',case['name'],result,flush=True);return result


def rapid_selection():
    selected=[]
    peaks=[]
    for c in CASES:peaks.extend(load_csv(HERE/'baseline'/c['name']/'maxima.csv'))
    fast=[c for c in CASES if 3600*max(decay_rates(c,40))>=20]
    for group in ['LHS','UN_history']:
        allowed={c['name'] for c in fast if c['group']==group}
        rows=[r for r in peaks if r['case'] in allowed and r['error_kind']=='temporal' and
              r['metric']=='relative_instantaneous' and r['maximum_observed']]
        if rows:selected.append(max(rows,key=lambda r:float(r['maximum_observed']))['case'])
    selected.append(max(fast,key=lambda c:max(decay_rates(c,40)))['name'])
    return sorted(set(selected))


def rapid_case(case):
    folder=HERE/'rapid'/case['name'];done=folder/'completed.json'
    if done.exists():return json.loads(done.read_text())
    folder.mkdir(parents=True,exist_ok=True);begin=time.perf_counter();tau=float(1/max(decay_rates(case,40)))
    end=20*tau;times=np.arange(41)*tau/2
    exactm=evaluated_modes(case,40,times,folder/'reference');exact=average(exactm)
    scale=np.maximum(abs(exact).max(axis=0),1.);rows=[];peaks=[];costs=[]
    for resolution in [10,20,40]:
        dt=tau/resolution
        t,cpp,cm,cost=new_cpp(case,40,dt,end,folder/f'tau{resolution}',record_interval=tau/2)
        ids=np.array([int(np.argmin(abs(t-x))) for x in times]);assert np.max(abs(t[ids]-times))<end*2e-12
        costs.append(cost)
        for j,pop in enumerate(POPS):
            for peak in maxima(case,pop,'temporal',times,cpp[ids,j],exact[:,j],scale[j],0.,'short_20tau_window'):
                peaks.append({**peak,'dt_s':dt,'resolution_per_tau':resolution,'tau_fast_s':tau})
            for i,timepoint in enumerate(times):
                defined=abs(exact[i,j])>max(1.,100*np.finfo(float).eps*scale[j])
                rows.append({'case':case['name'],'group':case['group'],'population':pop,'t_s':timepoint,
                    't_over_tau':timepoint/tau,'tau_fast_s':tau,'dt_s':dt,'resolution_per_tau':resolution,
                    'cpp_fine':cpp[ids[i],j],'analytic_N40':exact[i,j],
                    'fine_absolute_error':abs(cpp[ids[i],j]-exact[i,j]),
                    'fine_relative_error':abs((cpp[ids[i],j]-exact[i,j])/exact[i,j]) if defined else None,
                    'candidate_1h_output_available':bool(timepoint==0 or timepoint%3600==0),
                    'candidate_value_inside_step':None,'note':'No interpolation or hold assumption for candidate between hourly outputs'})
    # A resolved first-hour integration compares actual endpoints, not a
    # hypothetical candidate value in the interior of its first timestep.
    dt=tau/20;t,c,cm,cost=new_cpp(case,40,dt,3600.,folder/'first_hour_resolved',record_interval=3600.)
    costs.append(cost);low,_,_,_,unc=old_data(case);candidate,_=old_cpp(case,40,3600.)
    bridge=[]
    for j,pop in enumerate(POPS):
        a=low[1,j];defined=abs(a)>max(1.,20*unc[j])
        bridge.append({'case':case['name'],'group':case['group'],'population':pop,'time_s':3600,
            'tau_fast_s':tau,'candidate_dt_over_tau':3600/tau,'fine_dt_s':dt,'analytic_N40':a,
            'candidate_cpp_first_step':candidate[1,j],'fine_cpp_at_same_time':c[-1,j],
            'candidate_absolute_error':abs(candidate[1,j]-a),'fine_absolute_error':abs(c[-1,j]-a),
            'candidate_relative_error':abs((candidate[1,j]-a)/a) if defined else None,
            'fine_relative_error':abs((c[-1,j]-a)/a) if defined else None,
            'inside_first_candidate_step':'not defined; no continuous-maximum claim'})
    table(folder/'short_errors.csv',rows);table(folder/'short_maxima.csv',peaks)
    table(folder/'first_hour_comparison.csv',bridge);table(folder/'costs.csv',costs)
    result={'case':case['name'],'status':'completed','tau_fast_N40_s':tau,'short_end_s':end,
        'new_cpp_runs':4,'new_solver_seconds':sum(r['solver_seconds'] for r in costs),
        'wall_seconds':time.perf_counter()-begin,'candidate_subhour_values_invented':False}
    rv.save(done,result);print('RAPID',case['name'],result,flush=True);return result


def main():
    p=argparse.ArgumentParser();p.add_argument('task',choices=['benchmark','all']);args=p.parse_args()
    selection=json.loads((HERE/'selection.json').read_text());names=selection['cases'];byname={c['name']:c for c in CASES}
    build();start=time.perf_counter()
    if args.task=='benchmark':
        r=critical_case(byname[names[0]])
        estimate_case=max(r['wall_seconds'],r['high_reference_original_seconds']+r['low_reference_original_seconds']+r['new_process_seconds']+r['wall_seconds'])
        estimated=estimate_case*len(names)*max(1.,max(json.loads((SOURCE/'raw'/n/'reference_manifest.json').read_text())['N_high'] for n in names)/r['high_N_max'])
        rv.save(HERE/'benchmark_targeted.json',{'case':names[0],'wall_seconds':time.perf_counter()-start,
            'selected_cases':len(names),'estimated_selected_wall_upper_s':estimated,
            'additional_fast_diagnostics_budget_estimate_s':120,'remaining_budget_s':guard(0.),
            'note':'Original reference/process timings retained when cached; first pilot export interrupted by heterogeneous CSV metadata fields, adapter fixed without rerunning C++.',
            'cost_check_pass':estimated+120<guard(0.)-120})
    else:
        assert json.loads((HERE/'benchmark_targeted.json').read_text())['cost_check_pass']
        completed=[]
        for name in names:
            completed.append(critical_case(byname[name]));rv.save(HERE/'targeted_progress.json',completed)
        rapid=rapid_selection();rv.save(HERE/'rapid_selection.json',{'cases':rapid,
            'reason':'worst temporal instantaneous relative error in each fast group plus fastest spectral timescale; all drawn from the frozen 160'})
        for name in rapid:rapid_case(byname[name])
        rv.save(HERE/'targeted_complete.json',{'status':'completed','critical_cases':len(names),'rapid_cases':len(rapid),
            'wall_seconds':time.perf_counter()-start,'no_cpp_beyond_N160':True})


if __name__=='__main__':main()
