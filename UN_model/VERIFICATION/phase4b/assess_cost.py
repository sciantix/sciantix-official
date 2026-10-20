#!/usr/bin/env python3
"""Cost/accuracy gate before the 110 main simulations; no campaign launcher."""
import sys
sys.dont_write_bytecode = True
import argparse
import json
from pathlib import Path
import time
from common import HERE, END, rv, np, solution, gate_reference, write_csv


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--benchmark',type=Path,required=True)
    p.add_argument('--output',type=Path,required=True)
    p.add_argument('--reuse-cpp',type=Path,help='Reuse completed C++ accuracy prechecks without rerunning them')
    a=p.parse_args();out=a.output.resolve();assert out.is_relative_to(HERE)
    out.mkdir(parents=True,exist_ok=False)
    bench=json.loads(a.benchmark.read_text());assert bench['benchmarks']==5
    cases=json.loads((HERE/'cases.json').read_text())
    case=next(c for c in cases if c['name'].startswith('E2_'))
    # Existing 4A rule: time error <=5% of spatial error, each active population.
    # Same N=320 in both time resolutions; 4000 h endpoint fixed.
    settings={'temporal_fraction_of_spatial':.05,'reference_fraction_of_spatial':.01,
              'solver_budget_gate_seconds':3600.,'safety_step_limit_existing_harness':5000000,
              'observations_hours':[1.,24.,100.,1000.,4000.],
              'note':'A one-hour solver-only budget is a conservative autonomous gate, not a user-supplied cost limit.'}
    rv.save(out/'criteria.json',settings)
    times=3600*np.array(settings['observations_hours'])
    begin=time.perf_counter();low,lowm=gate_reference(case,320,times)
    analytic_low_seconds=time.perf_counter()-begin
    previous=None;refs=[];ref=None
    for n in [640,1280,2560,5120,10240]:
        start=time.perf_counter();high,highm=gate_reference(case,n,times)
        spatial=abs(high-low)
        gap=abs(high-previous) if previous is not None else None
        active=abs(high)>0
        ratios=np.divide(gap,spatial,out=np.zeros_like(spatial),where=spatial>0) if gap is not None else None
        record={'N':n,'seconds':time.perf_counter()-start,
            'max_reference_gap_over_spatial_error':float(ratios[active].max()) if ratios is not None else None,
            'means':high.tolist()}
        refs.append(record);rv.save(out/'reference_convergence.json',refs)
        if ratios is not None and ratios[active].max() <= settings['reference_fraction_of_spatial']:
            ref=high;break
        previous=high
    assert ref is not None, 'High N reference not sufficiently converged for cost gate'
    np.savez_compressed(out/'analytic_reference.npz',times=times,sameN=low,highN=ref,modes_sameN=lowm,N_high=n)
    measurements=[];samples={}
    for dt in [225.,112.5]:
        start=time.perf_counter()
        if a.reuse_cpp:
            folder=a.reuse_cpp/('cpp_'+str(dt))
            values=np.fromfile(folder/'output.bin',dtype='<f8').reshape(-1,4+3*320)
            t,means=values[:,0],values[:,1:4]
            timing=json.loads((folder/'timing.json').read_text())
            # Measured process times were preserved in the original CSV.
            import csv
            old=list(csv.DictReader(open(a.reuse_cpp/'accuracy_precheck.csv')))
            timing['process_seconds']=float(next(r['process_seconds'] for r in old if float(r['dt_s'])==dt))
            rv.save(out/('reused_cpp_'+str(dt)+'.json'),{'source':str(folder),'output_sha256':rv.sha(folder/'output.bin')})
        else:
            t,means,modes,timing=rv.run_cpp(out/('cpp_'+str(dt)),case,320,dt,END)
        ids=np.searchsorted(t,times);assert np.array_equal(t[ids],times)
        sample=means[ids];samples[dt]=(sample,timing)
        for i,hour in enumerate(settings['observations_hours']):
            for j,pop in enumerate(['c','mb','md']):
                active=not (pop=='mb' and case['gb']==0 and case['initial'][1]==0 and case['source'][1]==0)
                et=abs(sample[i,j]-low[i,j]);es=abs(ref[i,j]-low[i,j])
                measurements.append({'case':case['name'],'hour':hour,'population':pop,'dt_s':dt,
                    'active':bool(active),'cpp':float(sample[i,j]),'analytic_N320':float(low[i,j]),
                    'analytic_highN':float(ref[i,j]),'temporal_absolute_error':float(et),
                    'spatial_absolute_error':float(es),'temporal_over_spatial':float(et/es) if es>0 else None,
                    'solver_seconds':timing['solver_seconds'],'process_seconds':timing['process_seconds']})
        print('precheck',dt,'solver',timing['solver_seconds'],'s',flush=True)
    write_csv(out/'accuracy_precheck.csv',measurements)
    fine,timing=samples[112.5];coarse,_=samples[225.]
    estimates=[]
    for i,hour in enumerate(settings['observations_hours']):
        for j,pop in enumerate(['c','mb','md']):
            if ref[i,j]==0:continue
            e1=abs(coarse[i,j]-low[i,j]);e2=abs(fine[i,j]-low[i,j]);es=abs(ref[i,j]-low[i,j])
            order=float(np.log2(e1/e2)) if e1>0 and e2>0 else None
            # Predict using the observed local exponent only when it is a
            # plausible first-order regime. Otherwise use p=1 conservatively
            # and explicitly label the unresolved extrapolation.
            used_order=order if order is not None and .8<=order<=1.2 else 1.
            dt_needed=112.5*(settings['temporal_fraction_of_spatial']*es/e2)**(1/used_order) if e2>0 else 112.5
            dt_needed=min(dt_needed,112.5)
            steps=int(np.ceil(END/dt_needed))
            cpp_n320=timing['solver_seconds']*steps/timing['steps']
            estimates.append({'hour':hour,'population':pop,'observed_local_order':order,
                 'order_used_for_cost_extrapolation':used_order,'predicted_required_dt_s':float(dt_needed),
                 'predicted_steps_4000h':steps,'predicted_solver_N320_seconds':cpp_n320,
                 'predicted_solver_six_N_seconds':cpp_n320*sum([10,20,40,80,160,320])/320})
    write_csv(out/'required_step_estimates.csv',estimates)
    worst=min(estimates,key=lambda x:x['predicted_required_dt_s'])
    rows=bench['rows'];mean_solver=float(np.mean([r['solver_seconds'] for r in rows]))
    mean_process=float(np.mean([r['cpp_process_including_IO_seconds'] for r in rows]))
    mean_python=float(np.mean([r['python_analysis_seconds'] for r in rows]))
    # Clearly counterfactual baseline: all spatial runs at 225 s, which the
    # accuracy test can reject. Python scaling assumes hourly observations.
    naive={'temporal_solver_s':mean_solver*10*(4000+8000+16000+32000+64000)/64000,
       'temporal_python_without_cache_s':mean_python*50,
       'temporal_python_with_exact_reference_cache_s':mean_python*10,
       'spatial_solver_at_225s_s':mean_solver*10*sum([10,20,40,80,160,320])/40,
       'spatial_sameN_python_hourly_without_cache_s':mean_python*10*sum([10,20,40,80,160,320])/40,
       'note':'225 s spatial estimate is invalid if temporal contamination fails. High-N reference, extra checks and I/O are additional.'}
    pause=(worst['predicted_solver_six_N_seconds']>settings['solver_budget_gate_seconds'] or
           worst['predicted_steps_4000h']>settings['safety_step_limit_existing_harness'])
    result={'status':'cost_gate_stop' if pause else 'cost_gate_pass','main_campaign_started':False,
        'completed_cost_benchmarks':5,'completed_accuracy_prechecks':2,'case_screened':case['name'],
        'reference_N':n,'analytic_N320_seconds':analytic_low_seconds,
        'naive_cost_without_accuracy_requirement':naive,'binding_estimate':worst,
        'caution':'Fine-step costs are extrapolated from two measured step sizes, not measured at the predicted tiny dt. One violating case already makes the full requested fixed-step spatial campaign costly.',
        'decision':'Stop before all 110 main tests and present alternatives' if pause else 'Cost gate passed; campaign may proceed'}
    rv.save(out/'decision.json',result)
    print(json.dumps(result,indent=2),flush=True)


if __name__=='__main__':main()
