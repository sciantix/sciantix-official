#!/usr/bin/env python3
"""One sustainable extra halving for the two remaining preasymptotic points.

Keep all first-pass results intact; append raw data and write versioned tables.
"""
import sys
sys.dont_write_bytecode=True
import csv
import json
import time
from campaign_core import *
from run_campaign import assess


def load(path):
    with open(path,newline='') as f:return list(csv.DictReader(f))


def main():
    cases={c['name']:c for c in json.loads((HERE/'cases.json').read_text())}
    reviews=load(HERE/'deliverables/temporal_order_review.csv')
    targets=sorted(set(r['case'] for r in reviews if r['final_status'] in
        ['preasymptotic_not_yet_first_order','unresolved_transient_or_preasymptotic','cancellation_or_error_competition']))
    protocol={'cases':targets,'extra_dt_s':3.515625,'steps':4096000,
        'reason':'Two md sequences at 2 h remain preasymptotic (p=0.8894/0.8800) after five halvings; one more halving is supported by the unchanged harness (<5 million steps) and remaining 90-minute budget.',
        'tolerances_and_fixed_times':'unchanged; first-order screen remains [0.9,1.1]',
        'first_pass_preserved':'deliverables/ and all original per-case tables remain unchanged'}
    assert len(targets)==2
    rv.save(HERE/'targeted_extension_protocol.json',protocol)
    paths={}
    for name in targets:
        begin=time.perf_counter();case=cases[name];base=HERE/'raw'/name
        out=HERE/'extensions'/name;out.mkdir(parents=True,exist_ok=False)
        original=json.loads((base/'completed.json').read_text())
        binding=json.loads((base/'reference_manifest.json').read_text())
        with np.load(ROOT/binding['hourly_file']) as z:exact=z['means'];em=z['modes']
        with np.load(ROOT/binding['high_file']) as z:high=z['means'];hm=z['modes']
        scales=np.maximum(abs(exact).max(axis=0),1.);mscales=np.maximum(np.linalg.norm(em,axis=2).max(axis=0),1.)
        ids=np.array(OBS_HOURS);series={};new_dt=3.515625
        for dt in [7.03125,new_dt]:
            cpp_mean,cpp_modes,record=cpp(case,dt)
            be,bem=discrete_fixed(case,dt)
            series[dt]={'delta':cpp_mean[ids]-exact[ids],'be_gap':abs(cpp_mean[ids]-be)}
            if dt!=new_dt:continue
            error=cpp_mean-exact;modal_error=cpp_modes-em
            np.savez_compressed(base/f'modal_and_mean_errors_dt{dt:g}.npz',time=HOURS,mean_error=error,modal_error=modal_error)
            maxgap=max(float((abs(cpp_mean[ids]-be)/scales).max()),float((np.linalg.norm(cpp_modes[ids]-bem,axis=2)/mscales).max()))
            assert maxgap<1e-8
            reconstruction=float(abs(cpp_mean-average(cpp_modes)).max()/max(abs(cpp_mean).max(),1.))
            assert reconstruction<5e-14
            direct=average(hm[:,:,:40]);gap=abs(direct-exact[ids]);split=np.array(binding['hourly_validation']['split_gap_normalized'])*scales
            rows=[];decomp=[];summaries=[]
            for i,hour in enumerate(OBS_HOURS):
                for p,pop in enumerate(POPS):
                    value=exact[ids[i],p];err=error[ids[i],p];unc=max(gap[i,p],split[p])
                    defined=bool(abs(value)>max(1.,20*unc));modal_norm=np.linalg.norm(modal_error[ids[i],p])
                    rows.append({'case':name,'group':case['group'],'population':pop,'N':40,'dt_s':dt,'role':'refinement',
                      'hour':hour,'t_s':hour*3600,'cpp_mean':cpp_mean[ids[i],p],'analytic_sameN_mean':value,
                      'signed_error':err,'absolute_error':abs(err),'relative_instantaneous':abs(err/value) if defined else None,
                      'relative_defined':defined,'normalized_by_history_scale':abs(err)/scales[p],'history_scale':scales[p],
                      'reference_uncertainty_estimate':unc,'modal_L2_error':modal_norm,'modal_L2_normalized':modal_norm/mscales[p]})
                    total=cpp_mean[ids[i],p]-high[i,p];te=cpp_mean[ids[i],p]-direct[i,p];se=direct[i,p]-high[i,p]
                    decomp.append({'case':name,'population':pop,'N':40,'dt_s':dt,'hour':hour,'total_signed':total,
                      'temporal_implementation_signed':te,'pure_spatial_signed':se,'reference_form_difference':direct[i,p]-value,
                      'identity_residual':total-te-se})
            for p,pop in enumerate(POPS):
                summaries.append({'case':name,'population':pop,'dt_s':dt,'role':'refinement',
                  'max_mean_absolute_over_4001_hourly_rows':abs(error[:,p]).max(),'max_mean_normalized':abs(error[:,p]).max()/scales[p],
                  'max_modal_L2_absolute_over_4001_hourly_rows':np.linalg.norm(modal_error[:,p],axis=1).max(),
                  'max_modal_L2_normalized':np.linalg.norm(modal_error[:,p],axis=1).max()/mscales[p],
                  'mean_scale':scales[p],'modal_L2_scale':mscales[p]})
            impl=[{'case':name,'dt_s':dt,'role':'refinement','max_BE_gap_normalized':maxgap,
                  'mean_reconstruction_gap_normalized':reconstruction,'cpp_output_sha256':record['output_sha256'],
                  'cpp_solver_seconds':record['solver_seconds'],'new_process_seconds':record['new_process_seconds'],
                  'reused_4B':record['reused_from_4B']}]
        orders=assess(case,series,exact,em,hm,binding)
        for filename,new_rows in [('temporal_errors.csv',rows),('decomposition_N40.csv',decomp),
              ('temporal_hourly_summary.csv',summaries),('implementation_checks.csv',impl),('temporal_orders.csv',orders)]:
            write_csv(out/filename,load(base/filename)+new_rows)
        anomalies=[r for r in orders if r['status'] not in ['first_order_observed','inactive_population','initial_zero_error']]
        result={**original,'extra_temporal_refinements':6,'finest_dt_s':new_dt,
            'remaining_anomaly_rows':len(anomalies),'anomaly_categories':{s:sum(r['status']==s for r in anomalies) for s in sorted(set(r['status'] for r in anomalies))},
            'refinement_reasons':original['refinement_reasons']+[{'dt_s':new_dt,'reason':'documented targeted sixth halving; see targeted_extension_protocol.json'}],
            'solver_seconds_new':original['solver_seconds_new']+record['solver_seconds'],
            'cpp_process_seconds_new':original['cpp_process_seconds_new']+(record['new_process_seconds'] or 0),
            'max_BE_gap_normalized':max(original['max_BE_gap_normalized'],maxgap),
            'wall_seconds':original['wall_seconds']+time.perf_counter()-begin,
            'extension_wall_seconds':time.perf_counter()-begin,'completed_epoch':time.time(),
            'first_pass_result_preserved':str((base/'completed.json').relative_to(HERE))}
        rv.save(out/'completed.json',result);paths[name]=str(out.relative_to(HERE))
        print('EXTENDED',name,[(r['hour'],r['population'],r['p_log2_errors'],r['status']) for r in orders if r['hour']==2 and r['population']=='md'],flush=True)
    rv.save(HERE/'extensions_manifest.json',{'case_tables':paths,'protocol':'targeted_extension_protocol.json','extra_runs':2})


if __name__=='__main__':main()
