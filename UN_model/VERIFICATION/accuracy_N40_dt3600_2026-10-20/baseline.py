#!/usr/bin/env python3
"""Analyze the existing candidate; never launches C++ or regenerates samples."""
import sys
sys.dont_write_bytecode=True
import argparse,csv,gzip,json,time
from core import *


def analyze_case(case):
    folder=HERE/'baseline'/case['name'];p=folder/'completed.json'
    if p.exists():return json.loads(p.read_text())
    begin=time.perf_counter();folder.mkdir(parents=True,exist_ok=True)
    low,high,hm,refmeta,unc=old_data(case);cpp,cppmeta=old_cpp(case,40,3600.)
    scales=np.maximum(abs(low).max(axis=0),1.);highscales=np.maximum(abs(high).max(axis=0),1.)
    direct40=average(hm[:,:,:40]);ids=np.array(OBS_HOURS)
    peaks=[];temporal_selected=[];combined=[]
    with gzip.open(folder/'temporal_errors_hourly.csv.gz','wt',newline='') as f,open(folder/'temporal_hourly_lossless.csv','w',newline='') as wide:
        fields=['case','group','population','hour','t_s','cpp','analytic_N40','signed_error','absolute_error',
                'relative_instantaneous','normalized_population_scale','relative_status','population_scale']
        w=csv.DictWriter(f,fieldnames=fields);w.writeheader()
        ww=csv.writer(wide);ww.writerow(['case','hour','analytic_c','analytic_mb','analytic_md','error_c','error_mb','error_md','relative_valid_mask'])
        delta,ab,rel,norm,valid=metric_vectors(cpp,low,scales,unc)
        for i,t in enumerate(HOURS):
            ww.writerow([case['name'],i,*low[i],*delta[i],int(valid[i,0])+2*int(valid[i,1])+4*int(valid[i,2])])
            for j,pop in enumerate(POPS):
                row={'case':case['name'],'group':case['group'],'population':pop,'hour':i,'t_s':t,
                    'cpp':cpp[i,j],'analytic_N40':low[i,j],'signed_error':delta[i,j],'absolute_error':ab[i,j],
                    'relative_instantaneous':rel[i,j] if valid[i,j] else None,'normalized_population_scale':norm[i,j],
                    'relative_status':'defined' if valid[i,j] else ('zero_analytic' if low[i,j]==0 else 'numerically_small_analytic'),
                    'population_scale':scales[j]}
                w.writerow(row)
                if i in OBS_HOURS:temporal_selected.append(row)
    for j,pop in enumerate(POPS):
        peaks.extend(maxima(case,pop,'temporal',HOURS,cpp[:,j],low[:,j],scales[j],unc[j],'all_4001_hourly'))
        for kind,a,b,scale in [('spatial',direct40[:,j],high[:,j],highscales[j]),
                              ('total',cpp[ids,j],high[:,j],highscales[j])]:
            peaks.extend(maxima(case,pop,kind,OBS,a,b,scale,unc[j],'common_11_verified'))
    combined=combined_rows(case,OBS,cpp[ids],direct40,high,scales,highscales,unc,'common_11_verified',low[ids])
    table(folder/'maxima.csv',peaks);table(folder/'temporal_selected.csv',temporal_selected)
    table(folder/'spatial_total_errors.csv',combined)
    data={'case':case['name'],'group':case['group'],'status':'completed','new_cpp_runs':0,
        'reference_manifest':str((SOURCE/'raw'/case['name']/'reference_manifest.json').relative_to(ROOT)),
        'cpp':cppmeta,'reference_N':refmeta['N_high'],'temporal_scale':scales.tolist(),
        'highN_scale':highscales.tolist(),'reference_uncertainty_estimate':unc.tolist(),
        'max_reference_form_gap_normalized':float((abs(direct40-low[ids])/scales).max()),
        'wall_seconds':time.perf_counter()-begin,'full_hourly_gzip_bytes':(folder/'temporal_errors_hourly.csv.gz').stat().st_size,
        'wide_csv_bytes':(folder/'temporal_hourly_lossless.csv').stat().st_size}
    rv.save(p,data);print('BASELINE',case['name'],round(data['wall_seconds'],3),flush=True);return data


def combined_rows(case,times,cpp,low,high,scales,highscales,unc,scope,hourly_low=None):
    rows=[]
    for i,t in enumerate(times):
        for j,pop in enumerate(POPS):
            temporal=cpp[i,j]-low[i,j];spatial=low[i,j]-high[i,j];total=cpp[i,j]-high[i,j]
            residual=total-temporal-spatial
            assert abs(residual)/max(highscales[j],1.)<32*np.finfo(float).eps
            dominant='temporal' if abs(temporal)>abs(spatial) else 'spatial'
            if max(abs(temporal),abs(spatial))<=20*unc[j]:dominant='both_near_numerical_precision'
            for kind,numeric,analytic,scale in [('temporal',cpp[i,j],low[i,j],scales[j]),
                      ('spatial',low[i,j],high[i,j],highscales[j]),('total',cpp[i,j],high[i,j],highscales[j])]:
                signed=numeric-analytic;defined=abs(analytic)>max(1.,20*unc[j])
                rows.append({'case':case['name'],'group':case['group'],'population':pop,'hour':t/3600,'t_s':t,
                    'error_kind':kind,'numerical_or_truncated_mean':numeric,'analytic_reference':analytic,
                    'signed_error':signed,'absolute_error':abs(signed),
                    'relative_instantaneous':abs(signed/analytic) if defined else None,
                    'relative_status':'defined' if defined else ('zero_analytic' if analytic==0 else 'numerically_small_analytic'),
                    'normalized_population_scale':abs(signed)/max(scale,1.),'population_scale':scale,
                    'signed_temporal_component':temporal,'signed_spatial_component':spatial,'signed_total':total,
                    'identity_residual':residual,'dominant_component':dominant,
                    'reference_form_gap':low[i,j]-hourly_low[i,j] if hourly_low is not None else None,
                    'sampling_scope':scope})
    return rows


def select():
    peaks=[]
    for c in CASES:peaks.extend(load_csv(HERE/'baseline'/c['name']/'maxima.csv'))
    nominees=[]
    for group in ['LHS','UN_history']:
        for pop in POPS:
            for kind in ['temporal','spatial','total']:
                for metric in ['normalized_population_scale','relative_instantaneous']:
                    rows=[r for r in peaks if r['group']==group and r['population']==pop and r['error_kind']==kind
                          and r['metric']==metric and r['status']=='observed_discrete_maximum' and r['maximum_observed']]
                    if rows:
                        r=max(rows,key=lambda r:float(r['maximum_observed']))
                        nominees.append({**r,'selection_reason':f'worst_{group}_{pop}_{kind}_{metric}'})
    names=sorted(set(r['case'] for r in nominees))
    assert len(names)<=18,'Too many critical cases; assess cost before proceeding'
    table(HERE/'selection_nominations.csv',nominees)
    rv.save(HERE/'selection.json',{'cases':names,'selection':'Union of maxima by group/population/error kind, normalized and instantaneous relative; no 1%/3% pass/fail thresholds',
        'initial_common_sampling':'temporal all4001; spatial/total common11',
        'case_count':len(names),'new_cpp_planned_per_case':[{'N':80,'dt_s':3600},{'N':160,'dt_s':3600}],
        'reused_N40_dt_s':[3600,1800,900],
        'rapid_selection':'selected cases with 3600/tau_fast_N40>=20; short windows 20tau plus one resolved first-hour diagnostic',
        'stop_at_N':160})
    print('SELECTED',names,flush=True)


def main():
    p=argparse.ArgumentParser();p.add_argument('task',choices=['benchmark','all','select']);a=p.parse_args()
    if a.task=='select':select();return
    (HERE/'baseline').mkdir(exist_ok=True)
    selected=[CASES[0],CASES[128]] if a.task=='benchmark' else CASES
    start=time.perf_counter();rows=[]
    for c in selected:rows.append(analyze_case(c))
    if a.task=='benchmark':
        rv.save(HERE/'benchmark_baseline.json',{'cases':[r['case'] for r in rows],'wall_seconds':time.perf_counter()-start,
            'estimated_160_case_wall_s':float(np.mean([r['wall_seconds'] for r in rows])*160),
            'new_cpp_runs':0,'note':'pilot cases are reused in full analysis; targeted high-N cost benchmark follows selection'})
    else:select()


if __name__=='__main__':main()
