#!/usr/bin/env python3
"""Quantitative analysis of raw C++, exponential IVP, and spatial truncation."""
import sys
sys.dont_write_bytecode = True
from pathlib import Path
import csv
import gzip
import json
import time
import argparse
from reference import HERE, ROOT, OLD, rv, np, matrices, projections, VOLUME, hourly, high_reference, average, OBS, OBS_HOURS, HOURS
from campaign import CACHE, RESULTS, NS, DTS, load_cpp, configuration
from common import write_csv

OUT = HERE/'analysis_v2'
POPS = ['c','mb','md']


def norm(a): return np.linalg.norm(a,axis=-1)


class Table:
    def __init__(self,path):
        self.file=gzip.open(path,'wt',newline='') if str(path).endswith('.gz') else open(path,'w',newline='')
        self.writer=None
    def row(self,row):
        if self.writer is None:
            self.writer=csv.DictWriter(self.file,fieldnames=list(row));self.writer.writeheader()
        self.writer.writerow(row)
    def close(self):self.file.close()


def order(coarse,fine,ratio=2.,floor=0.):
    if max(coarse,fine)<=floor:return None,'reference_or_roundoff_floor'
    if coarse<=0 or fine<=0:return None,'zero_or_inactive'
    return float(np.log(coarse/fine)/np.log(ratio)),'resolved' if min(coarse,fine)>floor else 'near_reference_floor'


def discrete_be(case,n,dt):
    """Independent algebraic BE diagnostic, never used as the analytical IVP.

    NumPy solves I-hJ and composes its affine map. This isolates implementation
    error from expected BE-vs-exponential time error in the production solver.
    """
    loss=matrices(case,n)
    a=np.eye(3)[None]-dt*loss
    r=np.linalg.solve(a,np.broadcast_to(np.eye(3),a.shape))
    scale=max(case['source'])*14400000
    b=np.linalg.solve(a,np.broadcast_to(np.asarray(case['source'])*dt/scale,(n,3))[...,None])[...,0]
    block=np.zeros((n,4,4));block[:,:3,:3]=r;block[:,:3,3]=b;block[:,3,3]=1
    e=np.asarray(np.linalg.matrix_power(block,round(3600/dt)),dtype=np.longdouble)
    state=np.zeros((n,4),dtype=np.longdouble);state[:,3]=1
    q=projections(n)
    modes=np.zeros((4001,3,n))
    for i in range(4001):
        modes[i]=(state[:,:3]*(scale*q[:,None])).T
        if i<4000:state=np.einsum('nij,nj->ni',e,state)
    return average(modes),modes


def temporal(cases):
    history=Table(OUT/'temporal_history.csv.gz');modal=Table(OUT/'temporal_modal_errors.csv')
    rows=[];orders=[];checks=[]
    for case in cases:
        ref,rm,meta=hourly(case,40,CACHE)
        scales=np.maximum(abs(ref).max(axis=0),1.);mscales=np.maximum(norm(rm).max(axis=0),1.)
        unc=np.maximum(meta['direct_gap_normalized'],meta['split_gap_normalized'])
        munc=np.maximum(meta['modal_direct_gap_normalized'],meta['split_gap_normalized'])
        case_rows=[]
        for dt in DTS:
            record=json.loads((RESULTS/'temporal'/case['name']/f'N40_dt{dt:g}'/'cpp.json').read_text())
            t,cpp,cm,_=load_cpp(ROOT/record['source'],40)
            delta=cpp-ref;mdelta=cm-rm;me=abs(delta);ml=norm(mdelta)
            be,bem=discrete_be(case,40,dt)
            impl_mean=abs(cpp-be).max(axis=0)/scales
            impl_modal=norm(cm-bem).max(axis=0)/mscales
            reconstruction=float(abs(cpp-average(cm)).max()/max(abs(cpp).max(),1.))
            assert max(impl_mean.max(),impl_modal.max()) < 1e-8,('BE implementation',case['name'],dt)
            assert reconstruction<5e-14
            assert np.all(cm[0]==0) and np.all(cpp[0]==0)
            checks.append({'case':case['name'],'dt_s':dt,'max_BE_mean_gap_normalized':float(impl_mean.max()),
                           'max_BE_modal_L2_gap_normalized':float(impl_modal.max()),'mean_reconstruction_gap':reconstruction})
            for p,pop in enumerate(POPS):
                active=bool(abs(ref[:,p]).max()>0)
                row={'case':case['name'],'N':40,'dt_s':dt,'population':pop,'active':active,
                     'mean_scale_at_m3':scales[p],'modal_L2_scale_at_m3':mscales[p],
                     'max_mean_absolute':float(me[:,p].max()),'max_mean_normalized':float(me[:,p].max()/scales[p]),
                     'initial_mean_absolute':float(me[0,p]),'first_hour_mean_normalized':float(me[1,p]/scales[p]),
                     'final_mean_absolute':float(me[-1,p]),'final_mean_normalized':float(me[-1,p]/scales[p]),
                     'first24h_max_mean_normalized':float(me[:25,p].max()/scales[p]),
                     '24to1000h_max_mean_normalized':float(me[24:1001,p].max()/scales[p]),
                     '1000to4000h_max_mean_normalized':float(me[1000:,p].max()/scales[p]),
                     'max_modal_L2_absolute':float(ml[:,p].max()),'max_modal_L2_normalized':float(ml[:,p].max()/mscales[p]),
                     'final_modal_L2_normalized':float(ml[-1,p]/mscales[p]),
                     'reference_mean_crosscheck':float(unc[p]),'reference_modal_crosscheck':float(munc[p]),
                     'implementation_mean_gap':float(impl_mean[p]),'implementation_modal_gap':float(impl_modal[p])}
                rows.append(row);case_rows.append(row)
                for i in range(len(t)):
                    instant_scale=max(abs(ref[i,p]),1e-12*scales[p],1.)
                    history.row({'case':case['name'],'dt_s':dt,'hour':t[i]/3600,'population':pop,
                         'cpp':cpp[i,p],'analytic_sameN':ref[i,p],'signed_error':delta[i,p],
                         'absolute_error':me[i,p],'normalized_error':me[i,p]/scales[p],
                         'relative_with_floor':me[i,p]/instant_scale,'relative_denominator':instant_scale,
                         'modal_L2_absolute':ml[i,p],'modal_L2_normalized':ml[i,p]/mscales[p]})
                for k in range(40):
                    scale=max(abs(rm[:,p,k]).max(),1.)
                    modal.row({'case':case['name'],'dt_s':dt,'population':pop,'mode':k+1,'scale':scale,
                       'max_absolute':abs(mdelta[:,p,k]).max(),'max_normalized':abs(mdelta[:,p,k]).max()/scale,
                       'final_signed_error':mdelta[-1,p,k],'final_normalized':abs(mdelta[-1,p,k])/scale})
            print('ANALYZE_TEMP',case['name'],dt,flush=True)
        for pop in POPS:
            group=[r for r in case_rows if r['population']==pop]
            for a,b in zip(group,group[1:]):
                for quantity in ['max_mean_normalized','first_hour_mean_normalized','final_mean_normalized',
                                 'max_modal_L2_normalized','final_modal_L2_normalized']:
                    uncertainty=a['reference_modal_crosscheck'] if 'modal' in quantity else a['reference_mean_crosscheck']
                    p,status=order(a[quantity],b[quantity],floor=max(20*uncertainty,100*np.finfo(float).eps))
                    orders.append({'case':case['name'],'population':pop,'quantity':quantity,'coarse_dt_s':a['dt_s'],
                          'fine_dt_s':b['dt_s'],'coarse_error':a[quantity],'fine_error':b[quantity],
                          'order':p,'status':status,'nominal_method_order':1,
                          'floor':max(20*uncertainty,100*np.finfo(float).eps)})
    history.close();modal.close()
    write_csv(OUT/'temporal_summary.csv',rows);write_csv(OUT/'temporal_orders.csv',orders)
    write_csv(OUT/'implementation_checks.csv',checks)
    return rows,orders,checks


def spatial(cases):
    rows=[];orders=[];refrows=[];steady=[]
    modaltable=Table(OUT/'spatial_retained_modal_decomposition.csv.gz')
    for case in cases:
        hm,hmod,hmeta=high_reference(case,CACHE);nh=hmod.shape[-1]
        scales=np.maximum(abs(hm).max(axis=0),1.);mscales=np.maximum(norm(hmod).max(axis=0),1.)
        group=[]
        refrows.append({'case':case['name'],'N_high':nh,**hmeta['records'][-1],
                        'total_reference_seconds':hmeta['seconds']})
        for n in NS:
            record=json.loads((RESULTS/'spatial'/case['name']/f'N{n}_dt225'/'cpp.json').read_text())
            t,cpp,cm,_=load_cpp(ROOT/record['source'],n)
            ids=np.array(OBS_HOURS);cpp=cpp[ids];cm=cm[ids]
            same_modes=hmod[:,:,:n];same=average(same_modes)
            te=cpp-same;se=same-hm;total=cpp-hm
            tm=cm-same_modes
            temporal_L2=norm(tm);spatial_L2=np.sqrt(np.sum(hmod[:,:,n:]**2,axis=2))
            total_L2=np.hypot(temporal_L2,spatial_L2)
            closure=abs(total-(te+se))/scales[None]
            assert closure.max()<32*np.finfo(float).eps
            # Check against separately constructed hourly exponential cache.
            hourly_mean,hourly_modes,refmeta=hourly(case,n,CACHE)
            cross_mean=abs(same-hourly_mean[ids]).max(axis=0)/scales
            assert cross_mean.max()<1e-8
            for i,hour in enumerate(OBS_HOURS):
                for p,pop in enumerate(POPS):
                    active=bool(abs(hm[:,p]).max()>0)
                    row={'case':case['name'],'N':n,'dt_s':225,'hour':hour,'population':pop,'active':active,'N_high':nh,
                         'mean_scale':scales[p],'modal_L2_scale':mscales[p],
                         'cpp_mean':cpp[i,p],'analytic_sameN_mean':same[i,p],'analytic_highN_mean':hm[i,p],
                         'total_signed_mean_error':total[i,p],'temporal_signed_mean_error':te[i,p],
                         'spatial_signed_mean_error':se[i,p],'total_mean_normalized':abs(total[i,p])/scales[p],
                         'temporal_mean_normalized':abs(te[i,p])/scales[p],
                         'spatial_mean_normalized':abs(se[i,p])/scales[p],
                         'temporal_over_spatial_mean':float(abs(te[i,p]/se[i,p])) if se[i,p] else None,
                         'dominant_mean_term':'temporal' if abs(te[i,p])>abs(se[i,p]) else 'spatial',
                         'decomposition_residual_normalized':closure[i,p],
                         'temporal_modal_L2_absolute':temporal_L2[i,p],
                         'spatial_modal_L2_absolute':spatial_L2[i,p],'total_modal_L2_absolute':total_L2[i,p],
                         'temporal_modal_L2_normalized':temporal_L2[i,p]/mscales[p],
                         'spatial_modal_L2_normalized':spatial_L2[i,p]/mscales[p],
                         'total_modal_L2_normalized':total_L2[i,p]/mscales[p],
                         'sameN_reference_crosscheck_normalized':cross_mean[p]}
                    rows.append(row);group.append(row)
                    for k in range(n):
                        modaltable.row({'case':case['name'],'N':n,'hour':hour,'population':pop,'mode':k+1,
                          'cpp':cm[i,p,k],'analytic_sameN':same_modes[i,p,k],'analytic_highN':same_modes[i,p,k],
                          'total_signed_error':tm[i,p,k],'temporal_signed_error':tm[i,p,k],'spatial_signed_error':0.,
                          'note':'retained coefficient; omitted tail represented by full-field L2 separately'})
            # Mathematical stationary limit, separate from physical 4000 h.
            c_inf=case['source'][0]*case['R']**2/(15*case['D'])
            infinite=np.array([c_inf,case['gb']/case['bb']*c_inf,case['gd']/case['bd']*c_inf])
            fraction=np.sum(1/np.arange(1,n+1,dtype=float)**4)/(np.pi**4/90)
            equilibrium=infinite*fraction
            actual=cpp[-1]
            for p,pop in enumerate(POPS):
                steady.append({'case':case['name'],'N':n,'population':pop,'infinite_stationary_mean':infinite[p],
                      'finiteN_stationary_mean':equilibrium[p],'stationary_spatial_relative':1-fraction if infinite[p] else 0.,
                      'cpp_4000h_mean':actual[p],'cpp_4000h_fraction_of_stationary':actual[p]/equilibrium[p] if equilibrium[p] else None,
                      'slowest_mode1_tau_h':max(case['characteristics']['1']['tau_s'])/3600,
                      'note':'analytical stationary limit only; no claim 4000 h reaches stationarity'})
        for hour in OBS_HOURS:
            for pop in POPS:
                subset=[r for r in group if r['hour']==hour and r['population']==pop]
                for a,b in zip(subset,subset[1:]):
                    for quantity in ['spatial_mean_normalized','spatial_modal_L2_normalized']:
                        p,status=order(a[quantity],b[quantity],ratio=b['N']/a['N'],floor=100*np.finfo(float).eps)
                        orders.append({'case':case['name'],'hour':hour,'population':pop,'quantity':quantity,
                           'coarse_N':a['N'],'fine_N':b['N'],'coarse_error':a[quantity],'fine_error':b[quantity],
                           'order':p,'status':status,'N_high':nh})
        print('ANALYZE_SPACE',case['name'],nh,flush=True)
    modaltable.close()
    write_csv(OUT/'spatial_errors.csv',rows);write_csv(OUT/'spatial_orders.csv',orders)
    write_csv(OUT/'reference_convergence.csv',refrows);write_csv(OUT/'stationary_limit_separate.csv',steady)
    return rows,orders,refrows,steady


def fast():
    ledger=json.loads((HERE/'rapid_ledger.json').read_text());rows=[];orders=[]
    history=Table(OUT/'rapid_history.csv.gz')
    for record in ledger:
        case=record['case'];n=record['N'];resolution=record['resolution']
        folder=RESULTS/'rapid'/case/f'N{n}_tau{resolution}'
        with np.load(CACHE/f'rapid_{case}_N{n}.npz') as z:ref=z['means'];rm=z['modes'];t=z['time']
        with np.load(folder/'sampled.npz') as z:cpp=z['cpp'];cm=z['modes']
        scales=np.maximum(abs(ref).max(axis=0),1.);mscales=np.maximum(norm(rm).max(axis=0),1.)
        for p,pop in enumerate(POPS):
            err=abs(cpp[:,p]-ref[:,p]);merr=norm(cm[:,p]-rm[:,p])
            row={'case':case,'N':n,'population':pop,'resolution':resolution,'dt_s':record['dt_s'],
                 'tau_fast_s':record['tau_fast_s'],'end_s':record['end_s'],'steps':record['steps'],
                 'mean_scale':scales[p],'modal_scale':mscales[p],
                 'max_mean_normalized':float(err.max()/scales[p]),'final_mean_normalized':float(err[-1]/scales[p]),
                 'max_modal_L2_normalized':float(merr.max()/mscales[p]),
                 'initial_error':float(err[0]),'active':bool(abs(ref[:,p]).max()>0)}
            rows.append(row)
            for i in range(len(t)):
                history.row({'case':case,'N':n,'resolution':resolution,'population':pop,
                     't_s':t[i],'t_over_tau':t[i]/record['tau_fast_s'],
                     'cpp':cpp[i,p],'analytic':ref[i,p],'mean_normalized_error':err[i]/scales[p],
                     'modal_L2_normalized_error':merr[i]/mscales[p]})
    for case in sorted(set(r['case'] for r in rows)):
        for n in [40,320]:
            for pop in POPS:
                subset=[r for r in rows if r['case']==case and r['N']==n and r['population']==pop]
                for a,b in zip(subset,subset[1:]):
                    for quantity in ['max_mean_normalized','max_modal_L2_normalized']:
                        p,status=order(a[quantity],b[quantity],floor=1e-11)
                        orders.append({'case':case,'N':n,'population':pop,'quantity':quantity,
                           'coarse_resolution':a['resolution'],'fine_resolution':b['resolution'],
                           'coarse_error':a[quantity],'fine_error':b[quantity],'order':p,'status':status})
    history.close();write_csv(OUT/'rapid_summary.csv',rows);write_csv(OUT/'rapid_orders.csv',orders)
    return rows,orders


def main():
    global OUT
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output',type=Path,default=OUT)
    OUT=parser.parse_args().output.resolve()
    assert OUT.is_relative_to(HERE)
    OUT.mkdir(exist_ok=False)
    begin=time.perf_counter();cases=configuration()
    tr,to,checks=temporal(cases);sr,so,refs,steady=spatial(cases);fr,fo=fast()
    ledger=json.loads((HERE/'main_ledger.json').read_text());rapid=json.loads((HERE/'rapid_ledger.json').read_text())
    assert len(ledger)==110 and sum(r['stage']=='temporal' for r in ledger)==50
    assert len({(r['case'],r['N'],r['dt_s']) for r in ledger})==100
    summary={'main_configurations':110,'temporal_configurations':50,'spatial_configurations':60,
      'main_new_cpp_executions':sum(not r['reused'] for r in ledger),'main_reused_configurations':sum(r['reused'] for r in ledger),
      'rapid_executions':len(rapid),'rapid_selected_cases':sorted(set(r['case'] for r in rapid)),
      'main_new_solver_seconds':sum(r['solver_seconds_original'] for r in ledger if not r['reused']),
      'main_new_cpp_process_seconds':sum(r['new_process_seconds'] or 0 for r in ledger),
      'rapid_solver_seconds':sum(r['solver_seconds'] for r in rapid),
      'rapid_process_seconds':sum(r['process_seconds'] for r in rapid),
      'max_temporal_mean_error_normalized':max(r['max_mean_normalized'] for r in tr),
      'max_BE_implementation_gap_normalized':max(r['max_BE_modal_L2_gap_normalized'] for r in checks),
      'max_decomposition_residual_normalized':max(r['decomposition_residual_normalized'] for r in sr),
      'spatial_temporal_dominant_samples':sum(r['dominant_mean_term']=='temporal' and r['active'] and r['hour']>0 for r in sr),
      'analysis_wall_seconds':time.perf_counter()-begin,'LHS':0,
      'normalization':'per-population analytical history maximum, floor 1 at/m3; modal L2 separate scales',
      'limits':'Main grid is hourly; fast transients separate. No universal spatial order or stationary-at-4000h claim.'}
    rv.save(OUT/'summary.json',summary);print(json.dumps(summary,indent=2),flush=True)


if __name__=='__main__':main()
