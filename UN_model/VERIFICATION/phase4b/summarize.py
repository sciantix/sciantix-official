#!/usr/bin/env python3
"""Postprocess completed benchmarks and the cost gate; launches no simulations."""
import sys
sys.dont_write_bytecode = True
from pathlib import Path
import argparse
import csv
import json
import os
import time
from common import HERE, rv, np, write_csv


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--benchmark',type=Path,required=True)
    p.add_argument('--gate',type=Path,required=True)
    p.add_argument('--output',type=Path,required=True)
    a=p.parse_args();out=a.output.resolve();assert out.is_relative_to(HERE)
    out.mkdir(parents=True,exist_ok=False)
    os.environ['MPLCONFIGDIR']=str(out/'mplconfig')
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    bench=json.loads(a.benchmark.read_text());gate=json.loads(a.gate.read_text())
    with open(a.gate.parent/'accuracy_precheck.csv') as f:accuracy=list(csv.DictReader(f))
    estimates=[]
    timing_path=a.gate.parent/'cpp_112.5/timing.json'
    if not timing_path.exists():
        reused=json.loads((a.gate.parent/'reused_cpp_112.5.json').read_text())
        timing_path=Path(reused['source'])/'timing.json'
    timing=json.loads(timing_path.read_text())
    for r in accuracy:
        if float(r['dt_s'])!=112.5 or r['active']!='True':continue
        ratio=float(r['temporal_over_spatial'])
        dt=min(112.5,112.5*.05/ratio) if ratio>0 else 112.5
        steps=int(np.ceil(14400000/dt))
        cpp=steps*timing['solver_seconds']/timing['steps']
        estimates.append({'hour':float(r['hour']),'population':r['population'],
          'assumed_asymptotic_order':1.,'predicted_dt_s':dt,'predicted_steps':steps,
          'predicted_N320_solver_days':cpp/86400,'predicted_six_N_solver_days':cpp*630/320/86400})
    write_csv(out/'asymptotic_p1_cost.csv',estimates)
    worst=min(estimates,key=lambda r:r['predicted_dt_s'])
    rows=bench['rows']
    fig,ax=plt.subplots(figsize=(12,5),constrained_layout=True)
    x=np.arange(len(rows));width=.2
    for k,key in enumerate(['solver_seconds','cpp_process_including_IO_seconds','python_analysis_seconds','complete_case_including_IO_seconds']):
        ax.bar(x+(k-1.5)*width,[r[key] for r in rows],width,label=key)
    ax.set_xticks(x,[r['case'].split('_')[0] for r in rows]);ax.set_yscale('log')
    ax.set_ylabel('Tempo misurato [s]');ax.set_title('Cinque benchmark: 4000 h, N=40, Δt=225 s');ax.legend(fontsize=8)
    fig.savefig(out/'benchmark_times.png',dpi=160);fig.savefig(out/'benchmark_times.pdf');plt.close(fig)

    fig,axes=plt.subplots(3,1,figsize=(12,10),constrained_layout=True,sharex=True)
    errors=[]
    for r in rows:
        path=a.benchmark.parent/r['case']/'comparison.npz'
        with np.load(path) as z:
            t=z['time']/3600;actual=z['cpp'];exact=z['analytic']
        global_scale=max(abs(exact).max(),1.)
        for j,pop in enumerate(['c','mb','md']):
            delta=abs(actual[:,j]-exact[:,j]);normalized=delta/global_scale
            axes[j].semilogy(t[1:],np.where(normalized[1:]>0,normalized[1:],np.nan),label=r['case'].split('_')[0])
            errors.append({'case':r['case'],'population':pop,'dt_s':225.,'N':40,
              'max_absolute_error':float(delta.max()),'max_error_normalized_by_case_scale':float(normalized.max()),
              'final_absolute_error':float(delta[-1]),'max_error_hour':float(t[int(np.argmax(delta))]),
              'note':'E2 bulk is structurally zero; tiny full-4A-expm bulk values are reference roundoff, not physical bulk error' if pop=='mb' and r['case'].startswith('E2') else ''})
    for ax,pop in zip(axes,['c','mb','md']):
        ax.set_ylabel('Errore '+pop+' / scala caso');ax.grid(alpha=.25);ax.legend(fontsize=8)
    axes[-1].set_xlabel('Tempo fisico [h]')
    fig.suptitle('Storie d’errore dei soli cinque benchmark; non la campagna temporale 50/50')
    fig.savefig(out/'benchmark_error_histories.png',dpi=150);fig.savefig(out/'benchmark_error_histories.pdf');plt.close(fig)
    write_csv(out/'benchmark_errors_per_population.csv',errors)

    fig,axes=plt.subplots(1,2,figsize=(12,5),constrained_layout=True)
    for ax,pop in zip(axes,['c','md']):
        for dt in [225.,112.5]:
            rs=[r for r in accuracy if r['population']==pop and float(r['dt_s'])==dt]
            ax.loglog([float(r['hour']) for r in rs],[float(r['temporal_over_spatial']) for r in rs],'o-',label=f'Δt={dt} s')
        ax.axhline(.05,color='k',linestyle=':',label='limite 5%')
        ax.set_title(pop+' — E2, N=320');ax.set_xlabel('Tempo fisico [h]');ax.set_ylabel('Errore temporale / spaziale')
        ax.grid(True,which='both',alpha=.25);ax.legend()
    fig.suptitle('Lo stato finale passa; il transitorio a 1 h impedisce il dt uniforme proposto')
    fig.savefig(out/'spatial_accuracy_gate.png',dpi=160);fig.savefig(out/'spatial_accuracy_gate.pdf');plt.close(fig)

    fig,ax=plt.subplots(figsize=(8,5),constrained_layout=True)
    ref=json.loads((a.gate.parent/'reference_convergence.json').read_text())
    valid=[r for r in ref if r['max_reference_gap_over_spatial_error'] is not None]
    ax.loglog([r['N'] for r in valid],[r['max_reference_gap_over_spatial_error'] for r in valid],'o-')
    ax.axhline(.01,color='k',linestyle=':');ax.set_xlabel('N reference');ax.set_ylabel('Gap reference / troncamento N=320')
    ax.set_title('Reference E2 convergente prima della stima di costo');ax.grid(True,which='both',alpha=.25)
    fig.savefig(out/'reference_convergence.png',dpi=160);fig.savefig(out/'reference_convergence.pdf');plt.close(fig)
    rv.save(out/'summary.json',{'status':'cost_gate_stop','benchmark_simulations':5,'accuracy_precheck_simulations':2,
        'main_temporal_simulations':0,'main_spatial_simulations':0,'fast_transient_simulations':0,
        'lhs_samples':0,'binding_asymptotic_p1_estimate':worst,
        'measured_benchmark_cpp_solver_seconds_total':sum(r['solver_seconds'] for r in rows),
        'measured_benchmark_python_seconds_total':sum(r['python_analysis_seconds'] for r in rows),
        'measured_benchmark_complete_seconds_total':sum(r['complete_case_including_IO_seconds'] for r in rows),
        'note':'Four diagnostic figure pairs, not convergence plots for an unexecuted 110-run campaign.'})
    print('Postprocessing only; no simulations or LHS launched. Conservative p=1 cost:',worst)


if __name__=='__main__':main()
