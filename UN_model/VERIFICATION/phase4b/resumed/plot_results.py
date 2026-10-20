#!/usr/bin/env python3
"""Publication-ready PNG/PDF from the completed deterministic campaign."""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import os
import csv
import json
import time
from reference import HERE, ROOT, rv, np, HOURS, hourly
from campaign import CACHE, RESULTS, load_cpp

os.environ['MPLCONFIGDIR']=str(HERE/'mplconfig')
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

DATA=HERE/'analysis_v2'; OUT=HERE/'figures'
POPS=['c','mb','md']
LABELS={'c':'Gas in soluzione c','mb':'Gas nelle bolle bulk mb','md':'Gas su dislocazioni md'}


def read(name):
    with open(DATA/name,newline='') as f:return list(csv.DictReader(f))


def key(case):return case.split('_')[0]


def positive(x,y):
    x=np.asarray(x);y=np.asarray(y);keep=np.isfinite(y)&(y>0)&(x>0)
    return x[keep],y[keep]


def save(fig,name,title):
    fig.suptitle(title,fontsize=13)
    fig.tight_layout(rect=(0,0,1,.94))
    fig.savefig(OUT/(name+'.png'),dpi=180)
    fig.savefig(OUT/(name+'.pdf'))
    plt.close(fig)


def main():
    OUT.mkdir(exist_ok=False);start=time.perf_counter()
    cases=json.loads((HERE.parent/'cases.json').read_text());names=[c['name'] for c in cases]
    colors={name:plt.get_cmap('tab10')(i) for i,name in enumerate(names)}
    t=read('temporal_summary.csv');to=read('temporal_orders.csv');s=read('spatial_errors.csv')
    fast=read('rapid_summary.csv');fo=read('rapid_orders.csv')
    plt.rcParams.update({'font.size':9,'axes.grid':True,'grid.alpha':.25})
    fig,axs=plt.subplots(2,3,figsize=(14,8))
    for j,pop in enumerate(POPS):
        for name in names:
            subset=sorted([r for r in t if r['case']==name and r['population']==pop],key=lambda r:float(r['dt_s']))
            for i,quantity in enumerate(['max_mean_normalized','max_modal_L2_normalized']):
                x,y=positive([float(r['dt_s']) for r in subset],[float(r[quantity]) for r in subset])
                axs[i,j].loglog(x,y,'o-',color=colors[name],label=key(name),markersize=3)
                axs[i,j].set_xlabel('Δt [s]');axs[i,j].set_ylabel('Errore max / scala della popolazione')
                axs[i,j].set_title(LABELS[pop]+(' — media' if i==0 else ' — L2 modale'))
    axs[0,0].legend(ncol=2,fontsize=7)
    save(fig,'temporal_convergence','N=40, 4000 h — errore temporale C++ rispetto a expm allo stesso N; campioni orari')

    fig,axs=plt.subplots(1,3,figsize=(14,4.7))
    for j,pop in enumerate(POPS):
        for name in names:
            subset=[r for r in to if r['case']==name and r['population']==pop and r['quantity']=='max_mean_normalized'
                    and r['status']=='resolved' and r['order']]
            axs[j].semilogx([float(r['fine_dt_s']) for r in subset],[float(r['order']) for r in subset],'o-',
                           color=colors[name],label=key(name),markersize=3)
        axs[j].axhline(1,color='k',linestyle=':',label='Ordine BE = 1')
        axs[j].set_title(LABELS[pop]);axs[j].set_xlabel('Δt fine [s]');axs[j].set_ylabel('Ordine osservato, errore massimo')
    axs[0].legend(ncol=2,fontsize=7)
    save(fig,'temporal_orders','Ordini temporali — esclusi componenti inattivi e valori sotto il limite della reference')

    for quantity,filename,label in [('spatial_mean_normalized','spatial_mean_convergence','media'),
                                     ('spatial_modal_L2_normalized','spatial_L2_convergence','campo in norma L2')]:
        fig,axs=plt.subplots(2,3,figsize=(14,8))
        for i,hour in enumerate([1,4000]):
            for j,pop in enumerate(POPS):
                for name in names:
                    subset=[r for r in s if r['case']==name and r['population']==pop and float(r['hour'])==hour]
                    x,y=positive([int(r['N']) for r in subset],[float(r[quantity]) for r in subset])
                    axs[i,j].loglog(x,y,'o-',color=colors[name],label=key(name),markersize=3)
                axs[i,j].set_title(f'{LABELS[pop]} — {hour} h');axs[i,j].set_xlabel('N modi')
                axs[i,j].set_ylabel('Solo troncamento analitico / scala high-N')
        axs[0,0].legend(ncol=2,fontsize=7)
        save(fig,filename,f'Convergenza spaziale della {label}: analitica stesso N − analitica high-N; C++ grezzo separato')

    fig,axs=plt.subplots(2,2,figsize=(12,8))
    name=next(n for n in names if n.startswith('E2_'))
    for i,hour in enumerate([1,4000]):
        for j,pop in enumerate(['c','md']):
            subset=[r for r in s if r['case']==name and r['population']==pop and float(r['hour'])==hour]
            for q,style,label in [('total_mean_normalized','o-','Totale C++ − high-N'),
                                   ('temporal_mean_normalized','s--','Temporale / implementazione'),
                                   ('spatial_mean_normalized','^-','Solo spaziale analitico')]:
                x,y=positive([int(r['N']) for r in subset],[float(r[q]) for r in subset])
                axs[i,j].loglog(x,y,style,label=label,markersize=4)
            axs[i,j].set_xlabel('N modi');axs[i,j].set_ylabel('Errore / scala high-N')
            axs[i,j].set_title(f'E2, {pop}, {hour} h')
    axs[0,0].legend(fontsize=8)
    save(fig,'error_decomposition_E2','Δt=225 s — il totale grezzo non rappresenta il puro troncamento spaziale')

    fig,axs=plt.subplots(1,3,figsize=(14,4.7))
    selected=[c for c in cases if key(c['name']) in ['R3','E2','E3','E5']]
    for case in selected:
        ref,_,_=hourly(case,40,CACHE);scales=np.maximum(abs(ref).max(axis=0),1.)
        for dt,style in [(3600.,'--'),(225.,'-')]:
            record=json.loads((RESULTS/'temporal'/case['name']/f'N40_dt{dt:g}'/'cpp.json').read_text())
            t,means,_,_=load_cpp(ROOT/record['source'],40)
            for j,pop in enumerate(POPS):
                x,y=positive(t/3600,abs(means[:,j]-ref[:,j])/scales[j])
                axs[j].loglog(x,y,style,color=colors[case['name']],label=f'{key(case["name"])} dt={dt:g}s')
                axs[j].set_title(LABELS[pop]);axs[j].set_xlabel('Tempo [h]');axs[j].set_ylabel('Errore medio / scala della storia')
    axs[0].legend(ncol=2,fontsize=7)
    save(fig,'error_histories','Errore durante 4000 h, N=40 — osservazioni orarie; primi secondi verificati separatamente')

    fig,axs=plt.subplots(2,3,figsize=(14,8))
    for j,pop in enumerate(POPS):
        for name in names:
            subset=[r for r in fast if r['case']==name and r['population']==pop and int(r['N'])==320]
            for i,q in enumerate(['max_mean_normalized','max_modal_L2_normalized']):
                x,y=positive([1/float(r['resolution']) for r in subset],[float(r[q]) for r in subset])
                axs[i,j].loglog(x,y,'o-',color=colors[name],label=key(name),markersize=3)
                axs[i,j].set_title(LABELS[pop]+(' — media' if i==0 else ' — L2 modale'))
                axs[i,j].set_xlabel('Δt / τ rapido');axs[i,j].set_ylabel('Errore / scala della finestra breve')
    axs[0,0].legend(ncol=2,fontsize=7)
    save(fig,'fast_transient_convergence','Prove brevi a N=320 — durata 20 τ rapido, osservazioni ogni 0,5 τ')

    fig,axs=plt.subplots(1,2,figsize=(12,4.7))
    for name in names:
        info=json.loads((CACHE/f'{name}_high_reference.json').read_text())
        for j,q in enumerate(['max_mean_increment_over_N320_spatial','max_tail_energy_relative_increment']):
            records=[r for r in info['records'] if r[q] is not None]
            x,y=positive([r['N'] for r in records],[r[q] for r in records])
            axs[j].loglog(x,y,'o-',label=key(name),color=colors[name],markersize=3)
            axs[j].axhline(.01,color='k',linestyle=':')
            axs[j].set_xlabel('N reference');axs[j].set_ylabel('Incremento al raddoppio / scala verificata')
    axs[0].set_title('Media: incremento / troncamento N=320');axs[1].set_title('Energia della coda: incremento relativo')
    axs[0].legend(ncol=2,fontsize=7)
    save(fig,'high_reference_convergence','Convergenza effettiva high-N — massimo su popolazioni attive e tempi osservati')

    info={'figures':sorted(p.name for p in OUT.glob('*.png')),'png_dpi':180,'formats':['PNG','PDF'],
          'seconds':time.perf_counter()-start,'data':str(DATA.relative_to(ROOT)),
          'normalization':'per population; mean and modal L2 scales are separate',
          'no_zero_on_log_axes':True,'LHS':0}
    rv.save(OUT/'manifest.json',info);print(json.dumps(info,indent=2))


if __name__=='__main__':main()
