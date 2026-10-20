#!/usr/bin/env python3
"""Resolve each fast-test modal coefficient, including the fastest last mode."""
import sys
sys.dont_write_bytecode=True
import json
import csv
import time
import os
from reference import HERE, rv, np
from campaign import CACHE, RESULTS
from analyze import order
from common import write_csv
os.environ['MPLCONFIGDIR']=str(HERE/'mplconfig')
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt


def main():
    out=HERE/'rapid_modal';out.mkdir(exist_ok=False);start=time.perf_counter()
    ledger=json.loads((HERE/'rapid_ledger.json').read_text());rows=[];orders=[]
    for record in ledger:
        name=record['case'];n=record['N'];resolution=record['resolution']
        with np.load(CACHE/f'rapid_{name}_N{n}.npz') as z:exact=z['modes']
        with np.load(RESULTS/'rapid'/name/f'N{n}_tau{resolution}'/'sampled.npz') as z:cpp=z['modes']
        delta=cpp-exact
        for p,pop in enumerate(['c','mb','md']):
            for k in range(n):
                scale=max(abs(exact[:,p,k]).max(),1.)
                rows.append({'case':name,'N':n,'resolution':resolution,'dt_s':record['dt_s'],
                    'population':pop,'mode':k+1,'active':bool(abs(exact[:,p,k]).max()>0),'scale':scale,
                    'max_absolute':float(abs(delta[:,p,k]).max()),'max_normalized':float(abs(delta[:,p,k]).max()/scale),
                    'final_normalized':float(abs(delta[-1,p,k])/scale)})
    write_csv(out/'errors_per_mode.csv',rows)
    groups={}
    for row in rows:groups.setdefault((row['case'],row['N'],row['population'],row['mode']),[]).append(row)
    for identity,group in groups.items():
        for a,b in zip(group,group[1:]):
            p,status=order(a['max_normalized'],b['max_normalized'],floor=1e-11)
            orders.append({'case':identity[0],'N':identity[1],'population':identity[2],'mode':identity[3],
                'coarse_resolution':a['resolution'],'fine_resolution':b['resolution'],'coarse_error':a['max_normalized'],
                'fine_error':b['max_normalized'],'order':p,'status':status})
    write_csv(out/'orders_per_mode.csv',orders)
    fastest=[r for r in rows if r['mode']==r['N']]
    write_csv(out/'fastest_mode_errors.csv',fastest)
    finest=[r for r in orders if r['mode']==r['N'] and r['fine_resolution']==160 and r['status']=='resolved']
    summary={'rapid_runs':60,'modal_error_rows':len(rows),'modal_order_rows':len(orders),
       'fastest_mode_finest_order_range':[min(r['order'] for r in finest),max(r['order'] for r in finest)],
       'fastest_mode_max_error_finest':max(r['max_normalized'] for r in fastest if r['resolution']==160),
       'normalization':'each retained modal coefficient uses its own analytical maximum in the short window; floor 1',
       'seconds':time.perf_counter()-start}
    # Illustrate the fastest individual mode, which can have tiny weight in
    # mean/field norms and must not be hidden by those aggregate observables.
    fig,axs=plt.subplots(1,3,figsize=(13,4.5))
    name=next(r['case'] for r in ledger if r['case'].startswith('E2_'));n=320
    with np.load(CACHE/f'rapid_{name}_N{n}.npz') as z:exact=z['modes'];t=z['time']
    tau=next(r['tau_fast_s'] for r in ledger if r['case']==name and r['N']==n)
    for resolution in [10,20,40,80,160]:
        with np.load(RESULTS/'rapid'/name/f'N{n}_tau{resolution}'/'sampled.npz') as z:cpp=z['modes']
        for p,pop in enumerate(['c','mb','md']):
            scale=max(abs(exact[:,p,-1]).max(),1.)
            error=abs(cpp[:,p,-1]-exact[:,p,-1])/scale;mask=error>0
            axs[p].semilogy(t[mask]/tau,error[mask],label=f'Δt=τ/{resolution}')
            axs[p].set_title(f'E2: {pop}, modo 320');axs[p].set_xlabel('t / τ rapido')
            axs[p].set_ylabel('Errore / massimo analitico del modo');axs[p].grid(alpha=.25)
    axs[1].text(.5,.5,'Bulk inattivo: errore esattamente zero',ha='center',transform=axs[1].transAxes,fontsize=8)
    axs[0].legend(fontsize=7);fig.suptitle('Verifica del modo più rapido, separata dalle medie e dalla norma aggregata')
    fig.tight_layout(rect=(0,0,1,.94))
    fig.savefig(out/'fastest_mode_history.png',dpi=180);fig.savefig(out/'fastest_mode_history.pdf');plt.close(fig)
    summary['seconds_including_plot']=time.perf_counter()-start
    rv.save(out/'summary.json',summary);print(json.dumps(summary,indent=2))


if __name__=='__main__':main()
