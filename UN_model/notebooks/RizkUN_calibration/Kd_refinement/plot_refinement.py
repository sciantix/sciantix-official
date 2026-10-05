"""Plot saved results only; no model calls."""
from pathlib import Path
import os
os.environ.setdefault('MPLBACKEND','Agg')
os.environ.setdefault('MPLCONFIGDIR','/tmp/rizkun_calibration_matplotlib')
import json
import pandas as pd
import matplotlib.pyplot as plt

OUT=Path(__file__).resolve().parent

def plot_refinement(output_dir=OUT):
    output_dir=Path(output_dir)
    data=pd.read_csv(output_dir/'comparison_runs.csv');points=pd.read_csv(output_dir/'comparison_points.csv')
    plotdir=output_dir/'plots';plotdir.mkdir(exist_ok=True)
    styles={'K_d_low':('#2166ac','--'), 'K_d_150000':('#4393c3','-'), 'K_d_200000':('#d6604d','-'), 'K_d_250000':('#8c510a','-'), 'baseline':('#222222','--')}
    metrics={'swelling_d_percent':('P2 swelling','% volume'),'Rd_nm':('R_d (selection ≤1600 K)','nm'),'Nd':('N_d (diagnostic)','m^-3'),
             'matrix_gas_percent':('Matrix gas','% generated gas'),'bulk_gas_percent':('Bulk gas','% generated gas'),
             'dislocation_gas_percent':('Dislocation gas','% generated gas'),'grainface_gas_percent':('Grain-face gas','% generated gas'),
             'release_gas_percent':('FGR (guardrail)','% generated gas'),'Rgf_nm':('R_gf (guardrail)','nm'),'Ngf_vol':('N_gf volumetric (guardrail)','m^-3')}
    saved=[]
    for case in ['AP3.2','AP3.8','AP3.4','ANP6']:
        fig,axes=plt.subplots(2,5,figsize=(22,8),sharex=True)
        for ax,(metric,(title,unit)) in zip(axes.flat,metrics.items()):
            for candidate,(color,linestyle) in styles.items():
                s=data[data.candidate.eq(candidate)&data.experiment_case.eq(case)].sort_values('T')
                ax.plot(s['T'],s[metric],color=color,linestyle=linestyle,lw=2.2 if candidate=='K_d_200000' else 1.5,label=f'K_d={s.K_d.iloc[0]:g}')
            metric_point={'swelling_d_percent':'P2_swelling','Rd_nm':'R_d','Nd':'N_d'}.get(metric,metric)
            reference=points[points.candidate.eq('baseline')&points['case'].eq(case)&points.metric.eq(metric_point)&points.T_K.between(900,1800)]
            if metric in ['swelling_d_percent','Rd_nm','Nd']:
                reference=reference[reference.data_type.eq('experiment')]
                if metric=='Rd_nm':
                    for low in [True,False]:
                        p=reference[reference.T_K.le(1600) if low else reference.T_K.gt(1600)]
                        ax.scatter(p.T_K,p.observed,s=24,marker='^',facecolor='black' if low else 'white',edgecolor='black',label='P2 exp ≤1600 K' if low else 'P2 high-T diagnostic',zorder=5)
                    ax.axvspan(1600,1800,color='#eee',alpha=.5,zorder=0)
                else:ax.scatter(reference.T_K,reference.observed,color='black',s=20,marker='^',label='P2 experiment (N diagnostic)',zorder=5)
            elif metric.endswith('_gas_percent'):
                reference=reference[reference.data_type.eq('Rizk2025_model_benchmark')]
                if len(reference):ax.plot(reference.T_K,reference.observed,':',color='gray',lw=2,label='Rizk model benchmark')
            if metric in ['Nd','Ngf_vol']:ax.set_yscale('log')
            ax.set_title(title);ax.set_xlabel('T [K]');ax.set_ylabel(unit);ax.set_xlim(900,1800);ax.axvline(1600,color='#aaa',lw=.8,ls=':');ax.grid(alpha=.2)
        handles=[];labels=[]
        for index in [0,1,3]:
            h,l=axes.flat[index].get_legend_handles_labels();handles+=h;labels+=l
        unique=dict(zip(labels,handles));fig.legend(unique.values(),unique.keys(),loc='lower center',ncol=5,fontsize=8)
        fig.suptitle(f'K_d refinement only | {case} | f_n=5.5e-4, rho_d=3e13, Dv_d=10, Dg_d=13, Ngf0 factor=1 | dt=1h, 40 modes')
        fig.tight_layout(rect=(0,.08,1,.95));path=plotdir/f'Kd_comparison_{case.replace(".","p")}.png';fig.savefig(path,dpi=140);plt.close(fig);saved.append(str(path))
        fig,axes=plt.subplots(2,3,figsize=(16,8),sharex=True)
        for ax,col in zip(axes.flat,['p_b','p_d','p_gf','p_b_over_eq','p_d_over_eq','p_gf_over_eq']):
            for candidate,(color,linestyle) in styles.items():
                s=data[data.candidate.eq(candidate)&data.experiment_case.eq(case)].sort_values('T')
                ax.plot(s['T'],s[col],color=color,ls=linestyle,label=f'K_d={s.K_d.iloc[0]:g}')
            ax.set_title(col);ax.set_yscale('symlog',linthresh=1e-5 if 'over_eq' in col else 1);ax.set_xlabel('T [K]');ax.set_ylabel('ratio' if 'over_eq' in col else 'Pa');ax.grid(alpha=.2)
        h,l=axes.flat[0].get_legend_handles_labels();fig.legend(h,l,loc='lower center',ncol=5,fontsize=9)
        fig.suptitle(f'{case}: pressures as guardrails only');fig.tight_layout(rect=(0,.06,1,.95))
        path=plotdir/f'Kd_pressure_{case.replace(".","p")}.png';fig.savefig(path,dpi=140);plt.close(fig);saved.append(str(path))
    (output_dir/'plot_manifest.json').write_text(json.dumps(saved,indent=2));print(f'Saved {len(saved)} figures from existing results.')
    return saved

if __name__=='__main__':plot_refinement()
