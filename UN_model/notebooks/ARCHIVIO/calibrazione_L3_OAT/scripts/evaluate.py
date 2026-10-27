"""Separate observational errors and diagnostics; no aggregate score."""
import json
import numpy as np
import pandas as pd
import model as m

PARAMS = ['f_n', 'K_d', 'rho_d', 'Dv_dislocation_scale', 'Dg_dislocation_scale', 'NGF_AREAL_0']
BASE = {**m.MANUAL_PARAMS, 'NGF_AREAL_0': m.NGF_AREAL_0}
GAS = ['matrix_gas_percent', 'bulk_gas_percent', 'dislocation_gas_percent',
       'grainface_gas_percent', 'release_gas_percent']

def errors(values):
    residual = values.predicted - values.observed
    return dict(n=len(values), RMSE=float(np.sqrt(np.mean(residual**2))),
                MAPE=float(np.mean(np.abs(residual / values.observed))*100),
                bias=float(np.mean(residual)))

def evaluate(df, params):
    residuals = []
    metrics = []
    for case in m.EXPERIMENT_CASE_ORDER:
        sub = df[df.experiment_case == case].sort_values('T')
        ex = m.experimental_swelling_points_for_case(case)
        for point in ex:
            residuals.append(dict(case=case, observable='Sd_percent', T=point['T'],
                                  observed=point['swelling'],
                                  predicted=float(np.interp(point['T'], sub['T'], sub.swelling_d_percent)),
                                  boundary_900K=point['T'] < 900,
                                  role='control' if case == 'AP3.2' else 'target'))
    sub = df[df.experiment_case == 'AP3.4'].sort_values('T')
    for observable, ex, key, col, role in [('Rd_nm', m.EXP_RD_T_13, 'R_nm', 'Rd_nm', 'target'),
                                          ('Nd', m.EXP_ND_T_13, 'N', 'Nd', 'diagnostic')]:
        for point in ex:
            residuals.append(dict(case='AP3.4', observable=observable, T=point['T'],
                                  observed=point[key], predicted=float(np.interp(point['T'], sub['T'], sub[col])),
                                  boundary_900K=False, role=role))
    residuals = pd.DataFrame(residuals)
    for (case, observable), points in residuals.groupby(['case', 'observable']):
        for region, subset in [('all', points), ('low_le1500', points[points['T'] <= 1500]),
                               ('high_gt1500', points[points['T'] > 1500])]:
            if len(subset):
                metric = dict(case=case, observable=observable, region=region, **errors(subset))
                if observable == 'Nd':
                    metric['RMSE_log10'] = float(np.sqrt(np.mean(np.log10(subset.predicted/subset.observed)**2)))
                metrics.append(metric)
    ratios = {key: params[key]/BASE[key] for key in PARAMS}
    diagnostics = dict(D_cal=float(np.sqrt(np.mean(np.log10(list(ratios.values()))**2))),
                       parameter_ratios=ratios)
    frac = df[GAS].sum(axis=1)
    balance = (df.c+df.mb+df.md+df.q_gf+df.q_rel-df.generated)/df.generated
    diagnostics.update(max_relative_mass_residual=float(abs(balance).max()),
                       max_partition_sum_error_pp=float(abs(frac-100).max()),
                       max_gb_split_relative=float(abs((df.q_gb-df.q_gf-df.q_rel)/df.generated).max()),
                       max_single_size_identity_relative=float(abs((100*4*np.pi/3*df.Nd*(df.Rd_nm*1e-9)**3-df.swelling_d_percent)/df.swelling_d_percent).max()),
                       invalid_single_size_points=int((~df.valid_single_size.astype(bool)).sum()),
                       max_capture_fraction=float(df.max_f_cap_step.max()),
                       max_psi_d=float(df.psi_d.max()),
                       min_Nd=float(df.Nd.min()),
                       nonfinite_required=int((~np.isfinite(df[['c','mb','md','Nd','Rd_nm','swelling_d_percent',*GAS]])).sum().sum()),
                       negative_required=int((df[['c','mb','md','Nd','Rd_nm','swelling_d_percent',*GAS]] < 0).sum().sum()))
    summary = []
    for case in m.EXPERIMENT_CASE_ORDER:
        sub = df[df.experiment_case == case].sort_values('T')
        vals = {f'S_{t}': float(np.interp(t, sub['T'], sub.swelling_d_percent)) for t in (1500, 1590, 1600, 1700, 1800)}
        for col in ['Rd_nm','Nd','Rgf_nm','Ngf_areal',*GAS]:
            for t in (1200,1500,1600,1800):
                vals[f'{col}_{t}'] = float(np.interp(t, sub['T'], sub[col]))
        vals['S_growth_1600_1800'] = vals['S_1800']-vals['S_1600']
        summary.append(dict(case=case, **vals))
    return pd.DataFrame(metrics), residuals, diagnostics, pd.DataFrame(summary)

def save_evaluation(df, params, directory):
    metrics, residuals, diagnostics, summary = evaluate(df, params)
    metrics.to_csv(directory/'metrics.csv', index=False)
    residuals.to_csv(directory/'residuals.csv', index=False)
    summary.to_csv(directory/'temperature_summary.csv', index=False)
    (directory/'diagnostics.json').write_text(json.dumps(diagnostics, indent=2))
    return metrics, diagnostics

def contact_sheet(df, reference, name, path):
    import matplotlib.pyplot as plt
    from matplotlib.lines import Line2D
    plt.rcParams.update({'font.size':12, 'axes.labelsize':13, 'legend.fontsize':10})
    fig, axs = plt.subplots(4,2, figsize=(15,18), constrained_layout=True)
    for ax, case in zip(axs.flat[:4], m.EXPERIMENT_CASE_ORDER):
        ex = m.experimental_swelling_points_for_case(case)
        ax.scatter([p['T'] for p in ex], [p['swelling'] for p in ex], c='black', s=24, label='Esperimento')
        for frame, style, color, label in [(reference,'--','#666666','L3 originale'), (df,'-','#0072B2',name)]:
            sub = frame[frame.experiment_case == case].sort_values('T')
            ax.plot(sub['T'], sub.swelling_d_percent, style, color=color, label=label, lw=2)
        ax.set_ylabel('$S_d$ [%]')
        ax.text(.03,.95,case + (' — controllo' if case=='AP3.2' else ''), transform=ax.transAxes, va='top')
        ax.legend(loc='upper left',bbox_to_anchor=(.0,.88))
        if case=='ANP6':
            ax.axvspan(1590,1800, color='gray', alpha=.10)
            ax.text(.98,.06,'T > 1590 K: estrapolazione', ha='right', transform=ax.transAxes, fontsize=10)
    for ax, col, ex, key, ylabel in [(axs[2,0],'Rd_nm',m.EXP_RD_T_13,'R_nm','$R_d$ [nm] — AP3.4'),
                                    (axs[2,1],'Nd',m.EXP_ND_T_13,'N','$N_d$ [m$^{-3}$] — AP3.4')]:
        ax.scatter([p['T'] for p in ex],[p[key] for p in ex], c='black', s=24, label='Rizk Fig. 8' if col=='Rd_nm' else 'Rizk Fig. 7 corretta')
        for frame,style,color,label in [(reference,'--','#666666','L3 originale'),(df,'-','#0072B2',name)]:
            sub=frame[frame.experiment_case=='AP3.4'].sort_values('T')
            ax.plot(sub['T'],sub[col],style,color=color,label=label,lw=2)
        ax.set_ylabel(ylabel)
        if col=='Nd': ax.set_yscale('log')
        ax.legend(loc='upper left')
    colors = ['#0072B2','#E69F00','#009E73','#CC79A7','#D55E00']
    labels = ['Matrice','Bulk','Dislocazioni','Grain face','FGR']
    for ax,case in zip(axs[3], ['AP3.8','ANP6']):
        for frame,style in [(reference,'--'),(df,'-')]:
            sub=frame[frame.experiment_case==case].sort_values('T')
            for col,color,label in zip(GAS,colors,labels):
                ax.plot(sub['T'],sub[col],style,color=color,lw=1.8,label=label if style=='-' else None)
        ax.set_ylabel('Gas [% del generato]')
        ax.set_ylim(0,100)
        ax.text(.03,.95,case,transform=ax.transAxes,va='top')
        ax.legend(loc='upper left',bbox_to_anchor=(0,.88),ncol=2)
        ax.add_artist(ax.legend(handles=[Line2D([0],[0],color='black',ls='--',label='L3 originale'),
                                         Line2D([0],[0],color='black',label=name)],loc='lower left'))
        # Keep population legend as well as the curve-style legend.
        handles=[Line2D([0],[0],color=c,label=l) for c,l in zip(colors,labels)]
        ax.legend(handles=handles,loc='upper left',bbox_to_anchor=(0,.88),ncol=2)
    for ax in axs.flat:
        ax.set_xlim(890,1805)
        ax.set_xlabel('Temperatura [K]')
        ax.grid(alpha=.22)
    fig.savefig(path,dpi=160)
    plt.close(fig)
