"""Additional microstructure and FGR comparisons for selected candidates."""
import sys
import model as m
import pandas as pd
import matplotlib.pyplot as plt

def plot(name):
    reference=pd.read_csv(m.ROOT/'runs/baseline/RizkUN_L3_unificato/L3_unificato_grid.csv')
    frame=pd.read_csv(m.ROOT/'runs'/name/'results.csv')
    fig,axs=plt.subplots(3,2,figsize=(14,13),constrained_layout=True)
    plt.rcParams.update({'font.size':11})
    for ax,col,label in zip(axs.flat[:4],['Rb_nm','Nb','Rgf_nm','Ngf_areal'],
                           ['$R_b$ [nm] — AP3.4','$N_b$ [m$^{-3}$] — AP3.4',
                            '$R_{gf}$ [nm] — AP3.4','$N_{gf}$ [m$^{-2}$] — AP3.4']):
        for df,style,color,legend in [(reference,'--','gray','L3 originale'),(frame,'-','#0072B2',name)]:
            sub=df[df.experiment_case=='AP3.4'].sort_values('T')
            ax.plot(sub['T'],sub[col],style,color=color,label=legend)
        ax.set_ylabel(label)
        if col in ('Nb','Ngf_areal'):ax.set_yscale('log')
        ax.legend()
    for case,color in zip(m.EXPERIMENT_CASE_ORDER,['#0072B2','#E69F00','#009E73','#CC79A7']):
        for df,style in [(reference,'--'),(frame,'-')]:
            sub=df[df.experiment_case==case].sort_values('T')
            axs[2,0].plot(sub['T'],sub.release_gas_percent,style,color=color,label=case if style=='-' else None)
            axs[2,1].plot(sub['T'],sub.psi_d,style,color=color,label=case if style=='-' else None)
    axs[2,0].set_ylabel('FGR [% del generato]')
    axs[2,1].set_ylabel(r'$R_d/\delta_d$')
    for ax in axs.flat:
        ax.set_xlim(900,1800);ax.set_xlabel('Temperatura [K]');ax.grid(alpha=.2)
    axs[2,0].legend();axs[2,1].legend()
    fig.savefig(m.ROOT/'plots'/f'{name}_additional_diagnostics.png',dpi=160)
    plt.close(fig)

if __name__=='__main__':
    for name in sys.argv[1:]:plot(name)
