import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from scipy.optimize import brentq
from matplotlib.lines import Line2D

# --- 1. FUNZIONI TERMODINAMICHE (Benchmark Python) ---
def calculate_om(log10_po2, T_K, C_pu, R=8.314):
    po2 = 10**log10_po2
    ln_po2 = np.log(po2)
    log_v1 = -5 * ((44.0 + 55.8 * C_pu) / R - 376000 / (R * T_K) - 0.5 * ln_po2)
    log_v2 = -5 * (0.5 * (68.8 + 131.3 * C_pu) / R - 0.5 * 515000 / (R * T_K) - 0.25 * ln_po2)
    log_v3 = -5 * (((1/3) * (np.log(2) + (153.5 - 96.5 * C_pu + 331.0 * C_pu**2) / R - 891000 / (R * T_K))) - (1/3) * ln_po2)
    log_v4 = -5 * np.log(0.5 * C_pu)
    max_log = np.maximum.reduce([log_v1, log_v2, log_v3, log_v4])
    sum_exp = np.exp(log_v1 - max_log) + np.exp(log_v2 - max_log) + np.exp(log_v3 - max_log) + np.exp(log_v4 - max_log)
    S = np.exp(-1/5 * (max_log + np.log(sum_exp)))
    term5 = np.exp((-22.8 - 84.5 * C_pu) / R + 105000 / (R * T_K) + 0.5 * ln_po2)
    return 2 - S + term5

def solve_for_po2(target_om, T_K, C_pu, R=8.314):
    def objective(log_po2): return calculate_om(log_po2, T_K, C_pu, R) - target_om
    try:
        sol_log = brentq(objective, -160, 40) 
        return 10**sol_log
    except ValueError: return None

def solve_all_pressures(T_K, po2, C_pu):
    # Costanti di equilibrio dalle equazioni di Olander
    K_u24 = np.exp(-(78.3e3 / T_K) + 13.6)
    K_u46 = np.exp(-(16.4e3 / T_K) + 5.0)
    K_pu34 = np.exp(-(50.1e3 / T_K) + 10.3)
    K_pu23 = np.exp(-(92.5e3 / T_K) + 21.3)
    
    # Costanti di vaporizzazione (Tabella 11.1 Olander)
    K_UO   = np.exp(-49.5e3 / T_K + 11.9)
    K_UO2  = np.exp(-74.0e3 / T_K + 19.9)
    K_UO3  = np.exp(-44.0e3 / T_K + 11.9)
    K_PuO  = np.exp(-44.1e3 / T_K + 11.5)
    K_PuO2 = np.exp(-72.5e3 / T_K + 18.8)
    
    sqrt_po2 = np.sqrt(po2)
    
    # Concentrazioni Uranio
    denom_u = 1 + ((K_u24 * 2.0) / sqrt_po2) + (sqrt_po2 / (K_u46 * 2.0))
    n_u4 = (1 - C_pu) / denom_u
    n_u2 = ((K_u24 * 2.0) / sqrt_po2) * n_u4
    n_u6 = (sqrt_po2 / (K_u46 * 2.0)) * n_u4
    
    # Concentrazioni Plutonio
    ratio_34 = np.sqrt((K_pu34 * 2.0) / sqrt_po2)
    ratio_23 = np.sqrt((K_pu23 * 2.0) / sqrt_po2)
    denom_pu = 1 + ratio_34 + (ratio_23 * ratio_34)
    n_pu4 = C_pu / denom_pu
    n_pu3 = ratio_34 * n_pu4
    n_pu2 = ratio_23 * n_pu3
    
    # Ritorna tutte le 6 pressioni parziali
    return {
        'p_O2': po2,
        'p_UO': 2 * K_UO * n_u2,
        'p_UO2': 4 * K_UO2 * n_u4,
        'p_UO3': 8 * K_UO3 * n_u6,
        'p_PuO': 2 * K_PuO * n_pu2,
        'p_PuO2': 4 * K_PuO2 * n_pu4
    }

# --- 2. CARICAMENTO E CALCOLO ---
df = pd.read_csv("vapor_results.csv")
df.columns = df.columns.str.strip()

val_q, val_om_fix = 0.244, 1.98
species_colors = {
    'p_O2': 'black',
    'p_UO': 'blue',
    'p_UO2': 'green',
    'p_UO3': 'cyan',
    'p_PuO': 'red',
    'p_PuO2': 'orange'
}
brentq_results = {k: [] for k in species_colors.keys()}
valid_times = []

for i in range(len(df)):
    T_curr = df.iloc[i]['T']
    po2_ref = solve_for_po2(val_om_fix, T_curr, val_q)
    if po2_ref is not None:
        press = solve_all_pressures(T_curr, po2_ref, val_q)
        valid_times.append(df.iloc[i]['Time'])
        for k in species_colors.keys(): brentq_results[k].append(press[k])

# --- 3. PLOT ---
fig, ax1 = plt.subplots(figsize=(13, 8))

# Temperatura (Asse 1)
ax1.plot(df['Time'], df['T'], color='#d65f5f', linewidth=4, label='Temperature', alpha=0.8)
ax1.set_ylabel('Temperature (K)', color='#d65f5f', fontsize=12, fontweight='bold')
ax1.tick_params(axis='y', labelcolor='#d65f5f')

# Pressioni (Asse 2)
ax2 = ax1.twinx()
ax2.set_yscale('log')
ax2.set_ylabel('Partial Pressure (atm)', fontsize=12, fontweight='bold')

for spec, color in species_colors.items():
    ax2.plot(df['Time'], df[spec], color=color, linestyle='--', linewidth=2, alpha=0.9)
    ax2.scatter(valid_times, brentq_results[spec], color=color, marker='o', s=30, alpha=0.7, zorder=5)

ax1.set_xlabel('Time (s)', fontsize=12)
ax1.grid(True, which="both", linestyle=':', alpha=0.5)

# --- DOPPIA LEGENDA ---
method_legend = [
    Line2D([0], [0], color='gray', linestyle='--', lw=2, label='Sciantix (C++)'),
    Line2D([0], [0], marker='o', color='w', markerfacecolor='gray', markersize=10, label='Brentq (Python)')
]
l1 = ax2.legend(handles=method_legend, loc='upper left', title="Numerical Method")
ax2.add_artist(l1)

species_legend = [Line2D([0], [0], color=c, lw=2, label=s.replace('p_', '')) for s, c in species_colors.items()]
ax2.legend(handles=species_legend, loc='center right', title="Gas Species")

plt.title(f'Verification: Step Response Analysis\n(q={val_q}, O/M={val_om_fix})')
plt.tight_layout()
plt.show()

print("\n--- Generating Quantified Error Analysis Plots ---")

# Inizializziamo i contenitori per gli errori
abs_log_errors = {k: [] for k in species_colors.keys()}
rel_errors = {k: [] for k in species_colors.keys()}
valid_x = []

# Scegli l'asse X a seconda del test:
# Per Oscillation/Step usa: df.iloc[i]['Time']
# Per la Ramp usa: df.iloc[i]['T']
is_time_dependent = 'Time' in df.columns and len(df['Time'].unique()) > 1

for i in range(len(df)):
    T_curr = df.iloc[i]['T']
    po2_ref = solve_for_po2(val_om_fix, T_curr, val_q)
    
    if po2_ref is not None:
        press_py = solve_all_pressures(T_curr, po2_ref, val_q)
        
        # Scegliamo la coordinata X corretta per il punto
        if is_time_dependent:
            valid_x.append(df.iloc[i]['Time'])
        else:
            valid_x.append(T_curr)
            
        for spec in species_colors.keys():
            p_cpp = df.iloc[i][spec]
            p_py = press_py[spec]
            
            if p_cpp > 0 and p_py > 0:
                # Errore Assoluto calcolato sui logaritmi in base 10
                abs_log_err = abs(np.log10(p_cpp) - np.log10(p_py))
                # Errore Relativo classico
                rel_err = (p_cpp - p_py) / p_py
            else:
                abs_log_err = np.nan
                rel_err = np.nan
                
            abs_log_errors[spec].append(abs_log_err)
            rel_errors[spec].append(rel_err)

# Creazione del grafico a due pannelli verticali
fig, (ax_abs, ax_rel) = plt.subplots(2, 1, figsize=(12, 10), sharex=True)

# Definiamo le etichette dell'asse X
x_label = 'Time (s)' if is_time_dependent else 'Temperature (K)'

for spec, color in species_colors.items():
    label_clean = spec.replace('p_', '')
    # 1. Plot dell'Errore Assoluto Logaritmico
    ax_abs.plot(valid_x, abs_log_errors[spec], color=color, marker='o', markersize=3, linestyle='-', linewidth=1, label=label_clean)
    # 2. Plot dell'Errore Relativo
    ax_rel.plot(valid_x, rel_errors[spec], color=color, marker='o', markersize=3, linestyle='-', linewidth=1)

# Configurazione Pannello Errore Assoluto Logaritmico
ax_abs.set_yscale('log') # Indispensabile per apprezzare scostamenti infinitesimi
ax_abs.set_ylabel(r'$| \log_{10}(P_{C++}) - \log_{10}(P_{Py}) |$', fontsize=12, fontweight='bold')
ax_abs.set_title('Absolute Log-Error Distribution', fontsize=13, fontweight='bold')
ax_abs.grid(True, which="both", linestyle=':', alpha=0.5)
ax_abs.legend(loc='upper right', ncol=2, fontsize='small')

# Configurazione Pannello Errore Relativo
ax_rel.set_ylabel(r'Relative Error $\frac{P_{C++} - P_{Py}}{P_{Py}}$', fontsize=12, fontweight='bold')
ax_rel.set_title('Relative Error Distribution', fontsize=13, fontweight='bold')
ax_rel.axhline(y=0, color='black', linestyle='-', linewidth=0.8, alpha=0.7)
ax_rel.grid(True, which="both", linestyle=':', alpha=0.5)

plt.xlabel(x_label, fontsize=12, fontweight='bold')
plt.tight_layout()
plt.show()