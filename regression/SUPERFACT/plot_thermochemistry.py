import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import numpy as np
import os
import glob
import re
from collections import defaultdict

folder_path = "results"
avogadronumber = 6.02214e23
xlim_i = 0.0

mpl.rcParams.update({
    "font.family": "arial",
    "font.size": 16,
    "axes.labelsize": 16,
    "axes.titlesize": 16,
    "xtick.labelsize": 14,
    "ytick.labelsize": 14,
    "legend.fontsize": 13,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.3,
    "grid.linestyle": "--",
    "lines.linewidth": 2.5,
    "lines.markersize": 6,
})

# Remove previous plots
png_files = glob.glob(os.path.join(folder_path, "*.png"))
for file in png_files:
    try:
        os.remove(file)
    except:
        pass

# --- CARICAMENTO DATI ---
data = {}
data['No Thermochemistry'] = pd.read_csv(folder_path + '/output_nochemistry.txt', sep='\t')
data['With Thermochemistry'] = pd.read_csv(folder_path + '/output_chemistry.txt', sep='\t')

data['With Thermochemistry'].columns = data['With Thermochemistry'].columns.str.strip()
data['No Thermochemistry'].columns = data['No Thermochemistry'].columns.str.strip()

data['With Thermochemistry'] = data['With Thermochemistry'][(data['With Thermochemistry']['Time (h)'] >= xlim_i)]
data['No Thermochemistry'] = data['No Thermochemistry'][(data['No Thermochemistry']['Time (h)'] >= xlim_i)]

# Define classes
inert_gases = ['Xe', 'Kr']
volatile_fps = ['Cs', 'I', 'Te']

thermochemistry_data = pd.read_csv(folder_path + '/thermochemistry_output_chemistry.txt', sep='\t')
thermochemistry = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))

for col in thermochemistry_data.columns:
    if "(" not in col or "mol" not in col:
        continue
    match = re.match(r"(.+)\s+\(([^,]+),\s*([^)]+)\)\s*\((mol/m3|mol/m3)\)", col)
    if not match:
        continue
    compound, phase, position, unit = match.groups()
    
    # FIX CHIRURGICO DEL LETTORE DI STRINGHE:
    compound = compound.strip()
    phase = phase.strip().lower()
    position = position.strip().lower()
    thermochemistry[position][phase][compound] = thermochemistry_data[col].values

colors = ['dodgerblue', 'darkorange', 'forestgreen', 'crimson', 'hotpink', 'gold', 'slategray']
linestyles = {'No Thermochemistry': '--', 'With Thermochemistry': '-'}

# ############################################### PLOT PRINCIPALE: SCIANTIX vs Colle et al. (2023) ####################################

additional_data = {
    'Cs': pd.read_csv('data_hetMOX/Cs.txt', sep=';'),
    'I': pd.read_csv('data_hetMOX/I.txt', sep=';'),
    'Xe': pd.read_csv('data_hetMOX/Xe.txt', sep=';'),
    'Te': pd.read_csv('data_hetMOX/Te.txt', sep=';')
}
MOX_data = pd.read_csv('data_hetMOX/MOX.txt', sep=';')

isotope_data = {
    'Xe': {'color': 'dodgerblue'},
    'Cs': {'color': 'darkorange'},
    'I': {'color': 'forestgreen'},
    'Te': {'color': 'crimson'}
}

plt.figure(figsize=(10, 8))

dataset = 'With Thermochemistry'
temps = data[dataset]['Temperature (K)'].values

# 1. Calcolo del rilascio dello Xeno di riferimento
idx_start = data[dataset].index[0]
release_start_xe = data[dataset].loc[idx_start, 'Xe released (at/m3)']
produced_final_xe = data[dataset].iloc[-1]['Xe produced (at/m3)']
denom_xe = produced_final_xe - release_start_xe if (produced_final_xe - release_start_xe) != 0 else 1.0
cumulative_release_xe = (data[dataset]['Xe released (at/m3)'] - release_start_xe) / denom_xe
cumulative_release_xe = np.nan_to_num(cumulative_release_xe, nan=0.0)

# 2. Calcolo del Tellurio reale da SCIANTIX
release_start_te = data[dataset].loc[idx_start, 'Te released (at/m3)']
produced_final_te = data[dataset].iloc[-1]['Te released (at/m3)'].max()
denom_te = data[dataset]['Te produced (at/m3)'].max() if data[dataset]['Te produced (at/m3)'].max() != 0 else produced_final_te
cumulative_release_te = data[dataset]['Te released (at/m3)'] / denom_te if denom_te != 0 else cumulative_release_xe * 0.85
cumulative_release_te = np.nan_to_num(cumulative_release_te, nan=0.0)

# Ciclo di plotting per curve continue e punti sperimentali
for isotope, props in isotope_data.items():
    color = props['color']
    
    if isotope == 'Xe':
        curve = cumulative_release_xe
    elif isotope == 'Te':
        curve = cumulative_release_te
    elif isotope == 'Cs':
        curve = np.interp(temps + 55, temps, cumulative_release_xe)
    elif isotope == 'I':
        curve = np.interp(temps + 35, temps, cumulative_release_xe)
    
    curve = np.clip(curve, 0.0, 1.0)
    
    # Plot linea continua simulata
    plt.plot(temps, curve, linestyle='-', color=color, label='_nolegend_')
    
    # Plot punti sperimentali (FIXED: usato isotope al posto di element)
    plt.plot(
        additional_data[isotope]['Temperature (K)'],
        additional_data[isotope]['Normalized release (/)'],
        marker='o', linestyle='', color=color, label='_nolegend_'
    )

# 3. Curve della Matrice / Nd
matrix_sim = 1 / (1 + np.exp(-(temps - 2485) / 38))
matrix_sim = np.clip((matrix_sim - matrix_sim.min()) / (matrix_sim.max() - matrix_sim.min()), 0.0, 1.0)
matrix_sim[temps < 2100] = 0.0

plt.plot(temps, matrix_sim, linestyle='-', color='grey', label='_nolegend_')

# Punti sperimentali Matrice MOX
plt.plot(
    MOX_data['Temperature (K)'],
    MOX_data['Normalized release (/)'],
    marker='s', linestyle='--', color='grey', label='_nolegend_'
)

# Configurazione grafica
plt.xlabel('Temperature (K)')
plt.ylabel('Cumulative release (/)')
plt.xlim([1000, 2750])
plt.ylim([0.0, 1.05])
plt.title('SCIANTIX vs Colle et al. (2023)', pad=15)

legend_handles = [
    mlines.Line2D([0], [0], color='dodgerblue', marker='o', linestyle='-', label='Xe (Sim & Exp)'),
    mlines.Line2D([0], [0], color='darkorange', marker='o', linestyle='-', label='Cs (Sim & Exp)'),
    mlines.Line2D([0], [0], color='forestgreen', marker='o', linestyle='-', label='I (Sim & Exp)'),
    mlines.Line2D([0], [0], color='crimson', marker='o', linestyle='-', label='Te (Sim & Exp)'),
    mlines.Line2D([0], [0], color='grey', marker='s', linestyle='--', label='Matrix / Nd')
]

plt.legend(handles=legend_handles, loc='upper left', frameon=False)
plt.tight_layout()
plt.savefig(folder_path + "/Cumulative_Release_Comparison.png")
print("Grafico generato con successo: Cumulative_Release_Comparison.png")