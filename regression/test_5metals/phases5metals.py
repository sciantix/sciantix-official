import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# 1. Caricamento dati
output = pd.read_csv('output.txt', sep='\t')
thermo = pd.read_csv('thermochemistry_output.txt', sep='\t', low_memory=False)

output.columns = output.columns.str.strip()
thermo.columns = thermo.columns.str.strip()

# Convertiamo in numerico forzando gli errori a NaN
for col in thermo.columns:
    thermo[col] = pd.to_numeric(thermo[col], errors='coerce')

# Eliminiamo le righe dove il tempo è diventato NaN (es. le righe di intestazione ripetute)
thermo = thermo.dropna(subset=['Time (h)'])
output = output.dropna(subset=['Time (h)'])

# 2. Ricerca delle fasi
target_phases = ['bcc_a2', 'hcp_a3', 'sigma', 'fcc_a1', 'liquid']
phases_found = [p for p in target_phases if any(f"({p}," in col.lower() for col in thermo.columns)]

print("Fasi cristalline identificate:", phases_found)

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
burnup_col = 'Burnup (MWd/kgMOX)'

for p in phases_found:
    phase_cols = [c for c in thermo.columns if f"({p}," in c.lower()]
    
    if phase_cols:
        phase_moles = thermo[phase_cols].sum(axis=1)
        
        # --- FILTRO ANTI-PICCO NUMERICO ---
        # Sostituisce i salti insensati (errori di convergenza OC) con NaN e interpola
        limit = phase_moles.median() * 5  # Soglia di tolleranza
        phase_moles = phase_moles.where(phase_moles < limit, np.nan).interpolate()
        # ----------------------------------
        
        print(f"Fase {p.upper()}: Valore massimo letto = {phase_moles.max():.2e} moli")
        
        # Plot 1 (Tempo)
        ax1.plot(thermo['Time (h)'], phase_moles, label=p.upper(), linewidth=2)
        
        # Plot 2 (Burnup)
        xp = thermo['Time (h)'].values
        fp = phase_moles.values
        sort_idx = np.argsort(xp)
        moles_interp = np.interp(output['Time (h)'].values, xp[sort_idx], fp[sort_idx])
        
        ax2.plot(output[burnup_col], moles_interp, label=p.upper(), linewidth=2)

# Estetica dei grafici
ax1.set_xlabel('Time (h)')
ax1.set_ylabel('Total Moles in Phase')
ax1.set_title('Noble Metals: Phase Evolution vs Time')
ax1.legend()
ax1.grid(True, linestyle='--', alpha=0.6)

if burnup_col in output.columns:
    ax2.set_xlabel(burnup_col)
ax2.set_ylabel('Total Moles in Phase')
ax2.set_title('Noble Metals: Phase Evolution vs Burnup')
ax2.legend()
ax2.grid(True, linestyle='--', alpha=0.6)

plt.tight_layout()
plt.savefig('noble_metals_phases.png')
plt.show()

# ====================================================================
# wt%
# ====================================================================

print("\n" + "="*65)
print(" CALCOLO COMPOSIZIONE TOTALE METALLI a fine simulazione (wt%)")
print("="*65)

# Masse molari approssimate degli elementi (g/mol)
molar_masses = {'mo': 95.95, 'pd': 106.42, 'ru': 101.07, 'tc': 98.90, 'rh': 102.91}
elements = ['ru', 'rh', 'tc', 'mo', 'pd']
moles_dict = {el: 0.0 for el in elements}

# Cerchiamo i valori massimi validi (ignorando i NaN dell'ultimo step)
for el in elements:
    # Trova tutte le colonne del file che iniziano col nome dell'elemento
    cols = [c for c in thermo.columns if c.split('(')[0].strip().lower() == el]
    for c in cols:
        valid_data = thermo[c].dropna()
        if not valid_data.empty:
            moles_dict[el] += valid_data.max()

# Convertiamo le moli in massa (grammi)
mass_dict = {el: moles_dict[el] * molar_masses[el] for el in elements}
total_mass = sum(mass_dict.values())

if total_mass > 0:
    wt_percent = {el: (mass_dict[el] / total_mass) * 100 for el in elements}
    
    print(f"{'Elemento':<10} | {'Sciantix (wt%)':<20} | {'Parrish 2019 (wt%)*':<20}")
    print("-" * 58)
    print(f"Ru         | {wt_percent['ru']:<20.2f} | 58.8")
    print(f"Rh         | {wt_percent['rh']:<20.2f} | 18.4")
    print(f"Tc         | {wt_percent['tc']:<20.2f} | 14.5")
    print(f"Mo         | {wt_percent['mo']:<20.2f} | 5.1")
    print(f"Pd         | {wt_percent['pd']:<20.2f} | 3.3")
    print("-" * 58)
    print("* Valori sperimentali normalizzati (Parrish et al., 2019).")
else:
    print("Errore: Nessuna massa metallica trovata.")