#!/usr/bin/env python3

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import re

# ====================================================================
# Configurazione
# ====================================================================
METALS = ['MO', 'RU', 'TC', 'PD', 'RH']
METALLIC_PHASES = ['bcc_a2', 'hcp_a3', 'fcc_a1', 'sigma', 'liquid']
COLORS = {'RU': '#1f77b4', 'MO': '#ff7f0e', 'PD': '#2ca02c', 'TC': '#d62728', 'RH': '#9467bd'}

# IL NOME ESATTO DELLA TUA COLONNA
OXYGEN_COL = 'Fuel oxygen potential (KJ/mol)' 

def main():
    print("Generazione del grafico di Sensibilità al Potenziale di Ossigeno...")

    try:
        output = pd.read_csv('output.txt', sep='\t')
        thermo = pd.read_csv('thermochemistry_output.txt', sep='\t', low_memory=False)
    except FileNotFoundError:
        print("Errore: output.txt o thermochemistry_output.txt non trovati.")
        return

    output.columns = output.columns.str.strip()
    thermo.columns = thermo.columns.str.strip()

    if OXYGEN_COL not in output.columns:
        print(f"Errore: Colonna '{OXYGEN_COL}' non trovata in output.txt.")
        print("Colonne disponibili:", output.columns.tolist())
        return

    for col in thermo.columns:
        thermo[col] = pd.to_numeric(thermo[col], errors='coerce')

    thermo = thermo.dropna(subset=['Time (h)']).sort_values('Time (h)')
    output = output.dropna(subset=['Time (h)']).sort_values('Time (h)')

    # Interpolo il potenziale di ossigeno
    xp_out = output['Time (h)'].values
    x_oxygen = np.interp(thermo['Time (h)'].values, xp_out, output[OXYGEN_COL].values)

    # Estrazione delle moli
    alloy_moles = pd.DataFrame(index=thermo.index)
    for metal in METALS:
        metal_cols = []
        for phase in METALLIC_PHASES:
            pattern = re.compile(rf"^\s*{metal}\s*\({phase},.*?\)\s*\(mol/m3\)\s*$", re.IGNORECASE)
            metal_cols.extend([col for col in thermo.columns if pattern.match(col)])
        
        alloy_moles[metal] = thermo[metal_cols].sum(axis=1) if metal_cols else 0.0

    # Calcolo Frazioni e Maschera validità
    total_alloy_moles = alloy_moles.sum(axis=1)
    valid_mask = total_alloy_moles > 1e-10

    if not valid_mask.any():
        print("Attenzione: Nessuna fase metallica trovata.")
        return

    x_oxygen_clean = x_oxygen[valid_mask.values]
    fractions = alloy_moles[valid_mask].div(total_alloy_moles[valid_mask], axis=0) * 100.0
    
    y_data = [fractions[m] for m in METALS]

    # Ordinamento per asse X
    sort_idx = np.argsort(x_oxygen_clean)
    x_ox_sorted = x_oxygen_clean[sort_idx]
    y_data_sorted = [y.iloc[sort_idx] for y in y_data]

    # Creazione Figura
    fig, ax = plt.subplots(figsize=(10, 6))

    ax.stackplot(x_ox_sorted, y_data_sorted, labels=METALS, colors=[COLORS[m] for m in METALS], alpha=0.8)
    
    min_ox = x_ox_sorted.min()
    max_ox = x_ox_sorted.max()

    if np.isclose(min_ox, max_ox):
        buffer = abs(min_ox) * 0.05 if min_ox != 0 else 1.0
        ax.set_xlim(min_ox - buffer, max_ox + buffer)
        ax.set_xlabel(f"{OXYGEN_COL} (Costante)", fontsize=12)
        print("ATTENZIONE: Il potenziale di ossigeno è costante in questa simulazione! Il grafico sarà un blocco rettangolare. Lancialo dalla cartella di irraggiamento.")
    else:
        if x_ox_sorted.mean() < 0:
            ax.set_xlim(max_ox, min_ox)  # Asse invertito per assecondare la logica chimica (crescente verso dx)
            ax.set_xlabel('Oxygen Potential ($\Delta \bar{G}_{O_2}$) [kJ/mol]', fontsize=12)
        else:
            ax.set_xlim(min_ox, max_ox)
            ax.set_xlabel(OXYGEN_COL, fontsize=12)

    ax.set_ylabel('Composizione Relativa Lega Metallica (mol %)', fontsize=12)
    ax.set_title('Sensibilità della Fase $\epsilon$ al Potenziale di Ossigeno', fontsize=14)
    ax.set_ylim(0, 100)

    ax.grid(True, linestyle='--', alpha=0.4)
    ax.margins(0, 0)

    handles, labels = ax.get_legend_handles_labels()
    ax.legend(handles, labels, loc='upper center', ncol=5, fontsize=11, bbox_to_anchor=(0.5, 1.1))

    plt.tight_layout()
    plt.savefig('oxygen_sensitivity_5metals.png', dpi=300, bbox_inches='tight')
    print("Grafico salvato con successo: 'oxygen_sensitivity_5metals.png'")
    plt.show()

if __name__ == "__main__":
    main()