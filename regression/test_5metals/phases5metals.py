#!/usr/bin/env python3

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

def main():
    print("Extracting data for phase evolution plots...")

    # 1. Caricamento e pulizia dati
    try:
        output = pd.read_csv('output.txt', sep='\t')
        thermo = pd.read_csv('thermochemistry_output.txt', sep='\t', low_memory=False)
    except FileNotFoundError:
        print("Error: output.txt or thermochemistry_output.txt not found.")
        return

    output.columns = output.columns.str.strip()
    thermo.columns = thermo.columns.str.strip()

    for col in thermo.columns:
        thermo[col] = pd.to_numeric(thermo[col], errors='coerce')

    thermo = thermo.dropna(subset=['Time (h)'])
    output = output.dropna(subset=['Time (h)'])

    target_phases = ['bcc_a2', 'hcp_a3', 'sigma', 'fcc_a1']
    phases_found = [p for p in target_phases if any(f"({p}," in col.lower() for col in thermo.columns)]

    print("Phases found:", [p.upper() for p in phases_found])

    # Dizionario per memorizzare i dati puliti di ogni fase
    cleaned_phases_data = {}

    for p in phases_found:
        phase_cols = [c for c in thermo.columns if f"({p}," in c.lower()]
        if phase_cols:
            phase_moles = thermo[phase_cols].sum(axis=1)
            
            # Filtro sulla mediana (rimozione codice a barre)
            limit = phase_moles.median() * 5  
            phase_moles = phase_moles.where(phase_moles < limit, np.nan).interpolate()
            
            cleaned_phases_data[p.upper()] = phase_moles

    # Convertiamo il tempo letto da SCIANTIX (secondi) in ore per l'asse X
    time_hours_thermo = thermo['Time (h)'] / 3600.0
    time_hours_output = output['Time (h)'] / 3600.0
    burnup_col = 'Burnup (MWd/kgMOX)'

    # ====================================================================
    # CALCOLO WT%
    # ====================================================================
    print("\n" + "="*65)
    print(" Metals' composition (wt%) ")
    print("="*65)

    # 1. Definiamo le masse molari effettive dei prodotti di fissione
    molar_masses = {
        'mo': 96.91, 
        'tc': 98.91, 
        'ru': 102.34, 
        'rh': 102.91, 
        'pd': 106.75
    }

    # 2. Inseriamo gli yield di Samuelsson et al. (2020)
    yields = {
        'mo': 0.2187,
        'tc': 0.0531,
        'ru': 0.1984,
        'rh': 0.0594,
        'pd': 0.1547
    }

    # 3. yield * isotopic_molar_mass
    mass_dict = {el: yields[el] * molar_masses[el] for el in yields}
    total_mass = sum(mass_dict.values())

    if total_mass > 0:
        wt_percent = {el: (mass_dict[el] / total_mass) * 100 for el in yields}
        
        print(f"{'Element':<10} | {'Sciantix (wt%)':<20} | {'Parrish 2019 (wt%)*':<20}")
        print("-" * 58)
        print(f"Ru         | {wt_percent['ru']:<20.2f} | 58.8")
        print(f"Rh         | {wt_percent['rh']:<20.2f} | 18.4")
        print(f"Tc         | {wt_percent['tc']:<20.2f} | 14.5")
        print(f"Mo         | {wt_percent['mo']:<20.2f} | 5.1")
        print(f"Pd         | {wt_percent['pd']:<20.2f} | 3.3")
        print("-" * 58)
        print("* (Parrish et al., 2019).")
    else:
        print("Error in weight percentage calculation.")

if __name__ == "__main__":
    main()