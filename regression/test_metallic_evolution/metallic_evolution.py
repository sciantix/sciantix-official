#!/usr/bin/env python3

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import re

# ====================================================================
# Configuration
# ====================================================================
METALS = ['MO', 'RU', 'TC', 'PD', 'RH']
METALLIC_PHASES = ['bcc_a2', 'hcp_a3', 'fcc_a1', 'sigma', 'liquid']

# Distinctive colors for the thesis
COLORS = {'RU': '#1f77b4', 'MO': '#ff7f0e', 'PD': '#2ca02c', 'TC': '#d62728', 'RH': '#9467bd'}

def main():
    print("Extracting data for individual Stacked Area Plots...")

    # 1. Data Loading
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

    thermo = thermo.dropna(subset=['Time (h)']).sort_values('Time (h)')
    output = output.dropna(subset=['Time (h)']).sort_values('Time (h)')

    xp_out = output['Time (h)'].values
    x_burnup = np.interp(thermo['Time (h)'].values, xp_out, output['Burnup (MWd/kgMOX)'].values)
    x_temp = np.interp(thermo['Time (h)'].values, xp_out, output['Temperature (K)'].values)

    # 2. Extract moles ONLY in metallic phases
    alloy_moles = pd.DataFrame(index=thermo.index)
    
    for metal in METALS:
        metal_cols = []
        for phase in METALLIC_PHASES:
            pattern = re.compile(rf"^\s*{metal}\s*\({phase},.*?\)\s*\(mol/m3\)\s*$", re.IGNORECASE)
            metal_cols.extend([col for col in thermo.columns if pattern.match(col)])
        
        if metal_cols:
            alloy_moles[metal] = thermo[metal_cols].sum(axis=1)
        else:
            alloy_moles[metal] = 0.0

    # 3. Data Cleaning
    total_alloy_moles = alloy_moles.sum(axis=1)
    valid_mask = total_alloy_moles > 1e-10

    if not valid_mask.any():
        print("Warning: No metallic phase found with values > 0.")
        return

    x_burnup_clean = x_burnup[valid_mask.values]
    x_temp_clean = x_temp[valid_mask.values]

    fractions = alloy_moles[valid_mask].div(total_alloy_moles[valid_mask], axis=0) * 100.0
    
    y_data = [fractions[m] for m in METALS]
    labels = METALS
    colors = [COLORS[m] for m in METALS]

    # ====================================================================
    # FILE 1: COMPOSTION VS BURNUP
    # ====================================================================
    fig1, ax1 = plt.subplots(figsize=(9, 5))
    ax1.stackplot(x_burnup_clean, y_data, labels=labels, colors=colors, alpha=0.8)
    ax1.set_xlabel('Burnup (MWd/kgMOX)', fontsize=12)
    ax1.set_ylabel('Metallic Alloy Relative Composition (mol %)', fontsize=12)
    ax1.set_title('Metallic Alloy Evolution vs Burnup', fontsize=13, fontweight='bold', pad=15)
    ax1.set_ylim(0, 100)
    
    if np.isclose(x_burnup_clean.min(), x_burnup_clean.max()):
        ax1.set_xlim(x_burnup_clean.min() - 1, x_burnup_clean.max() + 1)
    else:
        ax1.set_xlim(x_burnup_clean.min(), x_burnup_clean.max())
        
    ax1.grid(True, linestyle='--', alpha=0.3)
    ax1.margins(0, 0)
    ax1.legend(loc='upper center', bbox_to_anchor=(0.5, -0.15), ncol=5, fontsize=11)
    
    plt.tight_layout()
    plt.savefig('metallic_evolution_vs_burnup.png', dpi=300, bbox_inches='tight')
    print("Saved: 'metallic_evolution_vs_burnup.png'")
    plt.close()

    # ====================================================================
    # FILE 2: COMPOSITION VS TEMPERATURE
    # ====================================================================
    fig2, ax2 = plt.subplots(figsize=(9, 5))
    sort_idx = np.argsort(x_temp_clean)
    x_temp_sorted = x_temp_clean[sort_idx]
    y_data_sorted = [y.iloc[sort_idx] for y in y_data]

    ax2.stackplot(x_temp_sorted, y_data_sorted, labels=labels, colors=colors, alpha=0.8)
    ax2.set_xlabel('Temperature (K)', fontsize=12)
    ax2.set_ylabel('Metallic Alloy Relative Composition (mol %)', fontsize=12)
    ax2.set_title('Metallic Alloy Evolution vs Temperature', fontsize=13, fontweight='bold', pad=15)
    ax2.set_ylim(0, 100)
    
    if np.isclose(x_temp_sorted.min(), x_temp_sorted.max()):
        buffer = 50.0 if x_temp_sorted.min() > 0 else 1.0
        ax2.set_xlim(x_temp_sorted.min() - buffer, x_temp_sorted.max() + buffer)
    else:
        ax2.set_xlim(x_temp_sorted.min(), x_temp_sorted.max())
        
    ax2.grid(True, linestyle='--', alpha=0.3)
    ax2.margins(0, 0)
    ax2.legend(loc='upper center', bbox_to_anchor=(0.5, -0.15), ncol=5, fontsize=11)
    
    plt.tight_layout()
    plt.savefig('metallic_evolution_vs_temperature.png', dpi=300, bbox_inches='tight')
    print("Saved: 'metallic_evolution_vs_temperature.png'")
    plt.close()

if __name__ == "__main__":
    main()