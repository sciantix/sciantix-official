import matplotlib.pyplot as plt
import matplotlib.lines as mlines

import pandas as pd

# Load data
data = {}
data['No Thermochemistry'] = pd.read_csv('results/output_nochemistry.txt', sep='\t')
data['With Thermochemistry'] = pd.read_csv('results/output_chemistry.txt', sep='\t')

# Compute derived quantities
for label in data:
    data[label]['Cs R/B'] = 100 * data[label]['Cs released (at/m3)'] / data[label]['Cs produced (at/m3)']
    data[label]['I R/B'] = 100 * data[label]['I released (at/m3)'] / data[label]['I produced (at/m3)']
    data[label]['Xe R/B'] = 100 * data[label]['Xe released (at/m3)'] / data[label]['Xe produced (at/m3)']
    data[label]['Kr R/B'] = 100 * data[label]['Kr released (at/m3)'] / data[label]['Kr produced (at/m3)']
    data[label]['Cs reacted'] = 100 * data[label]['Cs reacted (at/m3)'] / data[label]['Cs produced (at/m3)']    
    data[label]['I reacted'] = 100 * data[label]['I reacted (at/m3)'] / data[label]['I produced (at/m3)']
    data[label]['CsI'] = 100 * data[label]['CsI (1/m3)'] / data[label]['Cs produced (at/m3)']      
    data[label]['CsO2'] = 100 * data[label]['CsO2 (1/m3)'] / data[label]['Cs produced (at/m3)'] 
    data[label]['Cs2O'] = 100 * data[label]['Cs2O (1/m3)'] / data[label]['Cs produced (at/m3)']  
    data[label]['Cs2O2'] = 100 * data[label]['Cs2O2 (1/m3)'] / data[label]['Cs produced (at/m3)']   

# Define styles
rb_labels = ['Cs', 'I', 'Xe', 'Kr']
reacted_labels = ['Cs', 'I']
pr_labels = ['CsI', 'Cs2O2', 'CsO2', 'Cs2O']
colors = ['tab:blue', 'tab:orange', 'tab:green', 'tab:red']
linestyles = {'No Thermochemistry': '--', 'With Thermochemistry': '-'}

max_burnup = max(data['No Thermochemistry']['Burnup (MWd/kgUO2)'].max(),
                 data['With Thermochemistry']['Burnup (MWd/kgUO2)'].max())

x_margin = max_burnup * 0.1

# Create side-by-side plots
fig, axes = plt.subplots(1, 3, figsize=(15, 5))

# Plot R/B
for j, label in enumerate(rb_labels):
    for dataset in data:
        axes[0].plot(data[dataset]['Burnup (MWd/kgUO2)'], data[dataset][label + ' R/B'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel('Burnup (MWd/kgUO2)')
axes[0].set_ylabel('Percentage (%)')
axes[0].set_title('Released to Birth')
axes[0].grid(True)

# Plot Reacted
for j, label in enumerate(reacted_labels):
    for dataset in data:
        axes[1].plot(data[dataset]['Burnup (MWd/kgUO2)'], data[dataset][label+ ' reacted'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel('Burnup (MWd/kgUO2)')
axes[1].set_ylabel('Percentage (%)')
axes[1].set_title('Reacted to Birth')
axes[1].grid(True)

# Plot R/B
for j, label in enumerate(pr_labels):
    for dataset in data:
        axes[2].plot(data[dataset]['Burnup (MWd/kgUO2)'], data[dataset][label], linestyle=linestyles[dataset], color=colors[j], label=f'{label} ({dataset})')
axes[2].set_xlabel('Burnup (MWd/kgUO2)')
axes[2].set_ylabel('Percentage (%)')
axes[2].set_title('Produced with respect to Cs Birth')
axes[2].grid(True)

species_legend1 = [mlines.Line2D([], [], color=colors[j], marker='o', label=rb_labels[j]) for j in range(len(rb_labels))]
species_legend2 = [mlines.Line2D([], [], color=colors[j], marker='o', label=reacted_labels[j]) for j in range(len(reacted_labels))]
species_legend3 = [mlines.Line2D([], [], color=colors[j], marker='o', label=pr_labels[j]) for j in range(len(pr_labels))]
thermo_legend = [mlines.Line2D([], [], color='black', linestyle=linestyles[dataset], label=dataset) for dataset in linestyles]

# Add legends to the figure
axes[0].legend(handles=(thermo_legend))
axes[1].legend(handles=(thermo_legend))
axes[2].legend(handles=(thermo_legend))

for j, label in enumerate(rb_labels):
    dataset = 'With Thermochemistry'
    x_values = data[dataset]['Burnup (MWd/kgUO2)']
    y_values = data[dataset][label + ' R/B']

    # Prendi l'ultimo punto della curva
    last_x = x_values.iloc[-1]
    last_y = y_values.iloc[-1]

    # Offset verticale basato sull'indice per evitare sovrapposizioni
    offset = (j - len(rb_labels) / 2) * 15  # Offset verticale

    axes[0].annotate(label, 
                        (last_x, last_y), 
                        textcoords="offset points", 
                        xytext=(offset, 2),  
                        ha='left', 
                        color=colors[j],  
                        fontsize=10)
for j, label in enumerate(reacted_labels):
    dataset = 'With Thermochemistry'
    x_values = data[dataset]['Burnup (MWd/kgUO2)']
    y_values = data[dataset][label + ' reacted']

    # Prendi l'ultimo punto della curva
    last_x = x_values.iloc[-1]
    last_y = y_values.iloc[-1]

    # Offset verticale basato sull'indice per evitare sovrapposizioni
    offset = (j - len(reacted_labels) / 2) * 15  # Offset verticale

    axes[1].annotate(label, 
                        (last_x, last_y), 
                        textcoords="offset points", 
                        xytext=(offset, 2),  
                        ha='left', 
                        color=colors[j],  
                        fontsize=10)
for j, label in enumerate(pr_labels):
    dataset = 'With Thermochemistry'
    x_values = data[dataset]['Burnup (MWd/kgUO2)']
    y_values = data[dataset][label]

    # Prendi l'ultimo punto della curva
    last_x = x_values.iloc[-1]
    last_y = y_values.iloc[-1]

    # Offset verticale basato sull'indice per evitare sovrapposizioni
    offset = (j - len(pr_labels) / 2) * 15  # Offset verticale

    axes[2].annotate(label, 
                        (last_x, last_y), 
                        textcoords="offset points", 
                        xytext=(offset, 2),  
                        ha='left', 
                        color=colors[j],  
                        fontsize=10)
         
for ax in axes:
    ax.set_xlim(0, max_burnup + x_margin)  # Espande l'asse X

plt.tight_layout()
plt.savefig("results/ReleasedtoBirth.png")

# Create Reacted plot
plt.figure(figsize=(7, 7))
plt.plot(data['With Thermochemistry']['Burnup (MWd/kgUO2)'], data['With Thermochemistry']['Stoichiometry deviation (/)'], linestyle=linestyles['With Thermochemistry'], label='With Thermochemistry')

plt.xlabel('Burnup (MWd/kgUO2)')
plt.ylabel('Stoichiometry deviation (/)')
plt.title('')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.savefig("results/Stoichiometry.png")