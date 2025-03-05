import matplotlib.pyplot as plt
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
    data[label]['I reacted'] = 100 * data[label]['I reacted (at/m3)'] / data[label]['Cs produced (at/m3)']

# Define colors and labels
rb_labels = ['Cs R/B', 'I R/B', 'Xe R/B', 'Kr R/B']
reacted_labels = ['Cs reacted', 'I reacted']
colors = ['tab:blue', 'tab:orange', 'tab:green', 'tab:red']
linestyles = {'No Thermochemistry': '-', 'With Thermochemistry': '--'}

# Create side-by-side plots
fig, axes = plt.subplots(1, 2, figsize=(14, 7))

# Plot R/B
for j, label in enumerate(rb_labels):
    for dataset in data:
        axes[0].plot(data[dataset]['Burnup (MWd/kgUO2)'], data[dataset][label], linestyle=linestyles[dataset], color=colors[j], label=f'{label} ({dataset})')
axes[0].set_xlabel('Burnup (MWd/kgUO2)')
axes[0].set_ylabel('Percentage (%)')
axes[0].set_title('Comparison of Release Percentages')
axes[0].legend()
axes[0].grid(True)

# Plot Reacted
for j, label in enumerate(reacted_labels):
    for dataset in data:
        axes[1].plot(data[dataset]['Burnup (MWd/kgUO2)'], data[dataset][label], linestyle=linestyles[dataset], color=colors[j+2], label=f'{label} ({dataset})')
axes[1].set_xlabel('Burnup (MWd/kgUO2)')
axes[1].set_ylabel('Percentage (%)')
axes[1].set_title('Comparison of Reacted Percentages')
axes[1].legend()
axes[1].grid(True)

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

