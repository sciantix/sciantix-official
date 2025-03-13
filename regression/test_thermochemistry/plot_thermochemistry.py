import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import os
import glob

folder_path = "results"
avogadronumber = 6.02214e23

png_files = glob.glob(os.path.join(folder_path, "*.png"))
for file in png_files:
    os.remove(file)

# Load data
data = {}
data['No Thermochemistry'] = pd.read_csv(folder_path + '/output_nochemistry.txt', sep='\t')
data['With Thermochemistry'] = pd.read_csv(folder_path + '/output_chemistry.txt', sep='\t')

# Define styles
inert_gases = ['Xe', 'Kr']
volatile_fps = ['Cs', 'I']
gas_ideal = ['I - gas_ideal (mol)', 'I2 - gas_ideal (mol)', 'Cs - gas_ideal (mol)', 'Cs2 - gas_ideal (mol)', 'CsI - gas_ideal (mol)', 'Cs2I2 - gas_ideal (mol)']
liquid = ['I2 - LIQUID (mol)', 'Cs - LIQUID (mol)', 'CsI - LIQUID (mol)']
pure_condensed = ['I2_s(s) - pure condensed phases (mol)','Cs_bcc_a2(s) - pure condensed phases (mol)','CsI_csi_b2(s) - pure condensed phases (mol)','CsI3_csi3(s) - pure condensed phases (mol)','CsI4_csi4(s) - pure condensed phases (mol)']

all_gases =  volatile_fps + inert_gases
all_products = gas_ideal + liquid + pure_condensed

colors = ['dodgerblue', 'darkorange', 'forestgreen', 'crimson', 'hotpink', 'gold', 'slategray',
          'mediumpurple', 'saddlebrown', 'deepskyblue', 'darkolivegreen', 'limegreen', 'darkcyan', 'orangered']

linestyles = {'No Thermochemistry': '--', 'With Thermochemistry': '-'}

# Compute derived quantities
for dataset in data:
    for label in all_gases:
        data[dataset][label + ' released/birth'] = data[dataset][label + ' released (at/m3)']/data[dataset][label + ' produced (at/m3)']
    for label in volatile_fps:
        data[dataset][label + ' available/birth'] = (data[dataset][label + ' reacted (at/m3)']+ data[dataset][label + ' at grain boundary (at/m3)'])/data[dataset][label + ' produced (at/m3)']
        data[dataset][label + ' reacted/birth'] = data[dataset][label + ' reacted (at/m3)']/data[dataset][label + ' produced (at/m3)']
        data[dataset][label + ' reacted/available'] = data[dataset][label + ' reacted/birth']/data[dataset][label + ' available/birth']

if 'At grain boundary (mol)' not in data['With Thermochemistry'].columns:
    data['With Thermochemistry']['At grain boundary (mol)'] = 0  
for label in volatile_fps:
    data['With Thermochemistry']['At grain boundary (mol)'] += (1/avogadronumber)*(data['With Thermochemistry'][label+' at grain boundary (at/m3)'] + data['With Thermochemistry'][label+' reacted (at/m3)'])/(data['With Thermochemistry']['Intergranular bubble concentration (bub/m2)'] * 3/ data['With Thermochemistry']['Grain radius (m)'])

for label in all_products:
    data['With Thermochemistry'][label + ' produced/available'] = data['With Thermochemistry'][label]/data['With Thermochemistry']['At grain boundary (mol)']

#################################### VS BU #################################
label_x = 'Burnup (MWd/kgUO2)'

fig, axes = plt.subplots(1, 3, figsize=(20, 8))

for j, label in enumerate(all_gases):
    for dataset in data:
        axes[0].plot(data[dataset][label_x], data[dataset][label + ' released/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Released atoms to birth (/)')
axes[0].set_title('Release to birth')
axes[0].grid(True)

ax_temp_0 = axes[0].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[1].plot(data[dataset][label_x], data[dataset][label + ' available/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Available atoms to birth (/)')
axes[1].set_title('Available to birth')
axes[1].grid(True)

ax_temp_0 = axes[1].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[2].plot(data[dataset][label_x], data[dataset][label + ' reacted/available'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[2].set_xlabel(label_x)
axes[2].set_ylabel('Reacted atoms to available (/)')
axes[2].set_title('Reacted to available')
axes[2].grid(True)

ax_temp_0 = axes[2].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

species_legend1 = [mlines.Line2D([], [], color=colors[j], marker='o', label=all_gases[j]) for j in range(len(all_gases))]
species_legend2 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
species_legend3 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
thermo_legend = [mlines.Line2D([], [], color='black', linestyle=linestyles[dataset], label=dataset) for dataset in linestyles]

# Add legends to the figure
axes[0].legend(handles=(thermo_legend + species_legend1))
axes[1].legend(handles=(thermo_legend + species_legend2))
axes[2].legend(handles=(thermo_legend + species_legend3))

plt.tight_layout()
plt.savefig(folder_path +"/vsBu.png")

#################################### VS TIME #################################
label_x = 'Time (h)'

fig, axes = plt.subplots(1, 3, figsize=(20, 8))

for j, label in enumerate(all_gases):
    for dataset in data:
        axes[0].plot(data[dataset][label_x], data[dataset][label + ' released/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Released atoms to birth (/)')
axes[0].set_title('Release to birth')
axes[0].grid(True)

ax_temp_0 = axes[0].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[1].plot(data[dataset][label_x], data[dataset][label + ' available/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Available atoms to birth (/)')
axes[1].set_title('Available to birth')
axes[1].grid(True)

ax_temp_0 = axes[1].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[2].plot(data[dataset][label_x], data[dataset][label + ' reacted/available'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[2].set_xlabel(label_x)
axes[2].set_ylabel('Reacted atoms to available (/)')
axes[2].set_title('Reacted to available')
axes[2].grid(True)

ax_temp_0 = axes[2].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

species_legend1 = [mlines.Line2D([], [], color=colors[j], marker='o', label=all_gases[j]) for j in range(len(all_gases))]
species_legend2 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
species_legend3 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
thermo_legend = [mlines.Line2D([], [], color='black', linestyle=linestyles[dataset], label=dataset) for dataset in linestyles]

# Add legends to the figure
axes[0].legend(handles=(thermo_legend + species_legend1))
axes[1].legend(handles=(thermo_legend + species_legend2))
axes[2].legend(handles=(thermo_legend + species_legend3))

plt.tight_layout()
plt.savefig(folder_path +"/vsTime.png")

#################################### STOICHIOMETRY PLOT VS BU #################################
label_x = 'Burnup (MWd/kgUO2)'
label_y = 'Stoichiometry deviation (/)'
for dataset in data: 
    if label_y in data[dataset].columns:
        plt.figure(figsize=(7, 7))
        plt.plot(data[dataset][label_x], data[dataset][label_y], linestyle=linestyles[dataset], label=dataset)
        plt.xlabel(label_x)
        plt.ylabel(label_y)
        plt.legend()
        plt.grid(True)
        plt.tight_layout()
        plt.savefig(folder_path + "/stoichiometry.png")

#################################### VS TIME #################################
label_x = 'Time (h)'
xlim_o = min(data['With Thermochemistry'][label_x])
xlim_i = 40100
xlim_f = max(data['With Thermochemistry'][label_x])

fig, axes = plt.subplots(1, 3, figsize=(20, 8))

for j, label in enumerate(all_gases):
    for dataset in data:
        axes[0].plot(data[dataset][label_x], data[dataset][label + ' released/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Released atoms to birth (/)')
axes[0].set_title('Release to birth')
axes[0].grid(True)
axes[0].set_xlim([xlim_o,xlim_i])

ax_temp_0 = axes[0].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[1].plot(data[dataset][label_x], data[dataset][label + ' available/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Available atoms to birth (/)')
axes[1].set_title('Available to birth')
axes[1].grid(True)
axes[1].set_xlim([xlim_o,xlim_i])

ax_temp_0 = axes[1].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[2].plot(data[dataset][label_x], data[dataset][label + ' reacted/available'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[2].set_xlabel(label_x)
axes[2].set_ylabel('Reacted atoms to available (/)')
axes[2].set_title('Reacted to available')
axes[2].grid(True)
axes[2].set_xlim([xlim_o,xlim_i])

ax_temp_0 = axes[2].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

species_legend1 = [mlines.Line2D([], [], color=colors[j], marker='o', label=all_gases[j]) for j in range(len(all_gases))]
species_legend2 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
species_legend3 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
thermo_legend = [mlines.Line2D([], [], color='black', linestyle=linestyles[dataset], label=dataset) for dataset in linestyles]

# Add legends to the figure
axes[0].legend(handles=(thermo_legend + species_legend1))
axes[1].legend(handles=(thermo_legend + species_legend2))
axes[2].legend(handles=(thermo_legend + species_legend3))

plt.tight_layout()
plt.savefig(folder_path +"/vsTime1.png")

fig, axes = plt.subplots(1, 3, figsize=(20, 8))

for j, label in enumerate(all_gases):
    for dataset in data:
        axes[0].plot(data[dataset][label_x], data[dataset][label + ' released/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Released atoms to birth (/)')
axes[0].set_title('Release to birth')
axes[0].grid(True)
axes[0].set_xlim([xlim_i,xlim_f])

ax_temp_0 = axes[0].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[1].plot(data[dataset][label_x], data[dataset][label + ' available/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Available atoms to birth (/)')
axes[1].set_title('Available to birth')
axes[1].grid(True)
axes[1].set_xlim([xlim_i,xlim_f])

ax_temp_0 = axes[1].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[2].plot(data[dataset][label_x], data[dataset][label + ' reacted/available'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[2].set_xlabel(label_x)
axes[2].set_ylabel('Reacted atoms to available (/)')
axes[2].set_title('Reacted to available')
axes[2].grid(True)
axes[2].set_xlim([xlim_i,xlim_f])

ax_temp_0 = axes[2].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

species_legend1 = [mlines.Line2D([], [], color=colors[j], marker='o', label=all_gases[j]) for j in range(len(all_gases))]
species_legend2 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
species_legend3 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
thermo_legend = [mlines.Line2D([], [], color='black', linestyle=linestyles[dataset], label=dataset) for dataset in linestyles]

# Add legends to the figure
axes[0].legend(handles=(thermo_legend + species_legend1))
axes[1].legend(handles=(thermo_legend + species_legend2))
axes[2].legend(handles=(thermo_legend + species_legend3))

plt.tight_layout()
plt.savefig(folder_path +"/vsTime2.png")

############################################### PRODUCED ###########################

fig, axes = plt.subplots(1, 2, figsize=(20, 8))

for j, label in enumerate(all_products):
    dataset = "With Thermochemistry"
    if data[dataset][label + ' produced/available'].max() > 0.001:
        axes[0].plot(data[dataset][label_x], data[dataset][label + ' produced/available'], 
                        linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Produced to available (/)')
axes[0].set_title('Produced to available')
axes[0].grid(True)
axes[0].set_xlim([xlim_o,xlim_i])
axes[0].set_ylim([0,1])
axes[0].legend()

ax_temp_0 = axes[0].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

for j, label in enumerate(all_products):
    dataset = "With Thermochemistry"
    if data[dataset][label + ' produced/available'].max() > 0.001:
        axes[1].plot(data[dataset][label_x], data[dataset][label + ' produced/available'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Produced to available (/)')
axes[1].set_title('Produced to available')
axes[1].grid(True)
axes[1].set_xlim([xlim_i,xlim_f])
axes[1].set_ylim([0,1])
axes[1].legend()

ax_temp_0 = axes[1].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

plt.tight_layout()
plt.savefig(folder_path +"/Products.png")

# N = 3
# for j, label in enumerate(all_gases):
#     dataset = 'With Thermochemistry'
#     x_values = data[dataset][label_x]
#     y_values = data[dataset][label + ' released/birth']
#     last_x = x_values.iloc[-1-N*j]
#     last_y = y_values.iloc[-1-N*j]
#     axes[0].annotate(label, 
#                         (last_x, last_y), 
#                         textcoords="offset points", 
#                         xytext=(0, 0),  
#                         ha='right',
#                         color=colors[j],  
#                         fontsize=10)
# for j, label in enumerate(volatile_fps):
#     dataset = 'With Thermochemistry'
#     x_values = data[dataset][label_x]
#     y_values = data[dataset][label + ' available/birth']
#     last_x = x_values.iloc[-1-N*j]
#     last_y = y_values.iloc[-1-N*j]
#     axes[1].annotate(label, 
#                         (last_x, last_y), 
#                         textcoords="offset points", 
#                         xytext=(0, 0),  
#                         ha='right',
#                         color=colors[j],  
#                         fontsize=10)
# for j, label in enumerate(volatile_fps):
#     dataset = 'With Thermochemistry'
#     x_values = data[dataset][label_x]
#     y_values = data[dataset][label + ' reacted/available']
#     last_x = x_values.iloc[-1-N*j]
#     last_y = y_values.iloc[-1-N*j]
#     axes[2].annotate(label, 
#                         (last_x, last_y), 
#                         textcoords="offset points", 
#                         xytext=(0, 0),  
#                         ha='right',
#                         color=colors[j],  
#                         fontsize=10)