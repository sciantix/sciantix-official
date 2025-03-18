import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import numpy as np
import os
import glob

folder_path = "results"
avogadronumber = 6.02214e23
xlim_i = 54000

# Remove previous plots
png_files = glob.glob(os.path.join(folder_path, "*.png"))
for file in png_files:
    os.remove(file)

# Load data
data = {}
data['No Thermochemistry'] = pd.read_csv(folder_path + '/output_nochemistry.txt', sep='\t')
data['With Thermochemistry'] = pd.read_csv(folder_path + '/output_chemistry.txt', sep='\t')

# Print some main variables at the end of irradiation
print('--------------------------------------------------------------------')
print('Simulation results')
print('Burnup (MWd/kgUO2) = ', data['With Thermochemistry']['Burnup (MWd/kgUO2)'].max()) 
print('Grain diameter (um) = ', 2e6*data['With Thermochemistry']['Grain radius (m)'].max()) 
print('Stoichiometry deviation (/) = ', data['With Thermochemistry']['Stoichiometry deviation (/)'].max()) 

# Define classes
inert_gases = ['Xe', 'Kr']
volatile_fps = ['Cs', 'I']
gas_ideal = ['I - gas_ideal - GB (mol)', 'I2 - gas_ideal - GB (mol)', 'Cs - gas_ideal - GB (mol)', 'Cs2 - gas_ideal - GB (mol)', 'CsI - gas_ideal - GB (mol)', 'Cs2I2 - gas_ideal - GB (mol)']
liquid = ['I2 - LIQUID - GB (mol)', 'Cs - LIQUID - GB (mol)', 'CsI - LIQUID - GB (mol)']
pure_condensed = ['I2_s(s) - pure condensed phases - GB (mol)','Cs_bcc_a2(s) - pure condensed phases - GB (mol)','CsI_csi_b2(s) - pure condensed phases - GB (mol)','CsI3_csi3(s) - pure condensed phases - GB (mol)','CsI4_csi4(s) - pure condensed phases - GB (mol)']

all_gases =  volatile_fps + inert_gases
all_products = gas_ideal + liquid + pure_condensed

# Define style
colors = ['dodgerblue', 'darkorange', 'forestgreen', 'crimson', 'hotpink', 'gold', 'slategray',
          'mediumpurple', 'saddlebrown', 'deepskyblue', 'darkolivegreen', 'limegreen', 'darkcyan', 'orangered']
linestyles = {'No Thermochemistry': '--', 'With Thermochemistry': '-'}

N = 600

# Compute derived quantities
for dataset in data:
    for label in all_gases:
        data[dataset][label + ' released/birth'] = data[dataset][label + ' released (at/m3)']/data[dataset][label + ' produced (at/m3)']

    #for label in inert_gases:
        #print(label, dataset, data[dataset].loc[N, label + ' produced (at/m3)'], data[dataset].loc[N, label + ' released (at/m3)'], data[dataset].loc[N, label + ' at grain boundary (at/m3)'], data[dataset].loc[N, label + ' in grain (at/m3)'])
    
    for label in volatile_fps:
        data[dataset][label + ' available/birth'] = (data[dataset][label + ' reacted - GB (at/m3)']+ data[dataset][label + ' at grain boundary (at/m3)'])/data[dataset][label + ' produced (at/m3)']
        data[dataset][label + ' reacted/birth'] = data[dataset][label + ' reacted - GB (at/m3)']/data[dataset][label + ' produced (at/m3)']
        data[dataset][label + ' reacted/available'] = data[dataset][label + ' reacted/birth']/data[dataset][label + ' available/birth']
        print(label, dataset, data[dataset].loc[N, label + ' produced (at/m3)'], 
              data[dataset].loc[N, label + ' released (at/m3)'], 
              data[dataset].loc[N, label + ' at grain boundary (at/m3)'], data[dataset].loc[N, label + ' reacted - GB (at/m3)'],
              data[dataset].loc[N, label + ' in grain (at/m3)'], data[dataset].loc[N, label + ' reacted - IG (at/m3)'])
        print(label, dataset, data[dataset].loc[N, label + ' produced (at/m3)'] -
              data[dataset].loc[N, label + ' released (at/m3)'] -
              data[dataset].loc[N, label + ' at grain boundary (at/m3)'] - data[dataset].loc[N, label + ' reacted - GB (at/m3)'] - 
              data[dataset].loc[N, label + ' in grain (at/m3)'] -  data[dataset].loc[N, label + ' reacted - IG (at/m3)'])
    

if 'At grain boundary (mol)' not in data['With Thermochemistry'].columns:
    data['With Thermochemistry']['At grain boundary (mol)'] = 0  
for label in volatile_fps:
    data['With Thermochemistry']['At grain boundary (mol)'] += (1/avogadronumber)*(data['With Thermochemistry'][label+' at grain boundary (at/m3)'] + data['With Thermochemistry'][label+' reacted - GB (at/m3)'])/(data['With Thermochemistry']['Intergranular bubble concentration (bub/m2)'] * 3/ data['With Thermochemistry']['Grain radius (m)'])

for label in all_products:
    data['With Thermochemistry'][label + ' produced/available'] = data['With Thermochemistry'][label]/data['With Thermochemistry']['At grain boundary (mol)']

####################################### Comparison with experimetal data at 1900 K ########################################
mask_RAMP = data['With Thermochemistry']['Time (h)'] >= xlim_i

#################################### quantitative #################################
label_x = 'Time (h)'
xlim_o = min(data['With Thermochemistry'][label_x])
xlim_f = max(data['With Thermochemistry'][label_x])

fig, axes = plt.subplots(1, 2, figsize=(18, 8))

label = 'Cs'
dataset = "With Thermochemistry"
axes[0].plot(data[dataset][label_x], data[dataset][label + ' produced (at/m3)'], 
        linestyle=linestyles[dataset], label='Produced')
axes[0].plot(data[dataset][label_x], data[dataset][label + ' in grain (at/m3)'], 
        linestyle=linestyles[dataset], label='In grain')
axes[0].plot(data[dataset][label_x], data[dataset][label + ' reacted - IG (at/m3)'], 
        linestyle=linestyles[dataset], label='Reacted - IG')
axes[0].plot(data[dataset][label_x], data[dataset][label + ' at grain boundary (at/m3)'], 
        linestyle=linestyles[dataset], label='At grain boundary')
axes[0].plot(data[dataset][label_x], data[dataset][label + ' reacted - GB (at/m3)'], 
        linestyle=linestyles[dataset], label='Reacted - GB')
axes[0].plot(data[dataset][label_x], data[dataset][label + ' released (at/m3)'], 
        linestyle=linestyles[dataset], label='Released')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Atoms (at/m3)')
axes[0].set_xlim([xlim_o,xlim_i])

axes[1].plot(data[dataset][label_x], data[dataset][label + ' produced (at/m3)'], 
        linestyle=linestyles[dataset], label='Produced')
axes[1].plot(data[dataset][label_x], data[dataset][label + ' in grain (at/m3)'], 
        linestyle=linestyles[dataset],  label='In grain')
axes[1].plot(data[dataset][label_x], data[dataset][label + ' reacted - IG (at/m3)'], 
        linestyle=linestyles[dataset], label='Reacted - IG')
axes[1].plot(data[dataset][label_x], data[dataset][label + ' at grain boundary (at/m3)'], 
        linestyle=linestyles[dataset], label='At grain boundary')
axes[1].plot(data[dataset][label_x], data[dataset][label + ' reacted - GB (at/m3)'], 
        linestyle=linestyles[dataset], label='Reacted - GB')
axes[1].plot(data[dataset][label_x], data[dataset][label + ' released (at/m3)'], 
        linestyle=linestyles[dataset], label='Released')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Atoms (at/m3)')
axes[1].set_xlim([xlim_i,xlim_f])

plt.legend()

fig, axes = plt.subplots(1, 2, figsize=(18, 8))

label = 'Cs'
dataset = "No Thermochemistry"
axes[0].plot(data[dataset][label_x], data[dataset][label + ' produced (at/m3)'], 
        linestyle=linestyles[dataset], label='Produced')
axes[0].plot(data[dataset][label_x], data[dataset][label + ' in grain (at/m3)'], 
        linestyle=linestyles[dataset], label='In grain')
axes[0].plot(data[dataset][label_x], data[dataset][label + ' reacted - IG (at/m3)'], 
        linestyle=linestyles[dataset], label='Reacted - IG')
axes[0].plot(data[dataset][label_x], data[dataset][label + ' at grain boundary (at/m3)'], 
        linestyle=linestyles[dataset], label='At grain boundary')
axes[0].plot(data[dataset][label_x], data[dataset][label + ' reacted - GB (at/m3)'], 
        linestyle=linestyles[dataset], label='Reacted - GB')
axes[0].plot(data[dataset][label_x], data[dataset][label + ' released (at/m3)'], 
        linestyle=linestyles[dataset], label='Released')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Atoms (at/m3)')
axes[0].set_xlim([xlim_o,xlim_i])

axes[1].plot(data[dataset][label_x], data[dataset][label + ' produced (at/m3)'], 
        linestyle=linestyles[dataset], label='Produced')
axes[1].plot(data[dataset][label_x], data[dataset][label + ' in grain (at/m3)'], 
        linestyle=linestyles[dataset],  label='In grain')
axes[1].plot(data[dataset][label_x], data[dataset][label + ' reacted - IG (at/m3)'], 
        linestyle=linestyles[dataset], label='Reacted - IG')
axes[1].plot(data[dataset][label_x], data[dataset][label + ' at grain boundary (at/m3)'], 
        linestyle=linestyles[dataset], label='At grain boundary')
axes[1].plot(data[dataset][label_x], data[dataset][label + ' reacted - GB (at/m3)'], 
        linestyle=linestyles[dataset], label='Reacted - GB')
axes[1].plot(data[dataset][label_x], data[dataset][label + ' released (at/m3)'], 
        linestyle=linestyles[dataset], label='Released')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Atoms (at/m3)')
axes[1].set_xlim([xlim_i,xlim_f])

plt.legend()
plt.show()

#################################### VS TIME #################################
label_x = 'Time (h)'
xlim_o = min(data['With Thermochemistry'][label_x])
xlim_f = max(data['With Thermochemistry'][label_x])

fig, axes = plt.subplots(1, 3, figsize=(15, 4))

for j, label in enumerate(all_gases):
    for dataset in data:
        axes[0].plot(data[dataset][label_x], data[dataset][label + ' released/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Released atoms to birth (/)')
axes[0].set_title('Release to birth')
axes[0].set_xlim([xlim_o,xlim_i])
axes[0].set_ylim([0,1])

ax_temp_0 = axes[0].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

x_middle = (xlim_o + xlim_i) / 2
closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
y_middle = data[dataset]['Temperature (K)'][closest_idx]


ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
                    fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[1].plot(data[dataset][label_x], data[dataset][label + ' available/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Available atoms to birth (/)')
axes[1].set_title('Available to birth')
axes[1].set_xlim([xlim_o,xlim_i])
axes[1].set_ylim([0,1])

ax_temp_0 = axes[1].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

x_middle = (xlim_o + xlim_i) / 2
closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
y_middle = data[dataset]['Temperature (K)'][closest_idx]


ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
                    fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[2].plot(data[dataset][label_x], data[dataset][label + ' reacted/available'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[2].set_xlabel(label_x)
axes[2].set_ylabel('Reacted atoms to available (/)')
axes[2].set_title('Reacted to available')
axes[2].set_xlim([xlim_o,xlim_i])
axes[2].set_ylim([0,1])

ax_temp_0 = axes[2].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

x_middle = (xlim_o + xlim_i) / 2
closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
y_middle = data[dataset]['Temperature (K)'][closest_idx]


ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
                    fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

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
#plt.show()

fig, axes = plt.subplots(1, 3, figsize=(15, 4))

for j, label in enumerate(all_gases):
    for dataset in data:
        axes[0].plot(data[dataset][label_x], data[dataset][label + ' released/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Released atoms to birth (/)')
axes[0].set_title('Release to birth')
axes[0].set_xlim([xlim_i,xlim_f])
axes[0].set_ylim([0,1])

ax_temp_0 = axes[0].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

x_middle = (xlim_o + xlim_i) / 2
closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
y_middle = data[dataset]['Temperature (K)'][closest_idx]


ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
                    fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[1].plot(data[dataset][label_x], data[dataset][label + ' available/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Available atoms to birth (/)')
axes[1].set_title('Available to birth')
axes[1].set_xlim([xlim_i,xlim_f])
axes[1].set_ylim([0,1])

ax_temp_0 = axes[1].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

x_middle = (xlim_o + xlim_i) / 2
closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
y_middle = data[dataset]['Temperature (K)'][closest_idx]


ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
                    fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

for j, label in enumerate(volatile_fps):
    dataset = "With Thermochemistry"
    axes[2].plot(data[dataset][label_x], data[dataset][label + ' reacted/available'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[2].set_xlabel(label_x)
axes[2].set_ylabel('Reacted atoms to available (/)')
axes[2].set_title('Reacted to available')
axes[2].set_xlim([xlim_i,xlim_f])
axes[2].set_ylim([0,1])

ax_temp_0 = axes[2].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

x_middle = (xlim_o + xlim_i) / 2
closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
y_middle = data[dataset]['Temperature (K)'][closest_idx]


ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
                    fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

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
#plt.show()

############################################### PRODUCED ####################################

fig, axes = plt.subplots(1, 2, figsize=(15, 6))

for j, label in enumerate(all_products):
    dataset = "With Thermochemistry"
    if data[dataset][label + ' produced/available'].max() > 0.01:
        axes[0].plot(data[dataset][label_x], data[dataset][label + ' produced/available'], 
                        linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Produced to available (/)')
axes[0].set_title('Produced to available')
axes[0].set_xlim([xlim_o,xlim_i])
axes[0].set_ylim([0,1])
axes[0].legend(frameon = False,loc='upper left')

ax_temp_0 = axes[0].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

x_middle = (xlim_o + xlim_i) / 2
closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
y_middle = data[dataset]['Temperature (K)'][closest_idx]


ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
                    fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

for j, label in enumerate(all_products):
    dataset = "With Thermochemistry"
    if data[dataset][label + ' produced/available'].max() > 0.001:
        axes[1].plot(data[dataset][label_x], data[dataset][label + ' produced/available'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Produced to available (/)')
axes[1].set_title('Produced to available')
axes[1].set_xlim([xlim_i,xlim_f])
axes[1].set_ylim([0,1])
axes[1].legend(frameon = False,loc='upper left')

ax_temp_0 = axes[1].twinx()
for dataset in data:
    ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

x_middle = (xlim_o + xlim_i) / 2
closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
y_middle = data[dataset]['Temperature (K)'][closest_idx]


ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
                    fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

plt.tight_layout()
plt.savefig(folder_path +"/Products.png")
#plt.show()

############################################### PLOT RELEASED VS TEMPERATURA ####################################

# Experimental data
mass_sample = 10e-6  # kg
amu = 1.660539e-27  # kg
density = 10641.0  # kg/m3
volume_sample = mass_sample / density
print('------------------------------------------')
print('Experimental data')
print(f'Sample mass {round(mass_sample*1e6)} mg, volume {round(volume_sample*1e9,3)} mm3')

# Load additional experimental data
additional_data = {
    'Cs': pd.read_csv('data_notoxidised/Cs.txt', sep=';'),
    'I': pd.read_csv('data_notoxidised/I.txt', sep=';'),
    'Xe': pd.read_csv('data_notoxidised/Xe.txt', sep=';')
}

# Define isotopes and related properties
isotope_data = {
    'Cs': {'element': 'Cs', 'atomic_mass': 137},
    'I': {'element': 'I', 'atomic_mass': 129},
    'Xe': {'element': 'Xe', 'atomic_mass': 131} 
}

plt.figure(figsize=(10, 6))


for isotope, properties in isotope_data.items():
    element = properties['element']

    for dataset in data:
        cumulative_release_sim = (
            (data[dataset][f'{element} released (at/m3)'] - data[dataset].loc[mask_RAMP.idxmax(),f'{element} released (at/m3)'])/(data[dataset].iloc[-1][f'{element} produced (at/m3)'] - data[dataset].loc[mask_RAMP.idxmax(),f'{element} released (at/m3)'])
        )
        
        sim_filtered = data[dataset][data[dataset]['Time (h)'] > xlim_i]
        cumulative_release_sim = cumulative_release_sim[sim_filtered.index]

        plt.plot(
            sim_filtered['Temperature (K)'],
            cumulative_release_sim,
            linestyle=linestyles[dataset],
            color=colors[list(isotope_data.keys()).index(isotope)],
            label=f'Sim. {isotope} {dataset}'
        )

    plt.plot(
        additional_data[element]['Temperature (K)'],
        additional_data[element]['Release/birth (/)'],
        marker='o',
        linestyle='--',
        color=colors[list(isotope_data.keys()).index(isotope)],
        label=f'Exp. {isotope} {dataset}'
    )

plt.vlines(x = 2020, ymin = 0, ymax = 1, color = 'tab:red', linestyle = ':', label = 'UO2 Vaporisation onset')

plt.xlabel('Temperature (K)')
plt.ylabel('Cumulative release (/)')
plt.ylim([0,1])
plt.title('Comparison with experimental data')

handles, labels = plt.gca().get_legend_handles_labels()
legend_markers = [plt.Line2D([0], [0], marker='o', linestyle='--', color='black', label='Colle (2005)' )]
legend_lines = [plt.Line2D([0], [0], linestyle='-', color='black', label='Simulation w THERMOCHIMICA'),
                plt.Line2D([0], [0], linestyle='--', color='black', label='Simulazioni w\o THERMOCHIMICA'),
                plt.Line2D([0], [0], linestyle=':', color='tab:red', label='UO2 sublimation onset')]
legend_colors = [plt.Line2D([0], [0], marker='o', linestyle='', color=colors[list(isotope_data.keys()).index(isotope)], label=f'{isotope}') for isotope, properties in isotope_data.items()]

plt.legend(handles= legend_colors +legend_markers + legend_lines, loc='upper left', frameon=False)


plt.tight_layout()

plt.savefig(folder_path + "/Cumulative_Release_Comparison.png")