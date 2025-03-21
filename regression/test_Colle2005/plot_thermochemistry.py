import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import numpy as np
import os
import glob

folder_path = "results"
avogadronumber = 6.02214e23
xlim_i = 53400.100

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
volatile_fps = ['Cs', 'I', 'Te']

GAS_IDEAL_IG = [
    "I - GAS_IDEAL - IG (mol)", "I2 - GAS_IDEAL - IG (mol)", "Cs - GAS_IDEAL - IG (mol)",
    "Cs2 - GAS_IDEAL - IG (mol)", "CsI - GAS_IDEAL - IG (mol)", "Cs2I2 - GAS_IDEAL - IG (mol)",
    "Cs2Te - GAS_IDEAL - IG (mol)", "Cs2Te2 - GAS_IDEAL - IG (mol)", "Cs2Te3 - GAS_IDEAL - IG (mol)",
    "CsTe - GAS_IDEAL - IG (mol)", "CsTe2 - GAS_IDEAL - IG (mol)", "Te - GAS_IDEAL - IG (mol)",
    "Te2 - GAS_IDEAL - IG (mol)", "Te3 - GAS_IDEAL - IG (mol)", "Te4 - GAS_IDEAL - IG (mol)",
    "Te5 - GAS_IDEAL - IG (mol)", "Te6 - GAS_IDEAL - IG (mol)", "Te7 - GAS_IDEAL - IG (mol)"
]

LIQUID_IG = [
    "I2 - LIQUID - IG (mol)", "Cs - LIQUID - IG (mol)", "CsI - LIQUID - IG (mol)"
]

LIQUID_IONIC_IG = [
    "Cs+:Va - LIQUID_IONIC - IG (mol)", "Cs2Te - LIQUID_IONIC - IG (mol)", "Te - LIQUID_IONIC - IG (mol)"
]

FCC_A1_IG = [
    "Cs:Te - FCC_A1 - IG (mol)", "Cs:Va - FCC_A1 - IG (mol)"
]

PURE_CONDENSED_PHASES_IG = [
    "I2_s(s) - pure condensed phases - IG (mol)", "Cs_bcc_a2(s) - pure condensed phases - IG (mol)",
    "CsI_csi_b2(s) - pure condensed phases - IG (mol)", "CsI3_csi3(s) - pure condensed phases - IG (mol)",
    "CsI4_csi4(s) - pure condensed phases - IG (mol)", "Cs_bcc_a2_2(s) - pure condensed phases - IG (mol)",
    "Cs2Te_b(s) - pure condensed phases - IG (mol)", "CsTe_b(s) - pure condensed phases - IG (mol)",
    "Cs2Te(s) - pure condensed phases - IG (mol)", "Cs2Te3(s) - pure condensed phases - IG (mol)",
    "Cs2Te5(s) - pure condensed phases - IG (mol)", "Cs5Te3(s) - pure condensed phases - IG (mol)",
    "CsTe(s) - pure condensed phases - IG (mol)", "CsTe4(s) - pure condensed phases - IG (mol)",
    "Cs_hcp_a3(s) - pure condensed phases - IG (mol)", "Cs_hex_a8(s) - pure condensed phases - IG (mol)"
]

GAS_IDEAL_GB = [
    "I - GAS_IDEAL - GB (mol)", "I2 - GAS_IDEAL - GB (mol)", "Cs - GAS_IDEAL - GB (mol)",
    "Cs2 - GAS_IDEAL - GB (mol)", "CsI - GAS_IDEAL - GB (mol)", "Cs2I2 - GAS_IDEAL - GB (mol)",
    "Cs2Te - GAS_IDEAL - GB (mol)", "Cs2Te2 - GAS_IDEAL - GB (mol)", "Cs2Te3 - GAS_IDEAL - GB (mol)",
    "CsTe - GAS_IDEAL - GB (mol)", "CsTe2 - GAS_IDEAL - GB (mol)", "Te - GAS_IDEAL - GB (mol)",
    "Te2 - GAS_IDEAL - GB (mol)", "Te3 - GAS_IDEAL - GB (mol)", "Te4 - GAS_IDEAL - GB (mol)",
    "Te5 - GAS_IDEAL - GB (mol)", "Te6 - GAS_IDEAL - GB (mol)", "Te7 - GAS_IDEAL - GB (mol)"
]

LIQUID_GB = [
    "I2 - LIQUID - GB (mol)", "Cs - LIQUID - GB (mol)", "CsI - LIQUID - GB (mol)"
]

LIQUID_IONIC_GB = [
    "Cs+:Va - LIQUID_IONIC - GB (mol)", "Cs2Te - LIQUID_IONIC - GB (mol)", "Te - LIQUID_IONIC - GB (mol)"
]

FCC_A1_GB = [
    "Cs:Te - FCC_A1 - GB (mol)", "Cs:Va - FCC_A1 - GB (mol)"
]

PURE_CONDENSED_PHASES_GB = [
    "I2_s(s) - pure condensed phases - GB (mol)", "Cs_bcc_a2(s) - pure condensed phases - GB (mol)",
    "CsI_csi_b2(s) - pure condensed phases - GB (mol)", "CsI3_csi3(s) - pure condensed phases - GB (mol)",
    "CsI4_csi4(s) - pure condensed phases - GB (mol)", "Cs_bcc_a2_2(s) - pure condensed phases - GB (mol)",
    "Cs2Te_b(s) - pure condensed phases - GB (mol)", "CsTe_b(s) - pure condensed phases - GB (mol)",
    "Cs2Te(s) - pure condensed phases - GB (mol)", "Cs2Te3(s) - pure condensed phases - GB (mol)",
    "Cs2Te5(s) - pure condensed phases - GB (mol)", "Cs5Te3(s) - pure condensed phases - GB (mol)",
    "CsTe(s) - pure condensed phases - GB (mol)", "CsTe4(s) - pure condensed phases - GB (mol)",
    "Cs_hcp_a3(s) - pure condensed phases - GB (mol)", "Cs_hex_a8(s) - pure condensed phases - GB (mol)"
]

all_gases =  volatile_fps + inert_gases
all_products_GB = GAS_IDEAL_GB + LIQUID_GB + LIQUID_IONIC_GB + FCC_A1_GB + PURE_CONDENSED_PHASES_GB
all_products_IG = GAS_IDEAL_IG + LIQUID_IG + LIQUID_IONIC_IG + FCC_A1_IG + PURE_CONDENSED_PHASES_IG

# Define style
colors = [
    'dodgerblue', 'darkorange', 'forestgreen', 'crimson', 'hotpink', 'gold', 'slategray',
    'mediumpurple', 'saddlebrown', 'deepskyblue', 'darkolivegreen', 'limegreen', 'darkcyan',
    'orangered', 'mediumseagreen', 'royalblue', 'peru', 'deeppink', 'lightcoral', 'darkslategray',
    'mediumvioletred', 'cadetblue', 'chocolate', 'yellowgreen', 'indigo', 'maroon', 'steelblue',
    'lightseagreen', 'cornflowerblue', 'firebrick', 'turquoise', 'darkmagenta', 'olivedrab',
    'tomato', 'orchid', 'goldenrod', 'lawngreen', 'seagreen', 'slateblue', 'palevioletred',
    'darkkhaki', 'rosybrown', 'lightsteelblue', 'teal', 'midnightblue', 'sienna', 'plum',
    'darkred', 'mediumturquoise', 'lightsalmon', 'darkblue', 'chartreuse', 'palegreen',
    'lightcoral', 'thistle', 'mediumspringgreen', 'mediumslateblue', 'fuchsia', 'navy',
    'bisque', 'gainsboro', 'powderblue', 'peachpuff', 'lightcyan'
]

linestyles = {'No Thermochemistry': '--', 'With Thermochemistry': '-'}

data['With Thermochemistry']['IG total (mol)'] = 0
for label in all_products_IG:
    data['With Thermochemistry']['IG total (mol)'] += data['With Thermochemistry'][label]

data['With Thermochemistry']['GB total (mol)'] = 0
for label in all_products_GB:
    data['With Thermochemistry']['GB total (mol)'] += data['With Thermochemistry'][label]


# # Compute derived quantities
# for dataset in data:
#     for label in all_gases:
#         data[dataset][label + ' released/birth'] = data[dataset][label + ' released (at/m3)']/data[dataset][label + ' produced (at/m3)']

#     #for label in inert_gases:
#         #print(label, dataset, data[dataset].loc[N, label + ' produced (at/m3)'], data[dataset].loc[N, label + ' released (at/m3)'], data[dataset].loc[N, label + ' at grain boundary (at/m3)'], data[dataset].loc[N, label + ' in grain (at/m3)'])
    
#     for label in volatile_fps:
#         data[dataset][label + ' available/birth'] = (data[dataset][label + ' reacted - GB (at/m3)']+ data[dataset][label + ' at grain boundary (at/m3)'])/data[dataset][label + ' produced (at/m3)']
#         data[dataset][label + ' reacted/birth'] = data[dataset][label + ' reacted - GB (at/m3)']/data[dataset][label + ' produced (at/m3)']
#         data[dataset][label + ' reacted/available'] = data[dataset][label + ' reacted/birth']/data[dataset][label + ' available/birth']
#         print(label, dataset, data[dataset].loc[N, label + ' produced (at/m3)'], 
#               data[dataset].loc[N, label + ' released (at/m3)'], 
#               data[dataset].loc[N, label + ' at grain boundary (at/m3)'], data[dataset].loc[N, label + ' reacted - GB (at/m3)'],
#               data[dataset].loc[N, label + ' in grain (at/m3)'], data[dataset].loc[N, label + ' reacted - IG (at/m3)'])
#         print(label, dataset, data[dataset].loc[N, label + ' produced (at/m3)'] -
#               data[dataset].loc[N, label + ' released (at/m3)'] -
#               data[dataset].loc[N, label + ' at grain boundary (at/m3)'] - data[dataset].loc[N, label + ' reacted - GB (at/m3)'] - 
#               data[dataset].loc[N, label + ' in grain (at/m3)'] -  data[dataset].loc[N, label + ' reacted - IG (at/m3)'])
    

# if 'At grain boundary (mol)' not in data['With Thermochemistry'].columns:
#     data['With Thermochemistry']['At grain boundary (mol)'] = 0  
#     data['With Thermochemistry']['In grain (mol)'] = 0
# for label in volatile_fps:
#     data['With Thermochemistry']['At grain boundary (mol)'] += (1/avogadronumber)*(data['With Thermochemistry'][label+' at grain boundary (at/m3)'] + data['With Thermochemistry'][label+' reacted - GB (at/m3)'])/(data['With Thermochemistry']['Intergranular bubble concentration (bub/m2)'] * 3/ data['With Thermochemistry']['Grain radius (m)'])
#     data['With Thermochemistry']['In grain (mol)'] += (1/avogadronumber)*(data['With Thermochemistry'][label+' in grain (at/m3)'] + data['With Thermochemistry'][label+' reacted - IG (at/m3)']) * (4/3 * 3.1415 * data['With Thermochemistry']['Grain radius (m)']**3)

# for label in all_products_GB:
#     data['With Thermochemistry'][label + ' produced/available - GB'] = data['With Thermochemistry'][label]/data['With Thermochemistry']['At grain boundary (mol)']
# for label in all_products_IG:
#     data['With Thermochemistry'][label + ' produced/available - IG'] = data['With Thermochemistry'][label]/data['With Thermochemistry']['In grain (mol)']

####################################### Comparison with experimetal data at 1900 K ########################################
mask_RAMP = data['With Thermochemistry']['Time (h)'] >= xlim_i

#################################### quantitative #################################
label_x = 'Time (h)'
xlim_o = min(data['With Thermochemistry'][label_x])
xlim_f = max(data['With Thermochemistry'][label_x])

colors_element = {
    'Produced': 'dodgerblue',
    'In grain': 'darkorange',
    'At grain boundary': 'forestgreen',
    'Released': 'crimson',
    'Reacted - GB': 'hotpink',
    'Reacted - IG': 'gold'
}

def plot_species(data, label, ax, dataset):
    """ Plotta i dati per una specie specifica (Xe o Cs) """
    linestyle = linestyles[dataset]
    
    if dataset == 'With Thermochemistry':
        ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' produced (at/m3)'], 
                linestyle=linestyle, color=colors_element['Produced'], label='Produced')
    ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' in grain (at/m3)'], 
            linestyle=linestyle, color=colors_element['In grain'], label='In grain')
    ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' at grain boundary (at/m3)'], 
            linestyle=linestyle, color=colors_element['At grain boundary'], label='At grain boundary')
    ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' released (at/m3)'], 
            linestyle=linestyle, color=colors_element['Released'], label='Released')
    
    # Aggiungi reacted solo per Cs
    if label in volatile_fps:
        if dataset == 'With Thermochemistry':
            ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' reacted - GB (at/m3)'], 
                    linestyle=linestyle, color=colors_element['Reacted - GB'], label='Reacted - GB')
            ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' reacted - IG (at/m3)'], 
                    linestyle=linestyle, color=colors_element['Reacted - IG'], label='Reacted - IG')
    
    ax.set_xlabel('Time (h)')
    ax.set_ylabel('Atoms (at/m3)')
    if dataset == 'With Thermochemistry':
        ax.legend(frameon = False,loc='upper left')

fig, axes = plt.subplots(1,4, figsize=(18, 8))

for dataset in ['With Thermochemistry', 'No Thermochemistry']:
    plot_species(data, 'Xe', axes[0], dataset)
    plot_species(data, 'Cs', axes[1], dataset)
    plot_species(data, 'I', axes[2], dataset)
    plot_species(data, 'Te', axes[3], dataset)
    
axes[0].set_xlim([xlim_o,xlim_i])
axes[1].set_xlim([xlim_o,xlim_i])
axes[2].set_xlim([xlim_o,xlim_i])
axes[3].set_xlim([xlim_o,xlim_i])
axes[0].set_title('Xe')
axes[1].set_title('Cs')
axes[2].set_title('I')
axes[3].set_title('Te')

plt.tight_layout()
plt.savefig(folder_path +"/XeCs_base.png")
plt.show()

fig, axes = plt.subplots(1, 4, figsize=(18, 8))

for dataset in ['With Thermochemistry', 'No Thermochemistry']:
    plot_species(data, 'Xe', axes[0], dataset)
    plot_species(data, 'Cs', axes[1], dataset)
    plot_species(data, 'I', axes[2], dataset)
    plot_species(data, 'Te', axes[3], dataset)
    # ax_temp_0 = axes[0].twinx()
    # linestyle = linestyles[dataset]
    # ax_temp_0.plot(data[dataset]['Time (h)'], data[dataset]['Intergranular fractional coverage (/)'], 
    #                linestyle=linestyle, color='grey', linewidth = 1)

axes[0].set_xlim([xlim_i,xlim_f])
axes[1].set_xlim([xlim_i,xlim_f])
axes[2].set_xlim([xlim_i,xlim_f])
axes[3].set_xlim([xlim_i,xlim_f])
axes[0].set_title('Xe')
axes[1].set_title('Cs')
axes[2].set_title('I')
axes[3].set_title('Te')

plt.tight_layout()
plt.savefig(folder_path +"/XeCs_annealing.png")
plt.show()


############################################### PRODUCED ####################################

fig, axes = plt.subplots(1, 2, figsize=(18, 8))

for j, label in enumerate(all_products_GB):
    dataset = "With Thermochemistry"
    x = data[dataset][label]/data[dataset]["GB total (mol)"]
    x = np.nan_to_num(x, nan=0)
    if max(x) > 0.01:
        axes[0].plot(data[dataset][label_x],x , 
                        linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Mole fraction (/)')
axes[0].set_xlim([xlim_o,xlim_i])
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

for j, label in enumerate(all_products_GB):
    dataset = "With Thermochemistry"
    x = data[dataset][label]/data[dataset]["GB total (mol)"]
    x = np.nan_to_num(x, nan=0)
    if max(x) > 0.01:
        axes[1].plot(data[dataset][label_x], x, 
                    linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Mole fraction (/)')
axes[1].set_xlim([xlim_i,xlim_f])
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

plt.title('Products at GB')
plt.tight_layout()
plt.savefig(folder_path +"/Products_GB.png")
plt.show()

############################################### PRODUCED ####################################

fig, axes = plt.subplots(1, 2, figsize=(18, 8))

for j, label in enumerate(all_products_IG):
    dataset = "With Thermochemistry"
    x = data[dataset][label]/data[dataset]["IG total (mol)"]
    x = np.nan_to_num(x, nan=0)
    if max(x) > 0.01:
        axes[0].plot(data[dataset][label_x], x, 
                        linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Mole fraction (/)')
axes[0].set_xlim([xlim_o,xlim_i])
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

for j, label in enumerate(all_products_IG):
    dataset = "With Thermochemistry"
    x = data[dataset][label]/data[dataset]["IG total (mol)"]
    x = np.nan_to_num(x, nan=0)
    if max(x) > 0.01:
        axes[1].plot(data[dataset][label_x],x, 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Mole fraction (/)')
axes[1].set_xlim([xlim_i,xlim_f])
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

plt.title('Products at IG')
plt.tight_layout()
plt.savefig(folder_path +"/Products_IG.png")
plt.show()
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
    'Xe': pd.read_csv('data_notoxidised/Xe.txt', sep=';'),
    'Te': pd.read_csv('data_notoxidised/Te.txt', sep=';')
}

# Define isotopes and related properties
isotope_data = {
    'Cs': {'element': 'Cs', 'atomic_mass': 137},
    'I': {'element': 'I', 'atomic_mass': 129},
    'Xe': {'element': 'Xe', 'atomic_mass': 131},
     'Te': {'element': 'Te', 'atomic_mass': 127} 
}

plt.figure(figsize=(10, 6))


for isotope, properties in isotope_data.items():
    element = properties['element']

    for dataset in data:
        cumulative_release_sim = (
            (data[dataset][f'{element} released (at/m3)'] - data[dataset].loc[mask_RAMP.idxmax(),f'{element} released (at/m3)'])/(data[dataset].iloc[-1][f'{element} released (at/m3)'] - data[dataset].loc[mask_RAMP.idxmax(),f'{element} released (at/m3)'])
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
        linewidth=0.5,
        color=colors[list(isotope_data.keys()).index(isotope)],
        label=f'Exp. {isotope} {dataset}'
    )

#plt.vlines(x = 2020, ymin = 0, ymax = 1, color = 'tab:red', linestyle = ':', label = 'UO2 Vaporisation onset')

plt.xlabel('Temperature (K)')
plt.ylabel('Cumulative release (/)')
plt.xlim([800, 2773])
plt.ylim([0,1])
plt.axvspan(2020, plt.xlim()[1], color='gray', alpha=0.2, label="UO2 Vaporisation")
plt.text(2200, plt.ylim()[1] * 0.2, "UO2 vaporisation", color='gray', fontsize=12)
plt.title('Comparison with experimental data')

handles, labels = plt.gca().get_legend_handles_labels()
legend_markers = [plt.Line2D([0], [0], marker='o', linestyle='--', color='black', label='Colle (2005)' )]
legend_lines = [plt.Line2D([0], [0], linestyle='-', color='black', label='Simulation w THERMOCHIMICA'),
                plt.Line2D([0], [0], linestyle='--', color='black', label='Simulazioni w\o THERMOCHIMICA')]
                #plt.Line2D([0], [0], linestyle=':', color='tab:red', label='UO2 sublimation onset')]
legend_colors = [plt.Line2D([0], [0], marker='o', linestyle='', color=colors[list(isotope_data.keys()).index(isotope)], label=f'{isotope}') for isotope, properties in isotope_data.items()]

plt.legend(handles= legend_colors +legend_markers + legend_lines, loc='upper left', frameon=False)


plt.tight_layout()
plt.savefig(folder_path + "/Cumulative_Release_Comparison.png")
plt.show()

# #################################### VS TIME #################################
# label_x = 'Time (h)'
# xlim_o = min(data['With Thermochemistry'][label_x])
# xlim_f = max(data['With Thermochemistry'][label_x])

# fig, axes = plt.subplots(1, 3, figsize=(15, 4))

# for j, label in enumerate(all_gases):
#     for dataset in data:
#         axes[0].plot(data[dataset][label_x], data[dataset][label + ' released/birth'], 
#                      linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
# axes[0].set_xlabel(label_x)
# axes[0].set_ylabel('Released atoms to birth (/)')
# axes[0].set_title('Release to birth')
# axes[0].set_xlim([xlim_o,xlim_i])
# axes[0].set_ylim([0,1])

# ax_temp_0 = axes[0].twinx()
# for dataset in data:
#     ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
#                    linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
# ax_temp_0.set_ylabel('Temperature (K)')

# x_middle = (xlim_o + xlim_i) / 2
# closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
# y_middle = data[dataset]['Temperature (K)'][closest_idx]


# ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
#                     fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

# for j, label in enumerate(volatile_fps):
#     dataset = "With Thermochemistry"
#     axes[1].plot(data[dataset][label_x], data[dataset][label + ' available/birth'], 
#                      linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
# axes[1].set_xlabel(label_x)
# axes[1].set_ylabel('Available atoms to birth (/)')
# axes[1].set_title('Available to birth')
# axes[1].set_xlim([xlim_o,xlim_i])
# axes[1].set_ylim([0,1])

# ax_temp_0 = axes[1].twinx()
# for dataset in data:
#     ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
#                    linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
# ax_temp_0.set_ylabel('Temperature (K)')

# x_middle = (xlim_o + xlim_i) / 2
# closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
# y_middle = data[dataset]['Temperature (K)'][closest_idx]


# ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
#                     fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

# for j, label in enumerate(volatile_fps):
#     dataset = "With Thermochemistry"
#     axes[2].plot(data[dataset][label_x], data[dataset][label + ' reacted/available'], 
#                      linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
# axes[2].set_xlabel(label_x)
# axes[2].set_ylabel('Reacted atoms to available (/)')
# axes[2].set_title('Reacted to available')
# axes[2].set_xlim([xlim_o,xlim_i])
# axes[2].set_ylim([0,1])

# ax_temp_0 = axes[2].twinx()
# for dataset in data:
#     ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
#                    linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
# ax_temp_0.set_ylabel('Temperature (K)')

# x_middle = (xlim_o + xlim_i) / 2
# closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
# y_middle = data[dataset]['Temperature (K)'][closest_idx]


# ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
#                     fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

# species_legend1 = [mlines.Line2D([], [], color=colors[j], marker='o', label=all_gases[j]) for j in range(len(all_gases))]
# species_legend2 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
# species_legend3 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
# thermo_legend = [mlines.Line2D([], [], color='black', linestyle=linestyles[dataset], label=dataset) for dataset in linestyles]

# # Add legends to the figure
# axes[0].legend(handles=(thermo_legend + species_legend1))
# axes[1].legend(handles=(thermo_legend + species_legend2))
# axes[2].legend(handles=(thermo_legend + species_legend3))

# plt.tight_layout()
# # plt.savefig(folder_path +"/vsTime1.png")
# # #plt.show()

# fig, axes = plt.subplots(1, 3, figsize=(15, 4))

# for j, label in enumerate(all_gases):
#     for dataset in data:
#         axes[0].plot(data[dataset][label_x], data[dataset][label + ' released/birth'], 
#                      linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
# axes[0].set_xlabel(label_x)
# axes[0].set_ylabel('Released atoms to birth (/)')
# axes[0].set_title('Release to birth')
# axes[0].set_xlim([xlim_i,xlim_f])
# axes[0].set_ylim([0,1])

# ax_temp_0 = axes[0].twinx()
# for dataset in data:
#     ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
#                    linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
# ax_temp_0.set_ylabel('Temperature (K)')

# x_middle = (xlim_o + xlim_i) / 2
# closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
# y_middle = data[dataset]['Temperature (K)'][closest_idx]


# ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
#                     fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

# for j, label in enumerate(volatile_fps):
#     dataset = "With Thermochemistry"
#     axes[1].plot(data[dataset][label_x], data[dataset][label + ' available/birth'], 
#                      linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
# axes[1].set_xlabel(label_x)
# axes[1].set_ylabel('Available atoms to birth (/)')
# axes[1].set_title('Available to birth')
# axes[1].set_xlim([xlim_i,xlim_f])
# axes[1].set_ylim([0,1])

# ax_temp_0 = axes[1].twinx()
# for dataset in data:
#     ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
#                    linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
# ax_temp_0.set_ylabel('Temperature (K)')

# x_middle = (xlim_o + xlim_i) / 2
# closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
# y_middle = data[dataset]['Temperature (K)'][closest_idx]


# ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
#                     fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

# for j, label in enumerate(volatile_fps):
#     dataset = "With Thermochemistry"
#     axes[2].plot(data[dataset][label_x], data[dataset][label + ' reacted/available'], 
#                      linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
# axes[2].set_xlabel(label_x)
# axes[2].set_ylabel('Reacted atoms to available (/)')
# axes[2].set_title('Reacted to available')
# axes[2].set_xlim([xlim_i,xlim_f])
# axes[2].set_ylim([0,1])

# ax_temp_0 = axes[2].twinx()
# for dataset in data:
#     ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
#                    linestyle='-.', color='grey', linewidth = 1, label='Temperature (K)')
# ax_temp_0.set_ylabel('Temperature (K)')

# x_middle = (xlim_o + xlim_i) / 2
# closest_idx = (np.abs(data[dataset][label_x] - x_middle)).argmin()
# y_middle = data[dataset]['Temperature (K)'][closest_idx]


# ax_temp_0.annotate('T vs t', xy=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle), xytext=(x_middle + 0.1 * (xlim_i - xlim_o), y_middle + 100),
#                     fontsize=12, color='grey', arrowprops=dict(arrowstyle='->', color='grey'))

# species_legend1 = [mlines.Line2D([], [], color=colors[j], marker='o', label=all_gases[j]) for j in range(len(all_gases))]
# species_legend2 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
# species_legend3 = [mlines.Line2D([], [], color=colors[j], marker='o', label=volatile_fps[j]) for j in range(len(volatile_fps))]
# thermo_legend = [mlines.Line2D([], [], color='black', linestyle=linestyles[dataset], label=dataset) for dataset in linestyles]

# # Add legends to the figure
# axes[0].legend(handles=(thermo_legend + species_legend1))
# axes[1].legend(handles=(thermo_legend + species_legend2))
# axes[2].legend(handles=(thermo_legend + species_legend3))

# plt.tight_layout()
# # plt.savefig(folder_path +"/vsTime2.png")
# # plt.show()
