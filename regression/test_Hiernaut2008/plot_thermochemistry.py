import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import numpy as np
import os
import glob

folder_path = "results"
avogadronumber = 6.02214e23
xlim_i = 53400

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
gas_ideal_GB = ['I - gas_ideal - GB (mol)', 'I2 - gas_ideal - GB (mol)', 'Cs - gas_ideal - GB (mol)', 'Cs2 - gas_ideal - GB (mol)', 'CsI - gas_ideal - GB (mol)', 'Cs2I2 - gas_ideal - GB (mol)']
liquid_GB = ['I2 - LIQUID - GB (mol)', 'Cs - LIQUID - GB (mol)', 'CsI - LIQUID - GB (mol)']
pure_condensed_GB = ['I2_s(s) - pure condensed phases - GB (mol)','Cs_bcc_a2(s) - pure condensed phases - GB (mol)','CsI_csi_b2(s) - pure condensed phases - GB (mol)','CsI3_csi3(s) - pure condensed phases - GB (mol)','CsI4_csi4(s) - pure condensed phases - GB (mol)']

gas_ideal_IG = ['I - gas_ideal - IG (mol)', 'I2 - gas_ideal - IG (mol)', 'Cs - gas_ideal - IG (mol)', 'Cs2 - gas_ideal - IG (mol)', 'CsI - gas_ideal - IG (mol)', 'Cs2I2 - gas_ideal - IG (mol)']
liquid_IG = ['I2 - LIQUID - IG (mol)', 'Cs - LIQUID - IG (mol)', 'CsI - LIQUID - IG (mol)']
pure_condensed_IG = ['I2_s(s) - pure condensed phases - IG (mol)','Cs_bcc_a2(s) - pure condensed phases - IG (mol)','CsI_csi_b2(s) - pure condensed phases - IG (mol)','CsI3_csi3(s) - pure condensed phases - IG (mol)','CsI4_csi4(s) - pure condensed phases - IG (mol)']

all_gases =  volatile_fps + inert_gases
all_products_GB = gas_ideal_GB + liquid_GB + pure_condensed_GB
all_products_IG = gas_ideal_IG + liquid_IG + pure_condensed_IG

# Define style
colors = ['dodgerblue', 'darkorange', 'forestgreen', 'crimson', 'hotpink', 'gold', 'slategray',
          'mediumpurple', 'saddlebrown', 'deepskyblue', 'darkolivegreen', 'limegreen', 'darkcyan', 'orangered']
linestyles = {'No Thermochemistry': '--', 'With Thermochemistry': '-'}

N = 60

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
    data['With Thermochemistry']['In grain (mol)'] = 0
for label in volatile_fps:
    data['With Thermochemistry']['At grain boundary (mol)'] += (1/avogadronumber)*(data['With Thermochemistry'][label+' at grain boundary (at/m3)'] + data['With Thermochemistry'][label+' reacted - GB (at/m3)'])/(data['With Thermochemistry']['Intergranular bubble concentration (bub/m2)'] * 3/ data['With Thermochemistry']['Grain radius (m)'])
    data['With Thermochemistry']['In grain (mol)'] += (1/avogadronumber)*(data['With Thermochemistry'][label+' in grain (at/m3)'] + data['With Thermochemistry'][label+' reacted - IG (at/m3)']) * (4/3 * 3.1415 * data['With Thermochemistry']['Grain radius (m)']**3)

for label in all_products_GB:
    data['With Thermochemistry'][label + ' produced/available - GB'] = data['With Thermochemistry'][label]/data['With Thermochemistry']['At grain boundary (mol)']
for label in all_products_IG:
    data['With Thermochemistry'][label + ' produced/available - IG'] = data['With Thermochemistry'][label]/data['With Thermochemistry']['In grain (mol)']

####################################### Comparison with experimetal data at 1900 K ########################################
mask_1900 = data['With Thermochemistry']['Temperature (K)'] >= 1900
mask_RAMP = data['With Thermochemistry']['Time (h)'] >= xlim_i
if mask_1900.any():
    idx_1900 = mask_1900.idxmax()
    time_1900 = data['With Thermochemistry'].loc[idx_1900, 'Time (h)']
    print('--------------------------------------------------------------------')
    print('Up to 1900 K - release/birth')
    for label in all_gases:
        data[dataset][label + ' released/birth - annealing'] = (data[dataset][label + ' released (at/m3)'] - data[dataset].loc[mask_RAMP.idxmax(),label + ' released (at/m3)'])/(data[dataset][label + ' produced (at/m3)'] - data[dataset].loc[mask_RAMP.idxmax(),label + ' released (at/m3)'])
        value = 100*data['With Thermochemistry'].loc[idx_1900, label + ' released/birth - annealing']
        print(f"  {label} simulated (%): {round(value)}")
else:
    time_1900 = None

print(f"Cs experimental (%): {round(30)}")
print(f"I experimental (%): {round(20)}")

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
        ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' reacted - GB (at/m3)'], 
                linestyle=linestyle, color=colors_element['Reacted - GB'], label='Reacted - GB')
        ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' reacted - IG (at/m3)'], 
                linestyle=linestyle, color=colors_element['Reacted - IG'], label='Reacted - IG')
    
    ax.set_xlabel('Time (h)')
    ax.set_ylabel('Atoms (at/m3)')
    ax.legend()

fig, axes = plt.subplots(1, 3, figsize=(18, 8))

for dataset in ['With Thermochemistry', 'No Thermochemistry']:
    plot_species(data, 'Xe', axes[0], dataset)
    plot_species(data, 'Cs', axes[1], dataset)
    plot_species(data, 'I', axes[2], dataset)
    # ax_temp_0 = axes[0].twinx()
    # linestyle = linestyles[dataset]
    # ax_temp_0.plot(data[dataset]['Time (h)'], data[dataset]['Intergranular fractional coverage (/)'], 
    #                linestyle=linestyle, color='grey', linewidth = 1)

axes[0].set_xlim([xlim_o,xlim_i])
axes[1].set_xlim([xlim_o,xlim_i])
axes[2].set_xlim([xlim_o,xlim_i])
axes[0].set_title('Xe')
axes[1].set_title('Cs')
axes[2].set_title('I')

plt.savefig(folder_path +"/XeCs_base.png")
plt.show()

fig, axes = plt.subplots(1, 3, figsize=(18, 8))

for dataset in ['With Thermochemistry', 'No Thermochemistry']:
    plot_species(data, 'Xe', axes[0], dataset)
    plot_species(data, 'Cs', axes[1], dataset)
    plot_species(data, 'I', axes[2], dataset)
    # ax_temp_0 = axes[0].twinx()
    # linestyle = linestyles[dataset]
    # ax_temp_0.plot(data[dataset]['Time (h)'], data[dataset]['Intergranular fractional coverage (/)'], 
    #                linestyle=linestyle, color='grey', linewidth = 1)

axes[0].set_xlim([xlim_i,xlim_f])
axes[1].set_xlim([xlim_i,xlim_f])
axes[2].set_xlim([xlim_i,xlim_f])
axes[0].set_title('Xe')
axes[1].set_title('Cs')
axes[2].set_title('I')
plt.savefig(folder_path +"/XeCs_annealing.png")
plt.show()


############################################### PRODUCED ####################################

fig, axes = plt.subplots(1, 2, figsize=(18, 8))

for j, label in enumerate(all_products_GB):
    dataset = "With Thermochemistry"
    if data[dataset][label].max() > 0:
        axes[0].plot(data[dataset][label_x], data[dataset][label], 
                        linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Produced (mol)')
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
    if data[dataset][label].max() > 0:
        axes[1].plot(data[dataset][label_x], data[dataset][label], 
                    linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Produced (mol)')
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
    if data[dataset][label].max() > 0:
        axes[0].plot(data[dataset][label_x], data[dataset][label], 
                        linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Produced (mol)')
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
    if data[dataset][label].max() > 0:
        axes[1].plot(data[dataset][label_x], data[dataset][label], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[1].set_xlabel(label_x)
axes[1].set_ylabel('Produced (mol)')
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
    'Cs137': pd.read_csv('data_notoxidised/Cs137.txt', sep=';'),
    'I129': pd.read_csv('data_notoxidised/I129.txt', sep=';')
}

# Define isotopes and related properties
isotope_data = {
    'Cs137': {'element': 'Cs', 'atomic_mass': 137, 'yield': 0.060897 / (6.227860e-02 + 6.765883e-2 / (1 + 1.90e-22 * 3e13 / 1.52e-6) + 6.723289e-2 / (1 + 2.72e-18 * 3e13 / 2.09e-5))},
    'I129': {'element': 'I', 'atomic_mass': 129, 'yield': 0.008137 / (0.001241 + 0.008137 + 0.02921)}
}

plt.figure(figsize=(15, 6))

# Process and plot simulation data
for dataset in data:
    for isotope, properties in isotope_data.items():
        element = properties['element']
        atomic_mass = properties['atomic_mass']
        isotope_yield = properties['yield']
        
        # Compute release rate
    
        data[dataset][f'{element} release rate (at/m3/s)'] = (
            data[dataset][f'{element} released (at/m3)'].diff() / (3600 * data[dataset]['Time (h)'].diff())
        )
        data[dataset][f'{isotope} release rate (kg/s)'] = (
            data[dataset][f'{element} release rate (at/m3/s)'] * volume_sample * isotope_yield * atomic_mass * amu
        )
        
        # Filter data for Time (h) > 54000
        filtered_data = data[dataset][data[dataset]['Time (h)'] > xlim_i]
        plt.plot(
            filtered_data['Temperature (K)'], 
            filtered_data[f'{isotope} release rate (kg/s)'],
            linestyle=linestyles[dataset], 
            color=colors[list(isotope_data.keys()).index(isotope)],
            label=f'Simulation: {isotope} {dataset}'
        )

# Plot experimental data
for i, dataset in enumerate(additional_data):
    plt.plot(
        additional_data[dataset]['Temperature (K)'],
        additional_data[dataset]['Release quantity (kg/s)'],
        marker='o',
        color=colors[i],
        label=f'Experiment {dataset}'
    )

plt.vlines(x=1900, ymin = 1e-13, ymax=1e-9, color='red', linestyle=':', label='1900K')
plt.ylim([1e-13, 1e-9])

plt.xlabel('Temperature (K)')
plt.ylabel('Release quantity (kg/s)')
plt.yscale('log')
plt.title('Comparison with experimental data')
plt.grid(True)
plt.legend()
plt.tight_layout()

plt.savefig(folder_path + "/Released_vs_Temperature.png")
plt.show()


def cumtrapz_custom(y, x):
    """Calcola l'integrazione cumulativa dei dati y rispetto a x utilizzando la regola dei trapezi."""
    cum_int = np.zeros(len(y))
    for i in range(1, len(y)):
        dx = x[i] - x[i-1]
        cum_int[i] = cum_int[i-1] + 0.5 * (y[i] + y[i-1]) * dx
    return cum_int

plt.figure(figsize=(15, 6))

for dataset in data:
    for isotope, properties in isotope_data.items():
        element = properties['element']
        atomic_mass = properties['atomic_mass']
        isotope_yield = properties['yield']
        
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
            label=f'Simulation cumulative {isotope} {dataset}'
        )

for i, dataset in enumerate(additional_data):
    exp_df = additional_data[dataset].copy()
    time_sec = exp_df['Temperature (K)'].values - exp_df['Temperature (K)'][0]/ (30/60) # 30K/h heating
    
    cumulative_release_exp = cumtrapz_custom(exp_df['Release quantity (kg/s)'].values, time_sec)
    cumulative_release_exp /= cumulative_release_exp[-1]
    
    plt.plot(
        exp_df['Temperature (K)'],
        cumulative_release_exp,
        marker='o',
        linestyle='--',
        color=colors[i],
        label=f'Experimental cumulative {dataset}'
    )

plt.vlines(x=1900, ymin = 0, ymax=1, color='red', linestyle=':', label='1900K')
plt.ylim([0,1])
plt.xlabel('Temperature (K)')
plt.ylabel('Cumulative release (/)')
plt.title('Comparison with experimental data')
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.savefig(folder_path + "/Cumulative_Release_Comparison.png")
plt.show()