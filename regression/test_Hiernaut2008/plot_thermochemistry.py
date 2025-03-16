import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import numpy as np

folder_path = "results"
avogadronumber = 6.02214e23
xlim_i = 54000

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
gas_ideal = ['I - gas_ideal (mol)', 'I2 - gas_ideal (mol)', 'Cs - gas_ideal (mol)', 'Cs2 - gas_ideal (mol)', 'CsI - gas_ideal (mol)', 'Cs2I2 - gas_ideal (mol)']
liquid = ['I2 - LIQUID (mol)', 'Cs - LIQUID (mol)', 'CsI - LIQUID (mol)']
pure_condensed = ['I2_s(s) - pure condensed phases (mol)','Cs_bcc_a2(s) - pure condensed phases (mol)','CsI_csi_b2(s) - pure condensed phases (mol)','CsI3_csi3(s) - pure condensed phases (mol)','CsI4_csi4(s) - pure condensed phases (mol)']

all_gases =  volatile_fps + inert_gases
all_products = gas_ideal + liquid + pure_condensed

# Define style
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

#################################### VS TIME #################################
label_x = 'Time (h)'
xlim_o = min(data['With Thermochemistry'][label_x])
xlim_f = max(data['With Thermochemistry'][label_x])

fig, axes = plt.subplots(1, 3, figsize=(15, 6))

for j, label in enumerate(all_gases):
    for dataset in data:
        axes[0].plot(data[dataset][label_x], data[dataset][label + ' released/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Released atoms to birth (/)')
axes[0].set_title('Release to birth')
axes[0].grid(True)
axes[0].set_xlim([xlim_o,xlim_i])
axes[0].set_ylim([0,1])

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
axes[1].set_ylim([0,1])

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
axes[2].set_ylim([0,1])

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
#plt.show()

fig, axes = plt.subplots(1, 3, figsize=(15, 6))

for j, label in enumerate(all_gases):
    for dataset in data:
        axes[0].plot(data[dataset][label_x], data[dataset][label + ' released/birth'], 
                     linestyle=linestyles[dataset], color=colors[j], label=f'{label}')
axes[0].set_xlabel(label_x)
axes[0].set_ylabel('Released atoms to birth (/)')
axes[0].set_title('Release to birth')
axes[0].grid(True)
axes[0].set_xlim([xlim_i,xlim_f])
axes[0].set_ylim([0,1])

if time_1900 is not None:
    axes[0].axvline(x=time_1900, color='red', linestyle=':', label='1900K')

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
axes[1].set_ylim([0,1])

if time_1900 is not None:
    axes[1].axvline(x=time_1900, color='red', linestyle=':', label='1900K')

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
axes[2].set_ylim([0,1])

if time_1900 is not None:
    axes[2].axvline(x=time_1900, color='red', linestyle=':', label='1900K')

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
#plt.show()

############################################### PRODUCED ####################################

fig, axes = plt.subplots(1, 2, figsize=(15, 6))

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

if time_1900 is not None:
    axes[0].axvline(x=time_1900, color='red', linestyle=':', label='1900K')
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

if time_1900 is not None:
    axes[1].axvline(x=time_1900, color='red', linestyle=':', label='1900K')
axes[1].legend()

ax_temp_0 = axes[1].twinx()
dataset = "With Thermochemistry"
ax_temp_0.plot(data[dataset][label_x], data[dataset]['Temperature (K)'], 
                   linestyle='dashed', color='black', alpha=0.3, label='Temperature (K)')
ax_temp_0.set_ylabel('Temperature (K)')

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