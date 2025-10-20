import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import numpy as np
import os
import glob
import re
from collections import defaultdict

folder_path = "results"
avogadronumber = 6.02214e23
xlim_i = 53400.9

mpl.rcParams.update({
    "font.family": "arial",
    "font.size": 20,
    "axes.labelsize": 18,
    "axes.titlesize": 18,
    "xtick.labelsize": 15,
    "ytick.labelsize": 15,
    "legend.fontsize": 15,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.3,
    "grid.linestyle": "--",
    "lines.linewidth": 2,
    "lines.markersize": 6,
})

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

# Define classes
inert_gases = ['Xe', 'Kr']
volatile_fps = ['Cs', 'I', 'Te']

thermochemistry_data = pd.read_csv(folder_path + '/thermochemistry_output_chemistry.txt', sep='\t')

thermochemistry = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))

for col in thermochemistry_data.columns:
    if "(" not in col or "mol" not in col:
        continue

    # Match: Nome (fase, posizione) (unità)
    match = re.match(r"(.+)\s+\(([^,]+),\s*([^)]+)\)\s*\((mol/m3|mol/m3)\)", col)

    if not match:
        print(f"Colonna non riconosciuta: {col}")
        continue

    compound, phase, position, unit = match.groups()
    compound = compound.strip()
    phase = phase.strip().lower()
    position = position.strip().lower()

    thermochemistry[position][phase][compound] = thermochemistry_data[col].values

def total_phase_at_position(location, phase):
    for compound, values in thermochemistry[location][phase].items():
        total = np.zeros_like(values, dtype=float)
        break
    else:
        raise ValueError(f"Nessun composto trovato per {location}, {phase}")

    for compound, values in thermochemistry[location][phase].items():
        values = np.nan_to_num(values, nan=0.0)
        total += values
    return total

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

####################################### Comparison with experimetal data at 1900 K ########################################
mask_RAMP = data['With Thermochemistry']['Time (h)'] >= xlim_i
mask_RAMP2 = data['With Thermochemistry']['Temperature (K)'] >= 1900

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
    'Reacted - IG': 'gold',
    'Precipitated': 'slategray'
}

def plot_species(data, label, ax, dataset):
    """ Plotta i dati per una specie specifica (Xe o Cs) """
    linestyle = linestyles[dataset]
    
    if dataset == 'With Thermochemistry':
        ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' produced (at/m3)']/data[dataset][label + ' produced (at/m3)'], 
                linestyle=linestyle, color=colors_element['Produced'], label='Produced')
    ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' in grain (at/m3)']/data[dataset][label + ' produced (at/m3)'], 
            linestyle=linestyle, color=colors_element['In grain'], label='In grain')
    ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' at grain boundary (at/m3)']/data[dataset][label + ' produced (at/m3)'], 
            linestyle=linestyle, color=colors_element['At grain boundary'], label='At grain boundary')
    ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' released (at/m3)']/data[dataset][label + ' produced (at/m3)'], 
            linestyle=linestyle, color=colors_element['Released'], label='Released')
    
    # Aggiungi reacted solo per Cs
    if label in volatile_fps:
        if dataset == 'With Thermochemistry':
            ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' reacted - GB (at/m3)']/data[dataset][label + ' produced (at/m3)'], 
                    linestyle=linestyle, color=colors_element['Reacted - GB'], label='Reacted - GB')
            ax.plot(data[dataset]['Time (h)'], data[dataset][label + ' reacted - IG (at/m3)']/data[dataset][label + ' produced (at/m3)'], 
                    linestyle=linestyle, color=colors_element['Reacted - IG'], label='Reacted - IG')
            
    
    ax.set_xlabel('Time (h)')
    ax.set_ylabel('Atoms fraction (/)')
    if dataset == 'With Thermochemistry':
        ax.legend(frameon = False,loc='upper left')

species_list = ['Xe', 'Cs', 'I', 'Te']
label_x = 'Time (h)'
xlim_o = min(data['With Thermochemistry'][label_x])
xlim_f = max(data['With Thermochemistry'][label_x])


def plot_species(data, label, ax, dataset, show_legend=False):
    linestyle = linestyles[dataset]
    t = data[dataset]['Time (h)']
    norm = data[dataset][label + ' produced (at/m3)']
    
    lines = []
    lines += ax.plot(t, norm/norm, linestyle=linestyle, color=colors_element['Produced'], label='Produced')
    lines += ax.plot(t, data[dataset][label + ' in grain (at/m3)']/norm, linestyle=linestyle, color=colors_element['In grain'], label='In grain')
    lines += ax.plot(t, data[dataset][label + ' at grain boundary (at/m3)']/norm, linestyle=linestyle, color=colors_element['At grain boundary'], label='At grain boundary')
    lines += ax.plot(t, data[dataset][label + ' released (at/m3)']/norm, linestyle=linestyle, color=colors_element['Released'], label='Released')
    
    if label in volatile_fps:
        lines += ax.plot(t, data[dataset][label + ' reacted - GB (at/m3)']/norm, linestyle=linestyle, color=colors_element['Reacted - GB'], label='Reacted - GB')
        lines += ax.plot(t, data[dataset][label + ' reacted - IG (at/m3)']/norm, linestyle=linestyle, color=colors_element['Reacted - IG'], label='Reacted - IG')
    
    ax.set_xlabel('Time (h)')
    ax.set_ylabel('Atoms fraction (/)')
    
    if show_legend:
        handles, labels = ax.get_legend_handles_labels()
        return handles, labels
    return None, None

def plot_four_species(fig_name, time_range):
    fig, axes = plt.subplots(1, 4, figsize=(20, 5))
    
    all_handles, all_labels = None, None
    for i, spec in enumerate(species_list):
        handles, labels = plot_species(data, spec, axes[i], 'With Thermochemistry', show_legend=(i == 0))
        axes[i].set_title(spec)
        axes[i].set_xlim(time_range)
        if i == 1:
            all_handles, all_labels = handles, labels
    
    fig.legend(all_handles, all_labels, loc='center left', bbox_to_anchor=(1.01, 0.5), frameon=False)
    fig.tight_layout(rect=[0, 0, 1, 1])
    fig.savefig(folder_path + "/" + fig_name)

def plot_all_species_two_rows(fig_name, xlims):
    fig, axes = plt.subplots(2, 5, figsize=(20, 8))

    
    dataset = "With Thermochemistry"
    x = 'Temperature (K)'
    axes[0][0].plot(data[dataset][label_x],data[dataset][x] , 
                        linestyle=linestyles[dataset])
    axes[0][0].set_ylabel(x)
    axes[0][0].set_xlim([xlim_o,xlim_i])
    axes[0][0].set_ylim([500, 3000])
    axes[0][0].set_title('Temperature history')

    axes[1][0].plot(data[dataset][label_x],data[dataset][x], 
                        linestyle=linestyles[dataset])
    axes[1][0].set_xlabel(label_x)
    axes[1][0].set_ylabel(x)
    axes[1][0].set_xlim([xlim_i,xlim_f])
    axes[1][0].set_ylim([500, 3000])

    all_handles, all_labels = None, None
    for row, time_range in enumerate(xlims):
        for i, spec in enumerate(species_list):
            i +=1
            handles, labels = plot_species(data, spec, axes[row][i], 'With Thermochemistry', show_legend=(row == 0 and i == 2))
            if row == 0:
                axes[row][i].set_title(spec)
            axes[row][i].set_xlim(time_range)
            if row == 0 and i == 2:
                all_handles, all_labels = handles, labels
            if i == 1:
                axes[row][i].set_ylabel('Atoms fraction (/)')
            else:
                axes[row][i].set_ylabel('')
            if row == 1:
                axes[row][i].set_xlabel('Time (h)')
            else:
                axes[row][i].set_xlabel('')

            # ax_temp_0 = axes[row][i].twinx()
            # ax_temp_0.plot(data[dataset]['Time (h)'], data[dataset]['Temperature (K)'], 
            #             linestyle='-.', color='grey', linewidth = 1)
            # if i == 3:
            #     ax_temp_0.set_ylabel('Temperature (K)')
    
    fig.legend(all_handles, all_labels, loc='center right', frameon=False)
    fig.tight_layout(rect=[0, 0, 0.88, 1])
    fig.savefig(folder_path + "/" + fig_name)

plot_all_species_two_rows("XeCs_tot.png", [(xlim_o, xlim_i), (xlim_i, xlim_f)])
fig, ax = plt.subplots(1,1, figsize=(7,6))
plot_species(data, "I", ax, 'With Thermochemistry')
plt.xlim([xlim_i, xlim_f])
plt.savefig("Iodine.png")

fig, ax = plt.subplots(1,1, figsize=(7,6))
plt.plot(data['With Thermochemistry']["Time (h)"], data['With Thermochemistry']["Temperature (K)"])
plt.ylabel("Temperature (K)")
plt.xlabel("Time (h)")
plt.xlim([xlim_i, xlim_f])
plt.savefig("temperature.png")



##################################### stoichiometry ########################
fig, axes = plt.subplots(1, 2, figsize=(14, 6))

dataset = "With Thermochemistry"
x = 'Stoichiometry deviation (/)'
axes[0].plot(data[dataset][label_x],data[dataset][x] , 
                    linestyle=linestyles[dataset])
axes[0].set_xlabel(label_x)
axes[0].set_ylabel(x)
axes[0].set_xlim([xlim_o,xlim_i])
axes[0].set_ylim([-0.02,+0.02])

axes[1].plot(data[dataset][label_x],data[dataset][x], 
                    linestyle=linestyles[dataset])
axes[1].set_xlabel(label_x)
axes[1].set_ylabel(x)
axes[1].set_xlim([xlim_i,xlim_f])
axes[1].set_ylim([-0.02,+0.02])

plt.tight_layout()
plt.savefig(folder_path +"/stoic.png")

##################################### fractional coverage ########################
fig, axes = plt.subplots(1, 2, figsize=(14, 6))

dataset = "With Thermochemistry"
x = 'Intergranular fractional coverage (/)'
axes[0].plot(data[dataset][label_x],data[dataset][x] , 
                    linestyle=linestyles[dataset])
axes[0].set_xlabel(label_x)
axes[0].set_ylabel(x)
axes[0].set_xlim([xlim_o,xlim_i])

axes[1].plot(data[dataset][label_x],data[dataset][x], 
                    linestyle=linestyles[dataset])
axes[1].set_xlabel(label_x)
axes[1].set_ylabel(x)
axes[1].set_xlim([xlim_i,xlim_f])
plt.tight_layout()
plt.savefig(folder_path +"/Fc.png")
#plt.show()

##################################### grain radius ########################
fig, axes = plt.subplots(1, 2, figsize=(14, 6))

dataset = "With Thermochemistry"
x = 'Grain radius (m)'
axes[0].plot(data[dataset][label_x],data[dataset][x] , 
                    linestyle=linestyles[dataset])
axes[0].set_xlabel(label_x)
axes[0].set_ylabel(x)
axes[0].set_xlim([xlim_o,xlim_i])

axes[1].plot(data[dataset][label_x],data[dataset][x], 
                    linestyle=linestyles[dataset])
axes[1].set_xlabel(label_x)
axes[1].set_ylabel(x)
axes[1].set_xlim([xlim_i,xlim_f])
plt.tight_layout()
plt.savefig(folder_path +"/aradius.png")
#plt.show()
##################################### temperature ########################
fig, axes = plt.subplots(1, 2, figsize=(10, 5))

dataset = "With Thermochemistry"
x = 'Temperature (K)'
axes[0].plot(data[dataset][label_x],data[dataset][x] , 
                    linestyle=linestyles[dataset])
axes[0].set_xlabel(label_x)
axes[0].set_ylabel(x)
axes[0].set_xlim([xlim_o,xlim_i])
axes[0].set_ylim([500, 3000])

axes[1].plot(data[dataset][label_x],data[dataset][x], 
                    linestyle=linestyles[dataset])
axes[1].set_xlabel(label_x)
axes[1].set_xlim([xlim_i,xlim_f])
axes[1].set_ylim([500, 3000])

axes[0].set_title('Base irradiation')
axes[1].set_title('Annealing test in KC')
plt.tight_layout()
plt.savefig(folder_path +"/Temperature.png")
#plt.show()
############################################### PRODUCED ####################################

# Dizionario di mapping: chiave originale -> nome leggibile
pretty_labels = {
    "Cs2I2": r"Cs$_2$I$_2$",
    "Cs2Te": r"Cs$_2$Te",
    "Cs2Te2": r"Cs$_2$Te$_2$",
    "Cs2Te(s)": r"Cs$_2$Te (s)",
    "Cs2Te3(s)": r"Cs$_2$Te$_3$ (s)",
    "Cs2Te5(s)": r"Cs$_2$Te$_5$ (s)",
    "Cs2Te_b(s)": r"Cs$_2$Te (b, s)",
    "Cs5Te3(s)": r"Cs$_5$Te$_3$ (s)",
    "CsI3_csi3(s)": r"CsI$_3$ (s)",
    "CsI4_csi4(s)": r"CsI$_4$ (s)",
    "CsI_csi_b2(s)": r"CsI (b2, s)",
    "CsTe(s)": r"CsTe (s)",
    "CsTe4(s)": r"CsTe$_4$ (s)",
    "CsTe_b(s)": r"CsTe (b, s)",
    "Cs_bcc_a2(s)": r"Cs (bcc, a2)",
    "Cs_bcc_a2_2(s)": r"Cs (bcc, a2')",
    "Cs_hcp_a3(s)": r"Cs (hcp, a3)",
    "Cs_hex_a8(s)": r"Cs (hex, a8)",
    "I2_s(s)": r"I$_2$ (s)"
}


def plot_products(location, phases = ['gas', 'liquid', 'liquid_ionic', 'fcc_a1', 'pure_condensed'], initial=xlim_o, middle=xlim_i, final=xlim_f, label_x = 'Time (h)',threshold = 0.01):
    fig, axes = plt.subplots(1, 2 if label_x == 'Time (h)' else 1, figsize=(14, 6) if label_x == 'Time (h)' else (8, 6))

    if label_x == 'Temperature (K)':
        mask = (thermochemistry_data['Time (h)'] >= middle) & (thermochemistry_data['Time (h)'] <= final)
        x_data = thermochemistry_data['Temperature (K)'][mask].values
    else:
        x_data = thermochemistry_data[label_x].values


    total_location = np.zeros_like(x_data)
    for phase in phases:
        values = total_phase_at_position(location, phase)
        values = np.nan_to_num(values, nan=0)
        if label_x == 'Temperature (K)':
            values = values[mask]
        total_location += values
    
    compound_colors = {}
    cindex = 0
    already_plotted = set()
    for phase in phases:
        for compound, values in thermochemistry[location][phase].items():
            values = np.nan_to_num(values, nan=0)
            if label_x == 'Temperature (K)':
                values = values[mask]
            values = values.astype(float)
            total_location = total_location.astype(float)
            concentration = np.divide(
                values,
                total_location,
                out=np.zeros_like(values),
                where=total_location != 0
            )

            concentration = np.nan_to_num(concentration, nan=0)
            
            if max(concentration) > threshold:
                if compound not in compound_colors:
                    compound_colors[compound] = colors[cindex % len(colors)]
                    cindex += 1

                color = compound_colors[compound]
                linestyle = '--' if phase == 'gas' else '-'
                label = pretty_labels.get(compound, compound)  # se non trova usa l'originale

                plot_target = [axes] if label_x == 'Temperature (K)' else [axes[0], axes[1]]


                for ax in plot_target:
                    if label not in already_plotted:
                        ax.plot(x_data, concentration, color=color, linestyle=linestyle, label=label)
                    else:
                        ax.plot(x_data, concentration, color=color, linestyle=linestyle)
                already_plotted.add(label)
            
    if label_x == 'Time (h)':
        axes[0].set_xlabel(label_x)
        axes[1].set_xlabel(label_x)
        axes[0].set_xlim([initial, middle])
        axes[1].set_xlim([middle, final])
        axes[1].legend(frameon=False, loc='center left',  bbox_to_anchor=(1.01, 0.5))
            
        for ax in axes:
            ax.set_ylabel('Mole fraction (/)')
            ax.set_ylim([0, 1])
            ax_temp = ax.twinx()
            ax_temp.plot(thermochemistry_data[label_x], thermochemistry_data['Temperature (K)'],
                         linestyle='--', color='grey', linewidth=0.5)
            ax_temp.set_ylabel('Temperature (K)')
    else:
        axes.set_xlabel('Temperature (K)')
        axes.set_ylabel('Mole fraction (/)')
        axes.set_ylim([0, 1])
        axes.legend(frameon=False, loc='center left',  bbox_to_anchor=(1.01, 0.5))
        
    fig.suptitle('Products ' + location)
    fig.tight_layout()

    filename = "Products_" + location.replace(" ", "_")
    if label_x == 'Temperature (K)':
        filename += "_vs_Temperature"
    fig.savefig(folder_path + "/" + filename + ".png")
    plt.close(fig)

plot_products('at grain boundary')
plot_products('in the gap')
plot_products('at grain boundary', label_x='Temperature (K)')
plot_products('matrix', phases = ['gas', 'ionic_liquid', 'condensed'])
# ############################################### PRODUCED ####################################


# 
# fig, axes = plt.subplots(2, 1, figsize=(8, 8), sharex=True, height_ratios=[1, 0.25])

# dataset = "With Thermochemistry"


# filtered = data[dataset][data["With Thermochemistry"]['Xe at grain boundary (at/m3)'] >0]
# filtered = filtered.index

# Tfiltered = data[dataset].iloc[filtered]['Temperature (K)']
# # Plot mole fractions su axes[0]
# for j, label in enumerate(GAS_IDEAL_GB):
#     x = data[dataset].iloc[filtered][label]/ data[dataset].iloc[filtered]["GB total (mol)"]
#     x = np.nan_to_num(x, nan=0)
#     clean_label = label.replace("- GB (mol)", "")
#     clean_label = clean_label.replace("GAS_IDEAL", "gas")
#     if max(x) > 0.05:
#         axes[0].plot(Tfiltered, x, linestyle='-', color=colors[j], label=clean_label)

# for j, label in enumerate(LIQUID_GB + LIQUID_IONIC_GB + FCC_A1_GB + PURE_CONDENSED_PHASES_GB):
#     x = data[dataset].iloc[filtered][label] / data[dataset].iloc[filtered]["GB total (mol)"]
#     x = np.nan_to_num(x, nan=0)
#     clean_label = label.replace("- GB (mol)", "")
#     clean_label = clean_label.replace("LIQUID_IONIC", "liquid")
#     clean_label = clean_label.replace("LIQUID", "liquid")
#     clean_label = clean_label.replace("pure condensed phases", "condensed")
#     if max(x) > 0.05:
#         axes[0].plot(Tfiltered, x, linestyle='-.', color=colors[j + len(GAS_IDEAL_GB)], label=clean_label)

# axes[0].set_ylabel('Mole fraction (/)') 
# axes[0].set_ylim([0, 1])
# #axes[0].set_xlim([500, 3000])
# axes[0].legend(frameon=False, loc='best')

# # Plot somma dei gas su axes[1]
# gas_sum = np.zeros_like(Tfiltered)
# for label in GAS_IDEAL_GB:
#     gas_sum += data[dataset].iloc[filtered][label] / data[dataset].iloc[filtered]["GB total (mol)"]

# axes[1].plot(Tfiltered, gas_sum, linestyle='-', color='black')
# axes[1].set_xlabel('Temperature (K)')
# axes[1].set_ylabel('Gas fraction (/)') 
# axes[1].set_ylim([0, 1])
# #axes[1].set_xlim([500, 3000])
# axes[1].legend(frameon=False)

# plt.tight_layout()
# plt.savefig(folder_path + "/ProductsT.png")
# # plt.show()


############################################### PLOT RELEASED VS TEMPERATURA ####################################

# Experimental data
mass_sample = 10e-6  # kg
amu = 1.660539e-27  # kg
density = 10960.0  # kg/m3
volume_sample = mass_sample / density

n_grains = volume_sample/((4/3) * np.pi * (data['With Thermochemistry']['Grain radius (m)'].max())**3)
print('------------------------------------------')
print('Experimental data')
print(f'Sample mass {round(mass_sample*1e6)} mg, volume {round(volume_sample*1e9,3)} mm3')
print(f'Estimated number of grains per sample: {round(n_grains)}, relative factor: {(n_grains**(-1/3))}')

# Load additional experimental data
additional_data = {
    'Cs': pd.read_csv('data_notoxidised/Cs.txt', sep=';'),
    'I': pd.read_csv('data_notoxidised/I.txt', sep=';'),
    'Xe': pd.read_csv('data_notoxidised/Xe.txt', sep=';'),
    'Te': pd.read_csv('data_notoxidised/Te.txt', sep=';')
}

uo2_data = pd.read_csv('data_notoxidised/UO2.txt', sep=';')
uo_data = pd.read_csv('data_notoxidised/UO.txt', sep=';')

# Define isotopes and related properties
isotope_data = {
    'Cs': {'element': 'Cs', 'atomic_mass': 137},
    'I': {'element': 'I', 'atomic_mass': 129},
    'Xe': {'element': 'Xe', 'atomic_mass': 131},
     'Te': {'element': 'Te', 'atomic_mass': 127} 
}

plt.figure(figsize=(8, 6))


for isotope, properties in isotope_data.items():
    element = properties['element']

    for dataset in data:
        cumulative_release_sim = (
            (data[dataset][f'{element} released (at/m3)'] - data[dataset].loc[mask_RAMP.idxmax(),f'{element} released (at/m3)'])/(data[dataset].iloc[-1][f'{element} produced (at/m3)']- data[dataset].loc[mask_RAMP.idxmax(),f'{element} released (at/m3)'])
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

plt.plot(
    uo2_data['Temperature (K)'],
    uo2_data['Release/birth (/)'],
    marker='o',
    linestyle='--',
    linewidth=0.5,
    color='grey',
    label='Exp. UO2'
)

for dataset in data:
    plt.plot(
        data[dataset]['Temperature (K)'],
        1 - (data[dataset]['Grain radius (m)']/data[dataset]['Grain radius (m)'].max())**3,
        linestyle='-',
        color='grey',
        label='Calculated UO2'
    )

plt.xlabel('Temperature (K)')
plt.ylabel('Cumulative release (/)')
plt.xlim([750, 2773])
plt.ylim([0,1])
plt.title('Comparison with experimental data, Colle (2005)')

handles, labels = plt.gca().get_legend_handles_labels()
legend_markers = [plt.Line2D([0], [0], marker='o', linestyle='--', color='black', label='Colle (2005)' )]
legend_lines = [plt.Line2D([0], [0], linestyle='-', color='black', label='SCIANTIX w THERMOCHIMICA'),
                plt.Line2D([0], [0], linestyle='--', color='black', label='SCIANTIX w\o THERMOCHIMICA')]
                #plt.Line2D([0], [0], linestyle=':', color='tab:red', label='UO2 sublimation onset')]
legend_colors = [plt.Line2D([0], [0], marker='o', linestyle='--', color=colors[list(isotope_data.keys()).index(isotope)], label=f'Experimental {isotope}') for isotope, properties in isotope_data.items()]
legend_colors2 = [plt.Line2D([0], [0], marker='o', linestyle='--', color='grey', label=f'Experimental UO2')]

plt.legend(handles= legend_colors + legend_colors2 + legend_lines, loc='upper left', frameon=False)


plt.tight_layout()
plt.savefig(folder_path + "/Cumulative_Release_Comparison.png")
#plt.show()

plt.figure(figsize=(10,6))


for isotope, properties in isotope_data.items():
    element = properties['element']

    for dataset in ['With Thermochemistry']:
        cumulative_release_sim = (
            (data[dataset][f'{element} released (at/m3)'] - data[dataset].loc[mask_RAMP.idxmax(),f'{element} released (at/m3)'])/(data[dataset].iloc[-1][f'{element} produced (at/m3)']- data[dataset].loc[mask_RAMP.idxmax(),f'{element} released (at/m3)'])
        )
        
        sim_filtered = data[dataset][data[dataset]['Time (h)'] > xlim_i]
        cumulative_release_sim = cumulative_release_sim[sim_filtered.index]

        plt.plot(
            sim_filtered['Temperature (K)'],
            cumulative_release_sim,
            linestyle=linestyles[dataset],
            color=colors[list(isotope_data.keys()).index(isotope)],
            label=f'Sim. {isotope}'
        )

    plt.plot(
        additional_data[element]['Temperature (K)'],
        additional_data[element]['Release/birth (/)'],
        marker='o',
        linestyle='--',
        linewidth=0.5,
        color=colors[list(isotope_data.keys()).index(isotope)],
        label=f'Exp. {isotope}'
    )

# for dataset in ['With Thermochemistry']:
#     plt.plot(
#         data[dataset]['Temperature (K)'],
#         1 - (data[dataset]['Grain radius (m)']/data[dataset]['Grain radius (m)'].max())**3,
#         linestyle='-',
#         color='grey',
#         label='Calculated UO2'
#     )

for compound, values in thermochemistry[position][phase].items():
    if compound != "UO2": continue
    temperature = thermochemistry_data["Temperature (K)"].values
    norm = max(values) 
    plt.plot(
        temperature,
        values/norm,
        label=f'Sim. {compound}',
        color="gray"
    )


plt.plot(
    uo2_data['Temperature (K)'],
    uo2_data['Release/birth (/)'],
    marker='o',
    linestyle='--',
    linewidth=0.5,
    color='grey',
    label='Exp. UO2'
)



print(1 - (data[dataset].iloc[-1]['Grain radius (m)']/data[dataset]['Grain radius (m)'].max())**3)
plt.xlabel('Temperature (K)')
plt.ylabel('Cumulative release (/)')
plt.xlim([750, 2773])
plt.ylim([0,1])
#plt.title('Comparison with experimental data, Colle (2005)')

handles, labels = plt.gca().get_legend_handles_labels()
legend_markers = [plt.Line2D([0], [0], marker='o', linestyle='--', color='black', label='Colle (2005)' )]
legend_lines = [plt.Line2D([0], [0], linestyle='-', color='black', label='SCIANTIX w THERMOCHIMICA')]
   #              plt.Line2D([0], [0], linestyle='--', color='black', label='SCIANTIX w\o THERMOCHIMICA')]
legend_colors = [plt.Line2D([0], [0], marker='o', linestyle='--', color=colors[list(isotope_data.keys()).index(isotope)], label=f'Experimental {isotope}') for isotope, properties in isotope_data.items()]
legend_colors2 = [plt.Line2D([0], [0], marker='o', linestyle='--', color='grey', label=f'Experimental UO2')]


#plt.yscale('log')
plt.ylim([0, 1])
plt.legend(loc='upper left', frameon=False)#handles= legend_colors + legend_colors2 + legend_lines, loc='upper left', frameon=False)
plt.legend(frameon=False, loc='center left',  bbox_to_anchor=(1.01, 0.5))
plt.tight_layout()
plt.savefig(folder_path + "/Cumulative_Release_Comparison_log.png")
#plt.show()


plt.figure(figsize=(8, 6))

plt.plot(
    uo2_data['Temperature (K)'],
    uo2_data['Release/birth (/)'],
    marker='o',
    linestyle='--',
    linewidth=0.5,
    color='grey',
    label='Exp. UO2'
)

plt.plot(
    uo_data['Temperature (K)'],
    uo_data['Release/birth (/)'],
    marker='o',
    linestyle='--',
    linewidth=0.5,
    color='black',
    label='Exp. UO'
)

position = "matrix"
phase = "vapour"

# Assumendo che thermochemistry_data abbia una colonna Temperature (K)
temperature = thermochemistry_data["Temperature (K)"].values
time = thermochemistry_data["Time (h)"].values
xx = temperature
xlabel = "Temperature (K)"
plt.figure(figsize=(8,6))

for compound, values in thermochemistry[position][phase].items():
    plt.plot(
        xx,
        values,
        label=f'{compound} ({phase}, {position})'
    )

plt.xlabel(xlabel)
plt.ylabel('Concentration (mol/m3)')
plt.title(f'Compounds in phase "{phase}" at position "{position}"')
plt.legend()
plt.tight_layout()
filename = "NewVap"
plt.savefig(folder_path + "/" + filename + ".png")

position = "matrix"
phase = "solid"

plt.figure(figsize=(8,6))

for compound, values in thermochemistry[position][phase].items():
    plt.plot(
        xx,
        values,
        label=f'{compound} ({phase}, {position})'
    )

plt.xlabel(xlabel)
plt.ylabel('Concentration (mol/m3)')
plt.title(f'Compounds in phase "{phase}" at position "{position}"')
plt.legend()
plt.tight_layout()
filename = "NewVap_sol"
plt.savefig(folder_path + "/" + filename + ".png")

plt.figure(figsize=(8,6))
plt.plot(data['With Thermochemistry'][xlabel], data['With Thermochemistry']["Oxygen content (mol/m3)"], label= "Oxygen content (mol/m3)")
plt.plot(data['With Thermochemistry'][xlabel], data['With Thermochemistry']["Uranium content (mol/m3)"], label= "Uranium content (mol/m3)")
plt.xlabel(xlabel)
plt.ylabel('Concentration (mol/m3)')
plt.legend()
plt.tight_layout()
filename = "NewVap2"
plt.savefig(folder_path + "/" + filename + ".png")