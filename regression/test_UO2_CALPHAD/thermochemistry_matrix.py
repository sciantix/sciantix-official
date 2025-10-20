import subprocess
import shutil
import os 
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import numpy as np
import glob
import re
from collections import defaultdict

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


# Path to input settings file
input_file = "input_settings.txt"
input_initial = "input_initial_conditions.txt"
results_dir = "results"
plot_only = True

# Function to modify input settings
def modify_input_settings(value):
    with open(input_file, 'r') as file:
        lines = file.readlines()
    
    lines[25] = value + lines[25][1:]
    
    with open(input_file, 'w') as file:
        file.writelines(lines)

def modify_input_initial_conditions(value):
    with open(input_initial, 'r') as file:
        lines = file.readlines()
    
    lines[24] = value + '\n'

    with open(input_initial, 'w') as file:
        file.writelines(lines)

# Function to run sciantix and save output
def run_sciantix(output_name):
    subprocess.run("./sciantix.x", shell=True, check=True)
    shutil.move("thermochemistry_output.txt", f"{results_dir}/thermochemistry_{output_name}")
    shutil.move("output.txt", f"{results_dir}/{output_name}")
    shutil.move("input_check.txt", f"{results_dir}/input_check_{output_name}")

def plot(folder_path):



    # Remove previous plots
    png_files = glob.glob(os.path.join(folder_path, "*.png"))
    for file in png_files:
        os.remove(file)

    # Load data
    data = {}
    data['With Thermochemistry'] = pd.read_csv(folder_path + '/output_chemistry.txt', sep='\t')

    # Print some main variables at the end of irradiation
    print('--------------------------------------------------------------------')
    print('Simulation results')
    print('Burnup (MWd/kgUO2) = ', data['With Thermochemistry']['Burnup (MWd/kgUO2)'].max()) 
    print('Grain diameter (um) = ', 2e6*data['With Thermochemistry']['Grain radius (m)'].max()) 
    print('Initial Stoichiometry = ', data['With Thermochemistry']['Stoichiometry deviation (/)'].iloc[0])
    xdev = data['With Thermochemistry']['Stoichiometry deviation (/)'].iloc[0]
    OMoleFraction = (2 + xdev) / (1 + 2 + xdev)

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

    ##################################### grain radius ########################
    plt.figure(figsize=(10, 6))
    dataset = "With Thermochemistry"
    label_x = "Temperature (K)"
    x = "Grain radius (m)"
    plt.plot(data[dataset][label_x],data[dataset][x] , 
                        linestyle=linestyles[dataset])
    plt.xlabel(label_x)
    plt.ylabel(x)
    plt.title(f"Oxygen mole fraction: {OMoleFraction:.3f}")
    plt.tight_layout()
    plt.savefig(folder_path +"/Radius.png")
    #plt.show()
    ##################################### U/O content ########################
    plt.figure(figsize=(10, 6))
    dataset = "With Thermochemistry"
    label_x = "Temperature (K)"
    x = "Content (mol/m3)"
    plt.plot(data[dataset][label_x],data[dataset]["Uranium content (mol/m3)"] , 
                        linestyle=linestyles[dataset], label ="U")
    plt.plot(data[dataset][label_x],data[dataset]["Oxygen content (mol/m3)"] , 
                        linestyle=linestyles[dataset], label ="O")
    plt.xlabel(label_x)
    plt.ylabel(x)
    plt.legend()
    plt.title(f"Oxygen mole fraction: {OMoleFraction:.3f}")
    plt.tight_layout()
    plt.savefig(folder_path +"/Content.png")
    #plt.show()
    ##################################### Stoichiometry dev ########################
    plt.figure(figsize=(10, 6))
    dataset = "With Thermochemistry"
    label_x = "Temperature (K)"
    x = "Stoichiometry deviation (/)"
    plt.plot(data[dataset][label_x],data[dataset][x] , 
                        linestyle=linestyles[dataset])
    plt.xlabel(label_x)
    plt.ylabel(x)
    plt.title(f"Oxygen mole fraction: {OMoleFraction:.3f}")
    plt.tight_layout()
    plt.savefig(folder_path +"/Xstoic.png")
    #plt.show()
    ##################################### temperature ########################
    plt.figure(figsize=(10, 6))
    dataset = "With Thermochemistry"
    x = "Temperature (K)"
    label_x = "Time (h)"
    plt.plot(data[dataset][label_x],data[dataset][x] , 
                        linestyle=linestyles[dataset])
    plt.xlabel(label_x)
    plt.ylabel(x)
    plt.title(f"Oxygen mole fraction: {OMoleFraction:.3f}")
    plt.tight_layout()
    plt.savefig(folder_path +"/Temperature.png")
    #plt.show()
    ############################################### MATRIX ###################################
    plt.figure(figsize=(10, 6))
    dataset = "With Thermochemistry"
    label_x = "Temperature (K)"
    location = 'matrix'
    total_moles = data[dataset]["Uranium content (mol/m3)"] + data[dataset]["Oxygen content (mol/m3)"]
    total_moles = 1
    i = 0
    for phase in thermochemistry[location].keys():
        for compound, values in thermochemistry[location][phase].items():
            print(f"Plotting {compound} ({phase})")
            values = np.nan_to_num(values, nan=0).astype(float)
            if np.any(values)>0:
                norm_values = np.divide(values, total_moles, 
                                        out=np.zeros_like(values, dtype=float), 
                                        where=total_moles!=0)
                plt.plot(data[dataset][label_x], norm_values, label=compound+" ("+phase+")", color=colors[i], linestyle = '--' if phase=='gas' else '-')
                i += 1

    plt.xlabel(label_x)
    plt.ylabel("Specie's mole fraction")
    plt.legend(loc='center left', bbox_to_anchor=(1.01, 0.5), frameon=False)
    plt.title(f"Mole fraction O = {OMoleFraction:.2f}")
    plt.tight_layout()
    plt.savefig(folder_path + "/MATRIX.png")
    #plt.show()
    return

shutil.copy("../../build/sciantix.x", ".")

#otom = np.linspace(0.01, 0.99, 50)
#stoichiometry = ((2-3*otom)/(otom-1)).tolist()
stoichiometry = np.array([-0.5, -0.1, 0, 0.1, 0.5]).tolist()
print(stoichiometry)
phase_lines_all = []
phase_lines_all.append("Oxygen fraction\tTemperature(K)\tStable phase(s)\n")

for i, stoc in enumerate(stoichiometry):
    print("----------------------------"+str(stoc)+"----------------------------------------")
    modify_input_initial_conditions(str(stoc))
    folder_name = str(stoc).replace('.', '_')
    if plot_only == False:
        run_sciantix("output_chemistry.txt")
        plot(results_dir)
        os.makedirs(folder_name, exist_ok=True)
        for file in os.listdir(results_dir):
            if file.endswith(".png") or file.endswith(".txt"):
                shutil.move(os.path.join(results_dir, file), os.path.join(folder_name, file))
    elif plot_only == True:
        plot(folder_name)
    
    data = pd.read_csv(os.path.join(folder_name, "output_chemistry.txt"), sep='\t')
    thermochemistry_data = pd.read_csv(os.path.join(folder_name, "thermochemistry_output_chemistry.txt"), sep='\t')

    thermochemistry = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
    for col in thermochemistry_data.columns:
        if "(" not in col or "mol" not in col:
            continue
        match = re.match(r"(.+)\s+\(([^,]+),\s*([^)]+)\)\s*\((mol/m3)\)", col)
        if not match:
            continue
        compound, phase, position, unit = match.groups()
        compound = compound.strip()
        phase = phase.strip().lower()
        position = position.strip().lower()
        thermochemistry[position][phase][compound] = thermochemistry_data[col].values

    temperatures = data["Temperature (K)"].values
    xdev = data['Stoichiometry deviation (/)'].values
    oxygen_fraction_array = (2 + xdev) / (1 + 2 + xdev)

    for i, T in enumerate(temperatures):
        stable_phases = []
        location = "matrix"
        totalvalue = 0

        # calcolo del totale
        for phase, compounds in thermochemistry[location].items():
            totalvalue += sum(values[i] for values in compounds.values())

        grouped = {}

        for phase, compounds in thermochemistry[location].items():
            for compound, values in compounds.items():
                frac = np.nan_to_num(values[i] / totalvalue)
                if frac > 0.01:
                    # normalizza il nome (toglie suffissi tipo _S1)
                    clean_name = re.sub(r'_S\d+$', '', compound)

                    # collassa le categorie
                    if phase.lower() == "gas":
                        key = ("Gas", "gas")
                    elif phase.lower() == "ionic_liquid":
                        key = ("Ionic_liquid", "ionic_liquid")
                    else:
                        key = (clean_name, phase)

                    grouped[key] = grouped.get(key, 0) + frac

        # costruisci la stringa finale
        stable_phases = [f"{name}({frac:.2f}, {phase})" for (name, phase), frac in grouped.items()]
        stable_phases_str = ",".join(sorted(set(stable_phases)))
        phase_lines_all.append(f"{oxygen_fraction_array[i]:.4f}\t{T:.2f}\t{stable_phases_str}\n")

    # Scrivi file unico
    with open("phase_diagram_all.txt", 'w') as f:
        f.writelines(phase_lines_all)

    #shutil.rmtree(folder_name)

# Leggi il file
df = pd.read_csv("phase_diagram_all.txt", sep="\t")  # sostituisci con il tuo file


def phase_combo(phases_str):
    """Estrae combinazioni di fasi ignorando le frazioni numeriche."""
    if not isinstance(phases_str, str) or phases_str.strip() == "":
        return "Unknown"

    # trova pattern tipo Nome(numero, stato)
    matches = re.findall(r'([A-Za-z0-9_]+)\([^)]*,\s*([^)]+)\)', phases_str)
    # matches = [('UO3', 'condensed'), ('C1_MO2', 'gas'), ...]
    
    phases = [f"{name}" for name, state in matches]

    # normalizza: ordina e rimuovi duplicati
    return " + ".join(sorted(set(phases))) if phases else "Unknown"

# Applica la funzione
df['Phase combo'] = df['Stable phase(s)'].apply(phase_combo)

# Assegna un colore unico a ogni combinazione
unique_combos = df['Phase combo'].unique()
phase_colors = {combo: plt.cm.tab20(i % 20) for i, combo in enumerate(unique_combos)}

# Crea la figura
plt.close()
fig, ax = plt.subplots(figsize=(20,6))
for combo, group in df.groupby('Phase combo'):
    ax.scatter(group['Oxygen fraction'], group['Temperature(K)'],
            color=phase_colors[combo], label=combo, s=80)
    print(combo)

ax.set_xlabel("Oxygen fraction")
ax.set_ylabel("Temperature (K)")
ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=8)
plt.tight_layout()
plt.savefig("plotphase.png")
#plt.show()