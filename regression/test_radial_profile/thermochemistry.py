import subprocess
import shutil
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import json
import re

import matplotlib as mpl
import matplotlib.lines as mlines

import glob
from collections import defaultdict

mpl.rcParams.update({
    "font.family": "arial",
    "font.size": 20,
    "axes.labelsize": 18,
    "axes.titlesize": 18,
    "xtick.labelsize": 15,
    "ytick.labelsize": 15,
    "legend.fontsize": 10,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.3,
    "grid.linestyle": "--",
    "lines.linewidth": 2,
    "lines.markersize": 6,
})

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

    ##################################### temperature ########################
    plt.figure(figsize=(8, 6))
    dataset = "With Thermochemistry"
    x = "Temperature (K)"
    label_x = "Time (h)"
    plt.plot(data[dataset][label_x],data[dataset][x] , 
                        linestyle=linestyles[dataset])
    plt.xlabel(label_x)
    plt.ylabel(x)
    plt.tight_layout()
    plt.savefig(folder_path +"/Temperature.png")
    #plt.show()
    ############################################### MATRIX ###################################
    plt.figure(figsize=(8, 6))
    dataset = "With Thermochemistry"
    label_x = "Time (h)"
    location = 'in the gap'
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
    plt.ylabel("(mol m$^{-3}$)")
    plt.legend(loc='center left', bbox_to_anchor=(1.01, 0.5), frameon=False)
    plt.title(f"In the gap")
    plt.tight_layout()
    plt.savefig(folder_path + "/inthegap.png")
    #plt.show()
    plt.figure(figsize=(8, 6))
    location = 'at grain boundary'
    i = 0
    for phase in thermochemistry[location].keys():
        for compound, values in thermochemistry[location][phase].items():
            print(f"Plotting {compound} ({phase})")
            values = np.nan_to_num(values, nan=0).astype(float)
            if np.any(values)>0:
                norm_values = np.divide(values, total_moles, 
                                        out=np.zeros_like(values, dtype=float), 
                                        where=total_moles!=0)
                plt.plot(data[dataset][label_x], norm_values, label=compound+"("+phase+")", color=colors[i], linestyle = '--' if phase=='gas' else '-')
                i += 1

    plt.xlabel(label_x)
    plt.ylabel("(mol m$^{-3}$)")
    plt.legend(loc='center left', bbox_to_anchor=(1.01, 0.5), frameon=False)
    plt.title(f"At grain boundary")
    plt.tight_layout()
    plt.savefig(folder_path + "/atgrainboundary.png")
    #plt.show()
    return

# Path to input settings file
input_file = "input_settings.txt"
input_initial = "input_initial_conditions.txt"
results_dir = "results"
plot_only = False
plot_only_only = True
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

def fissrate_frombu(burnup, time_s, density=10970, porosity=0.05, fissionenergy=220): #fiss m-3 s-1
    return (density*(1 - porosity))* burnup * (3600*24)/(time_s * fissionenergy * 1.6022e-19) 

shutil.copy("../../build/sciantix.x", ".")

totalradius = 4.7
radialprofile = [0, 1, 2, 3, 4, 4.7]
temperatureprofile_Celsius = [868, 840, 757, 623, 415, 302]
temperaturecoefficient = (np.array(temperatureprofile_Celsius) + 273)/(temperatureprofile_Celsius[-1] + 273)
temperaturecoefficient = list(temperaturecoefficient)
burnupprofile = [56, 56, 57, 60, 81, 136]
burnupcoefficient = np.array(burnupprofile)/burnupprofile[-1]
burnupcoefficient = list(burnupcoefficient)
pressurecoefficient = [1.0]*6

print("Temperature coefficients: ", temperaturecoefficient)
print("Burn-up/Fission rate coefficients: ", burnupcoefficient)
print("Pressure coefficients: ", pressurecoefficient)

########## History: outer fuel radius ############
data = np.loadtxt("input_history_radial.txt")

time_h   = list(data[:,0]) 
temperature_K      = list(data[:,1])  
fissionrate_m3s   = list(data[:,2])
pressure_MPa    = list(data[:,3])

KC = True
annealing_time = 53400.099

# Thermochemistry input settings

# get outermost radius values at the initial time
outer_T = temperature_K[0]*temperaturecoefficient[-1]
outer_P = pressure_MPa[0]*pressurecoefficient[-1]*1e6*(-1)

with open("input_thermochemistry.json", "r") as f_thermochemistry:
    thermo_data = json.load(f_thermochemistry)

thermo_data["Settings"]["fission_products"]["gap settings"] = True
thermo_data["Settings"]["fission_products"]["gap temperature"] = outer_T
thermo_data["Settings"]["fission_products"]["gap pressure"] = outer_P
thermo_data["Settings"]["KC"] = KC
thermo_data["Settings"]["KC time"] = annealing_time

with open("input_thermochemistry.json", "w") as f_thermochemistry:
    json.dump(thermo_data, f_thermochemistry, indent=2)

# Generate input files for each radial profile
for i, (r, cT, cF, cP) in enumerate(zip(radialprofile, temperaturecoefficient, burnupcoefficient, pressurecoefficient)):
    if plot_only_only == True: continue
    # Sciantix input settings
    filename = f"input_history.txt"
    radial_position = round(r/totalradius, 2)
    j_max = len(time_h)
    with open(filename, "w") as f_input:
        for j, (t_h, T_K, F_m3s, P_MPa) in enumerate(zip(time_h, temperature_K, fissionrate_m3s, pressure_MPa)):
            t_h *= 1
            if t_h <= annealing_time and KC == True:
                T_K *= cT
                F_m3s *= cF
                P_MPa *= cP
            P_Pa = P_MPa*1e6*(-1)
            f_input.write(f"{t_h:.4f}   {T_K:.6e}   {F_m3s:.6e}   {P_MPa:.6e}   {P_Pa:.6e}")
            if j < (j_max-1): f_input.write("\n")
    
    folder_name = str(radial_position).replace('.', '_')

    if plot_only == False:
        run_sciantix("output_chemistry.txt")
        plot(results_dir)
        os.makedirs(folder_name, exist_ok=True)
        for file in os.listdir(results_dir):
            if file.endswith(".png") or file.endswith(".txt"):
                shutil.move(os.path.join(results_dir, file), os.path.join(folder_name, file))
    elif plot_only == True:
        plot(folder_name)

####### Radial plot ##################

colors_set = ["blue", "orange", "green", "red"]
markers = ["o", "s", "d", "^", "v", "x", "*"]

def radial_profile(quantity, name, results_base=".", totalradius=4.7, radialprofile=None, norm = None, labeladd = "Concentration (at/m3)", mole=None, basetime = time_h[-1]):
    """
    Plot radial profiles of one or more quantities.
    
    Parameters
    ----------
    quantity : str or list of str
        Quantity/quantities to plot.
    name : str
        Name for output PNG file.
    results_base : str
        Base folder for results.
    totalradius : float
        Total radius of the system.
    radialprofile : list or array
        List of radii to process.
    norm : str or list of str, optional
        Quantity/quantities for normalization.
    labeladd : str, optional
        Additional label for y-axis.
    mole : bool, optional
        If True, convert to moles.
    basetime : float, optional
        Time (in hours) to extract data from.
    """
    if radialprofile is None:
        raise ValueError("Radial profile?")

    radialprofile = np.array(radialprofile, dtype=float)

    if isinstance(quantity, str):
        quantities = [quantity]
    else:
        quantities = list(quantity)

    plt.figure(figsize=(8,6))

    for i, q in enumerate(quantities):
        values_profiles = []

        for r in radialprofile:
            folder_name = str(round(r / totalradius, 2)).replace('.', '_')
            folder_path = os.path.join(results_base, folder_name)
            file_path = os.path.join(folder_path, "output_chemistry.txt")
            if not os.path.exists(file_path):
                print(f"Missing file: {file_path}")
                continue

            df = pd.read_csv(file_path, sep="\t")
            if q not in df.columns:
                print(f"Quantity '{q}' not found in {file_path}")
                values_profiles.append(0)
                continue

            if basetime: idx = df.index[df["Time (h)"].values < basetime][-1]

            value = df[q].values[idx]
            if norm is not None:
                value /= df[norm[i]].values[idx]
            if mole is not None and norm is None:
                value /= 6.022e23
            values_profiles.append(value)

        values_profiles = np.array(values_profiles)
        plt.plot(radialprofile / totalradius, values_profiles, "-", color=colors_set[i], marker=markers[i], label= re.sub(r"\s*\((?:mol/m3|at/m3)\)", "", q))
    plt.xlabel("Radial position (r/R)")
    if len(quantities) == 1:
        plt.ylabel(f"{quantities[0]}")
        if norm is not None:
            plt.ylabel(f"{quantities[0]} / {norm[0]}")
            plt.ylim([0,1])
    else:
        plt.ylabel(f"{name}")

    if labeladd is not None:
            plt.ylabel(f"{labeladd}")

    ncol = 2 if len(quantities) < 6 else 3
    plt.legend(loc="upper center", bbox_to_anchor=(0.5, -0.15), frameon=False, ncol=ncol)

    plt.tight_layout()
    plt.savefig(f"{name.replace(' ', '_')}_{str(basetime).replace('.', '_')}h.png", bbox_inches="tight")
    plt.close()

def radial_integral_all(results_base=".", totalradius=4.7, radialprofile=None, position="at grain boundary", basetime = time_h[-1]):
    """
    Integrate radially all thermochemical quantities (mol m$^{-3}$) for a given position.
    It plots radial profiles of all species.

    Parameters
    ----------
    results_base : str
        Base folder for results.
    totalradius : float
        Total radius of the system.
    radialprofile : list or array
        List of radii to process.
    position: str
        Location: at grain boundary, in grain, in the gap
    basetime : float, optional
        Time (in hours) to extract data from.
    """
    if radialprofile is None:
        raise ValueError("Radial profile?")

    species_profiles = {}

    for r in radialprofile:
        folder_name = str(round(r/totalradius, 2)).replace('.', '_')
        folder_path = os.path.join(results_base, folder_name)
        file_path = os.path.join(folder_path, "thermochemistry_output_chemistry.txt")
        if not os.path.exists(file_path):
            print(f"Missing file: {file_path}")
            continue

        df = pd.read_csv(file_path, sep="\t")
        for col in df.columns:
            if "(" not in col or "mol/m3" not in col:
                continue
            if position not in col:
                continue

            if basetime: idx = df.index[df["Time (h)"].values < basetime][-1]

            value = df[col].values[idx]
            if col not in species_profiles:
                species_profiles[col] = []
            species_profiles[col].append((r, value))

    plt.figure(figsize=(8,6))
    for col, rv_pairs in species_profiles.items():
        rv_pairs = sorted(rv_pairs, key=lambda x: x[0])
        radii = np.array([rv[0] for rv in rv_pairs])
        values = np.array([rv[1] for rv in rv_pairs])
        if values.any() > 0:
            integral = np.trapezoid(values, radii) / totalradius
            if integral < 1e-3: continue
            print(f"{col}: Normalized integral = {integral:.3e}")
            plt.plot(radii/totalradius, values, linestyle = "--" if "gas" in col.lower() else "-",marker="o", label= re.sub(r"\s*\((?:mol/m3|at/m3)\)", "", col))

    plt.xlabel("Radial position (r/R)")
    plt.ylabel("Concentration (mol m$^{-3}$)")
    plt.title(f"{position}")

    ncol = 2
    plt.legend(loc='upper center', bbox_to_anchor=(0.5, -0.2),
               ncol=ncol, frameon=False)

    plt.tight_layout()
    plt.savefig(f"radial_profiles_{position.replace(' ', '_')}_{str(basetime).replace('.', '_')}h.png", bbox_inches="tight")
    plt.close()

time_plot = annealing_time 

radial_integral_all(results_base=".", totalradius=totalradius,
                    radialprofile=radialprofile,
                    position="at grain boundary")
radial_integral_all(results_base=".", totalradius=totalradius,
                    radialprofile=radialprofile,
                    position="at grain boundary", basetime=time_plot)

radial_integral_all(results_base=".", totalradius=totalradius,
                    radialprofile=radialprofile,
                    position="in the gap")
radial_integral_all(results_base=".", totalradius=totalradius,
                    radialprofile=radialprofile,
                    position="in the gap", basetime=time_plot)

radial_profile("Fission rate (fiss / m3 s)", name="FissRate", results_base=".", totalradius=totalradius, radialprofile=radialprofile, basetime=time_plot)
radial_profile("Temperature (K)", name="Temperature", results_base=".", totalradius=totalradius, radialprofile=radialprofile, labeladd="Temperature (K)",basetime=time_plot)
radial_profile("Fission rate (fiss / m3 s)", name="FissRate", results_base=".", totalradius=totalradius, radialprofile=radialprofile)
radial_profile("Temperature (K)", name="Temperature", results_base=".", totalradius=totalradius, radialprofile=radialprofile, labeladd="Temperature (K)")
radial_profile("Burnup (MWd/kgUO2)", name="Burnup", results_base=".", totalradius=totalradius, radialprofile=radialprofile, basetime=time_plot, labeladd="Burnup (MWd/kgUO2)")
radial_profile(["Xe in grain (at/m3)", "Cs in grain (at/m3)", "I in grain (at/m3)", "Te in grain (at/m3)"], name="In grain", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd="In grain / produced", basetime=time_plot)
radial_profile(["Xe in grain (at/m3)", "Cs in grain (at/m3)", "I in grain (at/m3)", "Te in grain (at/m3)"], name="In grain", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd="In grain / produced")
radial_profile(["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], name="Produced", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=None, mole=True, labeladd="Produced (mol m-3)")
radial_profile(["Xe at grain boundary (at/m3)", "Cs at grain boundary (at/m3)", "I at grain boundary (at/m3)", "Te at grain boundary (at/m3)"], name="At grain boundary", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "At grain boundary / produced", basetime=time_plot)
radial_profile(["Xe at grain boundary (at/m3)", "Cs at grain boundary (at/m3)", "I at grain boundary (at/m3)", "Te at grain boundary (at/m3)"], name="At grain boundary", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "At grain boundary / produced")
radial_profile(["Xe released (at/m3)", "Cs released (at/m3)", "I released (at/m3)", "Te released (at/m3)"], name="Released", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Released / produced", basetime=time_plot)
radial_profile(["Xe released (at/m3)", "Cs released (at/m3)", "I released (at/m3)", "Te released (at/m3)"], name="Released", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Released / produced")
radial_profile(["Xe reacted - GB (at/m3)", "Cs reacted - GB (at/m3)", "I reacted - GB (at/m3)", "Te reacted - GB (at/m3)"], name="Reacted - GB", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Reacted at GB / produced", basetime=time_plot)
# radial_profile(["Xe reacted - IG (at/m3)", "Cs reacted - IG (at/m3)", "I reacted - IG (at/m3)", "Te reacted - IG (at/m3)"], name="Reacted - IG", results_base=".", totalradius=totalradius,
#                radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Reacted in grain / released", basetime=time_plot)
radial_profile(["Xe reacted - GB (at/m3)", "Cs reacted - GB (at/m3)", "I reacted - GB (at/m3)", "Te reacted - GB (at/m3)"], name="Reacted - GB", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Reacted at GB / produced", basetime=time_plot)
# radial_profile(["Xe reacted - IG (at/m3)", "Cs reacted - IG (at/m3)", "I reacted - IG (at/m3)", "Te reacted - IG (at/m3)"], name="Reacted - IG", results_base=".", totalradius=totalradius,
#                radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Reacted in grain / released")

time_plot = annealing_time + 3
radial_integral_all(results_base=".", totalradius=totalradius,
                    radialprofile=radialprofile,
                    position="at grain boundary")
radial_integral_all(results_base=".", totalradius=totalradius,
                    radialprofile=radialprofile,
                    position="at grain boundary", basetime=time_plot)

radial_integral_all(results_base=".", totalradius=totalradius,
                    radialprofile=radialprofile,
                    position="in the gap")
radial_integral_all(results_base=".", totalradius=totalradius,
                    radialprofile=radialprofile,
                    position="in the gap", basetime=time_plot)

radial_profile("Fission rate (fiss / m3 s)", name="FissRate", results_base=".", totalradius=totalradius, radialprofile=radialprofile, basetime=time_plot)
radial_profile("Temperature (K)", name="Temperature", results_base=".", totalradius=totalradius, radialprofile=radialprofile, labeladd="Temperature (K)",basetime=time_plot)
radial_profile("Fission rate (fiss / m3 s)", name="FissRate", results_base=".", totalradius=totalradius, radialprofile=radialprofile)
radial_profile("Temperature (K)", name="Temperature", results_base=".", totalradius=totalradius, radialprofile=radialprofile, labeladd="Temperature (K)")
radial_profile("Burnup (MWd/kgUO2)", name="Burnup", results_base=".", totalradius=totalradius, radialprofile=radialprofile, basetime=time_plot, labeladd="Burnup (MWd/kgUO2)")
radial_profile(["Xe in grain (at/m3)", "Cs in grain (at/m3)", "I in grain (at/m3)", "Te in grain (at/m3)"], name="In grain", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd="In grain / produced", basetime=time_plot)
radial_profile(["Xe in grain (at/m3)", "Cs in grain (at/m3)", "I in grain (at/m3)", "Te in grain (at/m3)"], name="In grain", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd="In grain / produced")
radial_profile(["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], name="Produced", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=None, mole=True, labeladd="Produced (mol m-3)")
radial_profile(["Xe at grain boundary (at/m3)", "Cs at grain boundary (at/m3)", "I at grain boundary (at/m3)", "Te at grain boundary (at/m3)"], name="At grain boundary", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "At grain boundary / produced", basetime=time_plot)
radial_profile(["Xe at grain boundary (at/m3)", "Cs at grain boundary (at/m3)", "I at grain boundary (at/m3)", "Te at grain boundary (at/m3)"], name="At grain boundary", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "At grain boundary / produced")
radial_profile(["Xe released (at/m3)", "Cs released (at/m3)", "I released (at/m3)", "Te released (at/m3)"], name="Released", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Released / produced", basetime=time_plot)
radial_profile(["Xe released (at/m3)", "Cs released (at/m3)", "I released (at/m3)", "Te released (at/m3)"], name="Released", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Released / produced")
radial_profile(["Xe reacted - GB (at/m3)", "Cs reacted - GB (at/m3)", "I reacted - GB (at/m3)", "Te reacted - GB (at/m3)"], name="Reacted - GB", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Reacted at GB / produced", basetime=time_plot)
# radial_profile(["Xe reacted - IG (at/m3)", "Cs reacted - IG (at/m3)", "I reacted - IG (at/m3)", "Te reacted - IG (at/m3)"], name="Reacted - IG", results_base=".", totalradius=totalradius,
#                radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Reacted in grain / released", basetime=time_plot)
radial_profile(["Xe reacted - GB (at/m3)", "Cs reacted - GB (at/m3)", "I reacted - GB (at/m3)", "Te reacted - GB (at/m3)"], name="Reacted - GB", results_base=".", totalradius=totalradius,
               radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Reacted at GB / produced", basetime=time_plot)
# radial_profile(["Xe reacted - IG (at/m3)", "Cs reacted - IG (at/m3)", "I reacted - IG (at/m3)", "Te reacted - IG (at/m3)"], name="Reacted - IG", results_base=".", totalradius=totalradius,
#                radialprofile=radialprofile, norm=["Xe produced (at/m3)", "Cs produced (at/m3)", "I produced (at/m3)", "Te produced (at/m3)"], labeladd= "Reacted in grain / released")



radialfolder = "RadialPlots"
os.makedirs(radialfolder, exist_ok=True)
for file in os.listdir("."):
    if file.endswith(".png"):
        shutil.move(file, os.path.join(radialfolder, file))

