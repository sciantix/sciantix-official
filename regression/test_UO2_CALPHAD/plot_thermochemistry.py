import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import numpy as np
import os
import glob
import re
from collections import defaultdict

def plot(folder_path):

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
    plt.figure( figsize=(10, 10))
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
    plt.figure( figsize=(10, 10))
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
    plt.figure( figsize=(10, 10))
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
    plt.figure(figsize=(10, 10))
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
    plt.figure(figsize=(15, 10))
    dataset = "With Thermochemistry"
    label_x = "Temperature (K)"
    location = 'matrix'
    total_moles = data[dataset]["Uranium content (mol/m3)"] + data[dataset]["Oxygen content (mol/m3)"]

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
    plt.ylabel("Normalized moles fraction")
    plt.legend(loc='center left', bbox_to_anchor=(1.01, 0.5), frameon=False)
    plt.title(f"Oxygen mole fraction: {OMoleFraction:.3f}")
    plt.tight_layout()
    plt.savefig(folder_path + "/MATRIX.png")
    #plt.show()
    return

