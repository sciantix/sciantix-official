import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import numpy as np
import os
import glob
import re
from collections import defaultdict

folder_path = "."
avogadronumber = 6.02214e23
xlim_i = 12000.0

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
data = pd.read_csv(folder_path + '/output.txt', sep='\t')
thermochemistry_data = pd.read_csv(folder_path + '/thermochemistry_output.txt', sep='\t')


#Plotting

#Plotting
plt.figure(figsize=(10, 6))
plt.plot(data['Time (h)'], data['Temperature (K)'], color='blue')
plt.xlabel('Time (h)')
plt.ylabel('Temperature (K)')
plt.yscale('linear')
plt.legend()
plt.tight_layout()
plt.savefig(folder_path + '/temperature_plot.png')


plt.figure(figsize=(10, 6))
plt.plot(data['Time (h)'], data['Stoichiometry deviation (/)'], color='blue')
plt.xlabel('Time (h)')
plt.ylabel('Stoichiometry deviation (/)')
plt.yscale('linear')
plt.legend()
plt.tight_layout()
plt.savefig(folder_path + '/stoichiometry_plot.png')
plt.figure(figsize=(10, 6))
plt.plot(data['Temperature (K)'], data['U235 (at/m3)'], label='U235', color='red')
plt.plot(data['Temperature (K)'], data['U238 (at/m3)'], label='U238', color='green')
plt.plot(data['Temperature (K)'], data['Pu239 (at/m3)'], label='Pu239', color='purple')
plt.plot(data['Temperature (K)'], data['Pu241 (at/m3)'], label='Pu241', color='orange')
plt.xlabel('Temperature (K)')
plt.ylabel('-')
plt.title('at/m3 of U233, U235, U238, Pu239, and Pu241')
plt.yscale('log')
plt.legend()
plt.tight_layout()
plt.savefig(folder_path + '/atom_density_plot.png')

plt.figure(figsize=(10, 6))
plt.plot(data['Temperature (K)'], data['Plutonium content (mol/m3)'], label='Plutonium', color='red')
plt.plot(data['Temperature (K)'], data['Uranium content (mol/m3)'], label='Uranium', color='green')
plt.plot(data['Temperature (K)'], data['Oxygen content (mol/m3)'], label='Oxygen', color='purple')
plt.xlabel('Temperature (K)')
plt.ylabel('-')
plt.title('at/m3 of Plutonium, Uranium, and Oxygen')
plt.legend()
plt.tight_layout()
plt.savefig(folder_path + '/content_plot.png')

print(thermochemistry_data.columns)
print(data['Plutonium content (mol/m3)'])
print(data['Uranium content (mol/m3)'])
print(data['Oxygen content (mol/m3)'])

plt.figure(figsize=(10, 6))
plt.plot(thermochemistry_data['Temperature (K)'], thermochemistry_data['UO (solid, matrix) (mol/m3)'], label='UO', color='blue')
plt.plot(thermochemistry_data['Temperature (K)'], thermochemistry_data['UO2 (solid, matrix) (mol/m3)'], label='UO2', color='orange')
plt.plot(thermochemistry_data['Temperature (K)'], thermochemistry_data['UO3 (solid, matrix) (mol/m3)'], label='UO3', color='green')
plt.plot(thermochemistry_data['Temperature (K)'], thermochemistry_data['PuO (solid, matrix) (mol/m3)'], label='PuO', color='red')
plt.plot(thermochemistry_data['Temperature (K)'], thermochemistry_data['PuO2 (solid, matrix) (mol/m3)'], label='PuO2', color='purple')
plt.xlabel('Temperature (K)')
plt.ylabel('-')
plt.title('Solid Pressure of UO, UO2, UO3, PuO, and PuO2')
plt.yscale('log')
plt.legend()
plt.tight_layout()
plt.savefig(folder_path + '/solid_pressure_plot.png')

# Plotting
plt.figure(figsize=(10, 6))
plt.plot(thermochemistry_data['Temperature (K)'], thermochemistry_data['UO (vapour, matrix) (mol/m3)'], label='UO', color='blue')
plt.plot(thermochemistry_data['Temperature (K)'], thermochemistry_data['UO2 (vapour, matrix) (mol/m3)'], label='UO2', color='orange')
plt.plot(thermochemistry_data['Temperature (K)'], thermochemistry_data['UO3 (vapour, matrix) (mol/m3)'], label='UO3', color='green')
plt.plot(thermochemistry_data['Temperature (K)'], thermochemistry_data['PuO (vapour, matrix) (mol/m3)'], label='PuO', color='red')
plt.plot(thermochemistry_data['Temperature (K)'], thermochemistry_data['PuO2 (vapour, matrix) (mol/m3)'], label='PuO2', color='purple')
plt.xlabel('Temperature (K)')
plt.ylabel('-')
plt.title('Vapor Pressure of UO, UO2, UO3, PuO, and PuO2')
plt.legend()
plt.tight_layout()
plt.savefig(folder_path + '/vapor_pressure_plot.png')