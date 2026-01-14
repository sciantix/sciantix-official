import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import pandas as pd
import numpy as np
import os
import glob

folder_path = "data_notoxidised"
avogadronumber = 6.02214e23

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

plt.figure(figsize=(8,8))

for isotope, properties in isotope_data.items():
    element = properties['element']

    plt.plot(
        additional_data[element]['Temperature (K)'],
        additional_data[element]['Release/birth (/)'],
        marker='o',
        linestyle='--',
        linewidth=0.5,
        color=colors[list(isotope_data.keys()).index(isotope)],
        label=f'Exp. {isotope}'
    )

#plt.vlines(x = 2020, ymin = 0, ymax = 1, color = 'tab:red', linestyle = ':', label = 'UO2 Vaporisation onset')

plt.xlabel('Temperature (K)')
plt.ylabel('Cumulative release (/)')
plt.title('Experimental data, Colle (2005)')

handles, labels = plt.gca().get_legend_handles_labels()
legend_markers = [plt.Line2D([0], [0], marker='o', linestyle='--', color='black', label='Colle (2005)' )]
legend_colors = [plt.Line2D([0], [0], marker='o', linestyle='', color=colors[list(isotope_data.keys()).index(isotope)], label=f'Experimental {isotope}') for isotope, properties in isotope_data.items()]

plt.legend(handles= legend_colors, loc='upper left', frameon=False)


plt.tight_layout()
plt.savefig(folder_path + "/Exp_data.png")
plt.show()



from scipy.interpolate import interp1d

# Load experimental data
elements = ['Cs', 'Te', 'I', 'Xe']
data = {elem: pd.read_csv(f'data_notoxidised/{elem}.txt', sep=';') for elem in elements}

# Extract temperature and release values
temp = {elem: data[elem]['Temperature (K)'] for elem in elements}
release = {elem: data[elem]['Release/birth (/)'] for elem in elements}

# Interpolation functions
interp = {elem: interp1d(temp[elem], release[elem], kind='linear', fill_value='extrapolate') for elem in elements}

# Define a common temperature range
temp_common = np.linspace(
    max(min(temp[elem]) for elem in elements), 
    min(max(temp[elem]) for elem in elements), 
    100
)

# Compute interpolated values
release_interp = {elem: interp[elem](temp_common) for elem in elements}

for isotope, properties in isotope_data.items():
    plt.plot(
        temp_common,
        release_interp[isotope]/release_interp['Xe'],
        color=colors[list(isotope_data.keys()).index(isotope)],
        label=f'{isotope}'
    )
plt.xlabel('Temperature (K)')
plt.ylabel('Release quantity with respect to Xe (/)')
plt.title('Experimental data, Colle (2005)')
plt.legend()
plt.ylim([0,3])
plt.tight_layout()

plt.savefig(folder_path + "/Exp_ratio.png")
plt.show()

yields= {
    'Cs': 0.15,
    'I': 0.04,
    'Xe': 0.24,
    'Te': 0.03
}

for isotope, properties in isotope_data.items():
    plt.plot(
        temp_common,
        yields[isotope]*release_interp[isotope]/release_interp['Cs'],
        color=colors[list(isotope_data.keys()).index(isotope)],
        label=f'{isotope}'
    )
plt.xlabel('Temperature (K)')
plt.ylabel('Release quantity with respect to Xe (/)')
plt.title('Experimental data, Colle (2005)')
plt.legend()
plt.tight_layout()

plt.savefig(folder_path + "/Exp_ratio2.png")
plt.show()

# Compute numerical derivatives (finite differences)
release_derivative = {elem: np.gradient(release[elem], temp[elem]) for elem in elements}

# Define colors for each element
colors = {'Cs': 'dodgerblue', 'Te': 'crimson', 'I': 'forestgreen', 'Xe': 'darkorange'}

# Plot derivatives
plt.figure(figsize=(8, 8))
for elem in elements:
    plt.plot(temp[elem], release_derivative[elem], label=f'd(Release)/dT {elem}', color=colors[elem])

plt.xlabel('Temperature (K)')
plt.ylabel('Release Rate (1/K)')
plt.title('Release Kinetics (Derivative of Release w.r.t. Temperature)')
plt.legend()
plt.grid(True)
plt.tight_layout()

# Save and show the plot
plt.savefig(folder_path + "/Exp_derivative.png")
plt.show()




# Costante di Boltzmann in eV/K
k_B = 1.3806503e-23 

# Intervallo di temperature (in K) per il plot
T_min = 300
T_max = 2000
n_points = 500
T = np.linspace(T_min, T_max, n_points)

# Definizione dei processi e dei relativi parametri (k0 in questa notazione)
processes = {
    "g.b. diffusion": {
        "k0": 5.2e5,
        "H":35190
    },
    "volume diffusion": {
        "k0": 6.0e4 + 2.58e4,
        "H": 37650
    },
    "vaporization": {
        "k0": 8.9e10,
        "H": 70450
    }
}

# Calcolo e plot su scala semilogaritmica
plt.figure(figsize=(8,6))

for label, params in processes.items():
    k0 = params["k0"]
    H  = params["H"]
    
    # Formula di Arrhenius: k(T) = k0 * exp(-H / (kB * T))
    kT = k0 * np.exp(-H / (T))
    
    # Plot su scala semilogaritmica: k vs T
    plt.semilogy(T, kT, label=label)

plt.xlabel("Temperatura (K)")
plt.ylabel("k(T) [scala log]")
plt.title("Plot di Arrhenius su scala semilogaritmica")
plt.legend()
plt.grid(True)
plt.show()
