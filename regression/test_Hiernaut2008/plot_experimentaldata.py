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
    'I129': pd.read_csv('data_notoxidised/I129.txt', sep=';'),
    'Te130': pd.read_csv('data_notoxidised/Te130.txt', sep=';')
}

# Define isotopes and related properties
isotope_data = {
    'Cs137': {'element': 'Cs', 'atomic_mass': 137, 'yield': 6.227860e-02 / 0.15},
    'I129': {'element': 'I', 'atomic_mass': 129, 'yield':7.0611e-3 / (0.04)},
    'Te130': {'element': 'Te', 'atomic_mass': 130, 'yield': 0.025502 / (0.03)}
}

plt.figure(figsize=(15,8))

# Process and plot simulation data
for isotope, properties in isotope_data.items():
    plt.plot(
        additional_data[isotope]['Temperature (K)'],
        additional_data[isotope]['Release quantity (kg/s)'],
        marker='o',
        linewidth=0.5,
        color=colors[list(isotope_data.keys()).index(isotope)],
        label=f'{isotope}'
    )
plt.xlabel('Temperature (K)')
plt.ylabel('Release quantity (kg/s)')
plt.yscale('log')
plt.title('Experimental data')
plt.grid(True)
plt.legend()
plt.tight_layout()

plt.savefig(folder_path + "/Exp_data.png")
plt.show()

def cumtrapz_custom(y, x):
    """Calcola l'integrazione cumulativa dei dati y rispetto a x utilizzando la regola dei trapezi."""
    cum_int = np.zeros(len(y))
    for i in range(1, len(y)):
        dx = x[i] - x[i-1]
        cum_int[i] = cum_int[i-1] + 0.5 * (y[i] + y[i-1]) * dx
    return cum_int

plt.figure(figsize=(8,8))

for isotope, properties in isotope_data.items():
    element = properties['element']

    exp_df = additional_data[isotope].copy()
    time_sec = exp_df['Temperature (K)'].values - exp_df['Temperature (K)'][0]/ (30/60) # 30K/h heating
    
    cumulative_release_exp = cumtrapz_custom(exp_df['Release quantity (kg/s)'].values, time_sec)
    cumulative_release_exp /= cumulative_release_exp[-1]
    
    plt.plot(
        exp_df['Temperature (K)'],
        cumulative_release_exp,
        marker='o',
        linestyle='--',
        linewidth = 0.5,
        color=colors[list(isotope_data.keys()).index(isotope)],
        label=f'{isotope}'
    )

plt.xlabel('Temperature (K)')
plt.ylabel('Cumulative release (/)')
plt.xlim([300, 2800])
plt.ylim([0,1])
handles, labels = plt.gca().get_legend_handles_labels()
legend_markers = [plt.Line2D([0], [0], marker='o', linestyle='--', color='black', label='Hiernaut (2008)' )]
legend_colors = [plt.Line2D([0], [0], marker='o', linestyle='', color=colors[list(isotope_data.keys()).index(isotope)], label=f'{isotope}') for isotope, properties in isotope_data.items()]

plt.legend(handles= legend_colors +legend_markers, loc='upper left', frameon=False)


plt.tight_layout()
plt.savefig(folder_path + "/Exp_data2.png")
plt.show()


plt.figure(figsize=(15,8))

# Process and plot simulation data
from scipy.interpolate import interp1d

# Load experimental data
elements = ['Cs137', 'Te130', 'I129']
data = {elem: pd.read_csv(f'data_notoxidised/{elem}.txt', sep=';') for elem in elements}

# Extract temperature and release values
temp = {elem: data[elem]['Temperature (K)'] for elem in elements}
release = {elem: data[elem]['Release quantity (kg/s)'] for elem in elements}

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
        release_interp[isotope]/release_interp['Cs137'],
        marker='o',
        linewidth=0.5,
        color=colors[list(isotope_data.keys()).index(isotope)],
        label=f'{isotope}'
    )
plt.xlabel('Temperature (K)')
plt.ylabel('Release quantity with respect to Cs (/)')
plt.title('Experimental data')
plt.legend()
plt.ylim([0,1])
plt.tight_layout()

plt.savefig(folder_path + "/Exp_ratio.png")
plt.show()

# Compute numerical derivatives (finite differences)
release_derivative = {elem: np.gradient(release[elem], temp[elem]) for elem in elements}

# Define colors for each element
colors = {'Cs137': 'dodgerblue', 'Te130': 'crimson', 'I129': 'forestgreen'}

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