import matplotlib as mpl
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import glob
import re

folder_path = "."
avogadronumber = 6.02214e23

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

print("thermochemistry columns:", len(thermochemistry_data.columns))
print("data columns:", len(data.columns))


# -------------------------
# 1) Temperature vs Time
# -------------------------
plt.figure(figsize=(10, 6))
plt.plot(data['Time (h)'], data['Temperature (K)'], color='blue')
plt.xlabel('Time (h)')
plt.ylabel('Temperature (K)')
plt.title('Temperature history')
plt.yscale('linear')
plt.tight_layout()
plt.savefig(folder_path + '/temperature_plot.png')
plt.close()


# -------------------------
# 2) Stoichiometry vs Time
# -------------------------
plt.figure(figsize=(10, 6))
plt.plot(data['Time (h)'], data['Stoichiometry deviation (/)'], color='blue')
plt.xlabel('Time (h)')
plt.ylabel('Stoichiometry deviation (–)')
plt.title('Stoichiometry deviation')
plt.yscale('linear')
plt.tight_layout()
plt.savefig(folder_path + '/stoichiometry_plot.png')
plt.close()


# ---------------------------------------
# 3) Atomic densities (U/Pu isotopes) vs T
#    (automatico: prende tutte le colonne tipo U235 (at/m3), Pu239 (at/m3), ...)
# ---------------------------------------
iso_cols = [c for c in data.columns if re.match(r'^(U\d+|Pu\d+)\s+\(at/m3\)$', c)]
iso_cols = [c for c in iso_cols if c != 'U233 (at/m3)']  # opzionale se non esiste/ti confonde

plt.figure(figsize=(10, 6))
for c in iso_cols:
    label = c.split()[0]  # "U235"
    plt.plot(data['Temperature (K)'], data[c], label=label)
plt.xlabel('Temperature (K)')
plt.ylabel('Atomic density (at/m³)')
plt.title('U/Pu isotopes atomic density')
plt.yscale('log')
plt.legend(frameon=False)
plt.tight_layout()
plt.savefig(folder_path + '/atom_density_plot.png')
plt.close()


# ---------------------------------------
# 4) Element contents vs Temperature
# ---------------------------------------
plt.figure(figsize=(10, 6))
plt.plot(data['Temperature (K)'], data['Plutonium content (mol/m3)'], label='Plutonium', color='red')
plt.plot(data['Temperature (K)'], data['Uranium content (mol/m3)'], label='Uranium', color='green')
plt.plot(data['Temperature (K)'], data['Oxygen content (mol/m3)'], label='Oxygen', color='purple')
plt.xlabel('Temperature (K)')
plt.ylabel('Concentration (mol/m³)')
plt.title('Element contents')
plt.legend(frameon=False)
plt.tight_layout()
plt.savefig(folder_path + '/content_plot.png')
plt.close()


# -------------------------------------------------
# 5) MATRIX: solid species vs Temperature (mol/m3)
#    (automatico: tutte le colonne "(solid, matrix) (mol/m3)" con max>0)
# -------------------------------------------------
solid_matrix_cols = [c for c in thermochemistry_data.columns
                     if '(solid, matrix)' in c and c.endswith('(mol/m3)') and not c.startswith('Unnamed')]

plt.figure(figsize=(10, 6))
n_plotted = 0
for c in solid_matrix_cols:
    y = thermochemistry_data[c].to_numpy(dtype=float)
    if np.nanmax(y) <= 0:
        continue
    label = c.replace(' (solid, matrix) (mol/m3)', '')
    plt.plot(thermochemistry_data['Temperature (K)'], y, label=label)
    n_plotted += 1

plt.xlabel('Temperature (K)')
plt.ylabel('Concentration (mol/m³)')
plt.title('Matrix – solid species')
if n_plotted > 1:
    plt.legend(frameon=False)
plt.yscale('log')
plt.ylim(bottom=1e-5)
plt.tight_layout()
plt.savefig(folder_path + '/matrix_solid_species.png')
plt.close()


# -------------------------------------------------
# 6) MATRIX: vapour species vs Temperature (mol/m3)
#    (automatico: tutte "(vapour, matrix) (mol/m3)" con max>0)
# -------------------------------------------------
vapour_matrix_cols = [c for c in thermochemistry_data.columns
                      if '(vapour, matrix)' in c and c.endswith('(mol/m3)') and not c.startswith('Unnamed')]

plt.figure(figsize=(10, 6))
n_plotted = 0
for c in vapour_matrix_cols:
    y = thermochemistry_data[c].to_numpy(dtype=float)
    if np.nanmax(y) <= 0:
        continue
    label = c.replace(' (vapour, matrix) (mol/m3)', '')
    plt.plot(thermochemistry_data['Temperature (K)'], y, label=label)
    n_plotted += 1

plt.xlabel('Temperature (K)')
plt.ylabel('Concentration (mol/m³)')
plt.title('Matrix – vapour species')
if n_plotted > 1:
    plt.legend(frameon=False)
plt.yscale('log')
plt.ylim(bottom=1e-5)
plt.tight_layout()
plt.savefig(folder_path + '/matrix_vapour_species.png')
plt.close()


# -------------------------------------------------
# 7) MATRIX: gas species vs Temperature (mol/m3)
#    (automatico: tutte "(gas, matrix) (mol/m3)" con max>0)
# -------------------------------------------------
gas_matrix_cols = [c for c in thermochemistry_data.columns
                   if '(gas, matrix)' in c and c.endswith('(mol/m3)') and not c.startswith('Unnamed')]

plt.figure(figsize=(10, 6))
n_plotted = 0
for c in gas_matrix_cols:
    y = thermochemistry_data[c].to_numpy(dtype=float)
    if np.nanmax(y) <= 0:
        continue
    label = c.replace(' (gas, matrix) (mol/m3)', '')
    plt.plot(thermochemistry_data['Temperature (K)'], y, label=label)
    n_plotted += 1

plt.xlabel('Temperature (K)')
plt.ylabel('Concentration (mol/m³)')
plt.title('Matrix – gas species')
if n_plotted > 1:
    plt.legend(frameon=False)
plt.yscale('log')
plt.ylim(bottom=1e-5)
plt.tight_layout()
plt.savefig(folder_path + '/matrix_gas_species.png')
plt.close()


# -------------------------------------------------
# 8) FP: gas species in grain / GB / gap vs Temperature
#    (automatico: tutte le colonne "(gas, <pos>) (mol/m3)" con max>0)
# -------------------------------------------------
positions_fp = ['at grain boundary', 'in the gap']

for pos in positions_fp:
    cols = [c for c in thermochemistry_data.columns
            if f'(gas, {pos})' in c and c.endswith('(mol/m3)') and not c.startswith('Unnamed')]

    plt.figure(figsize=(10, 6))
    n_plotted = 0
    for c in cols:
        y = thermochemistry_data[c].to_numpy(dtype=float)
        if np.nanmax(y) <= 0:
            continue
        label = c.replace(f' (gas, {pos}) (mol/m3)', '')
        plt.plot(thermochemistry_data['Temperature (K)'], y, label=label)
        n_plotted += 1

    plt.xlabel('Temperature (K)')
    plt.ylabel('Concentration (mol/m³)')
    plt.title(f'Gas species – {pos}')
    if n_plotted > 1:
        plt.legend(frameon=False, ncol=2)  # ncol=2 per stare un po' più stretta
    plt.yscale('log')
    plt.ylim(bottom=1e-5)
    plt.tight_layout()
    plt.savefig(folder_path + f'/gas_species_{pos.replace(" ", "_")}.png')
    plt.close()

# -------------------------------------------------
# 9) Grain radius vs Temperature
# -------------------------------------------------
plt.figure(figsize=(10, 6))
plt.plot(data['Temperature (K)'], data['Grain radius (m)'], color='blue')
plt.xlabel('Temperature (K)')
plt.ylabel('Grain radius (m)')
plt.title('Grain radius vs temperature')
plt.yscale('linear')
plt.tight_layout()
plt.savefig(folder_path + '/grain_radius_vs_temperature.png')
plt.close()

# -------------------------------------------------
# 10) Validation: vaporisation of matrix vs vaporisation of Np
# -------------------------------------------------
import io

# experimental data Nd from Colle2023
np_data_str = """Temperature (K);Normalized release (/)
1532.8812195861742;0.0
1708.7769850584868;0.014630173498852908
1767.408906882591;0.019506897998470545
1846.567935750175;0.022798933833511024
1922.7894341215103;0.025968804758262486
2057.6428543169504;0.03157703793282279
2183.6896174286885;0.04730299365729769
2242.3156048477495;0.057055455465338384
2286.2598741906136;0.07350329475061217
2324.3290825409804;0.0988346207951834
2344.802779939074;0.1192694785162529
2382.8245130490977;0.18093758483674457
2406.2238721333533;0.2276475233840931
2414.9533819514963;0.24906685752363147
2432.4717456382123;0.30079370172018055
2447.040710018595;0.35290012093092255
2464.51753287001;0.4292161701917617
2484.8784765722876;0.5409889187788436
2499.44744095267;0.630054542313484
2516.930198209129;0.7500204842172822
2525.6893800524867;0.8123112613835489
2543.290825409804;0.9360196451047658
2546.299568766567;0.9555393765887611
2546.3885848422105;0.9560613539327207
2549.403262604017;0.9719837607048543
2552.495087631381;0.9848419260100186
2555.551306228488;0.9948828944445816
2558.892376267655;0.9962723660504947
2561.693415447916;1.0"""

df_np = pd.read_csv(io.StringIO(np_data_str), sep=';')
temp_np = df_np['Temperature (K)']
release_np = df_np['Normalized release (/)']

# matrix data
temp_k = thermochemistry_data['Temperature (K)']
vapour_cols = [c for c in thermochemistry_data.columns if '(vapour, matrix)' in c and 'mol/m3' in c]

total_conc_matrix = thermochemistry_data[vapour_cols].sum(axis=1)

max_conc = total_conc_matrix.max()
if max_conc > 0:
    release_sciantix = total_conc_matrix / max_conc
else:
    release_sciantix = total_conc_matrix * 0 

# graph 
plt.figure(figsize=(10, 8))

plt.plot(temp_k, release_sciantix, label='SCIANTIX (Release U+Pu)', color='blue', linewidth=3)
plt.plot(temp_np, release_np, label='Nd', color='red', linestyle='--', linewidth=3, marker='o', markersize=4)

plt.xlabel('Temperature (K)')
plt.ylabel('Normalized release (-)')
plt.title('Matrix vs Nd')
plt.legend(frameon=False)
plt.grid(True, which="both", ls="-", alpha=0.3)
plt.xlim(left=1000, right=3000)
plt.ylim(bottom=0, top=1.05)
plt.tight_layout()

plt.savefig(folder_path + '/validation_matrix_vap.png')
plt.close()
