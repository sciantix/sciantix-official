import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from scipy.optimize import brentq

# Python from Colab (with brentq method)

# Calculation of po2 from Kato's equation (NEA2024) pag. 206
def calculate_om(log10_po2, T_K, C_pu, R=8.314):
    po2 = 10**log10_po2
    ln_po2 = np.log(po2)
    log_v1 = -5 * ((44.0 + 55.8 * C_pu) / R - 376000 / (R * T_K) - 0.5 * ln_po2)
    log_v2 = -5 * (0.5 * (68.8 + 131.3 * C_pu) / R - 0.5 * 515000 / (R * T_K) - 0.25 * ln_po2)
    log_v3 = -5 * (((1/3) * (np.log(2) + (153.5 - 96.5 * C_pu + 331.0 * C_pu**2) / R - 891000 / (R * T_K))) - (1/3) * ln_po2)
    log_v4 = -5 * np.log(0.5 * C_pu)

    max_log = np.maximum.reduce([log_v1, log_v2, log_v3, log_v4])
    sum_exp = np.exp(log_v1 - max_log) + np.exp(log_v2 - max_log) + np.exp(log_v3 - max_log) + np.exp(log_v4 - max_log)
    S = np.exp(-1/5 * (max_log + np.log(sum_exp)))
    term5 = np.exp((-22.8 - 84.5 * C_pu) / R + 105000 / (R * T_K) + 0.5 * ln_po2)

    return 2 - S + term5

def solve_for_po2(target_om, T_K, C_pu, R=8.314):
    def objective(log_po2):
        return calculate_om(log_po2, T_K, C_pu, R) - target_om
    try:
        sol_log = brentq(objective, -80, 10)
        return 10**sol_log
    except ValueError:
        return None

# Calculation of concentrations

def get_equilibrium_constants(T_K):
    """
    constants from Olander
    """
    # ln K_u24 = -((78.3*10^3)/temperature) + 13.6
    ln_k_u24 = -(78.3e3 / T_K) + 13.6
    K_u24 = np.exp(ln_k_u24)

    # ln K_u46 = -((16.4*10^3)/temperature) + 5
    ln_k_u46 = -(16.4e3 / T_K) + 5.0
    K_u46 = np.exp(ln_k_u46)

    # ln K_pu34 = -((50.1*10^3)/temperature) + 10.3
    ln_k_pu34 = -(50.1e3 / T_K) + 10.3
    K_pu34 = np.exp(ln_k_pu34)

    # ln K_pu23 = -((92.5*10^3)/temperature) + 21.3
    ln_k_pu23 = -(92.5e3 / T_K) + 21.3
    K_pu23 = np.exp(ln_k_pu23)

    return K_u24, K_u46, K_pu34, K_pu23

def solve_species_concentrations(po2, T_K, C_pu, n_o=2.0):
    K_u24, K_u46, K_pu34, K_pu23 = get_equilibrium_constants(T_K)


    sqrt_po2 = np.sqrt(po2)

    # U
    denom_u = 1 + ((K_u24 * n_o) / sqrt_po2) + (sqrt_po2 / (K_u46 * n_o))

    n_u4 = (1 - C_pu) / denom_u
    n_u2 = ((K_u24 * n_o) / sqrt_po2) * n_u4
    n_u6 = (sqrt_po2 / (K_u46 * n_o)) * n_u4

    # Pu
    ratio_34 = np.sqrt((K_pu34 * n_o) / sqrt_po2)

    ratio_23 = np.sqrt((K_pu23 * n_o) / sqrt_po2)

    denom_pu = 1 + ratio_34 + (ratio_23 * ratio_34)

    n_pu4 = C_pu / denom_pu
    n_pu3 = ratio_34 * n_pu4
    n_pu2 = ratio_23 * n_pu3

    return {
        "U4": n_u4, "U2": n_u2, "U6": n_u6,
        "Pu4": n_pu4, "Pu3": n_pu3, "Pu2": n_pu2
    }

# Calculation of partial pressures

def get_vap_constants(T_K): # from Olander pag 158 tab 11.1
    K_UO  = np.exp(-49.5e3 / T_K + 11.9)
    K_UO2 = np.exp(-74.0e3 / T_K + 19.9)
    K_UO3 = np.exp(-44.0e3 / T_K + 11.9)

    K_PuO  = np.exp(-44.1e3 / T_K + 11.5)
    K_PuO2 = np.exp(-72.5e3 / T_K + 18.8)

    return K_UO, K_UO2, K_UO3, K_PuO, K_PuO2

def solve_partial_pressures(T_K, species):
    K_UO, K_UO2, K_UO3, K_PuO, K_PuO2 = get_vap_constants(T_K)

    n_u2 = species['U2']
    n_u4 = species['U4']
    n_u6 = species['U6']
    n_pu2 = species['Pu2']
    n_pu4 = species['Pu4']

    pUO = 2 * K_UO * n_u2
    pUO2 = 4 * K_UO2 * n_u4
    pUO3 = 8 * K_UO3 * n_u6

    pPuO = 2 * K_PuO * n_pu2
    pPuO2 = 4 * K_PuO2 * n_pu4

    return {
        "p_UO": pUO, "p_UO2": pUO2, "p_UO3": pUO3,
        "p_PuO": pPuO, "p_PuO2": pPuO2
    }

# Plotting po2

# Input parameters
pu_content = 0.125  # decimal Pu content over metal quantity
temps_kelvin = [1373, 1573, 1773, 2000]  # Temperatures in Kelvin
target_om = np.linspace(1.84, 2.12, 500)
R_const = 8.314

results_db = {T: {'om': [], 'po2': [], 'species': [], 'vap_pressures': []} for T in temps_kelvin}

for T in temps_kelvin:
    valid_om_list = []
    valid_po2_list = []
    species_list = []
    vap_press_list = []

    for om in target_om:
        po2_val = solve_for_po2(om, T, pu_content, R=R_const)

        if po2_val is not None:
            concs = solve_species_concentrations(po2_val, T, pu_content, n_o=2.0)

            vap_press = solve_partial_pressures(T, concs)

            valid_om_list.append(om)
            valid_po2_list.append(po2_val)
            species_list.append(concs)
            vap_press_list.append(vap_press)

    results_db[T]['om'] = np.array(valid_om_list)
    results_db[T]['po2'] = np.array(valid_po2_list)
    results_db[T]['species'] = species_list
    results_db[T]['vap_pressures'] = vap_press_list

plt.figure(figsize=(10, 7))

for T in temps_kelvin:

    raw_results = [solve_for_po2(om, T, pu_content, R=R_const) for om in target_om]
    raw_results = np.array(raw_results, dtype=object)
    mask = raw_results != None

    valid_om = target_om[mask]
    valid_po2 = raw_results[mask].astype(float)

    if len(valid_po2) > 0:
        # Oxygen Potential = R * T * ln(pO2) in kJ/mol
        oxygen_potential = (R_const * T * np.log(valid_po2)) / 1000

        plt.plot(valid_om, oxygen_potential, label=f'T = {T} K')
    else:
        print(f"Warning: No valid pO2 solutions found for T = {T} K")

plt.axvline(x=2.0, color='black', linestyle='--', alpha=0.3, label='Stoichiometric (O/M=2.0)')
plt.xlabel('Oxygen-to-Metal Ratio (O/M)', fontsize=12)
plt.ylabel('$\Delta \overline{G}_{O_2}$ (kJ/mol)', fontsize=12)
plt.grid(True, linestyle=':', alpha=0.6)
plt.legend()
plt.tight_layout()

plt.figure(figsize=(10, 7))

for T in temps_kelvin:

    raw_results = [solve_for_po2(om, T, pu_content, R=R_const) for om in target_om]
    raw_results = np.array(raw_results, dtype=object)
    mask = raw_results != None

    valid_om = target_om[mask]
    valid_po2 = raw_results[mask].astype(float)

    if len(valid_po2) > 0:
        plt.plot(valid_om, valid_po2, label=f'T = {T} K')

plt.axvline(x=2.0, color='black', linestyle='--', alpha=0.3, label='Stoichiometric (O/M=2.0)')
plt.xlabel('Oxygen-to-Metal Ratio (O/M)', fontsize=12)
plt.ylabel('$p_{O_2}$', fontsize=12)
plt.grid(True, linestyle=':', alpha=0.6)
plt.yscale('log')
plt.legend()
plt.tight_layout()

# Plotting concentrations of U and Pu
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))
temp_to_plot = 1773

if temp_to_plot in results_db:
    data = results_db[temp_to_plot]
    x = data['om']

    n_u4 = [s['U4'] for s in data['species']]
    n_u2 = [s['U2'] for s in data['species']]
    n_u6 = [s['U6'] for s in data['species']]

    n_pu4 = [s['Pu4'] for s in data['species']]
    n_pu3 = [s['Pu3'] for s in data['species']]
    n_pu2 = [s['Pu2'] for s in data['species']]

    # U
    ax1.plot(x, n_u4, label='$U^{4+}$', color='blue')
    ax1.plot(x, n_u6, label='$U^{6+}$', color='green', linestyle='--')
    ax1.plot(x, n_u2, label='$U^{2+}$', color='red', linestyle='-.')
    ax1.set_title(f'U (T={temp_to_plot} K)')
    ax1.set_xlabel('Oxygen-to-Metal Ratio (O/M)')
    ax1.set_ylabel('Concentrations')
    ax1.set_yscale('log')
    ax1.set_ylim(1e-9, 1)
    ax1.grid(True, which="both", alpha=0.3)
    ax1.legend()

    # Pu
    ax2.plot(x, n_pu4, label='$Pu^{4+}$', color='purple')
    ax2.plot(x, n_pu3, label='$Pu^{3+}$', color='orange', linestyle='--')
    ax2.plot(x, n_pu2, label='$Pu^{2+}$', color='brown', linestyle='-.')
    ax2.set_title(f'Pu (T={temp_to_plot} K)')
    ax2.set_xlabel('Oxygen-to-Metal Ratio (O/M)')
    ax2.set_ylabel('Concentrations')
    ax2.set_yscale('log')
    ax2.set_ylim(1e-9, 1)
    ax2.grid(True, which="both", alpha=0.3)
    ax2.legend()

plt.tight_layout()
plt.show()

# Plotting partial vapour pressures 
# fig 11.17 pag 160 Olander 

temp_vap_plot = 2000

if temp_vap_plot in results_db:
    data = results_db[temp_vap_plot]
    x = data['om']


    p_UO  = [v['p_UO'] for v in data['vap_pressures']]
    p_UO2 = [v['p_UO2'] for v in data['vap_pressures']]
    p_UO3 = [v['p_UO3'] for v in data['vap_pressures']]
    p_PuO = [v['p_PuO'] for v in data['vap_pressures']]
    p_PuO2 = [v['p_PuO2'] for v in data['vap_pressures']]

    plt.figure(figsize=(10, 7))

    # U
    plt.plot(x, p_UO, label='$p_{UO}$', color='blue')
    plt.plot(x, p_UO2, label='$p_{UO_2}$', color='green')
    plt.plot(x, p_UO3, label='$p_{UO_3}$', color='cyan')

    # Pu
    plt.plot(x, p_PuO, label='$p_{PuO}$', color='red', linestyle='--')
    plt.plot(x, p_PuO2, label='$p_{PuO_2}$', color='orange', linestyle='--')

    plt.title(f'Partial Vapor Pressures at T = {temp_vap_plot} K')
    plt.xlabel('Oxygen-to-Metal Ratio (O/M)')
    plt.ylabel('Partial Pressure (atm)')
    plt.yscale('log')
    plt.ylim(1e-15, 1e-4)
    plt.grid(True, which="both", alpha=0.3)
    plt.legend(loc='best')
    plt.tight_layout()
    plt.show()





# =============================================================================
# RAMP CASE ANALYSIS: P vs T
# =============================================================================
# --- 1. DEFINIZIONE VARIABILI
val_q = 0.244       # Contenuto di Plutonio
val_om_fix = 1.98   # Rapporto O/M fisso
print("\n--- Generating Ramp Analysis Plot (Validation) ---")

df = pd.read_csv("vapor_results.csv")
df.columns = df.columns.str.strip()

plt.figure(figsize=(10, 7))

# Specie da plottare
species_to_plot = {
    'p_O2': 'black',
    'p_UO': 'blue',    # Monossido di Uranio
    'p_UO2': 'green',   # Diossido di Uranio
    'p_UO3': 'cyan',    # Triossido di Uranio
    'p_PuO': 'red',     # Monossido di Plutonio
    'p_PuO2': 'orange'  # Diossido di Plutonio
}

# Usiamo df['T'] come asse X invece di df['Time']
for sp, col in species_to_plot.items():
    if sp in df.columns:
        plt.plot(df['T'], df[sp], label=sp, color=col, linewidth=2)

plt.yscale('log') # Fondamentale: le pressioni variano di ordini di grandezza
plt.xlabel('Temperature (K)', fontsize=12)
plt.ylabel('Partial Pressure (atm)', fontsize=12)
plt.title(f'Vaporization Ramp: Partial Pressures vs Temperature\n(q={val_q}, O/M={val_om_fix})')
plt.grid(True, which="both", linestyle=':', alpha=0.6)
plt.legend()

plt.tight_layout()
plt.show()