# Ref. Zullo G. et al (2022). Nuclear Engineering and Technology, 54, 1195-1205, https://www.sciencedirect.com/science/article/pii/S1738573321006148

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Constant conditions table data
constant_data = {
    'µ': [0, 10, 10**2, 10**3, 10**4, 10**5, 10**6, 10**7, 10**8, '>10**8'],
    'A': [-0.5028, -0.5643, -0.6998, -0.8979, -0.9764, -0.9798, -0.9797, -0.9797, -0.9797, -0.9797],
    'B': [-0.2662, -0.1951, -0.0498, 0.2502, 0.3470, 0.3367, 0.3343, 0.3340, 0.3340, 0.3340],
    'C': [-0.0397, -0.7793, -0.5863, -0.2448, -0.1205, -0.0841, -0.0731, -0.0702, -0.0698, -0.0698],
    'D': [-2.8320, -0.2846, -0.2571, -0.5961, -0.7202, -0.7575, -0.7687, -0.7716, -0.7720, -0.7720],
    'E': [1.4450, -0.4304, -0.4280, -0.4140, -0.4080, -0.4062, -0.4057, -0.4056, -0.4055, -0.4055]
}

# Time-varying conditions table data
varying_data = {
    'µ_range': ['[0, 10]', '[0, 10^2]', '[0, 10^3]', '[0, 10^4]', '[0, 10^5]', '[0, 10^6]', '[0, 10^7]', '[0, 10^8]', '[0, 10^9]', '[0, 10^{10}]', '[0, 10^{20}]', '[0, 10^{30}]', '[0, 10^{40}]'],
    'A': [-0.5633, -0.4725, -0.4705, -0.4748, -0.4748, -0.4716, -0.4995, -0.4760, -0.4964, -0.4912, -0.4874, -0.4889, -0.4716],
    'B': [-0.1979, -0.1248, -0.0781, -0.0602, -0.0602, -0.0680, 0.0052, -0.0540, -0.0015, -0.0143, -0.0227, -0.0237, -0.0581],
    'C': [-0.1460, -1.1640, -0.5503, -0.1693, -0.1693, -0.0283, -0.0218, -0.0192, -0.0130, -0.0130, -0.0130, -0.0130, -0.0130],
    'D': [-2.4400, 1.5660, 0.3801, -0.5628, -0.5628, -0.9075, -0.9272, -0.9300, -0.9450, -0.9450, -0.9450, -0.9450, -0.9450],
    'E': [1.3840, -1.8260, -1.1650, -0.5322, -0.5322, -0.3017, -0.2885, -0.2867, -0.3219, -0.3219, -0.3219, -0.3219, -0.3219]
}

# Create the DataFrames for both tables
df_constant = pd.DataFrame(constant_data)
df_varying = pd.DataFrame(varying_data)

# Epsilon_hat function to return term1, term2, and their sum
def epsilon_hat(N_delta_tau, N_M, A, B, C, D, E):
    # Compute log10(N_delta_tau) and log10(N_M)
    log_N_delta_tau = np.log10(N_delta_tau)
    log_N_M = np.log10(N_M)
    
    # First term: 10^(A * log10(N_delta_tau) + B)
    term1 = 10 ** (A * log_N_delta_tau + B)
    
    # Second term: 10^((C * log10(N_M))^2 + D * log10(N_M) + E)
    term2 = 10 ** ((C * log_N_M) ** 2 + D * log_N_M + E)
    
    # Return term1, term2, and their sum
    return term1, term2, term1 + term2


# Modify the select_coefficients function to return the unpacked values
def select_coefficients(N_delta_tau, N_M, condition='constant', µ=None):
    if condition == 'constant':
        if µ is None:
            raise ValueError("Please provide a value for µ when using constant conditions.")
        selected_row = df_constant[df_constant['µ'] == µ]
    elif condition == 'varying':
        if µ is None:
            raise ValueError("Please provide a µ_max range for time-varying conditions.")
        selected_row = df_varying[df_varying['µ_range'] == µ]
    else:
        raise ValueError("Condition must be 'constant' or 'varying'.")
    
    # Extract the coefficients A, B, C, D, E
    A = selected_row['A'].values[0]
    B = selected_row['B'].values[0]
    C = selected_row['C'].values[0]
    D = selected_row['D'].values[0]
    E = selected_row['E'].values[0]
    
    # Compute epsilon_hat using the fit function
    return epsilon_hat(N_delta_tau, N_M, A, B, C, D, E)

# Example input values for N_delta_tau and N_M
N_delta_tau = 1000
N_M = 40

decay_rate = 1.53e-6 # Xe133
a = 5e-6 # radius
D = 1e-20 # diffusivity

mu = decay_rate * a**2 / D

print(f"mu = {mu}")

# Option 1: Using constant conditions
µ_constant = 10**5
result_constant = select_coefficients(N_delta_tau, N_M, condition='constant', µ=µ_constant)
print(f"Fit result (epsilon_hat) for constant conditions (µ = {µ_constant}): {result_constant}")

# Option 2: Using time-varying conditions
µ_varying = '[0, 10^{30}]'
result_varying = select_coefficients(N_delta_tau, N_M, condition='varying', µ=µ_varying)
print(f"Fit result (epsilon_hat) for time-varying conditions (µ_max = {µ_varying}): {result_varying}")

# Arrays to store the results
term1_values = []
term2_values = []
sum_values = []

N_delta_tau_range = np.logspace(1, 5, 100)  # From 10^1 to 10^5

# Loop through the range of N_delta_tau
for N_delta_tau in N_delta_tau_range:
    term1, term2, sum_value = select_coefficients(N_delta_tau, N_M, condition='constant', µ=µ_constant)
    term1_values.append(term1)
    term2_values.append(term2)
    sum_values.append(sum_value)

# Plotting
plt.figure(figsize=(10, 6))
plt.plot(N_delta_tau_range, term1_values, label='Term 1', color='blue', linestyle='--')
plt.plot(N_delta_tau_range, term2_values, label='Term 2', color='green', linestyle='-.')
plt.plot(N_delta_tau_range, sum_values, label='Sum (Term 1 + Term 2)', color='red')

# Set log scale for x-axis
plt.xscale('log')

# Labels and title
plt.xlabel('N_delta_tau')
plt.ylabel('Values')
plt.title('Plot of Term 1, Term 2, and their Sum')
plt.legend()

# Show the plot
plt.grid(True)
plt.show()