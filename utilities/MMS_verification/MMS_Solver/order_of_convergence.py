"""
This script obtains the order of convergence of the solver
since it is backward euler, the order of convergence should be 1

"""
import math
import numpy as np
import matplotlib.pyplot as plt

pi = math.pi

dt_values = [200, 100, 50, 25, 12.5, 6.25, 3.125]  # Different time steps

a = 5e-6  # Grain radius
modes = 40
# Initialize arrays for storing results
L1 = np.zeros((len(dt_values), modes))  # L1 errors for each dt and each k mode
L2 = np.zeros((len(dt_values), modes))  # L2 errors for each dt and each k mode
L3 = np.zeros((len(dt_values), modes))  # L3 errors for each dt and each k mode
Linf = np.zeros((len(dt_values), modes))  # Linf errors for each dt and each k mode

# Initialize arrays for storing order of convergence
oc1 = np.zeros(modes)  # Order of convergence for L1
oc2 = np.zeros(modes)  # Order of convergence for L2
oc3 = np.zeros(modes)  # Order of convergence for L3
ocinf = np.zeros(modes)  # Order of convergence for Linf

# Loop over different k values (from 1 to 40)
for b in range(1, modes+1):  # Number of spatial modes (b is the current mode count)
    projections = np.arange(1, b + 1)  # spatial modes up to b

    # Loop over different time step values (dt)
    for j, dt in enumerate(dt_values):
        t = np.arange(0, 1000 + 1, dt)  # Time array
        C_M = 0.4*a**2*np. exp(0.005*t)  # Manufactured solution

        # Initialize numerical solution arrays
        C_N_tot = np.zeros(len(t))  # Spatial average numerical solution
        k = np.arange(1, b + 1)  # Spatial modes
        X_N = np.zeros((len(k), len(t)))  # Time coefficients for each mode
        S_k = np.zeros((len(k), len(t)))  # Source projection
        C_N = np.zeros((len(k), len(t)))  # Contribution of each mode
        lamda = np.zeros((len(k), len(t)))  # Eigenvalue

        D_M = np.exp(-0.001*t)  # Diffusion coefficient

        # Time-stepping loop for each time step
        for w in range(1, len(t)):
            for i in range(len(k)):
                # Calculate source rate
                S_k[i, w] = 2 * np.sqrt(2) * np.sqrt(np.pi) * (
    -0.03 * (-1) ** k[i] * a ** 4 * np.exp(0.005 * t[w]) / (np.pi ** 3 * k[i] ** 3)
    - 6.0 * (-1) ** k[i] * a ** 2 * np.exp(0.004 * t[w]) / (np.pi * k[i])
) / np.sqrt(a)

                # Calculate eigenvalue
                lamda[i, w] = D_M[w] * pi**2 * k[i]**2 / a**2

                # Update X_N using backward Euler method
                X_N[i, w] = (X_N[i, w-1] + dt * S_k[i, w]) / (1 + dt * lamda[i, w])

                # Calculate volume-average concentration for each mode
                C_N[i, w] = (X_N[i, w] * (-np.sqrt(8 / pi) * (-1)**k[i]) / k[i]) / (pi * 4 / 3 * a**1.5)

        # Total concentration by summing over all modes
        C_N_tot = np.sum(C_N, axis=0)

        # Calculate errors
        Error = C_M - C_N_tot
        L1[j, b-1] = np.mean(np.abs(Error))  # L1 error for current dt and k
        L2[j, b-1] = np.sqrt(np.mean(np.abs(Error)**2))  # L2 error for current dt and k
        Linf[j, b-1] = np.max(np.abs(Error))  # Linf error for current dt and k
        L3[j, b-1] = np.abs(Error[-1])  # L3 error for current dt and k

    # Order of convergence for the current mode (k = b)
    for l in range(len(dt_values) - 1):
        oc1[b-1] = np.log10(L1[l+1, b-1] / L1[l, b-1]) / np.log10(dt_values[l+1] / dt_values[l])
        oc2[b-1] = np.log10(L2[l+1, b-1] / L2[l, b-1]) / np.log10(dt_values[l+1] / dt_values[l])
        oc3[b-1] = np.log10(L3[l+1, b-1] / L3[l, b-1]) / np.log10(dt_values[l+1] / dt_values[l])
        ocinf[b-1] = np.log10(Linf[l+1, b-1] / Linf[l, b-1]) / np.log10(dt_values[l+1] / dt_values[l])

# Plotting the order of convergence for different spatial modes
plt.figure(figsize=(10, 6))
plt.plot(np.arange(1, modes + 1), oc1, label='Order of Convergence', color = 'purple')
# plt.plot(np.arange(1, 41), oc2, label='Order of Convergence (L2)', marker='x')
# plt.plot(np.arange(1, 41), oc3, label='Order of Convergence (L3)', marker='s')
# plt.plot(np.arange(1, 41), ocinf, label='Order of Convergence (Linf)', marker='d')
plt.xlabel('Number of Spatial Modes (k)')
plt.ylabel('Order of Convergence')
plt.legend()
plt.grid(True)
plt.title('Order of Convergence for Different Spatial Modes')
plt.show()
