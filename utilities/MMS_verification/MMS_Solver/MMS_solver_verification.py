"""
This script performs the MMS verification

"""
import math
import numpy as np
import matplotlib.pyplot as plt

pi = math.pi

dt = 20
t = np.arange(0, 1000 + 1, dt)

# Initialization
C_N_tot = np.zeros(len(t))  # Spatial average numerical solution

n = 20  # Number of spatial modes
k = np.arange(1, n + 1)  # Spatial Modes
n_modes = k[-1]

X_N = np.zeros((len(k), len(t)))  # Time coefficients
S_k = np.zeros((len(k), len(t)))  # Source projection
C_N = np.zeros((len(k), len(t)))  # Contribution of each mode
lamda = np.zeros((len(k), len(t)))  # Eigenvalues

a = 5e-6  # Grain radius

C_M = 0.4*a**2*np. exp(0.005*t) # Manufactured solution

D_M = np.exp(-0.001*t)  # No constraints on physical meaning (pure verification)

for j in range(1, len(t)):
    for i in range(len(k)):
        # Source rate
        S_k[i, j] =  2 * np.sqrt(2) * np.sqrt(np.pi) * (
    -0.03 * (-1) ** k[i] * a ** 4 * np.exp(0.005 * t[j]) / (np.pi ** 3 * k[i] ** 3)
    - 6.0 * (-1) ** k[i] * a ** 2 * np.exp(0.004 * t[j]) / (np.pi * k[i])
) / np.sqrt(a)

        # Eigenvalue
        lamda[i, j] = D_M[j] * pi ** 2 * k[i] ** 2 / a ** 2

        # Backward Euler update
        X_N[i, j] = (X_N[i, j - 1] + dt * S_k[i, j]) / (1 + dt * lamda[i, j])

        # Volume-average concentration
        C_N[i, j] = (
            (X_N[i, j] * (-np.sqrt(8 / pi) * (-1) ** k[i]) / k[i])
            / (pi * 4 / 3 * a**(3/2))
        )

C_N_tot = np.sum(C_N, axis=0)

Error = np.abs(C_M - C_N_tot)
L1 = np.mean(np.abs(Error))
L2 = np.sqrt(np.mean(np.abs(Error)**2))
L3 = np.max(np.abs(Error))
Linf = np.abs(Error[-1])

plt.plot(t, C_M, label="Manufactured")
plt.scatter(t, C_N_tot, label="Numerical", color="r", marker="x")
plt.legend()
plt.xlabel("t [s]")
plt.ylabel("$C_{av}(t)$ [at/$m^3$]")
plt.title(f"k={n_modes}")
plt.grid()
plt.show()

print(f"Average Error: {L1}")
print(f"RMSE: {L2}")
print(f"Max Error: {L3}")
print(f"Final Value Error: {Linf}")
