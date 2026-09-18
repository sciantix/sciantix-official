import numpy as np
from scipy.spatial.transform import Rotation as R
from scipy.spatial import ConvexHull
from scipy.integrate import simpson

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

from GB_functions import *


###################### Main code starts here ###################
parameters = makeparvec('UO2') # Default parameters for Cu
pars = parameters[0]  # 43-parameter vector
eRGBvalue = parameters[2]  # Energy of a "random" grain boundary in J/m^2
print(f"eRGB value (J/m2): {eRGBvalue}")

N =   1000

ksi = np.linspace(0, np.pi/2, N)    # rotation angle
eta = np.zeros(N)                   # fixed symm
phi = np.zeros(N)                   # pure twist angle

# Construct geometry matrix (4 x N)
geom = np.vstack([np.zeros(N), ksi, eta, phi])
twist_100 = eRGBvalue*set100(geom, pars)
twist_110 = eRGBvalue*set110(geom, pars)
twist_111 = eRGBvalue*set111(geom, pars)

# Plot result
plt.plot(np.rad2deg(ksi), twist_100, label='Twist <100>', color='blue')
plt.plot(np.rad2deg(ksi), twist_110, label='Twist <110>', color='orange')
plt.plot(np.rad2deg(ksi), twist_111, label='Twist <111>', color='green')
plt.xlabel("Twist misorientation angle (degrees)")
plt.ylabel("GB energy (J/m2)")
plt.title("Twist grain boundary energy")
plt.legend()
plt.ylim([0, 2.5])
plt.grid(True)
plt.show()

ksi = np.linspace(0, np.pi, N)    # rotation angle
eta = np.zeros(N)                   # fixed symm
phi = np.full(N, np.pi/2)           # pure tilt angle

# Construct geometry matrix (4 x N)
geom = np.vstack([np.zeros(N), ksi, eta, phi])
# Compute energy using your set100 function (translated to Python)
tilt_100 = eRGBvalue*set100(geom, pars)
tilt_110 = eRGBvalue*set110(geom, pars)
tilt_111 = eRGBvalue*set111(geom, pars)

# Plot result
plt.plot(np.rad2deg(ksi[ksi <= np.pi/2]), tilt_100[ksi <= np.pi/2], label='Tilt <100>', color='blue')
plt.plot(np.rad2deg(ksi[ksi <= np.pi]), tilt_110[ksi <= np.pi], label='Tilt <110>', color='orange')
plt.plot(np.rad2deg(ksi[ksi <= np.pi/3] ), tilt_111[ksi <= np.pi/3], label='Tilt <111>', color='green')
plt.xlabel("Tilt misorientation angle (degrees)")
plt.ylabel("GB energy (J/m2)")
plt.title("Symmetric tilt grain boundary energy")
plt.legend()
plt.ylim([0, 2.5])
plt.grid(True)
plt.show()

# Create grid
ksi = np.linspace(0, np.pi/2, N)    # rotation angle
eta = np.linspace(0, np.pi/2, N)    # asymm
phi = np.full(N, np.pi/2)         # pure tilt angle
X, Y = np.meshgrid(eta, ksi)

tilt_100 = np.zeros((N, N))  # Initialize result array

for i in range(N):
    eta_row = np.full(N, eta[i])  # create an array of length N filled with eta[i]
    geom = np.vstack([np.zeros(N), ksi, eta_row, phi])  # shape (4, N)
    tilt_100[i, :] = eRGBvalue * set100(geom, pars)    # store results in ith row

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
surf = ax.plot_surface(np.rad2deg(X), np.rad2deg(Y), tilt_100, cmap='viridis', edgecolor='none')
ax.set_title('Tilt grain boundary energy <100>')
ax.set_xlabel('eta (degrees)')
ax.set_ylabel('ksi (degrees)')
ax.set_zlabel('Energy (J/m2)')

plt.show()

# Create grid
ksi = np.linspace(0, np.pi, N)    # rotation angle
eta = np.linspace(0, np.pi/2, N)    # asymm
phi = np.full(N, np.pi/2)         # pure tilt angle
X, Y = np.meshgrid(eta, ksi)

tilt_100 = np.zeros((N, N))  # Initialize result array

for i in range(N):
    eta_row = np.full(N, eta[i])  # create an array of length N filled with eta[i]
    geom = np.vstack([np.zeros(N), ksi, eta_row, phi])  # shape (4, N)
    tilt_100[i, :] = eRGBvalue * set110(geom, pars)    # store results in ith row

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
surf = ax.plot_surface(np.rad2deg(X), np.rad2deg(Y), tilt_100, cmap='viridis', edgecolor='none')
ax.set_title('Tilt grain boundary energy <110>')
ax.set_xlabel('eta (degrees)')
ax.set_ylabel('ksi (degrees)')
ax.set_zlabel('Energy (J/m2)')

plt.show()


# Create grid
ksi = np.linspace(0, np.pi*2/3, N)    # rotation angle
eta = np.linspace(0, np.pi/2, N)    # asymm
phi = np.full(N, np.pi/2)         # pure tilt angle
X, Y = np.meshgrid(eta, ksi)

tilt_100 = np.zeros((N, N))  # Initialize result array

for i in range(N):
    eta_row = np.full(N, eta[i])  # create an array of length N filled with eta[i]
    geom = np.vstack([np.zeros(N), ksi, eta_row, phi])  # shape (4, N)
    tilt_100[i, :] = eRGBvalue * set111(geom, pars)    # store results in ith row

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
surf = ax.plot_surface(np.rad2deg(X), np.rad2deg(Y), tilt_100, cmap='viridis', edgecolor='none')
ax.set_title('Tilt grain boundary energy <111>')
ax.set_xlabel('eta (degrees)')
ax.set_ylabel('ksi (degrees)')
ax.set_zlabel('Energy (J/m2)')

plt.show()

# Define the range of psi from a small positive number to just under pi
psi_vals = np.linspace(0, 62.8, 500)
p_vals = p(psi_vals)

# Plot
plt.figure(figsize=(7, 7))
plt.plot(psi_vals, p_vals, label='$p(\\psi)$', color='blue')
plt.xlabel('$\\psi$ [degrees]')
plt.ylabel('$p(\\psi)$')
plt.title('Random misorientation distribution (Mackenzie, 1980)')
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()

area = simpson(p_vals, psi_vals)
print(f"Area in [0°, 62.8°]: {area:.5f}")