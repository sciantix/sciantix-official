"""
This script plots the manufactured solution in space and time

"""
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

a = 5e-6
# Define the function c(r,t)
def c(r, t):
    return (a**2-r**2)*np.exp(0.005*t)

# Create the meshgrid for r and t
r = np.linspace(-a, a, 100)
dt = 20
t = np.arange(0, 1000+dt, dt)
R, T = np.meshgrid(r, t)

# Calculate c(r, t)
C = c(R, T)

# Plotting the 3D surface
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
surf = ax.plot_surface(R, T, C, cmap='viridis')

ax.set_xlabel('r (m)')
ax.set_ylabel('t (s)')
ax.set_zlabel('c(r,t) (at/$m^3$)')
ax.set_title(r'3D plot of c(r,t) = $\alpha$ * ($a^2$-$r^2$) * $e^{\epsilon*t}$')

plt.colorbar(surf)
plt.show()