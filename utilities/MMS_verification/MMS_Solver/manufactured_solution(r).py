"""
This script plots the manufactured solution in space

"""
# Function of r at t=1

a = 5e-6
t = 1
r = np.linspace(0, a, 100)
c = (a**2-r**2)*np.exp(0.005*t)

plt.plot(r, c)
plt.xlabel('r (m)')
plt.ylabel('c(r) (at/$m^3$)')
plt.title(r'2D plot of c(r,t=1) = $\alpha$ * ($a^2$-$r^2$) * $e^{\epsilon*t}$')
plt.grid()