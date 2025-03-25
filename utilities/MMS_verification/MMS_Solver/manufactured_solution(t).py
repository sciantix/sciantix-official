"""
This script plots the manufactured solution in time

"""
# Function of t at r = 0.5

a = 5e-6
r = 0.5*a
dt = 20
t = np.arange(0, 1000+1, dt)

c = (a**2-r**2)*np.exp(0.005*t)

plt.plot(t, c)
plt.xlabel('t (s)')
plt.ylabel('c(t) (at/$m^3$)')
plt.title(r'2D plot of c(r=0.5a,t) = $\alpha$ * ($a^2$-$r^2$) * $e^{\epsilon*t}$')
plt.grid()