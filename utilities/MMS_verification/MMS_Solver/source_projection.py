"""
This script gives the source projection expression based on the source function obtained

"""

#Source Projection
# Define variables
k = sp.symbols('k', integer=True, positive=True)
psi = (1 / sp.sqrt(2 * sp.pi * a)) * sp.sin(k * sp.pi * r/a) / r

s_proj = 4*sp.pi*r**2 * psi * (0.005*(a**2 - r**2)*sp.exp(0.005*t) + 6*sp.exp(0.004*t))

# Compute the integral from 0 to a
integral_result = sp.integrate(s_proj, (r, 0, a))
integral_result.simplify()
integral_result
#print(integral_result)