"""
This script gives the volume averaged expression of the manufactured solution

"""
# Volume Average Concentartion
func = (a**2-r**2) * sp.exp(0.005*t)
C_av = (4*sp.pi*r**2 * func /((4/3)*sp.pi*a**3))

# Compute the integral from 0 to a
integral_result = sp.integrate(C_av, (r, 0, a))
integral_result.simplify()
print(integral_result)