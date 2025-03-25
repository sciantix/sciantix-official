"""
This script  gives the source function based on the manufactured solution and diffusion coefficient

"""


#Derivative:
import sympy as sp

# Define the symbolic variable
r, a, t = sp.symbols('r a t')

# Define the manufactured solution C_m
f_M = (a**2-r**2)*sp.exp(0.005*t)

# Define the diffusion coefficient (doesn't depend on r)
D = sp.exp(-10**(-3)*t)

# Compute the full expression
expr = (1 / r**2) * sp.diff(r**2 * sp.diff(f_M, r), r)
dfdt = sp.diff(f_M, t)

# Simplify the expression
simplified_expr = sp.simplify(expr)
simplified_expr

S = dfdt - D*simplified_expr
print(S)