import sympy as sp

# Define the variables and functions
r, t, D, lambda_ = sp.symbols('r t D lambda', real=True, positive=True)
S = sp.symbols('S')
c = sp.Function('c')(r, t)

# Define the proposed solution
c_expr = sp.exp(D*t) * sp.sin(sp.pi * r) * (1 + sp.cos(2 * sp.pi * t))

# Calculate the derivatives
dc_dt = sp.diff(c_expr, t)
laplacian_c = sp.diff(c_expr, r, r) + (1/r) * sp.diff(c_expr, r)

# Diffusion equation
diffusion_eq = sp.Eq(dc_dt, D * laplacian_c - lambda_ * c_expr + S)

# Display the calculated equations
print("Time derivative of c:")
sp.pprint(dc_dt)

print("\nLaplacian of c:")
sp.pprint(laplacian_c)

print("\nDiffusion equation:")
sp.pprint(diffusion_eq)

# Solve the equation to find lambda and S
solutions = sp.solve(diffusion_eq, (lambda_, S))

# Extract the solutions
lambda_solution = solutions[0][0]
S_solution = solutions[0][1]

print("\nSolutions found:")
print(f"lambda = {lambda_solution}")
print(f"S = {S_solution}")
print(f"D = {D}")

# Verify the solutions
diffusion_eq_subs = diffusion_eq.subs({lambda_: lambda_solution, S: S_solution})
simplified_eq = sp.simplify(diffusion_eq_subs)

print("\nDiffusion equation after substituting the solutions:")
sp.pprint(simplified_eq)
