import math
import numpy as np
import matplotlib.pyplot as plt

def SpectralDiffusionNonEquilibrium(gas_solution, gas_bubble, initial_condition_gas_solution, initial_condition_gas_bubble, parameter, increment):
	# SpectralDiffusionNonEquilibrium
	# Solver for the spatially averaged solution of the systems of PDEs:
	# |dy1/dt = D div grad y1 - gy1 + by2 + S1 - Ly1
	# |dy2/dt = Db div grad y2 + gy1 - by2 + S2 - Ly2

	#           ((D div grad - g - L)                      + b )   + S1
	#           (        + g            (Db div grad -b - L)   )   + S2

	# We apply a spectral approach in space, projecting the equation on the eigenfunctions of the laplacian operator.
	# We use the first order backward Euler solver in time.
	# The number of terms in the expansion, N, is fixed a priori.

	n = 0
	np1 = 1

	bubble_diffusion_rate = 0.0
	diffusion_rate_coeff = 0.0
	bubble_diffusion_rate_coeff = 0.0
	diffusion_rate = 0.0
	source_rate_coeff_solution = 0.0
	source_rate_coeff_bubbles = 0.0
	source_rate_solution = 0.0
	source_rate_bubble = 0.0
	projection_coeff = 0.0
	gas_solution_solution = 0.0
	gas_bubble_solution = 0.0

	pi = math.pi

	diffusion_rate_coeff = pi**2 * parameter[1] / parameter[5]**2
	bubble_diffusion_rate_coeff = pi**2 * parameter[8] / parameter[5]**2
	projection_coeff = -2.0 * math.sqrt(2.0 / pi)
	source_rate_coeff_solution = projection_coeff * parameter[6]
	source_rate_coeff_bubbles = projection_coeff * parameter[7]

	for n in range(int(parameter[0])):
		np1 = n + 1
		n_coeff = (-1.0)**np1 / np1

		diffusion_rate = diffusion_rate_coeff * np1**2
		bubble_diffusion_rate = bubble_diffusion_rate_coeff * np1**2
		source_rate_solution = source_rate_coeff_solution * n_coeff
		source_rate_bubble = source_rate_coeff_bubbles * n_coeff

		coeff_matrix = [
			1.0 + (diffusion_rate + parameter[3] + parameter[4]) * increment,
			-parameter[2] * increment,
			-parameter[3] * increment,
			1.0 + (bubble_diffusion_rate + parameter[2] + parameter[4]) * increment
		]

		initial_conditions = [
			initial_condition_gas_solution[n] + source_rate_solution * increment,
			initial_condition_gas_bubble[n] + source_rate_bubble * increment
		]

		Laplace2x2(coeff_matrix, initial_conditions)

		initial_condition_gas_solution[n] = initial_conditions[0]
		initial_condition_gas_bubble[n] = initial_conditions[1]

		gas_solution_solution += projection_coeff * n_coeff * initial_conditions[0] / ((4.0 / 3.0) * pi)
		gas_bubble_solution += projection_coeff * n_coeff * initial_conditions[1] / ((4.0 / 3.0) * pi)

	gas_solution[0] = gas_solution_solution
	gas_bubble[0] = gas_bubble_solution


def Laplace2x2(A, b):
	detA = A[0] * A[3] - A[1] * A[2]

	if detA != 0.0:
		detX = b[0] * A[3] - b[1] * A[1]
		detY = b[1] * A[0] - b[0] * A[2]
		b[0] = detX / detA
		b[1] = detY / detA

def main():

	num_steps = 1000
	increment = 0.01

	time_vector = np.arange(num_steps) * increment

	n_modes = 40
	D1 = 0.1
	b = 0.0
	g = 0.0
	L = 0.0
	a = 1.0
	S1 = 2.0
	S2 = 3.0
	D2 = 0.5

	parameter = [n_modes, D1, b, g, L, a, S1, S2, D2]

	initial_condition_gas_solution = np.zeros(parameter[0])
	initial_condition_gas_bubble = np.zeros(parameter[0])
	
	gas_solution = np.zeros(num_steps)
	gas_bubble = np.zeros(num_steps)
	
	for i in range(num_steps):
		SpectralDiffusionNonEquilibrium(gas_solution[i:i+1], gas_bubble[i:i+1], initial_condition_gas_solution, initial_condition_gas_bubble, parameter, increment)

	c_eq1 = S1 * a**2 / (15 * D1)
	c_eq2 = S2 * a**2 / (15 * D2)

	plt.plot(time_vector, gas_solution, label='Gas Solution')
	plt.plot(time_vector, gas_bubble, label='Gas Bubble')
	plt.axhline(y=c_eq1, color='r', linestyle='--', label='Equilibrium Concentration')
	plt.axhline(y=c_eq2, color='r', linestyle='--', label='Equilibrium Concentration')
	plt.xlabel('Time')
	plt.ylabel('Concentration')
	plt.title('Concentration Dynamics Over Time')
	plt.legend()
	plt.show()

if __name__ == "__main__":
	main()
