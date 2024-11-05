import math
import numpy as np
import matplotlib.pyplot as plt

def SpectralDiffusion2equations(gas_1, gas_2, initial_condition_gas_1, initial_condition_gas_2, parameter, increment):

	n = 0
	np1 = 1

	diffusion_rate1 = 0.0
	diffusion_rate2 = 0.0

	diffusion_rate_coeff1 = 0.0
	diffusion_rate_coeff2 = 0.0

	source_rate_coeff_1 = 0.0
	source_rate_coeff_2 = 0.0

	source_rate1 = 0.0
	source_rate2 = 0.0

	projection_coeff = 0.0

	gas_1_solution = 0.0
	gas_2_solution = 0.0

	pi = math.pi

	diffusion_rate_coeff1 = pi**2 * parameter[1] / parameter[3]**2 # pi^2 * D1 / a^2
	diffusion_rate_coeff2 = pi**2 * parameter[2] / parameter[3]**2 # pi^2 * D2 / a^2

	projection_coeff = math.sqrt(8.0 / pi)

	# source_rate_coeff_1 = projection_coeff * parameter[4] # - 2 sqrt(2/pi) * S1
	# source_rate_coeff_2 = projection_coeff * parameter[5] # - 2 sqrt(2/pi) * S2

	for n in range(int(parameter[0])):
		np1 = n + 1
		n_c = -(-1)**np1 / np1
		n_coeff1 = - ((-1)**np1 / np1) * (parameter[3] * parameter[4] + parameter[5]) + (2 * parameter[3] * parameter[4]) * ((-1)**np1 - 1) / (np1**3 * pi**2)
		n_coeff2 = - ((-1)**np1 / np1) * (parameter[3] * parameter[6] + parameter[7]) + (2 * parameter[3] * parameter[6]) * ((-1)**np1 - 1) / (np1**3 * pi**2)


		diffusion_rate1 = diffusion_rate_coeff1 * np1**2 # pi^2 * D1 * n^2 / a^2
		diffusion_rate2 = diffusion_rate_coeff2 * np1**2 # pi^2 * D2 * n^2 / a^2

		source_rate1 = projection_coeff * n_coeff1 
		source_rate2 = projection_coeff * n_coeff2

		coeff_matrix = [
			1.0 + (diffusion_rate1 + parameter[9] + parameter[10]) * increment,
			- parameter[8] * increment,
			- parameter[9] * increment,
			1.0 + (diffusion_rate2 + parameter[8] + parameter[10]) * increment
		]

		initial_conditions = [
			initial_condition_gas_1[n] + source_rate1 * increment,
			initial_condition_gas_2[n] + source_rate2 * increment
		]

		Laplace2x2(coeff_matrix, initial_conditions)

		initial_condition_gas_1[n] = initial_conditions[0]
		initial_condition_gas_2[n] = initial_conditions[1]

		gas_1_solution += projection_coeff * n_c * initial_conditions[0] / ((4.0 / 3.0) * pi)
		gas_2_solution += projection_coeff * n_c * initial_conditions[1] / ((4.0 / 3.0) * pi)

	gas_1[0] = gas_1_solution
	gas_2[0] = gas_2_solution


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
	D2 = 0.5
	a = 1.0
  # Source S1(r) = A1 * r + B1
	A1,B1 = [2.0, 2.0]
  # Source S1(r) = A1 * r + B1
	A2,B2 = [1.0, 3.0]
	b = 0.0
	g = 0.0
	L = 0.0

	parameter = [n_modes, D1, D2, a, A1, B1, A2, B2, b, g, L]

	initial_condition_gas_1 = np.zeros(parameter[0])
	initial_condition_gas_2 = np.zeros(parameter[0])

	gas_1 = np.zeros(num_steps)
	gas_2 = np.zeros(num_steps)

	for i in range(num_steps):
		SpectralDiffusion2equations(gas_1[i:i+1], gas_2[i:i+1], initial_condition_gas_1, initial_condition_gas_2, parameter, increment)

	c_eq1 = (-6 * a**2 / D1) * ( -((A1 * a + B1) / 90) + ( a * A1 / 240)  )
	c_eq2 = (-6 * a**2 / D2) * ( -((A2 * a + B2) / 90) + ( a * A2 / 240)  )

	plt.plot(time_vector, gas_1, color = 'b', label='Gas 1')
	plt.plot(time_vector, gas_2, color = 'r', label='Gas 2')
	plt.axhline(y=c_eq1, linestyle='--', color = 'b', label='Equilibrium Concentration 1')
	plt.axhline(y=c_eq2, linestyle='--', color = 'r', label='Equilibrium Concentration 2')
	plt.xlabel('Time')
	plt.ylabel('Concentration')
	plt.legend()
	plt.grid()
	plt.savefig('output.png')

if __name__ == "__main__":
	main()
