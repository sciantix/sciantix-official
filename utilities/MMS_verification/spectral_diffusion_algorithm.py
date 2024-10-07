import math
import numpy as np
import matplotlib.pyplot as plt
#gas 1 ..> C
#gas 2 ..> m

#the task will be to change the solver from solving 2 equations to 1 equation, meaning considering only C
#Change the python code to have the solution of just the diffusion 
#plot the average solution

#increment is the time step
def SpectralDiffusion1equation(gas_1, initial_condition_gas_1, parameter, increment):

#parameter = [n_modes, D1, a, S1] #We dont need b,g, nor L since we're only considering diffusion

	n = 0
	np1 = 1

	diffusion_rate1 = 0.0

	diffusion_rate_coeff1 = 0.0

	source_rate_coeff_1 = 0.0

	source_rate1 = 0.0

	projection_coeff = 0.0

	gas_1_solution = 0.0

	pi = math.pi

	diffusion_rate_coeff1 = pi**2 * parameter[1] / parameter[2]**2 # pi^2 * D1 / a^2

	projection_coeff = -2.0 * math.sqrt(2.0 / pi)

	source_rate_coeff_1 = projection_coeff * parameter[3] # - 2 sqrt(2/pi) * S1

	# dx_k/dt = -(D*pi^2*k^2/a^2)x_k + <S_k|psi>
	# Using backward euler:
	# (x(i+1)-x(i))/Dt = -(D*pi^2*k^2/a^2)x(i+1) + S_k which is <S|psi_k>
    # (1+(D*pi^2*k^2/a^2)Dt)x(i+1) = x(i) + <S_k|psi>Dt
    # x(i+1) = (x(i) + <S_k|psi>Dt)/(1+(D*pi^2*k^2/a^2)Dt); such that diffusion_rate1 = (pi^2 * D1 / a^2)*Dt*n_modes


	for n in range(int(parameter[0])): #n in range of n_modes
		np1 = n + 1
		n_coeff = (-1.0)**np1 / np1 ; # if we recall S_k= - (-1)^k/k * Sqrt(8/pi)
		# and so n_coeff will be used in the S_k term

		diffusion_rate1 = diffusion_rate_coeff1 * np1**2 # pi^2 * D1 * n^2 / a^2
		source_rate1 = source_rate_coeff_1 * n_coeff # [-  sqrt(8/pi) * S1 * (-1)^n/n] this is S_k

		coeff = 1.0 + (diffusion_rate1) * increment # denomenator when solving the backward euler

		initial_conditions = initial_condition_gas_1[n] + source_rate1 * increment

		#Laplace2x2(coeff, initial_conditions) # (A,b)

		initial_condition_gas_1[n] = initial_conditions

		gas_1_solution += projection_coeff * n_coeff * initial_conditions / ((4.0 / 3.0) * pi) #Volume average

	gas_1[0] = gas_1_solution



# def Laplace2x2(A, b):
# 	detA = A[0] * A[3] - A[1] * A[2]

# 	if detA != 0.0:
# 		detX = b[0] * A[3] - b[1] * A[1]
# 		detY = b[1] * A[0] - b[0] * A[2]
# 		b[0] = detX / detA
# 		b[1] = detY / detA

def main():

	num_steps = 1000
	increment = 0.01

	time_vector = np.arange(num_steps) * increment

	# Giovanni's Inputs
	n_modes = 40
	D1 = 3.0
	a = 2.0
	S1 = 4.0

	parameter = [n_modes, D1, a, S1] #We dont need b and g nor teh decay L

	initial_condition_gas_1 = np.zeros(parameter[0])
	
	gas_1 = np.zeros(num_steps)
	
	for i in range(num_steps):
		SpectralDiffusion1equation(gas_1[i:i+1], initial_condition_gas_1, parameter, increment)

	c_eq1 = S1 * a**2 / (15 * D1)

	plt.plot(time_vector, gas_1, label='Gas 1')
	plt.axhline(y=c_eq1, linestyle='--', label='Equilibrium Concentration 1')
	plt.xlabel('Time')
	plt.ylabel('Concentration')
	plt.legend()
	plt.show()

if __name__ == "__main__":
	main()

# 1 equation scalar decay
# 2 equations matrix laplace
#build diffusion rate and source rate
#in python get C ave (t)
#modify source_rate which is <S|psi>
# at the end C should approach an assumptotyic value which is how i can verify if the code works okay

