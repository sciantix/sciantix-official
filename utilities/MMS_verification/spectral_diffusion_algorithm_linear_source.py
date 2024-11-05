import math
import numpy as np
import matplotlib.pyplot as plt

def SpectralDiffusion1equation(gas, initial_condition_gas, parameter, increment):

#parameter = [n_modes, D1, a, S1] #We dont need b,g, nor L since we're only considering diffusion

	n = 0
	np1 = 1
	diffusion_rate = 0.0
	diffusion_rate_coeff = 0.0
	source_rate_coeff = 0.0
	source_rate = 0.0
	projection_coeff = 0.0
	gas_solution = 0.0
	pi = math.pi
	
	diffusion_rate_coeff = pi**2 * parameter[1] / parameter[2]**2 # pi^2 * D / a^2
	projection_coeff = math.sqrt(8.0 / pi)
	#source_rate_coeff = projection_coeff * parameter[3] # - 2 sqrt(2/pi) * S



	for n in range(int(parameter[0])): #n in range of n_modes
		np1 = n + 1
		n_coeff = - ((-1)**np1 / np1) * (parameter[2] * parameter[3] + parameter[4]) + (2 * parameter[2] * parameter[3]) * ((-1)**np1 - 1) / (np1**3 * pi**2)
		n_c = - (-1)**np1 / np1
		

		diffusion_rate = diffusion_rate_coeff * np1**2 
		source_rate = projection_coeff * n_coeff
		coeff = 1.0 + (diffusion_rate) * increment 
        
		initial_conditions = (initial_condition_gas[n] + source_rate * increment)/(coeff)
		initial_condition_gas[n] = initial_conditions
		gas_solution += projection_coeff * n_c * initial_conditions / ((4.0 / 3.0) * pi) #Volume average

	gas[0] = gas_solution


 #def main():

num_steps = 1000
increment = 0.01

time_vector = np.arange(num_steps) * increment

# Giovanni's Inputs
n_modes = 40
D = 3.0
a = 2.0
#Source has the shape ( S(r) = A * r + B )
A = 4.0 / a
B = 3.0

parameter = [n_modes, D, a, A, B]

initial_condition_gas = np.zeros(parameter[0])

gas = np.zeros(num_steps)

for i in range(num_steps):
	SpectralDiffusion1equation(gas[i:i+1], initial_condition_gas, parameter, increment)

c_eq = (-6 * a**2 / D) * ( -((A * a + B) / 90) + ( a * A / 240)  )


plt.plot(time_vector, gas, label='Gas ')
plt.axhline(y=c_eq, linestyle='--', label='Equilibrium Concentration ')
plt.title(f'Linear Source S(r) = {A}*r + {B}')
plt.xlabel('Time')
plt.ylabel('Concentration')
plt.legend()
plt.grid()
plt.savefig('output.png')
# if __name__ == "__main__":
# 	main()


