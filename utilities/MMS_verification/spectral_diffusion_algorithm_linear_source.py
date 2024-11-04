import math
import numpy as np
import matplotlib.pyplot as plt
#In this copy, I will try to add the effect of having S=S(r)

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
	projection_coeff = math.sqrt(8.0 / pi)
	source_rate_coeff_1 = projection_coeff * parameter[3] # - 2 sqrt(2/pi) * S1



	for n in range(int(parameter[0])): #n in range of n_modes
		np1 = n + 1
		n_coeff = -(((-1)**np1)/np1)+2*(((-1)**(np1)-1)/(np1**(3)*pi**(2))) #Linear Projection
		n_c = - (-1)**np1 / np1
		

		diffusion_rate1 = diffusion_rate_coeff1 * np1**2 
		source_rate1 = source_rate_coeff_1 * n_coeff 
		coeff = 1.0 + (diffusion_rate1) * increment # denomenator when solving the backward euler
        
		#x(i+1) = (x(i) + <S_k|psi>Dt)/(1+(D*pi^2*k^2/a^2)Dt) | This is the equation below
		initial_conditions = (initial_condition_gas_1[n] + source_rate1 * increment)/(coeff)
		initial_condition_gas_1[n] = initial_conditions
		gas_1_solution += projection_coeff * n_c * initial_conditions / ((4.0 / 3.0) * pi) #Volume average

	gas_1[0] = gas_1_solution


 #def main():

num_steps = 1000
increment = 0.01

time_vector = np.arange(num_steps) * increment

# Giovanni's Inputs
n_modes = 40
D1 = 3.0
a = 2.0
S1 = 4.0

parameter = [n_modes, D1, a, S1] #We dont need b and g nor the decay L

initial_condition_gas_1 = np.zeros(parameter[0])

gas_1 = np.zeros(num_steps)

for i in range(num_steps):
	SpectralDiffusion1equation(gas_1[i:i+1], initial_condition_gas_1, parameter, increment)

c_eq1 = S1 * a**2 / (24 * D1)


plt.plot(time_vector, gas_1, label='Gas ')
plt.axhline(y=c_eq1, linestyle='--', label='Equilibrium Concentration ')
plt.title('Linear Source S=yFr/a')
plt.xlabel('Time')
plt.ylabel('Concentration')
plt.legend()
plt.grid()
plt.savefig('output.png')
# if __name__ == "__main__":
# 	main()


