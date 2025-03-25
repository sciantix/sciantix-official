"""
This script simulates the solver for multiple number of modes and obtains the error vs k

"""
#Error Behaviour vs k

import math
import numpy as np
import matplotlib.pyplot as plt
import time
pi = math.pi

dt = 20
t = np.arange(0,1000+dt,dt)

# Initialization
C_M = np.zeros(len(t)) # Spatial average manufactured solution
C_N_tot = np.zeros(len(t)) # Spatial average numerical solution
E = np.zeros(len(t)) # Error

n = 40 # Number of spatial modes
E_av = np.zeros(n) # Error
E_RMSE = np.zeros(n) # Error
E_max = np.zeros(n) # Error
E_final = np.zeros(n) # Error
p_av = np.zeros(n) # Error
p_RMSE = np.zeros(n) # Error
p_max = np.zeros(n) # Error
p_final = np.zeros(n) # Error
t_ex = np.zeros(n) # Time

a = 5e-6 # Grain radius

C_M = 0.4*a**2*np. exp(0.005*t) # Spatial Average Concentration


D_M = np.exp(-0.001*t) # Diffusion Coefficient

for l in range(1,n+1):
  start_time = time.perf_counter()
  k = np.arange(1,l+1) # Spatial Modes
  n_modes = k[-1]
  X_N = np.zeros((len(k),len(t))) # 40 rows for k equations of X_n and 100 columns corresposing to t | Time coefficients
  S_k = np.zeros((len(k),len(t))) # Source projection on spatial modes
  C_N = np.zeros((len(k),len(t))) # Contribution of each k; C_N = X_n*psi_n
  lamda = np.zeros((len(k),len(t))) # k^2 * pi^2 / a^2

  for j in range(1,len(t)):
    for i in range(len(k)):
      # source rate
      S_k[i, j] = 2 * np.sqrt(2) * np.sqrt(np.pi) * (
    -0.03 * (-1) ** k[i] * a ** 4 * np.exp(0.005 * t[j]) / (np.pi ** 3 * k[i] ** 3)
    - 6.0 * (-1) ** k[i] * a ** 2 * np.exp(0.004 * t[j]) / (np.pi * k[i])
) / np.sqrt(a)


      # eigenvalue
      lamda[i][j] = D_M[j] * pi**2 * k[i]**2 / a**2

      # backward euler
      X_N[i][j] = (X_N[i][j-1] + dt * S_k[i][j]) / (1 + dt * lamda[i][j])

      # volume-average concentration
      C_N[i][j] = ( (X_N[i][j] * (- np.sqrt(8/pi)*(-1)**k[i])/k[i])) / (a**1.5*pi*4/3) # Each contribution of X_N*psi_N

  C_N_tot = np.sum(C_N, axis=0)
  end_time = time.perf_counter()
  execution_time = end_time - start_time
  t_ex[l-1] = execution_time
  # print(f"Execution time: {execution_time} seconds")

  plt.plot(t,C_M, label ='manufactured')
  plt.scatter(t,C_N_tot, label ='numerical', color ='r', marker = 'x')
  plt.legend(['Manufactured','Numerical'])
  plt.xlabel('t (s)')
  plt.ylabel('$C_{av}(t)$ (at/$m^3$)')
  plt.title('k={n_modes}'.format(n_modes = n_modes))
  plt.grid()
  plt.show()

  E = np.abs(C_M - C_N_tot)
  E_av[l-1] = np.mean(E)
  E_RMSE[l-1] = np.sqrt(np.sum(E**2)/len(t))
  E_max[l-1] =  np.max(E)
  E_final[l-1] = E[-1]


modes = np.arange(1,n+1)
fig = plt.figure(figsize=(20, 5))


plt.subplot(1,3,1)
plt.plot(modes, t_ex, label = 'execution time')
plt.legend()
plt.xlabel('number of modes (k)')
plt.ylabel('execution time (s)')
plt.grid()

plt.subplot(1,3,2)
plt.plot(modes, E_av, label = 'average error', marker = 'o')
plt.plot(modes, E_RMSE, label = 'RMSE', marker = 's')
plt.plot(modes, E_max, label = 'max error', marker = '^')
plt.plot(modes, E_final, label = 'final error', marker = 'x')
plt.legend()
plt.xlabel('number of modes (k)')
plt.ylabel('Error [at/$m^3$]')
plt.grid()

plt.subplot(1,3,3)
plt.plot(modes, np.log(E_av), label = 'average error', marker = 'o')
plt.plot(modes, np.log(E_RMSE), label = 'RMSE', marker = 's')
plt.plot(modes, np.log(E_max), label = 'max error', marker = '^')
plt.plot(modes, np.log(E_final), label = 'final error', marker = 'x')
plt.legend()
plt.xlabel('number of modes (k)')
plt.ylabel('Log Error')
plt.grid()