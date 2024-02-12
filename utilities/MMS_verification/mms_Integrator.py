"""
This script performs the numerical verification via method of manufactured solution (MMS) of the
SCIANTIX solver "NewtonBlackburn"

Author: G. Zullo
Ref: Oberkampf , W. L., Trucano , T. G., and Hirsch , C. (December 21, 2004). "Verification, validation, and predictive capability in computational engineering and physics." ASME. Appl. Mech. Rev. September 2004; 57(5): 345–384. https://doi.org/10.1115/1.1767847

"""

import numpy as np
import matplotlib.pyplot as plt

# ODE: dx/dt = S

# Manufactured parameters
# -----------------------
# S = 2t

def S_mms(t):
  return 2*t

# Manufactured solution
# ---------------------
# x = t^2
# dx/dt = 2t

def mms_solver(N, ti, tf):
  
  h = (tf - ti) / N
  t = np.linspace(ti, tf, N+1)
  x = np.ones_like(t)
  x[0] = 1 # initial condition

  for i in range(N): # temporal loop
    S = S_mms(t[i+1])

    # SCIANTIX solver
    x[i+1] = x[i] + S * h

  return x

def orderOfConvergence(error1, error2):
  return 1 - np.log2(2*error2/error1)

def error(mms, timeStepNumber, ti, tf):
  analyticalSolution = 1+np.linspace(ti,tf,timeStepNumber+1)**2
  return abs(mms-analyticalSolution)/analyticalSolution

# MMS verification
ti, tf = 0, 1

# Solution with N time steps
N = 20
time1 = np.linspace(ti,tf,N+1)
mms1 = mms_solver(N, ti, tf)

# Solution with 2N time steps
N2 = 2*N
time2 = np.linspace(ti,tf,N2+1)
mms2 = mms_solver(N2, ti, tf)

error1 = error(mms1, N, ti, tf)
error2 = error(mms2, N2, ti, tf)

plt.plot(time1, mms1, label='MMS(N)')
plt.plot(time2, mms2, label='MMS(2N)')
plt.plot(time1, 1+np.linspace(ti,tf,N+1)**2, label='Exact solution')
plt.ylabel('x')
plt.xlabel('time')
plt.legend()
plt.grid()
plt.show()

# fig, ax = plt.subplots()
# plt.plot(time1, error1, label = 'error(N)')
# plt.plot(time2, error2, label = 'error(2N)')
# plt.ylabel('error')
# plt.xlabel('time')
# plt.legend()
# plt.grid()
# plt.show()

print(f"Order of convergence = ", orderOfConvergence(error1[-1], error2[-1]))
# print(f"Order of convergence = ", orderOfConvergence(max(error1), max(error2)))
