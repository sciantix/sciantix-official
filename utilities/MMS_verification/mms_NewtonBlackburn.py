"""
This script performs the numerical verification via method of manufactured solution (MMS) of the
SCIANTIX solver "NewtonBlackburn"

Author: G. Zullo
Ref: Oberkampf , W. L., Trucano , T. G., and Hirsch , C. (December 21, 2004). "Verification, validation, and predictive capability in computational engineering and physics." ASME. Appl. Mech. Rev. September 2004; 57(5): 345–384. https://doi.org/10.1115/1.1767847

"""

import numpy as np
import matplotlib.pyplot as plt

# ODE: dx/dt = k (1 - beta * exp(alpha * x))

# Manufactured parameters
# -----------------------
# alpha = ln(P)/sin(t)
# beta = sin(t)
# k = cos(t)/(1-sin(t)*P)
# P = 10
def k_mms(t, x):
  return np.cos(t)/(1-np.sin(t)*P)

def alpha_mms(t, x):
  return np.log(P) / np.sin(t)

def beta_mms(t,x):
  return np.sin(t)

# Manufactured solution
# ---------------------
# x = sin(t)
# dx/dt = cos(t)
# dx/dt = cos(t)/(1-sin(t)*P) * (1 - sin(t) * exp(ln(P)*x/sin(t)))
def mms_solver(N, ti, t0, P):
  
  h = (tf - ti) / N
  t = np.linspace(ti, tf, N+1)
  x = np.ones_like(t)
  x[0] = np.sin(ti)

  for i in range(N): # temporal loop
    x0 = x[i]
    x00 = x[i]

    k = k_mms(t[i+1],x[i+1])
    beta = beta_mms(t[i+1],x[i+1])
    alpha = alpha_mms(t[i+1],x[i+1])

    # SCIANTIX solver
    iter = 0
    while (iter < 50):
      fun = x0 - x00 - k * h + k * beta * np.exp(alpha * x0) * h
      deriv = 1.0 + k * beta * alpha * np.exp(alpha * x0) * h
      x1 = x0 - fun/deriv
      x0 = x1

      if(abs(fun) < 0.001):
        x[i+1] = x1

      iter = iter + 1
    
    x[i+1] = x1

  return x

def orderOfConvergence(error1, error2):
  return 1 - np.log2(2*error2/error1)

def error(mms, timeStepNumber, ti, tf):
  analyticalSolution = np.sin(np.linspace(ti,tf,timeStepNumber+1))
  return abs(mms-analyticalSolution)/analyticalSolution

# MMS verification
P = 10.0
ti, tf = np.pi/10, np.pi*9/10

# Solution with N time steps
N = 100
time1 = np.linspace(ti,tf,N+1)
mms1 = mms_solver(N, ti, tf, P)

# Solution with 2N time steps
N2 = 2*N
time2 = np.linspace(ti,tf,N2+1)
mms2 = mms_solver(N2, ti, tf, P)

error1 = error(mms1, N, ti, tf)
error2 = error(mms2, N2, ti, tf)

plt.plot(time1, mms1, label='MMS(N)')
plt.plot(time2, mms2, label='MMS(2N)')
plt.plot(time1, np.sin(np.linspace(ti,tf,N+1)), label='Exact solution')
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
# print(f"Order of convergence = ", orderOfConvergence(max(e50), max(e100)))
