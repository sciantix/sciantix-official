import numpy as np
import os
import matplotlib
# matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib import cm
import re, ast


prediction_trace = []



final_optim_fr = np.genfromtxt('final_optim_fr.txt')

params_optimized = np.genfromtxt('params_optimized.txt')

optim_exp = np.genfromtxt('optim_data.txt')
talip_data = np.genfromtxt('Talip2014_release_data.txt')
# params = params_optimized.shape[1]
params = 2
if params == 1:
    plt.plot(optim_exp[:,0], params_optimized, color = 'g', label = 'optimization')
    plt.scatter(optim_exp[:,0], params_optimized, color = 'g', marker='o')
    plt.plot([optim_exp[0,0], optim_exp[-1,0]], [params_optimized[-1], params_optimized[-1]], color = 'r', label = 'Final Optimized')
    plt.xlabel('time / h')
    plt.ylabel('ln(sf) / -')
    plt.legend()
    plt.title('scaling factor')
    plt.show()
else:
    for i in range(params):

        plt.plot(optim_exp[:,0], params_optimized[:,i], color = 'g', label = 'optimization')
        plt.scatter(optim_exp[:,0], params_optimized[:,i], color = 'g', marker='o')
        plt.plot([optim_exp[0,0], optim_exp[-1,0]], [params_optimized[-1,i], params_optimized[-1,i]], color = 'r', label = 'Final Optimized')
        plt.xlabel('time / h')
        plt.ylabel('sf / -')
        plt.legend()
        plt.title('Scaling factor')
        plt.show()


plt.scatter(talip_data[:,0], talip_data[:,1], marker = '.', c = '#B3B3B3', label='Talip et al. (2014)')
plt.scatter(optim_exp[:,0], optim_exp[:,1], color = 'g', marker='x', label = 'optimization')
plt.scatter(optim_exp[:,0], optim_exp[:,2], facecolor = 'none', edgecolors='r',marker='o', label = 'filtered exp')
plt.plot(final_optim_fr[:,0], final_optim_fr[:,2], 'r', label = 'Optimization')

plt.xlabel('time / h')
plt.ylabel('fraction release / -')
plt.legend()
plt.title('Fraction release')
plt.show()


