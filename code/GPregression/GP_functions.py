"""

This is a python script to perform the correlation update in sciantix.

@author G. Nicodemo

"""

""" ------------------- Import requiered depedencies ------------------- """

import numpy as np
import matplotlib.pyplot as plt

from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D

from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import ConstantKernel, RBF

""" ------------------- Some useful GP functions ------------------ """

def load_txt_to_matrix(file_path):
    data = np.genfromtxt(file_path, delimiter='\t')      
    return data

def plot_gp(mu, cov, X, X_train=None, Y_train=None, samples=[], label = 'Mean prediction'):
    X = X.ravel()
    mu = mu.ravel()
    uncertainty = 1.96 * np.sqrt(np.diag(cov))
    
    plt.fill_between(X, mu + uncertainty, mu - uncertainty, alpha=0.1)
    plt.plot(X, mu, label= label)
    for i, sample in enumerate(samples):
        plt.plot(X, sample, lw=1, ls='--', label=f'Sample {i+1}')
    if X_train is not None:
        plt.plot(X_train, Y_train, 'rx')
    plt.legend()

def RBFkernel(l, sigma, scaling_factor, noise):
    rbf = ConstantKernel(sigma) * RBF(length_scale=l)
    gpr = GaussianProcessRegressor(kernel=rbf, alpha=scaling_factor*noise)
    return gpr
