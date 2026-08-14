"""

This is a python script with the correlations to be updated

@author G. Nicodemo

"""

""" ------------------- Import requiered depedencies ------------------- """
import numpy as np

def Turnbull_D(T,F):
    kB = 1.380649 * 1e-23
    return 7.6e-10 * np.exp(-4.86e-19/(kB *T)) + 5.64e-25 * np.sqrt(F) * np.exp(-1.91e-19/(kB *T)) + 8e-40 * F