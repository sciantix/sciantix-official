"""

This is a python script with the correlations to be updated

@author G. Nicodemo

"""

""" ------------------- Import requiered depedencies ------------------- """
import numpy as np

def Turnbull_D(T,F):
    kB = 1.380649 * 1e-23
    return 7.6e-10 * np.exp(-4.86e-19/(kB *T)) + 5.64e-25 * np.sqrt(F) * np.exp(-1.91e-19/(kB *T)) + 8e-40 * F

def Magni_MOX_Tmelt(x, Pu):
    gOM = 1014.15
    gPu = 364.85
    T0 = 3147 #(K)
    return T0 - gOM * x - gPu * Pu

def DiGennaro_MOX_Tmelt(x, Pu):
    x = x + 2
    maxOM = 2 - 0.235 * Pu
    return 3132 - 222.84 * Pu + 132.12 * Pu **2 - (283.72 * Pu - 282.79 * Pu **2) * ((np.abs(x - maxOM))/(2 - maxOM))