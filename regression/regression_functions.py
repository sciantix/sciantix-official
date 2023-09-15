"""

This is a python script to execute the regression (running the validation database) of sciantix.

@author G. Zullo

"""

import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt
import shutil

""" Defining useful functions"""
def import_data(filename):
  """
  This function import a .txt file into an ndarray
  """
  data = np.genfromtxt(filename, dtype= 'str', delimiter='\t')
  return data

def findSciantixVariablePosition(output, variable_name):
  """
  This function gets the output.txt file and the variable name,
  giving back its column index in the ndarray
  """
  i,j = np.where(output == variable_name)
  return int(j)

def plot(x, y):
    """
    Parameters
    ----------
    x : 
      TYPE = ndarray
      1st element of the array = name of the variable

    y : 
      TYPE = ndarray
      1st element of the array = name of the variable

    Returns
    -------
    Plot of x - y
    """
    
    # Setting the plot
    fig, ax = plt.subplots()

    ax.plot(x[1:].astype(float), y[1:].astype(float), color = 'blue')
    ax.set_xlabel(x[0])
    ax.set_ylabel(y[0])
    
    plt.show()

def are_files_equal(file1, file2):
  if os.path.exists(file1) is True and os.path.exists(file2) is True:
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
      return f1.read() == f2.read()
  elif os.path.exists(file1) is False:
    print("ERROR!")
    print(file1, "not found.")
  elif os.path.exists(file2) is False:
    print("ERROR!")
    print(file2, "not found.")
