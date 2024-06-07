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

def sciantix_dictionary(file):
	data = import_data(file)

	variable_labels = {
		"t": "Time (h)",
		"T": "Temperature (K)",
		"frate": "Fission rate (fiss / m3 s)",
		"sigma": "Hydrostatic stress (MPa)",
		"bu": "Burnup (MWd/kgUO2)",
		"N": "Intergranular bubble concentration (bub/m2)",
		"n": "Intergranular atoms per bubble (at/bub)",
		"v": "Intergranular vacancies per bubble (vac/bub)",
		"rad": "Intergranular bubble radius (m)",
		"A": "Intergranular bubble area (m2)",
		"vol": "Intergranular bubble volume (m3)",
		"fc": "Intergranular fractional coverage (/)",
		"fcsat": "Intergranular saturation fractional coverage (/)",
		"f": "Intergranular fractional intactness (/)",
		"fv": "Intergranular vented fraction (/)",
		"pv": "Intergranular venting probability (/)",
		"xe_p": "Xe produced (at/m3)",
		"xe_pHBS": "Xe produced in HBS (at/m3)",
		"xe_ig": "Xe in grain (at/m3)",
		"xe_igHBS": "Xe in grain HBS (at/m3)",
		"xe_igb": "Xe in intragranular bubbles (at/m3)",
		"xe_igs": "Xe in intragranular solution (at/m3)",
		"xe_gb": "Xe at grain boundary (at/m3)",
		"xe_r": "Xe released (at/m3)",
		"fgr" : "Fission gas release (/)",
		"alpha": "Restructured volume fraction (/)",
		"sv": "Intergranular S/V (1/m)",
		"swe_gb" : "Intergranular gas swelling (/)",
		"swe_igb": "Intragranular gas bubble swelling (/)",
		"swe_igs": "Intragranular gas solution swelling (/)",
		"a": "Grain radius (m)",
		"fima": "FIMA (%)",
	}

	sd = {}
	label = []
	name = []

	for var_name, var_label in variable_labels.items():
		label.append(var_label)
		name.append(var_name)

	for i in range(len(label)):
		try:
			var_index = findSciantixVariablePosition(data, label[i])
		except:
			var_index = None
		if var_index is not None:
			sd[name[i]] = data[1:, var_index].astype(float)
		else:
			sd[name[i]] = np.zeros_like(data[1:, 0].astype(float))
			print(f"Variable '{label[i]}' not found in the data.")
	
	return sd