"""

This is a python script to execute the regression (running the validation database) of SCIANTIX.

@author G. Zullo

"""

""" ------------------- Import requiered depedencies ------------------- """

import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt
import shutil
from regression_functions import *
import scipy.stats as stats
from sklearn.linear_model import LinearRegression

""" ------------------- Global Variables ------------------- """

# Intragranular gaseous swelling database from Baker 1977 experiments
igSwellingBaker = np.array([0.06, 0.07, 0.08, 0.09, 0.12, 0.15, 0.18, 0.24, 0.31])
igSwellingError = igSwellingBaker * np.sqrt(0.3**2 + 0.5**2)

# Data from SCIANTIX 1.0
igSwelling1 = np.array([0.033, 0.048, 0.062, 0.073, 0.079, 0.082, 0.083, 0.084, 0.086])
igRadius1 = np.array([0.515, 0.607, 0.681, 0.733, 0.763, 0.776, 0.782, 0.784, 0.785]) # nm
igDensity1 = np.array([5.78, 5.14, 4.69, 4.42, 4.27, 4.2, 4.18, 4.17, 4.17]) #10^23 bub/m3

# Data from SCIANTIX 2.0 Turnbull and GP update
iXeDiffTurnbull  = np.array([4.275988e-20, 8.863954e-20, 1.885549e-19, 4.235173e-19, 1.018579e-18, 2.549942e-18, 6.346746e-18, 1.518164e-17, 3.434229e-17])
iXeDiffGPupdated = np.array([6.20967e-20, 1.818894e-19, 4.14139e-19, 1.144568e-18, 3.698875e-18, 1.134597e-17, 3.047513e-17, 7.407139e-17, 1.70611e-16])
temperature = np.array([1273, 1373, 1473, 1573, 1673, 1773, 1873, 1973, 2073])
upper_error = 10 * iXeDiffTurnbull
lower_error = iXeDiffTurnbull *9 / 10

# Data generated from SCIANTIX 2.0
igSwelling2 = []

# Intragranular bubble density from Baker 1977 experiments
igDensityBaker = np.array([8.7, 7.8, 7, 6.4, 5.7, 5.3, 4.8, 4.4, 3.8]) #1e23 bub/m3
igDensityError = igDensityBaker * 0.3
igDensity2 = []

igRadiusBaker = np.array([0.55e-9, 0.6e-9, 0.65e-9, 0.70e-9, 0.80e-9, 0.88e-9, 0.98e-9, 1.10e-9, 1.25e-9])
igRadiusError = igRadiusBaker * 0.5

igRadius2 = []
FGR2 = []

number_of_tests_failed = 0

gold = []
sample_number = len(igSwelling1)

""" ------------------- Functions ------------------- """

# Verify the test results
def check_result(number_of_tests_failed):
  if are_files_equal('output.txt', 'output_gold.txt') == True:
    print(f"Test passed!\n")
  else:
    print(f"Test failed!\n")
    number_of_tests_failed += 1

  return number_of_tests_failed

# Verify the existence of the files: output.txt and output_gold.txt
def check_output(file):
  try :
    data = import_data("output.txt")
  except :
    print(f"output.txt not found in {file}")
    data = np.zeros(shape=(1, 1))

  try :
    data_gold = import_data("output_gold.txt")
  except :
    print(f"output_gold.txt not found in {file}")
    data_gold = np.ones(shape=(1, 1))

  return data, data_gold

# Execute sciantix in the current test folder
def do_sciantix():
  # copying input files from the regression folder into the current folder
  shutil.copy("../input_settings.txt", os.getcwd())
  shutil.copy("../input_scaling_factors.txt", os.getcwd())

  # copying and executing sciantix.exe into cwd
  shutil.copy("../sciantix.x", os.getcwd())
  os.system("./sciantix.x")

  # removing useless file
  os.remove("sciantix.x")
  os.remove("execution.txt")
  os.remove("input_check.txt")
  # os.remove("overview.txt")

# Replace the existing output_gold.txt with the new output.txt
def do_gold():
  try :
    os.path.exists('output.txt')

    os.remove('output_gold.txt')
    os.rename('output.txt', 'output_gold.txt')

  except :
    print(f"output.txt not found in {file}")

# Plot the regression test results
def do_plot():
  
  # SCIANTIX 2.0 Turbull diffusion coefficient vs. SCIANTIX 2.0 GPs updated diffusion coefficient: intragranular bubble swelling
  fig, ax = plt.subplots(1, 2, figsize=(12, 6))

  ax1 = ax[1]
  ax1.scatter(igSwellingBaker, gold, edgecolors='#757575', facecolors='red', marker='^', s=50, label='SCIANTIX Turnbull Xe diffusion coefficient', zorder=1)
  ax1.scatter(igSwellingBaker, igSwelling2, edgecolors='#757575', facecolors='green', marker='.', s=50, label='SCIANTIX updated Xe diffusion coefficient', zorder=2)
  ax1.plot([1e-3, 1e2], [1e-3, 1e2], '-', color='#757575')
  ax1.plot([1e-3, 1e2], [2e-3, 2e2], '--', color='#757575')
  ax1.plot([1e-3, 1e2], [5e-4, 5e1], '--', color='#757575')
  ax1.set_xlim(1e-2, 1)
  ax1.set_ylim(1e-2, 1)
  ax1.set_xscale('log')
  ax1.set_yscale('log')
  ax1.set_title('b) Predicted against measured intra-granular gas swelling')
  ax1.set_xlabel('Experimental (%)')
  ax1.set_ylabel('Calculated (%)')
  ax1.legend()

  ax2 = ax[0]
  
  ax2.errorbar(10 ** 4 / temperature, iXeDiffTurnbull, yerr=[lower_error, upper_error],edgecolors= None, color='red', marker = '.', fmt='^', capsize=4, capthick=1, ecolor='#999AA2', elinewidth = 0.6, label='Turnbull Xe diffusion coefficient', zorder=1)
  ax2.scatter(10 ** 4 / temperature, iXeDiffGPupdated, edgecolors='#757575', facecolors='green', marker='.', s=50, label='Updated Xe diffusion coefficient', zorder=2)
  ax2.set_yscale('log')
  ax2.set_title('a) Turnbull against updated Xe diffusion coefficient ')
  ax2.set_xlabel('$10^4/T$ (K)')
  ax2.set_ylabel('Diffusion coefficient ($m^2/s$)')
  ax2.legend()
  ax2.grid()
  
  plt.tight_layout()
  #plt.savefig('plot_GPupdate.png')
  plt.show()
  
  # Data vs. SCIANTIX 2.0: bubble density + errorbar
  fig, ax = plt.subplots()

  ax.errorbar(igDensityBaker, igDensity1, xerr = igDensityError, c = '#FA82B4', marker = '.', fmt='o', capsize=1, capthick=1, ecolor='#999AA2', elinewidth = 0.6, label='SCIANTIX 1.0')
  ax.errorbar(igDensityBaker, igDensity2, xerr = igDensityError, c = 'green', marker = '.', fmt='o', capsize=1, capthick=1, ecolor='#999AA2', elinewidth = 0.6, label='SCIANTIX 2.0')

  ax.plot([0.1, 100],[0.1, 100], '-', color = '#757575')
  ax.plot([0.1, 100],[0.05, 50],'--', color = '#757575')
  ax.plot([0.1, 100],[0.2, 200],'--', color = '#757575')
  ax.set_xlim(0.1, 100)
  ax.set_ylim(0.1, 100)

  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()

  #plt.show()
  #plt.close()

  # Data vs. SCIANTIX 2.0: bubble radius + errorbar
  fig, ax = plt.subplots()

  ax.errorbar(igRadiusBaker, igRadius1*1e-9, xerr = igRadiusError, c = '#FA82B4', marker = '.', fmt='o', capsize=1, capthick=1, ecolor='#999AA2', elinewidth = 0.6, label='SCIANTIX 1.0')
  ax.errorbar(igRadiusBaker, igRadius2, xerr = igRadiusError, c = 'green', marker = '.', fmt='o', capsize=1, capthick=1, ecolor='#999AA2', elinewidth = 0.6, label='SCIANTIX 2.0')

  ax.plot([0.1e-9, 100e-9],[0.1e-9, 100e-9], '-', color = '#757575')
  ax.plot([0.1e-9, 100e-9],[0.05e-9, 50e-9],'--', color = '#757575')
  ax.plot([0.1e-9, 100e-9],[0.2e-9, 200e-9],'--', color = '#757575')
  ax.set_xlim(0.1e-9, 100e-9)
  ax.set_ylim(0.1e-9, 100e-9)

  ax.set_xscale('log')
  ax.set_yscale('log')

  # ax.set_title('Intragranular gaseous swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()

  #plt.show()
  #plt.close()

# Fission gases release plot
#   fig, ax = plt.subplots()
#   ax.scatter(igSwelling2, FGR2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='FGR SCIANTIX 2.0')

#   ax.set_xscale('log')
#   ax.set_yscale('log')

#   ax.set_xlabel('Swelling (%)')
#   ax.set_ylabel('FGR (%)')
#   ax.legend()

#   plt.show()
#   plt.close()


# Main function of the baker regression
def regression_baker(wpath, mode_Baker, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):

  # Exit of the function without doing anything
  if mode_Baker == 0 :
    return folderList, number_of_tests, number_of_tests_failed

  # Get list of all files and directories in wpath
  files_and_dirs = os.listdir(wpath)

  # Sort them by filename
  sorted_files_and_dirs = sorted(files_and_dirs)

  # Iterate over sorted list
  for file in sorted_files_and_dirs:
    # Verify on a given folder, if Baker is in it's name
    if "Baker" in file and os.path.isdir(file):
      folderList.append(file)
      os.chdir(file)

      print(f"Now in folder {file}...")
      number_of_tests += 1

      # mode_gold = 0 : Use SCIANTIX / Don't use GOLD and check result
      if mode_gold == 0:

        do_sciantix()
        data, data_gold = check_output(file)
        number_of_tests_failed = check_result(number_of_tests_failed)

      # mode_gold = 1 : Use SCIANTIX / Use GOLD
      if mode_gold == 1:

        do_sciantix()
        data, data_gold = check_output(file)
        print("...golding results.")
        do_gold()

      # mode_gold = 2 : Don't use SCIANTIX / Don't use GOLD and check result
      if mode_gold == 2:

        data, data_gold = check_output(file)
        number_of_tests_failed = check_result(number_of_tests_failed)

      # mode_gold = 3 : Don't use SCIANTIX / Use GOLD
      if mode_gold == 3:

        data, data_gold = check_output(file)
        print("...golding existing results.")
        do_gold()

      # Retrieve the generated data of Fission gas release
      FGRPos = findSciantixVariablePosition(data, "Fission gas release (/)")
      FGR2.append(100*data[-1,FGRPos].astype(float))

      # Retrieve the generated data of Intragranular gas swelling
      intraGranularSwellingPos = findSciantixVariablePosition(data, "Intragranular gas swelling (/)")
      igSwelling2.append(100*data[-1,intraGranularSwellingPos].astype(float))

      # Retrieve the gold data of Intragranular gas swelling
      intraGranularSwellingGoldPos = findSciantixVariablePosition(data_gold, "Intragranular gas swelling (/)")
      gold.append(100*data_gold[-1,intraGranularSwellingGoldPos].astype(float))

      pos = findSciantixVariablePosition(data, "Intragranular bubble concentration (bub/m3)")
      igDensity2.append(1e-23*data[-1,pos].astype(float))

      pos = findSciantixVariablePosition(data, "Intragranular bubble radius (m)")
      igRadius2.append(data[-1,pos].astype(float))

      os.chdir('..')

  # Check if the user has chosen to display the various plots
  if mode_plot == 1:
    do_plot()

  """ Statistical analysis """
  # Experimental data: mean, median, ...
  print(f"Experimental data - mean: ", np.mean(igSwellingBaker))
  print(f"Experimental data - median: ", np.median(igSwellingBaker))
  print(f"Experimental data - Q1: ", np.percentile(igSwellingBaker, 25, interpolation = 'midpoint'))
  print(f"Experimental data - Q3: ", np.percentile(igSwellingBaker, 75, interpolation = 'midpoint'))

  # SCIANTIX 1.0: mean, median, ...
  print(f"SCIANTIX 1.0 - mean: ", np.mean(igSwelling1))
  print(f"SCIANTIX 1.0 - median: ", np.median(igSwelling1))
  print(f"SCIANTIX 1.0 - Q1: ", np.percentile(igSwelling1, 25, interpolation = 'midpoint'))
  print(f"SCIANTIX 1.0 - Q3: ", np.percentile(igSwelling1, 75, interpolation = 'midpoint'))

  # SCIANTIX 2.0: mean and median, ...
  print(f"SCIANTIX 2.0 - mean: ", np.mean(igSwelling2))
  print(f"SCIANTIX 2.0 - median: ", np.median(igSwelling2))
  print(f"SCIANTIX 2.0 - median: ", np.percentile(igSwelling2, 25, interpolation = 'midpoint'))
  print(f"SCIANTIX 2.0 - median: ", np.percentile(igSwelling2, 75, interpolation = 'midpoint'))

  # SCIANTIX 1.0 and 2.0 - Median absolute deviatiosns
  deviations_1 = abs(np.array(igSwellingBaker) - igSwelling1)
  deviations_2 = abs(np.array(igSwellingBaker) - igSwelling2)
  print(f"SCIANTIX 1.0 - MAD: ", np.median(deviations_1))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_2))

  # RMSE
  print(f"SCIANTIX 1.0 - RMSE: ", np.mean(np.array(igSwellingBaker) - igSwelling1)**2)
  print(f"SCIANTIX 2.0 - RMSE: ", np.mean(np.array(igSwellingBaker) - igSwelling2)**2)
  print("\n")

  return folderList, number_of_tests, number_of_tests_failed
