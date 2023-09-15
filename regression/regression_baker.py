"""

This is a python script to execute the regression (running the validation database) of sciantix.

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

# Intergranular gaseous swelling database from Baker 1977 experiments
igSwellingBaker = [0.06, 0.07, 0.08, 0.09, 0.12, 0.15, 0.18, 0.24, 0.31]
# Data from SCIANTIX 1.0
igSwelling1 = [0.033, 0.048, 0.062, 0.073, 0.079, 0.082, 0.083, 0.084, 0.086]
# Data generated from SCIANTIX 2.0
igSwelling2 = []
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
  # SCIANTIX 1.0 vs. SCIANTIX 2.0
  fig, ax = plt.subplots()

  ax.scatter(igSwellingBaker, igSwelling1, c = '#FA82B4', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 1.0')
  ax.scatter(igSwellingBaker, igSwelling2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 2.0')

  ax.plot([1e-3, 1e2],[1e-3, 1e2], '-', color = '#757575')
  ax.plot([1e-3, 1e2],[2e-3, 2e2],'--', color = '#757575')
  ax.plot([1e-3, 1e2],[5e-4, 5e1],'--', color = '#757575')
  ax.set_xlim(1e-2, 1e1)
  ax.set_ylim(1e-2, 1e1)

  ax.set_xscale('log')
  ax.set_yscale('log')

  # ax.set_title('Intragranular gaseous swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()

  plt.show()


  # GOLD vs. SCIANTIX 2.0
  fig, ax = plt.subplots()

  ax.scatter(igSwellingBaker, gold, c = '#C9C954', edgecolors= '#999AA2', marker = 'o', s=20, label='Gold')
  ax.scatter(igSwellingBaker, igSwelling2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 2.0')

  ax.plot([1e-3, 1e2],[1e-3, 1e2], '-', color = '#757575')
  ax.plot([1e-3, 1e2],[2e-3, 2e2],'--', color = '#757575')
  ax.plot([1e-3, 1e2],[5e-4, 5e1],'--', color = '#757575')

  ax.set_xlim(1e-2, 1e1)
  ax.set_ylim(1e-2, 1e1)

  ax.set_xscale('log')
  ax.set_yscale('log')

  # ax.set_title('Intergranular gaseous swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()

  plt.show()

  # Fission gases release plot
  fig, ax = plt.subplots()
  ax.scatter(igSwelling2, FGR2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='FGR SCIANTIX 2.0')

  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.set_xlabel('Swelling (%)')
  ax.set_ylabel('FGR (%)')
  ax.legend()

  plt.show()


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
