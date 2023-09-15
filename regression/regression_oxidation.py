"""

This is a python script to execute the regression (running the validation database) of sciantix.

@author G. Zullo

"""

""" UO2 stiochiometry deviation: calculations and experimental data """


""" ------------------- Import requiered depedencies ------------------- """

import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt
import shutil
from regression_functions import *

""" ------------------- Global Variables ------------------- """

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
  #shutil.copy("../input_settings.txt", os.getcwd())
  #shutil.copy("../input_scaling_factors.txt", os.getcwd())

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
def do_plot(time, temperature, stiochiometryDeviation, stiochiometryDeviationData, file):
  fig, ax = plt.subplots()

  ax.plot(stiochiometryDeviationData[1:,0].astype(float), stiochiometryDeviationData[1:,1].astype(float), 'k.', label='Data')
  ax.plot(time, stiochiometryDeviation, 'g.', label='SCIANTIX 2.0')

  axT = ax.twinx()
  axT.set_ylabel('Temperature (K)')
  axT.plot(time, temperature, label="Temperature",  color='orange')

  ax.set_title(file + ' - Stoichiometry deviation')
  ax.set_xlabel('Time (h)')
  ax.set_ylabel('Stoichiometry deviation (/)')
  h1, l1 = ax.get_legend_handles_labels()
  h2, l2 = axT.get_legend_handles_labels()
  ax.legend(h1+h2, l1+l2)

  plt.savefig(file + ' - Stoichiometry deviation.png')
  plt.show()



# Main function of the oxidation regression
def regression_oxidation(wpath, mode_oxidation, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):

  # Exit of the function without doing anything
  if mode_oxidation == 0:
    return folderList, number_of_tests, number_of_tests_failed

  # Get list of all files and directories in wpath
  files_and_dirs = os.listdir(wpath)

  # Sort them by filename
  sorted_files_and_dirs = sorted(files_and_dirs)
  #print(sorted_files_and_dirs)

  # Iterate over sorted list
  for file in sorted_files_and_dirs:
    # Verify on a given folder, if Baker is in it's name
    if "oxidation" in file and os.path.isdir(file):
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



      # output.txt
      # find indexes
      timePos = findSciantixVariablePosition(data, "Time (h)")
      temperaturePos = findSciantixVariablePosition(data, "Temperature (K)")
      stiochiometryDeviationPos = findSciantixVariablePosition(data, "Stoichiometry deviation (/)")

      # arrays
      time = data[1:,timePos].astype(float)
      temperature = data[1:,temperaturePos].astype(float)
      stiochiometryDeviation = data[1:,stiochiometryDeviationPos].astype(float)

      try :
        stiochiometryDeviationData = import_data('data.txt')
      except :
        stiochiometryDeviationData = np.zeros_like(stiochiometryDeviation)


      # Check if the user has chosen to display the various plots
      if mode_plot == 1:
        do_plot(time, temperature, stiochiometryDeviation, stiochiometryDeviationData, file)

      os.chdir('..')

  return folderList, number_of_tests, number_of_tests_failed
