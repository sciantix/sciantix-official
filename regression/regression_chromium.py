"""

This is a python script to execute the regression (running the validation database) of sciantix.

@author G. Nicodemo

"""

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
def do_plot(RigletMartial1_data, RigletMartial2_data, Killeen_data, temperature, CrContent, FGR, FIMA, CrContentG, temperatureG, FGRG, FIMAG):
  
  """ Plot: Chromium atoms in UO2 lattice """
  fig, ax = plt.subplots()
  
  conv_factor =  52 * 100 / 6.022e23 / 1.07998e7 
  print(conv_factor)

  ax.scatter(RigletMartial1_data[:,1], RigletMartial1_data[:,0] /conv_factor , marker = '.', c = '#B3B3B3', label='Data from Riglet-Martial et al. (2016)')
  ax.plot(temperatureG, CrContentG, 'k', label='SCIANTIX GOLD')
  ax.plot(temperature, CrContent, color = '#98E18D', label='SCIANTIX 2.0')

  # ax.set_title(file + ' - Chromium content in the lattice')
  ax.set_xlabel('Temperature (K)')
  ax.set_ylabel('Chromium content in the lattice (at/m3)')
  ax.legend()
  
  plt.show()
  
  fig, ax = plt.subplots()

  ax.scatter(RigletMartial2_data[:,1], RigletMartial2_data[:,0] / conv_factor, marker = '.', c = '#B3B3B3', label='Data from Riglet-Martial et al. (2016)')
  ax.plot(temperatureG, CrContentG, 'k', label='SCIANTIX GOLD')
  ax.plot(temperature, CrContent, color = '#98E18D', label='SCIANTIX 2.0')

  # ax.set_title(file + ' - Chromium content in the lattice')
  ax.set_xlabel('Temperature (K)')
  ax.set_ylabel('Chromium content in the lattice (at/m3)')
  ax.legend()
  
  plt.show()

  """ Plot: Fission gas release """
  fig, ax = plt.subplots()
  
  ax.scatter(Killeen_data[:,1], Killeen_data[:,0]/100, marker = '.', c = '#B3B3B3', label='Data from Killeen et al. (1980)')
  ax.plot(FIMAG, FGRG, 'k', label='SCIANTIX GOLD')
  ax.plot(FIMA, FGR, color = '#98E18D', label='SCIANTIX 2.0')

  # ax.set_title(file + ' - Release rate')
  ax.set_xlabel('Burnup FIMA (%)')
  ax.set_ylabel('Fission gas release (/)')
  ax.legend()

  # plt.savefig(file + '.png')
  plt.show()


# Main function of the Chromium regression
def regression_chromium(wpath, mode_Chromium, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):

  # Exit of the function without doing anything
  if mode_Chromium == 0:
    return folderList, number_of_tests, number_of_tests_failed

  # Get list of all files and directories in wpath
  files_and_dirs = os.listdir(wpath)

  # Sort them by filename
  sorted_files_and_dirs = sorted(files_and_dirs)
  #print(sorted_files_and_dirs)

  # Iterate over sorted list
  for file in sorted_files_and_dirs:
    # Verify on a given folder, if Baker is in it's name
    if "Chromium" in file and os.path.isdir(file):
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

      RigletMartial1_data = np.genfromtxt("Riglet-Martial_data_exp1.txt")
      RigletMartial2_data = np.genfromtxt("Riglet-Martial_data_exp2.txt")
      Killeen_data = np.genfromtxt("Killeen_exp.txt")

      # output.txt
      # find indexes
      timePos = findSciantixVariablePosition(data, "Time (h)")
      temperaturePos = findSciantixVariablePosition(data, "Temperature (K)")
      ChromiumContentPos = findSciantixVariablePosition(data, "Chromium solution (at/m3)")
      ChromiaContentPos = findSciantixVariablePosition(data, "Chromia solution (at/m3)")
      FGRPos = findSciantixVariablePosition(data, "Fission gas release (/)")
      FIMAPos = findSciantixVariablePosition(data, "FIMA (%)")

      # arrays
      #time = data[1:,timePos].astype(float)
      temperature = data[1:,temperaturePos].astype(float)
      CrContent = data[1:,ChromiumContentPos].astype(float) + data[1:,ChromiaContentPos].astype(float)
      FGR = data[1:,FGRPos].astype(float)
      FIMA = data[1:,FIMAPos].astype(float)

      # output_gold.txt
      # find indexes
      timePosG = findSciantixVariablePosition(data_gold, "Time (h)")
      temperaturePosG = findSciantixVariablePosition(data_gold, "Temperature (K)")
      ChromiumContentPosG = findSciantixVariablePosition(data_gold, "Chromium solution (at/m3)")
      ChromiaContentPosG = findSciantixVariablePosition(data_gold, "Chromia solution (at/m3)")
      FGRPosG = findSciantixVariablePosition(data_gold, "Fission gas release (/)")
      FIMAPosG = findSciantixVariablePosition(data_gold, "FIMA (%)")

      # arrays
      #timeG = data_gold[1:,timePosG].astype(float)
      temperatureG = data_gold[1:,temperaturePosG].astype(float)
      CrContentG = data_gold[1:,ChromiumContentPosG].astype(float) + data_gold[1:,ChromiaContentPosG].astype(float)
      FGRG = data_gold[1:,FGRPosG].astype(float)
      FIMAG = data_gold[1:,FIMAPosG].astype(float)

      # Check if the user has chosen to display the various plots
      if mode_plot == 1:
        do_plot(RigletMartial1_data, RigletMartial2_data, Killeen_data, temperature, CrContent, FGR, FIMA, CrContentG, temperatureG, FGRG, FIMAG)

      os.chdir('..')

  return folderList, number_of_tests, number_of_tests_failed








