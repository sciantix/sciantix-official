"""

This is a python script to execute the regression (running the validation database) of sciantix.

@author G. Zullo

"""

""" ------------------- Import requiered depedencies ------------------- """

import os
import subprocess
import numpy as np
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import shutil
from regression_functions import *

""" ------------------- Global Variables ------------------- """

""" ------------------- Functions ------------------- """

# Verify the test results
def check_result(number_of_tests_failed):
  if are_bounds_files_equal('output.txt', 'output_low_henry.txt', 'output_up_henry.txt', 'output_gold.txt') == True:
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
    data_low = import_data("output_low_henry.txt")
  except :
    print(f"output_low_henry.txt not found in {file}")
    data_low = np.ones(shape=(1, 1))

  try :
    data_up = import_data("output_up_henry.txt")
  except :
    print(f"output_up_henry.txt not found in {file}")
    data_up = np.ones(shape=(1, 1))

  try :
    data_gold = import_data("output_gold.txt")
  except :
    print(f"output_gold.txt not found in {file}")
    data_gold = np.ones(shape=(1, 1))
      

  return data, data_low, data_up, data_gold

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
def do_plot(time, temperature, heSingleConcen, heBubbleConcen,
            timeG, heSingleConcenG, temperatureG, heBubbleConcenG, timeB, heSingleConcenLB, heSingleConcenUP, temperatureB, heBubbleConcenLB, heBubbleConcenUB):
  fig, ax = plt.subplots(1,2)

  plt.subplots_adjust(left=0.1,
                      bottom=0.1,
                      right=0.9,
                      top=0.9,
                      wspace=0.34,
                      hspace=0.4)
  ax[0].plot(timeG, heSingleConcenG, 'k', label='SCIANTIX 2.0')
  ax[0].plot(time, heBubbleConcenG, color = '#98E18D', label='SCIANTIX 2.0')
  ax[0].plot(timeB, heSingleConcenLB, color = '#7AC5CD', linestyle ='--', label = 'Lower Bound')
  ax[0].plot(timeB, heSingleConcenUP, color = '#FF7F50', linestyle ='--', label = 'Upper Bound')


  axT = ax[0].twinx()
  axT.set_ylabel('Temperature (K)')
  axT.plot(time, temperature, 'r', linewidth=1, label="Temperature")

  # ax.set_title(file + ' - Fractional release')
  ax[0].set_xlabel('Time (h)')
  ax[0].set_ylabel('Helium fractional release (/)')
  h1, l1 = ax[0].get_legend_handles_labels()
  h2, l2 = axT.get_legend_handles_labels()
  # ax[0].legend(h1+h2, l1+l2)
  ax[0].legend(loc = 'upper left')

  """ Plot: Helium release rate """
  ax[1].plot(temperatureG, heSingleConcenG, 'k', label='Cognini et al. (2021)')
  ax[1].plot(temperature, heBubbleConcenG, color = '#98E18D', label='SCIANTIX 2.0')
  ax[1].plot(temperatureB, heBubbleConcenLB, color = '#7AC5CD', linestyle ='--', label = 'Lower Bound')
  ax[1].plot(temperatureB, heBubbleConcenUB, color = '#FF7F50', linestyle ='--', label = 'Upper Bound')
  

  # ax.set_title(file + ' - Release rate')
  ax[1].set_xlabel('Temperature (K)')
  ax[1].set_ylabel('Helium release rate (at m${}^{-3}$ s${}^{-1}$)')
  ax[1].legend()

  # plt.savefig(file + '.png')
  plt.show()


# Main function of the Talip regression
def regression_talip(wpath, mode_Talip, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):

  # Exit of the function without doing anything
  if mode_Talip == 0:
    return folderList, number_of_tests, number_of_tests_failed

  # Get list of all files and directories in wpath
  files_and_dirs = os.listdir(wpath)

  # Sort them by filename
  sorted_files_and_dirs = sorted(files_and_dirs)
  #print(sorted_files_and_dirs)

  # Iterate over sorted list
  for file in sorted_files_and_dirs:
    # Verify on a given folder, if Baker is in it's name
    if "Talip" in file and os.path.isdir(file):
      folderList.append(file)
      os.chdir(file)

      print(f"Now in folder {file}...")
      number_of_tests += 1


      # mode_gold = 0 : Use SCIANTIX / Don't use GOLD and check result
      if mode_gold == 0:

        do_sciantix()
        data, data_low, data_up, data_gold = check_output(file)
        number_of_tests_failed = check_result(number_of_tests_failed)

      # mode_gold = 1 : Use SCIANTIX / Use GOLD
      if mode_gold == 1:

        do_sciantix()
        data, data_low, data_up, data_gold = check_output(file)
        print("...golding results.")
        do_gold()

      # mode_gold = 2 : Don't use SCIANTIX / Don't use GOLD and check result
      if mode_gold == 2:

        data, data_low, data_up, data_gold = check_output(file)
        number_of_tests_failed = check_result(number_of_tests_failed)

      # mode_gold = 3 : Don't use SCIANTIX / Use GOLD
      if mode_gold == 3:

        data, data_low, data_up, data_gold = check_output(file)
        print("...golding existing results.")
        do_gold()

      try :
        data_Cognini = import_data("output_Cognini.txt")
      except :
        print(f"output_Cognini.txt not found in {file}")
        data_Cognini = np.zeros(shape=(1, 1))


      # output.txt
      # find indexes
      timePos = findSciantixVariablePosition(data, "Time (h)")
      temperaturePos = findSciantixVariablePosition(data, "Temperature (K)")
      hePos = findSciantixVariablePosition(data, "He fractional release (/)")
      heReleasedPos = findSciantixVariablePosition(data, "He fractional release (/)")
      heReleasedRatePos = findSciantixVariablePosition(data, "He release rate (at/m3 s)")


      # arrays
      time = data[1:,timePos].astype(float)
      temperature = data[1:,temperaturePos].astype(float)
      heReleasedFrac = data[1:,heReleasedPos].astype(float)
      heReleaseRate = data[1:,heReleasedRatePos].astype(float)

      # output_gold.txt
      # find indexes
      timePosG = findSciantixVariablePosition(data_Cognini, "Time (h)")
      heReleasedPosG = findSciantixVariablePosition(data_Cognini, "He fractional release (/)")
      heReleasedRatePosG = findSciantixVariablePosition(data_Cognini, "He release rate (at/m3 s)")
      temperaturePosG = findSciantixVariablePosition(data_Cognini, "Temperature (K)")

      # arrays
      timeG = data_Cognini[1:,timePosG].astype(float)
      heSingleConcenG = data_Cognini[1:,heReleasedPosG].astype(float)
      temperatureG = data_Cognini[1:,temperaturePosG].astype(float)
      heBubbleConcenG = data_Cognini[1:,heReleasedRatePosG].astype(float)

      # output_low.txt
      # find indexes
      timePosB = findSciantixVariablePosition(data_low, "Time (h)")
      heReleasedPosLB = findSciantixVariablePosition(data_low, "He fractional release (/)")
      heReleasedRatePosLB = findSciantixVariablePosition(data_low, "He release rate (at/m3 s)")
      temperaturePosB = findSciantixVariablePosition(data_low, "Temperature (K)")

      # arrays
      timeB = data_low[1:,timePosB].astype(float)
      heSingleConcenLB = data_low[1:,heReleasedPosLB].astype(float)
      temperatureB = data_low[1:,temperaturePosB].astype(float)
      heBubbleConcenLB = data_low[1:,heReleasedRatePosLB].astype(float)

      # output_low.txt
      heReleasedPosUB = findSciantixVariablePosition(data_up, "He fractional release (/)")
      heReleasedRatePosUB = findSciantixVariablePosition(data_up, "He release rate (at/m3 s)")

      heSingleConcenUP = data_up[1:,heReleasedPosUB].astype(float)
      heBubbleConcenUB = data_up[1:,heReleasedRatePosUB].astype(float)







      # Check if the user has chosen to display the various plots
      if mode_plot == 1:
        do_plot(time, temperature, heReleasedFrac, heReleaseRate,
                timeG, heSingleConcenG, temperatureG, heBubbleConcenG, timeB, heSingleConcenLB, heSingleConcenUP, temperatureB, heBubbleConcenLB, heBubbleConcenUB)


      os.chdir('..')

  return folderList, number_of_tests, number_of_tests_failed








