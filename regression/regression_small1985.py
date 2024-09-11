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

# Data generated from SCIANTIX 2.0
FGR2 = []
FGRBase = []
FGRBaseGold = []
BaseTime = [202,202,202,
            202,202,202,
            202,202,202,
            202,202,202]
# Data from Small 1985

FGROperational = [0.5,0.5,0.5,    #1400, 300-600-900 s annealing
                0.5,0.5,0.5,    #1500, 300-600-900 s annealing
                0.5,0.5,0.5,    #1600, 300-600-900 s annealing
                0.5,0.5,0.5  #1700, 300-600-900 s annealing
              ]
FGRAnnealing = [0.01,0.01,0.34,    #1400, 300-600-900 s annealing
                0.58,0.05,0.09,    #1500, 300-600-900 s annealing
                2.75,1.93,5.30,    #1600, 300-600-900 s annealing
                10.84,44.50,37.90  #1700, 300-600-900 s annealing
              ]

FGRSmall = [op + ann for op, ann in zip(FGROperational, FGRAnnealing)]

AnnFGRVersion2 = [16.298329999712923, 16.298329999712923, 16.298329999712923, 22.215839999712923, 25.475369999712925, 27.336879999712924, 32.40388999971292, 34.89426999971292, 36.262389999712916, 37.655079999712925, 39.69122999971292, 40.76714999971292]
TOtFGRVersion2 = [16.29833, 16.29833, 16.29833, 22.21584, 25.47537, 27.33688, 32.403890000000004, 34.89427, 36.262389999999996, 37.655080000000005, 39.69123, 40.76715]

goldFGR = []

number_of_tests_failed = 0
sample_number = len(FGR2)


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
  # Data vs. SCIANTIX 2.0
  fig, ax = plt.subplots()

  ax.scatter(FGRSmall, FGR2,c = '#9370DB', marker = '^', s=30, label='This work')
  ax.scatter(FGRSmall, goldFGR, c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017)', alpha=0.7)
  ax.scatter(FGRSmall, TOtFGRVersion2, c='green', marker = 'd', s=30, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
  ax.plot([0, 100],[0, 100], '-', color = '#757575')
  ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')
  
  ax.set_xlim(0, 100)
  ax.set_ylim(0, 100)

  ax.set_title('Fission gas release')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)

  
  plt.savefig('FGRTotal-Small1985')
  plt.show()

  fig, ax = plt.subplots()

  FGR2Annealing = []
  goldFGRAnnealing = []
  for i in range(len(FGR2)):
    FGR2Annealing.append(FGR2[i] - FGRBase[i])
    goldFGRAnnealing.append(goldFGR[i] - FGRBaseGold[i])
    
  ax.scatter(FGRAnnealing, FGR2Annealing,c = '#9370DB', marker = '^', s=30, label='This work', zorder=1)
  ax.scatter(FGRAnnealing, goldFGRAnnealing, c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017)', zorder=2, alpha=0.7)
  ax.scatter(FGRAnnealing, AnnFGRVersion2, c='green', marker = 'd', s=30, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
  ax.plot([0, 100],[0, 100], '-', color = '#757575')
  ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')
  
  ax.set_xlim(0, 50)
  ax.set_ylim(0, 50)

  ax.set_title('Fission gas release - Annealing phase')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)

  
  plt.savefig('FGRAnnealing-Small1985')
  plt.show()


# Main function of the baker regression
def regression_small1985(wpath, mode_Small1985, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):
  k=0
  # Exit of the function without doing anything
  if mode_Small1985 == 0 :
    return folderList, number_of_tests, number_of_tests_failed

  # Get list of all files and directories in wpath
  files_and_dirs = os.listdir(wpath)

  # Sort them by filename
  sorted_files_and_dirs = sorted(files_and_dirs)

  # Iterate over sorted list
  for file in sorted_files_and_dirs:
    # Verify on a given folder, if Baker is in it's name
    if "Small1985" in file and os.path.isdir(file):
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

      # Retrieve the gold data of Fission gas release
      FGRGoldPos = findSciantixVariablePosition(data_gold, "Fission gas release (/)")
      goldFGR.append(100*data_gold[-1,FGRGoldPos].astype(float))

      # Retrieve the FGR Base
      FGRBase.append(100*data[BaseTime[k],FGRPos].astype(float))
      FGRBaseGold.append(100*data_gold[BaseTime[k],FGRGoldPos].astype(float))
      k +=1

      os.chdir('..')

  # Check if the user has chosen to display the various plots
  if mode_plot == 1:
    do_plot()

  return folderList, number_of_tests, number_of_tests_failed
