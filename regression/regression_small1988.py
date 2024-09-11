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
BaseTime = [303,303,303,303,303,303,
            303,303,303,303,303,303,
            303,303,303,
            303,303,303,303]
# Data from Small 1988

FGROperational = [0.2, 0.2, 0.2, 0.2, 0.2, 0.2, #Expt A
                  0.2, 0.2, 0.2, 0.2, 0.2, 0.2, #Expt B
                  0.2, 0.2, 0.2, #Expt C
                  0.2, 0.2, 0.2, 0.2 #Expt D
        ] 
FGRAnnealing = [0, 0, 3.1, 2.8, 8.8, 11.1, #Expt A
                7.2, 9.3, 20.3, 28, 66.9, 62.8, #Expt B
                5.3, 39.8, 49.9, #Expt C
                0, 9.6, 21.4, 13.7 #Expt D
        ]

FGRSmall = [op + ann for op, ann in zip(FGROperational, FGRAnnealing)]

AnnFGRVersion2 =  [20.213013000000004, 28.704663000000004, 33.402063, 20.519613, 39.033473, 40.719743, 30.681779834495295, 38.970599999707446, 43.02848999970745, 45.47409999970745, 47.03757999970745, 48.06757999970745, 20.238650000000003, 24.628509999999995, 30.965970000000002, 27.400672999999998, 32.341823, 35.912853, 30.270893]
TOtFGRVersion2 = [25.411250000000003, 33.9029, 38.6003, 25.71785, 44.23171, 45.91798, 30.681779999999996, 38.9706, 43.028490000000005, 45.4741, 47.03758, 48.06758, 48.33234, 52.722199999999994, 59.05966, 32.59891, 37.54006, 41.11109, 35.46913]



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
  # FGR total
  # fig, ax = plt.subplots()

  # ax.scatter(FGRSmall, FGR2, c = '#9370DB', marker = '^', s=40,label='This work')
  # ax.scatter(FGRSmall, goldFGR, c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)', alpha=0.7)
  # ax.scatter(FGRSmall, TOtFGRVersion2, c='green', marker = 'd', s=40, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
  # ax.plot([0, 100],[0, 100], '-', color = '#757575')
  # ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  # ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')
  
  # ax.set_xlim(0, 100)
  # ax.set_ylim(0, 100)

  # ax.set_title('Fission gas release')
  # ax.set_xlabel('Experimental (%)')
  # ax.set_ylabel('Calculated (%)')
  # ax.legend()
  # ax.grid(color='gray', linestyle='--', linewidth=0.5)

  
  # plt.savefig('FGRTotal-Small1988')
  # plt.show()

  FGR2Annealing = []
  goldFGRAnnealing = []
  for i in range(len(FGR2)):
    FGR2Annealing.append(FGR2[i] - FGRBase[i])
    goldFGRAnnealing.append(goldFGR[i] - FGRBaseGold[i])
    
   #FGR annealing
  fig, ax = plt.subplots()
  ax.scatter(FGRAnnealing, FGR2Annealing,c = '#9370DB', marker = '^', s=40,label='This work')
  #ax.scatter(FGRAnnealing, goldFGRAnnealing,c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)', zorder=2, alpha=0.7)
  ax.scatter(FGRAnnealing, AnnFGRVersion2, c='green', marker = 'd', s=40, label='SCIANTIX 2.0', alpha =0.7)
  
  ax.plot([0, 100],[0, 100], '-', color = '#757575')
  ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')
  
  ax.set_xlim(0, 80)
  ax.set_ylim(0, 80)

  ax.set_title('Fission gas release - Annealing phase')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  
  plt.savefig('FGRAnnealing-Small1988')
  plt.show()

          # Median absolute deviations
  deviations_2 = abs(np.array(FGRAnnealing) - np.array(FGR2Annealing))
  deviations_Version2 = abs(np.array(FGRAnnealing) - np.array(AnnFGRVersion2))
  deviations_gold = abs(np.array(FGRAnnealing)-np.array(goldFGRAnnealing))
  
  print('FGR annealing')
  print(f"This work - MAD: ", np.median(deviations_2))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_Version2))
  print(f"Barani (2017) - MAD: ", np.median(deviations_gold))

# Main function of the baker regression
def regression_small1988(wpath, mode_Small1988, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):
  k=0
  # Exit of the function without doing anything
  if mode_Small1988 == 0 :
    return folderList, number_of_tests, number_of_tests_failed

  # Get list of all files and directories in wpath
  files_and_dirs = os.listdir(wpath)

  # Sort them by filename
  sorted_files_and_dirs = sorted(files_and_dirs)

  # Iterate over sorted list
  for file in sorted_files_and_dirs:
    # Verify on a given folder, if Baker is in it's name
    if "Small1988" in file and os.path.isdir(file):
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
