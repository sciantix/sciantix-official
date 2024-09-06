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

# # Data from SCIANTIX 1.0
# igSwelling1 = [0.033, 0.048]
# Data generated from SCIANTIX 2.0
FGR2 = []
FGRBase = []
FGRBaseGold = []
BaseTime = [1306,1306,
            1807,1807,
            1306,1306,1306,1306,1306,
            1807,1807,1807,1807]

# Data from Kashibe 1991
FGROperational = [22, 22, #23 GWd/t
                  43, 43, #28 GWd/t
                  22, 22, 22, 22, 22,
                  43, 43, 43, 43
                ]
FGRAnnealing = [5.9, 6.5, #1400°C, 23 GWd/t, Multiple-Single
                5.9, 7.6,  #1400°C, 28 GWd/t, Multiple-Single
                17.8,19.2,24,16.3,19.4, #1800°C 23 GWd/t, rate 1,2,3,4,5
                28.7,27.1,24,24.47 #1800°C 28 GWd/t, rate 1,2,4,5
              ]
FGRKashibe = [op + ann for op, ann in zip(FGROperational, FGRAnnealing)]


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

  ax.scatter(FGRKashibe, FGR2, c = '#9370DB', marker = '^', s=30, label='This work', zorder=1)
  ax.scatter(FGRKashibe, goldFGR, c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017)', zorder=2, alpha=0.7)
  
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
  
  plt.savefig('FGRTotal-Kashibe1991')
  plt.show()

  fig, ax = plt.subplots()

  FGR2Annealing = []
  goldFGRAnnealing = []
  for i in range(len(FGR2)):
    FGR2Annealing.append(FGR2[i] - FGRBase[i])
    goldFGRAnnealing.append(goldFGR[i] - FGRBaseGold[i])
    
  ax.scatter(FGRAnnealing, FGR2Annealing, c = '#9370DB', marker = '^', s=30, label='This work', zorder=1)
  ax.scatter(FGRAnnealing, goldFGRAnnealing, c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017)', zorder=2, alpha=0.7)
  
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
  
  plt.savefig('FGRAnnealing-Kashibe1991')
  plt.show()

# FGRAnnealing = [5.9, 6.5, #1400°C, 23 GWd/t, Multiple-Single
#                 5.9, 7.6,  #1400°C, 28 GWd/t, Multiple-Single
#                 17.8,19.2,24,16.3,19.4, #1800°C 23 GWd/t, rate 1,2,3,4,5
#                 28.7,27.1,24,24.47 #1800°C 28 GWd/t, rate 1,2,4,5
#               ]

  #GOLD vs. SCIANTIX 2.0, no error bars
  fig, ax = plt.subplots()

  ax.scatter(FGRAnnealing[4:9], FGR2Annealing[4:9],c='#9370DB', marker = '^', s=30,         label='This work - 1800°C - 23 GWd/tU', zorder = 1)
  ax.scatter(FGRAnnealing[4:9], goldFGRAnnealing[4:9],c='#ff7f0e', marker = '^', s=30,      label='Barani (2017) - 1800°C - 23 GWd/tU', zorder = 2)
  ax.scatter(FGRAnnealing[9:13], FGR2Annealing[9:13], c = '#9370DB', marker = 'o', s=30,    label='This work - 1800°C - 28 GWd/tU', zorder = 3)
  ax.scatter(FGRAnnealing[9:13], goldFGRAnnealing[9:13], c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017) - 1800°C - 28 GWd/tU', zorder = 4)
  
  ax.plot([0, 100],[0, 100], '-', color = '#757575')
  ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')
  
  ax.set_xlim(0, 40)
  ax.set_ylim(0, 40)

  ax.set_title('Fission gas release - Annealing phase')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)

  plt.savefig('FGRAnnealingDiviso-Kashibe1991')
  plt.show()

   #GOLD vs. SCIANTIX 2.0, no error bars
  fig, ax = plt.subplots()

  ax.scatter(FGRAnnealing[0:2], FGR2Annealing[0:2],c='#9370DB', marker = '^', s=30,         label='This work - 1400°C - 23 GWd/tU', zorder = 1)
  ax.scatter(FGRAnnealing[0:2], goldFGRAnnealing[0:2],c='#ff7f0e', marker = '^', s=30,      label='Barani (2017) - 1400°C - 23 GWd/tU', zorder = 2)
  ax.scatter(FGRAnnealing[2:4], FGR2Annealing[2:4], c = '#9370DB', marker = 'o', s=30,    label='This work - 1400°C - 28 GWd/tU', zorder = 3)
  ax.scatter(FGRAnnealing[2:4], goldFGRAnnealing[2:4], c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017) - 1400°C - 28 GWd/tU', zorder = 4)
  
  ax.plot([0, 100],[0, 100], '-', color = '#757575')
  ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')
  
  ax.set_xlim(0, 40)
  ax.set_ylim(0, 40)

  ax.set_title('Fission gas release - Annealing phase')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)

  plt.savefig('FGRAnnealingRamp-Kashibe1991')
  plt.show()


# Main function of the baker regression
def regression_kashibe1991(wpath, mode_Kashibe1991, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):
  k=0
  # Exit of the function without doing anything
  if mode_Kashibe1991 == 0 :
    return folderList, number_of_tests, number_of_tests_failed

  # Get list of all files and directories in wpath
  files_and_dirs = os.listdir(wpath)

  # Sort them by filename
  sorted_files_and_dirs = sorted(files_and_dirs)

  # Iterate over sorted list
  for file in sorted_files_and_dirs:
    # Verify on a given folder, if Baker is in it's name
    if "_Kashibe1991" in file and os.path.isdir(file):
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
