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

AnnFGRVersion2 = [18.023320000000005, 2.2132000000000076, 19.682140000000004, 1.8047499999999985, 14.202690000000004, 14.323930000000004, 14.646620000000006, 15.375080000000004, 17.886880000000005, 14.87368, 19.2245, 16.09091, 18.901650000000004]
TOtFGRVersion2 = [54.96282, 39.1527, 60.75551, 42.878119999999996, 51.14219, 51.26343, 51.58612, 52.31458, 54.82638, 55.94705, 60.297869999999996, 57.16428, 59.97502]

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
  
  # # FGR totale
  # fig, ax = plt.subplots()

  # ax.scatter(FGRKashibe, FGR2, c = '#9370DB', marker = '^', s=40, label='This work', zorder=1)
  # ax.scatter(FGRKashibe, goldFGR, c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)', zorder=2, alpha=0.7)
  # ax.scatter(FGRKashibe, TOtFGRVersion2, c='green', marker = 'd', s=40, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
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
  
  # plt.savefig('FGRTotal-Kashibe1991')
  # plt.show()

  FGR2Annealing = []
  goldFGRAnnealing = []
  for i in range(len(FGR2)):
    FGR2Annealing.append(FGR2[i] - FGRBase[i])
    goldFGRAnnealing.append(goldFGR[i] - FGRBaseGold[i])

  # # FGR annealing 
  # fig, ax = plt.subplots()

  # ax.scatter(FGRAnnealing, FGR2Annealing, c = '#9370DB', marker = '^', s=40, label='This work', zorder=1)
  # ax.scatter(FGRAnnealing, goldFGRAnnealing, c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)', zorder=2, alpha=0.7)
  # ax.scatter(FGRAnnealing, AnnFGRVersion2, c='green', marker = 'd', s=40, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
  # ax.plot([0, 100],[0, 100], '-', color = '#757575')
  # ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  # ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')
  
  # ax.set_xlim(0, 50)
  # ax.set_ylim(0, 50)

  # ax.set_title('Fission gas release - Annealing phase')
  # ax.set_xlabel('Experimental (%)')
  # ax.set_ylabel('Calculated (%)')
  # ax.legend()
  # ax.grid(color='gray', linestyle='--', linewidth=0.5)
  
  # plt.savefig('FGRAnnealing-Kashibe1991')
  # plt.show()

# FGRAnnealing = [5.9, 6.5, #1400°C, 23 GWd/t, Multiple-Single
#                 5.9, 7.6,  #1400°C, 28 GWd/t, Multiple-Single
#                 17.8,19.2,24,16.3,19.4, #1800°C 23 GWd/t, rate 1,2,3,4,5
#                 28.7,27.1,24,24.47 #1800°C 28 GWd/t, rate 1,2,4,5
#               ]

  # FGR annealing
  fig, ax = plt.subplots()

  ax.scatter(FGRAnnealing[4:9], FGR2Annealing[4:9],c='#9370DB', marker = '^', s=40,         label='This work - 23 GWd/tU')
  ax.scatter(FGRAnnealing[9:13], FGR2Annealing[9:13], facecolors= 'none', edgecolors = '#9370DB', marker = '^', s=40,    label='This work - 28 GWd/tU')
  #ax.scatter(FGRAnnealing[4:9], goldFGRAnnealing[4:9],c='#ff7f0e', marker = '^', s=40,      label='Barani (2017) - 23 GWd/tU')
  #ax.scatter(FGRAnnealing[9:13], goldFGRAnnealing[9:13], c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017) - 28 GWd/tU')
  ax.scatter(FGRAnnealing[4:9], AnnFGRVersion2[4:9], c='green', marker = 'd', s=40, label='SCIANTIX 2.0 - 23 GWd/tU', alpha =0.7)
  ax.scatter(FGRAnnealing[9:13], AnnFGRVersion2[9:13], facecolors='none', edgecolors = 'green', marker = 'd', s=40, label='SCIANTIX 2.0 - 28 GWd/tU',alpha =0.7)
  
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

  ax.scatter(FGRAnnealing[0:2], FGR2Annealing[0:2],c='#9370DB', marker = '^', s=40,         label='This work - 23 GWd/tU')
  ax.scatter(FGRAnnealing[2:4], FGR2Annealing[2:4], facecolors = 'none', edgecolors = '#9370DB', marker = '^', s=40,    label='This work - 28 GWd/tU')
  # ax.scatter(FGRAnnealing[0:2], goldFGRAnnealing[0:2],c='#ff7f0e', marker = '^', s=40,      label='Barani (2017) - 1400°C - 23 GWd/tU', zorder = 2)
  # ax.scatter(FGRAnnealing[2:4], goldFGRAnnealing[2:4], c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017) - 1400°C - 28 GWd/tU', zorder = 4)
  ax.scatter(FGRAnnealing[0:2], AnnFGRVersion2[0:2], c='green', marker = 'd', s=40, label='SCIANTIX 2.0 - 23 GWd/tU', alpha =0.7)
  ax.scatter(FGRAnnealing[2:4], AnnFGRVersion2[2:4], facecolors = 'none', edgecolors='green', marker = 'd', s=40, label='SCIANTIX 2.0 - 28 GWd/tU', alpha =0.7)
  
  ax.plot([0, 100],[0, 100], '-', color = '#757575')
  ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')
  
  ax.set_xlim(0, 30)
  ax.set_ylim(0, 30)

  ax.set_title('Fission gas release - Annealing phase')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)

  plt.savefig('FGRAnnealingRamp-Kashibe1991')
  plt.show()

  # Difference multiple-single ramp
  FGR2DeltaRamp = [FGR2Annealing[1] - FGR2Annealing[0], 
                   FGR2Annealing[3] - FGR2Annealing[2]]
  FGRDeltaRamp = [FGRAnnealing[1] - FGRAnnealing[0], 
                  FGRAnnealing[3] - FGRAnnealing[2]]
  goldFGRDeltaRamp = [goldFGRAnnealing[1] - goldFGRAnnealing[0], 
                      goldFGRAnnealing[3] - goldFGRAnnealing[2]]
  FGRDeltaRampVersion2 = [AnnFGRVersion2[1] - AnnFGRVersion2[0],
                          AnnFGRVersion2[3] - AnnFGRVersion2[2]]
  
  # fig, ax = plt.subplots()

  # categories = ['This work', 'Barani (2017)', 'Kashibe et al. (1991)']
  # val1 = [abs(FGR2DeltaRamp[0]), abs(goldFGRDeltaRamp[0]), abs(FGRDeltaRamp[0])]
  # val2 = [abs(FGR2DeltaRamp[1]), abs(goldFGRDeltaRamp[1]), abs(FGRDeltaRamp[1])]

  # width = 0.3
  # y = np.arange(len(categories))

  # plt.barh(y - width/2, val1, 0.8*width, label='23 GWd/tU', color='blue')
  # plt.barh(y + width/2, val2, 0.8*width, label='28 GWd/tU', color='green')

  # plt.yticks(y, categories)
  # plt.xlabel('Absolute Difference Between Single and Multiple Ramp Annealing (FGR)')
  # plt.title('Isothermal and cyclic annealing')

  # plt.legend()
  # plt.grid(color='gray', linestyle='--', linewidth=0.5)
  # #plt.savefig('FGRDeltaRamp-Kashibe1991'))
  # plt.show()

  fig, ax = plt.subplots(figsize=(8, 4))

  categories = ['23 GWd/tU', '28 GWd/tU']

  width = 1
  x = np.array([0, 5])

  #plt.barh(x, goldFGRDeltaRamp, 0.9*width, label='Barani (2017)', color='#ff7f0e', edgecolor='#D3D3D3')
  plt.barh(x + width, FGRDeltaRamp, 0.9*width, label='Kashibe (1991)', color='lightblue', edgecolor='#D3D3D3')
  plt.barh(x, FGR2DeltaRamp, 0.9*width, label='This work', color='#9370DB', edgecolor='#D3D3D3')
  plt.barh(x - width, FGRDeltaRampVersion2, 0.9*width, label='SCIANTIX 2.0', color='green', edgecolor='#D3D3D3')
  
  plt.yticks(x, categories)
  plt.xlabel('Difference Between Single and Multiple Ramp Annealing (FGR)')
  plt.title('Isothermal and cyclic annealing')

  plt.legend()
  
  plt.axvline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='x') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='x')

  plt.savefig('FGRDeltaRamp-Kashibe1991')
  plt.show()
  
      # Median absolute deviations
  deviations_2 = abs(np.array(FGRAnnealing) - np.array(FGR2Annealing))
  deviations_Version2 = abs(np.array(FGRAnnealing) - np.array(AnnFGRVersion2))
  deviations_gold = abs(np.array(FGRAnnealing)-np.array(goldFGRAnnealing))
  
  print(f"This work - MAD: ", np.median(deviations_2))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_Version2))
  print(f"Barani (2017) - MAD: ", np.median(deviations_gold))

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
