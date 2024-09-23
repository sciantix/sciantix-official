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

AnnFGRVersion2 =[19.956243, 24.697283, 29.256543, 19.956243, 37.023893, 39.245553, 23.6503398344953, 35.19551999970745, 40.598869999707446, 43.849179999707445, 45.92440999970745, 47.26928999970745, 23.231500000000004, 28.313430000000004, 35.59531, 24.697283, 27.560743000000002, 32.649173, 28.056343]
TOtFGRVersion2=[25.15448, 29.895519999999998, 34.45478, 25.15448, 42.22213, 44.44379, 23.65034, 35.19552, 40.59887, 43.84918, 45.92441, 47.269290000000005, 45.29628, 50.37821, 57.66009, 29.895519999999998, 32.75898, 37.847409999999996, 33.25458]
f2 = []
fgold = []
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

  
  # plt.savefig('Images/FGRTotal-Small1988')
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
  ax.scatter(FGRAnnealing, AnnFGRVersion2, c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0', alpha =0.7)
  
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
  
  plt.savefig('Images/FGRAnnealing-Small1988')
  plt.show()

    ###########################################################################

  ####################################### BARPLOTS #################################
  
  categories = ['1773 K', '1873 K', '1973 K', '2073 K', '2173 K', '2273 K']
  width = 1
  x = np.linspace(0,25,6)

  fig, ax = plt.subplots(figsize=(10, 5))
  plt.bar(x - width, FGRAnnealing[0:6], 0.9*width, label='Small ', color='#FFA07A', edgecolor = 'red')
  plt.bar(x, FGR2Annealing[0:6], 0.9*width, label='This work', color='#9370DB', edgecolor = '#6A34A2')
  plt.bar(x + width, AnnFGRVersion2[0:6], 0.9*width, label='SCIANTIX 2.0', color='#66CDAA',  edgecolor='#006400')
  
  plt.xticks(x, categories)
  plt.xlabel('Annealing temperature')
  plt.title('Experiments A - 18 GWd/tU - 1 K/s - 180 s')
  plt.ylabel('FGR (%)')
  plt.ylim([0,70])
  plt.legend()
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/A-Small1988')

  fig, ax = plt.subplots(figsize=(10, 5))

  #plt.barh(x, goldFGRDeltaRamp, 0.9*width, label='Barani (2017)', color='#ff7f0e', edgecolor='#D3D3D3')
  plt.bar(x - width, FGRAnnealing[6:12], 0.9*width, label='Small et al. (1988)', color='#FFA07A', edgecolor = 'red')
  plt.bar(x, FGR2Annealing[6:12], 0.9*width, label='This work', color='#9370DB', edgecolor = '#6A34A2')
  plt.bar(x + width, AnnFGRVersion2[6:12], 0.9*width, label='SCIANTIX 2.0', color='#66CDAA',  edgecolor='#006400')
  
  plt.xticks(x, categories)
  plt.xlabel('Annealing temperature')
  plt.title('Experiments B - 18 GWd/tU - 12.5 K/s - 1800 s')
  plt.ylabel('FGR (%)')
  plt.ylim([0,70])
  plt.legend()
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/B-Small1988')
  
  
  categories = ['1710 K \n1800 s', '1873 K \n300 s', '1873 K \n5000s']
  width = 1
  x = np.linspace(0,10,3)

  fig, ax = plt.subplots(figsize=(10, 5))
  plt.bar(x - width, FGRAnnealing[12:15], 0.9*width, label='Small et al. (1988)', color='#FFA07A', edgecolor = 'red')
  plt.bar(x, FGR2Annealing[12:15], 0.9*width, label='This work', color='#9370DB', edgecolor = '#6A34A2')
  plt.bar(x + width, AnnFGRVersion2[12:15], 0.9*width, label='SCIANTIX 2.0', color='#66CDAA',  edgecolor='#006400')
  
  plt.xticks(x, categories)
  plt.title('Experiments C - 38 GWd/tU - 12.5 K/s')
  plt.ylabel('FGR (%)')
  plt.ylim([0,70])
  plt.legend()
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/C-Small1988')

  categories  = ['1873 K \n180 s', '1973 K \n180 s', '2073 K \n180 s', '2273 K \n0 s']
  width = 1
  x = np.linspace(0,15,4)

  fig, ax = plt.subplots(figsize=(10, 5))
  plt.bar(x - width, FGRAnnealing[15:19], 0.9*width, label='Small et al. (1988)', color='#FFA07A', edgecolor = 'red')
  plt.bar(x, FGR2Annealing[15:19], 0.9*width, label='This work', color='#9370DB', edgecolor = '#6A34A2')
  plt.bar(x + width, AnnFGRVersion2[15:19], 0.9*width, label='SCIANTIX 2.0', color='#66CDAA',  edgecolor='#006400')
  
  plt.xticks(x, categories)
  plt.title('Experiments D - 18 GWd/tU - 50 K/s')
  plt.ylabel('FGR (%)')
  plt.ylim([0,70])
  plt.legend()
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/D-Small1988')
  plt.show()
  ###########################################################################

          # Median absolute deviations
  deviations_2 = abs(np.array(FGRAnnealing) - np.array(FGR2Annealing))
  deviations_Version2 = abs(np.array(FGRAnnealing) - np.array(AnnFGRVersion2))
  deviations_gold = abs(np.array(FGRAnnealing)-np.array(goldFGRAnnealing))
  print(FGRBase)
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

      # Retrieve the generated data of Intergranular gas swelling
      interGranularIntactPos = findSciantixVariablePosition(data, "Intergranular fractional intactness (/)")
      f2.append(data[-1,interGranularIntactPos].astype(float))

      # Retrieve the gold data of Intergranular gas swelling
      interGranularIntactGoldPos = findSciantixVariablePosition(data_gold, "Intergranular fractional intactness (/)")
      fgold.append(data_gold[-1,interGranularIntactGoldPos].astype(float))
      k +=1

      os.chdir('..')

  # Check if the user has chosen to display the various plots
  if mode_plot == 1:
    do_plot()

  return folderList, number_of_tests, number_of_tests_failed
