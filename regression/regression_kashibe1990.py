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
igSwelling2 = []
FGR2 = []
FGRBase = []
FGRBaseGold = []
BaseTime = [303,805,1306,1807,
            303,805,1306,1807]

# Data from Kashibe 1990
SwellingKashibe = [6, 6.7, 6.5, 7.4, #1600°C annealing, 6-16-23-28 GWd/t, 0 if missing datum
        8.9, 9.0, 10.4, 10 ##1800°C annealing, 6-16-23-28 GWd/t
        ]

# FGROperational = [0.2, 0.8, 21, 21, #6-16-23-28 GWd/t
#                   0.2, 0.8, 21, 21 ##6-16-23-28 GWd/t
#                 ]
FGROperational = [0.2, 1, 22, 43, #6-16-23-28 GWd/t
                  0.2, 1, 22, 43 ##6-16-23-28 GWd/t
                ]
FGRAnnealing = [05.80/0.75, 05.80/0.75, 13.71/0.75, 22.26, #1600°C annealing, 6-16-23-28 GWd/t, first datum missing
                14.16, 16.86, 25.3, 25.9 ##1800°C annealing, 6-16-23-28 GWd/t
                ]
FGRKashibe = [op + ann for op, ann in zip(FGROperational, FGRAnnealing)]

AnnFGRVersion2 = [7.671459, 25.042722, 13.39244, 13.953050000000005, 13.625053, 30.908262, 21.162490000000005, 22.744110000000006]
TotFGRVersion2 = [9.452466, 30.17189, 50.331939999999996, 55.02642, 15.40606, 36.03743, 58.10199, 63.81748]
SwellCorrVersion2 = [3.03019, 3.338526, 3.682544, 4.047388, 2.5628729999999997, 2.884574, 3.775009, 4.219698]

goldFGR = []
goldSwelling = []

f2 = []
fgold = []

number_of_tests_failed = 0
sample_number = len(igSwelling2)


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

  # ax.scatter(FGRKashibe, FGR2,c = '#9370DB', marker = '^', s=40, label='This work', zorder=1)
  # ax.scatter(FGRKashibe, goldFGR, c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)',zorder=2, alpha=0.7)
  # ax.scatter(FGRKashibe, TotFGRVersion2, c='green', marker = 'd', s=40, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
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

  # #plt.savefig('FGRTotal-Kashibe1990')

  # #plt.show()

  # fig, ax = plt.subplots()

  # ax.scatter(SwellingKashibe,  igSwelling2, c = '#9370DB', marker = '^', s=40, label='This work', zorder=1)
  # ax.scatter(SwellingKashibe,  goldSwelling, c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)', zorder=2, alpha=0.7)
  # ax.scatter(SwellingKashibe, SwellCorrVersion2, c='green', marker = 'd', s=40, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
  # ax.plot([0, 100],[0, 100], '-', color = '#757575')
  # ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  # ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')

  # ax.set_xlim(0, 20)
  # ax.set_ylim(0, 20)

  # ax.set_title('Intergranular swelling')
  # ax.set_xlabel('Experimental (%)')
  # ax.set_ylabel('Calculated (%)')
  # ax.legend()
  # ax.grid(color='gray', linestyle='--', linewidth=0.5)

  # plt.savefig('Swelling-Kashibe1990')
  # #plt.show()

  # # Swelling
  # fig, ax = plt.subplots()

  NewSwelling2 = []
  NewSwellinggold= []
  for i in range(len(f2)):
    NewSwelling2.append(igSwelling2[i]/f2[i])
    NewSwellinggold.append(goldSwelling[i]/fgold[i])
    
  # ax.scatter(SwellingKashibe, NewSwelling2,c='#9370DB', marker = '^', s=40, label='This work', zorder = 1)
  # ax.scatter(SwellingKashibe, NewSwellinggold, c = '#ff7f0e', marker = 'o', s=40,label='Barani (2017)', zorder = 2, alpha=0.7)
  # ax.scatter(SwellingKashibe, SwellCorrVersion2, c='green', marker = 'd', s=40, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
  # ax.plot([0, 100],[0, 100], '-', color = '#757575')
  # ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  # ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')

  # ax.set_xlim(0, 20)
  # ax.set_ylim(0, 20)

  # ax.set_title('Intergranular swelling')
  # ax.set_xlabel('Experimental (%)')
  # ax.set_ylabel('Calculated (%)')
  # ax.legend()
  # ax.grid(color='gray', linestyle='--', linewidth=0.5)
  
  # #plt.savefig('Swelling-Kashibe1990')
  # plt.show()

  # Swelling at different temperatures
  fig, ax = plt.subplots()

  ax.scatter(SwellingKashibe[1:4], NewSwelling2[1:4],c='#9370DB', marker = '^', s=40, label='This work - 1600°C')
  ax.scatter(SwellingKashibe[4:7], NewSwelling2[4:7], facecolors= 'none', edgecolors = '#9370DB', marker = '^', s=40,label='This work - 1800°C')
  #ax.scatter(SwellingKashibe[1:4], NewSwellinggold[1:4],c='#ff7f0e', marker = '^', s=40, label='Barani (2017) - 1600°C',  alpha=0.8)
  #ax.scatter(SwellingKashibe[4:7], NewSwellinggold[4:7], c = '#ff7f0e', marker = 'o', s=40,label='Barani (2017) - 1800°C',  alpha =0.8)
  ax.scatter(SwellingKashibe[1:4], SwellCorrVersion2[1:4], c='green', marker = 'd', s=40, label='SCIANTIX 2.0 - 1600°C',  alpha =0.7)
  ax.scatter(SwellingKashibe[4:7], SwellCorrVersion2[4:7], facecolors= 'none', edgecolors='green', marker = 'd', s=40, label='SCIANTIX 2.0 - 1800°C', alpha =0.7)
  
  ax.plot([0, 100],[0, 100], '-', color = '#757575')
  ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')

  ax.set_xlim(0, 15)
  ax.set_ylim(0, 15)

  ax.set_title('Intergranular swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  
  plt.savefig('SwellingDiviso-Kashibe1990')
  plt.show()

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
  
  # ax.set_xlim(0, 40)
  # ax.set_ylim(0, 40)

  # ax.set_title('Fission gas release - Annealing phase')
  # ax.set_xlabel('Experimental (%)')
  # ax.set_ylabel('Calculated (%)')
  # ax.legend()
  # ax.grid(color='gray', linestyle='--', linewidth=0.5)

  # plt.savefig('FGRAnnealing-Kashibe1990')
  # plt.show()

  # FGR annealing
  fig, ax = plt.subplots()

  ax.scatter(FGRAnnealing[1:4], FGR2Annealing[1:4],c='#9370DB', marker = '^', s=40, label='This work - 1600°C')
  ax.scatter(FGRAnnealing[4:8], FGR2Annealing[4:8],facecolors= 'none', edgecolors = '#9370DB', marker = '^', s=40,label='This work - 1800°C')
  # ax.scatter(FGRAnnealing[1:4], goldFGRAnnealing[1:4],c='#ff7f0e', marker = '^', s=40, label='Barani (2017) - 1600°C')
  # ax.scatter(FGRAnnealing[4:8], goldFGRAnnealing[4:8], c = '#ff7f0e', marker = 'o', s=40,label='Barani (2017) - 1800°C')
  ax.scatter(FGRAnnealing[1:4], AnnFGRVersion2[1:4], c='green', marker = 'd', s=40, label='SCIANTIX 2.0 - 1600°C', alpha =0.7)
  ax.scatter(FGRAnnealing[4:8], AnnFGRVersion2[4:8], facecolors='none', edgecolors = 'green', marker = 'd', s=40, label='SCIANTIX 2.0 - 1800°C', alpha =0.7)
  

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

  plt.savefig('FGRAnnealingDiviso-Kashibe1990')
  plt.show()

        # Median absolute deviations
  deviations_2 = abs(np.array(FGRAnnealing) - np.array(FGR2Annealing))
  deviations_Version2 = abs(np.array(FGRAnnealing) - np.array(AnnFGRVersion2))
  deviations_gold = abs(np.array(FGRAnnealing)-np.array(goldFGRAnnealing))
  
  print('FGR annealing')
  print(f"This work - MAD: ", np.median(deviations_2))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_Version2))
  print(f"Barani (2017) - MAD: ", np.median(deviations_gold))

        # Median absolute deviations
  deviations_2 = abs(np.array(SwellingKashibe) - np.array(NewSwelling2))
  deviations_Version2 = abs(np.array(SwellingKashibe) - np.array(SwellCorrVersion2))
  deviations_gold = abs(np.array(SwellingKashibe)-np.array(NewSwellinggold))
  
  print('Swelling')
  print(f"This work - MAD: ", np.median(deviations_2))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_Version2))
  print(f"Barani (2017) - MAD: ", np.median(deviations_gold))
    

# Main function of the baker regression
def regression_kashibe1990(wpath, mode_Kashibe1990, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):
  k=0
  # Exit of the function without doing anything
  if mode_Kashibe1990 == 0 :
    return folderList, number_of_tests, number_of_tests_failed

  # Get list of all files and directories in wpath
  files_and_dirs = os.listdir(wpath)

  # Sort them by filename
  sorted_files_and_dirs = sorted(files_and_dirs)

  # Iterate over sorted list
  for file in sorted_files_and_dirs:
    # Verify on a given folder, if Baker is in it's name
    if "_Kashibe1990" in file and os.path.isdir(file):
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

      # Retrieve the generated data of Intragranular gas swelling
      igSwellingPos = findSciantixVariablePosition(data, "Intergranular gas swelling (/)")
      igSwelling2.append(100*data[-1,igSwellingPos].astype(float))

      # Retrieve the gold data of Intragranular gas swelling
      igSwellingGoldPos = findSciantixVariablePosition(data_gold, "Intergranular gas swelling (/)")
      goldSwelling.append(100*data_gold[-1,igSwellingGoldPos].astype(float))

      # Retrieve the generated data of Intergranular gas swelling
      interGranularIntactPos = findSciantixVariablePosition(data, "Intergranular fractional intactness (/)")
      f2.append(data[-1,interGranularIntactPos].astype(float))

      # Retrieve the gold data of Intergranular gas swelling
      interGranularIntactGoldPos = findSciantixVariablePosition(data_gold, "Intergranular fractional intactness (/)")
      fgold.append(data_gold[-1,interGranularIntactGoldPos].astype(float))


      os.chdir('..')

  # Check if the user has chosen to display the various plots
  if mode_plot == 1:
    do_plot()

  return folderList, number_of_tests, number_of_tests_failed
