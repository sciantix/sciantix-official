"""

This is a python script to execute the regression (running the validation database) of sciantix.

@author G. Zullo

"""

""" ------------------- Import requieC2 depedencies ------------------- """

import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt
import shutil
from regression_functions import *
import scipy.stats as stats
from sklearn.linear_model import LinearRegression
import matplotlib.patches as mpatches
from matplotlib.ticker import (MultipleLocator, FormatStrFormatter, AutoMinorLocator)


""" ------------------- Global Variables ------------------- """

# # Data from SCIANTIX 1.0
# igSwelling1 = [0.033, 0.048]
# Data generated from SCIANTIX 2.0
igSwelling2 = []
FGR2Annealing = []
FGR2 = []
FGRBase = []
FGRBaseGold = []
bbconc = []
bbconc_gold = []
BaseTime = [303,805,1306,1807,
            303,805,1306,1807]
ListNames = ['1873 K 6 GWd tU$^{-1}$','1873 K 16 GWd tU$^{-1}$','1873 K 23 GWd tU$^{-1}$','1873 K 28 GWd tU$^{-1}$',
             '2073 K 6 GWd tU$^{-1}$','2073 K 16 GWd tU$^{-1}$','2073 K 23 GWd tU$^{-1}$','2073 K 28 GWd tU$^{-1}$']


# Data from Kashibe 1990
SwellingKashibe = [0, 6.7, 6.5, 7.4, #1873 K annealing, 6-16-23-28 GWd/t, 0 if missing datum
        8.9, 9.0, 10.4, 0 ##2073 K annealing, 6-16-23-28 GWd/t
        ]

bbConcKashibe = [0, 0, 1.6, (1.4+0.9)/2, #1873 K annealing, 6-16-23-28 GWd/t, 0 if missing datum
        3, (2.2+1.3)/2, 0, 0 ##2073 K annealing, 6-16-23-28 GWd/t
        ] #e12

# FGROperational = [0.2, 0.8, 21, 21, #6-16-23-28 GWd/t
#                   0.2, 0.8, 21, 21 ##6-16-23-28 GWd/t
#                 ]
FGROperational = [0.2, 1, 22, 43, #6-16-23-28 GWd/t
                  0.2, 1, 22, 43 ##6-16-23-28 GWd/t
                ]
FGRAnnealing = [05.80/0.75, 05.80/0.75, 13.71/0.75, 22.26, #1873 K annealing, 6-16-23-28 GWd/t, first datum missing
                14.16, 16.86, 25.3, 25.9 ##2073 K annealing, 6-16-23-28 GWd/t
                ]
FGRKashibe = [op + ann for op, ann in zip(FGROperational, FGRAnnealing)]

AnnFGRVersion2 = [3.3759655, 18.121944, 25.802892999999997, 27.482639, 7.9969505, 22.887864, 29.635942999999994, 31.023039000000004]
TotFGRVersion2 = [3.962144, 19.88403, 30.75867, 35.62334, 8.583129, 24.64995, 34.591719999999995, 39.163740000000004]
SwellCorrVersion2 = [2.8560209999999997, 3.322382, 3.027343, 3.039617, 2.33974, 2.753497, 2.909748, 3.0331580000000002]

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
  
  NewSwelling2 = []
  NewSwellinggold= []
  for i in range(len(f2)):
    NewSwelling2.append(igSwelling2[i])
    NewSwellinggold.append(goldSwelling[i]/fgold[i])
    
  plt.rcParams.update({'font.size': 14})
  fig, ax = plt.subplots(figsize=(7, 7))

  ax.errorbar(
      SwellingKashibe[1:4], NewSwelling2[1:4],
      elinewidth=0.5, linewidth=0.5, color='C0', fmt='o', label='1873 K'
  )

  ax.errorbar(
      SwellingKashibe[4:7], NewSwelling2[4:7],
      elinewidth=0.5, linewidth=0.5, color='C2', fmt='o', label='2073 K'
  )

  #ax.scatter(SwellingKashibe[1:4], NewSwelling2[1:4],c='C0', marker = '^', s=40, label='This work - 1873 K')
  #ax.scatter(SwellingKashibe[4:7], NewSwelling2[4:7], facecolors= 'none', edgecolors = 'C0', marker = '^', s=40,label='This work - 2073 K')
  #ax.scatter(SwellingKashibe[1:4], SwellCorrVersion2[1:4], c='C2', marker = 'd', s=40, label='SCIANTIX 2.0 - 1873 K',  alpha =0.7)
  #ax.scatter(SwellingKashibe[4:7], SwellCorrVersion2[4:7], facecolors= 'none', edgecolors='C2', marker = 'd', s=40, label='SCIANTIX 2.0 - 2073 K', alpha =0.7)
  
  r = range(1, 100)
  ax.plot(r, r, color='gray', linestyle='-', linewidth=0.5)
  ax.plot(r, [x * 2 for x in r], color='gray', linestyle='--', linewidth=0.5)
  ax.annotate('x2', (1.25, 3), color='k')
  ax.plot(r, [x * 0.5 for x in r], color='gray', linestyle='--', linewidth=0.5)
  ax.annotate('/2', (3, 1.3),  color='k')
  ax.set_yscale('log')
  ax.set_xscale('log')

  ax.tick_params(axis='both', which='major')
  ax.set_xlim(1, 100)
  ax.set_ylim(1, 100)

  ax.set_title('Intergranular swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  
  plt.savefig('Images/SwellingDiviso-Kashibe1990')
  plt.show()

  goldFGRAnnealing = []
  for i in range(len(FGR2)):
    FGR2Annealing.append(FGR2[i] - FGRBase[i])
    goldFGRAnnealing.append(goldFGR[i] - FGRBaseGold[i])

  fig, ax = plt.subplots(figsize=(7, 7))

  ax.errorbar(
      FGRAnnealing[1:4], FGR2Annealing[1:4],
      elinewidth=0.5, linewidth=0.5, color='C0', fmt='o', label='1873 K'
  )

  ax.errorbar(
      FGRAnnealing[4:8], FGR2Annealing[4:8],
      elinewidth=0.5, linewidth=0.5, color='C2', fmt='o', label='2073 K'
  )
  #ax.scatter(FGRAnnealing[1:4], FGR2Annealing[1:4],c='C0', marker = '^', s=40, label='This work - 1873 K')
  #ax.scatter(FGRAnnealing[4:8], FGR2Annealing[4:8],facecolors= 'none', edgecolors = 'C0', marker = '^', s=40,label='This work - 2073 K')
  #ax.scatter(FGRAnnealing[1:4], AnnFGRVersion2[1:4], c='C2', marker = 'd', s=40, label='SCIANTIX 2.0 - 1873 K', alpha =0.7)
  #ax.scatter(FGRAnnealing[4:8], AnnFGRVersion2[4:8], facecolors='none', edgecolors = 'C2', marker = 'd', s=40, label='SCIANTIX 2.0 - 2073 K', alpha =0.7)
  
  r = range(1, 100)
  ax.plot(r, r, color='gray', linestyle='-', linewidth=0.5)
  ax.plot(r, [x * 2 for x in r], color='gray', linestyle='--', linewidth=0.5)
  ax.annotate('x2', (1.25, 3), color='k')
  ax.plot(r, [x * 0.5 for x in r], color='gray', linestyle='--', linewidth=0.5)
  ax.annotate('/2', (3, 1.3),  color='k')
  ax.set_yscale('log')
  ax.set_xscale('log')
  
  ax.tick_params(axis='both', which='major')
  ax.set_xlim(1, 100)
  ax.set_ylim(1, 100)


  ax.set_title('Fission gas release - Annealing phase')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()

  plt.savefig('Images/FGRAnnealingDiviso-Kashibe1990')
  plt.show()

  # Bubble concentration
  fig, ax = plt.subplots(figsize=(7, 7))

  ax.errorbar(
      bbConcKashibe[2:4], bbconc[2:4],
      elinewidth=0.5, linewidth=0.5, color='C0', fmt='o', label='1873 K'
  )
  ax.errorbar(
      bbConcKashibe[4:6], bbconc[4:6],
      elinewidth=0.5, linewidth=0.5, color='C2', fmt='o', label='2073 K'
  )

  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.plot([1e-3, 1e3],[1e-3, 1e3], color='gray', linestyle='-', linewidth=0.5)
  ax.plot([1e-3, 1e3],[2e-3, 2e3], color='gray', linestyle='--', linewidth=0.5)
  ax.annotate('x2', (1.25e-1, 3e-1), color='k')
  ax.plot([1e-3, 1e3],[5e-4, 5e2], color='gray', linestyle='--', linewidth=0.5)
  ax.annotate('/2', (3e-1, 1.3e-1),  color='k')

  ax.tick_params(axis='both', which='major')
  
  ax.set_xlim(1e-1, 1e2)
  ax.set_ylim(1e-1, 1e2)

  ax.set_xlabel('Experimental (bub μm$^{-2}$')
  ax.set_ylabel('Calculated (bub μm$^{-2}$)')
  ax.set_title('Bubble concentration')
  ax.legend()
  plt.savefig('Images/BubConc-Kashibe1990')
  plt.show()

  ###########FGR##############

  categories = ['6 GWd tU$^{-1}$', '16 GWd tU$^{-1}$','23 GWd tU$^{-1}$', '28 GWd tU$^{-1}$']
  categories1 = ['16 GWd tU$^{-1}$','23 GWd tU$^{-1}$', '28 GWd tU$^{-1}$']
  categories2 = ['6 GWd tU$^{-1}$', '16 GWd tU$^{-1}$','23 GWd tU$^{-1}$']
  
  width = 1
  x = np.array([0,5,10,15])
  x1 = np.array([5,10,15])
  x2 = np.array([0,5,10])

  fig, ax = plt.subplots(figsize=(7, 7))

  FGR_values = FGRAnnealing[1:4]
  FGR_err = [np.array(FGR_values) / 2, np.array(FGR_values) * 2]

  plt.errorbar(x1, FGR_values, yerr=FGR_err, fmt='o', label='Une et al. (1990)', color='C1')
  plt.errorbar(x1, FGR2Annealing[1:4], fmt='s', label='This work', color='C0')

  plt.xticks(x1, categories1)
  plt.ylabel('FGR (%)')
  plt.xlabel('Burn-up')
  plt.title('Annealing at 1873 K')
  plt.legend(loc='best')
  plt.xlim([3,17])
  plt.ylim([0,1e2])
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/FGRSpecifico1600-Kashibe1990')

  fig, ax = plt.subplots(figsize=(7, 7))

  FGR_values = FGRAnnealing[4:8]
  FGR_err = [np.array(FGR_values) / 2, np.array(FGR_values) * 2]

  plt.errorbar(x, FGR_values, yerr=FGR_err, fmt='o', label='Une et al. (1990)', color='C1')
  plt.errorbar(x, FGR2Annealing[4:8], fmt='s', label='This work', color='C0')

  plt.xticks(x, categories)
  plt.ylabel('FGR (%)')
  plt.xlabel('Burn-up')
  plt.title('Annealing at 2073 K')
  plt.legend(loc='best')
  plt.xlim([-2,17])
  plt.ylim([0,1e2])
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/FGRSpecifico1800-Kashibe1990')
  plt.show()

  ######## Swelling ##############

  fig, ax = plt.subplots(figsize=(7, 7))

  SW_values = SwellingKashibe[1:4]
  SW_err = [np.array(SW_values) / 2, np.array(SW_values) * 2]

  plt.errorbar(x1, SW_values, yerr=SW_err, fmt='o', label='Une et al. (1990)', color='C1')
  plt.errorbar(x1, NewSwelling2[1:4], fmt='s', label='This work', color='C0')

  plt.xticks(x1, categories1)
  plt.ylabel('Swelling (%)')
  plt.xlabel('Burn-up')
  plt.title('Annealing at 1873 K')
  plt.ylim([1e-1,1e2])
  plt.yscale('log')
  
  plt.legend(loc='best')
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/SwellingSpecifico1600-Kashibe1990')

  fig, ax = plt.subplots(figsize=(7, 7))

  SW_values = SwellingKashibe[4:7]
  SW_err = [np.array(SW_values) / 2, np.array(SW_values) * 2]

  plt.errorbar(x2, SW_values, yerr=SW_err, fmt='o', label='Une et al. (1990)', color='C1')
  plt.errorbar(x2, NewSwelling2[4:7], fmt='s', label='This work', color='C0')

  plt.xticks(x2, categories2)
  plt.ylabel('Swelling (%)')
  plt.xlabel('Burn-up')
  plt.title('Annealing at 2073 K')
  plt.xlim([-2,12])
  plt.ylim([1e-1,1e2])
  plt.yscale('log')
  plt.legend(loc='best')
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/SwellingSpecifico1800-Kashibe1990')
  plt.show()


  ################################# Median absolute deviations ####################################
  deviations_2 = abs(np.array(FGRAnnealing[1:8]) - np.array(FGR2Annealing[1:8]))
  deviations_Version2 = abs(np.array(FGRAnnealing[1:8]) - np.array(AnnFGRVersion2[1:8]))
  deviations_gold = abs(np.array(FGRAnnealing[1:8])-np.array(goldFGRAnnealing[1:8]))

  print('FGR')
  print(f"This work - MAD: ", np.median(deviations_2))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_Version2))
  print(f"Barani (2017) - MAD: ", np.median(deviations_gold))

        # Median absolute deviations
  deviations_2 = abs(np.array(SwellingKashibe[1:7]) - np.array(NewSwelling2[1:7]))
  deviations_Version2 = abs(np.array(SwellingKashibe[1:7]) - np.array(SwellCorrVersion2[1:7]))
  deviations_gold = abs(np.array(SwellingKashibe[1:7])-np.array(NewSwellinggold[1:7]))
  
  print('Swelling')
  print(f"This work - MAD: ", np.median(deviations_2))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_Version2))
  print(f"Barani (2017) - MAD: ", np.median(deviations_gold))

  ######################################################à

  for i in np.arange(1,7):
    if SwellingKashibe[i] > 2 * NewSwelling2[i] or SwellingKashibe[i] < 0.5 * NewSwelling2[i]:
        print('Swelling')
        print(i)
        print(ListNames[i])

  for i in np.arange(1,8):
    if FGRAnnealing[i] > 2 * FGR2Annealing[i] or FGRAnnealing[i] < 0.5 * FGR2Annealing[i]:
        print('FGR')
        print(i)
        print(ListNames[i])

  #FGR base
  print('FGR base')
  print('This work ', FGRBase)
  print('exp', FGROperational)
          
  
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

      # Retrieve the generated data of Intergranular bubble concentration (bub/m2)
      BubConcPos = findSciantixVariablePosition(data, "Intergranular bubble concentration (bub/m2)")
      bbconc.append(1e-12*data[-1,BubConcPos].astype(float))

      # Retrieve the gold data of Intergranular bubble concentration
      BubConcPosGold = findSciantixVariablePosition(data_gold, "Intergranular bubble concentration (bub/m2)")
      bbconc_gold.append(1e-12*data_gold[-1,BubConcPosGold].astype(float))

      # Retrieve the generated data of Intragranular gas swelling
      igSwellingPos = findSciantixVariablePosition(data, "Intergranular gas swelling (/)")
      igSwelling2.append(100*data[-1,igSwellingPos].astype(float))

      # Retrieve the gold data of Intragranular gas swelling
      igSwellingGoldPos = findSciantixVariablePosition(data_gold, "Intergranular gas swelling (/)")
      goldSwelling.append(100*data_gold[-1,igSwellingGoldPos].astype(float))

      try:
          interGranularIntactPos = findSciantixVariablePosition(data, "Intergranular fractional intactness (/)")
          f2.append(data[-1,interGranularIntactPos].astype(float))

          interGranularIntactGoldPos = findSciantixVariablePosition(data_gold, "Intergranular fractional intactness (/)")
          fgold.append(data_gold[-1,interGranularIntactGoldPos].astype(float))
      except (KeyError, IndexError, TypeError):
          # Assign a default value of 1 if the variable is not found
          print("Variable 'Intergranular fractional intactness' not found, assigning default value of 1...")
          f2.append(1)
          fgold.append(1)

      os.chdir('..')

  # Check if the user has chosen to display the various plots
  if mode_plot == 1:
    do_plot()

  return folderList, number_of_tests, number_of_tests_failed, igSwelling2, SwellingKashibe, FGR2Annealing, FGRAnnealing
