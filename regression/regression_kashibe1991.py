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
from matplotlib.ticker import (MultipleLocator, FormatStrFormatter, AutoMinorLocator)


""" ------------------- Global Variables ------------------- """

# # Data from SCIANTIX 1.0
# igSwelling1 = [0.033, 0.048]
# Data generated from SCIANTIX 2.0
FGR2 = []
FGRBase = []
FGR2Annealing = []
FGRBaseGold = []
bbconc = []
bbconc_gold = []
ListNames = ['23 GWd tU$^{-1}$ Cyclic', '23 GWd tU$^{-1}$ Isothermal','28 GWd tU$^{-1}$ Cyclic','28 GWd tU$^{-1}$ Isothermal',
             '23 GWd tU$^{-1}$ 10 K s$^{-1}$', '23 GWd tU$^{-1}$ 1.7 K s$^{-1}$','23 GWd tU$^{-1}$ 0.5 K s$^{-1}$', '23 GWd tU$^{-1}$ 0.17 K s$^{-1}$', '23 GWd tU$^{-1}$ 0.03 K s$^{-1}$',
             '28 GWd tU$^{-1}$ 10 K s$^{-1}$', '28 GWd tU$^{-1}$ 1.7 K s$^{-1}$', '28 GWd tU$^{-1}$ 0.17 K s$^{-1}$', '28 GWd tU$^{-1}$ 0.03 K s$^{-1}$']
BaseTime = [1306,1306,
            1807,1807,
            1306,1306,1306,1306,1306,
            1807,1807,1807,1807]

bbConcKashibe = [0, 0, 0, 0,
                 (10+6.5+3.3+3.5)/4,0,0,0,0,
                 (2.2+3.1)/2, (4.4+5.9)/2, 0,0] #e12
# Data from Kashibe 1991
FGROperational = [22, 22, #23 GWd/t
                  43, 43, #28 GWd/t
                  22, 22, 22, 22, 22,
                  43, 43, 43, 43
                ]

rates1_index = [4,9]
rates2_index = [5,10]
rates3_index = [6]
rates4_index = [7,11]
rates5_index = [8,12]

multiple_index = [0,2]
single_index = [1,3]

FGRAnnealing = [5.9, 6.5, #1673 K, 23 GWd/t, Multiple-Single
                5.9, 7.6,  #1673 K, 28 GWd/t, Multiple-Single
                17.8,19.2,24,16.3,19.4, #2073 K 23 GWd/t, rate 1,2,3,4,5
                28.7,27.1,24,24.47 #2073 K 28 GWd/t, rate 1,2,4,5
              ]
FGRKashibe = [op + ann for op, ann in zip(FGROperational, FGRAnnealing)]


goldFGR = []

AnnFGRVersion2 = [28.410859999999996, 17.83658, 25.389330000000005, 14.450880000000002, 28.393400000000003, 28.34806, 28.376190000000005, 28.63582, 29.7709, 25.276210000000003, 27.588310000000003, 25.69226, 26.91145]

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
 
  goldFGRAnnealing = []
  for i in range(len(FGR2)):
    FGR2Annealing.append(FGR2[i] - FGRBase[i])
    goldFGRAnnealing.append(goldFGR[i] - FGRBaseGold[i])

  # FGR annealing
  plt.rcParams.update({'font.size': 18})
  plt.rcParams.update({'lines.markersize': 6})  # Corrected parameter for marker size
  plt.rcParams.update({'lines.linewidth': 2}) 
  fig, ax = plt.subplots(figsize=(8, 8))
  ax.errorbar(
      np.concatenate([FGRAnnealing[0:2],FGRAnnealing[4:9]]),
      np.concatenate([FGR2Annealing[0:2],FGR2Annealing[4:9]]),
      elinewidth=0.5, linewidth=0.5, color='C0', fmt='o', label='23 GWd tU$^{-1}$'
  )
  ax.errorbar(
      np.concatenate([FGRAnnealing[2:4],FGRAnnealing[9:13]]),
      np.concatenate([FGR2Annealing[2:4],FGR2Annealing[9:13]]),
      elinewidth=0.5, linewidth=0.5, color='C2', fmt='o', label='28 GWd tU$^{-1}$'
  )
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
  #ax.grid(color='gray', linestyle='--', linewidth=0.5)

  plt.show()

  # Bubble concentration
  fig, ax = plt.subplots(figsize=(7, 7))
  ax.errorbar(
      np.concatenate([bbConcKashibe[0:2],bbConcKashibe[4:9]]),
      np.concatenate([bbconc[0:2],bbconc[4:9]]),
      elinewidth=0.5, linewidth=0.5, color='C0', fmt='o', label='23 GWd tU$^{-1}$'
  )
  ax.errorbar(
      np.concatenate([bbConcKashibe[2:4],bbConcKashibe[9:13]]),
      np.concatenate([bbconc[2:4],bbconc[9:13]]),
      elinewidth=0.5, linewidth=0.5, color='C2', fmt='o', label='28 GWd tU$^{-1}$'
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
  #plt.savefig('Images/BubConc-Kashibe1990')
  plt.show()

   # FGR annealing
  categories = ['10 K s$^{-1}$', '1.7 K s$^{-1}$','0.5 K s$^{-1}$', '0.17 K s$^{-1}$', '0.03 K s$^{-1}$']

  rates1_index = [4, 9]
  rates2_index = [5, 10]
  rates3_index = [6]
  rates4_index = [7, 11]
  rates5_index = [8, 12]

  burnup23_index = [4, 5, 6, 7, 8]  # Indici per burnup 23
  burnup28_index = [9, 10, 11, 12]  # Indici per burnup 28

  plt.rcParams.update({'font.size': 18})
  plt.rcParams.update({'lines.markersize': 6})  # Corrected parameter for marker size
  plt.rcParams.update({'lines.linewidth': 2}) 
  fig, ax = plt.subplots(figsize=(8, 8))

  # Burnup 23 con pallini
  ax.errorbar(np.array(FGRAnnealing)[np.intersect1d(rates1_index, burnup23_index)], 
              np.array(FGR2Annealing)[np.intersect1d(rates1_index, burnup23_index)], 
              elinewidth=0.5, linewidth=0.5, color='C0', fmt='o', label=f'{categories[0]} (Burnup 23)')
  ax.errorbar(np.array(FGRAnnealing)[np.intersect1d(rates2_index, burnup23_index)], 
              np.array(FGR2Annealing)[np.intersect1d(rates2_index, burnup23_index)], 
              elinewidth=0.5, linewidth=0.5, color='C2', fmt='o', label=f'{categories[1]} (Burnup 23)')
  ax.errorbar(np.array(FGRAnnealing)[np.intersect1d(rates3_index, burnup23_index)], 
              np.array(FGR2Annealing)[np.intersect1d(rates3_index, burnup23_index)], 
              elinewidth=0.5, linewidth=0.5, color='C3', fmt='o', label=f'{categories[2]} (Burnup 23)')
  ax.errorbar(np.array(FGRAnnealing)[np.intersect1d(rates4_index, burnup23_index)], 
              np.array(FGR2Annealing)[np.intersect1d(rates4_index, burnup23_index)], 
              elinewidth=0.5, linewidth=0.5, color='C4', fmt='o', label=f'{categories[3]} (Burnup 23)')
  ax.errorbar(np.array(FGRAnnealing)[np.intersect1d(rates5_index, burnup23_index)], 
              np.array(FGR2Annealing)[np.intersect1d(rates5_index, burnup23_index)], 
              elinewidth=0.5, linewidth=0.5, color='C5', fmt='o', label=f'{categories[4]} (Burnup 23)')

  # Burnup 28 con triangoli
  ax.errorbar(np.array(FGRAnnealing)[np.intersect1d(rates1_index, burnup28_index)], 
              np.array(FGR2Annealing)[np.intersect1d(rates1_index, burnup28_index)], 
              elinewidth=0.5, linewidth=0.5, color='C0', fmt='^', label=f'{categories[0]} (Burnup 28)')
  ax.errorbar(np.array(FGRAnnealing)[np.intersect1d(rates2_index, burnup28_index)], 
              np.array(FGR2Annealing)[np.intersect1d(rates2_index, burnup28_index)], 
              elinewidth=0.5, linewidth=0.5, color='C2', fmt='^', label=f'{categories[1]} (Burnup 28)')
  ax.errorbar(np.array(FGRAnnealing)[np.intersect1d(rates3_index, burnup28_index)], 
              np.array(FGR2Annealing)[np.intersect1d(rates3_index, burnup28_index)], 
              elinewidth=0.5, linewidth=0.5, color='C3', fmt='^', label=f'{categories[2]} (Burnup 28)')
  ax.errorbar(np.array(FGRAnnealing)[np.intersect1d(rates4_index, burnup28_index)], 
              np.array(FGR2Annealing)[np.intersect1d(rates4_index, burnup28_index)], 
              elinewidth=0.5, linewidth=0.5, color='C4', fmt='^', label=f'{categories[3]} (Burnup 28)')
  ax.errorbar(np.array(FGRAnnealing)[np.intersect1d(rates5_index, burnup28_index)], 
              np.array(FGR2Annealing)[np.intersect1d(rates5_index, burnup28_index)], 
              elinewidth=0.5, linewidth=0.5, color='C5', fmt='^', label=f'{categories[4]} (Burnup 28)')

  # Linee guida
  r = range(1, 100)
  ax.plot(r, r, color='gray', linestyle='-', linewidth=0.5)
  ax.plot(r, [x * 2 for x in r], color='gray', linestyle='--', linewidth=0.5)
  ax.annotate('x2', (1.25, 3), color='k')
  ax.plot(r, [x * 0.5 for x in r], color='gray', linestyle='--', linewidth=0.5)
  ax.annotate('/2', (3, 1.3),  color='k')

  # Impostazioni scala logaritmica
  ax.set_yscale('log')
  ax.set_xscale('log')
  ax.tick_params(axis='both', which='major')
  ax.set_xlim(1, 100)
  ax.set_ylim(1, 100)

  # Titoli e legende
  ax.set_xlabel('FGR experimental (%)')
  ax.set_ylabel('FGR calculated (%)')
  # Legenda intelligente
  custom_lines = [plt.Line2D([0], [0], color='k', marker='o', linestyle='', markersize=8, label='23 GWd tU$^{-1}$'),
                  plt.Line2D([0], [0], color='k', marker='^', linestyle='', markersize=8, label='28 GWd tU$^{-1}$'),
                  plt.Line2D([0], [0], color='C0', marker='s', linestyle='', markersize=8, label=categories[0]),
                  plt.Line2D([0], [0], color='C2', marker='s', linestyle='', markersize=8, label=categories[1]),
                  plt.Line2D([0], [0], color='C3', marker='s', linestyle='', markersize=8, label=categories[2]),
                  plt.Line2D([0], [0], color='C4', marker='s', linestyle='', markersize=8, label=categories[3]),
                  plt.Line2D([0], [0], color='C5', marker='s', linestyle='', markersize=8, label=categories[4])]
  ax.legend(handles=custom_lines, loc='best', frameon=False)
  plt.tight_layout()
  plt.savefig('Images/FGRRate_Kashibe1991')
  plt.show()

  
  ###########################################################################
  
  ###########################################################################

      # Median absolute deviations
  deviations_2 = abs(np.array(FGRAnnealing) - np.array(FGR2Annealing))
  deviations_Version2 = abs(np.array(FGRAnnealing) - np.array(AnnFGRVersion2))
  deviations_gold = abs(np.array(FGRAnnealing)-np.array(goldFGRAnnealing))
  
  print(f"This work - MAD: ", np.median(deviations_2))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_Version2))
  print(f"Barani (2017) - MAD: ", np.median(deviations_gold))

  ###########################
  for i in range(len(FGR2Annealing)):
    if FGRAnnealing[i] > 2 * FGR2Annealing[i] or FGRAnnealing[i] < 0.5 * FGR2Annealing[i]:
        print(i)
        print(ListNames[i])

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

            # Retrieve the generated data of Intergranular bubble concentration (bub/m2)
      BubConcPos = findSciantixVariablePosition(data, "Intergranular bubble concentration (bub/m2)")
      bbconc.append(1e-12*data[-1,BubConcPos].astype(float))

      # Retrieve the gold data of Intergranular bubble concentration
      BubConcPosGold = findSciantixVariablePosition(data_gold, "Intergranular bubble concentration (bub/m2)")
      bbconc_gold.append(1e-12*data_gold[-1,BubConcPosGold].astype(float))


      os.chdir('..')

  # Check if the user has chosen to display the various plots
  if mode_plot == 1:
    do_plot()

  return folderList, number_of_tests, number_of_tests_failed, FGR2Annealing, FGRAnnealing
