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
import matplotlib.patches as mpatches


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
  # ax.scatter(FGRKashibe, TotFGRVersion2, c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
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

  # #plt.savefig('Images/FGRTotal-Kashibe1990')

  # #plt.show()

  # fig, ax = plt.subplots()

  # ax.scatter(SwellingKashibe,  igSwelling2, c = '#9370DB', marker = '^', s=40, label='This work', zorder=1)
  # ax.scatter(SwellingKashibe,  goldSwelling, c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)', zorder=2, alpha=0.7)
  # ax.scatter(SwellingKashibe, SwellCorrVersion2, c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
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

  # plt.savefig('Images/Swelling-Kashibe1990')
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
  # ax.scatter(SwellingKashibe, SwellCorrVersion2, c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
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
  
  # #plt.savefig('Images/Swelling-Kashibe1990')
  # plt.show()

  # Swelling at different temperatures
  fig, ax = plt.subplots()

  ax.scatter(SwellingKashibe[1:4], NewSwelling2[1:4],c='#9370DB', marker = '^', s=40, label='This work - 1600°C')
  ax.scatter(SwellingKashibe[4:7], NewSwelling2[4:7], facecolors= 'none', edgecolors = '#9370DB', marker = '^', s=40,label='This work - 1800°C')
  #ax.scatter(SwellingKashibe[1:4], NewSwellinggold[1:4],c='#ff7f0e', marker = '^', s=40, label='Barani (2017) - 1600°C',  alpha=0.8)
  #ax.scatter(SwellingKashibe[4:7], NewSwellinggold[4:7], c = '#ff7f0e', marker = 'o', s=40,label='Barani (2017) - 1800°C',  alpha =0.8)
  ax.scatter(SwellingKashibe[1:4], SwellCorrVersion2[1:4], c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - 1600°C',  alpha =0.7)
  ax.scatter(SwellingKashibe[4:7], SwellCorrVersion2[4:7], facecolors= 'none', edgecolors='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - 1800°C', alpha =0.7)
  
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
  
  plt.savefig('Images/SwellingDiviso-Kashibe1990')
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
  # ax.scatter(FGRAnnealing, AnnFGRVersion2, c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)
  
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

  # plt.savefig('Images/FGRAnnealing-Kashibe1990')
  # plt.show()

    # FGR annealing
  fig, ax = plt.subplots()

  ax.scatter(FGRAnnealing, FGR2Annealing,c='#9370DB', marker = '^', s=40, label='This work')
  ax.scatter(FGRAnnealing, AnnFGRVersion2, c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0', alpha =0.7)

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

  plt.savefig('Images/FGRAnnealingTotal-Kashibe1990')
  plt.show()

  # FGR annealing
  fig, ax = plt.subplots()

  ax.scatter(FGRAnnealing[1:4], FGR2Annealing[1:4],c='#9370DB', marker = '^', s=40, label='This work - 1600°C')
  ax.scatter(FGRAnnealing[4:8], FGR2Annealing[4:8],facecolors= 'none', edgecolors = '#9370DB', marker = '^', s=40,label='This work - 1800°C')
  # ax.scatter(FGRAnnealing[1:4], goldFGRAnnealing[1:4],c='#ff7f0e', marker = '^', s=40, label='Barani (2017) - 1600°C')
  # ax.scatter(FGRAnnealing[4:8], goldFGRAnnealing[4:8], c = '#ff7f0e', marker = 'o', s=40,label='Barani (2017) - 1800°C')
  ax.scatter(FGRAnnealing[1:4], AnnFGRVersion2[1:4], c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - 1600°C', alpha =0.7)
  ax.scatter(FGRAnnealing[4:8], AnnFGRVersion2[4:8], facecolors='none', edgecolors = '#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - 1800°C', alpha =0.7)
  

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

  plt.savefig('Images/FGRAnnealingDiviso-Kashibe1990')
  plt.show()

  ############################################  BARPLOTS #############################################

  fig, ax = plt.subplots(figsize=(10,5))

  categories = ['6 GWd/tU', '16 GWd/tU','23 GWd/tU', '28 GWd/tU']

  width = 1
  x = np.array([0,5,10,15])
  x1 = np.array([5,10,15])

  plt.bar(x1 - width, FGRAnnealing[1:4], 0.9*width, label='Une (1990)', color='#FFA07A', edgecolor='#D3D3D3')
  plt.bar(x - width, FGRAnnealing[4:8], 0.9*width, label='Une (1990)', color='none', edgecolor='red')
  plt.bar(x1, FGR2Annealing[1:4], 0.9*width, label='This work', color='#9370DB', edgecolor='#D3D3D3')
  plt.bar(x, FGR2Annealing[4:8], 0.9*width, label='This work', color='none', edgecolor='#6A34A2')
  plt.bar(x1 + width, AnnFGRVersion2[1:4], 0.9*width, label='SCIANTIX 2.0', color='#66CDAA', edgecolor='#D3D3D3')
  plt.bar(x + width, AnnFGRVersion2[4:8], 0.9*width, label='SCIANTIX 2.0', color='none', edgecolor='#006400')
  
  plt.xticks(x, categories)
  plt.ylabel('FGR (%)')
  plt.xlabel('Burn-up')
  #plt.title('Annealing - Burn-up effect')

  
  # Create colored patches for Kashibe, This Work, and SCIANTIX
  kashibe_patch = mpatches.Patch(facecolor='#FFA07A', edgecolor='red', label='Une (1990)')
  this_work_patch = mpatches.Patch(facecolor='#9370DB', edgecolor='#6A34A2', label='This work')
  sciantix_patch = mpatches.Patch(facecolor='#66CDAA', edgecolor='#006400', label='SCIANTIX 2.0')

  # Create line patches for the burnup (23 and 28 GWd/tU)
  T1600_patch = plt.Line2D([0], [0], color='black', linestyle='none', marker='s', markersize=10, label='1600 °C')
  T1800_patch = plt.Line2D([0], [0], color='black', linestyle='none', marker='s', markersize=10, label='1800 °C', markerfacecolor='none')

  # Combine the patches into a single legend
  legend_items = [kashibe_patch, this_work_patch, sciantix_patch, T1600_patch, T1800_patch]

  # Create the legend
  plt.legend(handles=legend_items, loc='best', ncol=2)
  plt.ylim([0,40])
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/FGRSpecifico-Kashibe1990')
  plt.show()
  
  ###########FGR##############

  categories = ['6 GWd/tU', '16 GWd/tU','23 GWd/tU', '28 GWd/tU']
  categories1 = ['16 GWd/tU','23 GWd/tU', '28 GWd/tU']
  categories2 = ['6 GWd/tU', '16 GWd/tU','23 GWd/tU']
  
  width = 1
  x = np.array([0,5,10,15])
  x1 = np.array([5,10,15])
  x2 = np.array([0,5,10])

  fig, ax = plt.subplots(figsize=(10,5))

  plt.bar(x1 - width, FGRAnnealing[1:4], 0.9*width, label='Une (1990)', color='#FFA07A', edgecolor='red')
  plt.bar(x1, FGR2Annealing[1:4], 0.9*width, label='This work', color='#9370DB', edgecolor='#6A34A2')
  plt.bar(x1 + width, AnnFGRVersion2[1:4], 0.9*width, label='SCIANTIX 2.0', color='#66CDAA', edgecolor='#006400')
  
  plt.xticks(x1, categories1)
  plt.ylabel('FGR (%)')
  plt.xlabel('Burn-up')
  plt.title('Annealing at 1600°C')
  plt.legend(loc='best')
  plt.ylim([0,40])
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/FGRSpecifico1600-Kashibe1990')

  fig, ax = plt.subplots(figsize=(10,5))

  plt.bar(x - width, FGRAnnealing[4:8], 0.9*width, label='Une (1990)', color='#FFA07A', edgecolor='red')
  plt.bar(x, FGR2Annealing[4:8], 0.9*width, label='This work', color='#9370DB', edgecolor='#6A34A2')
  plt.bar(x + width, AnnFGRVersion2[4:8], 0.9*width, label='SCIANTIX 2.0', color='#66CDAA', edgecolor='#006400')
  
  plt.xticks(x, categories)
  plt.ylabel('FGR (%)')
  plt.xlabel('Burn-up')
  plt.title('Annealing at 1800°C')
  plt.legend(loc='best')
  plt.ylim([0,40])
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/FGRSpecifico1800-Kashibe1990')
  plt.show()

  ######## Swelling ##############

  fig, ax = plt.subplots(figsize=(10,5))

  plt.bar(x1 - width, SwellingKashibe[1:4], 0.9*width, label='Une (1990)', color='#FFA07A', edgecolor='red')
  plt.bar(x1, NewSwelling2[1:4], 0.9*width, label='This work', color='#9370DB', edgecolor='#6A34A2')
  plt.bar(x1 + width, SwellCorrVersion2[1:4], 0.9*width, label='SCIANTIX 2.0', color='#66CDAA', edgecolor='#006400')
  
  plt.xticks(x1, categories1)
  plt.ylabel('Swelling (%)')
  plt.xlabel('Burn-up')
  plt.title('Annealing at 1600°C')
  plt.ylim([0,20])
  
  plt.legend(loc='best')
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/SwellingSpecifico1600-Kashibe1990')

  fig, ax = plt.subplots(figsize=(10,5))

  plt.bar(x2 - width, SwellingKashibe[4:7], 0.9*width, label='Une (1990)', color='#FFA07A', edgecolor='red')
  plt.bar(x2, NewSwelling2[4:7], 0.9*width, label='This work', color='#9370DB', edgecolor='#6A34A2')
  plt.bar(x2 + width, SwellCorrVersion2[4:7], 0.9*width, label='SCIANTIX 2.0', color='#66CDAA', edgecolor='#006400')
  
  plt.xticks(x2, categories2)
  plt.ylabel('Swelling (%)')
  plt.xlabel('Burn-up')
  plt.title('Annealing at 1800°C')
  plt.ylim([0,20])
  
  plt.legend(loc='best')
  
  plt.axhline(0, color='gray', linestyle='--', linewidth=1)
  plt.grid(color='gray', linestyle='--', linewidth=0.5, axis='y') 
  plt.minorticks_on()
  plt.grid(which='minor', color='lightgray', linestyle=':', linewidth=0.3, axis='y')

  plt.savefig('Images/SwellingSpecifico1800-Kashibe1990')
  plt.show()


  ################################# Median absolute deviations ####################################
  deviations_2 = abs(np.array(FGRAnnealing) - np.array(FGR2Annealing))
  deviations_Version2 = abs(np.array(FGRAnnealing) - np.array(AnnFGRVersion2))
  deviations_gold = abs(np.array(FGRAnnealing)-np.array(goldFGRAnnealing))

  print('FGR')
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
