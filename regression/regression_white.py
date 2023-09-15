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

# Data from SCIANTIX 1.0
gbSwelling1 = [1.19, 1.16, 1.13, 1.12, 1.29,                   # 4000
               0.91, 0.89, 0.85, 0.79, 0.77, 0.81,             # 4004
               0.89, 0.87, 0.82, 0.76, 0.79,                   # 4005
               1.89, 1.83, 1.68, 1.46, 1.33,                   # 4064
               1.52, 1.48, 1.41, 1.35, 1.31,                   # 4065
               0.79, 0.79, 0.77,                               # 4135
               0.83, 0.83, 0.81, 0.79,                         # 4136
               0.87, 0.86,                                     # 4140
               1.72, 1.69, 1.62, 1.49,                         # 4162
               0.85, 0.84, 0.83, 0.82                          # 4163
               ]

# Intergranular gaseous swelling database from White et al. (2006) experiments
gbConcWhite = [0.68, 0.80, 1.32, 1.99, 9.00,                   # 4000
               1.39, 2.12, 3.55, 1.87, 8.51, 27.90,            # 4004
               1.21, 2.93, 2.57, 2.62, 10.62,                  # 4005
               0.95, 0.87, 1.35, 0.74, 3.51,                   # 4064
               0.25, 0.37, 0.49, 0.88, 4.07,                   # 4065
               1.03, 1.52, 1.78,                               # 4135
               0.83, 0.59, 1.19, 1.69,                         # 4136
               1.59, 2.18,                                     # 4140
               3.15, 3.98, 4.92, 5.38,                         # 4162
               2.63, 3.56, 5.19, 5.7                           # 4163
               ]

# Intergranular gaseous swelling database from White et al. (2006) experiments
gbSwellingWhite = [0.97, 0.68, 0.53, 0.46, 0.17,                   # 4000
                   0.62, 0.7, 0.44, 0.56, 0.27, 0.16,              # 4004
                   0.94, 0.57, 0.42, 0.54, 0.27,                   # 4005
                   1.07, 0.86, 0.63, 0.74, 0.59,                   # 4064
                   1.25, 1.35, 0.97, 0.79, 0.21,                   # 4065
                   0.42, 0.16, 0.09,                               # 4135
                   0.6, 0.62, 0.26, 0.11,                          # 4136
                   0.26, 0.18,                                     # 4140
                   0.7, 0.46, 0.43, 0.43,                          # 4162
                   0.6, 0.59, 0.35, 0.4                            # 4163
                   ]
# Data generated from SCIANTIX 2.0
gbSwelling2 = []
FGR2 = []
bbarea2 = []
bbconc = []
gold = []

bbarea2_gold = []
bbconc_gold = []

sample_number = len(gbSwelling1)


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
def do_plot():
  fig, ax = plt.subplots()

  ax.scatter(gbSwellingWhite, gbSwelling1, c = '#FA82B4', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 1.0')
  ax.scatter(gbSwellingWhite, gbSwelling2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 2.0')

  ax.plot([1e-3, 1e2],[1e-3, 1e2], '-', color = '#757575')
  ax.plot([1e-3, 1e2],[2e-3, 2e2],'--', color = '#757575')
  ax.plot([1e-3, 1e2],[5e-4, 5e1],'--', color = '#757575')

  ax.set_xlim(1e-2, 1e1)
  ax.set_ylim(1e-2, 1e1)

  ax.set_xscale('log')
  ax.set_yscale('log')

  # ax.set_title('Intergranular gaseous swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()

  plt.show()

  # GOLD vs. SCIANTIX 2.0
  fig, ax = plt.subplots()

  ax.scatter(gbSwellingWhite, gold, c = '#C9C954', edgecolors= '#999AA2', marker = 'o', s=20, label='Gold')
  ax.scatter(gbSwellingWhite, gbSwelling2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 2.0')

  ax.plot([1e-3, 1e2],[1e-3, 1e2], '-', color = '#757575')
  ax.plot([1e-3, 1e2],[2e-3, 2e2],'--', color = '#757575')
  ax.plot([1e-3, 1e2],[5e-4, 5e1],'--', color = '#757575')

  ax.set_xlim(1e-2, 1e1)
  ax.set_ylim(1e-2, 1e1)

  ax.set_xscale('log')
  ax.set_yscale('log')

  # ax.set_title('Intergranular gaseous swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()

  plt.show()

  # GOLD vs. SCIANTIX 2.0 - area
  fig, ax = plt.subplots()

  ax.scatter(gbSwellingWhite, bbarea2_gold, c = '#C9C954', edgecolors= '#999AA2', marker = 'o', s=20, label='Gold')
  ax.scatter(gbSwellingWhite, bbarea2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 2.0')

  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.set_xlabel('Experimental swelling(%)')
  ax.set_ylabel('Calculated bubble area (m2)')
  ax.legend()

  plt.show()

  # GOLD vs. SCIANTIX 2.0 - conc
  fig, ax = plt.subplots()

  ax.scatter(gbConcWhite, bbconc_gold, c = '#C9C954', edgecolors= '#999AA2', marker = 'o', s=20, label='Gold')
  ax.scatter(gbConcWhite, bbconc, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 2.0')

  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.set_xlabel('Experimental (bub/m2)')
  ax.set_ylabel('Calculated (mbub/2)')
  ax.legend()

  plt.show()

  # FGR plot
  fig, ax = plt.subplots()
  ax.scatter(gbSwelling2, FGR2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='FGR SCIANTIX 2.0')

  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.set_xlabel('Swelling (%)')
  ax.set_ylabel('FGR (%)')
  ax.legend()

  plt.show()

  fig, axfig = plt.subplots(1,2)

  ax = axfig[0]
  ax.scatter(gbSwellingWhite, gbSwelling1, c = '#FA82B4', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 1.0')
  ax.scatter(gbSwellingWhite, gbSwelling2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 2.0')

  ax.plot([1e-3, 1e2],[1e-3, 1e2], '-', color = '#757575')
  ax.plot([1e-3, 1e2],[2e-3, 2e2],'--', color = '#757575')
  ax.plot([1e-3, 1e2],[5e-4, 5e1],'--', color = '#757575')

  ax.set_xlim(1e-2, 1e1)
  ax.set_ylim(1e-2, 1e1)

  ax.set_xscale('log')
  ax.set_yscale('log')

  # ax.set_title('Intergranular gaseous swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()

  # Box plot
  ax = axfig[1]

  data = [gbSwellingWhite, gbSwelling1, gbSwelling2]
  # plt.boxplot(data)

  bp = ax.boxplot(data, patch_artist = True)
  colors = ['#80C7E6', '#FA82B4', '#98E18D']

  for patch, color in zip(bp['boxes'], colors):
    patch.set_facecolor(color)

  for whisker in bp['whiskers']:
    whisker.set(color ='#8B008B', linewidth = 1.5, linestyle =":")

  for cap in bp['caps']:
    cap.set(color ='#8B008B', linewidth = 2)

  for median in bp['medians']:
    median.set(color ='black', linewidth = 2)

  for flier in bp['fliers']:
    flier.set(marker ='D', color ='#e7298a', alpha = 0.5)

  ax.tick_params(axis='y', which='minor', bottom=False)
  ax.minorticks_on()
  ax.grid(which='major', color='k', linestyle='--', alpha=0.2)
  ax.grid(which='minor', color='k', linestyle=':', alpha=0.2)

  ax.set_xticklabels(['Experimental', 'SCIANTIX 1.0', 'SCIANTIX 2.0'])
  ax.set_ylabel("Swelling (%)")

  # plt.title("Box plot")

  ax.get_xaxis().tick_bottom()
  ax.get_yaxis().tick_left()

  plt.show()
  # plt.savefig("boxplot_gb_swelling.png")


# Main function of the white regression
def regression_white(wpath, mode_White, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):

  # Exit of the function without doing anything
  if mode_White == 0:
    return folderList, number_of_tests, number_of_tests_failed

  # Get list of all files and directories in wpath
  files_and_dirs = os.listdir(wpath)

  # Sort them by filename
  sorted_files_and_dirs = sorted(files_and_dirs)

  # Iterate over sorted list
  for file in sorted_files_and_dirs:
    # Verify on a given folder, if Baker is in it's name
    if "White" in file and os.path.isdir(file):
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


      # Retrieve the generated data of Intergranular gas swelling
      interGranularSwellingPos = findSciantixVariablePosition(data, "Intergranular gas swelling (/)")
      gbSwelling2.append(100*data[-1,interGranularSwellingPos].astype(float))

      # Retrieve the gold data of Intergranular gas swelling
      interGranularSwellingGoldPos = findSciantixVariablePosition(data_gold, "Intergranular gas swelling (/)")
      gold.append(100*data_gold[-1,interGranularSwellingGoldPos].astype(float))

      # Retrieve the generated data of Intergranular bubble area (m2)
      bubbleAreaPos = findSciantixVariablePosition(data, "Intergranular bubble area (m2)")
      bbarea2.append(100*data[-1,bubbleAreaPos].astype(float))

      # Retrieve the gold data of Intergranular bubble area (m2)
      bubbleAreaGoldPos = findSciantixVariablePosition(data_gold, "Intergranular bubble area (m2)")
      bbarea2_gold.append(100*data_gold[-1,bubbleAreaGoldPos].astype(float))

      # Retrieve the generated data of Fission gas release
      FGRPos = findSciantixVariablePosition(data, "Fission gas release (/)")
      FGR2.append(100*data[-1,FGRPos].astype(float))

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

  """ Statistical analysis """
  # Experimental data: mean, median, ...
  print(f"Experimental data - mean: ", np.mean(gbSwellingWhite))
  print(f"Experimental data - median: ", np.median(gbSwellingWhite))
  print(f"Experimental data - Q1: ", np.percentile(gbSwellingWhite, 25, interpolation = 'midpoint'))
  print(f"Experimental data - Q3: ", np.percentile(gbSwellingWhite, 75, interpolation = 'midpoint'))

  # SCIANTIX 1.0: mean, median, ...
  print(f"SCIANTIX 1.0 - mean: ", np.mean(gbSwelling1))
  print(f"SCIANTIX 1.0 - median: ", np.median(gbSwelling1))
  print(f"SCIANTIX 1.0 - Q1: ", np.percentile(gbSwelling1, 25, interpolation = 'midpoint'))
  print(f"SCIANTIX 1.0 - Q3: ", np.percentile(gbSwelling1, 75, interpolation = 'midpoint'))

  # SCIANTIX 2.0: mean and median
  print(f"SCIANTIX 2.0 - mean: ", np.mean(gbSwelling2))
  print(f"SCIANTIX 2.0 - median: ", np.median(gbSwelling2))
  print(f"SCIANTIX 2.0 - Q1: ", np.percentile(gbSwelling2, 25, interpolation = 'midpoint'))
  print(f"SCIANTIX 2.0 - Q3: ", np.percentile(gbSwelling2, 75, interpolation = 'midpoint'))

  # Median absolute deviations
  deviations_1 = abs(np.array(gbSwellingWhite) - gbSwelling1)
  deviations_2 = abs(np.array(gbSwellingWhite) - gbSwelling2)
  print(f"SCIANTIX 1.0 - MAD: ", np.median(deviations_1))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_2))

  return folderList, number_of_tests, number_of_tests_failed
