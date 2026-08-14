"""
This is a python script to execute the regression (running the validation database) of SCIANTIX.

@author G. Nicodemo
"""

""" ------------------- Import Required Dependencies ------------------- """

import os
import numpy as np
import matplotlib.pyplot as plt
from regression_functions import *
from sklearn.linear_model import LinearRegression

""" ------------------- Global Variables ------------------- """
# Plot configuration
font_size = 10

plt.rcParams['axes.labelsize'] = font_size
plt.rcParams['xtick.labelsize'] = font_size
plt.rcParams['ytick.labelsize'] = font_size

# Intragranular gaseous swelling database from Baker 1977 experiments
igSwellingBaker = np.array([0.06, 0.07, 0.08, 0.09, 0.12, 0.15, 0.18, 0.24, 0.31])
igSwellingError = igSwellingBaker * np.sqrt(0.3**2 + 0.5**2)
temperature = np.array([1273, 1373, 1473, 1573, 1673, 1773, 1873, 1973, 2073])

# Data from SCIANTIX 2.0
intraGranularSwellingSciantix2 = np.array([0.0003073569, 0.0004259332, 0.0005552754, 0.0007511552, 0.001033993, 0.001326555, 0.001608567, 0.001869828, 0.002008811])*100

iXeDiffTurnbull  = np.array([4.275988e-20, 8.863954e-20, 1.885549e-19, 4.235173e-19, 1.018579e-18, 2.549942e-18, 6.346746e-18, 1.518164e-17, 3.434229e-17])
iXeDiffGPupdated = np.array([6.20967e-20, 1.818894e-19, 4.14139e-19, 1.144568e-18, 3.698875e-18, 1.134597e-17, 3.047513e-17, 7.407139e-17, 1.70611e-16])
upper_error = 10 * iXeDiffTurnbull
lower_error = iXeDiffTurnbull *9 / 10

# Data generated from SCIANTIX 2.0 with diffusivity updated by GPR
intraGranularSwellingSciantix2GPR = []

number_of_tests_failed = 0

""" ------------------- Functions ------------------- """

def do_plot():
  
  # SCIANTIX 2.0 Turbull diffusion coefficient vs. SCIANTIX 2.0 GPs updated diffusion coefficient: intragranular bubble swelling
  fig, ax = plt.subplots(1, 2, figsize=(12, 6))

  ax1 = ax[1]
  ax1.scatter(igSwellingBaker, intraGranularSwellingSciantix2, edgecolors='#757575', facecolors='red', marker='^', s=50, label='SCIANTIX - Turnbull Xe diffusion coefficient', zorder=1)
  ax1.scatter(igSwellingBaker, intraGranularSwellingSciantix2GPR, edgecolors='#757575', facecolors='green', marker='.', s=50, label='SCIANTIX - GPR updated Xe diffusion coefficient', zorder=2)
  ax1.plot([1e-3, 1e2], [1e-3, 1e2], '-', color='#757575')
  ax1.plot([1e-3, 1e2], [2e-3, 2e2], '--', color='#757575')
  ax1.plot([1e-3, 1e2], [5e-4, 5e1], '--', color='#757575')
  ax1.set_xlim(1e-2, 1)
  ax1.set_ylim(1e-2, 1)
  ax1.set_xscale('log')
  ax1.set_yscale('log')
  ax1.set_title('b) Predicted against measured intra-granular gas swelling')
  ax1.set_xlabel('Experimental (%)')
  ax1.set_ylabel('Calculated (%)')
  ax1.legend()

  ax2 = ax[0]
  
  ax2.errorbar(10 ** 4 / temperature, iXeDiffTurnbull, yerr=[lower_error, upper_error],edgecolors= None, color='red', fmt='^', capsize=4, capthick=1, ecolor='#999AA2', elinewidth = 0.6, label='Turnbull Xe diffusion coefficient', zorder=1)
  ax2.scatter(10 ** 4 / temperature, iXeDiffGPupdated, edgecolors='#757575', facecolors='green', marker='.', s=50, label='Updated Xe diffusion coefficient', zorder=2)
  ax2.set_yscale('log')
  ax2.set_title('a) Turnbull against updated Xe diffusion coefficient ')
  ax2.set_xlabel('$10^4/T$ (K)')
  ax2.set_ylabel('Diffusion coefficient ($m^2/s$)')
  ax2.legend()
  ax2.grid()
  
  plt.show()

def regression_baker_GPR(wpath, mode_Baker, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):
    """
    Main function to perform the regression tests on SCIANTIX.

    Parameters:
        wpath (str): Working path to search for folders.
        mode_Baker (int): Mode for executing tests.
        mode_gold (int): Mode for using/golding results.
        mode_plot (int): Mode for plotting results.
        folderList (list): List to store folder names.
        number_of_tests (int): Count of tests performed.
        number_of_tests_failed (int): Count of failed tests.

    Returns:
        folderList (list): Updated list of folder names.
        number_of_tests (int): Updated count of tests performed.
        number_of_tests_failed (int): Updated count of failed tests.
    """
    if mode_Baker == 0:
        return folderList, number_of_tests, number_of_tests_failed

    files_and_dirs = os.listdir(wpath)
    sorted_files_and_dirs = sorted(files_and_dirs)

    for file in sorted_files_and_dirs:
        if "GPR" in file and os.path.isdir(file):
            folderList.append(file)
            os.chdir(file)
            print(f"Now in folder {file}...")
            number_of_tests += 1

            if mode_gold == 0:
                do_sciantix_only()
                data, data_gold = check_output(file)
                number_of_tests_failed = check_result(number_of_tests_failed)

            elif mode_gold == 1:
                do_sciantix_only()
                data, data_gold = check_output(file)
                print("...golding results.")
                do_gold()

            elif mode_gold == 2:
                data, data_gold = check_output(file)
                number_of_tests_failed = check_result(number_of_tests_failed)

            elif mode_gold == 3:
                data, data_gold = check_output(file)
                print("...golding existing results.")
                do_gold()

            # FGRPos = findSciantixVariablePosition(data, "Fission gas release (/)")
            # FGR2.append(100 * data[-1, FGRPos].astype(float))

            intraGranularSwellingPos = findSciantixVariablePosition(data, "Intragranular gas bubble swelling (/)")
            intraGranularSwellingSciantix2GPR.append(100 * data[-1, intraGranularSwellingPos].astype(float))

            os.chdir('..')

    if mode_plot == 1:
        print("Plotting results...")
        do_plot()

    """ Statistical analysis """
    print(f"Experimental data - mean: ", np.mean(igSwellingBaker))
    print(f"Experimental data - median: ", np.median(igSwellingBaker))
    print(f"Experimental data - Q1: ", np.percentile(igSwellingBaker, 25))
    print(f"Experimental data - Q3: ", np.percentile(igSwellingBaker, 75))


    print(f"SCIANTIX 2.0 - mean: ", np.mean(intraGranularSwellingSciantix2))
    print(f"SCIANTIX 2.0 - median: ", np.median(intraGranularSwellingSciantix2))
    print(f"SCIANTIX 2.0 - Q1: ", np.percentile(intraGranularSwellingSciantix2, 25))
    print(f"SCIANTIX 2.0 - Q3: ", np.percentile(intraGranularSwellingSciantix2, 75))

    deviations_2 = abs(np.array(igSwellingBaker) - intraGranularSwellingSciantix2)
    print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_2))

    print(f"SCIANTIX 2.0 - RMSE: ", np.mean(np.array(igSwellingBaker) - intraGranularSwellingSciantix2)**2)
    print("\n")

    return folderList, number_of_tests, number_of_tests_failed
