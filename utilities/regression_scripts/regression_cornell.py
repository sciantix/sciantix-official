"""
This is a python script to execute the regression (running the validation database) of SCIANTIX.

@author G. Zullo
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

# Experimental data:
# Temperature (K), intra-granular bubble radius (nm), density (bub/m3)
exp_data = np.array([
    [1133, 0.85, 3.80],
    [1253, 0.90, 3.50],
    [1333, 0.95, 3.30],
    [1543, 1.00, 2.90],
    [1698, 1.00, 2.46],
    [1743, 1.15, 1.83],
    [1783, 1.20, 1.32],
    [1843, 1.30, 1.24],
    [1853, 1.40, 1.18]
])

intraGranularBubbleDensitySciantix = []
intraGranularBubbleRadiusSciantix = []

number_of_tests_failed = 0
gold = []
sample_number = len(exp_data[:,0])

""" ------------------- Functions ------------------- """

def do_plot():
    """Generate plots comparing experimental data with results."""

    # Plot 1: Intragranular bubble radius
    fig, ax = plt.subplots()
    ax.scatter(exp_data[:,1], intraGranularBubbleRadiusSciantix, edgecolors='#757575', facecolors='brown', marker='^', s=30, label='SCIANTIX 2.0', zorder=1)
    ax.plot([1e-3, 1e2], [1e-3, 1e2], '-', color='#757575')
    ax.plot([1e-3, 1e2], [2e-3, 2e2], '--', color='#757575')
    ax.plot([1e-3, 1e2], [5e-4, 5e1], '--', color='#757575')
    ax.set_xlim(1e-1, 1.5e1)
    ax.set_ylim(1e-1, 1.5e1)
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.set_xlabel('Experimental (nm)', fontsize=font_size)
    ax.set_ylabel('Calculated (nm)', fontsize=font_size)
    ax.tick_params(axis='both', which='major', labelsize=font_size)
    ax.legend()
    plt.show()

    # Plot 2: Intragranular bubble density
    fig, ax = plt.subplots()
    ax.scatter(exp_data[:,2], intraGranularBubbleDensitySciantix, edgecolors='#757575', facecolors='red', marker='^', s=30, label='SCIANTIX 2.0', zorder=1)
    ax.plot([1e-3, 1e2], [1e-3, 1e2], '-', color='#757575')
    ax.plot([1e-3, 1e2], [2e-3, 2e2], '--', color='#757575')
    ax.plot([1e-3, 1e2], [5e-4, 5e1], '--', color='#757575')
    ax.set_xlim(3e-1, 1.5e1)
    ax.set_ylim(3e-1, 1.5e1)
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.set_xlabel('Experimental (10^23 bub/m3)', fontsize=font_size)
    ax.set_ylabel('Calculated (10^23 bub/m3)', fontsize=font_size)
    ax.tick_params(axis='both', which='major', labelsize=font_size)
    ax.legend()
    plt.show()

def regression_cornell(wpath, mode_Baker, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):
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
        if "Cornell" in file and os.path.isdir(file):
            folderList.append(file)
            os.chdir(file)
            print(f"Now in folder {file}...")
            number_of_tests += 1

            if mode_gold == 0:
                do_sciantix()
                data, data_gold = check_output(file)
                number_of_tests_failed = check_result(number_of_tests_failed)

            elif mode_gold == 1:
                do_sciantix()
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

            pos = findSciantixVariablePosition(data, "Intragranular bubble concentration (bub/m3)")
            intraGranularBubbleDensitySciantix.append(data[-1, pos].astype(float)*1e-23)

            pos = findSciantixVariablePosition(data, "Intragranular bubble radius (m)")
            intraGranularBubbleRadiusSciantix.append(data[-1, pos].astype(float)*1e9)

            os.chdir('..')

    if mode_plot == 1:
        do_plot()

    # """ Statistical analysis """
    # print(f"Experimental data - mean: ", np.mean(igSwellingBaker))
    # print(f"Experimental data - median: ", np.median(igSwellingBaker))
    # print(f"Experimental data - Q1: ", np.percentile(igSwellingBaker, 25))
    # print(f"Experimental data - Q3: ", np.percentile(igSwellingBaker, 75))

    # print(f"SCIANTIX 1.0 - mean: ", np.mean(intraGranularSwellingSciantix1))
    # print(f"SCIANTIX 1.0 - median: ", np.median(intraGranularSwellingSciantix1))
    # print(f"SCIANTIX 1.0 - Q1: ", np.percentile(intraGranularSwellingSciantix1, 25))
    # print(f"SCIANTIX 1.0 - Q3: ", np.percentile(intraGranularSwellingSciantix1, 75))

    # print(f"SCIANTIX 2.0 - mean: ", np.mean(intraGranularSwellingSciantix))
    # print(f"SCIANTIX 2.0 - median: ", np.median(intraGranularSwellingSciantix))
    # print(f"SCIANTIX 2.0 - Q1: ", np.percentile(intraGranularSwellingSciantix, 25))
    # print(f"SCIANTIX 2.0 - Q3: ", np.percentile(intraGranularSwellingSciantix, 75))

    # deviations_1 = abs(np.array(igSwellingBaker) - intraGranularSwellingSciantix1)
    # deviations_2 = abs(np.array(igSwellingBaker) - intraGranularSwellingSciantix)
    # print(f"SCIANTIX 1.0 - MAD: ", np.median(deviations_1))
    # print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_2))

    # print(f"SCIANTIX 1.0 - RMSE: ", np.mean(np.array(igSwellingBaker) - intraGranularSwellingSciantix1)**2)
    # print(f"SCIANTIX 2.0 - RMSE: ", np.mean(np.array(igSwellingBaker) - intraGranularSwellingSciantix)**2)
    # print("\n")

    return folderList, number_of_tests, number_of_tests_failed
