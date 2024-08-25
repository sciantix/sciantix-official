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

# Intragranular gaseous swelling database from Baker 1977 experiments
igSwellingBaker = np.array([0.06, 0.07, 0.08, 0.09, 0.12, 0.15, 0.18, 0.24, 0.31])
igSwellingError = igSwellingBaker * np.sqrt(0.3**2 + 0.5**2)

# Data from SCIANTIX 1.0
igSwelling1 = np.array([0.033, 0.048, 0.062, 0.073, 0.079, 0.082, 0.083, 0.084, 0.086])
igRadius1 = np.array([0.515, 0.607, 0.681, 0.733, 0.763, 0.776, 0.782, 0.784, 0.785])  # nm
igDensity1 = np.array([5.78, 5.14, 4.69, 4.42, 4.27, 4.2, 4.18, 4.17, 4.17])  # 10^23 bub/m3

# Data generated from SCIANTIX 2.0
igSwelling2 = []

# Intragranular bubble density from Baker 1977 experiments
igDensityBaker = np.array([8.7, 7.8, 7, 6.4, 5.7, 5.3, 4.8, 4.4, 3.8])  # 1e23 bub/m3
igDensityError = igDensityBaker * 0.3
igDensity2 = []

igRadiusBaker = np.array([0.55e-9, 0.6e-9, 0.65e-9, 0.70e-9, 0.80e-9, 0.88e-9, 0.98e-9, 1.10e-9, 1.25e-9])
igRadiusError = igRadiusBaker * 0.5

igRadius2 = []
FGR2 = []

number_of_tests_failed = 0
gold = []
sample_number = len(igSwelling1)

""" ------------------- Functions ------------------- """

def do_plot():
    """Generate plots comparing experimental data with results."""
    igSwellingErrorVertL = np.abs(igSwelling2 - 100 * np.array([2.761719e-06] * 9))
    igSwellingErrorVertU = np.abs(igSwelling2 - 100 * np.array([0.001832503] * 9))

    # Plot 1: SCIANTIX 2.0-gold vs. SCIANTIX 2.0: intragranular bubble swelling
    fig, ax = plt.subplots()
    ax.scatter(igSwellingBaker, gold, edgecolors='#757575', facecolors='brown', marker='^', s=30, label='SCIANTIX 2.0 - Gold', zorder=1)
    ax.errorbar(igSwellingBaker, igSwelling2, edgecolors=None, color='green', fmt='^', capsize=1, capthick=1, ecolor='#999AA2', elinewidth=0.6, label='SCIANTIX 2.0 - Test', zorder=2)
    ax.plot([1e-3, 1e2], [1e-3, 1e2], '-', color='#757575')
    ax.plot([1e-3, 1e2], [2e-3, 2e2], '--', color='#757575')
    ax.plot([1e-3, 1e2], [5e-4, 5e1], '--', color='#757575')
    ax.set_xlim(0.5e-2, 1e1)
    ax.set_ylim(0.5e-2, 1e1)
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.set_xlabel('Experimental (%)', fontsize=font_size)
    ax.set_ylabel('Calculated (%)', fontsize=font_size)
    ax.tick_params(axis='both', which='major', labelsize=font_size)
    ax.legend()
    plt.show()

    # Plot 2: SCIANTIX 1.0 vs. SCIANTIX 2.0: intragranular bubble swelling + error bars
    fig, ax = plt.subplots()
    ax.scatter(igSwellingBaker, igSwelling1, edgecolors='#757575', facecolors='red', marker='^', s=30, label='SCIANTIX 1.0', zorder=1)
    ax.errorbar(igSwellingBaker, igSwelling2, xerr=igSwellingError, yerr=(igSwellingErrorVertL, igSwellingErrorVertU), edgecolors=None, color='green', fmt='^', capsize=1, capthick=1, ecolor='#999AA2', elinewidth=0.6, label='SCIANTIX 2.0', zorder=2)
    ax.plot([1e-3, 1e2], [1e-3, 1e2], '-', color='#757575')
    ax.plot([1e-3, 1e2], [2e-3, 2e2], '--', color='#757575')
    ax.plot([1e-3, 1e2], [5e-4, 5e1], '--', color='#757575')
    ax.set_xlim(0.5e-2, 1e1)
    ax.set_ylim(0.5e-2, 1e1)
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.set_xlabel('Experimental (%)', fontsize=font_size)
    ax.set_ylabel('Calculated (%)', fontsize=font_size)
    ax.tick_params(axis='both', which='major', labelsize=font_size)
    ax.legend()
    plt.show()

    # Plot 3: Data vs. SCIANTIX 2.0: bubble density + errorbar
    fig, ax = plt.subplots()
    ax.errorbar(igDensityBaker, igDensity1, xerr=igDensityError, c='#FA82B4', fmt='o', capsize=1, capthick=1, ecolor='#999AA2', elinewidth=0.6, label='SCIANTIX 1.0')
    ax.errorbar(igDensityBaker, igDensity2, xerr=igDensityError, c='green', fmt='o', capsize=1, capthick=1, ecolor='#999AA2', elinewidth=0.6, label='SCIANTIX 2.0')
    ax.plot([0.1, 100], [0.1, 100], '-', color='#757575')
    ax.plot([0.1, 100], [0.05, 50], '--', color='#757575')
    ax.plot([0.1, 100], [0.2, 200], '--', color='#757575')
    ax.set_xlim(0.1, 100)
    ax.set_ylim(0.1, 100)
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.set_xlabel('Experimental (10^23 bub / m3)', fontsize=font_size)
    ax.set_ylabel('Calculated (10^23 bub / m3)', fontsize=font_size)
    ax.tick_params(axis='both', which='major', labelsize=font_size)
    ax.legend()
    plt.show()


    # Plot 4: Data vs. SCIANTIX 2.0: bubble radius + errorbar
    fig, ax = plt.subplots()
    ax.errorbar(igRadiusBaker, igRadius1 * 1e-9, xerr=igRadiusError, c='#FA82B4', fmt='o', capsize=1, capthick=1, ecolor='#999AA2', elinewidth=0.6, label='SCIANTIX 1.0')
    ax.errorbar(igRadiusBaker, igRadius2, xerr=igRadiusError, c='green', fmt='o', capsize=1, capthick=1, ecolor='#999AA2', elinewidth=0.6, label='SCIANTIX 2.0')
    ax.plot([0.1e-9, 100e-9], [0.1e-9, 100e-9], '-', color='#757575')
    ax.plot([0.1e-9, 100e-9], [0.05e-9, 50e-9], '--', color='#757575')
    ax.plot([0.1e-9, 100e-9], [0.2e-9, 200e-9], '--', color='#757575')
    ax.set_xlim(0.1e-9, 100e-9)
    ax.set_ylim(0.1e-9, 100e-9)
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.set_xlabel('Experimental (m)', fontsize=font_size)
    ax.set_ylabel('Calculated (m)', fontsize=font_size)
    ax.tick_params(axis='both', which='major', labelsize=font_size)
    ax.legend()
    plt.show()

def regression_baker(wpath, mode_Baker, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):
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
        if "Baker" in file and os.path.isdir(file):
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

            FGRPos = findSciantixVariablePosition(data, "Fission gas release (/)")
            FGR2.append(100 * data[-1, FGRPos].astype(float))

            intraGranularSwellingPos = findSciantixVariablePosition(data, "Intragranular gas bubble swelling (/)")
            igSwelling2.append(100 * data[-1, intraGranularSwellingPos].astype(float))

            intraGranularSwellingGoldPos = findSciantixVariablePosition(data_gold, "Intragranular gas bubble swelling (/)")
            gold.append(100 * data_gold[-1, intraGranularSwellingGoldPos].astype(float))

            pos = findSciantixVariablePosition(data, "Intragranular bubble concentration (bub/m3)")
            igDensity2.append(1e-23 * data[-1, pos].astype(float))

            pos = findSciantixVariablePosition(data, "Intragranular bubble radius (m)")
            igRadius2.append(data[-1, pos].astype(float))

            os.chdir('..')

    if mode_plot == 1:
        do_plot()

    """ Statistical analysis """
    print(f"Experimental data - mean: ", np.mean(igSwellingBaker))
    print(f"Experimental data - median: ", np.median(igSwellingBaker))
    print(f"Experimental data - Q1: ", np.percentile(igSwellingBaker, 25))
    print(f"Experimental data - Q3: ", np.percentile(igSwellingBaker, 75))

    print(f"SCIANTIX 1.0 - mean: ", np.mean(igSwelling1))
    print(f"SCIANTIX 1.0 - median: ", np.median(igSwelling1))
    print(f"SCIANTIX 1.0 - Q1: ", np.percentile(igSwelling1, 25))
    print(f"SCIANTIX 1.0 - Q3: ", np.percentile(igSwelling1, 75))

    print(f"SCIANTIX 2.0 - mean: ", np.mean(igSwelling2))
    print(f"SCIANTIX 2.0 - median: ", np.median(igSwelling2))
    print(f"SCIANTIX 2.0 - Q1: ", np.percentile(igSwelling2, 25))
    print(f"SCIANTIX 2.0 - Q3: ", np.percentile(igSwelling2, 75))

    deviations_1 = abs(np.array(igSwellingBaker) - igSwelling1)
    deviations_2 = abs(np.array(igSwellingBaker) - igSwelling2)
    print(f"SCIANTIX 1.0 - MAD: ", np.median(deviations_1))
    print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_2))

    print(f"SCIANTIX 1.0 - RMSE: ", np.mean(np.array(igSwellingBaker) - igSwelling1)**2)
    print(f"SCIANTIX 2.0 - RMSE: ", np.mean(np.array(igSwellingBaker) - igSwelling2)**2)
    print("\n")

    return folderList, number_of_tests, number_of_tests_failed
