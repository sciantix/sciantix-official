"""
This is a python script to execute the regression (running the validation database) of SCIANTIX.

@author G. Zullo
"""

""" ------------------- Import Required Dependencies ------------------- """

import os
import numpy as np
import matplotlib.pyplot as plt
import shutil
from regression_functions import *

""" ------------------- Global Variables ------------------- """
# Plot configuration
font_size = 10

plt.rcParams['axes.labelsize'] = font_size
plt.rcParams['xtick.labelsize'] = font_size
plt.rcParams['ytick.labelsize'] = font_size

# Data generated from SCIANTIX 2.0
igSwelling2 = []

# Intragranular bubble density from Baker 1977 experiments
igDensityKashibe = [9.0, 6.7] # 1e23 bub/m3
igDensity2 = []

igRadiusKashibe = [1.1e-9, 1.95e-9]
igRadius2 = []

FGR2 = []

number_of_tests_failed = 0
gold = []
sample_number = len(igDensityKashibe)

""" ------------------- Functions ------------------- """

def do_plot():
    """Generate plots comparing experimental data with results."""
    
    # Plot 1: Data vs. SCIANTIX 2.0: intragranular bubble swelling
    fig, ax = plt.subplots()

    ax.scatter(igDensityKashibe, igDensity2, c = '#FA82B4', edgecolors= '#999AA2', marker = '^', s=20, label='SCIANTIX 2.0')

    ax.plot([0.1, 100],[0.1, 100], '-', color = '#757575')
    ax.plot([0.1, 100],[0.05, 50],'--', color = '#757575')
    ax.plot([0.1, 100],[0.2, 200],'--', color = '#757575')
    ax.set_xlim(0.1, 100)
    ax.set_ylim(0.1, 100)

    ax.set_xscale('log')
    ax.set_yscale('log')

    ax.set_xlabel('Experimental (%)')
    ax.set_ylabel('Calculated (%)')
    ax.legend()

    plt.show()

    # Plot 1: Data vs. SCIANTIX 2.0: intragranular bubble radius
    fig, ax = plt.subplots()

    ax.scatter(igRadiusKashibe, igRadius2, c = '#FA82B4', edgecolors= '#999AA2', marker = '^', s=20, label='SCIANTIX 2.0')

    ax.plot([0.1e-9, 100e-9],[0.1e-9, 100e-9], '-', color = '#757575')
    ax.plot([0.1e-9, 100e-9],[0.05e-9, 50e-9],'--', color = '#757575')
    ax.plot([0.1e-9, 100e-9],[0.2e-9, 200e-9],'--', color = '#757575')
    ax.set_xlim(0.1e-9, 100e-9)
    ax.set_ylim(0.1e-9, 100e-9)

    ax.set_xscale('log')
    ax.set_yscale('log')

    ax.set_xlabel('Experimental (m)')
    ax.set_ylabel('Calculated (m)')
    ax.legend()

    plt.show()

def regression_kashibe(wpath, mode_Kashibe, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):

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
    if mode_Kashibe == 0 :
        return folderList, number_of_tests, number_of_tests_failed

    files_and_dirs = os.listdir(wpath)

    sorted_files_and_dirs = sorted(files_and_dirs)

    for file in sorted_files_and_dirs:
        if "Kashibe" in file and os.path.isdir(file):
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
            FGR2.append(100*data[-1,FGRPos].astype(float))

            intraGranularSwellingPos = findSciantixVariablePosition(data, "Intragranular gas bubble swelling (/)")
            igSwelling2.append(100*data[-1,intraGranularSwellingPos].astype(float))

            intraGranularSwellingGoldPos = findSciantixVariablePosition(data_gold, "Intragranular gas bubble swelling (/)")
            gold.append(100*data_gold[-1,intraGranularSwellingGoldPos].astype(float))

            pos = findSciantixVariablePosition(data, "Intragranular bubble concentration (bub/m3)")
            igDensity2.append(1e-23*data[-1,pos].astype(float))

            pos = findSciantixVariablePosition(data, "Intragranular bubble radius (m)")
            igRadius2.append(data[-1,pos].astype(float))

            os.chdir('..')

    if mode_plot == 1:
      do_plot()

    return folderList, number_of_tests, number_of_tests_failed
