"""
This is a python script to execute the regression (running the validation database) of SCIANTIX.

@author G. Zullo
"""

""" ------------------- Import Required Dependencies ------------------- """

import os
import subprocess
import sys
import numpy as np
import matplotlib.pyplot as plt
import shutil
from regression_functions import *
import scipy.stats as stats
from sklearn.linear_model import LinearRegression

""" ------------------- Global Variables ------------------- """
# Plot configuration
font_size = 18

plt.rcParams['axes.labelsize'] = font_size
plt.rcParams['xtick.labelsize'] = font_size
plt.rcParams['ytick.labelsize'] = font_size
plt.rcParams['font.size'] = font_size
plt.rcParams['legend.fontsize'] = font_size 
plt.rcParams['lines.markersize'] = 8

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

goldFGR = []
goldFGRAnnealing = []

number_of_tests_failed = 0
sample_number = len(FGR2)


""" ------------------- Functions ------------------- """

def do_plot():
  
    for i in range(len(FGR2)):
      FGR2Annealing.append(FGR2[i] - FGRBase[i])
      goldFGRAnnealing.append(goldFGR[i] - FGRBaseGold[i])

    categories = ['10 K s$^{-1}$', '1.7 K s$^{-1}$','0.5 K s$^{-1}$', '0.17 K s$^{-1}$', '0.03 K s$^{-1}$']

    rates1_index = [4, 9]
    rates2_index = [5, 10]
    rates3_index = [6]
    rates4_index = [7, 11]
    rates5_index = [8, 12]

    burnup23_index = [4, 5, 6, 7, 8]
    burnup28_index = [9, 10, 11, 12]  

    fig, ax = plt.subplots(figsize=(8, 8))

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


    ax.set_xlabel('FGR experimental (%)')
    ax.set_ylabel('FGR calculated (%)')

    custom_lines = [plt.Line2D([0], [0], color='k', marker='o', linestyle='', markersize=8, label='23 GWd tU$^{-1}$'),
                    plt.Line2D([0], [0], color='k', marker='^', linestyle='', markersize=8, label='28 GWd tU$^{-1}$'),
                    plt.Line2D([0], [0], color='C0', marker='s', linestyle='', markersize=8, label=categories[0]),
                    plt.Line2D([0], [0], color='C2', marker='s', linestyle='', markersize=8, label=categories[1]),
                    plt.Line2D([0], [0], color='C3', marker='s', linestyle='', markersize=8, label=categories[2]),
                    plt.Line2D([0], [0], color='C4', marker='s', linestyle='', markersize=8, label=categories[3]),
                    plt.Line2D([0], [0], color='C5', marker='s', linestyle='', markersize=8, label=categories[4])]
    ax.legend(handles=custom_lines, loc='best', frameon=False)
    plt.tight_layout()
    #plt.savefig('FGRRate_Kashibe1991.png')
    plt.show()

    subprocess.run([sys.executable, "regression_kashibe1991_2.py"])  # For Windows/Linux/macOS
    
    ###########################################################################
    
    ###########################################################################

        # Median absolute deviations
    deviations_2 = abs(np.array(FGRAnnealing) - np.array(FGR2Annealing))
    deviations_gold = abs(np.array(FGRAnnealing)-np.array(goldFGRAnnealing))
    
    print(f"This work - MAD: ", np.median(deviations_2))
    print(f"Gold - MAD: ", np.median(deviations_gold))

# Main function of the baker regression
def regression_kashibe1991(wpath, mode_Kashibe1991, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):
    k=0
    # Exit of the function without doing anything
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
    if mode_Kashibe1991 == 0 :
      return folderList, number_of_tests, number_of_tests_failed

    files_and_dirs = os.listdir(wpath)

    sorted_files_and_dirs = sorted(files_and_dirs)

    for file in sorted_files_and_dirs:
        if "_Kashibe1991" in file and os.path.isdir(file):
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

    if mode_plot == 1:
      do_plot()

    return folderList, number_of_tests, number_of_tests_failed, FGR2Annealing, FGRAnnealing, goldFGRAnnealing
