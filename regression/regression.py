"""
This is a python script to execute the regression (running the validation database) of SCIANTIX.

@author G. Zullo
"""

""" ------------------- Import required dependencies ------------------- """
import os, sys
import subprocess
import numpy as np
import matplotlib.pyplot as plt
import shutil

from regression_baker import regression_baker
from regression_white import regression_white
from regression_talip import regression_talip
from regression_contact import regression_contact
from regression_oxidation import regression_oxidation
from regression_kashibe1993 import regression_kashibe1993
from regression_kashibe1990 import regression_kashibe1990
from regression_kashibe1991 import regression_kashibe1991
from regression_hbs import regression_hbs
from regression_chromium import regression_chromium
from regression_cornell import regression_cornell

def remove_output(file):
    os.chdir(file)
    print(f"Now in folder {file}...")
    try:
        os.remove("output.txt")
    except FileNotFoundError:
        print("no output.txt")
    os.chdir('..')

def get_user_choices():
    print("\n-----------------This script executes SCIANTIX into the validation cases-----------------\n")
    print("\tExecution option == 0 USE DEFAULT MODES")
    print("\tExecution option == 1 PERSONALISED MODES")
    print("\tExecution option == 2 REMOVE ALL OUTPUT FILES")
    return int(input("\nEnter Execution option (0, 1, 2) = "))

def get_mode_selections():
    print("Please select one option for the GOLD MODE :\n")
    print("\tMODE GOLD == 0: use SCIANTIX, check new results.")
    print("\tMODE GOLD == 1: use SCIANTIX, new results will be saved as gold results.")
    print("\tMODE GOLD == 2: do not use SCIANTIX, check existing results.")
    print("\tMODE GOLD == 3: do not use SCIANTIX, existing results will be saved as gold results.\n")
    mode_gold = int(input("Enter MODE GOLD (0, 1, 2, 3)= "))
    mode_plot = int(input("Enter MODE PLOT (0 or 1)= "))
    return mode_gold, mode_plot

" ------------------- Main part -------------------"
def main():
    shutil.copy("../build/sciantix.x", os.getcwd())

    wpath = os.path.dirname(os.path.realpath(__file__))
    os.chdir(wpath)

    folderList = []
    total_tests = 0
    total_tests_failed = 0

    regression_modules = [
        ('Baker', regression_baker),
        ('Kashibe 1990', regression_kashibe1990),
        ('Kashibe 1991', regression_kashibe1991),
        ('Kashibe 1993', regression_kashibe1993),
        ('White', regression_white),
        ('Talip', regression_talip),
        ('CONTACT', regression_contact),
        ('oxidation', regression_oxidation),
        ('HBS', regression_hbs),
        ('Chromium', regression_chromium),
        ('Cornell', regression_cornell)
    ]

    # Check if running in GitHub Actions environment
    if os.environ.get('GITHUB_ACTIONS') == 'true':
        # Set default execution modes for GitHub Actions
        mode_gold = 0
        mode_plot = 0
        test_condition = 1

        # Run all regression tests with default modes
        for name, func in regression_modules:
            folderList_part, tests_count, tests_failed_count = func(
                wpath, 1, mode_gold, mode_plot, [], 0, 0) [0:3]
            folderList += folderList_part
            total_tests += tests_count
            total_tests_failed += tests_failed_count

    else:  # User-run environment
        test_condition = 0
        execution_option = get_user_choices()

        # Option 2: Remove all output files
        if execution_option == 2:
            directories = ["Baker", "Kashibe 1990", "Kashibe 1991", "Kashibe 1993", "White", "Talip", "CONTACT", "oxidation", "HBS", "Chromium", "Cornell"]
            for file in os.listdir(wpath):
                if any(dir_name in file for dir_name in directories) and os.path.isdir(file):
                    remove_output(file)
            mode_gold, mode_plot = -1, -1

        # Option 0: Use default modes
        elif execution_option == 0:
            mode_gold, mode_plot = get_mode_selections()

            # Run all regression tests with default modes
            for name, func in regression_modules:
                folderList_part, tests_count, tests_failed_count = func(
                    wpath, 1, mode_gold, mode_plot, [], 0, 0) [0:3]
                folderList += folderList_part
                total_tests += tests_count
                total_tests_failed += tests_failed_count

        # Option 1: Personalized modes
        elif execution_option == 1:
            print("Possible regression options \n")
            for i, (name, _) in enumerate(regression_modules):
                print(f"{name} : {i}")
            regression_mode = int(input("Enter the chosen regression (0, 1, 2, 3, 4, 5, 6, 7, 8) = "))

            mode_gold, mode_plot = get_mode_selections()

            # Run the selected regression test
            mode_name, regression_func = regression_modules[regression_mode]
            regression_file = regression_func(
                wpath, 1, mode_gold, mode_plot, [], 0, 0)
            folderList = regression_file[0]
            tests_count = regression_file[1]
            tests_failed_count = regression_file[2]
            total_tests += tests_count
            total_tests_failed += tests_failed_count
            print(f"\nRegression selected: {mode_name}")

    print("MODE GOLD ==", mode_gold, "selected.")
    print("MODE PLOT ==", mode_plot, "selected.\n")

    print("-----------------SUMMARY-----------------")
    print("- List of tests performed:\n", folderList, "\n")
    print("! Number of tests =", total_tests)
    print("! Number of tests passed =", total_tests - total_tests_failed)
    print("! Number of tests failed =", total_tests_failed, "\n")

    if test_condition == 1:
        if total_tests_failed > 0:
            print("-----------------ONE OR MORE TESTS HAVE FAILED-----------------")
            sys.exit(1)

if __name__ == "__main__":
    main()
