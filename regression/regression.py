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

from regression_baker import regression_baker
from regression_white import regression_white
from regression_talip import regression_talip
from regression_contact import regression_contact
from regression_oxidation import regression_oxidation
from regression_kashibe1993 import regression_kashibe1993
from regression_kashibe1990 import regression_kashibe1990
from regression_kashibe1991 import regression_kashibe1991
from regression_hbs import regression_hbs
from regression_small1988 import regression_small1988
from regression_small1985 import regression_small1985

# The function `remove_routput` is used to remove an existing output file (output.txt) from a specified folder
def remove_output(file):
    # Change current working directory to the specified file's directory
    os.chdir(file)
    print(f"Now in folder {file}...")
    try :
        os.remove("output.txt")
    except :
        print("no output.txt")
    os.chdir('..') # Change working directory back to the parent directory

" ------------------- Main part -------------------"
def main():

    # Copy the file 'sciantix.x' from the parent directory's 'bin' folder to the current directory
    shutil.copy("../bin/sciantix.x", os.getcwd())

    # Stock the directory path of the current file
    wpath = os.path.dirname(os.path.realpath(__file__))
    os.chdir(wpath) # Change the working directory to the path stored in 'wpath'

    # Initialize different variables needed for the execution :
    # - A list 'folderList' to store the names of every test that will be executed. Different variations of this list are initialized for different test types.
    folderList = folderListB = folderListK = folderListW = folderListT = folderListC = folderListO = folderListH = folderListK1990 = folderListS = folderListS1985 = folderListK1991= []
    # - Variables to count the number of executed tests. Different counts are maintained for different test types.
    number_of_tests = number_of_tests_b = number_of_tests_k = number_of_tests_w = number_of_tests_t = number_of_tests_c = number_of_tests_o = number_of_tests_h = number_of_tests_k1990 = number_of_tests_s = number_of_tests_s1985 = number_of_tests_k1991= 0
    # - Variables to count the number of failed tests. Different counts are maintained for different test types.
    number_of_tests_failed = number_of_tests_failed_b = number_of_tests_failed_k = number_of_tests_failed_w = number_of_tests_failed_t = number_of_tests_failed_c = number_of_tests_failed_o = number_of_tests_failed_h = number_of_tests_failed_k1990 = number_of_tests_failed_s = number_of_tests_failed_s1985 = number_of_tests_failed_k1991 = 0

    # If the environment variable 'GITHUB_ACTIONS' is set to 'true', this means the script is running in a GitHub Actions environment.
    # In this case, specific versions of the variables are set for the pipeline environment with default values.
    # Each test is executed with 'sciantix', and no modifications are made to the gold standard test results.
    if os.environ.get('GITHUB_ACTIONS') == 'true':
        # Set default values for the execution modes.
        mode_gold = 0
        mode_plot = 0
        mode_Baker = 1
        mode_Kashibe1993 = 1
        mode_Kashibe1990 = 1
        mode_White = 1
        mode_Talip = 1
        mode_CONTACT = 1
        mode_oxidation = 1
        mode_hbs = 1
        mode_Small1988 = 1
        mode_Small1985 = 1
        mode_Kashibe1991 = 1
        # Set the test condition to '0' or '1'
        test_condition = 1

        # Run regression tests for each regression mode and update the test list and test counts accordingly.
        folderListB, number_of_tests_b, number_of_tests_failed_b = regression_baker(wpath, mode_Baker, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        folderListK, number_of_tests_k, number_of_tests_failed_k = regression_kashibe1993(wpath, mode_Kashibe1993, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        folderListW, number_of_tests_w, number_of_tests_failed_w, calculated, measured = regression_white(wpath, mode_White, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        folderListT, number_of_tests_t, number_of_tests_failed_t = regression_talip(wpath, mode_Talip, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        folderListC, number_of_tests_c, number_of_tests_failed_c = regression_contact(wpath, mode_CONTACT, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        folderListO, number_of_tests_o, number_of_tests_failed_o = regression_oxidation(wpath, mode_oxidation, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        folderListH, number_of_tests_h, number_of_tests_failed_h = regression_hbs(wpath, mode_hbs, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        folderListK1990, number_of_tests_k1990, number_of_tests_failed_k1990, calculatedS, measuredS,  calculatedF, measuredF = regression_kashibe1990(wpath, mode_Kashibe1990, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        folderListS, number_of_tests_s, number_of_tests_failed_s, calculated, measured = regression_small1988(wpath, mode_Small1988, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        folderListS1985, number_of_tests_s1985, number_of_tests_failed_s1985, calculated, measured = regression_small1985(wpath, mode_Small1985, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        folderListK1991, number_of_tests_k1991, number_of_tests_failed_k1991, calculated, measured = regression_kashibe1991(wpath, mode_Kashibe1991, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
        
        # Combine the test lists from the different modes into one comprehensive list.
        folderList = folderListB + folderListK + folderListW + folderListT + folderListC + folderListO + folderListH + folderListK1990 + folderListS + folderListS1985 +folderListK1991
        # Add up the counts of the executed tests from the different modes.
        number_of_tests = number_of_tests_b +  number_of_tests_k + number_of_tests_w + number_of_tests_t + number_of_tests_c + number_of_tests_o + number_of_tests_h + number_of_tests_k1990 + number_of_tests_s + number_of_tests_s1985 + number_of_tests_k1991
        # Add up the counts of the failed tests from the different modes.
        number_of_tests_failed = number_of_tests_failed_b + number_of_tests_failed_k + number_of_tests_failed_w + number_of_tests_failed_t + number_of_tests_failed_c + number_of_tests_failed_o + number_of_tests_failed_h + number_of_tests_failed_k1990 + number_of_tests_failed_s + number_of_tests_failed_s1985 + number_of_tests_failed_k1991



    # If the environment variable 'GITHUB_ACTIONS' is not set to 'true', this means the script is not running in a GitHub Actions environment. It's being run by a user.
    if os.environ.get('GITHUB_ACTIONS') != 'true':
        # Initialize test_condition to '0'. This means no specific test conditions are set for user-run tests.
        test_condition = 0

        # Provide options for the user to choose the execution mode.
        print("\n-----------------This script executes SCIANTIX into the validation cases-----------------\n")
        print("\tExecution option == 0 USE DEFAULT MODES")
        print("\tExecution option == 1 PERSONALISED MODES")
        print("\tExecution option == 2 REMOVE ALL OUTPUT FILES")
        execution_option = int(input("\nEnter Execution option (0, 1, 2) = ")) # Take the user's input for the execution option.

        # If execution_option is set to '2', this means the user wants to remove all output files.
        if execution_option == 2 :
            # For each file in the working directory, if the file is a directory and its name contains 'Baker', 'White', 'Talip', 'CONTACT', or 'oxidation', then run the 'remove_output' function on that file.
            for file in os.listdir(wpath):
                if "Baker" in file and os.path.isdir(file) is True:
                    remove_output(file)
                if "Kashibe1993" in file and os.path.isdir(file) is True:
                    remove_output(file)
                if "White" in file and os.path.isdir(file) is True:
                    remove_output(file)
                if "Talip" in file and os.path.isdir(file) is True:
                    remove_output(file)
                if "CONTACT" in file and os.path.isdir(file) is True:
                    remove_output(file)
                if "oxidation" in file and os.path.isdir(file) is True:
                    remove_output(file)
                if "HBS" in file and os.path.isdir(file) is True:
                    remove_output(file)
                if "Kashibe1990" in file and os.path.isdir(file) is True:
                    remove_output(file)
                if "Small1988" in file and os.path.isdir(file) is True:
                    remove_output(file)
                if "Small1985" in file and os.path.isdir(file) is True:
                    remove_output(file)
                if "Kashibe1991" in file and os.path.isdir(file) is True:
                    remove_output(file)
                # Set the gold mode and plot mode to '-1'. This means these modes are not in use when removing output files.
                mode_gold = -1
                mode_plot = -1

        # If execution_option is set to '0', this means the user wants to use default execution modes.
        if execution_option == 0 :

            # Set the default values for the Baker, White, Talip, Contact, and Oxidation modes.
            mode_Baker = 1
            mode_Kashibe1993 = 1
            mode_Kashibe1990 = 1
            mode_White = 1
            mode_Talip = 1
            mode_CONTACT = 1
            mode_oxidation = 1
            mode_hbs = 1
            mode_Small1988 = 1
            mode_Small1985 = 1
            mode_Kashibe1991 = 1

            # Ask the user to choose an option for the gold mode.
            print("Pleast select one option for the GOLD MODE :\n")
            print("\tMODE GOLD == 0: use SCIANTIX, check new results.\n")
            print("\tMODE GOLD == 1: use SCIANTIX, new results will be saved as gold results.\n ")
            print("\tMODE GOLD == 2: do not use SCIANTIX, check existing results.\n ")
            print("\tMODE GOLD == 3: do not use SCIANTIX, existing results will be saved as gold results.\n ")

            # Take the user's input for the gold mode.
            mode_gold = int(input("Enter MODE GOLD (0, 1, 2, 3)= "))

            # Ask the user to choose an option for the plot mode.
            mode_plot = int(input("Enter MODE PLOT (0 or 1)= "))

            # For each execution mode (Baker, White, Talip, Contact, Oxidation), run the corresponding regression test function and update the test list and test counts accordingly.
            folderListB, number_of_tests_b, number_of_tests_failed_b = regression_baker(wpath, mode_Baker, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            folderListK, number_of_tests_k, number_of_tests_failed_k = regression_kashibe1993(wpath, mode_Kashibe1993, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            folderListW, number_of_tests_w, number_of_tests_failed_w, calculated, measured  = regression_white(wpath, mode_White, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            folderListT, number_of_tests_t, number_of_tests_failed_t = regression_talip(wpath, mode_Talip, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            folderListC, number_of_tests_c, number_of_tests_failed_c = regression_contact(wpath, mode_CONTACT, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            folderListO, number_of_tests_o, number_of_tests_failed_o = regression_oxidation(wpath, mode_oxidation, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            folderListH, number_of_tests_h, number_of_tests_failed_h = regression_hbs(wpath, mode_hbs, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            folderListK1990, number_of_tests_k1990, number_of_tests_failed_k1990, calculatedS, measuredS,  calculatedF, measuredF = regression_kashibe1990(wpath, mode_Kashibe1990, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            folderListS, number_of_tests_s, number_of_tests_failed_s, calculated, measured  = regression_small1988(wpath, mode_Small1988, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            folderListS1985, number_of_tests_s1985, number_of_tests_failed_s1985, calculated, measured  = regression_small1985(wpath, mode_Small1985, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            folderListK1991, number_of_tests_k1991, number_of_tests_failed_k1991, calculated, measured  = regression_kashibe1991(wpath, mode_Kashibe1991, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
            
            # Combine the test lists from the different modes into one comprehensive list.
            folderList = folderListB + folderListK + folderListW + folderListT + folderListC + folderListO + folderListH + folderListK1990 + folderListS + folderListS1985 + folderListK1991
            # Add up the counts of the executed tests from the different modes.
            number_of_tests = number_of_tests_b +  number_of_tests_k + number_of_tests_w + number_of_tests_t + number_of_tests_c + number_of_tests_o + number_of_tests_h + number_of_tests_k1990 + number_of_tests_s + number_of_tests_s1985 + number_of_tests_k1991
            # Add up the counts of the failed tests from the different modes.
            number_of_tests_failed = number_of_tests_failed_b + number_of_tests_failed_k + number_of_tests_failed_w + number_of_tests_failed_t + number_of_tests_failed_c + number_of_tests_failed_o + number_of_tests_failed_h + number_of_tests_failed_k1990 + number_of_tests_failed_s + number_of_tests_failed_s1985 + number_of_tests_failed_k1991

        # Case where the user chose values
        if execution_option == 1 :

            # Provide options for the user to choose the type of regression test.
            print("Possible regression options \n")
            print("Baker : 0\nKashibe1993 : 1\nWhite : 2\nTalip : 3\nContact : 4\nOxidation : 5\nHBS : 6\nKashibe1990 : 7\nSmall1988 : 8\nSmall1985 : 9\nKashibe1991: 10\n")

            # Take the user's input for the regression type.
            regression_mode = int(input("Enter the chosen regression (0, 1, 2, 3, 4, 5, 6, 7, 8, 9) = "))

            # Ask the user to choose an option for the gold mode.
            print("Pleast select one option for the GOLD MODE :\n")
            print("\tMODE GOLD == 0: use SCIANTIX, check new results.\n")
            print("\tMODE GOLD == 1: use SCIANTIX, new results will be saved as gold results.\n ")
            print("\tMODE GOLD == 2: do not use SCIANTIX, check existing results.\n ")
            print("\tMODE GOLD == 3: do not use SCIANTIX, existing results will be saved as gold results.\n ")

            # Take the user's input for the gold mode.
            mode_gold = int(input("Enter MODE GOLD (0, 1, 2, 3)= "))

            # Ask the user to choose an option for the plot mode.
            mode_plot = int(input("Enter MODE PLOT (0 or 1)= "))

            # For each execution mode (Baker, White, Talip, Contact, Oxidation), if the mode is set to '1', run the corresponding regression test function and update the test list and test counts accordingly.
            if regression_mode == 0 :
                mode_Baker = 1
                folderList, number_of_tests, number_of_tests_failed = regression_baker(wpath, mode_Baker, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : Baker")
            if regression_mode == 1 :
                mode_Kashibe1993 = 1
                folderList, number_of_tests, number_of_tests_failed = regression_kashibe1993(wpath, mode_Kashibe1993, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : Kashibe 1993")
            if regression_mode == 2 :
                mode_White = 1
                folderList, number_of_tests, number_of_tests_failed, calculated, measured  = regression_white(wpath, mode_White, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : White")
            if regression_mode == 3 :
                mode_Talip = 1
                folderList, number_of_tests, number_of_tests_failed = regression_talip(wpath, mode_Talip, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : Talip")
            if regression_mode == 4 :
                mode_CONTACT = 1
                folderList, number_of_tests, number_of_tests_failed = regression_contact(wpath, mode_CONTACT, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : Contact")
            if regression_mode == 5 :
                mode_oxidation = 1
                folderList, number_of_tests, number_of_tests_failed = regression_oxidation(wpath, mode_oxidation, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : Oxidation")
            if regression_mode == 6 :
                mode_hbs = 1
                folderList, number_of_tests, number_of_tests_failed = regression_hbs(wpath, mode_hbs, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : HBS")
            if regression_mode == 7 :
                mode_Kashibe1990 = 1
                folderList, number_of_tests, number_of_tests_failed, calculatedS, measuredS,  calculatedF, measuredF = regression_kashibe1990(wpath, mode_Kashibe1990, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : Kashibe 1990")
            if regression_mode == 8 :
                mode_Small1988 = 1
                folderList, number_of_tests, number_of_tests_failed, calculated, measured  = regression_small1988(wpath, mode_Small1988, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : Small 1988")
            if regression_mode == 9 :
                mode_Small1985 = 1
                folderList, number_of_tests, number_of_tests_failed, calculated, measured  = regression_small1985(wpath, mode_Small1985, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : Small 1985")
            if regression_mode == 10 :
                mode_Kashibe1991 = 1
                folderList, number_of_tests, number_of_tests_failed, calculated, measured  = regression_kashibe1991(wpath, mode_Kashibe1991, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
                print("\nRegression selected : Kashibe 1991")

    print("MODE GOLD ==", mode_gold, "selected.")
    print("MODE PLOT ==", mode_plot, "selected.\n")

    print("-----------------SUMMARY-----------------")
    print("- List of tests performed:\n", folderList, "\n")
    print("! Number of tests = ", number_of_tests)
    print("! Number of tests passed = ", number_of_tests - number_of_tests_failed)
    print("! Number of tests failed = ", number_of_tests_failed, "\n")

    # If the test condition is set to one, run the verification
    # If there are any failed tests, exit the script with a non-zero status code to indicate the failure.
    if test_condition == 1 :
        if number_of_tests_failed > 0:
            print("-----------------ONE OR MORE TESTS HAVE FAILED-----------------")
            sys.exit(1)

# If the script is being run as the main program, call the 'main' function.
if __name__ == "__main__":
    main()
