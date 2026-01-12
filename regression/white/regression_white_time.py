"""

This is a python script to execute the regression of white cases of sciantix.

@author E. Cappellari

"""

""" ------------------- Import requiered depedencies ------------------- """

import os
import numpy as np
import matplotlib.pyplot as plt
import shutil
import pandas as pd
import glob


""" ------------------- Global Variables ------------------- """
# Plot configuration
font_size = 18

plt.rcParams['axes.labelsize'] = font_size
plt.rcParams['xtick.labelsize'] = font_size
plt.rcParams['ytick.labelsize'] = font_size
plt.rcParams['font.size'] = font_size
plt.rcParams['legend.fontsize'] = font_size 
plt.rcParams['lines.markersize'] = 8

def import_data(filename):
    """
    Imports a tab-delimited .txt file into a NumPy ndarray.
    
    Parameters
    ----------
    filename : str
        The path to the .txt file to import.
    
    Returns
    -------
    ndarray
        The data from the file as a NumPy array.
    """
    try:
        data = np.genfromtxt(filename, dtype='str', delimiter='\t')
    except Exception as e:
        raise RuntimeError(f"Error importing data from {filename}: {e}")
    return data

def findSciantixVariablePosition(output, variable_name):
    """
    Finds the column index of a specified variable in the ndarray.
    
    Parameters
    ----------
    output : ndarray
        The data array where variable names are searched.
    variable_name : str
        The name of the variable to find.
    
    Returns
    -------
    int
        The column index of the variable.
    
    Raises
    ------
    ValueError
        If the variable name is not found.
    """
    i, j = np.where(output == variable_name)
    if j.size == 0:
        raise ValueError(f"Variable '{variable_name}' not found.")
    return int(j[0])

# Main function of the white regression
def regression_white(wpath, inputfile):

    files_and_dirs = os.listdir(wpath)
    sorted_files_and_dirs = sorted(files_and_dirs)

    for file in sorted_files_and_dirs:
        if "White" in file and os.path.isdir(file):
            os.chdir(file)
            print(f"Now in folder {file}...")

            """Execute SCIANTIX and handle input/output files."""
            shutil.copy(f"../INPUTS/{inputfile}/input_settings.txt", os.getcwd())
            shutil.copy("../../../build/sciantix.x", os.getcwd())
            os.system("./sciantix.x")
            os.remove("sciantix.x")
            os.remove("input_settings.txt")

            ramptime = pd.read_csv("input_history.txt", delimiter='\t').iloc[1, 0] - 10
            endtime = pd.read_csv("input_history.txt", delimiter='\t').iloc[-1, 0]

            data = pd.read_csv("output.txt", delimiter='\t')

            fig, ax1 = plt.subplots(figsize=(10, 6))

            ax1.set_xlabel("Time (h)")
            ax1.set_ylabel("Fission gas release (%)")
            ax1.plot(
                data["Time (h)"],
                100*data["Fission gas release (/)"],
                label="FGR"
            )
            ax1.tick_params(axis='y')

            ax2 = ax1.twinx()
            ax2.set_ylabel("Temperature (K)")
            ax2.plot(
                data["Time (h)"],
                data["Temperature (K)"],
                linestyle='--',
                label="Temperature"
            )
            ax2.tick_params(axis='y')
            lines_1, labels_1 = ax1.get_legend_handles_labels()
            lines_2, labels_2 = ax2.get_legend_handles_labels()
            ax1.legend(lines_1 + lines_2, labels_1 + labels_2, loc='upper left', frameon=False)
  
            fig.tight_layout()
            plt.savefig("FGR_and_Temperature_vs_Time.png")
            plt.close()

            
            fig, ax1 = plt.subplots(figsize=(10, 6))

            ax1.set_xlabel("Time (h)")
            ax1.set_ylabel("Fission gas release (%)")
            ax1.plot(
                data["Time (h)"] - ramptime,
                100*data["Fission gas release (/)"],
                label="FGR"
            )
            ax1.tick_params(axis='y')
            ax1.set_xlim(0, endtime - ramptime)  # Set x-axis limits to focus on ramp-up phase

            ax2 = ax1.twinx()
            ax2.set_ylabel("Temperature (K)")
            ax2.plot(
                data["Time (h)"] - ramptime,
                data["Temperature (K)"],
                linestyle='--',
                label="Temperature"
            )
            ax2.tick_params(axis='y')
            ax2.set_xlim(0, endtime - ramptime)  # Set x-axis limits to focus on ramp-up phase
            lines_1, labels_1 = ax1.get_legend_handles_labels()
            lines_2, labels_2 = ax2.get_legend_handles_labels()
            ax1.legend(lines_1 + lines_2, labels_1 + labels_2, loc='upper left', frameon=False)
            plt.title(f"Ramp phase, beginning at time: {round(ramptime)} h")
            fig.tight_layout()
            plt.savefig("FGR_and_Temperature_vs_Time_ramp.png")
            plt.close()

            os.makedirs(inputfile, exist_ok=True)

            for png in glob.glob(os.path.join(inputfile, "*.png")):
                os.remove(png)

            os.rename("execution.txt", f"{inputfile}/execution.txt")
            os.rename("input_check.txt", f"{inputfile}/input_check.txt")
            os.rename("output.txt", f"{inputfile}/output.txt")
            os.rename("overview.txt", f"{inputfile}/overview.txt")
            os.rename("FGR_and_Temperature_vs_Time.png", f"{inputfile}/FGR_and_Temperature_vs_Time.png")
            os.rename("FGR_and_Temperature_vs_Time_ramp.png", f"{inputfile}/FGR_and_Temperature_vs_Time_ramp.png")
            os.chdir("..")

    return
        
regression_white('.', 'SCIANTIX_2026')