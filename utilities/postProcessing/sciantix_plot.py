"""
sciantix_plot.py is a Python script to plot default Sciantix (standalone) quantities

Instructions:
- Run sciantix_plot.py in a folder containing the output.txt file.

@author: Giovanni Zullo
"""

# Importing necessary libraries
import numpy as np
import os
import logging
import tkinter as tk
from tkinter import messagebox
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# Color palette for plots
colors = ["blue", "red", "green", "orange", "purple", "brown", "pink", "gray", "olive", "cyan"]

def is_output_here(filename):
    """
    Check if a given file is present in the current folder.
    """
    if os.path.isfile(filename):
        return True
    logging.error(f"{filename} not found!")
    return False

def import_data(filename):
    """
    This function imports a .txt file into an ndarray.
    """
    if not is_output_here(filename):
        logging.error(f"File {filename} not found in the directory.")
        return None  # Return None if file not found
    
    try:
        data = np.genfromtxt(filename, dtype='str', delimiter='\t')  # Convert the file data to numpy array
        return data
    except Exception as e:
        logging.error(f"Error reading {filename}: {str(e)}")
        return None

def find_sciantix_variable_position(output, variable_name):
    """
    This function gets the output.txt file and the variable name,
    giving back its column index in the ndarray.
    """
    try:
        i, j = np.where(output == variable_name)  # Find index where variable_name exists in 'output'
        return int(j)  # Return the column index
    except:
        logging.error(f"Variable {variable_name} not found in the output data.")
        return None

def working_dir(path):
    """
    This function receives the path of the folder that contains the sciantix file "output.txt".
    If output.txt is in the same folder of sciantix.py, it is not necessary to use this function.
    """
    os.chdir(path)  # Change the current working directory to 'path'
    logging.info(f"Working directory set to: {path}")
    return None

def plot_data(ax, xAxis, data, axisLabel, legendLabel, color, line_style='-', 
              axis_id='y1', secondary_axes=None, y_limits=None, x_limits=None):
    """
    Plots data on the given axis, handling multiple secondary axes and axis limits.
    Includes the ability to specify the line style.
    Uses axis_id as a unique identifier for secondary axes.
    The first secondary axis ('y2') is attached to the main plot frame, 
    and subsequent secondary axes are pushed to the right.
    """
    if secondary_axes is None:
        secondary_axes = {}

    if axis_id == 'y1':
        ax.plot(xAxis, data, label=legendLabel, color=color, linestyle=line_style)
        ax.set_ylabel(axisLabel, color=color)
        ax.tick_params(axis='y', labelcolor=color)  # Set tick color for primary y-axis
        if y_limits:
            ax.set_ylim(y_limits)
        if x_limits:
            ax.set_xlim(x_limits)
    else:
        if axis_id not in secondary_axes:
            new_ax = ax.twinx()
            secondary_axes[axis_id] = new_ax
            new_ax.set_ylabel(axisLabel, color=color)

            # Adjust position for additional secondary axes beyond the first one
            if axis_id != 'y2':
                ax_position = ax.get_position()
                new_ax.set_position([ax_position.x0, ax_position.y0, 
                                     ax_position.width, ax_position.height])
                offset = 60 * (len(secondary_axes) - 1)  # Offset for additional axes
                new_ax.spines['right'].set_position(('outward', offset))

        existing_ax = secondary_axes[axis_id]
        existing_ax.plot(xAxis, data, label=legendLabel, color=color, linestyle=line_style)
        existing_ax.tick_params(axis='y', labelcolor=color)
        if y_limits:
            existing_ax.set_ylim(y_limits)
        if x_limits:
            existing_ax.set_xlim(x_limits)

    return ax, secondary_axes

def sciantix_dictionary(file):
    data = import_data(file)
    if data is None:
        return {}

    variable_labels = {
        "t": "Time (h)",
        "T": "Temperature (K)",
        "frate": "Fission rate (fiss / m3 s)",
        "sigma": "Hydrostatic stress (MPa)",
        "bu": "Burnup (MWd/kgUO2)",
        "N": "Intergranular bubble concentration (bub/m2)",
        "n": "Intergranular atoms per bubble (at/bub)",
        "v": "Intergranular vacancies per bubble (vac/bub)",
        "rad": "Intergranular bubble radius (m)",
        "A": "Intergranular bubble area (m2)",
        "vol": "Intergranular bubble volume (m3)",
        "fc": "Intergranular fractional coverage (/)",
        "fcsat": "Intergranular saturation fractional coverage (/)",
        "f": "Intergranular fractional intactness (/)",
        "fv": "Intergranular vented fraction (/)",
        "pv": "Intergranular venting probability (/)",
        "xe_p": "Xe produced (at/m3)",
        "xe_ig": "Xe in grain (at/m3)",
        "xe_igHBS": "Xe in grain HBS (at/m3)",
        "xe_igb": "Xe in intragranular bubbles (at/m3)",
        "xe_igs": "Xe in intragranular solution (at/m3)",
        "xe_gb": "Xe at grain boundary (at/m3)",
        "xe_r": "Xe released (at/m3)",
        "fgr" : "Fission gas release (/)",
        "alpha": "Restructured volume fraction (/)",
        "sv": "Intergranular S/V (1/m)",
        "swe_gb" : "Intergranular gas swelling (/)",
        "a": "Grain radius (m)",
    }

    sd = {}
    label = []
    name = []

    for var_name, var_label in variable_labels.items():
        label.append(var_label)
        name.append(var_name)

    for i in range(len(label)):
        try:
            var_index = find_sciantix_variable_position(data, label[i])
        except:
            var_index = None
        if var_index is not None:
            sd[name[i]] = data[1:, var_index].astype(float)
        else:
            sd[name[i]] = np.zeros_like(data[1:, 0].astype(float))
            logging.warning(f"Variable '{label[i]}' not found in the data.")
    
    return sd

def create_plot(x_data, y_data, xlabel, ylabel, legends, colors, title=None, secondary_y=False):
    """
    Utility function to create plots.
    """
    fig, ax1 = plt.subplots()
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel, color=colors[0])
    
    # Primary plot
    ax1.plot(x_data, y_data[0], color=colors[0], label=legends[0])
    ax1.tick_params(axis='y', labelcolor=colors[0])
    
    if secondary_y:
        ax2 = ax1.twinx()
        ax2.set_ylabel(legends[1], color=colors[1])
        ax2.plot(x_data, y_data[1], color=colors[1], label=legends[1])
        ax2.tick_params(axis='y', labelcolor=colors[1])
    
    # Add legend and title
    ax1.legend(loc='upper left')
    if secondary_y:
        ax2.legend(loc='upper right')
    if title:
        plt.title(title)
    fig.tight_layout()
    plt.show()

def plot(sd):
    create_plot(
        x_data=sd["t"], 
        y_data=[sd["xe_p"]],
        xlabel='Time, h',
        ylabel='Xe produced, at / m3',
        legends=['Xe produced, at / m3'],
        colors=['tab:red'],
        secondary_y=False
    )

def main():
    data_dict = sciantix_dictionary('output.txt')
    plot(data_dict)

if __name__ == "__main__":
    main()
