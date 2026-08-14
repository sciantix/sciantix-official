"""
sciantix_plot.py – Plot default Sciantix (standalone) quantities

Run this script in a folder containing the output.txt file.
@author: Giovanni Zullo
"""

import numpy as np
import os
import logging
import matplotlib.pyplot as plt

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# Constants
FILENAME = 'output.txt'
COLORS = ["blue", "red", "green", "orange", "purple", "brown", "pink", "gray", "olive", "cyan"]

# ======================
# File and Data Handling
# ======================

def check_file_exists(filename):
    """Check if a file exists in the current directory."""
    if os.path.isfile(filename):
        return True
    logging.error(f"{filename} not found in the current directory.")
    return False

def import_data(filename):
    """Import tab-separated text data into a NumPy array."""
    if not check_file_exists(filename):
        return None
    try:
        return np.genfromtxt(filename, dtype='str', delimiter='\t')
    except Exception as e:
        logging.error(f"Error reading {filename}: {e}")
        return None

def get_variable_column_index(output, variable_name):
    """Return the column index for a given variable label."""
    try:
        _, col_indices = np.where(output == variable_name)
        return int(col_indices[0])
    except IndexError:
        logging.warning(f"Variable '{variable_name}' not found in output.")
        return None

# ===================
# Sciantix Dictionary
# ===================

def sciantix_dictionary(filename):
    """Build dictionary with Sciantix variable names mapped to their data arrays."""
    raw_data = import_data(filename)
    if raw_data is None:
        return {}

    labels = {
        "t": "Time (h)",
        "T": "Temperature (K)",
        "frate": "Fission rate (fiss / m3 s)",
        "sigma": "Hydrostatic stress (MPa)",
        "bu": "Burnup (MWd/kgUO2)",
        "Np": "HBS pore density (pores/m3)",
        "r": "HBS pore radius (m)",
        "P": "HBS porosity (/)",
        "xe_p": "Xe produced (at/m3)",
        "xe_ig": "Xe in grain (at/m3)",
        "xe_igHBS": "Xe in grain HBS (at/m3)",
        "xe_igb": "Xe in intragranular bubbles (at/m3)",
        "xe_igs": "Xe in intragranular solution (at/m3)",
        "xe_gb": "Xe at grain boundary (at/m3)",  # Note: 'xe_gb' appears twice, use one label
        "xe_po": "Xe in HBS pores (at/m3)",
        "tr": "trapping rate hbs (1/s)",
        "nu": "nucleation rate hbs (1/s)",
        "re": "re-solution rate hbs (1/s)"
    }

    data_dict = {}
    for var, label in labels.items():
        idx = get_variable_column_index(raw_data, label)
        try:
            data_dict[var] = raw_data[1:, idx].astype(float) if idx is not None else np.zeros_like(raw_data[1:, 0], dtype=float)
        except Exception as e:
            logging.warning(f"Error processing variable '{var}': {e}")
            data_dict[var] = np.zeros_like(raw_data[1:, 0], dtype=float)

    return data_dict

# =============
# Plot Functions
# =============

def create_dual_axis_plot(x, y1, y2, xlabel, y1_label, y2_label, y1_color, y2_color, title=None):
    """Create a dual-axis plot."""
    fig, ax1 = plt.subplots()
    ax1.plot(x, y1, color=y1_color, label=y1_label)
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(y1_label, color=y1_color)
    ax1.tick_params(axis='y', labelcolor=y1_color)

    ax2 = ax1.twinx()
    ax2.plot(x, y2, color=y2_color, label=y2_label)
    ax2.set_ylabel(y2_label, color=y2_color)
    ax2.tick_params(axis='y', labelcolor=y2_color)

    if title:
        plt.title(title)

    fig.tight_layout()
    plt.show()

def create_multi_plot(x, y_list, labels, xlabel, ylabel, colors, title=None):
    """Create a multi-line plot."""
    fig, ax = plt.subplots()
    for i, y in enumerate(y_list):
        ax.plot(x, y, label=labels[i], color=colors[i % len(colors)])
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.legend()
    if title:
        plt.title(title)
    fig.tight_layout()
    plt.show()

def plot_sciantix(data):
    """Plot predefined Sciantix quantities."""
    # HBS pore density and radius
    create_dual_axis_plot(
        x=data["bu"], 
        y1=data["Np"], 
        y2=data["r"],
        xlabel="Burnup (MWd/kgUO2)", 
        y1_label="HBS pore density (pores/m3)", 
        y2_label="HBS pore radius (m)", 
        y1_color='tab:red', 
        y2_color='tab:blue',
        title="HBS Pore Evolution"
    )

    # Xenon distribution
    create_multi_plot(
        x=data["bu"], 
        y_list=[data["xe_ig"], data["xe_igHBS"], data["xe_gb"], data["xe_po"]],
        labels=["Xe in grain", "Xe in grain HBS", "Xe at grain boundary", "Xe in HBS pores"],
        xlabel="Burnup (MWd/kgUO2)", 
        ylabel="Concentration (at/m3)",
        colors=COLORS,
        title="Xenon Distribution"
    )

    # Trapping vs Re-solution rate
    create_dual_axis_plot(
        x=data["bu"], 
        y1=data["tr"], 
        y2=data["re"],
        xlabel="Burnup (MWd/kgUO2)", 
        y1_label="Trapping rate (1/s)", 
        y2_label="Re-solution rate (1/s)", 
        y1_color='tab:green', 
        y2_color='tab:orange',
        title="HBS Trapping vs Re-solution Rate"
    )

# =====
# Main
# =====

def main():
    data = sciantix_dictionary(FILENAME)
    if data:
        plot_sciantix(data)
    else:
        logging.error("No data available to plot.")

if __name__ == "__main__":
    main()
