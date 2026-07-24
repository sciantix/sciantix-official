"""
sciantix_plot.py - Plot SCIANTIX (standalone) quantities

Run this script in a folder containing the output.txt file.
@author: Giovanni Zullo
"""

import numpy as np
import os
import logging
import matplotlib.pyplot as plt
import plotly.graph_objects as go

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
        "xe_gb": "Xe at grain boundary (at/m3)",
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
# Matplotlib Plots
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

# =============
# Modular Plots
# =============

def plot_pore_evolution(data):
    """Plot HBS pore density and radius as a function of burnup."""
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

def plot_xenon_distribution(data):
    """Plot Xenon distribution in different states as a function of burnup."""
    create_multi_plot(
        x=data["bu"], 
        y_list=[data["xe_ig"], data["xe_igHBS"], data["xe_gb"], data["xe_po"]],
        labels=["Xe in grain", "Xe in grain HBS", "Xe at grain boundary", "Xe in HBS pores"],
        xlabel="Burnup (MWd/kgUO2)", 
        ylabel="Concentration (at/m3)",
        colors=COLORS,
        title="Xenon Distribution"
    )

def plot_hbs_kinetics(data):
    """Plot trapping and re-solution rate in HBS as a function of burnup."""
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

# =======================
# Plotly Interactive Plot
# =======================

# def plot_temperature_and_fission_rate(data):
#     """Plot temperature and fission rate vs time using Plotly."""
#     time = data.get("t", [])
#     temperature = data.get("T", [])
#     fission_rate = data.get("frate", [])

#     if len(time) == 0 or len(temperature) == 0 or len(fission_rate) == 0:
#         logging.error("Missing data for temperature or fission rate.")
#         return

#     fig = go.Figure()

#     fig.add_trace(go.Scatter(
#         x=time,
#         y=temperature,
#         name="Temperature (K)",
#         yaxis="y1",
#         mode="lines",
#         line=dict(color="firebrick")
#     ))

#     fig.add_trace(go.Scatter(
#         x=time,
#         y=fission_rate,
#         name="Fission rate (fiss/m³/s)",
#         yaxis="y2",
#         mode="lines",
#         line=dict(color="royalblue", dash="dot")
#     ))

#     fig.update_layout(
#         title="Temperature and Fission Rate vs Time",
#         xaxis=dict(title="Time (h)", range=[30100.00000000, 30100.00011667]),
#         yaxis=dict(
#             title=dict(text="Temperature (K)", font=dict(color="firebrick")),
#             tickfont=dict(color="firebrick")
#         ),
#         yaxis2=dict(
#             title=dict(text="Fission rate (fiss/m³/s)", font=dict(color="royalblue")),
#             tickfont=dict(color="royalblue"),
#             anchor="x",
#             overlaying="y",
#             side="right"
#         ),
#         legend=dict(x=0.01, y=0.99),
#         template="plotly_white"
#     )

#     # Save to HTML (since fig.show() might not work in WSL)
#     fig.write_html("temperature_fission_rate_plot.html")
#     logging.info("Interactive plot saved as 'temperature_fission_rate_plot.html'")

#     fig.show()

# def plot_temperature_and_fission_rate(data):
#     """Plot temperature and fission rate vs time using Matplotlib."""
#     time = data.get("t", [])
#     temperature = data.get("T", [])
#     fission_rate = data.get("frate", [])

#     if len(time) == 0 or len(temperature) == 0 or len(fission_rate) == 0:
#         logging.error("Missing data for temperature or fission rate.")
#         return

#     fig, ax1 = plt.subplots()

#     # Primo asse: temperatura
#     ax1.set_xlabel("Time (h)")
#     ax1.set_ylabel("Temperature (K)", color="firebrick")
#     ax1.plot(time, temperature, color="firebrick", label="Temperature (K)")
#     ax1.tick_params(axis="y", labelcolor="firebrick")

#     # Limiti asse X
#     ax1.set_xlim([30100.00000000, 30100.0003])

#     # Secondo asse: fission rate
#     ax2 = ax1.twinx()
#     ax2.set_ylabel("Fission rate (fiss/m³/s)", color="royalblue")
#     ax2.plot(time, fission_rate, color="royalblue", linestyle="dotted", label="Fission rate")
#     ax2.tick_params(axis="y", labelcolor="royalblue")

#     # Titolo e layout
#     plt.title("Temperature and Fission Rate vs Time")
#     fig.tight_layout()
#     plt.show()

# =====
# Main
# =====

def main():
    data = sciantix_dictionary(FILENAME)
    if data:
        plot_temperature_and_fission_rate(data)
        plot_xenon_distribution(data)
        # plot_pore_evolution(data)
        # plot_hbs_kinetics(data)
    else:
        logging.error("No data available to plot.")

if __name__ == "__main__":
    main()
