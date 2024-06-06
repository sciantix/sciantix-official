"""
sciantix.py is a python module made to handle the sciantix (standalone) postprocessing,
and plot the quantities in the output.txt file
the purposes are:
	- to plot variables, choosing both x- and y- axes (is_main = 1)
	- to prepare quantities to be plotted (is_main = 2)

@author: Giovanni Zullo
@author: C Valot

Instructions:
- run sciantix.py in a folder containing the output.txt file

"""

# Importing necessary libraries
import numpy as np
import os
import importlib
import tkinter as tk
from tkinter import messagebox
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt
from matplotlib.figure import Figure
import matplotlib.gridspec as gridspec
import tkinter as tk
from tkinter import messagebox
from matplotlib.figure import Figure
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt
import sys


# Colors for the plots
colors = ["blue", "red", "green", "orange", "purple", "brown", "pink", "gray", "olive", "cyan"]
canvas = None  				# Declare canvas as a global variable
canvas_closed = False 		# Flag to track if sciantix window was closed

is_main = 0

""" Defining useful functions """
# Function to check if given file is present in the current folder
def is_output_here(filename):
	try:
		f = open(filename)  # Try opening the file
	except:
		print("ERROR: ", filename, " not found!")  # If file not found, print error
		return False
	else:
		f.close()  # Close the file if found
		return True

# Function to import a .txt file into a numpy array
def import_data(filename):
	"""
	This function import a .txt file into an ndarray
	"""

	if(is_output_here(filename) is False):
		return  # If file not found, return None
	data = np.genfromtxt(filename, dtype= 'str', delimiter='\t')  # Convert the file data to numpy array
	return data

# Function to get the column index of a given variable in the ndarray
def findSciantixVariablePosition(output, variable_name):
	"""
	This function gets the output.txt file and the variable name,
	giving back its column index in the ndarray
	"""

	i,j = np.where(output == variable_name)  # Find index where variable_name exists in 'output'
	return int(j)  # Return the column index

# Function to set the working directory to the folder that contains the sciantix file "output.txt"
def working_dir(path):
	"""
	This function receives the path of the folder that contains the sciantix file "output.txt".
	If output.txt is in the same folder of sciantix.py, it is not necessary to use this function.
	"""
	os.chdir(path)  # Change the current working directory to 'path'
	return None

# Function to print the names of the sciantix quantities in output.txt that can be plotted
def getDictionary(file):
	"""
	----------------
	Input parameters
	----------------

	output_filename : TYPE = str

		name of the output file (e.g. 'output.txt')
		the output file must be in the sciantix.py directory or
		the path must de defined by sciantix.working_dir(path).

		default name = 'output.txt'

	-------
	Returns
	-------
	The names of the sciantix quantities in output.txt that can be plotted
	and the corresponding values for the plot positions.
	"""

	if(is_output_here(file) is False):
		return  # If file not found, return None

	data = import_data(file)  # Import the file data
	output_tags = data[0,:-1]  # Extract the output tags (column headers) from the first row
	data_shape = data.shape  # Get the shape of the data array (rows, columns)
	tag_positions = np.linspace(0, data_shape[1] - 2, data_shape[1] - 1)  # Generate positions for the tags

	print(f"An " + file + " file has been detected in the current folder.")
	print("The file ", file, "contains the following variables:")
	print("")

	for i in range(0, data_shape[1] - 1):
		print("Position #", tag_positions[i].astype(int), ", variable = ", output_tags[i])  # Print the positions and variable names
	return output_tags

def sciantix(file, is_main):
	# Define the sciantix function which accepts two parameters: the file to be processed and a flag indicating
	# whether this function is called from the main script (is_main == 1) or not (is_main == 0).

	global canvas  # Indicate that we're using the global 'canvas' variable, not creating a local one.

	output_filename = file  # Name of the file to process.
	output_tags = getDictionary(output_filename)  # Get the tags (column headers) from the file.
	root = tk.Tk()  # Initialize a Tkinter root window.

	# Set initial size and position of the Tkinter window.
	root.geometry("800x600+100+50")

	def plot_graph():
		# Nested function to plot the graph based on selected x and y tags from listboxes.
		if not listbox_x.curselection() or not listbox_y.curselection():
			# If either of the listboxes does not have a selection, show a warning message and return.
			messagebox.showwarning("Selection error", "Please select an item from both lists.")
			return

		# Get the indices of the selected x and y tags.
		x_index = listbox_x.curselection()[0]
		y_indices = list(listbox_y.curselection())

		# Get the actual tag names based on these indices.
		x_name = output_tags[x_index]
		y_names = [output_tags[i] for i in y_indices]

		# Adjust figure width based on number of y-axes.
		fig_width = 10 + 0.75 * len(y_names)
		fig = Figure(figsize=(fig_width, 6))  # Create a matplotlib Figure with the adjusted width.
		fig.subplots_adjust(right=0.75 / fig_width * 10)  # Adjust right side of figure layout.
		ax = fig.add_subplot(111)  # Add a subplot to the figure.

		# Loop over each y tag.
		for i, y_name in enumerate(y_names):
			color = colors[i % len(colors)]  # Pick a color for this y-axis plot.
			new_ax = ax if i == 0 else ax.twinx()  # If this is the first y tag, use the existing ax, otherwise create a new y-axis.
			if i >= 1:  # Make the spacing for the extra axes
				new_pos = 1.0 + 0.2 * i
				new_ax.spines['right'].set_position(('axes', new_pos))  # Position the y-axis.
			plot(x_name, y_name, output_filename, new_ax, color)  # Call the plot function to draw this y-axis plot.
			new_ax.set_ylabel(y_name, color=color)  # Set the y label and its color.

		# If a matplotlib canvas already exists, forget its grid.
		global canvas
		if canvas:
			canvas.get_tk_widget().grid_forget()

		# Create a new Tkinter canvas widget and place it in the grid.
		tk_canvas = tk.Canvas(root)
		tk_canvas.grid(row=1, column=0, columnspan=3, sticky="nsew")

		# Add a horizontal scrollbar attached to the new Tkinter canvas and place it in the grid.
		scrollbar_x = tk.Scrollbar(root, command=tk_canvas.xview, orient="horizontal")
		scrollbar_x.grid(row=2, column=0, columnspan=3, sticky="ew")

		tk_canvas.configure(xscrollcommand=scrollbar_x.set)  # Configure the canvas to update the scrollbar.

		# Create a new matplotlib canvas and draw.
		canvas = FigureCanvasTkAgg(fig, master=tk_canvas)
		canvas.draw()

		# Pack the matplotlib canvas into the Tkinter canvas.
		canvas.get_tk_widget().pack(side="top", fill="both", expand=True)

		# Attach the matplotlib canvas to the Tkinter canvas.
		tk_canvas.create_window(0, 0, window=canvas.get_tk_widget(), anchor="nw")

		# Update the scroll region after the width and height of the thing inside the canvas changes.
		tk_canvas.configure(scrollregion=tk_canvas.bbox("all"))

		# Give more weight to the rows that contain the canvas and the scroll bar.
		root.grid_rowconfigure(1, weight=1)
		root.grid_rowconfigure(2, weight=0)
		root.grid_columnconfigure(0, weight=1)

		# Clear the selection in the y-axis listbox after plotting.
		listbox_y.selection_clear(0, 'end')

	# Define two different versions of the function 'hide_window' depending on the value of 'is_main'.
	if is_main == 0 :
		def hide_window():
			# If this function is not called from the main script, just withdraw (hide) the root window.
			root.withdraw()

	if is_main == 1 :
		def hide_window():
			# If this function is called from the main script, set the 'canvas_closed' flag to True,
			# quit the mainloop, and destroy the root window.
			global canvas_closed
			canvas_closed = True
			root.quit()
			root.destroy()

	# Override default window-closing behavior with the 'hide_window' function.
	root.protocol('WM_DELETE_WINDOW', hide_window)

	# Create a listbox for the x-axis tags and place it in the grid.
	listbox_x = tk.Listbox(root, exportselection=0, width=50)
	listbox_x.grid(row=0, column=0, sticky="nsew")

	# Create a button that calls the 'plot_graph' function when clicked and place it in the grid.
	button = tk.Button(root, text="Plot graph", command=plot_graph)
	button.grid(row=0, column=1)

	# Create a listbox for the y-axis tags and place it in the grid.
	listbox_y = tk.Listbox(root, selectmode=tk.MULTIPLE, exportselection=0, width=50)
	listbox_y.grid(row=0, column=2, sticky="nsew")

	# Insert all tags into both listboxes.
	for tag in output_tags:
		listbox_x.insert(tk.END, tag)
		listbox_y.insert(tk.END, tag)

	# Start the Tkinter main event loop.
	root.mainloop()

def plot(x_name, y_name, output_filename="output.txt", ax=None, color='blue'):
	# Define the plot function, which takes in the x and y tag names, the file name (default is 'output.txt'),
	# an optional Axes object on which to plot, and a color for the line.

	# verify if output file exists
	if(is_output_here(output_filename) is False):
		# If the file does not exist, return without doing anything.
		return

	# Read the data from the file.
	data = import_data(output_filename)

	# Extract the x and y data from the appropriate columns and convert to float.
	x = data[1:,findSciantixVariablePosition(data, x_name)].astype(float)
	y = data[1:,findSciantixVariablePosition(data, y_name)].astype(float)

	# Plot the y data against the x data on the provided axes (or on new axes if none were provided).
	ax.plot(x, y, color=color, label=y_name)

	# Set the labels for the x and y axes and color the y-axis label.
	ax.set_xlabel(x_name)
	ax.set_ylabel(y_name, color=color)
	ax.tick_params(axis='y', colors=color)  # Also color the y-axis tick labels.


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

def input_history(file):
	data = import_data(file)

	time = data[1:,findSciantixVariablePosition(data, "Time (h)")].astype(float)
	temperature = data[1:,findSciantixVariablePosition(data, "Temperature (K)")].astype(float)
	frate = data[1:,findSciantixVariablePosition(data, "Fission rate (fiss / m3 s)")].astype(float)
	sigma = data[1:,findSciantixVariablePosition(data, "Hydrostatic stress (MPa)")].astype(float)

	fig, ax = plt.subplots() 
	ax.set_xlabel('Time, h')
	secondary_axes_1 = {}

	ax, _ = plot_data(ax, time, temperature, 'Temperature (K)', 'T', 'tab:red', '-', 'y1')
	ax, secondary_axes_1 = plot_data(ax, time, frate, 'Fission rate (fiss / m3 s)', 'F', 'tab:blue','-', 'y2', secondary_axes_1)
	ax, secondary_axes_1 = plot_data(ax, time, sigma, 'Hydrostatic stress (MPa)', 's_h', 'tab:green','-', 'y3', secondary_axes_1)
	fig.tight_layout()

	plt.show()

def fission_gas(file):
	# Xe produced (at/m3)	Xe in grain (at/m3)	Xe in intragranular solution (at/m3)	Xe in intragranular bubbles (at/m3)	Xe at grain boundary (at/m3)	Xe released (at/m3)
	data = import_data(file)

	time = data[1:,findSciantixVariablePosition(data, "Time (h)")].astype(float)
	temperature = data[1:,findSciantixVariablePosition(data, "Temperature (K)")].astype(float)
	frate = data[1:,findSciantixVariablePosition(data, "Fission rate (fiss / m3 s)")].astype(float)
	sigma = data[1:,findSciantixVariablePosition(data, "Hydrostatic stress (MPa)")].astype(float)

	xe_p = data[1:,findSciantixVariablePosition(data, "Xe produced (at/m3)")].astype(float)
	xe_g = data[1:,findSciantixVariablePosition(data, "Xe in grain (at/m3)")].astype(float)
	xe_g1 = data[1:,findSciantixVariablePosition(data, "Xe in intragranular solution (at/m3)")].astype(float)
	xe_g2 = data[1:,findSciantixVariablePosition(data, "Xe in intragranular bubbles (at/m3)")].astype(float)
	xe_gb = data[1:,findSciantixVariablePosition(data, "Xe at grain boundary (at/m3)")].astype(float)
	xe_r = data[1:,findSciantixVariablePosition(data, "Xe released (at/m3)")].astype(float)

	# Plot : Xenon
	fig, ax = plt.subplots() 
	ax.set_xlabel('Time, h')

	x = time
	# x = temperature
	
	ax.plot(x, xe_p, 	color=colors[0], label="Xe produced (at/m3)")
	ax.plot(x, xe_g, 	color=colors[1], label="Xe in grain (at/m3)")
	ax.plot(x, xe_g1, 	color=colors[2], label="Xe in intragranular solution (at/m3)")
	ax.plot(x, xe_g2, 	color=colors[3], label="Xe in intragranular bubbles (at/m3)")
	ax.plot(x, xe_gb, 	color=colors[6], label="Xe at grain boundary (at/m3)")
	ax.plot(x, xe_r, 	color=colors[5], label="Xe released (at/m3)")
	
	# ax.set_yscale('log')
	ax.set_ylabel("Xe concentrations")
	ax.tick_params(axis='y')
	ax.legend()

	fig.tight_layout()

	plt.show()

def intergranular(file):
	# Intergranular bubble concentration (bub/m2)	Intergranular atoms per bubble (at/bub)	Intergranular vacancies per bubble (vac/bub)	Intergranular bubble radius (m)	Intergranular bubble area (m2)	Intergranular bubble volume (m3)
	# Intergranular fractional coverage (/)	Intergranular saturation fractional coverage (/)	Intergranular gas swelling (/)	Intergranular fractional intactness (/)	Burnup (MWd/kgUO2)	U235 (at/m3)	U238 (at/m3)	Intergranular S/V (1/m)
	# Intergranular vented fraction (/)	Intergranular venting probability (/)

	data = import_data(file)

	time = data[1:,findSciantixVariablePosition(data, "Time (h)")].astype(float)
	temperature = data[1:,findSciantixVariablePosition(data, "Temperature (K)")].astype(float)
	frate = data[1:,findSciantixVariablePosition(data, "Fission rate (fiss / m3 s)")].astype(float)
	sigma = data[1:,findSciantixVariablePosition(data, "Hydrostatic stress (MPa)")].astype(float)
	bu = data[1:,findSciantixVariablePosition(data, "Burnup (MWd/kgUO2)")].astype(float)/0.8814
	x1 = data[1:,findSciantixVariablePosition(data, "Intergranular bubble concentration (bub/m2)")].astype(float)
	x2 = data[1:,findSciantixVariablePosition(data, "Intergranular atoms per bubble (at/bub)")].astype(float)
	x3 = data[1:,findSciantixVariablePosition(data, "Intergranular vacancies per bubble (vac/bub)")].astype(float)

	xe_p = data[1:,findSciantixVariablePosition(data, "Xe produced (at/m3)")].astype(float)

	alpha = data[1:,findSciantixVariablePosition(data, "Restructured volume fraction (/)")].astype(float)
	xe_gb = data[1:,findSciantixVariablePosition(data, "Xe at grain boundary (at/m3)")].astype(float)
	SV = data[1:,findSciantixVariablePosition(data, "Intergranular S/V (1/m)")].astype(float)
	vol = data[1:,findSciantixVariablePosition(data, "Intergranular bubble volume (m3)")].astype(float)
	rad = data[1:,findSciantixVariablePosition(data, "Intergranular bubble radius (m)")].astype(float)
	fcsat = data[1:,findSciantixVariablePosition(data, "Intergranular saturation fractional coverage (/)")].astype(float)
	xe_g = data[1:,findSciantixVariablePosition(data, "Xe in grain (at/m3)")].astype(float)
	# xe_gHBS = data[1:,findSciantixVariablePosition(data, "Xe in grain HBS (at/m3)")].astype(float)

	area = data[1:,findSciantixVariablePosition(data, "Intergranular bubble area (m2)")].astype(float)
	ngb = data[1:,findSciantixVariablePosition(data, "Intergranular bubble concentration (bub/m2)")].astype(float)
	x = time

	# PLOT: HBS in grain
	# fig, ax1 = plt.subplots()

	# ax1.plot(time, xe_g, color='blue', label='xe_g')
	# ax1.plot(time, xe_gHBS, color='green', label='xe_g1')
	# ax1.set_xlabel('Time')
	# ax1.set_ylabel('xe_p, xe_p1, xe_p2', color='black')
	# ax2 = ax1.twinx()
	# ax2.plot(time, alpha, color='orange', label='alpha')
	# ax2.set_ylabel('alpha', color='black')
	# ax1.legend(loc='upper left')
	# ax2.legend(loc='upper right')
	# plt.show()

	# fig.tight_layout()
	# plt.show()

	# PLOT: burnup - gas in grain
	# fig, ax1 = plt.subplots()

	# ax1.plot(bu, xe_g, color='green', label='xe_g1')
	# ax1.plot(bu, xe_gHBS, color='red', label='xe_g2')
	# ax1.set_xlabel('Burnup')
	# ax1.set_ylabel('xe_p, xe_p1, xe_p2', color='black')
	# ax2 = ax1.twinx()
	# ax2.plot(bu, alpha, color='orange', label='alpha')
	# ax2.set_ylabel('alpha', color='black')
	# ax1.legend(loc='upper left')
	# ax2.legend(loc='upper right')
	# plt.show()

	# fig.tight_layout()
	# plt.show()

	# PLOT MMS
	# fig, ax = plt.subplots() 
	# ax.set_xlabel('Time, h')

	# ax.plot(x, xe_gb * 1e2, 'd', color=colors[2], label="MMS: B(t) = P/100")
	# ax.plot(x, x3 * 1e17, 'o', color=colors[1], label="MMS: v(t) = P*1e-17")
	# ax.plot(x, x2 * ngb * SV * 1e2, '+', color=colors[4], label="MMS: n(t) = B(t) / (N S/V)")

	# ax.plot(x, xe_p, color=colors[0], label="P(t)")

	# ax2 = ax.twinx()
	# ax2.set_ylabel('-')
	# ax2.plot(x, vol / (4.09e-29 * 1e-17 + 8.48e-29 * 1/100 / (ngb * SV)), color='red', label='V(t) = wn(t) + Wv(t)')
	# ax2.plot(x, (4/3 * 3.14 * rad**3 * 0.168610764) / (4.09e-29 * 1e-17 + 8.48e-29 * 1/100 / (ngb * SV)) , color='green', label='r(t)=(3/(4 pi f)V)^(1/3)')

	# ax.tick_params(axis='y')
	# ax2.tick_params(axis='y')
	# ax.legend(loc='upper left')
	# ax2.legend(loc='upper right')

	# fig.tight_layout()

	# plt.show()

	# PLOT N-A
	fig, ax = plt.subplots() 
	ax.set_xlabel('A')
	ax.set_ylabel('N')
	ax.plot(area*1e12, ngb*1e-12, color=colors[4])
	ax.tick_params(axis='y')
	ax.legend()
	fig.tight_layout()
	ax.set_xscale('log')
	ax.set_yscale('log')
	ax.set_xlim(1e-3, 10)
	ax.set_ylim(1e-2, 100)
	plt.show()

	# PLOT: Temperature:vacancies and atoms
	fig, ax = plt.subplots() 
	ax.set_xlabel('Temperature (K)')
	secondary_axes_1 = {}

	ax, _ = plot_data(ax, temperature, area, 'Intergranular bubble area', 'A', 'tab:red', '-', 'y1')
	ax, secondary_axes_1 = plot_data(ax, temperature, ngb, 'Intergranular bubble concentration', 'N', 'tab:blue','-', 'y2', secondary_axes_1)

	# fig.tight_layout()
	plt.show()

	# PLOT: Time:vacancies and atoms
	fig, ax = plt.subplots() 
	ax.set_xlabel('Time (h)')
	secondary_axes_1 = {}

	ax, _ = plot_data(ax, time, x3, 'Intergranular vacancies per bubble', 'N', 'tab:red', '-', 'y1')
	ax, secondary_axes_1 = plot_data(ax, time, x2, 'Intergranular atoms per bubble', 'n_g', 'tab:blue','-', 'y2', secondary_axes_1)

	# fig.tight_layout()
	plt.show()

	# PLOT: Temperature:vacancies and atoms
	fig, ax = plt.subplots() 
	ax.set_xlabel('Temperature (K)')
	secondary_axes_1 = {}

	ax, _ = plot_data(ax, temperature, x3, 'Intergranular vacancies per bubble', 'N', 'tab:red', '-', 'y1')
	ax, secondary_axes_1 = plot_data(ax, temperature, x2, 'Intergranular atoms per bubble', 'n_g', 'tab:blue','-', 'y2', secondary_axes_1)

	# fig.tight_layout()
	plt.show()

	# PLOT - Time:Intergranular parameters
	fig, ax = plt.subplots() 
	ax.set_xlabel('Time (h)')

	x = time

	ax.plot(x, np.ones_like(x) * 0.5, 																					color=colors[5], label="Saturation fractional coverage")
	ax.plot(x, data[1:,findSciantixVariablePosition(data, "Intergranular fractional coverage (/)")].astype(float), 		color=colors[0], label="Intergranular fractional coverage")
	ax.plot(x, data[1:,findSciantixVariablePosition(data, "Intergranular fractional intactness (/)")].astype(float), 	color=colors[1], label="Intergranular fractional intactness")
	ax.plot(x, data[1:,findSciantixVariablePosition(data, "Intergranular vented fraction (/)")].astype(float), 			color=colors[2], label="Intergranular vented fraction")
	ax.plot(x, data[1:,findSciantixVariablePosition(data, "Intergranular venting probability (/)")].astype(float), 		color=colors[3], label="Intergranular venting probability")
	ax.plot(x, data[1:,findSciantixVariablePosition(data, "Intergranular gas swelling (/)")].astype(float), 			color=colors[4], label="Intergranular gas swelling")

	ax.tick_params(axis='y')
	ax.legend()

	fig.tight_layout()

	plt.show()

	# PLOT - Temperature:Intergranular parameters
	fig, ax = plt.subplots() 
	ax.set_xlabel('Temperature (K)')

	x = temperature

	ax.plot(x, np.ones_like(x) * 0.5, 																					color=colors[5], label="Saturation fractional coverage")
	ax.plot(x, data[1:,findSciantixVariablePosition(data, "Intergranular fractional coverage (/)")].astype(float), 		color=colors[0], label="Intergranular fractional coverage")
	ax.plot(x, data[1:,findSciantixVariablePosition(data, "Intergranular fractional intactness (/)")].astype(float), 	color=colors[1], label="Intergranular fractional intactness")
	ax.plot(x, data[1:,findSciantixVariablePosition(data, "Intergranular vented fraction (/)")].astype(float), 			color=colors[2], label="Intergranular vented fraction")
	ax.plot(x, data[1:,findSciantixVariablePosition(data, "Intergranular venting probability (/)")].astype(float), 		color=colors[3], label="Intergranular venting probability")
	ax.plot(x, data[1:,findSciantixVariablePosition(data, "Intergranular gas swelling (/)")].astype(float), 			color=colors[4], label="Intergranular gas swelling")

	ax.tick_params(axis='y')
	ax.legend()

	fig.tight_layout()

	plt.show()

	# PLOT: Vented fraction:Fractional coverage
	fig, ax = plt.subplots() 
	ax.set_xlabel('Intergranular fractional coverage')
	ax.set_ylabel("Intergranular vented fraction")

	ax.plot(
		data[1:,findSciantixVariablePosition(data, "Intergranular fractional coverage (/)")].astype(float),
		data[1:,findSciantixVariablePosition(data, "Intergranular vented fraction (/)")].astype(float),
		color=colors[0], label="Intergranular fractional coverage (/)")

	ax.tick_params(axis='y')
	ax.legend()

	fig.tight_layout()

	plt.show()

	# PLOT: Time:FGR
	fig, ax = plt.subplots() 

	ax.plot(
		data[1:,findSciantixVariablePosition(data, "Time (h)")].astype(float),
		data[1:,findSciantixVariablePosition(data, "Fission gas release (/)")].astype(float),
		color=colors[0], label="FGR")

	ax.tick_params(axis='y')
	ax.legend()

	fig.tight_layout()

	plt.show()

	# PLOT: Time:burnup
	fig, ax = plt.subplots() 

	ax.plot(
		data[1:,findSciantixVariablePosition(data, "Time (h)")].astype(float),
		data[1:,findSciantixVariablePosition(data, "Burnup (MWd/kgUO2)")].astype(float)/0.8814,
		color=colors[0], label="Burnup")

	ax.tick_params(axis='y')
	ax.legend()

	fig.tight_layout()

	plt.show()

	# PLOT: S/V and grain radius - temperature
	a = data[1:,findSciantixVariablePosition(data, "Grain radius (m)")].astype(float)
	x = temperature

	fig, ax = plt.subplots() 
	ax.set_xlabel('Temperature (K)')
	secondary_axes_1 = {}

	ax, _ = plot_data(ax, x, SV, 'S/V (1/m)', 'SV', 'tab:red', '-', 'y1')
	ax, secondary_axes_1 = plot_data(ax, x, a, 'Grain radius (m)', 'a', 'tab:blue','-', 'y2', secondary_axes_1)
	fig.tight_layout()

	plt.show()

	# PLOT: S/V and grain radius - time
	SV = data[1:,findSciantixVariablePosition(data, "Intergranular S/V (1/m)")].astype(float)
	a = data[1:,findSciantixVariablePosition(data, "Grain radius (m)")].astype(float)
	x = time

	fig, ax = plt.subplots() 
	ax.set_xlabel('Time, h')
	secondary_axes_1 = {}

	ax, _ = plot_data(ax, x, SV, 'S/V (1/m)', 'SV', 'tab:red', '-', 'y1')
	ax, secondary_axes_1 = plot_data(ax, x, a, 'Grain radius (m)', 'a', 'tab:blue','-', 'y2', secondary_axes_1)
	fig.tight_layout()

	plt.show()

	# PLOT: He fractional releasse
	# re = data[1:,findSciantixVariablePosition(data, "He fractional release (/)")].astype(float)
	# x = time

	# fig, ax = plt.subplots() 
	# ax.set_xlabel('Time, h')
	# secondary_axes_1 = {}

	# ax, _ = plot_data(ax, x, re, 'He fractional release (/)', 'He', 'tab:red', '-', 'y1')
	# # ax, secondary_axes_1 = plot_data(ax, x, a, 'Grain radius (m)', 'a', 'tab:blue','-', 'y2', secondary_axes_1)
	# fig.tight_layout()

	# plt.show()

def main():
	is_main = int(input("plot mode (1 or 2): "))

	os.system('./sciantix.x > log.sciantix')

	# Check if the file 'output.txt' is in the current working directory.
	if 'output.txt' in os.listdir(os.getcwd()):

		if is_main == 1:
			# If the file exists, run the sciantix function with 'output.txt' as the filename and main flag.
			sciantix('output.txt', is_main)
			# After the function call, check if the canvas (graphical window) was closed by the user.
			if canvas_closed:
				# If the user has closed the window, terminate the Python process.
				sys.exit()

		elif is_main == 2:
			input_history('output.txt')
			fission_gas('output.txt')
			intergranular('output.txt')
	else:
		# If the 'output.txt' file does not exist in the current directory, print an error message.
		print('No file named "output.txt" found in the current directory')

if __name__ == "__main__":
	# This is the entry point of the script. When the script is run directly (not imported as a module),
	main()

