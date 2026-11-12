"""
sciantix_gui.py is a python module made to handle the sciantix (standalone) postprocessing,
and plot the quantities in the output.txt file
the purpose is to plot variables, choosing both x- and y- axes

@author: G. Zullo
@author: C. Valot

Instructions:
- run sciantix.py in a folder containing the output.txt file

"""

# Importing necessary libraries
import numpy as np
import os
import logging
import tkinter as tk
from tkinter import messagebox
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure
import matplotlib.pyplot as plt
import sys

# Initialize logging
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')

# Colors for the plots
COLORS = ["blue", "red", "green", "orange", "purple", "brown", "pink", "gray", "olive", "cyan"]

""" Utility Functions """

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
    Import a .txt file into an ndarray.
    """
    if not is_output_here(filename):
        return None
    return np.genfromtxt(filename, dtype='str', delimiter='\t')

def find_sciantix_variable_position(output, variable_name):
    """
    Get the column index of a given variable in the ndarray.
    """
    try:
        _, j = np.where(output == variable_name)
        return int(j)
    except ValueError:
        logging.error(f"Variable '{variable_name}' not found in the data.")
        return None

def get_dictionary(file):
    """
    Print the names of the sciantix quantities in output.txt that can be plotted.
    """
    data = import_data(file)
    if data is None:
        return None

    output_tags = data[0, :-1]
    tag_positions = np.arange(len(output_tags))

    logging.info(f"Detected {file} in the current folder with the following variables:")
    for i, tag in enumerate(output_tags):
        print(f"Position #{tag_positions[i]}, variable = {tag}")
    return output_tags

def create_axes(fig, ax, index, label, color):
    """
    Helper function to create a new axis or adjust an existing one.
    """
    new_ax = ax if index == 0 else ax.twinx()
    if index >= 1:
        new_pos = 1.0 + 0.2 * index
        new_ax.spines['right'].set_position(('axes', new_pos))
    new_ax.set_ylabel(label, color=color)
    return new_ax

def plot(x_name, y_name, output_filename="output.txt", ax=None, color='blue'):
    """
    Plot data from specified x and y columns of the output file.
    """
    if not is_output_here(output_filename):
        return

    data = import_data(output_filename)
    if data is None:
        return

    x_index = find_sciantix_variable_position(data, x_name)
    y_index = find_sciantix_variable_position(data, y_name)
    
    if x_index is None or y_index is None:
        logging.error("Unable to plot due to missing variable.")
        return

    x = data[1:, x_index].astype(float)
    y = data[1:, y_index].astype(float)
    ax.plot(x, y, color=color, label=y_name)
    ax.set_xlabel(x_name)
    ax.set_ylabel(y_name, color=color)
    ax.tick_params(axis='y', colors=color)

class SciantixPlotter:
    def __init__(self, output_filename):
        self.output_filename = output_filename
        self.canvas = None
        self.root = tk.Tk()
        self.output_tags = get_dictionary(output_filename)
        
        # Corrected condition to check if output_tags is not None and has elements
        if self.output_tags is not None and self.output_tags.size > 0:
            self.setup_gui()

    def setup_gui(self):
        """
        Sets up the Tkinter GUI.
        """
        self.root.geometry("800x600+100+50")
        self.root.protocol('WM_DELETE_WINDOW', self.hide_window)

        # Create listboxes for selecting x and y variables
        self.listbox_x = tk.Listbox(self.root, exportselection=0, width=50)
        self.listbox_x.grid(row=0, column=0, sticky="nsew")

        self.button = tk.Button(self.root, text="Plot graph", command=self.plot_graph)
        self.button.grid(row=0, column=1)

        self.listbox_y = tk.Listbox(self.root, selectmode=tk.MULTIPLE, exportselection=0, width=50)
        self.listbox_y.grid(row=0, column=2, sticky="nsew")

        for tag in self.output_tags:
            self.listbox_x.insert(tk.END, tag)
            self.listbox_y.insert(tk.END, tag)

    def hide_window(self):
        """
        Handles the window close event.
        """
        self.root.quit()
        self.root.destroy()

    def plot_graph(self):
        """
        Handle plotting logic with dynamic figure and axes.
        """
        x_index = self.listbox_x.curselection()
        y_indices = self.listbox_y.curselection()

        if not x_index or not y_indices:
            messagebox.showwarning("Selection error", "Please select an item from both lists.")
            return

        x_name = self.output_tags[x_index[0]]
        y_names = [self.output_tags[i] for i in y_indices]

        fig = Figure(figsize=(10 + 0.75 * len(y_names), 6))
        ax = fig.add_subplot(111)

        for i, y_name in enumerate(y_names):
            color = COLORS[i % len(COLORS)]
            new_ax = create_axes(fig, ax, i, y_name, color)
            plot(x_name, y_name, self.output_filename, new_ax, color)

        if self.canvas:
            self.canvas.get_tk_widget().grid_forget()

        self.canvas = FigureCanvasTkAgg(fig, master=self.root)
        self.canvas.draw()
        self.canvas.get_tk_widget().grid(row=1, column=0, columnspan=3, sticky="nsew")

    def mainloop(self):
        """
        Starts the Tkinter main loop.
        """
        self.root.mainloop()

def main():
    plotter = SciantixPlotter('output.txt')
    plotter.mainloop()

if __name__ == "__main__":
    main()
