"""
sciantix_ic.py is a python module made to handle the sciantix (standalone) postprocessing,
and plot the quantities in the ic_shape.txt file
the purpose is to plot the initial concentrations input from the initial_distribution.txt file
to check if the input is correct.

@author: A. Zayat
@author: G. Zullo

Instructions:
- run sciantix.py in a folder containing the output.txt file

"""
import numpy as np
import os
import tkinter as tk
from tkinter import filedialog, messagebox
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure

COLORS = ["blue", "red", "green", "orange", "purple", "brown", "pink", "gray", "olive", "cyan"]

class ProfilePlotterGUI:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("Sciantix Initial Concentration Radial Profile Plotter")
        self.canvas = None
        self.filename = None
        self.header = None
        self.data = None

        self.build_gui()

        # Try to auto-load ic_shape.txt
        default_file = "ic_shape.txt"
        if os.path.isfile(default_file):
            self.filename = default_file
            self.load_file_from_path(default_file)

    def build_gui(self):
        self.root.geometry("1000x600")

        # Open File Button
        tk.Button(self.root, text="Open File", command=self.load_file).grid(row=0, column=0, sticky="ew")

        # Y-variable Listbox
        self.listbox_y = tk.Listbox(self.root, selectmode=tk.MULTIPLE, exportselection=False, width=50)
        self.listbox_y.grid(row=1, column=0, sticky="ns")

        # Plot Button
        tk.Button(self.root, text="Plot", command=self.plot_selected).grid(row=2, column=0, sticky="ew")

    def load_file(self):
        path = filedialog.askopenfilename(filetypes=[("Text files", "*.txt")])
        if path:
            self.filename = path
            self.load_file_from_path(path)

    def load_file_from_path(self, path):
        try:
            raw_data = np.genfromtxt(path, delimiter='\t', dtype=str)
            self.header = raw_data[0]
            self.data = raw_data[1:].astype(float)

            self.listbox_y.delete(0, tk.END)
            for i, var in enumerate(self.header):
                if i > 0:  # Skip r(micron)
                    self.listbox_y.insert(tk.END, var)
        except Exception as e:
            messagebox.showerror("Error", f"Could not read file:\n{e}")

    def plot_selected(self):
        if self.data is None or self.header is None:
            messagebox.showwarning("No file", "Please load a data file first.")
            return

        selections = self.listbox_y.curselection()
        if not selections:
            messagebox.showwarning("No selection", "Please select at least one variable to plot.")
            return

        r = self.data[:, 0]
        fig = Figure(figsize=(10, 6))
        ax = fig.add_subplot(111)
        ax.set_title("Initial Concentration Radial Profile")
        ax.set_xlabel("r (micron)")
        ax.set_ylabel("C (at/m³)")

        for i, idx in enumerate(selections):
            y = self.data[:, idx + 1]
            label = self.header[idx + 1]
            ax.plot(r, y, label=label, color=COLORS[i % len(COLORS)])

        ax.legend()
        ax.grid(True)

        # Display in canvas
        if self.canvas:
            self.canvas.get_tk_widget().destroy()
        self.canvas = FigureCanvasTkAgg(fig, master=self.root)
        self.canvas.draw()
        self.canvas.get_tk_widget().grid(row=1, column=1, rowspan=2, sticky="nsew")

    def run(self):
        self.root.mainloop()


if __name__ == "__main__":
    app = ProfilePlotterGUI()
    app.run()
