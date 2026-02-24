"""
sciantix regression suite
author: Giovanni Zullo
"""

import os
import sys
import numpy as np
import matplotlib.pyplot as plt
import glob

# Add the project root to path so we can import regression.core
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from regression.core.common import load_output

def load_xy(filepath):
    """
    Load experimental data from file (X Y) using genfromtxt for simplicity as per user snippet.
    """
    if not os.path.exists(filepath):
        return np.array([])
    try:
        data = np.genfromtxt(filepath)
        if data.ndim == 1:
            return data.reshape(1, -1)
        return data
    except Exception as e:
        print(f"Error loading {filepath}: {e}")
        return np.array([])

def plot_killeen(output, exp_data, test_name, outdir):
    """
    Plot Fission Gas Release vs FIMA for Killeen test case.
    """
    
    # Simulation Data
    try:
        fima = output.get_all("FIMA (%)")
        fgr = output.get_all("Fission gas release (/)")
    except KeyError as e:
        print(f"  Missing column in {test_name}: {e}")
        return

    # Experimental Data
    # Killeen_data[:, 1] is FIMA (X)
    # Killeen_data[:, 0] / 100 is FGR (Y)
    exp_fima = np.array([])
    exp_fgr = np.array([])
    if exp_data.size > 0:
        exp_fima = exp_data[:, 1]
        exp_fgr = exp_data[:, 0] / 100.0

    # Plot
    fig, ax = plt.subplots()
    
    ax.plot(fima, fgr, color='#98E18D', label='SCIANTIX 2.0')
    
    if exp_fima.size > 0:
        ax.scatter(exp_fima, exp_fgr, marker='.', c='#B3B3B3', label='Data from Killeen et al. (1980)')

    ax.set_xlabel('Burnup FIMA (%)')
    ax.set_ylabel('Fission gas release (/)')
    ax.legend()
    
    out_path = os.path.join(outdir, f"{test_name}_FGR.png")
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()
    print(f"Saved: {out_path}")

def plot_solubility(output, exp_data, test_name, outdir):
    """
    Plot Chromium content vs Temperature for Solubility test cases.
    """
    
    # Simulation Data
    try:
        temperature = output.get_all("Temperature (K)")
        cr_solution = output.get_all("Chromium solution (at/m3)")
        chromia_solution = output.get_all("Chromia solution (at/m3)")
        cr_content = cr_solution + chromia_solution
    except KeyError as e:
        print(f"  Missing column in {test_name}: {e}")
        return

    # Experimental Data
    # Riglet-Martial data:  
    conv_factor = 52 * 100 / 6.022e23 / 1.07998e7
    
    exp_temp = np.array([])
    exp_cr = np.array([])
    
    if exp_data.size > 0:
        exp_temp = exp_data[:, 1]
        exp_cr = exp_data[:, 0] / conv_factor

    # Plot
    fig, ax = plt.subplots()
    
    ax.plot(temperature, cr_content, color='#98E18D', label='SCIANTIX 2.0')
    
    if exp_temp.size > 0:
        ax.scatter(exp_temp, exp_cr, marker='.', c='#B3B3B3', label='Data from Riglet-Martial et al. (2016)')

    ax.set_xlabel('Temperature (K)')
    ax.set_ylabel('Chromium content in the lattice (at/m3)')
    ax.legend()
    
    out_path = os.path.join(outdir, f"{test_name}_CrContent.png")
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()
    print(f"Saved: {out_path}")

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    regression_dir = script_dir 
    
    # Identify test directories
    # Case 1: Killeen
    # Case 2: solubility1
    # Case 3: solubility2
    
    # Create figures directory
    figures_dir = os.path.join(regression_dir, "figures")
    os.makedirs(figures_dir, exist_ok=True)
    
    # List of tuples: (TestDirName, ExpFileName, PlotFunction)
    test_cases = [
        ("test_Chromium_Killeen", "Killeen_exp.txt", plot_killeen),
        ("test_Chromium_solubility1", "Riglet-Martial_data_exp1.txt", plot_solubility),
        ("test_Chromium_solubility2", "Riglet-Martial_data_exp2.txt", plot_solubility)
    ]

    for dir_name, exp_file, plot_func in test_cases:
        test_dir = os.path.join(regression_dir, dir_name)
        if not os.path.isdir(test_dir):
            print(f"Directory not found: {test_dir}")
            continue
            
        test_name = dir_name
        
        # Load Output
        try:
            output = load_output(test_dir)
        except Exception as e:
            print(f"  Failed to load output for {test_name}: {e}")
            continue
            
        # Load Exp Data
        exp_path = os.path.join(test_dir, exp_file)
        exp_data = load_xy(exp_path)
        
        # Plot
        try:
            plot_func(output, exp_data, test_name, figures_dir)
        except Exception as e:
            print(f"  Error plotting {test_name}: {e}")
            import traceback
            traceback.print_exc()

if __name__ == "__main__":
    main()
