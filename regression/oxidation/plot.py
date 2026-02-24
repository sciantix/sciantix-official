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

def do_plot(time, temperature, stoichiometry_deviation, data_xy, test_name, outdir):
    """
    Plots stoichiometry deviation comparison.
    """
    fig, ax = plt.subplots(figsize=(8, 6))

    # Plot experimental data
    if data_xy.size > 0:
        # data_xy[:,0] is Time, data_xy[:,1] is Deviation
        ax.plot(data_xy[:, 0], data_xy[:, 1], 'k.', label='Data')

    # Plot comparison (Simulation)
    ax.plot(time, stoichiometry_deviation, 'g-', label='SCIANTIX 2.0')

    # Labeling
    ax.set_xlabel('Time (h)')
    ax.set_ylabel('Stoichiometry deviation (/)')
    
    # Legend
    ax.legend(loc='best')

    # Title with mean temperature
    mean_temp = np.mean(temperature)
    plt.title(f"{test_name}\nTemperature: {mean_temp:.0f} K")
    plt.tight_layout()

    # Save
    out_path = os.path.join(outdir, f"{test_name}.png")
    plt.savefig(out_path, dpi=150)
    plt.close()
    print(f"Saved: {out_path}")

def load_xy(filepath):
    """
    Load experimental data from file (Time, Deviation).
    Skips headers/comments and reads 2-column numeric data.
    """
    if not os.path.exists(filepath):
        return np.array([])
    try:
        with open(filepath, 'r') as f:
            lines = f.readlines()
        
        valid_lines = []
        for line in lines:
            parts = line.strip().split()
            if not parts: continue
            
            # Check if line starts with a number
            try:
                float(parts[0])
                # Ensure we have at least 2 columns
                if len(parts) >= 2:
                    valid_lines.append(line)
            except ValueError:
                # Likely header or comment
                continue
        
        if not valid_lines:
            return np.array([])
            
        from io import StringIO
        # Use simple whitespace delimiter splitting
        data = np.loadtxt(StringIO("".join(valid_lines)))
        
        # Ensure 2D array even if single point
        if data.ndim == 1:
            return data.reshape(1, -1)
            
        return data

    except Exception as e:
        print(f"Error loading {filepath}: {e}")
        return np.array([])

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    regression_dir = script_dir # The script is in regression/oxidation/
    
    # Find all test folders (exclude figures and hidden)
    # Assuming test folders start with "test_" or are just directories that are not 'figures'
    # Based on list_dir: test_UO2_oxidation_...
    test_dirs = sorted([d for d in glob.glob(os.path.join(regression_dir, "test_*")) if os.path.isdir(d)])
    
    if not test_dirs:
        print("No test directories found.")
        return

    # Create figures directory
    figures_dir = os.path.join(regression_dir, "figures")
    os.makedirs(figures_dir, exist_ok=True)

    for test_dir in test_dirs:
        test_name = os.path.basename(test_dir)
        
        # Load Output
        try:
            output = load_output(test_dir)
        except Exception as e:
            print(f"  Failed to load output for {test_name}: {e}")
            continue

        # Get variables
        try:
            time = output.get_all("Time (h)")
            temp = output.get_all("Temperature (K)")
            dev = output.get_all("Stoichiometry deviation (/)")
        except KeyError as e:
            print(f"  Missing column in {test_name}: {e}")
            continue

        # Load Experimental Data: data.txt
        f_data = os.path.join(test_dir, "data.txt")
        data_xy = load_xy(f_data)
        
        # Run Plotter
        try:
            do_plot(
                time, 
                temp, 
                dev,
                data_xy,
                test_name, 
                figures_dir
            )
        except Exception as e:
            print(f"  Error plotting {test_name}: {e}")

if __name__ == "__main__":
    main()
