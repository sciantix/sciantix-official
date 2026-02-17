import os
import sys
import numpy as np
import matplotlib.pyplot as plt

# Add the project root to path so we can import regression.core
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from regression.core.common import load_output
from regression.core.plot import history_plot

def load_experimental_xy(filepath):
    """
    Load 2-column experimental data (X Y).
    """
    if not os.path.isfile(filepath):
        print(f"Warning: file not found {filepath}")
        return np.array([]), np.array([])
    
    # Check if file has header or not. 
    try:
        data = np.loadtxt(filepath)
        if data.size == 0:
            return np.array([]), np.array([])
        if data.ndim == 1:
            # handle single point case if any
            return np.array([data[0]]), np.array([data[1]])
        return data[:, 0], data[:, 1]
    except Exception as e:
        print(f"Error loading {filepath}: {e}")
        return np.array([]), np.array([])

def main():
    # Directory of this script
    script_dir = os.path.dirname(os.path.abspath(__file__))
    # The HBS test case is in 'test_UO2HBS' subdir
    test_dir = os.path.join(script_dir, "test_UO2HBS")
    
    output_path = os.path.join(test_dir, "output.txt")
    if not os.path.isfile(output_path):
        print(f"Error: output.txt not found at {output_path}")
        return

    # Load Simulation Data
    # load_output expects the directory containing output.txt
    sim_data = load_output(test_dir)
    
    # Column mapping
    # Burnup (MWd/kgUO2) -> Index check
    # HBS pore density (pores/m3)
    # HBS porosity (/)
    # HBS pore radius (m)
    
    headers = [h.strip() for h in sim_data.header]
    
    try:
        idx_bu = headers.index("Burnup (MWd/kgUO2)")
        
        idx_porosity = headers.index("HBS porosity (/)")
        idx_density = headers.index("HBS pore density (pores/m3)")
        idx_radius = headers.index("HBS pore radius (m)")
    except ValueError as e:
        print(f"Error finding columns in output.txt: {e}")
        print("Available headers:", headers)
        return

    bu_sim = sim_data.data[:, idx_bu]
    porosity_sim = sim_data.data[:, idx_porosity]
    density_sim = sim_data.data[:, idx_density]
    radius_sim = sim_data.data[:, idx_radius]

    # Convert units if necessary
    # Experimental data units check:
    # exp_pore_density.txt: 81.31 2.79E+17 -> Looks like Burnup, Density
    # exp_porosity.txt: 81.31 0.05 -> Looks like Burnup, Porosity (fraction?)
    # exp_pore_radius.txt: 81.31 1.5e-7 -> Looks like Burnup, Radius (m)
    
    # Porosity int/fraction check:
    # exp_porosity might be %, let's check one value if possible or assume fraction (/)
    # Sim is (/).
    
    # Radius: Sim is m. Exp could be m or nm.
    # Looking at view_file exp_pore_density.txt: 81.31 2.79E+17. This is definitely pores/m3.
    
    # Load Experimental Data
    f_density = os.path.join(test_dir, "exp_pore_density.txt")
    exp_bu_dens, exp_dens_val = load_experimental_xy(f_density)

    f_porosity = os.path.join(test_dir, "exp_porosity.txt")
    exp_bu_por, exp_por_val = load_experimental_xy(f_porosity)
    # Check if porosity needs scaling. Usually 0-1 or 0-100.
    # If sim is 0-1, and exp is 0-100, we might need adjustments.
    # Assuming standard unit matching for now based on header "(/)".

    f_radius = os.path.join(test_dir, "exp_pore_radius.txt")
    exp_bu_rad, exp_rad_val = load_experimental_xy(f_radius)

    # Generate Plots
    # 1. Pore Density
    out_dens = os.path.join(test_dir, "plot_pore_density.png")
    history_plot(exp_bu_dens, exp_dens_val, 
                 bu_sim, density_sim, 
                 "Burnup (MWd/kgUO2)", "HBS Pore Density (pores/m$^3$)", 
                 "HBS Pore Density Evolution", out_dens)

    # 2. Porosity
    out_por = os.path.join(test_dir, "plot_porosity.png")
    # Note: If exp porosity is %, convert to fraction or vice versa.
    # Let's assume fraction for now.
    history_plot(exp_bu_por, exp_por_val,
                 bu_sim, porosity_sim,
                 "Burnup (MWd/kgUO2)", "HBS Porosity (/)", 
                 "HBS Porosity Evolution", out_por)

    # 3. Pore Radius
    out_rad = os.path.join(test_dir, "plot_pore_radius.png")
    history_plot(exp_bu_rad, exp_rad_val,
                 bu_sim, radius_sim,
                 "Burnup (MWd/kgUO2)", "HBS Pore Radius (m)",
                 "HBS Pore Radius Evolution", out_rad)

if __name__ == "__main__":
    main()
