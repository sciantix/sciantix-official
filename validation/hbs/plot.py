"""
sciantix regression suite
author: Giovanni Zullo
"""

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

    # Load output
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

    # Load experimental data
    f_density = os.path.join(test_dir, "exp_pore_density.txt")
    exp_bu_dens, exp_dens_val = load_experimental_xy(f_density)

    f_porosity = os.path.join(test_dir, "exp_porosity.txt")
    exp_bu_por, exp_por_val = load_experimental_xy(f_porosity)

    f_radius = os.path.join(test_dir, "exp_pore_radius.txt")
    exp_bu_rad, exp_rad_val = load_experimental_xy(f_radius)

    # Create figures directory
    outdir = os.path.join(script_dir, "figures")
    os.makedirs(outdir, exist_ok=True)

    # 1. Pore density
    out_dens = os.path.join(outdir, "plot_pore_density.png")
    history_plot(exp_bu_dens, exp_dens_val, 
                 bu_sim, density_sim, 
                 "Burnup (MWd/kgUO2)", "HBS pore density (pores/m$^3$)", 
                 "HBS pore density evolution", out_dens)

    # 2. Porosity
    out_por = os.path.join(outdir, "plot_porosity.png")
    history_plot(exp_bu_por, exp_por_val,
                 bu_sim, porosity_sim,
                 "Burnup (MWd/kgUO2)", "HBS porosity (/)", 
                 "HBS porosity evolution", out_por)

    # 3. Pore radius
    out_rad = os.path.join(outdir, "plot_pore_radius.png")
    history_plot(exp_bu_rad, exp_rad_val,
                 bu_sim, radius_sim,
                 "Burnup (MWd/kgUO2)", "HBS pore radius (m)",
                 "HBS pore radius evolution", out_rad)

    # 4. Walker Xe
    plot_walker_xe(sim_data, test_dir, outdir)

    # 5. Spino Swelling
    plot_spino_swelling(sim_data, test_dir, outdir)

def lassmann_fit(x, threshold=60):
    """
    K. Lassmann et al. / Journal of Nuclear Materials 226 (1995) 1-8
    """
    result = []
    for value in x:
        if value < threshold:
            result.append(1.46e-2 * value)  # linear increase
        else:
            result.append(1.46e-2 * (1/0.0584 + (60 - 1/0.0584) * np.exp(-0.0584 * (value - 60)))) # exponential decrease
    return np.array(result)

def plot_walker_xe(sim_data, test_dir, outdir):
    # Map variables
    try:
        bu = sim_data.get_all("Burnup (MWd/kgUO2)")
        xe_ig = sim_data.get_all("Xe in grain (at/m3)")
        xe_igs = sim_data.get_all("Xe in intragranular solution (at/m3)")
        xe_igb = sim_data.get_all("Xe in intragranular bubbles (at/m3)")
        xe_igHBS = sim_data.get_all("Xe in grain HBS (at/m3)")
        alpha = sim_data.get_all("Restructured volume fraction (/)")
    except KeyError as e:
        print(f"Missing column for Walker plot: {e}")
        return

    # Load experimental data
    walker_file = os.path.join(test_dir, "walker_data_1999.txt")
    if not os.path.exists(walker_file):
        print(f"Walker data not found: {walker_file}")
        return
    
    try:
        data_walker = np.genfromtxt(walker_file)
        # Handle case where file might have header or be empty
        if data_walker.ndim > 1 and data_walker.shape[1] >= 2:
            data_bu = data_walker[:, 0]
            data_xe = data_walker[:, 1]
        else:
            print(f"Invalid walker data format in {walker_file}")
            return
    except Exception as e:
        print(f"Error loading walker data: {e}")
        return

    bu_gwd_tu = np.linspace(0, 200, 1000)
    eq = 4.88897e26

    fig, ax1 = plt.subplots()
    
    # Plot Lassmann fit
    ax1.plot(bu_gwd_tu, lassmann_fit(bu_gwd_tu), color='darkorchid', linestyle='--', label='Lassmann fit (bu$_0$ = 60 GWd/tU)')
    
    # Plot Walker data
    ax1.scatter(data_bu, data_xe, color='navy', edgecolors='black', marker='.', label='Walker data (1999)')
    
    # Plot output
    # bu / 0.8814 converts MWd/kgUO2 to MWd/kgU = GWd/tU
    bu_converted = bu / 0.8814
    
    ax1.plot(bu_converted, xe_ig / eq, color='limegreen', linestyle='-.', label='Xe in grains - non HBS')
    ax1.plot(bu_converted, xe_igs / eq, linestyle='-.', label='Xe in dynamic solution - non HBS')
    ax1.plot(bu_converted, xe_igb / eq, linestyle='-.', label='Xe in intra-granular bubbles - non HBS')
    ax1.plot(bu_converted, xe_igHBS / eq, color='orangered', linestyle=':', label='Xe in grains - HBS')
    ax1.plot(bu_converted, (xe_igHBS + xe_ig) / eq, color='dimgray', linestyle='-', label='Xe in grains - sum')
    
    ax1.legend(loc='upper right', fontsize='medium')
    ax1.set_xlabel('Burnup (GWd/tU)', fontsize='large')
    ax1.set_ylabel('Xe in grains (wt%)', color='black', fontsize='large')
    ax1.set_ylim(0, 1.75)
    
    ax2 = ax1.twinx()
    ax2.plot(bu, alpha, color='gold', linestyle='--', label='Restructured volume fraction (/)')
    ax2.set_ylabel('Restructured volume fraction (/)', color='black', fontsize='large')

    out_path = os.path.join(outdir, "plot_walker_xe.png")
    plt.tight_layout()
    plt.savefig(out_path, dpi=180)
    plt.close()
    print("Saved:", out_path)

def plot_spino_swelling(sim_data, test_dir, outdir):
    try:
        bu = sim_data.get_all("Burnup (MWd/kgUO2)")
        swe_igs = sim_data.get_all("Intragranular gas solution swelling (/)")
        swe_igb = sim_data.get_all("Intragranular gas bubble swelling (/)")
        fima = sim_data.get_all("FIMA (%)")
        alpha = sim_data.get_all("Restructured volume fraction (/)")
    except KeyError as e:
        print(f"Missing column for Spino plot: {e}")
        return

    spino_file = os.path.join(test_dir, "spino_swelling_data.txt")
    if not os.path.exists(spino_file):
        print(f"Spino data not found: {spino_file}")
        return

    try:
        data_spino = np.genfromtxt(spino_file)
        if data_spino.ndim > 1 and data_spino.shape[1] >= 2:
            data_bu = data_spino[:, 0]
            data_swe = data_spino[:, 1]
        else:
            print(f"Invalid spino data format in {spino_file}")
            return
    except Exception as e:
        print(f"Error loading spino data: {e}")
        return

    # Plot output
    # bu / 0.8814 converts MWd/kgUO2 to MWd/kgU = GWd/tU
    bu_converted = bu / 0.8814

    fig, ax1 = plt.subplots()
    ax1.plot(bu_converted, 0.00303 * fima, color='indianred', linestyle='-.', label='Solid fission products (from Olander correlation)')
    ax1.plot(bu_converted, swe_igs, color='orchid', linestyle='-.', label='SCIANTIX gas solution')
    ax1.plot(bu_converted, swe_igb, color='darkgreen', linestyle='-.', label='SCIANTIX gas bubble')
    ax1.plot(bu_converted, swe_igs + swe_igb + 0.0032 * fima, color='darkblue', linestyle='-', label='Total')
    ax1.scatter(data_bu, data_swe, color='navy', edgecolors='black', marker='.', label='Spino et al. data (2005)')
    
    ax1.legend(loc='upper right', fontsize='medium')
    ax1.set_xlabel('Burnup (GWd/tU)', fontsize='large')
    ax1.set_ylabel('Fuel matrix swelling (/)', color='black', fontsize='large')
    ax1.set_xlim(0, 145)
    
    ax2 = ax1.twinx()
    ax2.plot(bu, alpha, color='gold', linestyle='--', label='Restructured volume fraction (/)')
    ax2.set_ylabel('Restructured volume fraction (/)', color='black', fontsize='large')
    
    out_path = os.path.join(outdir, "plot_spino_swelling.png")
    plt.tight_layout()
    plt.savefig(out_path, dpi=180)
    plt.close()
    print("Saved:", out_path)


if __name__ == "__main__":
    main()
