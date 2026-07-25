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

def do_plot(heReleasedTalip14000_data, heReleasedRateTalip14000_data, time, temperature, heReleasedFrac, heReleaseRate,
            test_name, outdir):
    """
    Plots Helium fractional release and release rate comparison.
    """
    fig, ax = plt.subplots(1, 2, figsize=(12, 5))

    plt.subplots_adjust(left=0.1,
                        bottom=0.1,
                        right=0.9,
                        top=0.9,
                        wspace=0.34,
                        hspace=0.4)

    # Plot 1: He fractional release
    if heReleasedTalip14000_data.size > 0:
        ax[0].scatter(heReleasedTalip14000_data[:,0], heReleasedTalip14000_data[:,1], marker='.', c='#B3B3B3', label='Data from Talip et al. (2014)')
    
    ax[0].plot(time, heReleasedFrac, color='#98E18D', label='SCIANTIX 2.0')

    axT = ax[0].twinx()
    axT.set_ylabel('Temperature (K)')
    axT.plot(time, temperature, 'r', linewidth=1, label="Temperature", alpha=0.5)

    ax[0].set_xlabel('Time (h)')
    ax[0].set_ylabel('Helium fractional release (/)')
    
    # Combined legend
    h1, l1 = ax[0].get_legend_handles_labels()
    h2, l2 = axT.get_legend_handles_labels()
    ax[0].legend(h1+h2, l1+l2, loc='best')

    # Plot 2: He release rate
    if heReleasedRateTalip14000_data.size > 0:
        ax[1].scatter(heReleasedRateTalip14000_data[:,0], heReleasedRateTalip14000_data[:,1], marker='.', c='#B3B3B3', label='Data from Talip et al. (2014)')
    
    # SCIANTIX release rate vs Temperature
    ax[1].plot(temperature, heReleaseRate, color='#98E18D', label='SCIANTIX 2.0')

    ax[1].set_xlabel('Temperature (K)')
    ax[1].set_ylabel('Helium release rate (at m$^{-3}$ s$^{-1}$)')
    ax[1].legend()

    # Save
    out_path = os.path.join(outdir, f"{test_name}.png")
    plt.savefig(out_path, dpi=150)
    plt.close()
    print(f"Saved: {out_path}")

def load_xy(filepath):
    if not os.path.exists(filepath):
        return np.array([])
    try:
        return np.genfromtxt(filepath)
    except:
        return np.array([])

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    regression_dir = script_dir # The script is in regression/talip/
    
    # Find all test folders
    test_dirs = sorted([d for d in glob.glob(os.path.join(regression_dir, "test_Talip*")) if os.path.isdir(d)])
    
    if not test_dirs:
        print("No test directories found.")
        return

    # Create figures directory
    figures_dir = os.path.join(regression_dir, "figures")
    os.makedirs(figures_dir, exist_ok=True)

    for test_dir in test_dirs:
        test_name = os.path.basename(test_dir)
        print(f"Processing {test_name}...")
        
        # Load Output
        try:
            output = load_output(test_dir)
        except Exception as e:
            print(f"  Failed to load output for {test_name}: {e}")
            continue

        # Get variables
        # Time (h)
        # Temperature (K)
        # He fractional release (/)
        # He release rate (at/m3 s)
        try:
            time = output.get_all("Time (h)")
            temp = output.get_all("Temperature (K)")
            frac = output.get_all("He fractional release (/)")
            rate = output.get_all("He release rate (at/m3 s)")
        except KeyError as e:
            print(f"  Missing column in {test_name}: {e}")
            continue

        # Load experimental data
        # Release data: Talip2014_release_data.txt
        # Rate data: Talip2014_rrate_data.txt
        f_release = os.path.join(test_dir, "Talip2014_release_data.txt")
        f_rate = os.path.join(test_dir, "Talip2014_rrate_data.txt")
        
        data_release = load_xy(f_release)
        data_rate = load_xy(f_rate)
        
        # Run Plotter
        try:
            do_plot(
                data_release, 
                data_rate, 
                time, 
                temp, 
                frac, 
                rate, 
                test_name, 
                figures_dir
            )
        except Exception as e:
            print(f"  Error plotting {test_name}: {e}")

if __name__ == "__main__":
    main()
