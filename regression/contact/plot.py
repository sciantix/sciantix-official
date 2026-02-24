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
    Load experimental data from file (X Y).
    """
    if not os.path.exists(filepath):
        return np.array([])
    try:
        # Check if file has header or not. 
        with open(filepath, 'r') as f:
            lines = f.readlines()
        
        valid_lines = []
        for line in lines:
            parts = line.strip().split()
            if not parts: continue
            try:
                float(parts[0])
                if len(parts) >= 2:
                    valid_lines.append(line)
            except ValueError:
                continue 
        
        if not valid_lines:
            return np.array([])
            
        from io import StringIO
        data = np.loadtxt(StringIO("".join(valid_lines)))
        
        if data.ndim == 1:
            return data.reshape(1, -1)
        return data
    except Exception as e:
        print(f"Error loading {filepath}: {e}")
        return np.array([])

def do_plot(exp_Xe133, exp_Kr85m, calculated_Kr85m_ANS, calculated_Xe133_ANS, 
            time, temperature, burnup, Xe133, Kr85m, expfgr, fgr, burnup2, 
            test_name, outdir):
    """
    Generates plots for Xe133, Kr85m, and FGR.
    """
    
    # 1. Xe133 Plot
    fig, ax = plt.subplots(figsize=(8, 6))
    
    # Temperature (Twin Axis)
    axT = ax.twinx()
    axT.set_ylabel('Temperature (K)')
    axT.plot(burnup, temperature, 'r-', linewidth=1, label="Temperature", alpha=0.5)
    axT.set_zorder(1) # behind

    # SCIANTIX
    ax.plot(burnup, Xe133, color='#98E18D', linewidth=2, label='SCIANTIX 2.0')
    
    # x/5 deviation
    # Ensure Xe133 > 0 for log fill
    # ax.fill_between(burnup, (Xe133)/5, (Xe133)*5, color='#ffcc80', alpha=0.5, label='x/5 deviation')

    # ANS Data
    if calculated_Xe133_ANS.size > 0:
        ax.plot(burnup2, calculated_Xe133_ANS[:,1], '--', color='black', linewidth=1, label='ANS5.4-2010 ${}^{133}$Xe')  

    # Experimental Data
    if exp_Xe133.size > 0:
        ax.plot(exp_Xe133[:,0], exp_Xe133[:,1], 'o', color='#B3B3B3', label='Data from IFPE ${}^{133}$Xe')

    ax.set_zorder(2)
    ax.patch.set_visible(False) # necessary for zorder with twinx?

    ax.set_xlabel('Burnup (GWd/tU)')
    ax.set_ylabel('Release-to-birth ratio (/)')
    ax.set_yscale('log')
    ax.set_ylim(1e-5, 1.0)

    # Combined Legend
    h1, l1 = ax.get_legend_handles_labels()
    h2, l2 = axT.get_legend_handles_labels()
    ax.legend(h1+h2, l1+l2, loc='lower right')
    
    out_xe = os.path.join(outdir, f"{test_name}_Xe133.png")
    plt.tight_layout()
    plt.savefig(out_xe, dpi=150)
    plt.close()
    print(f"Saved: {out_xe}")

    # 2. Kr85m Plot
    fig, ax = plt.subplots(figsize=(8, 6))

    axT = ax.twinx()
    axT.set_ylabel('Temperature (K)')
    axT.plot(burnup, temperature, 'r-', linewidth=1, label="Temperature", alpha=0.5)
    axT.set_zorder(1)

    ax.plot(burnup, Kr85m, color='#98E18D', linewidth=2, label='SCIANTIX 2.0')
    # ax.fill_between(burnup, (Kr85m)/5, (Kr85m)*5, color='#ffcc80', alpha=0.5, label='x/5 deviation')

    if calculated_Kr85m_ANS.size > 0:
        ax.plot(burnup2, calculated_Kr85m_ANS[:,1], '--', color='black', linewidth=1, label='ANS5.4-2010 ${}^{85m}$Kr')  

    if exp_Kr85m.size > 0:
        ax.plot(exp_Kr85m[:,0], exp_Kr85m[:,1], 'o', color='#B3B3B3', label='Data from IFPE ${}^{85m}$Kr')

    ax.set_zorder(2)
    ax.patch.set_visible(False)

    ax.set_xlabel('Burnup (GWd/tU)')
    ax.set_ylabel('Release-to-birth ratio (/)')
    ax.set_yscale('log')
    ax.set_ylim(1e-5, 1.0)

    h1, l1 = ax.get_legend_handles_labels()
    h2, l2 = axT.get_legend_handles_labels()
    ax.legend(h1+h2, l1+l2, loc='lower right')

    out_kr = os.path.join(outdir, f"{test_name}_Kr85m.png")
    plt.tight_layout()
    plt.savefig(out_kr, dpi=150)
    plt.close()
    print(f"Saved: {out_kr}")

    # 3. FGR Plot
    fig, ax = plt.subplots(figsize=(8, 6))
    
    axT = ax.twinx()
    axT.set_ylabel('Temperature (K)')
    axT.plot(burnup, temperature, 'r-', linewidth=1, label="Temperature", alpha=0.5)
    axT.set_zorder(1)

    ax.plot(burnup, fgr, color='#98E18D', linewidth=2, label='SCIANTIX 2.0')
    
    if expfgr.size > 0:
        ax.plot(expfgr[:, 0], expfgr[:, 1], '--o', color='black', linewidth=1, label='Exp.')
    
    ax.set_zorder(2)
    ax.patch.set_visible(False)
    
    ax.set_xlabel('Burnup (GWd/tU)')
    ax.set_ylabel('Fission gas release (/)')
    
    h1, l1 = ax.get_legend_handles_labels()
    h2, l2 = axT.get_legend_handles_labels()
    ax.legend(h1+h2, l1+l2, loc='best')

    out_fgr = os.path.join(outdir, f"{test_name}_FGR.png")
    plt.tight_layout()
    plt.savefig(out_fgr, dpi=150)
    plt.close()
    print(f"Saved: {out_fgr}")


def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    regression_dir = script_dir 
    
    # Find test folders containing "CONTACT"
    test_dirs = sorted([d for d in glob.glob(os.path.join(regression_dir, "*CONTACT*")) if os.path.isdir(d)])
    
    if not test_dirs:
        print("No Contact test directories found.")
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
            # Burnup conversion as per user request: / 0.8814
            burnup_raw = output.get_all("Burnup (MWd/kgUO2)") 
            burnup = burnup_raw / 0.8814
            
            Xe133 = output.get_all("Xe133 R/B (/)")
            Kr85m = output.get_all("Kr85m R/B (/)")
            fgr = output.get_all("Fission gas release (/)")
        except KeyError as e:
            print(f"  Missing column in {test_name}: {e}")
            continue

        # Load Experimental/Benchmark Data
        exp_Xe133 = load_xy(os.path.join(test_dir, "experimental_RB_Xe133.txt"))
        exp_Kr85m = load_xy(os.path.join(test_dir, "experimental_RB_Kr85m.txt"))
        exp_fgr = load_xy(os.path.join(test_dir, "experimental_fgr.txt"))
        
        calc_Xe133_ANS = load_xy(os.path.join(test_dir, "ANS54-2010-XE133.txt"))
        calc_Kr85m_ANS = load_xy(os.path.join(test_dir, "ANS54-2010-KR85M.txt"))
        
        # Calculate burnup2 for ANS data plotting
        # logic: burnup2 = calculated_burnup[:,1] * burnup[-1] / calculated_burnup[-1,1]
        time_burnup_data = load_xy(os.path.join(test_dir, "time_burnup.txt"))
        if time_burnup_data.size > 0:
            # Assuming time_burnup.txt is (Time, Burnup)
            # calculated_burnup[:,1]
            burnup2_raw = time_burnup_data[:, 1]
            if burnup2_raw[-1] != 0:
                burnup2 = burnup2_raw * burnup[-1] / burnup2_raw[-1]
            else:
                burnup2 = burnup2_raw
        else:
            burnup2 = np.array([])

        # Run Plotter
        try:
            do_plot(
                exp_Xe133, 
                exp_Kr85m, 
                calc_Kr85m_ANS, 
                calc_Xe133_ANS,
                time, 
                temp, 
                burnup, 
                Xe133, 
                Kr85m, 
                exp_fgr, 
                fgr, 
                burnup2,
                test_name, 
                figures_dir
            )
        except Exception as e:
            print(f"  Error plotting {test_name}: {e}")
            import traceback
            traceback.print_exc()

        # --------------------
        # Statistical analysis
        # --------------------
        print("\n" + "="*50)
        print("STATISTICAL ANALYSIS")
        print("="*50)

        # 1. Xe133
        if exp_Xe133.size > 0:
            exp_burn_xe = exp_Xe133[:, 0]
            exp_val_xe = exp_Xe133[:, 1]
            
            indices_xe = [np.abs(burnup - b).argmin() for b in exp_burn_xe]
            sim_val_xe = Xe133[indices_xe]
            
            error_xe = sim_val_xe - exp_val_xe
            rmse_xe = np.sqrt(np.mean(error_xe**2))
            mad_xe = np.median(np.abs(error_xe))
            
            print(f"Xe133 -> RMSE: {rmse_xe:.4e}, MAD: {mad_xe:.4e}")
        
        # 2. Kr85m
        if exp_Kr85m.size > 0:
            exp_burn_kr = exp_Kr85m[:, 0]
            exp_val_kr = exp_Kr85m[:, 1]
            
            indices_kr = [np.abs(burnup - b).argmin() for b in exp_burn_kr]
            sim_val_kr = Kr85m[indices_kr]
            
            error_kr = sim_val_kr - exp_val_kr
            rmse_kr = np.sqrt(np.mean(error_kr**2))
            mad_kr = np.median(np.abs(error_kr))
            
            print(f"Kr85m -> RMSE: {rmse_kr:.4e}, MAD: {mad_kr:.4e}")
        print("-" * 40)

if __name__ == "__main__":
    main()
