"""
sciantix regression suite
author: Elisa Cappellari, Giovanni Zullo
"""

import os
import sys
import numpy as np
import matplotlib.pyplot as plt
import glob

# Add the project root to path so we can import regression.core
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from regression.core.common import load_output

# ==========================================
# 1990 PLOTTING LOGIC
# ==========================================
def do_plot_1990(sim_fgr, sim_swelling, exp_fgr, exp_swelling, outdir):
    """
    Generates parity plots for FGR and Swelling (Kashibe 1990).
    """
    cases = [
        {'idx': 0, 'temp': 1873, 'bu': 6,  'col': 'C0', 'mrk': 'o'},
        {'idx': 1, 'temp': 1873, 'bu': 16, 'col': 'C2', 'mrk': 'o'},
        {'idx': 2, 'temp': 1873, 'bu': 23, 'col': 'C3', 'mrk': 'o'},
        {'idx': 3, 'temp': 1873, 'bu': 28, 'col': 'C4', 'mrk': 'o'},
        {'idx': 4, 'temp': 2073, 'bu': 6,  'col': 'C0', 'mrk': '^'},
        {'idx': 5, 'temp': 2073, 'bu': 16, 'col': 'C2', 'mrk': '^'},
        {'idx': 6, 'temp': 2073, 'bu': 23, 'col': 'C3', 'mrk': '^'},
        {'idx': 7, 'temp': 2073, 'bu': 28, 'col': 'C4', 'mrk': '^'},
    ]

    # --- Plot 1: FGR ---
    fig, ax = plt.subplots(figsize=(8, 8))
    for c in cases:
        i = c['idx']
        if exp_fgr[i] > 0 and sim_fgr[i] > 0:
            ax.errorbar(exp_fgr[i], sim_fgr[i], 
                        elinewidth=0.5, linewidth=0.5, 
                        color=c['col'], fmt=c['mrk'], markersize=8)

    r = np.linspace(1, 100, 100)
    ax.plot(r, r, color='gray', linestyle='-', linewidth=0.5)
    ax.plot(r, r * 2, color='gray', linestyle='--', linewidth=0.5)
    ax.plot(r, r * 0.5, color='gray', linestyle='--', linewidth=0.5)
    ax.annotate('x2', (1.25, 3), color='k')
    ax.annotate('/2', (3, 1.3),  color='k')

    ax.set_yscale('log')
    ax.set_xscale('log')
    ax.set_xlim(1, 100)
    ax.set_ylim(1, 100)
    ax.set_xlabel('FGR experimental (%)')
    ax.set_ylabel('FGR calculated (%)')

    custom_lines = [
        plt.Line2D([0], [0], color='k', marker='o', linestyle='', markersize=8, label='T = 1873 K'),
        plt.Line2D([0], [0], color='k', marker='^', linestyle='', markersize=8, label='T = 2073 K'),
        plt.Line2D([0], [0], color='C0', marker='s', linestyle='', markersize=8, label='6 GWd tU$^{-1}$'),
        plt.Line2D([0], [0], color='C2', marker='s', linestyle='', markersize=8, label='16 GWd tU$^{-1}$'),
        plt.Line2D([0], [0], color='C3', marker='s', linestyle='', markersize=8, label='23 GWd tU$^{-1}$'),
        plt.Line2D([0], [0], color='C4', marker='s', linestyle='', markersize=8, label='28 GWd tU$^{-1}$')
    ]
    ax.legend(handles=custom_lines, loc='best', frameon=False)

    out_fgr = os.path.join(outdir, "par_FGR_Kashibe1990.png")
    plt.tight_layout()
    plt.savefig(out_fgr, dpi=150)
    plt.close()
    print(f"Saved: {out_fgr}")

    # --- Plot 2: Swelling ---
    fig, ax = plt.subplots(figsize=(8, 8))
    for c in cases:
        i = c['idx']
        if exp_swelling[i] > 0 and sim_swelling[i] > 0:
            ax.errorbar(exp_swelling[i], sim_swelling[i], 
                        elinewidth=0.5, linewidth=0.5, 
                        color=c['col'], fmt=c['mrk'], markersize=8)

    ax.plot(r, r, color='gray', linestyle='-', linewidth=0.5)
    ax.plot(r, r * 2, color='gray', linestyle='--', linewidth=0.5)
    ax.plot(r, r * 0.5, color='gray', linestyle='--', linewidth=0.5)
    ax.annotate('x2', (1.25, 3), color='k')
    ax.annotate('/2', (3, 1.3),  color='k')

    ax.set_yscale('log')
    ax.set_xscale('log')
    ax.set_xlim(1, 100)
    ax.set_ylim(1, 100)
    ax.set_xlabel('Swelling experimental (%)')
    ax.set_ylabel('Swelling calculated (%)')
    ax.legend(handles=custom_lines, loc='best', frameon=False)

    out_sw = os.path.join(outdir, "par_Swelling_Kashibe1990.png")
    plt.tight_layout()
    plt.savefig(out_sw, dpi=150)
    plt.close()
    print(f"Saved: {out_sw}")


# ==========================================
# 1993 PLOTTING LOGIC
# ==========================================
def do_plot_1993(sim_density, sim_radius, exp_density, exp_radius, outdir):
    """
    Generates parity plots for Bubble Density and Radius (Kashibe 1993).
    """
    # Plot 1: Density
    fig, ax = plt.subplots(figsize=(6, 6))
    ax.scatter(exp_density, sim_density, c='#FA82B4', edgecolors='#999AA2', marker='^', s=50, label='SCIANTIX 2.0')

    r = np.linspace(0.1, 100, 1000)
    ax.plot(r, r, '-', color='#757575')
    ax.plot(r, r * 0.5, '--', color='#757575')
    ax.plot(r, r * 2.0, '--', color='#757575')

    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.set_xlim(0.1, 100)
    ax.set_ylim(0.1, 100)
    ax.set_xlabel('Experimental (10$^{23}$ bub/m$^3$)')
    ax.set_ylabel('Calculated (10$^{23}$ bub/m$^3$)')
    ax.legend()
    
    out_dens = os.path.join(outdir, "par_Density_Kashibe1993.png")
    plt.tight_layout()
    plt.savefig(out_dens, dpi=150)
    plt.close()
    print(f"Saved: {out_dens}")

    # Plot 2: Radius
    fig, ax = plt.subplots(figsize=(6, 6))
    ax.scatter(exp_radius, sim_radius, c='#FA82B4', edgecolors='#999AA2', marker='^', s=50, label='SCIANTIX 2.0')
    
    r = np.logspace(np.log10(0.1e-9), np.log10(100e-9), 100)
    ax.plot(r, r, '-', color='#757575')
    ax.plot(r, r * 0.5, '--', color='#757575')
    ax.plot(r, r * 2.0, '--', color='#757575')
    
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.set_xlim(0.1e-9, 100e-9)
    ax.set_ylim(0.1e-9, 100e-9)
    ax.set_xlabel('Experimental (m)')
    ax.set_ylabel('Calculated (m)')
    ax.legend()

    out_rad = os.path.join(outdir, "par_Radius_Kashibe1993.png")
    plt.tight_layout()
    plt.savefig(out_rad, dpi=150)
    plt.close()
    print(f"Saved: {out_rad}")

# ==========================================
# 1991 PLOTTING LOGIC
# ==========================================

# Data strings
ISO23_DATA = """
11.068411577343866; -0.03468834013740718
22.671144347504963; 0.11047279859867665
101.78068596223976; 1.6688800657174152
113.73501669513304; 2.071105851979345
135.53409038452662; 2.7222240793160815
147.1368231546877; 2.9203304362669087
158.73955592484884; 3.1124237208574552
170.34228869500993; 3.2976913557417404
181.94502146517107; 3.534151363422996
246.92032497807327; 5.021655452980754
259.5778516364308; 5.289805977155375
273.47769109644196; 5.599271545054677
286.369616396621; 5.771583085589111
299.1778278961495; 5.996957216621588
312.2706663178897; 6.163380795910916
"""

CYCLE23_DATA = """
19.50676268291558; 0.0567266112857574
32.04019594266178; 0.24234181076593053
43.44247014583533; 0.6915057833732536
54.23382300199917; 0.9795957054993174
65.59314179796105; 1.1827712949700917
77.5204265337211; 1.4910068846815836
89.38685777593132; 1.6906672958066054
101.09817226987734; 1.9417626579657075
112.32862484420441; 2.253224749647943
123.93135761436552; 2.5350875165360396
135.53409038452662; 2.7222240793160815
147.1368231546877; 2.9203304362669087
158.73955592484884; 3.1124237208574552
170.34228869500993; 3.2976913557417404
181.94502146517107; 3.534151363422996
193.54775423533215; 3.699429595596081
205.15048700549323; 3.8564195388401306
216.75321977565437; 4.020885193667233
228.35595254581546; 4.099705196227649
239.9586853159766; 4.214278602011351
251.56141808613768; 4.282535099073982
263.16415085629876; 4.409297165047439
274.7668836264599; 4.587251603817869
286.369616396621; 4.743266454246738
297.97234916678207; 4.867590788182245
309.5750819369432; 5.011416978421359
321.1778147071043; 5.186933685153839
333.8353413654619; 5.308332740643804
345.43807413562297; 5.440457817100752
357.4803043591993; 5.536951376936317
368.20404222252995; 5.624100297293069
380.2462724461063; 5.74517432184464
391.8490052162674; 5.854872263552439
403.6275369677946; 5.938730245657958
415.0544707565896; 6.0474530945505744
421.3832340857684; 6.062079486778282
"""

ISO28_DATA = """
4.386491816524284; 0.0
9.140119319097948; 0.02019367028814223
13.166563011079617; 1.498819083608879
12.991500241863022; 1.4741379310344822
18.24338331836084; 2.0911667453944247
25.070831317808015; 2.6661139656618413
34.5242208555041; 3.0782496671962907
46.078363623799305; 3.275862068965518
57.63250639209453; 3.4472023017134
69.18664916038972; 3.566324558766693
80.74079192868496; 3.6740241336367916
92.29493469698016; 3.793962296560313
103.84907746527537; 3.8576029544380965
115.40322023357058; 3.9359299179799887
126.95736300186579; 3.9775411173616178
138.511505770161; 4.068106668956927
149.54046023080642; 4.237978271138402
161.8823854605763; 4.4055857345299945
175.27468730564576; 4.617619272555503
186.52872246956966; 5.124704770902218
199.25828668831915; 5.784364666981576
210.98749222583095; 6.008738781294284
236.1965309930205; 6.31837505904582
248.80105037661528; 6.426074633915919
261.40556976021; 6.547236655644781
274.01008914380486; 6.752165013383719
287.1397968350494; 6.896138403401039
299.74431621864414; 7.01056920170052
312.6114297560638; 7.225968351440718
323.20272729366775; 7.354983467170523
"""

CYCLE28_DATA = """
9.140119319097948; 0.02019367028814223
13.166563011079617; 1.498819083608879
18.24338331836084; 2.0911667453944247
25.070831317808015; 2.6661139656618413
34.5242208555041; 3.0782496671962907
46.078363623799305; 3.275862068965518
57.63250639209453; 3.4472023017134
69.18664916038972; 3.566324558766693
80.74079192868496; 3.6740241336367916
92.29493469698016; 3.793962296560313
103.84907746527537; 3.8576029544380965
115.40322023357058; 3.9359299179799887
126.95736300186579; 3.9775411173616178
138.511505770161; 4.068106668956927
150.0656485384562; 4.173358526216344
161.6197913067514; 4.250869583888006
173.17393407504662; 4.330012453300123
184.50299614006335; 4.4923544098792085
196.28221961163703; 4.530725297376216
207.83636237993224; 4.643320307467684
219.39050514822745; 4.741229011895047
230.94464791652265; 4.819555975436938
242.49879068481792; 4.856271739597199
254.05293345311313; 4.954180444024562
265.6070762214083; 5.054536866062611
276.59563158146534; 5.179935322117654
288.7153617579987; 5.149997852879288
300.269504526294; 5.196504487482285
312.2613042176307; 5.295790033065659
322.9401331398428; 5.3468351440717985
334.93193283117955; 5.347283892300423
341.0241171999171; 5.503448275862068
350.6875820606731; 5.46330570704685
362.16092662779135; 5.5900221648922646
374.11098058185337; 5.6219178082191785
385.35001036555866; 5.695838880061837
396.9041531338539; 5.766822690771674
408.2159012986184; 5.773939900439663
416.86130882454563; 5.891166745394425
"""

def parse_xy(data_str):
    arr = []
    for line in data_str.strip().split("\n"):
        parts = line.split(";")
        if len(parts) >= 2:
            arr.append([float(parts[0]), float(parts[1])])
    return np.array(arr)

def do_plot_1991(datasets, exp_iso, exp_cyc, time_ref, outname, outdir):
    """
    Plots FGR profile for 1991 cases.
    datasets: list of (data_obj, label, linestyle). 
              data_obj should have 'Time (h)', 'Temperature (K)', 'Fission gas release (/)' all columns.
    exp_iso: numpy array (X,Y)
    exp_cyc: numpy array (X,Y)
    """
    fig, ax = plt.subplots(figsize=(11,8))
    ax_fgr = ax.twinx()

    # Process and Plot Simulation Data
    for output, label, style in datasets:
        # Time conversion
        time_h = output.get_all("Time (h)")
        temp = output.get_all("Temperature (K)")
        fgr = output.get_all("Fission gas release (/)")
        
        # Calculate time in minutes from reference
        time_min = (time_h - time_ref) * 60
        
        # Normalize FGR
        # Find index close to time_ref
        idx_ref = (np.abs(time_h - time_ref)).argmin()
        fgr_initial = fgr[idx_ref]
        fgr_pct = (fgr - fgr_initial) * 100
        
        # Plot Temp
        ax.plot(time_min, temp, linestyle=style, color="C0", label=label)
        
        # Plot FGR
        ax_fgr.plot(time_min, fgr_pct, linestyle=style, color="C2")

    # Plot Experimental Data
    if exp_iso is not None:
        ax_fgr.plot(exp_iso[:,0], exp_iso[:,1], label="ISO", color="black", linestyle=":")
    if exp_cyc is not None:
        ax_fgr.plot(exp_cyc[:,0], exp_cyc[:,1], label="Cyclic", color="black", linestyle="-")

    ax.set_xlabel("Time (min)")
    ax.set_ylabel("Temperature (K)", color='C0')
    ax.tick_params(axis='y', labelcolor='C0')
    
    ax_fgr.set_ylabel("Fission Gas Release (%)")
    ax_fgr.set_ylim([-1, 21])
    
    # Calculate xlim based on last time point of first dataset roughly
    last_time = datasets[0][0].get_all("Time (h)")[-1]
    ax.set_xlim([0, (last_time - time_ref) * 60 + 2])
    
    custom_lines = [plt.Line2D([0], [0], color='C2', linestyle='-',  label='FGR calculated (%)'),
                    plt.Line2D([0], [0], color='k', linestyle='-', label='FGR experimental (%)')]
    ax.legend(handles=custom_lines, loc='lower right', frameon=False)
    
    plt.tight_layout()
    out_path = os.path.join(outdir, outname)
    plt.savefig(out_path, dpi=150)
    plt.close()
    print(f"Saved: {out_path}")

# ==========================================
# MAIN
# ==========================================

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    regression_dir = script_dir 
    figures_dir = os.path.join(regression_dir, "figures")
    os.makedirs(figures_dir, exist_ok=True)

    # ------------------------------------------
    # KASHIBE 1990
    # ------------------------------------------
    exp_fgr = np.array([5.80/0.75, 5.80/0.75, 13.71/0.75, 22.26, 14.16, 16.86, 25.3, 25.9])
    exp_swelling = np.array([0, 6.7, 6.5, 7.4, 8.9, 9.0, 10.4, 0])
    
    sim_fgr = np.zeros(8)
    sim_swelling = np.zeros(8)
    test_dirs_1990 = glob.glob(os.path.join(regression_dir, "test_Kashibe1990_*"))
    found_1990 = False
    
    for test_dir in test_dirs_1990:
        test_name = os.path.basename(test_dir)
        try:
            output = load_output(test_dir)
            fgr_val = output.get_all("Fission gas release (/)")[-1] * 100.0
            sw_val = output.get_all("Intergranular gas swelling (/)")[-1] * 100.0
            
            if "1873K" in test_name: base = 0
            elif "2073K" in test_name: base = 4
            else: continue
                
            if "_06" in test_name: offset = 0
            elif "_16" in test_name: offset = 1
            elif "_23" in test_name: offset = 2
            elif "_28" in test_name: offset = 3
            else: continue
                
            idx = base + offset
            sim_fgr[idx] = fgr_val
            sim_swelling[idx] = sw_val
            found_1990 = True
        except: pass

    if found_1990:
        print("Plotting Kashibe 1990 results...")
        do_plot_1990(sim_fgr, sim_swelling, exp_fgr, exp_swelling, figures_dir)


    # ------------------------------------------
    # KASHIBE 1993
    # ------------------------------------------
    exp_density = np.array([9.0, 6.7]) 
    exp_radius = np.array([1.1e-9, 1.95e-9])
    sim_density = np.zeros(2)
    sim_radius = np.zeros(2)
    target_dirs_1993 = ["test_Kashibe1993_23", "test_Kashibe1993_44"]
    found_1993 = False
    
    for idx, dir_name in enumerate(target_dirs_1993):
        test_dir = os.path.join(regression_dir, dir_name)
        if not os.path.isdir(test_dir): continue
        try:
            output = load_output(test_dir)
            conc_bub_m3 = output.get_all("Intragranular bubble concentration (bub/m3)")[-1]
            sim_density[idx] = conc_bub_m3 * 1e-23
            rad_m = output.get_all("Intragranular bubble radius (m)")[-1]
            sim_radius[idx] = rad_m
            found_1993 = True
        except: pass

    if found_1993:
        print("Plotting Kashibe 1993 results...")
        do_plot_1993(sim_density, sim_radius, exp_density, exp_radius, figures_dir)
        
    # ------------------------------------------
    # KASHIBE 1991 (23 GWd/tU)
    # ------------------------------------------
    iso23 = parse_xy(ISO23_DATA)
    cyc23 = parse_xy(CYCLE23_DATA)
    time_ref_23 = 18395
    
    dir_cyc_23 = os.path.join(regression_dir, "test_Kashibe1991_1673K_23_Multiple")
    dir_iso_23 = os.path.join(regression_dir, "test_Kashibe1991_1673K_23_Single")
    
    datasets_23 = []
    if os.path.isdir(dir_cyc_23):
        try:
            datasets_23.append((load_output(dir_cyc_23), "Cyclic", "-"))
        except: pass
    if os.path.isdir(dir_iso_23):
        try:
            datasets_23.append((load_output(dir_iso_23), "Isothermal", ":"))
        except: pass
        
    if datasets_23:
        print("Plotting Kashibe 1991 (23 GWd) results...")
        do_plot_1991(datasets_23, iso23, cyc23, time_ref_23, "FGRRamp23_Kashibe1991.png", figures_dir)

    # ------------------------------------------
    # KASHIBE 1991 (28 GWd/tU)
    # ------------------------------------------
    iso28 = parse_xy(ISO28_DATA)
    cyc28 = parse_xy(CYCLE28_DATA)
    time_ref_28 = 22410
    
    dir_cyc_28 = os.path.join(regression_dir, "test_Kashibe1991_1673K_28_Multiple")
    dir_iso_28 = os.path.join(regression_dir, "test_Kashibe1991_1673K_28_Single")
    
    datasets_28 = []
    if os.path.isdir(dir_cyc_28):
        try:
            datasets_28.append((load_output(dir_cyc_28), "Cyclic", "-"))
        except: pass
    if os.path.isdir(dir_iso_28):
        try:
            datasets_28.append((load_output(dir_iso_28), "Isothermal", ":"))
        except: pass
        
    if datasets_28:
        print("Plotting Kashibe 1991 (28 GWd) results...")
        do_plot_1991(datasets_28, iso28, cyc28, time_ref_28, "FGRRamp28_Kashibe1991.png", figures_dir)


if __name__ == "__main__":
    main()
