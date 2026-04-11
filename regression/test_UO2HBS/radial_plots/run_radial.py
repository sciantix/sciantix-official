import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt

# Paths
base_dir = "/home/giovanni/sciantix-official"
sciantix_path = os.path.join(base_dir, "build/sciantix.x")
input_path = os.path.join(base_dir, "regression/test_UO2HBS")

# Simulation parameters
n_points_radial = 10
r_out = 0.00411  # m (radius of the pellet)
bu_avg = 40.0    # MWd/kgHM (average burnup) -- low enough that the center stays
                 # below the HBS burnup threshold (~50 MWd/kgHM), so the KJMA
                 # cut-off is actually exercised in the test.
fission_rate = 1.0e19  # fissions/m3s (typical)
temp_center = 1400.0   # K -- above the 1273.15 K effective-burnup threshold,
                       # so the center cannot form HBS (Heaviside kills bu_eff).
temp_edge = 723.0      # K

# Conversion factors
fission_energy_ev_per_fiss = 200.0e6
ev_to_j = 1.602e-19
fission_energy_j_per_fiss = fission_energy_ev_per_fiss * ev_to_j
mwd_to_j = 8.64e10
fuel_density = 10970.0  # kg/m3

# Radial grid
r_r0 = np.linspace(0.0, 1.0, n_points_radial)
radii = r_r0 * r_out

# Model for burnup profile (Lassmann et al., JNM 226 (1995) 1-8)
# f(r) = 1 + p1 * exp(-p2 * (rout_mm - r_mm)**p3), parameters calibrated here in mm
p1 = 3.45
p2 = 3.0
p3 = 0.45

rout_mm = r_out * 1000.0
radii_mm = radii * 1000.0
f_r = 1.0 + p1 * np.exp(-p2 * (rout_mm - radii_mm)**p3)
f_avg = np.mean(f_r)
burnups = bu_avg * (f_r / f_avg)

# Temperature profile (parabolic)
temp = temp_center - (temp_center - temp_edge) * (r_r0**2)

# Radial fission rate profile (self-shielding).
# With fission_rate_radial(r) ~ F_avg * f_r(r)/f_avg and a common irradiation
# duration, the resulting local burnup automatically reproduces burnups[i].
fission_rate_radial = fission_rate * (f_r / f_avg)

# Common irradiation duration to reach the target average burnup.
t_end_global_s = (bu_avg * mwd_to_j * fuel_density) / (fission_rate * fission_energy_j_per_fiss)
t_end_global_h = t_end_global_s / 3600.0
print(f"Common irradiation duration: {t_end_global_h:.1f} h ({t_end_global_s/86400/365.25:.2f} yr)")

# Hydrostatic stress (MPa), negative = compressive (PCMI at pellet-cladding interface)
sigma_h = -20.0

# Plot input profiles
fig, ax1 = plt.subplots(figsize=(8, 5))
ax2 = ax1.twinx()
ax1.plot(r_r0, temp, 'r-', label='Temperature (K)')
ax2.plot(r_r0, burnups, 'b-', label='Local Burnup (MWd/kgHM)')
ax1.set_xlabel('r/r0')
ax1.set_ylabel('Temperature (K)', color='r')
ax2.set_ylabel('Burnup (MWd/kgHM)', color='b')
plt.title('Radial Input Profiles')
plt.savefig('radial_profiles.png')
print("Profile plot saved as radial_profiles.png")

results = []

print(f"Starting radial simulation ({n_points_radial} points)...")

for i in range(n_points_radial):
    # Common irradiation duration for every point; the burnup variation across
    # radius comes from the local fission rate fission_rate_radial[i].
    t_end_h = t_end_global_h
    F_i = fission_rate_radial[i]
    T_i = temp[i]

    # Create a fresh directory for each point
    work_dir = f"point_{i}"
    os.makedirs(work_dir, exist_ok=True)

    # Copy input files
    for f_name in ["input_settings.txt", "input_scaling_factors.txt", "input_initial_conditions.txt"]:
        subprocess.run(["cp", os.path.join(input_path, f_name), work_dir])

    # Create input_history.txt with intermediate time points to guide the solver
    # Columns: time(h) temperature(K) fission_rate(fiss/m3s) hydrostatic_stress(MPa)
    with open(os.path.join(work_dir, "input_history.txt"), "w") as f:
        f.write(f"0.0 {T_i} {F_i} {sigma_h}\n")
        f.write(f"{0.25 * t_end_h} {T_i} {F_i} {sigma_h}\n")
        f.write(f"{0.50 * t_end_h} {T_i} {F_i} {sigma_h}\n")
        f.write(f"{0.75 * t_end_h} {T_i} {F_i} {sigma_h}\n")
        f.write(f"{t_end_h} {T_i} {F_i} {sigma_h}")

    # Run SCIANTIX
    subprocess.run([os.path.abspath(sciantix_path), "./"], cwd=work_dir, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    
    # Parse output.txt
    output_file = os.path.join(work_dir, "output.txt")
    if os.path.exists(output_file):
        with open(output_file, "r") as f:
            lines = f.readlines()
        if len(lines) > 1:
            header = lines[0].strip().split('\t')
            last_line = lines[-1].strip().split('\t')
            try:
                idx_eff_bu = next(k for k, h in enumerate(header) if "Effective burnup" in h)
                idx_restr = next(k for k, h in enumerate(header) if "Restructured volume fraction" in h)
                idx_por = next(k for k, h in enumerate(header) if "HBS porosity" in h)
                eff_bu = float(last_line[idx_eff_bu])
                restr = float(last_line[idx_restr])
                porosity = float(last_line[idx_por])
            except (StopIteration, ValueError, IndexError) as e:
                print(f"  Warning: failed to parse output for point {i}: {e}")
                continue

            results.append([r_r0[i], burnups[i], eff_bu, restr, porosity])
            print(f" r/r0={r_r0[i]:.3f} | Bu={burnups[i]:.1f} | T={temp[i]:.0f} K | F={F_i:.2e} | EffBu={eff_bu:.1f} | Alpha={restr:.3f} | Porosity={porosity:.4f}")

# Save results to file
with open("model_this_work.txt", "w") as f:
    for res in results:
        f.write(f"{res[0]} {res[1]} {res[2]} {res[3]} {res[4]}\n")

# Final radial plot
if results:
    res_arr = np.array(results)
    plt.figure(figsize=(8, 6))
    plt.plot(res_arr[:, 0], res_arr[:, 4] * 100, 'go-', label='Porosity (%)')
    plt.plot(res_arr[:, 0], res_arr[:, 3], 'm--', label='Restructured Fraction')
    plt.xlabel('r/r0')
    plt.ylabel('Value')
    plt.title('Radial HBS Evolution')
    plt.legend()
    plt.grid(True)
    plt.savefig('hbs_radial_results.png')
    print("Results plot saved as hbs_radial_results.png")
