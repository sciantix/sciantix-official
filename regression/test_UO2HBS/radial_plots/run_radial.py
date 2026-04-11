"""
Radial HBS driver.

Builds a pseudo-radial PWR pellet by running SCIANTIX at several radial
positions with:
    - parabolic temperature profile from temp_center (axis) to temp_edge (rim)
    - TUBRNP (Lassmann 1995) radial burnup profile with self-shielding
      enhancement toward the rim, giving a radially-varying fission rate
      with common irradiation duration.

Collects HBS metrics (alpha_r, porosity, pore density, pore radius) at every
radial position and produces five validation plots:

    1. radial_profiles.png        - input T(r) and burnup(r) profiles
    2. hbs_radial_results.png     - SCIANTIX radial xi(r) and alpha_r(r)
    3. plot_radial_porosity.png   - xi(r) vs Cappia 2016, Spino 2005, Barani 2022
    4. plot_radial_pore_radius.png- R_p(r) vs Spino 2006
    5. plot_radial_pore_density.png-N_p(r) vs Cappia 2016, Spino 1996, Barani 2022

The experimental datasets and literature model curves live in the same
directory (exp_*, model_*). If any file is missing the corresponding plot
is skipped with a warning.

@author G. Zullo
"""

import os
import subprocess

import matplotlib.pyplot as plt
import numpy as np

# =============================================================================
# Paths
# =============================================================================
base_dir = "/home/giovanni/sciantix-official"
sciantix_path = os.path.join(base_dir, "build/sciantix.x")
input_path = os.path.join(base_dir, "regression/test_UO2HBS")

# Ensure the script operates in its own directory regardless of where launched
os.chdir(os.path.dirname(os.path.abspath(__file__)))

# =============================================================================
# Simulation parameters
# =============================================================================
n_points_radial = 30
r_out = 0.00411      # m (pellet radius)
bu_avg = 40.0        # MWd/kgHM (volumetric-average burnup, see f_avg_volumetric
                     # below; low enough that the center is below the HBS burnup
                     # threshold so the KJMA cut-off is actually exercised)
fission_rate = 1.0e19  # fissions/m3s (volumetric average, rescaled radially via TUBRNP)
temp_center = 1400.0   # K (above 1273.15 K Heaviside for bu_eff, so center
                       # cannot form HBS)
temp_edge = 723.0      # K

# Hydrostatic stress (MPa), negative = compressive (PCMI convention)
sigma_h = -20.0

# Radial grid clustering toward the rim.
#   rim_clustering = 1.0  -> uniform grid (backward compatible)
#   rim_clustering = 2.0  -> mild quadratic clustering near r/r0 = 1
#   rim_clustering = 3.0  -> moderate cubic clustering (default)
#   rim_clustering >= 4.0 -> aggressive clustering, very few points below 0.5
# Sampling points are generated as r_r0 = 1 - (1 - x)^rim_clustering with x
# uniform in [0, 1]; higher exponents concentrate more points in the thin
# HBS-active layer near the pellet rim.
rim_clustering = 3.0

# =============================================================================
# Conversion factors
# =============================================================================
fission_energy_ev_per_fiss = 200.0e6
ev_to_j = 1.602e-19
fission_energy_j_per_fiss = fission_energy_ev_per_fiss * ev_to_j
mwd_to_j = 8.64e10
fuel_density = 10970.0  # kg/m3

# =============================================================================
# Plot style
# =============================================================================
plt.rcParams.update({
    "figure.figsize": (8.0, 5.5),
    "axes.labelsize": 11,
    "axes.titlesize": 12,
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "legend.fontsize": 9,
    "legend.frameon": True,
    "axes.grid": True,
    "grid.linestyle": "--",
    "grid.linewidth": 0.5,
    "grid.alpha": 0.6,
})

COLOR_CURRENT = "#2ca02c"   # SCIANTIX 2.2.1 (green)
COLOR_BARANI = "#8b4513"    # sienna
COLOR_CAPPIA = "#555555"    # gray
COLOR_SPINO_R = "#c44545"   # red
COLOR_SPINO_B = "#1f6aa3"   # blue
COLOR_SPINO_P = "#d62a7a"   # pink
COLOR_ALPHA = "#f39c12"     # orange (restructured fraction twin axis)
COLOR_T = "#c0392b"         # red (temperature)
COLOR_BU = "#1f6aa3"        # blue (burnup)

MARKER_SIZE = 5
STAR_SIZE = 12
LW_MODEL = 2.0
LW_REF = 1.5

DPI = 150


def _save(fig, filename):
    fig.tight_layout()
    fig.savefig(filename, dpi=DPI, bbox_inches="tight")
    plt.close(fig)
    print(f"  -> {filename}")


# =============================================================================
# Radial grid, TUBRNP burnup profile and radial fission rate (self-shielding)
# =============================================================================
# Non-uniform sampling clustered toward r/r0 = 1 via power law.
# rim_clustering = 1 recovers the plain uniform grid.
x_uniform = np.linspace(0.0, 1.0, n_points_radial)
r_r0 = 1.0 - (1.0 - x_uniform) ** rim_clustering
radii = r_r0 * r_out

# TUBRNP burnup shape factor (Lassmann et al. JNM 226 (1995) 1-8), mm units.
p1, p2, p3 = 3.45, 3.0, 0.45
rout_mm = r_out * 1000.0
radii_mm = radii * 1000.0
f_r = 1.0 + p1 * np.exp(-p2 * (rout_mm - radii_mm) ** p3)

# Volumetric mean of the TUBRNP profile over the cylindrical cross-section.
# Computed on a fine uniform grid independent of the sampling grid, so that
# bu_avg keeps its volumetric interpretation for any rim_clustering or
# n_points_radial. Cylindrical element of area = 2*pi*r*dr, hence
#   <f>_vol = (2/R^2) * int_0^R f(r) * r dr .
_r_int = np.linspace(0.0, r_out, 1000)
_f_int = 1.0 + p1 * np.exp(-p2 * ((r_out - _r_int) * 1000.0) ** p3)
f_avg_volumetric = 2.0 * np.trapz(_f_int * _r_int, _r_int) / (r_out ** 2)

burnups = bu_avg * (f_r / f_avg_volumetric)

# Parabolic temperature profile
temp = temp_center - (temp_center - temp_edge) * (r_r0 ** 2)

# Radial F-dot reproduces the TUBRNP enhancement at the rim
fission_rate_radial = fission_rate * (f_r / f_avg_volumetric)

# Common irradiation duration for every point
t_end_global_s = (bu_avg * mwd_to_j * fuel_density) / (fission_rate * fission_energy_j_per_fiss)
t_end_global_h = t_end_global_s / 3600.0
print(f"Common irradiation duration: {t_end_global_h:.1f} h ({t_end_global_s/86400/365.25:.2f} yr)")

# =============================================================================
# Plot 1: input profiles (T(r), bu(r))
# =============================================================================
fig, ax1 = plt.subplots()
ax1.plot(r_r0, temp, "-", color=COLOR_T, linewidth=LW_MODEL, label="Temperature")
ax1.axhline(1273.15, color=COLOR_T, linestyle=":", linewidth=1,
            alpha=0.6, label="HBS threshold (1273 K)")
ax1.set_xlabel(r"r/r$_0$")
ax1.set_ylabel("Temperature (K)", color=COLOR_T)
ax1.tick_params(axis="y", labelcolor=COLOR_T)

ax2 = ax1.twinx()
ax2.plot(r_r0, burnups, "-", color=COLOR_BU, linewidth=LW_MODEL, label="Local burnup")
ax2.set_ylabel(r"Local burnup (MWd kgHM$^{-1}$)", color=COLOR_BU)
ax2.tick_params(axis="y", labelcolor=COLOR_BU)
ax2.grid(False)

ax1.set_title(
    rf"Radial input profiles (TUBRNP, Bu$_{{\mathrm{{AV}}}}$ = {bu_avg:.0f} MWd kgHM$^{{-1}}$)"
)
lines1, labels1 = ax1.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax1.legend(lines1 + lines2, labels1 + labels2, loc="center left")
_save(fig, "radial_profiles.png")

# =============================================================================
# Run SCIANTIX at each radial point
# =============================================================================
results = []
print(f"Starting radial simulation ({n_points_radial} points)...")

for i in range(n_points_radial):
    t_end_h = t_end_global_h
    F_i = fission_rate_radial[i]
    T_i = temp[i]

    work_dir = f"point_{i}"
    os.makedirs(work_dir, exist_ok=True)

    # Copy input files
    for f_name in ("input_settings.txt",
                   "input_scaling_factors.txt",
                   "input_initial_conditions.txt"):
        subprocess.run(["cp", os.path.join(input_path, f_name), work_dir])

    # Write input_history.txt with intermediate time nodes to guide the
    # adaptive integrator.
    # Columns: time(h) temperature(K) fission_rate(fiss/m3s) sigma_h(MPa)
    with open(os.path.join(work_dir, "input_history.txt"), "w") as f:
        f.write(f"0.0 {T_i} {F_i} {sigma_h}\n")
        f.write(f"{0.25 * t_end_h} {T_i} {F_i} {sigma_h}\n")
        f.write(f"{0.50 * t_end_h} {T_i} {F_i} {sigma_h}\n")
        f.write(f"{0.75 * t_end_h} {T_i} {F_i} {sigma_h}\n")
        f.write(f"{t_end_h} {T_i} {F_i} {sigma_h}")

    subprocess.run(
        [os.path.abspath(sciantix_path), "./"],
        cwd=work_dir,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )

    # Parse output.txt via header matching
    output_file = os.path.join(work_dir, "output.txt")
    if not os.path.exists(output_file):
        print(f"  point {i}: no output file")
        continue

    with open(output_file, "r") as f:
        lines = f.readlines()
    if len(lines) <= 1:
        print(f"  point {i}: empty output")
        continue

    header = lines[0].strip().split("\t")
    last_line = lines[-1].strip().split("\t")

    try:
        idx_eff_bu = next(k for k, h in enumerate(header) if "Effective burnup" in h)
        idx_restr = next(k for k, h in enumerate(header) if "Restructured volume fraction" in h)
        idx_por = next(k for k, h in enumerate(header) if "HBS porosity" in h)
        idx_np = next(k for k, h in enumerate(header) if "HBS pore density" in h)
        idx_rp = next(k for k, h in enumerate(header) if "HBS pore radius" in h)
        eff_bu = float(last_line[idx_eff_bu])
        restr = float(last_line[idx_restr])
        porosity = float(last_line[idx_por])
        Np_val = float(last_line[idx_np])
        Rp_val = float(last_line[idx_rp])
    except (StopIteration, ValueError, IndexError) as e:
        print(f"  point {i}: parse error: {e}")
        continue

    results.append([r_r0[i], burnups[i], eff_bu, restr, porosity, Np_val, Rp_val])
    print(
        f"  r/r0={r_r0[i]:.3f} | Bu={burnups[i]:6.1f} | T={temp[i]:4.0f} K"
        f" | Alpha={restr:.3f} | xi={porosity:.4f}"
        f" | Np={Np_val:.2e} | Rp={Rp_val:.2e}"
    )

if not results:
    print("No results collected. Aborting plots.")
    raise SystemExit(1)

res_arr = np.array(results)
r_sim = res_arr[:, 0]
alpha_sim = res_arr[:, 3]
porosity_sim = res_arr[:, 4]
Np_sim = res_arr[:, 5]
Rp_sim = res_arr[:, 6]

# =============================================================================
# Save data files
# =============================================================================
# Full debug dump with header (all 7 columns)
with open(f"model_this_work_{int(bu_avg)}_full.txt", "w") as f:
    f.write("# r_r0 bu_local bu_eff alpha_r porosity pore_density pore_radius\n")
    for res in results:
        f.write(
            f"{res[0]:.6f} {res[1]:.3f} {res[2]:.3f} {res[3]:.6f}"
            f" {res[4]:.6e} {res[5]:.6e} {res[6]:.6e}\n"
        )

# 2-column files (format expected by Radial plot.py and comparison plots)
with open("model_this_work.txt", "w") as f:
    for res in results:
        f.write(f"{res[0]:.6f} {res[4]:.6e}\n")
with open("model_this_work_Np.txt", "w") as f:
    for res in results:
        f.write(f"{res[0]:.6f} {res[5]:.6e}\n")
with open("model_this_work_radius.txt", "w") as f:
    for res in results:
        f.write(f"{res[0]:.6f} {res[6]:.6e}\n")

# =============================================================================
# Plot 2: HBS radial evolution (SCIANTIX only, full pellet)
# =============================================================================
fig, ax1 = plt.subplots()
ax1.plot(r_sim, porosity_sim * 100, "-o",
         color=COLOR_CURRENT, linewidth=LW_MODEL, markersize=5,
         label="Porosity")
ax1.set_xlabel(r"r/r$_0$")
ax1.set_ylabel("Porosity (%)", color=COLOR_CURRENT)
ax1.tick_params(axis="y", labelcolor=COLOR_CURRENT)
ax1.set_title(
    rf"Radial HBS evolution (SCIANTIX 2.2.1, Bu$_{{\mathrm{{AV}}}}$ = {bu_avg:.0f} MWd kgHM$^{{-1}}$)"
)

ax2 = ax1.twinx()
ax2.plot(r_sim, alpha_sim, "--",
         color=COLOR_ALPHA, linewidth=LW_MODEL,
         label="Restructured fraction")
ax2.set_ylabel("Restructured volume fraction (/)", color=COLOR_ALPHA)
ax2.tick_params(axis="y", labelcolor=COLOR_ALPHA)
ax2.set_ylim(0.0, 1.05)
ax2.grid(False)

lines1, labels1 = ax1.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax1.legend(lines1 + lines2, labels1 + labels2, loc="upper left")
_save(fig, "hbs_radial_results.png")

# =============================================================================
# Plot 3: Porosity vs r/r0 - experimental + literature comparison
# =============================================================================
try:
    exp_p_cappia = np.genfromtxt("exp_porosity_Cappia.txt")
    exp_p_spino_67 = np.genfromtxt("exp_porosity_67.txt")
    exp_p_spino_97 = np.genfromtxt("exp_porosity_97.txt")
    model_barani = np.genfromtxt("model_Barani.txt")
except OSError as e:
    print(f"  skip plot_radial_porosity.png: {e}")
else:
    fig, ax = plt.subplots()
    ax.plot(exp_p_cappia[:, 0], exp_p_cappia[:, 1],
            "o", color=COLOR_CAPPIA, markersize=6,
            label="Cappia (2016)")
    ax.plot(exp_p_spino_67[:, 0], exp_p_spino_67[:, 1],
            "D", color=COLOR_SPINO_P, markersize=MARKER_SIZE,
            label=r"Spino (2005), Bu$_{\mathrm{AV}}=67$ MWd/kgU")
    ax.plot(exp_p_spino_97[:, 0], exp_p_spino_97[:, 1],
            "D", color=COLOR_SPINO_B, markersize=MARKER_SIZE,
            label=r"Spino (2005), Bu$_{\mathrm{AV}}=97$ MWd/kgU")
    ax.plot(model_barani[:, 0], model_barani[:, 1],
            "--*", color=COLOR_BARANI, linewidth=LW_REF, markersize=STAR_SIZE,
            label="Barani (2022)")
    ax.plot(r_sim, porosity_sim,
            "-*", color=COLOR_CURRENT, linewidth=LW_MODEL, markersize=STAR_SIZE,
            label=rf"This work, Bu$_{{\mathrm{{AV}}}}={int(bu_avg)}$ MWd/kgHM")
    ax.set_xlabel(r"Relative radius r/r$_0$ (/)")
    ax.set_ylabel("HBS porosity (/)")
    ax.set_xlim(0.8, 1.005)
    ax.set_ylim(0.0, 0.22)
    ax.legend(loc="upper left")
    _save(fig, "plot_radial_porosity.png")

# =============================================================================
# Plot 4: Pore radius vs r/r0 - Spino comparison
# =============================================================================
try:
    exp_r_80 = np.genfromtxt("exp_radius_80.txt")
    exp_r_98 = np.genfromtxt("exp_radius_98.txt")
except OSError as e:
    print(f"  skip plot_radial_pore_radius.png: {e}")
else:
    fig, ax = plt.subplots()
    ax.plot(exp_r_80[:, 0], exp_r_80[:, 1],
            "o", color=COLOR_SPINO_R, markersize=MARKER_SIZE,
            label=r"Spino (2006), Bu$_{\mathrm{AV}}=80$ MWd/kgU")
    ax.plot(exp_r_98[:, 0], exp_r_98[:, 1],
            "o", color=COLOR_SPINO_B, markersize=MARKER_SIZE,
            label=r"Spino (2006), Bu$_{\mathrm{AV}}=98$ MWd/kgU")
    ax.plot(r_sim, Rp_sim,
            "-*", color=COLOR_CURRENT, linewidth=LW_MODEL, markersize=STAR_SIZE,
            label=rf"This work, Bu$_{{\mathrm{{AV}}}}={int(bu_avg)}$ MWd/kgHM")
    ax.set_xlabel(r"Relative radius r/r$_0$ (/)")
    ax.set_ylabel("Average pore radius (m)")
    ax.set_xlim(0.8, 1.005)
    ax.set_ylim(0.0, 1.4e-6)
    ax.legend(loc="upper left")
    _save(fig, "plot_radial_pore_radius.png")

# =============================================================================
# Plot 5: Pore number density vs r/r0 - Cappia/Spino/Barani comparison
# =============================================================================
try:
    exp_d_cappia = np.genfromtxt("exp_density_Cappia.txt")
    exp_d_40 = np.genfromtxt("exp_density_40.txt")
    exp_d_57 = np.genfromtxt("exp_density_57.txt")
    exp_d_67 = np.genfromtxt("exp_density_67.txt")
    model_barani_np = np.genfromtxt("model_Barani_Np.txt")
except OSError as e:
    print(f"  skip plot_radial_pore_density.png: {e}")
else:
    fig, ax = plt.subplots()
    ax.plot(exp_d_cappia[:, 0], exp_d_cappia[:, 1],
            "o", color=COLOR_CAPPIA, markersize=MARKER_SIZE,
            label="Cappia (2016)")
    ax.plot(exp_d_40[:, 0], exp_d_40[:, 1],
            "o", color=COLOR_SPINO_R, markersize=MARKER_SIZE,
            label=r"Spino (1996), Bu$_{\mathrm{AV}}=40$ MWd/kgU")
    ax.plot(exp_d_57[:, 0], exp_d_57[:, 1],
            "o", color=COLOR_SPINO_B, markersize=MARKER_SIZE,
            label=r"Spino (1996), Bu$_{\mathrm{AV}}=57$ MWd/kgU")
    ax.plot(exp_d_67[:, 0], exp_d_67[:, 1],
            "o", color=COLOR_SPINO_P, markersize=MARKER_SIZE,
            label=r"Spino (1996), Bu$_{\mathrm{AV}}=67$ MWd/kgU")
    ax.plot(model_barani_np[:, 0], model_barani_np[:, 1],
            "--*", color=COLOR_BARANI, linewidth=LW_REF, markersize=STAR_SIZE,
            label="Barani (2022)")
    ax.plot(r_sim, Np_sim,
            "-*", color=COLOR_CURRENT, linewidth=LW_MODEL, markersize=STAR_SIZE,
            label=rf"This work, Bu$_{{\mathrm{{AV}}}}={int(bu_avg)}$ MWd/kgHM")
    ax.set_xlabel(r"Relative radius r/r$_0$ (/)")
    ax.set_ylabel(r"Pore number density (pores m$^{-3}$)")
    ax.set_xlim(0.9, 1.005)
    ax.set_ylim(0.0, 6e17)
    ax.legend(loc="upper left")
    _save(fig, "plot_radial_pore_density.png")

print("\nAll radial plots generated.")
