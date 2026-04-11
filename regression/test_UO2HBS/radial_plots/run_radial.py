"""
Radial HBS driver with multi-burnup sweep.

Builds a pseudo-radial PWR pellet by running SCIANTIX at several radial
positions for each target volumetric burnup in ``bu_avg_list``. The
fission rate is kept constant across runs; the irradiation duration
is scaled so that the volumetric mean burnup at the end of each run
matches the target.

For every ``bu_avg`` in the list the script:
    - builds the same TUBRNP (Lassmann 1995) radial shape factor f(r)
    - uses a fission rate profile fission_rate * f(r) / <f>_vol, so that
      the volumetric mean fission rate equals ``fission_rate``
    - runs SCIANTIX at every radial position with a common duration
      t_end = bu_avg / (volumetric mean F-dot) / (energy per fission)

Output (one combined set for the full sweep):
    1. radial_profiles.png         - T(r) and local burnup profiles (overlay)
    2. hbs_radial_results.png      - porosity(r) and alpha_r(r) (overlay)
    3. plot_radial_porosity.png    - xi(r) vs Cappia/Spino/Barani
    4. plot_radial_pore_radius.png - R_p(r) vs Spino
    5. plot_radial_pore_density.png- N_p(r) vs Cappia/Spino/Barani

Data files produced for each target burnup bu_avg:
    model_this_work_{bu}_full.txt  (r, bu_loc, bu_eff, alpha_r, xi, Np, Rp)
    model_this_work_{bu}.txt       (r, xi)
    model_this_work_Np_{bu}.txt    (r, Np)
    model_this_work_radius_{bu}.txt(r, Rp)

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

# Volumetric-average burnup sweep. The script runs the full radial
# simulation once for each value in this list, keeping the fission rate
# constant and scaling the irradiation duration so that the volumetric
# mean local burnup at the end of each run matches the target.
# Default: 40, 67, 97 MWd/kgHM — matches Spino 2005 porosity datasets.
bu_avg_list = [40.0, 67.0, 97.0]   # MWd/kgHM

fission_rate = 1.0e19  # fissions/m3s (volumetric mean, kept constant)
temp_center = 1400.0   # K (above 1273.15 K Heaviside for bu_eff, so center
                       # cannot form HBS)
temp_edge = 723.0      # K

# Hydrostatic stress (MPa), negative = compressive (PCMI convention)
sigma_h = -20.0

# Radial grid clustering toward the rim.
#   rim_clustering = 1.0  -> uniform grid (backward compatible)
#   rim_clustering = 2.0  -> mild quadratic clustering
#   rim_clustering = 3.0  -> moderate cubic clustering (default)
#   rim_clustering >= 4.0 -> aggressive clustering
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

COLOR_BARANI = "#8b4513"   # sienna
COLOR_CAPPIA = "#555555"
COLOR_SPINO_R = "#c44545"
COLOR_SPINO_B = "#1f6aa3"
COLOR_SPINO_P = "#d62a7a"
COLOR_ALPHA = "#f39c12"
COLOR_T = "#c0392b"

MARKER_SIZE = 5
STAR_SIZE = 11
LW_MODEL = 2.0
LW_REF = 1.5

DPI = 150

# Discrete colormap for the "This work" curves across bu_avg_list.
# viridis-like sequential palette: darker = lower burnup, brighter = higher.
_cmap = plt.get_cmap("viridis")
if len(bu_avg_list) == 1:
    this_work_colors = [_cmap(0.45)]
else:
    this_work_colors = [_cmap(0.15 + 0.70 * i / (len(bu_avg_list) - 1))
                        for i in range(len(bu_avg_list))]


def _save(fig, filename):
    fig.tight_layout()
    fig.savefig(filename, dpi=DPI, bbox_inches="tight")
    plt.close(fig)
    print(f"  -> {filename}")


def _plot_exp_with_connector(ax_, data, color, label, marker="o"):
    """Markers + thin dashed connector (sorted by r/r0)."""
    order = np.argsort(data[:, 0])
    xs = data[order, 0]
    ys = data[order, 1]
    ax_.plot(xs, ys, "--", color=color, linewidth=1.0, alpha=0.5)
    ax_.plot(xs, ys, marker, color=color, markersize=MARKER_SIZE, label=label)


# =============================================================================
# Radial grid and TUBRNP shape factor (shared across all bu_avg runs)
# =============================================================================
x_uniform = np.linspace(0.0, 1.0, n_points_radial)
r_r0 = 1.0 - (1.0 - x_uniform) ** rim_clustering
radii = r_r0 * r_out

# Lassmann et al. JNM 226 (1995) 1-8, parameters in mm units.
p1, p2, p3 = 3.45, 3.0, 0.45
rout_mm = r_out * 1000.0
radii_mm = radii * 1000.0
f_r = 1.0 + p1 * np.exp(-p2 * (rout_mm - radii_mm) ** p3)

# Volumetric mean of the TUBRNP profile over the cylindrical cross-section,
# computed on a fine uniform grid independent of the sampling grid.
#   <f>_vol = (2/R^2) * int_0^R f(r) * r dr .
_r_int = np.linspace(0.0, r_out, 1000)
_f_int = 1.0 + p1 * np.exp(-p2 * ((r_out - _r_int) * 1000.0) ** p3)
f_avg_volumetric = 2.0 * np.trapz(_f_int * _r_int, _r_int) / (r_out ** 2)
print(f"Volumetric mean of TUBRNP shape factor: f_avg_vol = {f_avg_volumetric:.4f}")

# Radial F-dot profile (same for every bu_avg run, its volumetric mean
# equals the scalar fission_rate by construction)
fission_rate_radial = fission_rate * (f_r / f_avg_volumetric)

# Parabolic temperature profile (same across runs; depends only on pellet geometry)
temp = temp_center - (temp_center - temp_edge) * (r_r0 ** 2)

# =============================================================================
# Run SCIANTIX at every radial point for every target bu_avg
# =============================================================================
# all_results[k] is an array (N_points, 7) for bu_avg_list[k] with columns:
#   [r_r0, bu_local, bu_eff, alpha_r, porosity, Np, Rp]
all_results = []

for bu_avg in bu_avg_list:
    bu_int = int(round(bu_avg))
    print(f"\n=== Sweep point bu_avg = {bu_avg:.1f} MWd/kgHM ===")

    # Local burnup target at every radial point (same TUBRNP shape, scaled)
    burnups_local = bu_avg * (f_r / f_avg_volumetric)

    # Common duration: same fission rate, scale t_end with bu_avg
    t_end_s = (bu_avg * mwd_to_j * fuel_density) / (fission_rate * fission_energy_j_per_fiss)
    t_end_h = t_end_s / 3600.0
    print(f"Duration: {t_end_h:.1f} h ({t_end_s/86400/365.25:.2f} yr)")

    run_dir = f"run_bu{bu_int}"
    os.makedirs(run_dir, exist_ok=True)

    results_this_bu = []

    for i in range(n_points_radial):
        F_i = fission_rate_radial[i]
        T_i = temp[i]

        work_dir = os.path.join(run_dir, f"point_{i}")
        os.makedirs(work_dir, exist_ok=True)

        for f_name in ("input_settings.txt",
                       "input_scaling_factors.txt",
                       "input_initial_conditions.txt"):
            subprocess.run(["cp", os.path.join(input_path, f_name), work_dir])

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

        results_this_bu.append(
            [r_r0[i], burnups_local[i], eff_bu, restr, porosity, Np_val, Rp_val]
        )

    if not results_this_bu:
        print(f"  WARNING: no valid results for bu_avg={bu_avg}")
        all_results.append(None)
        continue

    arr = np.array(results_this_bu)
    all_results.append(arr)

    # Save per-bu data files
    with open(f"model_this_work_{bu_int}_full.txt", "w") as f:
        f.write("# r_r0 bu_local bu_eff alpha_r porosity pore_density pore_radius\n")
        for row in arr:
            f.write(
                f"{row[0]:.6f} {row[1]:.3f} {row[2]:.3f} {row[3]:.6f}"
                f" {row[4]:.6e} {row[5]:.6e} {row[6]:.6e}\n"
            )
    with open(f"model_this_work_{bu_int}.txt", "w") as f:
        for row in arr:
            f.write(f"{row[0]:.6f} {row[4]:.6e}\n")
    with open(f"model_this_work_Np_{bu_int}.txt", "w") as f:
        for row in arr:
            f.write(f"{row[0]:.6f} {row[5]:.6e}\n")
    with open(f"model_this_work_radius_{bu_int}.txt", "w") as f:
        for row in arr:
            f.write(f"{row[0]:.6f} {row[6]:.6e}\n")

    n_ok = len(results_this_bu)
    print(f"  {n_ok}/{n_points_radial} points OK, saved data files with suffix _{bu_int}")

# Filter out failed runs (None entries)
valid_runs = [(bu, res, col) for bu, res, col
              in zip(bu_avg_list, all_results, this_work_colors)
              if res is not None]

if not valid_runs:
    print("No successful runs. Aborting plots.")
    raise SystemExit(1)

# =============================================================================
# Plot 1: input profiles (T(r) shared, local burnup overlay per bu_avg)
# =============================================================================
fig, ax1 = plt.subplots()
ax1.plot(r_r0, temp, "-", color=COLOR_T, linewidth=LW_MODEL, label="Temperature")
ax1.axhline(1273.15, color=COLOR_T, linestyle=":", linewidth=1,
            alpha=0.6, label="HBS threshold (1273 K)")
ax1.set_xlabel(r"r/r$_0$")
ax1.set_ylabel("Temperature (K)", color=COLOR_T)
ax1.tick_params(axis="y", labelcolor=COLOR_T)

ax2 = ax1.twinx()
for bu_avg, arr, col in valid_runs:
    bu_local = bu_avg * (f_r / f_avg_volumetric)
    ax2.plot(r_r0, bu_local, "-", color=col, linewidth=LW_MODEL,
             label=rf"Bu$_{{\mathrm{{AV}}}}={int(round(bu_avg))}$ MWd/kgHM")
ax2.set_ylabel(r"Local burnup (MWd kgHM$^{-1}$)")
ax2.grid(False)

ax1.set_title("Radial input profiles (TUBRNP)")
lines1, labels1 = ax1.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax1.legend(lines1 + lines2, labels1 + labels2, loc="center left")
_save(fig, "radial_profiles.png")

# =============================================================================
# Plot 2: HBS radial evolution (overlay of xi(r) and alpha_r(r) per bu_avg)
# =============================================================================
fig, ax1 = plt.subplots()
for bu_avg, arr, col in valid_runs:
    ax1.plot(arr[:, 0], arr[:, 4] * 100, "-o",
             color=col, linewidth=LW_MODEL, markersize=5,
             label=rf"$\xi$, Bu$_{{\mathrm{{AV}}}}={int(round(bu_avg))}$ MWd/kgHM")
ax1.set_xlabel(r"r/r$_0$")
ax1.set_ylabel("Porosity (%)")
ax1.set_title("Radial HBS evolution (SCIANTIX 2.2.1)")

ax2 = ax1.twinx()
for bu_avg, arr, col in valid_runs:
    ax2.plot(arr[:, 0], arr[:, 3], "--",
             color=col, linewidth=LW_REF, alpha=0.75,
             label=rf"$\alpha_r$, Bu$_{{\mathrm{{AV}}}}={int(round(bu_avg))}$")
ax2.set_ylabel(r"Restructured volume fraction $\alpha_r$ (/)")
ax2.set_ylim(0.0, 1.05)
ax2.grid(False)

lines1, labels1 = ax1.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax1.legend(lines1 + lines2, labels1 + labels2, loc="upper left", fontsize=8)
_save(fig, "hbs_radial_results.png")

# =============================================================================
# Plot 3: Porosity vs r/r0 (experimental + literature + this-work overlay)
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
    _plot_exp_with_connector(ax, exp_p_cappia, COLOR_CAPPIA, "Cappia (2016)")
    _plot_exp_with_connector(ax, exp_p_spino_67, COLOR_SPINO_P,
                             r"Spino (2005), Bu$_{\mathrm{AV}}=67$ MWd/kgU",
                             marker="D")
    _plot_exp_with_connector(ax, exp_p_spino_97, COLOR_SPINO_B,
                             r"Spino (2005), Bu$_{\mathrm{AV}}=97$ MWd/kgU",
                             marker="D")
    ax.plot(model_barani[:, 0], model_barani[:, 1],
            "--*", color=COLOR_BARANI, linewidth=LW_REF, markersize=STAR_SIZE,
            label="Barani (2022)")
    for bu_avg, arr, col in valid_runs:
        ax.plot(arr[:, 0], arr[:, 4],
                "-*", color=col, linewidth=LW_MODEL, markersize=STAR_SIZE,
                label=rf"This work, Bu$_{{\mathrm{{AV}}}}={int(round(bu_avg))}$ MWd/kgHM")
    ax.set_xlabel(r"Relative radius r/r$_0$ (/)")
    ax.set_ylabel("HBS porosity (/)")
    ax.set_xlim(0.8, 1.005)
    ax.set_ylim(0.0, 0.22)
    ax.legend(loc="upper left", fontsize=8)
    _save(fig, "plot_radial_porosity.png")

# =============================================================================
# Plot 4: Pore radius vs r/r0 (experimental + this-work overlay)
# =============================================================================
try:
    exp_r_80 = np.genfromtxt("exp_radius_80.txt")
    exp_r_98 = np.genfromtxt("exp_radius_98.txt")
except OSError as e:
    print(f"  skip plot_radial_pore_radius.png: {e}")
else:
    fig, ax = plt.subplots()
    _plot_exp_with_connector(ax, exp_r_80, COLOR_SPINO_R,
                             r"Spino (2006), Bu$_{\mathrm{AV}}=80$ MWd/kgU")
    _plot_exp_with_connector(ax, exp_r_98, COLOR_SPINO_B,
                             r"Spino (2006), Bu$_{\mathrm{AV}}=98$ MWd/kgU")
    for bu_avg, arr, col in valid_runs:
        ax.plot(arr[:, 0], arr[:, 6],
                "-*", color=col, linewidth=LW_MODEL, markersize=STAR_SIZE,
                label=rf"This work, Bu$_{{\mathrm{{AV}}}}={int(round(bu_avg))}$ MWd/kgHM")
    ax.set_xlabel(r"Relative radius r/r$_0$ (/)")
    ax.set_ylabel("Average pore radius (m)")
    ax.set_xlim(0.8, 1.005)
    ax.set_ylim(0.0, 1.4e-6)
    ax.legend(loc="upper left", fontsize=8)
    _save(fig, "plot_radial_pore_radius.png")

# =============================================================================
# Plot 5: Pore number density vs r/r0 (experimental + literature + this-work overlay)
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
    _plot_exp_with_connector(ax, exp_d_cappia, COLOR_CAPPIA, "Cappia (2016)")
    _plot_exp_with_connector(ax, exp_d_40, COLOR_SPINO_R,
                             r"Spino (1996), Bu$_{\mathrm{AV}}=40$ MWd/kgU")
    _plot_exp_with_connector(ax, exp_d_57, COLOR_SPINO_B,
                             r"Spino (1996), Bu$_{\mathrm{AV}}=57$ MWd/kgU")
    _plot_exp_with_connector(ax, exp_d_67, COLOR_SPINO_P,
                             r"Spino (1996), Bu$_{\mathrm{AV}}=67$ MWd/kgU")
    ax.plot(model_barani_np[:, 0], model_barani_np[:, 1],
            "--*", color=COLOR_BARANI, linewidth=LW_REF, markersize=STAR_SIZE,
            label="Barani (2022)")
    for bu_avg, arr, col in valid_runs:
        ax.plot(arr[:, 0], arr[:, 5],
                "-*", color=col, linewidth=LW_MODEL, markersize=STAR_SIZE,
                label=rf"This work, Bu$_{{\mathrm{{AV}}}}={int(round(bu_avg))}$ MWd/kgHM")
    ax.set_xlabel(r"Relative radius r/r$_0$ (/)")
    ax.set_ylabel(r"Pore number density (pores m$^{-3}$)")
    ax.set_xlim(0.9, 1.005)
    ax.set_ylim(0.0, 6e17)
    ax.legend(loc="upper left", fontsize=8)
    _save(fig, "plot_radial_pore_density.png")

print("\nAll radial plots generated.")
