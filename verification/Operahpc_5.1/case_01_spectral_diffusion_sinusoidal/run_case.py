"""
Case 01 - Spectral diffusion solver, sinusoidal manufactured solution.

Reproduces the "representative case" of OperaHPC WP5-D5.1, Section 4.3
(Figures 15-18): verification of SCIANTIX's PolyPole-2 spectral diffusion
solver (Solver::SpectralDiffusion) against

    c_M(r,t) = alpha * (a^2 - r^2) * sin(eps*t)      (Eq. 11)
    D_M(r,t) = gamma                                 (Eq. 12, constant)

    beta_M(r,t) = alpha*eps*(a^2-r^2)*cos(eps*t) + 6*alpha*gamma*sin(eps*t)   (Eq. 13)

Produces, like the report:
  - volume-averaged concentration c_bar(t): manufactured vs. numerical, for
    increasing number of spectral modes k (Figure 16 analogue)
  - radial concentration profile c(r) at a fixed time (Figure 17 analogue)
  - all four error metrics (L1/average, L2/RMSE, Linf/max, final-value) vs.
    number of modes

Run: python3 run_case.py
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "common"))
from spectral_diffusion_mms import run, reconstruct_profile, error_norms
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

# --- Manufactured solution parameters (Eq. 11-12) --------------------------
a = 5e-6       # grain radius [m]
alpha = 1.0    # amplitude of the manufactured solution [at/m^3 / m^2]
eps = 0.01     # angular frequency [rad/s]
gamma = 1e-13  # manufactured (constant) diffusion coefficient [m^2/s]

f = lambda t: alpha * np.sin(eps * t)
fprime = lambda t: alpha * eps * np.cos(eps * t)
D = lambda t: gamma * np.ones_like(t) if isinstance(t, np.ndarray) else gamma

dt = 5.0
t_end = 2000.0

# --- Volume-averaged concentration for increasing mode number --------------
mode_counts = [1, 5, 20, 40]

fig, ax = plt.subplots(figsize=(6.5, 4.5))
results = {}
for n_modes, style in zip(mode_counts, plot_style.LINE_CYCLE):
    t, C_M, C_N, X_N, k = run(f, fprime, D, a, n_modes, dt, t_end)
    results[n_modes] = (t, C_M, C_N, X_N, k)
    ax.plot(t, C_N, marker="x", linestyle="none", markersize=3.5, color=style, label=f"numerical, $k$={n_modes}")
ax.plot(t, C_M, "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured", zorder=0)
ax.set_xlabel("$t$ [s]")
ax.set_ylabel(r"$\bar{c}(t)$ [at/m$^3$]")
ax.set_title("Volume-averaged concentration vs. manufactured solution")
ax.legend(loc="center left", bbox_to_anchor=(1.02, 0.5))
fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "volume_averaged_comparison"))

# --- Radial profile at the final time step, highest-fidelity run -----------
n_modes_ref = max(mode_counts)
t, C_M, C_N, X_N, k = results[n_modes_ref]
r = np.linspace(0.0, a, 100)
j = -1
c_N_r = reconstruct_profile(X_N, k, a, r, j)
c_M_r = alpha * (a**2 - r**2) * np.sin(eps * t[j])

fig2, ax2 = plt.subplots(figsize=(6.5, 4.5))
ax2.plot(r * 1e6, c_M_r, "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured")
ax2.plot(r * 1e6, c_N_r, marker="x", linestyle="none", color=plot_style.NUMERICAL, markersize=4.5, label=f"numerical, $k$={n_modes_ref}")
ax2.set_xlabel(r"$r$ [$\mu$m]")
ax2.set_ylabel(f"$c(r, t={t[j]:.0f}$ s$)$ [at/m$^3$]")
ax2.set_title("Radial concentration profile")
ax2.legend()
fig2.tight_layout()
plot_style.save(fig2, os.path.join(HERE, "radial_profile_comparison"))

# --- All four error metrics vs. number of modes -----------------------------
mode_sweep = [1, 2, 5, 10, 20, 40, 80]
norm_history = {metric: [] for metric in ("L1", "L2", "Linf", "final")}
for n_modes in mode_sweep:
    t_, C_M_, C_N_, _, _ = run(f, fprime, D, a, n_modes, dt, t_end)
    norms = error_norms(C_M_, C_N_)
    for metric in norm_history:
        norm_history[metric].append(norms[metric])

fig3, ax3 = plt.subplots(figsize=(6.5, 4.5))
for metric, style in plot_style.ERROR_STYLE.items():
    ax3.semilogy(mode_sweep, norm_history[metric], marker=style["marker"], color=style["color"], label=metric)
ax3.set_xlabel("number of spectral modes $k$")
ax3.set_ylabel("error [at/m$^3$]")
ax3.set_title("Error metrics vs. number of modes")
ax3.legend()
fig3.tight_layout()
plot_style.save(fig3, os.path.join(HERE, "error_metrics_vs_modes"))

# --- Console report -----------------------------------------------------------
print("Case 01 - spectral diffusion, sinusoidal MMS")
print(f"  a={a}, alpha={alpha}, eps={eps}, gamma={gamma}, dt={dt}, t_end={t_end}")
for n_modes in mode_counts:
    t_, C_M_, C_N_, X_N_, k_ = results[n_modes]
    norms = error_norms(C_M_, C_N_)
    print(
        f"  k={n_modes:3d}  L1={norms['L1']:.6e}  L2={norms['L2']:.6e}  "
        f"Linf={norms['Linf']:.6e}  final={norms['final']:.6e}"
    )
print(f"  radial profile max abs error (k={n_modes_ref}): {np.max(np.abs(c_N_r - c_M_r)):.6e}")
print(f"  plots written to {HERE}")
