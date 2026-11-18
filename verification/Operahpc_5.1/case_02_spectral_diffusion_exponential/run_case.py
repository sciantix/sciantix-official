"""
Case 02 - Spectral diffusion solver, exponential manufactured solution.

This is the manufactured solution actually wired into the C++ solver on this
branch (see src/classes/Solver.C and src/models/GasDiffusion.C):

    c_M(r,t) = (a^2 - r^2) * exp(0.005*t)
    D_M(t)   = exp(-0.001*t)          (time-dependent, unlike case_01's constant D)

It is kept here, reorganised onto the shared kernel in ../common, as a second
data point for the same verification approach described in OperaHPC WP5-D5.1
Section 4.3: same PDE (Eq. 10), same solver, different (c_M, D_M) pair,
so both the manufactured-solution family and the time-varying-diffusivity
path through the solver get exercised.

Run: python3 run_case.py
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "common"))
from spectral_diffusion_mms import run, error_norms
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

a = 5e-6  # grain radius [m]

f = lambda t: np.exp(0.005 * t)
fprime = lambda t: 0.005 * np.exp(0.005 * t)
D = lambda t: np.exp(-0.001 * t)

dt = 20.0
t_end = 1000.0
mode_counts = [1, 5, 20, 40]

fig, ax = plt.subplots(figsize=(6.5, 4.5))
results = {}
for n_modes, style in zip(mode_counts, plot_style.LINE_CYCLE):
    t, C_M, C_N, X_N, k = run(f, fprime, D, a, n_modes, dt, t_end)
    results[n_modes] = (t, C_M, C_N)
    ax.plot(t, C_N, marker="x", linestyle="none", markersize=3.5, color=style, label=f"numerical, $k$={n_modes}")
ax.plot(t, C_M, "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured", zorder=0)
ax.set_xlabel("$t$ [s]")
ax.set_ylabel(r"$\bar{c}(t)$ [at/m$^3$]")
ax.set_title("Volume-averaged concentration vs. manufactured solution")
ax.legend(ncol=2)
fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "volume_averaged_comparison"))

# --- All four error metrics vs. number of modes -----------------------------
mode_sweep = [1, 2, 5, 10, 20, 40, 80]
norm_history = {metric: [] for metric in ("L1", "L2", "Linf", "final")}
for n_modes in mode_sweep:
    t_, C_M_, C_N_, _, _ = run(f, fprime, D, a, n_modes, dt, t_end)
    norms = error_norms(C_M_, C_N_)
    for metric in norm_history:
        norm_history[metric].append(norms[metric])

fig2, ax2 = plt.subplots(figsize=(6.5, 4.5))
for metric, style in plot_style.ERROR_STYLE.items():
    ax2.semilogy(mode_sweep, norm_history[metric], marker=style["marker"], color=style["color"], label=metric)
ax2.set_xlabel("number of spectral modes $k$")
ax2.set_ylabel("error [at/m$^3$]")
ax2.set_title("Error metrics vs. number of modes")
ax2.legend()
fig2.tight_layout()
plot_style.save(fig2, os.path.join(HERE, "error_metrics_vs_modes"))

print("Case 02 - spectral diffusion, exponential MMS (matches Solver.C / GasDiffusion.C)")
print(f"  a={a}, dt={dt}, t_end={t_end}")
for n_modes in mode_counts:
    t_, C_M_, C_N_ = results[n_modes]
    norms = error_norms(C_M_, C_N_)
    print(
        f"  k={n_modes:3d}  L1={norms['L1']:.6e}  L2={norms['L2']:.6e}  "
        f"Linf={norms['Linf']:.6e}  final={norms['final']:.6e}"
    )
print(f"  plots written to {HERE}")
