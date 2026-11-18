"""
Case 06 - Coupled two-species spectral diffusion (precursor -> daughter).

Coupling-relevant case, not in the report: SCIANTIX exchanges several
fission-gas isotopes per OFFBEAT node, including short-lived radioactive
precursors that decay into stable daughters while both diffuse in the grain
(the report itself flags this in Section 4.3: "SCIANTIX handles 5 elements
(3 stable and 2 non-stable)", e.g. 133-Xe -> Xe, 85m-Kr -> Kr). None of the
single-species cases (01-05) exercise the production/decay coupling term
between two simultaneously-diffusing species, i.e.
Solver::SpectralDiffusion2equations. This case verifies that coupling
directly via MMS:

    dc1/dt = D1 div grad c1 + lambda*c2 + beta1     (c1: stable daughter)
    dc2/dt = D2 div grad c2 - lambda*c2 + beta2      (c2: radioactive precursor)

with manufactured solutions from the same (a^2-r^2)*f(t) family used
elsewhere in this suite:

    c1_M(r,t) = (a^2-r^2) f1(t),   f1(t) = alpha1 * sin(eps1*t)   (slow, stable)
    c2_M(r,t) = (a^2-r^2) f2(t),   f2(t) = alpha2 * sin(eps2*t)   (fast, precursor)

with eps2 >> eps1, matching the report's note that non-stable nuclides need
more modes/finer resolution to capture their faster dynamics. The
manufactured sources and the coupled modal solver are implemented in
../common/coupled_diffusion_mms.py.

Run: python3 run_case.py
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "common"))
from coupled_diffusion_mms import run
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

# --- Manufactured solution / physical parameters ----------------------------
a = 5e-6          # grain radius [m]
D1 = lambda t: 1e-13 * np.ones_like(t) if isinstance(t, np.ndarray) else 1e-13   # daughter diffusivity
D2 = lambda t: 1e-13 * np.ones_like(t) if isinstance(t, np.ndarray) else 1e-13   # precursor diffusivity
lam = 0.01        # precursor decay constant [1/s], e.g. representative of a short-lived isotope

alpha1, eps1 = 1.0, 0.005   # stable daughter: slower manufactured oscillation
alpha2, eps2 = 1.0, 0.02    # precursor: faster ("fast-oscillating") manufactured oscillation

f1 = lambda t: alpha1 * np.sin(eps1 * t)
f1p = lambda t: alpha1 * eps1 * np.cos(eps1 * t)
f2 = lambda t: alpha2 * np.sin(eps2 * t)
f2p = lambda t: alpha2 * eps2 * np.cos(eps2 * t)

dt = 2.0
t_end = 800.0
mode_counts = [5, 20, 40, 60]

results = {}
for n_modes in mode_counts:
    results[n_modes] = run(f1, f1p, D1, f2, f2p, D2, lam, a, n_modes, dt, t_end)

# --- Time-series comparison, both species, highest-fidelity run ------------
n_ref = max(mode_counts)
t, C1_M, C1_N, C2_M, C2_N, X1_N, X2_N, k = results[n_ref]

fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))
axes[0].plot(t, C1_M, "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured $c_1$ (daughter)")
axes[0].plot(t, C1_N, marker="x", linestyle="none", color=plot_style.LINE_CYCLE[0], markersize=3.5, label=f"numerical, $k$={n_ref}")
axes[0].set_xlabel("$t$ [s]")
axes[0].set_ylabel(r"$\bar{c}_1(t)$ [at/m$^3$]")
axes[0].set_title("Stable daughter concentration")
axes[0].legend(loc="upper center", bbox_to_anchor=(0.5, -0.18), ncol=2)

axes[1].plot(t, C2_M, "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured $c_2$ (precursor)")
axes[1].plot(t, C2_N, marker="x", linestyle="none", color=plot_style.NUMERICAL, markersize=3.5, label=f"numerical, $k$={n_ref}")
axes[1].set_xlabel("$t$ [s]")
axes[1].set_ylabel(r"$\bar{c}_2(t)$ [at/m$^3$]")
axes[1].set_title("Radioactive precursor concentration")
axes[1].legend(loc="upper center", bbox_to_anchor=(0.5, -0.18), ncol=2)

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "coupled_species_comparison"))

# --- All four error metrics vs. number of modes, both species --------------
mode_sweep = [2, 5, 10, 20, 40, 60, 80]
norms1 = {m: [] for m in ("L1", "L2", "Linf", "final")}
norms2 = {m: [] for m in ("L1", "L2", "Linf", "final")}


def norms_of(err):
    return {
        "L1": float(np.mean(err)),
        "L2": float(np.sqrt(np.mean(err**2))),
        "Linf": float(np.max(err)),
        "final": float(err[-1]),
    }


for n_modes in mode_sweep:
    t_, C1_M_, C1_N_, C2_M_, C2_N_, *_ = run(f1, f1p, D1, f2, f2p, D2, lam, a, n_modes, dt, t_end)
    n1 = norms_of(np.abs(C1_M_ - C1_N_))
    n2 = norms_of(np.abs(C2_M_ - C2_N_))
    for m in norms1:
        norms1[m].append(n1[m])
        norms2[m].append(n2[m])

fig2, axes2 = plt.subplots(1, 2, figsize=(11, 4.5))
for metric, style in plot_style.ERROR_STYLE.items():
    axes2[0].semilogy(mode_sweep, norms1[metric], marker=style["marker"], color=style["color"], label=metric)
    axes2[1].semilogy(mode_sweep, norms2[metric], marker=style["marker"], color=style["color"], label=metric)
axes2[0].set_xlabel("number of spectral modes $k$")
axes2[0].set_ylabel("error [at/m$^3$]")
axes2[0].set_title("Daughter species: error vs. modes")
axes2[0].legend()
axes2[1].set_xlabel("number of spectral modes $k$")
axes2[1].set_ylabel("error [at/m$^3$]")
axes2[1].set_title("Precursor species: error vs. modes")
axes2[1].legend()

fig2.tight_layout()
plot_style.save(fig2, os.path.join(HERE, "error_metrics_vs_modes"))

# --- Console report -----------------------------------------------------------
print("Case 06 - coupled precursor-daughter spectral diffusion MMS")
print(f"  a={a}, lambda={lam}, alpha1={alpha1}, eps1={eps1}, alpha2={alpha2}, eps2={eps2}, dt={dt}, t_end={t_end}")
for n_modes in mode_counts:
    t_, C1_M_, C1_N_, C2_M_, C2_N_, *_ = results[n_modes]
    n1 = norms_of(np.abs(C1_M_ - C1_N_))
    n2 = norms_of(np.abs(C2_M_ - C2_N_))
    print(
        f"  k={n_modes:3d}  daughter:   L1={n1['L1']:.3e} L2={n1['L2']:.3e} Linf={n1['Linf']:.3e} final={n1['final']:.3e}"
    )
    print(
        f"           precursor:  L1={n2['L1']:.3e} L2={n2['L2']:.3e} Linf={n2['Linf']:.3e} final={n2['final']:.3e}"
    )
print(f"  plots written to {HERE}")
