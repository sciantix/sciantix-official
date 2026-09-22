"""
Case 04 - Grain-boundary gas balance, fast-oscillating manufactured solution.

Reproduces the second MMS extension described in OperaHPC WP5-D5.1, Section
4.3 (Figure 19): verification of the grain-boundary gas balance

    dc_GB/dt = J_in - R                                              (Eq. 15)

where J_in is the incoming intra-granular flux and R is the release rate.
As in the report, intra-granular resolution and grain-boundary bubble
evolution are neglected: gas reaching the boundary is released directly,
i.e. R = lambda_b * c_GB (a linear release-rate law, the same structural form
as Solver::Decay, dy/dt = -L*y + S, already used and verified elsewhere in
SCIANTIX's MMS suite, see ../../utilities/MMS_verification/mms_Decay.py).

Manufactured solution (fast-oscillating, per the report):

    c_GB_M(t) = C0 + A * sin(omega * t)

R_M(t) is evaluated from the release law above, and J_in is then manufactured
so that the ODE holds exactly:

    R_M(t)   = lambda_b * c_GB_M(t)
    J_in_M(t) = dc_GB_M/dt + R_M(t) = A*omega*cos(omega*t) + lambda_b*c_GB_M(t)

The solver advances c_GB with the same first-order backward-Euler scheme as
Solver::Decay: c[n+1] = (c[n] + J_in[n+1]*dt) / (1 + lambda_b*dt).

Run: python3 run_case.py
"""

import os
import sys
import time

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "common"))
from spectral_diffusion_mms import order_of_convergence
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

# --- Manufactured solution parameters ---------------------------------------
C0 = 1.0e17        # baseline grain-boundary concentration [at/m^3]
A = 5.0e16         # oscillation amplitude [at/m^3]
omega = 0.05       # fast angular frequency [rad/s], "fast-oscillating" per report
lambda_b = 2.0e-3  # release rate constant [1/s]


def c_GB_M(t):
    return C0 + A * np.sin(omega * t)


def R_M(t):
    return lambda_b * c_GB_M(t)


def J_in_M(t):
    return A * omega * np.cos(omega * t) + R_M(t)


def solve(N, ti, tf):
    """Backward-Euler solve of dc_GB/dt = J_in - lambda_b*c_GB, Solver::Decay pattern."""
    dt = (tf - ti) / N
    t = np.linspace(ti, tf, N + 1)
    c = np.zeros(N + 1)
    c[0] = c_GB_M(ti)
    for i in range(N):
        c[i + 1] = (c[i] + J_in_M(t[i + 1]) * dt) / (1.0 + lambda_b * dt)
    return t, c


def error_norms_1d(c_num, c_exact):
    err = np.abs(c_num - c_exact)
    return {
        "L1": float(np.mean(err)),
        "L2": float(np.sqrt(np.mean(err**2))),
        "Linf": float(np.max(err)),
        "final": float(err[-1]),
    }


ti, tf = 0.0, 500.0

# --- Reference run for the time-series plot ---------------------------------
N_ref = 400
t1, c1 = solve(N_ref, ti, tf)
err1 = np.abs(c1 - c_GB_M(t1))

# --- Convergence sweep: all four error metrics + runtime vs. N -------------
N_values = [25, 50, 100, 200, 400, 800, 1600]
norms_hist = {metric: [] for metric in ("L1", "L2", "Linf", "final")}
runtimes = []
for N in N_values:
    reps = []
    for _ in range(7):
        t0 = time.perf_counter()
        t_N, c_N = solve(N, ti, tf)
        reps.append(time.perf_counter() - t0)
    runtimes.append(float(np.median(reps)))
    norms = error_norms_1d(c_N, c_GB_M(t_N))
    for metric in norms_hist:
        norms_hist[metric].append(norms[metric])

oc = {
    metric: [
        order_of_convergence(vals[i], vals[i + 1], N_values[i + 1] / N_values[i])
        for i in range(len(N_values) - 1)
    ]
    for metric, vals in norms_hist.items()
}

# --- Plots -------------------------------------------------------------------
fig, axes = plt.subplots(2, 2, figsize=(11, 8.5))

axes[0, 0].plot(t1, c_GB_M(t1), "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured $c_{GB,M}$")
axes[0, 0].plot(t1, c1, marker="x", linestyle="none", color=plot_style.NUMERICAL, markersize=3.5, label=f"numerical (N={N_ref})")
axes[0, 0].set_xlabel("$t$ [s]")
axes[0, 0].set_ylabel(r"$c_{GB}(t)$ [at/m$^3$]")
axes[0, 0].set_title("Grain-boundary concentration")
plot_style.inset_legend(axes[0, 0], loc="upper right")

axes[0, 1].plot(t1, err1, color=plot_style.LINE_CYCLE[0])
axes[0, 1].set_xlabel("$t$ [s]")
axes[0, 1].set_ylabel("absolute error [at/m$^3$]")
axes[0, 1].set_title(f"Absolute error vs. time (N={N_ref})")

for metric, style in plot_style.ERROR_STYLE.items():
    axes[1, 0].loglog(N_values, norms_hist[metric], marker=style["marker"], color=style["color"], label=metric)
ref = norms_hist["L2"][0] * (np.array(N_values) / N_values[0]) ** -1
axes[1, 0].loglog(N_values, ref, "k--", linewidth=1.1, label=r"$O(1/N)$ reference")
plot_style.clean_log_ticks(axes[1, 0], N_values)
axes[1, 0].set_xlabel("number of time steps $N$")
axes[1, 0].set_ylabel("error [at/m$^3$]")
axes[1, 0].set_title("Convergence vs. number of time steps")
axes[1, 0].legend()

axes[1, 1].loglog(N_values, np.array(runtimes) * 1e3, marker="o", color=plot_style.LINE_CYCLE[0])
plot_style.clean_log_ticks(axes[1, 1], N_values)
axes[1, 1].set_xlabel("number of time steps $N$")
axes[1, 1].set_ylabel("runtime [ms]")
axes[1, 1].set_title("Runtime vs. number of time steps")

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "grain_boundary_release_comparison"))

# --- Console report -----------------------------------------------------------
print("Case 04 - grain-boundary release, fast-oscillating MMS")
print(f"  C0={C0:.3e}, A={A:.3e}, omega={omega}, lambda_b={lambda_b}")
for metric in norms_hist:
    vals = ", ".join(f"{v:.3e}" for v in norms_hist[metric])
    print(f"  {metric:6s}: {vals}")
    print(f"    observed order per refinement: {['%.3f' % o for o in oc[metric]]}")
print("  runtime [ms] vs N: " + ", ".join(f"N={n}: {v * 1e3:.3f}" for n, v in zip(N_values, runtimes)))
print(f"  plots written to {HERE}")
