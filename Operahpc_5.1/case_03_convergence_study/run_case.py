"""
Case 03 - Convergence and runtime study for the spectral diffusion solver MMS.

Reproduces OperaHPC WP5-D5.1, Figure 18 (four panels), using the report's
representative sinusoidal manufactured solution (same c_M, D_M as case_01):

  - Top-left:     temporal convergence, all four error metrics vs. time step
                   dt, at a fixed, large number of spectral modes (so spatial
                   error is negligible). The solver uses first-order backward
                   Euler in time, so the observed order of accuracy should be
                   close to 1 for every metric.
  - Top-right:    spatial convergence, all four error metrics vs. number of
                   spectral modes k, at a fixed, small dt.
  - Bottom-left:  runtime vs. dt, for several fixed mode counts (k=20,40,100),
                   showing the added cost of temporal refinement.
  - Bottom-right: runtime vs. number of modes, at a fixed dt, showing the
                   added cost of spatial refinement.

Each runtime point is the *minimum* of several repeated timed runs (not the
mean/median): timing noise from the OS scheduler, Python/GC overhead, etc.
only ever adds time, so the minimum across repeats is the standard estimator
of the actual cost of the computation itself (the report's median-of-several
approach was tried first here and left the runtime-vs-modes panel visibly
non-monotonic at these sub-10ms problem sizes -- min removed that noise).

At this case's parameters (the report's own Eq. 11-12 values, same as case
01), the modal relaxation time 1/lambda_1 = a^2/(D*pi^2) ~ 25 s is much
shorter than the forcing period 2*pi/eps ~ 630 s, so the manufactured
solution sits close to its quasi-steady state at every instant and backward
Euler tracks it very well even at large dt -- hence the absolute errors here
are small (~1e-13 at dt=80s) well before dt is actually small. That is a
genuine property of the report's chosen parameters, not a numerical-noise
floor (double-precision epsilon at this solution's ~1e-11 magnitude is
~1e-27, many orders below anything plotted here): the temporal-convergence
panel also reports *relative* error (normalised by max|c_M|) so the
magnitude is legible without cross-referencing c_M's scale, and the
spatial-convergence panel marks the dt-driven error floor explicitly, so the
plateau there reads as an expected effect (spatial error has dropped below
the fixed dt's own truncation error) rather than as broken convergence.

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
from spectral_diffusion_mms import run, error_norms, order_of_convergence
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

a = 5e-6
alpha = 1.0
eps = 0.01
gamma = 1e-13

f = lambda t: alpha * np.sin(eps * t)
fprime = lambda t: alpha * eps * np.cos(eps * t)
D = lambda t: gamma * np.ones_like(t) if isinstance(t, np.ndarray) else gamma

t_end = 2000.0
N_TIMING_REPEATS = 40


def timed_run(n_modes, dt, t_end, repeats=N_TIMING_REPEATS):
    """Minimum wall-clock time [s] of `repeats` timed solves, plus the result."""
    times = []
    result = None
    for _ in range(repeats):
        t0 = time.perf_counter()
        result = run(f, fprime, D, a, n_modes, dt, t_end)
        times.append(time.perf_counter() - t0)
    return float(np.min(times)), result


# --- Temporal convergence: sweep dt at fixed, high mode count --------------
n_modes_fixed = 100
dt_values = [80.0, 40.0, 20.0, 10.0, 5.0, 2.5]

temporal_norms = {metric: [] for metric in ("L1", "L2", "Linf", "final")}
for dt in dt_values:
    t, C_M, C_N, X_N, k = run(f, fprime, D, a, n_modes_fixed, dt, t_end)
    norms = error_norms(C_M, C_N)
    for metric in temporal_norms:
        temporal_norms[metric].append(norms[metric])

temporal_oc = {
    metric: [
        order_of_convergence(vals[i], vals[i + 1], dt_values[i] / dt_values[i + 1])
        for i in range(len(dt_values) - 1)
    ]
    for metric, vals in temporal_norms.items()
}

# Relative error, normalised by the manufactured solution's own peak
# magnitude, so the (genuinely small) absolute numbers above are legible
# without separately knowing c_M's scale.
C_M_peak = 0.4 * a**2 * alpha
temporal_norms_relative = {metric: [v / C_M_peak for v in vals] for metric, vals in temporal_norms.items()}

# --- Spatial convergence: sweep number of modes at fixed, small dt ---------
dt_fixed = 2.5
mode_values = [1, 2, 5, 10, 20, 40, 80]

spatial_norms = {metric: [] for metric in ("L1", "L2", "Linf", "final")}
for n_modes in mode_values:
    t, C_M, C_N, X_N, k = run(f, fprime, D, a, n_modes, dt_fixed, t_end)
    norms = error_norms(C_M, C_N)
    for metric in spatial_norms:
        spatial_norms[metric].append(norms[metric])

# --- Runtime vs. dt, for several fixed mode counts --------------------------
runtime_dt_values = [80.0, 40.0, 20.0, 10.0, 5.0]
runtime_mode_fixed_values = [20, 40, 100]

runtime_vs_dt = {k_fixed: [] for k_fixed in runtime_mode_fixed_values}
for k_fixed in runtime_mode_fixed_values:
    for dt in runtime_dt_values:
        med_time, _ = timed_run(k_fixed, dt, t_end)
        runtime_vs_dt[k_fixed].append(med_time)

# --- Runtime vs. number of modes, at fixed dt -------------------------------
runtime_mode_sweep = [1, 5, 10, 20, 40, 80, 100, 150]
dt_for_mode_runtime = 10.0

runtime_vs_modes = []
for n_modes in runtime_mode_sweep:
    med_time, _ = timed_run(n_modes, dt_for_mode_runtime, t_end)
    runtime_vs_modes.append(med_time)

# --- Plots: 4-panel figure mirroring Figure 18 ------------------------------
fig, axes = plt.subplots(2, 2, figsize=(11, 8.5))

ax = axes[0, 0]
for metric, style in plot_style.ERROR_STYLE.items():
    ax.loglog(dt_values, temporal_norms[metric], marker=style["marker"], color=style["color"], label=metric)
ref = temporal_norms["L2"][0] * (np.array(dt_values) / dt_values[0]) ** 1
ax.loglog(dt_values, ref, "k--", linewidth=1.1, label=r"$O(\Delta t)$ reference")
plot_style.clean_log_ticks(ax, dt_values)
ax.set_xlabel(r"$\Delta t$ [s]")
ax.set_ylabel("absolute error [at/m$^3$]")
ax.set_title(f"Temporal convergence ($k$={n_modes_fixed})")
ax.legend(fontsize=8.5)
ax_rel = ax.twinx()
ax_rel.set_yscale("log")
ax_rel.set_ylim(np.array(ax.get_ylim()) / C_M_peak)
ax_rel.set_ylabel(r"relative error ($/\ \max|c_M|$)")

ax = axes[0, 1]
for metric, style in plot_style.ERROR_STYLE.items():
    ax.semilogy(mode_values, spatial_norms[metric], marker=style["marker"], color=style["color"], label=metric)
dt_floor = temporal_norms["L2"][dt_values.index(dt_fixed)]
linf_floor = spatial_norms["Linf"][-1]
ax.axhline(dt_floor, color="gray", linestyle=":", linewidth=1.3, zorder=0)
ax.text(mode_values[len(mode_values) // 2], linf_floor * 2.6,
        rf"floor set by $\Delta t$={dt_fixed}s's own truncation error", fontsize=7.5, color="gray", ha="center")
ax.set_xlabel("number of spectral modes $k$")
ax.set_ylabel("error [at/m$^3$]")
ax.set_title(rf"Spatial convergence ($\Delta t$={dt_fixed} s)")
ax.legend(fontsize=8.5)

ax = axes[1, 0]
for k_fixed, style in zip(runtime_mode_fixed_values, plot_style.LINE_CYCLE):
    ax.plot(runtime_dt_values, np.array(runtime_vs_dt[k_fixed]) * 1e3, marker="o", color=style, label=f"$k$={k_fixed}")
ax.set_xlabel(r"$\Delta t$ [s]")
ax.set_ylabel("runtime [ms]")
ax.set_title("Runtime vs. time step")
ax.invert_xaxis()
ax.legend()

ax = axes[1, 1]
ax.plot(runtime_mode_sweep, np.array(runtime_vs_modes) * 1e3, marker="o", color=plot_style.LINE_CYCLE[0])
ax.set_xlabel("number of spectral modes $k$")
ax.set_ylabel("runtime [ms]")
ax.set_title(rf"Runtime vs. number of modes ($\Delta t$={dt_for_mode_runtime} s)")

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "convergence_and_runtime_study"))

# --- Console report -----------------------------------------------------------
print("Case 03 - convergence and runtime study (sinusoidal MMS, same case as case_01)")
print(f"\n  Temporal convergence: k={n_modes_fixed} modes, dt swept {dt_values}")
print(f"  (relative to max|c_M| = {C_M_peak:.3e} at/m^3; modal relaxation time 1/lambda_1 ~ {a**2/(gamma*np.pi**2):.1f}s"
      f" vs. forcing period 2*pi/eps ~ {2*np.pi/eps:.0f}s -- solution is quasi-steady, so absolute errors are genuinely small)")
for metric in temporal_norms:
    vals = ", ".join(f"{v:.3e}" for v in temporal_norms[metric])
    rel_vals = ", ".join(f"{v:.3e}" for v in temporal_norms_relative[metric])
    print(f"    {metric:6s}: {vals}")
    print(f"      relative      : {rel_vals}")
    print(f"      observed order per refinement: {['%.3f' % oc for oc in temporal_oc[metric]]}")

print(f"\n  Spatial convergence: dt={dt_fixed}s, modes swept {mode_values}")
for metric in spatial_norms:
    vals = ", ".join(f"{v:.3e}" for v in spatial_norms[metric])
    print(f"    {metric:6s}: {vals}")

print(f"\n  Runtime vs. dt (minimum of {N_TIMING_REPEATS} runs), dt swept {runtime_dt_values}")
for k_fixed in runtime_mode_fixed_values:
    vals = ", ".join(f"{v * 1e3:.3f} ms" for v in runtime_vs_dt[k_fixed])
    print(f"    k={k_fixed:4d}: {vals}")

print(f"\n  Runtime vs. modes (minimum of {N_TIMING_REPEATS} runs, dt={dt_for_mode_runtime}s), modes swept {runtime_mode_sweep}")
print("    " + ", ".join(f"k={n}: {v * 1e3:.3f} ms" for n, v in zip(runtime_mode_sweep, runtime_vs_modes)))

print(f"\n  plots written to {HERE}")
