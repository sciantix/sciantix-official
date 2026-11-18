"""
Case 08 - Restart consistency under OFFBEAT's per-call invocation pattern.

Cases 01-07 all integrate the spectral diffusion MMS continuously, in one
in-process loop from t=0 to t_end. That is not how SCIANTIX is actually
exercised when coupled to OFFBEAT: OFFBEAT calls SCIANTIX repeatedly (once
per node, once per macro time step), and the state SCIANTIX needs to resume
from is handed back and forth as ASCII text rather than kept in memory --
written to `output.txt` (sciantix_variable fields at `std::setprecision(7)`,
history_variable fields at `std::setprecision(10)`,
src/file_manager/Output.C:57-97) and read back in from
`input_initial_conditions.txt` at the next call
(src/file_manager/InputReading.C:115-149).

This case reuses case 01's manufactured solution and the shared kernel's
`run_with_restart(...)` (../common/spectral_diffusion_mms.py) to ask two
separate questions:

  1. Is the restart *pattern itself* (splitting one continuous integration
     into repeated calls that resume from a passed-in state) lossless? Since
     the backward-Euler modal update X_k[j] = f(X_k[j-1], ...) is Markovian,
     restarting at full double precision should reproduce the continuous
     run in `run(...)` to machine epsilon, regardless of call cadence.
  2. What does the *finite-precision ASCII hand-off* actually cost, at the
     precision SCIANTIX's own I/O uses (7 and 10 significant digits), across
     a range of call cadences (steps_per_call) -- and is that cost negligible
     next to the scheme's own time-discretisation error (already quantified
     in cases 01/03), or does it dominate for coarse macro steps / low
     precision?

Produces a 4-panel figure:
  - Top-left:    |c_N - c_M| vs t for the continuous (no-restart) baseline
                  and for restarted runs at increasing call frequency, all
                  at SCIANTIX's actual output precision (7 digits).
  - Top-right:   final-time error vs. ASCII precision (significant digits),
                  at a fixed call cadence, converging to the continuous
                  baseline as digits -> double precision (~15-16).
  - Bottom-left: final-time error vs. call cadence (steps per driver call),
                  at SCIANTIX's actual 7- and 10-digit precisions, showing
                  whether more frequent restarts accumulate error.
  - Bottom-right: restart-induced error vs. the scheme's own dt-driven
                  discretisation error (from case 01/03), at several dt, to
                  show whether ASCII restart at 7 digits ever becomes the
                  dominant error source.

Run: python3 run_case.py
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "common"))
from spectral_diffusion_mms import run, run_with_restart, error_norms
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

# --- Manufactured solution parameters (same family as case 01) -------------
a = 5e-6
alpha = 1.0
eps = 0.01
gamma = 1e-13

f = lambda t: alpha * np.sin(eps * t)
fprime = lambda t: alpha * eps * np.cos(eps * t)
D = lambda t: gamma * np.ones_like(t) if isinstance(t, np.ndarray) else gamma

n_modes = 40
dt = 80.0       # a coarser, more macro-step-realistic dt than cases 01/02 use
t_end = 4000.0

SCIANTIX_VARIABLE_DIGITS = 7   # src/file_manager/Output.C:66, :97
HISTORY_VARIABLE_DIGITS = 10   # src/file_manager/Output.C:60, :92

# Solver::SpectralDiffusion (src/classes/Solver.C:40) advances the modal state
# by exactly one step of size `increment` per call -- there is no internal
# sub-stepping -- so steps_per_call=1 is the physically accurate cadence for
# today's interface. Larger cadences are swept as a sensitivity check only,
# in case a future coupling scheme batches several steps per driver call.

# --- Panel 1: pointwise error vs. time, continuous vs. restarted -----------
t_ref, C_M_ref, C_N_ref, _, _ = run(f, fprime, D, a, n_modes, dt, t_end)
err_ref = np.abs(C_N_ref - C_M_ref)
# t=0 error is exactly 0 by construction (c(r,0)=0 for both); drop it so the
# log-scale axis isn't stretched over ~300 empty decades down to that point.

cadences = [1, 4, 16]  # driver calls every 1, 4, 16 micro steps
restart_traces = {}
for steps_per_call in cadences:
    t_r, C_M_r, C_N_r, _, _ = run_with_restart(
        f, fprime, D, a, n_modes, dt, steps_per_call, t_end, digits=SCIANTIX_VARIABLE_DIGITS
    )
    restart_traces[steps_per_call] = (t_r, C_M_r, C_N_r)

# --- Panel 2: error vs. ASCII precision (digits), fixed cadence -------------
digit_sweep = [1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 15]
steps_per_call_fixed = 4
final_err_vs_digits = []
for digits in digit_sweep:
    _, C_M_d, C_N_d, _, _ = run_with_restart(f, fprime, D, a, n_modes, dt, steps_per_call_fixed, t_end, digits=digits)
    final_err_vs_digits.append(np.abs(C_N_d[-1] - C_M_d[-1]))
continuous_final_err = np.abs(C_N_ref[-1] - C_M_ref[-1])

# --- Panel 3: error vs. call cadence, at both SCIANTIX I/O precisions ------
cadence_sweep = [1, 2, 4, 8, 16, 32]
final_err_vs_cadence = {SCIANTIX_VARIABLE_DIGITS: [], HISTORY_VARIABLE_DIGITS: []}
for digits in final_err_vs_cadence:
    for steps_per_call in cadence_sweep:
        _, C_M_c, C_N_c, _, _ = run_with_restart(f, fprime, D, a, n_modes, dt, steps_per_call, t_end, digits=digits)
        final_err_vs_cadence[digits].append(np.abs(C_N_c[-1] - C_M_c[-1]))

# --- Panel 4: restart error (7 digits, every call) vs. dt-driven scheme error
dt_sweep = [320.0, 160.0, 80.0, 40.0, 20.0, 10.0, 5.0]
discretisation_err = []
restart_err_7digits = []
for dt_ in dt_sweep:
    _, C_M_dt, C_N_dt, _, _ = run(f, fprime, D, a, n_modes, dt_, t_end)
    discretisation_err.append(np.abs(C_N_dt[-1] - C_M_dt[-1]))
    _, C_M_rdt, C_N_rdt, _, _ = run_with_restart(
        f, fprime, D, a, n_modes, dt_, 1, t_end, digits=SCIANTIX_VARIABLE_DIGITS
    )
    restart_err_7digits.append(np.abs(C_N_rdt[-1] - C_M_rdt[-1]))

# --- Figure ------------------------------------------------------------------
fig, axes = plt.subplots(2, 2, figsize=(11, 8.5))

ax = axes[0, 0]
ax.semilogy(t_ref[1:], err_ref[1:], color=plot_style.MANUFACTURED, linewidth=1.8, label="continuous (no restart)")
for steps_per_call, style in zip(cadences, plot_style.LINE_CYCLE):
    t_r, C_M_r, C_N_r = restart_traces[steps_per_call]
    err_r = np.abs(C_N_r - C_M_r)
    ax.semilogy(t_r[1:], err_r[1:], color=style, linewidth=1.2, label=f"restarted every {steps_per_call} step(s), {SCIANTIX_VARIABLE_DIGITS} digits")
ax.set_xlabel("$t$ [s]")
ax.set_ylabel(r"$|c_N - c_M|$ [at/m$^3$]")
ax.set_title("Pointwise error: continuous vs. restarted")
ax.legend(fontsize=7.5)

ax = axes[0, 1]
ax.semilogy(digit_sweep, final_err_vs_digits, marker="o", color=plot_style.ERROR_STYLE["L1"]["color"], label=f"restarted (every {steps_per_call_fixed} steps)")
ax.axhline(continuous_final_err, color=plot_style.MANUFACTURED, linestyle="--", linewidth=1.3, label="continuous baseline")
ax.axvline(SCIANTIX_VARIABLE_DIGITS, color=plot_style.ERROR_STYLE["Linf"]["color"], linestyle=":", linewidth=1.2, label=f"SCIANTIX output.txt ({SCIANTIX_VARIABLE_DIGITS} digits)")
ax.set_xlabel("ASCII restart precision [significant digits]")
ax.set_ylabel("final-time error [at/m$^3$]")
ax.set_title("Error vs. restart precision")
ax.legend(fontsize=7.5)

ax = axes[1, 0]
for digits, style in zip(final_err_vs_cadence, [plot_style.ERROR_STYLE["L2"]["color"], plot_style.ERROR_STYLE["final"]["color"]]):
    ax.semilogx(cadence_sweep, final_err_vs_cadence[digits], marker="s", color=style, label=f"{digits} digits")
ax.axhline(continuous_final_err, color=plot_style.MANUFACTURED, linestyle="--", linewidth=1.3, label="continuous baseline")
ax.ticklabel_format(axis="y", style="sci", scilimits=(0, 0), useMathText=True)
plot_style.clean_log_ticks(ax, cadence_sweep)
ax.set_xlabel("micro steps per driver call")
ax.set_ylabel("final-time error [at/m$^3$]")
ax.set_title("Error vs. call cadence (no growth with less frequent restarts)")
ax.legend(fontsize=7.5)

ax = axes[1, 1]
ax.loglog(dt_sweep, discretisation_err, marker="o", color=plot_style.MANUFACTURED, label="dt-driven discretisation error (no restart)")
ax.loglog(dt_sweep, restart_err_7digits, marker="^", color=plot_style.ERROR_STYLE["Linf"]["color"], label=f"restarted every step, {SCIANTIX_VARIABLE_DIGITS} digits")
plot_style.clean_log_ticks(ax, dt_sweep)
ax.set_xlabel("$dt$ [s]")
ax.set_ylabel("final-time error [at/m$^3$]")
ax.set_title("Restart error vs. scheme's own discretisation error")
ax.legend(fontsize=7.5)

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "restart_consistency"))

# --- Console report -----------------------------------------------------------
print("Case 08 - macro-step restart consistency")
print(f"  a={a}, alpha={alpha}, eps={eps}, gamma={gamma}, k={n_modes}, dt={dt}, t_end={t_end}")
print(f"  continuous (no restart) final-time error: {continuous_final_err:.6e}")
print("  restart at full double precision (digits=None) must equal the continuous run exactly:")
for steps_per_call in cadences:
    _, C_M_x, C_N_x, _, _ = run_with_restart(f, fprime, D, a, n_modes, dt, steps_per_call, t_end, digits=None)
    max_diff = np.max(np.abs(C_N_x - C_N_ref))
    print(f"    steps_per_call={steps_per_call:3d}  max|C_N_restarted - C_N_continuous| = {max_diff:.3e} (should be 0)")
print(f"  restarted every step at {SCIANTIX_VARIABLE_DIGITS} digits (SCIANTIX's own output.txt precision):")
for steps_per_call in cadences:
    t_r, C_M_r, C_N_r = restart_traces[steps_per_call]
    norms = error_norms(C_M_r, C_N_r)
    print(
        f"    steps_per_call={steps_per_call:3d}  L1={norms['L1']:.6e}  L2={norms['L2']:.6e}  "
        f"Linf={norms['Linf']:.6e}  final={norms['final']:.6e}"
    )
if dt in dt_sweep:
    idx = dt_sweep.index(dt)
    print(
        f"  at dt={dt}: discretisation error={discretisation_err[idx]:.3e}  "
        f"restart error ({SCIANTIX_VARIABLE_DIGITS} digits, every step)={restart_err_7digits[idx]:.3e}"
    )
print(f"  plots written to {HERE}")
