"""
Case 13 - UO2 stoichiometry deviation (oxygen-to-metal ratio): the
equilibrium root-find (Solver::NewtonBlackburn) and the Langmuir-based
time evolution toward it (Solver::NewtonLangmuirBasedModel).

Both are genuinely OFFBEAT-exchanged quantities: the wrapper reads and
writes `Sciantix_variables[85]` <-> `oxygenMetalRatio_[cellI]`
(offbeat-official, offbeatLib/fissionGasRelease/fgrSCIANTIX.C:485,531),
which downstream feeds thermal-conductivity and diffusivity correlations.
Unlike `Solver::QuarticEquation` (case 11, a genuine defect), both solvers
here already use the *correct* pattern -- `abs(function) < tol` with a
generous `max_iter=50` -- so this case is framed as a check for a
regression, not a bug hunt; the open question is whether that check is
well-scaled for these specific equations, which is what the case answers
quantitatively rather than asserting.

Part A -- Solver::NewtonBlackburn (src/models/UO2Thermochemistry.C):
solves the Blackburn (1973) correlation

    2*ln(x(x+2)/(1-x)) + 108*x^2 - 32700/T + 9.92 = ln(p_O2)

for the equilibrium stoichiometry deviation x_eq, given T and the gap
oxygen partial pressure p_O2 -- an algebraic root-find, not a time
integration. Verified by manufacturing a target x_eq and, at a
representative temperature, computing exactly the p_O2 that makes it the
true root (`blackburn_forward`, the same correlation StoichiometryDeviation.C
already runs forward for `BlackburnThermochemicalModel`), then checking
NewtonBlackburn recovers it.

T does not need to be swept here: because p_O2 is manufactured from
(x_target, T) through the same correlation the solver then inverts, the
`-32700/T` term cancels exactly in the round trip for any T -- a first
version of this case swept T and every curve landed on top of the others
by construction, which is uninformative rather than a confirmation of
robustness. The genuinely open question for a Newton solver is not "does
temperature affect the answer" (structurally it cannot, here) but
"how sensitive is convergence to the starting guess" -- so Part A sweeps
the initial guess `a0` at fixed (T, x_target) instead, and finds a real,
sharply-bounded basin of convergence (see below).

Part B -- Solver::NewtonLangmuirBasedModel (src/models/
StoichiometryDeviation.C, case iStoichiometryDeviation=6, Massih 2018):
advances x(t) via backward Euler on dx/dt = K*(1-beta*exp(alpha*x)). The
substitution y=beta*exp(alpha*x) turns this into the logistic equation
dy/dt=alpha*K*y*(1-y), which has an exact closed form -- so this case
verifies the solver against the *exact* solution of its own governing
ODE (same style as checking backward Euler on dy/dt=-y), rather than one
built by adding an artificial forcing term. K, beta, alpha are computed
from the real Massih (2018) sub-correlations (surface exchange, Langmuir
adsorption) at representative temperature and steam/gap-pressure
conditions within the model's stated 1073-1673 K range.

Produces a 4-panel figure:
  - Top-left:    NewtonBlackburn recovery of a manufactured target x_eq,
                 across a sweep of target values, at one representative T
                 (T itself provably cannot change this, see above).
  - Top-right:   NewtonBlackburn's actual sensitivity: recovered value and
                 iterations taken vs. the initial guess a0, at fixed
                 (T, x_target) -- revealing a narrow basin of convergence
                 (diverges to NaN once a0 is a only few times the target),
                 unlike `UO2Thermochemistry.C`'s own usage, which always
                 starts from the previous time step's converged value.
  - Bottom-left: x(t) trajectory, NewtonLangmuirBasedModel vs. the exact
                 logistic solution.
  - Bottom-right: temporal convergence (error vs. dt, all four norms),
                 confirming first-order (backward-Euler) accuracy.

Run: python3 run_case.py
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "common"))
from stoichiometry_mms import (
    newton_blackburn,
    blackburn_forward,
    run_langmuir,
    error_norms,
)
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

# --- Part A: NewtonBlackburn equilibrium root-find --------------------------
T_rep = 1500.0  # representative temperature; provably does not affect the result (see docstring)
x_target_sweep = [0.0005, 0.001, 0.005, 0.01, 0.02, 0.03]

x_rec_sweep, err_sweep, iters_sweep = [], [], []
for x_target in x_target_sweep:
    p_O2 = blackburn_forward(x_target, T_rep)
    x_rec, it = newton_blackburn(0.0, T_rep, p_O2)
    x_rec_sweep.append(x_rec)
    err_sweep.append(abs(x_rec - x_target))
    iters_sweep.append(it)

# Initial-guess sensitivity, at a fixed target: UO2Thermochemistry.C always
# starts from the previous step's converged value (a warm start), so this
# sweeps how far off that warm start could plausibly be before the solver
# stops converging.
x_target_fixed = 0.01
p_O2_fixed = blackburn_forward(x_target_fixed, T_rep)
a0_sweep = [0.0, 0.005, 0.01, 0.015, 0.02, 0.025, 0.028, 0.03, 0.035, 0.04, 0.05]
a0_x_rec, a0_iters = [], []
with np.errstate(invalid="ignore"):
    for a0 in a0_sweep:
        x_rec, it = newton_blackburn(a0, T_rep, p_O2_fixed)
        a0_x_rec.append(x_rec)
        a0_iters.append(it)

# --- Part B: NewtonLangmuirBasedModel, Massih (2018) sub-correlations -------
T_lang = 1400.0
steam_pressure = 1.0     # atm, representative
gap_O2_pressure = 1e-8   # atm, representative

surface_to_volume = 225.0
k_star = 1e4 * np.exp(-21253.0 / T_lang - 2.43)
tau_inv = k_star * surface_to_volume / 8.0e4
s = 0.023
ka = 1e13 * np.exp(-21557.0 / T_lang)
B = s / np.sqrt(2 * np.pi * 8.314 * T_lang * 0.018)
A = 1.0135e5 * B / (1.66e-6 * ka)
theta = A * steam_pressure * 1.013e5 / (1 + A * steam_pressure * 1.013e5)
gamma = np.sqrt(np.exp(-32700.0 / T_lang + 9.92) * 1.013e5)
rad_c = np.sqrt(0.0004)
beta = rad_c * gamma / np.sqrt(gap_O2_pressure * 1.013e5)
alpha = 57.0 / 2.0
K = tau_inv * theta

x0 = 0.0
tau_char = 1.0 / (alpha * K)
t_end = 4 * tau_char

dt_ref = tau_char / 40
t1, xM1, xN1, iters1 = run_langmuir(x0, K, beta, alpha, dt_ref, t_end)

dt_ratios = [2.0, 0.5, 0.1, 0.02, 0.005]
conv_norms = {metric: [] for metric in ("L1", "L2", "Linf", "final")}
for r in dt_ratios:
    _, xM_, xN_, _ = run_langmuir(x0, K, beta, alpha, r * tau_char, t_end)
    norms = error_norms(xM_, xN_)
    for metric in conv_norms:
        conv_norms[metric].append(norms[metric])
order_L2 = np.log(conv_norms["L2"][0] / conv_norms["L2"][-1]) / np.log(dt_ratios[0] / dt_ratios[-1])

# --- Figure ------------------------------------------------------------------
fig, axes = plt.subplots(2, 2, figsize=(11, 8.5))

ax = axes[0, 0]
ax.plot(x_target_sweep, x_target_sweep, "-", color=plot_style.MANUFACTURED, linewidth=1.4, label="exact (y=x)")
ax.plot(x_target_sweep, x_rec_sweep, marker="o", linestyle="none", markersize=6, color=plot_style.NUMERICAL, label=f"NewtonBlackburn (T={T_rep:.0f} K)")
ax.set_xlabel(r"manufactured target $x_{eq}^M$")
ax.set_ylabel(r"recovered $x_{eq}^N$ (NewtonBlackburn)")
ax.set_title("Equilibrium stoichiometry deviation: root-find recovery")
ax.legend(fontsize=8.5)

ax = axes[0, 1]
a0_err = np.abs(np.array(a0_x_rec) - x_target_fixed)
converged = ~np.isnan(a0_err)
a0_arr = np.array(a0_sweep)
first_diverging = a0_arr[~converged][0] if (~converged).any() else a0_arr[-1]
ax.semilogy(a0_arr[converged], np.maximum(a0_err[converged], 1e-18), marker="s", color=plot_style.LINE_CYCLE[0], label="converged")
ax.axvspan(first_diverging, a0_arr[-1], color=plot_style.NUMERICAL, alpha=0.12)
ax.axvline(x_target_fixed, color="gray", linestyle=":", linewidth=1.2)
ax.set_xlim(a0_arr[0], a0_arr[-1])
y0, y1 = ax.get_ylim()
ax.text(x_target_fixed, y0, "  target $x_{eq}^M$", fontsize=7.5, color="gray", va="bottom")
ax.text((first_diverging + a0_arr[-1]) / 2, y1, "diverges to NaN", fontsize=8, color=plot_style.NUMERICAL, ha="center", va="top")
ax.set_xlabel(r"initial guess $a_0$ (target fixed at $x_{eq}^M$=0.01)")
ax.set_ylabel("absolute error")
ax.set_title("NewtonBlackburn: narrow basin of convergence in $a_0$")

ax = axes[1, 0]
ax.plot(t1 / tau_char, xM1, "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="exact (logistic)")
ax.plot(t1 / tau_char, xN1, marker="x", linestyle="none", markersize=5, color=plot_style.NUMERICAL, label="numerical (NewtonLangmuirBasedModel)")
ax.set_xlabel(r"$t / \tau_{char}$")
ax.set_ylabel(r"stoichiometry deviation $x(t)$")
ax.set_title(f"Approach to equilibrium (T={T_lang:.0f} K, $x_{{eq}}$={xM1[-1]:.4f})")
ax.legend(fontsize=8.5)

ax = axes[1, 1]
for metric, style in plot_style.ERROR_STYLE.items():
    ax.loglog(dt_ratios, conv_norms[metric], marker=style["marker"], color=style["color"], label=metric)
ref = conv_norms["L2"][0] * (np.array(dt_ratios) / dt_ratios[0]) ** 1
ax.loglog(dt_ratios, ref, "k--", linewidth=1.1, label=r"$O(\Delta t)$ reference")
plot_style.clean_log_ticks(ax, dt_ratios)
ax.set_xlabel(r"$\Delta t / \tau_{char}$")
ax.set_ylabel("error")
ax.set_title(f"Temporal convergence (observed order $\\approx${order_L2:.2f})")
ax.legend(fontsize=8.5)

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "stoichiometry_deviation"))

# --- Console report -----------------------------------------------------------
print("Case 13 - stoichiometry deviation (NewtonBlackburn + NewtonLangmuirBasedModel)")
print(f"  Part A - NewtonBlackburn equilibrium root-find (T={T_rep:.0f} K):")
for x_t, e, it in zip(x_target_sweep, err_sweep, iters_sweep):
    print(f"    x_target={x_t:.4f}  error={e:.3e}  iters={it}")
print(f"  Part A - initial-guess sensitivity (x_target={x_target_fixed}, T={T_rep:.0f} K):")
for a0, x_rec, it in zip(a0_sweep, a0_x_rec, a0_iters):
    print(f"    a0={a0:.3f}  x_recovered={x_rec}  iters={it}")
print(f"  Part B - NewtonLangmuirBasedModel: K={K:.4e} 1/s, beta={beta:.4f}, alpha={alpha}, tau_char={tau_char:.1f} s")
print(f"    reference dt={dt_ref:.1f}s ({dt_ref/tau_char:.4f} tau): final error={error_norms(xM1, xN1)['final']:.3e}")
print(f"    observed order of convergence (L2): {order_L2:.3f}")
for r, e in zip(dt_ratios, conv_norms["L2"]):
    print(f"    dt/tau={r:.3f}  L2_err={e:.4e}")
print(f"  plots written to {HERE}")
