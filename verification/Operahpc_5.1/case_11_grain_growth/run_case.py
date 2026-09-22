"""
Case 11 - Grain growth (Solver::QuarticEquation, Ainscough model) --
verifies the solver every other case in this suite implicitly assumes is
exact, and surfaces a genuine defect in how it decides it has converged.

Grain radius `a` is held fixed in cases 01-10 (and in the report). In a real
coupled run it isn't: OFFBEAT's default `igrain_growth 1` evolves it every
call via the Ainscough et al. (1973) model
(src/models/GrainGrowth.C:59-70,138-139), whose "growing" branch reduces to
the quadratic special case of `Solver::QuarticEquation`
(src/classes/Solver.C:346-376):

    y^2 - R_old*y - K*dt = 0     (a=0, b=0, c=1, d=-R_old, e=-K*dt)
    K = grain-boundary mobility (Ainscough), Matrix.C:32:
        K(T) = 1.455e-8 * exp(-32114.5/T)   [m^2/s]

`Solver::QuarticEquation` is a Newton iteration capped at `max_iter=5`, with
an early-exit check `if (function < tol) return y1;` (tol=1e-3), evaluated
on the *residual*, i.e. a length-squared quantity. For physically realistic
grain radii (~1e-6 to 1e-5 m), K*dt -- the "e" coefficient -- is of order
1e-16 to 1e-8 m^2, always many orders of magnitude below tol=1e-3. That
means the early-exit check is satisfied after the very *first* Newton step
regardless of dt or how accurate that step actually is: `max_iter=5` never
engages beyond iteration 1 for this equation, in practice. One Newton step
from y0=R_old is an excellent approximation when K*dt is small (short time
steps), but its error grows with K*dt, i.e. with macro time-step size and
temperature (mobility is Arrhenius in T) -- exactly the direction a
coarser, computationally cheaper OFFBEAT base-irradiation time step pushes.

This case:
  1. Manufactures a saturating grain-growth trajectory R_M(t) (Ainscough-
     like: fast initial growth, slowing toward a limiting radius) and
     derives the exact K*dt each step needs, the same way every other case
     in this suite derives its source term from the manufactured solution.
  2. Advances it with a faithful reproduction of the *shipped* algorithm
     (one Newton step, terminated by the tolerance check exactly as
     written) and, for comparison only, with the same Newton recursion run
     to full convergence (no early exit) -- isolating whether the
     discrete scheme itself is sound (it is) from whether the shipped
     termination logic honours it (it does not, at large K*dt).
  3. Sweeps time-step size from ~1 hour to several hundred days (a
     plausible range for OFFBEAT's own macro time-stepping, see case
     08/09) at a representative high temperature, to show where the
     resulting error becomes practically significant.
  4. Separately exercises the general (non-degenerate) quartic Newton solve
     at well-scaled, order-1 coefficients with a known root, sweeping the
     initial guess. This shows the same early-exit risk is not unique to
     grain growth's tiny length-squared residuals: even here, some initial
     guesses converge cleanly (several iterations, tiny error) while others
     exit after a single step with 15-60% error, whenever that first step's
     residual happens to land below the fixed, signed threshold. The defect
     is the convergence check itself -- unscaled and not an absolute value
     -- not something specific to grain growth's units.

This is reported as a finding, not silently patched: the "full convergence"
trace uses `common/quartic_solver_mms.newton_to_full_convergence`, which is
NOT what `Solver::QuarticEquation` does today.

Run: python3 run_case.py
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "common"))
from quartic_solver_mms import (
    quartic_step,
    newton_to_full_convergence,
    mobility_ainscough,
    R_M,
    error_norms,
)
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

# --- Manufactured grain-growth trajectory + representative physics --------
R0 = 5e-6      # initial grain radius [m] (matches case 01's `a`)
dR = 5e-6      # growth toward R0+dR = 1e-5 m
tau = 100 * 24 * 3600.0  # saturation timescale [s] (~100 days)
T_rep = 1900.0  # representative high fuel temperature [K]
K = mobility_ainscough(T_rep)

t_end = 3 * tau


def run(dt, use_full_convergence=False):
    t = np.arange(0.0, t_end + dt, dt)
    n_t = len(t)
    R_Mt = R_M(t, R0, dR, tau)
    R_N = np.zeros(n_t)
    R_N[0] = R_Mt[0]
    iters = np.zeros(n_t - 1, dtype=int)
    for j in range(1, n_t):
        Kdt = R_Mt[j] ** 2 - R_Mt[j - 1] * R_Mt[j]
        if use_full_convergence:
            R_N[j] = newton_to_full_convergence(R_N[j - 1], 0.0, 0.0, 1.0, -R_N[j - 1], -Kdt)
            iters[j - 1] = 50
        else:
            R_N[j], iters[j - 1] = quartic_step(R_N[j - 1], 0.0, 0.0, 1.0, -R_N[j - 1], -Kdt)
    return t, R_Mt, R_N, iters


# --- Panel 1: trajectory at a challenging (coarse) time step --------------
dt_challenge = 30 * 24 * 3600.0  # 30 days
t1, RM1, RN_shipped1, iters1 = run(dt_challenge, use_full_convergence=False)
_, _, RN_full1, _ = run(dt_challenge, use_full_convergence=True)

# --- Panel 2: error vs. time-step size, shipped vs. fully-converged -------
dt_days_sweep = [0.05, 1, 5, 10, 30, 100, 300, 1000]
dt_sweep = [d * 24 * 3600.0 for d in dt_days_sweep]
err_shipped, err_full, iters_avg = [], [], []
for dt in dt_sweep:
    t_, RM_, RN_s, it_ = run(dt, use_full_convergence=False)
    _, _, RN_f, _ = run(dt, use_full_convergence=True)
    err_shipped.append(error_norms(RM_, RN_s)["final"])
    err_full.append(error_norms(RM_, RN_f)["final"])
    iters_avg.append(np.mean(it_))

# --- Panel 4: generic, well-scaled quartic (order-1 coefficients/root) ----
# a*y^4 + b*y^3 + c*y^2 + d*y + e = 0, manufactured root y_M with all
# coefficients O(1) so the fixed tol=1e-3 is dimensionally appropriate.
a4, b4, c4, d4 = 1.0, -2.0, 0.5, 3.0
y_M_generic = 1.7
e4 = -(a4 * y_M_generic**4 + b4 * y_M_generic**3 + c4 * y_M_generic**2 + d4 * y_M_generic)
y0_sweep = [0.5, 1.0, 1.5, 2.0, 2.5, 3.0]
generic_err, generic_iters = [], []
for y0 in y0_sweep:
    root, it = quartic_step(y0, a4, b4, c4, d4, e4)
    generic_err.append(abs(root - y_M_generic))
    generic_iters.append(it)

# --- Figure ------------------------------------------------------------------
fig, axes = plt.subplots(2, 2, figsize=(11, 8.5))

ax = axes[0, 0]
ax.plot(t1 / 86400, RM1 * 1e6, "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured")
ax.plot(t1 / 86400, RN_shipped1 * 1e6, marker="x", linestyle="none", markersize=5,
        color=plot_style.ERROR_STYLE["Linf"]["color"], label="shipped (1 Newton step, as coded)")
ax.plot(t1 / 86400, RN_full1 * 1e6, marker="+", linestyle="none", markersize=6,
        color=plot_style.ERROR_STYLE["L1"]["color"], label="Newton run to full convergence")
ax.set_xlabel("$t$ [days]")
ax.set_ylabel(r"grain radius $R(t)$ [$\mu$m]")
ax.set_title(f"Trajectory at a coarse time step ($\\Delta t$={dt_challenge/86400:.0f} days)")
plot_style.inset_legend(ax, loc="lower right", fontsize=8.5)

ax = axes[0, 1]
ax.loglog(dt_days_sweep, err_shipped, marker="^", color=plot_style.ERROR_STYLE["Linf"]["color"], label="shipped")
ax.loglog(dt_days_sweep, np.maximum(err_full, 1e-20), marker="o", color=plot_style.ERROR_STYLE["L1"]["color"], label="fully converged (floored at $10^{-20}$)")
plot_style.clean_log_ticks(ax, dt_days_sweep)
ax.set_xlabel(r"$\Delta t$ [days]")
ax.set_ylabel("final-time error [m]")
ax.set_title("Error vs. macro time-step size")
ax.legend(fontsize=8.5)

ax = axes[1, 0]
ax.semilogx(dt_days_sweep, iters_avg, marker="s", color=plot_style.LINE_CYCLE[0])
ax.axhline(5, color="gray", linestyle="--", linewidth=1.2)
ax.text(dt_days_sweep[0], 5.15, "max_iter = 5 (never reached)", fontsize=8, color="gray")
ax.set_ylim(0, 6)
plot_style.clean_log_ticks(ax, dt_days_sweep)
ax.set_xlabel(r"$\Delta t$ [days]")
ax.set_ylabel("Newton iterations actually taken")
ax.set_title("Shipped solver: always exits after 1 iteration")

ax = axes[1, 1]
ax.semilogy(y0_sweep, np.maximum(generic_err, 1e-18), marker="D", color=plot_style.LINE_CYCLE[2])
ax.set_xlabel(r"initial guess $y_0$")
ax.set_ylabel(r"$|y_N - y_M|$")
ax.set_title("Well-scaled quartic (root=1.7): reliability depends on $y_0$")

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "grain_growth"))

# --- Console report -----------------------------------------------------------
print("Case 11 - grain growth (Solver::QuarticEquation, Ainscough model)")
print(f"  R0={R0}, dR={dR}, tau={tau/86400:.0f} days, T={T_rep} K, K={K:.4e} m^2/s")
print(f"  at dt={dt_challenge/86400:.0f} days: shipped final err={error_norms(RM1, RN_shipped1)['final']:.3e} m"
      f"  ({100*error_norms(RM1, RN_shipped1)['final']/RM1[-1]:.2f}% of R)"
      f",  fully-converged final err={error_norms(RM1, RN_full1)['final']:.3e} m")
print("  error vs. dt, shipped (1 Newton step) vs. fully converged:")
for d, es, ef, it in zip(dt_days_sweep, err_shipped, err_full, iters_avg):
    print(f"    dt={d:7.2f} days  shipped={es:.3e} m  full={ef:.3e} m  iters_taken(shipped)={it:.1f}")
print("  generic well-scaled quartic sanity check (root=1.7):")
for y0, err, it in zip(y0_sweep, generic_err, generic_iters):
    print(f"    y0={y0:.1f}  |err|={err:.3e}  iters={it}")
print(f"  plots written to {HERE}")
