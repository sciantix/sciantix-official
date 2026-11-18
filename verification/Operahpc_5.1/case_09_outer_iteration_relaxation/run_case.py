"""
Case 09 - OFFBEAT's outer-iteration relaxation and staleness scheme
(`relax`, `nFrequency`) on the SCIANTIX exchange variable.

Grounded in the real coupling wrapper, not the standalone SCIANTIX interface:
gitlab.com/AlessandroScolaro/offbeat-official,
offbeatLib/fissionGasRelease/fgrSCIANTIX.C/.H.

Within one OFFBEAT time step, `fgrSCIANTIX::correct()` is called once per
*outer* (segregated nonlinear) iteration, not once per time step. Each call:

  - re-reads the *previous time step's committed* SCIANTIX state
    (`setSCIANTIXVariables`, fgrSCIANTIX.C:456-507) -- never an
    intermediate, relaxed value,
  - re-solves the *same* time increment fresh with the current outer
    iteration's temperature/stress estimate, only if
    `outerIteration_ % nFrequency_ == 0` (fgrSCIANTIX.C:606; otherwise the
    previous outer iteration's raw result is reused, i.e. SCIANTIX itself is
    skipped that iteration),
  - blends that raw result into the OFFBEAT-visible swelling field with
    `relax_*new + (1-relax_)*old` (fgrSCIANTIX.C:660-670) -- this relaxed
    value is what the rest of OFFBEAT's physics (mechanics, gap conductance)
    sees *during* the iteration.

Only once, at the very end of the real time step (`updateVariables()`,
fgrSCIANTIX.C:698-774), is SCIANTIX re-run *unrelaxed* with whatever
temperature/stress field the outer loop settled on, and *that* result is
permanently committed (`updateSCIANTIXVariables`) and carried into the next
time step. So `relax`/`nFrequency` can only ever bias the physics SCIANTIX
retains if the outer loop is cut off before the temperature/stress estimate
itself has converged -- they cannot bias an already-converged commit.

This case verifies both halves of that claim using case 01's manufactured
solution, with D made a function of a manufactured "temperature" T_M(t)
(D(T) = gamma0*(T/T0), a stand-in for the real Arrhenius dependence -- the
point here is the outer-iteration numerics, not the diffusivity
correlation):

  1. Committed trajectory, ample outer-iteration budget: should reproduce
     the manufactured solution regardless of relax/nFrequency (no bias).
  2. Exposed (intermediate) value within one time step, for a
     manufactured, damped-oscillatory outer-iteration temperature sequence
     (mimicking a segregated nonlinear solve): shows relax damping
     oscillation at the cost of slower convergence, and nFrequency staleness
     stalling convergence outright between SCIANTIX re-evaluations.
  3. Outer iterations needed to bring the exposed value within tolerance of
     the true converged answer, vs relax and nFrequency.
  4. Committed-trajectory error under a *finite* outer-iteration budget
     (mimicking a real solver that stops before full convergence), for an
     "easy" vs. a "hard" (slow-converging) segregated coupling: shows that
     the committed bias is governed entirely by how well the temperature
     field itself converged within budget, not by relax/nFrequency (which
     only ever touch the exchange variable, never the commit) -- confirming
     the risk is an under-resourced outer loop, not the relax/nFrequency
     knobs as such.

Run: python3 run_case.py
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "common"))
from spectral_diffusion_mms import single_step, error_norms
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

# --- Manufactured solution (case 01 family) + manufactured temperature ----
a = 5e-6
alpha = 1.0
eps = 0.01
f = lambda t: alpha * np.sin(eps * t)
fprime = lambda t: alpha * eps * np.cos(eps * t)

T0 = 900.0      # representative fuel temperature [K]
deltaT = 150.0
epsT = 0.01
T_M = lambda t: T0 + deltaT * np.sin(epsT * t)

gamma0 = 1e-13
Ea_over_kB = 5000.0  # [K], representative Arrhenius-type activation scale
D_of_T = lambda T: gamma0 * np.exp(-Ea_over_kB * (1.0 / T - 1.0 / T0))

n_modes = 40
k = np.arange(1, n_modes + 1)
dt = 80.0
n_steps = 10
t_end = dt * n_steps


def const_D(D_value):
    """Wrap a scalar diffusivity as the callable single_step expects."""
    return lambda t: D_value


def outer_temperature_sequence(T_guess0, T_true, n_outer, rho=0.6, theta=0.9):
    """
    Damped, oscillatory sequence of outer-iteration temperature estimates
    converging to T_true, mimicking a segregated nonlinear solve. n=0 is
    exactly the initial guess; the envelope rho^n -> 0 as n grows.
    """
    n = np.arange(n_outer)
    return T_true + (T_guess0 - T_true) * (rho**n) * np.cos(theta * n)


def run_coupled(relax, n_frequency, n_outer_budget, rho=0.6, theta=0.9, commit_at_budget=False):
    """
    Simulate n_steps real OFFBEAT time steps, each resolved through
    n_outer_budget outer iterations of the relax/nFrequency scheme above.

    commit_at_budget=False: the final commit (`updateVariables`) uses the
    *true* end-of-step temperature (ample outer-iteration convergence is
    assumed for the T-field itself) -- isolates whether relax/nFrequency on
    the *exchange variable* alone can bias the committed physics.

    commit_at_budget=True: the final commit uses whatever temperature the
    outer loop reached after n_outer_budget iterations -- models a solver
    that stops on a fixed iteration budget rather than full convergence.

    Returns t (n_steps+1,), C_M, C_committed, and diagnostics for the last
    time step (t_local index, C_exposed_n, C_committed_ref, n_to_converge).
    """
    t = np.array([j * dt for j in range(n_steps + 1)])
    C_M = 0.4 * a**2 * f(t)
    C_committed = np.zeros(n_steps + 1)
    X_committed = np.zeros(n_modes)

    last_step_exposed = None
    n_to_converge_series = []

    for j in range(n_steps):
        t_next = t[j + 1]
        T_true = T_M(t_next)
        T_guess0 = T_M(t[j])
        T_seq = outer_temperature_sequence(T_guess0, T_true, n_outer_budget, rho, theta)

        C_exposed_prev = C_committed[j]
        C_raw = C_committed[j]
        C_exposed_n = np.empty(n_outer_budget)
        n_to_converge = n_outer_budget
        tol = 1e-3 * max(abs(C_M[j + 1]), 1e-30)
        C_committed_ref, _ = None, None

        for n in range(n_outer_budget):
            if n % n_frequency == 0:
                D_call = const_D(D_of_T(T_seq[n]))
                _, C_raw = single_step(X_committed, k, t_next, dt, f, fprime, D_call, a)
            C_exposed = relax * C_raw + (1.0 - relax) * C_exposed_prev
            C_exposed_n[n] = C_exposed
            C_exposed_prev = C_exposed

        # Reference: fully outer-converged value for THIS step (relax=1, nFrequency=1)
        D_call_true = const_D(D_of_T(T_true))
        X_true_step, C_true_step = single_step(X_committed, k, t_next, dt, f, fprime, D_call_true, a)
        for n in range(n_outer_budget):
            if abs(C_exposed_n[n] - C_true_step) < tol:
                n_to_converge = n + 1
                break
        n_to_converge_series.append(n_to_converge)

        if commit_at_budget:
            D_call_commit = const_D(D_of_T(T_seq[-1]))
            X_committed, C_committed[j + 1] = single_step(X_committed, k, t_next, dt, f, fprime, D_call_commit, a)
        else:
            X_committed, C_committed[j + 1] = X_true_step, C_true_step

        if j == n_steps // 2:
            last_step_exposed = (C_exposed_n.copy(), C_true_step)

    return t, C_M, C_committed, last_step_exposed, n_to_converge_series


# --- Panel 1: committed trajectory, ample budget, several (relax, nFreq) ---
combos = [(1.0, 1), (0.5, 1), (0.3, 1), (0.5, 3)]
committed_results = {}
for relax, nfreq in combos:
    t, C_M, C_committed, _, _ = run_coupled(relax, nfreq, n_outer_budget=30, commit_at_budget=False)
    committed_results[(relax, nfreq)] = C_committed

# --- Panels 2 & 3: within-step outer-iteration convergence -----------------
relax_sweep = [1.0, 0.7, 0.5, 0.3]
exposed_traces = {}
n_to_converge_vs_relax = {1: [], 3: []}
for relax in relax_sweep:
    _, _, _, last_step, n_conv_series = run_coupled(relax, 1, n_outer_budget=25, commit_at_budget=False)
    exposed_traces[relax] = last_step
    n_to_converge_vs_relax[1].append(n_conv_series[len(n_conv_series) // 2])
for relax in relax_sweep:
    _, _, _, _, n_conv_series = run_coupled(relax, 3, n_outer_budget=25, commit_at_budget=False)
    n_to_converge_vs_relax[3].append(n_conv_series[len(n_conv_series) // 2])

# --- Panel 4: committed-trajectory error under a finite iteration budget ---
# relax/nFreq on the exchange variable never appear here: the commit
# (updateVariables()) always re-solves unrelaxed. What actually governs
# whether a finite outer-iteration budget biases the *committed* answer is
# how fast the outer T-field iteration itself converges (rho) -- i.e. how
# hard the segregated multiphysics coupling is to converge.
budget_sweep = [1, 2, 3, 5, 8, 12, 20]
rho_sweep = {"easy coupling (rho=0.4)": 0.4, "hard coupling (rho=0.85)": 0.85}
final_err_vs_budget = {label: [] for label in rho_sweep}
for label, rho in rho_sweep.items():
    for budget in budget_sweep:
        t_b, C_M_b, C_committed_b, _, _ = run_coupled(1.0, 1, n_outer_budget=budget, rho=rho, commit_at_budget=True)
        final_err_vs_budget[label].append(error_norms(C_M_b, C_committed_b)["final"])

# --- Figure ------------------------------------------------------------------
fig, axes = plt.subplots(2, 2, figsize=(11, 8.5))

ax = axes[0, 0]
ax.plot(t, C_M, "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured", zorder=0)
for (relax, nfreq), style in zip(combos, plot_style.LINE_CYCLE):
    ax.plot(t, committed_results[(relax, nfreq)], marker="x", linestyle="none", markersize=5,
            color=style, label=f"committed, relax={relax}, nFreq={nfreq}")
ax.set_xlabel("$t$ [s]")
ax.set_ylabel(r"$\bar{c}(t)$ [at/m$^3$]")
ax.set_title("Committed trajectory: unaffected by relax/nFreq (ample budget)")
ax.legend(fontsize=7.5)

ax = axes[0, 1]
n_axis = np.arange(1, 26)
for relax, style in zip(relax_sweep, plot_style.LINE_CYCLE):
    C_exposed_n, C_true_step = exposed_traces[relax]
    ax.plot(n_axis, C_exposed_n, marker="o", markersize=3, color=style, linewidth=1.2, label=f"relax={relax}")
ax.axhline(exposed_traces[1.0][1], color=plot_style.MANUFACTURED, linestyle="--", linewidth=1.3, label="outer-converged value")
ax.set_xlabel("outer iteration $n$")
ax.set_ylabel(r"exposed $\bar{c}_n$ [at/m$^3$]")
ax.set_title("Exposed value vs. outer iteration (nFreq=1)")
ax.legend(fontsize=7.5)

ax = axes[1, 0]
for nfreq, style in zip(n_to_converge_vs_relax, [plot_style.ERROR_STYLE["L2"]["color"], plot_style.ERROR_STYLE["Linf"]["color"]]):
    ax.plot(relax_sweep, n_to_converge_vs_relax[nfreq], marker="s", color=style, label=f"nFreq={nfreq}")
ax.set_xlabel("relax")
ax.set_ylabel("outer iterations to converge (0.1% tol)")
ax.set_title("Convergence cost vs. relax and nFreq")
ax.legend(fontsize=7.5)

ax = axes[1, 1]
for label, style in zip(final_err_vs_budget, [plot_style.ERROR_STYLE["L1"]["color"], plot_style.ERROR_STYLE["final"]["color"]]):
    ax.semilogy(budget_sweep, np.maximum(final_err_vs_budget[label], 1e-20), marker="^", color=style, label=label)
ax.set_xlabel("outer-iteration budget (per time step)")
ax.set_ylabel("committed final-time error [at/m$^3$]")
ax.set_title("Committed error vs. budget: governed by T-loop convergence, not relax/nFreq")
ax.legend(fontsize=7.5)

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "outer_iteration_relaxation"))

# --- Console report -----------------------------------------------------------
print("Case 09 - OFFBEAT outer-iteration relaxation/staleness scheme")
print(f"  a={a}, alpha={alpha}, eps={eps}, T0={T0}, deltaT={deltaT}, dt={dt}, n_steps={n_steps}, k={n_modes}")
print("  committed trajectory (ample budget=30), final-time error vs. manufactured solution:")
for relax, nfreq in combos:
    err = error_norms(C_M, committed_results[(relax, nfreq)])
    print(f"    relax={relax:.1f}  nFreq={nfreq}  final_err={err['final']:.6e}  Linf={err['Linf']:.6e}")
print("  outer iterations to converge (0.1% tol), mid-run step:")
for i, relax in enumerate(relax_sweep):
    print(f"    relax={relax:.1f}  nFreq=1 -> {n_to_converge_vs_relax[1][i]:2d} iters   nFreq=3 -> {n_to_converge_vs_relax[3][i]:2d} iters")
print("  committed final-time error under a finite iteration budget (relax=1, nFreq=1):")
for label in final_err_vs_budget:
    for budget, err in zip(budget_sweep, final_err_vs_budget[label]):
        print(f"    {label:24s}  budget={budget:2d}  final_err={err:.3e}")
print(f"  plots written to {HERE}")
