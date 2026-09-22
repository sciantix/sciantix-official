"""
Case 12 - Grain-boundary bubble coalescence (Solver::BinaryInteraction) and
the full intergranular swelling chain it feeds.

Completes case 10's coverage of the intergranular bubble model
(InterGranularBubbleBehavior.C): case 10 verified vacancies-per-bubble
(`Solver::LimitedGrowth`, stress-driven); this case verifies bubble
*concentration* (`Solver::BinaryInteraction`, coalescence-driven,
InterGranularBubbleBehavior.C:167-170) and the downstream algebra that
combines concentration and volume into the swelling OFFBEAT actually
consumes as an eigenstrain (`Sciantix_variables[18]*I`,
offbeat-official/fgrSCIANTIX.C:551):

    V = n_Xe*VdW_Xe + n_vacancies*V_Schottky              (line 151-154)
    R = 0.620350491*(V/lenticular_shape_factor)^(1/3)     (line 158-160)
    A = pi*(R*sin(semidihedral_angle))^2                  (line 163-165)
    N_new = N_old / (1 + 2.0*N_old*dA)                    (line 168-170, Solver::BinaryInteraction)
    swelling = (3/a_grain) * N * V                          (line 293-296)

`Solver::BinaryInteraction` is the exact (not first-order-approximate)
solution of dy/dA = -k*y^2 for constant k over one step -- unlike every
other solver verified in this suite (backward-Euler, genuine O(dt) error),
its accuracy should not depend on step size at all. This case checks that
directly: it manufactures vacancies-per-bubble and Xe-atoms-per-bubble
trajectories (representative saturating growth, same style as case 11's
grain radius), computes the exact volume/radius/area/concentration/
swelling chain from them, and re-derives concentration numerically with
`Solver::BinaryInteraction` using only each step's area increment.

Produces a 4-panel figure:
  - Top-left:    concentration N_M(t) vs. N_N(t), with the driving bubble
                 area A_M(t) on a twin axis.
  - Top-right:   the actual OFFBEAT-consumed quantity: intergranular gas
                 swelling, manufactured vs. recomputed from the numerical
                 concentration.
  - Bottom-left: relative error vs. time-step size, spanning steps from
                 coarser than the saturation timescale to 1/1000th of it --
                 flat at floating-point precision throughout, unlike every
                 O(dt) case elsewhere in this suite.
  - Bottom-right: what a coding defect *would* look like here -- two
                 plausible miscodings (missing self-dependence on y in the
                 denominator; wrong coalescence coefficient) run through
                 the same manufactured check, to show the size of error
                 this case would actually catch.

Run: python3 run_case.py
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "common"))
from coalescence_swelling_mms import run, saturating, error_norms, INTERACTION_COEFFICIENT
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

# --- Representative manufactured inputs ------------------------------------
tau = 200 * 24 * 3600.0   # saturation timescale [s] (~200 days, same order as case 11)
t_end = 3 * tau
N0 = 1.0e14                # representative intergranular bubble concentration [# / m^2]
a_grain = 5e-6              # representative grain radius [m] (matches case 01/10)

y0, dy = 50.0, 150.0        # manufactured vacancies-per-bubble range
n0, dn = 20.0, 80.0         # manufactured Xe-atoms-per-bubble range


def trajectories(dt):
    t = np.arange(0.0, t_end + dt, dt)
    y_M = saturating(t, y0, dy, tau)
    n_Xe_M = saturating(t, n0, dn, tau)
    return t, run(t, y_M, n_Xe_M, N0, a_grain)


# --- Panels 1 & 2: reference trajectory -------------------------------------
dt_ref = tau / 20
t1, res1 = trajectories(dt_ref)

# --- Panel 3: relative error vs. time-step size ------------------------------
dt_ratios = [2.0, 1.0, 0.5, 0.2, 0.05, 0.01, 0.001]
rel_err = []
for r in dt_ratios:
    t_, res_ = trajectories(r * tau)
    err = error_norms(res_["N_M"], res_["N_N"])
    rel_err.append(err["Linf"] / N0)

# --- Panel 4: what a plausible coding defect would look like ---------------
def buggy_missing_self_term(y_old, k, increment):
    """Miscoding: y/(1+k*increment) -- drops the y_old inside the product."""
    return y_old / (1.0 + k * increment)


def buggy_wrong_coefficient(y_old, k, increment):
    """Miscoding: right formula, coefficient off by a factor of 2 (k=1 not 2)."""
    return y_old / (1.0 + (k / 2.0) * y_old * increment)


bug_variants = {
    "correct (as shipped)": None,
    "missing $y_{old}$ in denominator": buggy_missing_self_term,
    "coefficient off by 2x": buggy_wrong_coefficient,
}
bug_rel_err = {}
for label, fn in bug_variants.items():
    t_, res_ = trajectories(dt_ref)
    if fn is not None:
        y_M = saturating(t_, y0, dy, tau)
        n_Xe_M = saturating(t_, n0, dn, tau)
        res_ = run(t_, y_M, n_Xe_M, N0, a_grain, coalescence_step=fn)
    err = error_norms(res_["N_M"], res_["N_N"])
    bug_rel_err[label] = err["Linf"] / N0

# --- Figure ------------------------------------------------------------------
fig, axes = plt.subplots(2, 2, figsize=(11, 8.5))

ax = axes[0, 0]
ax.plot(t1 / 86400, res1["N_M"], "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured $N_M$")
ax.plot(t1 / 86400, res1["N_N"], marker="x", linestyle="none", markersize=5, color=plot_style.NUMERICAL, label="numerical (BinaryInteraction)")
ax.set_xlabel("$t$ [days]")
ax.set_ylabel(r"bubble concentration $N(t)$ [m$^{-2}$]")
ax2 = ax.twinx()
ax2.plot(t1 / 86400, res1["A_M"], "--", color=plot_style.LINE_CYCLE[3], linewidth=1.2, label="bubble area $A_M(t)$")
ax2.set_ylabel(r"bubble area $A(t)$ [m$^2$]", color=plot_style.LINE_CYCLE[3])
ax.set_title("Concentration (coalescence) vs. driving bubble area")
plot_style.inset_legend(ax, loc="center right", fontsize=8)

ax = axes[0, 1]
ax.plot(t1 / 86400, res1["swelling_M"], "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured")
ax.plot(t1 / 86400, res1["swelling_N"], marker="x", linestyle="none", markersize=5, color=plot_style.NUMERICAL, label="numerical")
ax.set_xlabel("$t$ [days]")
ax.set_ylabel("intergranular gas swelling [-]")
ax.set_title("Downstream: the eigenstrain OFFBEAT consumes")
ax.legend(fontsize=8.5)

ax = axes[1, 0]
ax.loglog(dt_ratios, rel_err, marker="o", color=plot_style.LINE_CYCLE[0])
plot_style.clean_log_ticks(ax, dt_ratios)
ax.set_xlabel(r"$\Delta t / \tau$")
ax.set_ylabel(r"relative error $|N_N-N_M|_\infty / N_0$")
ax.set_title("Error vs. step size: flat, unlike O($\\Delta t$) solvers elsewhere")

ax = axes[1, 1]
colors = [plot_style.ERROR_STYLE["L1"]["color"], plot_style.ERROR_STYLE["Linf"]["color"], plot_style.ERROR_STYLE["final"]["color"]]
ax.bar(range(len(bug_rel_err)), list(bug_rel_err.values()), color=colors)
ax.set_yscale("log")
ax.set_xticks(range(len(bug_rel_err)))
ax.set_xticklabels(list(bug_rel_err.keys()), fontsize=7.5, rotation=12)
ax.set_ylabel("relative error (same check)")
ax.set_title("What a plausible miscoding would look like")

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "coalescence_swelling"))

# --- Console report -----------------------------------------------------------
print("Case 12 - grain-boundary bubble coalescence (Solver::BinaryInteraction) + swelling chain")
print(f"  N0={N0:.1e} m^-2, a_grain={a_grain}, tau={tau/86400:.0f} days, k={INTERACTION_COEFFICIENT}")
print(f"  reference dt={dt_ref/86400:.1f} days: N error Linf={error_norms(res1['N_M'], res1['N_N'])['Linf']:.3e}"
      f"  swelling error Linf={error_norms(res1['swelling_M'], res1['swelling_N'])['Linf']:.3e}")
print("  relative error vs. dt/tau (flat -> exact regardless of step size):")
for r, e in zip(dt_ratios, rel_err):
    print(f"    dt/tau={r:.3f}  rel_err={e:.3e}")
print("  what a plausible miscoding would look like (same manufactured check):")
for label, e in bug_rel_err.items():
    print(f"    {label:34s}  rel_err={e:.3e}")
print(f"  plots written to {HERE}")
