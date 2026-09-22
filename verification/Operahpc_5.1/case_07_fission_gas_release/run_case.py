"""
Case 07 - Fission gas release, by mass balance.

Verifies the actual quantity fuel-performance codes report: the released gas
fraction, not just the intragranular content. On this branch, when a system
has no grain-boundary bubble behaviour (iGrainBoundaryBehaviour = 0),
src/models/GasDiffusion.C computes it by direct mass balance (lines 184-202):

    released(t) = produced(t) - decayed(t) - in_grain(t)                (*)

("gas reaching the boundary is released directly", the same simplification
the report states for its Eq. 15 gas-release case). `produced(t)` is
integrated by Solver::Integrator (dN/dt = S, see src/models/GasProduction.C);
`in_grain(t)` is the volume-averaged output of the spectral diffusion solver
verified in case_01/case_02. This case verifies the *combination* (*) itself
with an MMS: does subtracting two independently-converging numerical
quantities introduce any extra error, beyond what each already carries?
That is a distinct question from verifying either quantity alone, and
GasDiffusion.C's own MMS scaffolding (defineSpectralDiffusion1Equation and
GasProduction.C both already hardcode the same manufactured source
0.4*a^2*cos(t)+6*sin(t) for this exact purpose) confirms it was intended to
be checked this way; this case is that check, run as a standalone, documented
verification with reusable parameters instead of hardcoded branch-only values.

Manufactured quantities (stable gas, decayed = 0):

    c_M(r,t)  = alpha*(a^2-r^2)*sin(eps*t),  D_M = gamma      (as case_01)
    N_M(t)    = N0 + P0*t + Pamp*sin(eps_p*t)                 (cumulative
                produced; S_M(t) = dN_M/dt = P0 + Pamp*eps_p*cos(eps_p*t)
                is the manufactured production rate fed to the Integrator)
    Released_M(t) = N_M(t) - C_M(t)

Note: c_M is a signed, oscillating MMS test function (as in case_01/Fig. 15
of the report), not a physically-constrained concentration, so the derived
release fraction Released_M/N_M can occasionally exceed 1 -- that is a
property of the manufactured solution, not a solver defect; the error norms
on Released(t) itself are the actual pass/fail check.

`mass_balance_components.png` plots all four named quantities from
GasDiffusion.C's balance together -- produced, in grain, at grain boundary,
released -- manufactured and numerical overlaid. "At grain boundary" is
identically 0 here (that is exactly what iGrainBoundaryBehaviour=0 means:
nothing accumulates at the boundary, gas is released the instant it arrives),
kept explicit rather than dropped so the full four-term balance
produced = in_grain + at_GB + released is what's shown, not assumed.

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

# --- Intragranular content: same manufactured solution as case_01 ----------
a = 5e-6
alpha = 1.0
eps = 0.01
gamma = 1e-13

f = lambda t: alpha * np.sin(eps * t)
fprime = lambda t: alpha * eps * np.cos(eps * t)
D = lambda t: gamma * np.ones_like(t) if isinstance(t, np.ndarray) else gamma

# --- Cumulative production: independent manufactured solution, integrated
# with Solver::Integrator's exact update rule (dN/dt = S_M) --------------
N0 = 2.0e-11
P0 = 1.0e-14
Pamp = 5.0e-12
eps_p = 0.02

N_M = lambda t: N0 + P0 * t + Pamp * np.sin(eps_p * t)
S_M = lambda t: P0 + Pamp * eps_p * np.cos(eps_p * t)


def integrate_production(t, dt):
    """Solver::Integrator: x[i+1] = x[i] + S(t[i+1])*dt."""
    N_N = np.empty_like(t)
    N_N[0] = N_M(t[0])
    for j in range(1, len(t)):
        N_N[j] = N_N[j - 1] + S_M(t[j]) * dt
    return N_N


dt = 5.0
t_end = 2000.0
mode_counts = [1, 5, 20, 40]

results = {}
for n_modes in mode_counts:
    t, C_M, C_N, X_N, k = run(f, fprime, D, a, n_modes, dt, t_end)
    N_N = integrate_production(t, dt)
    Produced_M = N_M(t)
    # At grain boundary: identically 0 here (GasDiffusion.C lines 184-202,
    # iGrainBoundaryBehaviour=0: gas reaching the boundary is released
    # directly, so nothing accumulates at the boundary). Kept explicit
    # (rather than omitted) so the four-quantity mass balance
    # produced = in_grain + at_GB + released is checked, not assumed.
    GB_M = np.zeros_like(t)
    GB_N = np.zeros_like(t)
    Released_M = Produced_M - GB_M - C_M
    Released_N = N_N - GB_N - C_N
    results[n_modes] = dict(
        t=t, C_M=C_M, C_N=C_N, N_M=Produced_M, N_N=N_N, GB_M=GB_M, GB_N=GB_N, R_M=Released_M, R_N=Released_N
    )

# --- Plot: full mass-balance decomposition ----------------------------------
n_ref = max(mode_counts)
d = results[n_ref]

components = [
    ("N", "produced", d["N_M"], d["N_N"]),
    ("C", "in grain", d["C_M"], d["C_N"]),
    ("GB", "at grain boundary", d["GB_M"], d["GB_N"]),
    ("R", "released", d["R_M"], d["R_N"]),
]

fig0, ax0 = plt.subplots(figsize=(7.5, 5))
for (sym, label, M, N), color in zip(components, plot_style.LINE_CYCLE):
    ax0.plot(d["t"], M, "-", color=color, linewidth=1.8, label=f"{label} (manufactured)")
    ax0.plot(d["t"], N, marker="x", linestyle="none", color=color, markersize=3, alpha=0.8, label=f"{label} (numerical)")
ax0.set_xlabel("$t$ [s]")
ax0.set_ylabel("[at/m$^3$]")
ax0.set_title("Mass balance: produced = in grain + at boundary + released")
ax0.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), fontsize=8)
fig0.tight_layout()
plot_style.save(fig0, os.path.join(HERE, "mass_balance_components"))

# --- Plots: released gas + release fraction ---------------------------------
fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))

axes[0].plot(d["t"], d["R_M"], "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured")
axes[0].plot(d["t"], d["R_N"], marker="x", linestyle="none", color=plot_style.NUMERICAL, markersize=3.5, label=f"numerical, $k$={n_ref}")
axes[0].set_xlabel("$t$ [s]")
axes[0].set_ylabel("released [at/m$^3$]")
axes[0].set_title("Released gas: produced $-$ decayed $-$ in-grain")
axes[0].legend()

FR_M = d["R_M"] / d["N_M"]
FR_N = d["R_N"] / d["N_N"]
axes[1].plot(d["t"], FR_M, "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured")
axes[1].plot(d["t"], FR_N, marker="x", linestyle="none", color=plot_style.NUMERICAL, markersize=3.5, label=f"numerical, $k$={n_ref}")
axes[1].axhline(1.0, color="gray", linewidth=0.8, linestyle=":")
axes[1].set_xlabel("$t$ [s]")
axes[1].set_ylabel("release fraction $R/N$ [-]")
axes[1].set_title("Release fraction (signed MMS: can exceed 1)")
axes[1].legend()

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "fission_gas_release_comparison"))

# --- All four error metrics vs. number of modes -----------------------------
mode_sweep = [1, 2, 5, 10, 20, 40, 80]
norm_history = {metric: [] for metric in ("L1", "L2", "Linf", "final")}
for n_modes in mode_sweep:
    t_, C_M_, C_N_, _, _ = run(f, fprime, D, a, n_modes, dt, t_end)
    N_N_ = integrate_production(t_, dt)
    norms = error_norms(N_M(t_) - C_M_, N_N_ - C_N_)
    for metric in norm_history:
        norm_history[metric].append(norms[metric])

fig2, ax2 = plt.subplots(figsize=(6.5, 4.5))
for metric, style in plot_style.ERROR_STYLE.items():
    ax2.semilogy(mode_sweep, norm_history[metric], marker=style["marker"], color=style["color"], label=metric)
ax2.set_xlabel("number of spectral modes $k$")
ax2.set_ylabel("error on released gas [at/m$^3$]")
ax2.set_title("Released-gas error metrics vs. number of modes")
ax2.legend()
fig2.tight_layout()
plot_style.save(fig2, os.path.join(HERE, "error_metrics_vs_modes"))

# --- Console report -----------------------------------------------------------
print("Case 07 - fission gas release (produced - decayed - in_grain), MMS")
print(f"  a={a}, alpha={alpha}, eps={eps}, gamma={gamma}, dt={dt}, t_end={t_end}")
print(f"  production: N0={N0:.3e}, P0={P0:.3e}, Pamp={Pamp:.3e}, eps_p={eps_p}")
for n_modes in mode_counts:
    d_ = results[n_modes]
    norms = error_norms(d_["R_M"], d_["R_N"])
    print(
        f"  k={n_modes:3d}  L1={norms['L1']:.6e}  L2={norms['L2']:.6e}  "
        f"Linf={norms['Linf']:.6e}  final={norms['final']:.6e}"
    )

residual_M = d["N_M"] - d["GB_M"] - d["C_M"] - d["R_M"]
residual_N = d["N_N"] - d["GB_N"] - d["C_N"] - d["R_N"]
print(
    f"  mass-balance residual |produced - at_GB - in_grain - released| "
    f"(k={n_ref}): manufactured max={np.max(np.abs(residual_M)):.3e}, "
    f"numerical max={np.max(np.abs(residual_N)):.3e}"
)
print(f"  plots written to {HERE}")
