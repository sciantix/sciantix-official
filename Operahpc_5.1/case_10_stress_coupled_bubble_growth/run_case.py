"""
Case 10 - Grain-boundary bubble growth driven by a manufactured hydrostatic
stress history (Solver::LimitedGrowth).

None of cases 01-09 touch `Solver::LimitedGrowth` (src/classes/Solver.C:22-25)
or the stress-dependent grain-boundary bubble model that uses it
(src/models/InterGranularBubbleBehavior.C:103-143, Pastore et al., 2013),
and none manufacture the hydrostatic stress that OFFBEAT actually passes to
SCIANTIX every call (offbeat-official,
offbeatLib/fissionGasRelease/fgrSCIANTIX.C:614-619, 652-653:
`Hydrostaticstress_input`) -- confirmed genuine two-way mechanical coupling:
OFFBEAT's stress state affects SCIANTIX's grain-boundary bubble growth,
whose gaseous swelling (fgrSCIANTIX.C:542,551,
InterGranularBubbleBehavior.C:293-296) feeds back into OFFBEAT's mechanics
as an eigenstrain.

The governing equation (backward-Euler, closed-form via the quadratic
formula in Solver::LimitedGrowth) is

    dy/dt = a/y + b(t),   a = growth_rate,   b(t) = equilibrium_term(t)

where, within one step, b(t) is evaluated from the previous step's bubble
radius and temperature (held fixed here, as in `common/limited_growth_mms.py`)
and the CURRENT hydrostatic stress:

    equilibrium_pressure(t) = 2*surface_tension/r_bubble - sigma_H(t)*1e6
    b(t) = -volume_flow_rate * equilibrium_pressure(t) / (kB * T)

This case manufactures y_M(t) = y0*(1 + eps*sin(omega*t)) (always positive),
derives the b_M(t) needed for the ODE to hold exactly, inverts it to the
manufactured stress sigma_H_M(t) that would produce it, round-trips that
stress forward through SCIANTIX's own equilibrium-pressure/term algebra
(exactly as InterGranularBubbleBehavior.C computes it from an OFFBEAT-
supplied stress), and advances y with `Solver::LimitedGrowth`'s own
closed-form update.

Representative constants: surface_tension=0.7 N/m, Schottky
volume=4.09e-29 m^3 (src/operations/SetMatrix.C:52,55, real SCIANTIX
defaults), grain_boundary_thickness=5e-10 m (SetMatrix.C:58),
grain-boundary vacancy diffusivity from the Reynolds & Burton (1979)
correlation SCIANTIX/OFFBEAT both default to
(src/classes/Matrix.C:63-69; offbeat-official
offbeatLib/fissionGasRelease/fgrSCIANTIX.H: `igrain_boundary_vacancy_
diffusion_coefficient 1`) at a representative fuel temperature T=1600 K.
Bubble radius, atoms-per-bubble and fractional coverage (which set
`growth_rate`/`volume_flow_rate`) are held at representative fixed values
rather than re-derived self-consistently -- this case verifies the
LimitedGrowth solver and the stress-to-equilibrium-term coupling, not the
bubble-radius/coverage sub-models.

Produces a 4-panel figure:
  - Top-left:    y_M(t) vs. y_N(t) (intergranular vacancies per bubble).
  - Top-right:   the manufactured hydrostatic stress sigma_H_M(t) -- the
                 actual coupling variable OFFBEAT would supply.
  - Bottom-left: temporal convergence (all four error metrics) vs. dt.
  - Bottom-right: final-time error vs. stress-oscillation amplitude --
                 how much a more strongly time-varying stress signal (a
                 faster PCMI transient, say) costs the solver in accuracy
                 at fixed dt.

Run: python3 run_case.py
"""

import os
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "common"))
from limited_growth_mms import run, error_norms, y_M as y_M_func
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

# --- Representative physical constants (real SCIANTIX defaults where noted)
kB = 1.380651e-23              # Boltzmann constant [J/K] (include/namespaces/Constants.h)
T = 1600.0                     # representative fuel temperature [K]
surface_tension = 0.7          # [N/m] (SetMatrix.C:52)
schottky_volume = 4.09e-29     # [m^3] (SetMatrix.C:55)
gb_thickness = 5.0e-10         # [m] (SetMatrix.C:58)
sink_strength = 0.4054         # low fractional-coverage limit (InterGranularBubbleBehavior.C:96-101)

D_gb = 6.9e-4 * np.exp(-5.35e-19 / (kB * T))          # Reynolds & Burton, 1979 (Matrix.C:65)
volume_flow_rate = 2.0 * np.pi * gb_thickness * D_gb * sink_strength

n_atoms_per_bubble = 50.0      # representative (InterGranularBubbleBehavior.C:107)
a = volume_flow_rate * n_atoms_per_bubble / schottky_volume   # growth_rate

r_bubble = 5e-9                # representative intergranular bubble radius [m]

y0 = 100.0
eps_amp = 0.3
omega = 2.0 * np.pi / 1000.0
dt = 80.0
t_end = 800.0

# --- Panel 1 & 2: reference run --------------------------------------------
t, Y_M, Y_N, sigma_H_M = run(a, y0, eps_amp, omega, dt, t_end, surface_tension, r_bubble, kB, T, volume_flow_rate)
ref_norms = error_norms(Y_M, Y_N)

# --- Panel 3: temporal convergence ------------------------------------------
dt_sweep = [80.0, 40.0, 20.0, 10.0, 5.0, 2.5]
conv_norms = {metric: [] for metric in ("L1", "L2", "Linf", "final")}
for dt_ in dt_sweep:
    _, Y_M_, Y_N_, _ = run(a, y0, eps_amp, omega, dt_, t_end, surface_tension, r_bubble, kB, T, volume_flow_rate)
    norms = error_norms(Y_M_, Y_N_)
    for metric in conv_norms:
        conv_norms[metric].append(norms[metric])
order_L2 = np.log(conv_norms["L2"][0] / conv_norms["L2"][-1]) / np.log(dt_sweep[0] / dt_sweep[-1])

# --- Panel 4: sensitivity to stress-oscillation amplitude ------------------
eps_sweep = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4]
final_err_vs_eps = []
stress_amp_vs_eps = []
for eps_ in eps_sweep:
    _, Y_M_, Y_N_, sigma_ = run(a, y0, eps_, omega, dt, t_end, surface_tension, r_bubble, kB, T, volume_flow_rate)
    final_err_vs_eps.append(error_norms(Y_M_, Y_N_)["final"])
    stress_amp_vs_eps.append((sigma_.max() - sigma_.min()) / 2.0)

# --- Figure ------------------------------------------------------------------
fig, axes = plt.subplots(2, 2, figsize=(11, 8.5))

ax = axes[0, 0]
ax.plot(t, Y_M, "-", color=plot_style.MANUFACTURED, linewidth=1.8, label="manufactured", zorder=0)
ax.plot(t, Y_N, marker="x", linestyle="none", markersize=5, color=plot_style.NUMERICAL, label="numerical (LimitedGrowth)")
ax.set_xlabel("$t$ [s]")
ax.set_ylabel("intergranular vacancies per bubble $y(t)$")
ax.set_title("Manufactured vs. numerical solution")
ax.legend(fontsize=8.5)

ax = axes[0, 1]
ax.plot(t, sigma_H_M, "-o", color=plot_style.LINE_CYCLE[3], markersize=4, linewidth=1.4)
ax.axhline(0.0, color="gray", linewidth=0.8, linestyle=":")
ax.set_xlabel("$t$ [s]")
ax.set_ylabel(r"$\sigma_H^M(t)$ [MPa]")
ax.set_title("Manufactured hydrostatic stress (the coupling variable)")

ax = axes[1, 0]
for metric, style in plot_style.ERROR_STYLE.items():
    ax.loglog(dt_sweep, conv_norms[metric], marker=style["marker"], color=style["color"], label=metric)
plot_style.clean_log_ticks(ax, dt_sweep)
ax.set_xlabel("$dt$ [s]")
ax.set_ylabel("error [vacancies/bubble]")
ax.set_title(f"Temporal convergence (observed order $\\approx${order_L2:.2f})")
ax.legend(fontsize=8.5)

ax = axes[1, 1]
ax2 = ax.twiny()
ax.semilogy(eps_sweep, final_err_vs_eps, marker="^", color=plot_style.ERROR_STYLE["final"]["color"])
ax.set_xlabel("stress-oscillation amplitude parameter $\\epsilon$")
ax.set_ylabel("final-time error [vacancies/bubble]")
ax.set_title("Error vs. how strongly the stress input varies")
ax2.set_xlim(ax.get_xlim())
ax2.set_xticks(eps_sweep[::2])
ax2.set_xticklabels([f"{s:.0f}" for s in stress_amp_vs_eps[::2]])
ax2.set_xlabel(r"stress oscillation amplitude [MPa]")

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "stress_coupled_bubble_growth"))

# --- Console report -----------------------------------------------------------
print("Case 10 - stress-coupled grain-boundary bubble growth (Solver::LimitedGrowth)")
print(f"  T={T} K, growth_rate a={a:.4e}, volume_flow_rate={volume_flow_rate:.4e}, y0={y0}, eps={eps_amp}, dt={dt}, t_end={t_end}")
print(f"  manufactured stress range: [{sigma_H_M.min():.2f}, {sigma_H_M.max():.2f}] MPa")
print(f"  reference run errors: L1={ref_norms['L1']:.6e}  L2={ref_norms['L2']:.6e}  Linf={ref_norms['Linf']:.6e}  final={ref_norms['final']:.6e}")
print(f"  observed order of convergence (L2, dt {dt_sweep[0]}->{dt_sweep[-1]}): {order_L2:.3f}")
print("  final-time error vs. stress-oscillation amplitude:")
for eps_, err, samp in zip(eps_sweep, final_err_vs_eps, stress_amp_vs_eps):
    print(f"    eps={eps_:.2f}  stress_amplitude={samp:6.1f} MPa  final_err={err:.3e}")
print(f"  plots written to {HERE}")
