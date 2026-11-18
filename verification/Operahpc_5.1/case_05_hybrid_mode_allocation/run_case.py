"""
Case 05 - Hybrid mode allocation for the OFFBEAT-SCIANTIX coupling.

This is the case reported in OperaHPC WP5-D5.1, Section 4.3, Table 5: SCIANTIX
is called by OFFBEAT at every spatial node and every time step, for 5 elements
(3 stable + 2 non-stable). Running all 5 at the default 40 spectral modes is
the accuracy baseline. The report proposes a *hybrid* allocation -- 20 modes
for the stable nuclides (Xe, Kr), 40 modes kept for the fast-decaying
non-stable ones (133-Xe, 85m-Kr) -- and reports it "preserves accuracy while
cutting SCIANTIX runtime by 20-50%", with relative errors against the
manufactured solution differing by about 0.05 percentage points between the
two allocations (Table 5).

This case reproduces that comparison using the same sinusoidal manufactured
solution as case_01/case_03 (Eq. 11-14) as the representative single-nuclide
verification problem -- since the error/runtime scaling with mode count
depends only on the diffusion geometry and time discretisation, not on which
isotope is being tracked, the same MMS stands in for each of the 5 elements.

Reported here, mirroring Table 5:
  - % relative error (Average/RMSE/Max/Final) at k=40 vs. the hybrid
    allocation (k=20 for the stable-nuclide role)
  - wall-clock runtime for one OFFBEAT-node call emulating 5 coupled species
    (all at 40 modes, vs. 3 at 20 + 2 at 40 modes)

Note: the report's own percentages come from a full reactor-representative
OFFBEAT run (non-smooth T(t), Fdot(t) histories); the absolute % values here
will differ since this case uses a single smooth manufactured time history at
a fixed dt -- what is reproduced is the qualitative finding: reducing modes
40 -> 20 for the stable-nuclide role costs a negligible fraction of a percent
of extra error, for a real reduction in per-node runtime.

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
from spectral_diffusion_mms import run, error_norms
import plot_style

plot_style.apply()
HERE = os.path.dirname(__file__)

# --- Manufactured solution, same representative case as case_01 ------------
a = 5e-6
alpha = 1.0
eps = 0.01
gamma = 1e-13

f = lambda t: alpha * np.sin(eps * t)
fprime = lambda t: alpha * eps * np.cos(eps * t)
D = lambda t: gamma * np.ones_like(t) if isinstance(t, np.ndarray) else gamma

dt = 5.0
t_end = 2000.0

N_STABLE_ELEMENTS = 3     # e.g. Xe, Kr, and one other stable species
N_NONSTABLE_ELEMENTS = 2  # e.g. 133-Xe, 85m-Kr
N_MODES_FULL = 40
N_MODES_HYBRID_STABLE = 20

# --- Relative error: full 40-mode vs. hybrid 20-mode (stable-nuclide role) -
rel_errors = {}
for label, n_modes in (("40 modes", N_MODES_FULL), ("Hybrid (20 modes)", N_MODES_HYBRID_STABLE)):
    t, C_M, C_N, X_N, k = run(f, fprime, D, a, n_modes, dt, t_end)
    norms = error_norms(C_M, C_N)
    scale = np.mean(np.abs(C_M))
    rel_errors[label] = {
        "Average": norms["L1"] / scale * 100.0,
        "RMSE": norms["L2"] / scale * 100.0,
        "Max": norms["Linf"] / scale * 100.0,
        "Final": norms["final"] / scale * 100.0,
    }

# --- Runtime: one OFFBEAT-node call, 5 coupled species -----------------------
def time_node_call(mode_list, repeats=9):
    times = []
    for _ in range(repeats):
        t0 = time.perf_counter()
        for n_modes in mode_list:
            run(f, fprime, D, a, n_modes, dt, t_end)
        times.append(time.perf_counter() - t0)
    return float(np.median(times))


modes_all40 = [N_MODES_FULL] * (N_STABLE_ELEMENTS + N_NONSTABLE_ELEMENTS)
modes_hybrid = [N_MODES_HYBRID_STABLE] * N_STABLE_ELEMENTS + [N_MODES_FULL] * N_NONSTABLE_ELEMENTS

runtime_all40 = time_node_call(modes_all40)
runtime_hybrid = time_node_call(modes_hybrid)
runtime_reduction_pct = (1.0 - runtime_hybrid / runtime_all40) * 100.0

# --- Plots -------------------------------------------------------------------
fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))

metrics = ["Average", "RMSE", "Max", "Final"]
x = np.arange(len(metrics))
width = 0.35
axes[0].bar(x - width / 2, [rel_errors["40 modes"][m] for m in metrics], width, label="40 modes", color=plot_style.LINE_CYCLE[0])
axes[0].bar(x + width / 2, [rel_errors["Hybrid (20 modes)"][m] for m in metrics], width, label="Hybrid (20 modes)", color=plot_style.LINE_CYCLE[1])
axes[0].set_xticks(x)
axes[0].set_xticklabels(metrics)
axes[0].set_ylabel("relative error [%]")
axes[0].set_title("Relative error: 40 modes vs. hybrid allocation")
axes[0].legend()

labels = ["All 5 species\nat 40 modes", "Hybrid\n(3$\\times$20 + 2$\\times$40)"]
axes[1].bar(labels, [runtime_all40 * 1e3, runtime_hybrid * 1e3], color=[plot_style.LINE_CYCLE[0], plot_style.LINE_CYCLE[1]])
axes[1].set_ylabel("runtime per node call [ms]")
axes[1].set_title(f"Runtime reduction: {runtime_reduction_pct:.1f}%")

fig.tight_layout()
plot_style.save(fig, os.path.join(HERE, "hybrid_mode_allocation"))

# --- Console report, mirroring Table 5 --------------------------------------
print("Case 05 - hybrid mode allocation (report Table 5 analogue)")
print(f"  a={a}, alpha={alpha}, eps={eps}, gamma={gamma}, dt={dt}, t_end={t_end}")
print(f"\n  {'% Relative error':<18s} {'40 modes':>12s} {'Hybrid approach':>18s}")
for m in metrics:
    print(f"  {m:<18s} {rel_errors['40 modes'][m]:>12.3f} {rel_errors['Hybrid (20 modes)'][m]:>18.3f}")
print(
    f"\n  runtime, 5-species node call: all-40 modes = {runtime_all40 * 1e3:.3f} ms, "
    f"hybrid = {runtime_hybrid * 1e3:.3f} ms  ->  {runtime_reduction_pct:.1f}% reduction"
)
print(f"  plots written to {HERE}")
