"""
Dislocation density correlation rho_d(B, T) used by iCoarseningDislocationDensity = 1
(src/models/IntraGranularBubbleBehavior.C, coarseningDislocationDensityVeshchunov2009).

Source: Veshchunov & Shestak, J. Nucl. Mater. 384 (2009) 12-18. The curves are MFPR model calculations,
not measurements:
  - Fig. 4, base parameter set, 900-1400 K. The density is "rather insensitive to temperature" up to 1300 K
    and drops to a flat ~8e12 m-2 at 1400 K (the authors call the drop "somewhat overestimated");
  - Fig. 5, modified parameter set, 900-1500 K, smooth decrease with temperature.
No calculations above 1500 K ("were not attempted").

The curves were extracted from the vector paths of the PDF (validation/VS2009_dislocation_fit, ex2.py) and
sampled every 5 GWd/tHM from 5 to 100 (VS2009_fig4_grid.csv, VS2009_fig5_grid.csv). The previous version of
this script fitted 19 hand-digitised points whose labels did not match Fig. 4 (the "1100 K" points were the
1000 K curve, the "1400 K" points nearly duplicated 1200 K); that fit is Eq. (15) of the Nicodemo manuscript.

Model: rho(B, T) = A * B^n * [g + (1 - g) / (1 + exp((T - Tc) / dT))], B in GWd/tHM, fit in log space.
SCIANTIX uses the Fig. 5 coefficients, holds T at 1500 K above it, and converts MWd/kgUO2 to GWd/tHM (/0.8815).
The A*B^n*f(T) form cannot represent the flat 1400 K curve of Fig. 4: the Fig. 4 fit is limited to <= 1300 K.

Usage: python3 dd_fit.py   (writes dislocation_density_fit.png next to this script)
"""
import os
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

HERE = os.path.dirname(os.path.abspath(__file__))


def load(name):
    d = np.genfromtxt(os.path.join(HERE, name), delimiter=",", names=True)
    return d["burnup_GWd_t"], d["T_K"], d["rho_d_m2"]


def rho(X, A, n, g, Tc, dT):
    B, T = X
    return A * B ** n * (g + (1.0 - g) / (1.0 + np.exp((T - Tc) / dT)))


def fit(B, T, R):
    p, _ = curve_fit(lambda X, lA, n, g, Tc, dT: np.log(rho(X, np.exp(lA), n, g, Tc, dT)), (B, T), np.log(R),
                     p0=[np.log(6.5e12), 1.1, 0.5, 1100, 50],
                     bounds=([20, 0.3, 1e-4, 900, 1], [40, 2, 1, 1600, 300]), maxfev=20000)
    p[0] = np.exp(p[0])
    return p


B4, T4, R4 = load("VS2009_fig4_grid.csv")
B5, T5, R5 = load("VS2009_fig5_grid.csv")
m4 = T4 <= 1300
fits = {"Fig. 5 (all T, used in SCIANTIX)": (fit(B5, T5, R5), B5, T5, R5),
        "Fig. 4 (T <= 1300 K)": (fit(B4[m4], T4[m4], R4[m4]), B4, T4, R4)}

for name, (p, B, T, R) in fits.items():
    ratio = rho((B, T), *p) / R
    print(f"{name}: A={p[0]:.4g} n={p[1]:.4f} g={p[2]:.4g} Tc={p[3]:.1f} dT={p[4]:.2f}")
    for t in np.unique(T):
        r = ratio[T == t]
        print(f"   {t:.0f} K: model/figure {r.min():.2f}-{r.max():.2f}")

eq15 = (6.545e12, 1.151, 0.608, 1109.0, 25.8)  # previous fit, Nicodemo manuscript Eq. (15)
fig, axs = plt.subplots(1, 2, figsize=(12, 5), sharey=True)
bb = np.linspace(3, 100, 200)
for ax, (name, (p, B, T, R)) in zip(axs, fits.items()):
    for t in np.unique(T):
        m = T == t
        line, = ax.semilogy(B[m], R[m], "o", ms=3, label=f"{t:.0f} K")
        ax.semilogy(bb, rho((bb, np.full_like(bb, min(t, 1500))), *p), "-", lw=1, color=line.get_color())
        ax.semilogy(bb, rho((bb, np.full_like(bb, t)), *eq15), ":", lw=1, color=line.get_color())
    ax.set(title=f"{name}\nsymbols: figure, solid: fit, dotted: previous Eq. (15)", xlabel="Burnup (GWd/tHM)",
           xlim=(0, 100), ylim=(5e12, 3e15))
    ax.title.set_fontsize(9)
    ax.grid(alpha=0.3)
axs[0].set_ylabel(r"Dislocation density (m$^{-2}$)")
axs[1].legend(fontsize=8)
plt.tight_layout()
plt.savefig(os.path.join(HERE, "dislocation_density_fit.png"), dpi=130)
