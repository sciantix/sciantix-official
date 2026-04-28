"""
Dislocation density correlation vs burnup and temperature.
Data extracted from Fig. 4 of Veshchunov & Shestak (2009),
J. Nucl. Mater. 384, 12-18.

Model C (power-law in burnup + sigmoid in T):
    rho(bu, T) = A * bu^n * [A_inf + (1 - A_inf) / (1 + exp((T - Tc) / dT))]

    - bu^n captures the near-linear growth of dislocation density with burnup
      (the physical saturation of the Veshchunov model is well above the
      experimental data range 0-100 GWd/t, so the data stay in the ascending
      regime).
    - The sigmoid in T reproduces the smooth transition from the low-T plateau
      (~900 K) to the high-T plateau observed in the data (~1200-1400 K).

Fit is performed on ALL datasets (900, 1100, 1200, 1400 K) in log-space
to give uniform weight across decades of dislocation density.
"""

import os
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

# ====================================================================
# Matplotlib style
# ====================================================================
plt.rcParams.update({
    "font.size": 12,
    "axes.labelsize": 14,
    "axes.titlesize": 16,
    "xtick.labelsize": 12,
    "ytick.labelsize": 12,
    "legend.fontsize": 11,
    "figure.titlesize": 16,
    "lines.linewidth": 2,
    "lines.markersize": 6,
    "axes.grid": True
})

MAIN_DIRECTORY = '/home/giovanni/research-manuscripts/Zullo_et_al__HBS/Images/'

# ====================================================================
# Input data: Dislocation Density vs Burn-up
# Extracted from Fig. 4 of Veshchunov & Shestak (2009)
# ====================================================================

# --- 900 K ---
burnup_900K = np.array([
    2.8946284961435964,
    13.004144594186585,
    32.622622347871435,
    52.863612658853235,
    72.92674223917876,
    92.60999643180631
])
density_900K = np.array([
    2.4051268092831812e13,
    1.1598242154108258e14,
    3.4575605679079175e14,
    6.189348504806831e14,
    9.224638569970858e14,
    1.270892779387564e15
])
temperature_900K = np.full_like(burnup_900K, 900.0)

# --- 1100 K ---
burnup_1100K = np.array([
    3.281091318310324,
    19.74309005571871,
    39.76559712348695,
    59.823237175088515,
    79.88856256690363
])
density_1100K = np.array([
    2.4054308390999836e13,
    1.550063209589349e14,
    3.7486537913178444e14,
    5.964692589619128e14,
    8.660224753091009e14
])
temperature_1100K = np.full_like(burnup_1100K, 1100.0)

# --- 1200 K ---
burnup_1200K = np.array([
    3.692805972607246,
    25.34460516564652,
    53.48173359317102,
    81.06661543106526
])
density_1200K = np.array([
    17806129952100.39,
    159407486736836.88,
    391623822584971.6,
    693594420834459.6
])
temperature_1200K = np.full_like(burnup_1200K, 1200.0)

# --- 1400 K ---
burnup_1400K = np.array([
    3.693903878352039,
    25.540032388219466,
    53.28960008783246,
    81.2609447478934
])
density_1400K = np.array([
    1.7574698220424674e13,
    1.5530049422198353e14,
    3.8650933929277044e14,
    6.846228286008604e14
])
temperature_1400K = np.full_like(burnup_1400K, 1400.0)

# ====================================================================
# Combine ALL data for the fit
# ====================================================================
burnup_all = np.concatenate([burnup_900K, burnup_1100K, burnup_1200K, burnup_1400K])
temperature_all = np.concatenate([temperature_900K, temperature_1100K,
                                  temperature_1200K, temperature_1400K])
density_all = np.concatenate([density_900K, density_1100K, density_1200K, density_1400K])

# ====================================================================
# Model definition
# rho(bu, T) = A * bu^n * f(T)
# f(T) = A_inf + (1 - A_inf) / (1 + exp((T - Tc) / dT))
# ====================================================================
def dislocation_density(X, A, n, A_inf, Tc, dT):
    bu, T = X
    fT = A_inf + (1.0 - A_inf) / (1.0 + np.exp((T - Tc) / dT))
    return A * np.power(bu, n) * fT

def dislocation_density_log(X, A, n, A_inf, Tc, dT):
    """Model evaluated in log10 space for uniform weight across decades."""
    return np.log10(dislocation_density(X, A, n, A_inf, Tc, dT))

# ====================================================================
# Curve fitting (log-space) with physical bounds
# ====================================================================
#   A:     prefactor of the power law
#   n:     burnup exponent (expected ~1.1-1.2 from log-log slope analysis)
#   A_inf: residual fraction at high T (>0 so rho never goes to zero)
#   Tc:    half-transition temperature
#   dT:    transition width
bounds = ([1e11, 0.8, 0.1, 900.0, 5.0],
          [1e14, 2.0, 0.95, 1400.0, 300.0])
p0 = [6e12, 1.15, 0.55, 1100.0, 50.0]

popt, pcov = curve_fit(
    dislocation_density_log,
    (burnup_all, temperature_all),
    np.log10(density_all),
    p0=p0, bounds=bounds, maxfev=30000
)
A_fit, n_fit, Ainf_fit, Tc_fit, dT_fit = popt
perr = np.sqrt(np.diag(pcov))

# Goodness of fit
pred_all = dislocation_density((burnup_all, temperature_all), *popt)
rmse_log = np.sqrt(np.mean((np.log10(pred_all) - np.log10(density_all)) ** 2))

# ====================================================================
# Print fitted parameters
# ====================================================================
print("=" * 72)
print("Dislocation density correlation - fit on ALL data (900, 1100, 1200, 1400 K)")
print("  rho(bu, T) = A * bu^n * [A_inf + (1 - A_inf) / (1 + exp((T - Tc)/dT))]")
print("=" * 72)
print(f"  A     = ({A_fit:.3e} +/- {perr[0]:.2e})")
print(f"  n     = ({n_fit:.3f} +/- {perr[1]:.3f})   burnup exponent")
print(f"  A_inf = ({Ainf_fit:.3f} +/- {perr[2]:.3f})       high-T residual fraction")
print(f"  Tc    = ({Tc_fit:.1f} +/- {perr[3]:.1f}) K      half-transition temperature")
print(f"  dT    = ({dT_fit:.1f} +/- {perr[4]:.1f}) K       transition width")
print(f"  RMSE(log10) = {rmse_log:.4f}  (typical error factor {10**rmse_log:.3f}x)")

# Per-temperature residuals
print()
print(f"  Per-T mean |log10(pred/obs)|:")
for bu_arr, dens_arr, T in [
    (burnup_900K, density_900K, 900.0),
    (burnup_1100K, density_1100K, 1100.0),
    (burnup_1200K, density_1200K, 1200.0),
    (burnup_1400K, density_1400K, 1400.0),
]:
    T_arr = np.full_like(bu_arr, T)
    pred = dislocation_density((bu_arr, T_arr), *popt)
    res = np.mean(np.abs(np.log10(pred) - np.log10(dens_arr)))
    print(f"    T = {T:.0f} K -> {res:.4f}")

# ====================================================================
# Plot 1: rho vs burnup at all temperatures
# ====================================================================
burnup_fit = np.linspace(0.5, 100, 300)
fit_900K  = dislocation_density((burnup_fit, np.full_like(burnup_fit, 900.0)),  *popt)
fit_1100K = dislocation_density((burnup_fit, np.full_like(burnup_fit, 1100.0)), *popt)
fit_1200K = dislocation_density((burnup_fit, np.full_like(burnup_fit, 1200.0)), *popt)
fit_1400K = dislocation_density((burnup_fit, np.full_like(burnup_fit, 1400.0)), *popt)

plt.figure(figsize=(9, 6))
plt.scatter(burnup_900K,  density_900K,  color="blue",   label="Data (900 K)")
plt.scatter(burnup_1100K, density_1100K, color="orange", label="Data (1100 K)")
plt.scatter(burnup_1200K, density_1200K, color="red",    label="Data (1200 K)")
plt.scatter(burnup_1400K, density_1400K, color="green",  label="Data (1400 K)")

plt.plot(burnup_fit, fit_900K,  "--", color="blue",   label="Fit (900 K)")
plt.plot(burnup_fit, fit_1100K, "--", color="orange", label="Fit (1100 K)")
plt.plot(burnup_fit, fit_1200K, "--", color="red",    label="Fit (1200 K)")
plt.plot(burnup_fit, fit_1400K, "--", color="green",  label="Fit (1400 K)")

# HBS formation range (Veshchunov et al., 2009, Fig. 3)
plt.axhline(y=6e14, color="gray", linestyle=":",  linewidth=1.5)
plt.axhline(y=1e15, color="gray", linestyle="--", linewidth=1.5)
plt.fill_between(burnup_fit, 6e14, 1e15, color="gray", alpha=0.2,
                 label="HBS formation range")

plt.xlabel("Effective burnup (MWd/kgHM)")
plt.ylabel(r"Dislocation density (m$^{-2}$)")
plt.yscale("log")
plt.ylim(5e12, 3e15)
plt.grid(True, which="both", alpha=0.4)
plt.legend(loc="lower right", ncol=2)
plt.tight_layout()
plt.savefig(os.path.join(MAIN_DIRECTORY, "dislocation_density_fit.png"), dpi=130)
plt.close()

# ====================================================================
# Plot 2: 3D surface rho(bu, T)
# ====================================================================
burnup_vals = np.linspace(0.5, 100, 100)
temperature_vals = np.linspace(900, 1400, 100)
B, T = np.meshgrid(burnup_vals, temperature_vals)
RHO = dislocation_density((B, T), *popt)
log_RHO = np.log10(RHO)

fig = plt.figure(figsize=(11, 7))
ax = fig.add_subplot(111, projection="3d")
surf = ax.plot_surface(B, T, log_RHO, cmap="viridis",
                       edgecolor="none", alpha=0.9)

# Experimental points
ax.scatter(burnup_900K,  temperature_900K,  np.log10(density_900K),
           color="blue",   edgecolor="k", s=40, label="900 K")
ax.scatter(burnup_1100K, temperature_1100K, np.log10(density_1100K),
           color="orange", edgecolor="k", s=40, label="1100 K")
ax.scatter(burnup_1200K, temperature_1200K, np.log10(density_1200K),
           color="red",    edgecolor="k", s=40, label="1200 K")
ax.scatter(burnup_1400K, temperature_1400K, np.log10(density_1400K),
           color="green",  edgecolor="k", s=40, label="1400 K")

# HBS thresholds as contour lines on the surface
ax.contour(B, T, log_RHO, levels=[np.log10(6e14)],
           colors="red",  linewidths=2, linestyles="solid")
ax.contour(B, T, log_RHO, levels=[np.log10(1e15)],
           colors="blue", linewidths=2, linestyles="dashed")

ax.set_xlabel("Effective burnup (MWd/kgHM)")
ax.set_ylabel("Temperature (K)")
ax.set_zlabel(r"log$_{10}$($\rho_d$ m$^{-2}$)", labelpad=12)
ax.legend(loc="upper left")
fig.colorbar(surf, ax=ax, shrink=0.5, aspect=10,
             label=r"log$_{10}$($\rho_d$ m$^{-2}$)")

plt.tight_layout()
plt.savefig(os.path.join(MAIN_DIRECTORY, "dislocation_density_surface.png"), dpi=130)
plt.close()