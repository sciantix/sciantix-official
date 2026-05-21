"""
Calibration of KJMA(bu or bu_eff) models for HBS formation on PIE data.

Currently active:
    - KJMA(bu_eff), Barani et al. 2022 original
    - KJMA(bu_eff), Barani et al. 2022 + incubation burnup

Commented out:
    - KJMA(rho_d) fit on dislocation density correlation
    - Temperature-dependence plots

Reference data:
    PIE data from Gerczak 2018 / Noirot 2015, rim positions (T ~ 900 K).

@author G. Zullo
"""

import os
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

plt.rcParams.update({
    "font.size": 12,
    "axes.labelsize": 14,
    "xtick.labelsize": 12,
    "ytick.labelsize": 12,
    "legend.fontsize": 10.5,
    "lines.linewidth": 2,
    "axes.grid": True,
    "grid.linestyle": "--",
    "grid.linewidth": 0.5,
    "grid.alpha": 0.6,
})

# ============================================================================
# Dislocation density correlation (fit on Veshchunov 2009, Fig. 4)
# --------------------------------------------------------------------------
# NOTE (2026): the dislocation-density-driven KJMA approach is kept in the
# source for future work but is NOT included in the current manuscript.
# The functions below are left defined so the calibration can be re-enabled
# by flipping the `RUN_RHO_FIT` switch further down. See the commented-out
# plotting blocks at the bottom of the file.
# ============================================================================
A_DISL = 6.545e12
N_DISL = 1.151
A_INF  = 0.608
TC_SIG = 1109.0   # K
DT_SIG = 25.8     # K

def rho_d(bu, T):
    """Dislocation density (m^-2) as a function of local burnup and temperature."""
    fT = A_INF + (1.0 - A_INF) / (1.0 + np.exp((T - TC_SIG) / DT_SIG))
    return A_DISL * np.power(np.maximum(bu, 1e-12), N_DISL) * fT

# ============================================================================
# KJMA driven by dislocation density (dimensionless form) - kept for future work
# ============================================================================
RHO_CRIT  = 6.0e14   # m^-2, HBS nucleation threshold (Veshchunov 2009)
RHO_SCALE = 1.0e15   # m^-2, normalization

def kjma_rho(rho, K_rho, gamma_rho):
    xi = np.maximum((rho - RHO_CRIT) / RHO_SCALE, 0.0)
    return 1.0 - np.exp(-K_rho * np.power(xi, gamma_rho))

def kjma_rho_of_bu(bu, T, K_rho, gamma_rho):
    return kjma_rho(rho_d(bu, T), K_rho, gamma_rho)

# ============================================================================
# Reference KJMA in burnup space
# ============================================================================
K_BA     = 2.77e-7
GAMMA_BA = 3.54
BU_INC   = 15.0  # MWd/kgU

def kjma_bu_shifted(bu):
    return 1.0 - np.exp(-K_BA * np.power(np.maximum(bu - BU_INC, 0.0), GAMMA_BA))

def kjma_bu_barani(bu):
    """Original Barani 2022 KJMA without incubation burnup."""
    return 1.0 - np.exp(-K_BA * np.power(np.maximum(bu, 0.0), GAMMA_BA))

# ============================================================================
# PIE data (Gerczak 2018 / Noirot 2015)
# ============================================================================
bu_exp    = np.array([64.32, 70.91, 72.27, 77.05, 83.86, 88.41, 90.68, 129.77])
alpha_exp = np.array([0.2687, 0.5438, 0.5534, 0.5979, 0.6211, 0.6869, 0.7566, 1.0002])
T_PIE     = 900.0  # K, assumed rim temperature for all points

# ============================================================================
# Calibration switch
# ----------------------------------------------------------------------------
# Set RUN_RHO_FIT = True to re-enable the KJMA(rho_d) direct fit against PIE
# data (requires also uncommenting the plotting blocks below).
# ============================================================================
RUN_RHO_FIT = False

# -- KJMA(rho_d) calibration: kept for future work -----------------------------
# rho_exp = rho_d(bu_exp, T_PIE)
#
# def fit_func(rho, log_K, gamma):
#     # log_K to enforce K > 0 and improve conditioning
#     return kjma_rho(rho, np.exp(log_K), gamma)
#
# p0 = [np.log(5.0), 2.5]
# bounds = ([np.log(1e-3), 0.5], [np.log(1e3), 8.0])
#
# if RUN_RHO_FIT:
#     popt, pcov = curve_fit(fit_func, rho_exp, alpha_exp, p0=p0, bounds=bounds, maxfev=20000)
#     K_rho = np.exp(popt[0])
#     gamma_rho = popt[1]
#     perr = np.sqrt(np.diag(pcov))
# else:
#     # Frozen values from previous calibration, reported here for reference
#     K_rho = 2.5971
#     gamma_rho = 1.1043
#     perr = np.array([0.169, 0.167])

# ============================================================================
# Goodness of fit
# ============================================================================
def rmse(obs, pred):
    return np.sqrt(np.mean((obs - pred) ** 2))

rmse_ref    = rmse(alpha_exp, kjma_bu_shifted(bu_exp))
rmse_barani = rmse(alpha_exp, kjma_bu_barani(bu_exp))

# -- KJMA(rho_d) goodness of fit: kept for future work ------------------------
# rmse_new = rmse(alpha_exp, kjma_rho(rho_exp, K_rho, gamma_rho))

print("=" * 74)
print("Calibration of KJMA(bu_eff) models on PIE data")
print("=" * 74)
print("  RMSE on PIE data:")
print(f"    KJMA(bu_eff): Barani et al. 2022 original: {rmse_barani:.4f}")
print(f"    KJMA(bu_eff): Barani et al. 2022 + bu_inc=15:   {rmse_ref:.4f}")
# print(f"    KJMA(rho_d) this work:         {rmse_new:.4f}   (disabled)")
print("=" * 74)

# -- Parameters summary of the rho_d fit: kept for future work ----------------
# print()
# print(f"  [rho_d fit, currently disabled]")
# print(f"  K_rho     = {K_rho:.4f}     (stderr on log K: {perr[0]:.3f})")
# print(f"  gamma_rho = {gamma_rho:.4f}     (stderr: {perr[1]:.3f})")
# print(f"  rho_crit  = {RHO_CRIT:.2e} m^-2   (fixed)")
# print(f"  rho_scale = {RHO_SCALE:.2e} m^-2   (fixed)")

# ============================================================================
# Plot 1: f_HBS vs burnup at T = 900 K
# ============================================================================
fig, ax = plt.subplots(figsize=(9.5, 6.2))

bu_plot = np.linspace(0, 160, 600)

ax.scatter(bu_exp, alpha_exp,
           color="black", s=60, zorder=5, edgecolors="black", linewidth=0.8,
           label="PIE data (Gerczak 2018 / Noirot 2015)")

ax.plot(bu_plot, kjma_bu_barani(bu_plot),
        ":", color="#1f77b4", linewidth=2.0,
        label="KJMA: Barani et al. 2022 (original)")

ax.plot(bu_plot, kjma_bu_shifted(bu_plot),
        "-", color="#2ca02c", linewidth=2.2,
        label=f"KJMA: Barani et al. 2022 + $bu_{{inc}}={BU_INC:.0f} MWd/kgHM$")

# -- KJMA(rho_d) curve: kept for future work ---------------------------------
# ax.plot(bu_plot, kjma_rho_of_bu(bu_plot, T_PIE, K_rho, gamma_rho),
#         "--", color="#d62728", linewidth=2.4,
#         label=(r"KJMA($\rho_d$): this work"
#                f" ($K_\\rho$={K_rho:.2f}, $\\gamma_\\rho$={gamma_rho:.2f})"))

ax.annotate(
    f"RMSE on PIE:\n"
    f"KJMA: Barani et al. 2022 original: {rmse_barani:.3f}\n"
    f"KJMA: Barani et al. 2022 + bu_inc: {rmse_ref:.3f}",
    # f"  KJMA(rho_d) this work:    {rmse_new:.3f}",  # kept for future work
    xy=(0.97, 0.04), xycoords="axes fraction",
    ha="right", va="bottom", fontsize=10, fontfamily="monospace",
    bbox=dict(boxstyle="round,pad=0.4", fc="white", ec="gray", alpha=0.9),
)

ax.set_xlabel("Effective burnup (MWd/kgHM)")
ax.set_ylabel("Restructured volume fraction (/)")
ax.set_xlim(0, 160)
ax.set_ylim(-0.02, 1.08)
ax.legend(loc="center right", framealpha=0.95)

fig.tight_layout()
fig.savefig("kjma_rho_fit.png", dpi=200, bbox_inches="tight")

# ============================================================================
# Plot 2: temperature dependence of KJMA(rho_d) - kept for future work
# ----------------------------------------------------------------------------
# This plot shows (left) the emergent T dependence of f_HBS(bu) via rho_d,
# and (right) the universal curve f_HBS(rho_d). It is disabled because the
# rho_d approach is not part of the current manuscript.
# ============================================================================
# fig, axes = plt.subplots(1, 2, figsize=(14, 5.8))
#
# # Panel 1: f_HBS vs bu at multiple T
# ax = axes[0]
# temperatures = [900, 1100, 1200, 1300, 1400]
# cmap = plt.cm.plasma_r
# colors = cmap(np.linspace(0.15, 0.85, len(temperatures)))
#
# for T, c in zip(temperatures, colors):
#     ax.plot(bu_plot, kjma_rho_of_bu(bu_plot, T, K_rho, gamma_rho),
#             "-", color=c, linewidth=2.0, label=f"T = {T} K")
#
# ax.plot(bu_plot, kjma_bu_shifted(bu_plot),
#         "--", color="black", linewidth=1.5, alpha=0.7,
#         label="KJMA(bu) - no T dep.")
#
# ax.set_xlabel("Local burnup (MWd/kgHM)")
# ax.set_ylabel("Restructured volume fraction (/)")
# ax.set_xlim(0, 160)
# ax.set_ylim(-0.02, 1.08)
# ax.set_title(r"Emergent T dependence of KJMA($\rho_d$)")
# ax.legend(loc="center right", framealpha=0.95, fontsize=10)
#
# # Panel 2: universal curve f_HBS vs rho_d
# ax = axes[1]
# rho_plot = np.logspace(np.log10(2e14), np.log10(5e15), 400)
#
# ax.plot(rho_plot, kjma_rho(rho_plot, K_rho, gamma_rho),
#         "--", color="#d62728", linewidth=2.4,
#         label=rf"$K_\rho$={K_rho:.2f}, $\gamma_\rho$={gamma_rho:.2f}")
#
# ax.scatter(rho_exp, alpha_exp, color="black", s=55, zorder=5,
#            edgecolors="black", linewidth=0.8, label="PIE data (T=900 K)")
#
# ax.axvline(RHO_CRIT, color="gray", linestyle=":", linewidth=1.2, alpha=0.7)
# ax.text(RHO_CRIT * 1.05, 0.05, r"$\rho_{crit}$", color="gray")
#
# ax.set_xscale("log")
# ax.set_xlabel(r"Dislocation density $\rho_d$ (m$^{-2}$)")
# ax.set_ylabel("Restructured volume fraction (/)")
# ax.set_xlim(2e14, 5e15)
# ax.set_ylim(-0.02, 1.08)
# ax.set_title(r"Universal curve $f_{HBS}(\rho_d)$")
# ax.legend(loc="lower right", framealpha=0.95, fontsize=11)
#
# fig.tight_layout()
# fig.savefig(os.path.join(MAIN_DIRECTORY, "kjma_rho_T_dependence.png"),
#             dpi=200, bbox_inches="tight")
# plt.close()

# ============================================================================
# Summary table: f_HBS at bu_local = 75 MWd/kgHM for various T
# ----------------------------------------------------------------------------
# Disabled because it reports values from the rho_d-driven model.
# ============================================================================
# print()
# print("=" * 74)
# print("f_HBS at bu_local = 75 MWd/kgHM, for various T (this calibration)")
# print("=" * 74)
# print(f"  {'T (K)':<10}{'rho_d (m^-2)':<18}{'f_HBS':<12}{'KJMA(bu) ref.':<15}")
# print("-" * 74)
# f_ref = kjma_bu_shifted(75.0)
# for T in [800, 900, 1000, 1100, 1200, 1300, 1400]:
#     r = rho_d(75.0, T)
#     f = kjma_rho(r, K_rho, gamma_rho)
#     print(f"  {T:<10.0f}{r:<18.3e}{f:<12.4f}{f_ref:<15.4f}")

print()
print("Saved: kjma_rho_fit.png")