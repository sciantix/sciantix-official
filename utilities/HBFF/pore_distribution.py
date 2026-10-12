"""
HBS pore classes from the SCIANTIX moments, rupture per class, and cases for the micromechanics (MMM).
@ author: E.Cappellari, POLIMI-CEA 2026
@ date:   October 2026

Python transcription of iHighBurnupStructureFragmentation = 4 (src/models/HighBurnupStructureFragmentation.C, case 4),
with its checks and the figures and tables of README_HBFFmodel.md, where the model is described.

SCIANTIX carries for the HBS pores 
    -   the mean volume V, 
    -   the mean Xe atoms per pore n
    -   and its variance Var(n). 

This script 

1.  rebuilds classes (R_i, V_i, w_i, n_i, p_i) that reproduce these three moments,

        sum_i w_i V_i = V,      sum_i w_i n_i = n,      sum_i w_i (n_i - n)^2 = Var(n),

2.  evaluates the rupture criterion on every class, 
3.  writes the inputs of the runs of MMM: 
        -   radius, 
        -   porosity
        -   and pressure of every pore class
        -   and the confining pressure.

With N_p the pore density, the porosity xi = N_p sum_i w_i V_i = N_p V holds by construction.

Tags: [P] paper, [R] reduction into the SCIANTIX state, [N] numerics, [?] unknown parameter, [E] deviation from a source.
K15: K. Kulacsy, J. Nucl. Mater. 466 (2015) 409. J19: L.O. Jernkvist, EPJ Nucl. Sci. Technol. 5 (2019) 11.
OHP: OperaHPC Deliverable D5.2 (2026), section 2.2.

usage
    python3 utilities/HBFF/pore_distribution.py                    all sections: tables on screen, figures/pore_eos*.png
                                                                   and figures/classes_*.png, with the rupture and
                                                                   release figures of the NFIR case
                                                                   (regression/hbs_nfir/test_UO2HBS_frag_j19damage)
    python3 utilities/HBFF/pore_distribution.py --mmm              mmm_inputs/run_*.txt, one file per MMM run of the
                                                                   coarse cover of the four factors of an RVE: radius,
                                                                   porosity, pressure and confinement of every class
    python3 utilities/HBFF/pore_distribution.py --check CASE_DIR   replay of option 4 on a SCIANTIX case (the folder
                                                                   with input_history.txt, input_settings.txt and
                                                                   output.txt), against the columns of output.txt;
                                                                   on the NFIR case, also STATES against the output
    python3 utilities/HBFF/pore_distribution.py --rupture CASE_DIR the rupture and release figures of another case:
                                                                   figures/classes_rupture_<case>.png (distribution and
                                                                   rupture of the classes, from the broken fractions of
                                                                   output.txt) and figures/classes_fgr_<case>.png
"""

# %% ------------------------------------------------------------------------------------------------------------------
# Libraries and paths
import argparse
import itertools
import os
import sys

import matplotlib.pyplot as plt
import numpy as np
from scipy.interpolate import RegularGridInterpolator
from scipy.optimize import brentq
from scipy.special import ndtr  # standard normal cdf

HERE = os.path.dirname(os.path.abspath(__file__))
FIG_DIR = os.path.join(HERE, "figures")
NFIR_CASE = os.path.normpath(os.path.join(HERE, "..", "..", "regression", "hbs_nfir", "test_UO2HBS_frag_j19damage"))

# %% ------------------------------------------------------------------------------------------------------------------
# Constants (from papers and original SCIANTIX code)
K_B = 1.380651e-23  # (J/K) from include/namespaces/Constants.h 
GAMMA = 1.1  # (N/m) UO2HBS, from SetMatrix.C
ETA_CAP = 0.65  # cap of the hard-sphere packing fraction, from HighBurnupStructurePorosity.C
SIGMA_R = 21.0e6  # (Pa) J19 Table 4: rupture stress that pairs with the mean-field stress concentration factor
XI_PERC = 0.29  # (/) J19, percolation of the HBS pores
C_DP = 55.0  # (N/m) J19 Table 4: steady-state limit of the overpressure of a pore growing by punching, c_dp / R
K15_SIGMA = 0.356  # K15 Table 2: sigma_ln r of the reference pore-size distribution

# %% ------------------------------------------------------------------------------------------------------------------
# Model choices (TBD)
N_CLASSES, SPAN = 81, 4.0  # nodes at ln R = ln R_median +/- 4 s
A_CLOSURE = 2.0  # closure exponent of the gas per class, n_i ~ R_i^a (2: gas density ~ 1/R, 3: uniform pressure)
TAU = 2.0  # (s) J19 Table 3 (interconnection and microcracking modes; J19 rupture is instantaneous)
R_MIN_EVAL, XI_MAX_EVAL = 1.0e-7, 0.35  # validity window of the criterion, on the mean pore

# %% ------------------------------------------------------------------------------------------------------------------
# Colours of the figures
BLUE, ORANGE, GREEN, GREY, BROWN, GRID = "#2a78d6", "#eb6834", "#2f9e6e", "#898781", "#3e1c0f", "#e1e0d9"
SEQ_BLUE = ("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281")  # light to dark, for a sequence of curves

# %% ------------------------------------------------------------------------------------------------------------------
# Equation of state: hard spheres (Carnahan-Starling), from HighBurnupStructurePorosity.C

def xe_volume(T):
    """Volume of a Xe atom, v = pi/6 d^3, with the hard-sphere diameter d(T)."""
    d = 4.45e-10 * (0.8542 - 0.03996 * np.log(T / 231.2))
    return np.pi / 6.0 * d**3

def Z(eta):
    """Carnahan-Starling compressibility factor p V / (n k T)."""
    return (1 + eta + eta**2 - eta**3) / (1 - eta) ** 3

def pore_pressure(n, V, T):
    """Gas pressure of pores holding n atoms in the volume V. Above eta = 0.65 the pressure is linear."""
    return n * K_B * T * Z(np.minimum(n * xe_volume(T) / V, ETA_CAP)) / V


# %% ------------------------------------------------------------------------------------------------------------------
# Pore classes
#
# 1. Radius. Log-normal (K15 Eq. 6) on N_CLASSES nodes x_i equally spaced in [-SPAN, SPAN], ln R_i = const + s x_i,
#    with the number weights w_i ~ exp(-x_i^2 / 2). The tails beyond +/- SPAN s are dropped. The constant is fixed so
#    that the third moment is exact on the nodes: sum(w R^3)^(1/3) = R_V, i.e. sum(w V) = V for any width s.
# 2. Gas. Distribution (TBD):
#       n_i = n R_i^a / sum_j w_j R_j^a
#    The gas density of a class is n_i / V_i ~ R_i^(a - 3): a = 3 is a uniform density and pressure; a = 2 a density
#    ~ 1/R, which for an ideal gas is a pressure ~ 1/R (the shape of the punching overpressure of K15 and J19).
#    The width s is the root of sum_i w_i (n_i - n)^2 = Var(n) on the truncated nodes.
# 3. Pressure. Hard-sphere EoS on every class. The pressure of the mean quantities, p_EoS(n, V, T) (the
#    'HBS pore pressure' of SCIANTIX), is the pressure of the volume-weighted mean density.

X = np.linspace(-SPAN, SPAN, N_CLASSES)  # node coordinates, in units of s
W = np.exp(-0.5 * X**2)  # Gaussian number weight in x = (ln R - ln R_median) / s
W /= W.sum()

def sphere_volume(R):
    return 4.0 / 3.0 * np.pi * R**3

def radius_of(V):
    return np.cbrt(3.0 * V / (4.0 * np.pi))

def classes(R_V, s):
    """Radius classes of width s (sigma of ln R): R (m), V (m3) and the number weights w, sum(w) = 1.
    R_V is the radius of the mean pore volume."""
    g = np.exp(s * X)  # R_i up to a common factor
    R = R_V * g / np.cbrt(np.sum(W * g**3))  # the common factor: sum(w R^3)^(1/3) = R_V
    return R, sphere_volume(R), W

def cv2_on_nodes(s, a):
    """CV^2 of n on the nodes for the closure n_i ~ R_i^a: it depends on s and a."""
    e = np.exp(a * s * X)
    return np.sum(W * e * e) / np.sum(W * e) ** 2 - 1.0

def width(cv2, a):
    """Width s that gives CV_n^2 = Var(n) / n^2 on the nodes. The value of the continuous log-normal,
    sqrt(ln(1 + CV_n^2)) / a, is below the root: it is doubled until it brackets the root."""
    if cv2 <= 0.0:
        return 0.0
    s_hi = np.sqrt(np.log1p(cv2)) / a
    while cv2_on_nodes(s_hi, a) < cv2:
        s_hi *= 2.0
    return brentq(lambda s: cv2_on_nodes(s, a) - cv2, 0.0, s_hi, xtol=1e-15)

def build(R_V, n_mean, var_n, T, a=A_CLOSURE):
    """Classes that hold V (through R_V), n and Var(n), with n_i = n_mean R_i^a / <R^a> and p_i from the EoS.
    Var(n) = 0 gives s = 0: all the classes coincide with the mean pore."""
    s = width(var_n / n_mean**2, a)
    R, V, w = classes(R_V, s)
    n_i = n_mean * R**a / np.sum(w * R**a)
    return {"R": R, "V": V, "w": w, "n": n_i, "p": pore_pressure(n_i, V, T), "sigma": s,
            "R_median": R[N_CLASSES // 2], "a": a}

def moments(d):
    """Number-weighted moments of the classes: V, R_V (cube root of <R^3>), n, Var(n), <p>."""
    w = d["w"]
    return {"V": np.sum(w * d["V"]), "R_V": np.cbrt(np.sum(w * d["R"] ** 3)),
            "n": np.sum(w * d["n"]), "var_n": np.sum(w * (d["n"] - np.sum(w * d["n"])) ** 2),
            "p_mean": np.sum(w * d["p"])}

def check_moments(d, V, n, var_n, tol=1e-8):
    """Relative error of the three conserved moments; raises if one exceeds tol."""
    m = moments(d)
    err = {"V": abs(m["V"] / V - 1.0), "n": abs(m["n"] / n - 1.0), "var_n": abs(m["var_n"] / var_n - 1.0)}
    bad = [k for k, e in err.items() if e > tol]
    if bad:
        raise AssertionError(f"moments not conserved (relative error > {tol}): " + ", ".join(f"{k} {err[k]:.2e}" for k in bad))
    return err


# %% ------------------------------------------------------------------------------------------------------------------
# Rupture per class
#
# Class i breaks when the first principal stress at the pore wall exceeds the rupture stress (OHP 2.2.2):
#     s_hat (p_i - 2 gamma / R_i - P_h) >= sigma_R + P_h    <=>    p_i >= P_cr,i = P_h + 2 gamma / R_i + (sigma_R + P_h) / s_hat
# s_hat is the stress concentration factor per unit net pressure; s_hat and sigma_R come as a pair. 
# Here the mean-field pair of J19 Eq. 22: s_hat = xi / (1 - xi), sigma_R = 21 MPa, the same for all classes, which gives
#     P_cr,i = 2 gamma / R_i + [sigma_R (1 - xi) + P_h] / xi.

def stress_concentration(xi):
    """Mean-field stress concentration factor of J19 Eq. 22: porosity only, no dependence on the class."""
    return xi / (1.0 - xi)

def critical_pressure(R, xi, Ph):
    """Critical gas pressure of a class of radius R (Pa)."""
    return Ph + 2.0 * GAMMA / R + (SIGMA_R + Ph) / stress_concentration(xi)

def broken(d, T, xi, Ph):
    """True for the classes that meet the criterion at T, xi, P_h. For xi >= 0.29 every class is open."""
    if xi >= XI_PERC:
        return np.ones(N_CLASSES, dtype=bool)
    p = pore_pressure(d["n"], d["V"], T)
    return stress_concentration(xi) * (p - 2.0 * GAMMA / d["R"] - Ph) >= SIGMA_R + Ph

def rupture_step(beta, V, n_ret, var_ret, S1, S2, T, xi, Ph, R_mean, dt, a=A_CLOSURE):
    """One time step of option 4, on the broken fractions beta_i and on the retained fractions S1, S2 already diluted
    by the growth of the restructured volume. Transcription of case 4 of HighBurnupStructureFragmentation.C.

        -   n_ret, var_ret: retained moments of n ('Xe atoms per HBS pore' and its variance); 
        -   S1, S2: retained fractions of the first and second moment, 
                    so that the unvented moments are n_ret / S1 and (var_ret + n_ret^2) / S2.

    Returns beta, D* = sum w beta, G* = sum w n beta / sum w n, and the factors f1, f2 by which the venting of the
    step reduces the two retained moments."""
    n_u = n_ret / S1
    valid = R_mean >= R_MIN_EVAL and 0.0 < xi < XI_MAX_EVAL
    if not valid or n_u <= 0.0 or V <= 0.0:
        return beta, 0.0, 0.0, 1.0, 1.0
    var_u = max((var_ret + n_ret**2) / S2 - n_u**2, 0.0)
    d = build(radius_of(V), n_u, var_u, T, a)
    w, n_i = d["w"], d["n"]
    kept_old, kept2_old = np.sum(w * n_i * (1.0 - beta)), np.sum(w * n_i**2 * (1.0 - beta))
    # progressive rupture: first-order relaxation of every class towards its instantaneous state b*_i (0 or 1)
    beta = np.where(broken(d, T, xi, Ph), beta + (1.0 - beta) * (1.0 - np.exp(-dt / TAU)), beta)
    kept_new, kept2_new = np.sum(w * n_i * (1.0 - beta)), np.sum(w * n_i**2 * (1.0 - beta))
    f1 = kept_new / kept_old if kept_old > 0.0 else 1.0
    f2 = kept2_new / kept2_old if kept2_old > 0.0 else 1.0
    return beta, np.sum(w * beta), np.sum(w * n_i * beta) / n_u, f1, f2


# %% ------------------------------------------------------------------------------------------------------------------
# SCIANTIX states of regression/hbs_nfir/test_UO2HBS_frag_j19damage (formation 4, porosity 3), read from output.txt:
# the unvented moments n = n / S and var = (Var + n^2) / S_2 - n^2.
# V (m3), n (atoms per pore), var (Var(n)), xi (/), Np (pores/m3), T (K), Ph (Pa, positive in compression).

# TBD
STATES = {
    "NFIR, end of base irradiation": {"T": 903.15, "Ph": 70.0e6, "xi": 0.1053, "Np": 5.774e17, "V": 1.825e-19, "n": 7.227e8, "var": 2.928e17},
    "NFIR, anneal at 1200 C": {"T": 1473.5, "Ph": 0.0, "xi": 0.1031, "Np": 5.844e17, "V": 1.764e-19, "n": 7.141e8, "var": 2.904e17},
}

# grids of the parameter space; the SCIANTIX states of STATES have CV_n = 0.75 and eta_bar = 0.09
GRID_CV = (0.1, 0.3, 0.5, 0.7, 0.9, 1.1)
GRID_ETA = (0.02, 0.04, 0.07, 0.10, 0.15, 0.23)

def dimensionless(st):
    """The dimensionless quantities of a state: xi, CV_n, eta_bar = n v_Xe(T) / V."""
    return st["xi"], np.sqrt(st["var"]) / st["n"], st["n"] * xe_volume(st["T"]) / st["V"]

def savefig(fig, name):
    os.makedirs(FIG_DIR, exist_ok=True)
    fig.tight_layout()
    fig.savefig(os.path.join(FIG_DIR, name), dpi=150)
    plt.close(fig)
    print("wrote", os.path.join("figures", name))


# %% ------------------------------------------------------------------------------------------------------------------
# Sections: tables on screen and figures

def section_eos():
    """The equation of state of a single pore, against the ideal gas; the dots are the cap eta = 0.65. It plots
        -   pore_eos.png
        -   pore_eos_volume.png
    """
    V_ref = sphere_volume(0.35e-6)
    fig, ax = plt.subplots(figsize=(6.4, 4.2))
    for T, color in zip((600.0, 1000.0, 1500.0), (SEQ_BLUE[1], SEQ_BLUE[3], SEQ_BLUE[4])):
        n_cap = ETA_CAP * V_ref / xe_volume(T)
        n = np.linspace(1.0e6, 1.6 * n_cap, 400)
        ax.plot(n, pore_pressure(n, V_ref, T) / 1e6, color=color, lw=1.8, label=f"hard sphere, {T:.0f} K")
        ax.plot(n, n * K_B * T / V_ref / 1e6, color=color, lw=1.0, ls="--")
        ax.plot([n_cap], [pore_pressure(n_cap, V_ref, T) / 1e6], "o", color=color, ms=6, mec="k", zorder=5)
    ax.plot([], [], color=GREY, lw=1.0, ls="--", label="ideal gas")
    ax.plot([], [], "o", color=GREY, mec="k", label=r"cap $\eta=0.65$ (linear beyond)")
    ax.set(xlabel="Xe atoms per pore  n", ylabel=r"pore pressure  $p_g$  (MPa)", yscale="log", ylim=(1.0, 1.0e5),
           title=r"Pore pressure, $R=0.35\ \mu$m")
    ax.grid(True, color=GRID)
    ax.legend(frameon=False, fontsize=8)
    savefig(fig, "pore_eos.png")

    T = 1000.0
    fig, ax = plt.subplots(figsize=(6.4, 4.2))
    for n, color in zip((1.0e9, 2.0e9, 4.0e9), (SEQ_BLUE[1], SEQ_BLUE[3], SEQ_BLUE[4])):
        V_cap = n * xe_volume(T) / ETA_CAP
        V = np.geomspace(0.6 * V_cap, 1.0e-18, 400)
        ax.plot(V * 1e18, pore_pressure(n, V, T) / 1e6, color=color, lw=1.8, label=f"hard sphere, n = {n:.0e}")
        ax.plot(V * 1e18, n * K_B * T / V / 1e6, color=color, lw=1.0, ls="--")
        ax.plot([V_cap * 1e18], [pore_pressure(n, V_cap, T) / 1e6], "o", color=color, ms=6, mec="k", zorder=5)
    ax.axvline(V_ref * 1e18, color=GREY, lw=1.0, ls=":")
    ax.text(V_ref * 1e18 * 1.03, 2.0, r"$R=0.35\ \mu$m", fontsize=8)
    ax.plot([], [], color=GREY, lw=1.0, ls="--", label="ideal gas")
    ax.plot([], [], "o", color=GREY, mec="k", label=r"cap $\eta=0.65$ (smaller volume: linear in $1/V$)")
    ax.set(xlabel=r"pore volume  $V_p$  ($\mu$m$^3$)", ylabel=r"pore pressure  $p_g$  (MPa)", xscale="log", yscale="log",
           ylim=(1.0, 1.0e5), title="Pore pressure at fixed gas, T = 1000 K")
    ax.grid(True, color=GRID)
    ax.legend(frameon=False, fontsize=8)
    savefig(fig, "pore_eos_volume.png")

def section_inputs():
    """Inputs and test of the conserved quantities, on the NFIR state at the end of the base irradiation."""
    st = STATES["NFIR, end of base irradiation"]
    R_V = radius_of(st["V"])
    d = build(R_V, st["n"], st["var"], st["T"])
    m = moments(d)
    err = check_moments(d, st["V"], st["n"], st["var"])  # raises if a moment is off by more than 1e-8
    xi, cv, eta = dimensionless(st)
    print("\n== NFIR, end of base irradiation ==")
    print(f"xi = {xi:.4f}, CV_n = {cv:.4f}, eta_bar = {eta:.4f}, R_V = {R_V * 1e6:.4f} um, a = {A_CLOSURE}")
    print(f"sigma_lnR = {d['sigma']:.4f} (K15 reference {K15_SIGMA}), median = {d['R_median'] * 1e6:.4f} um, "
          f"R from {d['R'].min() * 1e6:.3f} to {d['R'].max() * 1e6:.3f} um")
    print(f"{'':8s}{'target':>14s}{'classes':>16s}{'rel. err.':>12s}")
    for name, key, target in (("V (m3)", "V", st["V"]), ("n", "n", st["n"]), ("Var(n)", "var_n", st["var"])):
        print(f"{name:8s}{target:14.5e}{m[key]:16.5e}{err[key]:12.1e}")
    print(f"R_V = {m['R_V'] * 1e6:.4f} um (input {R_V * 1e6:.4f} um), <p> = {m['p_mean'] / 1e6:.1f} MPa")
    return st, R_V, d, m

def section_k15_check():
    """Self-check of build() on a case with a known solution: Var(n) of the K15 reference width with the closure
    n_i ~ R_i^a must give back s = 0.356. Not a SCIANTIX state."""
    R_V, n = 0.35e-6, 1.4531e9
    R, _, w = classes(R_V, K15_SIGMA)
    n_i = n * R**A_CLOSURE / np.sum(w * R**A_CLOSURE)
    var = np.sum(w * (n_i - n) ** 2)
    s = build(R_V, n, var, 800.0)["sigma"]
    print(f"\n== self-check on the K15 width ==\nCV_n = {np.sqrt(var) / n:.4f} -> s = {s:.6f} (expected {K15_SIGMA})")
    assert abs(s - K15_SIGMA) < 1e-10

def section_truncation(d):
    """Truncation of the log-normal tails. The continuous log-normal has an infinite tail towards large radii; the
    classes cover x in [-SPAN, SPAN] and the weights are renormalised on the nodes. A quantity weighted by R^k (k = 0
    number, 3 volume, a gas, 2a the second moment of n) is again Gaussian in x with the mean shifted to k s, so the
    fraction outside +/- SPAN is
        f_k = Phi(-SPAN - k s) + [1 - Phi(SPAN - k s)].
    The upper tail dominates and grows with k: Var(n) is the most sensitive of the three moments, which is why s is
    solved on the truncated nodes instead of being taken from the continuous formula."""
    s, a = d["sigma"], d["a"]
    print("\n== truncation of the tails ==")
    print(f"s = {s:.4f}, R_min = {d['R'].min() * 1e6:.3f} um, R_max = {d['R'].max() * 1e6:.3f} um")
    print(f"{'weight':26s}{'k':>6s}{'below -4s':>12s}{'above +4s':>12s}{'outside':>12s}")
    for name, k in (("number", 0.0), ("volume", 3.0), ("gas n ~ R^a", a), ("n^2 ~ R^(2a)  (Var n)", 2.0 * a)):
        lo, hi = ndtr(-SPAN - k * s), 1.0 - ndtr(SPAN - k * s)
        print(f"{name:26s}{k:6.2f}{lo:12.2e}{hi:12.2e}{lo + hi:12.2e}")

def section_distributions(st, d, m):
    """Radius pdf, pressure per class and gas per class. The capillary pressure 2 gamma / R is drawn in grey on the
    pressure panel: it is the part of the gas pressure carried by the pore surface. Ten times it (dashed) is on the
    scale of p_i, to compare the shapes. What the closure fixes is the gas density of the classes, ~ R^(a - 3): the
    pressure of the same gas as an ideal gas (dashed orange) has that shape, 1 / R for a = 2 like 2 gamma / R; the
    hard-sphere pressure p_i is above it by the factor Z(eta_i), which grows towards the small classes."""
    R_um = d["R"] * 1e6
    pdf = d["w"] / np.gradient(np.log(d["R"]))  # pdf of ln R: integrates to 1 over ln R
    p_eos = float(pore_pressure(m["n"], m["V"], st["T"])) / 1e6  # EoS of the mean quantities, conserved
    fig, ax = plt.subplots(1, 3, figsize=(12, 3.8))
    for ax_ in ax:
        ax_.axvline(m["R_V"] * 1e6, color=BROWN, lw=1.2, label=r"$R_V$ (mean volume)")
    ax[0].plot(R_um, pdf, color=BLUE)
    ax[0].set(xlabel="R (um)", ylabel="pdf in ln R", title=f"radii, s = {d['sigma']:.3f}")
    ax[1].plot(R_um, d["p"] / 1e6, color=ORANGE, label=r"$p_i$ (hard spheres)")
    ax[1].plot(R_um, d["n"] * K_B * st["T"] / d["V"] / 1e6, color=ORANGE, ls="--", label=r"$n_ik_BT/V_i$ (same gas, ideal)")
    ax[1].axhline(p_eos, color="#ab522f", label=r"$p_{EoS}(\bar n,\bar V)$ (conserved)")
    ax[1].set(xlabel="R (um)", ylabel="p (MPa)", title=f"pressure per class, T = {st['T']:.0f} K")
    ax[2].plot(R_um, d["n"], color=GREEN)
    ax[2].axhline(m["n"], color="#887B75", label=r"$\bar n$ (conserved)")
    ax[2].set(xlabel="R (um)", ylabel="n_i", title="Xe atoms per class")
    for ax_ in ax:
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False, fontsize=7, loc="upper right" if ax_ is ax[1] else "best")
    savefig(fig, "classes_distributions.png")

def section_cumulative(st, R_V, d):
    """Each moment as the cumulative contribution of the classes up to the radius R,
        sum_{R_i <= R} w_i V_i / V,     sum_{R_i <= R} w_i n_i / n,     sum_{R_i <= R} w_i (n_i - n)^2 / Var(n).
    All curves end at 1; they show which pores carry each moment: the number sits near the median, the volume, the gas
    and above all the variance come from the large pores, so the upper tail matters more for Var(n) than for the
    number of pores."""
    w, V, n, R = d["w"], d["V"], d["n"], d["R"]
    contrib = {"number  (w)": (w, 1.0), "volume  (w V / V)": (w * V, st["V"]), "gas  (w n / n)": (w * n, st["n"]),
               "variance  (w (n - n)^2)": (w * (n - st["n"]) ** 2, st["var"])}
    fig, ax = plt.subplots(figsize=(6.2, 3.8))
    for (name, (term, target)), color in zip(contrib.items(), (GREY, BLUE, GREEN, ORANGE)):
        cum = np.cumsum(term) / target
        ax.semilogx(R * 1e6, cum, color=color, label=f"{name}")
        assert abs(np.sum(term) / target - 1.0) < 1e-8, name
    ax.axhline(1.0, color="k", lw=0.8, ls="--")
    ax.axvline(R_V * 1e6, color=BROWN, lw=1.2)
    ax.set(xlabel="R (um)", ylabel="cumulative fraction of the moment")
    ax.grid(True, color=GRID)
    ax.legend(frameon=False, fontsize=7, loc="center left")
    savefig(fig, "classes_cumulative.png")
    print("\n== share of each moment carried by the classes with R <= R_V ==")
    for name, (term, target) in contrib.items():
        print(f"{name:26s}{np.sum(term[R <= R_V]) / target:.3f}")

def section_mean_pressure(st, R_V, d):
    """Mean pressure and what is conserved. p_EoS(n, V, T) is the pressure of the volume-weighted mean density
    rho_vol = sum(w n) / sum(w V) = n / V, conserved. The number-weighted mean <p_i> is not, and cannot be together
    with n and V when the pressure is higher in the small pores: <rho> = n / V - Cov(rho, V) / V, with
    Cov(rho, V) < 0 when rho falls with V, and p(rho) convex gives <p_i> > p_EoS. Within the closure n_i ~ R_i^a,
    imposing <p_i> = p_EoS forces Cov = 0, a uniform pressure."""
    T = st["T"]
    w, Vi, ni, pi = d["w"], d["V"], d["n"], d["p"]
    p_bar = float(pore_pressure(st["n"], st["V"], T))
    rho_i = ni / Vi
    rho_vol = np.sum(w * ni) / np.sum(w * Vi)
    p_rho_vol = float(pore_pressure(rho_vol, 1.0, T))  # EoS at the density rho_vol: V = 1 m3, n = rho_vol atoms
    cov = np.sum(w * rho_i * Vi) - np.sum(w * rho_i) * np.sum(w * Vi)
    print("\n== mean pressure ==")
    print(f"p_EoS(n, V, T) = {p_bar / 1e6:.1f} MPa   (eta_bar = {st['n'] * xe_volume(T) / st['V']:.3f})")
    for name, wt in (("number-weighted", w), ("volume-weighted", w * Vi), ("gas-weighted", w * ni)):
        print(f"<p>, {name:16s}: {np.sum(wt * pi) / np.sum(wt) / 1e6:7.1f} MPa")
    print(f"p of the class with R = R_V     : {np.interp(R_V, d['R'], pi) / 1e6:7.1f} MPa")
    print(f"volume-weighted mean density rho_vol = {rho_vol:.4e} atoms/m3, n/V = {st['n'] / st['V']:.4e}; "
          f"p(rho_vol) = {p_rho_vol / 1e6:.3f} MPa (rel. diff. from p_EoS {abs(p_rho_vol / p_bar - 1.0):.1e})")
    print(f"<rho> = {np.sum(w * rho_i):.4e},  n/V - Cov(rho, V)/V = {st['n'] / st['V'] - cov / st['V']:.4e}  "
          f"(Cov/V = {cov / st['V']:.3e} < 0: higher density in the small pores)")


def section_closure_exponent(st, R_V):
    """Sensitivity to the closure exponent a at the same moments. On the pressure plot: 2 gamma / R in grey, and the
    punching limit of J19 Eq. 21, P_h + 2 gamma / R + c_dp / R (dashed): a class above it would grow by dislocation
    punching in steady state, so the closure is not consistent with the base irradiation there."""
    T, Ph = st["T"], st["Ph"]
    print(f"\n== closure exponent, {T:.0f} K ==")
    print(f"{'a':>5s}{'sigma_lnR':>11s}{'p_min':>9s}{'p_max':>9s}{'<p>_vol':>9s}   (MPa)   classes above the punching limit"
          "   rel. err. of V, n, Var(n)")
    fig, ax = plt.subplots(figsize=(5.8, 3.8))
    for a_ in (3.0, 2.75, 2.5, 2.25, 2.0):
        dp = build(R_V, st["n"], st["var"], T, a_)
        e = check_moments(dp, st["V"], st["n"], st["var"])
        pv = np.sum(dp["w"] * dp["V"] * dp["p"]) / np.sum(dp["w"] * dp["V"]) / 1e6
        above = np.sum(dp["w"][dp["p"] > Ph + (2.0 * GAMMA + C_DP) / dp["R"]])
        print(f"{a_:5.2f}{dp['sigma']:11.4f}{dp['p'].min() / 1e6:9.1f}{dp['p'].max() / 1e6:9.1f}{pv:9.1f}   {above:18.4f}"
              f"   {e['V']:.0e} {e['n']:.0e} {e['var_n']:.0e}")
        ax.plot(dp["R"] * 1e6, dp["p"] / 1e6, label=f"a = {a_}")
    R = np.geomspace(0.07e-6, 1.2e-6, 100)
    ax.plot(R * 1e6, 2.0 * GAMMA / R / 1e6, color=GREY, label=r"$2\gamma/R$")
    ax.plot(R * 1e6, (Ph + (2.0 * GAMMA + C_DP) / R) / 1e6, "k--", lw=1, label=r"$P_h+2\gamma/R+c_{dp}/R$ (J19)")
    ax.set(xlabel="R (um)", ylabel="p (MPa)", yscale="log", title=f"T = {T:.0f} K, $P_h$ = {Ph / 1e6:.0f} MPa")
    ax.grid(True, color=GRID)
    ax.legend(frameon=False, fontsize=7)
    savefig(fig, "classes_closure_exponent.png")

def section_moment_checks(st, R_V):
    """Check of the conserved moments: for each a, the number-weighted distributions of V_i and n_i with the target
    mean (solid) and the mean of the classes (dashed) on top of each other; then the relative error of V, n, Var(n)
    against a, CV_n and n. The dashed line is the tolerance of check_moments."""
    T = st["T"]
    fig, ax = plt.subplots(2, 3, figsize=(12, 6.2), sharex="row")
    for j, a_ in enumerate((3.0, 2.5, 2.0)):
        dp = build(R_V, st["n"], st["var"], T, a_)
        mp = moments(dp)
        w, V, n = dp["w"], dp["V"], dp["n"]
        ax[0, j].plot(V * 1e19, w / np.gradient(np.log(V)), color=BLUE)
        ax[0, j].axvline(st["V"] * 1e19, color="k", lw=1.6, label=f"target {st['V'] * 1e19:.4f}")
        ax[0, j].axvline(mp["V"] * 1e19, color=ORANGE, lw=1.2, ls="--", label=f"classes {mp['V'] * 1e19:.4f}")
        ax[0, j].set(title=f"a = {a_}", xlabel="V (1e-19 m3)", ylabel="pdf in ln V", xscale="log")
        ax[1, j].plot(n / 1e9, w / np.gradient(np.log(n)), color=GREEN)
        ax[1, j].axvline(st["n"] / 1e9, color="k", lw=1.6, label=f"target {st['n'] / 1e9:.4f}")
        ax[1, j].axvline(mp["n"] / 1e9, color=ORANGE, lw=1.2, ls="--", label=f"classes {mp['n'] / 1e9:.4f}")
        ax[1, j].set(xlabel="n (1e9 atoms)", ylabel="pdf in ln n", xscale="log")
    for ax_ in ax.ravel():
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False, fontsize=7)
    savefig(fig, "classes_moments.png")

    def errors(n_mean, var_n, V_mean, a_):
        mp = moments(build(radius_of(V_mean), n_mean, var_n, T, a_))
        return abs(mp["V"] / V_mean - 1), abs(mp["n"] / n_mean - 1), abs(mp["var_n"] / var_n - 1)

    cv = np.sqrt(st["var"]) / st["n"]
    scans = (("exponent a", np.linspace(2.0, 3.0, 11), lambda x: errors(st["n"], st["var"], st["V"], x)),
             ("CV_n", np.linspace(0.1, 2.0, 12), lambda x: errors(st["n"], (x * st["n"]) ** 2, st["V"], A_CLOSURE)),
             ("mean gas n (1e9)", np.linspace(0.3, 3.0, 10), lambda x: errors(x * 1e9, (cv * x * 1e9) ** 2, st["V"], A_CLOSURE)))
    fig, ax = plt.subplots(1, 3, figsize=(12, 3.6), sharey=True)
    worst = 0.0
    for ax_, (label, xs, fn) in zip(ax, scans):
        e = np.array([fn(x) for x in xs])
        worst = max(worst, e.max())
        for k, (name, color) in enumerate((("V", BLUE), ("n", ORANGE), ("Var(n)", GREEN))):
            ax_.semilogy(xs, e[:, k] + 1e-17, "o-", ms=3, color=color, label=name)  # 1e-17 keeps exact zeros on the log axis
        ax_.axhline(1e-8, color="k", ls="--", lw=1)
        ax_.set(xlabel=label, ylim=(1e-17, 1e-6))
        ax_.grid(True, color=GRID)
    ax[0].set_ylabel("relative error")
    ax[0].legend(frameon=False, fontsize=8)
    savefig(fig, "classes_moment_errors.png")
    print(f"\n== conserved moments ==\nlargest relative error over the three scans: {worst:.1e}")

def section_heatup(st, R_V):
    """Rupture per class on a heat-up, rigid pores at fixed gas n_i and volume V_i (the NFIR anneal: P_h = 0).
    Instantaneous criterion, no relaxation time: D* = sum w [broken], G* = sum w n [broken] / sum w n.
    The porosity xi is an input: the pore density is not part of the moments. Solid lines: P_h = 0; dotted: the P_h of
    the base irradiation. The curves are staircases, one step per class that breaks: the more classes, the smaller
    the steps."""
    xi = st["xi"]
    T_list = np.linspace(600.0, 1700.0, 111)
    print(f"\n== heat-up at fixed gas, xi = {xi:.4f} ==")
    fig, ax = plt.subplots(1, 2, figsize=(10, 3.6))
    for Ph, ls in ((0.0, "-"), (st["Ph"], ":")):
        for a_, color in zip((3.0, 2.75, 2.5, 2.25, 2.0), ("C0", "C1", "C2", "C3", "C4")):
            d = build(R_V, st["n"], st["var"], st["T"], a_)
            D, G = [], []
            for Tk in T_list:
                b = broken(d, Tk, xi, Ph)
                D.append(np.sum(d["w"][b]))
                G.append(np.sum((d["w"] * d["n"])[b]) / np.sum(d["w"] * d["n"]))
            ax[0].plot(T_list, D, ls, color=color, label=f"a = {a_}" if Ph == 0.0 else None)
            ax[1].plot(T_list, G, ls, color=color)
            p_cr = critical_pressure(d["R"], xi, Ph)
            k = int(np.argmin(np.abs(T_list - 1473.0)))
            print(f"P_h = {Ph / 1e6:3.0f} MPa, a = {a_:4.2f}: P_cr = {p_cr.min() / 1e6:.0f}..{p_cr.max() / 1e6:.0f} MPa, "
                  f"p at 1473 K = {pore_pressure(d['n'], d['V'], 1473.0).min() / 1e6:.0f}..{pore_pressure(d['n'], d['V'], 1473.0).max() / 1e6:.0f} MPa, "
                  f"D* = {D[k]:.3f}, G* = {G[k]:.3f}")
    ax[0].set(xlabel="T (K)", ylabel="D*", title="broken pores (number)")
    ax[1].set(xlabel="T (K)", ylabel="G*", title="broken gas")
    fig.suptitle(rf"solid: $P_h$ = 0 (anneal); dotted: $P_h$ = {st['Ph'] / 1e6:.0f} MPa (base irradiation)", fontsize=9)
    for ax_ in ax:
        ax_.grid(True, color=GRID)
    ax[0].legend(frameon=False, fontsize=8)
    savefig(fig, "classes_heatup.png")

def normalised(a_, cv_n_, eta_bar, T_=1000.0, R_V_=0.35e-6):
    """Classes in relative variables for given (a, CV_n, eta_bar): x = R / R_V, p / p_bar, n / n_bar, w.
    R_V_ and T_ only set the scale: n_bar is chosen so that the mean packing fraction is eta_bar."""
    V_ = sphere_volume(R_V_)
    n_ = eta_bar * V_ / xe_volume(T_)
    dd = build(R_V_, n_, (cv_n_ * n_) ** 2, T_, a_)
    return {"x": dd["R"] / R_V_, "w": dd["w"], "p_rel": dd["p"] / float(pore_pressure(n_, V_, T_)), "n_rel": dd["n"] / n_,
            "sigma": dd["sigma"], "x_median": dd["R_median"] / R_V_}

def section_normalised(st):
    """Normalised representation for the micromechanics. In x_i = R_i / R_V, p_i / p_bar and w_i depend on three
    numbers only: a, CV_n and eta_bar = n v_Xe(T) / V. R_V and p_bar rescale the axes, T enters through eta_bar.
    In the dilute limit (eta_bar -> 0) the shape is p_i / p_bar ~ x_i^(a - 3); the table gives the largest relative
    deviation from it over the central classes (within +/- 2 s of the median, about 96 % of the pores) and over all
    classes. One universal curve is a usable approximation only below eta_bar of about 0.02; at the packing fraction
    of the SCIANTIX states (0.09) it is not."""
    _, cv, eta_state = dimensionless(st)
    etas = (1e-4, 0.02, 0.05, eta_state, 0.15, 0.25)
    ref = normalised(A_CLOSURE, cv, etas[0])  # dilute limit
    central = np.abs(np.log(ref["x"] / ref["x_median"])) <= 2.0 * ref["sigma"] * (1.0 + 1e-9)   # nodes at +/- 2 s included
    print("\n== normalised pressure, deviation from the dilute limit ==")
    print(f"a = {A_CLOSURE}, CV_n = {cv:.3f}, s = {ref['sigma']:.4f}; central classes: x in [{ref['x'][central].min():.2f}, "
          f"{ref['x'][central].max():.2f}], weight {ref['w'][central].sum():.3f}")
    print(f"{'eta_bar':>9s}{'max dev, central':>18s}{'max dev, all':>14s}")
    fig, ax = plt.subplots(1, 2, figsize=(11, 3.8))
    for eta_, color in zip(etas[1:], ("#86b6ef", "#5598e7", ORANGE, "#1c5cab", "#104281")):
        nn = normalised(A_CLOSURE, cv, eta_)
        dev = nn["p_rel"] / ref["p_rel"] - 1.0
        print(f"{eta_:9.3f}{np.max(np.abs(dev[central])):18.3f}{np.max(np.abs(dev)):14.3f}")
        ax[0].plot(nn["x"], nn["p_rel"], color=color, label=f"eta_bar = {eta_:.3f}")
        ax[1].plot(nn["x"], dev, color=color, label=f"eta_bar = {eta_:.3f}")
    ax[0].plot(ref["x"], ref["p_rel"], "k--", lw=1, label="dilute limit")
    ax[0].set(xlabel="R / R_V", ylabel="p / p_bar", title=f"normalised pressure, a = {A_CLOSURE}, CV_n = {cv:.2f}", xscale="log", yscale="log")
    ax[1].axhline(0.0, color="k", lw=0.8, ls="--")
    ax[1].set(xlabel="R / R_V", ylabel="(p/p_bar) / (dilute limit) - 1", title="deviation from the dilute limit", xscale="log")
    for ax_ in ax:
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False, fontsize=7)
    savefig(fig, "classes_normalised.png")

def section_coverage():
    """Coverage of the parameter space.

    parameter | enters                          | through
    CV_n      | the radius distribution (s)     | moments
    eta_bar   | the shape p / p_bar             | Z(eta); carries n, V and T (through v_Xe(T))
    xi        | the geometry of the RVE         | porosity
    P_h/p_bar | the net load of every class     | p_i - P_h; a uniform P_h superposes only on a uniform pore pressure
    """
    cv_ref, eta_ref = 0.7, 0.09
    cmap = lambda k, n: plt.cm.viridis(0.1 + 0.8 * k / max(n - 1, 1))
    fig, ax = plt.subplots(1, 3, figsize=(13, 3.8))
    for k, cv_ in enumerate(GRID_CV):  # radius distribution against CV_n: depends on s only
        nn = normalised(A_CLOSURE, cv_, eta_ref)
        ax[0].plot(nn["x"], nn["w"] / np.gradient(np.log(nn["x"])), color=cmap(k, len(GRID_CV)), label=f"CV_n = {cv_}, s = {nn['sigma']:.3f}")
        ax[2].plot(nn["x"], nn["p_rel"], color=cmap(k, len(GRID_CV)), label=f"CV_n = {cv_}")
    for k, e_ in enumerate(GRID_ETA):  # pressure shape against eta_bar
        nn = normalised(A_CLOSURE, cv_ref, e_)
        ax[1].plot(nn["x"], nn["p_rel"], color=cmap(k, len(GRID_ETA)), label=f"eta_bar = {e_}")
    ax[0].set(xlabel="R / R_V", ylabel="pdf in ln R", title=f"radius distribution, a = {A_CLOSURE}", xscale="log")
    ax[1].set(xlabel="R / R_V", ylabel="p / p_bar", title=f"pressure shape against eta_bar (CV_n = {cv_ref})", xscale="log", yscale="log")
    ax[2].set(xlabel="R / R_V", ylabel="p / p_bar", title=f"pressure shape against CV_n (eta_bar = {eta_ref})", xscale="log", yscale="log")
    for ax_ in ax:
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False, fontsize=7)
    savefig(fig, "classes_coverage.png")

def section_normalised_vs_direct():
    """Normalised against direct: do CV_n and eta_bar suffice for the gas pressure of the classes, with R_V, n and T
    left out of the parameter space?
    A. fixed (CV_n, eta_bar), different (R_V, T, n) with n = eta_bar V / v_Xe(T): the direct classes lie on the
       normalised curve.
    B. fixed R_V, T, CV_n, only n varied: eta_bar changes and so does the shape; each direct curve coincides with the
       normalised one at its own eta_bar.
    C. random states, direct against normalised at the (CV_n, eta_bar) of the state, and against the interpolation in
       the grid GRID_CV x GRID_ETA (linear in ln(p / p_bar)) [N]. The SCIANTIX states are marked."""
    a = A_CLOSURE

    def direct(R_V_, n_, cv_n_, T_):
        V_ = sphere_volume(R_V_)
        dd = build(R_V_, n_, (cv_n_ * n_) ** 2, T_, a)
        return dd["R"] / R_V_, dd["p"] / float(pore_pressure(n_, V_, T_)), n_ * xe_volume(T_) / V_

    cmap = lambda k, n: plt.cm.viridis(0.1 + 0.8 * k / max(n - 1, 1))
    fig, ax = plt.subplots(1, 3, figsize=(14, 3.9))
    combos = ((0.5, 0.04), (0.9, 0.04), (0.5, 0.15), (0.9, 0.15))
    errA = 0.0
    for k, (cv_, eta_) in enumerate(combos):
        nn = normalised(a, cv_, eta_)
        ax[0].plot(nn["x"], nn["p_rel"], color=cmap(k, len(combos)), label=f"CV_n = {cv_}, eta_bar = {eta_}")
        for (RV_, T_), mk in zip(((0.25e-6, 600.0), (0.40e-6, 1000.0), (0.80e-6, 1500.0)), ("o", "s", "^")):
            x, y, _ = direct(RV_, eta_ * sphere_volume(RV_) / xe_volume(T_), cv_, T_)
            errA = max(errA, np.max(np.abs(y / nn["p_rel"] - 1.0)))
            ax[0].plot(x[::8], y[::8], mk, ms=4, mfc="none", color=cmap(k, len(combos)))
    ax[0].set(xlabel="R / R_V", ylabel="p / p_bar", xscale="log", yscale="log", title="A: same (CV_n, eta_bar); o, s, ^ = 3 (R_V, T, n)")
    errB = 0.0
    for k, n_ in enumerate((0.3e9, 0.7e9, 1.4e9, 2.8e9)):
        x, y, eta_ = direct(0.35e-6, n_, 0.7, 800.0)
        nn = normalised(a, 0.7, eta_)
        errB = max(errB, np.max(np.abs(y / nn["p_rel"] - 1.0)))
        ax[1].plot(x, y, color=cmap(k, 4), label=f"n = {n_:.1e}, eta_bar = {eta_:.3f}")
        ax[1].plot(nn["x"][::8], nn["p_rel"][::8], "k.", ms=3)
    ax[1].set(xlabel="R / R_V", ylabel="p / p_bar", xscale="log", yscale="log", title="B: only n varied (dots: normalised at its eta_bar)")
    grid_lnp = np.array([[np.log(normalised(a, cv_, eta_)["p_rel"]) for eta_ in GRID_ETA] for cv_ in GRID_CV])  # (cv, eta, class)
    interp = RegularGridInterpolator((GRID_CV, GRID_ETA), grid_lnp)
    rng = np.random.default_rng(1)
    ex, it, pts = [], [], []
    while len(ex) < 200:
        RV_, T_, cv_ = rng.uniform(0.15e-6, 0.9e-6), rng.uniform(600.0, 1700.0), rng.uniform(GRID_CV[0], GRID_CV[-1])
        x, y, eta_ = direct(RV_, rng.uniform(0.02, 0.23) * sphere_volume(RV_) / xe_volume(T_), cv_, T_)
        if not GRID_ETA[0] <= eta_ <= GRID_ETA[-1]:
            continue
        ex.append(np.max(np.abs(y / normalised(a, cv_, eta_)["p_rel"] - 1.0)))
        it.append(np.max(np.abs(y / np.exp(interp((cv_, eta_))) - 1.0)))
        pts.append((cv_, eta_))
    pts = np.array(pts)
    sc = ax[2].scatter(pts[:, 1], pts[:, 0], c=np.log10(np.maximum(it, 1e-16)), cmap="viridis", s=14)
    for name, st in STATES.items():
        _, cv_, eta_ = dimensionless(st)
        ax[2].plot(eta_, cv_, "*", color=ORANGE, ms=11, mec="k", mew=0.5)
    fig.colorbar(sc, ax=ax[2], label="log10 max rel. error, grid interpolation")
    ax[2].set(xlabel="eta_bar", ylabel="CV_n", title="C: 200 random states; stars: SCIANTIX states")
    for ax_ in ax[:2]:
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False, fontsize=6)
    ax[2].grid(True, color=GRID)
    savefig(fig, "classes_normalised_vs_direct.png")
    print("\n== normalised against direct ==")
    print(f"A: max |direct / normalised - 1| = {errA:.1e}\nB: max |direct / normalised - 1| = {errB:.1e}")
    print(f"C: normalised on the exact (CV_n, eta_bar): max rel. error {max(ex):.1e}")
    print(f"C: grid interpolation ({len(GRID_CV)} x {len(GRID_ETA)} points): max {max(it):.2e}, median {np.median(it):.2e}")


# %% ------------------------------------------------------------------------------------------------------------------
# Inputs for MMM
#
# MMM solves a linear elastic problem on a periodic RVE of spherical pores under internal pressure (OHP 2.2.2) and
# returns the maximum first principal stress at every pore. An input is, for every size class of the pores, its
# radius, its porosity and its gas pressure, and the confining pressure. No SCIANTIX run is needed. The run files
# written here are read by Merope/distributedpore.py, which throws the spheres of the RVE (4 columns, lines of comment
# starting with #; the side of the cube is fixed there, and the number of pores follows from the porosities).
# An RVE is scale-free in length and linear in the load, so the radius of the mean pore volume R_V and the mean
# pressure p_bar are not factors. With the closure exponent fixed, radius, porosity and pressure of the classes, each
# relative to its mean, depend on four factors only:
#   xi           porosity: the distance between the pores
#   s            width of the radius distribution (sigma of ln R): R_k / R_V and the shares of the porosity xi_k / xi
#   eta_bar      mean packing fraction of the gas: with s, the pressure profile p_k / p_bar
#   P_h / p_bar  confinement
# The 81 nodes are grouped in classes (bins of x centred on 0, +/-1, +/-2, ...; a node on a bin edge is shared in
# halves). Each class keeps the pore volume and the gas of its nodes, so V and n of the population are exact: the
# porosities of the classes sum to xi and p_EoS(n, V) of the RVE is p_bar. Var(n) is reduced by the grouping.

MMM_CLASSES = 7  # classes of an input, odd
# low and high level of each factor, TBD
MMM_FACTORS = {"xi": (0.07, 0.145), "s": (0.21, 0.42), "eta": (0.07, 0.11), "Ph_rel": (0.0, 1.0)}
MMM_RUN_DIR = "mmm_inputs"

def reduced_classes(d, T, n_classes=MMM_CLASSES):
    """Grouping of the nodes in n_classes classes: a list of dicts with the x range, the number fraction w, R, V, n, p
    and the share of the porosity w V / <V>."""
    half = (n_classes - 1) // 2
    edges = (-np.inf,) + tuple(j + 0.5 for j in range(-half, half)) + (np.inf,)
    m = moments(d)
    out = []
    for lo, hi in zip(edges[:-1], edges[1:]):
        share = ((X > lo + 1e-9) & (X < hi - 1e-9)) + 0.5 * (np.isclose(X, lo) | np.isclose(X, hi))  # edge nodes in halves
        wk = np.sum(d["w"] * share)
        Vk = np.sum(d["w"] * share * d["V"]) / wk
        nk = np.sum(d["w"] * share * d["n"]) / wk
        out.append({"x_lo": max(lo, -SPAN), "x_hi": min(hi, SPAN), "w": wk, "V": Vk, "R": radius_of(Vk), "n": nk,
                    "p": float(pore_pressure(nk, Vk, T)), "xi_share": wk * Vk / m["V"]})
    w = np.array([c["w"] for c in out])
    assert abs(np.sum(w) - 1.0) < 1e-12 and abs(sum(c["xi_share"] for c in out) - 1.0) < 1e-12
    assert abs(np.sum(w * np.array([c["n"] for c in out])) / m["n"] - 1.0) < 1e-12
    return out

def mmm_case(tag, origin, xi, s, eta, Ph_rel, R_V, T, a=A_CLOSURE):
    """A run from its four factors (porosity xi, width s, mean packing fraction eta, confinement Ph_rel = P_h / p_bar)
    and from the scale R_V (m), T (K): the parameters, CV_n, p_bar and P_h (Pa), and the 81 classes."""
    V = sphere_volume(R_V)
    n = eta * V / xe_volume(T)
    cv2 = cv2_on_nodes(s, a)
    p_bar = float(pore_pressure(n, V, T))
    return {"id": tag, "origin": origin, "xi": xi, "eta": eta, "R_V": R_V, "T": T, "a": a, "cv": np.sqrt(cv2), "p_bar": p_bar,
            "Ph": Ph_rel * p_bar, "classes": build(R_V, n, cv2 * n**2, T, a)}

def mmm_design():
    """Coarse cover of the four factors of MMM_FACTORS, 9 runs: the centre and half of the 16 corners, those where the
    level of the fourth factor is the product of the levels of the other three (-1 low, +1 high). Every factor is at
    each level in 4 runs and every pair of factors takes its four combinations of levels twice. R_V and T are those
    of the NFIR anneal: they set the scale of the radii and of the pressures."""
    ref = STATES["NFIR, anneal at 1200 C"]
    names = list(MMM_FACTORS)
    signs = [(0, 0, 0, 0)] + [(i, j, k, i * j * k) for i, j, k in itertools.product((-1, 1), repeat=3)]
    cases = []
    for run, sign in enumerate(signs):
        par = {f: 0.5 * (lo + hi) + 0.5 * sg * (hi - lo) for (f, (lo, hi)), sg in zip(MMM_FACTORS.items(), sign)}
        origin = "centre" if run == 0 else "corner " + " ".join(f"{f}{'+' if sg > 0 else '-'}" for f, sg in zip(names, sign))
        cases.append(mmm_case(f"R{run}", origin, R_V=radius_of(ref["V"]), T=ref["T"], **par))
    return cases

def mmm_run_files(cases, n_classes=MMM_CLASSES):
    """One text file per run in MMM_RUN_DIR, with one row per pore class: radius (um), porosity of the class,
    gas pressure (MPa), confining pressure (MPa), the input of Merope/distributedpore.py; and index.txt with the
    factors of every run."""
    folder = os.path.join(HERE, MMM_RUN_DIR)
    os.makedirs(folder, exist_ok=True)
    for old in os.listdir(folder):  # the runs of a previous design
        if old == "index.txt" or (old.startswith("run_") and old.endswith(".txt")):
            os.remove(os.path.join(folder, old))
    index = [f"# Inputs for MMM: {len(cases)} runs, {n_classes} pore classes each (python3 utilities/HBFF/pore_distribution.py --mmm)",
             f"# closure exponent a = {A_CLOSURE:g}; R_V and T set the scale of the radii and of the pressures",
             "# geometry of the runs: python3 utilities/HBFF/Merope/distributedpore.py (it reads every run_*.txt of this folder)",
             f"# {'run':6s}{'xi':>8s}{'s':>8s}{'CV_n':>8s}{'eta_bar':>9s}{'Ph/p_bar':>10s}{'R_V_um':>9s}{'T_K':>9s}{'p_bar_MPa':>11s}{'Ph_MPa':>9s}  file"]
    for c in cases:
        d, xi, p_bar, Ph = c["classes"], c["xi"], c["p_bar"], c["Ph"]
        file = f"run_{c['id']}.txt"
        lines = [f"# MMM input, run {c['id']} ({c['origin']})",
                 f"# xi = {xi:g}, s = {d['sigma']:.4g} (CV_n = {c['cv']:.4g}), eta_bar = {c['eta']:.4g}, P_h / p_bar = {Ph / p_bar:.4g}, a = {c['a']:g}",
                 f"# scale: R_V = {c['R_V'] * 1e6:.4g} um, T = {c['T']:g} K, p_bar = {p_bar / 1e6:.5g} MPa",
                 "# one row per pore class; the porosities sum to xi",
                 f"# {'radius_um':>10s}{'porosity':>14s}{'pressure_MPa':>14s}{'confinement_MPa':>17s}"]
        lines += [f"{k['R'] * 1e6:12.6f}{xi * k['xi_share']:14.6e}{k['p'] / 1e6:14.4f}{Ph / 1e6:17.4f}"
                  for k in reduced_classes(d, c["T"], n_classes)]
        with open(os.path.join(folder, file), "w") as fh:
            fh.write("\n".join(lines) + "\n")
        index.append(f"  {c['id']:6s}{xi:8.4f}{d['sigma']:8.4f}{c['cv']:8.4f}{c['eta']:9.4f}{Ph / p_bar:10.3f}{c['R_V'] * 1e6:9.4f}{c['T']:9.1f}"
                     f"{p_bar / 1e6:11.2f}{Ph / 1e6:9.2f}  {file}")
    with open(os.path.join(folder, "index.txt"), "w") as fh:
        fh.write("\n".join(index) + "\n")
    print(f"wrote {len(cases)} run files and index.txt in {MMM_RUN_DIR}/")
    print("\n".join(index))


# %% ------------------------------------------------------------------------------------------------------------------
# Replay of option 4 on a SCIANTIX case

def read_output(path):
    """Columns of output.txt used here, by name; 'beta' is the (rows, 81) array of the broken fractions."""
    with open(path) as fh:
        names = [n.split(" (")[0].strip() for n in fh.readline().rstrip("\n").split("\t") if n.strip()]
    keys = ["Time", "Temperature", "Fission rate", "Hydrostatic stress", "Fission gas release", "Restructured volume fraction",
            "HBS porosity", "HBS pore density",
            "HBS pore volume", "HBS pore radius", "Xe atoms per HBS pore", "Xe atoms per HBS pore - variance",
            "HBS fragmented fraction", "HBS burst release fraction", "HBS gas retention fraction",
            "HBS gas retention fraction - second moment"]
    classes_ = [f"HBS broken fraction - class {i}" for i in range(N_CLASSES)]
    missing = [k for k in keys + classes_ if k not in names]
    if missing:
        raise SystemExit(f"{path}: not an output of iHighBurnupStructureFragmentation = 4 (no column '{missing[0]}')")
    data = np.loadtxt(path, skiprows=1, usecols=[names.index(k) for k in keys + classes_])
    out = {k: data[:, i] for i, k in enumerate(keys)}
    out["beta"] = data[:, len(keys):]
    for k in ("HBS gas retention fraction", "HBS gas retention fraction - second moment"):
        out[k] = np.where(out[k] > 0.0, out[k], 1.0)  # 0 before the first step of the model
    return out

def time_steps(case_dir, n_rows):
    """Time step (s) of every row of output.txt, by the loop of MainSCIANTIX.C and TimeStepCalculation.C: the times of
    output.txt have 10 digits and cannot give it."""
    t_in = np.loadtxt(os.path.join(case_dir, "input_history.txt"))[:, 0]
    n_steps = 100.0
    with open(os.path.join(case_dir, "input_settings.txt")) as fh:
        for line in fh:
            if "Number_of_time_steps_per_interval" in line:
                n_steps = float(line.split("#")[0])
    dt, t = [0.0], t_in[0]
    while t < t_in[-1]:
        lower = upper = 0.0
        for j in range(len(t_in) - 1):
            if t_in[j] <= t < t_in[j + 1]:
                lower, upper = t_in[j], t_in[j + 1]
        step = (upper - lower) / n_steps
        if t + step > upper:
            step = upper - t
        dt.append(step * 3600.0)
        t += step
    if len(dt) != n_rows:
        raise SystemExit(f"time loop gives {len(dt)} rows, output.txt has {n_rows}")
    return np.array(dt)

def state_of_row(o, k):
    """The state of row k of output.txt in the format of STATES: a row holds the retained moments and S, S_2 after the
    venting of the step, so the unvented moments are n / S and (Var + n^2) / S_2 - (n / S)^2 of the same row."""
    S, S2 = o["HBS gas retention fraction"][k], o["HBS gas retention fraction - second moment"][k]
    n = o["Xe atoms per HBS pore"][k] / S
    var = max((o["Xe atoms per HBS pore - variance"][k] + o["Xe atoms per HBS pore"][k] ** 2) / S2 - n**2, 0.0)
    return {"T": o["Temperature"][k], "Ph": -o["Hydrostatic stress"][k] * 1.0e6, "xi": o["HBS porosity"][k],
            "Np": o["HBS pore density"][k], "V": o["HBS pore volume"][k], "n": n, "var": var}

def states_agree(o, tol=1.0e-3):
    """STATES against the output of the NFIR case: the last row at full power and the row of the peak temperature of
    the transient. Prints the two states as read from the output; False if a value of STATES is off by more than tol
    (relative; STATES holds 4 digits)."""
    at_power = np.nonzero(o["Fission rate"] >= o["Fission rate"].max() * (1.0 - 1e-6))[0]
    k0 = transient_start(o)
    rows = {"NFIR, end of base irradiation": int(at_power[-1]), "NFIR, anneal at 1200 C": k0 + int(np.argmax(o["Temperature"][k0:]))}
    print("\n== STATES against the output of the case ==")
    worst = 0.0
    for name, k in rows.items():
        st = state_of_row(o, k)
        print(f'"{name}": ' + ", ".join(f'"{key}": {v:.6g}' if key == "T" else f'"{key}": {v:.4g}' for key, v in st.items()))
        for key, v in st.items():
            ref = STATES[name][key]
            worst = max(worst, abs(v - ref) / max(abs(ref), 1.0e6 if key == "Ph" else 0.0))  # P_h = 0 in the anneal: against 1 MPa
    print(f"largest relative difference from STATES: {worst:.1e}" + ("" if worst <= tol else f"  -> STATES is stale (more than {tol:.0e})"))
    return worst <= tol

def check(case_dir, tol=1.0e-6):
    """Replays the recursion of option 4 on the history of a SCIANTIX case and compares D, G, S, S_2 and the broken
    fraction of every class with output.txt. A row of output.txt holds the retained moments and S, S_2 after the
    venting of the step: the unvented moments are n / S and (Var + n^2) / S_2 of the same row. The state read from
    output.txt has 7 digits, so the agreement is not to round-off: tol [N] is above the digits of the file.
    Returns 0 if every difference is within tol and, on the NFIR case, STATES agrees with the output; 1 otherwise."""
    o = read_output(os.path.join(case_dir, "output.txt"))
    rows = len(o["Time"])
    dt = time_steps(case_dir, rows)
    beta, S1, S2, D, G = np.zeros(N_CLASSES), 1.0, 1.0, 0.0, 0.0
    err = {"D": 0.0, "G": 0.0, "S": 0.0, "S_2": 0.0, "beta_i": 0.0}
    row = dict.fromkeys(err, 0)
    for k in range(1, rows):
        alpha_old, alpha = o["Restructured volume fraction"][k - 1], o["Restructured volume fraction"][k]
        dilution = alpha_old / alpha if alpha > alpha_old > 0.0 else 1.0
        if dilution < 1.0:  # [R] the vented fractions 1 - S and 1 - S_2 are diluted like the broken fractions
            S1, S2 = 1.0 - (1.0 - S1) * dilution, 1.0 - (1.0 - S2) * dilution
        beta, d_star, g_star, f1, f2 = rupture_step(
            beta * dilution, o["HBS pore volume"][k], o["Xe atoms per HBS pore"][k], o["Xe atoms per HBS pore - variance"][k],
            o["HBS gas retention fraction"][k], o["HBS gas retention fraction - second moment"][k], o["Temperature"][k],
            o["HBS porosity"][k], -o["Hydrostatic stress"][k] * 1.0e6, o["HBS pore radius"][k], dt[k])
        D, G = max(D * dilution, d_star), max(G * dilution, g_star)
        S1, S2 = S1 * f1, S2 * f2
        for name, mine, theirs in (("D", D, o["HBS fragmented fraction"][k]), ("G", G, o["HBS burst release fraction"][k]),
                                   ("S", S1, o["HBS gas retention fraction"][k]),
                                   ("S_2", S2, o["HBS gas retention fraction - second moment"][k]),
                                   ("beta_i", 0.0, np.max(np.abs(beta - o["beta"][k])))):
            if abs(mine - theirs) > err[name]:
                err[name], row[name] = abs(mine - theirs), k
    print(f"\n== replay of option 4 on {case_dir} ({rows} rows) ==")
    print(f"final  D = {D:.6e} (SCIANTIX {o['HBS fragmented fraction'][-1]:.6e}),  G = {G:.6e} ({o['HBS burst release fraction'][-1]:.6e}),  "
          f"S = {S1:.6f} ({o['HBS gas retention fraction'][-1]:.6f})")
    for name in err:
        print(f"max |{name}: python - sciantix| = {err[name]:.2e}  (row {row[name]}, T = {o['Temperature'][row[name]]:.1f} K)")
    ok = max(err.values()) <= tol
    if not ok:
        print(f"the transcription and the C++ differ by more than {tol:.0e}")
    if os.path.samefile(case_dir, NFIR_CASE):
        ok = states_agree(o) and ok
    return 0 if ok else 1

def transient_start(o):
    """Row at which the out-of-pile transient starts: the first one without fissions after the base irradiation
    (the first row, if the history has no fissions)."""
    at_power = np.nonzero(o["Fission rate"] > 0.0)[0]
    return int(at_power[-1]) + 1 if len(at_power) else 0

def case_label(case_dir):
    """Name of a case in the figure files: regression/hbs_nfir/test_UO2HBS_frag_j19damage -> nfir_j19damage."""
    group, case = os.path.normpath(os.path.abspath(case_dir)).split(os.sep)[-2:]
    return group.replace("hbs_", "") + "_" + case.replace("test_UO2HBS_frag_", "")

def rupture_figure(case_dir, fractions=(0.25, 0.5, 1.0)):
    """Distribution and rupture of the classes of a SCIANTIX case, at the rows where D reaches the given fractions of
    its final value (or at the last row with S >= 0.1, if the gas is all vented before): number distribution with
    its broken part, pressure against critical pressure (2 gamma / R in grey), gas distribution with its vented part,
    and D, G against the temperature. Returns the figure and the name of the case."""
    o = read_output(os.path.join(case_dir, "output.txt"))
    D, G, T = o["HBS fragmented fraction"], o["HBS burst release fraction"], o["Temperature"]
    if D[-1] <= 0.0:
        raise SystemExit(f"{case_dir}: no class breaks")
    rows = [int(np.argmax(D >= f * D[-1] * (1.0 - 1e-6))) for f in fractions]
    # the unvented moments are n / S and (Var + n^2) / S_2: they cannot be rebuilt once the retained gas is negligible
    S_min = 0.1
    last_rebuildable = int(np.max(np.nonzero(o["HBS gas retention fraction"] >= S_min)))
    rows = sorted(set(min(k, last_rebuildable) for k in rows))
    first = int(np.argmax(D > 0.0))
    fig, ax = plt.subplots(2, 2, figsize=(11, 7.4))
    for k, color in zip(rows, ("#86b6ef", BLUE, "#104281")):
        st = state_of_row(o, k)
        n_u, xi, Ph = st["n"], st["xi"], st["Ph"]
        d = build(radius_of(st["V"]), n_u, st["var"], T[k])
        beta, R_um, dlnR = o["beta"][k], d["R"] * 1e6, np.gradient(np.log(d["R"]))
        label = f"T = {T[k]:.0f} K, D = {D[k]:.3f}, G = {G[k]:.3f}"
        ax[0, 0].plot(R_um, d["w"] / dlnR, color=color, lw=1.0)
        ax[0, 0].fill_between(R_um, d["w"] * beta / dlnR, color=color, alpha=0.45, lw=0, label=label)
        ax[0, 1].plot(R_um, d["p"] / 1e6, color=color, label=f"$p_i$, {T[k]:.0f} K")
        ax[0, 1].plot(R_um, critical_pressure(d["R"], xi, Ph) / 1e6, color=color, ls="--")
        gas = d["w"] * d["n"] / n_u / dlnR
        ax[1, 0].plot(R_um, gas, color=color, lw=1.0)
        ax[1, 0].fill_between(R_um, gas * beta, color=color, alpha=0.45, lw=0, label=label)
        ax[1, 1].plot(T[k], D[k], "o", color=color)
        ax[1, 1].plot(T[k], G[k], "s", color=color)
    ax[0, 1].plot(R_um, 2.0 * GAMMA / d["R"] / 1e6, color=GREY, label=r"$2\gamma/R$")
    ax[0, 1].plot([], [], "k--", label=r"$P_{cr,i}$ (dashed)")
    k0 = transient_start(o)
    heat = slice(max(first - 1, k0), k0 + int(np.argmax(T[k0:])) + 1)  # heat-up of the transient, from the first break
    ax[1, 1].plot(T[heat], D[heat], color=ORANGE, label="D (number)")
    ax[1, 1].plot(T[heat], G[heat], color=GREEN, label="G (gas)")
    ax[0, 0].set(xlabel="R (um)", ylabel="pdf in ln R", xscale="log", title="pores: all (line) and broken (filled)")
    ax[0, 1].set(xlabel="R (um)", ylabel="p (MPa)", xscale="log", yscale="log", title="pressure and critical pressure per class")
    ax[1, 0].set(xlabel="R (um)", ylabel="gas share per ln R", xscale="log", title="gas: all (line) and vented (filled)")
    ax[1, 1].set(xlabel="T (K)", ylabel="fraction", title="broken fractions on the heat-up")
    for ax_ in ax.ravel():
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False, fontsize=7)
    name = case_label(case_dir)
    fig.suptitle(f"{name}: classes rebuilt from output.txt, a = {A_CLOSURE}", fontsize=10)
    return fig, name

def fgr_figure(case_dir):
    """Fission gas release of the transient of a SCIANTIX case: the release of the base irradiation is subtracted, in
    per cent of the gas produced, against the time from the start of the transient and against the temperature of the
    heat-up. With the Kr-85 curve of the NFIR anneal (regression/hbs_nfir/data, Fig. 9 of the source) when the data
    folder of the case has it; the measured release is put on the temperature axis through the measured temperature
    history. Returns the figure and the name of the case."""
    o = read_output(os.path.join(case_dir, "output.txt"))
    k0 = transient_start(o)
    k1 = k0 + int(np.argmax(o["Temperature"][k0:]))  # peak temperature
    minutes = (o["Time"][k0:] - o["Time"][k0]) * 60.0
    release = 100.0 * (o["Fission gas release"][k0:] - o["Fission gas release"][k0])
    T_C = o["Temperature"][k0:] - 273.15
    fig, ax = plt.subplots(1, 3, figsize=(15, 4.0))
    ax[0].plot(minutes, release, color=ORANGE, label="SCIANTIX")
    heat = slice(0, k1 - k0 + 1)
    for ax_ in ax[1:]:
        ax_.plot(T_C[heat], release[heat], color=ORANGE, label="SCIANTIX")
    ax[2].plot(T_C[heat], 100.0 * o["HBS fragmented fraction"][k0:][heat], color=BLUE, ls="--", label="D (% of the pores)")
    ax[2].plot(T_C[heat], 100.0 * o["HBS burst release fraction"][k0:][heat], color=GREEN, ls="--", label="G (% of the pore gas)")
    data_dir = os.path.join(case_dir, "..", "data")
    if os.path.exists(os.path.join(data_dir, "CumulativeRelease.txt")):
        load = lambda name: np.loadtxt(os.path.join(data_dir, name), delimiter=";", comments="#", skiprows=2, encoding="utf-8")
        cum, temp = load("CumulativeRelease.txt"), load("TemperatureAnnealing.txt")
        ax[0].plot(cum[:, 0], cum[:, 1], "o", ms=3, mfc="none", color="k", label="Kr-85 (NFIR, Fig. 9)")
        ramp = cum[:, 0] <= temp[np.argmax(temp[:, 1]), 0]  # the measured release on the heat-up
        T_data = np.interp(cum[ramp, 0], temp[:, 0], temp[:, 1])
        for ax_ in ax[1:]:
            ax_.plot(T_data, cum[ramp, 1], "o", ms=3, mfc="none", color="k", label="Kr-85 (NFIR, Fig. 9)")
        print("\n== release on the heat-up, % of the gas produced ==")
        print(f"{'T (C)':>8s}{'SCIANTIX':>10s}{'Kr-85':>8s}")
        for T_ in (800.0, 900.0, 1000.0, 1100.0, 1150.0, 1200.0):
            if T_ <= T_C[heat].max() and T_data.min() <= T_ <= T_data.max():
                print(f"{T_:8.0f}{np.interp(T_, T_C[heat], release[heat]):10.2f}{np.interp(T_, T_data, cum[ramp, 1]):8.2f}")
    ax[0].set(xlabel="time from the start of the transient (min)", ylabel="release (% of the gas produced)", title="cumulative release")
    ax[1].set(xlabel="T (C)", ylabel="release (% of the gas produced)", title="release on the heat-up")
    ax[2].set(xlabel="T (C)", ylabel="%", yscale="log", ylim=(1e-3, 100.0), title="onset: release, D and G on the heat-up")
    for ax_ in ax:
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False, fontsize=7)
    name = case_label(case_dir)
    fig.suptitle(f"{name}: fission gas release of the transient, a = {A_CLOSURE}", fontsize=10)
    print(f"release at the end of the transient: {release[-1]:.2f} % of the gas produced; D = {o['HBS fragmented fraction'][-1]:.4f}, "
          f"G = {o['HBS burst release fraction'][-1]:.4f}, 1 - S = {1.0 - o['HBS gas retention fraction'][-1]:.4f}")
    return fig, name

def case_figures(case_dir):
    """The rupture and the release figures of a SCIANTIX case."""
    for figure, prefix in ((rupture_figure, "classes_rupture_"), (fgr_figure, "classes_fgr_")):
        fig, name = figure(case_dir)
        savefig(fig, f"{prefix}{name}.png")


# %% ------------------------------------------------------------------------------------------------------------------
# Main
def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--mmm", action="store_true",
                    help=f"write {MMM_RUN_DIR}/run_*.txt: one file per run of the coarse cover of the four factors of an RVE "
                         "(porosity, width of the radii, packing fraction, confinement), with radius, porosity, pressure and "
                         "confining pressure of every class")
    ap.add_argument("--classes", type=int, default=MMM_CLASSES, help=f"with --mmm: classes per run, odd, 3 to 9 (default {MMM_CLASSES})")
    ap.add_argument("--check", metavar="CASE_DIR", help="replay option 4 on a SCIANTIX case and compare with its output.txt")
    ap.add_argument("--rupture", metavar="CASE_DIR", help="rupture and release figures of a SCIANTIX case other than the NFIR one")
    args = ap.parse_args()
    if args.check:
        return check(args.check)
    if args.rupture:
        plt.switch_backend("Agg")
        case_figures(args.rupture)
        return 0
    if args.mmm:
        if args.classes % 2 == 0 or not 3 <= args.classes <= 9:
            ap.error("--classes is odd, from 3 to 9")
        mmm_run_files(mmm_design(), args.classes)
        return 0
    plt.switch_backend("Agg")  # the figures go to files
    section_eos()
    st, R_V, d, m = section_inputs()
    section_k15_check()
    section_truncation(d)
    section_distributions(st, d, m)
    section_cumulative(st, R_V, d)
    section_mean_pressure(st, R_V, d)
    section_closure_exponent(st, R_V)
    section_moment_checks(st, R_V)
    section_heatup(st, R_V)
    section_normalised(st)
    section_coverage()
    section_normalised_vs_direct()
    if os.path.exists(os.path.join(NFIR_CASE, "output.txt")):
        case_figures(NFIR_CASE)
    else:
        print(f"\nno output.txt in {NFIR_CASE}: run the case for the rupture and release figures")
    return 0

if __name__ == "__main__":
    sys.exit(main())
