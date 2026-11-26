"""
HBS pore classes from the SCIANTIX moments, rupture per class, and cases for the micromechanics (MMM).
@ author: E.Cappellari, POLIMI-CEA 2026
@ date:   October 2026

Python transcription of iHighBurnupStructureFragmentation = 4, described implementation in README.md.

SCIANTIX carries for the HBS pores 
    -   the mean volume V, 
    -   the mean Xe atoms per pore n
    -   and its variance Var(n). 

This script 

1.  rebuilds classes (R_i, V_i, w_i, n_i, p_i) that reproduce these three moments,
        sum_i w_i V_i = V,      sum_i w_i n_i = n,      sum_i w_i (n_i - n)^2 = Var(n),
    w_i is the weight of each class i, characterised by its own radius R_i and pressure p_i, with the pressure closure
    of the C++: every class at the same fraction f of the way from equilibrium to the punching limit (build());

2.  evaluates the rupture criterion on every class, and compares the pressure of the classes with the measured HBS
    pores (section_measured());

3.  writes the inputs of the runs of MMM (mmm_inputs/run_*.txt):
        -   radius R,
        -   porosity xi = N_p sum_i w_i V_i = N_p V, where N_p is the pore density,
        -   and load of every pore class: the in-pile net pressure on the wall of the pressure closure, f c_dp / R
        -   and the confining pressure (currently neglected).
    one file per run of the Latin hypercube in the three factors of the RVE
        -   porosity,
        -   width of the radii,
        -   packing fraction (it only sets f, the scale of the load)

4.  draws the rupture and release figures of the NFIR case of option 4 from its output.txt (NFIR_CASE).
Reference papers that will be cited in the following:
    -   K. Kulacsy, J. Nucl. Mater. 466 (2015) 409.             = K15
    -   L.O. Jernkvist, EPJ Nucl. Sci. Technol. 5 (2019) 11.    = J19
    -   OperaHPC Deliverable D5.2 (2026), section 2.2.          = OHP

usage
    python3 utilities/HBFF/pore_distribution.py
"""

# %% ------------------------------------------------------------------------------------------------------------------
# Libraries and paths
import os
import sys

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.cm import ScalarMappable
from matplotlib.colors import LinearSegmentedColormap, Normalize
from scipy.interpolate import RegularGridInterpolator
from scipy.optimize import brentq, least_squares
from scipy.special import ndtr  # standard normal cdf
from scipy.stats import qmc

HERE = os.path.dirname(os.path.abspath(__file__))
FIG_DIR = os.path.join(HERE, "figures")
NFIR_CASE = os.path.normpath(os.path.join(HERE, "..", "..", "regression", "hbs_nfir", "test_UO2HBS_frag_j19damage"))
DATA_DIR = os.path.join(HERE, "data")

# %% ------------------------------------------------------------------------------------------------------------------
# Constants (from papers and original SCIANTIX code)
K_B = 1.380651e-23  # (J/K) from include/namespaces/Constants.h 
N_A = 6.02214076e23  # (1/mol)
GAMMA = 1.1  # (N/m) UO2HBS, from SetMatrix.C
ETA_CAP = 0.65  # cap of the hard-sphere packing fraction, from HighBurnupStructurePorosity.C
SIGMA_R = 21.0e6  # (Pa) J19 Table 4: rupture stress that pairs with the mean-field stress concentration factor
XI_PERC = 0.29  # (/) J19, percolation of the HBS pores
# Punching limit of the overpressure of a pore, c_dp / R with c_dp = G b (Greenwood et al., J. Nucl. Mater. 1 (1959) 305;
# J19 2.3.2), as in the C++: G of dense UO2 at T (Pu = 0, x = 0, porosity 0: the matrix at the pore wall) from
# NEA/NSC/R(2024)1 Eq. 7.2; b = a / sqrt(2), the Burgers vector 1/2 <110> of the fluorite, with a = 4 / sqrt(3)
# (r_U4+ + r_O2-) of UO2 (NEA/NSC/R(2024)1 2.2, Table 2.3). J19 Table 4 takes 55 N/m (Gao et al. 2013), about 2 G b.
BURGERS = 4.0 / np.sqrt(3.0) * (0.9972e-10 + 1.372e-10) / np.sqrt(2.0)  # (m)
c_dp = lambda T: 82.52e9 * (1.009549 - 1.182e-5 * T - 6.671e-8 * T**2) * BURGERS  # (N/m) 30.1 at 903 K
K15_SIGMA = 0.356  # K15 Table 2: sigma_ln r of the reference pore-size distribution
NFIR_GAS_VOLUME = 1.518e-28  # (m3/atom) volume per gas atom in the pores of the NFIR disc, from the measured Xe
NFIR_BURNUP = 103.5  # (GWd/tHM) burnup of the NFIR disc 

# %% ------------------------------------------------------------------------------------------------------------------
# Model choices
N_CLASSES, SPAN = 81, 4.0  # nodes at ln R = ln R_median +/- 4 s
# Exponent of the gas closure n_i ~ R_i^a (2: gas density ~ 1/R, 3: uniform pressure), the closure of the C++ until
# 24/11/2026: kept as a bracket of the pressure closure (classes_closure.png) and in the unused checks at the end
A_CLOSURE = 2.0
# Design of the MMM runs
MMM_CLASSES = 7  # classes of an input
MMM_RUNS, MMM_SEED = 20, 1  # runs of the Latin hypercube and its seed
# Ranges of the factors.
#   xi   up to xi_sat of HighBurnupStructurePorosity.C.
#        Below 0.02 the cube of Merope/merope_pore_pressure.py holds fewer than two pores of each class at the ends.
#   s    SCIANTIX 0.05 to 0.42 (CV_n 0.10 to 1.00); NFIR disc 0.36; K15 0.356
#   eta  SCIANTIX 0.034 to 0.228, 0.048 to 0.228 for xi >= 0.02; NFIR disc 0.14 (1474 K) to 0.18 (293 K)
MMM_FACTORS = {"xi": (0.02, 0.18), "s": (0.05, 0.42), "eta": (0.045, 0.23)} #TBC
# Runs appended to the hypercube (0: none), R20 on: xi from the end of MMM_FACTORS up to MMM_XI_MAX, s and eta_bar in
# the strata the hypercube leaves empty, so that the 20 runs and the appended ones together are a Latin hypercube of
# MMM_RUNS + MMM_APPENDED levels with xi up to MMM_XI_MAX. The runs of the hypercube do not change.
# 15 runs to 0.30 keep the width of the xi levels (0.008); 0.29: J19 percolation, every class open above.
MMM_APPENDED, MMM_XI_MAX = 15, 0.30
MMM_PH_REL = 0.0  # P_h / p_bar of the runs
MMM_RUN_DIR = "mmm_inputs"
MMM_STATE = "NFIR, end of base irradiation" # geometry before the transient

# %% ------------------------------------------------------------------------------------------------------------------
# SCIANTIX states of regression/hbs_nfir/test_UO2HBS_frag_j19damage (formation 1, porosity 3), read from output.txt:
# the unvented moments n = n / S and var = (Var + n^2) / S_2 - n^2.
# V (m3), n (atoms per pore), var (Var(n)), xi (/), Np (pores/m3), T (K), Ph (Pa, positive in compression).
# These states are copied by hand: states_agree() compares them with output.txt, update them if the output changes.
# They do NOT enter the MMM runs (mmm_inputs/run_*.txt), except for T of MMM_STATE, an input of the history that sets
# the scale p_bar only: the run NFIR is the measured disc (nfir_disc(), NFIR_GAS_VOLUME) and the hypercube is the fixed
# box MMM_FACTORS around R_V of the disc. A change of the SCIANTIX output moves the SCIANTIX points of the figures,
# not the runs; check only that the new states still fall inside MMM_FACTORS.

STATES = {
    "NFIR, end of base irradiation": {"T": 903.15, "Ph": 0.0, "xi": 0.2549, "Np": 9.461e16, "V": 2.694e-18, "n": 6.241e9, "var": 1.175e19},
    "NFIR, anneal at 1200 C": {"T": 1473.5, "Ph": 0.0, "xi": 0.2493, "Np": 9.265e16, "V": 2.691e-18, "n": 6.373e9, "var": 1.199e19},
}
# The case uses 0.12 # sf_hbs_pore_nucleation_rate. The same case without that line (nucleation x 1), end of base
# irradiation, from a run with iHighBurnupStructureFragmentation = 0: with option 4 every pore breaks from 78 MWd/kgUO2
# and the end of base holds no intact pore (before the first break the two runs are identical). Copied by hand, not
# checked by states_agree(): drawn next to STATES in mmm_gas_volume.png, mmm_pore_radius.png and
# classes_pressure_measured.png only.
STATE_NUCLEATION_X1 = {"T": 903.15, "Ph": 0.0, "xi": 0.3052, "Np": 6.577e17, "V": 4.640e-19, "n": 8.974e8, "var": 2.229e17}

# %% ------------------------------------------------------------------------------------------------------------------
# Colours of the figures
BLUE, ORANGE, GREEN, GREY, BROWN, GRID = "#2a78d6", "#eb6834", "#2f9e6e", "#898781", "#3e1c0f", "#e1e0d9"
SEQ_BLUE = ("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281")
SEQ_ORANGE = ("#f2b094", "#ee8c62", "#eb6834", "#b54a1f", "#7c300f")

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
# 1. Radius. Log-normal (K15 Eq. 6) on N_CLASSES nodes x_i equally spaced in [-SPAN, SPAN], 
#       ln R_i = const + s x_i, with the number weights w_i ~ exp(-x_i^2 / 2). 
#    The tails beyond +/- SPAN s are dropped. 
#    The constant is fixed so that the third moment is exact on the nodes: sum(w R^3)^(1/3) = R_V.
# 2. Gas: the pressure closure (README 3.2). In the base irradiation the pressure of a pore lies between the
#    equilibrium P_h + 2 gamma / R (pore fed by vacancies) and the punching limit P_h + (2 gamma + c_dp) / R (J19 Eq. 21:
#    above it the pore grows by punching). At every state (T, P_h), in-pile and in a transient, every class is at the
#    same fraction f of the way:
#       p_i = P_h + (2 gamma + f c_dp) / R_i,     n_i = EoS^-1(p_i, V_i, T)
#    f and the width s are the roots of sum_i w_i n_i = n and sum_i w_i (n_i - n)^2 = Var(n) on the truncated nodes.
#    f = 0 is the equilibrium, f = 1 the punching limit, f > 1 more gas than punching allows (no cap). The net
#    pressure of a class is exactly f c_dp / R_i. The gas closure n_i ~ R_i^a of the C++ until 24/11 is build_gas(),
#    a bracket.
# 3. Pressure. Hard-sphere EoS on every class.
#    The pressure of the mean quantities, p_EoS(n, V, T) is the pressure of the volume-weighted mean density.

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

def build_gas(R_V, n_mean, var_n, T, a):
    """Classes of the gas closure n_i = n_mean R_i^a / <R^a> (the C++ until 24/11/2026), with p_i from the EoS at T.
    Var(n) = 0 gives s = 0: all the classes coincide with the mean pore."""
    s = width(var_n / n_mean**2, a)
    R, V, w = classes(R_V, s)
    n_i = n_mean * R**a / np.sum(w * R**a)
    return {"R": R, "V": V, "w": w, "n": n_i, "p": pore_pressure(n_i, V, T), "sigma": s,
            "R_median": R[N_CLASSES // 2], "a": a}

def gas(p, V, T):
    """Xe atoms of pores of volume V at the pressure p and temperature T: the inverse of pore_pressure (linear in n
    above eta = 0.65), by bisection on eta Z(eta); 0 where p <= 0."""
    p, V = np.broadcast_arrays(np.asarray(p, float), np.asarray(V, float))
    v = xe_volume(T)
    y = p * v / (K_B * T)  # eta Z(eta) below the cap
    lo, hi = np.zeros_like(y), np.full_like(y, ETA_CAP)
    for _ in range(60):
        mid = 0.5 * (lo + hi)
        below = mid * Z(mid) < y
        lo, hi = np.where(below, mid, lo), np.where(below, hi, mid)
    linear = y >= ETA_CAP * Z(ETA_CAP)
    return np.where(linear, p * V / (K_B * T * Z(ETA_CAP)), np.where(p > 0.0, 0.5 * (lo + hi) * V / v, 0.0))

def build(R_V, n_mean, var_n, T, Ph=0.0, s=None):
    """Classes of the pressure closure (option 4) that hold V (through R_V), n and Var(n) at the state (T, Ph):
    p_i = Ph + (2 gamma + f c_dp) / R_i and n_i = gas(p_i, V_i, T). f and s are the roots of the mean and of the
    variance of n; with s given, f is the root of the mean alone (var_n is not used). Also returns f and f_min, the f
    below which the smallest class has no gas."""

    def at(s_):
        R, V, w = classes(R_V, s_)
        n_of = lambda f_: gas(Ph + (2.0 * GAMMA + f_ * c_dp(T)) / R, V, T)
        f_lo = np.min(-(Ph * R + 2.0 * GAMMA) / c_dp(T))  # every class at p <= 0: no gas
        f_hi = max(1.0, f_lo + 1.0)
        while np.sum(w * n_of(f_hi)) < n_mean:
            f_hi = f_lo + 2.0 * (f_hi - f_lo)
        f_ = brentq(lambda q: np.sum(w * n_of(q)) / n_mean - 1.0, f_lo, f_hi, xtol=1e-13, rtol=1e-14)
        return R, V, w, n_of(f_), f_

    if s is None:
        cv2 = var_n / n_mean**2
        s = 0.0
        if cv2 > 0.0:
            def excess(s_):
                _, _, w, n_i, _ = at(s_)
                return np.sum(w * (n_i - n_mean) ** 2) / n_mean**2 / cv2 - 1.0
            s_hi = np.sqrt(np.log1p(cv2)) / 3.0  # the continuous log-normal with n ~ R^3: doubled until it brackets
            while excess(s_hi) < 0.0:
                s_hi *= 2.0
            s = brentq(excess, 0.0, s_hi, xtol=1e-15, rtol=1e-14)
    R, V, w, n_i, f = at(s)
    return {"R": R, "V": V, "w": w, "n": n_i, "p": pore_pressure(n_i, V, T), "sigma": s, "R_median": R[N_CLASSES // 2],
            "f": f, "f_min": np.max(-(Ph * R + 2.0 * GAMMA) / c_dp(T))}

def local_exponent(d):
    """d ln n / d ln R of the classes: a of the gas closure, a function of R for the pressure closure."""
    return np.gradient(np.log(d["n"]), np.log(d["R"]))

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
# [TBC]
#
# Class i breaks when the first principal stress at the pore wall exceeds the rupture stress (OHP 2.2.2):
#     s_hat (p_i - 2 gamma / R_i - P_h) >= sigma_R + P_h    
#       <=>    p_i >= P_cr,i = P_h + 2 gamma / R_i + (sigma_R + P_h) / s_hat
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

# The s_hat of MMM (mmm_surrogate.py: log-normal per class, from the Latin hypercube) replaces this pair in the C++.

def porosity_per_bin(xi, R_V, s, edges):
    """Porosity in the bins of pore diameter `edges` (m) of a log-normal of ln R with the porosity xi, the radius of
    the mean pore volume R_V and the width s, tails included: the porosity-weighted ln R is Gaussian with the same s
    and the median R_V exp(1.5 s^2)."""
    with np.errstate(divide="ignore"):  # the edge at zero diameter
        z = (np.log(0.5 * edges) - np.log(R_V) - 1.5 * s**2) / s
    return xi * np.diff(ndtr(z))

def dimensionless(st):
    """The dimensionless quantities of a state: xi, CV_n, eta_bar = n v_Xe(T) / V."""
    return st["xi"], np.sqrt(st["var"]) / st["n"], st["n"] * xe_volume(st["T"]) / st["V"]

# %% ------------------------------------------------------------------------------------------------------------------
# Data

# Measured pores of the NFIR disc, J. Noirot et al., J. Nucl. Mater. 446 (2014) 163, Fig. 12: the bars of
# DATA_DIR/PoreRadius*.txt are the porosity, in per cent of the surface, per 0.2 um of pore diameter from 0 to 4 um,
# before and after the anneal.
def nfir_disc(name="PoreRadiusASIRRADIATED.txt"):
    """Pores of the NFIR disc, with the least-squares fit of a log-normal:
    the bars (porosity per bin), the edges of the bins (m) and the fitted xi, R_V (m), s."""
    with open(os.path.join(DATA_DIR, name), encoding="utf-8") as fh:
        bars = np.array([float(line.split(";")[1]) for line in fh if line.startswith("Bar")]) / 100.0
    edges = 0.2e-6 * np.arange(len(bars) + 1)
    fit = least_squares(lambda q: porosity_per_bin(q[0], q[1] * 1e-6, q[2], edges) - bars, x0=(bars.sum(), 0.5, 0.3),
                        bounds=(1e-3, (1.0, 5.0, 2.0)))  # xi, R_V (um), s
    return {"bars": bars, "edges": edges, "xi": fit.x[0], "R_V": fit.x[1] * 1e-6, "s": fit.x[2]}

# Average volume per fission-gas atom in the HBS bubbles against the local burnup, J. Noirot et al., J. Nucl. Mater.
# 446 (2014) 163, Fig. 17: the file in DATA_DIR and the label of every series.
GAS_VOLUME_SERIES = {"AtomicVolumePERIPHERY.txt": "UO2 and MOX, periphery (Noirot et al., 2008)",
                     "AtomicVolumeMIDRADIUS.txt": "MOX, mid-radius Pu-rich agglomerates (Noirot et al., 2008)",
                     "AtomicVolumeROD07.txt": "NFIR UO2 disc, rod 07 (Noirot et al., 2014)"}

def gas_volume_data():
    """The series of GAS_VOLUME_SERIES: {label: (local burnup (GWd/tHM), volume per gas atom (A^3))}, from the lines
    'burnup;volume' of the files."""
    out = {}
    for name, label in GAS_VOLUME_SERIES.items():
        rows = []
        with open(os.path.join(DATA_DIR, name), encoding="utf-8") as fh:
            for line in fh:
                fields = line.split(";")
                try:
                    rows.append((float(fields[0]), float(fields[1])))
                except (ValueError, IndexError):
                    pass  # comments, header and description
        out[label] = np.array(rows).T
    return out

# Pores measured by R. Dowek et al., J. Nucl. Mater. 543 (2021) 152591 (D21), in DATA_DIR: Table 1, the collection of
# the literature (PorePressureCOLLECTION.txt); Table 2, samples A (ramped) and B; Table 3, sample B along the radius;
# Tables 4 and 5, single bubbles of samples A and B. Molar volumes of the fission gas (Xe + Kr), pressures from the
# Soave equation of state.
def number(text):
    """First number of a cell ('1878 (ramp)' -> 1878.0); nan for 'nan', a range or no number."""
    try:
        return float(text.split()[0])
    except (ValueError, IndexError):
        return np.nan

def read_columns(name):
    """A table of DATA_DIR written by columns, 'quantity;value;value;...': {quantity: [cells]}."""
    with open(os.path.join(DATA_DIR, name), encoding="utf-8") as fh:
        rows = [line.rstrip("\n").split(";") for line in fh if line.strip() and not line.startswith("#")]
    return {r[0].strip(): [v.strip() for v in r[1:]] for r in rows}

def cells(table, prefix):
    """The row of a read_columns table whose quantity starts with prefix."""
    return next(v for k, v in table.items() if k.startswith(prefix))

def literature_pores():
    """Rows of D21 Table 1: reference, local burnup (GWd/tU), location, mean bubble diameter D (m), porosity xi and
    volume per gas atom v (m3); nan where the table has none or a range."""
    with open(os.path.join(DATA_DIR, "PorePressureCOLLECTION.txt"), encoding="utf-8") as fh:
        rows = [line.rstrip("\n").split(";") for line in fh if line.strip() and not line.startswith("#")][1:]
    return [{"ref": r[0], "bu": number(r[2]), "where": r[3], "D": number(r[5]) * 1e-6, "xi": number(r[6]) / 100.0,
             "v": number(r[7]) * 1e-5 / N_A} for r in rows]

def single_bubbles():
    """Single bubbles of D21 (Tables 4, 5): sample, position, R (m), v (m3 per gas atom), the Soave pressures p_293 and
    p_irr (Pa) and T_irr (K), the temperature of irradiation of the position from Table 2: the centre for 0R, the rim
    for 1R, nan elsewhere."""
    t2 = read_columns("Dowek2021_T2.txt")
    T_irr = {(s, pos): number(cells(t2, f"Temperature in the {where}")[k]) + 273.15
             for k, s in enumerate("AB") for pos, where in (("0R", "centre"), ("1R", "rim"))}
    out = []
    for sample, name in (("A", "Dowek2021_T4.txt"), ("B", "Dowek2021_T5.txt")):
        t = read_columns(name)
        for k, pos in enumerate(t["Position"]):
            out.append({"sample": sample, "position": pos, "R": 0.5e-6 * number(cells(t, "Equivalent spherical diameter")[k]),
                        "v": number(cells(t, "Molar volume")[k]) / N_A, "p_293": 1e6 * number(cells(t, "Pressure at 293 K")[k]),
                        "p_irr": 1e6 * number(cells(t, "Pressure at irradiated")[k]), "T_irr": T_irr.get((sample, pos), np.nan)})
    return out

def dowek_gas_volumes():
    """Every volume per gas atom of D21: the means of sample B along the radius (Table 3, kind 'mean') and the single
    bubbles of samples A and B (Tables 4, 5, kind 'single bubble'), with sample, position, v (m3) and bu, the average
    burnup of the sample (GWd/tU, Table 2); the local burnup of the positions is not given."""
    t2, t3 = read_columns("Dowek2021_T2.txt"), read_columns("Dowek2021_T3.txt")
    bu = dict(zip("AB", map(number, cells(t2, "Burn-up"))))
    means = [{"sample": "B", "position": pos, "v": number(v) / N_A, "kind": "mean"}
             for pos, v in zip(t3["Position"], cells(t3, "Molar volume"))]
    return [dict(d, bu=bu[d["sample"]]) for d in means + [dict(b, kind="single bubble") for b in single_bubbles()]]

FIGURES = []  # names of the figures written, listed once at the end of main()

def savefig(fig, name):
    os.makedirs(FIG_DIR, exist_ok=True)
    fig.tight_layout()
    fig.savefig(os.path.join(FIG_DIR, name), dpi=150)
    plt.close(fig)
    FIGURES.append(name)

def print_figures():
    print(f"\nwrote {len(FIGURES)} figures in figures/: " + ", ".join(FIGURES))

# %% ------------------------------------------------------------------------------------------------------------------
# Sections: tables on screen and figures

def section_inputs():
    """Inputs and test of the conserved quantities, on the NFIR state at the end of the base irradiation."""
    st = STATES["NFIR, end of base irradiation"]
    R_V = radius_of(st["V"])
    d = build(R_V, st["n"], st["var"], st["T"], Ph=st["Ph"])
    m = moments(d)
    err = check_moments(d, st["V"], st["n"], st["var"])  # raises if a moment is off by more than 1e-8
    xi, cv, eta = dimensionless(st)
    print("\n== NFIR, end of base irradiation ==")
    print(f"xi = {xi:.4f}, CV_n = {cv:.4f}, eta_bar = {eta:.4f}, R_V = {R_V * 1e6:.4f} um, P_h = {st['Ph'] / 1e6:.0f} MPa; "
          f"pressure closure: f = {d['f']:.4f}")
    print(f"sigma_lnR = {d['sigma']:.4f} (K15 reference {K15_SIGMA}), median = {d['R_median'] * 1e6:.4f} um, "
          f"R from {d['R'].min() * 1e6:.3f} to {d['R'].max() * 1e6:.3f} um")
    print(f"V, n, Var(n) of the classes within {max(err[k] for k in ('V', 'n', 'var_n')):.0e} of the targets; "
          f"<p> = {m['p_mean'] / 1e6:.1f} MPa")
    return st, R_V, d, m

def section_nfir_data():
    """The measured pores of the NFIR disc in the variables of the classes, next to the SCIANTIX states
        -   porosity,
        -   radius of the mean pore volume and 
        -   width from the fit of nfir_disc, 
        -   mean packing fraction from the volume per gas atom."""
    print("\n== NFIR disc, measured pores (log-normal fit of the bars) against the SCIANTIX states ==")
    print("CV_n and f of the disc: pressure closure at the measured s and gas density, 903 K, P_h = 0; SCIANTIX: at the "
          "T, P_h of the state")
    print(f"{'':34s}{'xi':>8s}{'R_V (um)':>10s}{'s':>8s}{'CV_n':>8s}{'eta_bar':>9s}{'f':>8s}")
    inside = lambda key, value: MMM_FACTORS[key][0] <= value <= MMM_FACTORS[key][1]
    T_mmm = STATES[MMM_STATE]["T"]
    for label, name in (("disc, as irradiated", "PoreRadiusASIRRADIATED.txt"), ("disc, after the anneal", "PoreRadiusPOSTANNEALING.txt")):
        disc = nfir_disc(name)
        V_ = sphere_volume(disc["R_V"])
        n_ = V_ / NFIR_GAS_VOLUME  # atoms: the volume over the volume per gas atom
        d = build(disc["R_V"], n_, None, T_mmm, s=disc["s"])
        cv = np.sqrt(np.sum(d["w"] * (d["n"] - n_) ** 2)) / n_
        print(f"{label:34s}{disc['xi']:8.4f}{disc['R_V'] * 1e6:10.4f}{disc['s']:8.4f}{cv:8.4f}{xe_volume(T_mmm) / NFIR_GAS_VOLUME:9.4f}{d['f']:8.3f}")
        assert inside("xi", disc["xi"]) and inside("s", disc["s"]), label
    for name, st in STATES.items():
        xi, cv, eta = dimensionless(st)
        d = build(radius_of(st["V"]), st["n"], st["var"], st["T"], st["Ph"])
        eta_disc = xe_volume(st["T"]) / NFIR_GAS_VOLUME
        print(f"{'SCIANTIX, ' + name.removeprefix('NFIR, '):34s}{xi:8.4f}{radius_of(st['V']) * 1e6:10.4f}{d['sigma']:8.4f}{cv:8.4f}{eta:9.4f}"
              f"{d['f']:8.3f}   (disc at {st['T']:.0f} K: eta_bar = {eta_disc:.4f})")
        assert inside("eta", eta_disc), name

def section_distributions(st, d, m):
    """Radius pdf, pressure per class and gas per class."""
    R_um = d["R"] * 1e6
    pdf = d["w"] / np.gradient(np.log(d["R"]))  # pdf of ln R: integrates to 1 over ln R
    p_eos = float(pore_pressure(m["n"], m["V"], st["T"])) / 1e6  # EoS of the mean quantities, conserved
    fig, ax = plt.subplots(1, 3, figsize=(12, 3.8))
    for ax_ in ax:
        ax_.axvline(m["R_V"] * 1e6, color=BROWN, lw=1.2, label=r"$R_V$ (mean volume)")
    ax[0].plot(R_um, pdf, color=BLUE)
    ax[0].set(xlabel="R (um)", ylabel="pdf in ln R", title=f"radii, s = {d['sigma']:.3f}")
    ax[1].plot(R_um, d["p"] / 1e6, color=ORANGE, label=r"$p_i$")
    ax[1].axhline(p_eos, color="#ab522f", label=r"$p_{EoS}(\bar n,\bar V)$ = $\bar p$")
    ax[1].set(xlabel="R (um)", ylabel="p (MPa)", title=f"pressure per class, T = {st['T']:.0f} K")
    ax[2].plot(R_um, d["n"], color=GREEN, label=r"$n_i$")
    ax[2].axhline(m["n"], color="#887B75", label=r"$\bar n$")
    ax[2].set(xlabel="R (um)", ylabel="n", title="Xe atoms per class")
    for ax_ in ax:
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False, loc="upper right")
    savefig(fig, "classes_distributions.png")

def section_mean_pressure(st, R_V, d):
    """Mean pressure and what is conserved. p_EoS(n, V, T) is the pressure of the volume-weighted mean density
    rho_vol = sum(w n) / sum(w V) = n / V, conserved. The number-weighted mean <p_i> is not, and cannot be together
    with n and V when the pressure is higher in the small pores: <rho> = n / V - Cov(rho, V) / V, with
    Cov(rho, V) < 0 when rho falls with V, and p(rho) convex gives <p_i> > p_EoS. Imposing <p_i> = p_EoS forces
    Cov = 0, a uniform pressure (the gas closure with a = 3)."""
    T = st["T"]
    w, Vi, ni, pi = d["w"], d["V"], d["n"], d["p"]
    p_bar = float(pore_pressure(st["n"], st["V"], T))
    rho_vol = np.sum(w * ni) / np.sum(w * Vi)
    p_rho_vol = float(pore_pressure(rho_vol, 1.0, T))  # EoS at the density rho_vol: V = 1 m3, n = rho_vol atoms
    means = [np.sum(wt * pi) / np.sum(wt) / 1e6 for wt in (w, w * Vi, w * ni)]
    print("\n== mean pressure ==")
    print(f"p_EoS(n, V, T) = {p_bar / 1e6:.1f} MPa; <p> weighted by number / volume / gas = "
          + " / ".join(f"{v:.1f}" for v in means) + " MPa")
    if abs(p_rho_vol / p_bar - 1.0) > 1e-10:
        print(f"p(rho_vol) = {p_rho_vol / 1e6:.3f} MPa differs from p_EoS: the volume-weighted density is not n / V")


def section_closure(st, R_V):
    """The pressure closure against the gas closure n_i ~ R_i^a (a = 2, the C++ until 24/11; a = 3, one pressure) at
    the same moments: pressure of the classes between the equilibrium P_h + 2 gamma / R and the punching limit
    P_h + (2 gamma + c_dp) / R of J19 Eq. 21, and the local exponent d ln n_i / d ln R_i."""
    T, Ph = st["T"], st["Ph"]
    print(f"\n== closures of the gas per class, {T:.0f} K, P_h = {Ph / 1e6:.0f} MPa ==")
    print(f"{'closure':18s}{'s':>8s}{'f':>8s}{'p_min':>8s}{'p_max':>8s}{'a_loc':>12s}   (MPa)   classes above the punching limit")
    ds = {"pressure closure": build(R_V, st["n"], st["var"], T, Ph=Ph),
          "a = 2": build_gas(R_V, st["n"], st["var"], T, 2.0), "a = 3": build_gas(R_V, st["n"], st["var"], T, 3.0)}
    style = {"pressure closure": (ORANGE, "-", 2.2), "a = 2": (BLUE, "-", 1.2), "a = 3": (GREEN, "--", 1.2)}
    fig, ax = plt.subplots(1, 2, figsize=(11.0, 4.0))
    for label, d in ds.items():
        check_moments(d, st["V"], st["n"], st["var"])  # raises if a moment is off
        a_loc = local_exponent(d)
        above = np.sum(d["w"][d["p"] > Ph + (2.0 * GAMMA + c_dp(T)) / d["R"]])
        f_txt = f"{d['f']:8.3f}" if "f" in d else f"{'-':>8s}"
        print(f"{label:18s}{d['sigma']:8.4f}{f_txt}{d['p'].min() / 1e6:8.0f}{d['p'].max() / 1e6:8.0f}"
              f"{a_loc.min():6.2f}-{a_loc.max():<5.2f}   {above:18.4f}")
        color, ls, lw = style[label]
        ax[0].plot(d["R"] * 1e6, d["p"] / 1e6, ls, color=color, lw=lw, label=label + (f", f = {d['f']:.2f}" if "f" in d else ""))
        ax[1].plot(d["R"] * 1e6, a_loc, ls, color=color, lw=lw, label=label)
    R = np.geomspace(ds["a = 2"]["R"].min(), ds["a = 2"]["R"].max(), 100)
    ax[0].plot(R * 1e6, (Ph + 2.0 * GAMMA / R) / 1e6, ":", color=GREY, lw=1.2, label=r"equilibrium $P_h+2\gamma/R$")
    ax[0].plot(R * 1e6, (Ph + (2.0 * GAMMA + c_dp(T)) / R) / 1e6, ":", color="k", lw=1.2,
               label=rf"punching $P_h+(2\gamma+Gb)/R$, $Gb$ = {c_dp(T):.1f} N/m (NEA)")
    ax[0].set(xlabel="R (um)", ylabel="p (MPa)", xscale="log", yscale="log",
              title=f"SCIANTIX NFIR, end of base: T = {T:.0f} K, $P_h$ = {Ph / 1e6:.0f} MPa")
    ax[1].set(xlabel="R (um)", ylabel=r"$d\ln n_i/d\ln R_i$", xscale="log", title="gas per class: local exponent")
    for ax_ in ax:
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False, fontsize=8)
        ax_.title.set_fontsize(9)
        ax_.set_xticks((0.2, 0.3, 0.5, 0.7, 1.0, 2.0), labels=("0.2", "0.3", "0.5", "0.7", "1", "2"))
        ax_.tick_params(which="minor", labelbottom=False, labelleft=False)
    ax[0].set_yticks((30, 50, 100, 200, 500, 1000), labels=("30", "50", "100", "200", "500", "1000"))
    savefig(fig, "classes_closure.png")

def measured_pores():
    """Mean measured HBS pores with a radius and a volume per gas atom: the NFIR disc (Noirot 2014: R_V of the fit and
    NFIR_GAS_VOLUME), the rows of D21 Table 1 at the rim or on a disc (R = D / 2) and the two single bubbles of D21
    sample B at the rim (Table 5, with the Soave pressure of D21). T of the irradiation: 903 K for the discs (MMM_STATE),
    650 K for the rim rows of Table 1 (the rim temperature of the evaluation of Noirot 2008), the rim temperature of
    D21 Table 2 for sample B. The P_h of the irradiation is not known for any of them."""
    T_disc = STATES[MMM_STATE]["T"]
    out = [{"name": "NFIR disc, Noirot 2014 (R_V of the fit)", "short": "NFIR disc (fit)", "R": nfir_disc()["R_V"],
            "v": NFIR_GAS_VOLUME, "T": T_disc, "p_soave": np.nan}]
    for r in literature_pores():
        if r["where"] in ("1R", "fuel disc") and np.isfinite(r["D"]) and np.isfinite(r["v"]):
            disc = r["where"] == "fuel disc"
            out.append({"name": f"{r['ref']}, {r['where']}, {r['bu']:.0f} GWd/t (R = D/2)",
                        "short": "NFIR disc (D/2)" if disc else f"{r['ref'].split(' ')[0]} rim {r['bu']:.0f} GWd/t",
                        "R": 0.5 * r["D"], "v": r["v"], "T": T_disc if disc else 650.0, "p_soave": np.nan})
    for b in sorted((b for b in single_bubbles() if b["sample"] == "B" and b["position"] == "1R"), key=lambda b: b["R"]):
        out.append({"name": "Dowek et al., 2021, sample B, rim, single bubble", "short": "Dowek B rim", "R": b["R"], "v": b["v"],
                    "T": b["T_irr"], "p_soave": b["p_irr"]})
    return out

def overpressure_fraction(p, R, Ph, T):
    """f of a pore of radius R at the pressure p and T: (p - P_h - 2 gamma / R) R / c_dp(T), 0 at equilibrium, 1 at
    punching."""
    return (p - Ph - 2.0 * GAMMA / R) * R / c_dp(T)

def section_measured(st):
    """Level and shape of the pressure closure against the measured pores (measured_pores()): f of every pore at
    P_h = 0 / 30 / 70 MPa with the hard spheres (HS) and with the Soave pressure of D21, the c_dp that would put it at
    f = 1 (P_h = 0), next to the f of the SCIANTIX state; then the only pair in one population (D21 sample B, rim):
    the measured local exponent d ln n / d ln R against the pressure closure."""
    PH = (0.0, 30e6, 70e6)
    pores = measured_pores()
    d = build(radius_of(st["V"]), st["n"], st["var"], st["T"], Ph=st["Ph"])
    R_b, p_b = radius_of(st["V"]), float(pore_pressure(st["n"], st["V"], st["T"]))
    print(f"\n== measured HBS pores against the pressure closure: f at P_h = 0 / 30 / 70 MPa; c_dp1 = (p - 2 gamma / R) R, "
          f"the c_dp that gives f = 1 at P_h = 0 (N/m; c_dp = G b = {c_dp(st['T']):.1f} N/m at {st['T']:.0f} K) ==")
    print(f"{'pore':52s}{'R_um':>6s}{'v_A3':>6s}{'T_K':>5s}{'eta':>7s}{'p_HS':>6s}{'p_So':>6s}{'f (HS)':>21s}{'f (Soave)':>21s}{'c_dp1':>7s}")
    f_txt = lambda p, R, T: " / ".join(f"{overpressure_fraction(p, R, Ph, T):.2f}" for Ph in PH) if np.isfinite(p) else "-"
    for q in pores:
        q["eta"] = xe_volume(q["T"]) / q["v"]
        q["p"] = float(pore_pressure(q["eta"] / xe_volume(q["T"]), 1.0, q["T"]))  # eta / v_Xe atoms in 1 m3
        print(f"{q['name']:52s}{q['R'] * 1e6:6.3f}{q['v'] * 1e30:6.0f}{q['T']:5.0f}{q['eta']:7.3f}{q['p'] / 1e6:6.0f}"
              f"{q['p_soave'] / 1e6 if np.isfinite(q['p_soave']) else np.nan:6.0f}{f_txt(q['p'], q['R'], q['T']):>21s}{f_txt(q['p_soave'], q['R'], q['T']):>21s}"
              f"{(q['p'] - 2.0 * GAMMA / q['R']) * q['R']:7.0f}")
    f_b = f"{overpressure_fraction(p_b, R_b, st['Ph'], st['T']):.2f} at P_h = {st['Ph'] / 1e6:.0f}"
    print(f"{'SCIANTIX NFIR, end of base (mean pore)':52s}{R_b * 1e6:6.3f}{st['V'] / st['n'] * 1e30:6.0f}{st['T']:5.0f}"
          f"{st['n'] * xe_volume(st['T']) / st['V']:7.3f}{p_b / 1e6:6.0f}{'':6s}{f_b:>21s}"
          f"   classes: f = {d['f']:.3f}")
    b1, b2 = [q for q in pores if q["short"] == "Dowek B rim"]
    n1, n2 = sphere_volume(b1["R"]) / b1["v"], sphere_volume(b2["R"]) / b2["v"]
    print(f"Dowek B rim, {b1['T']:.0f} K: R {b1['R'] * 1e6:.3f} -> {b2['R'] * 1e6:.3f} um, gas atoms {n1:.3e} -> {n2:.3e}: "
          f"d ln n / d ln R = {np.log(n2 / n1) / np.log(b2['R'] / b1['R']):.2f} measured; pressure closure from the smaller "
          "bubble: " + ", ".join(f"{np.log(gas(Ph + (b1['p'] - Ph) * b1['R'] / b2['R'], sphere_volume(b2['R']), b1['T']) / n1) / np.log(b2['R'] / b1['R']):.2f}"
                                  f" at P_h = {Ph / 1e6:.0f} MPa" for Ph in (0.0, 70e6)) + "; gas closure: a")

    fig, ax = plt.subplots(figsize=(7.0, 4.6))
    ax.axhspan(0.0, 1.0, color=GREY, alpha=0.15, lw=0, label="f = 0 to 1: equilibrium to punching")
    ink = "#52514e"
    for q in pores:
        for p, ls, mfc, dx in ((q["p"], "-", ink, 1.0), (q["p_soave"], "--", "white", 1.04)):
            if np.isfinite(p):
                x_ = q["R"] * 1e6 * dx
                ax.plot([x_, x_], [overpressure_fraction(p, q["R"], 70e6, q["T"]), overpressure_fraction(p, q["R"], 0.0, q["T"])], ls, color=ink, lw=1.4)
                ax.plot(x_, overpressure_fraction(p, q["R"], 30e6, q["T"]), "o", color=ink, mfc=mfc, ms=7, mew=1.2)
        ax.annotate(q["short"], (q["R"] * 1e6, overpressure_fraction(q["p"], q["R"], 0.0, q["T"])), xytext=(4, 4), textcoords="offset points",
                    fontsize=7, color=ink)
    ax.plot(R_b * 1e6, d["f"], "s", color=ORANGE, ms=9, mec="white", mew=0.8,
            label=f"SCIANTIX NFIR, end of base ($P_h$ = {st['Ph'] / 1e6:.0f} MPa), nucleation x0.12: f of the classes")
    st1 = STATE_NUCLEATION_X1  # the same case without the nucleation factor
    d1 = build(radius_of(st1["V"]), st1["n"], st1["var"], st1["T"], Ph=st1["Ph"])
    ax.plot(radius_of(st1["V"]) * 1e6, d1["f"], "^", color=ORANGE, ms=10, mec="white", mew=0.8,
            label="the same, nucleation x1")
    print(f"{'SCIANTIX NFIR, end of base, nucleation x1':52s}{radius_of(st1['V']) * 1e6:6.3f}{st1['V'] / st1['n'] * 1e30:6.0f}"
          f"{st1['T']:5.0f}{st1['n'] * xe_volume(st1['T']) / st1['V']:7.3f}   classes: f = {d1['f']:.3f}")
    ax.plot([], [], "o-", color=ink, ms=7, label="measured, hard spheres: bar $P_h$ = 70 to 0 MPa, dot 30 MPa")
    ax.plot([], [], "o--", color=ink, mfc="white", ms=7, label="the same with the Soave pressure of Dowek 2021")
    ax.set(xlabel="pore radius (um)", ylabel="f", xscale="log", xlim=(0.2, 1.4), ylim=(-0.5, 7.5),
           title="HBS pores at their irradiation temperature: f = (p - P_h - 2γ/R) R / Gb, G(T) and b of NEA")
    ax.set_xticks((0.2, 0.3, 0.5, 0.7, 1.0), labels=("0.2", "0.3", "0.5", "0.7", "1"))
    ax.tick_params(axis="x", which="minor", labelbottom=False)
    ax.grid(True, color=GRID)
    ax.legend(frameon=False, fontsize=7, loc="upper left")
    ax.title.set_fontsize(9)
    savefig(fig, "classes_pressure_measured.png")

def normalised(a_, cv_n_, eta_bar, T_=1000.0, R_V_=0.35e-6):
    """Classes of the gas closure in relative variables for given (a, CV_n, eta_bar): x = R / R_V, p / p_bar,
    n / n_bar, w. R_V_ and T_ only set the scale: n_bar is chosen so that the mean packing fraction is eta_bar."""
    V_ = sphere_volume(R_V_)
    n_ = eta_bar * V_ / xe_volume(T_)
    dd = build_gas(R_V_, n_, (cv_n_ * n_) ** 2, T_, a_)
    return {"x": dd["R"] / R_V_, "w": dd["w"], "p_rel": dd["p"] / float(pore_pressure(n_, V_, T_)), "n_rel": dd["n"] / n_,
            "sigma": dd["sigma"], "x_median": dd["R_median"] / R_V_}


# %% ------------------------------------------------------------------------------------------------------------------
# Inputs for MMM
#
# MMM solves a linear elastic problem on a periodic RVE of spherical pores under internal pressure and
# returns the maximum first principal stress at every pore.
# An input is, for every size class of the pores, its radius, its porosity and its load, the net pressure on the wall
# p - 2 gamma / R - P_h. With the pressure closure, at every state (in-pile and in a transient), the load of a class
# is exactly
#     L_k = f c_dp / R_k,
# the same shape R_V / R_k for every f: the load of the pk runs. An RVE is scale-free in length and linear in the load,
# so radius, porosity and load of the classes, each relative to its mean, depend on two factors:
#   xi       porosity: the distance between the pores
#   s        width of the radius distribution (sigma of ln R): R_k / R_V and the shares of the porosity xi_k / xi
# The third factor of the design, eta_bar (mean packing fraction of the gas), only sets f, the scale of the load: it no
# longer changes s_hat. It is kept so that the 20 cubes of the hypercube (xi, s) stay the ones already meshed.
# The unit runs (the same pressure 1 in every class) are not the load of any state of the pressure closure.
# Confinement: the runs have P_h = 0.
# Reference: the NFIR disc before the anneal. The run NFIR is the log-normal fit of its measured pores with the measured gas density; the runs of the hypercube take its R_V and the T of MMM_STATE, the end of the base irradiation, which only sets p_bar.
# The side of the cube, the repulsion distance and the mesh size of Merope/merope_pore_pressure.py are in um.
# The 81 nodes are grouped in classes (bins of x centred on 0, +/-1, +/-2, ...). 
# The run files written here are read by Merope/merope_pore_pressure.py.

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

def packing_max(reduced, T):
    """Largest packing fraction of the classes of a run."""
    return max(k["n"] * xe_volume(T) / k["V"] for k in reduced)

def mmm_case(tag, origin, xi, s, eta, Ph_rel, R_V, T):
    """A run from its three factors (porosity xi, width s, mean packing fraction eta), from the confinement
    Ph_rel = P_h / p_bar and from the scale R_V (m), T (K): the parameters, CV_n, p_bar and P_h (Pa), and the 81 classes
    of the pressure closure in-pile at T, P_h with the width s (f from the mean gas)."""
    V = sphere_volume(R_V)
    n = eta * V / xe_volume(T)
    p_bar = float(pore_pressure(n, V, T))
    d = build(R_V, n, None, T, Ph=Ph_rel * p_bar, s=s)
    cv = np.sqrt(np.sum(d["w"] * (d["n"] - n) ** 2)) / n
    return {"id": tag, "origin": origin, "xi": xi, "eta": eta, "R_V": R_V, "T": T, "f": d["f"], "cv": cv, "p_bar": p_bar,
            "Ph": Ph_rel * p_bar, "classes": d}

def mmm_design(n_runs=MMM_RUNS):
    """Latin hypercube of n_runs runs in the box of MMM_FACTORS. 
    The levels of the factors are paired by permutations that lower the centred discrepancy of
    the points (scipy.stats.qmc, optimization 'random-cd')."""
    lo, hi = np.array(list(MMM_FACTORS.values())).T
    u = qmc.LatinHypercube(d=len(MMM_FACTORS), scramble=False, optimization="random-cd", rng=MMM_SEED).random(n_runs)
    assert all(np.allclose(np.sort(u[:, j]), (np.arange(n_runs) + 0.5) / n_runs) for j in range(u.shape[1]))
    R_V, T = nfir_disc()["R_V"], STATES[MMM_STATE]["T"]
    digits = len(str(n_runs - 1))
    return [mmm_case(f"R{run:0{digits}d}", f"Latin hypercube, run {run + 1} of {n_runs}", R_V=R_V, T=T, Ph_rel=MMM_PH_REL,
                     **dict(zip(MMM_FACTORS, x))) for run, x in enumerate(qmc.scale(u, lo, hi))]

def mmm_appended(cases, n_new=MMM_APPENDED):
    """Runs appended to the Latin hypercube `cases`, in the box of MMM_FACTORS with xi up to MMM_XI_MAX.
    With N = len(cases) + n_new levels per factor, every factor of the box is cut in N strata; each stratum holds at
    most one run of the hypercube, and the new runs take the centres of the empty ones (in xi, those above the end of
    MMM_FACTORS), so that old and new runs together are a Latin hypercube of N levels. The pairing of the levels of the
    new runs maximises their smallest distance in (xi, s), the factors of the geometry, to every other run (random
    search, seed MMM_SEED)."""
    if n_new == 0:
        return []
    box = dict(MMM_FACTORS, xi=(MMM_FACTORS["xi"][0], MMM_XI_MAX))
    lo, hi = np.array(list(box.values())).T
    N = len(cases) + n_new
    u_old = (np.array([[c["xi"], c["classes"]["sigma"], c["eta"]] for c in cases]) - lo) / (hi - lo)
    empty = []
    for j, name in enumerate(box):
        strata = np.floor(u_old[:, j] * N).astype(int)
        if len(set(strata)) != len(cases) or strata.min() < 0 or strata.max() >= N:
            raise SystemExit(f"{name}: two runs of the hypercube in one of {N} strata (or outside the box): the union is "
                             f"no Latin hypercube of {N} levels; change MMM_APPENDED or MMM_XI_MAX")
        empty.append(np.setdiff1d(np.arange(N), strata))
    rng = np.random.default_rng(MMM_SEED)
    best, best_gap = None, -1.0
    for _ in range(20000):
        u_new = np.column_stack([(rng.permutation(e) + 0.5) / N for e in empty])
        points = np.vstack([u_old, u_new])[:, :2]  # xi, s
        gap = np.linalg.norm(points[len(cases):, None] - points[None], axis=-1)
        gap[np.arange(n_new), len(cases) + np.arange(n_new)] = np.inf
        if gap.min() > best_gap:
            best, best_gap = u_new, gap.min()
    u_all = np.vstack([u_old, best])
    assert all(np.array_equal(np.sort(np.floor(u_all[:, j] * N)), np.arange(N)) for j in range(len(box)))
    R_V, T = cases[0]["R_V"], cases[0]["T"]
    digits = len(str(N - 1))
    return [mmm_case(f"R{len(cases) + k:0{digits}d}",
                     f"appended run {k + 1} of {n_new}: with the {len(cases)} of the hypercube a Latin hypercube of {N} "
                     f"levels, xi up to {MMM_XI_MAX:g}", R_V=R_V, T=T, Ph_rel=MMM_PH_REL, **dict(zip(box, x)))
            for k, x in enumerate(lo + best * (hi - lo))]

def mmm_reference():
    """The reference run NFIR: the NFIR disc before the anneal."""
    disc, T = nfir_disc(), STATES[MMM_STATE]["T"]
    return mmm_case("NFIR", "NFIR disc before the anneal, log-normal fit of the measured pores", disc["xi"], disc["s"], xe_volume(T) / NFIR_GAS_VOLUME, MMM_PH_REL, disc["R_V"], T)

def mmm_run_files(cases, n_classes=MMM_CLASSES):
    """One text file per run in MMM_RUN_DIR, with one row per pore class: radius (um), porosity of the class,
    load (MPa: the in-pile net pressure of the pressure closure, f c_dp / R; the gas pressures in a comment line),
    confining pressure (MPa), the input of Merope/merope_pore_pressure.py; and index.txt with the factors of every run."""
    folder = os.path.join(HERE, MMM_RUN_DIR)
    os.makedirs(folder, exist_ok=True)
    for old in os.listdir(folder):  # the runs of a previous design
        if old == "index.txt" or (old.startswith("run_") and old.endswith(".txt")):
            os.remove(os.path.join(folder, old))
    ranges = ", ".join(f"{name} {lo:g}-{hi:g}" for name, (lo, hi) in zip(("xi", "s", "eta_bar"), MMM_FACTORS.values()))
    n_new = sum(c["origin"].startswith("appended") for c in cases)
    appended = (f" + {n_new} runs appended ({cases[-n_new]['id']}-{cases[-1]['id']}), xi up to {MMM_XI_MAX:g}: together a "
                f"Latin hypercube of {len(cases)} levels" if n_new else "")
    index = [f"Latin hypercube of {len(cases) - n_new} runs (seed {MMM_SEED}) in {ranges}{appended};",
             f"{n_classes} pore classes each, pressure closure; reference: NFIR disc before the anneal (sets R_V and T)",
             f"load of a class: f c_dp / R (in-pile net pressure p - 2 gamma / R - P_h of the pressure closure; gamma = {GAMMA:g} N/m, "
             f"c_dp = G b = {c_dp(cases[0]['T']):.2f} N/m at {cases[0]['T']:g} K): the shape R_V / R in every run, eta_bar only sets f",
             f"# {'run':6s}{'xi':>8s}{'s':>8s}{'CV_n':>8s}{'eta_bar':>9s}{'eta_max':>9s}{'f':>8s}{'Ph/p_bar':>10s}{'R_V_um':>9s}{'T_K':>9s}{'p_bar_MPa':>11s}{'Ph_MPa':>9s}  file"]
    for c in [mmm_reference()] + cases:
        d, xi, p_bar, Ph = c["classes"], c["xi"], c["p_bar"], c["Ph"]
        file = f"run_{c['id']}.txt"
        reduced = reduced_classes(d, c["T"], n_classes)
        R_k, p_k = np.array([k["R"] for k in reduced]), np.array([k["p"] for k in reduced])
        load = c["f"] * c_dp(c["T"]) / R_k
        if c["f"] <= 0.0:
            raise SystemExit(f"run {c['id']}: f = {c['f']:.3f}, the gas is below the equilibrium: no positive load")
        lines = [f"# MMM input, run {c['id']} ({c['origin']})",
                 f"# xi = {xi:g}, s = {d['sigma']:.4g} (CV_n = {c['cv']:.4g}), eta_bar = {c['eta']:.4g}, f = {c['f']:.4g}, P_h / p_bar = {Ph / p_bar:.4g}",
                 f"# scale: R_V = {c['R_V'] * 1e6:.4g} um, T = {c['T']:g} K, p_bar = {p_bar / 1e6:.5g} MPa",
                 f"# pressure_MPa = load of MMM, the in-pile net pressure of the pressure closure f c_dp / R (c_dp = G b = {c_dp(c['T']):.2f} N/m)",
                 "# gas pressure p of the classes (MPa): " + " ".join(f"{p / 1e6:.4f}" for p in p_k),
                 "# one row per pore class; the porosities sum to xi",
                 f"# {'radius_um':>10s}{'porosity':>14s}{'pressure_MPa':>14s}{'confinement_MPa':>17s}"]
        lines += [f"{k['R'] * 1e6:12.6f}{xi * k['xi_share']:14.6e}{q / 1e6:14.4f}{Ph / 1e6:17.4f}" for k, q in zip(reduced, load)]
        with open(os.path.join(folder, file), "w") as fh:
            fh.write("\n".join(lines) + "\n")
        index.append(f"  {c['id']:6s}{xi:8.4f}{d['sigma']:8.4f}{c['cv']:8.4f}{c['eta']:9.4f}{packing_max(reduced, c['T']):9.4f}{c['f']:8.3f}{Ph / p_bar:10.3f}"
                     f"{c['R_V'] * 1e6:9.4f}{c['T']:9.1f}{p_bar / 1e6:11.2f}{Ph / 1e6:9.2f}  {file}")
    with open(os.path.join(folder, "index.txt"), "w") as fh:
        fh.write("\n".join(index) + "\n")
    print(f"wrote {len(cases) + 1} run files in {MMM_RUN_DIR}/ for Merope/merope_pore_pressure.py "
          f"(the table of the runs is {MMM_RUN_DIR}/index.txt)")

    disc = nfir_disc()
    R_V, T = cases[0]["R_V"], cases[0]["T"]

    blue, orange = LinearSegmentedColormap.from_list("blue", SEQ_BLUE), LinearSegmentedColormap.from_list("orange", SEQ_ORANGE)
    by_s, by_eta = Normalize(*MMM_FACTORS["s"]), Normalize(*MMM_FACTORS["eta"])
    fig, ax = plt.subplots(1, 3, figsize=(14.5, 4.3))
    D = np.linspace(0.0, 6.0e-6, 301)[1:]  # (m) pore diameter
    cumulative = lambda xi, R_V_, s: 100.0 * np.cumsum(porosity_per_bin(xi, R_V_, s, np.append(0.0, D)))
    for c in cases:
        s, reduced = c["classes"]["sigma"], reduced_classes(c["classes"], T, n_classes)
        R_k = np.array([k["R"] for k in reduced])
        load = c["f"] * c_dp(T) / R_k / 1e6
        ax[0].plot(D * 1e6, cumulative(1.0, R_V, s), color=blue(by_s(s)), lw=1.2)
        ax[1].plot(R_k * 1e6, [c["xi"] * k["xi_share"] for k in reduced], "o-", color=blue(by_s(s)), lw=1.0, ms=4.5, mec="white", mew=0.5)
        ax[2].plot(R_k * 1e6, load, "-", color=orange(by_eta(c["eta"])), lw=1.0)
        ax[2].plot(R_k * 1e6, load, "o", color=orange(by_eta(c["eta"])), ms=4.5, mec="white", mew=0.5)
    R = np.geomspace(0.15e-6, 2.5e-6, 50)
    ax[2].plot(R * 1e6, c_dp(T) / R / 1e6, "k--", lw=1, label=rf"$Gb/R$, {c_dp(T):.1f} N/m: f = 1 (NEA)")
    ax[0].plot(disc["edges"][1:] * 1e6, 100.0 * np.cumsum(disc["bars"]) / disc["bars"].sum(), "o", color="k", ms=4, mec="white", mew=0.5, zorder=4,
               label="NFIR disc, as irradiated")
    ax[0].plot(D * 1e6, cumulative(disc["xi"] / disc["bars"].sum(), disc["R_V"], disc["s"]), color=BROWN, lw=1.5, ls="--", zorder=3,
               label=f"Log-normal fit of the NFIR disc")
    ax[0].set(xlabel="Pore diameter (um)", ylabel="Cumulative share of the porosity (%)", xlim=(0.0, 6.0),
              title=f"Radius distributions, $R_V$ = {R_V * 1e6:.2f} um")
    ax[1].set(xlabel="radius of the class (um)", ylabel="Porosity of the class", xscale="log", yscale="log",
              title=f"{n_classes} pore classes of every run")
    ax[2].set(xlabel="radius of the class (um)", ylabel="load of MMM (MPa)", xscale="log", yscale="log",
              title=r"Load: $f\,c_{dp}/R$ (pressure closure, in-pile, $P_h$ = 0)")
    ax[2].legend(frameon=False, loc="lower left", fontsize=9)
    fig.colorbar(ScalarMappable(by_s, blue), ax=ax[1], label="s")
    fig.colorbar(ScalarMappable(by_eta, orange), ax=ax[2], label=r"$\bar \eta$")
    for ax_ in ax:
        ax_.grid(True, color=GRID)
        ax_.title.set_fontsize(10)
    for ax_ in ax[1:]:  # decimal labels on the logarithmic axis of the radii
        ax_.set_xticks((0.2, 0.5, 1.0, 2.0), labels=("0.2", "0.5", "1", "2"))
        ax_.tick_params(axis="x", which="minor", labelbottom=False)
    ax[0].legend(frameon=False, loc="lower right")
    savefig(fig, "mmm_runs.png")

def gas_volume_figure(cases, n_classes=MMM_CLASSES):
    """Volume per gas atom of the pores, V / n = v_Xe(T) / eta, against the measured average volume per fission-gas atom in the HBS bubbles."""
    T, A3 = cases[0]["T"], 1.0e30  # A^3 per m3

    # Noirot 2008 data
    data = gas_volume_data() # (A^3/at)

    # the rim rows of Dowek review in Table 1, Noirot 2008 excluded
    rim = [r for r in literature_pores() if r["where"] == "1R" and r["ref"] != "Noirot et al., 2008" and np.isfinite(r["v"])] # (m3/at)

    # Dowek, samples A and B at the average burnup of the sample
    dowek = dowek_gas_volumes() # (m3/at)

    # concatenate all datasets: averages in the HBS, the single bubbles of Dowek excluded
    measured = np.concatenate([v for _, v in data.values()] + [[r["v"] * A3 for r in rim],
                              [d["v"] * A3 for d in dowek if d["position"] == "1R" and d["kind"] == "mean"]])

    # LHS ranges
    eta_lo, eta_hi = MMM_FACTORS["eta"]
    band = (xe_volume(T) / eta_hi * A3, xe_volume(T) / eta_lo * A3)

    # EXP , SCIANTIX with and without the nucleation factor
    st, st1 = STATES[MMM_STATE], STATE_NUCLEATION_X1
    columns = [("Exp.\nNFIR", GREEN, "D", mmm_reference()["classes"]),
               ("SCIANTIX\nnucl.\nx0.12", ORANGE, "s", build(radius_of(st["V"]), st["n"], st["var"], st["T"], Ph=st["Ph"])),
               ("SCIANTIX\nnucl.\nx1", ORANGE, "^", build(radius_of(st1["V"]), st1["n"], st1["var"], st1["T"], Ph=st1["Ph"]))]
    columns += [(c["id"][1:], BLUE, "o", c["classes"]) for c in cases]

    fig, ax = plt.subplots(1, 2, figsize=(15.0, 7.0), sharey=True, gridspec_kw={"width_ratios": (1.4, 2.0)})
    ax[0].axhspan(*band, color=BLUE, alpha=0.12, lw=0, label=rf"MMM input, $\bar\eta$ = {eta_lo:g} to {eta_hi:g}")
    ax[1].axhspan(*band, color=BLUE, alpha=0.12, lw=0)

    for (label, (bu, v)), marker in zip(data.items(), ("o", "^", "-")):
        if marker == "-": 
            # NFIR
            ax[0].plot(bu, v, "-", color=GREEN, lw=5, solid_capstyle="butt", label=label)
        else:
            ax[0].plot(bu, v, marker, color="#52514e", mfc="none", ms=7, mew=1.2, label=label)
    ax[0].plot([r["bu"] for r in rim], [r["v"] * A3 for r in rim], "s", color="#52514e", mfc="none", ms=7, mew=1.2,
               label="rim, " + ", ".join(r["ref"].replace(" et al.,", "") for r in rim))
    for sample, marker in (("A", "D"), ("B", "v")):
        for at_rim in (True, False):
            pts = [d for d in dowek if d["sample"] == sample and (d["position"] == "1R") == at_rim]
            if pts:
                ax[0].plot([d["bu"] for d in pts], [d["v"] * A3 for d in pts], marker, color=BROWN, mfc=BROWN if at_rim else "none",
                           ms=7, mew=1.2, label=f"Dowek et al. 2021, sample {sample}, {'rim' if at_rim else 'not rim'} (average burnup)")
    ax[0].plot([NFIR_BURNUP] * len(STATES), [st["V"] / st["n"] * A3 for st in STATES.values()], "s", color=ORANGE, ms=7,
               mec="white", mew=1.0, zorder=4, label="SCIANTIX, NFIR case, nucleation x0.12")
    ax[0].plot(NFIR_BURNUP, st1["V"] / st1["n"] * A3, "^", color=ORANGE, ms=8, mec="white", mew=1.0, zorder=4,
               label="SCIANTIX, NFIR case, nucleation x1 (end of base)")
    ax[1].axhspan(measured.min(), measured.max(), color=GREY, alpha=0.2, lw=0, label="measured, all series")
    xs = np.r_[0.0, 2.6, 5.2, 7.4 + np.arange(len(cases))]
    for x, (name, color, marker, d) in zip(xs, columns):
        reduced, m = reduced_classes(d, T, n_classes), moments(d)
        v = np.array([k["V"] / k["n"] for k in reduced]) * A3
        gas = np.array([k["w"] * k["n"] for k in reduced]) / m["n"]
        ax[1].scatter(np.full(n_classes, x), v, s=8.0 + 160.0 * gas, marker=marker, color=color, edgecolors="white", linewidths=0.5,
                      zorder=3)
        ax[1].plot(x, m["V"] / m["n"] * A3, "_", color="k", ms=14, mew=1.6, zorder=4)
    ax[1].scatter([], [], s=60, color=GREY, edgecolors="white", label="class, area ~ share of the gas")
    ax[1].plot([], [], "_", color="k", ms=14, mew=1.6, label="mean V / n")
    ax[1].axvline(0.5 * (xs[2] + xs[3]), color=GREY, lw=0.8)
    ax[1].set_xticks(xs, labels=[c[0] for c in columns])
    ax[1].set_xlabel("MMM run")
    ax[0].set(xlabel="local burnup (GWd/tHM)", ylabel=r"volume per gas atom  ($\AA^3$/atom)", yscale="log", ylim=(20.0, 1000.0))
    ax[0].set_yticks((20, 50, 100, 200, 500, 1000), labels=("20", "50", "100", "200", "500", "1000"))
    ax[0].tick_params(axis="y", which="minor", labelleft=False)
    for ax_ in ax:
        ax_.grid(True, color=GRID)
    ax[0].legend(frameon=False, loc="upper left")
    ax[1].legend(frameon=False,ncol=3, loc="lower left")
    fig.suptitle(f"Volume per gas atom: measured and imposed for MMM calculations with {n_classes} classes and at T = {T:.0f} K")
    savefig(fig, "mmm_gas_volume.png")

def pore_radius_figure(cases, n_classes=MMM_CLASSES):
    """Pore radii of the classes against the pore sizes measured on the NFIR disc."""
    T, um = cases[0]["T"], 1.0e6
    disc, after = nfir_disc(), nfir_disc("PoreRadiusPOSTANNEALING.txt")
    st, st1 = STATES[MMM_STATE], STATE_NUCLEATION_X1  # SCIANTIX with and without the nucleation factor
    d_state = build(radius_of(st["V"]), st["n"], st["var"], st["T"], Ph=st["Ph"])
    d_state1 = build(radius_of(st1["V"]), st1["n"], st1["var"], st1["T"], Ph=st1["Ph"])
    edges = 0.5 * disc["edges"] * um  # (um) radius edges of the bins of 0.2 um of diameter
    cum = np.concatenate(([0.0], np.cumsum(disc["bars"]))) / disc["bars"].sum()
    measured = np.interp((0.05, 0.95), cum, edges)
    columns = [("Exp.\nNFIR", GREEN, "D", mmm_reference()["classes"]),
               ("SCIANTIX\nnucl.\nx0.12", ORANGE, "s", d_state),
               ("SCIANTIX\nnucl.\nx1", ORANGE, "^", d_state1)]
    columns += [(c["id"][1:], BLUE, "o", c["classes"]) for c in cases]
    reduced = [reduced_classes(d, T, n_classes) for *_, d in columns]
    R_runs = np.array([[k["R"] for k in red] for red in reduced[3:]]) * um
    band = (R_runs.min(), R_runs.max())

    fig, ax = plt.subplots(1, 2, figsize=(16.0, 7.0), sharey=True, gridspec_kw={"width_ratios": (1.15, 1.35)})
    ax[0].axhspan(*band, color=BLUE, alpha=0.12, lw=0, label=f"MMM input, {band[0]:.2f} to {band[1]:.2f} um")
    ax[1].axhspan(*band, color=BLUE, alpha=0.12, lw=0)
    ax[1].axhspan(*measured, color=GREY, alpha=0.2, lw=0, label="NFIR disc, 90 % of the measured porosity")
    ax[0].barh(edges[:-1], 100.0 * disc["bars"], height=np.diff(edges), align="edge", color=GREEN, alpha=0.45, edgecolor="white",
               lw=0.5, label="NFIR UO2 disc, as irradiated (Noirot et al., 2014)")
    ax[0].stairs(100.0 * after["bars"], edges, orientation="horizontal", color=GREEN, lw=1.4, label="NFIR UO2 disc, after the anneal (Noirot et al., 2014)")
    r = np.linspace(0.0, edges[-1], 401)  # (um)
    per_bin = lambda xi, R_V, s: 100.0 * porosity_per_bin(xi, R_V, s, 2.0e-6 * r) * (edges[1] - edges[0]) / np.diff(r)  # % per bar
    ax[0].plot(per_bin(disc["xi"], disc["R_V"], disc["s"]), 0.5 * (r[1:] + r[:-1]), color=BROWN, lw=1.5, ls="--",
               label=f"Log-normal fit: $R_V$ = {disc['R_V'] * um:.2f} um, s = {disc['s']:.2f}")
    ax[0].plot(per_bin(st["xi"], radius_of(st["V"]), d_state["sigma"]), 0.5 * (r[1:] + r[:-1]), color=ORANGE, lw=1.8,
               label=f"SCIANTIX, NFIR end of base irradiation, nucleation x0.12: $R_V$ = {radius_of(st['V']) * um:.2f} um, "
                     f"s = {d_state['sigma']:.2f}")
    ax[0].plot(per_bin(st1["xi"], radius_of(st1["V"]), d_state1["sigma"]), 0.5 * (r[1:] + r[:-1]), color=ORANGE, lw=1.8,
               ls=":", label=f"SCIANTIX, nucleation x1: $R_V$ = {radius_of(st1['V']) * um:.2f} um, s = {d_state1['sigma']:.2f}")
    xs = np.r_[0.0, 2.6, 5.2, 7.4 + np.arange(len(cases))]
    for x, (name, color, marker, d), red in zip(xs, columns, reduced):
        R_k = np.array([k["R"] for k in red]) * um
        share = np.array([k["xi_share"] for k in red])
        ax[1].scatter(np.full(n_classes, x), R_k, s=8.0 + 160.0 * share, marker=marker, color=color, edgecolors="white",
                      linewidths=0.5, zorder=3)
        ax[1].plot(x, moments(d)["R_V"] * um, "_", color="k", ms=14, mew=1.6, zorder=4)
    ax[1].scatter([], [], s=60, color=GREY, edgecolors="white", label="class, area ~ share of the porosity")
    ax[1].plot([], [], "_", color="k", ms=14, mew=1.6, label="$R_V$")
    ax[1].axvline(0.5 * (xs[2] + xs[3]), color=GREY, lw=0.8)
    ax[1].set_xticks(xs, labels=[c[0] for c in columns])
    ax[1].set(xlabel="MMM run", xlim=(-0.7, xs[-1] + 0.7))
    ax[0].set(xlabel="porosity per 0.1 um of radius (%)", ylabel="pore radius (um)", xlim=(0.0, None), ylim=(0.0, edges[-1]))
    for ax_ in ax:
        ax_.grid(True, color=GRID)
    ax[0].legend(frameon=False, loc="upper right")
    ax[1].legend(frameon=False, ncol=3, loc="lower left")
    fig.suptitle(f"Pore radius: measured on the NFIR disc and imposed for MMM calculations of the {n_classes} classes")
    savefig(fig, "mmm_pore_radius.png")

# %% ------------------------------------------------------------------------------------------------------------------
# The NFIR case of option 4: its output.txt

def read_output(path):
    """Columns of output.txt used here, by name; 'beta' is the (rows, 81) array of the broken fractions.
    Also used by regression/hbs_nfir/plot.py."""
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

def state_of_row(o, k):
    """The state of row k of output.txt in the format of STATES: a row holds the retained moments and S, S_2 after the
    venting of the step, so the unvented moments are n / S and (Var + n^2) / S_2 - (n / S)^2 of the same row."""
    S, S2 = o["HBS gas retention fraction"][k], o["HBS gas retention fraction - second moment"][k]
    n = o["Xe atoms per HBS pore"][k] / S
    var = max((o["Xe atoms per HBS pore - variance"][k] + o["Xe atoms per HBS pore"][k] ** 2) / S2 - n**2, 0.0)
    return {"T": o["Temperature"][k], "Ph": -o["Hydrostatic stress"][k] * 1.0e6, "xi": o["HBS porosity"][k],
            "Np": o["HBS pore density"][k], "V": o["HBS pore volume"][k], "n": n, "var": var}

def classes_of_row(o, k):
    """Classes of option 4 at row k of output.txt: the unvented moments of the row with the pressure closure at the
    T, P_h of the row."""
    st = state_of_row(o, k)
    return build(radius_of(st["V"]), st["n"], st["var"], st["T"], st["Ph"])

def states_agree(o, tol=1.0e-3):
    """STATES against the output of the NFIR case: the last row at full power and the row of the peak temperature of
    the transient. False if a value of STATES is off by more than tol (relative; STATES holds 4 digits): then it
    prints the two states as read from the output, to be copied into STATES."""
    at_power = np.nonzero(o["Fission rate"] >= o["Fission rate"].max() * (1.0 - 1e-6))[0]
    k0 = transient_start(o)
    rows = {"NFIR, end of base irradiation": int(at_power[-1]), "NFIR, anneal at 1200 C": k0 + int(np.argmax(o["Temperature"][k0:]))}
    worst, lines = 0.0, []
    for name, k in rows.items():
        st = state_of_row(o, k)
        lines.append(f'"{name}": ' + ", ".join(f'"{key}": {v:.6g}' if key == "T" else f'"{key}": {v:.4g}' for key, v in st.items()))
        for key, v in st.items():
            ref = STATES[name][key]
            worst = max(worst, abs(v - ref) / max(abs(ref), 1.0e6 if key == "Ph" else 0.0))  # P_h = 0 in the anneal: against 1 MPa
    print(f"STATES against output.txt: largest relative difference {worst:.1e}")
    if worst > tol:
        print(f"STATES is stale (more than {tol:.0e}), the states of the output:\n" + "\n".join(lines))
    return worst <= tol

def transient_start(o):
    """Row at which the out-of-pile transient starts: the first one without fissions after the base irradiation."""
    return int(np.nonzero(o["Fission rate"] > 0.0)[0][-1]) + 1

def rupture_figure(o, fractions=(0.25, 0.5, 1.0)):
    """Distribution and rupture of the classes of the NFIR case, at the rows where D reaches the given fractions of
    its final value (or at the last row with S >= 0.1, if the gas is all vented before): number distribution with
    its broken part, pressure against critical pressure (2 gamma / R in grey), gas distribution with its vented part,
    and D, G against the temperature."""
    D, G, T = o["HBS fragmented fraction"], o["HBS burst release fraction"], o["Temperature"]
    rows = [int(np.argmax(D >= f * D[-1] * (1.0 - 1e-6))) for f in fractions]
    # the unvented moments are n / S and (Var + n^2) / S_2: they cannot be rebuilt once the retained gas is negligible
    S_min = 0.1
    last_rebuildable = int(np.max(np.nonzero(o["HBS gas retention fraction"] >= S_min)))
    rows = sorted(set(min(k, last_rebuildable) for k in rows))
    first = int(np.argmax(D > 0.0))
    fig, ax = plt.subplots(2, 2, figsize=(11, 7.4))
    for k, color in zip(rows, ("#86b6ef", BLUE, "#104281")):
        st = state_of_row(o, k)
        d = classes_of_row(o, k)
        beta, R_um = o["beta"][k], d["R"] * 1e6
        pdf = d["w"] / np.gradient(R_um)  # per um of radius
        label = f"T = {T[k]:.0f} K, D = {D[k]:.3f}, G = {G[k]:.3f}"
        ax[0, 0].plot(R_um, pdf, color=color, lw=1.0)
        ax[0, 0].fill_between(R_um, pdf * beta, color=color, alpha=0.45, lw=0, label=label)
        ax[0, 1].plot(R_um, d["p"] / 1e6, color=color, label=f"$p_i$, {T[k]:.0f} K")
        ax[0, 1].plot(R_um, critical_pressure(d["R"], st["xi"], st["Ph"]) / 1e6, color=color, ls="--")
        gas = pdf * d["n"] / st["n"]
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
    ax[0, 0].set(xlabel="R (um)", ylabel="pdf in R (1/um)", title="pores: all (line) and broken (filled)")
    ax[0, 1].set(xlabel="R (um)", ylabel="p (MPa)", yscale="log", title="pressure and critical pressure per class")
    ax[1, 0].set(xlabel="R (um)", ylabel="gas share per um", title="gas: all (line) and vented (filled)")
    ax[1, 1].set(xlabel="T (K)", ylabel="fraction", title="broken fractions on the heat-up")
    for ax_ in ax.ravel():
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False)
    savefig(fig, "classes_rupture_nfir_j19damage.png")

def fgr_figure(o):
    """Fission gas release of the transient of the NFIR case, the release of the base irradiation subtracted, in per
    cent of the gas produced, against the time from the start of the transient, with the Kr-85 release measured in the
    anneal (Noirot 2014, Fig. 9, in regression/hbs_nfir/data)."""
    k0 = transient_start(o)
    minutes = (o["Time"][k0:] - o["Time"][k0]) * 60.0
    release = 100.0 * (o["Fission gas release"][k0:] - o["Fission gas release"][k0])
    cum = np.loadtxt(os.path.join(NFIR_CASE, "..", "data", "CumulativeRelease.txt"), delimiter=";", comments="#", skiprows=2,
                     encoding="utf-8")
    fig, ax = plt.subplots(figsize=(6.4, 4.4))
    ax.plot(minutes, release, color=ORANGE, label="SCIANTIX")
    ax.plot(cum[:, 0], cum[:, 1], "o", ms=3, mfc="none", color="k", label="NFIR, Noirot et al., 2014 (Kr-85)")
    ax.set(xlabel="time from the start of the transient (min)", ylabel="release (% of the gas produced)")
    ax.grid(True, color=GRID)
    ax.legend(frameon=False)
    savefig(fig, "classes_fgr_nfir_j19damage.png")
    print(f"release at the end of the transient: {release[-1]:.2f} % of the gas produced; D = {o['HBS fragmented fraction'][-1]:.4f}, "
          f"G = {o['HBS burst release fraction'][-1]:.4f}, 1 - S = {1.0 - o['HBS gas retention fraction'][-1]:.4f}")


# %% ------------------------------------------------------------------------------------------------------------------
# Main
def main():
    assert MMM_CLASSES % 2 == 1 and 3 <= MMM_CLASSES <= 9, "MMM_CLASSES is odd, from 3 to 9"
    assert MMM_RUNS >= 2, "MMM_RUNS is at least 2"
    plt.switch_backend("Agg")  # the figures go to files
    st, R_V, d, m = section_inputs()
    section_nfir_data()
    section_distributions(st, d, m)  # plot
    section_mean_pressure(st, R_V, d) # consistency check
    section_closure(st, R_V) # plot
    section_measured(st) # plot
    print("\n== inputs for MMM ==")
    cases = mmm_design(MMM_RUNS)
    cases += mmm_appended(cases, MMM_APPENDED)
    mmm_run_files(cases, MMM_CLASSES)
    gas_volume_figure(cases, MMM_CLASSES)
    pore_radius_figure(cases, MMM_CLASSES)
    output = os.path.join(NFIR_CASE, "output.txt")
    if not os.path.exists(output):
        print(f"\nno {output}: run the NFIR case for its rupture and release figures")
        print_figures()
        return 0

    o = read_output(output)
    status = 0 if states_agree(o) else 1
    rupture_figure(o)
    fgr_figure(o)
    print_figures()
    return status

if __name__ == "__main__":
    sys.exit(main())


# Unused, used before as checks
def section_k15_check():
    """Self-check of build_gas() on a case with a known solution:
    Var(n) of the K15 reference width with the closure n_i ~ R_i^a must give back s = 0.356.."""
    R_V, n = 0.35e-6, 1.4531e9
    R, _, w = classes(R_V, K15_SIGMA)
    n_i = n * R**A_CLOSURE / np.sum(w * R**A_CLOSURE)
    var = np.sum(w * (n_i - n) ** 2)
    s = build_gas(R_V, n, var, 800.0, A_CLOSURE)["sigma"]
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

def section_closure_data():
    """The closure against measured pores (D21): volume per gas atom against the radius. Single bubbles (Tables 4, 5)
    and mean values with a mean bubble diameter (Table 1, radius D / 2), against the classes of the NFIR disc (fitted
    R_V and s, NFIR_GAS_VOLUME) for a = 2 and a = 3 and the classes of the SCIANTIX state at the end of the base
    irradiation, within +/- 3 s of the median. The closure gives V_i / n_i ~ R_i^(3 - a): slope 1 for a = 2, flat for
    a = 3. Two bubbles at one position give a local slope, hence a local a; the line joins them."""
    disc, st, T = nfir_disc(), STATES["NFIR, end of base irradiation"], STATES["NFIR, anneal at 1200 C"]["T"]
    central = np.abs(X) <= 3.0
    fig, ax = plt.subplots(figsize=(6.6, 4.6))
    for a_, ls in ((2.0, "-"), (3.0, "--")):
        n_ = sphere_volume(disc["R_V"]) / NFIR_GAS_VOLUME
        d = build_gas(disc["R_V"], n_, cv2_on_nodes(disc["s"], a_) * n_**2, T, a_)
        ax.plot(d["R"][central] * 1e6, (d["V"] / d["n"])[central] * 1e30, ls, color=GREEN, lw=1.5, label=f"classes of the NFIR disc, a = {a_:g}")
    d = build_gas(radius_of(st["V"]), st["n"], st["var"], st["T"], A_CLOSURE)
    ax.plot(d["R"][central] * 1e6, (d["V"] / d["n"])[central] * 1e30, color=ORANGE, lw=1.5, label="classes of SCIANTIX, end of base, a = 2")
    rows = [r for r in literature_pores() if np.isfinite(r["D"]) and np.isfinite(r["v"])]
    for r in rows:
        rim = r["where"] in ("1R", "fuel disc")
        ax.plot(0.5 * r["D"] * 1e6, r["v"] * 1e30, "s", color="#52514e", mfc="#52514e" if rim else "none", ms=7, mew=1.2)
        ax.annotate(f"{r['ref'].replace(' et al.,', '')}, {r['where']}", (0.5 * r["D"] * 1e6, r["v"] * 1e30), xytext=(6, -3),
                    textcoords="offset points", fontsize=6.5, color="#52514e")
    bubbles = single_bubbles()
    for b in bubbles:
        rim = b["position"] == "1R"
        ax.plot(b["R"] * 1e6, b["v"] * 1e30, "o", color="#52514e", mfc="#52514e" if rim else "none", ms=7, mew=1.2)
        ax.annotate(f"{b['sample']} {b['position']}", (b["R"] * 1e6, b["v"] * 1e30), xytext=(6, 2), textcoords="offset points",
                    fontsize=6.5, color="#52514e")
    print("\n== closure against single bubbles (D21): local exponent from two bubbles at one position ==")
    for sample in "AB":
        for pos in sorted({b["position"] for b in bubbles if b["sample"] == sample}):
            pair = [b for b in bubbles if b["sample"] == sample and b["position"] == pos]
            if len(pair) != 2:
                continue
            (R1, v1), (R2, v2) = [(b["R"], b["v"]) for b in pair]
            slope = np.log(v2 / v1) / np.log(R2 / R1)
            ax.plot([R1 * 1e6, R2 * 1e6], [v1 * 1e30, v2 * 1e30], color="#52514e", lw=0.8)
            print(f"sample {sample}, {pos}: R {R1 * 1e6:.3f} and {R2 * 1e6:.3f} um, v {v1 * 1e30:.0f} and {v2 * 1e30:.0f} A^3, "
                  f"p_293 {pair[0]['p_293'] / 1e6:.1f} and {pair[1]['p_293'] / 1e6:.1f} MPa: d ln v / d ln R = {slope:.2f}, a = {3.0 - slope:.2f}")
    ax.plot([], [], "o", color="#52514e", ms=7, label="single bubble (D21 Tables 4, 5)")
    ax.plot([], [], "s", color="#52514e", ms=7, label="mean of a sample (D21 Table 1)")
    ax.plot([], [], "o", color="#52514e", mfc="none", ms=7, mew=1.2, label="open: not at the rim, not HBS")
    ax.set(xlabel="pore radius (um)", ylabel=r"volume per gas atom  ($\AA^3$/atom)", xscale="log", yscale="log",
           title="Gas density against the radius: closure and measured pores")
    ax.set_xticks((0.2, 0.5, 1.0, 2.0), labels=("0.2", "0.5", "1", "2"))
    ax.set_yticks((50, 100, 200, 500), labels=("50", "100", "200", "500"))
    ax.tick_params(which="minor", labelbottom=False, labelleft=False)
    ax.grid(True, color=GRID)
    ax.legend(frameon=False, fontsize=7, loc="upper left")
    savefig(fig, "classes_closure_data.png")

def section_moment_checks(st, R_V):
    """Check of the conserved moments: for each a, the number-weighted distributions of V_i and n_i with the target
    mean (solid) and the mean of the classes (dashed) on top of each other; then the relative error of V, n, Var(n)
    against a, CV_n and n. The dashed line is the tolerance of check_moments."""
    T = st["T"]
    fig, ax = plt.subplots(2, 3, figsize=(12, 6.2), sharex="row")
    for j, a_ in enumerate((3.0, 2.5, 2.0)):
        dp = build_gas(R_V, st["n"], st["var"], T, a_)
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
        mp = moments(build_gas(radius_of(V_mean), n_mean, var_n, T, a_))
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
            d = build_gas(R_V, st["n"], st["var"], st["T"], a_)
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

def section_coverage():
    """Coverage of the parameter space.

    parameter | enters                          | through
    CV_n      | the radius distribution (s)     | moments
    eta_bar   | the shape p / p_bar             | Z(eta); carries n, V and T (through v_Xe(T))
    xi        | the geometry of the RVE         | porosity
    """
    cv_ref, eta_ref = 0.7, 0.09
    # grids of the parameter space; the SCIANTIX states of STATES have CV_n = 0.51-0.53 and eta_bar = 0.11
    GRID_CV = (0.1, 0.3, 0.5, 0.7, 0.9, 1.1)
    GRID_ETA = (0.02, 0.04, 0.07, 0.10, 0.15, 0.23)
    cmap = lambda k, n: plt.cm.viridis(0.1 + 0.8 * k / max(n - 1, 1))
    fig, ax = plt.subplots(1, 3, figsize=(13, 3.8))
    for k, cv_ in enumerate(GRID_CV):  # radius distribution against CV_n: depends on s only
        nn = normalised(A_CLOSURE, cv_, eta_ref)
        ax[0].plot(nn["x"], nn["w"] / np.gradient(np.log(nn["x"])), color=cmap(k, len(GRID_CV)), label=f"$CV_n$ = {cv_}, s = {nn['sigma']:.3f}")
        ax[2].plot(nn["x"], nn["p_rel"], color=cmap(k, len(GRID_CV)), label=f"$CV_n$ = {cv_}")
    for k, e_ in enumerate(GRID_ETA):  # pressure shape against eta_bar
        nn = normalised(A_CLOSURE, cv_ref, e_)
        ax[1].plot(nn["x"], nn["p_rel"], color=cmap(k, len(GRID_ETA)), label=r"$\bar \eta$ = "+f"{e_}")
    ax[0].set(xlabel="$R / R_V$", ylabel="pdf in ln R", title=f"radius distribution, a = {A_CLOSURE}")
    ax[1].set(xlabel="$R / R_V$", ylabel=r"p / $\bar p$", title=r"pressure shape against  $\bar \eta$ ($CV_n$ = $CV_{ref}$)", yscale="log")
    ax[2].set(xlabel="$R / R_V$", ylabel=r"p / $\bar p$", title=r"pressure shape against $CV_n$ ($\bar \eta$  = $\eta_{ref}$)",  yscale="log")
    for ax_ in ax:
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False)
    savefig(fig, "classes_coverage.png")
