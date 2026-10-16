"""
HBS pore classes from the SCIANTIX moments, rupture per class, and cases for the micromechanics (MMM).
@ author: E.Cappellari, POLIMI-CEA 2026
@ date:   October 2026

Python transcription of iHighBurnupStructureFragmentation = 4, the model is described in README_HBFFmodel.md.

SCIANTIX carries for the HBS pores 
    -   the mean volume V, 
    -   the mean Xe atoms per pore n
    -   and its variance Var(n). 

This script 

1.  rebuilds classes (R_i, V_i, w_i, n_i, p_i) that reproduce these three moments,
        sum_i w_i V_i = V,      sum_i w_i n_i = n,      sum_i w_i (n_i - n)^2 = Var(n),
    w_i is the weight of each class i, characterised by its own radius R_i and pressure p_i

2.  evaluates the rupture criterion on every class, 

3.  writes the inputs of the runs of MMM (mmm_inputs/run_*.txt): 
        -   radius R, 
        -   porosity xi = N_p sum_i w_i V_i = N_p V, where N_p is the pore density, 
        -   and pressure of every pore class p
        -   and the confining pressure (currently neglected).
    one file per run of the Latin hypercube in the three factors of the RVE 
        -   porosity, 
        -   width of the radii, 
        -   packing fraction
    
4.  replays option 4 on the NFIR case (NFIR_CASE, when its output.txt is available) against the C++ and draws its
    rupture and release figures.

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
C_DP = 55.0  # (N/m) J19 Table 4: steady-state limit of the overpressure of a pore growing by punching, c_dp / R
K15_SIGMA = 0.356  # K15 Table 2: sigma_ln r of the reference pore-size distribution
NFIR_GAS_VOLUME = 1.518e-28  # (m3/atom) volume per gas atom in the pores of the NFIR disc, from the measured Xe
NFIR_BURNUP = 103.5  # (GWd/tHM) burnup of the NFIR disc 

# %% ------------------------------------------------------------------------------------------------------------------
# Model choices
N_CLASSES, SPAN = 81, 4.0  # nodes at ln R = ln R_median +/- 4 s
A_CLOSURE = 2.0  # closure exponent of the gas per class, n_i ~ R_i^a (2: gas density ~ 1/R, 3: uniform pressure)
TAU = 2.0  # (s) J19 Table 3 (interconnection and microcracking modes; J19 rupture is instantaneous)
R_MIN_EVAL, XI_MAX_EVAL = 1.0e-7, 0.35  # validity window of the criterion, on the mean pore
# Design of the MMM runs
MMM_CLASSES = 7  # classes of an input
MMM_RUNS, MMM_SEED = 20, 1  # runs of the Latin hypercube and its seed
# Ranges of the factors.
#   xi   up to xi_sat of HighBurnupStructurePorosity.C.
#        Below 0.02 the cube of Merope/distributedpore.py holds fewer than two pores of each class at the ends.
#   s    SCIANTIX 0.05 to 0.42 (CV_n 0.10 to 1.00); NFIR disc 0.36; K15 0.356
#   eta  SCIANTIX 0.034 to 0.228, 0.048 to 0.228 for xi >= 0.02; NFIR disc 0.14 (1474 K) to 0.18 (293 K)
MMM_FACTORS = {"xi": (0.02, 0.18), "s": (0.05, 0.42), "eta": (0.045, 0.23)} #TBC
MMM_PH_REL = 0.0  # P_h / p_bar of the runs
MMM_RUN_DIR = "mmm_inputs"
MMM_STATE = "NFIR, end of base irradiation"

# %% ------------------------------------------------------------------------------------------------------------------
# SCIANTIX states of regression/hbs_nfir/test_UO2HBS_frag_j19damage (formation 1, porosity 3), read from output.txt:
# the unvented moments n = n / S and var = (Var + n^2) / S_2 - n^2.
# V (m3), n (atoms per pore), var (Var(n)), xi (/), Np (pores/m3), T (K), Ph (Pa, positive in compression).

# TBC
STATES = {
    "NFIR, end of base irradiation": {"T": 903.15, "Ph": 70.0e6, "xi": 0.1304, "Np": 4.924e17, "V": 2.649e-19, "n": 1.199e9, "var": 3.698e17},
    "NFIR, anneal at 1200 C": {"T": 1473.5, "Ph": 0.0, "xi": 0.1228, "Np": 5.142e17, "V": 2.388e-19, "n": 1.148e9, "var": 3.761e17},
}


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
# 2. Gas. Distribution:
#       n_i = n R_i^a / sum_j w_j R_j^a
#    The gas density of a class is n_i / V_i ~ R_i^(a - 3)
#       a = 3 is a uniform pressure; a = 2 a density ~ 1/R (shape of punching overpressure in K15 and J19).
#    The width s is the root of sum_i w_i (n_i - n)^2 = Var(n) on the truncated nodes.
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

def rupture_step(beta, V, n_ret, var_ret, S1, S2, T, xi, Ph, R_mean, dt, a=A_CLOSURE): #TBC
    """Transcription of HighBurnupStructureFragmentation.C.
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

def savefig(fig, name):
    os.makedirs(FIG_DIR, exist_ok=True)
    fig.tight_layout()
    fig.savefig(os.path.join(FIG_DIR, name), dpi=150)
    plt.close(fig)
    print("wrote", os.path.join("figures", name))

def column_positions(n_runs):
    """x of the columns of the MMM panels: the two references (NFIR disc, SCIANTIX), then the runs; wider steps around
    SCIANTIX, the widest label."""
    return np.r_[0.0, 1.5, 2.9 + np.arange(n_runs)]

# %% ------------------------------------------------------------------------------------------------------------------
# Sections: tables on screen and figures

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

def section_nfir_data():
    """The measured pores of the NFIR disc in the variables of the classes, next to the SCIANTIX states
        -   porosity,
        -   radius of the mean pore volume and 
        -   width from the fit of nfir_disc, 
        -   mean packing fraction from the volume per gas atom."""
    print("\n== NFIR disc, measured pores (log-normal fit of the bars) against the SCIANTIX states ==")
    print(f"{'':34s}{'xi':>8s}{'R_V (um)':>10s}{'s':>8s}{'CV_n':>8s}{'eta_bar':>9s}")
    inside = lambda key, value: MMM_FACTORS[key][0] <= value <= MMM_FACTORS[key][1]
    for label, name in (("disc, as irradiated", "PoreRadiusASIRRADIATED.txt"), ("disc, after the anneal", "PoreRadiusPOSTANNEALING.txt")):
        disc = nfir_disc(name)
        print(f"{label:34s}{disc['xi']:8.4f}{disc['R_V'] * 1e6:10.4f}{disc['s']:8.4f}{np.sqrt(cv2_on_nodes(disc['s'], A_CLOSURE)):8.4f}")
        assert inside("xi", disc["xi"]) and inside("s", disc["s"]), label
    for name, st in STATES.items():
        xi, cv, eta = dimensionless(st)
        eta_disc = xe_volume(st["T"]) / NFIR_GAS_VOLUME
        print(f"{'SCIANTIX, ' + name.removeprefix('NFIR, '):34s}{xi:8.4f}{radius_of(st['V']) * 1e6:10.4f}{width(cv**2, A_CLOSURE):8.4f}{cv:8.4f}{eta:9.4f}"
              f"   (disc at {st['T']:.0f} K: eta_bar = {eta_disc:.4f})")
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
    """Sensitivity to the closure exponent a at the same moments. 
    On the pressure plot: 2 gamma / R in grey, and the punching limit of J19 Eq. 21, P_h + 2 gamma / R + c_dp / R (dashed)."""
    T, Ph = st["T"], st["Ph"]
    print(f"\n== closure exponent, {T:.0f} K ==")
    print(f"{'a':>5s}{'sigma_lnR':>11s}{'p_min':>9s}{'p_max':>9s}{'<p>_vol':>9s}   (MPa)   classes above the punching limit"
          "   rel. err. of V, n, Var(n)")
    fig, ax = plt.subplots(figsize=(5.8, 3.8))
    for a_ in (3.0, 2.5, 2.0):
        dp = build(R_V, st["n"], st["var"], T, a_)
        e = check_moments(dp, st["V"], st["n"], st["var"])
        pv = np.sum(dp["w"] * dp["V"] * dp["p"]) / np.sum(dp["w"] * dp["V"]) / 1e6
        above = np.sum(dp["w"][dp["p"] > Ph + (2.0 * GAMMA + C_DP) / dp["R"]])
        print(f"{a_:5.2f}{dp['sigma']:11.4f}{dp['p'].min() / 1e6:9.1f}{dp['p'].max() / 1e6:9.1f}{pv:9.1f}   {above:18.4f}"
              f"   {e['V']:.0e} {e['n']:.0e} {e['var_n']:.0e}")
        ax.plot(dp["R"] * 1e6, dp["p"] / 1e6, label=f"a = {a_}")
    R = np.geomspace(min(dp["R"]), max(dp["R"]), 100)
    ax.plot(R * 1e6, 2.0 * GAMMA / R / 1e6, color=GREY, label=r"$2\gamma/R$")
    ax.plot(R * 1e6, (Ph + (2.0 * GAMMA + C_DP) / R) / 1e6, "k--", lw=1, label=r"$P_h+2\gamma/R+c_{dp}/R$ (Jernkvist et al. 2019)")
    ax.set(xlabel="R (um)", ylabel="p (MPa)", yscale="log", title=f"T = {T:.0f} K, $P_h$ = {Ph / 1e6:.0f} MPa")
    ax.grid(True, color=GRID)
    ax.legend(frameon=False, loc="best")
    savefig(fig, "classes_closure_exponent.png")

def normalised(a_, cv_n_, eta_bar, T_=1000.0, R_V_=0.35e-6):
    """Classes in relative variables for given (a, CV_n, eta_bar): x = R / R_V, p / p_bar, n / n_bar, w.
    R_V_ and T_ only set the scale: n_bar is chosen so that the mean packing fraction is eta_bar."""
    V_ = sphere_volume(R_V_)
    n_ = eta_bar * V_ / xe_volume(T_)
    dd = build(R_V_, n_, (cv_n_ * n_) ** 2, T_, a_)
    return {"x": dd["R"] / R_V_, "w": dd["w"], "p_rel": dd["p"] / float(pore_pressure(n_, V_, T_)), "n_rel": dd["n"] / n_,
            "sigma": dd["sigma"], "x_median": dd["R_median"] / R_V_}

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


# %% ------------------------------------------------------------------------------------------------------------------
# Inputs for MMM
#
# MMM solves a linear elastic problem on a periodic RVE of spherical pores under internal pressure and
# returns the maximum first principal stress at every pore. 
# An input is, for every size class of the pores, its radius, its porosity and its gas pressure. 
# An RVE is scale-free in length and linear in the load, so the radius of the mean pore volume R_V and the mean
# pressure p_bar are not factors. With the closure exponent fixed, radius, porosity and pressure of the classes, each
# relative to its mean, depend on three factors:
#   xi       porosity: the distance between the pores
#   s        width of the radius distribution (sigma of ln R): R_k / R_V and the shares of the porosity xi_k / xi
#   eta_bar  mean packing fraction of the gas: with s, the pressure profile p_k / p_bar
# Confinement: the runs have P_h = 0.
# Reference: the NFIR disc before the anneal. The run NFIR is the log-normal fit of its measured pores with the measured gas density; the runs of the hypercube take its R_V and the T of MMM_STATE, the end of the base irradiation, which only sets p_bar.
# The side of the cube, the repulsion distance and the mesh size of Merope/distributedpore.py are in um.
# The 81 nodes are grouped in classes (bins of x centred on 0, +/-1, +/-2, ...). 
# The run files written here are read by Merope/distributedpore.py.

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

def mmm_case(tag, origin, xi, s, eta, Ph_rel, R_V, T, a=A_CLOSURE):
    """A run from its three factors (porosity xi, width s, mean packing fraction eta), from the confinement
    Ph_rel = P_h / p_bar and from the scale R_V (m), T (K): the parameters, CV_n, p_bar and P_h (Pa), and the 81 classes."""
    V = sphere_volume(R_V)
    n = eta * V / xe_volume(T)
    cv2 = cv2_on_nodes(s, a)
    p_bar = float(pore_pressure(n, V, T))
    return {"id": tag, "origin": origin, "xi": xi, "eta": eta, "R_V": R_V, "T": T, "a": a, "cv": np.sqrt(cv2), "p_bar": p_bar,
            "Ph": Ph_rel * p_bar, "classes": build(R_V, n, cv2 * n**2, T, a)}

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

def mmm_reference():
    """The reference run NFIR: the NFIR disc before the anneal."""
    disc, T = nfir_disc(), STATES[MMM_STATE]["T"]
    return mmm_case("NFIR", "NFIR disc before the anneal, log-normal fit of the measured pores", disc["xi"], disc["s"], xe_volume(T) / NFIR_GAS_VOLUME, MMM_PH_REL, disc["R_V"], T)

def mmm_run_files(cases, n_classes=MMM_CLASSES):
    """One text file per run in MMM_RUN_DIR, with one row per pore class: radius (um), porosity of the class,
    gas pressure (MPa), confining pressure (MPa), the input of Merope/distributedpore.py; and index.txt with the
    factors of every run."""
    folder = os.path.join(HERE, MMM_RUN_DIR)
    os.makedirs(folder, exist_ok=True)
    for old in os.listdir(folder):  # the runs of a previous design
        if old == "index.txt" or (old.startswith("run_") and old.endswith(".txt")):
            os.remove(os.path.join(folder, old))
    ranges = ", ".join(f"{name} {lo:g}-{hi:g}" for name, (lo, hi) in zip(("xi", "s", "eta_bar"), MMM_FACTORS.values()))
    index = [f"Latin hypercube of {len(cases)} runs (seed {MMM_SEED}) in {ranges};", 
             f"{n_classes} pore classes each, closure exponent a = {A_CLOSURE:g}; reference: NFIR disc before the anneal (sets R_V and T)",
             f"# {'run':6s}{'xi':>8s}{'s':>8s}{'CV_n':>8s}{'eta_bar':>9s}{'eta_max':>9s}{'Ph/p_bar':>10s}{'R_V_um':>9s}{'T_K':>9s}{'p_bar_MPa':>11s}{'Ph_MPa':>9s}  file"]
    for c in [mmm_reference()] + cases:
        d, xi, p_bar, Ph = c["classes"], c["xi"], c["p_bar"], c["Ph"]
        file = f"run_{c['id']}.txt"
        lines = [f"# MMM input, run {c['id']} ({c['origin']})",
                 f"# xi = {xi:g}, s = {d['sigma']:.4g} (CV_n = {c['cv']:.4g}), eta_bar = {c['eta']:.4g}, P_h / p_bar = {Ph / p_bar:.4g}, a = {c['a']:g}",
                 f"# scale: R_V = {c['R_V'] * 1e6:.4g} um, T = {c['T']:g} K, p_bar = {p_bar / 1e6:.5g} MPa",
                 "# one row per pore class; the porosities sum to xi",
                 f"# {'radius_um':>10s}{'porosity':>14s}{'pressure_MPa':>14s}{'confinement_MPa':>17s}"]
        reduced = reduced_classes(d, c["T"], n_classes)
        lines += [f"{k['R'] * 1e6:12.6f}{xi * k['xi_share']:14.6e}{k['p'] / 1e6:14.4f}{Ph / 1e6:17.4f}" for k in reduced]
        with open(os.path.join(folder, file), "w") as fh:
            fh.write("\n".join(lines) + "\n")
        index.append(f"  {c['id']:6s}{xi:8.4f}{d['sigma']:8.4f}{c['cv']:8.4f}{c['eta']:9.4f}{packing_max(reduced, c['T']):9.4f}{Ph / p_bar:10.3f}{c['R_V'] * 1e6:9.4f}"
                     f"{c['T']:9.1f}{p_bar / 1e6:11.2f}{Ph / 1e6:9.2f}  {file}")
    with open(os.path.join(folder, "index.txt"), "w") as fh:
        fh.write("\n".join(index) + "\n")
    print(f"wrote {len(cases) + 1} run files and index.txt in {MMM_RUN_DIR}/ that can be read by Merope/distributedpore.py")
    print("\n".join(index))

    _, disc = len(cases), nfir_disc()
    R_V, T, _ = cases[0]["R_V"], cases[0]["T"], cases[0]["a"]

    blue, orange = LinearSegmentedColormap.from_list("blue", SEQ_BLUE), LinearSegmentedColormap.from_list("orange", SEQ_ORANGE)
    by_s, by_eta = Normalize(*MMM_FACTORS["s"]), Normalize(*MMM_FACTORS["eta"])
    fig, ax = plt.subplots(1, 3, figsize=(14.5, 4.3))
    D = np.linspace(0.0, 6.0e-6, 301)[1:]  # (m) pore diameter
    cumulative = lambda xi, R_V_, s: 100.0 * np.cumsum(porosity_per_bin(xi, R_V_, s, np.append(0.0, D)))
    for c in cases:
        s, reduced = c["classes"]["sigma"], reduced_classes(c["classes"], T, n_classes)
        R_k, p_k = np.array([k["R"] for k in reduced]), np.array([k["p"] for k in reduced])
        ax[0].plot(D * 1e6, cumulative(1.0, R_V, s), color=blue(by_s(s)), lw=1.2)
        ax[1].plot(R_k * 1e6, [c["xi"] * k["xi_share"] for k in reduced], "o-", color=blue(by_s(s)), lw=1.0, ms=4.5, mec="white", mew=0.5)
        ax[2].plot(R_k / R_V, p_k / c["p_bar"], "-", color=orange(by_eta(c["eta"])), lw=1.0)
        ax[2].plot(R_k / R_V, p_k / c["p_bar"], "o", color=orange(by_eta(c["eta"])), ms=4.5, mec="white", mew=0.5)
    ax[0].plot(disc["edges"][1:] * 1e6, 100.0 * np.cumsum(disc["bars"]) / disc["bars"].sum(), "o", color="k", ms=4, mec="white", mew=0.5, zorder=4,
               label="NFIR disc, as irradiated")
    ax[0].plot(D * 1e6, cumulative(disc["xi"] / disc["bars"].sum(), disc["R_V"], disc["s"]), color=BROWN, lw=1.5, ls="--", zorder=3,
               label=f"Log-normal fit of the NFIR disc")
    ax[0].set(xlabel="Pore diameter (um)", ylabel="Cumulative share of the porosity (%)", xlim=(0.0, 6.0),
              title=f"Radius distributions, $R_V$ = {R_V * 1e6:.2f} um")
    ax[1].set(xlabel="radius of the class (um)", ylabel="Porosity of the class", xscale="log", yscale="log",
              title=f"{n_classes} pore classes of every run")
    ax[2].set(xlabel=f"$R / R_V$", ylabel=r"p / $\bar p$", yscale="log", title="Normalised pressure")
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

    # EXP , SCIANTIX
    st = STATES[MMM_STATE]
    columns = [("Exp.\nNFIR", GREEN, "D", mmm_reference()["classes"]),
               ("SCIANTIX\nNFIR", ORANGE, "s", build(radius_of(st["V"]), st["n"], st["var"], st["T"]))]
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
               mec="white", mew=1.0, zorder=4, label="SCIANTIX, NFIR case")
    ax[1].axhspan(measured.min(), measured.max(), color=GREY, alpha=0.2, lw=0, label="measured, all series")
    xs = column_positions(len(cases))
    for x, (name, color, marker, d) in zip(xs, columns):
        reduced, m = reduced_classes(d, T, n_classes), moments(d)
        v = np.array([k["V"] / k["n"] for k in reduced]) * A3
        gas = np.array([k["w"] * k["n"] for k in reduced]) / m["n"]
        ax[1].scatter(np.full(n_classes, x), v, s=8.0 + 160.0 * gas, marker=marker, color=color, edgecolors="white", linewidths=0.5,
                      zorder=3)
        ax[1].plot(x, m["V"] / m["n"] * A3, "_", color="k", ms=14, mew=1.6, zorder=4)
    ax[1].scatter([], [], s=60, color=GREY, edgecolors="white", label="class, area ~ share of the gas")
    ax[1].plot([], [], "_", color="k", ms=14, mew=1.6, label="mean V / n")
    ax[1].axvline(0.5 * (xs[1] + xs[2]), color=GREY, lw=0.8)
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
    disc, after, st = nfir_disc(), nfir_disc("PoreRadiusPOSTANNEALING.txt"), STATES[MMM_STATE]
    s_state = width(st["var"] / st["n"] ** 2, A_CLOSURE)
    edges = 0.5 * disc["edges"] * um  # (um) radius edges of the bins of 0.2 um of diameter
    cum = np.concatenate(([0.0], np.cumsum(disc["bars"]))) / disc["bars"].sum()
    measured = np.interp((0.05, 0.95), cum, edges)
    columns = [("Exp.\nNFIR", GREEN, "D", mmm_reference()["classes"]),
               ("SCIANTIX\nNFIR", ORANGE, "s", build(radius_of(st["V"]), st["n"], st["var"], st["T"]))]
    columns += [(c["id"][1:], BLUE, "o", c["classes"]) for c in cases]
    reduced = [reduced_classes(d, T, n_classes) for *_, d in columns]
    R_runs = np.array([[k["R"] for k in red] for red in reduced[2:]]) * um
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
    ax[0].plot(per_bin(st["xi"], radius_of(st["V"]), s_state), 0.5 * (r[1:] + r[:-1]), color=ORANGE, lw=1.8,
               label=f"SCIANTIX, NFIR end of base irradiation")
    xs = column_positions(len(cases))
    for x, (name, color, marker, d), red in zip(xs, columns, reduced):
        R_k = np.array([k["R"] for k in red]) * um
        share = np.array([k["xi_share"] for k in red])
        ax[1].scatter(np.full(n_classes, x), R_k, s=8.0 + 160.0 * share, marker=marker, color=color, edgecolors="white",
                      linewidths=0.5, zorder=3)
        ax[1].plot(x, moments(d)["R_V"] * um, "_", color="k", ms=14, mew=1.6, zorder=4)
    ax[1].scatter([], [], s=60, color=GREY, edgecolors="white", label="class, area ~ share of the porosity")
    ax[1].plot([], [], "_", color="k", ms=14, mew=1.6, label="$R_V$")
    ax[1].axvline(0.5 * (xs[1] + xs[2]), color=GREY, lw=0.8)
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
    """Time step (s) of every row of output.txt."""
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

def check(o, tol=1.0e-6):
    """Replays the recursion of option 4 on the history of the NFIR case and compares D, G, S, S_2 and the broken
    fraction of every class with its output o. A row of output.txt holds the retained moments and S, S_2 after the
    venting of the step: the unvented moments are n / S and (Var + n^2) / S_2 of the same row. The state read from
    output.txt has 7 digits, so the agreement is not to round-off: tol [N] is above the digits of the file.
    Returns 0 if every difference is within tol and STATES agrees with the output; 1 otherwise."""
    rows = len(o["Time"])
    dt = time_steps(NFIR_CASE, rows)
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
    print(f"\n== replay of option 4 on {NFIR_CASE} ({rows} rows) ==")
    print(f"final  D = {D:.6e} (SCIANTIX {o['HBS fragmented fraction'][-1]:.6e}),  G = {G:.6e} ({o['HBS burst release fraction'][-1]:.6e}),  "
          f"S = {S1:.6f} ({o['HBS gas retention fraction'][-1]:.6f})")
    for name in err:
        print(f"max |{name}: python - sciantix| = {err[name]:.2e}  (row {row[name]}, T = {o['Temperature'][row[name]]:.1f} K)")
    ok = max(err.values()) <= tol
    if not ok:
        print(f"the transcription and the C++ differ by more than {tol:.0e}")
    ok = states_agree(o) and ok
    return 0 if ok else 1

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
        d = build(radius_of(st["V"]), st["n"], st["var"], T[k])
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
    section_distributions(st, d, m)
    section_mean_pressure(st, R_V, d)
    section_closure_exponent(st, R_V)
    section_coverage()
    print("\n== inputs for MMM ==")
    cases = mmm_design(MMM_RUNS)
    mmm_run_files(cases, MMM_CLASSES)
    gas_volume_figure(cases, MMM_CLASSES)
    pore_radius_figure(cases, MMM_CLASSES)
    output = os.path.join(NFIR_CASE, "output.txt")
    if not os.path.exists(output):
        print(f"\nno {output}: run the NFIR case for its replay and its rupture and release figures")
        return 0
    o = read_output(output)
    status = check(o)
    rupture_figure(o)
    fgr_figure(o)
    return status

if __name__ == "__main__":
    sys.exit(main())


# Unused, used before as checks
def section_k15_check():
    """Self-check of build() on a case with a known solution:
    Var(n) of the K15 reference width with the closure n_i ~ R_i^a must give back s = 0.356.."""
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
        d = mmm_case("", "", disc["xi"], disc["s"], xe_volume(T) / NFIR_GAS_VOLUME, MMM_PH_REL, disc["R_V"], T, a_)["classes"]
        ax.plot(d["R"][central] * 1e6, (d["V"] / d["n"])[central] * 1e30, ls, color=GREEN, lw=1.5, label=f"classes of the NFIR disc, a = {a_:g}")
    d = build(radius_of(st["V"]), st["n"], st["var"], st["T"])
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

