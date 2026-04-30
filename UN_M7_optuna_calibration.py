# UN M7 GLOBAL/TARGETED SENSITIVITY SWEEP - complete notebook cell / standalone script
#
# Purpose:
#   Complete self-contained M7 model + compact global sensitivity sweep.
#   This file DOES NOT need previous notebook variables.
#
# Physics kept as in our M7 implementation:
#   - gas mass coupling of bulk nucleation
#   - phi re-solution for bulk/dislocation bubbles
#   - bulk-dislocation capture with the Barani/Rizk-like linear clipped step:
#         f_cap = min(1, max(0, capture_scale * N_d * Delta V_cap))
#   - dislocation-bubble coalescence ALWAYS ON; only its scale is varied, never set to 0.
#
# How to use in VS Code/Jupyter:
#   1) Open this file, copy everything into one notebook cell, run.
#   2) The script saves CSV + PNG outputs in OUTPUT_DIR.
#   3) If WSL disconnects, rerun the same cell: the fast sweep resumes from CSV.
#
# Important settings to adjust if it is too slow:
#   N_GLOBAL_FAST = 40        # lighter first pass
#   N_TOP_FINAL   = 4         # fewer final checks
#   SHOW_PLOTS    = False     # already default; figures are saved to disk
#
# ============================================================

import os
import csv
import math
import random
from dataclasses import dataclass
from typing import Optional, Sequence, Dict, List, Tuple

try:
    import matplotlib.pyplot as plt
except ModuleNotFoundError:
    plt = None

# ============================================================
# USER CONFIGURATION
# ============================================================

OUTPUT_DIR = "UN_M7_global_sensitivity_fullcell"
os.makedirs(OUTPUT_DIR, exist_ok=True)

RANDOM_SEED = 123

# Compact by default. Increase only after checking that the pipeline works.
N_GLOBAL_FAST = 64
N_TOP_FINAL = 6
PLOT_TOP_N = 3

FAST_DT_H = 12.0
FAST_N_MODES = 22

FINAL_DT_H = 1.0
FINAL_N_MODES = 40

SAVE_FIGS = True
SHOW_PLOTS = False  # safer for WSL; plots are saved as PNGs
RESUME_FAST = True

# Score weights. Pressure is deliberately weak: it is a sanity term, not the main calibration target.
W_SWELLING = 1.00
W_RD = 0.60
W_ND_LEVEL = 0.65
W_ND_DROP = 1.00
W_PRESSURE = 0.20   # pressure is a physical sanity term; penalizes large p/p_eq deviations
W_FDOT_PRIOR = 0.25
W_RIZK_GAS_SHAPE = 0.08
W_HIGHT_GUARD = 0.08

# Model family switches. They are set by --family in the Optuna runner.
MODEL_FAMILY = "M7_full"
USE_PHI_GAS_RESOLUTION = True       # phi multiplies gas re-solution terms only
USE_NUCLEATION_MASS_COUPLING = True # source_c=beta-2nu, source_mb=2nu
USE_BULK_DISLOCATION_CAPTURE = True # Barani-like linear clipped capture
PRESSURE_FREE_FACTOR = 3.0          # no pressure penalty inside [1/3, 3] for p/p_eq

# Fixed constants / nominal Rizk values
GRAIN_RADIUS = 6.0e-6       # m
XE_YIELD = 0.24             # Xe atoms/fission
GAMMA_B = 1.11              # J/m2
OMEGA_FG = 8.5e-29          # m3
LATTICE_PARAMETER = 4.889e-10

FISSION_RATE_NOMINAL = 5.0e19
F_N_NOMINAL = 1.0e-6
K_D_NOMINAL = 5.0e5
RHO_D_NOMINAL = 3.0e13

# ============================================================
# GLOBAL COMPACT PARAMETER RANGES
# ============================================================
# These are sampled with a simple Latin-hypercube-like method, implemented without scipy.
# Keep fission rate close to nominal by default.

PARAM_RANGES = {
    # name: (low, high, scale)
    "f_n":                 (1.0e-8, 1.0e-6, "log"),
    "K_d":                 (1.5e5, 1.0e6, "log"),
    "rho_d":               (1.0e13, 8.0e13, "log"),
    "fission_rate":        (4.0e19, 6.0e19, "linear"),
    "Dv_scale":            (0.2, 3.0, "log"),
    "Dg_scale":            (0.3, 3.0, "log"),
    "D2_xe_scale":         (0.1, 10.0, "log"),
    "b_scale":             (0.2, 5.0, "log"),
    "gb_scale":            (0.2, 5.0, "log"),
    "gd_scale":            (0.2, 8.0, "log"),
    "coalescence_d_scale": (0.3, 10.0, "log"),  # never zero
    "capture_scale":       (0.3, 3.0, "log"),   # no zero in this sweep
}

PARAM_NAMES = list(PARAM_RANGES.keys())

# Main diagnostic plotting grids
TEMPS_MAIN = [float(T) for T in range(900, 2001, 50)]
TEMPS_GAS_PARTITION = [float(T) for T in range(900, 2601, 100)]
BURNUPS = [1.1, 1.3, 3.2]

# ============================================================
# DIGITIZED EXPERIMENTAL POINTS
# ============================================================

EXP_SWELLING_T = [
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1127.0, "swelling": 0.68},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1228.0, "swelling": 0.59},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1312.0, "swelling": 0.43},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1402.0, "swelling": 0.58},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1485.0, "swelling": 1.22},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1549.0, "swelling": 1.84},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1598.0, "swelling": 1.66},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1632.0, "swelling": 2.13},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1669.0, "swelling": 3.60},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1685.0, "swelling": 2.72},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 899.0,  "swelling": 0.63},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1154.0, "swelling": 1.17},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1228.0, "swelling": 1.08},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1325.0, "swelling": 1.28},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1435.0, "swelling": 1.32},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1514.0, "swelling": 2.10},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1570.0, "swelling": 2.72},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1608.0, "swelling": 2.91},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1635.0, "swelling": 3.28},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1656.0, "swelling": 3.75},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1044.0, "swelling": 1.11},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1220.0, "swelling": 1.22},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1377.0, "swelling": 1.33},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1534.0, "swelling": 2.83},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1595.0, "swelling": 3.53},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1639.0, "swelling": 3.86},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1661.0, "swelling": 2.45},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1709.0, "swelling": 2.93},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1724.0, "swelling": 3.15},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 984.0,  "swelling": 0.72},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1056.0, "swelling": 1.06},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1126.0, "swelling": 1.26},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1247.0, "swelling": 1.58},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1343.0, "swelling": 1.79},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1420.0, "swelling": 2.08},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1459.0, "swelling": 2.40},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1511.0, "swelling": 2.83},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1557.0, "swelling": 3.31},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1590.0, "swelling": 3.75},
]

EXP_SWELLING_BURNUP_1600 = [
    {"burnup": 1.12, "swelling": 1.64, "series": "approx 100 kW/m"},
    {"burnup": 1.11, "swelling": 2.90, "series": "approx 119 kW/m"},
    {"burnup": 1.31, "swelling": 3.51, "series": "measurement"},
    {"burnup": 3.18, "swelling": 3.72, "series": "measurement"},
]

EXP_ND_T_13 = [
    {"T": 1153.0, "N": 5.22e19}, {"T": 1202.0, "N": 3.69e19},
    {"T": 1227.0, "N": 3.00e19}, {"T": 1235.0, "N": 3.57e19},
    {"T": 1248.0, "N": 2.44e19}, {"T": 1330.0, "N": 2.90e19},
    {"T": 1338.0, "N": 4.10e19}, {"T": 1377.0, "N": 2.71e19},
    {"T": 1408.0, "N": 1.67e19}, {"T": 1427.0, "N": 1.85e19},
    {"T": 1493.0, "N": 3.00e19}, {"T": 1507.0, "N": 2.44e19},
    {"T": 1524.0, "N": 1.56e19}, {"T": 1538.0, "N": 7.54e18},
    {"T": 1555.0, "N": 7.80e18}, {"T": 1561.0, "N": 1.92e19},
    {"T": 1599.0, "N": 5.34e18}, {"T": 1622.0, "N": 7.80e18},
    {"T": 1628.0, "N": 1.73e19}, {"T": 1649.0, "N": 5.34e18},
    {"T": 1656.0, "N": 3.65e18}, {"T": 1656.0, "N": 2.41e18},
    {"T": 1669.0, "N": 1.96e18}, {"T": 1682.0, "N": 2.44e19},
    {"T": 1685.0, "N": 1.40e19}, {"T": 1723.0, "N": 7.70e17},
    {"T": 1742.0, "N": 1.02e18}, {"T": 1739.0, "N": 1.65e18},
]

EXP_RD_T_13 = [
    {"T": 1045.0, "R_nm": 54.06}, {"T": 1219.0, "R_nm": 60.37},
    {"T": 1374.0, "R_nm": 69.58}, {"T": 1535.0, "R_nm": 104.85},
    {"T": 1594.0, "R_nm": 120.83}, {"T": 1641.0, "R_nm": 143.73},
    {"T": 1663.0, "R_nm": 122.76}, {"T": 1710.0, "R_nm": 157.99},
    {"T": 1725.0, "R_nm": 173.67},
]

# ============================================================
# MODEL PARAMETERS
# ============================================================

@dataclass(frozen=True)
class Candidate:
    label: str
    f_n: float
    K_d: float
    rho_d: float
    fission_rate: float
    Dv_scale: float
    Dg_scale: float
    b_scale: float
    gb_scale: float
    gd_scale: float
    coalescence_d_scale: float
    capture_scale: float
    D2_xe_scale: float = 1.0


@dataclass
class UNParameters:
    temperature: float = 1600.0
    fission_rate: float = FISSION_RATE_NOMINAL
    grain_radius: float = GRAIN_RADIUS
    target_burnup_percent_fima: Optional[float] = None
    final_time: float = 24.0 * 3600.0
    dt: float = FAST_DT_H * 3600.0
    n_modes: int = FAST_N_MODES

    xe_yield: float = XE_YIELD
    precursor_factor: float = 1.0

    # Xe diffusivity: D1 + D2 + D3 (Rizk Table 2).
    D10: float = 1.56e-3
    Q1: float = 4.94
    A20_xe: float = 1.21e-67
    B21_xe: float = 25.87
    B22_xe: float = -1.49
    B23_xe: float = 0.0
    A30: float = 1.85e-39
    D2_xe_scale: float = 1.0
    Dg_scale: float = 1.0

    kB_eV: float = 8.617333262e-5
    kB_J: float = 1.380649e-23

    # Vacancy diffusivity: refit previously obtained from Rizk Fig. 4.
    D10_vU: float = 1.35e-2
    Q1_vU: float = 5.66
    B21_vU_refit: float = -0.62
    B22_vU_refit: float = -0.04
    A20_vU_fig4_refit: float = 4.6304523933553033e-29
    Dv_scale: float = 1.0

    radius_in_lattice: float = 0.21e-9
    omega_fg: float = OMEGA_FG
    lattice_parameter: float = LATTICE_PARAMETER
    gamma_b: float = GAMMA_B
    hydrostatic_stress: float = 0.0
    min_radius_for_pressure: float = 1.0e-15

    f_n: float = F_N_NOMINAL
    rho_d: float = RHO_D_NOMINAL
    K_d: float = K_D_NOMINAL
    r_d: float = 3.46e-10
    Z_d: float = 5.0

    Dg_extra_scale: float = 1.0
    gb_scale: float = 1.0
    gd_scale: float = 1.0
    b_scale: float = 1.0
    coalescence_d_scale: float = 1.0
    capture_scale: float = 1.0

    R_b: float = 0.0
    N_b: float = 0.0
    R_d: float = 0.0
    N_d: Optional[float] = None
    c0: float = 0.0
    mb0: float = 0.0
    md0: float = 0.0
    nvb0: Optional[float] = None
    nvd0: Optional[float] = None

    bulk_seed_radius_nm: float = 0.0
    vacancy_absorption_only: bool = True
    update_bulk_vacancies: bool = True
    min_number_density: float = 0.0
    min_volume: float = 0.0

    def __post_init__(self):
        if self.N_d is None:
            self.N_d = self.K_d * self.rho_d
        if self.target_burnup_percent_fima is not None:
            self.final_time = burnup_percent_to_time(
                self.target_burnup_percent_fima,
                self.fission_rate,
                self.lattice_parameter,
            )

# ============================================================
# HELPER FUNCTIONS
# ============================================================

def omega_matrix(p: UNParameters) -> float:
    return p.lattice_parameter**3 / 4.0


def uranium_atom_density_from_lattice(lattice_parameter: float) -> float:
    return 4.0 / lattice_parameter**3


def burnup_percent_to_time(burnup_percent_fima: float, fission_rate: float, lattice_parameter: float) -> float:
    if fission_rate <= 0.0:
        raise ValueError("fission_rate must be positive")
    return (burnup_percent_fima / 100.0) * uranium_atom_density_from_lattice(lattice_parameter) / fission_rate


def time_to_burnup_percent(time: float, fission_rate: float, lattice_parameter: float) -> float:
    return 100.0 * fission_rate * time / uranium_atom_density_from_lattice(lattice_parameter)


def sphere_volume(R: float) -> float:
    return 0.0 if R <= 0.0 else (4.0 / 3.0) * math.pi * R**3


def radius_from_volume(V: float) -> float:
    return 0.0 if V <= 0.0 else (3.0 * V / (4.0 * math.pi)) ** (1.0 / 3.0)


def xe_diffusivity_UN(p: UNParameters):
    T = p.temperature
    kBT = p.kB_eV * T
    D1 = p.D10 * math.exp(-p.Q1 / kBT)
    # Irradiation-enhanced Xe diffusivity. Rizk notes it is usually small,
    # but it is kept explicitly here instead of being set to zero.
    try:
        expo = (
            -p.B21_xe / kBT
            -p.B22_xe / (kBT**2)
            -p.B23_xe / (kBT**3)
        )
        # avoid numerical overflow if trial ranges are later modified
        expo = max(min(expo, 700.0), -745.0)
        D2 = math.sqrt(p.fission_rate) * p.A20_xe * math.exp(expo)
    except OverflowError:
        D2 = math.inf
    D3 = p.A30 * p.fission_rate
    Dg_unscaled = D1 + p.D2_xe_scale * D2 + D3
    Dg = Dg_unscaled * p.precursor_factor * p.Dg_extra_scale
    return Dg, {
        "D1_Xe": D1,
        "D2_Xe": D2,
        "D2_Xe_scaled": p.D2_xe_scale * D2,
        "D3_Xe": D3,
        "D2_Xe_over_Dg_unscaled": (p.D2_xe_scale * D2 / Dg_unscaled) if Dg_unscaled > 0 and math.isfinite(Dg_unscaled) else math.nan,
        "Dg": Dg,
    }


def vacancy_diffusivity_UN(p: UNParameters):
    T = p.temperature
    kBT = p.kB_eV * T
    D1 = p.D10_vU * math.exp(-p.Q1_vU / kBT)
    D2 = math.sqrt(p.fission_rate) * p.A20_vU_fig4_refit * math.exp(
        p.B21_vU_refit / kBT + p.B22_vU_refit / (kBT**2)
    )
    Dv = (D1 + D2) * p.Dv_scale
    return Dv, {"Dv1": D1, "Dv2": D2, "Dv": Dv}


def b0_resolution(R: float) -> float:
    R = max(R, 1.0e-15)
    return 1.0e-25 * (2.64 - 2.02 * math.exp(-2.61e-9 / R))


def resolution_rates_UN(p: UNParameters, R_b: float, R_d: float):
    b_b = p.fission_rate * b0_resolution(R_b + p.radius_in_lattice) * p.b_scale
    b_d = p.fission_rate * b0_resolution(R_d + p.radius_in_lattice) * p.b_scale
    return b_b, b_d


def trapping_rates_UN(p: UNParameters, Dg: float, R_b: float, N_b: float, R_d: float, N_d: float):
    Rb_eff = R_b + p.radius_in_lattice
    Rd_eff = R_d + p.radius_in_lattice
    g_b_unscaled = 0.0 if N_b <= 0.0 else 4.0 * math.pi * Dg * Rb_eff * N_b

    Gamma_d = 1.0 / math.sqrt(math.pi * p.rho_d)
    den = math.log(Gamma_d / (p.Z_d * p.r_d)) - 3.0 / 5.0
    if den <= 0.0:
        raise ValueError(f"Invalid dislocation sink denominator: {den:g}")

    free_dislocation = max(p.rho_d - 2.0 * R_d * N_d, 0.0)
    term_bubbles = 4.0 * math.pi * Dg * Rd_eff * N_d
    term_dislocation = (2.0 * math.pi * Dg / den) * free_dislocation
    g_d_unscaled = term_bubbles + term_dislocation

    g_b = p.gb_scale * g_b_unscaled
    g_d = p.gd_scale * g_d_unscaled

    return g_b, g_d, {
        "Gamma_d": Gamma_d,
        "den": den,
        "free_dislocation": free_dislocation,
        "term_bubbles": term_bubbles,
        "term_dislocation": term_dislocation,
        "g_b_unscaled": g_b_unscaled,
        "g_d_unscaled": g_d_unscaled,
    }


def beta_production(p: UNParameters) -> float:
    return p.xe_yield * p.fission_rate


def nucleation_rate_bulk(p: UNParameters, Dg: float, c: float) -> float:
    return 8.0 * math.pi * p.f_n * Dg * p.omega_fg ** (1.0 / 3.0) * max(c, 0.0) ** 2


def phi_population(m_gas: float, N: float) -> float:
    if N <= 0.0 or m_gas <= 0.0:
        return 0.0
    atoms_per_bubble = m_gas / N
    if atoms_per_bubble <= 1.0:
        return 0.0
    return 1.0 / (atoms_per_bubble - 1.0)


def coalescence_lambda(Vd: float, Nd: float) -> float:
    xi = max(0.0, min(Vd * Nd, 0.999999))
    return (2.0 - xi) / (2.0 * (1.0 - xi) ** 3)


def pressure_internal(p: UNParameters, m_gas: float, n_vac: float) -> float:
    if m_gas <= 0.0:
        return 0.0
    if n_vac <= 0.0:
        return math.inf
    denom = n_vac * omega_matrix(p)
    return math.inf if denom <= 0.0 else p.kB_J * p.temperature * m_gas / denom


def pressure_equilibrium(p: UNParameters, R: float) -> float:
    return 2.0 * p.gamma_b / max(R, p.min_radius_for_pressure) - p.hydrostatic_stress


def gas_only_radius_for_population(p: UNParameters, m_gas: float, N: float) -> float:
    if m_gas <= 0.0 or N <= 0.0:
        return 0.0
    return radius_from_volume(p.omega_fg * m_gas / N)


def radius_for_vacancy_update(p: UNParameters, R_old: float, N: float, m_gas: float) -> float:
    if R_old > 0.0:
        return R_old
    return gas_only_radius_for_population(p, m_gas, N)


def wigner_seitz_delta(N: float) -> float:
    return (3.0 / (4.0 * math.pi * max(N, 1.0))) ** (1.0 / 3.0)


def zeta_geometry(R: float, N: float) -> float:
    delta = wigner_seitz_delta(N)
    psi = max(R / delta, 1.0e-12)
    den = -psi**6 + 5.0 * psi**2 - 9.0 * psi + 5.0
    den = max(den, 1.0e-30)
    return max(10.0 * psi * (1.0 + psi**3) / den, 1.0e-30)


def vacancy_concentration_implicit_step(p: UNParameters, Dv: float, R: float, N: float, m_gas: float, n_old: float, dt: float):
    if N <= 0.0 or m_gas <= 0.0:
        return n_old, 0.0
    R_update = radius_for_vacancy_update(p, R, N, m_gas)
    if R_update <= 0.0:
        return n_old, 0.0

    p_eq = 2.0 * p.gamma_b / R_update - p.hydrostatic_stress
    p_int_old = pressure_internal(p, m_gas, n_old)

    if p.vacancy_absorption_only and p_int_old <= p_eq:
        return n_old, 0.0

    delta = wigner_seitz_delta(N)
    zeta = zeta_geometry(R_update, N)
    A = 2.0 * math.pi * Dv * delta * N / (p.kB_J * p.temperature * zeta)
    C = p.kB_J * p.temperature * m_gas / omega_matrix(p)
    B = n_old - dt * A * p_eq
    disc = B * B + 4.0 * dt * A * C

    if disc < 0.0:
        raise ValueError(f"Negative discriminant in vacancy step: {disc:g}")

    sqrt_disc = math.sqrt(disc)
    if B >= 0.0:
        n_new = 0.5 * (B + sqrt_disc)
    else:
        denom = sqrt_disc - B
        n_new = 0.0 if denom <= 0.0 else (2.0 * dt * A * C) / denom

    if p.vacancy_absorption_only:
        n_new = max(n_new, n_old)

    return n_new, (n_new - n_old) / dt


def initialize_vacancy_concentration(p: UNParameters, N: float, R: float, m_gas: float) -> float:
    if N <= 0.0 or R <= 0.0:
        return 0.0
    vacancy_volume = max(N * sphere_volume(R) - p.omega_fg * m_gas, 0.0)
    return vacancy_volume / omega_matrix(p)


def initialize_modes_from_average(average: float, n_modes: int, n_iter: int = 20):
    modes = [0.0 for _ in range(n_modes)]
    projection_coeff = -math.sqrt(8.0 / math.pi)
    remainder = average
    for _ in range(n_iter):
        reconstructed = 0.0
        for i in range(n_modes):
            n = i + 1
            n_coeff = (-1.0) ** n / n
            modes[i] += projection_coeff * n_coeff * remainder
            reconstructed += projection_coeff * n_coeff * modes[i] * 3.0 / (4.0 * math.pi)
        remainder = average - reconstructed
    return modes


def reconstruct_average(modes: Sequence[float]) -> float:
    projection_coeff = -2.0 * math.sqrt(2.0 / math.pi)
    average = 0.0
    for i, value in enumerate(modes):
        n = i + 1
        n_coeff = (-1.0) ** n / n
        average += projection_coeff * n_coeff * value / ((4.0 / 3.0) * math.pi)
    return average


def det3(A):
    return (
        A[0][0] * (A[1][1] * A[2][2] - A[1][2] * A[2][1])
        - A[0][1] * (A[1][0] * A[2][2] - A[1][2] * A[2][0])
        + A[0][2] * (A[1][0] * A[2][1] - A[1][1] * A[2][0])
    )


def solve3x3_cramer(A, b):
    detA = det3(A)
    if abs(detA) < 1.0e-300:
        raise ZeroDivisionError("Singular 3x3 system")
    Ax = [[b[i], A[i][1], A[i][2]] for i in range(3)]
    Ay = [[A[i][0], b[i], A[i][2]] for i in range(3)]
    Az = [[A[i][0], A[i][1], b[i]] for i in range(3)]
    return [det3(Ax) / detA, det3(Ay) / detA, det3(Az) / detA]


def sciantix_3x3_exchange_step(
    modes_c,
    modes_mb,
    modes_md,
    Dg: float,
    R_grain: float,
    source_c: float,
    source_mb: float,
    source_md: float,
    g_b: float,
    g_d: float,
    b_b_gas: float,
    b_d_gas: float,
    dt: float,
):
    projection_coeff = -2.0 * math.sqrt(2.0 / math.pi)
    diffusion_rate_coeff = math.pi**2 * Dg / R_grain**2

    for i in range(len(modes_c)):
        n = i + 1
        n_coeff = (-1.0) ** n / n
        diffusion_rate = diffusion_rate_coeff * n**2

        src_c = projection_coeff * source_c * n_coeff
        src_mb = projection_coeff * source_mb * n_coeff
        src_md = projection_coeff * source_md * n_coeff

        A = [
            [1.0 + (diffusion_rate + g_b + g_d) * dt, -b_b_gas * dt, -b_d_gas * dt],
            [-g_b * dt, 1.0 + b_b_gas * dt, 0.0],
            [-g_d * dt, 0.0, 1.0 + b_d_gas * dt],
        ]
        rhs = [
            modes_c[i] + src_c * dt,
            modes_mb[i] + src_mb * dt,
            modes_md[i] + src_md * dt,
        ]
        modes_c[i], modes_mb[i], modes_md[i] = solve3x3_cramer(A, rhs)

    return reconstruct_average(modes_c), reconstruct_average(modes_mb), reconstruct_average(modes_md)


def reset_modes_to_averages(c: float, mb: float, md: float, n_modes: int):
    return (
        initialize_modes_from_average(max(c, 0.0), n_modes),
        initialize_modes_from_average(max(mb, 0.0), n_modes),
        initialize_modes_from_average(max(md, 0.0), n_modes),
    )

# ============================================================
# SOLVER M7
# ============================================================

def solve_UN_M7(p: UNParameters, keep_history: bool = True):
    modes_c = initialize_modes_from_average(p.c0, p.n_modes)
    modes_mb = initialize_modes_from_average(p.mb0, p.n_modes)
    modes_md = initialize_modes_from_average(p.md0, p.n_modes)

    R_b = p.R_b
    R_d = p.R_d
    N_b = p.N_b
    N_d = p.N_d
    V_b = sphere_volume(R_b)
    V_d = sphere_volume(R_d)

    nvb = initialize_vacancy_concentration(p, N_b, R_b, p.mb0) if p.nvb0 is None else p.nvb0
    nvd = initialize_vacancy_concentration(p, N_d, R_d, p.md0) if p.nvd0 is None else p.nvd0

    beta = beta_production(p)
    initial_gas = p.c0 + p.mb0 + p.md0
    generated = 0.0
    q_gb = 0.0
    retained = initial_gas
    t = 0.0

    capture_fraction_sum = 0.0
    capture_raw_sum = 0.0
    capture_bubbles_cumulative = 0.0
    max_f_cap_step = 0.0

    hist_keys = [
        "time", "burnup_percent_fima", "c", "mb", "md", "Nb", "Nd", "Vb", "Vd", "Rb", "Rd",
        "nvb", "nvd", "generated", "retained", "q_gb", "swelling_b", "swelling_d", "swelling_ig",
        "p_b", "p_d", "p_b_eq", "p_d_eq", "lambda_d", "nu_b", "phi_b", "phi_d",
        "f_cap_step", "cap_raw_step", "capture_fraction_sum", "capture_raw_sum",
        "capture_bubbles_cumulative", "max_f_cap_step",
        "matrix_gas_percent", "bulk_gas_percent", "dislocation_gas_percent", "qgb_gas_percent",
    ]
    hist = {key: [] for key in hist_keys}

    def append_state(nu_b=0.0, phi_b=0.0, phi_d=0.0, lambda_d=0.0, fcap=0.0, capraw=0.0):
        if not keep_history:
            return
        c_av = reconstruct_average(modes_c)
        mb_av = reconstruct_average(modes_mb)
        md_av = reconstruct_average(modes_md)
        p_b = pressure_internal(p, mb_av, nvb)
        p_d = pressure_internal(p, md_av, nvd)
        p_b_eq = pressure_equilibrium(p, R_b)
        p_d_eq = pressure_equilibrium(p, R_d)

        hist["time"].append(t)
        hist["burnup_percent_fima"].append(time_to_burnup_percent(t, p.fission_rate, p.lattice_parameter))
        hist["c"].append(c_av)
        hist["mb"].append(mb_av)
        hist["md"].append(md_av)
        hist["Nb"].append(N_b)
        hist["Nd"].append(N_d)
        hist["Vb"].append(V_b)
        hist["Vd"].append(V_d)
        hist["Rb"].append(R_b)
        hist["Rd"].append(R_d)
        hist["nvb"].append(nvb)
        hist["nvd"].append(nvd)
        hist["generated"].append(generated)
        hist["retained"].append(retained)
        hist["q_gb"].append(q_gb)
        hist["swelling_b"].append(N_b * V_b)
        hist["swelling_d"].append(N_d * V_d)
        hist["swelling_ig"].append(N_b * V_b + N_d * V_d)
        hist["p_b"].append(p_b)
        hist["p_d"].append(p_d)
        hist["p_b_eq"].append(p_b_eq)
        hist["p_d_eq"].append(p_d_eq)
        hist["lambda_d"].append(lambda_d)
        hist["nu_b"].append(nu_b)
        hist["phi_b"].append(phi_b)
        hist["phi_d"].append(phi_d)
        hist["f_cap_step"].append(fcap)
        hist["cap_raw_step"].append(capraw)
        hist["capture_fraction_sum"].append(capture_fraction_sum)
        hist["capture_raw_sum"].append(capture_raw_sum)
        hist["capture_bubbles_cumulative"].append(capture_bubbles_cumulative)
        hist["max_f_cap_step"].append(max_f_cap_step)
        hist["matrix_gas_percent"].append(100.0 * c_av / generated if generated > 0.0 else 0.0)
        hist["bulk_gas_percent"].append(100.0 * mb_av / generated if generated > 0.0 else 0.0)
        hist["dislocation_gas_percent"].append(100.0 * md_av / generated if generated > 0.0 else 0.0)
        hist["qgb_gas_percent"].append(100.0 * q_gb / generated if generated > 0.0 else 0.0)

    append_state()
    last_rates = {}
    n_steps = int(math.ceil(p.final_time / p.dt))

    for _ in range(n_steps):
        dt = min(p.dt, p.final_time - t)
        if dt <= 0.0:
            break

        c_old = reconstruct_average(modes_c)
        mb_old = reconstruct_average(modes_mb)
        md_old = reconstruct_average(modes_md)

        Nb_old = N_b
        Nd_old = N_d
        Vd_old = V_d
        Rb_old = R_b
        Rd_old = R_d
        Vcap_old = sphere_volume(Rb_old + Rd_old)

        Dg, D_parts = xe_diffusivity_UN(p)
        Dv, Dv_parts = vacancy_diffusivity_UN(p)
        b_b, b_d = resolution_rates_UN(p, R_b, R_d)
        g_b, g_d, trapping_parts = trapping_rates_UN(p, Dg, R_b, Nb_old, R_d, Nd_old)

        nu_b = nucleation_rate_bulk(p, Dg, c_old)
        phi_b = phi_population(mb_old, Nb_old)
        phi_d = phi_population(md_old, Nd_old)

        if USE_PHI_GAS_RESOLUTION:
            b_b_gas = b_b * phi_b
            b_d_gas = b_d * phi_d
        else:
            b_b_gas = b_b
            b_d_gas = b_d

        # N_b destruction uses original b_b * phi_b term.
        N_b = (Nb_old + dt * nu_b) / (1.0 + dt * b_b * phi_b)
        N_b = max(N_b, p.min_number_density)

        # Optional M7 gas mass coupling of bulk nucleation.
        if USE_NUCLEATION_MASS_COUPLING:
            source_c = beta - 2.0 * nu_b
            source_mb = 2.0 * nu_b
        else:
            source_c = beta
            source_mb = 0.0
        source_md = 0.0

        c_new, mb_new, md_new = sciantix_3x3_exchange_step(
            modes_c, modes_mb, modes_md,
            Dg, p.grain_radius,
            source_c, source_mb, source_md,
            g_b, g_d,
            b_b_gas, b_d_gas,
            dt,
        )

        if c_new < 0.0 or mb_new < 0.0 or md_new < 0.0:
            c_new = max(c_new, 0.0)
            mb_new = max(mb_new, 0.0)
            md_new = max(md_new, 0.0)
            modes_c, modes_mb, modes_md = reset_modes_to_averages(c_new, mb_new, md_new, p.n_modes)

        dmb_dt = (mb_new - mb_old) / dt
        dmd_dt = (md_new - md_old) / dt

        if p.update_bulk_vacancies:
            nvb, dnvb_dt = vacancy_concentration_implicit_step(p, Dv, R_b, N_b, mb_new, nvb, dt)
        else:
            dnvb_dt = 0.0
        nvd, dnvd_dt = vacancy_concentration_implicit_step(p, Dv, R_d, Nd_old, md_new, nvd, dt)

        if N_b > 0.0:
            V_b_growth = V_b + dt * (p.omega_fg / N_b * dmb_dt + omega_matrix(p) / N_b * dnvb_dt)
            V_b_growth = max(V_b_growth, p.min_volume)
        else:
            V_b_growth = 0.0

        if Nd_old > 0.0:
            dVd_growth_dt = p.omega_fg / Nd_old * dmd_dt + omega_matrix(p) / Nd_old * dnvd_dt
            V_d_growth = max(V_d + dt * dVd_growth_dt, p.min_volume)
        else:
            dVd_growth_dt = 0.0
            V_d_growth = 0.0

        # Dislocation-bubble coalescence: ON. Scale can vary but is never zero in candidate ranges.
        lambda_d = coalescence_lambda(Vd_old, Nd_old)
        dVd_positive = max(V_d_growth - Vd_old, 0.0)
        if dVd_positive > 0.0 and Nd_old > 0.0:
            denominator = 1.0 + p.coalescence_d_scale * 4.0 * lambda_d * Nd_old * dVd_positive
            N_d = Nd_old / denominator
        else:
            N_d = Nd_old
        N_d = max(N_d, p.min_number_density)

        # Reconstruct volumes after gas/vacancy update and dislocation coalescence.
        V_b = (p.omega_fg * max(mb_new, 0.0) + omega_matrix(p) * nvb) / N_b if N_b > 0.0 else 0.0
        V_d = (p.omega_fg * max(md_new, 0.0) + omega_matrix(p) * nvd) / N_d if N_d > 0.0 else 0.0
        V_b = max(V_b, p.min_volume)
        V_d = max(V_d, p.min_volume)
        R_b = radius_from_volume(V_b)
        R_d = radius_from_volume(V_d)

        # Bulk-dislocation capture: Barani/Rizk-like linear clipped step.
        Vcap_new = sphere_volume(R_b + R_d)
        delta_Vcap = max(Vcap_new - Vcap_old, 0.0)
        if USE_BULK_DISLOCATION_CAPTURE:
            cap_raw_step = p.capture_scale * N_d * delta_Vcap
            f_cap = max(0.0, min(cap_raw_step, 1.0))
        else:
            cap_raw_step = 0.0
            f_cap = 0.0

        capture_raw_sum += cap_raw_step
        capture_fraction_sum += f_cap
        max_f_cap_step = max(max_f_cap_step, f_cap)

        if f_cap > 0.0 and N_b > 0.0:
            mb_before = max(mb_new, 0.0)
            nvb_before = max(nvb, 0.0)
            captured_bubbles = f_cap * N_b

            mb_new = (1.0 - f_cap) * mb_before
            md_new = max(md_new, 0.0) + f_cap * mb_before
            nvb = (1.0 - f_cap) * nvb_before
            nvd = max(nvd, 0.0) + f_cap * nvb_before
            N_b = (1.0 - f_cap) * N_b

            capture_bubbles_cumulative += captured_bubbles

            modes_c, modes_mb, modes_md = reset_modes_to_averages(c_new, mb_new, md_new, p.n_modes)

            V_b = (p.omega_fg * max(mb_new, 0.0) + omega_matrix(p) * nvb) / N_b if N_b > 0.0 else 0.0
            V_d = (p.omega_fg * max(md_new, 0.0) + omega_matrix(p) * nvd) / N_d if N_d > 0.0 else 0.0
            V_b = max(V_b, p.min_volume)
            V_d = max(V_d, p.min_volume)
            R_b = radius_from_volume(V_b)
            R_d = radius_from_volume(V_d)

        generated += beta * dt
        retained = max(c_new, 0.0) + max(mb_new, 0.0) + max(md_new, 0.0)
        q_gb = max(initial_gas + generated - retained, 0.0)
        t += dt

        last_rates = {
            "Dg": Dg, "Dv": Dv, "beta": beta,
            "g_b": g_b, "g_d": g_d,
            "b_b": b_b, "b_d": b_d,
            "b_b_gas": b_b_gas, "b_d_gas": b_d_gas,
            "nu_b": nu_b, "phi_b": phi_b, "phi_d": phi_d,
            "lambda_d": lambda_d,
            "dVd_growth_dt": dVd_growth_dt,
            "dnvb_dt": dnvb_dt, "dnvd_dt": dnvd_dt,
            "f_cap_step": f_cap,
            "cap_raw_step": cap_raw_step,
            "capture_fraction_sum": capture_fraction_sum,
            "capture_raw_sum": capture_raw_sum,
            "capture_bubbles_cumulative": capture_bubbles_cumulative,
            "max_f_cap_step": max_f_cap_step,
            **D_parts, **Dv_parts, **trapping_parts,
        }

        append_state(nu_b=nu_b, phi_b=phi_b, phi_d=phi_d, lambda_d=lambda_d, fcap=f_cap, capraw=cap_raw_step)

    if not keep_history:
        c_av = reconstruct_average(modes_c)
        mb_av = reconstruct_average(modes_mb)
        md_av = reconstruct_average(modes_md)
        p_b = pressure_internal(p, mb_av, nvb)
        p_d = pressure_internal(p, md_av, nvd)
        p_b_eq = pressure_equilibrium(p, R_b)
        p_d_eq = pressure_equilibrium(p, R_d)
        hist = {
            "time": [t],
            "burnup_percent_fima": [time_to_burnup_percent(t, p.fission_rate, p.lattice_parameter)],
            "c": [c_av], "mb": [mb_av], "md": [md_av],
            "Nb": [N_b], "Nd": [N_d],
            "Vb": [V_b], "Vd": [V_d],
            "Rb": [R_b], "Rd": [R_d],
            "nvb": [nvb], "nvd": [nvd],
            "generated": [generated],
            "retained": [retained],
            "q_gb": [q_gb],
            "swelling_b": [N_b * V_b],
            "swelling_d": [N_d * V_d],
            "swelling_ig": [N_b * V_b + N_d * V_d],
            "p_b": [p_b], "p_d": [p_d],
            "p_b_eq": [p_b_eq], "p_d_eq": [p_d_eq],
            "lambda_d": [last_rates.get("lambda_d", 0.0)],
            "nu_b": [last_rates.get("nu_b", 0.0)],
            "phi_b": [last_rates.get("phi_b", 0.0)],
            "phi_d": [last_rates.get("phi_d", 0.0)],
            "f_cap_step": [last_rates.get("f_cap_step", 0.0)],
            "cap_raw_step": [last_rates.get("cap_raw_step", 0.0)],
            "capture_fraction_sum": [capture_fraction_sum],
            "capture_raw_sum": [capture_raw_sum],
            "capture_bubbles_cumulative": [capture_bubbles_cumulative],
            "max_f_cap_step": [max_f_cap_step],
            "matrix_gas_percent": [100.0 * c_av / generated if generated > 0.0 else 0.0],
            "bulk_gas_percent": [100.0 * mb_av / generated if generated > 0.0 else 0.0],
            "dislocation_gas_percent": [100.0 * md_av / generated if generated > 0.0 else 0.0],
            "qgb_gas_percent": [100.0 * q_gb / generated if generated > 0.0 else 0.0],
        }

    return hist, last_rates

# ============================================================
# RUN WRAPPER
# ============================================================

_RUN_CACHE: Dict[Tuple, Dict] = {}

def run_model_point(
    T: float,
    burnup: float,
    cand: Candidate,
    dt_h: float,
    n_modes: int,
    keep_history: bool = False,
):
    key = (
        round(float(T), 6), round(float(burnup), 6),
        cand.label, cand.f_n, cand.K_d, cand.rho_d, cand.fission_rate,
        cand.Dv_scale, cand.Dg_scale, cand.b_scale, cand.gb_scale, cand.gd_scale,
        cand.coalescence_d_scale, cand.capture_scale, cand.D2_xe_scale,
        MODEL_FAMILY, USE_PHI_GAS_RESOLUTION, USE_NUCLEATION_MASS_COUPLING, USE_BULK_DISLOCATION_CAPTURE,
        float(dt_h), int(n_modes), bool(keep_history),
    )
    if key in _RUN_CACHE:
        return _RUN_CACHE[key]

    p = UNParameters(
        temperature=float(T),
        fission_rate=cand.fission_rate,
        grain_radius=GRAIN_RADIUS,
        target_burnup_percent_fima=float(burnup),
        dt=float(dt_h) * 3600.0,
        n_modes=int(n_modes),
        xe_yield=XE_YIELD,
        f_n=cand.f_n,
        K_d=cand.K_d,
        rho_d=cand.rho_d,
        Dv_scale=cand.Dv_scale,
        Dg_extra_scale=cand.Dg_scale,
        D2_xe_scale=cand.D2_xe_scale,
        b_scale=cand.b_scale,
        gb_scale=cand.gb_scale,
        gd_scale=cand.gd_scale,
        coalescence_d_scale=cand.coalescence_d_scale,
        capture_scale=cand.capture_scale,
        bulk_seed_radius_nm=0.0,
    )

    hist, rates = solve_UN_M7(p, keep_history=keep_history)

    pb_eq = hist["p_b_eq"][-1]
    pd_eq = hist["p_d_eq"][-1]

    row = {
        "T": float(T),
        "burnup": float(burnup),
        "swelling_b_percent": 100.0 * hist["swelling_b"][-1],
        "swelling_d_percent": 100.0 * hist["swelling_d"][-1],
        "swelling_ig_percent": 100.0 * hist["swelling_ig"][-1],
        "Nb": hist["Nb"][-1],
        "Nd": hist["Nd"][-1],
        "Rb_nm": hist["Rb"][-1] * 1.0e9,
        "Rd_nm": hist["Rd"][-1] * 1.0e9,
        "p_b": hist["p_b"][-1],
        "p_b_eq": pb_eq,
        "p_d": hist["p_d"][-1],
        "p_d_eq": pd_eq,
        "p_b_over_eq": hist["p_b"][-1] / pb_eq if pb_eq > 0.0 else math.nan,
        "p_d_over_eq": hist["p_d"][-1] / pd_eq if pd_eq > 0.0 else math.nan,
        "matrix_gas_percent": hist["matrix_gas_percent"][-1],
        "bulk_gas_percent": hist["bulk_gas_percent"][-1],
        "dislocation_gas_percent": hist["dislocation_gas_percent"][-1],
        "qgb_gas_percent": hist["qgb_gas_percent"][-1],
        "f_cap_step_final": hist["f_cap_step"][-1],
        "cap_raw_step_final": hist["cap_raw_step"][-1],
        "capture_fraction_sum": hist["capture_fraction_sum"][-1],
        "capture_raw_sum": hist["capture_raw_sum"][-1],
        "capture_bubbles_cumulative": hist["capture_bubbles_cumulative"][-1],
        "max_f_cap_step": hist["max_f_cap_step"][-1],
        "hist": hist if keep_history else None,
        "rates": rates,
    }

    _RUN_CACHE[key] = row
    return row

# ============================================================
# SCORING
# ============================================================

def rmse(values):
    values = [v for v in values if math.isfinite(v)]
    if not values:
        return math.nan
    return math.sqrt(sum(v * v for v in values) / len(values))


def log10_rmse(pairs):
    errs = []
    for pred, exp in pairs:
        if pred > 0.0 and exp > 0.0 and math.isfinite(pred) and math.isfinite(exp):
            errs.append(math.log10(pred / exp))
    return rmse(errs)


def safe_log10_ratio(a: float, b: float) -> float:
    if a > 0.0 and b > 0.0 and math.isfinite(a) and math.isfinite(b):
        return math.log10(a / b)
    return 99.0


def candidate_to_dict(cand: Candidate) -> Dict:
    return {
        "label": cand.label,
        "f_n": cand.f_n,
        "K_d": cand.K_d,
        "rho_d": cand.rho_d,
        "fission_rate": cand.fission_rate,
        "Dv_scale": cand.Dv_scale,
        "Dg_scale": cand.Dg_scale,
        "b_scale": cand.b_scale,
        "gb_scale": cand.gb_scale,
        "gd_scale": cand.gd_scale,
        "coalescence_d_scale": cand.coalescence_d_scale,
        "capture_scale": cand.capture_scale,
        "D2_xe_scale": cand.D2_xe_scale,
        "family": MODEL_FAMILY,
    }


def fast_subset_points():
    sw_keep = {
        899.0, 1127.0, 1228.0, 1402.0, 1514.0, 1598.0, 1632.0, 1656.0,
        1044.0, 1377.0, 1534.0, 1595.0, 1639.0, 1661.0,
        984.0, 1126.0, 1343.0, 1511.0, 1557.0, 1590.0,
    }
    nd_keep = {1202.0, 1330.0, 1493.0, 1555.0, 1622.0, 1669.0, 1723.0, 1742.0}
    rd_keep = {1045.0, 1374.0, 1535.0, 1594.0, 1641.0, 1710.0, 1725.0}
    return (
        [p for p in EXP_SWELLING_T if p["T"] <= 1700.0 and p["T"] in sw_keep],
        [p for p in EXP_ND_T_13 if p["T"] in nd_keep],
        [p for p in EXP_RD_T_13 if p["T"] in rd_keep],
    )


def nd_drop_score(cand: Candidate, dt_h: float, n_modes: int) -> float:
    # This explicitly rewards the high-T decrease of dislocation bubble number density.
    # It does not change the equation; it only tells the optimizer not to ignore coalescence.
    nd1400 = run_model_point(1400.0, 1.3, cand, dt_h, n_modes, keep_history=False)["Nd"]
    nd1550 = run_model_point(1550.0, 1.3, cand, dt_h, n_modes, keep_history=False)["Nd"]
    nd1650 = run_model_point(1650.0, 1.3, cand, dt_h, n_modes, keep_history=False)["Nd"]
    nd1725 = run_model_point(1725.0, 1.3, cand, dt_h, n_modes, keep_history=False)["Nd"]

    d1650_1400 = safe_log10_ratio(nd1650, nd1400)
    d1725_1400 = safe_log10_ratio(nd1725, nd1400)
    d1725_1550 = safe_log10_ratio(nd1725, nd1550)

    # Desired qualitative behaviour from digitized points: drop by high T.
    # Penalize primarily "not enough drop". Excessive drop is already controlled by log-RMSE Nd.
    terms = [
        max(0.0, d1650_1400 + 0.25),  # want <= -0.25 decade by 1650 K
        max(0.0, d1725_1400 + 0.80),  # want <= -0.80 decade by 1725 K
        max(0.0, d1725_1550 + 0.45),  # want <= -0.45 decade from 1550 to 1725 K
    ]

    # Small monotonicity guard above 1500 K.
    if nd1650 > nd1550:
        terms.append(abs(safe_log10_ratio(nd1650, nd1550)))
    if nd1725 > nd1650:
        terms.append(abs(safe_log10_ratio(nd1725, nd1650)))

    return rmse(terms)


def pressure_score(cand: Candidate, dt_h: float, n_modes: int) -> float:
    if W_PRESSURE <= 0.0:
        return 0.0

    # Rizk-like sanity: bulk and especially dislocation bubbles should not stay
    # orders of magnitude away from mechanical equilibrium. We allow a free band
    # around equilibrium because pressure is not a direct experimental target.
    free = math.log10(PRESSURE_FREE_FACTOR)
    terms = []
    for T in [1200.0, 1400.0, 1600.0, 1700.0, 1800.0]:
        out = run_model_point(T, 3.2, cand, dt_h, n_modes, keep_history=False)
        for key, mult in [("p_b_over_eq", 0.8), ("p_d_over_eq", 1.6)]:
            val = out[key]
            if val > 0.0 and math.isfinite(val):
                raw = abs(math.log10(max(val, 1.0e-30)))
                err = max(0.0, raw - free)
                terms.append(mult * min(err, 2.5))
            else:
                terms.append(2.5 * mult)
    return rmse(terms)


def rizk_gas_shape_score(cand: Candidate, dt_h: float, n_modes: int) -> float:
    # Weak qualitative check inspired by Rizk Fig. 9: at low T bulk gas is high, then it transfers to dislocation gas.
    # This must NOT dominate the experimental fit.
    vals = {}
    for T in [1200.0, 1600.0, 1800.0, 2000.0]:
        vals[T] = run_model_point(T, 1.1, cand, dt_h, n_modes, keep_history=False)

    terms = []
    # Low-T bulk should not be almost zero.
    terms.append(max(0.0, 45.0 - vals[1200.0]["bulk_gas_percent"]) / 100.0)
    # By 2000 K bulk should mostly vanish.
    terms.append(max(0.0, vals[2000.0]["bulk_gas_percent"] - 15.0) / 100.0)
    # Dislocation gas should increase between 1600 and 2000 K.
    terms.append(max(0.0, vals[1600.0]["dislocation_gas_percent"] - vals[2000.0]["dislocation_gas_percent"]) / 100.0)
    return rmse(terms)


def highT_guard_score(cand: Candidate, dt_h: float, n_modes: int) -> float:
    # Very light guard against absurd divergence. We do not fit high-T swelling directly.
    terms = []
    for T in [1800.0, 2000.0]:
        out32 = run_model_point(T, 3.2, cand, dt_h, n_modes, keep_history=False)
        out13 = run_model_point(T, 1.3, cand, dt_h, n_modes, keep_history=False)
        terms.append(max(0.0, out32["swelling_d_percent"] - 60.0) / 60.0)
        terms.append(max(0.0, out13["Rd_nm"] - 600.0) / 600.0)
    return rmse(terms)


def score_candidate(cand: Candidate, dt_h: float, n_modes: int, use_full_exp: bool = True) -> Dict:
    if use_full_exp:
        sw_points = [p for p in EXP_SWELLING_T if p["T"] <= 1700.0]
        nd_points = EXP_ND_T_13
        rd_points = EXP_RD_T_13
    else:
        sw_points, nd_points, rd_points = fast_subset_points()

    sw_errs = []
    sw_ig_errs = []
    for exp in sw_points:
        out = run_model_point(exp["T"], exp["burnup"], cand, dt_h, n_modes, keep_history=False)
        sw_errs.append(out["swelling_d_percent"] - exp["swelling"])
        sw_ig_errs.append(out["swelling_ig_percent"] - exp["swelling"])

    score_swd = rmse(sw_errs)
    score_swig_diag = rmse(sw_ig_errs)

    n_pairs = []
    for exp in nd_points:
        out = run_model_point(exp["T"], 1.3, cand, dt_h, n_modes, keep_history=False)
        n_pairs.append((out["Nd"], exp["N"]))
    score_Nd = log10_rmse(n_pairs)

    r_pairs = []
    for exp in rd_points:
        out = run_model_point(exp["T"], 1.3, cand, dt_h, n_modes, keep_history=False)
        r_pairs.append((out["Rd_nm"], exp["R_nm"]))
    score_Rd = log10_rmse(r_pairs)

    score_Nd_drop = nd_drop_score(cand, dt_h, n_modes)
    score_pressure = pressure_score(cand, dt_h, n_modes)
    score_rizk_gas_shape = rizk_gas_shape_score(cand, dt_h, n_modes)
    score_highT_guard = highT_guard_score(cand, dt_h, n_modes)
    score_fdot_prior = abs(math.log10(cand.fission_rate / FISSION_RATE_NOMINAL))

    total = (
        W_SWELLING * score_swd
        + W_RD * score_Rd
        + W_ND_LEVEL * score_Nd
        + W_ND_DROP * score_Nd_drop
        + W_PRESSURE * score_pressure
        + W_FDOT_PRIOR * score_fdot_prior
        + W_RIZK_GAS_SHAPE * score_rizk_gas_shape
        + W_HIGHT_GUARD * score_highT_guard
    )

    out1600 = run_model_point(1600.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    out32_1600 = run_model_point(1600.0, 3.2, cand, dt_h, n_modes, keep_history=False)
    outNd1400 = run_model_point(1400.0, 1.3, cand, dt_h, n_modes, keep_history=False)
    outNd1725 = run_model_point(1725.0, 1.3, cand, dt_h, n_modes, keep_history=False)

    result = {
        **candidate_to_dict(cand),
        "dt_h": dt_h,
        "n_modes": n_modes,
        "use_full_exp": int(use_full_exp),
        "score_total": total,
        "score_swd": score_swd,
        "score_swig_diag": score_swig_diag,
        "score_Nd": score_Nd,
        "score_Rd": score_Rd,
        "score_Nd_drop": score_Nd_drop,
        "score_pressure": score_pressure,
        "score_fdot_prior": score_fdot_prior,
        "score_rizk_gas_shape": score_rizk_gas_shape,
        "score_highT_guard": score_highT_guard,
        "swD_1p3_1600K": out1600["swelling_d_percent"],
        "swB_1p3_1600K": out1600["swelling_b_percent"],
        "Rd_1p3_1600K": out1600["Rd_nm"],
        "Nd_1p3_1600K": out1600["Nd"],
        "Nd_1p3_1400K": outNd1400["Nd"],
        "Nd_1p3_1725K": outNd1725["Nd"],
        "Nd_drop_1725_over_1400_log10": safe_log10_ratio(outNd1725["Nd"], outNd1400["Nd"]),
        "swD_3p2_1600K": out32_1600["swelling_d_percent"],
        "swB_3p2_1600K": out32_1600["swelling_b_percent"],
        "max_f_cap_step_3p2_1600K": out32_1600["max_f_cap_step"],
        "capture_fraction_sum_3p2_1600K": out32_1600["capture_fraction_sum"],
        "capture_raw_sum_3p2_1600K": out32_1600["capture_raw_sum"],
    }
    return result

# ============================================================
# LATIN-HYPERCUBE-LIKE CANDIDATES
# ============================================================

def scale_unit_to_range(u: float, low: float, high: float, scale: str) -> float:
    u = max(0.0, min(1.0, u))
    if scale == "log":
        return 10.0 ** (math.log10(low) + u * (math.log10(high) - math.log10(low)))
    if scale == "linear":
        return low + u * (high - low)
    raise ValueError(f"Unknown scale: {scale}")


def lhs_unit_samples(n: int, d: int, seed: int) -> List[List[float]]:
    rng = random.Random(seed)
    samples = [[0.0 for _ in range(d)] for __ in range(n)]
    for j in range(d):
        bins = list(range(n))
        rng.shuffle(bins)
        for i in range(n):
            samples[i][j] = (bins[i] + rng.random()) / n
    rng.shuffle(samples)
    return samples


def make_candidate_from_values(label: str, vals: Dict[str, float]) -> Candidate:
    return Candidate(
        label=label,
        f_n=float(vals["f_n"]),
        K_d=float(vals["K_d"]),
        rho_d=float(vals["rho_d"]),
        fission_rate=float(vals["fission_rate"]),
        Dv_scale=float(vals["Dv_scale"]),
        Dg_scale=float(vals["Dg_scale"]),
        b_scale=float(vals["b_scale"]),
        gb_scale=float(vals["gb_scale"]),
        gd_scale=float(vals["gd_scale"]),
        coalescence_d_scale=float(vals["coalescence_d_scale"]),
        capture_scale=float(vals["capture_scale"]),
        D2_xe_scale=float(vals.get("D2_xe_scale", 1.0)),
    )


def generate_global_candidates(n: int = N_GLOBAL_FAST, seed: int = RANDOM_SEED) -> List[Candidate]:
    anchors = [
        # Nominal Rizk-like case
        {
            "label": "ANCHOR_nominal",
            "f_n": 1.0e-6, "K_d": 5.0e5, "rho_d": 3.0e13, "fission_rate": 5.0e19,
            "Dv_scale": 1.0, "Dg_scale": 1.0, "b_scale": 1.0, "gb_scale": 1.0,
            "gd_scale": 1.0, "coalescence_d_scale": 1.0, "capture_scale": 1.0,
        },
        # Previous physically promising region, but fission rate kept nominal.
        {
            "label": "ANCHOR_prev_region_nomFdot",
            "f_n": 3.0e-7, "K_d": 3.0e5, "rho_d": 3.0e13, "fission_rate": 5.0e19,
            "Dv_scale": 0.3, "Dg_scale": 1.0, "b_scale": 1.0, "gb_scale": 1.0,
            "gd_scale": 1.0, "coalescence_d_scale": 1.0, "capture_scale": 1.0,
        },
        # Stronger dislocation feeding and coalescence: tests tutor's Nd-drop concern.
        {
            "label": "ANCHOR_high_gd_coal",
            "f_n": 1.0e-7, "K_d": 3.0e5, "rho_d": 3.0e13, "fission_rate": 5.0e19,
            "Dv_scale": 0.5, "Dg_scale": 1.0, "b_scale": 1.0, "gb_scale": 0.8,
            "gd_scale": 2.5, "coalescence_d_scale": 4.0, "capture_scale": 1.0,
        },
        # Stronger re-solution + coalescence.
        {
            "label": "ANCHOR_high_b_coal",
            "f_n": 1.0e-7, "K_d": 3.0e5, "rho_d": 3.0e13, "fission_rate": 5.0e19,
            "Dv_scale": 0.5, "Dg_scale": 1.0, "b_scale": 2.5, "gb_scale": 1.0,
            "gd_scale": 1.5, "coalescence_d_scale": 4.0, "capture_scale": 1.0,
        },
        # More bulk feeding, tests bulk gas peak / capture path.
        {
            "label": "ANCHOR_high_gb_capture",
            "f_n": 3.0e-7, "K_d": 3.0e5, "rho_d": 3.0e13, "fission_rate": 5.0e19,
            "Dv_scale": 0.5, "Dg_scale": 1.0, "b_scale": 1.0, "gb_scale": 2.5,
            "gd_scale": 1.0, "coalescence_d_scale": 2.5, "capture_scale": 2.0,
        },
        # Slightly high fission rate, but still within narrow band.
        {
            "label": "ANCHOR_slight_high_Fdot",
            "f_n": 3.0e-7, "K_d": 3.0e5, "rho_d": 3.0e13, "fission_rate": 6.0e19,
            "Dv_scale": 0.3, "Dg_scale": 1.0, "b_scale": 1.0, "gb_scale": 1.0,
            "gd_scale": 1.0, "coalescence_d_scale": 1.0, "capture_scale": 1.0,
        },
    ]

    candidates: List[Candidate] = []
    seen = set()
    for a in anchors:
        label = a.pop("label")
        cand = make_candidate_from_values(label, a)
        key = tuple(round(getattr(cand, name), 12) for name in PARAM_NAMES)
        if key not in seen:
            candidates.append(cand)
            seen.add(key)

    n_lhs = max(0, n - len(candidates))
    samples = lhs_unit_samples(n_lhs, len(PARAM_NAMES), seed)
    for i, uvec in enumerate(samples):
        vals = {}
        for name, u in zip(PARAM_NAMES, uvec):
            low, high, scale = PARAM_RANGES[name]
            vals[name] = scale_unit_to_range(u, low, high, scale)
        cand = make_candidate_from_values(f"LHS_{i:04d}", vals)
        candidates.append(cand)

    return candidates

# ============================================================
# CSV UTILITIES
# ============================================================

CSV_ORDER = [
    "label", "score_total", "score_swd", "score_Nd", "score_Nd_drop", "score_Rd", "score_pressure",
    "score_fdot_prior", "score_rizk_gas_shape", "score_highT_guard",
    "f_n", "K_d", "rho_d", "fission_rate", "Dv_scale", "Dg_scale", "b_scale", "gb_scale", "gd_scale",
    "coalescence_d_scale", "capture_scale",
    "swD_1p3_1600K", "swB_1p3_1600K", "Rd_1p3_1600K", "Nd_1p3_1600K",
    "Nd_1p3_1400K", "Nd_1p3_1725K", "Nd_drop_1725_over_1400_log10",
    "swD_3p2_1600K", "swB_3p2_1600K", "max_f_cap_step_3p2_1600K",
    "capture_fraction_sum_3p2_1600K", "capture_raw_sum_3p2_1600K",
    "dt_h", "n_modes", "use_full_exp", "error",
]


def safe_float(x, default=math.inf):
    try:
        if x is None or x == "":
            return default
        return float(x)
    except Exception:
        return default


def row_score(row: Dict) -> float:
    return safe_float(row.get("score_total"), math.inf)


def write_csv(path: str, rows: List[Dict]):
    if not rows:
        return
    all_keys = []
    for k in CSV_ORDER:
        if any(k in r for r in rows):
            all_keys.append(k)
    for r in rows:
        for k in r.keys():
            if k not in all_keys and k not in ("hist", "rates"):
                all_keys.append(k)
    with open(path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=all_keys, extrasaction="ignore")
        writer.writeheader()
        for r in rows:
            clean = {k: v for k, v in r.items() if k not in ("hist", "rates")}
            writer.writerow(clean)


def read_csv_rows(path: str) -> List[Dict]:
    if not os.path.exists(path):
        return []
    with open(path, "r", newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def candidate_from_score_row(row: Dict, label_prefix: str = "best_M7") -> Candidate:
    return Candidate(
        label=f"{label_prefix}_{row['label']}",
        f_n=float(row["f_n"]),
        K_d=float(row["K_d"]),
        rho_d=float(row["rho_d"]),
        fission_rate=float(row["fission_rate"]),
        Dv_scale=float(row["Dv_scale"]),
        Dg_scale=float(row["Dg_scale"]),
        b_scale=float(row["b_scale"]),
        gb_scale=float(row["gb_scale"]),
        gd_scale=float(row["gd_scale"]),
        coalescence_d_scale=float(row["coalescence_d_scale"]),
        capture_scale=float(row["capture_scale"]),
        D2_xe_scale=float(row.get("D2_xe_scale", 1.0)),
    )

# ============================================================
# SWEEP
# ============================================================

def print_best_table(rows: List[Dict], n: int = 12):
    rows = sorted(rows, key=row_score)[:n]
    print("-" * 170)
    print(
        f"{'rank':>4s} | {'score':>9s} | {'label':>28s} | {'f_n':>9s} | {'K_d':>9s} | {'rho_d':>9s} | "
        f"{'Fdot':>9s} | {'Dv':>5s} | {'Dg':>5s} | {'b':>5s} | {'gb':>5s} | {'gd':>5s} | {'coal':>5s} | {'cap':>5s} | "
        f"{'swD1600':>8s} | {'Rd1600':>8s} | {'NdDrop':>8s}"
    )
    print("-" * 170)
    for i, r in enumerate(rows, start=1):
        print(
            f"{i:4d} | {row_score(r):9.4g} | {str(r.get('label')):>28s} | "
            f"{safe_float(r.get('f_n'), math.nan):9.1e} | {safe_float(r.get('K_d'), math.nan):9.1e} | "
            f"{safe_float(r.get('rho_d'), math.nan):9.1e} | {safe_float(r.get('fission_rate'), math.nan):9.1e} | "
            f"{safe_float(r.get('Dv_scale'), math.nan):5.2g} | {safe_float(r.get('Dg_scale'), math.nan):5.2g} | "
            f"{safe_float(r.get('b_scale'), math.nan):5.2g} | {safe_float(r.get('gb_scale'), math.nan):5.2g} | "
            f"{safe_float(r.get('gd_scale'), math.nan):5.2g} | {safe_float(r.get('coalescence_d_scale'), math.nan):5.2g} | "
            f"{safe_float(r.get('capture_scale'), math.nan):5.2g} | "
            f"{safe_float(r.get('swD_1p3_1600K'), math.nan):8.3g} | {safe_float(r.get('Rd_1p3_1600K'), math.nan):8.3g} | "
            f"{safe_float(r.get('Nd_drop_1725_over_1400_log10'), math.nan):8.3g}"
        )
    print("-" * 170)


def run_fast_sweep(n_global: int = N_GLOBAL_FAST, use_full_exp: bool = False) -> List[Dict]:
    candidates = generate_global_candidates(n_global, RANDOM_SEED)
    partial_path = os.path.join(OUTPUT_DIR, "global_fast_partial.csv")
    all_path = os.path.join(OUTPUT_DIR, "global_fast_all.csv")

    rows: List[Dict] = []
    done_labels = set()
    if RESUME_FAST:
        existing = read_csv_rows(all_path) or read_csv_rows(partial_path)
        if existing:
            rows.extend(existing)
            done_labels = {r.get("label") for r in existing if r.get("label")}

    print("=" * 140)
    print("START M7 GLOBAL COMPACT FAST SWEEP")
    print("=" * 140)
    print(f"Output dir           = {OUTPUT_DIR}")
    print(f"Candidates total     = {len(candidates)}")
    print(f"Already done         = {len(done_labels)}")
    print(f"FAST dt_h/n_modes    = {FAST_DT_H:g} h / {FAST_N_MODES}")
    print(f"Score points         = {'full digitized datasets' if use_full_exp else 'representative subset'}")
    print(f"Pressure weight      = {W_PRESSURE:g} (weak sanity term; set 0 to disable)")
    print("Coalescence          = always ON, scale sampled but never zero")
    print("Capture law          = linear clipped Barani/Rizk-like; no exponential replacement")
    print("=" * 140)

    for i, cand in enumerate(candidates, start=1):
        if cand.label in done_labels:
            continue
        try:
            score = score_candidate(cand, FAST_DT_H, FAST_N_MODES, use_full_exp=use_full_exp)
        except Exception as e:
            score = {**candidate_to_dict(cand), "error": repr(e), "score_total": math.inf}
        rows.append(score)

        if len(rows) % 3 == 0 or i == len(candidates):
            rows_sorted = sorted(rows, key=row_score)
            write_csv(partial_path, rows_sorted)
            best = rows_sorted[0]
            print(
                f"[{i:4d}/{len(candidates)}] best={row_score(best):.5g} label={best.get('label')} "
                f"f_n={safe_float(best.get('f_n'), math.nan):.2e} K_d={safe_float(best.get('K_d'), math.nan):.2e} "
                f"rho_d={safe_float(best.get('rho_d'), math.nan):.2e} Fdot={safe_float(best.get('fission_rate'), math.nan):.2e} "
                f"Dv={safe_float(best.get('Dv_scale'), math.nan):.2g} Dg={safe_float(best.get('Dg_scale'), math.nan):.2g} "
                f"b={safe_float(best.get('b_scale'), math.nan):.2g} gb={safe_float(best.get('gb_scale'), math.nan):.2g} "
                f"gd={safe_float(best.get('gd_scale'), math.nan):.2g} coal={safe_float(best.get('coalescence_d_scale'), math.nan):.2g} "
                f"cap={safe_float(best.get('capture_scale'), math.nan):.2g}"
            )

    rows_sorted = sorted(rows, key=row_score)
    write_csv(all_path, rows_sorted)
    print("\nFAST sweep complete. Best candidates:")
    print_best_table(rows_sorted[:12])
    return rows_sorted


def run_final_from_fast(fast_rows: List[Dict], n_top: int = N_TOP_FINAL) -> List[Dict]:
    valid_fast = [r for r in sorted(fast_rows, key=row_score) if math.isfinite(row_score(r))]
    final_path = os.path.join(OUTPUT_DIR, "global_final_top.csv")

    print("\n" + "=" * 140)
    print("START M7 GLOBAL FINAL CHECK")
    print("=" * 140)
    print(f"Top from FAST        = {min(n_top, len(valid_fast))}")
    print(f"FINAL dt_h/n_modes   = {FINAL_DT_H:g} h / {FINAL_N_MODES}")
    print("Score points         = full digitized datasets")
    print("=" * 140)

    final_rows = []
    for rank, row in enumerate(valid_fast[:n_top], start=1):
        cand = candidate_from_score_row(row, label_prefix=f"FINAL_rank{rank}")
        try:
            score = score_candidate(cand, FINAL_DT_H, FINAL_N_MODES, use_full_exp=True)
        except Exception as e:
            score = {**candidate_to_dict(cand), "error": repr(e), "score_total": math.inf}
        final_rows.append(score)
        write_csv(final_path, sorted(final_rows, key=row_score))
        print(
            f"FINAL {rank:02d}: score={row_score(score):.6g}, old={row.get('label')}, "
            f"f_n={cand.f_n:g}, K_d={cand.K_d:g}, rho_d={cand.rho_d:g}, Fdot={cand.fission_rate:g}, "
            f"Dv={cand.Dv_scale:g}, Dg={cand.Dg_scale:g}, b={cand.b_scale:g}, gb={cand.gb_scale:g}, "
            f"gd={cand.gd_scale:g}, coal={cand.coalescence_d_scale:g}, cap={cand.capture_scale:g}"
        )

    final_rows = sorted(final_rows, key=row_score)
    write_csv(final_path, final_rows)
    print("\nFINAL check complete. Best candidates:")
    print_best_table(final_rows, n=len(final_rows))
    write_summary(final_rows)
    return final_rows

# ============================================================
# PLOTTING AND SUMMARY
# ============================================================

def _maybe_savefig(name: str):
    if SAVE_FIGS and plt is not None:
        plt.savefig(os.path.join(OUTPUT_DIR, name), dpi=180, bbox_inches="tight")


def _show_or_close():
    if plt is None:
        return
    if SHOW_PLOTS:
        plt.show()
    else:
        plt.close()


def plot_exp_swelling_for_burnup(bu: float):
    if plt is None:
        return
    series_names = sorted({p["series"] for p in EXP_SWELLING_T if abs(p["burnup"] - bu) < 1.0e-9})
    for series in series_names:
        pts = [p for p in EXP_SWELLING_T if abs(p["burnup"] - bu) < 1.0e-9 and p["series"] == series]
        marker = "x" if "119" in series else "^"
        plt.scatter([p["T"] for p in pts], [p["swelling"] for p in pts], marker=marker, s=65, label=f"Exp P2 {series}", zorder=5)


def make_diagnostics_for_candidate(cand: Candidate, dt_h: float = FINAL_DT_H, n_modes: int = FINAL_N_MODES, prefix: Optional[str] = None):
    if plt is None:
        print("matplotlib not available: plots skipped.")
        return {}

    prefix = prefix or cand.label
    print("\n" + "=" * 140)
    print(f"DIAGNOSTICS FOR {prefix}")
    print("=" * 140)
    print(candidate_to_dict(cand))
    print("=" * 140)

    results_by_burnup: Dict[float, List[Dict]] = {}
    for bu in BURNUPS:
        rows = [run_model_point(T, bu, cand, dt_h, n_modes, keep_history=False) for T in TEMPS_MAIN]
        results_by_burnup[bu] = rows

    for bu, rows in results_by_burnup.items():
        Ts = [r["T"] for r in rows]
        plt.figure(figsize=(9, 5.8))
        plt.plot(Ts, [r["swelling_d_percent"] for r in rows], label="M7 dislocation/P2 swelling")
        plt.plot(Ts, [r["swelling_b_percent"] for r in rows], linestyle="--", label="M7 bulk swelling")
        plot_exp_swelling_for_burnup(bu)
        plt.xlabel("T [K]")
        plt.ylabel("Fission gas swelling [%]")
        plt.title(f"{prefix}: swelling vs T at {bu:.1f}% FIMA")
        plt.grid(True, alpha=0.3)
        plt.legend(fontsize=8)
        plt.tight_layout()
        _maybe_savefig(f"{prefix}_swelling_T_{bu:.1f}FIMA.png")
        _show_or_close()

    burnup_grid = [0.2, 0.5, 0.8, 1.1, 1.3, 1.6, 2.0, 2.5, 3.2, 4.0, 5.0, 6.0]
    rows_bu = [run_model_point(1600.0, bu, cand, dt_h, n_modes, keep_history=False) for bu in burnup_grid]
    plt.figure(figsize=(9, 5.8))
    plt.plot(burnup_grid, [r["swelling_d_percent"] for r in rows_bu], label="M7 dislocation/P2 swelling")
    plt.plot(burnup_grid, [r["swelling_b_percent"] for r in rows_bu], linestyle="--", label="M7 bulk swelling")
    plt.scatter([p["burnup"] for p in EXP_SWELLING_BURNUP_1600], [p["swelling"] for p in EXP_SWELLING_BURNUP_1600], marker="^", s=80, label="Exp P2 approx")
    plt.xlabel("Burnup [% FIMA]")
    plt.ylabel("Fission gas swelling [%]")
    plt.title(f"{prefix}: swelling vs burnup at 1600 K")
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=8)
    plt.tight_layout()
    _maybe_savefig(f"{prefix}_swelling_vs_burnup_1600K.png")
    _show_or_close()

    rows = results_by_burnup[1.3]
    Ts = [r["T"] for r in rows]
    plt.figure(figsize=(9, 5.8))
    plt.plot(Ts, [r["Rd_nm"] for r in rows], label="M7 dislocation R_d")
    plt.plot(Ts, [r["Rb_nm"] for r in rows], linestyle="--", label="M7 bulk R_b")
    plt.scatter([p["T"] for p in EXP_RD_T_13], [p["R_nm"] for p in EXP_RD_T_13], marker="^", s=70, label="Exp P2 / large bubbles")
    plt.yscale("log")
    plt.xlabel("T [K]")
    plt.ylabel("Bubble radius [nm]")
    plt.title(f"{prefix}: bubble radius at 1.3% FIMA")
    plt.grid(True, which="both", alpha=0.3)
    plt.legend(fontsize=8)
    plt.tight_layout()
    _maybe_savefig(f"{prefix}_radius_T_1.3FIMA.png")
    _show_or_close()

    plt.figure(figsize=(9, 5.8))
    plt.plot(Ts, [r["Nd"] for r in rows], label="M7 dislocation N_d")
    plt.plot(Ts, [r["Nb"] for r in rows], linestyle="--", label="M7 bulk N_b")
    plt.scatter([p["T"] for p in EXP_ND_T_13], [p["N"] for p in EXP_ND_T_13], marker="^", s=70, label="Exp P2 / large bubbles")
    plt.yscale("log")
    plt.xlabel("T [K]")
    plt.ylabel("Bubble concentration [m$^{-3}$]")
    plt.title(f"{prefix}: bubble concentration at 1.3% FIMA")
    plt.grid(True, which="both", alpha=0.3)
    plt.legend(fontsize=8)
    plt.tight_layout()
    _maybe_savefig(f"{prefix}_concentration_T_1.3FIMA.png")
    _show_or_close()

    rows32 = results_by_burnup[3.2]
    Ts32 = [r["T"] for r in rows32]
    plt.figure(figsize=(9, 5.8))
    plt.plot(Ts32, [r["p_b"] for r in rows32], label="Bulk pressure p_b")
    plt.plot(Ts32, [r["p_b_eq"] for r in rows32], linestyle="--", label="Bulk equilibrium p_b,eq")
    plt.plot(Ts32, [r["p_d"] for r in rows32], label="Dislocation pressure p_d")
    plt.plot(Ts32, [r["p_d_eq"] for r in rows32], linestyle="--", label="Dislocation equilibrium p_d,eq")
    plt.yscale("log")
    plt.xlabel("T [K]")
    plt.ylabel("Pressure [Pa]")
    plt.title(f"{prefix}: pressure diagnostic at 3.2% FIMA")
    plt.grid(True, which="both", alpha=0.3)
    plt.legend(fontsize=8)
    plt.tight_layout()
    _maybe_savefig(f"{prefix}_pressure_T_3.2FIMA.png")
    _show_or_close()

    plt.figure(figsize=(9, 5.8))
    plt.plot(Ts32, [r["p_b_over_eq"] for r in rows32], label="p_b / p_b,eq")
    plt.plot(Ts32, [r["p_d_over_eq"] for r in rows32], label="p_d / p_d,eq")
    plt.axhline(1.0, linestyle=":", linewidth=1)
    plt.xlabel("T [K]")
    plt.ylabel("Pressure ratio [-]")
    plt.title(f"{prefix}: pressure ratio diagnostic at 3.2% FIMA")
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=8)
    plt.tight_layout()
    _maybe_savefig(f"{prefix}_pressure_ratio_T_3.2FIMA.png")
    _show_or_close()

    for bu in [1.1, 3.2]:
        rows_gas = [run_model_point(T, bu, cand, dt_h, n_modes, keep_history=False) for T in TEMPS_GAS_PARTITION]
        Ts = [r["T"] for r in rows_gas]
        plt.figure(figsize=(9, 5.8))
        plt.plot(Ts, [r["matrix_gas_percent"] for r in rows_gas], label="Matrix")
        plt.plot(Ts, [r["bulk_gas_percent"] for r in rows_gas], linestyle="--", label="Bulk bubbles")
        plt.plot(Ts, [r["dislocation_gas_percent"] for r in rows_gas], linestyle="-.", label="Dislocation bubbles")
        plt.plot(Ts, [r["qgb_gas_percent"] for r in rows_gas], linestyle=":", label="Gas to grain face q_gb")
        plt.xlabel("T [K]")
        plt.ylabel("Amount of generated gas [%]")
        plt.title(f"{prefix}: gas partition at {bu:.1f}% FIMA")
        plt.xlim(900, 2600)
        plt.ylim(0, 100)
        plt.grid(True, alpha=0.3)
        plt.legend(fontsize=8)
        plt.tight_layout()
        _maybe_savefig(f"{prefix}_gas_partition_{bu:.1f}FIMA.png")
        _show_or_close()

    rows = rows32
    Ts = [r["T"] for r in rows]
    plt.figure(figsize=(9, 5.8))
    plt.plot(Ts, [r["max_f_cap_step"] for r in rows], label="max f_cap step, clipped fraction")
    plt.plot(Ts, [r["capture_fraction_sum"] for r in rows], label="sum clipped f_cap steps, diagnostic")
    plt.plot(Ts, [r["capture_raw_sum"] for r in rows], label="sum raw capture hazard, diagnostic")
    plt.yscale("symlog", linthresh=1e-5)
    plt.xlabel("T [K]")
    plt.ylabel("Capture diagnostic")
    plt.title(f"{prefix}: capture diagnostics at 3.2% FIMA")
    plt.grid(True, which="both", alpha=0.3)
    plt.legend(fontsize=8)
    plt.tight_layout()
    _maybe_savefig(f"{prefix}_capture_diagnostic_3.2FIMA.png")
    _show_or_close()

    return results_by_burnup


def capture_diagnostic_for_candidate(cand: Candidate, T: float = 1600.0, burnup: float = 3.2):
    out = run_model_point(T, burnup, cand, FINAL_DT_H, FINAL_N_MODES, keep_history=True)
    hist = out["hist"]
    print("\n" + "=" * 140)
    print(f"CAPTURE CHECK: {cand.label}, T={T}, burnup={burnup}")
    print("=" * 140)
    print("f_cap_step is the physical clipped per-step fraction and must be <= 1.")
    print("capture_fraction_sum can exceed 1 because it is a diagnostic sum, not a physical fraction.")
    print("-" * 140)
    print(f"final f_cap_step       = {hist['f_cap_step'][-1]:.6e}")
    print(f"max f_cap_step         = {max(hist['f_cap_step']):.6e}")
    print(f"final cap_raw_step     = {hist['cap_raw_step'][-1]:.6e}")
    print(f"capture_fraction_sum   = {hist['capture_fraction_sum'][-1]:.6e}")
    print(f"capture_raw_sum        = {hist['capture_raw_sum'][-1]:.6e}")
    print(f"captured bubbles cum.  = {hist['capture_bubbles_cumulative'][-1]:.6e} m^-3")
    print("=" * 140)
    if max(hist["f_cap_step"]) > 1.0 + 1e-12:
        raise RuntimeError("BUG: f_cap_step exceeded 1. The clipping is not working.")
    return hist


def write_summary(final_rows: List[Dict]):
    if not final_rows:
        return
    best = sorted(final_rows, key=row_score)[0]
    path = os.path.join(OUTPUT_DIR, "global_sensitivity_summary.md")
    with open(path, "w", encoding="utf-8") as f:
        f.write("# M7 global sensitivity summary\n\n")
        f.write("This run keeps M7 equations fixed. Coalescence is always ON; only its scale is varied.\n\n")
        f.write("## Score weights\n\n")
        f.write(f"- W_SWELLING = {W_SWELLING}\n")
        f.write(f"- W_RD = {W_RD}\n")
        f.write(f"- W_ND_LEVEL = {W_ND_LEVEL}\n")
        f.write(f"- W_ND_DROP = {W_ND_DROP}\n")
        f.write(f"- W_PRESSURE = {W_PRESSURE}\n")
        f.write(f"- W_FDOT_PRIOR = {W_FDOT_PRIOR}\n")
        f.write(f"- W_RIZK_GAS_SHAPE = {W_RIZK_GAS_SHAPE}\n")
        f.write(f"- W_HIGHT_GUARD = {W_HIGHT_GUARD}\n\n")
        f.write("## Best FINAL candidate\n\n")
        for key in CSV_ORDER:
            if key in best and key != "error":
                f.write(f"- `{key}` = {best.get(key)}\n")
        f.write("\n## All FINAL candidates\n\n")
        f.write("| rank | score | f_n | K_d | rho_d | Fdot | Dv | Dg | b | gb | gd | coal | cap | swD1600 | Rd1600 | Nd_drop1725/1400 |\n")
        f.write("|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|\n")
        for i, r in enumerate(sorted(final_rows, key=row_score), start=1):
            f.write(
                f"| {i} | {row_score(r):.6g} | {safe_float(r.get('f_n')):.3e} | {safe_float(r.get('K_d')):.3e} | "
                f"{safe_float(r.get('rho_d')):.3e} | {safe_float(r.get('fission_rate')):.3e} | "
                f"{safe_float(r.get('Dv_scale')):.3g} | {safe_float(r.get('Dg_scale')):.3g} | "
                f"{safe_float(r.get('b_scale')):.3g} | {safe_float(r.get('gb_scale')):.3g} | "
                f"{safe_float(r.get('gd_scale')):.3g} | {safe_float(r.get('coalescence_d_scale')):.3g} | "
                f"{safe_float(r.get('capture_scale')):.3g} | {safe_float(r.get('swD_1p3_1600K')):.4g} | "
                f"{safe_float(r.get('Rd_1p3_1600K')):.4g} | {safe_float(r.get('Nd_drop_1725_over_1400_log10')):.4g} |\n"
            )
    print(f"Summary written to: {path}")

# ============================================================
# RUN
# ============================================================

RUN_AUTOMATIC = False

if RUN_AUTOMATIC:
    print("#" * 140)
    print("UN M7 GLOBAL SENSITIVITY FULL CELL")
    print("#" * 140)
    print(f"Output dir       = {OUTPUT_DIR}")
    print(f"N_GLOBAL_FAST    = {N_GLOBAL_FAST}")
    print(f"FAST dt, modes   = {FAST_DT_H:g} h, {FAST_N_MODES}")
    print(f"FINAL dt, modes  = {FINAL_DT_H:g} h, {FINAL_N_MODES}")
    print(f"N_TOP_FINAL      = {N_TOP_FINAL}")
    print(f"PLOT_TOP_N       = {PLOT_TOP_N}")
    print("#" * 140)

    fast_rows = run_fast_sweep(n_global=N_GLOBAL_FAST, use_full_exp=False)
    final_rows = run_final_from_fast(fast_rows, n_top=N_TOP_FINAL)

    for rank, row in enumerate(sorted(final_rows, key=row_score)[:PLOT_TOP_N], start=1):
        cand = candidate_from_score_row(row, label_prefix=f"best_M7_global_rank{rank}")
        capture_diagnostic_for_candidate(cand, T=1600.0, burnup=3.2)
        make_diagnostics_for_candidate(cand, dt_h=FINAL_DT_H, n_modes=FINAL_N_MODES, prefix=f"best_M7_global_rank{rank}")

    print("\nDONE.")
    print(f"Results folder: {OUTPUT_DIR}")
    print("Main files:")
    print(" - global_fast_all.csv")
    print(" - global_final_top.csv")
    print(" - global_sensitivity_summary.md")
    print(" - best_M7_global_rank*.png")



# ============================================================
# OPTUNA CALIBRATION RUNNER
# ============================================================

def set_model_family(family: str):
    global MODEL_FAMILY, USE_PHI_GAS_RESOLUTION, USE_NUCLEATION_MASS_COUPLING, USE_BULK_DISLOCATION_CAPTURE
    allowed = {"M7_full", "M7_no_phi", "capture_only", "baseline"}
    if family not in allowed:
        raise ValueError(f"Unknown family {family!r}. Allowed: {sorted(allowed)}")
    MODEL_FAMILY = family
    if family == "M7_full":
        USE_PHI_GAS_RESOLUTION = True
        USE_NUCLEATION_MASS_COUPLING = True
        USE_BULK_DISLOCATION_CAPTURE = True
    elif family == "M7_no_phi":
        USE_PHI_GAS_RESOLUTION = False
        USE_NUCLEATION_MASS_COUPLING = True
        USE_BULK_DISLOCATION_CAPTURE = True
    elif family == "capture_only":
        USE_PHI_GAS_RESOLUTION = False
        USE_NUCLEATION_MASS_COUPLING = False
        USE_BULK_DISLOCATION_CAPTURE = True
    elif family == "baseline":
        USE_PHI_GAS_RESOLUTION = False
        USE_NUCLEATION_MASS_COUPLING = False
        USE_BULK_DISLOCATION_CAPTURE = False


def optuna_candidate_from_trial(trial, family: str) -> Candidate:
    # Ranges are intentionally narrower than the old random sweep but still
    # broad enough to discover different families.
    vals = {
        "f_n": trial.suggest_float("f_n", 1.0e-8, 3.0e-7, log=True),
        "K_d": trial.suggest_float("K_d", 1.5e5, 9.0e5, log=True),
        "rho_d": trial.suggest_float("rho_d", 1.0e13, 5.0e13, log=True),
        # Treat fission_rate as a power/case scaling, not as a material constant.
        "fission_rate": trial.suggest_float("fission_rate", 4.0e19, 8.0e19, log=False),
        "Dv_scale": trial.suggest_float("Dv_scale", 0.15, 3.0, log=True),
        "Dg_scale": trial.suggest_float("Dg_scale", 0.25, 2.5, log=True),
        "D2_xe_scale": trial.suggest_float("D2_xe_scale", 0.1, 10.0, log=True),
        "b_scale": trial.suggest_float("b_scale", 0.15, 5.0, log=True),
        "gb_scale": trial.suggest_float("gb_scale", 0.2, 5.0, log=True),
        "gd_scale": trial.suggest_float("gd_scale", 0.15, 5.0, log=True),
        # Coalescence is never allowed to be zero.
        "coalescence_d_scale": trial.suggest_float("coalescence_d_scale", 0.3, 15.0, log=True),
        "capture_scale": trial.suggest_float("capture_scale", 0.05, 5.0, log=True),
    }

    if family == "baseline":
        vals["capture_scale"] = 0.0

    return Candidate(
        label=f"{family}_trial_{trial.number:05d}",
        f_n=vals["f_n"],
        K_d=vals["K_d"],
        rho_d=vals["rho_d"],
        fission_rate=vals["fission_rate"],
        Dv_scale=vals["Dv_scale"],
        Dg_scale=vals["Dg_scale"],
        b_scale=vals["b_scale"],
        gb_scale=vals["gb_scale"],
        gd_scale=vals["gd_scale"],
        coalescence_d_scale=vals["coalescence_d_scale"],
        capture_scale=vals["capture_scale"],
        D2_xe_scale=vals["D2_xe_scale"],
    )


def _is_scalar_for_attr(v):
    return isinstance(v, (str, int, float, bool)) and not (isinstance(v, float) and (math.isnan(v) or math.isinf(v)))


def objective_factory(family: str, dt_h: float, n_modes: int, use_full_exp: bool):
    set_model_family(family)

    def objective(trial):
        cand = optuna_candidate_from_trial(trial, family)
        try:
            row = score_candidate(cand, dt_h, n_modes, use_full_exp=use_full_exp)
            value = float(row["score_total"])
        except Exception as e:
            trial.set_user_attr("error", repr(e))
            return float("inf")

        for k, v in row.items():
            if k in ("hist", "rates"):
                continue
            if _is_scalar_for_attr(v):
                trial.set_user_attr(k, v)

        rates = run_model_point(1600.0, 1.3, cand, dt_h, n_modes, keep_history=False).get("rates", {})
        for k in ["D1_Xe", "D2_Xe", "D2_Xe_scaled", "D3_Xe", "D2_Xe_over_Dg_unscaled", "Dg", "Dv"]:
            if k in rates and _is_scalar_for_attr(rates[k]):
                trial.set_user_attr(k, rates[k])

        return value

    return objective


def trials_to_rows(study) -> List[Dict]:
    rows = []
    for t in study.trials:
        if t.value is None:
            continue
        row = dict(t.user_attrs)
        row["trial_number"] = t.number
        row["score_total"] = t.value
        row["state"] = str(t.state)
        rows.append(row)
    return sorted(rows, key=row_score)


def enqueue_known_good_trials(study):
    # Seeds from the last constrained M7 sweep. They only guide Optuna; it can still explore.
    known = [
        {
            "f_n": 4.70e-8, "K_d": 5.75e5, "rho_d": 3.04e13, "fission_rate": 4.76e19,
            "Dv_scale": 0.591, "Dg_scale": 0.552, "D2_xe_scale": 1.0,
            "b_scale": 0.233, "gb_scale": 1.588, "gd_scale": 0.328,
            "coalescence_d_scale": 8.15, "capture_scale": 2.97,
        },
        {
            "f_n": 1.0e-8, "K_d": 8.0e5, "rho_d": 1.0e13, "fission_rate": 7.5e19,
            "Dv_scale": 3.0, "Dg_scale": 0.5, "D2_xe_scale": 1.0,
            "b_scale": 0.3, "gb_scale": 1.0, "gd_scale": 1.0,
            "coalescence_d_scale": 1.0, "capture_scale": 0.3,
        },
        {
            "f_n": 1.0e-6, "K_d": 5.0e5, "rho_d": 3.0e13, "fission_rate": 5.0e19,
            "Dv_scale": 1.0, "Dg_scale": 1.0, "D2_xe_scale": 1.0,
            "b_scale": 1.0, "gb_scale": 1.0, "gd_scale": 1.0,
            "coalescence_d_scale": 1.0, "capture_scale": 1.0,
        },
    ]
    for k in known:
        try:
            study.enqueue_trial(k, skip_if_exists=True)
        except TypeError:
            # Older Optuna fallback
            study.enqueue_trial(k)


def run_optuna_calibration(
    family: str,
    n_trials: int,
    output_dir: str,
    fast_dt_h: float,
    fast_n_modes: int,
    final_dt_h: float,
    final_n_modes: int,
    n_top_final: int,
    use_full_exp_fast: bool = False,
    make_plots: bool = True,
):
    global OUTPUT_DIR, FAST_DT_H, FAST_N_MODES, FINAL_DT_H, FINAL_N_MODES
    OUTPUT_DIR = output_dir
    FAST_DT_H = fast_dt_h
    FAST_N_MODES = fast_n_modes
    FINAL_DT_H = final_dt_h
    FINAL_N_MODES = final_n_modes
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    set_model_family(family)

    try:
        import optuna
    except ModuleNotFoundError:
        raise SystemExit("Optuna is not installed. Run: pip install optuna")

    db_path = os.path.join(OUTPUT_DIR, f"{family}.db")
    storage = f"sqlite:///{db_path}"
    study = optuna.create_study(
        study_name=f"UN_M7_{family}",
        direction="minimize",
        storage=storage,
        load_if_exists=True,
    )
    enqueue_known_good_trials(study)

    print("#" * 120)
    print(f"OPTUNA CALIBRATION: {family}")
    print("#" * 120)
    print(f"Output dir        = {OUTPUT_DIR}")
    print(f"Trials requested  = {n_trials}")
    print(f"FAST dt/modes     = {FAST_DT_H:g} h / {FAST_N_MODES}")
    print(f"FINAL dt/modes    = {FINAL_DT_H:g} h / {FINAL_N_MODES}")
    print(f"Pressure weight   = {W_PRESSURE:g}; free factor = {PRESSURE_FREE_FACTOR:g}")
    print(f"D2 Xe             = included, with D2_xe_scale")
    print(f"Coalescence       = active, scale never zero")
    print(f"Family switches   = phi_gas:{USE_PHI_GAS_RESOLUTION}, nuc_mass:{USE_NUCLEATION_MASS_COUPLING}, capture:{USE_BULK_DISLOCATION_CAPTURE}")
    print("#" * 120)

    study.optimize(
        objective_factory(family, FAST_DT_H, FAST_N_MODES, use_full_exp_fast),
        n_trials=n_trials,
        gc_after_trial=True,
        show_progress_bar=True,
    )

    rows = trials_to_rows(study)
    fast_csv = os.path.join(OUTPUT_DIR, f"optuna_fast_trials_{family}.csv")
    write_csv(fast_csv, rows)
    print(f"Fast trials written to: {fast_csv}")
    print_best_table(rows, n=min(15, len(rows)))

    final_rows = []
    complete_rows = [r for r in rows if math.isfinite(row_score(r))]
    for rank, r in enumerate(complete_rows[:n_top_final], start=1):
        cand = candidate_from_score_row(r, label_prefix=f"FINAL_rank{rank}_{family}")
        try:
            row = score_candidate(cand, FINAL_DT_H, FINAL_N_MODES, use_full_exp=True)
            final_rows.append(row)
        except Exception as e:
            bad = {**candidate_to_dict(cand), "error": repr(e), "score_total": math.inf}
            final_rows.append(bad)

    final_rows = sorted(final_rows, key=row_score)
    final_csv = os.path.join(OUTPUT_DIR, f"optuna_final_top_{family}.csv")
    write_csv(final_csv, final_rows)
    print(f"Final rows written to: {final_csv}")
    print_best_table(final_rows, n=min(len(final_rows), n_top_final))

    if make_plots:
        for rank, row in enumerate(final_rows[:min(3, len(final_rows))], start=1):
            cand = candidate_from_score_row(row, label_prefix=f"best_{family}_rank{rank}")
            capture_diagnostic_for_candidate(cand, T=1600.0, burnup=3.2)
            make_diagnostics_for_candidate(cand, dt_h=FINAL_DT_H, n_modes=FINAL_N_MODES, prefix=f"best_{family}_rank{rank}")

    # Minimal summary.
    summary_path = os.path.join(OUTPUT_DIR, f"optuna_summary_{family}.md")
    with open(summary_path, "w", encoding="utf-8") as f:
        f.write(f"# UN M7 Optuna calibration summary — {family}\n\n")
        f.write(f"- Trials requested: {n_trials}\n")
        f.write(f"- Fast numerics: dt_h={FAST_DT_H}, n_modes={FAST_N_MODES}\n")
        f.write(f"- Final numerics: dt_h={FINAL_DT_H}, n_modes={FINAL_N_MODES}\n")
        f.write(f"- D2 Xe: included\n")
        f.write(f"- Pressure weight: {W_PRESSURE}\n")
        f.write(f"- Family switches: phi_gas={USE_PHI_GAS_RESOLUTION}, nucleation_mass={USE_NUCLEATION_MASS_COUPLING}, capture={USE_BULK_DISLOCATION_CAPTURE}\n\n")
        f.write("## Best final candidates\n\n")
        f.write("| rank | label | score | f_n | K_d | rho_d | Fdot | Dv | Dg | D2Xe | b | gb | gd | coal | cap | swD1600 | Rd1600 | NdDrop |\n")
        f.write("|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|\n")
        for i, r in enumerate(final_rows[:n_top_final], start=1):
            f.write(
                f"| {i} | {r.get('label')} | {row_score(r):.5g} | "
                f"{safe_float(r.get('f_n')):.3e} | {safe_float(r.get('K_d')):.3e} | {safe_float(r.get('rho_d')):.3e} | {safe_float(r.get('fission_rate')):.3e} | "
                f"{safe_float(r.get('Dv_scale')):.3g} | {safe_float(r.get('Dg_scale')):.3g} | {safe_float(r.get('D2_xe_scale'), 1.0):.3g} | "
                f"{safe_float(r.get('b_scale')):.3g} | {safe_float(r.get('gb_scale')):.3g} | {safe_float(r.get('gd_scale')):.3g} | "
                f"{safe_float(r.get('coalescence_d_scale')):.3g} | {safe_float(r.get('capture_scale')):.3g} | "
                f"{safe_float(r.get('swD_1p3_1600K')):.4g} | {safe_float(r.get('Rd_1p3_1600K')):.4g} | {safe_float(r.get('Nd_drop_1725_over_1400_log10')):.4g} |\n"
            )
    print(f"Summary written to: {summary_path}")
    return final_rows


def main():
    import argparse
    parser = argparse.ArgumentParser(description="Optuna calibration for UN M7 model families.")
    parser.add_argument("--family", choices=["M7_full", "M7_no_phi", "capture_only", "baseline"], required=True)
    parser.add_argument("--n-trials", type=int, default=300)
    parser.add_argument("--output-dir", default=None)
    parser.add_argument("--fast-dt-h", type=float, default=12.0)
    parser.add_argument("--fast-n-modes", type=int, default=22)
    parser.add_argument("--final-dt-h", type=float, default=1.0)
    parser.add_argument("--final-n-modes", type=int, default=40)
    parser.add_argument("--n-top-final", type=int, default=6)
    parser.add_argument("--full-exp-fast", action="store_true", help="Use full experimental datasets already in fast objective; slower.")
    parser.add_argument("--no-plots", action="store_true")
    args = parser.parse_args()

    out = args.output_dir or os.path.join("UN_M7_optuna_results", args.family)
    run_optuna_calibration(
        family=args.family,
        n_trials=args.n_trials,
        output_dir=out,
        fast_dt_h=args.fast_dt_h,
        fast_n_modes=args.fast_n_modes,
        final_dt_h=args.final_dt_h,
        final_n_modes=args.final_n_modes,
        n_top_final=args.n_top_final,
        use_full_exp_fast=args.full_exp_fast,
        make_plots=not args.no_plots,
    )


if __name__ == "__main__":
    main()
