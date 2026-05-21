# un_model.py — UN fission gas behaviour, pure module.
#
# Provides:
#   - Candidate, UNParameters dataclasses
#   - Helper functions for diffusivity, resolution, trapping, nucleation,
#     coalescence, pressure, vacancy ODE, spectral 3-equation gas balance
#   - solve_UN(p) — main solver
#
# Physics flags:
#   USE_PHI_GAS_RESOLUTION       = False  (per-atom b from Setyawan 2018 /
#                                          Matthews 2014 → φ does NOT belong
#                                          on dc/dt or dm_b/dt; only on dN_b/dt)
#   USE_NUCLEATION_MASS_COUPLING = True   (Pizzocri 2020 mass conservation)
#   USE_GB_BUBBLES               = True   (Rizk 2025 §A.2 grain-face bubbles)
#   USE_BULK_DISLOCATION_CAPTURE = True   (Barani-like sweeping; with rho_d=1e14
#                                          calibrated 2026-05-21 it improves the
#                                          Ronchi fit by ~8% on RMSE Sw_d. Not
#                                          in Rizk 2025 / 2023 — kept as a UN
#                                          extension justified empirically.)

import math
from dataclasses import dataclass
from typing import Optional, Sequence, Dict, List, Tuple

# ============================================================
# PHYSICS FLAGS
# ============================================================
# Re-solution rate b(R) used here comes from Setyawan 2018 (MD) and Matthews
# 2014, both of which explicitly state b is computed on a "per bubble atom"
# basis (probability per unit time that an individual gas atom in a bubble
# is ejected back to the matrix). The standard moment closure for such a
# per-atom b puts the bare b on the gas balance and b·φ on the bubble-count
# equation (φ converts per-atom rate → per-bubble extinction rate).
# Applying φ also to dc/dt and dm_b/dt (the Barani 2019/2020 closure choice)
# is an alternative not directly justified by the MD-derived b: it fits
# Ronchi 1978 better but at the price of an extra φ factor that does not
# follow from the per-atom definition. We therefore default to OFF
# (per-atom-rigorous, Rizk 2025 Eq. 21a/b/c as published) and keep ON as
# an ablation option for studying the moment-closure sensitivity.
USE_PHI_GAS_RESOLUTION = False
USE_NUCLEATION_MASS_COUPLING = True

# Inter-granular (grain face) bubble model: Rizk 2025 §A.2 + Eqs. 39, 40.
# When False, gas leaving the grain interior is directly counted as FGR
# (legacy behaviour, no GB bubbles).
USE_GB_BUBBLES = True

# Bulk → dislocation capture (Barani-like sweeping). Not in Rizk 2025 / 2023.
# History:
#   2026-05-09 audit: REMOVED because (a) marginally degrades the fit at the
#                     then-default rho_d=3e13 + phi=ON closure, (b) not in any
#                     UN literature paper (Rizk/Pizzocri/Barani 2025).
#   2026-05-21      : RE-INTRODUCED as a toggleable flag with mass-conserving
#                     bookkeeping (gas mb→md, vacancies nvb→nvd, count N_b ↓,
#                     N_d unchanged "absorption" interpretation). With the new
#                     defaults (phi=OFF rigorous closure, rho_d=1e14 UN-realistic),
#                     capture-ON improves Ronchi fit from RMSE 0.937 (cap=F) to
#                     0.857 (cap=T). Default flipped to True.
# When True, dislocation bubbles in growth sweep a fraction
#   f_cap = capture_scale · N_d · 4π(R_d+R_b)² · ΔR_d
# of nearby bulk bubbles into the dislocation population (gas + vacancies +
# bubble count). Mass-conserving by construction. dN_b/dV_d = -N_b·N_d in
# differential form (Olander §10.4 cross-section form).
USE_BULK_DISLOCATION_CAPTURE = True

# All Rizk physical constants live in the caller (UN_clean.ipynb / RIZK_CONSTANTS).
# This module is parameter-free: only physics formulas, solver, and algorithmic guards.

# ============================================================
# DATACLASSES
# ============================================================

@dataclass(frozen=True)
class Candidate:
    label: str
    f_n: float
    K_d: float
    rho_d: float
    fission_rate: float

    Dv_scale: float = 1.0
    Dv_D1_scale: float = 1.0
    Dv_D3_scale: float = 1.0
    Dg_scale: float = 1.0
    b_scale: float = 1.0
    gb_scale: float = 1.0
    gd_scale: float = 1.0
    coalescence_d_scale: float = 1.0
    capture_scale: float = 1.0

    D2_xe_scale: float = 1.0

    Dg_D1_scale: float = 1.0
    Dg_D3_scale: float = 1.0

    b_bulk_scale: float = 1.0
    b_dislocation_scale: float = 1.0

    gd_bubble_scale: float = 1.0
    gd_line_scale: float = 1.0
    gd_line_alpha: float = 1.0


@dataclass(kw_only=True)
class UNParameters:
    # --- Run-specific (must be supplied per call) ---
    temperature: float
    fission_rate: float
    grain_radius: float
    target_burnup_percent_fima: Optional[float] = None
    final_time: float = 24.0 * 3600.0
    dt: float = 12.0 * 3600.0
    n_modes: int = 22

    # --- Fission yield / precursor ---
    xe_yield: float
    precursor_factor: float = 1.0

    # --- Boltzmann ---
    kB_eV: float
    kB_J: float

    # --- Geometry / matrix ---
    lattice_parameter: float
    omega_fg: float
    radius_in_lattice: float
    gamma_b: float
    hydrostatic_stress: float = 0.0
    min_radius_for_pressure: float = 1.0e-15

    # --- Xe diffusivity D = D1 + D3 (D2 diagnostic only for Xe) ---
    D10: float
    Q1: float
    A20_xe: float
    B21_xe: float
    B22_xe: float
    B23_xe: float
    A30: float
    D2_xe_scale: float = 1.0
    Dg_scale: float = 1.0
    Dg_D1_scale: float = 1.0
    Dg_D3_scale: float = 1.0
    Dg_extra_scale: float = 1.0

    # --- Vacancy diffusivity (D1 thermal + D3 athermal; D2 dropped) ---
    D10_vU: float
    Q1_vU: float
    A30_vU: float
    Dv_scale: float = 1.0
    Dv_D1_scale: float = 1.0
    Dv_D3_scale: float = 1.0

    # --- Resolution shape b0(R), Rizk 2025 Eq. 8 ---
    b0_prefactor: float
    b0_a1: float
    b0_a2: float
    b0_b1: float

    # --- Dislocations ---
    f_n: float
    rho_d: float
    K_d: float
    r_d: float
    Z_d: float

    # --- Inter-granular (grain face) bubble model — Rizk 2025 §A.2 ---
    N_gf_0: float          # m^-2, initial GB bubble density
    D_vgb_ratio: float     # D_v_gb / D_1_v_thermal (multiplier)
    delta_gb: float        # m, GB diffusion layer thickness
    R_gf_0: float          # m, initial GB bubble radius
    F_c_sat: float         # saturation coverage threshold
    theta_rad: float       # rad, semi-dihedral angle

    # --- Calibration scales ---
    gb_scale: float = 1.0
    gd_scale: float = 1.0
    b_scale: float = 1.0
    b_bulk_scale: float = 1.0
    b_dislocation_scale: float = 1.0
    gd_bubble_scale: float = 1.0
    gd_line_scale: float = 1.0
    gd_line_alpha: float = 1.0
    coalescence_d_scale: float = 1.0
    capture_scale: float = 1.0

    # --- Initial conditions ---
    R_b: float = 0.0
    N_b: float = 0.0
    R_d: float = 0.0
    N_d: Optional[float] = None
    c0: float = 0.0
    mb0: float = 0.0
    md0: float = 0.0
    nvb0: Optional[float] = None
    nvd0: Optional[float] = None

    # --- Solver behaviour ---
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
    try:
        expo = (
            -p.B21_xe / kBT
            -p.B22_xe / (kBT**2)
            -p.B23_xe / (kBT**3)
        )
        expo = max(min(expo, 700.0), -745.0)
        D2 = math.sqrt(p.fission_rate) * p.A20_xe * math.exp(expo)
    except OverflowError:
        D2 = math.inf
    D3 = p.A30 * p.fission_rate

    Dg_unscaled = p.Dg_D1_scale * D1 + p.Dg_D3_scale * D3
    Dg = Dg_unscaled * p.Dg_scale * p.precursor_factor * p.Dg_extra_scale
    return Dg, {
        "D1_Xe": D1,
        "D2_Xe": D2,
        "D2_Xe_scaled": 0.0,
        "D3_Xe": D3,
        "Dg_D1_scaled": p.Dg_D1_scale * D1,
        "Dg_D3_scaled": p.Dg_D3_scale * D3,
        "D2_Xe_over_Dg_unscaled": (D2 / Dg_unscaled) if Dg_unscaled > 0 and math.isfinite(Dg_unscaled) else math.nan,
        "Dg": Dg,
    }


def vacancy_diffusivity_UN(p: UNParameters):
    T = p.temperature
    kBT = p.kB_eV * T
    D1 = p.D10_vU * math.exp(-p.Q1_vU / kBT)
    D3 = p.A30_vU * p.fission_rate
    Dv_unscaled = p.Dv_D1_scale * D1 + p.Dv_D3_scale * D3
    Dv = Dv_unscaled * p.Dv_scale
    return Dv, {
        "Dv1": D1,
        "Dv3": D3,
        "Dv1_scaled": p.Dv_D1_scale * D1,
        "Dv3_scaled": p.Dv_D3_scale * D3,
        "Dv": Dv,
    }


def gb_vacancy_diffusivity_UN(p: UNParameters) -> float:
    """D_v_gb = D_vgb_ratio * D_1_thermal_v   (Rizk 2025 §A.2 + Tab. 1).

    Note: only the THERMAL Arrhenius part of D_v enters the GB multiplier;
    no D_3 athermal here. This is what Rizk uses for SIFGRS in BISON.
    """
    kBT = p.kB_eV * p.temperature
    D1_thermal_v = p.D10_vU * math.exp(-p.Q1_vU / kBT)
    return p.D_vgb_ratio * D1_thermal_v


def b0_resolution(R: float, prefactor: float, a1: float, a2: float, b1: float) -> float:
    R = max(R, 1.0e-15)
    return prefactor * (a1 - a2 * math.exp(-b1 / R))


def resolution_rates_UN(p: UNParameters, R_b: float, R_d: float):
    b0_b = b0_resolution(R_b + p.radius_in_lattice, p.b0_prefactor, p.b0_a1, p.b0_a2, p.b0_b1)
    b0_d = b0_resolution(R_d + p.radius_in_lattice, p.b0_prefactor, p.b0_a1, p.b0_a2, p.b0_b1)
    b_b = p.fission_rate * b0_b * p.b_scale * p.b_bulk_scale
    b_d = p.fission_rate * b0_d * p.b_scale * p.b_dislocation_scale
    return b_b, b_d


def trapping_rates_UN(p: UNParameters, Dg: float, R_b: float, N_b: float, R_d: float, N_d: float):
    Rb_eff = R_b + p.radius_in_lattice
    Rd_eff = R_d + p.radius_in_lattice
    g_b_unscaled = 0.0 if N_b <= 0.0 else 4.0 * math.pi * Dg * Rb_eff * N_b

    Gamma_d = 1.0 / math.sqrt(math.pi * p.rho_d)
    den = math.log(Gamma_d / (p.Z_d * p.r_d)) - 3.0 / 5.0
    if den <= 0.0:
        raise ValueError(f"Invalid dislocation sink denominator: {den:g}")

    free_dislocation = max(p.rho_d - p.gd_line_alpha * 2.0 * R_d * N_d, 0.0)
    term_bubbles = 4.0 * math.pi * Dg * Rd_eff * N_d
    term_dislocation = (2.0 * math.pi * Dg / den) * free_dislocation
    g_d_unscaled = p.gd_bubble_scale * term_bubbles + p.gd_line_scale * term_dislocation

    g_b = p.gb_scale * g_b_unscaled
    g_d = p.gd_scale * g_d_unscaled

    return g_b, g_d, {
        "Gamma_d": Gamma_d,
        "den": den,
        "free_dislocation": free_dislocation,
        "term_bubbles": term_bubbles,
        "term_dislocation": term_dislocation,
        "term_bubbles_scaled": p.gd_bubble_scale * term_bubbles,
        "term_dislocation_scaled": p.gd_line_scale * term_dislocation,
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


# === Inter-granular (grain face) bubble helpers — Rizk 2025 Eqs. 39, 40 ===

def zeta_gf(F_c: float) -> float:
    """Eq. 39 of Rizk 2025: GB-bubble sink-strength geometric factor.

        ζ_gf = -[(3 - F_c)(1 - F_c) + 2 ln F_c] / 4

    F_c is the fraction of grain-face area covered by bubble projections.
    """
    F_c = max(min(F_c, 0.99), 1.0e-6)
    inner = (3.0 - F_c) * (1.0 - F_c) + 2.0 * math.log(F_c)
    return max(-inner / 4.0, 1.0e-30)


def lenticular_shape_factor(theta: float) -> float:
    """g(θ) = 1 - 1.5 cosθ + 0.5 cos²θ  (lenticular bubble volume factor)."""
    c = math.cos(theta)
    return 1.0 - 1.5 * c + 0.5 * c * c


def R_gf_from_V(V_gf: float, theta: float) -> float:
    """Eq. 40 of Rizk 2025: lenticular-bubble radius of curvature from volume.

        V = (4π/3) R³ · g(θ)        ⇒        R = (3 V / [4π g(θ)])^(1/3)
    """
    if V_gf <= 0.0:
        return 0.0
    g = lenticular_shape_factor(theta)
    return (3.0 * V_gf / (4.0 * math.pi * g)) ** (1.0 / 3.0)


def V_gf_from_R(R_gf: float, theta: float) -> float:
    """Inverse of Eq. 40."""
    g = lenticular_shape_factor(theta)
    return (4.0 / 3.0) * math.pi * R_gf ** 3 * g


def F_c_coverage(N_gf: float, R_gf: float, theta: float) -> float:
    """Fraction of grain-face area covered by lenticular bubble projections.

    Each bubble projects onto the grain face as a circle of radius R · sinθ.
    Coverage = N_gf · π · (R sinθ)².
    """
    return math.pi * N_gf * R_gf ** 2 * math.sin(theta) ** 2


def gb_vacancy_step(p: UNParameters, D_v_gb: float, R_gf: float,
                    N_gf: float, m_f: float, n_v_f_old: float,
                    F_c: float, dt: float):
    """Backward-Euler step on Eq. 21f for grain-face bubbles.

    Differs from intragranular Eq. 21f via: (a) δ → δ_gb (constant GB
    thickness, not Wigner-Seitz), (b) ζ → ζ_gf (Eq. 39, function of F_c),
    (c) D_v → D_v_gb (Rizk Tab. 1: 10⁶ × D_1_thermal).

    Same quadratic form as `vacancy_concentration_implicit_step`, with the
    A coefficient redefined.
    """
    if N_gf <= 0.0 or m_f <= 0.0 or R_gf <= 0.0:
        return n_v_f_old, 0.0

    p_eq = 2.0 * p.gamma_b / R_gf - p.hydrostatic_stress
    p_int_old = pressure_internal(p, m_f, n_v_f_old)

    if p.vacancy_absorption_only and p_int_old <= p_eq:
        return n_v_f_old, 0.0

    zeta = zeta_gf(F_c)
    A = 2.0 * math.pi * D_v_gb * p.delta_gb * N_gf / (p.kB_J * p.temperature * zeta)
    C = p.kB_J * p.temperature * m_f / omega_matrix(p)
    B = n_v_f_old - dt * A * p_eq
    disc = B * B + 4.0 * dt * A * C

    if disc < 0.0:
        raise ValueError(f"Negative discriminant in GB vacancy step: {disc:g}")

    sqrt_disc = math.sqrt(disc)
    if B >= 0.0:
        n_new = 0.5 * (B + sqrt_disc)
    else:
        denom = sqrt_disc - B
        n_new = 0.0 if denom <= 0.0 else (2.0 * dt * A * C) / denom

    if p.vacancy_absorption_only:
        n_new = max(n_new, n_v_f_old)

    return n_new, (n_new - n_v_f_old) / dt


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
# SOLVER
# ============================================================

def solve_UN(p: UNParameters, keep_history: bool = True):
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

    # --- Inter-granular (grain-face) bubble state ---
    # Convention: m_f, n_v_f are atom/vacancy densities PER m^3 OF GRAIN (3D),
    # consistent with c, m_b, m_d. N_gf is the published 2D density (Rizk
    # Tab. 1: 2e13 m^-2 of grain face). For per-bubble volume calculations
    # and the vacancy ODE we therefore convert to a 3D bubble density via
    #     N_gf_3D = N_gf * (3 / r_grain)        [bubbles per m^3 of grain]
    # F_c (coverage of the grain face) keeps the 2D N_gf.
    N_gf = p.N_gf_0
    N_gf_3D = N_gf * 3.0 / p.grain_radius
    R_gf = p.R_gf_0
    V_gf = V_gf_from_R(R_gf, p.theta_rad)
    m_f = 0.0
    # Initial vacancies fill the entire bubble volume (no gas yet).
    n_v_f = max(V_gf * N_gf_3D / omega_matrix(p), 0.0)
    F_c = F_c_coverage(N_gf, R_gf, p.theta_rad)
    fgr_total = 0.0

    # Bulk → dislocation capture accumulators (zero when flag OFF; updated in loop).
    f_cap_step = 0.0
    cap_raw_step = 0.0
    capture_fraction_sum = 0.0
    capture_raw_sum = 0.0
    capture_bubbles_cumulative = 0.0
    max_f_cap_step = 0.0

    hist_keys = [
        "time", "burnup_percent_fima", "c", "mb", "md", "Nb", "Nd", "Vb", "Vd", "Rb", "Rd",
        "nvb", "nvd", "generated", "retained", "q_gb", "swelling_b", "swelling_d", "swelling_ig",
        "p_b", "p_d", "p_b_eq", "p_d_eq", "lambda_d", "nu_b", "phi_b", "phi_d",
        "matrix_gas_percent", "bulk_gas_percent", "dislocation_gas_percent", "qgb_gas_percent",
        # Inter-granular (grain-face) bubble outputs:
        "mf", "nvf", "Vgf", "Rgf", "Ngf", "F_c", "swelling_gf", "fgr_total",
        "fgr_percent", "swelling_gf_percent", "intergranular_gas_percent",
        "released_gas_percent",
        # Solid fission product swelling (Rizk 2025 Eq. 19): 0.5·B per FIMA.
        "swelling_solid", "swelling_solid_percent",
        # Bulk → dislocation capture diagnostics (USE_BULK_DISLOCATION_CAPTURE).
        "f_cap_step", "cap_raw_step", "capture_fraction_sum",
        "capture_raw_sum", "capture_bubbles_cumulative", "max_f_cap_step",
    ]
    hist = {key: [] for key in hist_keys}

    def append_state(nu_b=0.0, phi_b=0.0, phi_d=0.0, lambda_d=0.0):
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
        hist["matrix_gas_percent"].append(100.0 * c_av / generated if generated > 0.0 else 0.0)
        hist["bulk_gas_percent"].append(100.0 * mb_av / generated if generated > 0.0 else 0.0)
        hist["dislocation_gas_percent"].append(100.0 * md_av / generated if generated > 0.0 else 0.0)
        hist["qgb_gas_percent"].append(100.0 * q_gb / generated if generated > 0.0 else 0.0)
        hist["mf"].append(m_f)
        hist["nvf"].append(n_v_f)
        hist["Vgf"].append(V_gf)
        hist["Rgf"].append(R_gf)
        hist["Ngf"].append(N_gf)
        hist["F_c"].append(F_c)
        # GB volume fraction = N_gf_3D · V_gf  (N_gf_3D = N_gf * 3/r_grain).
        swelling_gf_vol = N_gf_3D * V_gf
        hist["swelling_gf"].append(swelling_gf_vol)
        hist["fgr_total"].append(fgr_total)
        hist["swelling_gf_percent"].append(100.0 * swelling_gf_vol)
        hist["intergranular_gas_percent"].append(100.0 * m_f / generated if generated > 0.0 else 0.0)
        hist["released_gas_percent"].append(100.0 * fgr_total / generated if generated > 0.0 else 0.0)
        hist["fgr_percent"].append(100.0 * fgr_total / generated if generated > 0.0 else 0.0)
        # Solid swelling (Rizk 2025 Eq. 19)
        bu_pct_now = time_to_burnup_percent(t, p.fission_rate, p.lattice_parameter)
        sw_solid_pct = 0.5 * bu_pct_now
        hist["swelling_solid"].append(sw_solid_pct / 100.0)
        hist["swelling_solid_percent"].append(sw_solid_pct)
        # Bulk → dislocation capture diagnostics (zero when flag OFF).
        hist["f_cap_step"].append(f_cap_step)
        hist["cap_raw_step"].append(cap_raw_step)
        hist["capture_fraction_sum"].append(capture_fraction_sum)
        hist["capture_raw_sum"].append(capture_raw_sum)
        hist["capture_bubbles_cumulative"].append(capture_bubbles_cumulative)
        hist["max_f_cap_step"].append(max_f_cap_step)

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

        N_b = (Nb_old + dt * nu_b) / (1.0 + dt * b_b * phi_b)
        N_b = max(N_b, p.min_number_density)

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

        # Coalescence: backward Euler step on  dN_d/dV_d = -4 λ N_d^2.
        # λ is lagged at (Vd_old, Nd_old); N_new is treated implicitly,
        # giving the quadratic  4 λ_old ΔV · N_new^2 + N_new - N_old = 0.
        # The positive root, in numerically stable form, is
        #     N_new = 2 N_old / (1 + sqrt(1 + 16 λ_old ΔV N_old)).
        # We do NOT use Rizk 2025 Eq. 15 (closed-form with constant λ over ΔV):
        # SCIANTIX integrates every kinetic equation via implicit Euler, and
        # treating λ as constant over the step is only valid for vanishing dt.
        lambda_d = coalescence_lambda(Vd_old, Nd_old)
        dVd_positive = max(V_d_growth - Vd_old, 0.0)
        coal_coeff = p.coalescence_d_scale * 4.0 * lambda_d * dVd_positive
        if coal_coeff > 0.0 and Nd_old > 0.0:
            disc = 1.0 + 4.0 * coal_coeff * Nd_old
            N_d = 2.0 * Nd_old / (math.sqrt(disc) + 1.0)
        else:
            N_d = Nd_old
        N_d = max(N_d, p.min_number_density)

        V_b = (p.omega_fg * max(mb_new, 0.0) + omega_matrix(p) * nvb) / N_b if N_b > 0.0 else 0.0
        V_d = (p.omega_fg * max(md_new, 0.0) + omega_matrix(p) * nvd) / N_d if N_d > 0.0 else 0.0
        V_b = max(V_b, p.min_volume)
        V_d = max(V_d, p.min_volume)
        R_b = radius_from_volume(V_b)
        R_d = radius_from_volume(V_d)

        # === Bulk → dislocation capture (Barani-like sweeping) ===
        # Not in Rizk 2025. Active only when USE_BULK_DISLOCATION_CAPTURE = True
        # (audit 2026-05-09: OFF by default, ablation-only).
        # The volume swept by dislocation bubbles in growth this step,
        # delta_Vcap = 4π(R_d + R_b)² · ΔR_d, captures a fraction
        # f_cap = capture_scale · N_d · delta_Vcap of nearby bulk bubbles
        # (gas mb, vacancies nvb, count N_b) into the dislocation population.
        f_cap_step = 0.0
        cap_raw_step = 0.0
        if USE_BULK_DISLOCATION_CAPTURE:
            delta_Rd_cap = max(R_d - Rd_old, 0.0)
            delta_Vcap = 4.0 * math.pi * (Rd_old + Rb_old) ** 2 * delta_Rd_cap
            cap_raw_step = p.capture_scale * N_d * delta_Vcap
            f_cap_step = max(0.0, min(cap_raw_step, 1.0))

            capture_raw_sum += cap_raw_step
            capture_fraction_sum += f_cap_step
            if f_cap_step > max_f_cap_step:
                max_f_cap_step = f_cap_step

            if f_cap_step > 0.0 and N_b > 0.0:
                mb_before = max(mb_new, 0.0)
                nvb_before = max(nvb, 0.0)
                capture_bubbles_cumulative += f_cap_step * N_b

                mb_new = (1.0 - f_cap_step) * mb_before
                md_new = max(md_new, 0.0) + f_cap_step * mb_before
                nvb = (1.0 - f_cap_step) * nvb_before
                nvd = max(nvd, 0.0) + f_cap_step * nvb_before
                N_b = max((1.0 - f_cap_step) * N_b, p.min_number_density)

                modes_c, modes_mb, modes_md = reset_modes_to_averages(
                    c_new, mb_new, md_new, p.n_modes
                )

                V_b = (p.omega_fg * max(mb_new, 0.0) + omega_matrix(p) * nvb) / N_b if N_b > 0.0 else 0.0
                V_d = (p.omega_fg * max(md_new, 0.0) + omega_matrix(p) * nvd) / N_d if N_d > 0.0 else 0.0
                V_b = max(V_b, p.min_volume)
                V_d = max(V_d, p.min_volume)
                R_b = radius_from_volume(V_b)
                R_d = radius_from_volume(V_d)

        # === Inter-granular (grain-face) bubble step ===
        # Gas leaving the grain interior in this step (conservation balance).
        delta_grain_internal = (c_new - c_old) + (mb_new - mb_old) + (md_new - md_old)
        gas_to_gb = max(beta * dt - delta_grain_internal, 0.0)

        if USE_GB_BUBBLES:
            m_f_provisional = m_f + gas_to_gb

            # Vacancy ODE on GB bubble (BE quadratic, with D_v_gb, δ_gb, ζ_gf).
            # Pass N_gf_3D so that A = 2π D_v_gb δ_gb N_3D / (kT ζ_gf) has the
            # same units as the intragranular case.
            D_v_gb = gb_vacancy_diffusivity_UN(p)
            n_v_f, dnvf_dt = gb_vacancy_step(
                p, D_v_gb, R_gf, N_gf_3D, m_f_provisional, n_v_f, F_c, dt,
            )

            # Volume + radius update from gas + vacancies (lenticular geometry).
            V_gf_new = ((p.omega_fg * max(m_f_provisional, 0.0)
                         + omega_matrix(p) * n_v_f) / N_gf_3D
                        if N_gf_3D > 0.0 else 0.0)
            V_gf_new = max(V_gf_new, p.min_volume)
            R_gf_new = R_gf_from_V(V_gf_new, p.theta_rad)
            F_c_new = F_c_coverage(N_gf, R_gf_new, p.theta_rad)

            # Saturation cap + FGR release at F_c ≥ F_c,sat.
            fgr_step = 0.0
            if F_c_new > p.F_c_sat:
                R_gf_sat = math.sqrt(p.F_c_sat / (math.pi * N_gf
                                                  * math.sin(p.theta_rad) ** 2))
                V_gf_sat = V_gf_from_R(R_gf_sat, p.theta_rad)
                m_f_sat = max(
                    (V_gf_sat * N_gf_3D - omega_matrix(p) * n_v_f) / p.omega_fg,
                    0.0,
                )
                fgr_step = max(m_f_provisional - m_f_sat, 0.0)
                m_f = m_f_sat
                V_gf = V_gf_sat
                R_gf = R_gf_sat
                F_c = p.F_c_sat
            else:
                m_f = m_f_provisional
                V_gf = V_gf_new
                R_gf = R_gf_new
                F_c = F_c_new

            fgr_total += fgr_step
        else:
            # Legacy: no GB bubbles, gas leaving the grain is released directly.
            fgr_total += gas_to_gb

        generated += beta * dt
        retained = max(c_new, 0.0) + max(mb_new, 0.0) + max(md_new, 0.0)
        # q_gb keeps its original meaning ("everything that has left the grain
        # interior"), now decomposed into m_f (in GB bubbles) + fgr_total
        # (released to plenum). Conservation: q_gb = m_f + fgr_total.
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
            **D_parts, **Dv_parts, **trapping_parts,
        }

        append_state(nu_b=nu_b, phi_b=phi_b, phi_d=phi_d, lambda_d=lambda_d)

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
            "matrix_gas_percent": [100.0 * c_av / generated if generated > 0.0 else 0.0],
            "bulk_gas_percent": [100.0 * mb_av / generated if generated > 0.0 else 0.0],
            "dislocation_gas_percent": [100.0 * md_av / generated if generated > 0.0 else 0.0],
            "qgb_gas_percent": [100.0 * q_gb / generated if generated > 0.0 else 0.0],
            "mf": [m_f],
            "nvf": [n_v_f],
            "Vgf": [V_gf],
            "Rgf": [R_gf],
            "Ngf": [N_gf],
            "F_c": [F_c],
            "swelling_gf": [N_gf_3D * V_gf],
            "fgr_total": [fgr_total],
            "swelling_gf_percent": [100.0 * N_gf_3D * V_gf],
            "swelling_solid": [
                0.005 * time_to_burnup_percent(t, p.fission_rate, p.lattice_parameter)
            ],
            "swelling_solid_percent": [
                0.5 * time_to_burnup_percent(t, p.fission_rate, p.lattice_parameter)
            ],
            "intergranular_gas_percent": [100.0 * m_f / generated if generated > 0.0 else 0.0],
            "released_gas_percent": [100.0 * fgr_total / generated if generated > 0.0 else 0.0],
            "fgr_percent": [100.0 * fgr_total / generated if generated > 0.0 else 0.0],
            "f_cap_step": [f_cap_step],
            "cap_raw_step": [cap_raw_step],
            "capture_fraction_sum": [capture_fraction_sum],
            "capture_raw_sum": [capture_raw_sum],
            "capture_bubbles_cumulative": [capture_bubbles_cumulative],
            "max_f_cap_step": [max_f_cap_step],
        }

    return hist, last_rates
