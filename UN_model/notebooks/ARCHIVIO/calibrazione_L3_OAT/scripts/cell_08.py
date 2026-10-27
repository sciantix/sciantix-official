import math
from dataclasses import dataclass
from typing import Optional, Sequence, Dict, List, Tuple

# Solver core from Zullo's un_model.py, embedded for standalone use.
# Main switches/constants are intentionally defined in the first notebook cell.

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
    Dv_D2_scale: float = 1.0
    Dv_dislocation_scale: float = 1.0

    Dg_scale: float = 1.0
    b_scale: float = 1.0
    gb_scale: float = 1.0
    gd_scale: float = 1.0
    coalescence_d_scale: float = 1.0
    capture_scale: float = 1.0

    D2_xe_scale: float = 1.0

    Dg_D1_scale: float = 1.0
    Dg_D3_scale: float = 1.0
    Dg_dislocation_scale: float = 1.0

    b_bulk_scale: float = 1.0
    b_dislocation_scale: float = 1.0

    gd_bubble_scale: float = 1.0
    gd_line_scale: float = 1.0
    gd_line_alpha: float = 1.0


@dataclass
class UNParameters:
    temperature: float = 1600.0
    fission_rate: float = FISSION_RATE_NOMINAL
    grain_radius: float = GRAIN_RADIUS
    target_burnup_percent_fima: Optional[float] = None
    final_time: float = 24.0 * 3600.0
    dt: float = 3600.0
    n_modes: int = 40

    xe_yield: float = XE_YIELD
    precursor_factor: float = 1.0

    D10: float = 1.56e-3
    Q1: float = 4.94
    A20_xe: float = 1.21e-67
    B21_xe: float = 25.87
    B22_xe: float = -1.49
    B23_xe: float = 0.0
    A30: float = 1.85e-39
    D2_xe_scale: float = 1.0
    Dg_scale: float = 1.0
    Dg_D1_scale: float = 1.0
    Dg_D3_scale: float = 1.0
    Dg_dislocation_scale: float = 1.0
    xe_diffusivity_mode: str = XE_DIFFUSIVITY_MODE

    kB_eV: float = 8.617333262e-5
    kB_J: float = 1.380649e-23

    D10_vU: float = 1.35e-2
    Q1_vU: float = 5.66
    B21_vU: float = -0.62
    B22_vU: float = -0.04
    A20_vU: float = A20_VU_DEFAULT
    Dv_scale: float = 1.0
    Dv_D1_scale: float = 1.0
    Dv_D2_scale: float = 1.0
    Dv_dislocation_scale: float = 1.0
    vacancy_diffusivity_mode: str = VU_DIFFUSIVITY_MODE

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
    b_bulk_scale: float = 1.0
    b_dislocation_scale: float = 1.0
    gd_bubble_scale: float = 1.0
    gd_line_scale: float = 1.0
    gd_line_alpha: float = 1.0
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
    vacancy_absorption_only: bool = False
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


def _safe_exp(x: float) -> float:
    return math.exp(max(min(x, 700.0), -745.0))


def xe_diffusivity_UN(p: UNParameters):
    """Xe / fission-gas diffusivity with selectable Rizk and Matthews modes."""
    T = p.temperature
    kBT = p.kB_eV * T
    mode = p.xe_diffusivity_mode

    # Diagnostics.  In lookup mode the total D_Xe is available, but an
    # artificial D1/D2/D3 decomposition is deliberately NOT invented.
    D1 = D2 = D3 = 0.0
    D1_Xe_i = D2_Xe_i_0 = D2_Xe_i_1 = 0.0
    D1_XeVU = D2_XeVU = 0.0
    D2_scaled = 0.0
    Dg_lookup_base = math.nan
    Dg_lookup_fission_rate_ref = math.nan
    Dg_lookup_D3_ref = math.nan
    Dg_lookup_D3_added = math.nan
    Dg_lookup_total_before_global_scale = math.nan

    if mode == "matthews2025_fig42_lookup":
        # Unified reference: lookup at Fdot=1e19 is used as a non-D3
        # contribution; add the FULL pin-specific athermal ballistic D3.
        # This is still an approximate 1-D slice, NOT a 2-D D(T,Fdot) lookup.
        Dg_lookup_base = matthews2025_DXe_lookup(T)
        Dg_lookup_fission_rate_ref = MATTHEWS2025_LOOKUP_FISSION_RATE
        Dg_lookup_D3_ref = p.A30 * Dg_lookup_fission_rate_ref
        D3 = p.A30 * p.fission_rate
        Dg_lookup_D3_added = p.Dg_D3_scale * D3
        Dg_unscaled = Dg_lookup_base + Dg_lookup_D3_added
        Dg_lookup_total_before_global_scale = Dg_unscaled
        D1 = D2 = math.nan
        D2_scaled = math.nan

    elif mode == "rizk2025_table_D1D3":
        # Rizk 2025 Table 2. D2 for Xe is computed only as a diagnostic and intentionally neglected.
        D1 = p.D10 * math.exp(-p.Q1 / kBT)
        D2 = math.sqrt(p.fission_rate) * p.A20_xe * _safe_exp(
            -p.B21_xe / kBT - p.B22_xe / (kBT**2) - p.B23_xe / (kBT**3)
        )
        D3 = p.A30 * p.fission_rate
        Dg_unscaled = p.Dg_D1_scale * D1 + p.Dg_D3_scale * D3
        D2_scaled = 0.0

    elif mode == "rizk2025_refit_plot":
        # Previous 16test mode: refit of digitized Rizk 2025 D_gas curve.
        D10 = 2.967341817979e-03
        Q1 = 5.013616576464e+00
        A20 = 4.498475254045e-68
        B1 = -1.790671812685e+01
        B2 = 9.255235831189e-01
        A30 = 1.189275430019e-38
        D1 = D10 * math.exp(-Q1 / kBT)
        D2 = math.sqrt(p.fission_rate) * A20 * _safe_exp(-B1 / kBT - B2 / (kBT**2))
        D3 = A30 * p.fission_rate
        D2_scaled = p.D2_xe_scale * D2
        Dg_unscaled = p.Dg_D1_scale * D1 + D2_scaled + p.Dg_D3_scale * D3

    elif mode == "rizk2023_table_raw":
        # Rizk 2023 Table 3.1 raw Xe diffusivity: Xei + {Xe:VU} + athermal D3.
        D1_Xe_i = 6.12e-5 * math.exp(-5.76 / kBT)
        D2_Xe_i_0 = math.sqrt(p.fission_rate) * 3.40e-12 * _safe_exp(
            -27.67 / kBT - (-5.28) / (kBT**2) - 0.302 / (kBT**3)
        )
        D2_Xe_i_1 = math.sqrt(p.fission_rate) * 1.45e-33 * _safe_exp(
            -(-1.80) / kBT - 0.156 / (kBT**2)
        )
        D3 = 8.2e-42 * p.fission_rate
        D1_XeVU = 7.54e-4 * math.exp(-5.76 / kBT)
        D2_XeVU = math.sqrt(p.fission_rate) * 2.06e-33 * _safe_exp(
            -(-0.408) / kBT - (-0.0798) / (kBT**2) - 0.00940 / (kBT**3)
        )
        D1 = D1_Xe_i + D1_XeVU
        D2 = D2_Xe_i_0 + D2_Xe_i_1 + D2_XeVU
        D2_scaled = p.D2_xe_scale * D2
        Dg_unscaled = p.Dg_D1_scale * D1 + D2_scaled + p.Dg_D3_scale * D3

    else:
        raise ValueError(
            f"Unknown xe_diffusivity_mode={mode!r}. "
            "Use 'matthews2025_fig42_lookup', 'rizk2025_table_D1D3', "
            "'rizk2025_refit_plot', or 'rizk2023_table_raw'."
        )

    # Matthews lookup has no D1/D2 decomposition; D3 is explicitly added.
    # Dg_D3_scale and Dg_scale remain meaningful.
    Dg = Dg_unscaled * p.Dg_scale * p.precursor_factor * p.Dg_extra_scale

    return Dg, {
        "xe_diffusivity_mode": mode,
        "D1_Xe": D1,
        "D2_Xe": D2,
        "D2_Xe_scaled": D2_scaled,
        "D3_Xe": D3,
        "Dg_lookup_base": Dg_lookup_base,
        "Dg_lookup_fission_rate_ref": Dg_lookup_fission_rate_ref,
        "Dg_lookup_D3_ref": Dg_lookup_D3_ref,
        "Dg_lookup_D3_added": Dg_lookup_D3_added,
        "Dg_lookup_total_before_global_scale": Dg_lookup_total_before_global_scale,
        "D1_Xe_i_2023": D1_Xe_i,
        "D2_Xe_i0_2023": D2_Xe_i_0,
        "D2_Xe_i1_2023": D2_Xe_i_1,
        "D1_XeVU_2023": D1_XeVU,
        "D2_XeVU_2023": D2_XeVU,
        "Dg_D1_scaled": p.Dg_D1_scale * D1 if math.isfinite(D1) else math.nan,
        "Dg_D3_scaled": p.Dg_D3_scale * D3 if math.isfinite(D3) else math.nan,
        "D2_Xe_over_Dg_unscaled": (
            D2 / Dg_unscaled
            if Dg_unscaled > 0 and math.isfinite(Dg_unscaled) and math.isfinite(D2)
            else math.nan
        ),
        "Dg": Dg,
        "Dg_dislocation_scale": p.Dg_dislocation_scale,
        "Dg_dislocation": Dg * p.Dg_dislocation_scale,
    }


def vacancy_diffusivity_UN(p: UNParameters):
    """Effective matrix transport for bubble growth with selectable Rizk/Matthews modes.

    For the Matthews mode, the total U self-diffusivity from Fig. 4.2(b) is
    used as the effective rate-limiting vacancy-mediated matrix transport
    coefficient.  It is not the microscopic mobility of a single uranium vacancy.
    """
    T = p.temperature
    kBT = p.kB_eV * T
    mode = p.vacancy_diffusivity_mode

    Dv_lookup_base = math.nan
    Dv_nonthermal_effective = math.nan

    if mode == "matthews2025_DU_fig42_lookup":
        Dv_lookup_base = matthews2025_DU_lookup(T)

        # Published Matthews/LANL thermal vacancy-mediated U self-diffusion
        # contribution, also used to construct D_v^GB.
        D1 = 3.25e-4 * math.exp(-5.95 / kBT)

        # Do NOT call the remainder "D2": the total Centipede self-diffusivity
        # may contain irradiation-enhanced, athermal, and other defect-cluster contributions.
        D2 = math.nan
        Dv_nonthermal_effective = max(Dv_lookup_base - D1, 0.0)
        Dv_unscaled = Dv_lookup_base

    elif mode == "rizk2025_code_refit_old":
        D1 = p.D10_vU * math.exp(-p.Q1_vU / kBT)
        D2 = math.sqrt(p.fission_rate) * p.A20_vU * _safe_exp(
            p.B21_vU / kBT + p.B22_vU / (kBT**2)
        )
        Dv_unscaled = p.Dv_D1_scale * D1 + p.Dv_D2_scale * D2

    elif mode == "rizk2025_refit_A20_only":
        D1 = 1.35e-2 * math.exp(-5.66 / kBT)
        D2 = math.sqrt(p.fission_rate) * 1.386341579723e-28 * _safe_exp(
            -0.62 / kBT - 0.04 / (kBT**2)
        )
        Dv_unscaled = p.Dv_D1_scale * D1 + p.Dv_D2_scale * D2

    elif mode == "rizk2025_refit_full":
        # Previous 16test selected mode.
        D1 = 1.122978768506e-02 * math.exp(-5.596873538604e+00 / kBT)
        D2 = math.sqrt(p.fission_rate) * 7.805188680989e-28 * _safe_exp(
            -9.932675113163e-01 / kBT - 2.082395503235e-02 / (kBT**2)
        )
        Dv_unscaled = p.Dv_D1_scale * D1 + p.Dv_D2_scale * D2

    elif mode == "rizk2023_table_raw":
        D1 = 2.11e-4 * math.exp(-6.04 / kBT)
        D2 = math.sqrt(p.fission_rate) * 8.12e-42 * _safe_exp(
            -(-6.01) / kBT - 0.543 / (kBT**2) - (-0.0124) / (kBT**3)
        )
        Dv_unscaled = p.Dv_D1_scale * D1 + p.Dv_D2_scale * D2

    else:
        raise ValueError(
            f"Unknown vacancy_diffusivity_mode={mode!r}. "
            "Use 'matthews2025_DU_fig42_lookup', 'rizk2025_code_refit_old', "
            "'rizk2025_refit_A20_only', 'rizk2025_refit_full', or 'rizk2023_table_raw'."
        )

    # Matthews lookup is a total effective D_U curve, so component-specific
    # Dv_D1_scale / Dv_D2_scale are intentionally not applied in that mode.
    Dv = Dv_unscaled * p.Dv_scale
    Dv_dislocation = Dv * p.Dv_dislocation_scale

    return Dv, {
        "vacancy_diffusivity_mode": mode,
        "Dv1": D1,
        "Dv2": D2,
        "Dv_lookup_base": Dv_lookup_base,
        "Dv_nonthermal_effective": Dv_nonthermal_effective,
        "Dv1_scaled": (
            p.Dv_D1_scale * D1
            if mode != "matthews2025_DU_fig42_lookup"
            else D1
        ),
        "Dv2_scaled": (
            p.Dv_D2_scale * D2
            if mode != "matthews2025_DU_fig42_lookup" and math.isfinite(D2)
            else math.nan
        ),
        "Dv": Dv,
        "Dv_dislocation_scale": p.Dv_dislocation_scale,
        "Dv_dislocation": Dv_dislocation,
        "A20_vU_active": p.A20_vU if mode == "rizk2025_code_refit_old" else math.nan,
    }

def b0_resolution(R: float) -> float:
    R = max(R, 1.0e-15)
    return 1.0e-25 * (2.64 - 2.02 * math.exp(-2.61e-9 / R))


def resolution_rates_UN(p: UNParameters, R_b: float, R_d: float):
    b_b = p.fission_rate * b0_resolution(R_b + p.radius_in_lattice) * p.b_scale * p.b_bulk_scale
    b_d = p.fission_rate * b0_resolution(R_d + p.radius_in_lattice) * p.b_scale * p.b_dislocation_scale
    return b_b, b_d


def ws_gas_trapping_factor(R_eff: float, N: float) -> Tuple[float, Dict[str, float]]:
    """Wigner-Seitz correction factor for gas trapping by spherical bubbles.

    The original Rizk-like term is:
        g = 4*pi*D*R*N

    This optional correction rewrites the sink rate in terms of the
    concentration averaged over a spherical Wigner-Seitz cell:

        delta = (3/(4*pi*N))**(1/3)
        x = R/delta
        zeta = 1 - 3/2*x + 1/2*x**3
        f_WS = (1 - x**3) / zeta

    so that:
        g_WS = 4*pi*D*R*N*f_WS

    f_WS -> 1 in the dilute limit x << 1.
    A cap is used for numerical robustness near x -> 1.
    """
    if N <= 0.0 or R_eff <= 0.0:
        return 1.0, {
            "delta_ws": math.nan,
            "x_ws": math.nan,
            "zeta_ws": math.nan,
            "factor_ws_raw": 1.0,
            "factor_ws": 1.0,
        }

    delta = (3.0 / (4.0 * math.pi * N)) ** (1.0 / 3.0)
    if not math.isfinite(delta) or delta <= 0.0:
        return 1.0, {
            "delta_ws": delta,
            "x_ws": math.nan,
            "zeta_ws": math.nan,
            "factor_ws_raw": 1.0,
            "factor_ws": 1.0,
        }

    # The single-size model is geometrically invalid once R approaches/exceeds delta.
    # Clamp x only for evaluating the diagnostic correction factor.
    x = R_eff / delta
    x_eval = min(max(x, 0.0), 1.0 - 1.0e-9)
    zeta = 1.0 - 1.5 * x_eval + 0.5 * x_eval**3

    if zeta <= 0.0 or not math.isfinite(zeta):
        factor_raw = WS_GAS_TRAPPING_MAX_FACTOR
    else:
        factor_raw = (1.0 - x_eval**3) / zeta

    factor = min(max(factor_raw, 0.0), WS_GAS_TRAPPING_MAX_FACTOR)

    return factor, {
        "delta_ws": delta,
        "x_ws": x,
        "zeta_ws": zeta,
        "factor_ws_raw": factor_raw,
        "factor_ws": factor,
    }


def trapping_rates_UN(p: UNParameters, Dg: float, R_b: float, N_b: float, R_d: float, N_d: float):
    Rb_eff = R_b + p.radius_in_lattice
    Rd_eff = R_d + p.radius_in_lattice

    # Bulk-bubble gas trapping. Default is original Rizk-like dilute-sink form.
    ws_b_factor, ws_b_diag = ws_gas_trapping_factor(Rb_eff, N_b)
    if not USE_WS_GAS_TRAPPING_BULK:
        ws_b_factor = 1.0
    g_b_unscaled = 0.0 if N_b <= 0.0 else 4.0 * math.pi * Dg * Rb_eff * N_b * ws_b_factor

    Gamma_d = 1.0 / math.sqrt(math.pi * p.rho_d)
    den = math.log(Gamma_d / (p.Z_d * p.r_d)) - 3.0 / 5.0
    if den <= 0.0:
        raise ValueError(f"Invalid dislocation sink denominator: {den:g}")

    free_dislocation = max(p.rho_d - p.gd_line_alpha * 2.0 * R_d * N_d, 0.0)
    Dg_dislocation = Dg * p.Dg_dislocation_scale

    # Spherical dislocation-bubble gas trapping. Default is original Rizk-like dilute-sink form.
    # The dislocation line-sink term below is left untouched.
    ws_d_factor, ws_d_diag = ws_gas_trapping_factor(Rd_eff, N_d)
    if not USE_WS_GAS_TRAPPING_DISLOCATION:
        ws_d_factor = 1.0
    term_bubbles = 4.0 * math.pi * Dg * Rd_eff * N_d * ws_d_factor

    term_dislocation = (2.0 * math.pi * Dg_dislocation / den) * free_dislocation
    g_d_unscaled = p.gd_bubble_scale * term_bubbles + p.gd_line_scale * term_dislocation

    g_b = p.gb_scale * g_b_unscaled
    g_d = p.gd_scale * g_d_unscaled

    return g_b, g_d, {
        "Gamma_d": Gamma_d,
        "den": den,
        "free_dislocation": free_dislocation,
        "term_bubbles": term_bubbles,
        "term_dislocation": term_dislocation,
        "Dg_dislocation_scale": p.Dg_dislocation_scale,
        "Dg_dislocation": Dg_dislocation,
        "term_bubbles_scaled": p.gd_bubble_scale * term_bubbles,
        "term_dislocation_scaled": p.gd_line_scale * term_dislocation,
        "g_b_unscaled": g_b_unscaled,
        "g_d_unscaled": g_d_unscaled,
        "USE_WS_GAS_TRAPPING_BULK": USE_WS_GAS_TRAPPING_BULK,
        "USE_WS_GAS_TRAPPING_DISLOCATION": USE_WS_GAS_TRAPPING_DISLOCATION,
        "ws_bulk_factor": ws_b_factor,
        "ws_bulk_factor_raw": ws_b_diag["factor_ws_raw"],
        "ws_bulk_delta": ws_b_diag["delta_ws"],
        "ws_bulk_x": ws_b_diag["x_ws"],
        "ws_bulk_zeta": ws_b_diag["zeta_ws"],
        "ws_dislocation_factor": ws_d_factor,
        "ws_dislocation_factor_raw": ws_d_diag["factor_ws_raw"],
        "ws_dislocation_delta": ws_d_diag["delta_ws"],
        "ws_dislocation_x": ws_d_diag["x_ws"],
        "ws_dislocation_zeta": ws_d_diag["zeta_ws"],
    }


def beta_production(p: UNParameters) -> float:
    return p.xe_yield * p.fission_rate


PHI_B_SCALE_CURRENT = 1.0
NUCLEATION_CLUSTER_SIZE_CURRENT = 2.0


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


def seed_vacancies_for_pressure_factor(
    p: UNParameters,
    gas_atoms_per_bubble: float,
    pressure_factor: float,
) -> Tuple[float, float]:
    """Return (vacancies_per_bubble, radius) for a gas dimer seed at p = factor * p_eq.

    Per-bubble equations:
        p = kBT * n_g / (n_v * Omega)
        p_eq = 2 gamma / R
        R = [3*(Omega_fg*n_g + Omega*n_v)/(4*pi)]^(1/3)
        p = pressure_factor * p_eq

    The equation is solved by robust bisection in n_v.
    """
    ng = max(float(gas_atoms_per_bubble), 1.0e-30)
    f = max(float(pressure_factor), 1.0e-12)
    Om = omega_matrix(p)

    def radius_from_nv(nv: float) -> float:
        V = p.omega_fg * ng + Om * nv
        return radius_from_volume(max(V, 1.0e-300))

    def residual(nv: float) -> float:
        R = radius_from_nv(nv)
        p_int = p.kB_J * p.temperature * ng / (nv * Om)
        p_eq = 2.0 * p.gamma_b / max(R, p.min_radius_for_pressure) - p.hydrostatic_stress
        return p_int - f * p_eq

    lo = 1.0e-30
    hi = 1.0
    # Increase upper bound until pressure is below target.
    for _ in range(300):
        if residual(hi) < 0.0:
            break
        hi *= 2.0
    else:
        # Fallback: very large vacancy count if no bracket is found.
        R = radius_from_nv(hi)
        return hi, R

    for _ in range(160):
        mid = math.sqrt(lo * hi)
        if residual(mid) > 0.0:
            lo = mid
        else:
            hi = mid

    nv = hi
    return nv, radius_from_nv(nv)


def dimer_seed_for_population(p: UNParameters, N: float) -> Tuple[float, float, float]:
    """Return (m_seed, n_v_seed, R_seed) for N bubbles."""
    if N <= 0.0 or not USE_EQUILIBRIUM_DIMER_SEED:
        return 0.0, 0.0, 0.0
    nv_per_bubble, R_seed = seed_vacancies_for_pressure_factor(
        p,
        SEED_GAS_ATOMS_PER_BUBBLE,
        SEED_PRESSURE_FACTOR,
    )
    m_seed = SEED_GAS_ATOMS_PER_BUBBLE * N
    n_v_seed = nv_per_bubble * N
    return m_seed, n_v_seed, R_seed


def vacancies_for_pressure_factor_from_gas(
    p: UNParameters,
    m_gas: float,
    N: float,
    pressure_factor: float,
) -> Tuple[float, float]:
    """Return (total_vacancy_concentration, radius) for existing gas m_gas in N bubbles.

    It solves per bubble:
        p = kBT * n_g / (n_v * Omega)
        p_eq = 2 gamma / R
        R = [3*(Omega_fg*n_g + Omega*n_v)/(4*pi)]^(1/3)
        p = pressure_factor * p_eq
    where n_g = m_gas/N and n_v is vacancies per bubble.
    """
    if m_gas <= 0.0 or N <= 0.0:
        return 0.0, 0.0
    gas_atoms_per_bubble = max(m_gas / N, 1.0e-30)
    nv_per_bubble, R_seed = seed_vacancies_for_pressure_factor(
        p,
        gas_atoms_per_bubble,
        pressure_factor,
    )
    return nv_per_bubble * N, R_seed


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

    if not math.isfinite(psi):
        return 1.0e300

    if USE_ZETA_OVERFLOW_GUARD and psi >= 1.0:
        # In the original formula the denominator becomes non-positive for large psi
        # and is clipped to 1e-30. Avoid evaluating psi**6 when psi is enormous.
        if psi > 1.0e75:
            return 1.0e300
        return max(10.0 * psi * (1.0 + psi**3) / 1.0e-30, 1.0e-30)

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
# SOLVER
# ============================================================


# ============================================================
# 8test additions: dynamic rho_d nucleation and grain-face bubbles
# ============================================================

def rho_d_for_time_burnup(p: UNParameters, burnup_percent: float) -> float:
    """Time-dependent dislocation density used by the dynamic N_d update.

    If USE_DYNAMIC_RHO_D_NUCLEATION is off, keep the p.rho_d value used at initialization.
    If RHO_MODE == "constant", this also remains constant.
    Otherwise reuse effective_rho_d(T, F, candidate-like) at call time.
    """
    if (not USE_DYNAMIC_RHO_D_NUCLEATION) or globals().get("RHO_MODE", "constant") == "constant":
        return max(float(p.rho_d), 1.0e10)
    try:
        cand_tmp = Candidate(
            label="rho_runtime",
            f_n=p.f_n,
            K_d=p.K_d,
            rho_d=RHO_D_NOMINAL,
            fission_rate=p.fission_rate,
        )
        return max(float(effective_rho_d(float(p.temperature), float(burnup_percent), cand_tmp)), 1.0e10)
    except Exception:
        # Fallback keeps the notebook robust if this function is called before rho helpers are defined.
        return max(float(p.rho_d), 1.0e10)


def gf_theta_rad() -> float:
    return math.radians(float(THETA_GF_DEG))


def gf_lens_factor(theta: Optional[float] = None) -> float:
    """Geometric factor for a symmetric spherical lenticular bubble.

    Uses the cos^3(theta) spherical-lens form requested for 8test.
    """
    theta = gf_theta_rad() if theta is None else float(theta)
    c = math.cos(theta)
    if USE_LENTICULAR_COS3:
        return 1.0 - 1.5 * c + 0.5 * c**3
    return 1.0 - 1.5 * c + 0.5 * c**2


def gf_volume_from_radius(R: float) -> float:
    R = max(float(R), 0.0)
    return (4.0 / 3.0) * math.pi * R**3 * gf_lens_factor()


def gf_radius_from_volume(V: float) -> float:
    V = max(float(V), 0.0)
    ftheta = max(gf_lens_factor(), 1.0e-30)
    if V <= 0.0:
        return 0.0
    return (3.0 * V / (4.0 * math.pi * ftheta)) ** (1.0 / 3.0)


def gf_area_from_radius(R: float) -> float:
    theta = gf_theta_rad()
    return math.pi * max(float(R), 0.0)**2 * math.sin(theta)**2


def gf_zeta_from_coverage(Fc: float) -> float:
    Fc_eff = min(max(float(Fc), 1.0e-12), 0.999999999)
    return -(((3.0 - Fc_eff) * (1.0 - Fc_eff) + 2.0 * math.log(Fc_eff)) / 4.0)


def gf_pressure_internal(p: UNParameters, ng: float, nv: float) -> float:
    if ng <= 0.0:
        return 0.0
    if nv <= 0.0:
        return 1.0e30
    return p.kB_J * p.temperature * ng / (nv * omega_matrix(p))


def gf_pressure_equilibrium(p: UNParameters, R: float) -> float:
    if R <= 0.0:
        return 1.0e30
    return 2.0 * p.gamma_b / max(R, p.min_radius_for_pressure) - p.hydrostatic_stress


def initialize_grainface_state(p: UNParameters) -> Dict[str, float]:
    """Initial grain-face state.

    q_gf and q_rel are volumetric gas concentrations [at/m3 fuel].
    qv_gf is volumetric vacancy inventory in grain-face bubbles [vac/m3 fuel].
    Ngf_areal is the grain-face bubble number density [bub/m2 GB].
    """
    NgfA = float(NGF_AREAL_0)
    NgfV = 3.0 * NgfA / (2.0 * p.grain_radius)
    R0 = float(R_GF0)
    V0 = gf_volume_from_radius(R0)
    # Seed only the geometrical vacancy volume implied by the initial bubble radius.
    # No initial gas is added.
    qv0 = NgfV * V0 / omega_matrix(p) if NgfV > 0.0 else 0.0
    A0 = gf_area_from_radius(R0)
    return {
        "q_gf": 0.0,
        "q_rel": 0.0,
        "qv_gf": max(qv0, 0.0),
        "Ngf_areal": NgfA,
        "Ngf_vol": NgfV,
        "ng_gf": 0.0,
        "nv_gf": (qv0 / NgfV) if NgfV > 0.0 else 0.0,
        "V_gf": V0,
        "R_gf": R0,
        "A_gf": A0,
        "Fc": NgfA * A0,
        "p_gf": 0.0,
        "p_gf_eq": gf_pressure_equilibrium(p, R0),
        "zeta_gf": gf_zeta_from_coverage(max(NgfA * A0, 1.0e-12)),
        "dnv_gf_dt": 0.0,
        "dA_gf_dt": 0.0,
        "dNgf_dt": 0.0,
        "dq_gf_in_step": 0.0,
        "dq_rel_step": 0.0,
        "gf_saturated": 0.0,
    }


def _refresh_grainface_derived(p: UNParameters, gf: Dict[str, float]) -> None:
    NgfA = max(float(gf.get("Ngf_areal", 0.0)), 0.0)
    NgfV = 3.0 * NgfA / (2.0 * p.grain_radius) if NgfA > 0.0 else 0.0
    gf["Ngf_areal"] = NgfA
    gf["Ngf_vol"] = NgfV
    if NgfV > 0.0:
        ng = max(gf.get("q_gf", 0.0), 0.0) / NgfV
        nv = max(gf.get("qv_gf", 0.0), 0.0) / NgfV
    else:
        ng = 0.0
        nv = 0.0
    V = p.omega_fg * ng + omega_matrix(p) * nv
    R = gf_radius_from_volume(V)
    A = gf_area_from_radius(R)
    Fc = NgfA * A
    zeta = gf_zeta_from_coverage(max(Fc, 1.0e-12))
    p_int = gf_pressure_internal(p, ng, nv)
    p_eq = gf_pressure_equilibrium(p, R)
    gf.update({
        "ng_gf": ng,
        "nv_gf": nv,
        "V_gf": V,
        "R_gf": R,
        "A_gf": A,
        "Fc": Fc,
        "zeta_gf": zeta,
        "p_gf": p_int,
        "p_gf_eq": p_eq,
    })



def _limited_growth_grainface(nv0: float, growth_rate: float, equilibrium_term: float, dt: float) -> float:
    """Positive quadratic update equivalent to SCIANTIX Solver::LimitedGrowth."""
    x = float(nv0) + float(equilibrium_term) * float(dt)
    disc = x * x + 4.0 * max(float(growth_rate), 0.0) * float(dt)
    return 0.5 * (x + math.sqrt(max(disc, 0.0)))



def _solve_grainface_3x3(
    V0: float,
    A0: float,
    N0: float,
    Fc0: float,
    gasvol0: float,
    gasvol1: float,
    delta_nv: float,
    omega_v: float,
    kappa_gf: float,
    saturated_branch: bool,
) -> Tuple[float, float, float]:
    """Coupled 3x3 Pastore grain-face predictor for V_gf, A_gf, N_gf.

    Below saturation, the volume equation contains the N-coupling associated
    with conservation of the total retained grain-face gas during coalescence.

    After saturation, Pastore Eq. (28) releases the gas carried by the bubbles
    lost through interconnection. That release exactly cancels the old-gas/N
    coupling in dV/dt, so only NEW gas arriving during the step contributes to
    the increase of gas atoms per surviving bubble.
    """
    tiny = 1.0e-300
    V0 = max(float(V0), tiny)
    A0 = max(float(A0), tiny)
    N0 = max(float(N0), tiny)
    kappa_gf = max(float(kappa_gf), tiny)

    beta = (2.0 / 3.0) * A0 / V0

    if saturated_branch:
        # Pastore saturated branch:
        # dN/dt = -(N/A) dA/dt
        gamma = N0 / A0
        bN = 2.0 * N0

        # With Pastore Eq. (28), gas lost with disappearing bubbles does not
        # accumulate in the survivors. Hence the N-coupling term cancels.
        alpha = 0.0
        delta_gasvol = max(float(gasvol1) - float(gasvol0), 0.0)
        bV = V0 + float(delta_nv) * float(omega_v) + delta_gasvol / (N0 * kappa_gf)
    else:
        # Pastore/White pre-saturation coalescence.
        gamma = 6.0 * N0 * N0 / (3.0 + 4.0 * max(float(Fc0), 0.0))
        bN = N0 + gamma * A0

        alpha = max(float(gasvol0), 0.0) / (N0 * N0 * kappa_gf)
        bV = (
            V0
            + float(delta_nv) * float(omega_v)
            + max(float(gasvol1), 0.0) / (N0 * kappa_gf)
        )

    bA = A0 / 3.0

    # [ 1      0      alpha ] [V1]   [bV]
    # [-beta   1      0     ] [A1] = [bA]
    # [ 0      gamma  1     ] [N1]   [bN]
    det = 1.0 - alpha * beta * gamma
    if (not math.isfinite(det)) or abs(det) < 1.0e-18:
        raise RuntimeError(f"Singular grain-face 3x3 system: det={det:.6e}")

    V1 = (bV - alpha * (bN - gamma * bA)) / det
    A1 = bA + beta * V1
    N1 = bN - gamma * A1

    if not (math.isfinite(V1) and math.isfinite(A1) and math.isfinite(N1)):
        raise RuntimeError(f"Non-finite grain-face 3x3 solution: V={V1}, A={A1}, N={N1}")

    return V1, A1, N1



def grain_boundary_vacancy_diffusivity(p: UNParameters, Dv1_bulk: float) -> float:
    """Return the vacancy diffusivity used along grain boundaries.

    12test default:
        Matthews et al. / LANL LA-UR-25-28152:
        D_v^GB = 325 exp[-5.95/(k_B T)] m^2/s.

    Legacy comparison:
        D_v^GB = 1e6 * D_v,1^bulk

    The bulk/intragranular vacancy diffusivity itself is NOT changed here.
    """
    mode = str(DV_GB_MODE)

    if mode == "miller2025_LAUR2528152":
        kBT = p.kB_eV * p.temperature
        if kBT <= 0.0:
            return 0.0
        return max(
            float(MILLER_DV_GB_D0)
            * math.exp(-float(MILLER_DV_GB_Q_EV) / kBT),
            0.0,
        )

    if mode == "rizk_legacy_1e6_Dv1":
        # Preserve the old Rizk GB closure independently of the selected bulk mode.
        # If the bulk mode is Matthews, do NOT multiply the Matthews thermal
        # contribution by 1e6 and call it "Rizk"; reconstruct the Rizk thermal D1.
        if str(p.vacancy_diffusivity_mode) == "matthews2025_DU_fig42_lookup":
            kBT = p.kB_eV * p.temperature
            Dv1_legacy = 1.35e-2 * math.exp(-5.66 / kBT) if kBT > 0.0 else 0.0
        else:
            Dv1_legacy = max(float(Dv1_bulk), 0.0)
        return max(float(DV_GB_MULTIPLIER) * Dv1_legacy, 0.0)

    raise ValueError(
        f"Unknown DV_GB_MODE={mode!r}. "
        "Use 'miller2025_LAUR2528152' or 'rizk_legacy_1e6_Dv1'."
    )


def update_grainface_state(p: UNParameters, gf: Dict[str, float], dq_to_gb: float, dt: float, Dv1_bulk: float) -> Dict[str, float]:
    """Advance grain-face bubbles with coupled Pastore numerics.

    Numerical structure
    -------------------
    1) vacancies per bubble: SCIANTIX LimitedGrowth update;
    2) below saturation: coupled 3x3 Pastore/White coalescence predictor;
    3) at the first crossing: project exactly onto Fc = Fc,sat while preserving
       the mean state of the surviving bubbles;
    4) after saturation: use Pastore Eqs. (25)-(28) consistently:
           Fc = N A = Fc,sat,
           dN/dt = -(N/A) dA/dt,
           dq_rel/dt = -kappa_gf * n_g * dN/dt.
       The post-saturation bookkeeping therefore no longer changes q_gf and
       then recomputes a different A after the 3x3 solve.
    """
    dq_to_gb = max(float(dq_to_gb), 0.0)
    dt = max(float(dt), 0.0)

    gf["dq_gf_in_step"] = dq_to_gb
    gf["dq_rel_step"] = 0.0
    gf["dA_gf_dt"] = 0.0
    gf["dNgf_dt"] = 0.0
    gf["dnv_gf_dt"] = 0.0

    if dt <= 0.0 or not USE_INTERGRANULAR_MODEL:
        return gf

    _refresh_grainface_derived(p, gf)

    q_old = max(float(gf.get("q_gf", 0.0)), 0.0)
    q_rel_old = max(float(gf.get("q_rel", 0.0)), 0.0)
    N0 = max(float(gf["Ngf_areal"]), 1.0e-30)
    V0 = max(float(gf["V_gf"]), 1.0e-300)
    A0 = max(float(gf["A_gf"]), 1.0e-300)
    Fc0 = max(float(gf["Fc"]), 0.0)
    nv0 = max(float(gf["nv_gf"]), 0.0)

    kappa_gf = 3.0 / (2.0 * p.grain_radius)

    saturated_before = bool(gf.get("saturation_reached", False)) or Fc0 >= FC_SAT * (1.0 - 1.0e-12)
    gf["saturation_reached"] = saturated_before
    gf["gf_saturated"] = 1.0 if saturated_before else 0.0

    # Gas arriving from the grain interior during this step.
    q_trial = q_old + dq_to_gb

    # Before release, distribute the newly available gas using the old bubble
    # population. In the saturated branch this is also the Pastore-consistent
    # final gas content per surviving bubble, because Eq. (28) removes the gas
    # carried by the bubbles that disappear.
    ng_trial_at_N0 = q_trial / (kappa_gf * N0)

    # ------------------------------------------------------------
    # SCIANTIX LimitedGrowth update for vacancies per bubble.
    # ------------------------------------------------------------
    Dv_gb = grain_boundary_vacancy_diffusivity(p, Dv1_bulk)
    zeta0 = max(float(gf["zeta_gf"]), 1.0e-300)
    R0 = max(float(gf["R_gf"]), 1.0e-300)
    p_eq0 = gf_pressure_equilibrium(p, R0)

    nv1 = nv0
    if Dv_gb > 0.0:
        volume_flow_rate = 2.0 * math.pi * Dv_gb * DELTA_GB / zeta0
        growth_rate = volume_flow_rate * ng_trial_at_N0 / max(omega_matrix(p), 1.0e-300)
        equilibrium_term = -volume_flow_rate * p_eq0 / (p.kB_J * p.temperature)

        nv_candidate = _limited_growth_grainface(
            nv0, growth_rate, equilibrium_term, dt
        )

        if GF_VACANCY_ABSORPTION_ONLY:
            nv1 = max(nv_candidate, nv0)
        else:
            nv1 = max(nv_candidate, 0.0)

    delta_nv = nv1 - nv0
    gf["dnv_gf_dt"] = delta_nv / dt

    gasvol0 = p.omega_fg * q_old
    gasvol1 = p.omega_fg * q_trial

    # ------------------------------------------------------------
    # Already saturated: exact Pastore constant-coverage/release bookkeeping.
    # ------------------------------------------------------------
    if saturated_before:
        # Keep the same 3x3 predictor used for the analytical linearized system.
        V_lin, A_lin, N_lin = _solve_grainface_3x3(
            V0=V0,
            A0=A0,
            N0=N0,
            Fc0=Fc0,
            gasvol0=gasvol0,
            gasvol1=gasvol1,
            delta_nv=delta_nv,
            omega_v=omega_matrix(p),
            kappa_gf=kappa_gf,
            saturated_branch=True,
        )

        if V_lin <= 0.0 or A_lin <= 0.0 or N_lin <= 0.0:
            raise RuntimeError(
                f"Non-positive saturated grain-face 3x3 predictor: "
                f"V={V_lin:.6e}, A={A_lin:.6e}, N={N_lin:.6e}, "
                f"Fc0={Fc0:.6e}, T={p.temperature:.1f} K, dt={dt/3600.0:.4g} h"
            )

        # Physical composition after LimitedGrowth + newly arriving gas.
        # Eq. (28) ensures bubble loss does NOT concentrate the old gas in the
        # survivors; ng therefore follows q_trial/(kappa*N0).
        ng1 = ng_trial_at_N0
        V1 = p.omega_fg * ng1 + omega_matrix(p) * nv1
        R1 = gf_radius_from_volume(V1)
        A1 = gf_area_from_radius(R1)

        # Pastore Eq. (25): enforce saturation coverage exactly.
        # If A were to shrink, do not create bubbles; this mirrors the
        # one-way "loss of bubbles" interpretation used in SCIANTIX.
        if A1 >= A0:
            N1 = min(N0, FC_SAT / max(A1, 1.0e-300))
        else:
            N1 = N0

        # Pastore Eq. (28), integrated consistently with q = kappa*N*ng.
        # Gas in bubbles lost through interconnection is released.
        survival = max(min(N1 / N0, 1.0), 0.0)
        q_gf1 = q_trial * survival
        dq_rel = max(q_trial - q_gf1, 0.0)

        gf["q_gf"] = q_gf1
        gf["q_rel"] = q_rel_old + dq_rel
        gf["qv_gf"] = nv1 * (kappa_gf * N1)
        gf["Ngf_areal"] = N1
        gf["saturation_reached"] = True
        gf["gf_saturated"] = 1.0
        gf["dq_rel_step"] = dq_rel

        _refresh_grainface_derived(p, gf)

        # Numerical invariant: for monotonic bubble growth the saturated branch
        # must remain on Fc_sat to roundoff.
        if A1 >= A0 and abs(gf["Fc"] - FC_SAT) > 5.0e-10:
            raise RuntimeError(
                f"Pastore saturation invariant lost: Fc={gf['Fc']:.12g}, "
                f"target={FC_SAT:.12g}"
            )

        gf["dA_gf_dt"] = (gf["A_gf"] - A0) / dt
        gf["dNgf_dt"] = (gf["Ngf_areal"] - N0) / dt
        return gf

    # ------------------------------------------------------------
    # Below saturation: coupled 3x3 Pastore/White coalescence.
    # ------------------------------------------------------------
    V_lin, A_lin, N_lin = _solve_grainface_3x3(
        V0=V0,
        A0=A0,
        N0=N0,
        Fc0=Fc0,
        gasvol0=gasvol0,
        gasvol1=gasvol1,
        delta_nv=delta_nv,
        omega_v=omega_matrix(p),
        kappa_gf=kappa_gf,
        saturated_branch=False,
    )

    if V_lin <= 0.0 or A_lin <= 0.0 or N_lin <= 0.0:
        raise RuntimeError(
            f"Non-positive grain-face 3x3 solution: "
            f"V={V_lin:.6e}, A={A_lin:.6e}, N={N_lin:.6e}, "
            f"Fc0={Fc0:.6e}, T={p.temperature:.1f} K, dt={dt/3600.0:.4g} h"
        )

    # Preserve total grain-face gas below saturation (no release).
    gf["q_gf"] = q_trial
    gf["q_rel"] = q_rel_old
    gf["Ngf_areal"] = N_lin
    gf["qv_gf"] = nv1 * (kappa_gf * N_lin)
    _refresh_grainface_derived(p, gf)

    # First attainment of saturation. Project onto Fc_sat without changing the
    # mean gas/vacancy content of the surviving bubbles.
    if gf["Fc"] >= FC_SAT:
        A_cross = max(gf["A_gf"], 1.0e-300)
        N_before = gf["Ngf_areal"]
        N_target = min(N_before, FC_SAT / A_cross)
        survival = max(min(N_target / N_before, 1.0), 0.0)

        q_before = gf["q_gf"]
        gf["q_gf"] *= survival
        gf["qv_gf"] *= survival
        gf["Ngf_areal"] = N_target

        dq_rel = max(q_before - gf["q_gf"], 0.0)
        gf["q_rel"] = q_rel_old + dq_rel
        gf["dq_rel_step"] = dq_rel
        gf["saturation_reached"] = True
        gf["gf_saturated"] = 1.0

        _refresh_grainface_derived(p, gf)

    gf["dA_gf_dt"] = (gf["A_gf"] - A0) / dt
    gf["dNgf_dt"] = (gf["Ngf_areal"] - N0) / dt

    return gf

def solve_UN(p: UNParameters, keep_history: bool = True):
    # Initial modes are set after optional seed initialization below.
    modes_c = None
    modes_mb = None
    modes_md = None

    R_b = p.R_b
    R_d = p.R_d
    N_b = p.N_b

    # 8test: if dynamic rho_d is active, initialize dislocation sites from rho_d(T,F=0),
    # not from the final-burnup rho_d used by 7test.
    rho_d_current = rho_d_for_time_burnup(p, 0.0)
    if USE_DYNAMIC_RHO_D_NUCLEATION:
        p.rho_d = rho_d_current
        N_d = p.K_d * rho_d_current
    else:
        N_d = p.N_d

    c0_eff = p.c0
    mb0_eff = p.mb0
    md0_eff = p.md0

    nvb = initialize_vacancy_concentration(p, N_b, R_b, mb0_eff) if p.nvb0 is None else p.nvb0
    nvd = initialize_vacancy_concentration(p, N_d, R_d, md0_eff) if p.nvd0 is None else p.nvd0

    # Optional initial dimer seed for dislocation bubbles.
    if (
        USE_EQUILIBRIUM_DIMER_SEED
        and SEED_INITIAL_DISLOCATION_BUBBLES
        and N_d > 0.0
        and R_d <= 0.0
        and md0_eff <= 0.0
        and nvd <= 0.0
    ):
        md0_eff, nvd, R_d = dimer_seed_for_population(p, N_d)

    V_b = sphere_volume(R_b)
    V_d = sphere_volume(R_d)

    modes_c, modes_mb, modes_md = reset_modes_to_averages(c0_eff, mb0_eff, md0_eff, p.n_modes)

    beta = beta_production(p)
    initial_gas = c0_eff + mb0_eff + md0_eff
    generated = 0.0
    q_gb = 0.0          # total gas that has reached the grain boundary = q_gf + q_rel
    q_gb_total_target = 0.0
    retained = initial_gas
    t = 0.0

    gf = initialize_grainface_state(p) if USE_INTERGRANULAR_MODEL else initialize_grainface_state(p)

    capture_fraction_sum = 0.0
    capture_raw_sum = 0.0
    capture_bubbles_cumulative = 0.0
    max_f_cap_step = 0.0

    bulk_first_gas_seed_done = False
    dislocation_first_gas_seed_done = False

    hist_keys = [
        "time", "burnup_percent_fima", "rho_d", "rho_d_next", "delta_Nd_rho", "c", "mb", "md", "Nb", "Nd", "Vb", "Vd", "Rb", "Rd",
        "nvb", "nvd", "generated", "retained", "retained_ig", "q_gb", "q_gf", "q_rel", "swelling_b", "swelling_d", "swelling_gf", "swelling_ig", "swelling_total_gas",
        "Ngf_areal", "Ngf_vol", "ng_gf", "nv_gf", "V_gf", "R_gf", "A_gf", "Fc_gf", "p_gf", "p_gf_eq", "zeta_gf", "gf_saturated",
        "p_b", "p_d", "p_b_eq", "p_d_eq", "lambda_d", "nu_b", "phi_b", "phi_d",
        "f_cap_step", "cap_raw_step", "capture_fraction_sum", "capture_raw_sum",
        "capture_bubbles_cumulative", "max_f_cap_step",
        "matrix_gas_percent", "bulk_gas_percent", "dislocation_gas_percent", "qgb_gas_percent", "grainface_gas_percent", "release_gas_percent",
    ]
    hist = {key: [] for key in hist_keys}

    def append_state(nu_b=0.0, phi_b=0.0, phi_d=0.0, lambda_d=0.0, fcap=0.0, capraw=0.0, delta_Nd_rho=0.0, rho_next=None):
        if not keep_history:
            return
        c_av = reconstruct_average(modes_c)
        mb_av = reconstruct_average(modes_mb)
        md_av = reconstruct_average(modes_md)
        p_b = pressure_internal(p, mb_av, nvb)
        p_d = pressure_internal(p, md_av, nvd)
        p_b_eq = pressure_equilibrium(p, R_b)
        p_d_eq = pressure_equilibrium(p, R_d)
        _refresh_grainface_derived(p, gf)
        q_gf = gf["q_gf"] if USE_INTERGRANULAR_MODEL else q_gb
        q_rel = gf["q_rel"] if USE_INTERGRANULAR_MODEL else 0.0
        swelling_gf = gf["Ngf_vol"] * gf["V_gf"] if USE_INTERGRANULAR_MODEL else 0.0
        retained_ig = c_av + mb_av + md_av
        retained_all = retained_ig + q_gf

        hist["time"].append(t)
        hist["burnup_percent_fima"].append(time_to_burnup_percent(t, p.fission_rate, p.lattice_parameter))
        hist["rho_d"].append(p.rho_d)
        hist["rho_d_next"].append(p.rho_d if rho_next is None else rho_next)
        hist["delta_Nd_rho"].append(delta_Nd_rho)
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
        hist["retained"].append(retained_all)
        hist["retained_ig"].append(retained_ig)
        hist["q_gb"].append(q_gb)
        hist["q_gf"].append(q_gf)
        hist["q_rel"].append(q_rel)
        hist["swelling_b"].append(N_b * V_b)
        hist["swelling_d"].append(N_d * V_d)
        hist["swelling_gf"].append(swelling_gf)
        hist["swelling_ig"].append(N_b * V_b + N_d * V_d)
        hist["swelling_total_gas"].append(N_b * V_b + N_d * V_d + swelling_gf)
        hist["Ngf_areal"].append(gf["Ngf_areal"])
        hist["Ngf_vol"].append(gf["Ngf_vol"])
        hist["ng_gf"].append(gf["ng_gf"])
        hist["nv_gf"].append(gf["nv_gf"])
        hist["V_gf"].append(gf["V_gf"])
        hist["R_gf"].append(gf["R_gf"])
        hist["A_gf"].append(gf["A_gf"])
        hist["Fc_gf"].append(gf["Fc"])
        hist["p_gf"].append(gf["p_gf"])
        hist["p_gf_eq"].append(gf["p_gf_eq"])
        hist["zeta_gf"].append(gf["zeta_gf"])
        hist["gf_saturated"].append(gf.get("gf_saturated", 0.0))
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
        hist["grainface_gas_percent"].append(100.0 * q_gf / generated if generated > 0.0 else 0.0)
        hist["release_gas_percent"].append(100.0 * q_rel / generated if generated > 0.0 else 0.0)

    append_state()
    last_rates = {}
    n_steps = int(math.ceil(p.final_time / p.dt))

    for _ in range(n_steps):
        dt = min(p.dt, p.final_time - t)
        if dt <= 0.0:
            break

        burnup_old = time_to_burnup_percent(t, p.fission_rate, p.lattice_parameter)
        burnup_new = time_to_burnup_percent(t + dt, p.fission_rate, p.lattice_parameter)
        rho_old = rho_d_for_time_burnup(p, burnup_old)
        p.rho_d = rho_old

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
        Dv_d = Dv * p.Dv_dislocation_scale
        b_b, b_d = resolution_rates_UN(p, R_b, R_d)
        g_b, g_d, trapping_parts = trapping_rates_UN(p, Dg, R_b, Nb_old, R_d, Nd_old)

        nu_b = nucleation_rate_bulk(p, Dg, c_old)
        phi_b_raw = phi_population(mb_old, Nb_old)
        # Unified L3 model-form control; 1.0 reproduces the original phi closure.
        phi_b = max(float(PHI_B_SCALE_CURRENT), 0.0) * phi_b_raw
        phi_d = phi_population(md_old, Nd_old)

        if USE_PHI_GAS_RESOLUTION:
            b_b_gas = b_b * phi_b
            b_d_gas = b_d * phi_d
        else:
            b_b_gas = b_b
            b_d_gas = b_d

        delta_Nb_nucleated = max(dt * nu_b, 0.0)
        N_b = (Nb_old + delta_Nb_nucleated) / (1.0 + dt * b_b * phi_b)
        N_b = max(N_b, p.min_number_density)

        if USE_EQUILIBRIUM_DIMER_SEED and SEED_NEW_BULK_BUBBLES:
            source_c = beta - SEED_GAS_ATOMS_PER_BUBBLE * nu_b
            source_mb = SEED_GAS_ATOMS_PER_BUBBLE * nu_b
        elif USE_NUCLEATION_MASS_COUPLING:
            n0_eff = max(float(NUCLEATION_CLUSTER_SIZE_CURRENT), 0.0)
            source_c = beta - n0_eff * nu_b
            source_mb = n0_eff * nu_b
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

        nvb_before_vacancy_update = nvb
        nvd_before_vacancy_update = nvd
        nvb_seed_added = 0.0
        nvd_seed_added = 0.0
        bulk_first_gas_seed_applied = False
        dislocation_first_gas_seed_applied = False

        if (
            USE_FIRST_GAS_PRESSURE_SEED
            and SEED_BULK_ON_FIRST_GAS
            and (not bulk_first_gas_seed_done)
            and N_b > 0.0
            and mb_old <= 0.0
            and mb_new > 0.0
        ):
            nvb_target, _Rb_seed_from_gas = vacancies_for_pressure_factor_from_gas(
                p, mb_new, N_b, FIRST_GAS_PRESSURE_FACTOR
            )
            if nvb_target > nvb:
                nvb_seed_added = nvb_target - nvb
                nvb = nvb_target
            bulk_first_gas_seed_done = True
            bulk_first_gas_seed_applied = True

        if (
            USE_FIRST_GAS_PRESSURE_SEED
            and SEED_DISLOCATION_ON_FIRST_GAS
            and (not dislocation_first_gas_seed_done)
            and Nd_old > 0.0
            and md_old <= 0.0
            and md_new > 0.0
        ):
            nvd_target, _Rd_seed_from_gas = vacancies_for_pressure_factor_from_gas(
                p, md_new, Nd_old, FIRST_GAS_PRESSURE_FACTOR
            )
            if nvd_target > nvd:
                nvd_seed_added = nvd_target - nvd
                nvd = nvd_target
            dislocation_first_gas_seed_done = True
            dislocation_first_gas_seed_applied = True

        if p.update_bulk_vacancies and not bulk_first_gas_seed_applied:
            nvb, dnvb_dt = vacancy_concentration_implicit_step(p, Dv, R_b, N_b, mb_new, nvb, dt)
        else:
            dnvb_dt = (nvb - nvb_before_vacancy_update) / dt

        if not dislocation_first_gas_seed_applied:
            nvd, dnvd_dt = vacancy_concentration_implicit_step(p, Dv_d, R_d, Nd_old, md_new, nvd, dt)
        else:
            dnvd_dt = (nvd - nvd_before_vacancy_update) / dt

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

        # Coalescence of already-grown large/dislocation bubbles.
        lambda_d = coalescence_lambda(Vd_old, Nd_old)
        dVd_positive = max(V_d_growth - Vd_old, 0.0)
        if dVd_positive > 0.0 and Nd_old > 0.0:
            denominator = 1.0 + p.coalescence_d_scale * 4.0 * lambda_d * Nd_old * dVd_positive
            N_d = Nd_old / denominator
        else:
            N_d = Nd_old
        N_d = max(N_d, p.min_number_density)

        # Dynamic dislocation-density nucleation after coalescence.
        rho_next = rho_d_for_time_burnup(p, burnup_new)
        delta_Nd_rho = 0.0
        if USE_DYNAMIC_RHO_D_NUCLEATION and rho_old > 0.0 and N_d > 0.0:
            N_after_coalescence = N_d
            N_d = max(N_after_coalescence * (rho_next / rho_old), p.min_number_density)
            delta_Nd_rho = N_d - N_after_coalescence
        p.rho_d = rho_next

        V_b = (p.omega_fg * max(mb_new, 0.0) + omega_matrix(p) * nvb) / N_b if N_b > 0.0 else 0.0
        V_d = (p.omega_fg * max(md_new, 0.0) + omega_matrix(p) * nvd) / N_d if N_d > 0.0 else 0.0
        V_b = max(V_b, p.min_volume)
        V_d = max(V_d, p.min_volume)
        R_b = radius_from_volume(V_b)
        R_d = radius_from_volume(V_d)

        delta_Rd_cap = max(R_d - Rd_old, 0.0)
        delta_Vcap = 4.0 * math.pi * (Rd_old + Rb_old) ** 2 * delta_Rd_cap
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

            # Numerical solver correction, aligned with the unified L3 reference:
            # transfer bulk -> dislocation in each spectral mode, preserving
            # the existing radial distributions and total gas mode-by-mode.
            for imode in range(len(modes_mb)):
                dm_captured = f_cap * modes_mb[imode]
                modes_mb[imode] -= dm_captured
                modes_md[imode] += dm_captured
            # modes_c is unchanged by bulk-bubble capture.

            V_b = (p.omega_fg * max(mb_new, 0.0) + omega_matrix(p) * nvb) / N_b if N_b > 0.0 else 0.0
            V_d = (p.omega_fg * max(md_new, 0.0) + omega_matrix(p) * nvd) / N_d if N_d > 0.0 else 0.0
            V_b = max(V_b, p.min_volume)
            V_d = max(V_d, p.min_volume)
            R_b = radius_from_volume(V_b)
            R_d = radius_from_volume(V_d)

        generated += beta * dt
        retained_ig = max(c_new, 0.0) + max(mb_new, 0.0) + max(md_new, 0.0)
        q_gb_total_target = max(initial_gas + generated - retained_ig, 0.0)
        q_gb_before = gf["q_gf"] + gf["q_rel"] if USE_INTERGRANULAR_MODEL else q_gb
        dq_to_gb = max(q_gb_total_target - q_gb_before, 0.0)

        # Grain-boundary vacancy diffusivity used by the intergranular module.
        # 12test default uses the updated Miller/LANL Arrhenius expression.
        # The intragranular bulk vacancy diffusivity remains unchanged.
        Dv_gb = grain_boundary_vacancy_diffusivity(p, Dv_parts.get("Dv1", 0.0))

        if USE_INTERGRANULAR_MODEL:
            update_grainface_state(p, gf, dq_to_gb, dt, Dv_parts.get("Dv1", 0.0))
            q_gb = gf["q_gf"] + gf["q_rel"]
            retained = retained_ig + gf["q_gf"]
        else:
            q_gb = q_gb_total_target
            retained = retained_ig

        t += dt

        last_rates = {
            "Dg": Dg, "Dv": Dv, "Dv_dislocation": Dv_d, "Dv_gb": Dv_gb, "beta": beta,
            "rho_d_old": rho_old, "rho_d_next": rho_next, "delta_Nd_rho": delta_Nd_rho,
            "g_b": g_b, "g_d": g_d,
            "b_b": b_b, "b_d": b_d,
            "b_b_gas": b_b_gas, "b_d_gas": b_d_gas,
            "nu_b": nu_b, "phi_b": phi_b, "phi_d": phi_d,
            "lambda_d": lambda_d,
            "dVd_growth_dt": dVd_growth_dt,
            "dnvb_dt": dnvb_dt, "dnvd_dt": dnvd_dt,
            "dnv_gf_dt": gf.get("dnv_gf_dt", 0.0),
            "dA_gf_dt": gf.get("dA_gf_dt", 0.0),
            "dNgf_dt": gf.get("dNgf_dt", 0.0),
            "dq_gf_in_step": gf.get("dq_gf_in_step", 0.0),
            "dq_rel_step": gf.get("dq_rel_step", 0.0),
            "nvb_seed_added": locals().get("nvb_seed_added", 0.0),
            "nvd_seed_added": locals().get("nvd_seed_added", 0.0),
            "bulk_first_gas_seed_applied": float(locals().get("bulk_first_gas_seed_applied", False)),
            "dislocation_first_gas_seed_applied": float(locals().get("dislocation_first_gas_seed_applied", False)),
            "first_gas_pressure_factor": FIRST_GAS_PRESSURE_FACTOR,
            "seed_pressure_factor": SEED_PRESSURE_FACTOR,
            "seed_gas_atoms_per_bubble": SEED_GAS_ATOMS_PER_BUBBLE,
            "f_cap_step": f_cap,
            "cap_raw_step": cap_raw_step,
            "capture_fraction_sum": capture_fraction_sum,
            "capture_raw_sum": capture_raw_sum,
            "capture_bubbles_cumulative": capture_bubbles_cumulative,
            "max_f_cap_step": max_f_cap_step,
            **D_parts, **Dv_parts, **trapping_parts,
        }

        append_state(nu_b=nu_b, phi_b=phi_b, phi_d=phi_d, lambda_d=lambda_d, fcap=f_cap, capraw=cap_raw_step, delta_Nd_rho=delta_Nd_rho, rho_next=rho_next)

    if not keep_history:
        c_av = reconstruct_average(modes_c)
        mb_av = reconstruct_average(modes_mb)
        md_av = reconstruct_average(modes_md)
        p_b = pressure_internal(p, mb_av, nvb)
        p_d = pressure_internal(p, md_av, nvd)
        p_b_eq = pressure_equilibrium(p, R_b)
        p_d_eq = pressure_equilibrium(p, R_d)
        _refresh_grainface_derived(p, gf)
        q_gf = gf["q_gf"] if USE_INTERGRANULAR_MODEL else q_gb
        q_rel = gf["q_rel"] if USE_INTERGRANULAR_MODEL else 0.0
        swelling_gf = gf["Ngf_vol"] * gf["V_gf"] if USE_INTERGRANULAR_MODEL else 0.0
        retained_ig = c_av + mb_av + md_av
        hist = {
            "time": [t],
            "burnup_percent_fima": [time_to_burnup_percent(t, p.fission_rate, p.lattice_parameter)],
            "rho_d": [p.rho_d], "rho_d_next": [p.rho_d], "delta_Nd_rho": [last_rates.get("delta_Nd_rho", 0.0)],
            "c": [c_av], "mb": [mb_av], "md": [md_av],
            "Nb": [N_b], "Nd": [N_d],
            "Vb": [V_b], "Vd": [V_d],
            "Rb": [R_b], "Rd": [R_d],
            "nvb": [nvb], "nvd": [nvd],
            "generated": [generated],
            "retained": [retained_ig + q_gf],
            "retained_ig": [retained_ig],
            "q_gb": [q_gf + q_rel],
            "q_gf": [q_gf],
            "q_rel": [q_rel],
            "swelling_b": [N_b * V_b],
            "swelling_d": [N_d * V_d],
            "swelling_gf": [swelling_gf],
            "swelling_ig": [N_b * V_b + N_d * V_d],
            "swelling_total_gas": [N_b * V_b + N_d * V_d + swelling_gf],
            "Ngf_areal": [gf["Ngf_areal"]],
            "Ngf_vol": [gf["Ngf_vol"]],
            "ng_gf": [gf["ng_gf"]],
            "nv_gf": [gf["nv_gf"]],
            "V_gf": [gf["V_gf"]],
            "R_gf": [gf["R_gf"]],
            "A_gf": [gf["A_gf"]],
            "Fc_gf": [gf["Fc"]],
            "p_gf": [gf["p_gf"]], "p_gf_eq": [gf["p_gf_eq"]],
            "zeta_gf": [gf["zeta_gf"]], "gf_saturated": [gf.get("gf_saturated", 0.0)],
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
            "qgb_gas_percent": [100.0 * (q_gf + q_rel) / generated if generated > 0.0 else 0.0],
            "grainface_gas_percent": [100.0 * q_gf / generated if generated > 0.0 else 0.0],
            "release_gas_percent": [100.0 * q_rel / generated if generated > 0.0 else 0.0],
        }

    return hist, last_rates
