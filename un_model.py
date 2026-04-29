"""UN intragranular fission-gas model extracted from 2UNpython_tests.ipynb."""

import math
from dataclasses import dataclass
from typing import Optional

FISSION_RATE = 5.0e19
GRAIN_RADIUS = 6.0e-6
XE_YIELD = 0.24
DT_SCAN_H = 12.0
N_MODES_SCAN = 25


@dataclass
class UNParameters:
    temperature: float = 1600.0
    fission_rate: float = FISSION_RATE
    grain_radius: float = GRAIN_RADIUS
    final_time: float = 24.0 * 3600.0
    target_burnup_percent_fima: Optional[float] = None
    dt: float = 60.0
    n_modes: int = 40
    xe_yield: float = XE_YIELD
    precursor_factor: float = 1.0
    D10: float = 1.56e-3
    Q1: float = 4.94
    A20: float = 1.21e-67
    B21: float = 25.87
    B22: float = -1.49
    B23: float = 0.0
    A30: float = 1.85e-39
    kB_eV: float = 8.617333262e-5
    kB_J: float = 1.380649e-23
    use_sciantix_D2_zero: bool = True
    D10_vU: float = 1.35e-2
    Q1_vU: float = 5.66
    B21_vU_refit: float = -0.62
    B22_vU_refit: float = -0.04
    A20_vU_fig4_refit: float = 4.6304523933553033e-29
    radius_in_lattice: float = 0.21e-9
    omega_fg: float = 8.5e-29
    lattice_parameter: float = 4.889e-10
    gamma_b: float = 1.11
    hydrostatic_stress: float = 0.0
    min_radius_for_pressure: float = 1.0e-15
    f_n: float = 1.0e-6
    rho_d: float = 3.0e13
    K_d: float = 5.0e5
    r_d: float = 3.46e-10
    Z_d: float = 5.0
    R_b: float = 0.0
    N_b: float = 0.0
    R_d: float = 0.0
    N_d: Optional[float] = None
    c0: float = 0.0
    mb0: float = 0.0
    md0: float = 0.0
    nvb0: Optional[float] = None
    nvd0: Optional[float] = None
    diffusivity_sf: float = 1.0
    trapping_sf: float = 1.0
    resolution_sf: float = 1.0
    g_d_scale: float = 1.0
    nucleation_gas_coupling: bool = False
    phi_resolution_mode: str = "none"
    bulk_dislocation_capture: bool = False
    vacancy_absorption_only: bool = True
    update_bulk_vacancies: bool = True
    min_number_density: float = 0.0
    min_volume: float = 0.0
    bulk_seed_radius_nm: float = 0.0

    def __post_init__(self):
        if self.N_d is None:
            self.N_d = self.K_d * self.rho_d
        if self.target_burnup_percent_fima is not None:
            self.final_time = burnup_percent_to_time(
                self.target_burnup_percent_fima,
                self.fission_rate,
                self.lattice_parameter,
            )


def omega_matrix(p):
    return p.lattice_parameter**3 / 4.0


def uranium_atom_density_from_lattice(lattice_parameter):
    return 4.0 / lattice_parameter**3


def burnup_percent_to_time(burnup_percent_fima, fission_rate, lattice_parameter):
    if fission_rate <= 0.0:
        raise ValueError("fission_rate must be positive to compute time from burnup.")
    burnup_fraction_fima = burnup_percent_fima / 100.0
    uranium_density = uranium_atom_density_from_lattice(lattice_parameter)
    return burnup_fraction_fima * uranium_density / fission_rate


def time_to_burnup_percent(time, fission_rate, lattice_parameter):
    uranium_density = uranium_atom_density_from_lattice(lattice_parameter)
    return 100.0 * fission_rate * time / uranium_density


def sphere_volume(R):
    if R <= 0.0:
        return 0.0
    return (4.0 / 3.0) * math.pi * R**3


def radius_from_volume(V):
    if V <= 0.0:
        return 0.0
    return (3.0 * V / (4.0 * math.pi)) ** (1.0 / 3.0)


def xe_diffusivity_UN(p):
    T = p.temperature
    F = p.fission_rate
    kBT = p.kB_eV * T
    D1 = p.D10 * math.exp(-p.Q1 / kBT)
    if p.use_sciantix_D2_zero:
        D2 = 0.0
    else:
        D2 = p.A20 * math.sqrt(F) * math.exp(
            -p.B21 / kBT - p.B22 / (kBT**2) - p.B23 / (kBT**3)
        )
    D3 = p.A30 * F
    Dg = (D1 + D2 + D3) * p.diffusivity_sf * p.precursor_factor
    return Dg, {"D1": D1, "D2": D2, "D3": D3, "Dg": Dg}


def vacancy_diffusivity_UN(p):
    T = p.temperature
    F = p.fission_rate
    kBT = p.kB_eV * T
    D1 = p.D10_vU * math.exp(-p.Q1_vU / kBT)
    D2 = math.sqrt(F) * p.A20_vU_fig4_refit * math.exp(
        p.B21_vU_refit / kBT + p.B22_vU_refit / (kBT**2)
    )
    Dv = D1 + D2
    return Dv, {"Dv1": D1, "Dv2": D2, "Dv": Dv}


def b0_resolution(R):
    R = max(R, 1.0e-15)
    return 1.0e-25 * (2.64 - 2.02 * math.exp(-2.61e-9 / R))


def resolution_rates_UN(p, R_b, R_d):
    Rb_eff = R_b + p.radius_in_lattice
    Rd_eff = R_d + p.radius_in_lattice
    b_b = p.fission_rate * b0_resolution(Rb_eff) * p.resolution_sf
    b_d = p.fission_rate * b0_resolution(Rd_eff) * p.resolution_sf
    return b_b, b_d


def trapping_rates_UN(p, Dg, R_b, N_b, R_d, N_d):
    Rb_eff = R_b + p.radius_in_lattice
    Rd_eff = R_d + p.radius_in_lattice
    g_b = 0.0 if N_b <= 0.0 else 4.0 * math.pi * Dg * Rb_eff * N_b
    Gamma_d = 1.0 / math.sqrt(math.pi * p.rho_d)
    den = math.log(Gamma_d / (p.Z_d * p.r_d)) - 3.0 / 5.0
    if den <= 0.0:
        raise ValueError(f"Invalid dislocation sink denominator: {den:g}")
    free_dislocation = max(p.rho_d - 2.0 * R_d * N_d, 0.0)
    term_bubbles = 4.0 * math.pi * Dg * Rd_eff * N_d
    term_dislocation = (2.0 * math.pi * Dg / den) * free_dislocation
    g_d_unscaled = term_bubbles + term_dislocation
    g_d = p.g_d_scale * g_d_unscaled
    return p.trapping_sf * g_b, p.trapping_sf * g_d, {
        "Gamma_d": Gamma_d,
        "den": den,
        "free_dislocation": free_dislocation,
        "term_bubbles": term_bubbles,
        "term_dislocation": term_dislocation,
        "g_d_unscaled": g_d_unscaled,
        "g_d_scale": p.g_d_scale,
    }


def beta_production(p):
    return p.xe_yield * p.fission_rate


def nucleation_rate_bulk(p, Dg, c):
    return 8.0 * math.pi * p.f_n * Dg * p.omega_fg ** (1.0 / 3.0) * max(c, 0.0) ** 2


def phi_bulk(mb, Nb):
    if Nb <= 0.0:
        return 0.0
    atoms_per_bubble = mb / Nb
    if atoms_per_bubble <= 1.0:
        return 0.0
    return 1.0 / (atoms_per_bubble - 1.0)


def phi_population(m_gas, N):
    if N <= 0.0:
        return 0.0
    atoms_per_bubble = m_gas / N
    if atoms_per_bubble <= 1.0:
        return 0.0
    return 1.0 / (atoms_per_bubble - 1.0)


def coalescence_lambda(Vd, Nd):
    xi = max(0.0, min(Vd * Nd, 0.999999))
    return (2.0 - xi) / (2.0 * (1.0 - xi) ** 3)


def pressure_internal(p, m_gas, n_vac):
    if m_gas <= 0.0:
        return 0.0
    if n_vac <= 0.0:
        return math.inf
    denominator = n_vac * omega_matrix(p)
    if denominator <= 0.0:
        return math.inf
    return p.kB_J * p.temperature * m_gas / denominator


def pressure_equilibrium(p, R):
    R_eff = max(R, p.min_radius_for_pressure)
    return 2.0 * p.gamma_b / R_eff - p.hydrostatic_stress


def gas_only_radius_for_population(p, m_gas, N):
    if m_gas <= 0.0 or N <= 0.0:
        return 0.0
    return radius_from_volume(p.omega_fg * m_gas / N)


def radius_for_vacancy_update(p, R_old, N, m_gas):
    if R_old > 0.0:
        return R_old
    return gas_only_radius_for_population(p, m_gas, N)


def pressure_equilibrium_for_vacancy_update(p, R_update):
    if R_update <= 0.0:
        return math.inf
    return 2.0 * p.gamma_b / R_update - p.hydrostatic_stress


def wigner_seitz_delta(N):
    return (3.0 / (4.0 * math.pi * max(N, 1.0))) ** (1.0 / 3.0)


def zeta_geometry(R, N):
    delta = wigner_seitz_delta(N)
    psi = max(R / delta, 1.0e-12)
    den = -psi**6 + 5.0 * psi**2 - 9.0 * psi + 5.0
    if den <= 1.0e-30:
        den = 1.0e-30
    return max(10.0 * psi * (1.0 + psi**3) / den, 1.0e-30)


def vacancy_concentration_implicit_step(p, Dv, R, N, m_gas, n_old, dt):
    if N <= 0.0 or m_gas <= 0.0:
        return n_old, 0.0
    R_update = radius_for_vacancy_update(p, R, N, m_gas)
    if R_update <= 0.0:
        return n_old, 0.0
    p_eq = pressure_equilibrium_for_vacancy_update(p, R_update)
    p_int_old = pressure_internal(p, m_gas, n_old)
    if p.vacancy_absorption_only and p_int_old <= p_eq:
        return n_old, 0.0
    delta = wigner_seitz_delta(N)
    zeta = zeta_geometry(R_update, N)
    A = 2.0 * math.pi * Dv * delta * N / (p.kB_J * p.temperature * zeta)
    C = p.kB_J * p.temperature * m_gas / omega_matrix(p)
    B = n_old - dt * A * p_eq
    discriminant = B * B + 4.0 * dt * A * C
    if discriminant < 0.0:
        raise ValueError(f"Negative discriminant in vacancy implicit step: {discriminant:g}")
    sqrt_disc = math.sqrt(discriminant)
    if B >= 0.0:
        n_new = 0.5 * (B + sqrt_disc)
    else:
        denominator = sqrt_disc - B
        n_new = 0.0 if denominator <= 0.0 else (2.0 * dt * A * C) / denominator
    if p.vacancy_absorption_only:
        n_new = max(n_new, n_old)
    return n_new, (n_new - n_old) / dt


def initialize_vacancy_concentration(p, N, R, m_gas):
    if N <= 0.0 or R <= 0.0:
        return 0.0
    total_bubble_volume = N * sphere_volume(R)
    gas_volume = p.omega_fg * m_gas
    vacancy_volume = max(total_bubble_volume - gas_volume, 0.0)
    return vacancy_volume / omega_matrix(p)


def initialize_modes_from_average(average, n_modes, n_iter=20):
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


def reconstruct_average(modes):
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
        raise ZeroDivisionError("Singular 3x3 system in spectral mode solve")
    Ax = [[b[i], A[i][1], A[i][2]] for i in range(3)]
    Ay = [[A[i][0], b[i], A[i][2]] for i in range(3)]
    Az = [[A[i][0], A[i][1], b[i]] for i in range(3)]
    return [det3(Ax) / detA, det3(Ay) / detA, det3(Az) / detA]


def sciantix_3x3_exchange_step(
    modes_c,
    modes_mb,
    modes_md,
    Dg,
    R,
    beta,
    g_b,
    g_d,
    b_b,
    b_d,
    dt,
    source_mb=0.0,
    source_md=0.0,
):
    projection_coeff = -2.0 * math.sqrt(2.0 / math.pi)
    diffusion_rate_coeff = math.pi**2 * Dg / R**2
    for i in range(len(modes_c)):
        n = i + 1
        n_coeff = (-1.0) ** n / n
        diffusion_rate = diffusion_rate_coeff * n**2
        source_rate_c = projection_coeff * beta * n_coeff
        source_rate_mb = projection_coeff * source_mb * n_coeff
        source_rate_md = projection_coeff * source_md * n_coeff
        A = [
            [1.0 + (diffusion_rate + g_b + g_d) * dt, -b_b * dt, -b_d * dt],
            [-g_b * dt, 1.0 + b_b * dt, 0.0],
            [-g_d * dt, 0.0, 1.0 + b_d * dt],
        ]
        rhs = [
            modes_c[i] + source_rate_c * dt,
            modes_mb[i] + source_rate_mb * dt,
            modes_md[i] + source_rate_md * dt,
        ]
        modes_c[i], modes_mb[i], modes_md[i] = solve3x3_cramer(A, rhs)
    return reconstruct_average(modes_c), reconstruct_average(modes_mb), reconstruct_average(modes_md)


def solve_UN_sciantix_intragranular_extended(p):
    modes_c = initialize_modes_from_average(p.c0, p.n_modes)
    modes_mb = initialize_modes_from_average(p.mb0, p.n_modes)
    modes_md = initialize_modes_from_average(p.md0, p.n_modes)
    R_b, R_d, N_b, N_d = p.R_b, p.R_d, p.N_b, p.N_d
    V_b, V_d = sphere_volume(R_b), sphere_volume(R_d)
    nvb = initialize_vacancy_concentration(p, N_b, R_b, p.mb0) if p.nvb0 is None else p.nvb0
    nvd = initialize_vacancy_concentration(p, N_d, R_d, p.md0) if p.nvd0 is None else p.nvd0
    beta = beta_production(p)
    initial_gas = p.c0 + p.mb0 + p.md0
    generated = 0.0
    q_gb = 0.0
    retained = initial_gas
    seed_vacancies_added_total = 0.0
    hist = {
        "time": [0.0],
        "burnup_percent_fima": [time_to_burnup_percent(0.0, p.fission_rate, p.lattice_parameter)],
        "c": [reconstruct_average(modes_c)],
        "mb": [reconstruct_average(modes_mb)],
        "md": [reconstruct_average(modes_md)],
        "Nb": [N_b],
        "Nd": [N_d],
        "Vb": [V_b],
        "Vd": [V_d],
        "Rb": [R_b],
        "Rd": [R_d],
        "nvb": [nvb],
        "nvd": [nvd],
        "generated": [generated],
        "retained": [retained],
        "q_gb": [q_gb],
        "qdot_gb": [0.0],
        "swelling_b": [N_b * V_b],
        "swelling_d": [N_d * V_d],
        "swelling_ig": [N_b * V_b + N_d * V_d],
        "p_b": [pressure_internal(p, p.mb0, nvb)],
        "p_d": [pressure_internal(p, p.md0, nvd)],
        "p_b_eq": [pressure_equilibrium(p, R_b)],
        "p_d_eq": [pressure_equilibrium(p, R_d)],
        "lambda_d": [coalescence_lambda(V_d, N_d)],
        "nu_b": [0.0],
        "phi_b": [0.0],
        "dnvb_dt": [0.0],
        "dnvd_dt": [0.0],
        "dVd_growth_dt": [0.0],
        "seed_vacancies_added_total": [0.0],
    }
    last_rates = {}
    n_steps = int(math.ceil(p.final_time / p.dt))
    for _ in range(n_steps):
        t_old = hist["time"][-1]
        dt = min(p.dt, p.final_time - t_old)
        if dt <= 0.0:
            break
        c_old, mb_old, md_old = hist["c"][-1], hist["mb"][-1], hist["md"][-1]
        Nb_old, Nd_old, Vd_old = N_b, N_d, V_d
        Dg, D_parts = xe_diffusivity_UN(p)
        Dv, Dv_parts = vacancy_diffusivity_UN(p)
        b_b, b_d = resolution_rates_UN(p, R_b, R_d)
        g_b_old, g_d, trapping_parts_old = trapping_rates_UN(p, Dg, R_b, Nb_old, R_d, Nd_old)
        nu_b = nucleation_rate_bulk(p, Dg, c_old)
        phi_b = phi_bulk(mb_old, Nb_old)
        N_b = max((Nb_old + dt * nu_b) / (1.0 + dt * b_b * phi_b), p.min_number_density)
        dN_b_positive = max(N_b - Nb_old, 0.0)
        seed_vacancies_added_step = 0.0
        if dN_b_positive > 0.0 and p.bulk_seed_radius_nm > 0.0:
            V_seed = sphere_volume(p.bulk_seed_radius_nm * 1.0e-9)
            seed_vacancies_added_step = dN_b_positive * V_seed / omega_matrix(p)
            nvb += seed_vacancies_added_step
            seed_vacancies_added_total += seed_vacancies_added_step
        # SCIANTIX-like operator splitting: gas solver uses old-step bulk trapping.
        g_b = g_b_old
        trapping_parts = dict(trapping_parts_old)
        trapping_parts["g_b_old_before_Nb_update"] = g_b_old
        trapping_parts["g_b_used_after_Nb_update"] = g_b
        trapping_parts["Nb_old"] = Nb_old
        trapping_parts["Nb_used_for_gb"] = Nb_old
        phi_d = phi_population(md_old, Nd_old)
        b_b_gas = b_b
        b_d_gas = b_d
        if p.phi_resolution_mode in ("bulk_only", "bulk_and_dislocation"):
            b_b_gas = b_b * phi_b
        if p.phi_resolution_mode == "bulk_and_dislocation":
            b_d_gas = b_d * phi_d
        nucleation_source = 2.0 * nu_b if p.nucleation_gas_coupling else 0.0
        c_new, mb_new, md_new = sciantix_3x3_exchange_step(
            modes_c,
            modes_mb,
            modes_md,
            Dg,
            p.grain_radius,
            beta - nucleation_source,
            g_b,
            g_d,
            b_b_gas,
            b_d_gas,
            dt,
            source_mb=nucleation_source,
        )
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
            V_b_growth = V_b
        if Nd_old > 0.0:
            dVd_growth_dt = p.omega_fg / Nd_old * dmd_dt + omega_matrix(p) / Nd_old * dnvd_dt
            V_d_growth = max(V_d + dt * dVd_growth_dt, p.min_volume)
        else:
            dVd_growth_dt = 0.0
            V_d_growth = V_d
        capture_fraction = 0.0
        captured_bubbles = 0.0
        if p.bulk_dislocation_capture and N_b > 0.0 and Nd_old > 0.0:
            Rb_growth = radius_from_volume(V_b_growth)
            Rd_growth = radius_from_volume(V_d_growth)
            dV_capture = max(sphere_volume(Rb_growth + Rd_growth) - sphere_volume(R_b + R_d), 0.0)
            captured_bubbles = min(max(Nd_old * N_b * dV_capture, 0.0), N_b)
            capture_fraction = max(0.0, min(captured_bubbles / N_b, 1.0))
            if capture_fraction > 0.0:
                mb_transfer = capture_fraction * mb_new
                nvb_transfer = capture_fraction * nvb
                mb_new -= mb_transfer
                md_new += mb_transfer
                nvb -= nvb_transfer
                nvd += nvb_transfer
                N_b = max(N_b - captured_bubbles, p.min_number_density)
                modes_mb = initialize_modes_from_average(mb_new, p.n_modes)
                modes_md = initialize_modes_from_average(md_new, p.n_modes)
        lambda_d = coalescence_lambda(Vd_old, Nd_old)
        dVd_positive = max(V_d_growth - Vd_old, 0.0)
        if dVd_positive > 0.0 and Nd_old > 0.0:
            N_d = Nd_old / (1.0 + 4.0 * lambda_d * Nd_old * dVd_positive)
        else:
            N_d = Nd_old
        N_d = max(N_d, p.min_number_density)
        V_b = (p.omega_fg * max(mb_new, 0.0) + omega_matrix(p) * nvb) / N_b if N_b > 0.0 else 0.0
        V_d = (p.omega_fg * max(md_new, 0.0) + omega_matrix(p) * nvd) / N_d if N_d > 0.0 else 0.0
        V_b, V_d = max(V_b, p.min_volume), max(V_d, p.min_volume)
        R_b, R_d = radius_from_volume(V_b), radius_from_volume(V_d)
        generated += beta * dt
        retained = c_new + mb_new + md_new
        q_gb_old = q_gb
        q_gb = max(initial_gas + generated - retained, 0.0)
        qdot_gb = max((q_gb - q_gb_old) / dt, 0.0)
        hist["time"].append(t_old + dt)
        hist["burnup_percent_fima"].append(time_to_burnup_percent(t_old + dt, p.fission_rate, p.lattice_parameter))
        hist["c"].append(c_new)
        hist["mb"].append(mb_new)
        hist["md"].append(md_new)
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
        hist["qdot_gb"].append(qdot_gb)
        hist["swelling_b"].append(N_b * V_b)
        hist["swelling_d"].append(N_d * V_d)
        hist["swelling_ig"].append(N_b * V_b + N_d * V_d)
        hist["p_b"].append(pressure_internal(p, mb_new, nvb))
        hist["p_d"].append(pressure_internal(p, md_new, nvd))
        hist["p_b_eq"].append(pressure_equilibrium(p, R_b))
        hist["p_d_eq"].append(pressure_equilibrium(p, R_d))
        hist["lambda_d"].append(lambda_d)
        hist["nu_b"].append(nu_b)
        hist["phi_b"].append(phi_b)
        hist["dnvb_dt"].append(dnvb_dt)
        hist["dnvd_dt"].append(dnvd_dt)
        hist["dVd_growth_dt"].append(dVd_growth_dt)
        hist["seed_vacancies_added_total"].append(seed_vacancies_added_total)
        last_rates = {
            "Dg": Dg,
            "Dv": Dv,
            "beta": beta,
            "g_b": g_b,
            "g_b_old_before_Nb_update": g_b_old,
            "g_d": g_d,
            "b_b": b_b,
            "b_d": b_d,
            "b_b_gas": b_b_gas,
            "b_d_gas": b_d_gas,
            "nu_b": nu_b,
            "nucleation_source": nucleation_source,
            "phi_b": phi_b,
            "phi_d": phi_d,
            "capture_fraction": capture_fraction,
            "captured_bubbles": captured_bubbles,
            "lambda_d": lambda_d,
            "dVd_growth_dt": dVd_growth_dt,
            "dnvb_dt": dnvb_dt,
            "dnvd_dt": dnvd_dt,
            "seed_vacancies_added_step": seed_vacancies_added_step,
            "seed_vacancies_added_total": seed_vacancies_added_total,
            "p_b": pressure_internal(p, mb_new, nvb),
            "p_d": pressure_internal(p, md_new, nvd),
            "p_b_eq": pressure_equilibrium(p, R_b),
            "p_d_eq": pressure_equilibrium(p, R_d),
            "Nb_old": Nb_old,
            "Nb_updated_before_gas": N_b,
            "Nd_old": Nd_old,
            "Nd_after_coalescence": N_d,
            **D_parts,
            **Dv_parts,
            **trapping_parts,
        }
    return hist, last_rates


_RUN_CACHE = {}


def clear_run_cache():
    _RUN_CACHE.clear()


def run_model_point(
    T,
    burnup,
    f_n,
    K_d=5.0e5,
    g_d_scale=1.0,
    bulk_seed_radius_nm=0.0,
    dt_h=DT_SCAN_H,
    n_modes=N_MODES_SCAN,
    fission_rate=FISSION_RATE,
    grain_radius=GRAIN_RADIUS,
    xe_yield=XE_YIELD,
    nucleation_gas_coupling=False,
    phi_resolution_mode="none",
    bulk_dislocation_capture=False,
    include_history=False,
):
    key = (
        round(float(T), 6),
        round(float(burnup), 6),
        float(f_n),
        float(K_d),
        float(g_d_scale),
        float(bulk_seed_radius_nm),
        float(dt_h),
        int(n_modes),
        float(fission_rate),
        float(grain_radius),
        float(xe_yield),
        bool(nucleation_gas_coupling),
        str(phi_resolution_mode),
        bool(bulk_dislocation_capture),
        bool(include_history),
    )
    if key in _RUN_CACHE:
        return _RUN_CACHE[key]
    p = UNParameters(
        temperature=float(T),
        fission_rate=float(fission_rate),
        grain_radius=float(grain_radius),
        target_burnup_percent_fima=float(burnup),
        dt=float(dt_h) * 3600.0,
        n_modes=int(n_modes),
        xe_yield=float(xe_yield),
        f_n=float(f_n),
        K_d=float(K_d),
        g_d_scale=float(g_d_scale),
        nucleation_gas_coupling=bool(nucleation_gas_coupling),
        phi_resolution_mode=str(phi_resolution_mode),
        bulk_dislocation_capture=bool(bulk_dislocation_capture),
        R_b=0.0,
        N_b=0.0,
        R_d=0.0,
        N_d=None,
        c0=0.0,
        mb0=0.0,
        md0=0.0,
        vacancy_absorption_only=True,
        update_bulk_vacancies=True,
        bulk_seed_radius_nm=float(bulk_seed_radius_nm),
    )
    hist, rates = solve_UN_sciantix_intragranular_extended(p)
    generated = hist["generated"][-1]
    if generated > 0.0:
        matrix_gas_percent = 100.0 * hist["c"][-1] / generated
        bulk_gas_percent = 100.0 * hist["mb"][-1] / generated
        dislocation_gas_percent = 100.0 * hist["md"][-1] / generated
        qgb_gas_percent = 100.0 * hist["q_gb"][-1] / generated
    else:
        matrix_gas_percent = bulk_gas_percent = dislocation_gas_percent = qgb_gas_percent = 0.0
    p_b_eq = hist["p_b_eq"][-1]
    p_d_eq = hist["p_d_eq"][-1]
    row = {
        "T": float(T),
        "burnup": float(burnup),
        "f_n": float(f_n),
        "K_d": float(K_d),
        "g_d_scale": float(g_d_scale),
        "bulk_seed_radius_nm": float(bulk_seed_radius_nm),
        "nucleation_gas_coupling": bool(nucleation_gas_coupling),
        "phi_resolution_mode": str(phi_resolution_mode),
        "bulk_dislocation_capture": bool(bulk_dislocation_capture),
        "swelling_b_percent": 100.0 * hist["swelling_b"][-1],
        "swelling_d_percent": 100.0 * hist["swelling_d"][-1],
        "swelling_ig_percent": 100.0 * hist["swelling_ig"][-1],
        "Nb": hist["Nb"][-1],
        "Nd": hist["Nd"][-1],
        "Rb_nm": hist["Rb"][-1] * 1.0e9,
        "Rd_nm": hist["Rd"][-1] * 1.0e9,
        "p_b": hist["p_b"][-1],
        "p_b_eq": p_b_eq,
        "p_d": hist["p_d"][-1],
        "p_d_eq": p_d_eq,
        "p_b_over_eq": hist["p_b"][-1] / p_b_eq if p_b_eq > 0.0 else math.nan,
        "p_d_over_eq": hist["p_d"][-1] / p_d_eq if p_d_eq > 0.0 else math.nan,
        "matrix_gas_percent": matrix_gas_percent,
        "bulk_gas_percent": bulk_gas_percent,
        "dislocation_gas_percent": dislocation_gas_percent,
        "qgb_gas_percent": qgb_gas_percent,
        "Dg": rates.get("Dg", math.nan),
        "Dv": rates.get("Dv", math.nan),
        "g_b": rates.get("g_b", math.nan),
        "g_d": rates.get("g_d", math.nan),
        "g_d_unscaled": rates.get("g_d_unscaled", math.nan),
        "b_b_gas": rates.get("b_b_gas", math.nan),
        "b_d_gas": rates.get("b_d_gas", math.nan),
        "capture_fraction": rates.get("capture_fraction", math.nan),
    }
    if include_history:
        row["hist"] = hist
        row["rates"] = rates
    _RUN_CACHE[key] = row
    return row
