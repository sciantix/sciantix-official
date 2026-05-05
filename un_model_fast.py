"""Vectorised twin of un_model.py.

Identical public API (`UNParameters`, `run_model_point`, `solve_UN_fast`),
but the inner spectral mode loop is batched into a single
`np.linalg.solve` call on a (n_modes, 3, 3) tensor.

This is a non-destructive add-on: the original `un_model.py` is unchanged,
so existing Optuna SQLite studies remain numerically reproducible.

Numerical equivalence is checked by `un_model_parity.py` against
the reference implementation. Differences are at the 1e-10..1e-6 relative
level, driven by FP-summation order in `np.linalg.solve` (LAPACK LU)
versus the hand-rolled Cramer's rule in `un_model.solve3x3_cramer`.
"""

from __future__ import annotations

import math
from typing import Optional

import numpy as np

import un_model as base
from un_model import (
    DT_SCAN_H,
    FISSION_RATE,
    GRAIN_RADIUS,
    N_MODES_SCAN,
    UNParameters,
    XE_YIELD,
    beta_production,
    coalescence_lambda,
    initialize_vacancy_concentration,
    nucleation_rate_bulk,
    omega_matrix,
    phi_bulk,
    phi_population,
    pressure_equilibrium,
    pressure_internal,
    radius_from_volume,
    resolution_rates_UN,
    sphere_volume,
    time_to_burnup_percent,
    trapping_rates_UN,
    vacancy_concentration_implicit_step,
    vacancy_diffusivity_UN,
    xe_diffusivity_UN,
)


_PROJECTION_COEFF = -2.0 * math.sqrt(2.0 / math.pi)
_RECON_COEFF = _PROJECTION_COEFF / ((4.0 / 3.0) * math.pi)


def _n_arrays(n_modes: int):
    ns = np.arange(1, n_modes + 1, dtype=np.float64)
    n_coeffs = ((-1.0) ** ns) / ns
    return ns, n_coeffs


def initialize_modes_from_average_np(average: float, n_modes: int, n_iter: int = 20) -> np.ndarray:
    """Vectorised port of un_model.initialize_modes_from_average."""
    modes = np.zeros(n_modes, dtype=np.float64)
    proj = -math.sqrt(8.0 / math.pi)
    _, n_coeffs = _n_arrays(n_modes)
    remainder = float(average)
    inner_recon = proj * n_coeffs * 3.0 / (4.0 * math.pi)
    for _ in range(n_iter):
        modes += proj * n_coeffs * remainder
        remainder = float(average) - float(np.dot(inner_recon, modes))
    return modes


def reconstruct_average_np(modes: np.ndarray) -> float:
    n_modes = modes.shape[0]
    _, n_coeffs = _n_arrays(n_modes)
    return float(np.dot(_RECON_COEFF * n_coeffs, modes))


def sciantix_3x3_exchange_step_np(
    modes_c: np.ndarray,
    modes_mb: np.ndarray,
    modes_md: np.ndarray,
    Dg: float,
    R: float,
    beta: float,
    g_b: float,
    g_d: float,
    b_b: float,
    b_d: float,
    dt: float,
    source_mb: float = 0.0,
    source_md: float = 0.0,
):
    n_modes = modes_c.shape[0]
    ns, n_coeffs = _n_arrays(n_modes)
    diffusion_rate_coeff = math.pi ** 2 * Dg / (R * R)
    diffusion_rates = diffusion_rate_coeff * ns * ns

    A = np.empty((n_modes, 3, 3), dtype=np.float64)
    A[:, 0, 0] = 1.0 + (diffusion_rates + g_b + g_d) * dt
    A[:, 0, 1] = -b_b * dt
    A[:, 0, 2] = -b_d * dt
    A[:, 1, 0] = -g_b * dt
    A[:, 1, 1] = 1.0 + b_b * dt
    A[:, 1, 2] = 0.0
    A[:, 2, 0] = -g_d * dt
    A[:, 2, 1] = 0.0
    A[:, 2, 2] = 1.0 + b_d * dt

    rhs = np.empty((n_modes, 3), dtype=np.float64)
    proj_dt = _PROJECTION_COEFF * n_coeffs * dt
    rhs[:, 0] = modes_c + beta * proj_dt
    rhs[:, 1] = modes_mb + source_mb * proj_dt
    rhs[:, 2] = modes_md + source_md * proj_dt

    sol = np.linalg.solve(A, rhs[..., None])[..., 0]
    modes_c[:] = sol[:, 0]
    modes_mb[:] = sol[:, 1]
    modes_md[:] = sol[:, 2]
    return (
        reconstruct_average_np(modes_c),
        reconstruct_average_np(modes_mb),
        reconstruct_average_np(modes_md),
    )


def solve_UN_fast(p: UNParameters):
    """Vectorised port of un_model.solve_UN_sciantix_intragranular_extended.

    Identical control flow and physics; only the mode loop is batched.
    """
    modes_c = initialize_modes_from_average_np(p.c0, p.n_modes)
    modes_mb = initialize_modes_from_average_np(p.mb0, p.n_modes)
    modes_md = initialize_modes_from_average_np(p.md0, p.n_modes)
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
        "c": [reconstruct_average_np(modes_c)],
        "mb": [reconstruct_average_np(modes_mb)],
        "md": [reconstruct_average_np(modes_md)],
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
        c_new, mb_new, md_new = sciantix_3x3_exchange_step_np(
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
                modes_mb = initialize_modes_from_average_np(mb_new, p.n_modes)
                modes_md = initialize_modes_from_average_np(md_new, p.n_modes)
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


_RUN_CACHE: dict = {}


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
    """Drop-in replacement for un_model.run_model_point using the vectorised solver."""
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
    hist, rates = solve_UN_fast(p)
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
