"""Factories: turn RIZK_CONSTANTS + MANUAL_PARAMS into model-level objects.

Public API:
    build_candidate(manual_params, rizk_constants) -> un_model.Candidate
    build_un_params(T, bu, cand, dt_h, n_modes, manual_params, rizk_constants) -> UNParameters
    model_runner(T, bu, cand, dt_h, n_modes, keep_history) -> dict

`model_runner` is the high-level entry point used by all plotting scripts:
it builds UNParameters, calls solve_UN, and returns a flat dict of the
last-step quantities (swelling %, N_b, N_d, R_b, R_d, gas partition, etc.).
"""

import math

import un_model as m

from rizk_constants import RIZK_CONSTANTS as DEFAULT_RIZK_CONSTANTS
from manual_params import MANUAL_PARAMS as DEFAULT_MANUAL_PARAMS
from rho_d_laws import resolve_rho_d


def build_candidate(manual_params=None, rizk_constants=None):
    """Build a Candidate (frozen container of free-fit + scale parameters)."""
    mp = manual_params if manual_params is not None else DEFAULT_MANUAL_PARAMS
    rk = rizk_constants if rizk_constants is not None else DEFAULT_RIZK_CONSTANTS
    return m.Candidate(
        label=mp["label"], f_n=mp["f_n"],
        K_d=rk["K_D"], rho_d=mp["rho_d"],
        fission_rate=mp["fission_rate"],
        Dv_scale=mp["Dv_scale"], Dv_D1_scale=mp["Dv_D1_scale"], Dv_D3_scale=mp["Dv_D3_scale"],
        Dg_scale=mp["Dg_scale"], Dg_D1_scale=mp["Dg_D1_scale"], Dg_D3_scale=mp["Dg_D3_scale"],
        b_scale=mp["b_scale"], gb_scale=mp["gb_scale"], gd_scale=mp["gd_scale"],
        b_bulk_scale=mp["b_bulk_scale"], b_dislocation_scale=mp["b_dislocation_scale"],
        gd_bubble_scale=mp["gd_bubble_scale"], gd_line_scale=mp["gd_line_scale"],
        gd_line_alpha=mp["gd_line_alpha"],
        coalescence_d_scale=mp["coalescence_d_scale"],
        capture_scale=mp["capture_scale"],
        D2_xe_scale=mp["D2_xe_scale"],
    )


def build_un_params(T, bu, cand=None, dt_h=12.0, n_modes=22,
                    manual_params=None, rizk_constants=None):
    """Build UNParameters for a run at (T [K], bu [% FIMA]).

    All Rizk constants flow from `rizk_constants` (default: RIZK_CONSTANTS).
    The candidate carries free-fit + scale parameters; if None, build a default
    candidate from `manual_params` (default: MANUAL_PARAMS).
    """
    mp = manual_params if manual_params is not None else DEFAULT_MANUAL_PARAMS
    rk = rizk_constants if rizk_constants is not None else DEFAULT_RIZK_CONSTANTS
    c = cand if cand is not None else build_candidate(mp, rk)

    rho_d_actual = resolve_rho_d(T, bu, mp)

    return m.UNParameters(
        temperature=float(T),
        target_burnup_percent_fima=float(bu),
        dt=float(dt_h) * 3600.0,
        n_modes=int(n_modes),
        # Geometry / matrix
        grain_radius=rk["GRAIN_RADIUS"],
        lattice_parameter=rk["LATTICE_PARAMETER"],
        omega_fg=rk["OMEGA_FG"],
        radius_in_lattice=rk["RADIUS_IN_LATTICE"],
        gamma_b=rk["GAMMA_B"],
        xe_yield=rk["XE_YIELD"],
        # Dislocations
        r_d=rk["R_D_LINE"],
        Z_d=rk["Z_D"],
        K_d=rk["K_D"],
        # Xe diffusivity
        D10=rk["D10_XE"], Q1=rk["Q1_XE"],
        A20_xe=rk["A20_XE"], B21_xe=rk["B21_XE"],
        B22_xe=rk["B22_XE"], B23_xe=rk["B23_XE"],
        A30=rk["A30_XE"],
        # Vacancy diffusivity (D1 thermal + D3 athermal; D2 dropped)
        D10_vU=rk["D10_VU"], Q1_vU=rk["Q1_VU"],
        A30_vU=rk["A30_VU"],
        # Resolution shape b0(R)  (Rizk Eq. 8)
        b0_prefactor=rk["B0_PREFACTOR"],
        b0_a1=rk["B0_A1"],
        b0_a2=rk["B0_A2"],
        b0_b1=rk["B0_B1"],
        # Inter-granular bubbles (Rizk 2025 §A.2)
        N_gf_0=rk["N_GF_0"],
        D_vgb_ratio=rk["D_VGB_RATIO"],
        delta_gb=rk["DELTA_GB"],
        R_gf_0=rk["R_GF_0"],
        F_c_sat=rk["F_C_SAT"],
        theta_rad=math.radians(rk["THETA_DEG"]),
        # Boltzmann
        kB_eV=rk["KB_EV"], kB_J=rk["KB_J"],
        # Free-fit / candidate
        fission_rate=c.fission_rate,
        f_n=c.f_n,
        rho_d=rho_d_actual,
        # Scales
        Dv_scale=c.Dv_scale, Dg_scale=c.Dg_scale,
        Dg_D1_scale=c.Dg_D1_scale, Dg_D3_scale=c.Dg_D3_scale,
        Dv_D1_scale=c.Dv_D1_scale, Dv_D3_scale=c.Dv_D3_scale,
        D2_xe_scale=c.D2_xe_scale,
        b_scale=c.b_scale,
        b_bulk_scale=c.b_bulk_scale, b_dislocation_scale=c.b_dislocation_scale,
        gb_scale=c.gb_scale, gd_scale=c.gd_scale,
        gd_bubble_scale=c.gd_bubble_scale, gd_line_scale=c.gd_line_scale,
        gd_line_alpha=c.gd_line_alpha,
        coalescence_d_scale=c.coalescence_d_scale,
        capture_scale=c.capture_scale,
    )


def model_runner(T, bu, cand=None, dt_h=12.0, n_modes=22,
                 keep_history=False, manual_params=None, rizk_constants=None):
    """High-level entry point: solve_UN and return a flat dict of last-step quantities.

    If keep_history=True, the full history is included under hist["hist"].
    """
    p = build_un_params(T, bu, cand, dt_h, n_modes, manual_params, rizk_constants)
    hist, rates = m.solve_UN(p, keep_history=keep_history)
    pb_eq = hist["p_b_eq"][-1]
    pd_eq = hist["p_d_eq"][-1]
    return {
        "T": float(T), "burnup": float(bu),
        "swelling_b_percent":  100.0 * hist["swelling_b"][-1],
        "swelling_d_percent":  100.0 * hist["swelling_d"][-1],
        "swelling_ig_percent": 100.0 * hist["swelling_ig"][-1],
        "Nb": hist["Nb"][-1], "Nd": hist["Nd"][-1],
        "Rb_nm": hist["Rb"][-1] * 1.0e9, "Rd_nm": hist["Rd"][-1] * 1.0e9,
        "p_b": hist["p_b"][-1], "p_b_eq": pb_eq,
        "p_d": hist["p_d"][-1], "p_d_eq": pd_eq,
        "p_b_over_eq": hist["p_b"][-1] / pb_eq if pb_eq > 0.0 else float("nan"),
        "p_d_over_eq": hist["p_d"][-1] / pd_eq if pd_eq > 0.0 else float("nan"),
        "matrix_gas_percent":      hist["matrix_gas_percent"][-1],
        "bulk_gas_percent":        hist["bulk_gas_percent"][-1],
        "dislocation_gas_percent": hist["dislocation_gas_percent"][-1],
        "qgb_gas_percent":         hist["qgb_gas_percent"][-1],
        # Inter-granular (grain-face) bubble outputs
        "Rgf_nm": hist["Rgf"][-1] * 1.0e9,
        "Ngf":    hist["Ngf"][-1],
        "F_c":    hist["F_c"][-1],
        "swelling_gf_percent":     hist["swelling_gf_percent"][-1],
        "intergranular_gas_percent": hist["intergranular_gas_percent"][-1],
        "released_gas_percent":      hist["released_gas_percent"][-1],
        "fgr_percent":               hist["fgr_percent"][-1],
        # Solid fission product swelling (Rizk 2025 Eq. 19, 0.5·B per FIMA)
        "swelling_solid_percent":  hist["swelling_solid_percent"][-1],
        # Gas-only total swelling = bulk + dislocation + grain-face
        "swelling_gas_total_percent":
            100.0 * (hist["swelling_b"][-1]
                     + hist["swelling_d"][-1]
                     + hist["swelling_gf"][-1]),
        # Grand total = gas + solid (matches what experimentalists measure
        # for a fully irradiated pellet, before thermal expansion).
        "swelling_total_percent":
            100.0 * (hist["swelling_b"][-1]
                     + hist["swelling_d"][-1]
                     + hist["swelling_gf"][-1])
            + hist["swelling_solid_percent"][-1],
        "hist": hist if keep_history else None,
        "rates": rates,
    }
