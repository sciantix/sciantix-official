"""Rizk 2025 literature values — single source of truth for the UN model.

Source key: J. Rizk et al., JNM 606 (2025) 155604.
Every entry below is either lifted verbatim from the paper (with the page/
table/equation reference in the comment) or, in the few flagged cases,
a documented divergence (paper typo correction or Fig. 4 refit).

This dict is consumed by `config.builder.build_un_params` and is the only
place where Rizk physical constants are defined. `un_model.py` is parameter-
free and accepts these values via `UNParameters(...)` keyword arguments.
"""

RIZK_CONSTANTS = {
    # --- Geometry / matrix ---
    "GRAIN_RADIUS":        6.0e-6,    # m              (Rizk 2025, Table 1)
    "LATTICE_PARAMETER":   4.889e-10, # m, UN          (Rizk 2025, Table 1)
    "OMEGA_FG":            8.5e-29,   # m^3/at         (Rizk 2025, Table 1)
    "RADIUS_IN_LATTICE":   0.21e-9,   # m, ~a/(2sqrt(2)) (Rizk 2025)
    "GAMMA_B":             1.11,      # J/m^2          (Rizk 2025, Table 1, Eq. 16)

    # --- Fission yield (Xe-equivalent stand-in for noble gases) ---
    "XE_YIELD":            0.24,      # at/fission     (Rizk 2025)

    # --- Dislocations (constant rho_d in Rizk 2025, Sec. 2.2.2) ---
    # K_D = 5e5: Rizk 2025 Sec. 4 explicitly lowers from the prior UC/UO2 value of 1e6
    # ("It was found to be essential to slightly lower the value of K... to 5e5").
    "K_D":                 5.0e5,     # bub/m          (Rizk 2025, Table 1 + Sec. 4)
    "R_D_LINE":            3.46e-10,  # m, ~a/sqrt(2)  (Rizk 2025, Table 1)
    "Z_D":                 5.0,       # geometric      (Rizk 2025, Eq. 23)

    # --- Xe diffusivity D_g = D1 + D3 (D2 negligible for Xe per Rizk Sec. 3.1.1) ---
    "D10_XE":              1.56e-3,   # m^2/s          (Rizk 2025, Table 2)
    "Q1_XE":               4.94,      # eV             (Rizk 2025, Table 2)
    "A30_XE":              1.85e-39,  # m^5            (Rizk 2025, Table 2; D3 = A30 * Fdot)
    # D2_xe diagnostic only -- not summed into D_g.
    "A20_XE":              1.21e-67,  # m^(7/2) s^(-1/2)
    "B21_XE":              25.87,     # eV
    "B22_XE":              -1.49,     # eV^2
    "B23_XE":              0.0,       # eV^3

    # --- Vacancy diffusivity (D1 thermal + D3 athermal; D2 omitted) ---
    # Rizk 2025 Fig. 4 actually plots D_tot which at T>=1300 K is dominated by D1
    # (the Arrhenius thermal term from Tab. 2 below). The Tab. 2 D2 fit parameters
    # for V_U are mathematically broken (off by ~14 OOM); per Schneider 2026
    # cluster dynamics the true D2_v at 1500 K is ~1e-24 m^2/s, negligible vs D1.
    # We therefore drop D2 entirely and add D3 athermal mixing from Schneider 2024:
    # the paper reports D3_v = 2.48e-22 m^2/s at F = 5e18 fiss/m^3/s (Matzke's
    # reference fission rate, NOT our DN1/Rizk-validation 5e19). Therefore
    #   A30 = D3 / F_Schneider = 2.48e-22 / 5e18 = 4.96e-41 m^5
    # At our reference F=5e19 this gives D3 = 2.48e-21 m^2/s (10x larger than
    # the value initially extracted by NotebookLM, which conflated the two F's).
    "D10_VU":   1.35e-2,    # m^2/s  (Rizk 2025 Tab. 2)
    "Q1_VU":    5.66,       # eV     (Rizk 2025 Tab. 2)
    "A30_VU":   4.96e-41,   # m^5    (Schneider 2024 athermal mixing for V_U)

    # --- Re-solution shape b0(R) = pref * (a1 - a2 * exp(-b1/R))  (Rizk 2025 Eq. 8) ---
    "B0_PREFACTOR":        1.0e-25,   # m^6 at/bub/fission
    "B0_A1":               2.64,
    "B0_A2":               2.02,
    "B0_B1":               2.61e-9,   # m

    # --- Boltzmann ---
    "KB_EV":               8.617333262e-5,
    "KB_J":                1.380649e-23,

    # --- Inter-granular (grain face) bubble model — Rizk 2025 §A.2 ---
    # Initial grain-face bubble number density (Rizk Tab. 1, inherited from UO2 [41]).
    "N_GF_0":              2.0e13,    # m^-2
    # GB vacancy diffusivity: D_v_gb = ratio * D_1_thermal (Rizk Tab. 1, from UO2 [48]).
    # ONLY thermal D_1 enters the multiplier basis (no D_3 athermal here).
    "D_VGB_RATIO":         1.0e6,
    "DELTA_GB":            4.0e-10,   # m, GB diffusion-layer thickness (Rizk Tab. 1)
    # Initial grain-face bubble radius (Rizk Tab. 1).
    "R_GF_0":              2.42e-10,  # m
    # Saturation coverage threshold for interconnection / FGR (Rizk Tab. 1, UO2 [38]).
    "F_C_SAT":             0.5,
    # Semi-dihedral angle θ from cos θ = γ_GB/(2γ_b). Rizk Tab. 1: θ ≈ 59°.
    "THETA_DEG":           59.0,
}
