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

    # --- Vacancy diffusivity (Fig. 4 refit) ---
    # NOTE: the published Table 2 vacancy values do NOT reproduce Fig. 4 (the
    # printed A20_VU = 1.32e-19 with B21_VU = 25.87 eV gives D2_v ~ 1e-100 m^2/s
    # at T=1500 K; the figure shows ~1e-21). The refit below reproduces Fig. 4.
    # The functional form deliberately uses +B21/(kT) (vs paper's -B21/(kT)) so
    # that B21_VU_refit < 0 acts as a positive activation energy ~0.62 eV.
    "D10_VU":              1.35e-2,   # m^2/s
    "Q1_VU":               5.66,      # eV
    "A20_VU":              4.6304523933553033e-29,  # m^(7/2) s^(-1/2)  (refit from Fig. 4)
    "B21_VU":              -0.62,     # eV   (refit)
    "B22_VU":              -0.04,     # eV^2 (refit)

    # --- Re-solution shape b0(R) = pref * (a1 - a2 * exp(-b1/R))  (Rizk 2025 Eq. 8) ---
    "B0_PREFACTOR":        1.0e-25,   # m^6 at/bub/fission
    "B0_A1":               2.64,
    "B0_A2":               2.02,
    "B0_B1":               2.61e-9,   # m

    # --- Boltzmann ---
    "KB_EV":               8.617333262e-5,
    "KB_J":                1.380649e-23,
}
