"""Free-fit parameters and calibration scales (degrees of freedom).

These are NOT Rizk literature values; they are the knobs we tune (or leave at
1.0 = Rizk-nominal) to study sensitivity and to calibrate against experimental
data. Edit this file (or override at script level) when running scans.
"""

MANUAL_PARAMS = {
    "label": "manual_candidate",

    # --- Free fit parameters ---
    # f_n: UN-specific recalibration on the Ronchi 1978 dislocation swelling
    # dataset (39 anchor points, 1.1 / 1.3 / 3.2 % FIMA).
    # History:
    #   2026-05-09: f_n=3e-6 chosen with USE_PHI_GAS_RESOLUTION = True (Barani
    #               moment closure). Gave RMSE 1.03, max |bias|/bu 0.40.
    #   2026-05-21: switched default to USE_PHI_GAS_RESOLUTION = False
    #               (per-atom-rigorous closure consistent with Setyawan 2018 /
    #               Matthews 2014 derivation of b). With phi=OFF the calibrate_f_n
    #               scan finds a monotone improvement as f_n shrinks; optimum at
    #               the Olander 2006 lower bound 1e-7 (RMSE 1.76, bias -1.55).
    #               Even there, the model still under-predicts Sw_d on Ronchi by
    #               ~50% — f_n alone cannot compensate the rigorous closure. The
    #               residual misfit is open thesis work (candidates: K_d, rho_d
    #               law, D_v at moderate T, nu_b ∝ c^2 form, loop-punching cap).
    "f_n":          1.0e-7,   # Olander lower bound; rigorous-closure best fit on Ronchi 1978
    # rho_d: UN dislocation density.
    #   Rizk 2025 Table 1 uses 3e13 m^-2 (UO2/UC heritage). Under the rigorous
    #   per-atom closure (USE_PHI_GAS_RESOLUTION = False), the sensitivity scan
    #   on Ronchi 1978 identifies rho_d ≈ 1e14 m^-2 as the value that brings
    #   the global bias to zero (RMSE 0.857, bias -0.100 — better than the
    #   Barani-closure fit at rho_d=3e13). 1e14 is also consistent with
    #   measured UN dislocation densities in Blank 1984 Table 3 (specimen
    #   C3/1, 6.8 a/o): 6.4e14 m^-2 at 940 K up to 8.6e14 at 1300 K (lower
    #   bound at low burnup ~ 10^14 m^-2). Adopting 1e14 therefore aligns the
    #   model both with UN microstructure data and with Ronchi swelling.
    "rho_d":        1.0e14,   # m^-2  (UN-realistic; sensitivity-derived 2026-05-21)
    "fission_rate": 5.0e19,   # fiss/(m^3 s) (DN1/Rizk validation, LHR~100 kW/m, d=8.30 mm)

    # --- Dislocation-density law toggle (mutually exclusive) ---
    # Both False           -> rho_d is the constant MANUAL_PARAMS['rho_d']
    #                         (Rizk 2025 paper choice, Sec. 2.2.2).
    # USE_RHO_FT = True    -> Blank-saturating + Ray-Blank burnup law
    #                         rho_d = rho_scale * rho_0(F) * f_sat_norm(T)
    #                         (parameters in config.rho_d_laws.RHO_FT_PARAMS).
    # USE_RHO_EXP = True   -> Rizk-NEAMS 2023 exponential law (Eq. 3.38)
    #                         rho_d = rho_scale * RHO_0 * exp[C*F/(T0-T)]
    #                         with RHO_MAX cap (parameters in config.rho_d_laws.RHO_EXP_PARAMS).
    #                         This is what Rizk's published NEAMS code uses.
    "USE_RHO_FT":    False,
    "USE_RHO_EXP":   False,
    "rho_scale":     1.0,     # multiplier consulted by both laws above

    # --- Scales (1.0 = Rizk-nominal) ---
    "coalescence_d_scale":  1.0,
    "capture_scale":        1.0,   # bulk→disloc capture, only consulted if
                                   #   un_model.USE_BULK_DISLOCATION_CAPTURE = True
                                   #   (default OFF per audit 2026-05-09; ablation only).
    "gb_scale":             1.0,
    "gd_bubble_scale":      1.0,
    "gd_line_scale":        1.0,
    "gd_line_alpha":        1.0,
    "b_bulk_scale":         1.0,
    "b_dislocation_scale":  1.0,
    "Dv_scale":             1.0,
    "Dv_D1_scale":          1.0,
    "Dv_D3_scale":          1.0,
    "Dg_scale":             1.0,
    "Dg_D1_scale":          1.0,
    "Dg_D3_scale":          1.0,

    # Aggregate / deprecated scales (locked at 1.0)
    "b_scale":              1.0,
    "gd_scale":             1.0,
    "D2_xe_scale":          1.0,
}
