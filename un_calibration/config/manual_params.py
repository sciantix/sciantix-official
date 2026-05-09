"""Free-fit parameters and calibration scales (degrees of freedom).

These are NOT Rizk literature values; they are the knobs we tune (or leave at
1.0 = Rizk-nominal) to study sensitivity and to calibrate against experimental
data. Edit this file (or override at script level) when running scans.
"""

MANUAL_PARAMS = {
    "label": "manual_candidate",

    # --- Free fit parameters ---
    # f_n: UN-specific recalibration on the Ronchi 1978 dislocation swelling
    # dataset (39 anchor points, 1.1 / 1.3 / 3.2 % FIMA). The U3Si2-inherited
    # Rizk value 1e-6 gives RMSE 1.51% Sw and a +1.24% systematic bias at
    # 1.3% FIMA. Fine-grained scan (un_calibration/scripts/calibrate_f_n.py)
    # picks 3e-6 as the value that balances per-burnup biases (max |bias|/bu
    # = 0.40, RMSE 1.03 — 32% better than reference). Still inside the
    # Olander 2006 range (1e-7..1e-2).
    "f_n":          3.0e-6,   # UN-recalibrated against Ronchi 1978
    "rho_d":        3.0e13,   # m^-2  (Rizk Table 1; used when both rho_d laws are off)
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
