"""Optional dislocation-density laws rho_d(F, T).

Rizk 2025 uses constant rho_d (Sec. 2.2.2). These two alternatives are
available for sensitivity studies and for matching Rizk's NEAMS 2023
implementation (the exponential law).

The active law is selected by toggles in MANUAL_PARAMS:
    USE_RHO_FT = True   -> rho_d_FT  (Blank-saturating + Ray-Blank burnup)
    USE_RHO_EXP = True  -> rho_d_exp (Rizk-NEAMS 2023, Eq. 3.38)
    Both False          -> constant rho_d from MANUAL_PARAMS['rho_d']
The two toggles are mutually exclusive (enforced in `resolve_rho_d`).
"""

import math


# ============================================================================
# RHO_FT_PARAMS -- T- and burnup-dependent dislocation-density law.
# Lineage:
#   - Saturating temperature shape: Blank 1984 Table 3 fit (specimen C3/1, 6.8 a/o)
#   - Ray-Blank burnup law:         Blank 1984 + v14 wrapper
# Not Rizk's own constants -- Rizk uses constant rho_d (Sec. 2.2.2).
# ============================================================================
RHO_FT_PARAMS = {
    # f_sat(T) = rho_inf - (rho_inf - rho_at_anchor) * exp(-(T-T_anchor)/tau)
    # Units: 10^14 m^-2.  Normalised at T_REF when used as a multiplier.
    "RHO_INF":        9.104,    # asymptotic value (10^14 m^-2)
    "RHO_AT_ANCHOR":  6.357,    # value at T = T_ANCHOR (10^14 m^-2)
    "T_ANCHOR":       940.0,    # K, anchor temperature (controls T-onset)
    "TAU":            203.76,   # K, characteristic width of the rise
    "T_REF":          1025.0,   # K, reference for normalisation

    # Ray-Blank burnup law: rho_0(F) = max(rho_FAB, C1 * (F - F0))
    "C1":        1.6e14,   # m^-2 / (a/o)
    "F0":        2.4,      # a/o threshold for linear growth
    "RHO_FAB":   3.0e13,   # m^-2, as-fabricated floor (Blank 1984)
}

# ============================================================================
# RHO_EXP_PARAMS -- Rizk NEAMS 2023 exponential law (Eq. 3.38, Table 3.3).
#   rho_d(F, T) = rho_scale * RHO_0 * exp[ C * F / max(TD, T0 - T) ]
# bounded above by RHO_MAX (network saturation cap = 10^15 m^-2).
# Diverges as T -> T0 (1900 K), the breakaway-swelling onset.
# Calibrated for UN; UC reference value: RHO_0 = 2e13.
# ============================================================================
RHO_EXP_PARAMS = {
    "RHO_0":   1.0e12,   # m^-2 (Rizk-NEAMS calibrated for UN; UC value: 2e13)
    "C":       49000.0,  # K * (FIMA)^-1
    "T0":      1900.0,   # K, divergence temperature (breakaway-swelling onset)
    "TD":      150.0,    # K, minimum gap T0-T to avoid asymptotic blow-up
    "RHO_MAX": 1.0e15,   # m^-2, network saturation cap
}


def rho_d_FT(T, burnup, rho_scale=1.0, params=None):
    """Blank-saturating temperature shape * Ray-Blank burnup growth.

    burnup is in % FIMA (a/o). T in K.
    """
    p = params if params is not None else RHO_FT_PARAMS
    f_sat   = p["RHO_INF"] - (p["RHO_INF"] - p["RHO_AT_ANCHOR"]) \
              * math.exp(-(T - p["T_ANCHOR"]) / p["TAU"])
    f_sat_0 = p["RHO_INF"] - (p["RHO_INF"] - p["RHO_AT_ANCHOR"]) \
              * math.exp(-(p["T_REF"] - p["T_ANCHOR"]) / p["TAU"])
    f_sat_norm = f_sat / f_sat_0
    rho_0 = max(p["RHO_FAB"], p["C1"] * (burnup - p["F0"]))
    return rho_scale * rho_0 * f_sat_norm


def rho_d_exp(T, burnup, rho_scale=1.0, params=None):
    """Rizk-NEAMS 2023 exponential law (Eq. 3.38, Table 3.3 calibrated for UN).

    burnup is in % FIMA (a/o); Rizk's c uses FIMA as a fraction, so we divide by 100.
    T in K. Capped at RHO_MAX (network saturation).
    """
    p = params if params is not None else RHO_EXP_PARAMS
    F_frac = burnup / 100.0
    gap = max(p["TD"], p["T0"] - T)
    expo = min(p["C"] * F_frac / gap, 700.0)   # overflow guard; cap kicks in anyway
    val = p["RHO_0"] * math.exp(expo)
    return rho_scale * min(val, p["RHO_MAX"])


def resolve_rho_d(T, burnup, manual_params):
    """Pick the active rho_d law based on MANUAL_PARAMS toggles.

    Returns the rho_d value at (T, burnup) in m^-2.
    Raises if both USE_RHO_FT and USE_RHO_EXP are True.
    """
    if manual_params["USE_RHO_FT"] and manual_params["USE_RHO_EXP"]:
        raise ValueError("USE_RHO_FT and USE_RHO_EXP are mutually exclusive; pick one.")
    if manual_params["USE_RHO_EXP"]:
        return rho_d_exp(T, burnup, manual_params["rho_scale"])
    if manual_params["USE_RHO_FT"]:
        return rho_d_FT(T, burnup, manual_params["rho_scale"])
    return manual_params["rho_d"]
