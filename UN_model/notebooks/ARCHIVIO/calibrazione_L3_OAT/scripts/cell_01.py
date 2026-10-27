# ============================================================
# USER SETTINGS — change this cell for quick manual tests
# ============================================================

# Output
OUTPUT_DIR = "RizkUN_L3_unificato"
CASE_LABEL = "L3_unificato"
SHOW_PLOTS = False          # False = save PNG and close figures; True = show inline too

# Numerics
DT_H = 3.0                 # unified L3 convergence-audited setting (AP3.4 at 1800 K)
N_MODES = 160              # unified L3 spectral setting; FGR depends on spatial convergence
# Fast exploratory use of fewer modes is NOT FGR-converged

# Temperature / burnup grids
T_MIN = 900.0
T_MAX_SWELLING = 1800.0     # swelling plots usually compare to experimental range
T_MAX_DIAGNOSTIC = 1800.0   # extended high-T diagnostic range for "Monte Resegone"
T_STEP = 25.0               # refined T-grid for high-T FGR diagnostics
BURNUPS = [1.1, 1.3, 3.2]  # unique burnups; benchmark execution is case-specific below

# ------------------------------------------------------------
# Ronchi/DN1 separate-effects benchmark cases
# ------------------------------------------------------------
# Fission-rate density is reconstructed from the experimental linear heat rate
# and fuel diameter using 200 MeV/fission. AP3.2 and AP3.8 have the same
# burnup (1.1% FIMA) but are DIFFERENT benchmark cases and must not share Fdot.
ENERGY_PER_FISSION_J = 3.20435313e-11  # 200 MeV/fission

EXPERIMENT_CASES = {
    "AP3.2": {
        "burnup": 1.1,
        "linear_power_kW_m": 100.0,
        "fuel_diameter_mm": 8.3,
        "fission_rate": 5.767844763153473e19,
        "exp_series": "100 kW/m",
        "fuel": r"$(U_{0.8}Pu_{0.2})N$",
    },
    "AP3.8": {
        "burnup": 1.1,
        "linear_power_kW_m": 119.0,
        "fuel_diameter_mm": 8.3,
        "fission_rate": 6.863735268152632e19,
        "exp_series": "119 kW/m",
        "fuel": r"$(U_{0.8}Pu_{0.2})N$",
    },
    "AP3.4": {
        "burnup": 1.3,
        "linear_power_kW_m": 130.0,
        "fuel_diameter_mm": 8.3,
        "fission_rate": 7.498198192099515e19,
        "exp_series": "measurement",
        "fuel": r"$(U_{0.8}Pu_{0.2})N$",
    },
    "ANP6": {
        "burnup": 3.2,
        "linear_power_kW_m": 125.0,
        "fuel_diameter_mm": 8.0,
        "fission_rate": 7.760680190110210e19,
        "exp_series": "measurement",
        "fuel": r"$(U_{0.8}Pu_{0.2})N$",
    },
}
EXPERIMENT_CASE_ORDER = ["AP3.2", "AP3.8", "AP3.4", "ANP6"]

# ------------------------------------------------------------
# Physics switches — visible on purpose
# ------------------------------------------------------------
# Zullo current/default demo: True, True, True
USE_PHI_GAS_RESOLUTION = False
USE_NUCLEATION_MASS_COUPLING = True
USE_BULK_DISLOCATION_CAPTURE = True

# Intergranular model: corrected SCIANTIX-style 3x3 Pastore evolution
USE_DYNAMIC_RHO_D_NUCLEATION = False  # 10test baseline: keep rho_d constant; do not evolve N_d with rho_d(T,F(t))
USE_INTERGRANULAR_MODEL = True        # split q_gb into grain-face bubbles and released gas
INTERGRANULAR_POST_SAT_MODE = "pastore_3x3_exact_release"  # 11test: replaces old capacity clamp
USE_LENTICULAR_COS3 = True            # use spherical-lens 1 - 1.5 cos(theta) + 0.5 cos^3(theta)
GF_VACANCY_ABSORPTION_ONLY = False     #True keep consistent with current intragranular absorption-only default

# 9test additions: Wigner-Seitz correction for GAS trapping g
# Default False = original Rizk-like gas trapping.
# Toggle independently for bulk bubbles and for the spherical dislocation-bubble term.
USE_WS_GAS_TRAPPING_BULK = False
USE_WS_GAS_TRAPPING_DISLOCATION = False

# Numerical cap for the Wigner-Seitz correction factor.
# This avoids blow-up when R approaches the Wigner-Seitz cell radius,
# where the single-size approximation is already at its limit.
WS_GAS_TRAPPING_MAX_FACTOR = 10.0

# ------------------------------------------------------------
# Bubble first-gas pressure initialization
# ------------------------------------------------------------
# New test requested:
# - do NOT put a gas dimer inside bubbles at t=0;
# - when a bubble population first receives gas, assign vacancies so that
#   p = FIRST_GAS_PRESSURE_FACTOR * p_eq at the end of that first gas step;
# - after that, leave normal vacancy absorption evolution active.
USE_FIRST_GAS_PRESSURE_SEED = True
FIRST_GAS_PRESSURE_FACTOR = 1.0 / 3.0
SEED_BULK_ON_FIRST_GAS = True
SEED_DISLOCATION_ON_FIRST_GAS = True

# Legacy option from the previous test. Keep False for the current test.
# If True, it seeds a dimer already at birth/t=0; this is NOT the current default.
USE_EQUILIBRIUM_DIMER_SEED = False
SEED_GAS_ATOMS_PER_BUBBLE = 2.0
SEED_PRESSURE_FACTOR = 1.0 / 3.0
SEED_INITIAL_DISLOCATION_BUBBLES = False
SEED_NEW_BULK_BUBBLES = False

# Numerical safety: avoid crashing if a runaway state makes psi=R/delta astronomically large.
# It does not fix the physics; it only lets plots/CSV finish and should be read with diagnostics.
USE_ZETA_OVERFLOW_GUARD = True

# ------------------------------------------------------------
# Physical constants / nominal values
# ------------------------------------------------------------
GRAIN_RADIUS = 6.0e-6
XE_YIELD = 0.24
GAMMA_B = 1.11
OMEGA_FG = 8.5e-29
LATTICE_PARAMETER = 4.889e-10

# ------------------------------------------------------------
# Intergranular / grain-face bubble parameters from Rizk/SIFGRS
# ------------------------------------------------------------
NGF_AREAL_0 = 1.0e13       # [bub/m2] initial grain-face bubble areal density 2.0e13
DELTA_GB = 4.0e-10         # [m] grain-boundary diffusion layer thickness
FC_SAT = 0.5               # [-] grain-face saturation coverage
THETA_GF_DEG = 59.0        # [deg] semi-dihedral angle
R_GF0 = 2.42e-10           # [m] initial grain-face bubble radius of curvature
GAMMA_GB = 1.1391          # [J/m2] grain-boundary energy, used only for reference/diagnostics
# Grain-boundary vacancy diffusivity.
# 12test default: updated LANL/Matthews et al. expression based on the newer
# lower-length-scale vacancy dataset. The old Rizk-style x1e6 branch is kept
# only as a selectable legacy option for controlled comparisons.
DV_GB_MODE = "rizk_legacy_1e6_Dv1"    # options: "miller2025_LAUR2528152", "rizk_legacy_1e6_Dv1"
DV_GB_MULTIPLIER = 1.0e6   # legacy option only: D_v^gb = 1e6 * D_U,1
MILLER_DV_GB_D0 = 325.0    # [m2/s]
MILLER_DV_GB_Q_EV = 5.95   # [eV]

FISSION_RATE_NOMINAL = 5.767844763153473e19  # legacy/default helper value = AP3.2; benchmark runner overrides per case
F_N_NOMINAL = 4.2e-4      # selected calibrated L3 (Rizk 2025 nominal is 1e-6)
K_D_NOMINAL = 4.1e5       # selected calibrated L3 (Rizk 2025 K_d is 5e5)
RHO_D_NOMINAL = 3.5e13    # selected calibrated L3, held constant [m^-2]

# ------------------------------------------------------------
# Diffusivity modes
# ------------------------------------------------------------
# Xe / fission-gas diffusivity mode.
# Options:
#   "matthews2025_fig42_lookup" : user-digitized Matthews 2025 Fig. 4.2(f), y=1e-6,
#                                 piecewise-linear lookup in (1e4/T, log10 D).
#                                 Figure 4.2 is at Fdot = 1e19 m^-3 s^-1.
#   "rizk2025_table_D1D3"       : Rizk 2025 Table 2, Xe, with D2 neglected; Dg = D1 + D3.
#   "rizk2025_refit_plot"       : previous 16test selected mode; refit of digitized Rizk 2025 D_gas.
#   "rizk2023_table_raw"        : Rizk 2023 Table 3.1 raw Xe fit.
XE_DIFFUSIVITY_MODE = "rizk2025_refit_plot"

# Effective matrix / vacancy-mediated transport mode used for bubble growth.
# Options:
#   "matthews2025_DU_fig42_lookup" : user-digitized Matthews 2025 Fig. 4.2(b), y=1e-6.
#                                    This is total U self-diffusivity D_U^tot and is used as
#                                    the effective rate-limiting matrix diffusivity for bubble growth.
#                                    It is NOT the microscopic mobility of a single V_U.
#   "rizk2025_code_refit_old"      : previous Zullo/v14-style refit used in the code.
#   "rizk2025_refit_A20_only"      : digitized Rizk 2025 V_U curve, only A20 refitted.
#   "rizk2025_refit_full"          : previous 16test selected mode; full D1+D2 refit.
#   "rizk2023_table_raw"           : Rizk 2023 Table 3.1 raw V_U fit.
VU_DIFFUSIVITY_MODE = "rizk2025_refit_full"

# Diffusivity comparison plot: use the main 1.3% FIMA Ronchi case.
DIFFUSIVITY_PLOT_CASE_ID = "AP3.4"

# Legacy constants kept for backward compatibility with the previous code-refit option.
A20_VU_REFIT = 4.6304523933553033e-29  # previous code/Zullo-v14 Fig. 4 refit
A20_VU_RIZK2025 = 1.32e-19             # Rizk 2025 Table 2 coefficient, not used by default
A20_VU_DEFAULT = A20_VU_REFIT
B21_VU = -0.62
B22_VU = -0.04

# ------------------------------------------------------------
# Manual case parameters and scale factors
# No Optuna candidate is hidden here: these are all editable.
# ------------------------------------------------------------
MANUAL_PARAMS = {
    "f_n": F_N_NOMINAL,
    "K_d": K_D_NOMINAL,
    "rho_d": RHO_D_NOMINAL,
    "fission_rate": FISSION_RATE_NOMINAL,  # compatibility only; benchmark runner overrides from EXPERIMENT_CASES

    "Dv_scale": 1.0,        # total U-vacancy diffusivity scale; Rizk 2023 calibrated model used x5
    "Dv_D1_scale": 1.0,
    "Dv_D2_scale": 1.0,
    "Dv_dislocation_scale": 10.0,  # selected calibrated L3; non-nominal dislocation multiplier

    "Dg_scale": 1.0,        # total Xe/gas diffusivity scale; Rizk 2023 calibrated model used x5
    "D2_xe_scale": 0.0,     # 12test: neglect irradiation-enhanced Xe D2; keep same D1/D3 baseline
    "Dg_D1_scale": 1.0,
    "Dg_D3_scale": 1.0,
    "Dg_dislocation_scale": 17.0,  # selected calibrated L3; non-nominal dislocation/pipe multiplier

    "b_scale": 1.0,
    "b_bulk_scale": 1.0,
    "b_dislocation_scale": 1.0,

    "gb_scale": 1.0,
    "gd_scale": 1.0,
    "gd_bubble_scale": 1.0,
    "gd_line_scale": 1.0,
    "gd_line_alpha": 1.0,

    "coalescence_d_scale": 1.0,
    "capture_scale": 1.0,
}

# Dislocation-density mode.
# "constant" uses MANUAL_PARAMS["rho_d"].
# "rhoSat_RayBlank" uses the v14-style saturating Ray/Blank shape with global scale RHO_SCALE.
# "rizk2023" uses Rizk 2023 Eq. (3.38)-style empirical exponential density:
#     rho_d = rho0 * exp(c*F / max(Td, T0 - T)), capped at rho_cap.
#     IMPORTANT: F is FIMA fraction, e.g. 1.3% FIMA -> F = 0.013.
# "rizk2023_fit" uses the digitized Rizk 2023/Centipede Fig. 4.10 + Fig. 4.11 fitted/interpolated surface.
RHO_MODE = "constant"          # 10test baseline: independent of T and burnup
RHO_SCALE = 1.0                # only used by rhoSat_RayBlank
RHO_FAB = 1e13                 # only used by rhoSat_RayBlank

# Rizk 2023 empirical dislocation-density parameters, Table 3.3 value used for UN.
# Keep these unchanged to reproduce the published placeholder rho_d model.
RIZK2023_RHO0 = 1.0e12      # [m^-2]
RIZK2023_C = 49000.0        # [K / FIMA_fraction]
RIZK2023_T0 = 1900.0        # [K]
RIZK2023_TD = 150.0         # [K]
RIZK2023_RHO_CAP = 1.0e15   # [m^-2]

# Digitized Rizk 2023/Centipede rho_d(F,T) fit options.
# "shifted" is the recommended default: it follows Fig. 4.10 burnup dependence and
# corrects the 6.8% FIMA T-shape with the less aggressive "Centipede Shifted" curve from Fig. 4.11.
# "normal" follows the sharper "Centipede Normal" peak.
# "fig410" uses Fig. 4.10 interpolation only.
RHO_RIZK2023_FIT_VARIANT = "shifted"   # options: "fig410", "shifted", "normal"
RHO_RIZK2023_FIT_SCALE = 1.0           # global multiplier for sensitivity tests
RHO_RIZK2023_FIT_CAP = 4.0e15          # [m^-2]

# Smooth Rizk-2023-inspired rho_d(F,T) option.
# This is NOT the raw Fig. 4.10+4.11 surface. It deliberately removes the mid-temperature peak
# and replaces it with a smooth monotonic high-T decrease. Useful for sensitivity/calibration.
# Form: rho = rho_fab + scale*rho_amp*(1-exp(-F/Fc))*[f_min + (1-f_min)/(1+exp((T-T_half)/width))]
RHO_RIZK2023_SMOOTH_SCALE = 0.10      # try 0.05--0.20; keeps low-burnup rho near Rizk-2025 scale
RHO_RIZK2023_SMOOTH_RHO_FAB = 3.0e13  # [m^-2] high-T/fabrication floor
RHO_RIZK2023_SMOOTH_RHO_AMP = 7.5e14  # [m^-2] burnup-generated amplitude before scale factor
RHO_RIZK2023_SMOOTH_FC_PERCENT = 3.0  # [% FIMA] burnup saturation scale
RHO_RIZK2023_SMOOTH_T_HALF = 1550.0   # [K] temperature where high-T decrease is halfway
RHO_RIZK2023_SMOOTH_WIDTH = 120.0     # [K] smoothness of temperature transition
RHO_RIZK2023_SMOOTH_F_MIN = 0.08      # residual fraction of generated rho at very high T
RHO_RIZK2023_SMOOTH_CAP = 4.0e15      # [m^-2]


# Calibration guard: this notebook is intentionally the Rizk-2025-diffusivity baseline.
assert XE_DIFFUSIVITY_MODE == "rizk2025_refit_plot"
assert VU_DIFFUSIVITY_MODE == "rizk2025_refit_full"
assert DV_GB_MODE == "rizk_legacy_1e6_Dv1"
