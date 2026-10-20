# ============================================================
# USER SETTINGS — change this cell for quick manual tests
# ============================================================

# Output
OUTPUT_DIR = "BaseRizk2025"
CASE_LABEL = "BaseRizk2025"
SHOW_PLOTS = False          # False = save PNG and close figures; True = show inline too

# Numerics
DT_H = 1.0                 # user-selected FGR timestep; convergence remains case-dependent
N_MODES = 160              # unchanged from unified solver; FGR depends on spatial convergence
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
NGF_AREAL_0 = 2.0e13       # [bub/m2] Rizk 2025 reference initial grain-face density
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
F_N_NOMINAL = 1.0e-6      # Rizk 2025 uses inherited Barani U3Si2 homogeneous nucleation factor
K_D_NOMINAL = 5.0e5       # [bubbles/m] calibrated by Rizk 2025
RHO_D_NOMINAL = 3.0e13    # [m^-2] Rizk 2025 reference, held constant

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
    "Dv_dislocation_scale": 1.0,   # Rizk reference: no extra dislocation-vacancy multiplier

    "Dg_scale": 1.0,        # total Xe/gas diffusivity scale; Rizk 2023 calibrated model used x5
    "D2_xe_scale": 0.0,     # 12test: neglect irradiation-enhanced Xe D2; keep same D1/D3 baseline
    "Dg_D1_scale": 1.0,
    "Dg_D3_scale": 1.0,
    "Dg_dislocation_scale": 1.0,   # Rizk reference: no extra dislocation-gas multiplier

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


# Transport-mode guard: use exactly the same Fig. 4 refits as in L3.
assert XE_DIFFUSIVITY_MODE == "rizk2025_refit_plot"
assert VU_DIFFUSIVITY_MODE == "rizk2025_refit_full"
assert DV_GB_MODE == "rizk_legacy_1e6_Dv1"


# ============================================================
# Matthews 2025 / Schneider updated diffusivity lookup tables
# USER-DIGITIZED POINTS — EMBEDDED FOR A SELF-CONTAINED NOTEBOOK
# ============================================================
#
# Coordinates:
#   x = 1e4 / T
#   y = log10(D [m^2/s])
#
# Source:
#   Matthews et al. (2025), Fig. 4.2
#   hyper-stoichiometric y = 1e-6 = UN_1.000001
#   figure fission-rate density = 1e19 fissions m^-3 s^-1
#
# We intentionally DO NOT impose an analytical fit.  The model uses a
# piecewise-linear interpolation in (1e4/T, log10 D), matching the lookup
# philosophy used in the updated BISON workflow.
#
# The experimental-pin-specific Fdot values remain active elsewhere in 16test.

from bisect import bisect_left

MATTHEWS2025_LOOKUP_Y = 1.0e-6
MATTHEWS2025_LOOKUP_FISSION_RATE = 1.0e19

MATTHEWS2025_DU_LOG_LOOKUP_RAW = [
    (4.077313711905928, -15.66237964224148),
    (4.156225712137465, -15.82095733898184),
    (4.202182342644798, -15.93984036511862),
    (4.228034124955607, -16.15097581559484),
    (4.306908440373277, -16.32274319718765),
    (4.366017070919201, -16.46805583944782),
    (4.405360016593376, -16.58691374237536),
    (4.451241277472983, -16.73217613821704),
    (4.497160223166452, -16.86424884920628),
    (4.54296611441833, -17.03589061475287),
    (4.615151375375114, -17.23401224284135),
    (4.660957266626991, -17.40565400838793),
    (4.752757473200068, -17.68298911521885),
    (4.884126734330503, -18.00004401586259),
    (5.002004832097573, -18.40937646405502),
    (5.126647353953257, -18.76597529604687),
    (5.277443136630663, -19.22819209969533),
    (5.369356397645332, -19.46595815196888),
    (5.461194289032272, -19.73010357394736),
    (5.520189865136603, -19.91498527076488),
    (5.592262071651795, -20.15267595341071),
    (5.651182278128397, -20.36393701993316),
    (5.710215539046592, -20.53562903189823),
    (5.775862484797946, -20.70734616707256),
    (5.86112437616559, -20.95827678098933),
    (5.920044582642193, -21.16953784751177),
    (5.989298802520655, -21.36471726865114),
    (6.075908449384135, -21.59403633968374),
    (6.153306252858602, -21.83248002073994),
    (6.262749233122412, -22.10768384198676),
    (6.326551716595192, -22.28195865943538),
    (6.390328030058345, -22.46539298025376),
    (6.504442357040732, -22.71313573806439),
    (6.572863847212833, -22.87826849881634),
    (6.682542357563294, -23.07103678973533),
    (6.764742357804478, -23.23622189050653),
    (6.87465639824159, -23.34655465109769),
    (6.989189445378022, -23.44774535499217),
    (7.085455825794194, -23.51222825871528),
    (7.177155539530297, -23.56753421239554),
    (7.268724403218264, -23.6686376829246),
    (7.378900143751654, -23.68737540981816),
    (7.507316380995679, -23.75198044025286),
    (7.594475598061346, -23.78894994052052),
    (7.699927651856903, -23.8534677375898),
    (7.82393424247863, -23.8539387977631),
    (7.938467289615064, -23.95512950165758),
    (8.071659553616179, -23.95563545517706),
    (8.167925934032347, -24.02011835890016),
    (8.27807550455611, -24.04801558916349),
    (8.383527558351666, -24.11253338623276),
    (8.479924788815978, -24.13121877310707),
    (8.567005495852761, -24.19566678348401),
    (8.658757549608122, -24.23265373042475),
    (8.718333576526, -24.27867805402365),
    (8.787121446832892, -24.31557776759897),
    (8.878794990559367, -24.38004322464899),
    (8.947373500789237, -24.49021896518238),
    (9.043639881205408, -24.55470186890549),
    (9.135261084912628, -24.63748633269502),
    (9.24527980538825, -24.71118107980715),
    (9.309396328976566, -24.77554185681866),
    (9.387369872662845, -24.81247646374014),
    (9.446893559561468, -24.87681979407856),
    (9.520221926538801, -24.93205596106648),
    (9.61194781028453, -24.97820241137699),
    (9.680500150504772, -25.09753765528014),
    (9.776845040949825, -25.13454204889396),
    (9.877573408007549, -25.24483991613895),
    (9.982946951774224, -25.3368362233175),
    (10.08367531883195, -25.44713409056249),
    (10.18902269258899, -25.54828990111081),
    (10.3175174398619, -25.58541642143623),
    (10.39995297018974, -25.6681659918796),
    (10.52844771746265, -25.70529251220502),
    (10.68426395478707, -25.82495924289679),
    (10.79908487202941, -25.82539540972392),
    (10.78982068862114, -25.85283902648703),
    (10.8815727423765, -25.88982597342777),
    (11.02854351644669, -25.8903842669665),
    (11.18002861717769, -25.9184385172876),
    (11.2810186843317, -25.93714135083498),
    (11.41421094833281, -25.93764730435446)
]

MATTHEWS2025_DXE_LOG_LOOKUP_RAW = [
    (4.0626026272578, -14.12337611563509),
    (4.119047619047616, -14.2261683894337),
    (4.154967159277502, -14.31875505837504),
    (4.196018062397371, -14.42162349340393),
    (4.267857142857142, -14.56556821862944),
    (4.324302134646961, -14.64774618609946),
    (4.39614121510673, -14.79169091132497),
    (4.442323481116585, -14.91514826560569),
    (4.539819376026272, -15.079580361776),
    (4.586001642036124, -15.22365202238531),
    (4.644228357051632, -15.33884223966521),
    (4.691740976403331, -15.44597335293441),
    (4.751131750592957, -15.54708091211141),
    (4.798644369944659, -15.65421202538061),
    (4.861004682843762, -15.7672344683839),
    (4.917425918323906, -15.86835671915624),
    (4.973847153804053, -15.95158460679612),
    (5.006512079608344, -16.10054269201779),
    (5.04808562154108, -16.20173840076686),
    (5.125293627987592, -16.31468738579342),
    (5.169836708629812, -16.44569234150124),
    (5.223288405400476, -16.54682928386893),
    (5.288618257009063, -16.68966097383096),
    (5.359887186036612, -16.81456891746985),
    (5.401460727969348, -16.91576462621892),
    (5.454912424740012, -17.01093678087579),
    (5.484607811834824, -17.07640252974134),
    (5.532120431186523, -17.201428006143),
    (5.582602589247705, -17.29661485239521),
    (5.627145669889925, -17.40972544497058),
    (5.662780134403697, -17.48112659835625),
    (5.710292753755399, -17.58229292391463),
    (5.748896756978654, -17.69543289968069),
    (5.805317992458797, -17.77866078732058),
    (5.870647844067387, -17.89763332643933),
    (5.938947234385454, -18.0344855370952),
    (5.971612160189748, -18.09397180665458),
    (6.033972473088854, -18.20102946194705),
    (6.090393708568996, -18.32004607585185),
    (6.123058634373292, -18.39742670854368),
    (6.158693098887066, -18.46882786192936),
    (6.200266640819804, -18.54616441983515),
    (6.253718337590465, -18.65326614991366),
    (6.304200495651645, -18.74248820845506),
    (6.369530347260233, -18.88531989841709),
    (6.464555585963633, -19.02203988471484),
    (6.512068205315334, -19.1470653611165),
    (6.607093444018732, -19.28378534741425),
    (6.725874992397983, -19.43231737637086),
    (6.811991614972939, -19.56311664974383),
    (6.868412850453081, -19.62845017425126),
    (6.930773163352188, -19.71164867870044),
    (6.996103014960776, -19.77097334071101),
    (7.061432866569362, -19.86012194127567),
    (7.159427643982243, -19.90139063260496),
    (7.108945485921063, -19.88971081430422),
    (7.159427643982243, -19.93717935886988),
    (7.221787956881348, -19.97862434934333),
    (7.269300576233049, -20.03803716092597),
    (7.331660889132155, -20.10930608995352),
    (7.391051663321781, -20.1448009843115),
    (7.480137824606217, -20.19207853813765),
    (7.55734583105273, -20.28713316003175),
    (7.625645221370798, -20.35240791815778),
    (7.711761843945753, -20.39373537586846),
    (7.845391085872409, -20.51237000829424),
    (7.780061234263822, -20.45304534628368),
    (7.937446785866327, -20.57156244594668),
    (8.002776637474915, -20.61895753253561),
    (8.071076027792984, -20.66633792752919),
    (8.154223111658457, -20.72557443996767),
    (8.231431118104972, -20.79080512330767),
    (8.311608663260962, -20.85602111505231),
    (8.409603440673845, -20.91518416951406),
    (8.486811447120356, -20.98041485285406),
    (8.561049914857389, -21.06355459092186),
    (8.6382579213039, -21.14071484968349),
    (8.70952685033145, -21.23579885476828),
    (8.765948085811594, -21.30709716698653),
    (8.816430243872775, -21.37246007468464),
    (8.872851479352917, -21.42586402377043),
    (8.941150869670986, -21.52096272045056),
    (8.982724411603723, -21.6400527923321),
    (9.048054263212313, -21.72323660518594),
    (9.095566882564013, -21.76475505363613),
    (9.169805350301043, -21.87771873025802),
    (9.241074279328592, -21.99069709847527),
    (9.303434592227696, -22.06196602750282),
    (9.371733982545766, -22.16302951189377),
    (9.454881066411239, -22.26998432601881),
    (9.526149995438789, -22.40085705736851),
    (9.62414477285167, -22.48984405038436),
    (9.692444163169744, -22.56704838393204),
    (9.772621708325737, -22.66208831423078),
    (9.849829714772246, -22.66170633275176),
    (9.930007259928241, -22.6613096596774),
    (9.998306650246308, -22.64904217756278),
    (10.07551465669282, -22.61883625752967),
    (10.14678358572037, -22.56480056984397),
    (10.24774790184273, -22.5106179662048),
    (10.33089498570821, -22.43862914900525),
    (10.41404206957368, -22.37260511951653),
    (10.50015869214864, -22.22902415819181),
    (10.60409254698048, -22.09728462271664),
    (10.71396547923129, -21.93569176549668),
    (10.80305164051573, -21.82788483884152),
    (10.90995503405705, -21.7557784888792),
    (11.00794981146993, -21.69564578912456),
    (11.15048766952503, -21.61739835230727),
    (11.26036060177584, -21.53334773532795),
    (11.37023353402664, -21.4970154200352),
    (11.45338061789211, -21.42502660283566)
]


def _prepare_log_lookup(raw_points):
    """Sort x and average exact duplicate x values without smoothing the curve."""
    grouped = {}
    for x, y in raw_points:
        grouped.setdefault(float(x), []).append(float(y))
    return [
        (x, sum(vals) / len(vals))
        for x, vals in sorted(grouped.items(), key=lambda kv: kv[0])
    ]


MATTHEWS2025_DU_LOG_LOOKUP = _prepare_log_lookup(MATTHEWS2025_DU_LOG_LOOKUP_RAW)
MATTHEWS2025_DXE_LOG_LOOKUP = _prepare_log_lookup(MATTHEWS2025_DXE_LOG_LOOKUP_RAW)


def _piecewise_log_lookup(T, table):
    """Return D(T) from a piecewise-linear interpolation in x=1e4/T, log10(D).

    Outside the digitized interval, use the nearest end segment for linear
    extrapolation in log-space.  The current 900-1800 K benchmark range lies
    inside both digitized ranges.
    """
    T = float(T)
    if T <= 0.0:
        raise ValueError("Temperature must be positive.")

    xq = 1.0e4 / T
    xs = [p[0] for p in table]
    ys = [p[1] for p in table]

    if len(xs) < 2:
        raise ValueError("Lookup table must contain at least two points.")

    j = bisect_left(xs, xq)
    if j <= 0:
        i0, i1 = 0, 1
    elif j >= len(xs):
        i0, i1 = len(xs) - 2, len(xs) - 1
    else:
        i0, i1 = j - 1, j

    x0, x1 = xs[i0], xs[i1]
    y0, y1 = ys[i0], ys[i1]
    if x1 == x0:
        yq = 0.5 * (y0 + y1)
    else:
        yq = y0 + (xq - x0) * (y1 - y0) / (x1 - x0)

    return 10.0 ** yq


def matthews2025_DU_lookup(T):
    """Total U self-diffusivity used as effective vacancy-mediated matrix transport."""
    return _piecewise_log_lookup(T, MATTHEWS2025_DU_LOG_LOOKUP)


def matthews2025_DXe_lookup(T):
    """Xe diffusivity from Matthews 2025 Fig. 4.2(f), y=1e-6."""
    return _piecewise_log_lookup(T, MATTHEWS2025_DXE_LOG_LOOKUP)


# Experimental data digitized for comparison
"""Digitized experimental data used by the UN calibration pipeline."""

EXP_SWELLING_T = [
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1127.0, "swelling": 0.68},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1228.0, "swelling": 0.59},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1312.0, "swelling": 0.43},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1402.0, "swelling": 0.58},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1485.0, "swelling": 1.22},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1549.0, "swelling": 1.84},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1598.0, "swelling": 1.66},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1632.0, "swelling": 2.13},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1669.0, "swelling": 3.60},
    {"figure": "Fig3a", "burnup": 1.1, "series": "100 kW/m", "T": 1685.0, "swelling": 2.72},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 899.0, "swelling": 0.63},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1154.0, "swelling": 1.17},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1228.0, "swelling": 1.08},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1325.0, "swelling": 1.28},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1435.0, "swelling": 1.32},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1514.0, "swelling": 2.10},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1570.0, "swelling": 2.72},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1608.0, "swelling": 2.91},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1635.0, "swelling": 3.28},
    {"figure": "Fig3a", "burnup": 1.1, "series": "119 kW/m", "T": 1656.0, "swelling": 3.75},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1044.0, "swelling": 1.11},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1220.0, "swelling": 1.22},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1377.0, "swelling": 1.33},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1534.0, "swelling": 2.83},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1595.0, "swelling": 3.53},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1639.0, "swelling": 3.86},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1661.0, "swelling": 2.45},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1709.0, "swelling": 2.93},
    {"figure": "Fig3b", "burnup": 1.3, "series": "measurement", "T": 1724.0, "swelling": 3.15},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 984.0, "swelling": 0.72},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1056.0, "swelling": 1.06},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1126.0, "swelling": 1.26},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1247.0, "swelling": 1.58},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1343.0, "swelling": 1.79},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1420.0, "swelling": 2.08},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1459.0, "swelling": 2.40},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1511.0, "swelling": 2.83},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1557.0, "swelling": 3.31},
    {"figure": "Fig3c", "burnup": 3.2, "series": "measurement", "T": 1590.0, "swelling": 3.75},
]

EXP_SWELLING_BURNUP_1600 = [
    {"burnup": 1.12, "swelling": 1.64, "series": "approx 100 kW/m"},
    {"burnup": 1.11, "swelling": 2.90, "series": "approx 119 kW/m"},
    {"burnup": 1.31, "swelling": 3.51, "series": "measurement"},
    {"burnup": 3.18, "swelling": 3.72, "series": "measurement"},
]

EXP_ND_T_13 = [
    {"T": 1153.0, "N": 5.22e19},
    {"T": 1202.0, "N": 3.69e19},
    {"T": 1227.0, "N": 3.00e19},
    {"T": 1235.0, "N": 3.57e19},
    {"T": 1248.0, "N": 2.44e19},
    {"T": 1330.0, "N": 2.90e19},
    {"T": 1338.0, "N": 4.10e19},
    {"T": 1377.0, "N": 2.71e19},
    {"T": 1408.0, "N": 1.67e19},
    {"T": 1427.0, "N": 1.85e19},
    {"T": 1493.0, "N": 3.00e19},
    {"T": 1507.0, "N": 2.44e19},
    {"T": 1524.0, "N": 1.56e19},
    {"T": 1538.0, "N": 7.54e18},
    {"T": 1555.0, "N": 7.80e18},
    {"T": 1561.0, "N": 1.92e19},
    {"T": 1599.0, "N": 5.34e18},
    {"T": 1622.0, "N": 7.80e18},
    {"T": 1628.0, "N": 1.73e19},
    {"T": 1649.0, "N": 5.34e18},
    {"T": 1656.0, "N": 3.65e18},
    {"T": 1656.0, "N": 2.41e18},
    {"T": 1669.0, "N": 1.96e18},
    {"T": 1682.0, "N": 2.44e19},
    {"T": 1685.0, "N": 1.40e19},
    {"T": 1723.0, "N": 7.70e17},
    {"T": 1742.0, "N": 1.02e18},
    {"T": 1739.0, "N": 1.65e18},
]

EXP_RD_T_13 = [
    {"T": 1045.0, "R_nm": 54.06},
    {"T": 1219.0, "R_nm": 60.37},
    {"T": 1374.0, "R_nm": 69.58},
    {"T": 1535.0, "R_nm": 104.85},
    {"T": 1594.0, "R_nm": 120.83},
    {"T": 1641.0, "R_nm": 143.73},
    {"T": 1663.0, "R_nm": 122.76},
    {"T": 1710.0, "R_nm": 157.99},
    {"T": 1725.0, "R_nm": 173.67},
]


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



# ============================================================
# MANUAL RUNNER, DIAGNOSTICS, AND PLOTS
# ============================================================

from pathlib import Path
import csv
import math
from dataclasses import replace
from typing import List, Dict

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

RUN_CACHE = {}


# Experimental Xe diffusivity points digitized approximately from
# Schneider et al. (2026), Fig. 7; original experimental sources are
# Biddle et al., Kim et al., Melehan et al., and Oi et al.
#
# Embedded directly in the notebook so the overlay does NOT depend on an
# external CSV file. Values are stored as (T [K], log10(D_Xe [m^2/s])).
SCHNEIDER_FIG7_EXP_POINTS = {
    "Melehan et al.": [
        (1288, -19.3), (1296, -19.8),
        (1668, -17.5), (1668, -17.1),
        (1768, -17.0), (1768, -16.6),
        (1878, -16.4), (1885, -16.0),
    ],
    "Biddle et al.": [
        (1085, -22.6),
        (1280, -20.4), (1290, -20.0),
        (1490, -19.8),
        (1665, -18.6),
        (1860, -17.4),
    ],
    "Kim et al.": [
        (1478, -17.8), (1482, -18.2), (1485, -18.9), (1487, -19.4),
        (1575, -18.6),
        (1678, -18.4), (1682, -17.9),
        (1865, -18.2), (1868, -17.8),
    ],
    "Oi et al.": [
        (1530, -14.9),
        (1760, -14.2),
        (1860, -14.1), (1880, -13.9),
        (2080, -13.1), (2160, -13.2),
        (2340, -12.5), (2360, -12.4),
        (2500, -12.3),
    ],
}

# Deliberately use markers different from Schneider Fig. 7.
EXP_XE_MARKERS = {
    "Biddle et al.": "x",
    "Kim et al.": "+",
    "Melehan et al.": "P",
    "Oi et al.": "X",
}
EXP_XE_LABELS = {
    name: f"{name} (exp. Xe diffusivity)" for name in EXP_XE_MARKERS
}

def load_schneider_fig7_exp_points():
    records = []
    for experiment, points in SCHNEIDER_FIG7_EXP_POINTS.items():
        for T_K, log10_D in points:
            records.append({
                "experiment": experiment,
                "T_K": float(T_K),
                "log10_D_m2_s": float(log10_D),
                "D_m2_s": 10.0 ** float(log10_D),
            })
    return pd.DataFrame.from_records(records)

# ---- Rizk 2025 Fig. 9 digitized gas-partition reference curves ----
# Added in 7test_UN.
# Source file used to build these arrays:
# gas_partition_digitized_wide_25K.csv
#
# NOTE:
# - 8test separates grain-face bubbles and FGR in the model and in the Rizk overlay.
RIZK2025_GAS_PARTITION = {
    1.1: {
        "T_K": np.array([900.0, 925.0, 950.0, 975.0, 1000.0, 1025.0, 1050.0, 1075.0, 1100.0, 1125.0, 1150.0, 1175.0, 1200.0, 1225.0, 1250.0, 1275.0, 1300.0, 1325.0, 1350.0, 1375.0, 1400.0, 1425.0, 1450.0, 1475.0, 1500.0, 1525.0, 1550.0, 1575.0, 1600.0, 1625.0, 1650.0, 1675.0, 1700.0, 1725.0, 1750.0, 1775.0, 1800.0, 1825.0, 1850.0, 1875.0, 1900.0, 1925.0, 1950.0, 1975.0, 2000.0, 2025.0, 2050.0, 2075.0, 2100.0, 2125.0, 2150.0, 2175.0, 2200.0, 2225.0, 2250.0, 2275.0, 2300.0, 2325.0, 2350.0, 2375.0, 2400.0, 2425.0, 2450.0, 2475.0, 2500.0, 2525.0, 2550.0, 2575.0, 2600.0], dtype=float),
        "Matrix": np.array([5.9073, 5.888, 5.8687, 5.806, 5.7915, 5.7143, 5.7143, 5.6757, 5.6371, 5.6178, 5.5792, 5.5598, 5.5598, 5.5598, 5.5212, 5.5212, 5.4826, 5.4826, 5.444, 5.4054, 5.3282, 5.2239, 4.9807, 4.5907, 4.1313, 3.473, 2.8185, 2.1115, 1.7761, 1.1403, 0.695, 0.5985, 0.5405, 0.5019, 0.4633, 0.4247, 0.4247, 0.4247, 0.4247, 0.4247, 0.4247, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861], dtype=float),
        "Bulk bubbles": np.array([77.3745, 77.405, 77.471, 77.5386, 77.6448, 77.7262, 77.7799, 77.8407, 77.8764, 77.9416, 77.9549, 78.0444, 78.0695, 78.1467, 78.1367, 78.1588, 78.2239, 78.2722, 78.3205, 78.3301, 78.4098, 78.4556, 78.4942, 78.4942, 78.3956, 78.181, 77.7992, 76.5202, 75.0, 73.3809, 70.6178, 67.7992, 64.9421, 62.0463, 58.61, 55.184, 48.61, 42.21, 34.4402, 27.9151, 21.4286, 16.7588, 12.0463, 9.2157, 6.4479, 4.7746, 3.417, 2.6823, 1.9305, 1.6024, 1.2893, 0.8083, 0.7529, 0.5695, 0.3861, 0.492, 0.3861, 0.4247, 0.4054, 0.3921, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861], dtype=float),
        "Dislocation bubbles": np.array([15.251, 15.2124, 15.1931, 15.1737, 15.1351, 15.0965, 15.0965, 15.0579, 15.0386, 15.0253, 15.0193, 14.9566, 14.9421, 14.9095, 14.9035, 14.8649, 14.8649, 14.8649, 14.8263, 14.8456, 14.8649, 14.9839, 15.0965, 15.5318, 16.2548, 16.8103, 17.9537, 19.5282, 21.4286, 24.0486, 26.139, 29.4062, 32.471, 35.1888, 38.8996, 42.7152, 48.7066, 55.7746, 62.9344, 69.7237, 76.1004, 78.8404, 82.3674, 86.2684, 89.6367, 91.5387, 92.8185, 93.7637, 94.4015, 94.844, 95.3282, 95.5403, 95.6757, 95.8098, 95.9459, 96.0002, 96.0878, 96.1004, 96.139, 96.1776, 96.2062, 96.1999, 96.2162, 96.2355, 96.2548, 96.2219, 96.2212, 96.2355, 96.2355], dtype=float),
        "Grain boundary bubbles": np.array([2.1622, 2.1947, 2.1622, 2.1525, 2.1622, 2.1236, 2.1236, 2.1236, 2.1236, 2.1266, 2.1236, 2.1205, 2.1236, 2.1236, 2.1236, 2.1236, 2.0849, 2.1042, 2.1236, 2.0656, 2.0077, 1.9137, 1.5444, 1.412, 1.3803, 1.4149, 1.2487, 1.0877, 1.1197, 1.1019, 1.1583, 1.11, 1.1197, 1.1197, 1.1147, 1.11, 1.1197, 1.1724, 1.1969, 1.209, 1.1969, 1.1969, 1.2162, 1.1969, 1.1969, 1.1969, 1.1583, 1.1257, 1.1583, 1.1197, 1.0811, 1.0425, 1.1197, 1.0425, 1.0425, 1.0008, 1.0039, 0.9978, 0.9653, 0.9653, 0.9459, 0.9266, 0.9266, 0.9073, 0.888, 0.888, 0.888, 0.8977, 0.9073], dtype=float),
        "FGR": np.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5241, 0.8108, 0.8784, 1.0425, 1.0039, 1.3127, 1.4937, 1.4872, 1.6876, 1.8533, 1.8147, 2.0463, 2.0656, 2.1622, 2.2587, 2.3552, 2.4903, 2.5869, 2.6963, 2.8185, 2.8813, 2.8958, 2.9488, 3.0116, 3.0502, 2.973, 3.0888, 3.1274, 3.166, 3.2046, 3.2046, 3.2046, 3.2239, 3.2432, 3.2432, 3.2819, 3.2819, 3.3205, 3.3205, 3.3205, 3.3591, 3.3591, 3.3591, 3.3591, 3.3591, 3.3591, 3.3591, 3.3591], dtype=float),
    },
    3.2: {
        "T_K": np.array([900.0, 925.0, 950.0, 975.0, 1000.0, 1025.0, 1050.0, 1075.0, 1100.0, 1125.0, 1150.0, 1175.0, 1200.0, 1225.0, 1250.0, 1275.0, 1300.0, 1325.0, 1350.0, 1375.0, 1400.0, 1425.0, 1450.0, 1475.0, 1500.0, 1525.0, 1550.0, 1575.0, 1600.0, 1625.0, 1650.0, 1675.0, 1700.0, 1725.0, 1750.0, 1775.0, 1800.0, 1825.0, 1850.0, 1875.0, 1900.0, 1925.0, 1950.0, 1975.0, 2000.0, 2025.0, 2050.0, 2075.0, 2100.0, 2125.0, 2150.0, 2175.0, 2200.0, 2225.0, 2250.0, 2275.0, 2300.0, 2325.0, 2350.0, 2375.0, 2400.0, 2425.0, 2450.0, 2475.0, 2500.0, 2525.0, 2550.0, 2575.0, 2600.0], dtype=float),
        "Matrix": np.array([2.6255, 2.6255, 2.6401, 2.5982, 2.5483, 2.5097, 2.471, 2.4627, 2.4364, 2.4623, 2.4395, 2.4326, 2.4284, 2.4022, 2.3938, 2.3327, 2.3166, 2.3434, 2.4825, 2.3552, 2.3166, 2.3021, 2.2008, 2.0849, 1.9305, 1.7535, 1.4649, 1.0425, 0.8108, 0.6757, 0.5792, 0.5019, 0.4633, 0.4633, 0.4247, 0.4247, 0.4247, 0.4247, 0.4247, 0.4247, 0.4247, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861, 0.3861], dtype=float),
        "Bulk bubbles": np.array([88.8, 88.8687, 88.9333, 88.9812, 89.0, 89.0, 89.0, 89.0, 89.0, 89.0, 89.0, 89.0, 89.0, 89.0086, 89.0312, 89.0633, 89.1, 89.1367, 89.1688, 89.1914, 89.2, 89.1938, 89.1757, 89.1465, 89.1069, 89.0579, 89.0, 88.8962, 88.7018, 88.4065, 88.0, 86.8122, 85.0, 83.0534, 81.0, 78.9657, 74.8649, 70.1761, 64.2471, 56.562, 48.8417, 42.4687, 34.6525, 28.7452, 22.8571, 18.1425, 13.5135, 10.6564, 7.8378, 6.1669, 4.749, 3.7162, 3.0116, 2.4388, 1.9348, 1.5042, 1.1518, 0.8822, 0.7, 0.5738, 0.4712, 0.3955, 0.35, 0.3222, 0.298, 0.278, 0.2629, 0.2533, 0.25], dtype=float),
        "Dislocation bubbles": np.array([6.7568, 6.7568, 6.7181, 6.6988, 6.6795, 6.6795, 6.6409, 6.612, 6.6023, 6.5637, 6.5251, 6.5251, 6.5251, 6.4686, 6.4575, 6.4286, 6.4093, 6.4093, 6.3807, 6.3707, 6.3707, 6.39, 6.4479, 6.5782, 6.7568, 6.9719, 7.3745, 7.8736, 8.6873, 9.6435, 10.5792, 11.8383, 13.2625, 14.4156, 16.2741, 17.6938, 21.9691, 25.681, 32.1622, 39.5969, 46.2934, 53.6838, 60.1931, 65.7035, 70.8494, 75.4363, 79.9614, 82.7536, 85.2317, 87.5945, 88.8803, 89.9364, 91.0039, 91.4443, 91.9305, 92.2418, 92.471, 92.7314, 92.8571, 92.9987, 93.0502, 93.1178, 93.2046, 93.2239, 93.2819, 93.2847, 93.3205, 93.3205, 93.3205], dtype=float),
        "Grain boundary bubbles": np.array([2.5483, 2.5097, 2.5097, 2.5097, 2.5097, 2.5097, 2.471, 2.4807, 2.4324, 2.471, 2.4324, 2.4324, 2.4324, 2.4324, 2.4517, 2.4324, 2.4324, 2.4324, 2.278, 1.7448, 1.6602, 1.4607, 1.5444, 1.5444, 1.3514, 1.1583, 1.1969, 1.2268, 1.2741, 1.3401, 1.3514, 1.4358, 1.4865, 1.5058, 1.5444, 1.6409, 1.7375, 1.9813, 2.1236, 2.2524, 2.4324, 2.5686, 2.6917, 2.7992, 2.8571, 2.9585, 2.973, 3.0381, 3.0116, 3.0792, 3.0888, 3.0888, 3.0792, 3.0888, 3.0888, 3.0888, 3.0888, 3.0858, 3.0695, 3.0502, 3.0502, 3.0502, 3.0475, 3.0199, 3.0116, 3.0279, 3.0116, 3.0116, 3.0116], dtype=float),
        "FGR": np.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5019, 0.7641, 0.9653, 1.0618, 1.1197, 1.1728, 1.39, 1.448, 1.6023, 1.5927, 1.7375, 1.8349, 1.8919, 2.0206, 2.0077, 2.0994, 2.2008, 2.3301, 2.5097, 2.7102, 2.8958, 3.0502, 3.2432, 3.3784, 3.4363, 3.668, 3.7066, 3.7838, 3.861, 3.8996, 3.9382, 3.9382, 3.9768, 4.112, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0541, 4.0927, 4.0927], dtype=float),
    },
}

def rizk2025_partition_reference_for_burnup(bu: float):
    """Return digitized Rizk 2025 gas partition for 1.1 or 3.2% FIMA."""
    if abs(float(bu) - 1.1) < 1e-6:
        return RIZK2025_GAS_PARTITION[1.1]
    if abs(float(bu) - 3.2) < 1e-6:
        return RIZK2025_GAS_PARTITION[3.2]
    return None

PARTITION_PLOT_STYLES = {
    "Matrix": {"color": "tab:blue", "model_label": "Model Matrix (solution)", "ref_label": "Rizk 2025 Matrix"},
    "Bulk bubbles": {"color": "tab:orange", "model_label": "Model Bulk bubbles", "ref_label": "Rizk 2025 Bulk"},
    "Dislocation bubbles": {"color": "tab:green", "model_label": "Model Dislocation bubbles", "ref_label": "Rizk 2025 Dislocation"},
    "Grain boundary bubbles": {"color": "gold", "model_label": "Model Grain-face bubbles", "ref_label": "Rizk 2025 GB bubbles"},
    "FGR": {"color": "tab:red", "model_label": "Model FGR / released", "ref_label": "Rizk 2025 FGR"},
}


def plot_rizk2025_partition_dashed_lines(bu: float):
    """Overlay dashed Rizk 2025 Fig. 9 partition curves with the same colors as the model curves.

    IMPORTANT: Rizk curves are individual fractions from 0 to 100, not stacked/cumulative.
    """
    ref = rizk2025_partition_reference_for_burnup(bu)
    if ref is None:
        return

    T_ref = ref["T_K"]
    for key, style in PARTITION_PLOT_STYLES.items():
        plt.plot(
            T_ref,
            ref[key],
            "--",
            color=style["color"],
            linewidth=2.0,
            label=style["ref_label"],
        )



def ensure_output_dir(path=OUTPUT_DIR):
    Path(path).mkdir(parents=True, exist_ok=True)

def make_candidate(label=CASE_LABEL) -> Candidate:
    return Candidate(label=label, **dict(MANUAL_PARAMS))

# ---- Optional rho_d models ----
RHO_FAB_DEFAULT = 3.0e13
C1_RB = 1.6e14
F0_RB = 2.4
TREF_RB = 1025.0
RHO_SAT_RHO940 = 6.3571
RHO_SAT_RHOINF = 9.1036
RHO_SAT_TAU_K = 203.76

# Rizk 2023 empirical dislocation-density model.
# Eq. form: rho = rho0 * exp(c*F / max(Td, T0 - T)), capped at rho_cap.
# F must be the FIMA fraction, not the percent value.
def rho_rizk2023(T: float, burnup_percent: float) -> float:
    F_fima_fraction = float(burnup_percent) / 100.0
    denominator_K = max(float(RIZK2023_TD), float(RIZK2023_T0) - float(T))
    exponent = float(RIZK2023_C) * F_fima_fraction / denominator_K
    rho = float(RIZK2023_RHO0) * math.exp(exponent)
    return min(rho, float(RIZK2023_RHO_CAP))


# ---- Rizk 2023/Centipede digitized rho_d(F,T) fit ----
F_GRID = np.array([0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0, 4.25, 4.5, 4.75, 5.0, 5.25, 5.5, 5.75, 6.0, 6.25, 6.5, 6.75, 7.0, 7.25, 7.5, 7.75, 8.0, 8.25, 8.5, 8.75, 9.0, 9.25, 9.5, 9.75, 10.0, 10.25, 10.5, 10.75, 11.0, 11.25, 11.5, 11.75, 12.0, 12.25, 12.5, 12.75, 13.0], dtype=float)
T_SLICES = np.array([1000.0, 1200.0, 1400.0, 1600.0, 1800.0], dtype=float)

RHO_CURVES = {
    1000.0: np.array([181208053691275.2, 261169902929109.03, 326019617965926.7, 374176759288834.7, 416623644811564.2, 460786278949972.25, 503355704697986.5, 543016112222848.1, 581569437274135.2, 620662430843226.6, 659008776458440.9, 695841623278697.6, 731414558595766.6, 765403939467177.2, 798786783686112.5, 832472896231285.5, 866159008776458.4, 900171926891099.2, 933531233866804.2, 965002956949891.8, 995869901910170.4, 1027959164192910.0, 1058208569953536.4, 1083449465924263.4, 1107382550335570.4, 1132476888726398.8, 1157718120805369.0, 1182885906040268.2, 1208053691275167.8, 1233920581655481.0, 1258389261744966.2, 1279082774049216.8, 1298657718120805.2, 1318791946308724.8, 1338926174496644.2, 1359060402684563.8, 1379194630872483.2, 1399328859060402.8, 1419463087248322.0, 1439597315436241.5, 1459731543624161.0, 1480584851390220.5, 1500000000000000.0, 1515124552012538.2, 1530201342281879.2, 1550036066064961.2, 1569953536396489.2, 1585837147806939.8, 1600671140939597.2, 1615409246349507.8, 1630872483221476.8, 1648724611134386.2, 1648724611134386.2], dtype=float),
    1200.0: np.array([100671140939597.31, 160452558987948.7, 225348477026329.38, 296660507853147.9, 371321631388745.44, 446244192049561.2, 521166752710376.9, 594889344082562.8, 667139907072793.0, 736163417866146.4, 805369127516778.5, 886403777429704.4, 952891068662880.6, 981334490642886.8, 1012132163138874.4, 1114519310626566.4, 1239029426948889.8, 1320062985066368.8, 1391584925141972.0, 1460633217844681.5, 1528652555498193.0, 1601243263000018.0, 1672302529685080.0, 1736334546345922.2, 1798141455859576.8, 1861208052258096.0, 1922818791946308.8, 1980123902942694.8, 2037429013939080.5, 2101259726674828.8, 2162106350025812.8, 2209834593382154.0, 2256582343830665.0, 2313829499678579.5, 2371192565823438.0, 2419406518330112.0, 2466442953020134.0, 2518171237902952.0, 2570211667527103.5, 2619773625834206.0, 2667785234899328.5, 2713946573257701.0, 2759163655136809.0, 2804174443694320.0, 2848993288590604.0, 2893619122376745.0, 2938822922044398.0, 2959215281362932.0, 2979607640681466.0, 3000000000000000.0, 3000000000000000.0, 3000000000000000.0, 3000000000000000.0], dtype=float),
    1400.0: np.array([120805369127516.77, 263006539033238.25, 406556530717604.5, 567863722028916.4, 697728446050593.8, 773388993383370.6, 810586093524984.2, 813980257279062.4, 815171317555634.0, 815171317555634.0, 815171317555634.0, 815171317555634.0, 815171317555634.0, 815171317555634.0, 815171317555634.0, 815303779583186.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.0, 815436241610738.1, 815436241610738.2, 815436241610738.2], dtype=float),
    1600.0: np.array([90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58, 90604026845637.58], dtype=float),
    1800.0: np.array([50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66, 50335570469798.66], dtype=float)
}

FIG411_T = np.array([1000.0, 1010.0, 1020.0, 1030.0, 1040.0, 1050.0, 1060.0, 1070.0, 1080.0, 1090.0, 1100.0, 1110.0, 1120.0, 1130.0, 1140.0, 1150.0, 1160.0, 1170.0, 1180.0, 1190.0, 1200.0, 1210.0, 1220.0, 1230.0, 1240.0, 1250.0, 1260.0, 1270.0, 1280.0, 1290.0, 1300.0, 1310.0, 1320.0, 1330.0, 1340.0, 1350.0, 1360.0, 1370.0, 1380.0, 1390.0, 1400.0, 1410.0, 1420.0, 1430.0, 1440.0, 1450.0, 1460.0, 1470.0, 1480.0, 1490.0, 1500.0, 1510.0, 1520.0, 1530.0, 1540.0, 1550.0, 1560.0, 1570.0, 1580.0, 1590.0, 1600.0, 1610.0, 1620.0, 1630.0, 1640.0, 1650.0, 1660.0, 1670.0, 1680.0, 1690.0, 1700.0, 1710.0, 1720.0, 1730.0, 1740.0, 1750.0, 1760.0, 1770.0, 1780.0, 1790.0, 1800.0], dtype=float)
FIG411_NORMAL = np.array([1238475361116869.8, 1206337687469762.5, 1173819890801022.8, 1140921971110651.5, 1103900292579539.2, 1071751100052987.8, 1042881497059395.6, 1018905772275045.4, 1009279280168768.8, 1012806251350726.8, 1038959044349880.8, 1086989609583554.0, 1164274558990539.8, 1255222815757060.5, 1358195289791762.8, 1464662325664722.0, 1582389293675001.0, 1696864500065623.2, 1815302492939039.2, 1938245467553644.8, 2059809596413371.5, 2186839463820598.0, 2332739554877922.0, 2493570412815698.0, 2654799810222290.0, 2817095956383626.0, 2960206583514625.0, 3085054308231974.0, 3183770904787842.0, 3238297996791840.5, 3207847433577905.0, 3103706625722913.0, 2930258834476760.0, 2692509540684900.0, 2437700514402336.5, 2179489306379048.0, 1920357113814714.0, 1669195322395927.5, 1424536574663567.2, 1182259008646188.8, 968550857726628.2, 794358996089738.8, 666341484684512.8, 568211961742959.6, 502287021154946.2, 452830188679245.75, 408805031446541.25, 369272237196765.8, 332434860736747.8, 300089847259658.8, 270440251572327.3, 238095238095238.38, 212039532794250.0, 187780772686433.28, 168912848158131.34, 157232704402515.84, 142857142857142.97, 132075471698113.31, 119496855345912.06, 107816711590296.6, 97933513027852.77, 88948787061994.7, 79964061096136.64, 76370170709793.42, 75471698113207.61, 74573225516621.81, 69182389937106.98, 62893081761006.34, 57502246181491.52, 54806828391734.1, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.71, 56603773584905.69, 56603773584905.695, 56603773584905.695], dtype=float)
FIG411_SHIFTED = np.array([842767295597484.2, 823899371069182.4, 795597484276729.6, 773584905660377.4, 745283018867924.6, 743396226415094.2, 701886792452830.1, 701886792452830.1, 683962264150943.2, 679245283018867.9, 694339622641509.4, 716981132075471.6, 730458221024258.8, 805031446540880.5, 830188679245283.0, 905660377358490.6, 932614555256064.8, 1009433962264151.0, 1066037735849056.4, 1122641509433962.4, 1194339622641509.2, 1264150943396226.2, 1330188679245283.0, 1452830188679245.5, 1567924528301886.8, 1665768194070080.8, 1739279588336192.0, 1816981132075472.0, 1905660377358490.8, 1935849056603773.2, 1949685534591195.2, 1903563941299790.5, 1822102425876011.2, 1747169811320754.8, 1617924528301886.8, 1473413379073756.2, 1485062893081761.0, 1286037735849056.8, 1091737150292778.0, 932075471698113.2, 781354051054384.0, 675471698113207.5, 600000000000000.0, 533892101892775.7, 473217237387065.75, 420000000000000.0, 372225988700564.94, 327254237288135.6, 286169491525423.7, 250056497175141.2, 220000000000000.0, 194666666666666.62, 172000000000000.0, 152000000000000.03, 134666666666666.66, 120000000000000.0, 107052121212121.2, 95156363636363.64, 84734545454545.45, 76208484848484.86, 70000000000000.01, 65318636363636.37, 61076363636363.64, 57248636363636.37, 53810909090909.09, 50738636363636.37, 48007272727272.734, 45592272727272.734, 43469090909090.91, 41613181818181.81, 40000000000000.0, 38505000000000.0, 37040000000000.0, 35635000000000.0, 34320000000000.004, 33125000000000.0, 32080000000000.0, 31215000000000.0, 30560000000000.0, 30145000000000.0, 30000000000000.0], dtype=float)


def _interp_F_curve(F_percent, T_slice):
    F = np.asarray(F_percent, dtype=float)
    y = RHO_CURVES[float(T_slice)]
    return np.interp(F, F_GRID, y, left=y[0], right=y[-1])


def rho_base_fig410(F_percent, T_K):
    F = np.asarray(F_percent, dtype=float)
    T = np.asarray(T_K, dtype=float)
    F_b, T_b = np.broadcast_arrays(F, T)
    flatF = F_b.ravel()
    flatT = T_b.ravel()
    vals_by_T = np.vstack([_interp_F_curve(flatF, Ts) for Ts in T_SLICES])
    out = np.empty_like(flatF, dtype=float)
    for i, Ti in enumerate(flatT):
        out[i] = np.interp(Ti, T_SLICES, vals_by_T[:, i], left=vals_by_T[0, i], right=vals_by_T[-1, i])
    return out.reshape(F_b.shape)


def _interp_fig411(T_K, variant):
    T = np.asarray(T_K, dtype=float)
    if variant == "normal":
        y = FIG411_NORMAL
    elif variant == "shifted":
        y = FIG411_SHIFTED
    else:
        raise ValueError("variant must be 'normal' or 'shifted'")
    return np.interp(T, FIG411_T, y, left=y[0], right=y[-1])


def _correction_factor(T_K, variant):
    T = np.asarray(T_K, dtype=float)
    base_ref = rho_base_fig410(np.full_like(T, 6.8, dtype=float), T)
    target_ref = _interp_fig411(T, variant)
    return np.clip(target_ref / np.maximum(base_ref, 1.0e10), 0.2, 4.0)


def _burnup_blend_weight(F_percent, F_ref=6.8):
    F = np.asarray(F_percent, dtype=float)
    return np.clip(F / F_ref, 0.0, 1.0) ** 0.7


def rho_d_rizk2023_centipede(F_percent, T_K, variant="shifted", rho_cap=4.0e15):
    """
    Return dislocation line density [m^-2].
    """
    base = rho_base_fig410(F_percent, T_K)
    if variant == "fig410":
        rho = base
    elif variant in ("normal", "shifted"):
        F = np.asarray(F_percent, dtype=float)
        T = np.asarray(T_K, dtype=float)
        F_b, T_b = np.broadcast_arrays(F, T)
        C = _correction_factor(T_b, variant)
        w = _burnup_blend_weight(F_b)
        rho = base * (1.0 + w * (C - 1.0))
    else:
        raise ValueError("variant must be 'fig410', 'normal', or 'shifted'")
    return np.clip(rho, 1.0e12, rho_cap)




def rho_rizk2023_smooth_monotone(T: float, burnup_percent: float) -> float:
    """
    Smooth Rizk-2023-inspired dislocation density [m^-2].

    Purpose: keep the qualitative idea that rho_d decreases at high temperature,
    but remove the artificial mid-temperature peak/kink from the digitized Fig. 4.10+4.11 fit.

    rho = rho_fab + scale*rho_amp*(1-exp(-F/Fc))*f_T(T)
    f_T = f_min + (1-f_min)/(1+exp((T-T_half)/width))

    This is smooth, continuous, monotonic decreasing in T, and monotonic increasing/saturating in burnup.
    """
    F = max(float(burnup_percent), 0.0)
    T = float(T)
    rho_fab = float(RHO_RIZK2023_SMOOTH_RHO_FAB)
    amp = float(RHO_RIZK2023_SMOOTH_RHO_AMP)
    scale = float(RHO_RIZK2023_SMOOTH_SCALE)
    Fc = max(float(RHO_RIZK2023_SMOOTH_FC_PERCENT), 1.0e-12)
    T_half = float(RHO_RIZK2023_SMOOTH_T_HALF)
    width = max(float(RHO_RIZK2023_SMOOTH_WIDTH), 1.0e-12)
    f_min = min(max(float(RHO_RIZK2023_SMOOTH_F_MIN), 0.0), 1.0)
    cap = float(RHO_RIZK2023_SMOOTH_CAP)

    burnup_part = 1.0 - math.exp(-F / Fc)
    # overflow-safe logistic for high-T decrease
    x = (T - T_half) / width
    if x > 80.0:
        logistic_decrease = 0.0
    elif x < -80.0:
        logistic_decrease = 1.0
    else:
        logistic_decrease = 1.0 / (1.0 + math.exp(x))
    fT = f_min + (1.0 - f_min) * logistic_decrease
    rho = rho_fab + scale * amp * burnup_part * fT
    return min(max(rho, 1.0e10), cap)

def rho_sat_shape10(T: float) -> float:
    return RHO_SAT_RHOINF - (RHO_SAT_RHOINF - RHO_SAT_RHO940) * math.exp(-(float(T) - 940.0) / RHO_SAT_TAU_K)

def rho_sat_factor_raw(T: float) -> float:
    return rho_sat_shape10(float(T)) / rho_sat_shape10(TREF_RB)

def rho_burnup_1025(F_a_o: float) -> float:
    rho_bu = C1_RB * max(float(F_a_o) - F0_RB, 0.0)
    return max(RHO_FAB, rho_bu)

def effective_rho_d(T: float, burnup: float, cand: Candidate) -> float:
    if RHO_MODE == "constant":
        return cand.rho_d
    if RHO_MODE == "rhoSat_RayBlank":
        return max(rho_burnup_1025(burnup) * RHO_SCALE * rho_sat_factor_raw(T), 1.0e10)
    if RHO_MODE == "rizk2023":
        return max(rho_rizk2023(T, burnup), 1.0e10)
    if RHO_MODE == "rizk2023_fit":
        rho_fit = rho_d_rizk2023_centipede(
            F_percent=burnup,
            T_K=T,
            variant=RHO_RIZK2023_FIT_VARIANT,
            rho_cap=RHO_RIZK2023_FIT_CAP,
        )
        return max(float(RHO_RIZK2023_FIT_SCALE) * float(rho_fit), 1.0e10)
    if RHO_MODE == "rizk2023_smooth":
        return max(rho_rizk2023_smooth_monotone(T, burnup), 1.0e10)
    raise ValueError(f"Unknown RHO_MODE={RHO_MODE!r}")

def run_model_point(T: float, burnup: float, cand: Candidate, dt_h=DT_H, n_modes=N_MODES, keep_history=False):
    rho_eff = effective_rho_d(T, burnup, cand)
    cache_key = (
        cand, float(T), float(burnup), float(dt_h), int(n_modes), bool(keep_history),
        USE_PHI_GAS_RESOLUTION, USE_NUCLEATION_MASS_COUPLING, USE_BULK_DISLOCATION_CAPTURE,
        USE_DYNAMIC_RHO_D_NUCLEATION, USE_INTERGRANULAR_MODEL, USE_LENTICULAR_COS3,
        A20_VU_DEFAULT, RHO_MODE, RHO_SCALE, RHO_RIZK2023_FIT_VARIANT,
        RHO_RIZK2023_FIT_SCALE, RHO_RIZK2023_FIT_CAP,
        RHO_RIZK2023_SMOOTH_SCALE, RHO_RIZK2023_SMOOTH_RHO_FAB, RHO_RIZK2023_SMOOTH_RHO_AMP,
        RHO_RIZK2023_SMOOTH_FC_PERCENT, RHO_RIZK2023_SMOOTH_T_HALF, RHO_RIZK2023_SMOOTH_WIDTH,
        RHO_RIZK2023_SMOOTH_F_MIN, RHO_RIZK2023_SMOOTH_CAP,
        XE_DIFFUSIVITY_MODE, VU_DIFFUSIVITY_MODE,
        DV_GB_MODE, DV_GB_MULTIPLIER, MILLER_DV_GB_D0, MILLER_DV_GB_Q_EV,
        GF_VACANCY_ABSORPTION_ONLY, INTERGRANULAR_POST_SAT_MODE,
        USE_WS_GAS_TRAPPING_BULK, USE_WS_GAS_TRAPPING_DISLOCATION, WS_GAS_TRAPPING_MAX_FACTOR,
        USE_FIRST_GAS_PRESSURE_SEED, FIRST_GAS_PRESSURE_FACTOR,
        SEED_BULK_ON_FIRST_GAS, SEED_DISLOCATION_ON_FIRST_GAS,
        USE_EQUILIBRIUM_DIMER_SEED, SEED_GAS_ATOMS_PER_BUBBLE, SEED_PRESSURE_FACTOR,
        SEED_INITIAL_DISLOCATION_BUBBLES, SEED_NEW_BULK_BUBBLES,
        USE_ZETA_OVERFLOW_GUARD, NGF_AREAL_0, DELTA_GB, FC_SAT,
        THETA_GF_DEG, R_GF0, GAMMA_GB,
        PHI_B_SCALE_CURRENT, NUCLEATION_CLUSTER_SIZE_CURRENT,
    )
    if not keep_history and cache_key in RUN_CACHE:
        return RUN_CACHE[cache_key]

    p = UNParameters(
        temperature=float(T),
        fission_rate=cand.fission_rate,
        grain_radius=GRAIN_RADIUS,
        target_burnup_percent_fima=float(burnup),
        dt=float(dt_h) * 3600.0,
        n_modes=int(n_modes),
        xe_yield=XE_YIELD,

        f_n=cand.f_n,
        K_d=cand.K_d,
        rho_d=rho_eff,

        Dv_scale=cand.Dv_scale,
        Dv_D1_scale=cand.Dv_D1_scale,
        Dv_D2_scale=cand.Dv_D2_scale,
        Dv_dislocation_scale=cand.Dv_dislocation_scale,
        vacancy_diffusivity_mode=VU_DIFFUSIVITY_MODE,
        A20_vU=A20_VU_DEFAULT,
        B21_vU=B21_VU,
        B22_vU=B22_VU,

        Dg_scale=cand.Dg_scale,
        Dg_D1_scale=cand.Dg_D1_scale,
        Dg_D3_scale=cand.Dg_D3_scale,
        Dg_dislocation_scale=cand.Dg_dislocation_scale,
        D2_xe_scale=getattr(cand, "D2_xe_scale", 1.0),
        xe_diffusivity_mode=XE_DIFFUSIVITY_MODE,

        b_scale=cand.b_scale,
        b_bulk_scale=cand.b_bulk_scale,
        b_dislocation_scale=cand.b_dislocation_scale,

        gb_scale=cand.gb_scale,
        gd_scale=cand.gd_scale,
        gd_bubble_scale=cand.gd_bubble_scale,
        gd_line_scale=cand.gd_line_scale,
        gd_line_alpha=cand.gd_line_alpha,

        coalescence_d_scale=cand.coalescence_d_scale,
        capture_scale=cand.capture_scale,
    )

    hist, rates = solve_UN(p, keep_history=keep_history)

    # Last values.
    out = {
        "label": cand.label,
        "xe_diffusivity_mode": XE_DIFFUSIVITY_MODE,
        "vacancy_diffusivity_mode": VU_DIFFUSIVITY_MODE,
        "T": float(T),
        "burnup": float(burnup),
        "rho_d_eff": rho_eff,

        "c": hist["c"][-1],
        "mb": hist["mb"][-1],
        "md": hist["md"][-1],
        "Nb": hist["Nb"][-1],
        "Nd": hist["Nd"][-1],
        "Rb_nm": hist["Rb"][-1] * 1e9,
        "Rd_nm": hist["Rd"][-1] * 1e9,

        "swelling_b_percent": 100.0 * hist["swelling_b"][-1],
        "swelling_d_percent": 100.0 * hist["swelling_d"][-1],
        "swelling_ig_percent": 100.0 * hist["swelling_ig"][-1],
        "swelling_gf_percent": 100.0 * hist.get("swelling_gf", [0.0])[-1],
        "swelling_total_gas_percent": 100.0 * hist.get("swelling_total_gas", hist["swelling_ig"])[-1],

        "Ngf_areal": hist.get("Ngf_areal", [math.nan])[-1],
        "Ngf_vol": hist.get("Ngf_vol", [math.nan])[-1],
        "Rgf_nm": hist.get("R_gf", [math.nan])[-1] * 1e9,
        "Fc_gf": hist.get("Fc_gf", [math.nan])[-1],
        "p_gf": hist.get("p_gf", [math.nan])[-1],
        "p_gf_eq": hist.get("p_gf_eq", [math.nan])[-1],
        "p_gf_over_eq": (hist.get("p_gf", [math.nan])[-1] / hist.get("p_gf_eq", [math.nan])[-1]) if hist.get("p_gf_eq", [0.0])[-1] else math.nan,

        "p_b": hist["p_b"][-1],
        "p_b_eq": hist["p_b_eq"][-1],
        "p_d": hist["p_d"][-1],
        "p_d_eq": hist["p_d_eq"][-1],
        "p_b_over_eq": hist["p_b"][-1] / hist["p_b_eq"][-1] if hist["p_b_eq"][-1] else math.nan,
        "p_d_over_eq": hist["p_d"][-1] / hist["p_d_eq"][-1] if hist["p_d_eq"][-1] else math.nan,

        "matrix_gas_percent": hist["matrix_gas_percent"][-1],
        "bulk_gas_percent": hist["bulk_gas_percent"][-1],
        "dislocation_gas_percent": hist["dislocation_gas_percent"][-1],
        "qgb_gas_percent": hist["qgb_gas_percent"][-1],
        "grainface_gas_percent": hist.get("grainface_gas_percent", [0.0])[-1],
        "release_gas_percent": hist.get("release_gas_percent", [0.0])[-1],

        "q_gb": hist["q_gb"][-1],
        "q_gf": hist.get("q_gf", [0.0])[-1],
        "q_rel": hist.get("q_rel", [0.0])[-1],
        "generated": hist["generated"][-1],
        "retained": hist["retained"][-1],

        "lambda_d": hist["lambda_d"][-1],
        "nu_b": hist["nu_b"][-1],
        "phi_b": hist["phi_b"][-1],
        "phi_d": hist["phi_d"][-1],
        "f_cap_step": hist["f_cap_step"][-1],
        "cap_raw_step": hist["cap_raw_step"][-1],
        "capture_fraction_sum": hist["capture_fraction_sum"][-1],
        "capture_raw_sum": hist["capture_raw_sum"][-1],
        "capture_bubbles_cumulative": hist["capture_bubbles_cumulative"][-1],
        "max_f_cap_step": hist["max_f_cap_step"][-1],

        "hist": hist if keep_history else None,
        "rates": rates,
    }

    # Add rate diagnostics.
    for k, v in rates.items():
        if isinstance(v, (int, float)):
            out[k] = v

    # Geometry diagnostics.
    Rb = hist["Rb"][-1]
    Rd = hist["Rd"][-1]
    Nb = hist["Nb"][-1]
    Nd = hist["Nd"][-1]
    out["psi_b"] = Rb / wigner_seitz_delta(Nb) if Nb > 0 and math.isfinite(Rb) else math.nan
    out["psi_d"] = Rd / wigner_seitz_delta(Nd) if Nd > 0 and math.isfinite(Rd) else math.nan
    out["porosity_b"] = hist["swelling_b"][-1]
    out["porosity_d"] = hist["swelling_d"][-1]
    out["valid_single_size"] = (
        math.isfinite(out["Rd_nm"])
        and math.isfinite(out["Nd"])
        and (out["Nd"] > 1.0e18)
        and (out["psi_d"] < 0.8 if math.isfinite(out["psi_d"]) else False)
        and (out["porosity_d"] < 0.5 if math.isfinite(out["porosity_d"]) else False)
    )

    if not keep_history:
        RUN_CACHE[cache_key] = out
    return out

def temperature_grid(tmax):
    n = int(round((float(tmax) - T_MIN) / T_STEP))
    return [T_MIN + i * T_STEP for i in range(n + 1)]

def simulate_grid(cand: Candidate):
    RUN_CACHE.clear()
    rows = []
    for bu in BURNUPS:
        Ts = temperature_grid(T_MAX_DIAGNOSTIC)
        print(f"Running burnup {bu:g}% FIMA: {len(Ts)} temperature points")
        for T in Ts:
            rows.append(run_model_point(T, bu, cand, DT_H, N_MODES, keep_history=False))
    return rows

def rows_for_burnup(rows, bu):
    return sorted([r for r in rows if abs(r["burnup"] - float(bu)) < 1e-9], key=lambda r: r["T"])

def row_near(rows, bu, T):
    sub = rows_for_burnup(rows, bu)
    return min(sub, key=lambda r: abs(r["T"] - T))

def savefig(name):
    ensure_output_dir()
    path = Path(OUTPUT_DIR) / name
    plt.savefig(path, dpi=180, bbox_inches="tight")
    if not SHOW_PLOTS:
        plt.close()
    return path

def write_csv(rows, cand):
    ensure_output_dir()
    path = Path(OUTPUT_DIR) / f"{cand.label}_grid.csv"
    df = pd.DataFrame([{k: v for k, v in r.items() if k not in ("hist", "rates")} for r in rows])
    df.to_csv(path, index=False)
    return path, df

# ---- plotting helpers ----

def plot_exp_swelling_for_burnup(bu):
    series_names = sorted({p["series"] for p in EXP_SWELLING_T if abs(p["burnup"] - bu) < 1e-9})
    for series in series_names:
        pts = [p for p in EXP_SWELLING_T if abs(p["burnup"] - bu) < 1e-9 and p["series"] == series]
        marker = "x" if "119" in series else "^"
        plt.scatter([p["T"] for p in pts], [p["swelling"] for p in pts], marker=marker, s=65, label=f"Exp P2 {series}", zorder=5)

def plot_swelling_T(rows, bu, cand):
    sub = [r for r in rows_for_burnup(rows, bu) if r["T"] <= T_MAX_SWELLING]
    Ts = [r["T"] for r in sub]
    plt.figure(figsize=(9, 5.8))
    plt.plot(Ts, [r["swelling_d_percent"] for r in sub], label="Model dislocation/P2 swelling")
    plt.plot(Ts, [r["swelling_b_percent"] for r in sub], "--", label="Model bulk swelling")
    plt.plot(Ts, [r["swelling_gf_percent"] for r in sub], "-.", label="Model intergranular/grain-face swelling")
    plot_exp_swelling_for_burnup(bu)
    plt.xlabel("Temperature [K]")
    plt.ylabel("Fission gas swelling [%]")
    plt.title(f"{cand.label}: swelling vs T at {bu:.1f}% FIMA")
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=8)
    return savefig(f"{cand.label}_swelling_T_{bu:.1f}FIMA.png")

def plot_swelling_burnup_1600(cand):
    burnup_grid = [0.2, 0.5, 0.8, 1.1, 1.3, 1.6, 2.0, 2.5, 3.2, 4.0, 5.0, 6.0]
    rows_bu = [run_model_point(1600.0, bu, cand, DT_H, N_MODES, keep_history=False) for bu in burnup_grid]
    plt.figure(figsize=(9, 5.8))
    plt.plot(burnup_grid, [r["swelling_d_percent"] for r in rows_bu], label="Model dislocation/P2 swelling")
    plt.plot(burnup_grid, [r["swelling_b_percent"] for r in rows_bu], "--", label="Model bulk swelling")
    plt.plot(burnup_grid, [r["swelling_gf_percent"] for r in rows_bu], "-.", label="Model intergranular/grain-face swelling")
    plt.scatter([p["burnup"] for p in EXP_SWELLING_BURNUP_1600], [p["swelling"] for p in EXP_SWELLING_BURNUP_1600], marker="^", s=80, label="Exp P2 approx")
    plt.xlabel("Burnup [% FIMA]")
    plt.ylabel("Fission gas swelling [%]")
    plt.title(f"{cand.label}: swelling vs burnup at 1600 K")
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=8)
    return savefig(f"{cand.label}_swelling_vs_burnup_1600K.png")

def existing_bulk_value(r, key):
    """Return finite bulk value only when the bulk population exists."""
    if r.get("Nb", 0.0) > 0.0 and r.get("Rb_nm", 0.0) > 0.0 and r.get("mb", 0.0) > 0.0:
        val = r.get(key, math.nan)
        return val if math.isfinite(val) and val > 0.0 else math.nan
    return math.nan

def existing_dislocation_value(r, key):
    """Return finite dislocation value only when the dislocation population has gas/volume."""
    if r.get("Nd", 0.0) > 0.0 and r.get("Rd_nm", 0.0) > 0.0 and r.get("md", 0.0) > 0.0:
        val = r.get(key, math.nan)
        return val if math.isfinite(val) and val > 0.0 else math.nan
    return math.nan

def plot_radius_concentration_T(rows, cand, bu=1.3):
    """Plot bulk, dislocation and intergranular grain-face R/N together."""
    sub = rows_for_burnup(rows, bu)
    Ts = [r["T"] for r in sub]
    fig, axes = plt.subplots(1, 2, figsize=(13, 4.8))

    ax = axes[0]
    ax.semilogy(Ts, [existing_dislocation_value(r, "Rd_nm") for r in sub], label="Model R_d dislocation")
    ax.semilogy(Ts, [existing_bulk_value(r, "Rb_nm") for r in sub], "--", label="Model R_b bulk")
    ax.semilogy(Ts, [r.get("Rgf_nm", math.nan) for r in sub], "-.", label="Model R_gf intergranular")
    if abs(bu - 1.3) < 1e-9:
        ax.scatter([p["T"] for p in EXP_RD_T_13], [p["R_nm"] for p in EXP_RD_T_13], marker="^", color="red", label="Rizk Fig. 7/8 exp")
    ax.set_xlabel("Temperature [K]")
    ax.set_ylabel("Bubble radius [nm]")
    ax.set_title(f"Bulk + dislocation + intergranular bubble radius ({bu:.1f}% FIMA)")
    ax.grid(True, alpha=0.3, which="both")
    ax.legend(fontsize=8)

    ax = axes[1]
    ax.semilogy(Ts, [existing_dislocation_value(r, "Nd") for r in sub], label="Model N_d dislocation")
    ax.semilogy(Ts, [existing_bulk_value(r, "Nb") for r in sub], "--", label="Model N_b bulk")
    ax.semilogy(Ts, [r.get("Ngf_vol", math.nan) for r in sub], "-.", label="Model N_gf intergranular (vol. equiv.)")
    if abs(bu - 1.3) < 1e-9:
        ax.scatter([p["T"] for p in EXP_ND_T_13], [p["N"] for p in EXP_ND_T_13], marker="^", color="red", label="Rizk Fig. 8 exp")
    ax.set_xlabel("Temperature [K]")
    ax.set_ylabel("Bubble concentration [m$^{-3}$]")
    ax.set_title(f"Bulk + dislocation + intergranular bubble concentration ({bu:.1f}% FIMA)")
    ax.grid(True, alpha=0.3, which="both")
    ax.legend(fontsize=8)

    plt.tight_layout()
    return savefig(f"{cand.label}_RbRd_NbNd_T_{bu:.1f}FIMA.png")

def plot_pressure_T(rows, cand, bu):
    sub = rows_for_burnup(rows, bu)
    Ts = [r["T"] for r in sub]
    plt.figure(figsize=(9, 5.8))
    # Mask non-existing populations; otherwise p_eq uses R_min=1e-15 and shows fake ~1e15 Pa plateaus.
    plt.semilogy(Ts, [existing_bulk_value(r, "p_b") for r in sub], label="p_b bulk")
    plt.semilogy(Ts, [existing_bulk_value(r, "p_b_eq") for r in sub], "--", label="p_b,eq bulk")
    plt.semilogy(Ts, [existing_dislocation_value(r, "p_d") for r in sub], label="p_d dislocation")
    plt.semilogy(Ts, [existing_dislocation_value(r, "p_d_eq") for r in sub], "--", label="p_d,eq dislocation")
    plt.semilogy(
        Ts,
        [r.get("p_gf", math.nan) if math.isfinite(r.get("p_gf", math.nan)) and r.get("p_gf", 0.0) > 0.0 else math.nan for r in sub],
        label="p_gf grain boundary",
    )
    plt.semilogy(
        Ts,
        [r.get("p_gf_eq", math.nan) if math.isfinite(r.get("p_gf_eq", math.nan)) and r.get("p_gf_eq", 0.0) > 0.0 else math.nan for r in sub],
        "--",
        label="p_gf,eq grain boundary",
    )
    plt.xlabel("Temperature [K]")
    plt.ylabel("Pressure [Pa]")
    plt.title(f"{cand.label}: pressure and equilibrium pressure at {bu:.1f}% FIMA")
    plt.grid(True, alpha=0.3, which="both")
    plt.legend(fontsize=8)
    return savefig(f"{cand.label}_pressure_T_{bu:.1f}FIMA.png")

def plot_pressure_ratio_T(rows, cand, bu):
    sub = rows_for_burnup(rows, bu)
    Ts = [r["T"] for r in sub]
    plt.figure(figsize=(9, 5.8))
    bulk_ratio = [
        (r["p_b_over_eq"] if math.isfinite(r.get("p_b_over_eq", math.nan)) and not math.isnan(existing_bulk_value(r, "p_b")) else math.nan)
        for r in sub
    ]
    disl_ratio = [
        (r["p_d_over_eq"] if math.isfinite(r.get("p_d_over_eq", math.nan)) and not math.isnan(existing_dislocation_value(r, "p_d")) else math.nan)
        for r in sub
    ]
    plt.plot(Ts, bulk_ratio, label="p_b/p_b,eq")
    plt.plot(Ts, disl_ratio, label="p_d/p_d,eq")
    plt.axhline(1.0, color="black", linewidth=1, linestyle="--")
    plt.xlabel("Temperature [K]")
    plt.ylabel("Pressure ratio [-]")
    plt.title(f"{cand.label}: pressure ratio at {bu:.1f}% FIMA")
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=8)
    return savefig(f"{cand.label}_pressure_ratio_T_{bu:.1f}FIMA.png")

def plot_gas_partition(rows, cand, bu):
    """Plot gas-partition fractions as direct line-to-line comparison with Rizk.

    Model curves are solid lines; Rizk digitized reference curves are dashed lines
    in the same colors. This avoids the ambiguity of stacked/cumulative areas and
    lets the user immediately see whether the model lies above or below Rizk.
    """
    sub = rows_for_burnup(rows, bu)
    Ts = [r["T"] for r in sub]

    model_series = {
        "Matrix": [r["matrix_gas_percent"] for r in sub],
        "Bulk bubbles": [r["bulk_gas_percent"] for r in sub],
        "Dislocation bubbles": [r["dislocation_gas_percent"] for r in sub],
        "Grain boundary bubbles": [r.get("grainface_gas_percent", 0.0) for r in sub],
        "FGR": [r.get("release_gas_percent", 0.0) for r in sub],
    }

    plt.figure(figsize=(10.0, 6.4))

    for key, style in PARTITION_PLOT_STYLES.items():
        plt.plot(
            Ts,
            model_series[key],
            "-",
            color=style["color"],
            linewidth=2.2,
            label=style["model_label"],
        )

    plot_rizk2025_partition_dashed_lines(bu)

    plt.xlabel("Temperature [K]")
    plt.ylabel("Fraction of generated gas [%]")
    plt.title(f"{cand.label}: gas partition vs T at {bu:.1f}% FIMA")
    plt.ylim(0, 100)
    plt.grid(True, alpha=0.25)
    plt.legend(fontsize=7, loc="center right", ncol=2)
    return savefig(f"{cand.label}_gas_partition_{bu:.1f}FIMA.png")



def plot_diffusivities(rows, cand):
    """Compare the active Matthews/Schneider curves with the preserved Rizk baselines."""
    case_id = DIFFUSIVITY_PLOT_CASE_ID
    sub = rows_for_case(rows, case_id)
    if not sub:
        raise ValueError(f"No rows available for diffusivity plot case {case_id!r}.")

    Ts = np.array([float(r["T"]) for r in sub], dtype=float)
    case = experiment_case(case_id)
    Fdot = float(case["fission_rate"])
    kB = 8.617333262145e-5

    # Active/new curves from the actual run.
    Dxe_active = np.array([max(float(r.get("Dg", math.nan)), 1e-300) for r in sub])
    Dv_active = np.array([max(float(r.get("Dv", math.nan)), 1e-300) for r in sub])

    # Matthews user-digitized lookup references.
    Dxe_matthews = np.array([matthews2025_DXe_lookup(T) for T in Ts], dtype=float)
    Du_matthews = np.array([matthews2025_DU_lookup(T) for T in Ts], dtype=float)
    Dv_matthews_thermal = np.array(
        [3.25e-4 * math.exp(-5.95 / (kB * T)) for T in Ts], dtype=float
    )

    # Previous 16test Rizk Xe mode: rizk2025_refit_plot with D2_xe_scale=0.
    Dxe_rizk = []
    for T in Ts:
        kBT = kB * T
        D1 = 2.967341817979e-03 * math.exp(-5.013616576464e+00 / kBT)
        D2 = math.sqrt(Fdot) * 4.498475254045e-68 * _safe_exp(
            -(-1.790671812685e+01) / kBT - 9.255235831189e-01 / (kBT**2)
        )
        D3 = 1.189275430019e-38 * Fdot
        Dxe_rizk.append(
            MANUAL_PARAMS["Dg_scale"]
            * (
                MANUAL_PARAMS["Dg_D1_scale"] * D1
                + MANUAL_PARAMS["D2_xe_scale"] * D2
                + MANUAL_PARAMS["Dg_D3_scale"] * D3
            )
        )
    Dxe_rizk = np.array(Dxe_rizk, dtype=float)

    # Previous 16test Rizk vacancy mode: rizk2025_refit_full.
    Dv_rizk = []
    Dv1_rizk = []
    for T in Ts:
        kBT = kB * T
        D1 = 1.122978768506e-02 * math.exp(-5.596873538604e+00 / kBT)
        D2 = math.sqrt(Fdot) * 7.805188680989e-28 * _safe_exp(
            -9.932675113163e-01 / kBT - 2.082395503235e-02 / (kBT**2)
        )
        Dv1_rizk.append(D1)
        Dv_rizk.append(
            MANUAL_PARAMS["Dv_scale"]
            * (
                MANUAL_PARAMS["Dv_D1_scale"] * D1
                + MANUAL_PARAMS["Dv_D2_scale"] * D2
            )
        )
    Dv_rizk = np.array(Dv_rizk, dtype=float)
    Dv1_rizk = np.array(Dv1_rizk, dtype=float)

    # GB references: show BOTH closures independently of the active selection.
    Dv_gb_rizk = 1.0e6 * (
        1.35e-2 * np.exp(-5.66 / (kB * Ts))
    )
    Dv_gb_matthews = 325.0 * np.exp(-5.95 / (kB * Ts))

    plt.figure(figsize=(11.0, 6.8))

    # Xe: new vs old.
    plt.semilogy(
        Ts, np.maximum(Dxe_active, 1e-300),
        linewidth=2.4,
        label=r"ACTIVE $D_{Xe}$: Matthews/Schneider lookup",
    )
    plt.semilogy(
        Ts, np.maximum(Dxe_rizk, 1e-300),
        "--", linewidth=1.8,
        label=r"Rizk 2025 $D_{Xe}$ (previous 16test)",
    )

    # Matrix/vacancy-mediated transport: new vs old.
    plt.semilogy(
        Ts, np.maximum(Dv_active, 1e-300),
        linewidth=2.4,
        label=r"ACTIVE $D_v^{eff}$: Matthews $D_U^{tot}$ lookup",
    )
    plt.semilogy(
        Ts, np.maximum(Dv_rizk, 1e-300),
        "--", linewidth=1.8,
        label=r"Rizk 2025 $D_v$ total (previous 16test)",
    )

    # Thermal and GB context.
    plt.semilogy(
        Ts, np.maximum(Dv_matthews_thermal, 1e-300),
        ":", linewidth=1.6,
        label=r"Matthews thermal vacancy-mediated $D_U$",
    )
    plt.semilogy(
        Ts, np.maximum(Dv1_rizk, 1e-300),
        ":", linewidth=1.6,
        label=r"Rizk 2025 thermal $D_{v,1}$",
    )
    plt.semilogy(
        Ts, np.maximum(Dv_gb_matthews, 1e-300),
        "-.", linewidth=1.5,
        label=r"Matthews $D_v^{GB}$",
    )
    plt.semilogy(
        Ts, np.maximum(Dv_gb_rizk, 1e-300),
        "-.", linewidth=1.5,
        label=r"Rizk legacy $D_v^{GB}=10^6D_{v,1}$",
    )

    # Experimental Xe overlay.
    exp_df = load_schneider_fig7_exp_points()
    if exp_df is not None and not exp_df.empty:
        for exp_name, marker in EXP_XE_MARKERS.items():
            sdf = exp_df[exp_df["experiment"] == exp_name].copy()
            sdf = sdf[(sdf["T_K"] >= min(Ts)) & (sdf["T_K"] <= max(Ts))]
            if sdf.empty:
                continue
            plt.semilogy(
                sdf["T_K"].to_numpy(dtype=float),
                np.maximum(sdf["D_m2_s"].to_numpy(dtype=float), 1e-300),
                linestyle="None",
                marker=marker,
                markersize=7,
                markeredgewidth=1.6,
                zorder=10,
                label=EXP_XE_LABELS.get(exp_name, exp_name),
            )

    plt.xlabel("Temperature [K]")
    plt.ylabel("Diffusivity [m$^2$/s]")
    plt.title(
        f"{cand.label}: updated vs Rizk diffusivities — {case_id}, "
        f"Fdot={Fdot:.2e} m$^{{-3}}$ s$^{{-1}}$\n"
        r"Matthews Fig. 4.2 lookup itself digitized at $\dot F=10^{19}$ m$^{-3}$ s$^{-1}$"
    )
    plt.grid(True, alpha=0.3, which="both")
    plt.legend(fontsize=7.5, ncol=2)
    return savefig(f"{cand.label}_diffusivities_updated_vs_Rizk_{case_id.replace('.', 'p')}.png")

def plot_capture_diagnostic(rows, cand, bu):
    sub = rows_for_burnup(rows, bu)
    Ts = [r["T"] for r in sub]
    plt.figure(figsize=(9, 5.8))
    plt.semilogy(Ts, [max(r["max_f_cap_step"], 1e-300) for r in sub], label="max f_cap step, clipped fraction")
    plt.semilogy(Ts, [max(r["capture_fraction_sum"], 1e-300) for r in sub], label="sum clipped f_cap steps, diagnostic")
    plt.semilogy(Ts, [max(r["capture_raw_sum"], 1e-300) for r in sub], label="sum raw capture hazard, diagnostic")
    plt.xlabel("Temperature [K]")
    plt.ylabel("Capture diagnostic")
    plt.title(f"{cand.label}: capture diagnostics at {bu:.1f}% FIMA")
    plt.grid(True, alpha=0.3, which="both")
    plt.legend(fontsize=8)
    return savefig(f"{cand.label}_capture_diagnostic_{bu:.1f}FIMA.png")

def plot_rho_surface(cand):
    # Simple visualization of active rho mode.
    from mpl_toolkits.mplot3d import Axes3D  # noqa: F401
    T_grid = np.arange(900, 2101, 50)
    F_grid = np.linspace(0, 100, 40)
    TT, FF = np.meshgrid(T_grid, F_grid)
    ZZ = np.vectorize(lambda T, F: math.log10(effective_rho_d(T, F/20.0 if False else F, cand)))(TT, FF)
    fig = plt.figure(figsize=(9, 6.2))
    ax = fig.add_subplot(111, projection="3d")
    surf = ax.plot_surface(TT, FF, ZZ, alpha=0.65)
    ax.set_xlabel("Temperature [K]")
    ax.set_ylabel("Burnup [MWd/kgHM proxy]")
    ax.set_zlabel(r"$\log_{10}\rho_d$ [m$^{-2}$]")
    ax.set_title(f"rho_d(F,T) -- {RHO_MODE}, rho_d={cand.rho_d:.2e} m^-2")
    return savefig(f"{cand.label}_rho_d_surface.png")

def make_all_plots(rows, cand):
    saved = []
    for bu in BURNUPS:
        saved.append(plot_swelling_T(rows, bu, cand))
    saved.append(plot_swelling_burnup_1600(cand))
    # R and N diagnostics now include both bulk and dislocation bubbles.
    for bu in BURNUPS:
        saved.append(plot_radius_concentration_T(rows, cand, bu))
    for bu in BURNUPS:
        saved.append(plot_pressure_T(rows, cand, bu))
        saved.append(plot_pressure_ratio_T(rows, cand, bu))
        saved.append(plot_gas_partition(rows, cand, bu))
        saved.append(plot_capture_diagnostic(rows, cand, bu))
    saved.append(plot_diffusivities(rows, cand))
    saved.append(plot_rho_surface(cand))
    return saved

def print_summary(rows, cand):
    print("\n" + "=" * 120)
    print(f"Manual case: {cand.label}")
    print("=" * 120)
    print("Physics switches:")
    print(f"  USE_PHI_GAS_RESOLUTION       = {USE_PHI_GAS_RESOLUTION}")
    print(f"  USE_NUCLEATION_MASS_COUPLING = {USE_NUCLEATION_MASS_COUPLING}")
    print(f"  USE_BULK_DISLOCATION_CAPTURE = {USE_BULK_DISLOCATION_CAPTURE}")
    print("Key parameters:")
    print(f"  K_d                          = {cand.K_d:.6g}")
    print(f"  rho_d                        = {cand.rho_d:.6g}")
    print(f"  N_d0 = K_d*rho_d             = {cand.K_d*cand.rho_d:.6e}")
    print(f"  Xe diffusivity mode          = {XE_DIFFUSIVITY_MODE}")
    print(f"  Matrix/vacancy mode          = {VU_DIFFUSIVITY_MODE}")
    print(f"  GB vacancy mode              = {DV_GB_MODE}")
    if XE_DIFFUSIVITY_MODE == "matthews2025_fig42_lookup" or VU_DIFFUSIVITY_MODE == "matthews2025_DU_fig42_lookup":
        print(f"  Matthews lookup source Fdot  = {MATTHEWS2025_LOOKUP_FISSION_RATE:.3e} m^-3 s^-1")
        print(f"  Matthews lookup chemistry y  = {MATTHEWS2025_LOOKUP_Y:.1e}")
    print(f"  USE_FIRST_GAS_PRESSURE_SEED  = {USE_FIRST_GAS_PRESSURE_SEED}")
    print(f"  FIRST_GAS_PRESSURE_FACTOR    = {FIRST_GAS_PRESSURE_FACTOR:.6g}")
    print(f"  SEED_BULK_ON_FIRST_GAS       = {SEED_BULK_ON_FIRST_GAS}")
    print(f"  SEED_DISLOCATION_ON_FIRST_GAS= {SEED_DISLOCATION_ON_FIRST_GAS}")
    print(f"  legacy dimer seed at birth   = {USE_EQUILIBRIUM_DIMER_SEED}")
    print("\nSelected diagnostics:")
    header = f"{'bu [%]':>7s} {'T [K]':>7s} {'sw_d [%]':>10s} {'R_d [nm]':>10s} {'N_d [m^-3]':>13s} {'R_b [nm]':>10s} {'N_b [m^-3]':>13s} {'p_d/peq':>9s} {'qgb [%]':>9s}"
    print(header)
    print("-" * len(header))
    for bu in BURNUPS:
        for T in [1200.0, 1600.0, 1800.0, 2000.0]:
            if T < T_MIN or T > T_MAX_DIAGNOSTIC:
                continue
            r = row_near(rows, bu, T)
            rb = r['Rb_nm'] if r.get('Nb', 0.0) > 0.0 and r.get('Rb_nm', 0.0) > 0.0 else math.nan
            nb_ = r['Nb'] if r.get('Nb', 0.0) > 0.0 else math.nan
            print(f"{bu:7.2f} {r['T']:7.0f} {r['swelling_d_percent']:10.3g} {r['Rd_nm']:10.3g} {r['Nd']:13.3e} {rb:10.3g} {nb_:13.3e} {r['p_d_over_eq']:9.3g} {r['qgb_gas_percent']:9.3g}")
    print("=" * 120)

def main():
    ensure_output_dir()
    cand = make_candidate()
    rows = simulate_grid(cand)
    csv_path, df = write_csv(rows, cand)
    print_summary(rows, cand)
    saved = make_all_plots(rows, cand)
    print(f"\nCSV written to: {csv_path}")
    print("Plots written:")
    for p in saved:
        print(f"  - {p}")
    return rows, df, saved

def inspect_point(T=1600.0, burnup=1.3, keep_history=True):
    cand = make_candidate()
    out = run_model_point(T, burnup, cand, DT_H, N_MODES, keep_history=keep_history)
    keys = ["T", "burnup", "swelling_d_percent", "swelling_b_percent", "Rd_nm", "Nd", "p_d", "p_d_eq", "p_d_over_eq", "qgb_gas_percent", "phi_b", "phi_d", "max_f_cap_step"]
    print({k: out.get(k) for k in keys})
    print("\nrates:")
    for k in ["xe_diffusivity_mode", "vacancy_diffusivity_mode", "Dg", "Dg_dislocation", "Dv", "Dv_dislocation", "Dv1", "Dv2", "Dv_lookup_base", "Dv_nonthermal_effective", "Dg_lookup_base", "A20_vU_active", "g_b", "g_d", "b_b", "b_d", "b_b_gas", "b_d_gas", "term_bubbles", "term_dislocation"]:
        print(f"  {k:20s} = {out.get(k)}")
    return out


# ============================================================
# 16testUN — CASE-SPECIFIC RONCHI/DN1 BENCHMARK EXECUTION
# ============================================================
# The solver above remains unchanged. This block changes only how benchmark
# cases are instantiated and plotted: each experimental pin gets its own Fdot.


def experiment_case(case_id: str) -> dict:
    if case_id not in EXPERIMENT_CASES:
        raise KeyError(f"Unknown experiment case {case_id!r}. Valid: {list(EXPERIMENT_CASES)}")
    return EXPERIMENT_CASES[case_id]


def candidate_for_experiment(cand: Candidate, case_id: str) -> Candidate:
    """Return the same physical/calibration candidate with experiment-specific Fdot."""
    case = experiment_case(case_id)
    return replace(
        cand,
        label=f"{cand.label}_{case_id.replace('.', 'p')}",
        fission_rate=float(case["fission_rate"]),
    )


def run_model_point_case(T: float, case_id: str, cand: Candidate, dt_h=DT_H, n_modes=N_MODES, keep_history=False):
    """Run one material-point benchmark using the experimental pin's burnup and Fdot."""
    case = experiment_case(case_id)
    cc = candidate_for_experiment(cand, case_id)
    out = run_model_point(
        T=float(T),
        burnup=float(case["burnup"]),
        cand=cc,
        dt_h=dt_h,
        n_modes=n_modes,
        keep_history=keep_history,
    )
    # Explicit provenance in CSV/output rows.
    out["experiment_case"] = case_id
    out["linear_power_kW_m"] = float(case["linear_power_kW_m"])
    out["fuel_diameter_mm"] = float(case["fuel_diameter_mm"])
    out["fission_rate_case"] = float(case["fission_rate"])
    out["fuel_description"] = case.get("fuel", "")
    return out


def simulate_grid(cand: Candidate):
    """Run the four experimental benchmark cases separately.

    Note: this is FOUR case families, not three, because AP3.2 and AP3.8 are
    distinct 1.1% FIMA pins with different linear powers / fission rates.
    """
    RUN_CACHE.clear()
    rows = []
    Ts = temperature_grid(T_MAX_DIAGNOSTIC)
    for case_id in EXPERIMENT_CASE_ORDER:
        case = experiment_case(case_id)
        print(
            f"Running {case_id}: BU={case['burnup']:g}% FIMA, "
            f"q'={case['linear_power_kW_m']:g} kW/m, "
            f"d={case['fuel_diameter_mm']:g} mm, "
            f"Fdot={case['fission_rate']:.4e} m^-3 s^-1, "
            f"{len(Ts)} temperature points"
        )
        for T in Ts:
            rows.append(run_model_point_case(T, case_id, cand, DT_H, N_MODES, keep_history=False))
    return rows


def rows_for_case(rows, case_id: str):
    return sorted([r for r in rows if r.get("experiment_case") == case_id], key=lambda r: r["T"])


def row_near_case(rows, case_id: str, T: float):
    sub = rows_for_case(rows, case_id)
    if not sub:
        raise ValueError(f"No rows found for {case_id}")
    return min(sub, key=lambda r: abs(r["T"] - float(T)))


def experimental_swelling_points_for_case(case_id: str):
    case = experiment_case(case_id)
    bu = float(case["burnup"])
    series = case["exp_series"]
    return [
        p for p in EXP_SWELLING_T
        if abs(float(p["burnup"]) - bu) < 1e-9 and p["series"] == series
    ]


def case_info_text(case_id: str) -> str:
    case = experiment_case(case_id)
    return (
        f"Experiment: {case_id}\n"
        f"Burnup: {case['burnup']:.1f}% FIMA\n"
        f"Linear heat rate: {case['linear_power_kW_m']:.0f} kW/m\n"
        f"Fission rate: {case['fission_rate']:.2e} m$^{{-3}}$ s$^{{-1}}$\n"
        f"Fuel diameter: {case['fuel_diameter_mm']:.1f} mm"
    )


def add_case_info_box(ax, case_id: str, loc=(0.98, 0.04)):
    ax.text(
        loc[0], loc[1], case_info_text(case_id),
        transform=ax.transAxes,
        ha="right", va="bottom",
        fontsize=8.5,
        bbox=dict(boxstyle="round,pad=0.45", facecolor="white", edgecolor="0.35", alpha=0.92),
        zorder=20,
    )


def plot_swelling_case(rows, case_id: str, cand: Candidate):
    """Case-specific swelling plot with matched experimental points and provenance box."""
    case = experiment_case(case_id)
    sub = [r for r in rows_for_case(rows, case_id) if r["T"] <= T_MAX_SWELLING]
    Ts = [r["T"] for r in sub]
    cc = candidate_for_experiment(cand, case_id)

    fig, ax = plt.subplots(figsize=(9.4, 6.0))
    ax.plot(Ts, [r["swelling_d_percent"] for r in sub], label="Model dislocation/P2 swelling")
    ax.plot(Ts, [r["swelling_b_percent"] for r in sub], "--", label="Model bulk swelling")
    ax.plot(Ts, [r["swelling_gf_percent"] for r in sub], "-.", label="Model intergranular/grain-face swelling")

    pts = experimental_swelling_points_for_case(case_id)
    marker = "x" if case_id == "AP3.8" else "^"
    if pts:
        ax.scatter(
            [p["T"] for p in pts], [p["swelling"] for p in pts],
            marker=marker, s=68,
            label=f"Exp P2 {case_id} ({case['linear_power_kW_m']:.0f} kW/m)",
            zorder=5,
        )

    ax.set_xlabel("Temperature [K]")
    ax.set_ylabel("Fission gas swelling [%]")
    ax.set_title(f"{cand.label}: swelling vs T — {case_id}, {case['burnup']:.1f}% FIMA")
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=8, loc="upper left")
    add_case_info_box(ax, case_id)
    fig.tight_layout()
    return savefig(f"{cc.label}_swelling_T_{case['burnup']:.1f}FIMA.png")


def plot_swelling_case_comparison_1600(rows, cand: Candidate):
    """Replace the old arbitrary continuous-burnup sweep with actual experimental cases at 1600 K."""
    fig, ax = plt.subplots(figsize=(9.4, 5.9))
    # Small horizontal offsets only to separate AP3.2/AP3.8 at the same burnup.
    offsets = {"AP3.2": -0.035, "AP3.8": 0.035, "AP3.4": 0.0, "ANP6": 0.0}
    for case_id in EXPERIMENT_CASE_ORDER:
        case = experiment_case(case_id)
        r = row_near_case(rows, case_id, 1600.0)
        x = float(case["burnup"]) + offsets.get(case_id, 0.0)
        ax.scatter([x], [r["swelling_d_percent"]], s=75, label=f"Model {case_id}")
        ax.annotate(case_id, (x, r["swelling_d_percent"]), xytext=(4, 5), textcoords="offset points", fontsize=8)

    for p in EXP_SWELLING_BURNUP_1600:
        # Match the two 1.1% experimental points to their respective pins.
        if "100" in p["series"]:
            x = 1.1 + offsets["AP3.2"]
            lab = "Exp AP3.2"
        elif "119" in p["series"]:
            x = 1.1 + offsets["AP3.8"]
            lab = "Exp AP3.8"
        elif abs(float(p["burnup"]) - 1.31) < 0.05:
            x = 1.3
            lab = "Exp AP3.4"
        else:
            x = 3.2
            lab = "Exp ANP6"
        ax.scatter([x], [p["swelling"]], marker="x", s=75, label=lab)

    ax.set_xlabel("Burnup [% FIMA]")
    ax.set_ylabel("P2/dislocation swelling [%]")
    ax.set_title(f"{cand.label}: case-specific swelling comparison near 1600 K")
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=7, ncol=2)
    fig.tight_layout()
    return savefig(f"{cand.label}_swelling_case_specific_1600K.png")


def make_all_plots(rows, cand):
    """Case-aware plot suite. All physics outputs are evaluated with each pin's Fdot."""
    saved = []

    # Main validation plots: one swelling figure per experimental pin.
    for case_id in EXPERIMENT_CASE_ORDER:
        saved.append(plot_swelling_case(rows, case_id, cand))
    saved.append(plot_swelling_case_comparison_1600(rows, cand))

    # R/N experimental diagnostics exist for the 1.3% FIMA/AP3.4 benchmark.
    ap34_rows = rows_for_case(rows, "AP3.4")
    ap34_cand = candidate_for_experiment(cand, "AP3.4")
    saved.append(plot_radius_concentration_T(ap34_rows, ap34_cand, 1.3))

    # Remaining diagnostics are also run case-by-case so no 1.1% cases are mixed.
    for case_id in EXPERIMENT_CASE_ORDER:
        case = experiment_case(case_id)
        crows = rows_for_case(rows, case_id)
        cc = candidate_for_experiment(cand, case_id)
        bu = float(case["burnup"])
        saved.append(plot_pressure_T(crows, cc, bu))
        saved.append(plot_pressure_ratio_T(crows, cc, bu))
        saved.append(plot_gas_partition(crows, cc, bu))
        saved.append(plot_capture_diagnostic(crows, cc, bu))

    # Diffusivities are Fdot-dependent; show them for every benchmark case.
    for case_id in EXPERIMENT_CASE_ORDER:
        crows = rows_for_case(rows, case_id)
        cc = candidate_for_experiment(cand, case_id)
        # Use a local case-aware version of the diffusivity plot.
        sub = crows
        Ts = [r["T"] for r in sub]
        plt.figure(figsize=(10.2, 6.2))
        plt.semilogy(Ts, [max(r.get("Dg", math.nan), 1e-300) for r in sub], linewidth=2.0, label=r"Model $D_g^{Xe}$")
        plt.semilogy(Ts, [max(r.get("Dv", math.nan), 1e-300) for r in sub], label="Dv vacancies, bulk total")
        plt.semilogy(Ts, [max(r.get("Dv1", math.nan), 1e-300) for r in sub], "--", label="Dv1 vacancies, bulk thermal")
        plt.semilogy(Ts, [max(r.get("Dv2", math.nan), 1e-300) for r in sub], ":", label="Dv2 vacancies, bulk irradiation enhanced")
        plt.semilogy(Ts, [max(r.get("Dv_gb", math.nan), 1e-300) for r in sub], "-.", label="Dv vacancies, grain boundaries")
        plt.xlabel("Temperature [K]")
        plt.ylabel("Diffusivity [m$^2$/s]")
        plt.title(f"{cc.label}: diffusivities vs T")
        plt.grid(True, alpha=0.3, which="both")
        plt.legend(fontsize=8)
        saved.append(savefig(f"{cc.label}_diffusivities_T.png"))

    saved.append(plot_rho_surface(cand))
    return saved


def print_summary(rows, cand):
    print("\n" + "=" * 145)
    print(f"Manual physics candidate: {cand.label}")
    print("Case-specific experimental inputs:")
    for case_id in EXPERIMENT_CASE_ORDER:
        c = experiment_case(case_id)
        print(
            f"  {case_id:5s}: BU={c['burnup']:.1f}% FIMA, q'={c['linear_power_kW_m']:.0f} kW/m, "
            f"d={c['fuel_diameter_mm']:.1f} mm, Fdot={c['fission_rate']:.4e} m^-3 s^-1"
        )
    print("=" * 145)
    print("Physics switches:")
    print(f"  USE_PHI_GAS_RESOLUTION       = {USE_PHI_GAS_RESOLUTION}")
    print(f"  USE_NUCLEATION_MASS_COUPLING = {USE_NUCLEATION_MASS_COUPLING}")
    print(f"  USE_BULK_DISLOCATION_CAPTURE = {USE_BULK_DISLOCATION_CAPTURE}")
    print("Key calibration parameters:")
    print(f"  f_n                          = {cand.f_n:.6g}")
    print(f"  K_d                          = {cand.K_d:.6g}")
    print(f"  rho_d                        = {cand.rho_d:.6g}")
    print(f"  Dv_dislocation_scale         = {cand.Dv_dislocation_scale:.6g}")
    print(f"  Dg_dislocation_scale         = {cand.Dg_dislocation_scale:.6g}")
    print("\nSelected diagnostics:")
    header = f"{'case':>6s} {'bu [%]':>7s} {'T [K]':>7s} {'Fdot':>11s} {'sw_d [%]':>10s} {'R_d [nm]':>10s} {'N_d [m^-3]':>13s} {'p_d/peq':>9s} {'qgb [%]':>9s} {'FGR [%]':>9s}"
    print(header)
    print("-" * len(header))
    for case_id in EXPERIMENT_CASE_ORDER:
        c = experiment_case(case_id)
        for T in [1200.0, 1600.0, 1800.0]:
            r = row_near_case(rows, case_id, T)
            print(
                f"{case_id:>6s} {c['burnup']:7.2f} {r['T']:7.0f} {c['fission_rate']:11.3e} "
                f"{r['swelling_d_percent']:10.3g} {r['Rd_nm']:10.3g} {r['Nd']:13.3e} "
                f"{r['p_d_over_eq']:9.3g} {r['qgb_gas_percent']:9.3g} {r['release_gas_percent']:9.3g}"
            )
    print("=" * 145)


def main():
    ensure_output_dir()
    cand = make_candidate()
    rows = simulate_grid(cand)
    csv_path, df = write_csv(rows, cand)
    print_summary(rows, cand)
    saved = make_all_plots(rows, cand)
    print(f"\nCSV written to: {csv_path}")
    print("Plots written:")
    for p in saved:
        print(f"  - {p}")
    return rows, df, saved


def inspect_case_point(case_id="AP3.4", T=1600.0, keep_history=True):
    cand = make_candidate()
    out = run_model_point_case(T, case_id, cand, DT_H, N_MODES, keep_history=keep_history)
    keys = [
        "experiment_case", "T", "burnup", "fission_rate_case", "swelling_d_percent",
        "swelling_b_percent", "Rd_nm", "Nd", "p_d", "p_d_eq", "p_d_over_eq",
        "qgb_gas_percent", "grainface_gas_percent", "release_gas_percent",
    ]
    print({k: out.get(k) for k in keys})
    return out
