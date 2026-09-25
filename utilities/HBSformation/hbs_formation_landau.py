"""High-burnup structure formation as a second-order phase transition.

This is the reference implementation of the HBS-formation model that
`src/models/HighBurnupStructureFormation.C` implements as
`iHighBurnupStructureFormation = 4`. The two can be compared directly
(`compare_with_sciantix.py`), and agree on every timestep of
`regression/hbs/test_UO2HBS_landau`.

@author  E. Cappellari
@date    2026-09-06
---------------------------------------------------------------------------------
The HBS is interpreted as a second-order phase transition.  The order parameter
is the mean misorientation of the subgrains normalized to its maximum,
eta = theta/theta_max, with theta_max = theta_HAGB so that eta runs over [0, 1];
the external condition is the local burnup.  The energy F(eta) is built by
splitting the dislocations into three populations that must add up to rho_tot --
free, stored in low-angle walls, annihilated by the sweeping boundaries -- and
giving each the energy per unit length E_D = G b^2 f(nu)/(4 pi) ln(R/b) of the
state it is in, with R the spacing of the dislocations in that state.  F depends
on eta through eta^2 and |eta| only, because the energy is invariant under the
sign of theta.  It is a constrained minimisation of the stored energy (no
entropy term), with the Landau order parameter as its variable.  
Minimizing F over the range of eta for which that partition is physical gives the 
equilibrium misorientation; the subgrain size follows from the same wall geometry; 
and the restructured fraction follows from the lever rule, because the measured 
misorientation is the mean of a two-phase mixture.

Three quantities are produced:

    output 1    Theta       mean misorientation             [deg]   Eq. (8)
    output 2    r_n         subgrain radius                 [m]     Eq. (9)
    output 3    X           restructured volume fraction    [-]     Eq. (10)

The dislocation density is fixed to Nogita & Une (1994), read as a pure source above a
critical density rho_crit.

Equations
---------------------------------------------------------------------------------

  (1)  dislocations available to polygonize -- Nogita & Une (1994), a pure source
       rho_tot(bu) = max( 10^(2.2e-2*bu + 13.8) - rho_crit , 0 )        [m^-2]
       only the density above the critical one takes part; below it
       rho_tot = 0 and Theta = 0.  rho_crit is the fixed density scale that the
       energy balance lacks, and it gives a continuous threshold at
       rho_Nogita(bu_c) = rho_crit

  (2)  elastic constants -- NEA Recommendations on fuel properties (2025),
       Nuclear Science NEA/NSC/R(2024)1, p. 124.  Both depend on temperature,
       q = plutonium fraction (0 for UO2), P = porosity, x = deviation from
       stoichiometry.

       G(T,P,x,q) = 1e9 * [82.52*(1-q) + 94.91*q]
                        * (1 - P)^2 / (1 + 0.95275*P)
                        * (1 - 2.88078*x + 15.49419*x^2)
                        * (1.009549 - 1.182e-5*T - 6.671e-8*T^2)        [Pa]

       nu(T,P,x,q) = [0.32051*(1-q) + 0.31882*q]
                   * (1 - 1.03223*P)
                   * (1 + 0.69962*x - 7.52905*x^2)
                   * (1.017906 - 6.420e-5*T + 1.506e-8*T^2)             [-]

       f(nu) = (1 - nu/2)/(1 - nu), the edge/screw average of the dislocation
       line energy prefactor (Hansen 1986), enters A1 and A2 of Eq. (4).

  (3)  wall geometry -- dislocations at spacing d give theta = b/d, so a wall
       carrying n families has line length L = n*theta/b, and the low-angle
       boundary area per unit volume is (S/V) = 3*sqrt(rho_LAGB)/beta
       rho_LAGB_max = (3*n*theta_max / (beta*b))^2                      [m^-2]
       SoverV_max   = 9*n*theta_max / (beta^2*b)                        [m^-1]
       x_max        = k*rho_LAGB_max / rho_tot                          [-]

  (4)  dislocation line energies      E_D = G b^2 f(nu)/(4 pi) * ln(R/b)
       with R the spacing of the dislocations (Humphreys et al. 2017, Eq. 2.6):
       A1      = f(nu)/(4 pi)*ln( rho_tot^(-1/2) / b )               random array
       A2(eta) = f(nu)/(4 pi)*ln( min(b/theta, rho_tot^(-1/2)) / b )  Read-Shockley wall
       theta = eta*theta_max; b/theta is the spacing in the wall (Eq. 4.4).

  (5)  dislocation balance
       rho_ord(eta)   = rho_LAGB_max * eta^2                condensed into LAGB walls
       x(eta)         = x_max * eta^2 = k*rho_ord/rho_tot   extended swept volume
       rho_free(eta)  = (rho_tot - rho_ord) * exp(-x)       still random
       rho_swept(eta) = (rho_tot - rho_ord) * (1 - exp(-x)) annihilated

       exp(-x) is Gourdet & Montheillet's d(rho_i) = -rho_i dV (their Eq. 4)
       integrated over the swept volume: linear in x for a small sweep, bounded
       by the free dislocations there are for a large one.

       Each population carries the line energy of the state it is in:

           F = rho_free*A1*G b^2 + rho_ord*A2*G b^2                   [J/m^3]

  (6)  F = C0 + E_wall + E_sweep                                      [J/m^3]
       C0      = rho_tot * A1 * G b^2          all dislocations random
       E_wall  = rho_ord * (A2 - A1) * G b^2   condensing into walls, <= 0
       E_sweep = -rho_swept * A1 * G b^2       annihilation, <= 0

  (7)  equilibrium: the minimum of F on [0, min(eta_balance, 1)], found
       numerically (A2 carries theta*ln(theta), the sweep an exponential).
       Both terms are negative from eta = 0 on, so Theta > 0 wherever
       rho_tot > 0.  F has no threshold of its own: every length in it (rho^-1/2,
       b/theta with theta ~ sqrt(rho), beta/sqrt(rho)) scales with the density, so
       the balance looks the same at any rho.  The threshold is set by rho_crit in
       Eq. (1), and Theta leaves zero continuously, as sqrt(bu - bu_c) (the
       mean-field exponent 1/2), with no jump.

  (7a)      eta = 1   <=>   Theta = theta_HAGB

       with theta_max = theta_HAGB the order parameter saturates exactly where the
       substructure becomes high-angle, and Eq. (8) reduces to a clip of eta at 1.  rho_ord = rho_tot
       at the same burnup only if the bound (7b) binds up to saturation.

  (7b) dislocation balance   rho_ord <= rho_tot. 
       The equilibrium is the minimum of F on the admissible interval:

           eta_balance = sqrt( min(rho_tot / rho_LAGB_max, 1) )         [-]
           eta_eq      = argmin F  on  [0, min(eta_balance, 1)]

       On the bound this is exactly the classical theta ~ sqrt(rho_tot),
           theta_bal = eta_balance*theta_max = beta*b*sqrt(rho_tot) / (3n)
       every dislocation is in a wall, the misorientation can only grow as fast
       as the dislocations that feed it, and the sweep stops because there is
       nothing free left to sweep. 
       
  (8)  mean misorientation                                 <-- output 1
       Theta = min( eta_eq*theta_max*180/pi , theta_HAGB )              [deg]
       eta   = (Theta*pi/180)/theta_max

  (9)  subgrain radius                                     <-- output 2
       SoverV  = SoverV_max * eta * exp(-x)     the walls in the swept volume go
                                                too (Gourdet & Montheillet Eq. 8)
       r_n     = min( 1.5/SoverV , R_grain )                            [m]
       Undefined where eta = 0: below the threshold there is no substructure,
       so the radius is not a length.

  (10) restructured fraction                               <-- output 3
       X = clip( (Theta - theta_u) / (theta_HAGB - theta_u), 0, ALPHA_MAX )
       The measured Theta is not a local misorientation: it is the mean over the
       EBSD map, built as Theta = [AMis*(f1 - f10) + 10*f10]/100, i.e. exactly
       the weighted mean of a two-phase mixture.  The functional,
       calibrated on Theta, therefore predicts the mixture MEAN, and the fraction
       is recovered by inverting the mixture.  theta_u is the misorientation of
       the unrestructured matrix, and it is fixed by the measurement convention:
       f1 is the "restructured fraction at 1 deg", so 1 deg is the lower bin edge
       that decides which boundaries enter the mixture at all.  A map whose whole
       resolved population sits at that edge carries no restructuring.

  (11) driving force, reported for the nucleation criterion, not an output
       dE_s = C0 + E_wall + E_sweep = F(eta_eq)                         [J/m^3]


No surface energy
---------------------------------------------------------------------------------
[P] The stored energy is the dislocation energy alone, as in Gourdet &
    Montheillet (2003) and the CDRX models built on it.  Read-Shockley
    gamma(theta) appears there as the energy that sets the driving pressure and
    the mobility of a MIGRATING boundary, never as a term added to the stored
    energy.
[P] Adding gamma(theta)*(S/V) would count the walls twice: Humphreys, Rohrer &
    Rollett (2017) Eq. (2.13) shows that the Read-Shockley boundary energy IS
    the summed strain energy of the dislocations in the wall, which is the same
    object E_wall already carries through A2.


ASSUMPTIONS, IN THE TAGS USED THROUGHOUT THIS FOLDER
---------------------------------------------------------------------------------
  [P]  taken from the literature as written
  [R]  reduction / modelling choice of this work
  [N]  numerics
  [?]  not measured or not calibrated; a placeholder
  [E]  known error, deviation or internal inconsistency

[P] The partition, Eq. (5).  HRR 2.2.3.1 splits the total density into the
    dislocations stored in cell/subgrain walls and those inside the cells.
    Eq. (5) is that split plus a third, annihilated population, and it closes:
    rho_ord + rho_swept + rho_free = rho_tot to 2e-16 (selftest).
[P] The line energy, Eq. (4), is HRR Eq. (2.6) term for term, including
    f(nu) = (1 - nu/2)/(1 - nu) for a mixed edge/screw population (Hansen 1986).
[P] The wall geometry, Eq. (3), is HRR Eq. (6.32): rho = (S/V)*L = 3 theta/(bD)
    with S/V ~ 3/D (Eq. 2.12) and theta = b/h (Eq. 4.4), plus a factor n for the
    families in a wall (Gourdet & Montheillet, range 1-3).
[P] Only the FREE dislocations are swept: Gourdet & Montheillet Eq. (4),
    d rho_i = -rho_i dV.  The walls inside the swept volume go too, their Eq. (8).

[R] D = beta/sqrt(rho_LAGB) is a Holt-type similarity relation written on the
    WALL density, not on the total.  It is what makes r_n a function of theta
    alone, and it is why beta = 21 here rather than the value Holt's relation
    takes on rho_tot.  beta is the analogue of the geometric parameter of Rest &
    Hofman (2000), who use 5.
[R] theta_max is a pure NORMALIZATION (Eq. 7a): rho_LAGB, S/V and x depend on
    theta = eta*theta_max alone, so it cancels out of Theta, r_n and X at fixed
    beta and k.  It is set to theta_HAGB so that eta = 1 and Theta = theta_HAGB
    coincide.
[R] F is a CONSTRAINED MINIMISATION of the stored energy, not a Landau free
    energy.

[?] Eq. (1) is used far outside the range it was fitted in.  Nogita & Une (1994)
    state that the density "increases exponentially with burnup IN THE RANGE OF
    6-44 GWd/t" and give log N = 2.2e-2 Bu + 13.8 for it.  This model evaluates
    that correlation from the threshold at 47 GWd/tU up to 150, i.e. entirely
    above the fitted range, and the paper's only datum beyond it disagrees with
    the extrapolation by a factor 7: at 83 GWd/t the measurement is 6.0e14 m^-2
    while Eq. (1) gives 4.2e15.  Nogita & Une attribute that to the Ham method
    saturating on extremely tangled dislocations and do not resolve it.  Every
    quantitative output of this model therefore rests on an extrapolated source.
[?] n, the number of dislocation families in a wall, is fixed at 2 and not
    fitted; Gourdet & Montheillet give the range 1-3.

[E] rho_crit = 6.85e14 m^-2 is LARGER than the only measured high-burnup density
    of the paper Eq. (1) comes from (6.0e14 m^-2 at 83 GWd/t): the threshold at
    47 GWd/tU is reached only because the correlation is trusted over the
    measurement.  The same paper reports sub-boundaries appearing between 30 and
    44 GWd/t, i.e. the observed onset of polygonization is BELOW the model's
    threshold, not above it.  rho_crit is calibrated on the EBSD misorientations,
    not on Nogita & Une; its agreement to within 15 % with the independent
    6e14 m^-2 of Veshchunov & Shestak (2009) is what supports the value.
[E] Gourdet & Montheillet Eq. (8) is applied to the radius but not to the energy.
    Eq. (9) removes the wall area inside the swept volume (S/V -> S/V exp(-x)),
    while F still counts ALL of rho_ord.  The same walls are present for the
    energy and absent for the geometry.  Making it consistent means rho_ord ->
    rho_ord exp(-x) in F, which moves every calibrated number.
[E] The sweep is slaved to the WALL content, not to a migrating boundary.  In
    Gourdet & Montheillet Eq. (5) the swept volume is proportional to the HAGB
    area fraction and the HAGB velocity, with "only the HABs are mobile"; here
    x = k rho_ord/rho_tot is already active at eta -> 0+, when not one high-angle
    boundary exists.  A sweep proportional to the HAGB fraction times a mobility
    M(T) would be closer to the mechanism.
[E] No temperature dependence.  f(nu)*G*b^2 multiplies every term of F, so it
    cannot move the minimum and Theta, r_n and X are functions of the local
    burnup alone.  The literature makes every step thermally activated: recovery
    (HRR 6.2, 84-90 kJ/mol), climb (6.3), boundary mobility (ch. 5).  The largest
    residuals on X are exactly the hot points (Noirot 1023-1081 K, Gerczak 845 K).
    Temperature is meant to enter through a future rho_tot(bu, T) replacing
    Eq. (1), and through rho_crit(T).
[E] The lever rule of Eq. (10) is partly circular with Theta.  The measured
    Theta = [AMis*(f1 - f10) + 10*f10]/100 is BUILT from f10, which is the
    measured X, and data set C puts X in the calibration objective.  R2(X) is
    therefore an in-sample score, not independent evidence; the leave-one-paper-
    out columns of `calibrate.py --study` are the out-of-sample check.
[E] The lever rule saturates with a corner: X reaches its cap exactly where
    Theta reaches theta_HAGB, so dX/dbu drops to zero abruptly.  Downstream that
    shows up as a transient dip in the HBS porosity, which is driven by
    dalpha_r/dt (porosity option 3).


Calibration
---------------------------------------------------------------------------------
`calibrate.py` fits beta, k and rho_crit jointly on the mean misorientation, the
measured subgrain sizes and the restructured fraction, and prints the result ready
to paste back into the constants below and into the `case 4` parameter push of
`src/models/HighBurnupStructureFormation.C`.

    python3 calibrate.py

VALIDATION
---------------------------------------------------------------------------------
Against the EBSD rows of `data/`, `--validate` gives

  mean misorientation  Theta   N = 41   RMSE = 1.6958 deg   R2 = 0.7883
  restructured fraction X      N = 27   RMSE = 0.1915       R2 = 0.7634
  subgrain radius       r_n    N = 14   RMSE = 0.1198 um    R2 = 0.2931

(Zacharie-Aubrun + Onofri standard UO2 rows; the calibration itself uses all four papers, data
set C of calibrate.py).  `comparison.py` scores the same model on ALL 127 targets of the four
papers, weighted by Rose rank x relevance: RMSE_w = 0.1880 with R2_w = +0.740 on the fraction,
1.812 deg / +0.756 on Theta and 0.1350 um / -0.212 on the radius.

References
---------------------------------------------------------------------------------
Nogita & Une, Nucl. Instrum. Methods B 91 (1994) 301-306.
NEA, "Recommendations on nuclear fuel properties", Nuclear Science
    NEA/NSC/R(2024)1 (2025), p. 124 -- shear modulus of UO2/MOX.
Hansen, Mater. Sci. Eng. 81 (1986) 141-161.
Gourdet & Montheillet, Acta Mater. 51 (2003) 2685-2699.
Rest & Hofman, J. Nucl. Mater. 277 (2000) 231-238.
Muramatsu, Takahashi et al. (2014) -- nucleation criterion and phase field.
Zacharie-Aubrun et al., J. Appl. Phys. 132 (2022) 195903.
Onofri et al., J. Nucl. Mater. 615 (2025) 155981.
"""

from __future__ import annotations

import argparse
import math
import sys
from dataclasses import dataclass

from hbs_dataset import dataset_dir, load_rows

# ---------------------------------------------------------------------------
# CONSTANTS
# ---------------------------------------------------------------------------

# --- elastic constants -----------------------------------------------------
BURGERS = 3.889087296526011e-10  # m      Burgers vector, Djonovic thesis

# --- shear modulus and Poisson ratio, Eq. (2): NEA/NSC/R(2024)1 p. 124 -----
# The shear modulus is quoted in GPa: SCIANTIX stores the modulus of the matrix in 
# MPa, while the Landau functional works in Pa.
G_UO2 = 82.52                    # GPa    UO2 end member
G_PUO2 = 94.91                   # GPa    PuO2 end member
G_POROSITY_COEFF = 0.95275       # -
G_STOICH_LINEAR = 2.88078        # -
G_STOICH_QUADRATIC = 15.49419    # -
G_TEMP_CONSTANT = 1.009549       # -
G_TEMP_LINEAR = 1.182e-5         # 1/K
G_TEMP_QUADRATIC = 6.671e-8      # 1/K2

NU_UO2 = 0.32051                 # -      UO2 end member
NU_PUO2 = 0.31882                # -      PuO2 end member
NU_POROSITY_COEFF = 1.03223      # -
NU_STOICH_LINEAR = 0.69962       # -
NU_STOICH_QUADRATIC = 7.52905    # -
NU_TEMP_CONSTANT = 1.017906      # -
NU_TEMP_LINEAR = 6.420e-5        # 1/K
NU_TEMP_QUADRATIC = 1.506e-8     # 1/K2

# Default state of the fuel when the caller does not say otherwise.
FABRICATION_POROSITY = 0.05      # -      as-fabricated porosity of UO2
PLUTONIUM_FRACTION = 0.0         # -      UO2; set to q for MOX

# --- LAGB / HAGB boundary --------------------------------------------------
# Both the upper end of the validity of the functional for the MATRIX and the
# composition of the restructured phase in the lever rule, Eq. (10).
THETA_HAGB = 10.0               # deg
THETA_MAX = math.radians(THETA_HAGB)    # rad     = 0.174533 (10 deg)

# Misorientation of the unrestructured matrix, i.e. the lower end member of the
# mixture Eq. (10) inverts.  It is the lower binning edge of the EBSD data, not a
# fit: the dataset reports a "restructured fraction at 1 deg" (f1) and one "at 10
# deg" (f10), so 1 deg is the threshold below which a boundary is not counted, and
# the mixture that produces Theta is built over the f1 population.
THETA_U = 1.0               # deg

# --- host grain ------------------------------------------------------------
GRAIN_RADIUS = 5.0e-6           # m

# --- adjustable parameters -------------------------------------------------
# n     number of dislocation families in a wall, range 1-3.
#       Gourdet & Montheillet, Acta Mater. 51 (2003) 2685-2699.  Not fitted.
# beta  geometric parameter linking the dislocation density to the crystallite
#       size; the analogue of the one of Rest & Hofman (2000).
# k     swept volume per unit wall fraction, Eq. (5): x = k*rho_ord/rho_tot is the
#       (extended) volume swept by the mobile boundaries.
# rho_crit  critical dislocation density, Eq. (1): only the dislocations above it are
#       available to polygonize, the rest stay as tangles.  It is the fixed density
#       scale the energy balance lacks (every other length in F scales as rho^-1/2),
#       and it gives the continuous threshold Theta ~ sqrt(bu - bu_c).  The analogue of
#       rho_crit = 6e14 of Veshchunov & Shestak (2009), used by formation option 3;
#       it must be >= rho_Nogita(0) = 10^13.8.
#
# beta, k and rho_crit come from `calibrate.py` (data set C, w_r = 0.2, w_X = 1):
# all four papers, each point weighted by its Rose quality rank x relevance
# (hbs_dataset.study_weight), which is also the weighting `comparison.py` scores with.
N_FAMILIES = 2.0                        # -
BETA       = 21.36831476383452          # -
K_SWEEP    = 0.6787994413909928         # -
RHO_CRIT   = 685421967748407.1          # m^-2   threshold at 47.1 GWd/tU

# --- Nogita & Une (1994), Eq. (1) ------------------------------------------
NOGITA_SLOPE = 2.2e-2           # 1/(GWd/tU)
NOGITA_INTERCEPT = 13.8         # log10(m^-2)

# --- SCIANTIX-specific cap -------------------------------------------------
# The restructured fraction must stay strictly below 1.  SCIANTIX divides by
# (1 - alpha) downstream -- GasDiffusion.C (sweeping term), Matrix.C (pore
# nucleation), System.C (production split) -- and alpha = 1 exactly would produce
# inf/NaN.  The cap is applied HERE as well as in the C++.
# How the other formation options handle the same thing:
#   option 3  caps at exactly this value, f_max = 1 - 1e-9, in the same way;
#   options 1 and 2  do NOT cap. They evaluate alpha_r = 1 - exp(-K (bu - bu_inc)^n)
#     and rely on the exponential never reaching zero.
ALPHA_MAX = 1.0 - 1.0e-9        # -

# --- Reference conditions --------------------------------------------------
REFERENCE_TEMPERATURE = 600.0    # K
REFERENCE_POROSITY = FABRICATION_POROSITY

# --- numerical minimization of Eq. (7) --------------------------------------
# [N] F is unimodal on the admissible interval at every burnup, so the coarse scan
#     brackets the minimum and the golden section refines it.  The tolerance is
#     deliberately far below what the function can resolve: F is flat at its bottom,
#     so eta is determined only to ~sqrt(eps) ~ 1e-8 whatever the tolerance (this is
#     the MINIMUM_RESOLUTION of compare_with_sciantix.py).  It is kept at 1e-13 so
#     that the search takes a FIXED number of steps, which is what lets the C++ port
#     follow the same path statement by statement.
MINIMIZATION_NODES = 400        # -      coarse scan of F(eta) before the golden section
MINIMIZATION_TOLERANCE = 1e-13  # -      on eta


# ---------------------------------------------------------------------------
# THE MODEL
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class ModelParameters:
    """The parameters `calibrate.py` is allowed to move (n is held fixed)."""

    n_families: float = N_FAMILIES
    beta: float = BETA
    k_sweep: float = K_SWEEP
    rho_crit: float = RHO_CRIT


DEFAULT_PARAMETERS = ModelParameters()


@dataclass
class HbsState:
    """The complete state of the model at one (burnup, temperature) point.

    The three quantities SCIANTIX consumes are `theta_deg`, `subgrain_radius_m`
    and `restructured_fraction`; the rest is carried for diagnostics.
    """
    burnup: float             # GWd/tU     input
    temperature: float        # K          input
    porosity: float           # -          input
    rho_tot: float            # m^-2       Eq. (1)
    shear_modulus: float      # Pa         Eq. (2)
    c0: float                 # J/m3       Eq. (6), all dislocations random
    e_wall: float             # J/m3       Eq. (6), at the equilibrium
    e_sweep: float            # J/m3       Eq. (6), at the equilibrium
    eta: float                # -          Eq. (8), after the cap
    theta_deg: float          # deg        Eq. (8)   <-- output 1
    subgrain_radius_m: float  # m          Eq. (9)   <-- output 2
    restructured_fraction: float  # -      Eq. (10)  <-- output 3
    driving_force: float      # J/m3       Eq. (11)
    rho_ordered: float        # m^-2       Eq. (5), condensed into LAGB walls
    rho_swept: float          # m^-2       Eq. (5), annihilated by the sweep
    rho_free: float           # m^-2       Eq. (5), still a random array
    balance_limited: bool     # -          Eq. (7b) is what set eta


def dislocation_density_nogita(burnup):
    """Eq. (1) -- total dislocation density [m^-2]. Nogita & Une (1994).

    log10(rho_tot) = 2.2e-2*bu + 13.8, with bu in GWd/tU.
    """
    return math.pow(10.0, NOGITA_SLOPE * burnup + NOGITA_INTERCEPT)


def dislocation_source(burnup, parameters=DEFAULT_PARAMETERS):
    """Eq. (1) -- the dislocations available to polygonize [m^-2], the density the model partitions.

        rho_tot(bu) = max( rho_Nogita(bu) - rho_crit , 0 )

    Eq. (1) is read as a pure source term, and only its part above the critical
    density rho_crit takes part in the partition.  With the Read-Shockley cut-offs any
    rho_tot > 0 polygonizes, since F has no energetic threshold of its own (every
    length in it scales as rho^-1/2); rho_crit is the fixed density scale that puts
    the threshold at rho_Nogita(bu_c) = rho_crit.  Theta then leaves zero
    continuously, roughly as sqrt(rho_tot), i.e. as sqrt(bu - bu_c) near the threshold.
    """
    return max(dislocation_density_nogita(burnup) - parameters.rho_crit, 0.0)


def shear_modulus(temperature, porosity=FABRICATION_POROSITY,
                  stoichiometry_deviation=0.0, plutonium_fraction=PLUTONIUM_FRACTION):
    """Eq. (2) -- shear modulus [Pa]. NEA/NSC/R(2024)1 p. 124."""
    composition = G_UO2 * (1.0 - plutonium_fraction) + G_PUO2 * plutonium_fraction
    porosity_factor = (1.0 - porosity) ** 2 / (1.0 + G_POROSITY_COEFF * porosity)
    stoichiometry_factor = (1.0 - G_STOICH_LINEAR * stoichiometry_deviation
                            + G_STOICH_QUADRATIC * stoichiometry_deviation ** 2)
    temperature_factor = (G_TEMP_CONSTANT
                          - G_TEMP_LINEAR * temperature
                          - G_TEMP_QUADRATIC * temperature * temperature)
    return 1.0e9 * composition * porosity_factor * stoichiometry_factor * temperature_factor


def poisson_ratio(temperature, porosity=FABRICATION_POROSITY,
                  stoichiometry_deviation=0.0, plutonium_fraction=PLUTONIUM_FRACTION):
    """Eq. (2) -- Poisson ratio [-]. NEA/NSC/R(2024)1 p. 124."""
    composition = NU_UO2 * (1.0 - plutonium_fraction) + NU_PUO2 * plutonium_fraction
    porosity_factor = 1.0 - NU_POROSITY_COEFF * porosity
    stoichiometry_factor = (1.0 + NU_STOICH_LINEAR * stoichiometry_deviation
                            - NU_STOICH_QUADRATIC * stoichiometry_deviation ** 2)
    temperature_factor = (NU_TEMP_CONSTANT
                          - NU_TEMP_LINEAR * temperature
                          + NU_TEMP_QUADRATIC * temperature * temperature)
    return composition * porosity_factor * stoichiometry_factor * temperature_factor


def line_energy_prefactor(temperature, porosity=FABRICATION_POROSITY,
                          stoichiometry_deviation=0.0,
                          plutonium_fraction=PLUTONIUM_FRACTION):
    """f(nu) = (1 - nu/2)/(1 - nu), Eq. (2). Hansen, Mater. Sci. Eng. 81 (1986) 141.

    The average over edge and screw character of the dislocation line energy.
    """
    nu = poisson_ratio(temperature, porosity, stoichiometry_deviation, plutonium_fraction)
    return (1.0 - 0.5 * nu) / (1.0 - nu)


def wall_geometry(rho_tot, parameters=DEFAULT_PARAMETERS):
    """Eq. (3) -- geometry of a fully developed low-angle boundary wall.

    Returns (rho_LAGB_max [m^-2], SoverV_max [m^-1], x_max [-]), the values the
    three geometric quantities take at eta = 1; x_max = k*rho_LAGB_max/rho_tot is
    the extended swept volume of Eq. (5).  The eta-dependence is applied in
    `dislocation_partition` and `hbs_state`.
    """
    n_families, beta, k_sweep = parameters.n_families, parameters.beta, parameters.k_sweep
    rho_lagb_max = math.pow(3.0 * n_families * THETA_MAX / (beta * BURGERS), 2.0)
    s_over_v_max = 9.0 * n_families * THETA_MAX / (beta * beta * BURGERS)
    swept_max = k_sweep * rho_lagb_max / rho_tot
    return rho_lagb_max, s_over_v_max, swept_max


def dislocation_partition(rho_tot, eta, parameters=DEFAULT_PARAMETERS):
    """Eq. (5) -- the three dislocation populations at a given eta [m^-2].

        rho_ord   = rho_LAGB_max * eta^2                  condensed into the LAGB walls
        x         = k * rho_ord / rho_tot                 extended swept volume
        rho_free  = (rho_tot - rho_ord) * exp(-x)         still a random array
        rho_swept = (rho_tot - rho_ord) * (1 - exp(-x))   annihilated by the sweep

    exp(-x) is Gourdet & Montheillet's d(rho_i) = -rho_i dV (their Eq. 4) integrated
    over the swept volume: linear in x for a small sweep, and never more than the
    free dislocations there are.
    """
    rho_lagb_max, _, swept_max = wall_geometry(rho_tot, parameters)
    # The `min` is the bound of Eq. (7b) written on the density; it only ever
    # absorbs the last-bit rounding of eta = sqrt(rho_tot/rho_LAGB_max).
    rho_ordered = min(rho_lagb_max * eta * eta, rho_tot)
    rho_free = (rho_tot - rho_ordered) * math.exp(-swept_max * eta * eta)
    return rho_ordered, rho_tot - rho_ordered - rho_free, rho_free


def line_energy_coefficients(temperature, rho_tot, eta, porosity=FABRICATION_POROSITY,
                             stoichiometry_deviation=0.0):
    """Eq. (4) -- (A1, A2(eta)), the line energies in units of G b^2 [-].

        A1      = f(nu)/(4 pi) * ln( rho_tot^(-1/2) / b )                random array
        A2(eta) = f(nu)/(4 pi) * ln( min(b/theta, rho_tot^(-1/2)) / b )  wall

    Humphreys, Rohrer & Rollett (2017) Eq. (2.6), with the outer cut-off at the
    spacing of the dislocations: rho_tot^(-1/2) in a random array, h = b/theta in a
    Read-Shockley wall (their Eqs. 4.4-4.5).  The cap keeps a wall more dilute than
    the random array from being dearer than it.
    """
    f_nu = line_energy_prefactor(temperature, porosity, stoichiometry_deviation)
    spacing = math.pow(rho_tot, -0.5)
    theta = eta * THETA_MAX
    wall_spacing = spacing if theta * spacing <= BURGERS else BURGERS / theta
    a1 = f_nu / (4.0 * math.pi) * math.log(spacing / BURGERS)
    a2 = f_nu / (4.0 * math.pi) * math.log(wall_spacing / BURGERS)
    return a1, a2


def energy_terms(temperature, rho_tot, eta, porosity=FABRICATION_POROSITY,
                 stoichiometry_deviation=0.0, parameters=DEFAULT_PARAMETERS):
    """Eq. (6) -- (C0, E_wall, E_sweep) at a given eta [J/m^3].

        F       = rho_free*A1*G b^2 + rho_ord*A2*G b^2 = C0 + E_wall + E_sweep
        C0      = rho_tot * A1 * G b^2            all dislocations random
        E_wall  = rho_ord * (A2 - A1) * G b^2     condensing into walls (< 0)
        E_sweep = -rho_swept * A1 * G b^2         annihilation (< 0)
    """
    gb2 = shear_modulus(temperature, porosity, stoichiometry_deviation) * BURGERS * BURGERS
    a1, a2 = line_energy_coefficients(temperature, rho_tot, eta, porosity, stoichiometry_deviation)
    rho_ordered, rho_swept, _ = dislocation_partition(rho_tot, eta, parameters)
    return rho_tot * a1 * gb2, rho_ordered * (a2 - a1) * gb2, -rho_swept * a1 * gb2


def reduced_energy(rho_tot, eta, parameters=DEFAULT_PARAMETERS):
    """(F - C0) / (f(nu) G b^2 / 4 pi) at a given eta [m^-2].

        = -rho_swept * L1 + rho_ord * (L2 - L1),   L = ln(R/b) of Eq. (4)

    f(nu) G b^2 / (4 pi) multiplies every term of F, so the minimum does not depend on
    it; this is what `equilibrium_eta` compares, and what the C++ port evaluates with
    the same arithmetic, statement by statement.
    """
    rho_lagb_max, _, swept_max = wall_geometry(rho_tot, parameters)
    spacing = math.pow(rho_tot, -0.5)
    log_free = math.log(spacing / BURGERS)
    theta = eta * THETA_MAX
    log_wall = log_free if theta * spacing <= BURGERS else math.log(BURGERS / theta / BURGERS)
    rho_ordered = min(rho_lagb_max * eta * eta, rho_tot)
    rho_free = (rho_tot - rho_ordered) * math.exp(-swept_max * eta * eta)
    rho_swept = rho_tot - rho_ordered - rho_free
    return -rho_swept * log_free + rho_ordered * (log_wall - log_free)


def equilibrium_eta(rho_tot, eta_upper, parameters=DEFAULT_PARAMETERS):
    """Eq. (7) -- the eta that minimizes F on the admissible interval [0, eta_upper].

    A2 carries the Read-Shockley theta*ln(theta) and the sweep an exponential, so F is
    not a polynomial in eta: a coarse scan brackets the minimum, a golden section
    refines it.  Temperature, porosity and stoichiometry do not enter (see
    `reduced_energy`).
    """
    def energy(eta):
        return reduced_energy(rho_tot, eta, parameters)

    nodes = MINIMIZATION_NODES
    grid = [eta_upper * i / nodes for i in range(nodes + 1)]
    best = min(range(nodes + 1), key=lambda i: energy(grid[i]))
    low, high = grid[max(best - 1, 0)], grid[min(best + 1, nodes)]
    ratio = (math.sqrt(5.0) - 1.0) / 2.0
    while high - low > MINIMIZATION_TOLERANCE:
        left, right = high - ratio * (high - low), low + ratio * (high - low)
        if energy(left) < energy(right):
            high = right
        else:
            low = left
    eta = 0.5 * (low + high)
    # on the bound (7b) or the HAGB cap, exactly: F is flat there to rounding, so a
    # bracket closing on eta_upper counts as reaching it
    if eta_upper - eta <= MINIMIZATION_TOLERANCE or energy(eta_upper) <= energy(eta):
        eta = eta_upper
    return eta if energy(eta) < 0.0 else 0.0


def hbs_state(burnup, temperature, porosity=FABRICATION_POROSITY,
              stoichiometry_deviation=0.0, grain_radius_m=GRAIN_RADIUS,
              parameters=DEFAULT_PARAMETERS):
    """The model. Eqs. (1)-(11) for one (burnup [GWd/tU], temperature [K]) point.

    This is the function `case 4` of `src/models/HighBurnupStructureFormation.C`
    mirrors, statement by statement: scalar, `math` only, same order, same arithmetic,
    so that `compare_with_sciantix.py` can check the two against each other.

    Returns an `HbsState`.  `subgrain_radius_m` is `nan` where eta = 0, where there
    are no subgrains; SCIANTIX writes 0.0 there instead.
    """
    # (1) dislocation density produced by irradiation
    rho_tot = dislocation_source(burnup, parameters)

    # (2)-(3) elastic constants, wall geometry
    rho_lagb_max, s_over_v_max, swept_max = (wall_geometry(rho_tot, parameters) if rho_tot > 0.0
                                             else (0.0, 0.0, 0.0))

    if rho_tot > 0.0:
        # (7b) admissibility, and the LAGB/HAGB cap of Eq. (8).
        #      eta_hagb is 1.0 by construction, since THETA_MAX = radians(THETA_HAGB)
        #      (Eq. 7a).  It is written out because the C++ carries theta_max and
        #      theta_HAGB as two parameters (offsets 4 and 5) and evaluates this same
        #      quotient; keeping the statement here is what keeps the two in step.
        eta_balance = math.sqrt(min(rho_tot / rho_lagb_max, 1.0))
        eta_hagb = math.radians(THETA_HAGB) / THETA_MAX

        # (4)-(7) minimum of F on the admissible interval
        eta = equilibrium_eta(rho_tot, min(eta_balance, eta_hagb), parameters)
        balance_limited = eta_balance < eta_hagb and eta >= eta_balance * (1.0 - 1e-9)
    else:
        eta, balance_limited = 0.0, False          # fresh fuel: nothing to partition

    # (8) mean misorientation                                     <-- output 1
    theta = math.degrees(eta * THETA_MAX)
    # exact inverse of the line above, so a no-op in Python.  It is kept because the
    # C++ applies the cap of Eq. (8) here and must re-derive eta from the capped
    # angle; both files therefore carry the same two statements in the same order.
    eta = math.radians(theta) / THETA_MAX

    # (9) subgrain radius, capped at the host grain               <-- output 2
    #     the walls inside the swept volume go with it (Gourdet & Montheillet
    #     Eq. 8), so the wall area left is S/V * exp(-x)
    s_over_v = s_over_v_max * eta * math.exp(-swept_max * eta * eta)
    if s_over_v > 0.0:
        radius = min(1.5 / s_over_v, grain_radius_m)   # a subgrain cannot exceed its grain
    else:
        radius = math.nan                          # no substructure, not a length

    # (10) restructured fraction, lever rule                      <-- output 3
    fraction = (theta - THETA_U) / (THETA_HAGB - THETA_U)
    if fraction < 0.0:
        fraction = 0.0
    elif fraction > ALPHA_MAX:
        fraction = ALPHA_MAX

    # (11) driving force, for the nucleation criterion
    if rho_tot > 0.0:
        c0, e_wall, e_sweep = energy_terms(temperature, rho_tot, eta, porosity,
                                           stoichiometry_deviation, parameters)
        rho_ordered, rho_swept, rho_free = dislocation_partition(rho_tot, eta, parameters)
    else:
        c0, e_wall, e_sweep = 0.0, 0.0, 0.0
        rho_ordered, rho_swept, rho_free = 0.0, 0.0, 0.0
    driving_force = c0 + e_wall + e_sweep

    return HbsState(
        burnup=burnup,
        temperature=temperature,
        porosity=porosity,
        rho_tot=rho_tot,
        shear_modulus=shear_modulus(temperature, porosity, stoichiometry_deviation),
        c0=c0,
        e_wall=e_wall,
        e_sweep=e_sweep,
        eta=eta,
        theta_deg=theta,
        subgrain_radius_m=radius,
        restructured_fraction=fraction,
        driving_force=driving_force,
        rho_ordered=rho_ordered,
        rho_swept=rho_swept,
        rho_free=rho_free,
        balance_limited=balance_limited,
    )


def hbs_state_array(burnup, temperature, **keywords):
    """numpy convenience wrapper over `hbs_state`. No physics of its own.

    `burnup` and `temperature` are broadcast against each other; returns a dict
    of arrays with the same keys as the fields of `HbsState`.
    """
    import numpy as np

    bu, temp = np.broadcast_arrays(np.asarray(burnup, dtype=float),
                                   np.asarray(temperature, dtype=float))
    states = [hbs_state(float(b), float(t), **keywords) for b, t in zip(bu.ravel(), temp.ravel())]
    keys = HbsState.__dataclass_fields__.keys()
    return {k: np.array([getattr(s, k) for s in states]).reshape(bu.shape) for k in keys}


# ---------------------------------------------------------------------------
# VALIDATION AGAINST THE EBSD DATASET
# ---------------------------------------------------------------------------

DATA_FILE = dataset_dir()          # the data/ folder of curated JSON datasets


def load_ebsd(path=DATA_FILE):
    """The EBSD rows of the curated datasets, as a list of dicts of floats.

    Joined by `hbs_dataset.load_rows` from `data/*.json`: the standard-UO2 EBSD maps of
    Zacharie-Aubrun et al. (2022) and Onofri et al. (2025), with the local conditions
    (burnup, temperature) of each radial point.

    `porosity` and `grain_radius` are the as-fabricated values of the specimen, used
    respectively in Eq. (2) and as the ceiling of Eq. (9); the module defaults stand in
    wherever the JSON files carry no value.

    `burnup` is the local burnup, which is what this model uses; `burnup_effective` is
    carried for the comparison with the KJMA options of SCIANTIX, which are driven by
    the effective burnup instead.
    """
    return load_rows(path, fabrication_porosity=FABRICATION_POROSITY,
                     grain_radius=GRAIN_RADIUS)


def theta_measured(row):
    """The measured mean misorientation [deg], Eq. (10) read backwards.

        Theta = [ AMis*(f1 - f10) + 10*f10 ] / 100

    written as the mixture mean it is: a fraction f10 of the map is restructured
    and sits at 10 deg, the remaining f1 - f10 is matrix and sits at AMis.
    """
    f1, f10, amis = row["f1"], row["f10"], row["amis"]
    if not f1 > 0.0:
        return 0.0
    return (f1 * 0.01) * (amis * (f1 - f10) / f1 + 10.0 * (f10 / f1))


def measured_radius(row):
    """The measured subgrain radius [m], or nan.

    ECD50% of the NEW grains where it exists -- there the grain that grew is the
    new one -- of the sub-grains otherwise, HALVED because ECD is a diameter and
    the model predicts a radius.
    """
    ecd = row["ecd_new"] if not math.isnan(row["ecd_new"]) else row["ecd_sub"]
    return ecd / 2.0 * 1e-6 if not math.isnan(ecd) else math.nan


def _rmse(observed, predicted):
    n = len(observed)
    return math.sqrt(sum((o - p) ** 2 for o, p in zip(observed, predicted)) / n)


def _r_squared(observed, predicted):
    """Coefficient of determination against the null model "predict the mean"."""
    mean = sum(observed) / len(observed)
    ss_res = sum((o - p) ** 2 for o, p in zip(observed, predicted))
    ss_tot = sum((o - mean) ** 2 for o in observed)
    return 1.0 - ss_res / ss_tot


def validate(path=DATA_FILE, verbose=True, parameters=DEFAULT_PARAMETERS):
    """The three metrics of the module docstring. Returns them as a dict.

    Selection of the points, identical to the calibration:
      Theta   every row with burnup > 0                                  (41)
      X       rows that also carry a restructured fraction at 10 deg     (27)
      r_n     rows that carry a size, halved from ECD50%                 (14)

    Each point is evaluated with the porosity and the grain radius of its own
    specimen, so Eq. (2) and the ceiling of Eq. (9) see the real fuel.

    A size point below the threshold of Eq. (1) has no predicted radius -- there
    are no subgrains there -- so it cannot be scored.  Such points are counted and
    reported rather than scored, because which points fall below the threshold is a
    property of the calibration and must stay visible when rho_crit moves.
    """
    rows = load_ebsd(path)
    metrics = {}

    theta_obs, theta_mod = [], []
    frac_obs, frac_mod = [], []
    size_obs, size_mod = [], []
    size_below_threshold = []

    for row in rows:
        if not row["burnup"] > 0.0:
            continue
        state = hbs_state(row["burnup"], row["temperature"],
                          porosity=row["porosity"], grain_radius_m=row["grain_radius"],
                          parameters=parameters)

        theta_obs.append(theta_measured(row))
        theta_mod.append(state.theta_deg)

        if not math.isnan(row["f10"]):
            frac_obs.append(row["f10"] / 100.0)
            frac_mod.append(state.restructured_fraction)

        radius = measured_radius(row)
        if not math.isnan(radius):
            if math.isnan(state.subgrain_radius_m):
                size_below_threshold.append((row["label"], row["burnup"]))
            else:
                size_obs.append(radius)
                size_mod.append(state.subgrain_radius_m)

    metrics["theta"] = dict(n=len(theta_obs), rmse=_rmse(theta_obs, theta_mod),
                            r2=_r_squared(theta_obs, theta_mod))
    metrics["fraction"] = dict(n=len(frac_obs), rmse=_rmse(frac_obs, frac_mod),
                               r2=_r_squared(frac_obs, frac_mod))
    metrics["radius"] = dict(n=len(size_obs), rmse=_rmse(size_obs, size_mod),
                             r2=_r_squared(size_obs, size_mod))
    metrics["radius_below_threshold"] = size_below_threshold

    if verbose:
        print("Validation against %s" % path)
        print("  dislocation density: Nogita & Une (1994);  shear modulus: NEA/NSC/R(2024)1")
        print("  n = %g, beta = %g, k = %g"
              % (parameters.n_families, parameters.beta, parameters.k_sweep))
        print()
        print("  mean misorientation  Theta   N = %2d   RMSE = %.4f deg   R2 = %.4f"
              % (metrics["theta"]["n"], metrics["theta"]["rmse"], metrics["theta"]["r2"]))
        print("  restructured fraction X      N = %2d   RMSE = %.4f       R2 = %.4f"
              % (metrics["fraction"]["n"], metrics["fraction"]["rmse"], metrics["fraction"]["r2"]))
        print("  subgrain radius       r_n    N = %2d   RMSE = %.4f um    R2 = %.4f"
              % (metrics["radius"]["n"], metrics["radius"]["rmse"] * 1e6, metrics["radius"]["r2"]))
        if size_below_threshold:
            print()
            print("  %d size point(s) not scored: below the threshold of Eq. (1), where there"
                  % len(size_below_threshold))
            print("  are no subgrains and the radius is not a length --")
            for label, burnup in size_below_threshold:
                print("    %-20s bu = %g GWd/tU" % (label, burnup))
    return metrics

def _bisect(function, low, high, tolerance=1e-9):
    """Smallest root bracketed by [low, high] of a monotone sign-changing function."""
    f_low = function(low)
    while high - low > tolerance:
        middle = 0.5 * (low + high)
        if (function(middle) > 0.0) == (f_low > 0.0):
            low = middle
        else:
            high = middle
    return 0.5 * (low + high)


def regime_boundaries(temperature=REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY,
                      parameters=DEFAULT_PARAMETERS):
    """(threshold, onset, saturation) in GWd/tU, by bisection on Theta.

    threshold is where Theta leaves zero, 0.0 if it is already positive in fresh
    fuel (as it is with the Read-Shockley cut-offs, which have no energetic
    threshold); onset is Theta = theta_u, where X leaves zero.
    """
    def theta(burnup):
        return hbs_state(burnup, temperature, porosity=porosity, parameters=parameters).theta_deg

    threshold = 0.0 if theta(0.0) > 0.0 else _bisect(lambda b: theta(b) - 1e-12, 0.0, 200.0)
    onset = _bisect(lambda b: theta(b) - THETA_U, threshold, 300.0)
    saturation = _bisect(lambda b: theta(b) - THETA_HAGB * (1.0 - 1e-12), onset, 400.0)
    return threshold, onset, saturation


def selftest(verbose=True):
    """Every invariant the model must satisfy. """
    checks = []

    def check(name, condition, detail=""):
        checks.append((name, bool(condition), detail))

    # below the critical density of Eq. (1) there is nothing to polygonize
    bu_crit = (math.log10(DEFAULT_PARAMETERS.rho_crit) - NOGITA_INTERCEPT) / NOGITA_SLOPE
    for temperature in (300.0, 723.0, 1200.0):
        for burnup in (0.0, 0.999 * bu_crit):
            virgin = hbs_state(burnup, temperature)
            check("below rho_crit (bu = %.2f, T = %g K): no structure" % (burnup, temperature),
                  virgin.rho_tot == 0.0 and virgin.theta_deg == 0.0
                  and virgin.restructured_fraction == 0.0 and math.isnan(virgin.subgrain_radius_m))

    # the threshold is continuous: Theta leaves zero as sqrt(bu - bu_c), with no jump
    steps = (1e-4, 1e-3, 1e-2, 1e-1)
    thetas = [hbs_state(bu_crit + d, REFERENCE_TEMPERATURE).theta_deg for d in steps]
    exponents = [math.log(thetas[i + 1] / thetas[i]) / math.log(10.0) for i in range(len(steps) - 1)]
    check("continuous threshold at bu_c = %.3f: Theta -> 0, exponent ~ 1/2" % bu_crit,
          0.0 < thetas[0] < 0.01 and all(0.4 < e < 0.6 for e in exponents),
          "Theta(bu_c + 1e-4) = %.2e deg, local exponents %s"
          % (thetas[0], ", ".join("%.3f" % e for e in exponents)))

    # a wall is never dearer than the random array (Humphreys et al. 2017, 6.4.1).
    # Only burnups above the threshold can test this: below it rho_tot = 0 and every
    # energy term is exactly 0, which would satisfy "<= 0" without testing anything.
    wall_states = [hbs_state(b, REFERENCE_TEMPERATURE) for b in (50.0, 60.0, 90.0, 120.0, 150.0)]
    assert all(s.rho_tot > 0.0 for s in wall_states), "E_wall check needs rho_tot > 0"
    worst_wall = max(s.e_wall for s in wall_states)
    check("E_wall < 0 wherever rho_tot > 0: condensing into walls lowers the energy",
          worst_wall < 0.0,
          "max E_wall = %+.3e J/m3 over bu = 50-150 GWd/tU" % worst_wall)

    # the regimes on either side
    _, BU_ONSET, BU_SATURATION = regime_boundaries(
        temperature=REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY, parameters=DEFAULT_PARAMETERS)
    below = hbs_state(BU_ONSET - 0.01, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY)
    above = hbs_state(BU_SATURATION + 0.01, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY)
    check("below the onset X = 0", below.restructured_fraction == 0.0)
    check("above saturation Theta = theta_HAGB and X = ALPHA_MAX",
          above.theta_deg == THETA_HAGB and above.restructured_fraction == ALPHA_MAX)
    check("X is never exactly 1 (SCIANTIX divides by 1 - X)",
          all(hbs_state(b, REFERENCE_TEMPERATURE).restructured_fraction < 1.0
              for b in (86.0, 100.0, 200.0, 500.0)))

    # monotonicity in burnup at fixed temperature
    burnups = [1.0 + 0.5 * i for i in range(400)]        # 1 -> 200.5 GWd/tU
    states = [hbs_state(b, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY) for b in burnups]
    check("Theta is non-decreasing in burnup",
          all(b.theta_deg >= a.theta_deg - 1e-15 for a, b in zip(states, states[1:])))
    check("X is non-decreasing in burnup",
          all(b.restructured_fraction >= a.restructured_fraction - 1e-15
              for a, b in zip(states, states[1:])))
    radii = [s.subgrain_radius_m for s in states if not math.isnan(s.subgrain_radius_m)]
    check("the subgrain radius is non-increasing in burnup",
          all(b <= a + 1e-20 for a, b in zip(radii, radii[1:])))

    # the subgrain radius never exceeds the grain that hosts it
    ceiling_worst = 0.0
    for grain_radius in (1.0e-6, 5.0e-6, 1.0e-5):
        for burnup in burnups:
            radius = hbs_state(burnup, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY,
                               grain_radius_m=grain_radius).subgrain_radius_m
            if not math.isnan(radius):
                ceiling_worst = max(ceiling_worst, radius / grain_radius)
    check("r_n never exceeds the host grain radius", ceiling_worst <= 1.0,
          "max r_n / R_grain = %.6f" % ceiling_worst)

    # the three outputs depend on the local burnup alone
    worst_invariance = 0.0
    for burnup in (52.0, 60.0, 70.0, 85.0, 120.0):
        base = hbs_state(burnup, 600.0, porosity=0.05, stoichiometry_deviation=0.0)
        for temperature in (500.0, 900.0, 1300.0):
            for porosity in (0.02, 0.05, 0.10):
                for deviation in (0.0, 0.01):
                    other = hbs_state(burnup, temperature, porosity=porosity,
                                      stoichiometry_deviation=deviation)
                    for got, want in ((other.theta_deg, base.theta_deg),
                                      (other.restructured_fraction,
                                       base.restructured_fraction)):
                        worst_invariance = max(worst_invariance, abs(got - want))
    check("Theta and X depend on burnup alone: f(nu) G b^2 multiplies every term of F",
          # F is flat at its minimum, so the numerical eta is resolved to ~sqrt(eps)
          worst_invariance < 1e-6, "max absolute difference %.2e" % worst_invariance)
    check("the driving force still moves with temperature",
          hbs_state(70.0, 500.0).driving_force > hbs_state(70.0, 1200.0).driving_force)

    # eta_eq is the minimum of F on the admissible interval
    worst_minimum = 0.0
    for burnup in (40.0, 50.0, 60.0, 70.0, 80.0, 100.0):
        state = hbs_state(burnup, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY)
        if state.rho_tot == 0.0:
            continue
        rho_lagb_max, _, _ = wall_geometry(state.rho_tot)
        upper = min(math.sqrt(min(state.rho_tot / rho_lagb_max, 1.0)), 1.0)
        f_eq = state.e_wall + state.e_sweep
        for i in range(201):
            _, e_wall, e_sweep = energy_terms(REFERENCE_TEMPERATURE, state.rho_tot, upper * i / 200,
                                              REFERENCE_POROSITY)
            worst_minimum = max(worst_minimum, (f_eq - (e_wall + e_sweep)) / abs(f_eq))
    check("F(eta_eq) <= F(eta) on the admissible interval", worst_minimum < 1e-12,
          "max relative excess %.2e" % worst_minimum)

    # the dislocation balance, Eq. (5) and Eq. (7b).
    worst_closure, worst_free, worst_swept = 0.0, 0.0, 0.0
    for state in states:
        if state.rho_tot == 0.0:                    # below rho_crit: nothing to partition
            continue
        rho_ordered, rho_swept, rho_free = (state.rho_ordered, state.rho_swept,
                                            state.rho_free)
        worst_closure = max(worst_closure,
                            abs(rho_ordered + rho_swept + rho_free - state.rho_tot)
                            / state.rho_tot)
        worst_free = min(worst_free, rho_free / state.rho_tot)
        worst_swept = min(worst_swept, rho_swept / state.rho_tot)
    check("rho_ord + rho_swept + rho_free = rho_tot", worst_closure < 1e-15,
          "max relative closure error %.2e" % worst_closure)
    check("rho_free >= 0: the walls never hold more dislocations than exist",
          worst_free == 0.0, "min rho_free / rho_tot = %.3e" % worst_free)
    check("rho_swept >= 0: the sweep is a sink, never a source",
          worst_swept == 0.0, "min rho_swept / rho_tot = %.3e" % worst_swept)

    # the sweep, Eq. (5): 1 - exp(-x) is linear in x for a small sweep (the
    #     d(rho_i) = -rho_i dV of Gourdet & Montheillet) and saturates at 1
    rho_test = dislocation_density_nogita(60.0)
    rho_lagb_max, _, swept_max = wall_geometry(rho_test)
    eta_balance = math.sqrt(min(rho_test / rho_lagb_max, 1.0))
    for eta_test in (1e-4 * eta_balance, 0.5 * eta_balance, 0.99 * eta_balance):
        _, swept, _ = dislocation_partition(rho_test, eta_test)
        x = swept_max * eta_test * eta_test
        linear = (rho_test - rho_lagb_max * eta_test * eta_test) * x
        check("sweep at x = %.3g: 0 <= rho_swept <= linear sweep, equal as x -> 0" % x,
              # rho_swept = rho_tot - rho_ord - rho_free cancels to ~eps/x at tiny x
              0.0 <= swept <= linear * (1.0 + 1e-6)
              and (x > 1e-6 or abs(swept / linear - 1.0) < 1e-6),
              "rho_swept / linear = %.6f" % (swept / linear))

    # on the bound the model is the classical theta ~ sqrt(rho_tot): with
    #     every dislocation in a wall, the misorientation can only grow as fast
    #     as the dislocations that feed it.
    #     Whether the bound binds at all depends on the calibration: with the shipped
    #     parameters it never does, so the law is checked on a small-k set that
    #     binds (the sweep is weak, the walls take everything), and the shipped set
    #     is only counted.
    binding_parameters = ModelParameters(beta=22.0, k_sweep=0.1)
    shipped_bound_points = sum(state.balance_limited for state in states)
    worst_sqrt_law, bound_points = 0.0, 0
    for burnup in burnups:
        state = hbs_state(burnup, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY,
                          parameters=binding_parameters)
        if not state.balance_limited:
            continue
        bound_points += 1
        classical = math.degrees(binding_parameters.beta * BURGERS * math.sqrt(state.rho_tot)
                                 / (3.0 * binding_parameters.n_families))
        worst_sqrt_law = max(worst_sqrt_law,
                             abs(state.theta_deg - classical) / classical)
    check("on the balance bound Theta = beta b sqrt(rho_tot) / 3n",
          bound_points > 0 and worst_sqrt_law < 1e-14,
          "%d points on the bound with the binding set, max relative error %.2e; "
          "%d with the shipped set" % (bound_points, worst_sqrt_law, shipped_bound_points))

    # with theta_max = theta_HAGB the order parameter reaches 1 exactly where the
    #     substructure becomes high-angle.  That the last free dislocation enters a
    #     wall at the same burnup holds only when the bound binds up to saturation
    #     (the binding set above); in general the walls hold a fraction <= 1.
    # just past the bisected saturation burnup, where the minimum sits on the cap exactly
    at_cap = hbs_state(BU_SATURATION + 1e-3, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY)
    check("eta = 1 <=> Theta = theta_HAGB, with rho_ord <= rho_tot there",
          abs(at_cap.eta - 1.0) < 1e-9
          and abs(math.degrees(THETA_MAX) - THETA_HAGB) < 1e-12
          and at_cap.rho_ordered <= at_cap.rho_tot,
          "eta = %.9f at bu = %.4f, rho_ord / rho_tot = %.6f"
          % (at_cap.eta, BU_SATURATION, at_cap.rho_ordered / at_cap.rho_tot))

    # the exact bridge to the stored energy of Muramatsu et al. (2014), Eq. 8:
    #    E_s = rho_tot*G*b^2/2, so C0/E_s = f(nu)*ln(rho_tot^(-1/2)/b)/(2 pi): the
    #    constant 1/2 of Muramatsu is Humphreys' c2 ~ 0.5 (Eq. 2.8), here resolved
    #    into its logarithm, which grows slowly as the dislocations crowd.
    worst_ratio = 0.0
    for temperature in (600.0, 1000.0):
        for burnup in (40.0, 60.0, 80.0, 150.0):
            state = hbs_state(burnup, temperature)
            if state.rho_tot == 0.0:
                continue
            expected_ratio = (line_energy_prefactor(temperature)
                              * math.log(math.pow(state.rho_tot, -0.5) / BURGERS) / (2.0 * math.pi))
            stored = state.rho_tot * state.shear_modulus * BURGERS ** 2 / 2.0
            worst_ratio = max(worst_ratio,
                              abs(state.c0 / stored - expected_ratio) / expected_ratio)
    check("C0 / E_s(Muramatsu Eq. 8) = f(nu) ln(rho_tot^-1/2 / b) / 2pi",
          worst_ratio < 1e-14, "max relative spread %.2e" % worst_ratio)

    # the numpy wrapper carries no physics of its own
    try:
        import numpy  # noqa: F401
    except ImportError:
        check("hbs_state_array agrees with hbs_state", True, "skipped, numpy not available")
    else:
        grid_bu = [20.0, 40.0, 60.0, 80.0, 120.0]
        arrays = hbs_state_array(grid_bu, 750.0)
        worst_wrapper = 0.0
        for index, burnup in enumerate(grid_bu):
            scalar = hbs_state(burnup, 750.0)
            for key in ("theta_deg", "restructured_fraction", "rho_tot", "driving_force"):
                worst_wrapper = max(worst_wrapper,
                                    abs(float(arrays[key][index]) - getattr(scalar, key)))
        check("hbs_state_array agrees with hbs_state", worst_wrapper == 0.0,
              "max absolute difference %.2e" % worst_wrapper)

    if verbose:
        print("Self-test -- the model against itself, no experimental data")
        print()
    failures = 0
    for name, passed, detail in checks:
        failures += not passed
        if verbose:
            print("  [%s] %s%s" % ("ok" if passed else "FAIL", name,
                                   ("   (%s)" % detail) if detail else ""))
    if verbose:
        print()
        print("  %d checks, %d failed" % (len(checks), failures))
    if failures:
        raise AssertionError("%d self-test check(s) failed" % failures)
    return checks


def plot(path="hbs_formation_landau.png", temperature=REFERENCE_TEMPERATURE):
    """The three outputs against burnup, with the EBSD points. Needs matplotlib."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    plt.style.use("seaborn-v0_8-whitegrid")
    plt.rcParams.update({
        "figure.figsize": (10, 7),
        "font.family": "serif",
        "font.serif": ["Times New Roman", "Times", "Nimbus Roman", "DejaVu Serif"],
        "mathtext.fontset": "dejavuserif",
        "font.size": 20,
        "axes.labelsize": 20,
        "axes.titlesize": 20,
        "xtick.labelsize": 20,
        "ytick.labelsize": 20,
        "legend.fontsize": 20,
        "figure.dpi": 300,
        "axes.grid": True,
        "grid.alpha": 0.5,
        "grid.linestyle": "--",
        "lines.linewidth": 3,
        "lines.markersize": 6,
        "legend.frameon": True,
        "legend.loc": "upper right"
    })

    burnups = [1.0 + 0.25 * i for i in range(800)]
    states = [hbs_state(b, temperature) for b in burnups]
    rows_O = [r for r in load_ebsd() if r["Dataset"] == "Onofri"]
    rows_Z = [r for r in load_ebsd() if r["Dataset"] == "Zacharie"]

    figure, axes = plt.subplots(1, 3, figsize=(18, 6.0))

    axes[0].plot(burnups, [s.theta_deg for s in states], "-", color="k", 
        label="This work")
    axes[0].plot([r["burnup"] for r in rows_O], [theta_measured(r) for r in rows_O], "o", 
         label="Onofri et al. (2025)")
    axes[0].plot([r["burnup"] for r in rows_Z], [theta_measured(r) for r in rows_Z], "*", 
         label="Zacharie-Aubrun et al. (2022)")

    axes[0].set_ylabel(r"mean misorientation  $\Theta$  (deg)")

    axes[1].plot(burnups, [s.subgrain_radius_m * 1e6 for s in states], "-", color="k",
        label="This work")

    sizes = [(r["burnup"], measured_radius(r) * 1e6) for r in rows_O]
    sizes = [(b, e) for b, e in sizes if not math.isnan(e)]
    axes[1].plot([b for b, _ in sizes], [e for _, e in sizes], "o",  
        label="Onofri et al. (2025)")

    sizes = [(r["burnup"], measured_radius(r) * 1e6) for r in rows_Z]
    sizes = [(b, e) for b, e in sizes if not math.isnan(e)]
    axes[1].plot([b for b, _ in sizes], [e for _, e in sizes], "*", 
         label="Zacharie-Aubrun et al. (2022)")

    axes[1].legend()
    axes[1].set_ylabel(r"subgrain radius  $r_n$  ($\mu$m)")
    axes[1].set_ylim(0.0, 1.5)

    axes[2].plot(burnups, [s.restructured_fraction for s in states], "-", color="k",
        label="This work")

    fractions = [(r["burnup"], r["f10"] / 100.0) for r in rows_O if not math.isnan(r["f10"])]
    axes[2].plot([b for b, _ in fractions], [x for _, x in fractions], "o", 
         label="Onofri et al. (2025)")

    fractions = [(r["burnup"], r["f10"] / 100.0) for r in rows_Z if not math.isnan(r["f10"])]
    axes[2].plot([b for b, _ in fractions], [x for _, x in fractions],  "*", 
         label="Zacharie-Aubrun et al. (2022)")

    axes[2].set_ylabel("restructured fraction  $X$  (-)")

    for axis in axes:
        axis.set_xlim(burnups[0], burnups[-1])
        axis.set_xlabel("burnup  (GWd/tU)")
    figure.suptitle("HBS formation, Landau functional -- T = %g K, "
                    r"$\rho_{tot}$ Nogita & Une (1994)" % temperature)
    figure.tight_layout()
    figure.savefig(path, dpi=150)
    print("written: %s" % path)

def main(argv=None):
    parser = argparse.ArgumentParser(
        description="HBS formation as a second-order phase transition (Landau functional). "
                    "Reference implementation of iHighBurnupStructureFormation = 4.",
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--validate", action="store_true",
                        help="the model against the EBSD measurements")
    parser.add_argument("--selftest", action="store_true",
                        help="the model against itself: invariants, no experimental data")
    parser.add_argument("--plot", nargs="?", const="hbs_formation_landau.png", default=None,
                        metavar="PNG", help="the three outputs against burnup")
    parser.add_argument("--temperature", type=float, default=REFERENCE_TEMPERATURE, metavar="K",
                        help="temperature for --table and --plot (default %g)"
                             % REFERENCE_TEMPERATURE)
    parser.add_argument("--point", nargs=2, type=float, default=None, metavar=("BU", "T"),
                        help="evaluate the model at one (burnup [GWd/tU], temperature [K])")
    arguments = parser.parse_args(argv)

    if not any((arguments.validate, arguments.selftest,
                arguments.plot, arguments.point)):
        parser.print_help()
        return 0

    printed = False
    for enabled, action in ((arguments.selftest, lambda: selftest()),
                            (arguments.validate, lambda: validate())):
        if enabled:
            if printed:
                print("\n" + "=" * 72 + "\n")
            action()
            printed = True

    if arguments.point:
        if printed:
            print("\n" + "=" * 72 + "\n")
        state = hbs_state(arguments.point[0], arguments.point[1])
        width = max(len(f) for f in HbsState.__dataclass_fields__)
        for field_name in HbsState.__dataclass_fields__:
            print("  %-*s  %.17g" % (width, field_name, getattr(state, field_name)))
        printed = True

    if arguments.plot:
        plot(arguments.plot, arguments.temperature)
    return 0


if __name__ == "__main__":
    sys.exit(main())
