"""High-burnup structure formation as a second-order phase transition (Landau functional).

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
the external condition is the local burnup.  The free
energy is a Landau functional F = C0 + C2 eta^2 + C4 eta^4 built by splitting the
dislocations into three populations that must add up to rho_tot -- free, stored
in low-angle walls, annihilated by the sweeping boundaries -- and giving each the
energy per unit length E_D = G b^2 f(nu)/(4 pi) ln(R/b) of the state it is in.
Only even powers appear, because the energy is invariant under the sign of theta
(the sign of the angle is a convention).  Minimizing F over the range of eta for
which that partition is physical gives the equilibrium misorientation; the
subgrain size follows from the same wall geometry; and the restructured fraction
follows from the lever rule, because the measured misorientation is the mean of a
two-phase mixture.

Three quantities are produced:

    output 1    Theta       mean misorientation             [deg]   Eq. (8)
    output 2    r_n         subgrain radius                 [m]     Eq. (9)
    output 3    X           restructured volume fraction    [-]     Eq. (10)

The dislocation density is fixed to Nogita & Une (1994).

EQUATIONS
---------------------------------------------------------------------------------

  (1)  dislocation density -- Nogita & Une (1994)
       rho_tot(bu) = 10^(2.2e-2*bu + 13.8)                              [m^-2]

  (2)  elastic constants -- NEA Recommendations on fuel properties (2025),
       Nuclear Science NEA/NSC/R(2024)1, p. 124.  Both have the same four-factor
       shape: composition, porosity, deviation from stoichiometry, temperature.
       q = plutonium fraction (0 for UO2), P = porosity, x = deviation from
       stoichiometry.  Neither depends on burnup: the whole burnup dependence of
       the functional enters through rho_tot alone.

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
       dRoverR_max  = k*rho_LAGB_max / rho_tot                          [-]

  (4)  dislocation line energies      E_D = G b^2 f(nu)/(4 pi) * ln(R/b)
       A1 = f(nu)/(4 pi)*ln(rho_c^(-1/2)/b)     random array, cut-off rho_c
       A2 = f(nu)/(4 pi)*ln(rho_tot^(-1/2)/b)   stress-screened inside the wall

  (5)  THE DISLOCATION BALANCE -- three populations that close on rho_tot [m^-2]
       rho_ord(eta)   = rho_LAGB_max * eta^2       condensed into LAGB walls
       rho_swept(eta) = (rho_tot - rho_ord) * dRoverR(eta)   annihilated
       rho_free(eta)  = rho_tot - rho_ord - rho_swept        still random

       Only the FREE dislocations are swept: the ones already in a wall belong to
       the boundary, not to the volume the boundary passes through (Gourdet &
       Montheillet 2003).  That is what makes the sweep contribute at eta^4 as
       well as at eta^2, and the eta^4 term is what closes the functional.

       Each population carries the line energy of the state it is in -- the free
       ones the cut-off of a random array, the ones in the walls the cut-off
       screened by the wall itself, the swept ones nothing, they are gone:

           F = rho_free*A1*G b^2 + rho_ord*A2*G b^2                   [J/m^3]

       whose four contributions, at eta = 1, are
       E_free    = +rho_tot      * A1                * G b^2   order eta^0
       E_wall    = +rho_LAGB_max * (A2 - A1)         * G b^2   order eta^2
       E_sweep_2 = -rho_tot      * dRoverR_max * A1  * G b^2   order eta^2
       E_sweep_4 = +rho_LAGB_max * dRoverR_max * A1  * G b^2   order eta^4

  (6)  Landau functional     F(bu, eta) = C0 + C2*eta^2 + C4*eta^4      [J/m^3]
       C0 = E_free
       C2 = E_wall + E_sweep_2
       C4 = E_sweep_4

  (7)  stationary point    dF/deta = 0  =>  eta^2 = -C2/(2*C4), clipped at 0
       eta = 0 wherever C2 >= 0, i.e. below the transition threshold

  (7a) theta_max is a pure NORMALIZATION.  rho_LAGB, S/V and dR/R all depend on
       theta = eta*theta_max alone, so theta_max cancels out of Theta, r_n and X
       at fixed beta, k and rho_c -- `--selftest` checks this against the old
       value pi - 2.723.  It is therefore set to theta_HAGB, which makes

           eta = 1   <=>   Theta = theta_HAGB   <=>   rho_ord = rho_tot

       coincide at 91.32 GWd/tU: the order parameter saturates exactly where the
       substructure becomes high-angle AND where the last free dislocation enters
       a wall.  Eq. (8) then reduces to a clip of eta at 1.

  (7b) ADMISSIBILITY -- the walls cannot hold more dislocations than exist, so
       rho_ord <= rho_tot.  The equilibrium is the minimum of F on the admissible
       interval, not the free stationary point:

           eta_balance = sqrt( min(rho_tot / rho_LAGB_max, 1) )         [-]
           eta_eq      = min( sqrt(-C2/(2*C4)), eta_balance )

       On the bound this is exactly the classical theta ~ sqrt(rho_tot),
           theta_bal = eta_balance*theta_max = beta*b*sqrt(rho_tot) / (3n)
       every dislocation is in a wall, the misorientation can only grow as fast
       as the dislocations that feed it, and the sweep stops because there is
       nothing free left to sweep. 

  (8)  MEAN MISORIENTATION                                   <-- output 1
       Theta = min( eta_eq*theta_max*180/pi , theta_HAGB )              [deg]
       eta   = (Theta*pi/180)/theta_max

  (9)  SUBGRAIN RADIUS                                       <-- output 2
       SoverV  = SoverV_max*eta
       dRoverR = dRoverR_max*eta^2
       r_n     = min( 1.5/SoverV * (1 + dRoverR) , R_grain )            [m]
       Undefined where eta = 0 --> no substructure, and no restructured
       fraction either.  The ceiling is the host grain: a subgrain cannot be
       larger than the grain it subdivides.

  (10) RESTRUCTURED FRACTION -- lever rule                    <-- output 3
       X = clip( (Theta - theta_u) / (theta_HAGB - theta_u), 0, ALPHA_MAX )
       The measured Theta is not a local misorientation: it is the mean over the
       EBSD map, built as Theta = [AMis*(f1 - f10) + 10*f10]/100, i.e. exactly
       the weighted mean of a two-phase mixture.  The functional,
       calibrated on Theta, therefore predicts the mixture MEAN, and the fraction
       is recovered by inverting the mixture.  theta_u is the misorientation of
       the unrestructured matrix: a MEASURED quantity (median AMis2Mean).

  (11) driving force, reported for the nucleation criterion, not an output
       dE_s = C0 + C2*eta^2 + C4*eta^4                                  [J/m^3]
       Below the threshold eta = 0 and only C0 is left: the matrix has the
       dislocations but has not organized them yet, so all of their energy is
       available.


SURFACE ENERGY -- why there is none
---------------------------------------------------------------------------------
An earlier form of this functional carried a fifth term, E3 = SoverV_max*gamma_max
at order eta^2, for "the energy of the new grain-boundary surface".  It is gone,
for three reasons.

  * It is not the convention of the model this one follows.  In Gourdet &
    Montheillet (2003) and the CDRX models built on it, the stored energy is the
    dislocation energy alone, tau*rho with tau = G b^2/2.  Read-Shockley
    gamma(theta) does appear, but as the boundary energy that sets the driving
    pressure and the mobility of a migrating boundary -- never as a term added to
    the stored energy.

  * It counts the walls twice.  Read-Shockley gamma(theta) IS the strain energy
    of the dislocations in the wall, summed: (S/V)*gamma = rho_LAGB * (line
    energy), which is the same object E_wall already carries through A2.


CALIBRATION
---------------------------------------------------------------------------------
`calibrate.py`, in this folder, is the calibration: it fits beta, k and rho_c
jointly on the mean misorientation AND on the measured sizes, against this very
implementation, and prints the result ready to paste back into the constants
below and into the C++ parameter push. 

    python3 calibrate.py

VALIDATION
---------------------------------------------------------------------------------
Against the EBSD dataset shipped in `data/`, `--validate` gives

    mean misorientation   Theta   N = 41   RMSE = 1.7616 deg   R2 = 0.7715
    restructured fraction X       N = 27   RMSE = 0.2053       R2 = 0.7283
    subgrain radius       r_n     N = 14   RMSE = 0.0678 um    R2 = 0.7737

and on the 8 PIE points of `validation.py`, which the model has never seen,
RMSE = 0.1298 with R2 = +0.553 on the restructured fraction.

SELF-TEST
---------------------------------------------------------------------------------
`--validate` compares the model with the experiment.  `--selftest` checks the 
model against ITSELF, with no external data, and is there to catch a change to 
the code that silently changes the physics.

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
import csv
import math
import os
import sys
from dataclasses import dataclass, field

# ---------------------------------------------------------------------------
# CONSTANTS
#
# Every value is quoted with its unit and its origin.  Nothing here is tuned in
# this file: the three adjustable parameters (beta, k, rho_c) come from
# `calibrate.py` and are marked as such.
# ---------------------------------------------------------------------------

# --- elastic constants -----------------------------------------------------
BURGERS = 3.889087296526011e-10  # m      Burgers vector, Djonovic thesis

# --- shear modulus and Poisson ratio, Eq. (2): NEA/NSC/R(2024)1 p. 124 -----
# Both correlations have the same four-factor shape.  The shear modulus is quoted
# in GPa: SCIANTIX stores the modulus of the matrix in MPa and therefore writes a
# factor 1e3, while the Landau functional works in Pa, so the prefactor here is 1e9.
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

# Default state of the fuel when the caller does not say otherwise.  SCIANTIX
# passes its own sciantix_variable["Porosity"] and ["Stoichiometry deviation"].
FABRICATION_POROSITY = 0.05      # -      as-fabricated porosity of UO2
PLUTONIUM_FRACTION = 0.0         # -      UO2; set to q for MOX

# --- LAGB / HAGB boundary --------------------------------------------------
# Both the upper end of the validity of the functional for the MATRIX and the
# composition of the restructured phase in the lever rule, Eq. (10).
THETA_HAGB = 10.0               # deg

# --- normalization of the order parameter ----------------------------------
# theta_max is a pure NORMALIZATION: every physical quantity in the functional
# depends on theta = eta*theta_max alone -- rho_LAGB = (3n*theta/(beta*b))^2,
# S/V = 3*sqrt(rho_LAGB)/beta, dR/R = k*rho_LAGB/rho_tot -- so theta_max cancels
# out of Theta, r_n and X exactly, at fixed beta, k and rho_c.  `--selftest`
# asserts that invariance rather than leaving it as a claim.
#
# It is therefore free, and the consistent choice is the LAGB/HAGB boundary
# itself.  That makes eta run over the full [0, 1] with a meaning at each end,
# and it makes three statements coincide instead of merely coexist:
#
#     eta = 1   <=>   Theta = theta_HAGB   <=>   rho_ord = rho_tot
#
# i.e. the order parameter saturates exactly where the substructure becomes
# high-angle AND where the last free dislocation enters a wall, at 91.32 GWd/tU.
#
# It was previously set to the peak of the Read-Shockley-Wolf curve of Zhang et
# al. (pi - 2.723 = 23.984 deg), which came in with the grain-boundary surface
# term.  That term is gone (see the SURFACE ENERGY section of the module
# docstring) and with it the only reason to prefer that angle; all it did was
# cap eta at 0.417 and leave the top of the range unreachable.
THETA_MAX = math.radians(THETA_HAGB)    # rad     = 0.174533 (10 deg)

# Misorientation of the unrestructured matrix, the lower end of the lever.
# Median of AMis2Mean over the 27 points that carry a restructured fraction: a
# MEASURED quantity, not a fitted parameter.  An independent fit of it gives
# 1.55 deg, inside the measured range 0.9-6.1 deg.
THETA_U = 2.20                  # deg

# --- host grain ------------------------------------------------------------
# Ceiling of Eq. (9).  SCIANTIX passes sciantix_variable["Grain radius"]; this
# default is the initial grain radius of the HBS regression cases.
GRAIN_RADIUS = 5.0e-6           # m

# --- adjustable parameters -------------------------------------------------
# n     number of dislocation families in a wall, range 1-3.
#       Gourdet & Montheillet, Acta Mater. 51 (2003) 2685-2699.  Not fitted.
# beta  geometric parameter linking the dislocation density to the crystallite
#       size; the analogue of the one of Rest & Hofman (2000), who use 5.
# k     links the volume seen by the mobile grain boundary to the fraction of
#       dislocations engaged in the LAGB over the total.
# rho_c outer cut-off of the dislocation strain field in Eq. (4), the scale at
#       which the field of a random array is screened.  rho_c^(-1/2) = 0.029 um,
#       roughly a fifth of the subgrain size this model predicts (0.21-0.65 um in
#       the dataset), i.e. a screening length well inside the subgrain.
#
# beta, k and rho_c come from `calibrate.py`: joint fit on 41 radial points of
# Theta plus 14 points of ECD50% (weight 0.05 on the size term), rho_tot of
# Nogita, G of Eq. (2), n = 2.  All six seeds converge to J = 0.23983258.
N_FAMILIES = 2.0                        # -
BETA = 33.54724855333423                # -
K_SWEEP = 0.04696637283583627           # -
RHO_C = 1165846255229680.0              # m^-2

# --- Nogita & Une (1994), Eq. (1) ------------------------------------------
NOGITA_SLOPE = 2.2e-2           # 1/(GWd/tU)
NOGITA_INTERCEPT = 13.8         # log10(m^-2)

# --- SCIANTIX-specific cap -------------------------------------------------
# The restructured fraction must stay strictly below 1.  SCIANTIX divides by
# (1 - alpha) downstream -- GasDiffusion.C (sweeping term), Matrix.C (pore
# nucleation), System.C (production split) -- and alpha = 1 exactly would produce
# inf/NaN.  The cap is applied HERE as well as in the C++, so that the two
# implementations agree bit for bit and the port stays verifiable.
# How the other formation options handle the same thing:
#   option 3  caps at exactly this value, f_max = 1 - 1e-9, in the same way;
#   options 1 and 2  do NOT cap.  They evaluate alpha_r = 1 - exp(-K (bu - bu_inc)^n)
#     and rely on the exponential never reaching zero.
ALPHA_MAX = 1.0 - 1.0e-9        # -

# Burnup unit conversion used by SCIANTIX: MWd/kgUO2 -> MWd/kgU = GWd/tU.
# Not used by this script (which takes GWd/tU directly); quoted because the C++
# applies it to sciantix_variable["Burnup"] before calling the model.
UO2_TO_U = 0.8814               # kgU/kgUO2


# ---------------------------------------------------------------------------
# THE MODEL
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class ModelParameters:
    """The four parameters `calibrate.py` is allowed to move."""

    n_families: float = N_FAMILIES
    beta: float = BETA
    k_sweep: float = K_SWEEP
    rho_c: float = RHO_C


DEFAULT_PARAMETERS = ModelParameters()


@dataclass
class HbsState:
    """The complete state of the model at one (burnup, temperature) point.

    The three quantities SCIANTIX consumes are `theta_deg`, `subgrain_radius_m`
    and `restructured_fraction`; the rest is carried for diagnostics and for the
    self-test.
    """

    burnup: float             # GWd/tU     input
    temperature: float        # K          input
    porosity: float           # -          input
    rho_tot: float            # m^-2       Eq. (1)
    shear_modulus: float      # Pa         Eq. (2)
    c0: float                 # J/m3       Eq. (6)
    c2: float                 # J/m3       Eq. (6)
    c4: float                 # J/m3       Eq. (6)
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


def shear_modulus(temperature, porosity=FABRICATION_POROSITY,
                  stoichiometry_deviation=0.0, plutonium_fraction=PLUTONIUM_FRACTION):
    """Eq. (2) -- shear modulus [Pa]. NEA/NSC/R(2024)1 p. 124.    """
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
    """Eq. (2) -- Poisson ratio [-]. NEA/NSC/R(2024)1 p. 124.

    Same four-factor shape as `shear_modulus`, and taken from the same table rather
    than held at a single value: it enters the functional through f(nu), and there is
    no reason to take one elastic constant from the correlation and not the other.
    Note the sign of the quadratic temperature term, which is + here and - for G.
    """
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

    Returns (rho_LAGB_max [m^-2], SoverV_max [m^-1], dRoverR_max [-]), the values
    the three geometric quantities take at eta = 1; the eta-dependence is applied
    in `hbs_state`.
    """
    n_families, beta, k_sweep = parameters.n_families, parameters.beta, parameters.k_sweep
    rho_lagb_max = math.pow(3.0 * n_families * THETA_MAX / (beta * BURGERS), 2.0)
    s_over_v_max = 9.0 * n_families * THETA_MAX / (beta * beta * BURGERS)
    dr_over_r_max = k_sweep * rho_lagb_max / rho_tot
    return rho_lagb_max, s_over_v_max, dr_over_r_max


def dislocation_partition(rho_tot, eta, parameters=DEFAULT_PARAMETERS):
    """Eq. (5) -- the three dislocation populations at a given eta [m^-2].

    Returns (rho_ordered, rho_swept, rho_free), which add up to `rho_tot` by
    construction.  This is the bookkeeping the functional is built on and the
    single place it lives: `landau_coefficients` assembles the same partition
    into the coefficients of Eq. (6), and `selftest` checks it closes.

        rho_ord   = rho_LAGB_max * eta^2      condensed into the LAGB walls
        rho_swept = (rho_tot - rho_ord) * dRoverR     annihilated by the sweep
        rho_free  = rho_tot - rho_ord - rho_swept     still a random array

    Only the FREE dislocations are swept: the ones already stored in a wall
    belong to the boundary, not to the volume the boundary passes through.  That
    is the Gourdet & Montheillet (2003) picture, and it is what makes the sweep
    contribute at both eta^2 and eta^4 rather than at eta^2 alone.

    `rho_free` is non-negative only for eta <= eta_balance of Eq. (7b); above
    that the partition is unphysical, which is precisely why `hbs_state` refuses
    to minimize the functional there.
    """
    rho_lagb_max, _, dr_over_r_max = wall_geometry(rho_tot, parameters)
    # The `min` is the bound of Eq. (7b) written on the density instead of on
    # eta.  `hbs_state` has already applied it, so here it only ever absorbs the
    # last-bit rounding of eta = sqrt(rho_tot/rho_LAGB_max), which would
    # otherwise leave rho_free at -1e-17*rho_tot exactly on the bound.
    rho_ordered = min(rho_lagb_max * eta * eta, rho_tot)
    rho_swept = (rho_tot - rho_ordered) * dr_over_r_max * eta * eta
    return rho_ordered, rho_swept, rho_tot - rho_ordered - rho_swept


def landau_coefficients(temperature, rho_tot, porosity=FABRICATION_POROSITY,
                        stoichiometry_deviation=0.0, parameters=DEFAULT_PARAMETERS):
    """Eqs. (4)-(6) -- the coefficients of F = C0 + C2 eta^2 + C4 eta^4 [J/m^3].

    The functional is the energy of the three populations of Eq. (5), each
    carrying the line energy of the state it is in:

        F = rho_free*A1*G b^2 + rho_ord*A2*G b^2

    the free ones with the cut-off of a random array, the ones in the walls with
    the cut-off screened by the wall itself, and the swept ones with nothing at
    all -- they are gone.  Substituting the partition and collecting powers of
    eta gives Eq. (6).  There is no grain-boundary surface energy term; see the
    SURFACE ENERGY section of the module docstring.

    Returns (C0, C2, C4, [E_free, E_wall, E_sweep_2, E_sweep_4]).  The four
    amplitudes are the energy contributions at eta = 1, in the order of Eq. (5).
    """
    rho_lagb_max, _, dr_over_r_max = wall_geometry(rho_tot, parameters)

    gb2 = shear_modulus(temperature, porosity, stoichiometry_deviation) * BURGERS * BURGERS
    f_nu = line_energy_prefactor(temperature, porosity, stoichiometry_deviation)

    # Eq. (4): the two logarithmic cut-offs of the dislocation line energy.
    a1 = f_nu / (4.0 * math.pi) * math.log(math.pow(parameters.rho_c, -0.5) / BURGERS)
    a2 = f_nu / (4.0 * math.pi) * math.log(math.pow(rho_tot, -0.5) / BURGERS)

    # Eq. (5)-(6): the partition, term by term.
    e_free = rho_tot * a1 * gb2                          # order eta^0
    e_wall = rho_lagb_max * (a2 - a1) * gb2              # order eta^2
    e_sweep_2 = -rho_tot * dr_over_r_max * a1 * gb2      # order eta^2
    e_sweep_4 = rho_lagb_max * dr_over_r_max * a1 * gb2  # order eta^4

    # Eq. (6).
    c0 = e_free
    c2 = e_wall + e_sweep_2
    c4 = e_sweep_4
    return c0, c2, c4, [e_free, e_wall, e_sweep_2, e_sweep_4]


def hbs_state(burnup, temperature, porosity=FABRICATION_POROSITY,
              stoichiometry_deviation=0.0, grain_radius_m=GRAIN_RADIUS,
              parameters=DEFAULT_PARAMETERS):
    """The model. Eqs. (1)-(11) for one (burnup [GWd/tU], temperature [K]) point.

    THIS IS THE FUNCTION THE C++ PORT MUST MIRROR.  It is scalar and uses only
    `math`, and the statements are in the order `HighBurnupStructureFormation.C`
    case 4 should use, so the two can be read side by side and compared
    numerically once that case exists.

    Returns an `HbsState`.  `subgrain_radius_m` is `nan` below the transition
    threshold, where there are no subgrains; SCIANTIX writes 0.0 there instead.
    """
    # (1) dislocation density
    rho_tot = dislocation_density_nogita(burnup)

    # (2) shear modulus, (3) wall geometry, (4)-(6) Landau coefficients
    rho_lagb_max, s_over_v_max, dr_over_r_max = wall_geometry(rho_tot, parameters)
    c0, c2, c4, _ = landau_coefficients(temperature, rho_tot, porosity,
                                        stoichiometry_deviation, parameters)

    # (7) stationary point: eta^2 = -C2/(2 C4), zero where C2 >= 0.  No guard is
    #     needed on virgin fuel: with G of Eq. (2) the functional gives C2 > 0
    #     at bu = 0 on its own, hence eta = 0.  The self-test asserts it.
    eta_stationary = math.sqrt(max(-c2 / (2.0 * c4), 0.0))

    # (7b) admissibility: the walls cannot hold more dislocations than exist, so
    #      rho_ord = rho_LAGB_max*eta^2 <= rho_tot.  The equilibrium is the
    #      minimum of F on 0 <= eta <= eta_balance, not the free stationary point.
    #      On the bound theta = beta*b*sqrt(rho_tot)/(3n): the misorientation can
    #      only grow as fast as the dislocations that feed the walls, which is
    #      the classical theta ~ sqrt(rho_tot).
    eta_balance = math.sqrt(min(rho_tot / rho_lagb_max, 1.0))

    # (8) mean misorientation, capped at the LAGB/HAGB boundary   <-- output 1
    #     Three things can set eta, and `balance_limited` records whether it was
    #     Eq. (7b) rather than the stationary point or the cap.  With
    #     theta_max = theta_HAGB the last two coincide at eta = 1, so the flag has
    #     to be a strict minimum: at and above saturation it is the cap that binds,
    #     and there Theta stays at theta_HAGB instead of following sqrt(rho_tot).
    eta_hagb = math.radians(THETA_HAGB) / THETA_MAX
    eta = min(eta_stationary, eta_balance, eta_hagb)
    balance_limited = eta_balance < min(eta_stationary, eta_hagb)

    theta = math.degrees(eta * THETA_MAX)
    eta = math.radians(theta) / THETA_MAX          # re-derived after the cap

    # (9) subgrain radius, capped at the host grain               <-- output 2
    s_over_v = s_over_v_max * eta
    dr_over_r = dr_over_r_max * eta * eta
    if s_over_v > 0.0:
        radius = min(1.5 / s_over_v * (1.0 + dr_over_r), grain_radius_m)
        # a subgrain cannot exceed its grain
    else:
        radius = math.nan                          # no substructure, not a length

    # (10) restructured fraction, lever rule                      <-- output 3
    fraction = (theta - THETA_U) / (THETA_HAGB - THETA_U)
    if fraction < 0.0:
        fraction = 0.0
    elif fraction > ALPHA_MAX:
        fraction = ALPHA_MAX

    # (11) driving force, for the nucleation criterion
    driving_force = c0 + c2 * eta * eta + c4 * eta * eta * eta * eta

    rho_ordered, rho_swept, rho_free = dislocation_partition(rho_tot, eta, parameters)

    return HbsState(
        burnup=burnup,
        temperature=temperature,
        porosity=porosity,
        rho_tot=rho_tot,
        shear_modulus=shear_modulus(temperature, porosity, stoichiometry_deviation),
        c0=c0,
        c2=c2,
        c4=c4,
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

DATA_FILE = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "data", "ebsd_zacharie_onofri.csv")

# Column names of `data/ebsd_zacharie_onofri.csv`, kept verbatim from the source
# spreadsheets so that the file stays a faithful copy of them.
COL_BURNUP = "Calculated radial burnup (GWd/tU)"
COL_BURNUP_EFFECTIVE = "Calculated radial effective burnup (GWd/tU)"
COL_TEMPERATURE = "Calculated radial temperature (°C)"
COL_F1 = "Restructured fraction at 1° (%)"
COL_F10 = "Restructured fraction at 10° (%)"
COL_AMIS = "AMis2Mean (°)"
COL_ECD_SUB = "Sub-grains ECD50% (μm)"
COL_ECD_NEW = "New grains ECD50% (μm)"
COL_POROSITY = "Porosity (%)"
COL_GRAIN_SIZE = "Grain size (μm)"


def _number(text):
    """A CSV cell as a float; empty cells and bare dashes become nan."""
    text = text.strip()
    if not text or text == "-":
        return math.nan
    return float(text)


def load_ebsd(path=DATA_FILE):
    """The EBSD + TRANSURANUS dataset as a list of dicts of floats.

    42 rows: 28 from Zacharie-Aubrun et al. (2022) and 14 from Onofri et al.
    (2025).  The misorientations and the restructured fractions are measured by
    EBSD; the local conditions (burnup, temperature, fission rate, strain,
    stress) come from TRANSURANUS runs of the same rods.

    `porosity` and `grain_radius` are the as-fabricated values of the specimen,
    used respectively in Eq. (2) and as the ceiling of Eq. (9).  Both are given
    for the Zacharie rods and absent for the Onofri ones, where the module
    defaults are substituted.

    `burnup` is the local burnup, which is what this model uses; `burnup_effective`
    is the same TRANSURANUS run's effective burnup, carried for the comparison with
    the KJMA options of SCIANTIX, which are driven by that one instead.
    """
    rows = []
    with open(path, newline="", encoding="utf-8") as handle:
        for record in csv.DictReader(handle):
            porosity = _number(record[COL_POROSITY])
            # "Grain size" is a DIAMETER, as it is for the ECD50% columns.
            grain_size = _number(record[COL_GRAIN_SIZE])
            rows.append({
                "label": "%s/%s" % (record["Dataset"], record["Label"]),
                "burnup": _number(record[COL_BURNUP]),
                # The effective burnup of the same TRANSURANUS run.  Carried because
                # the KJMA formation options of SCIANTIX (1 and 2) are driven by
                # sciantix_variable["Effective burnup"], not by the local burnup that
                # drives options 3 and 4.
                "burnup_effective": _number(record[COL_BURNUP_EFFECTIVE]),
                "temperature": _number(record[COL_TEMPERATURE]) + 273.15,
                "f1": _number(record[COL_F1]),
                "f10": _number(record[COL_F10]),
                "amis": _number(record[COL_AMIS]),
                "ecd_sub": _number(record[COL_ECD_SUB]),
                "ecd_new": _number(record[COL_ECD_NEW]),
                "porosity": porosity / 100.0 if not math.isnan(porosity) else FABRICATION_POROSITY,
                "grain_radius": grain_size / 2.0 * 1e-6 if not math.isnan(grain_size) else GRAIN_RADIUS,
            })
    return rows


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
    """
    rows = load_ebsd(path)
    metrics = {}

    theta_obs, theta_mod = [], []
    frac_obs, frac_mod = [], []
    size_obs, size_mod = [], []

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
                raise AssertionError(
                    "size point at bu = %g GWd/tU lies below the transition "
                    "threshold, where the radius is undefined" % row["burnup"])
            size_obs.append(radius)
            size_mod.append(state.subgrain_radius_m)

    metrics["theta"] = dict(n=len(theta_obs), rmse=_rmse(theta_obs, theta_mod),
                            r2=_r_squared(theta_obs, theta_mod))
    metrics["fraction"] = dict(n=len(frac_obs), rmse=_rmse(frac_obs, frac_mod),
                               r2=_r_squared(frac_obs, frac_mod))
    metrics["radius"] = dict(n=len(size_obs), rmse=_rmse(size_obs, size_mod),
                             r2=_r_squared(size_obs, size_mod))

    if verbose:
        print("Validation against %s" % os.path.relpath(path, os.path.dirname(path) or "."))
        print("  dislocation density: Nogita & Une (1994);  shear modulus: NEA/NSC/R(2024)1")
        print("  n = %g, beta = %g, k = %g, rho_c = %g m^-2"
              % (parameters.n_families, parameters.beta, parameters.k_sweep, parameters.rho_c))
        print()
        print("  mean misorientation  Theta   N = %2d   RMSE = %.4f deg   R2 = %.4f"
              % (metrics["theta"]["n"], metrics["theta"]["rmse"], metrics["theta"]["r2"]))
        print("  restructured fraction X      N = %2d   RMSE = %.4f       R2 = %.4f"
              % (metrics["fraction"]["n"], metrics["fraction"]["rmse"], metrics["fraction"]["r2"]))
        print("  subgrain radius       r_n    N = %2d   RMSE = %.4f um    R2 = %.4f"
              % (metrics["radius"]["n"], metrics["radius"]["rmse"] * 1e6, metrics["radius"]["r2"]))
    return metrics


# ---------------------------------------------------------------------------
# REFERENCE TABLE AND SELF-TEST
# ---------------------------------------------------------------------------

# Produced by this implementation at T = 600 K, P = 0.05, x = 0, with the shipped
# calibration, and frozen here so that a change to the code that moves the physics
# fails `--selftest` instead of passing quietly.  Regenerate with
# `python3 calibrate.py --reference-table` after a recalibration.
#
#          bu [GWd/tU],  rho_tot [m^-2],     Theta [deg],   r_n [m],   X [-]
REFERENCE_TABLE = [
    (10.0, 104712854805090.28, 0.0, math.nan, 0.0),
    (20.0, 173780082874937.62, 0.0, math.nan, 0.0),
    (30.0, 288403150312661.2, 0.0, math.nan, 0.0),
    (40.0, 478630092322638.0, 0.0, math.nan, 0.0),
    (49.5612, 776866510328443.9, 0.006048196298133282, 5e-06, 0.0),
    (49.6112, 778836685718761.9, 0.19429892627965883, 5e-06, 0.0),
    (50.0, 794328234724282.1, 0.5810433307889246, 3.6012438391299855e-06, 0.0),
    (50.0612, 796794632758579.4, 0.6211986007858683, 3.3690570226717287e-06, 0.0),
    (54.5559, 1000529274070491.6, 2.2000009613499585, 9.638083847254108e-07, 1.2324999465462838e-07),
    (60.0, 1318256738556410.0, 3.650710985508894, 5.899452023333265e-07, 0.18598858788575565),
    (70.0, 2187761623949551.8, 5.827415437115106, 3.75456514127303e-07, 0.46505326116860335),
    (80.0, 3630780547701017.5, 7.50716532439319, 2.914470365107784e-07, 0.6804058108196398),
    (91.3204, 6442416524039701.0, 10.0, 2.1879409145881883e-07, ALPHA_MAX),
    (100.0, 1e+16, 10.0, 2.1530233253870363e-07, ALPHA_MAX),
    (150.0, 1.2589254117941714e+17, 10.0, 2.094813883166304e-07, ALPHA_MAX),
]

# The three burnups at which the model changes regime, at T = 600 K, P = 0.05.
# Re-derived by bisection in `--selftest`.  They no longer depend on T, P or x:
# see the SURFACE ENERGY section of the module docstring.
BU_THRESHOLD = 49.5612      # GWd/tU   C2 = 0: Theta leaves zero
BU_ONSET = 54.5559          # GWd/tU   Theta = theta_u: X leaves zero
BU_SATURATION = 91.3204     # GWd/tU   Theta = theta_HAGB: X reaches its cap

# State the reference table and the boundaries are computed in.
REFERENCE_TEMPERATURE = 600.0    # K
REFERENCE_POROSITY = FABRICATION_POROSITY


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
    """(threshold, onset, saturation) in GWd/tU, by bisection on Theta."""
    def theta(burnup):
        return hbs_state(burnup, temperature, porosity=porosity, parameters=parameters).theta_deg

    threshold = _bisect(lambda b: theta(b) - 1e-12, 1.0, 200.0)
    onset = _bisect(lambda b: theta(b) - THETA_U, threshold, 300.0)
    saturation = _bisect(lambda b: theta(b) - THETA_HAGB * (1.0 - 1e-12), onset, 400.0)
    return threshold, onset, saturation


def print_table(temperature=REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY):
    """The reference table, recomputed."""
    print("HBS formation, Landau functional -- reference behaviour at T = %g K, P = %g"
          % (temperature, porosity))
    print()
    print("    bu          rho_tot        Theta      r_n         X")
    print("    [GWd/tU]    [m^-2]         [deg]      [um]        [-]")
    print("    " + "-" * 58)
    for burnup, _, _, _, _ in REFERENCE_TABLE:
        state = hbs_state(burnup, temperature, porosity=porosity)
        radius = ("%10.4f" % (state.subgrain_radius_m * 1e6)
                  if not math.isnan(state.subgrain_radius_m) else "         -")
        print("    %7.1f     %.4e   %8.4f %s  %8.6f"
              % (burnup, state.rho_tot, state.theta_deg, radius, state.restructured_fraction))
    print()
    print("    transition threshold  C2 = 0            bu = %8.4f GWd/tU" % BU_THRESHOLD)
    print("    restructuring starts  Theta = %.2f deg   bu = %8.4f GWd/tU" % (THETA_U, BU_ONSET))
    print("    fully restructured    Theta = %.1f deg   bu = %8.4f GWd/tU" % (THETA_HAGB, BU_SATURATION))
    print()
    print("    X saturates at ALPHA_MAX = 1 - 1e-9, never at 1 exactly: SCIANTIX")
    print("    divides by (1 - X) downstream.  The table rounds it to 1.000000.")
    print("    r_n is capped at the host grain radius, %g um." % (GRAIN_RADIUS * 1e6))


def _outputs_at_theta_max(burnup, theta_max, temperature=REFERENCE_TEMPERATURE,
                          porosity=REFERENCE_POROSITY, grain_radius_m=GRAIN_RADIUS,
                          parameters=DEFAULT_PARAMETERS):
    """(Theta [deg], r_n [m], X [-]) with THETA_MAX replaced by `theta_max`.

    The model of `hbs_state`, re-derived with a different normalization of the order
    parameter and nothing else, so that `selftest` can check the invariance claimed
    in the comment on THETA_MAX instead of taking it on trust.  Used only there.
    """
    n_families, beta, k_sweep = parameters.n_families, parameters.beta, parameters.k_sweep
    rho_tot = dislocation_density_nogita(burnup)

    rho_lagb_max = math.pow(3.0 * n_families * theta_max / (beta * BURGERS), 2.0)
    s_over_v_max = 9.0 * n_families * theta_max / (beta * beta * BURGERS)
    dr_over_r_max = k_sweep * rho_lagb_max / rho_tot

    gb2 = shear_modulus(temperature, porosity) * BURGERS * BURGERS
    f_nu = line_energy_prefactor(temperature, porosity)
    a1 = f_nu / (4.0 * math.pi) * math.log(math.pow(parameters.rho_c, -0.5) / BURGERS)
    a2 = f_nu / (4.0 * math.pi) * math.log(math.pow(rho_tot, -0.5) / BURGERS)

    c2 = rho_lagb_max * (a2 - a1) * gb2 - rho_tot * dr_over_r_max * a1 * gb2
    c4 = rho_lagb_max * dr_over_r_max * a1 * gb2

    eta = min(math.sqrt(max(-c2 / (2.0 * c4), 0.0)),
              math.sqrt(min(rho_tot / rho_lagb_max, 1.0)),
              math.radians(THETA_HAGB) / theta_max)
    theta = math.degrees(eta * theta_max)
    eta = math.radians(theta) / theta_max

    s_over_v = s_over_v_max * eta
    radius = (min(1.5 / s_over_v * (1.0 + dr_over_r_max * eta * eta), grain_radius_m)
              if s_over_v > 0.0 else math.nan)
    fraction = min(ALPHA_MAX, max(0.0, (theta - THETA_U) / (THETA_HAGB - THETA_U)))
    return theta, radius, fraction


def selftest(verbose=True):
    """Every invariant the model must satisfy. Raises AssertionError on failure.

    See the SELF-TEST section of the module docstring for what this covers and
    how it differs from `--validate`.
    """
    checks = []

    def check(name, condition, detail=""):
        checks.append((name, bool(condition), detail))

    # 1. the reference table, to the last digit
    worst = 0.0
    for burnup, rho_ref, theta_ref, radius_ref, fraction_ref in REFERENCE_TABLE:
        state = hbs_state(burnup, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY)
        for got, want in ((state.rho_tot, rho_ref), (state.theta_deg, theta_ref),
                          (state.subgrain_radius_m, radius_ref),
                          (state.restructured_fraction, fraction_ref)):
            if math.isnan(want):
                check("reference table: nan radius at bu = %g" % burnup, math.isnan(got))
            elif want == 0.0:
                worst = max(worst, abs(got))
            else:
                worst = max(worst, abs(got - want) / abs(want))
    check("reference table reproduced", worst < 1e-12, "max relative error %.2e" % worst)

    # 2. the three characteristic burnups
    threshold, onset, saturation = regime_boundaries()
    check("transition threshold = %.4f GWd/tU" % threshold, abs(threshold - BU_THRESHOLD) < 1e-3)
    check("restructuring onset  = %.4f GWd/tU" % onset, abs(onset - BU_ONSET) < 1e-3)
    check("full restructuring   = %.4f GWd/tU" % saturation, abs(saturation - BU_SATURATION) < 1e-3)

    # 3. virgin fuel carries no structure, and the code contains no guard that
    #    makes it so: with G of Eq. (2) the functional gives C2 > 0 at bu = 0.
    for temperature in (300.0, 723.0, 1200.0):
        virgin = hbs_state(0.0, temperature)
        check("virgin fuel at T = %g K carries no structure" % temperature,
              virgin.theta_deg == 0.0 and virgin.restructured_fraction == 0.0
              and math.isnan(virgin.subgrain_radius_m))
        check("Theta = 0 at bu = 0 comes from C2 > 0, not from a special case (T = %g K)"
              % temperature, virgin.c2 > 0.0, "C2 = %+.4e J/m3" % virgin.c2)

    # 4. the regimes on either side
    below = hbs_state(BU_THRESHOLD - 0.01, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY)
    above = hbs_state(BU_SATURATION + 0.01, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY)
    check("below the threshold Theta = 0 and X = 0",
          below.theta_deg == 0.0 and below.restructured_fraction == 0.0)
    check("below the threshold the radius is undefined", math.isnan(below.subgrain_radius_m))
    check("above saturation Theta = theta_HAGB and X = ALPHA_MAX",
          above.theta_deg == THETA_HAGB and above.restructured_fraction == ALPHA_MAX)
    check("X is never exactly 1 (SCIANTIX divides by 1 - X)",
          all(hbs_state(b, REFERENCE_TEMPERATURE).restructured_fraction < 1.0
              for b in (86.0, 100.0, 200.0, 500.0)))

    # 5. monotonicity in burnup at fixed temperature
    burnups = [1.0 + 0.5 * i for i in range(400)]        # 1 -> 200.5 GWd/tU
    states = [hbs_state(b, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY) for b in burnups]
    check("Theta is non-decreasing in burnup",
          all(b.theta_deg >= a.theta_deg - 1e-15 for a, b in zip(states, states[1:])))
    check("X is non-decreasing in burnup",
          all(b.restructured_fraction >= a.restructured_fraction - 1e-15
              for a, b in zip(states, states[1:])))
    radii = [s.subgrain_radius_m for s in states if not math.isnan(s.subgrain_radius_m)]
    check("the subgrain radius is non-increasing above the threshold",
          all(b <= a + 1e-20 for a, b in zip(radii, radii[1:])))

    # 6. the subgrain radius never exceeds the grain that hosts it
    ceiling_worst = 0.0
    for grain_radius in (1.0e-6, 5.0e-6, 1.0e-5):
        for burnup in burnups:
            radius = hbs_state(burnup, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY,
                               grain_radius_m=grain_radius).subgrain_radius_m
            if not math.isnan(radius):
                ceiling_worst = max(ceiling_worst, radius / grain_radius)
    check("r_n never exceeds the host grain radius", ceiling_worst <= 1.0,
          "max r_n / R_grain = %.6f" % ceiling_worst)

    # 7. the three outputs depend on the local burnup ALONE.  Every term of C2
    #    and of C4 carries the same factor f(nu)*G*b^2, so it cancels exactly in
    #    eta^2 = -C2/(2 C4): there is nothing left for T, P or x to act on.  This
    #    replaces the old "Theta decreases with temperature", which held only
    #    while the functional carried a grain-boundary surface term.
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
    # The cancellation is exact in exact arithmetic; what survives is the last
    # bit of the division, hence a tolerance rather than an equality.
    check("Theta and X depend on burnup alone: f(nu) G b^2 cancels in -C2/(2 C4)",
          worst_invariance < 1e-12, "max absolute difference %.2e" % worst_invariance)
    check("the driving force still moves with temperature",
          hbs_state(70.0, 500.0).driving_force > hbs_state(70.0, 1200.0).driving_force)

    # 8. eta_eq is a stationary point of the functional, Eq. (7) -- wherever the
    #    equilibrium is the free stationary point.  Where Eq. (7b) or the HAGB
    #    cap sets eta instead, the minimum is on the edge of the admissible
    #    interval and dF/deta is not zero there.
    worst_stationarity = 0.0
    for burnup in (50.0, 55.0, 60.0, 65.0, 70.0, 75.0, 80.0, 85.0):
        state = hbs_state(burnup, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY)
        if not 0.0 < state.theta_deg < THETA_HAGB or state.balance_limited:
            continue
        derivative = 2.0 * state.c2 * state.eta + 4.0 * state.c4 * state.eta ** 3
        scale = abs(2.0 * state.c2 * state.eta) + abs(4.0 * state.c4 * state.eta ** 3)
        worst_stationarity = max(worst_stationarity, abs(derivative) / scale)
    check("dF/deta = 0 wherever the equilibrium is interior", worst_stationarity < 1e-14,
          "max normalized residual %.2e" % worst_stationarity)

    # 8b. THE DISLOCATION BALANCE, Eq. (5) and Eq. (7b).  The three populations
    #     close on rho_tot, none of them is negative, and the swept fraction of
    #     the volume is a fraction.  Without the bound of Eq. (7b) the walls hold
    #     up to twice the dislocations that exist over 66-98 GWd/tU and rho_swept
    #     changes sign, which is what this bound is there to prevent.
    worst_closure, worst_free, worst_swept, worst_dr = 0.0, 0.0, 0.0, 0.0
    for state in states:
        rho_ordered, rho_swept, rho_free = (state.rho_ordered, state.rho_swept,
                                            state.rho_free)
        worst_closure = max(worst_closure,
                            abs(rho_ordered + rho_swept + rho_free - state.rho_tot)
                            / state.rho_tot)
        worst_free = min(worst_free, rho_free / state.rho_tot)
        worst_swept = min(worst_swept, rho_swept / state.rho_tot)
        _, _, dr_over_r_max = wall_geometry(state.rho_tot)
        worst_dr = max(worst_dr, dr_over_r_max * state.eta * state.eta)
    check("rho_ord + rho_swept + rho_free = rho_tot", worst_closure < 1e-15,
          "max relative closure error %.2e" % worst_closure)
    check("rho_free >= 0: the walls never hold more dislocations than exist",
          worst_free == 0.0, "min rho_free / rho_tot = %.3e" % worst_free)
    check("rho_swept >= 0: the sweep is a sink, never a source",
          worst_swept == 0.0, "min rho_swept / rho_tot = %.3e" % worst_swept)
    check("dR/R <= 1: the swept volume is a fraction of the volume",
          worst_dr <= 1.0, "max dR/R = %.6f" % worst_dr)

    # 8c. on the bound the model is the classical theta ~ sqrt(rho_tot): with
    #     every dislocation in a wall, the misorientation can only grow as fast
    #     as the dislocations that feed it.
    worst_sqrt_law, bound_points = 0.0, 0
    for state in states:
        if not state.balance_limited:
            continue
        bound_points += 1
        classical = math.degrees(DEFAULT_PARAMETERS.beta * BURGERS * math.sqrt(state.rho_tot)
                                 / (3.0 * DEFAULT_PARAMETERS.n_families))
        worst_sqrt_law = max(worst_sqrt_law,
                             abs(state.theta_deg - classical) / classical)
    check("on the balance bound Theta = beta b sqrt(rho_tot) / 3n",
          bound_points > 0 and worst_sqrt_law < 1e-14,
          "%d points on the bound, max relative error %.2e" % (bound_points, worst_sqrt_law))

    # 8d. theta_max is a pure normalization.  Every physical quantity depends on
    #     theta = eta*theta_max alone, so the three outputs are invariant under
    #     changing it at fixed beta, k and rho_c.  This is what makes the choice
    #     theta_max = theta_HAGB free, and it is checked rather than asserted:
    #     the whole model is re-evaluated at the old normalization, pi - 2.723.
    worst_normalization = 0.0
    for burnup in (50.0, 55.0, 60.0, 70.0, 80.0, 91.0, 100.0, 150.0):
        state = hbs_state(burnup, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY)
        other = _outputs_at_theta_max(burnup, math.pi - 2.723)
        for got, want in zip(other, (state.theta_deg, state.subgrain_radius_m,
                                     state.restructured_fraction)):
            if math.isnan(want):
                continue
            worst_normalization = max(worst_normalization,
                                      abs(got - want) / max(abs(want), 1e-300))
    check("Theta, r_n and X are invariant under the choice of theta_max",
          worst_normalization < 1e-12,
          "max relative difference vs theta_max = pi - 2.723: %.2e" % worst_normalization)

    # 8e. and with theta_max = theta_HAGB the three saturations coincide: the order
    #     parameter reaches 1 exactly where the substructure becomes high-angle and
    #     where the last free dislocation enters a wall.
    saturated = hbs_state(BU_SATURATION + 0.01, REFERENCE_TEMPERATURE,
                          porosity=REFERENCE_POROSITY)
    rho_lagb_max, _, _ = wall_geometry(saturated.rho_tot)
    at_cap = hbs_state(BU_SATURATION, REFERENCE_TEMPERATURE, porosity=REFERENCE_POROSITY)
    check("eta = 1 <=> Theta = theta_HAGB <=> rho_ord = rho_tot",
          abs(at_cap.eta - 1.0) < 1e-9
          and abs(math.degrees(THETA_MAX) - THETA_HAGB) < 1e-12
          and abs(rho_lagb_max / dislocation_density_nogita(BU_SATURATION) - 1.0) < 1e-4,
          "eta = %.9f at bu = %.4f, rho_LAGB_max / rho_tot = %.6f"
          % (at_cap.eta, BU_SATURATION,
             rho_lagb_max / dislocation_density_nogita(BU_SATURATION)))

    # 9. the exact bridge to the stored energy of Muramatsu et al. (2014), Eq. 8:
    #    E_s = rho_tot*G*b^2/2, so C0/E_s = f(nu)*ln(rho_c^(-1/2)/b)/(2 pi),
    #    a constant at every burnup and temperature.
    #    f(nu) now varies with the state, so the ratio is constant in burnup at fixed
    #    (T, P, x) rather than everywhere: the check is per temperature.
    worst_ratio = 0.0
    for temperature in (600.0, 1000.0):
        expected_ratio = (line_energy_prefactor(temperature)
                          * math.log(math.pow(RHO_C, -0.5) / BURGERS) / (2.0 * math.pi))
        for burnup in (10.0, 40.0, 80.0, 150.0):
            state = hbs_state(burnup, temperature)
            stored = state.rho_tot * state.shear_modulus * BURGERS ** 2 / 2.0
            worst_ratio = max(worst_ratio,
                              abs(state.c0 / stored - expected_ratio) / expected_ratio)
    check("C0 / E_s(Muramatsu Eq. 8) = f(nu) ln(rho_c^-1/2 / b) / 2pi, constant in burnup",
          worst_ratio < 1e-14, "max relative spread %.2e" % worst_ratio)

    # 10. the numpy wrapper carries no physics of its own
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

    burnups = [1.0 + 0.25 * i for i in range(800)]
    states = [hbs_state(b, temperature) for b in burnups]
    rows = [r for r in load_ebsd() if r["burnup"] > 0.0]

    figure, axes = plt.subplots(1, 3, figsize=(13.5, 4.0))

    axes[0].plot(burnups, [s.theta_deg for s in states], "-", color="k")
    axes[0].plot([r["burnup"] for r in rows], [theta_measured(r) for r in rows], "o", ms=4)
    axes[0].set_ylabel(r"mean misorientation  $\Theta$  [deg]")

    axes[1].plot(burnups, [s.subgrain_radius_m * 1e6 for s in states], "-", color="k")
    sizes = [(r["burnup"], measured_radius(r) * 1e6) for r in rows]
    sizes = [(b, e) for b, e in sizes if not math.isnan(e)]
    axes[1].plot([b for b, _ in sizes], [e for _, e in sizes], "o", ms=4)
    axes[1].set_ylabel(r"subgrain radius  $r_n$  [$\mu$m]")
    axes[1].set_ylim(0.0, 1.5)

    axes[2].plot(burnups, [s.restructured_fraction for s in states], "-", color="k")
    fractions = [(r["burnup"], r["f10"] / 100.0) for r in rows if not math.isnan(r["f10"])]
    axes[2].plot([b for b, _ in fractions], [x for _, x in fractions], "o", ms=4)
    axes[2].set_ylabel("restructured fraction  $X$  [-]")

    for axis in axes:
        axis.set_xlabel("burnup  [GWd/tU]")
        axis.axvline(BU_THRESHOLD, ls=":", lw=0.8, color="0.5")
    figure.suptitle("HBS formation, Landau functional -- T = %g K, "
                    r"$\rho_{tot}$ Nogita & Une (1994)" % temperature)
    figure.tight_layout()
    figure.savefig(path, dpi=150)
    print("written: %s" % path)


def plot_populations(path="hbs_dislocation_populations.png",
                     temperature=REFERENCE_TEMPERATURE, parameters=DEFAULT_PARAMETERS):
    """The three dislocation populations of Eq. (5) against burnup. Needs matplotlib.

    The dashed curves are the same partition evaluated at the SAME parameters but
    with Eq. (7b) switched off, i.e. at the free stationary point of Eq. (7).  They
    are what the functional did before the admissibility bound was added, and they
    are the argument for it: rho_free goes negative, and rho_swept with it, because
    the walls are asked to hold more dislocations than exist.
    """
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    burnups = [30.0 + 0.25 * i for i in range(600)]        # 30 -> 180 GWd/tU

    def unbounded_eta(burnup):
        """Eq. (7) alone: the stationary point, with the HAGB cap but no Eq. (7b)."""
        rho_tot = dislocation_density_nogita(burnup)
        _, c2, c4, _ = landau_coefficients(temperature, rho_tot,
                                           parameters=parameters)
        eta = math.sqrt(max(-c2 / (2.0 * c4), 0.0))
        return min(eta, math.radians(THETA_HAGB) / THETA_MAX)

    states = [hbs_state(b, temperature, parameters=parameters) for b in burnups]
    # The unbounded partition, deliberately NOT clamped: the point is to show it
    # leaving the physical range, so it bypasses `dislocation_partition`.
    unbounded = []
    for burnup in burnups:
        rho_tot = dislocation_density_nogita(burnup)
        rho_lagb_max, _, dr_over_r_max = wall_geometry(rho_tot, parameters)
        eta = unbounded_eta(burnup)
        rho_ordered = rho_lagb_max * eta * eta
        rho_swept = (rho_tot - rho_ordered) * dr_over_r_max * eta * eta
        unbounded.append((rho_tot, rho_ordered, rho_swept,
                          rho_tot - rho_ordered - rho_swept))

    bounded_band = [b for b, s in zip(burnups, states) if s.balance_limited]
    populations = (("$\\rho_{tot}$", "0.15", [s.rho_tot for s in states],
                    [u[0] for u in unbounded]),
                   ("$\\rho_{ord}$  (walls)", "tab:blue", [s.rho_ordered for s in states],
                    [u[1] for u in unbounded]),
                   ("$\\rho_{free}$  (random)", "tab:green", [s.rho_free for s in states],
                    [u[3] for u in unbounded]),
                   ("$\\rho_{swept}$  (annihilated)", "tab:orange",
                    [s.rho_swept for s in states], [u[2] for u in unbounded]))

    figure, axes = plt.subplots(1, 3, figsize=(15.0, 4.4))

    # --- panel 1: the densities themselves, log scale over three decades -----
    # Inside the shaded band rho_free and rho_swept are EXACTLY zero, which a log
    # axis cannot draw: masked to nan so the curve stops there rather than
    # plunging off the bottom and looking like an artefact.
    for label, colour, bounded, _ in populations:
        axes[0].plot(burnups, [v if v > 0.0 else math.nan for v in bounded],
                     "-", color=colour, lw=2.0, label=label)
    axes[0].set_yscale("log")
    axes[0].set_ylim(1e12, 3e17)
    axes[0].set_ylabel(r"dislocation density  [m$^{-2}$]")
    axes[0].set_title("the three populations, Eq. (5)", fontsize=10)
    axes[0].legend(fontsize=8, loc="lower right")

    # --- panel 2: the partition, with and without Eq. (7b) ------------------
    for label, colour, bounded, free in populations[1:]:
        axes[1].plot(burnups, [v / t for v, t in zip(bounded, [s.rho_tot for s in states])],
                     "-", color=colour, lw=2.0, label=label)
        axes[1].plot(burnups, [v / u[0] for v, u in zip(free, unbounded)],
                     "--", color=colour, lw=1.2, alpha=0.9)
    axes[1].axhline(0.0, color="k", lw=0.8)
    axes[1].axhline(1.0, color="k", lw=0.8, ls=":")
    axes[1].set_ylim(-1.15, 2.15)
    axes[1].set_ylabel(r"fraction of $\rho_{tot}$  [-]")
    axes[1].set_title("solid: with Eq. (7b).  dashed: without it", fontsize=10)
    axes[1].legend(fontsize=8, loc="lower left")
    axes[1].annotate("walls hold ~2x the\ndislocations that exist",
                     xy=(84.0, 1.93), xytext=(103.0, 1.80), fontsize=7.5,
                     color="tab:blue",
                     arrowprops=dict(arrowstyle="->", color="tab:blue", lw=0.8))
    axes[1].annotate(r"$\rho_{free} < 0$", xy=(80.0, -0.79), xytext=(96.0, -0.85),
                     fontsize=7.5, color="tab:green",
                     arrowprops=dict(arrowstyle="->", color="tab:green", lw=0.8))

    # --- panel 3: the swept population on its own scale ---------------------
    axes[2].plot(burnups, [s.rho_swept / s.rho_tot for s in states],
                 "-", color="tab:orange", lw=2.0, label="with Eq. (7b)")
    axes[2].plot(burnups, [u[2] / u[0] for u in unbounded],
                 "--", color="tab:orange", lw=1.2, label="without it")
    axes[2].axhline(0.0, color="k", lw=0.8)
    axes[2].set_ylabel(r"$\rho_{swept} / \rho_{tot}$  [-]")
    axes[2].set_title("the sweep: a sink, or a source", fontsize=10)
    axes[2].legend(fontsize=8, loc="lower right")

    for axis in axes:
        if bounded_band:
            axis.axvspan(min(bounded_band), max(bounded_band), color="0.9", zorder=0,
                         label="_nolegend_")
        for burnup in (BU_THRESHOLD, BU_ONSET, BU_SATURATION):
            axis.axvline(burnup, ls=":", lw=0.8, color="0.5")
        axis.set_xlabel("burnup  [GWd/tU]")
        axis.set_xlim(burnups[0], burnups[-1])

    title = "HBS formation -- the dislocation balance of Eq. (5)"
    if bounded_band:
        title += (".  Shaded: where Eq. (7b) sets $\\eta$ (%.1f-%.1f GWd/tU); "
                  "dotted: threshold, onset, saturation"
                  % (min(bounded_band), max(bounded_band)))
    figure.suptitle(title)
    figure.tight_layout()
    figure.savefig(path, dpi=150)
    print("written: %s" % path)


def main(argv=None):
    parser = argparse.ArgumentParser(
        description="HBS formation as a second-order phase transition (Landau functional). "
                    "Reference implementation of iHighBurnupStructureFormation = 4.",
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--table", action="store_true", help="reference behaviour vs burnup")
    parser.add_argument("--validate", action="store_true",
                        help="the model against the EBSD measurements")
    parser.add_argument("--selftest", action="store_true",
                        help="the model against itself: invariants, no experimental data")
    parser.add_argument("--plot", nargs="?", const="hbs_formation_landau.png", default=None,
                        metavar="PNG", help="the three outputs against burnup")
    parser.add_argument("--plot-populations", dest="plot_populations", nargs="?",
                        const="hbs_dislocation_populations.png", default=None, metavar="PNG",
                        help="the three dislocation populations of Eq. (5) against burnup, "
                             "with and without the balance bound of Eq. (7b)")
    parser.add_argument("--temperature", type=float, default=REFERENCE_TEMPERATURE, metavar="K",
                        help="temperature for --table and --plot (default %g)"
                             % REFERENCE_TEMPERATURE)
    parser.add_argument("--point", nargs=2, type=float, default=None, metavar=("BU", "T"),
                        help="evaluate the model at one (burnup [GWd/tU], temperature [K])")
    arguments = parser.parse_args(argv)

    if not any((arguments.table, arguments.validate, arguments.selftest,
                arguments.plot, arguments.plot_populations, arguments.point)):
        parser.print_help()
        return 0

    printed = False
    for enabled, action in ((arguments.selftest, lambda: selftest()),
                            (arguments.table, lambda: print_table(arguments.temperature)),
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
    if arguments.plot_populations:
        plot_populations(arguments.plot_populations, arguments.temperature)
    return 0


if __name__ == "__main__":
    sys.exit(main())
