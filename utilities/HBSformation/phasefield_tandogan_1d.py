"""1D orientation phase field for dislocation-driven grain nucleation,
reproduced for Cu and then driven by the HBS Landau model for UO2.

    python3 phasefield_tandogan_1d.py

References
==========
[T26]  I.T. Tandogan, M. Budnitzki, S. Sandfeld, "A multi-physics model for
       dislocation driven spontaneous grain nucleation and microstructure
       evolution in polycrystals", J. Mech. Phys. Solids 206 (2026) 106325.
[PFRP] N. Ofori-Opoku, J. Warren, P.-C. Simon, "Phase Field Method Recommended
       Practices - Model Formulation". PFHub.
[HBS]  hbs_formation_landau.py in this folder (Landau model of the HBS).
[G03]  S. Gourdet, F. Montheillet, "A model of continuous dynamic
       recrystallization", Acta Mater. 51 (2003) 2685. 
[Z21]  J. Zhang et al., J. Am. Ceram. Soc. (2021): the Bulatov-Reed-Kumar
       5-DOF grain boundary energy function refitted for UO2 (eRGB = 1.545
       J/m^2).
[ON25] M.-L. Onofri et al.: TEM dislocation line densities in irradiated UO2,
       1.1 +- 0.2e9 cm/cm^3 inside the grains against 2.1 +- 0.2e9 cm/cm^3 over
       the 500-600 nm next to the original grain boundaries (1 cm/cm^3 = 1e4
       m^-2, so 1.1e13 and 2.1e13 m^-2).  Used in 3.5, to set the diffuse
       width nu.


1. THE MODEL OF [T26], SECTION 2
================================
Cosserat crystal plasticity coupled to a Henry-Mellenthin-Plapp (HMP)
orientation phase field:

    eta   in [0, 1]   crystalline order, 1 in the grains, < 1 in the diffuse GB
    theta             lattice orientation (= Cosserat microrotation)
    rho               statistically stored dislocation (SSD) density

Free energy, Eq. (15):

    psi = f0 [ alpha V(eta) + nu^2/2 |grad eta|^2 + mu^2 g(eta) |grad theta|^2 ]
        + elastic terms
        + phi(eta) * sum_a lambda/2 G r_a^2,         r_a^2 = b^2 sum_b h_ab rho_b         (21)

    V(eta)  = (1 - eta)^2 / 2                                                  (35)
    g(eta)  = (7 eta^3 - 6 eta^4)/(1 - eta)^3 + c ln(1 - eta) + C0            (35)-(36)
              singular at eta = 1 -> localised GB; the log gives Read-Shockley
    phi'(eta) = c3/2 [1 - tanh(c1 (eta - c2))]                                 (38)
              ~ c3 inside the GB (eta < c2), ~ 0 in the bulk (eta > c2)

The nucleation mechanism (Sec. 2.3, 3.2) is the combination of two terms:

  (a) the SSD term  -phi'(eta) lambda/2 G b^2 rho  in Eq. (32) pushes eta down
      only inside the GB: the GB widens towards eta_eq of Eq. (42) and the
      orientation gradient spreads out (an unstable, non-localised state);
  (b) the recovery term  -rho C_D A(eta) <eta_dot>  of Eq. (28) removes the
      dislocations where eta grows again: if eta_eq is above the initial GB
      depth, eta rises at the GB centre, rho drops there, the SSD force
      disappears, eta -> 1 and a dislocation-free grain with an intermediate
      orientation is born.  Two GBs then move apart, as in SIBM.

2. THE 1D REDUCTION
===================
* Geometry.  x in [0, L], one GB at L/2 (Sec. 3.2).  The paper's periodic
  bicrystal (Fig. 3) is symmetric about the grain centres, so zero-flux walls at
  x = 0 and x = L are exact and L is a HALF period.
  L must be about 10 nu.  The SSD term first WIDENS the boundary (Sec. 3.2.1)
  and only then does it split into the two boundaries of Sec. 3.2.2; if the
  widened boundary reaches the walls first, the orientation gradient disappears,
  the whole domain recrystallises as a single grain and nothing nucleates at all
  (limitation 8).  [T26]'s 10 um half period is 10 nu at their nu = 1 um, and Cu
  keeps it (CU_GRID); UO2 has nu = 2.5 um (3.5), so L = 25 um.
* No mechanics.  u = 0, no slip.  A large Cosserat modulus mu_c enforces
  e^e_skew = 0 and, with the eigenrotation initialised as in Eq. (27),
  e* = -theta.  Eq. (34), with tau_* = tau_hat g(eta) (Eq. 23), becomes

      tau_hat g(eta) d theta/dt = f0 d/dx [ mu^2 g(eta) d theta/dx ]           (34)

* Order parameter, Eq. (32), one slip system, h = 1 -> r^2 = b^2 rho:

      tau_eta d eta/dt = f0 nu^2 eta''                     f_eta1
                       - f0 alpha V'(eta)                  f_eta2
                       - f0 mu^2 g'(eta) theta'^2          f_eta3
                       - phi'(eta) lambda/2 G b^2 rho      f_eta4   (Eq. 41) (32)

* Dislocations, Eq. (28), recovery branch only (no slip -> no production):

      d rho/dt = - rho C_D A(eta) <d eta/dt>  [+ S(t)]                         (28)

  S(t) is a dislocation source, used only for UO2 (Part C) and related
  to dislocations formed during irradiation - not considered in the work
  of [T26].

* State vector: y = [eta, theta, rho_free, rho_wall], each of length `cells`.
  rho_wall is the polygonisation field of 3.6; with POLYGONISATION_YEARS = None
  it stays at 0 and the model is exactly [T26].

* Mobilities: Table 1 "recrystallisation" values, tau_eta = 1e4 f0 t0,
  tau_hat = 1e1 f0 t0, t0 = 1 s.  The initial GB is relaxed with
  tau_eta = 1e2 f0 t0 only to get to equilibrium faster.

3. UO2 APPLIED
==============
3.1 The functional
------------------
* Delta theta is the misorientation of the original grains imposed in initial_state().  
  It is a usual high-angle boundary (GRAIN_MISORIENTATION_DEG, 20 deg by default, 
  inside the 0-30 deg range where Fig. 2 is calibrated).
* The orientation of the new grain is an output of Eqs. (32)-(34): theta
  of the nucleus lies between the two parents and, by Eq. (43), it is their
  average if rho is the same on both sides and moves towards the grain with
  the lower rho otherwise (Fig. 6).  Its misorientation to the closer
  parent, `nucleus_misorientation_deg`, is what can be compared with the
  misorientation Theta(bu) of hbs_state().
* This functional does not produce misorientation from dislocations.  
  rho is a scalar SSD density carrying stored energy only, no orientation.  
  In [T26] the GND content is the Cosserat curvature kappa = grad theta (Eq. 3), 
  and new lattice rotation comes only from plastic slip (Eqs. 24-26).  
  Without mechanics (no slip) theta can only be redistributed: with a uniform rho 
  a nucleus, when it forms, sits at Delta theta / 2; otherwise the GB migrates 
  (theta of the centre goes to a parent's value, event "migration").  
  The split rho_tot -> rho_ord (walls, GND, misorientation) + rho_free (random) 
  is not done by the functional of [T26].  
  Polygonisation (3.6) adds one, but it is not usable yet: see the OPEN
  paragraph there.
* Consequence for rho: with the transfer off the phase field does not know
  about the split, so the natural input is the total density of Nogita-Une,
  rho_tot.  With the transfer of 3.6 on, the phase field has its own split
  (rho_free, rho_wall and what the recovery of Eq. (28) removes) and it can be
  put side by side with the Landau one: plot_partition().

3.2 Inputs
----------
Dislocations are produced by the source S(bu).

  Delta theta   irradiation angles -> initial_state(), height of the theta step at L/2.
  S(bu)         d rho_ref/d bu * bu_rate, rho_ref = hbs_state(bu).rho_tot
                -> run_irradiation() builds S and _right_hand_side() adds it to
                   d rho/dt, in place of the Kocks-Mecking production of
                   Eq. (28) (irradiation instead of slip).  Independent of the
                   local rho, so a recovered nucleus is loaded again, and
                   STRICTLY UNIFORM in x: irradiation makes dislocations
                   everywhere in the grain, so S has no shape, and the domain
                   average of the density produced is exactly rho_tot(bu).
  G(bu, T)      hbs_state(bu).shear_modulus
  b             Burger's vector of [HBS].
  nu            UO2_NU, the diffuse width, set on the layer of [ON25], see 3.5.
  c3            calibrate_c3() / c3_rules(), see 3.3.
  t0            UO2_T0, the phase-field clock, see 3.4.
  C_D           UO2_C_D, weaker recovery, see 3.4.
  M0            POLYGONISATION_YEARS -> mobility_from_timescale(), the transfer
                rho_free -> rho_wall, see 3.6.
  Theta_HAGB    THETA_HAGB of [HBS], the ceiling of the walls, wall_saturation().
  Landau Theta, r_n, X are used only for comparison (tables and plots).

3.3 What c3 is and how it is chosen
-----------------------------------
c3 is the height of phi'(eta) inside the GB (Eq. 38): it multiplies the
stored-energy force f_eta4 = -c3 * lambda/2 G b^2 rho.  
It fixes how much rho is needed to widen the GB, through Eq. (42):

    eta_eq = 1 - c3 * (lambda/2 G b^2 rho) / (f0 alpha)

Too large a c3 drives eta_eq below 0 (not allowed); too small a c3 and the
GB never widens enough to nucleate. 
The c3 of Table 1 of [T26] (eta_eq = 0.579 at rho0) belongs to Cu. For UO2:

  * onset rule (calibrate_c3, used when CALIBRATE_C3 = True): c3 is found by
    bisection so that recrystallisation starts at the experimental HBS onset 
    (60 GWd/tU).  CALIBRATE_C3 = False uses UO2_C3, the
    value the bisection returns for the default configuration.

3.4 Time, burnup evolution
--------------------------
    
    bu(t) = bu_start + BU_RATE t, 

with BU_RATE ~ 10 GWd/tU per year; plot_snapshots() shows the profiles of that
single history at several burnups. The mobilities are given in
units of f0 t0 (Table 1, t0 = 1 s for Cu). UO2_T0 is now chosen so that the
real rate gives the same product.  This is a placeholder for the UO2 GB
mobility.
# TODO (later, the only one left open besides 3.6): replace UO2_T0 by the UO2 GB
#   mobility at TEMPERATURE (Arrhenius M = M0 exp(-Q/RT), then t0 =
#   tau_eta/(f0 M)).  Limitation 6 is what it costs until then.

3.5 GB energy: gamma(Delta theta) fitted on UO2
-----------------------------------------------
[T26] fits f0 and c of Eq. (35) to atomistic Cu <100> tilt energies (Fig. 2).  
c3 and the critical radius, f0 and c are refitted here on [Z21] for UO2.

The 5-DOF function of [Z21] gives gamma(misorientation, boundary plane); 
the phase field assumes isotropic interfaces, a(n_gb, theta) = 1, so the 
random misorientation axes and random boundary planes at fixed misorientation 
angle (uo2_gb_energy.random_gamma) are considered. 
Two symmetric-tilt cross sections are tabulated next to it for comparison.  
Fitted over 0-30 deg with fit_interface_parameters(), at nu = UO2_NU:

    target                     c     f0 [kPa]   rms [J/m^2]   max
    random axis and plane     34       672        0.015       3.9 %
    symmetric tilt <110>      30       716        0.017       3.1 %
    symmetric tilt <100>      45       818        0.029       3.6 %

    c -> Read-Shockley coefficient of the log term of Eq. (35).
    alpha and mu -> Cu values of Table 1; 
    nu -> GB layer of [ON25], see below.

3.6 Polygonisation: the tangle and the walls as two reservoirs
---------------------------------------------------------------

# TODO: check the equations here

From Eq. (15) without mechanics (u = 0 and no slip). Theta is purely skew, 
so the symmetric elastic term drops; with the eigenrotation initialised as in 
Eq. (27) and a large mu_c the Cosserat penalty stays at 0.  
What remains is the plain Henry-Mellenthin-Plapp functional plus a dislocation 
reservoir.

Three dislocation populations:

    psi = f0 [ alpha V + nu^2/2 (d eta/dx)^2 + mu^2 g(eta) (d theta/dx)^2 ]
        + phi(eta) lambda/2 G b^2 rho_free                              (H1)

    tau_eta d eta/dt   = -delta psi / delta eta                         (H2)
    tau_*   d theta/dt = -delta psi / delta theta,  tau_* = tau_hat g   (H3)

    d rho_tot /dt = S_irr - rho_free C_D A(eta) <d eta/dt>              (H4)
    d rho_free/dt = S_irr - rho_free C_D A(eta) <d eta/dt> - R          (H5)
    d rho_wall/dt = + R,          R = M rho_free dG                     (H6)

    dG = d psi/d rho_free - d psi/d rho_wall
       = phi(eta) lambda/2 G b^2 - 2 mu^2 f0 g(eta) b^2 rho_wall        (H7)

Recovery acts only where eta grows, as in Eq. (28).

The link is the standard split of the curvature, the plastic part being what 
the walls carry (Nye):

    kappa = d theta/dx = kappa^e + kappa^p,   kappa^p = b s(x) rho_wall  (P1)

    psi_grad = f0 mu^2 g(eta) [ b^2 rho_wall^2                wall
                              + (d theta/dx - kappa^p)^2 ]    mismatch   (P2)

which reduces exactly to (H1) whenever Nye holds.  Then

    tau_* d theta/dt = f0 d/dx [ mu^2 g(eta) (d theta/dx - b s rho_wall) ] (P3)
    mu_w = d psi/d rho_wall = 2 f0 mu^2 g b (2 b rho_wall - s d theta/dx)   (P4)

(P3) is LOCAL - one derivative of rho_wall, so the Jacobian stays banded - and
(P4) collapses to (H7) at the Nye-consistent state s d theta/dx = b rho_wall.
(H7) is the relaxed limit; the extra term is what makes rho_wall and theta
talk to each other. 

R is SIGNED, not <dG>+, and the reason is physical: where a nucleus forms
eta -> 1, so g -> infinity and mu_w >> mu_f, hence dG < 0 and the wall
DISSOLVES.  With a Macaulay bracket a nucleus would keep its substructure for
ever.  The dissipation is R dG = M rho_free dG^2 >= 0 either way.

What is still an input: the sign.  A scalar rho_free has no polarity, so
nothing in the free energy says which way the GNDs stored at x tilt the
lattice.  s(x) is imposed on the subgrain pattern, as in [G03], where the same
assumption is made explicitly ("all the dislocations absorbed by a LAB have
same sign").  Only the MAGNITUDE and the SATURATION come from the energy.

  OPEN    IT IS NOT INTEGRABLE IN REASONABLE TIME, which is why
          POLYGONISATION_YEARS is None.

4. LIMITATIONS
==============
1. 1D: no GB curvature, hence no capillary pressure; a nucleus of any size
   survives.  critical_radius() estimates r* = 2 gamma / (lambda/2 G b^2 rho)
   a posteriori, to be compared with the Landau sub-grain radius.
2. No mechanics: no slip, no stress, no Kocks-Mecking production; tau_eta is
   constant (the switch of Eq. 39 is not used).  The functional of [T26]
   therefore makes no new lattice rotation (3.1); the polygonisation of 3.6
   adds it as a constitutive transfer, not as mechanics.
3. One slip system (h = 1), isotropic GB energy (a(n_gb, theta) = 1).
4. The GB energy is fitted only over 0-30 deg (Cu: <100> tilt, Fig. 2; UO2:
   the random average of [Z21], 3.5).  gamma(Delta theta) is monotone by
   construction: no HAGB cusps, and the maximum near 20-25 deg of the UO2
   tilt cross sections is not reproduced.  Above 30 deg the curve is an
   extrapolation, so GRAIN_MISORIENTATION_DEG should stay at or below 30.
5. For UO2 the diffuse-interface parameters alpha, mu, c1, c2 and the
   mobilities are still the Cu values of Table 1; f0, c, nu, G, b, c3, C_D and
   t0 are changed (f0 and c fitted in 3.5 AT nu = UO2_NU, which is set on the
   GB layer of [ON25], 3.5; t0 is a placeholder, 3.4).
6. Irradiation protocol: the result depends on BU_RATE * t0 and t0 is not calibrated for UO2; no recovery other than Eq. (28).
   The source is strictly uniform in x.
   The calibrated c3 drives eta_eq below 0 at high burnup with rho_tot (3.3),
   so the results above that burnup are outside the model; the c3 table of
   main() prints the burnup at which it happens for the c3 in use.
7. The event ("nucleus", "migration", "collapse", "none") is read off the
   profiles by classify_event(), in the terms [T26] use in Secs. 3.2.2-3.2.3:
   rho recovered at the GB centre, the two grain centres still crystalline at
   that moment, and a later bulge of eta above c2 with eta dipping below c2 on
   both sides.  The three numbers in it (the tenth of rho_ref, c2 itself, and
   "both sides") are still thresholds put on a continuous field, but they are
   the paper's own picture rather than a separate set invented here.
   ONE case does not come out as in [T26]: at Delta theta = 2.5 deg the paper
   reports that rho is never recovered, while here the orientation gradient
   does finally disappear and the whole domain recovers at once at
   t ~ 4.5e3 s.  classify_event() calls that a "collapse" and not a nucleus,
   which is the statement Fig. 5 makes, but the underlying profile differs.
8. The domain is the half period of Fig. 3, from grain centre to grain centre,
   with zero-flux walls.  This is exact for the symmetric bicrystal (dtheta/dx
   = 0 at the grain centres), but it FORBIDS the antisymmetric mode, i.e. a
   rigid drift of the whole two-GB structure.  With the source now strictly
   uniform the 1D problem is exactly mirror-symmetric about L/2, so migration
   is not merely damped, it is unreachable, and the only accessible outcome is
   the symmetric nucleus at Delta theta / 2 (Eq. 43 with equal rho).  The
   asymmetric branch of Fig. 6, rho unequal across the GB, is still reachable
   in run_case(), where rho0 is imposed as a pair - not in the irradiation
   protocol.  Getting migration back under irradiation needs the FULL period
   with periodic boundaries, and a polycrystal after that: that is the next
   step, not something a seeded perturbation should be asked to fake.
   L must also be about 10 nu, or the widened boundary reaches the walls before
   it can split and the run collapses into a single grain instead (see 2).
9. Polygonisation (3.6) gives the magnitude of the misorientation, not its
   SIGN: s(x) is imposed on the subgrain pattern, because a scalar rho_free
   carries no polarity.  The mobility M0 is a free parameter, not measured.
   The ceiling is imposed too: wall_saturation() stops the absorption at
   Theta_HAGB because the energy alone does not bound it (the capacity
   rho_wall_eq of Eq. (P6) diverges as eta falls), exactly as [HBS] caps eta at
   eta_HAGB in Eq. (L8).  Both are constitutive statements ADDED to the
   functional, and the subgrain size D that converts rho_wall into a
   misorientation is an input as well (2 r_n at the onset burnup, held fixed
   while [HBS] has r_n shrinking with burnup).  The level of phi (its constant c0, free in
   [T26]) sets mu_f and therefore the whole driving force; the convention
   phi(0) = 0 used here makes the bulk stored energy c2 c3 lambda/2 G b^2
   instead of the classical lambda/2 G b^2, so c3 now does double duty as the
   nucleation knob of 3.3 AND as the scale of the stored energy.  Splitting
   the two would need a phi normalised independently of c3.


5. NUMERICS  (checked against [PFRP])
======================================
* Finite volumes on a uniform grid, zero-flux faces at the walls; g at the
  faces by harmonic (series) average; theta'^2 at the cell centres as the
  mean of the two face values.  Method of lines, stiff BDF (scipy) with
  adaptive time step and a sparse Jacobian pattern ([PFRP] item 8).  The
  pattern is built as a sparse matrix, not as the dense 4n x 4n block: at the
  default grid that is 128 MB per evolve() call, and the fit of 3.5 makes
  dozens of them.
* Interface resolution.  For UO2 the eta well is nu/sqrt(alpha) = 0.56 um
  (3.5), i.e. ~56 cells at dx = 10 nm (Grid: 2500 cells over 25 um; [T26] uses
  400 blocks over the 20 um period, dx = 50 nm, and Cu keeps nu = 1 um, 0.22 um
  and ~22 cells over 400 cells, CU_GRID).  The THETA transition is much
  narrower, it shrinks with the misorientation, and it is what sets dx.  With
  the UO2 fit (nu = 2.5 um, c = 34) its 10-90% width is

      Delta theta    5 deg    15 deg   20 deg   30 deg
      width          0.83 um  0.27 um  0.09 um  0.03 um
      cells at 10 nm 83       27       9        3

  so [PFRP] item 3 (5-10 cells) is met at the default Delta theta = 20 deg and
  not at 30 deg, where the core is 3 cells: the same statement as before the
  refit (4 cells at 30 deg with nu = 1 um, c = 7), because c and nu grow
  together and their effects on the theta core largely cancel.
  dx, NOT the cell count, is what has to be held: the domain was lengthened to
  25 um (Sec. 2) and the cell count with it.
* Grid convergence of gamma (5, 15, 30 deg), the quantity fitted in 3.5:

      cells over 10 um    Cu, c = 3                 UO2, nu = 2.5 um, c = 34
       400 (dx 25 nm)     0.3686  0.7045  0.8774    0.7825  1.3889  1.5188
      1000 (dx 10 nm)     0.3686  0.7045  0.8722    0.7826  1.3893  1.5117
      2000 (dx  5 nm)     0.3686  0.7046  0.8722    0.7826  1.3893  1.5114

  Cu is converged at dx = 25 nm, which is why CU_GRID keeps 400 cells over its
  10 um; UO2 is within 0.5% of the converged value there (0.47% at 30 deg) and
  within 0.02% from dx = 10 to 5 nm, so UO2 runs at dx = 10 nm.  gamma stays
  converged even where the theta core is only ~3 cells wide because g is
  floored at 0.01 (Eq. 36), so mu^2 g |grad theta|^2 stays bounded as the core
  sharpens.
  A COARSER grid is not a cheaper one here.  At 400 cells over 10 um the UO2
  theta core is ~4 cells, g jumps by orders of magnitude between neighbours, and
  one irradiation history costs 4-10 times MORE than at 10 nm (246 s against
  26 s, with overflows inside scipy's numerical Jacobian) while the onset it
  returns stops being monotone in c3.  That is why the c3 bisection of 3.3 runs
  on the production grid: see the comment at CALIBRATE_C3.
* g is frozen above eta_cutoff = 1 - 1e-4, to control the singularity: that is
  [T26] Sec. 2.2.2, "g(eta) -> g(min(eta, eta_cutoff))".  A(eta) of Eq. (29) is
  frozen the same way.  The paper states the cutoff only for g, so for A it is a
  necessity of this implementation: without it one UO2 history at 2500 cells
  goes from 50 s to more than 5 min (recovery_localiser()).  [PFRP] recommends plotting the state functions, which
  tandogan_cu_fig1_state_functions.png does.
* Equations are kept dimensional (SI), not non-dimensionalised as [PFRP]
  suggests; the time unit t0 enters only through the mobilities.
* The initial condition is a sharp step relaxed without dislocations before
  anything else is switched on ([PFRP] item 7).
"""

import math
import os
import sys
from dataclasses import dataclass, replace

import numpy as np
from scipy.integrate import solve_ivp
from scipy.sparse import diags, kron

from hbs_formation_landau import (
    BURGERS,
    REFERENCE_TEMPERATURE,
    THETA_HAGB,
    hbs_state,
    regime_boundaries,
)


def _pyplot():
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    return plt


# ###########################################################################
#
#   PART A - THE MODEL (material independent)
#
# ###########################################################################

# ---------------------------------------------------------------------------
# A.1  Constants
# ---------------------------------------------------------------------------

ETA_CUTOFF = 1.0 - 1.0e-4           # -      g and A frozen above, Sec. 2.2.2
ETA_WALL_CUTOFF = 0.95              # -      g frozen above this in the WALL self
                                    #        energy only (3.6): see wall_coupling()


# ---------------------------------------------------------------------------
# A.2  Parameters
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class PhaseFieldParameters:
    """Model parameters.  Defaults: Cu, Table 1 of [T26] and caption of Fig. 4."""

    # phase field, Eq. (15)
    f0: float = 371.0e3             # Pa     normalisation coefficient
    nu: float = 1.0e-6              # m      eta gradient coefficient
    alpha: float = 20.0             # -      weight of V(eta)
    mu: float = 2.5e-6 / math.pi    # m      theta gradient coefficient
    c: float = 3.0                  # -      Read-Shockley log term of g, Eq. (35)
    # inverse mobilities, Eqs. (23), (30); in units of f0*t0
    tau_eta: float = 1.0e4          # f0*t0  recrystallisation value
    tau_hat: float = 1.0e1          # f0*t0  recrystallisation value
    t0: float = 1.0                 # s      time unit of the mobilities (Table 1: 1 s).
                                    #        The whole phase-field clock scales with it:
                                    #        larger t0 = less mobile GBs (UO2: see C.1)
    # stored dislocation energy, Eq. (15) last term
    lam: float = 0.3                # -      lambda
    shear_modulus: float = 75.0e9   # Pa     mu^e  (UO2: from hbs_state)
    burgers: float = 0.2556e-9      # m      b     (UO2: BURGERS of [HBS])
    # phi'(eta), Eq. (38)
    c1: float = 100.0               # -      steepness of the step
    c2: float = 0.95                # -      position of the step
    c3: float = 1.7                 # -      height of the step (UO2: uo2_parameters)
    # recovery, Eq. (28)
    c_d: float = 100.0              # -      C_D

    @property
    def stored_energy_per_rho(self):
        """lambda/2 * G * b^2 [J/m]: multiplies rho in Eq. (15) (h = 1)."""
        return 0.5 * self.lam * self.shear_modulus * self.burgers ** 2


# ---------------------------------------------------------------------------
# A.3  Functions of eta: Eqs. (29), (35)-(38), (42)
# ---------------------------------------------------------------------------

def potential_derivative(eta):
    """V'(eta), with V = (1 - eta)^2 / 2, Eq. (35)."""
    return -(1.0 - eta)


def _log_cosh(z):
    """ln cosh z: overflow-free for real z, and defined for complex z."""
    if np.iscomplexobj(z):
        return np.log(np.cosh(z))
    return np.logaddexp(z, -z) - math.log(2.0)


def _g_raw(eta, c):
    """g(eta) of Eq. (35) without the constant C0."""
    return (7.0 * eta ** 3 - 6.0 * eta ** 4) / (1.0 - eta) ** 3 + c * np.log(1.0 - eta)


def g_offset(c):
    """C0 of Eq. (36), shift that makes g >= 0.01 on [0, eta_cutoff].

    g must stay positive: it multiplies |grad theta|^2 in Eq. (15) and it is
    the inverse rotational mobility in Eq. (23).  g_raw is 0 at eta = 0 and
    the log term c ln(1 - eta) makes it negative at intermediate eta, so
    min(g_raw) = -m <= 0.  Shifting by C0 = -min(g_raw) + 0.01 = m + 0.01
    moves that minimum up to exactly 0.01; the minus turns the (negative)
    minimum into a positive shift.  Eq. (36) in [T26] is written with the
    same meaning, |min| + 0.01.  The min(..., 0) only guards the case
    g_raw >= 0, where no shift beyond 0.01 is needed.  The minimum is found
    on a fine grid; selftest() checks min g = 0.01.
    """
    eta = np.linspace(0.0, ETA_CUTOFF, 200001)
    return -min(_g_raw(eta, c).min(), 0.0) + 0.01


def coupling_g(eta, c, offset):
    """g(eta) and g'(eta), Eq. (35), with g -> g(min(eta, eta_cutoff))."""
    e = np.minimum(eta, ETA_CUTOFF)
    g = _g_raw(e, c) + offset
    dg = ((21.0 * e ** 2 - 24.0 * e ** 3) / (1.0 - e) ** 3
          + 3.0 * (7.0 * e ** 3 - 6.0 * e ** 4) / (1.0 - e) ** 4
          - c / (1.0 - e))
    dg = np.where(eta < ETA_CUTOFF, dg, 0.0)     # g is constant above the cutoff
    return g, dg


def coupling_phi(eta, p):
    """phi(eta) of Eq. (37), with c0 fixed by phi(0) = 0.

        phi = c3/2 {eta - c1^-1 ln[cosh(c1 (c2 - eta))]} + c0

    phi(0) = 0 is the physical one: a dislocation in a fully disordered region
    has no lattice to strain, hence no stored energy, and phi >= 0 everywhere.
    It gives phi(1) ~ c2 c3, i.e. the bulk stored energy is c2 c3 lambda/2 G b^2
    and not the classical lambda/2 G b^2.

    ln(cosh z) is evaluated as logaddexp(z, -z) - ln 2, which does not overflow;
    on a complex argument (verify_mathematics(), complex step) logaddexp does not
    exist, and log(cosh z) is used instead - safe because |c1 (c2 - eta)| stays
    of order 100 there.
    """
    z = p.c1 * (p.c2 - eta)
    log_cosh = _log_cosh(z)
    log_cosh0 = _log_cosh(p.c1 * p.c2)
    return 0.5 * p.c3 * ((eta - log_cosh / p.c1) + log_cosh0 / p.c1)


def phi_derivative(eta, p):
    """phi'(eta), Eq. (38): ~c3 in the GB, ~0 in the bulk.

    This is what confines the stored-energy force f_eta4 to the GB.
    """
    return 0.5 * p.c3 * (1.0 - np.tanh(p.c1 * (eta - p.c2)))


def wall_coupling(eta, p, offset):
    """g and g' for the WALL self-energy of Eq. (P2'), frozen above ETA_WALL_CUTOFF.

    WHY A SECOND CUTOFF.  The term f0 mu^2 g(eta) b^2 rho_wall^2 says that a
    wall cannot be paid for in a perfect crystal, and it says it with a
    divergence: g ~ (1 - eta)^-3, g' ~ (1 - eta)^-4.  Differentiated, it puts a
    force on eta of f0 mu^2 g'(eta) b^2 rho_wall^2, which at eta = 0.999 and a
    modest rho_wall = 1e13 m^-2 is already eta_dot = 0.9 s^-1 - against
    3.2e6 s of physical time per GWd/tU.  The integration then either crawls or
    fails in the linear solve ("factor is exactly singular").

    Freezing g above ETA_WALL_CUTOFF = c2 costs nothing physical: above c2 the
    transfer of 3.6 has dG < 0 by a wide margin (mu_w carries the same g), so
    walls do not grow there anyway - the cutoff only bounds the PENALTY for the
    walls that numerics leaves behind.  It is the same device [T26] use for g in
    their Sec. 2.2.2, applied where the divergence actually bites.

    Note it is applied to the energy, not to the force alone: total_energy(),
    wall_potential() and the eta force all use this g, so the system stays the
    exact gradient flow that verify_mathematics() checks.
    """
    e = np.minimum(eta, ETA_WALL_CUTOFF)
    g, dg = coupling_g(e, p.c, offset)
    return g, np.where(eta < ETA_WALL_CUTOFF, dg, 0.0)


def recovery_localiser(eta):
    """A(eta), Eq. (29), frozen above eta_cutoff like g.

    [T26] state the cutoff only for g (Sec. 2.2.2), so this freeze is a
    numerical necessity of THIS implementation, not a statement of the paper.
    Measured on one UO2 irradiation history at the production grid (2500
    cells): 50 s frozen, still unfinished after 5 min unfrozen.  At 1000 cells
    the two cost the same, which is why the need is easy to miss: A ~ 1/(1 -
    eta) multiplies <eta_dot> in Eq. (28), and the finer the grid the closer to
    1 the nucleus gets, the stiffer the rho equation becomes.
    """
    e = np.minimum(eta, ETA_CUTOFF)
    return (7.0 * e ** 3 - 6.0 * e ** 4) / (1.0 - e)


def eta_equilibrium(rho, p):
    """eta_eq of a widened GB, Eq. (42), with phi'(eta_eq) = c3.

    Nucleation (Sec. 3.2.3) needs eta_eq above the initial GB depth.
    """
    return 1.0 - p.c3 * p.stored_energy_per_rho * rho / (p.f0 * p.alpha)


# ---------------------------------------------------------------------------
# A.7  Polygonisation: the tangle and the walls as two reservoirs (3.6)
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Polygonisation:
    """Transfer of dislocations from the tangle into the walls, docstring 3.6.

    mobility  M_0 of R = (M_0/g(eta)) rho_free dG.  0 disables the transfer and
              the model is exactly [T26].  The 1/g(eta) is the device [T26] use
              for the eigenrotation in Eq. (23), tau_* = tau_hat g(eta), and it
              is needed for the same reason: mu_w carries a factor g, so with a
              constant M the relaxation rate of Eq. (H6),
              k = 4 M rho_free f0 mu^2 b^2 g(eta), diverges with g as eta -> 1
              (tau ~ 9 ms at the cutoff against 3.2e6 s of physical time per
              GWd/tU) and the step size collapses.  Dividing by g makes k
              independent of eta and says the physical thing: dislocations
              cannot climb into walls in a perfect lattice.  Use
              mobility_from_timescale() to choose M_0.
    sign      s(x), array (n): which way the GNDs stored at x tilt the lattice,
              i.e. the sign of the plastic curvature kappa^p = b s rho_wall.
              None means +1 everywhere (a single wall).  This is the one
              ingredient that does NOT follow from the free energy: a scalar
              density has no polarity (limitation 9).
    subgrain  D [m], the spacing of the walls: the misorientation a wall carries
              is Theta_wall = b rho_wall D (Frank).  Only the CEILING and the
              post-processing need it, not the transfer itself.
    theta_max Theta_HAGB [deg], the ceiling of Theta_wall: a wall at that angle
              has its cores overlapping (spacing b/Theta ~ 6 b) and cannot
              absorb any more dislocations.  Same cap, and same value, as
              Eq. (L8) of [HBS]; see wall_saturation().
    """

    mobility: float = 0.0
    sign: object = None
    subgrain: float = 1.0e-6
    theta_max: float = THETA_HAGB


def plastic_curvature(rho_wall, sign, p):
    """kappa^p = b s(x) rho_wall [rad/m], Eq. (P1'), the curvature the walls carry.

    Nye's relation read forwards: a wall of volumetric density rho_wall tilts
    the lattice at the rate b rho_wall per unit length, with the sign s(x).
    """
    return p.burgers * sign * rho_wall


def tangle_potential(eta, p):
    """mu_f = d psi / d rho_free [J/m], energy per unit length in the tangle.

    The last term of Eq. (15) with r^2 = b^2 rho and h = 1, differentiated
    with respect to rho: phi(eta) lambda/2 G b^2.  Orientation-blind.
    """
    return coupling_phi(eta, p) * p.stored_energy_per_rho


def wall_potential(eta, rho_wall, g_elastic, sign, p, offset):
    """mu_w = d psi / d rho_wall [J/m], energy per unit length in a wall.

    From the split of Eq. (P2'), psi_grad = f0 mu^2 g [ b^2 rho_wall^2 +
    (d theta/dx - kappa^p)^2 ], the first term being the wall's own
    Read-Shockley energy and the second the mismatch between the lattice
    curvature and the one the walls carry.  In the continuum

        mu_w = 2 f0 mu^2 g(eta) b ( 2 b rho_wall - s d theta/dx ),

    and at the Nye-consistent state, s d theta/dx = b rho_wall, it collapses to
    2 f0 mu^2 g b^2 rho_wall, which is Eq. (H7): the handwritten driving force
    is the RELAXED limit of this one.

    DISCRETELY it is written the way it is differentiated.  kappa^p enters the
    mismatch at the FACES, as (kappa^p_i + kappa^p_i+1)/2, so

        mu_w,i = 2 f0 mu^2 g_i b^2 rho_wall,i
                 - f0 mu^2 b s_i [ (g e)_i-1/2 + (g e)_i+1/2 ],

    with (g e)_f = g_face (d theta/dx - kappa^p)_f, which is the quantity the
    theta flux already carries: `g_elastic`, of length n + 1.  Writing it with
    cell-centred g and d theta/dx instead is the same thing only where the
    fields are smooth: verify_mathematics() measures 31% at 60 cells, because
    the error sits exactly where s(x) changes sign - that is, at the walls.
    The two zero-flux faces carry g_elastic = 0, so the boundary cells get one
    face, which is also what the derivative says (limitation 8).
    """
    g, _ = wall_coupling(eta, p, offset)          # frozen above ETA_WALL_CUTOFF
    return (2.0 * p.f0 * p.mu ** 2 * g * p.burgers ** 2 * rho_wall
            - p.f0 * p.mu ** 2 * p.burgers * sign * (g_elastic[:-1] + g_elastic[1:]))


def polygonisation_drive(eta, rho_wall, g_elastic, sign, p, offset):
    """dG = mu_f - mu_w [J/m], the affinity of the reaction, Eq. (H7).

    g_elastic = g_face * (d theta/dx - kappa^p) at the n + 1 faces, the same
    array the theta flux is built from (wall_potential).
    """
    return (tangle_potential(eta, p)
            - wall_potential(eta, rho_wall, g_elastic, sign, p, offset))


def wall_misorientation(rho_wall, subgrain, p):
    """Theta_wall = b rho_wall D [deg], Frank's rule over one subgrain, Eq. (P7).

    This is the quantity that can be compared with Theta(bu) of [HBS]: theta of
    the field is the LATTICE orientation, which follows the walls through the
    elastic curvature of Eq. (P3').
    """
    return np.degrees(p.burgers * np.abs(rho_wall) * subgrain)


def wall_saturation(rho_wall, drive, pp, p):
    """s_cap in [0, 1], the ceiling of the walls, Eq. (P8).

        s_cap = 1                             where dG < 0  (the wall dissolves)
                <1 - Theta_wall/Theta_HAGB>+  where dG > 0  (the wall absorbs)

    WHY IT IS NEEDED.  The capacity of Eq. (P6), rho_wall^eq = phi lambda G /
    (4 mu^2 f0 g), diverges as eta falls, because g is floored at 0.01 while phi
    stays finite: without a ceiling Theta_wall runs to 60 deg at 70 GWd/tU and
    122 deg at 80, against 2.8 and 3.9 deg from [HBS].

    WHAT IT SAYS.  A wall at Theta_HAGB has a dislocation spacing b/Theta ~ 6 b:
    the cores overlap, the wall IS a high-angle boundary and cannot absorb any
    more dislocations.  Beyond that angle the object no longer belongs to the
    rho_wall reservoir at all - it is a boundary, i.e. a dip of eta and a step of
    theta.  [HBS] states the same thing as eta <= eta_HAGB in Eq. (L8), so the
    value Theta_HAGB = 10 deg is not a new parameter.

    WHAT IT COSTS.  It is a constitutive CONSTRAINT put on the kinetics, not on
    the free energy: at the ceiling dG stays positive and frozen, like any
    saturated reservoir.  It is of the same status as the sign s(x) of
    limitation 9 - imposed, not derived.  The dissipation stays non-negative,
    R dG = M rho dG^2 s_cap / g >= 0, since s_cap >= 0.  s_cap is a clipped
    straight line, 0 exactly at the ceiling: continuous, so R is continuous,
    but its slope jumps there and the BDF steps do get shorter near saturation.
    
    """
    head_room = np.clip(1.0 - wall_misorientation(rho_wall, pp.subgrain, p) / pp.theta_max,
                        0.0, 1.0)
    return np.where(drive > 0.0, head_room, 1.0)


def polygonisation_rate(eta, rho_wall, g_elastic, sign, rho_free, pp, p, offset):
    """R = M rho dG s_cap / g [m^-2 s^-1], the net flux tangle -> wall, (H5)-(H6).

    SIGNED, not a Macaulay bracket, and the reason is physical: where a nucleus
    forms eta -> 1, so g(eta) -> infinity and mu_w >> mu_f, hence dG < 0 and the
    wall DISSOLVES.  With <dG>+ a nucleus would keep its substructure for ever.
    The dissipation is R dG >= 0 either way, so both forms are admissible; only
    this one lets the wall disappear again.

    EACH BRANCH IS LIMITED BY THE RESERVOIR IT EMPTIES: the rate is proportional
    to rho_free when the wall absorbs (dG > 0) and to rho_wall when it dissolves
    (dG < 0).  Without the second half a wall goes on "dissolving" through zero
    into a negative density - which it did, reaching -3e15 m^-2 at 99 GWd/tU and
    taking eta down with it through the wall term of Eq. (P2') - and a negative
    density has no meaning.  Both prefactors are >= 0, so the dissipation is
    unchanged in sign.

    s_cap is the ceiling of wall_saturation(): it stops the absorption at
    Theta_HAGB and leaves the dissolution branch untouched.
    """
    g, _ = coupling_g(eta, p.c, offset)
    drive = polygonisation_drive(eta, rho_wall, g_elastic, sign, p, offset)
    reservoir = np.where(drive > 0.0, np.maximum(rho_free, 0.0),
                         np.maximum(rho_wall, 0.0))
    return (pp.mobility * reservoir * drive
            * wall_saturation(rho_wall, drive, pp, p) / g)


def mobility_from_timescale(years, rho_free, p):
    """M_0 giving a transfer time `years` at a free density rho_free [m^-2].

    The wall relaxes as k = 4 M_0 rho_free f0 mu^2 b^2 once the 1/g(eta) is in
    the mobility, so M_0 = 1 / (tau k'), with k' the bracket below.  Choosing
    the mobility from a time is the only honest way to set it: M_0 is not
    measured (limitation 9).
    """
    tau = years * SECONDS_PER_YEAR
    return 1.0 / (tau * 4.0 * rho_free * p.f0 * p.mu ** 2 * p.burgers ** 2)


# ---------------------------------------------------------------------------
# A.4  1D solver
# ---------------------------------------------------------------------------

@dataclass
class Grid:
    length: float = 25.0e-6         # m      grain-centre to grain-centre, Fig. 3.
                                    #        ~10 nu: the SSD term of Eq. (32) widens
                                    #        the GB before it splits (Sec. 3.2.1 of
                                    #        [T26]) and a domain that the widened GB
                                    #        fills cannot nucleate at all, see
                                    #        limitation 8.  [T26]'s 10 um half period
                                    #        is 10 nu at THEIR nu = 1 um; CU_GRID
                                    #        keeps it for Cu.
    cells: int = 2500               # -      dx = 10 nm; [T26] uses 400 blocks over
                                    #        the 20 um period (dx = 50 nm), but the
                                    #        UO2 fit (nu = 2.5 um, c = 34) narrows
                                    #        the theta transition to ~0.09 um at
                                    #        Delta theta = 20 deg, see NUMERICS

    @property
    def dx(self):
        return self.length / self.cells

    @property
    def x(self):
        """Cell centres."""
        return (np.arange(self.cells) + 0.5) * self.dx


def _right_hand_side(p, grid, offset, source=None, polygonisation=None):
    """d/dt of the state y = [eta (n), theta (n), rho_free (n), rho_wall (n)].

    source(t) [m^-2/s]: dislocation production, uniform in x - irradiation makes
    dislocations everywhere in the grain, so S has no shape at all (docstring
    3.2).  None means no production, as in [T26] after loading.

    polygonisation: a Polygonisation, the transfer of the tangle into the
    walls of docstring 3.6.  None (or mobility 0) is exactly [T26]: rho_wall
    then stays 0 and drops out of every term.
    """
    n, dx = grid.cells, grid.dx
    tau_eta = p.tau_eta * p.f0 * p.t0           # J s m^-3
    tau_hat = p.tau_hat * p.f0 * p.t0           # J s m^-3
    energy = p.stored_energy_per_rho

    active = polygonisation is not None and polygonisation.mobility > 0.0
    sign = 1.0
    if polygonisation is not None and polygonisation.sign is not None:
        sign = polygonisation.sign
    def rhs(t, y):
        eta, theta = y[:n], y[n:2 * n]
        rho, rho_wall = y[2 * n:3 * n], y[3 * n:]
        g, dg = coupling_g(eta, p.c, offset)

        # gradients on the n + 1 faces; the two wall faces stay 0 (zero flux)
        dtheta = np.zeros(n + 1)
        dtheta[1:-1] = np.diff(theta) / dx
        deta = np.zeros(n + 1)
        deta[1:-1] = np.diff(eta) / dx

        g_face = np.zeros(n + 1)
        g_face[1:-1] = 2.0 * g[:-1] * g[1:] / (g[:-1] + g[1:])     # harmonic average

        # Eq. (34), with the curvature split of Eq. (P1'): what drives theta is
        # the ELASTIC curvature, the part of d theta/dx the walls do not already
        # account for.  kappa^p is put on the faces, where dtheta lives.
        #     tau_hat g theta_dot = f0 d/dx [ mu^2 g (d theta/dx - kappa^p) ]
        # This is the link rho_wall -> theta, and it is LOCAL: one derivative of
        # rho_wall, so the Jacobian stays banded.
        elastic = dtheta
        if active:
            kappa_p = plastic_curvature(rho_wall, sign, p)
            face = np.zeros(n + 1)
            face[1:-1] = 0.5 * (kappa_p[:-1] + kappa_p[1:])
            elastic = dtheta - face
        g_elastic = g_face * elastic          # (g e)_f, the faces of Eq. (P3')
        flux = p.mu ** 2 * g_elastic
        theta_dot = p.f0 * np.diff(flux) / dx / (tau_hat * g)

        # What multiplies g'(eta) in the eta force is the WHOLE gradient energy
        # that Eq. (P2') declares, g(eta) [ b^2 rho_wall^2 + (d theta/dx -
        # kappa^p)^2 ]: the Read-Shockley energy of the walls themselves plus the
        # mismatch between the lattice curvature and the one they carry.  The two
        # differ only away from the Nye-consistent state, but taking |grad theta|^2
        # instead would make the eta equation inconsistent with the energy the
        # theta equation is derived from.  With the transfer off rho_wall = 0 and
        # kappa^p = 0, so this is Eq. (15) unchanged and the [T26] path is exact.
        curvature_sq = 0.5 * (elastic[:-1] ** 2 + elastic[1:] ** 2)   # at cell centres
        wall_sq = 0.0
        if active:
            # the wall self-energy carries its own g, frozen above
            # ETA_WALL_CUTOFF (wall_coupling): without that freeze g' reaches
            # 3e12 at eta = 0.999 and the problem stops being integrable
            g_wall, dg_wall = wall_coupling(eta, p, offset)
            wall_sq = (p.burgers * rho_wall) ** 2

        # Eq. (32): order parameter, forces of Eq. (41)
        # (f_eta2 and f_eta3 kept grouped: the irradiation protocol is
        #  sensitive to the round-off of this sum)
        f_eta1 = p.f0 * p.nu ** 2 * np.diff(deta) / dx               # gradient
        f_eta23 = p.f0 * (p.alpha * potential_derivative(eta)        # potential
                          + p.mu ** 2 * dg * curvature_sq)           # orientation
        if active:
            f_eta23 = f_eta23 + p.f0 * p.mu ** 2 * dg_wall * wall_sq  # walls
        f_eta4 = phi_derivative(eta, p) * energy * rho               # stored dislocations
        eta_dot = (f_eta1 - f_eta23 - f_eta4) / tau_eta

        # Eq. (28): recovery, active only where eta grows (<.> Macaulay bracket)
        rho_dot = -rho * p.c_d * recovery_localiser(eta) * np.maximum(eta_dot, 0.0)

        # >>> POLYGONISATION (docstring 3.6): rho_free -> rho_wall at rate R,
        #     Eqs. (H5)-(H6).  Purely local: the wall is its own field and the
        #     lattice follows it through the elastic curvature above.
        rho_wall_dot = np.zeros(n)
        if active:
            transfer = polygonisation_rate(eta, rho_wall, g_elastic, sign,
                                           rho, polygonisation, p, offset)
            rho_dot = rho_dot - transfer
            rho_wall_dot = transfer

        # >>> DISLOCATION SOURCE: irradiation production (UO2, Part C.2).
        #     Replaces the Kocks-Mecking term of Eq. (28); uniform in x.
        if source is not None:
            rho_dot = rho_dot + source(t)

        return np.concatenate([eta_dot, theta_dot, rho_dot, rho_wall_dot])

    return rhs


def _sparsity(n):
    """Jacobian pattern: nearest-neighbour coupling within and between the
    four fields [eta, theta, rho_free, rho_wall].

    Banded throughout: the polygonisation of 3.6 couples rho_wall to theta only
    through one spatial derivative, Eq. (P3'), so nothing is non-local.

    Kept SPARSE: the dense 4n x 4n pattern is 128 MB at the default grid and it
    is rebuilt at every evolve() call, which is what made the gamma fit of 3.5
    grow to several GB.
    """
    band = diags([1.0, 1.0, 1.0], [-1, 0, 1], shape=(n, n))
    return kron(np.ones((4, 4)), band, format="csr")


def evolve(p, grid, y0, t_end, times, source=None, max_step=np.inf,
           polygonisation=None):
    """Integrate from 0 to t_end, output at `times` (stiff BDF, adaptive)."""
    rhs = _right_hand_side(p, grid, g_offset(p.c), source, polygonisation)
    sol = solve_ivp(rhs, (0.0, t_end), y0, method="BDF", t_eval=times, max_step=max_step,
                    jac_sparsity=_sparsity(grid.cells), rtol=1e-5, atol=1e-8)
    if not sol.success:
        raise RuntimeError(sol.message)
    return sol


# ---------------------------------------------------------------------------
# A.5  Initial condition, Sec. 3.2
# ---------------------------------------------------------------------------

_RELAXED = {}                       # cache, see initial_state()


def initial_state(p, grid, delta_theta_deg, eta0=0.99):
    """Relaxed GB without dislocations.

    eta = eta0 everywhere, theta a sharp step of height delta_theta at L/2,
    then relaxed with rho = 0 (tau_eta = 1e2 f0 t0 to get there faster).

    CACHED, because this relaxation is the single most repeated computation in
    the script: run_case() does one per case, gamma_curve() one per angle and
    per c of the fit, and every step of the c3 bisection one more - all of them
    the same profile.  rho = 0 here, so f_eta4 = 0 and c3, C_D, lambda, G and b
    do not enter it; only the parameters in `key` do.  A copy is returned
    because the callers write rho into it.
    """
    key = (p.f0, p.nu, p.alpha, p.mu, p.c, p.tau_hat, p.t0,
           eta0, delta_theta_deg, grid.length, grid.cells)
    if key not in _RELAXED:
        n = grid.cells
        # >>> UO2 INPUT: delta_theta_deg is the misorientation of the PARENT
        #     grains (a fixed HAGB for UO2), imposed only here.  Every later
        #     orientation, including the nucleus', comes from Eq. (34).
        theta = math.radians(delta_theta_deg) * 0.5 * (
            1.0 + np.tanh((grid.x - 0.5 * grid.length) / (2.0 * grid.dx)))
        y0 = np.concatenate([np.full(n, eta0), theta, np.zeros(n), np.zeros(n)])
        t_relax = 2.0e4 * p.t0
        relax = evolve(replace(p, tau_eta=1.0e2), grid, y0, t_relax, [t_relax])
        _RELAXED[key] = relax.y[:, -1]
    return _RELAXED[key].copy()


# ---------------------------------------------------------------------------
# A.6  Post-processing
# ---------------------------------------------------------------------------

def gb_energy(p, grid, y):
    """GB energy [J/m^2] of a profile without dislocations, Eq. (15) line 1.

    The bulk contributes nothing (V(1) = 0, no gradients), so the integral
    over the domain is the energy per unit GB area; Fig. 2 calibrates it.
    """
    n, dx = grid.cells, grid.dx
    eta, theta = y[:n], y[n:2 * n]
    g, _ = coupling_g(eta, p.c, g_offset(p.c))
    deta = np.diff(eta) / dx
    dtheta = np.diff(theta) / dx
    g_face = 2.0 * g[:-1] * g[1:] / (g[:-1] + g[1:])
    density = p.f0 * p.alpha * 0.5 * (1.0 - eta) ** 2
    faces = p.f0 * (0.5 * p.nu ** 2 * deta ** 2 + p.mu ** 2 * g_face * dtheta ** 2)
    return float(density.sum() * dx + faces.sum() * dx)


def critical_radius(p, gamma, rho):
    """Capillarity, absent in 1D (limitation 1).

    A nucleus of radius r survives if the stored energy beats the curvature
    pressure, lambda/2 G b^2 rho > 2 gamma / r.
    """
    return 2.0 * gamma / (p.stored_energy_per_rho * rho)


def state_at(outcome, t):
    """Column of the solution closest to time t."""
    return outcome.solution.y[:, int(np.argmin(np.abs(outcome.solution.t - t)))]


# ---------------------------------------------------------------------------
# A.8  Verification of the mathematics (docstring 6)
#
#      No calibration, no material data, no experiment: only the questions a
#      variational model has to answer about itself.  verify_mathematics()
#      runs them; the numbers it prints are quoted in docstring 6.
# ---------------------------------------------------------------------------

def total_energy(p, grid, y, polygonisation=None, face="harmonic"):
    """Psi [J/m^2]: the whole functional, Eq. (P2') plus the stored energy.

        Psi = int [ f0 (alpha V + nu^2/2 (d eta/dx)^2)
                    + f0 mu^2 g(eta) ( b^2 rho_wall^2 + (d theta/dx - kappa^p)^2 )
                    + phi(eta) lambda/2 G b^2 rho_free ] dx

    gb_energy() is the same thing for a profile without dislocations and
    without walls; this one is what the dynamics has to decrease.

    face: how g is averaged at the faces of the orientation term.  The theta
    flux and gb_energy() use the HARMONIC mean, the eta force is written as if
    each cell carried the mean of its two faces (ARITHMETIC).  The two agree to
    O(dx) - verify_mathematics() measures it - and both are offered so the
    comparison can be made.

    Accepts a complex state: every expression here is analytic, which is what
    makes the complex step of _energy_gradient() possible.
    """
    n, dx = grid.cells, grid.dx
    eta, theta = y[:n], y[n:2 * n]
    rho_free, rho_wall = y[2 * n:3 * n], y[3 * n:]
    offset = g_offset(p.c)
    if np.iscomplexobj(y):
        g = _g_raw(eta, p.c) + offset            # below the cutoff by construction
    else:
        g, _ = coupling_g(eta, p.c, offset)
    active = polygonisation is not None and polygonisation.mobility > 0.0
    sign = 1.0
    if active and polygonisation.sign is not None:
        sign = polygonisation.sign

    cell = (p.f0 * p.alpha * 0.5 * (1.0 - eta) ** 2
            + coupling_phi(eta, p) * p.stored_energy_per_rho * rho_free)
    elastic = np.diff(theta) / dx
    if active:
        kappa_p = plastic_curvature(rho_wall, sign, p)
        # the wall self-energy is frozen above ETA_WALL_CUTOFF, as in the force
        if np.iscomplexobj(y):
            e_wall = np.where(eta.real < ETA_WALL_CUTOFF, eta, ETA_WALL_CUTOFF + 0j)
            g_wall = _g_raw(e_wall, p.c) + offset
        else:
            g_wall, _ = wall_coupling(eta, p, offset)
        cell = cell + p.f0 * p.mu ** 2 * g_wall * (p.burgers * rho_wall) ** 2
        elastic = elastic - 0.5 * (kappa_p[:-1] + kappa_p[1:])
    if face == "harmonic":
        g_face = 2.0 * g[:-1] * g[1:] / (g[:-1] + g[1:])
    else:
        g_face = 0.5 * (g[:-1] + g[1:])
    faces = p.f0 * (0.5 * p.nu ** 2 * (np.diff(eta) / dx) ** 2
                    + p.mu ** 2 * g_face * elastic ** 2)
    return (cell.sum() + faces.sum()) * dx


_FIELDS = {"eta": 0, "theta": 1, "rho_free": 2, "rho_wall": 3}


def _energy_gradient(p, grid, y, polygonisation, field, face, step=1e-30):
    """d Psi / d y_i per unit volume, by the COMPLEX STEP.

    Im[Psi(y + i h e_k)] / h is exact to machine precision: there is no
    difference of two nearly equal numbers, which matters here because the wall
    term carries g(eta), and g reaches 1e12 at the cutoff - a finite difference
    of Psi loses every digit of the small terms against it.
    """
    n = grid.cells
    start = _FIELDS[field] * n
    out = np.zeros(n)
    base = y.astype(complex)
    for i in range(n):
        probe = base.copy()
        probe[start + i] += 1j * step
        out[i] = total_energy(p, grid, probe, polygonisation, face).imag / step
    return out / grid.dx


def _face_g_elastic(p, grid, y, polygonisation):
    """(g e)_f = g_face (d theta/dx - kappa^p) at the n + 1 faces, as the rhs builds it."""
    n, dx = grid.cells, grid.dx
    g, _ = coupling_g(y[:n], p.c, g_offset(p.c))
    dtheta = np.zeros(n + 1)
    dtheta[1:-1] = np.diff(y[n:2 * n]) / dx
    g_face = np.zeros(n + 1)
    g_face[1:-1] = 2.0 * g[:-1] * g[1:] / (g[:-1] + g[1:])
    elastic = dtheta
    if polygonisation is not None and polygonisation.mobility > 0.0:
        sign = 1.0 if polygonisation.sign is None else polygonisation.sign
        kappa_p = plastic_curvature(y[3 * n:], sign, p)
        face = np.zeros(n + 1)
        face[1:-1] = 0.5 * (kappa_p[:-1] + kappa_p[1:])
        elastic = dtheta - face
    return g_face * elastic


def _test_state(grid, seed=3):
    """A smooth non-equilibrium state with all four fields switched on.

    eta is kept inside [0.35, 0.85]: at eta -> 1 the wall term is g(eta) ~ 1e12
    times everything else (walls cannot be paid for in a perfect crystal, 3.6),
    which is the physics but leaves no digits for a comparison.
    """
    x = grid.x / grid.length
    return np.concatenate([
        0.6 + 0.25 * np.cos(2.0 * np.pi * x),
        math.radians(20.0) * 0.5 * (1.0 + np.tanh((x - 0.5) / 0.05)),
        5.0e14 * (1.0 + 0.3 * np.sin(4.0 * np.pi * x)),
        2.0e13 * (1.0 + 0.5 * np.cos(6.0 * np.pi * x)) ** 2])


def _relaxation(p, grid, polygonisation, rho0=5.0e14, c_d=None, samples=40):
    """Relaxed GB loaded with a uniform rho_free, then left alone: no source."""
    n = grid.cells
    p = p if c_d is None else replace(p, c_d=c_d)
    y = initial_state(p, grid, 20.0)
    y[2 * n:3 * n] = rho0
    t_end = 1.0e3 * p.t0
    return p, evolve(p, grid, y, t_end, np.linspace(0.0, t_end, samples),
                     polygonisation=polygonisation)


def verify_mathematics(p=None, cells=(60, 120), verbose=True):
    """M1-M4 of docstring 6: is the model the gradient flow it claims to be?

    Returns True if every check passes.  Nothing here uses a calibration, a
    burnup or an experimental number: it asks only whether the implemented
    right-hand side is the variational derivative of Eq. (P2'), whether that
    energy decreases, whether the transfer conserves dislocations and whether
    it dissipates.  A failure is a bug in the equations; a model that passes can
    still be the wrong model.
    """
    ok = True

    def check(name, cond, detail):
        nonlocal ok
        ok &= bool(cond)
        if verbose:
            print(f"[{'PASS' if cond else 'FAIL'}] {name}: {detail}")

    p = p or replace(uo2_parameters(), c3=1.0)
    subgrain = 2.0 * hbs_state(HBS_ONSET_BURNUP, REFERENCE_TEMPERATURE).subgrain_radius_m

    # --- M1: the right-hand side against d Psi / d field -------------------
    harmonic = {}
    for n_cells in cells:
        grid = Grid(cells=n_cells)
        n = grid.cells
        pp = Polygonisation(mobility=mobility_from_timescale(3.0, 1.0e14, p),
                            sign=np.sign(subgrain_pattern(grid, subgrain)),
                            subgrain=subgrain)
        y = _test_state(grid)
        d = _right_hand_side(p, grid, g_offset(p.c), None, pp)(0.0, y)
        g, _ = coupling_g(y[:n], p.c, g_offset(p.c))
        # the equations are tau * d(field)/dt = -d Psi / d field
        force_eta = -(p.tau_eta * p.f0 * p.t0) * d[:n]
        force_theta = -(p.tau_hat * p.f0 * p.t0) * g * d[n:2 * n]

        exact = _energy_gradient(p, grid, y, pp, "eta", "arithmetic")
        check(f"M1 eta = dPsi/deta at {n_cells} cells",
              np.abs(exact - force_eta).max() / np.abs(exact).max() < 1e-12,
              f"{np.abs(exact - force_eta).max() / np.abs(exact).max():.1e} "
              f"(walls included: Eq. (P5))")

        # Eq. (34) of [T26] carries no factor 2 against Eq. (15): the theta
        # mobility is half the variational one, which is a convention, not an
        # error - the flow still descends the same Psi.
        exact = _energy_gradient(p, grid, y, pp, "theta", "harmonic")
        check(f"M1 theta = dPsi/dtheta / 2 at {n_cells} cells",
              np.abs(0.5 * exact - force_theta).max() / np.abs(0.5 * exact).max() < 1e-12,
              f"{np.abs(0.5 * exact - force_theta).max() / np.abs(0.5 * exact).max():.1e} "
              f"(Eq. 34 convention)")

        exact = _energy_gradient(p, grid, y, pp, "rho_free", "harmonic")
        mu_f = tangle_potential(y[:n], p)
        check(f"M1 mu_free at {n_cells} cells",
              np.abs(exact - mu_f).max() / np.abs(exact).max() < 1e-12,
              f"{np.abs(exact - mu_f).max() / np.abs(exact).max():.1e}")

        # mu_wall: exact in the interior.  In the two zero-flux cells the
        # discrete derivative keeps ONE face while wall_potential() assumes two,
        # so it is 4/3 too large there - a boundary artefact, limitation 8.
        mu_w = wall_potential(y[:n], y[3 * n:], _face_g_elastic(p, grid, y, pp),
                              pp.sign, p, g_offset(p.c))
        exact = _energy_gradient(p, grid, y, pp, "rho_wall", "harmonic")
        worst = np.abs(exact - mu_w).max() / np.abs(exact).max()
        check(f"M1 mu_wall at {n_cells} cells", worst < 1e-12, f"{worst:.1e}")

        exact = _energy_gradient(p, grid, y, pp, "eta", "harmonic")
        harmonic[n_cells] = np.abs(exact - force_eta).max() / np.abs(exact).max()

    # the eta force and the theta flux average g at the faces differently; the
    # difference has to vanish with dx, or the scheme is not consistent
    coarse, fine = cells[0], cells[-1]
    check("M1 arithmetic vs harmonic face average is O(dx)",
          harmonic[fine] < 0.75 * harmonic[coarse],
          f"{harmonic[coarse]:.1e} at {coarse} cells -> {harmonic[fine]:.1e} at {fine}")

    # --- M2 to M4: the dynamics --------------------------------------------
    grid = Grid(cells=cells[-1])
    n = grid.cells
    pp = Polygonisation(mobility=mobility_from_timescale(3.0, 1.0e14, p),
                        sign=np.sign(subgrain_pattern(grid, subgrain)),
                        subgrain=subgrain)
    for tag, transfer in (("transfer off", None), ("transfer on", pp)):
        q, sol = _relaxation(p, grid, transfer, c_d=10.0)
        psi = np.array([total_energy(q, grid, sol.y[:, k], transfer)
                        for k in range(sol.t.size)])
        rise = float(np.diff(psi).max())
        check(f"M2 Psi does not increase, {tag}", rise <= 0.0,
              f"{psi[0]:.4f} -> {psi[-1]:.4f} J/m^2, largest step {rise:+.1e}")

    q, sol = _relaxation(p, grid, pp, c_d=0.0)
    total = sol.y[2 * n:3 * n].sum(axis=0) + sol.y[3 * n:].sum(axis=0)
    moved = sol.y[3 * n:].sum(axis=0)[-1] / total[0]
    drift = abs(total[-1] - total[0]) / total[0]
    check("M3 the transfer conserves rho_free + rho_wall",
          drift < 1e-12 and moved > 1e-3 and sol.y[3 * n:].min() >= 0.0,
          f"drift {drift:.1e}, {100 * moved:.1f}% moved into the walls, "
          f"min rho_wall {sol.y[3 * n:].min():.1e}")

    worst, capped = 0.0, 0
    for k in range(sol.t.size):
        y = sol.y[:, k]
        faces = _face_g_elastic(q, grid, y, pp)
        drive = polygonisation_drive(y[:n], y[3 * n:], faces, pp.sign, q, g_offset(q.c))
        rate = polygonisation_rate(y[:n], y[3 * n:], faces, pp.sign, y[2 * n:3 * n],
                                   pp, q, g_offset(q.c))
        worst = min(worst, float((rate * drive).min()))
        capped += int((wall_saturation(y[3 * n:], drive, pp, q) < 1.0).sum())
    check("M4 the transfer dissipates, ceiling included", worst >= 0.0,
          f"min R dG = {worst:.1e}, ceiling active in {capped} cell-times")
    return ok


# ###########################################################################
#
#   PART B - Cu: REPRODUCTION OF [T26], SECTION 3.2 (Figs. 1, 2, 4, 5, 6)
#
#   run_case() implement the paper's step protocol: a uniform rho switched on 
#   at t = 0.  Cu ONLY.  For UO2 the dislocations are produced by irradiation 
#   (Part C.2).
#
# ###########################################################################

CU = PhaseFieldParameters()                 # Table 1, c1, c2, c3 of Fig. 4
CU_RHO0 = 2.5e15                            # m^-2   Sec. 3.2
CU_DELTA_THETA = 15.0                       # deg    Fig. 4
# Cu keeps the geometry of [T26] Fig. 3: the 10 um half period at THEIR nu = 1 um,
# which is the same 10 nu as the UO2 Grid() default.  400 cells, where gamma is
# converged (NUMERICS) and every Cu result of selftest() is identical to the one
# at 1000 cells at half the cost.
CU_GRID = Grid(length=10.0e-6, cells=400)


@dataclass
class Outcome:
    delta_theta_deg: float
    rho0: object                    # m^-2, scalar or (left, right) as in Fig. 6
    eta_eq: float                   # Eq. (42), at the larger rho
    eta_gb0: float                  # GB depth before rho is switched on
    nucleated: bool
    t_recovery: float               # s, first time rho(centre) < 0.1 rho0 (nan: never)
    recrystallised: float           # fraction of cells with rho < 0.1 rho0 at t_end
    theta_centre_deg: float
    initial: np.ndarray             # relaxed GB, before rho is switched on
    solution: object
    event: str = "none"             # "nucleus", "migration" (SIBM) or "none"
    # OUTPUT of the functional: misorientation of the nucleus to the closer
    # parent grain at t_end, min(theta_c, Delta theta - theta_c) (nan: no nucleus)
    nucleus_misorientation_deg: float = math.nan


def nucleus_misorientation(theta_centre_deg, delta_theta_deg):
    """Misorientation of the new grain to the closer of the two parents."""
    return min(theta_centre_deg, delta_theta_deg - theta_centre_deg)


def grain_statistics(p, grid, y, rho_ref):
    """(recrystallised fraction, number of grains, number of new grains).

    Grain: a run of cells with eta > c2.  New grain: a grain with mean
    rho < 0.1 rho_ref (dislocation free).  Recrystallised fraction: cells
    with eta > c2 and rho < 0.1 rho_ref.
    """
    n = grid.cells
    eta, rho = y[:n], y[2 * n:3 * n]
    bulk = eta > p.c2
    fresh = bulk & (rho < 0.1 * rho_ref) if rho_ref > 0 else np.zeros(n, bool)
    edges = np.flatnonzero(np.diff(np.concatenate([[0], bulk.astype(int), [0]])))
    grains = list(zip(edges[::2], edges[1::2]))
    new = sum(1 for a, b in grains if rho_ref > 0 and rho[a:b].mean() < 0.1 * rho_ref)
    return float(fresh.mean()), len(grains), new


def classify_event(p, grid, sol, rho_ref, delta_theta_deg):
    """(event, index of the event, nucleus misorientation [deg]) - [T26]'s test.

    rho_ref: the density the recovery is measured against, a scalar (run_case,
    where rho0 is switched on at t = 0) or one value per output time
    (run_irradiation, where it is what the source has produced so far).

    This is the criterion of [T26] rather than a set of thresholds invented here
    (limitation 7).  Their Sec. 3.2.2 describes the birth of a nucleus as the
    recovery of rho at the GB centre followed by "two separate grain boundaries
    ... created from the single expanding boundary, which move away from each
    other"; Sec. 3.2.3 states the same thing on the profiles, "the BULGE of
    order parameter eta forming at the same position [as the recovery], and the
    corresponding crystalline bulk, i.e. grad theta = 0, region forming".  Three
    things have to be true, and each one throws out a different impostor:

      1. rho at the centre falls below a tenth of rho_ref.  The nucleus is
         "a dislocation-free new grain"; take the FIRST time this happens, which
         is when it is born.
      2. at that moment the two grain centres (the zero-flux walls, the last
         places to disorder) are still crystalline, eta > c2.  A nucleus is born
         INSIDE a parent grain.  This is what separates [T26]'s Delta theta = 5
         deg case, where rho is recovered at t = 4e3 s while the parents are
         still there, from their 2.5 deg case, where "the initially localized
         gradient in lattice orientation becomes more and more diffuse until it
         disappears" and eta is a uniform eta_eq before anything recovers.
      3. eta at the centre later rises above c2 with eta DIPPING below c2 on
         both sides of it: the bulge and the two new boundaries.  Without this
         the boundary has merely moved.

    Condition 3 is written on the profile rather than on the grain count of
    grain_statistics() on purpose: at Delta theta = 5 deg the parents are below
    c2 by the time the bulge closes, so there are never three finished grains to
    count, only the two dips.

    The other two outcomes:

      migration   recovered, but no bulge with dips on both sides, and theta at
                  the centre has ended on one of the parents: the boundary moved
                  (SIBM), Eq. (43) with rho unequal across it.
      collapse    recovered with condition 2 already lost - the whole domain had
                  disordered into the uniform eta_eq state first and then
                  re-crystallised as ONE grain at the mean orientation.  rho is
                  recovered, so a test that looks only at rho and at theta calls
                  this a nucleus; it is not one, and it is what a domain shorter
                  than the widened boundary always gives (limitation 8).
    """
    n, centre = grid.cells, grid.cells // 2
    ref = np.broadcast_to(np.asarray(rho_ref, dtype=float), (sol.t.size,))
    eta, rho_c = sol.y[:n], sol.y[2 * n + centre]

    recovered = np.flatnonzero((ref > 0.0) & (rho_c < 0.1 * ref))
    if not recovered.size:
        return "none", -1, math.nan
    k = int(recovered[0])

    born_in_a_grain = min(eta[0, k], eta[n - 1, k]) > p.c2
    bulge = (eta[centre] > p.c2) & (eta[:centre].min(axis=0) < p.c2) \
        & (eta[centre + 1:].min(axis=0) < p.c2)
    grown = np.flatnonzero(bulge)
    grown = grown[grown >= k]
    if born_in_a_grain and grown.size:
        # Eq. (43) is about the SETTLED orientation of the new grain, so read
        # theta at the last time the nucleus is still a grain between two
        # boundaries, not at the instant it is born
        theta_nuc = math.degrees(sol.y[n + centre, int(grown[-1])])
        return "nucleus", k, nucleus_misorientation(theta_nuc, delta_theta_deg)

    theta_end = math.degrees(sol.y[n + centre, -1])
    if not 0.1 * delta_theta_deg < theta_end < 0.9 * delta_theta_deg:
        return "migration", k, math.nan
    return "collapse", k, math.nan


def run_case(p, delta_theta_deg, rho0, t_end=1.0e4, grid=None, samples=(), every=50.0):
    """Step protocol of Sec. 3.2: relax the GB, switch on rho0, evolve.

    t_end, samples, every are in units of t0 (= seconds for Cu); the
    returned solution and t_recovery are in seconds.

    rho0 is uniform (Figs. 4, 5) or a pair (rho_left, rho_right) with a jump
    at the GB (Fig. 6); the orientation of the nucleus then follows Eq. (43).

    The event is read off the profiles the way [T26] read off theirs
    (Secs. 3.2.2-3.2.3): see classify_event().
    """
    grid = grid or Grid()
    n, centre = grid.cells, grid.cells // 2
    y = initial_state(p, grid, delta_theta_deg)
    eta_gb0 = y[:n].min()
    relaxed = y.copy()

    # >>> UO2 INPUT (step protocol): rho0 is hbs_state(bu).rho_tot (or rho_free)
    rho_left, rho_right = rho0 if isinstance(rho0, tuple) else (rho0, rho0)
    y[2 * n:3 * n] = np.where(grid.x < 0.5 * grid.length, rho_left, rho_right)
    rho_scale = max(rho_left, rho_right)

    t_end, every = t_end * p.t0, every * p.t0
    samples = np.asarray(samples, dtype=float) * p.t0
    times = np.union1d(np.arange(every, t_end + 0.5 * every, every), samples)
    sol = evolve(p, grid, y, t_end, times)

    # the thresholds below use the larger of the two densities
    rho0 = rho_scale
    rho_c = sol.y[2 * n + centre]
    hit = np.flatnonzero(rho_c < 0.1 * rho0) if rho0 > 0 else np.array([], int)
    t_recovery = float(sol.t[hit[0]]) if hit.size else math.nan

    event, _, misorientation = classify_event(p, grid, sol, rho0, delta_theta_deg)
    recrystallised = grain_statistics(p, grid, sol.y[:, -1], rho0)[0]
    theta_c = math.degrees(sol.y[n + centre, -1])
    return Outcome(delta_theta_deg, (rho_left, rho_right) if rho_left != rho_right else rho_left,
                   eta_equilibrium(rho0, p), eta_gb0, event == "nucleus", t_recovery,
                   recrystallised, theta_c, relaxed, sol, event, misorientation)

FIT_ANGLES = (2.5, 5.0, 7.5, 10.0, 15.0, 20.0, 25.0, 30.0)


def gamma_curve(p, angles, grid=None):
    """gamma(Delta theta) [J/m^2] of the relaxed 1D profiles, Fig. 2."""
    grid = grid or Grid()
    return np.array([gb_energy(p, grid, initial_state(p, grid, a)) for a in angles])


def fit_interface_parameters(targets, angles=FIT_ANGLES,
                             c_grid=(3.0, 5.0, 7.0, 9.0, 12.0, 16.0, 20.0, 26.0,
                                     30.0, 34.0, 38.0, 45.0),
                             base=None, grid=None):
    """(f0, c) reproducing each target curve [J/m^2] at `angles`, docstring 3.5.

    Eq. (15) is linear in f0 at fixed nu, alpha, mu, so for each c the optimal
    f0 is in closed form, f0 = f0_base * (gamma . target) / (gamma . gamma),
    and only the SHAPE parameter c has to be scanned; c is the Read-Shockley
    coefficient of the log term of Eq. (35).

    The grid must span a wide range because the optimal c moves with nu: the
    shape of gamma(Delta theta) is set by the PRODUCT of the two (nu = 1, 1.5
    and 2.5 um give c = 7, 16 and 34).  A grid stopping at 12, as the first
    version of this fit did, silently returns its own last point.

    targets: {label: gamma at `angles`}.  The 1D profiles are relaxed once per
    c and shared by all the targets - that relaxation is the whole cost.

    Returns {label: (f0, c, rms, table)}, table = (c, f0, rms, max relative
    error) for every c of c_grid.
    """
    base = base or CU
    curves = {}
    for c in c_grid:
        curves[c] = gamma_curve(replace(base, c=c), angles, grid)

    fits = {}
    for label, target in targets.items():
        target = np.asarray(target, dtype=float)
        table = []
        for c in c_grid:
            gamma = curves[c]
            scale = float(gamma @ target / (gamma @ gamma))
            residual = scale * gamma - target
            table.append((c, base.f0 * scale, float(np.sqrt(np.mean(residual ** 2))),
                          float(np.max(np.abs(residual / target)))))
            print(f"  {label}: c = {c:5.2f}, f0 = {table[-1][1] / 1e3:7.1f} kPa, "
                  f"rms = {table[-1][2]:.4f} J/m^2, max = {100 * table[-1][3]:4.1f} %")
        best = min(table, key=lambda row: row[2])
        print(f"  -> {label}: f0 = {best[1] / 1e3:.1f} kPa, c = {best[0]:g} "
              f"(rms = {best[2]:.4f} J/m^2, max = {100 * best[3]:.1f} %)")
        fits[label] = (best[1], best[0], best[2], table)
    return fits


def plot_gb_energy(p, path, angles=(1.0, 2.5, 5.0, 7.5, 10.0, 15.0, 20.0, 25.0, 30.0),
                   targets=None, grid=None):
    """Fig. 2 of [T26]: GB energy of the relaxed 1D profile vs misorientation.

    targets: optional dict {label: gamma at `angles`} drawn as reference, e.g.
    the UO2 curves of uo2_gb_energy (docstring 3.5).
    """
    plt = _pyplot()
    grid = grid or Grid()
    gammas = gamma_curve(p, angles, grid)
    for a, gamma in zip(angles, gammas):
        print(f"  Delta theta = {a:5.1f} deg: gamma = {gamma:.3f} J/m^2")
    fig, ax = plt.subplots(figsize=(5.4, 3.8))
    for label, values in (targets or {}).items():
        ax.plot(angles, values, "--", lw=1.0, label=label)
    ax.plot(angles, gammas, "x-", color="k",
            label=rf"1D, $f_0$ = {p.f0 / 1e3:.0f} kPa, $c$ = {p.c:g}")
    ax.axvline(30.0, color="grey", lw=0.8, ls=":")
    ax.set_xlabel(r"$\Delta\theta$ [deg]")
    ax.set_ylabel(r"$\gamma$ [J/m$^2$]")
    ax.set_title(f"Fig. 2: GB energy, c = {p.c:g}", fontsize=9)
    ax.legend(fontsize=7)
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")


def _plot_family(p, cases, times, path, title):
    """eta, theta, rho at `times` for several (label, delta_theta, rho0) cases."""
    plt = _pyplot()
    grid = CU_GRID
    n = grid.cells
    x = grid.x * 1e6
    styles = ["-", "--", "-.", ":"]
    colours = plt.cm.tab10(np.arange(len(cases)))
    fig, axes = plt.subplots(1, 3, figsize=(15, 4))
    for (label, angle, rho0), colour in zip(cases, colours):
        out = run_case(p, angle, rho0, t_end=times[-1], grid=grid, samples=times)
        print(f"  {label}: {out.event}, theta_centre = "
              f"{out.theta_centre_deg:.2f} deg, nucleus misorientation = "
              f"{out.nucleus_misorientation_deg:.2f} deg")
        for t, style in zip(times, styles):
            y = state_at(out, t * p.t0)
            axes[0].plot(x, y[:n], style, color=colour,
                         label=label if t == times[0] else None)
            axes[1].plot(x, np.degrees(y[n:2 * n]), style, color=colour)
            axes[2].plot(x, y[2 * n:3 * n], style, color=colour)
    axes[0].set_ylabel(r"$\eta$")
    axes[1].set_ylabel(r"$\theta$ [deg]")
    axes[2].set_ylabel(r"$\rho$ [m$^{-2}$]")
    for ax in axes:
        ax.set_xlabel(r"$x$ [µm]")
    axes[0].legend(fontsize=7)
    fig.suptitle(title + "   lines: " + ", ".join(
        f"{s} t={t:g} s" for t, s in zip(times, styles)), fontsize=9)
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")

def selftest():
    """Cu checks against [T26], plus the UO2 extensions; True if all pass.

    The [T26] part must stay untouched by everything added for UO2: the
    polygonisation of 3.6 is off (rho_wall = 0, kappa^p = 0), so the eta force
    reduces to Eq. (32) and the source to the uniform one.
    """
    ok = True

    def check(name, cond, detail):
        nonlocal ok
        ok &= bool(cond)
        print(f"[{'PASS' if cond else 'FAIL'}] {name}: {detail}")

    rho0 = CU_RHO0
    eq = eta_equilibrium(rho0, CU)
    check("Eq. (42), Cu", abs(eq - 0.579) < 1e-3, f"eta_eq = {eq:.4f} (paper 0.579)")

    g, _ = coupling_g(np.linspace(0, ETA_CUTOFF, 1001), CU.c, g_offset(CU.c))
    check("g >= 0.01", g.min() > 0.0099, f"min g = {g.min():.4f}")

    # Sec. 3.2.1: without recovery the GB only widens to eta_eq
    no_rec = run_case(replace(CU, c_d=0.0), 15.0, rho0, t_end=1.0e4, grid=CU_GRID)
    eta_c = no_rec.solution.y[CU_GRID.cells // 2, -1]
    check("Cu 15 deg, C_D = 0: GB widens to eta_eq", abs(eta_c - eq) < 0.03,
          f"eta(centre) = {eta_c:.3f}, no nucleus = {not no_rec.nucleated}")

    # Sec. 3.2.2: with recovery a nucleus forms
    rec = run_case(CU, 15.0, rho0, t_end=1.0e4, grid=CU_GRID)
    check("Cu 15 deg, C_D = 100: nucleus", rec.nucleated and rec.event == "nucleus",
          f"{rec.event}, theta_centre = {rec.theta_centre_deg:.2f} deg, "
          f"recrystallised {rec.recrystallised:.2f}")

    # Fig. 2
    gamma = gb_energy(CU, CU_GRID, no_rec.initial)
    check("Cu 15 deg GB energy vs Fig. 2", 0.6 < gamma < 0.9, f"gamma = {gamma:.3f} J/m^2 (Fig. 2: ~0.75)")

    # Fig. 5: misorientation threshold.  2.5 deg: [T26] report that rho is never
    # recovered; here the orientation gradient does finally disappear and the
    # whole domain recovers at once, which classify_event() calls a collapse -
    # not a nucleus, which is the statement Fig. 5 makes (limitation 7).
    for angle, expected, paper in [(10.0, True, "immediate"),
                                   (5.0, True, "delayed, ~4e3 s"),
                                   (2.5, False, "never")]:
        out = run_case(CU, angle, rho0, t_end=1.0e4, grid=CU_GRID)
        check(f"Cu {angle:g} deg: nucleus = {expected} (paper: {paper})",
              out.nucleated == expected,
              f"{out.event}, t_recovery = {out.t_recovery:g} s")

    # Docstring 3.5: with UO2_F0, UO2_C and UO2_NU the 1D gamma(Delta theta) must sit
    # on the random-axis / random-plane average of [Z21].  The targets below
    # are that average (uo2_gb_energy.target), frozen here as a regression
    # guard so that selftest() does not depend on the sampled table.
    # The tolerance is the residual of that fit, not a wish: at nu = UO2_NU the
    # fit is rms 0.015 J/m^2 and max 3.9% (0.009 and 2.2% at the Cu nu = 1 um),
    # so 0.05 J/m^2 is "on the target curve to within the fit", and anything
    # larger means f0, c or nu have moved.
    uo2 = replace(CU, f0=UO2_F0, c=UO2_C, nu=UO2_NU)
    for angle, expected, tol in [(5.0, 0.797, 0.05), (15.0, 1.368, 0.05),
                                 (30.0, 1.531, 0.05)]:
        gamma = gb_energy(uo2, Grid(), initial_state(uo2, Grid(), angle))
        check(f"UO2 {angle:g} deg GB energy vs [Z21]", abs(gamma - expected) < tol,
              f"gamma = {gamma:.3f} J/m^2 (target {expected:.3f})")

    # Docstring 3.6: the ceiling of the walls, Eq. (P8).  Theta_wall = b rho D,
    # so rho_cap is the density that carries exactly Theta_HAGB over a subgrain.
    pp = Polygonisation(mobility=1.0, subgrain=0.5e-6)
    q = replace(CU, burgers=BURGERS)
    rho_cap = math.radians(THETA_HAGB) / (q.burgers * pp.subgrain)
    rho_w = np.array([0.0, 0.5, 1.0, 2.0]) * rho_cap
    absorbing = wall_saturation(rho_w, np.ones(4), pp, q)        # dG > 0
    dissolving = wall_saturation(rho_w, -np.ones(4), pp, q)      # dG < 0
    check("ceiling: absorption stops at Theta_HAGB",
          abs(absorbing[0] - 1.0) < 1e-12 and abs(absorbing[1] - 0.5) < 1e-9
          and absorbing[2] == 0.0 and absorbing[3] == 0.0,
          f"s_cap = {np.round(absorbing, 3)} at Theta_wall/Theta_HAGB = 0, 0.5, 1, 2")
    check("ceiling: dissolution is never capped", bool(np.all(dissolving == 1.0)),
          f"s_cap = {np.round(dissolving, 3)} for dG < 0")

    return ok


# ###########################################################################
#
#   PART C - UO2: COUPLING TO THE HBS LANDAU MODEL
#
# ###########################################################################

# ---------------------------------------------------------------------------
# C.1  Inputs from the Landau model and parameters
# ---------------------------------------------------------------------------

RHO_KEYS = {"tot": "rho_tot",       # Nogita-Une, Eq. (1) of [HBS]
            "free": "rho_free"}     # Landau, Eq. (5) of [HBS]


def dislocation_density(burnup, temperature=REFERENCE_TEMPERATURE, which="tot"):
    """rho(bu) [m^-2] that feeds the SSD term (see docstring 3.1)."""
    # >>> UO2 INPUT: dislocation density from hbs_state
    return getattr(hbs_state(burnup, temperature), RHO_KEYS[which])


SECONDS_PER_YEAR = 365.25 * 86400.0


def uo2_parameters(temperature=REFERENCE_TEMPERATURE, **changes):
    """UO2 parameters: Table 1 of [T26] with f0, c, nu, G, b, t0, C_D and c3 changed.

    f0 = UO2_F0, c = UO2_C (run configuration): the GB energy refitted on
    [Z21], docstring 3.5.  nu = UO2_NU: the diffuse width set on the GB layer
    of [ON25], docstring 3.5; f0 and c are refitted AT that nu.  alpha, mu, c1,
    c2, lambda and the tau stay at the Cu values of Table 1 (limitation 5).
    t0 = UO2_T0 (run configuration): the phase-field clock of UO2 GBs.
    C_D = UO2_C_D (run configuration): weaker recovery than Cu (docstring 3.4).

    c3 = UO2_C3 (docstring 3.3): the onset calibration, i.e. what calibrate_c3()
    returns for the default configuration.  CALIBRATE_C3 = True recomputes it
    instead of trusting the constant.  G here is the one at bu_sat; callers
    replace it with G(bu) (uo2_at).
    """
    _, _, bu_saturation = regime_boundaries()
    state = hbs_state(bu_saturation, temperature)
    # >>> UO2 INPUT: GB energy, f0 and c fitted on [Z21] at nu = UO2_NU
    #     (docstring 3.5), c3 from the onset calibration (docstring 3.3)
    return replace(CU, f0=UO2_F0, c=UO2_C, nu=UO2_NU,
                   shear_modulus=state.shear_modulus, burgers=BURGERS, t0=UO2_T0,
                   c_d=UO2_C_D, c3=UO2_C3, **changes)


# ---------------------------------------------------------------------------
# C.1b  GB energy: the [Z21] targets and the fit of docstring 3.6
# ---------------------------------------------------------------------------

UO2_GAMMA_LABELS = {"random": "[Z21], random axis and plane",
                    "st110": r"[Z21], sym. tilt $\langle 110\rangle$",
                    "st100": r"[Z21], sym. tilt $\langle 100\rangle$"}


def uo2_gamma_targets(angles=FIT_ANGLES, which=UO2_GAMMA_LABELS):
    """{label: gamma [J/m^2]} from uo2_gb_energy, for plots and the fit.

    The table is built by `python3 uo2_gb_energy.py` and the
    rest of this script does not need it.
    """
    import uo2_gb_energy
    return {UO2_GAMMA_LABELS[key]: uo2_gb_energy.target(angles, key) for key in which}


def fit_uo2_interface(angles=FIT_ANGLES, which=UO2_GAMMA_LABELS, **keywords):
    """Re-run the fit of docstring 3.5 on the [Z21] curves; prints the table.

    UO2_F0 and UO2_C are the "random axis and plane" entry of the result.  The
    fit is run at the UO2 diffuse width nu = UO2_NU (docstring 3.5), not at the
    Cu one: gamma scales with nu, so f0 and c have to be refitted whenever nu
    changes.  Returns {label: (f0, c, rms, table)} as fit_interface_parameters.
    """
    keywords.setdefault("base", replace(CU, nu=UO2_NU))
    print(f"GB energy fit on [Z21] (Grid.cells = "
          f"{(keywords.get('grid') or Grid()).cells}, nu = {keywords['base'].nu * 1e6:g} um):")
    return fit_interface_parameters(uo2_gamma_targets(angles, which), angles, **keywords)


def uo2_at(burnup, temperature=REFERENCE_TEMPERATURE, p=None, which="tot"):
    """Landau state at `burnup` and the parameters with G(burnup)."""
    state = hbs_state(burnup, temperature)
    p = p or uo2_parameters(temperature)
    # >>> UO2 INPUT: shear modulus G(bu, T), enters f_eta4 of Eq. (32)
    return state, replace(p, shear_modulus=state.shear_modulus)


# ---------------------------------------------------------------------------
# C.2  The only UO2 protocol: dislocations produced by irradiation
#
#      d rho/dt = S(bu) - rho C_D A(eta) <d eta/dt>,   S = d rho_ref/d bu * bu_rate
#      bu(t)    = bu_start + bu_rate t        (real time, bu_rate in GWd/tU/s)
#
#      Natural evolution: one run from bu_start to bu_end, the profiles at
#      given burnups are snapshots of the same history (plot_snapshots).
#      The result depends on bu_rate * t0 (docstring 3.4), not on each alone.
#
#      Without recovery rho(bu) = rho_ref(bu) - rho_ref(bu_start): no step, the
#      stored energy builds up only through S.  The parent misorientation is
#      fixed; the nucleus misorientation is an output.
# ---------------------------------------------------------------------------

@dataclass
class IrradiationOutcome:
    delta_theta_deg: float
    bu_nucleation: float            # GWd/tU, first recovery at the GB centre (nan: none)
    event: str                      # "nucleus", "migration" (SIBM) or "none"
    nucleus_misorientation_deg: float   # output, 5 GWd/tU after bu_nucleation (nan: none)
    burnup: np.ndarray              # sampled burnups
    rho_centre: np.ndarray
    rho_mean: np.ndarray
    rho_ref: np.ndarray             # rho produced since bu_start, without recovery
    eta_centre: np.ndarray
    eta_end: np.ndarray
    theta_centre_deg: np.ndarray
    solution: object
    recrystallised: np.ndarray = None   # fraction of fresh cells, vs burnup
    grains: np.ndarray = None           # number of grains, vs burnup
    new_grains: np.ndarray = None       # number of dislocation-free grains, vs burnup
    wall_misorientation_deg: np.ndarray = None   # b rho_wall D (median), vs burnup (3.6)
    rho_wall_mean: np.ndarray = None    # m^-2, domain mean of rho_wall, vs burnup
    wall_misorientation_max_deg: np.ndarray = None   # b max(rho_wall) D, the ceiling check
    subgrain_size: float = math.nan     # m, D of Frank's rule and of the ceiling
    temperature: float = math.nan       # K
    which: str = "tot"                  # rho fed to the SSD term


def subgrain_pattern(grid, size, jitter=0.1, seed=0):
    """s(x): sign of the plastic curvature kappa^p = b s rho_wall, docstring 3.5.

    Subgrains of width `size`, with an edge on the original GB (L/2);
    alternating, so two neighbours tilt the lattice the opposite way and every
    wall carries a misorientation.  A +-`jitter` random amplitude is the
    physical seed that breaks the mirror symmetry (instead of round-off).
    Steps smoothed over 2 dx, as the initial GB.  run_irradiation takes the
    sign of this, so the amplitude only sets where the walls sit.
    """
    rng = np.random.default_rng(seed)
    x = grid.x - 0.5 * grid.length
    k_min = int(math.floor(x.min() / size)) - 1
    k_max = int(math.ceil(x.max() / size)) + 1
    ks = np.arange(k_min, k_max + 1)
    amplitude = 0.5 * (-1.0) ** ks * (1.0 + jitter * rng.uniform(-1.0, 1.0, ks.size))
    pattern = np.full_like(x, amplitude[0])
    for k, jump in zip(ks[1:], np.diff(amplitude)):
        pattern += jump * 0.5 * (1.0 + np.tanh((x - k * size) / (2.0 * grid.dx)))
    return pattern


def run_irradiation(p, delta_theta_deg, bu_start, bu_end, bu_rate,
                    temperature=REFERENCE_TEMPERATURE, which="tot", grid=None,
                    samples_per_gwd=4, subgrain_size=None, seed=0,
                    polygonisation=0.0):
    """Relaxed GB at bu_start with rho = 0, then irradiate up to bu_end.

    The event is classified exactly as in run_case, by classify_event(), with
    rho0 replaced by the density produced so far.  The walls sit on a pattern of
    period subgrain_size (default 2 r_n at HBS_ONSET_BURNUP), which carries s(x).

    polygonisation: mobility M0 of docstring 3.6 [m J^-1 s^-1].  0 disables it
    and the model is exactly [T26]; otherwise the tangle converts into walls,
    Eqs. (H5)-(H6), on the subgrain_size pattern that carries the signs s(x).
    The walls then tilt the lattice through the elastic curvature, Eq. (P3').
    The same subgrain_size is the D of the ceiling Theta_wall <= Theta_HAGB,
    wall_saturation().
    """
    grid = grid or Grid()
    n, centre = grid.cells, grid.cells // 2

    # >>> DISLOCATION SOURCE: S(bu) = d rho_ref/d bu * bu_rate, tabulated once
    bu_table = np.linspace(bu_start, bu_end, int(20 * (bu_end - bu_start)) + 2)
    rho_table = np.array([dislocation_density(b, temperature, which) for b in bu_table])
    rate_table = np.gradient(rho_table, bu_table) * bu_rate

    def source(t):
        return float(np.interp(bu_start + bu_rate * t, bu_table, rate_table))

    rho_table = rho_table - rho_table[0]          # produced since bu_start

    if subgrain_size is None:
        subgrain_size = 2.0 * hbs_state(HBS_ONSET_BURNUP, temperature).subgrain_radius_m

    # >>> POLYGONISATION (docstring 3.6): the walls, s(x) = +-1, and the ceiling
    transfer = None
    if polygonisation > 0.0:
        transfer = Polygonisation(mobility=polygonisation,
                                  sign=np.sign(subgrain_pattern(grid, subgrain_size,
                                                                seed=seed)),
                                  subgrain=subgrain_size)

    # >>> UO2 INPUT: fixed parent misorientation
    y = initial_state(p, grid, delta_theta_deg)
    t_end = (bu_end - bu_start) / bu_rate
    times = np.linspace(0.0, t_end, int(samples_per_gwd * (bu_end - bu_start)) + 1)
    sol = evolve(p, grid, y, t_end, times, source=source,
                 max_step=0.25 / bu_rate,        # do not step over the source
                 polygonisation=transfer)

    bu = bu_start + bu_rate * sol.t
    ref = np.interp(bu, bu_table, rho_table)
    rho_c = sol.y[2 * n + centre]
    eta_end = np.minimum(sol.y[0], sol.y[n - 1])
    # the event, and the burnup at which it happens, read off the profiles
    # exactly as [T26] read them off theirs: see classify_event()
    stats = np.array([grain_statistics(p, grid, sol.y[:, k], ref[k]) for k in range(bu.size)])
    event, k_event, misorientation = classify_event(p, grid, sol, ref, delta_theta_deg)
    bu_nuc = float(bu[k_event]) if k_event >= 0 else math.nan
    # >>> POLYGONISATION OUTPUT (3.6): the misorientation the walls carry over one
    #     subgrain, Delta theta = b rho_wall D by Frank's rule.  This is what can
    #     be compared with Theta(bu) of [HBS]; theta of the field is the lattice
    #     orientation, which follows it through Eq. (P3').
    #     The MEDIAN over the domain, not the maximum: the maximum sits in the
    #     core of the pre-existing GB, where eta is lowest and the capacity of
    #     Eq. (P6) is largest, and is not representative of a subgrain wall.
    wall = wall_misorientation(np.median(sol.y[3 * n:], axis=0), subgrain_size, p)
    wall_max = wall_misorientation(sol.y[3 * n:].max(axis=0), subgrain_size, p)
    return IrradiationOutcome(delta_theta_deg, bu_nuc, event, misorientation, bu, rho_c,
                              sol.y[2 * n:3 * n].mean(axis=0), ref, sol.y[centre], eta_end,
                              np.degrees(sol.y[n + centre]), sol,
                              stats[:, 0], stats[:, 1].astype(int), stats[:, 2].astype(int),
                              wall, sol.y[3 * n:].mean(axis=0), wall_max, subgrain_size,
                              temperature, which)


def calibrate_c3(p, delta_theta_deg, bu_target, bu_start, bu_rate,
                 temperature=REFERENCE_TEMPERATURE, which="tot",
                 bracket=(0.05, 10.0), tol=0.5, margin=10.0, polygonisation=0.0,
                 grid=None):
    """c3 such that recrystallisation starts at bu_target (docstring 3.3).

    Onset = the burnup at which run_irradiation classifies its event, i.e. the
    recovery at the GB centre.  A larger c3 gives an earlier onset, so bisection
    (on log c3) within `bracket`; each run stops at bu_target + margin.  Stops
    when the onset is within `tol` GWd/tU of the target.  The bracket is wider
    than it was with the Cu GB energy: c3 scales with f0 alpha in Eq. (42), so
    the refit of docstring 3.5 multiplies every c3 by UO2_F0 / 371 kPa = 2.21.

    A dozen irradiation histories on the production grid: allow the best part of
    an hour.  That is why main() does not do it on every call - see the comment
    at CALIBRATE_C3 - and why it must NOT be made cheap by coarsening `grid`,
    which makes onset(c3) non-monotone AND each history slower (NUMERICS).
    """
    lo, hi = bracket
    bu_end = bu_target + margin
    print(f"  bisection at {(grid or Grid()).cells} cells, "
          f"polygonisation M0 = {polygonisation:g}")

    def onset(c3):
        out = run_irradiation(replace(p, c3=c3), delta_theta_deg, bu_start, bu_end,
                              bu_rate, temperature, which, grid=grid,
                              polygonisation=polygonisation)
        bu = out.bu_nucleation if not math.isnan(out.bu_nucleation) else math.inf
        print(f"  c3 = {c3:.4f}: onset = {bu:6.2f} GWd/tU ({out.event})")
        return bu

    for _ in range(12):
        c3 = math.sqrt(lo * hi)
        bu = onset(c3)
        if abs(bu - bu_target) <= tol:
            break
        if bu > bu_target:
            lo = c3                 # too late: more stored-energy force
        else:
            hi = c3
    print(f"  -> c3 = {c3:.4f}")
    return c3


def plot_snapshots(p, out, burnups, path, grid=None):
    """eta, theta, rho_free, rho_wall profiles at several burnups of ONE history."""
    plt = _pyplot()
    grid = grid or Grid()
    n = grid.cells
    x = grid.x * 1e6
    fig, axes = plt.subplots(1, 4, figsize=(19, 4))
    colours = plt.cm.viridis(np.linspace(0.0, 0.9, len(burnups)))
    for bu, colour in zip(burnups, colours):
        k = int(np.argmin(np.abs(out.burnup - bu)))
        y = out.solution.y[:, k]
        days = out.solution.t[k] / 86400.0
        axes[0].plot(x, y[:n], color=colour, label=f"bu = {out.burnup[k]:g} GWd/tU, t = {days:.0f} d")
        axes[1].plot(x, np.degrees(y[n:2 * n]), color=colour)
        axes[2].plot(x, y[2 * n:3 * n], color=colour)
        axes[3].plot(x, y[3 * n:], color=colour)
    axes[0].set_ylabel(r"$\eta$")
    axes[1].set_ylabel(r"$\theta$ [deg]")
    axes[2].set_ylabel(r"$\rho_{free}$ [m$^{-2}$]")
    axes[3].set_ylabel(r"$\rho_{wall}$ [m$^{-2}$]  (3.6: 0 if the transfer is off)")
    for ax in axes:
        ax.set_xlabel(r"$x$ [µm]")
    axes[0].legend(fontsize=7)
    fig.suptitle(rf"UO$_2$ irradiation, $\Delta\theta$ = {out.delta_theta_deg:g}$^\circ$, "
                 f"{out.event} at bu = {out.bu_nucleation:.2f} GWd/tU, "
                 f"c3 = {p.c3:.3f}, t0 = {p.t0:.3g} s", fontsize=9)
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")


def irradiation(p, angles, bu_start, bu_end, bu_rate, temperature, which, path,
                snapshot_burnups=(), snapshot_path=None, polygonisation=0.0,
                partition_path=None, grid=None):
    """run_irradiation for several parent misorientations, table and figures.

    p: UO2 parameters (c3 already chosen); G is taken at bu_start and kept.
    """
    plt = _pyplot()
    _, p = uo2_at(bu_start, temperature, p, which)
    print(f"UO2, T = {temperature:g} K, c3 = {p.c3:.3f}, t0 = {p.t0:.3g} s, "
          f"source d rho_{which}/d bu, bu {bu_start:g} -> {bu_end:g} GWd/tU at "
          f"{bu_rate * SECONDS_PER_YEAR:g} GWd/tU/yr")
    print(f"polygonisation: M0 = {polygonisation:g}, C_D = {p.c_d:g}")
    print(f"{'dtheta':>7} {'bu_rec':>7} {'event':>10} {'dth_nuc':>8} "
          f"{'Theta_Landau':>13} {'rho(bu_rec)':>12} {'max_new':>8} "
          f"{'Th_wall_end':>11} {'wall/prod':>9}")
    outcomes = []
    for angle in angles:
        out = run_irradiation(p, angle, bu_start, bu_end, bu_rate, temperature, which,
                              grid=grid, polygonisation=polygonisation)
        outcomes.append(out)
        if snapshot_path and snapshot_burnups:
            plot_snapshots(p, out, snapshot_burnups, snapshot_path.format(angle=angle),
                           grid=grid)
        if partition_path:
            plot_partition(p, out, partition_path.format(angle=angle), grid=grid)
        # the partition at the end of the history: what the walls hold and what
        # fraction of the production ended up in them (docstring 3.6)
        share = (out.rho_wall_mean[-1] / out.rho_ref[-1]) if out.rho_ref[-1] > 0 else 0.0
        tail = f"{out.wall_misorientation_deg[-1]:11.2f} {share:9.3f}"
        if math.isnan(out.bu_nucleation):
            print(f"{angle:7.2f} {'--':>7} {out.event:>10} {'':>8} {'':>13} {'':>12} "
                  f"{out.new_grains.max():8d} {tail}")
        else:
            s = hbs_state(out.bu_nucleation, temperature)
            print(f"{angle:7.2f} {out.bu_nucleation:7.2f} {out.event:>10} "
                  f"{out.nucleus_misorientation_deg:8.2f} {s.theta_deg:13.2f} "
                  f"{np.interp(out.bu_nucleation, out.burnup, out.rho_ref):12.3e} "
                  f"{out.new_grains.max():8d} {tail}")

    fig, axes = plt.subplots(1, 3, figsize=(14, 3.9))
    colours = plt.cm.viridis(np.linspace(0.0, 0.85, len(outcomes)))
    for out, colour in zip(outcomes, colours):
        label = rf"$\Delta\theta$ = {out.delta_theta_deg:g}$^\circ$, {out.event}"
        axes[0].semilogy(out.burnup, np.maximum(out.rho_centre, 1e10), color=colour, label=label)
        axes[0].semilogy(out.burnup, out.rho_mean, "--", color=colour, lw=0.8)
        axes[1].plot(out.burnup, out.eta_centre, color=colour, label=label)
        axes[2].plot(out.burnup, out.theta_centre_deg, color=colour, label=label)
        for ax in axes:
            if not math.isnan(out.bu_nucleation):
                ax.axvline(out.bu_nucleation, color=colour, lw=0.6, ls=":")
    axes[0].semilogy(outcomes[0].burnup, outcomes[0].rho_ref, "k", lw=1.5,
                     label=rf"$\int S = \rho_{{{which}}}(bu) - \rho_{{{which}}}(bu_0)$")
    axes[0].set_ylim(1e12, None)
    axes[0].set_ylabel(r"$\rho$ [m$^{-2}$]: centre (—), domain mean (- -)")
    axes[1].set_ylabel(r"$\eta_T$ at the GB centre")
    axes[2].set_ylabel(r"$\theta$ at the GB centre [deg]")
    for ax in axes:
        ax.set_xlabel("burnup [GWd/tU]")
    axes[0].legend(fontsize=7)
    fig.suptitle(f"irradiation source, {bu_rate * SECONDS_PER_YEAR:g} GWd/tU/yr, "
                 f"t0 = {p.t0:.3g} s, c3 = {p.c3:.3f}, T = {temperature:g} K", fontsize=10)
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")
    return outcomes


# ---------------------------------------------------------------------------
# C.3  c3: the three rules of docstring 3.3, on the same footing
# ---------------------------------------------------------------------------

def burnup_at_eta_eq(target, p, temperature=REFERENCE_TEMPERATURE, which="tot",
                     bu_range=(1.0, 200.0), samples=4000):
    """First burnup where eta_eq(rho(bu)) drops below `target` (nan: never).

    rho(bu) is monotone, so eta_eq of Eq. (42) falls monotonically: this is the
    burnup at which the widened GB of Eq. (42) becomes deeper than `target`.
    With target = eta_GB0 it is where mechanism (a) takes over from (b) and the
    boundary disorders instead of nucleating; with target = 0 it is where the
    model stops making sense at all (limitation 6).
    """
    bu = np.linspace(bu_range[0], bu_range[1], samples)
    eq = np.array([eta_equilibrium(dislocation_density(b, temperature, which), p)
                   for b in bu])
    below = np.flatnonzero(eq < target)
    return float(bu[below[0]]) if below.size else math.nan



# ---------------------------------------------------------------------------
# C.4  Dislocation density and its partition
#
#      [HBS] splits rho_tot into rho_ord (condensed into the LAGB walls),
#      rho_swept (annihilated) and rho_free (still a random tangle), Eq. (5).
#      The phase field has two reservoirs, rho_free and rho_wall (3.6), and a
#      third sink, the recovery of Eq. (28).  These figures put the two
#      partitions side by side.
# ---------------------------------------------------------------------------

def landau_partition(burnups, temperature=REFERENCE_TEMPERATURE):
    """(rho_tot, rho_ord, rho_swept, rho_free, Theta) of [HBS] against burnup."""
    states = [hbs_state(b, temperature) for b in np.asarray(burnups, dtype=float)]
    return (np.array([s.rho_tot for s in states]),
            np.array([s.rho_ordered for s in states]),
            np.array([s.rho_swept for s in states]),
            np.array([s.rho_free for s in states]),
            np.array([s.theta_deg for s in states]))


def plot_landau_partition(path, temperature=REFERENCE_TEMPERATURE, bu_range=(0.0, 120.0)):
    """The [HBS] partition of rho_tot against burnup: the target of 3.6.

    Independent of the phase field: it is the Landau model alone, drawn here
    because it is what the phase-field partition (plot_partition) is compared
    with.  rho_ord is the population the polygonisation of 3.6 must reproduce.
    """
    plt = _pyplot()
    bu = np.linspace(bu_range[0], bu_range[1], 241)
    tot, ord_, swept, free, theta = landau_partition(bu, temperature)
    onset, _, saturation = regime_boundaries(temperature)

    fig, (a1, a2) = plt.subplots(1, 2, figsize=(11, 4.0))
    a1.semilogy(bu, tot, "k", lw=1.6, label=r"$\rho_{tot}$, Nogita-Une")
    a1.semilogy(bu, free, color="tab:blue", label=r"$\rho_{free}$ (tangle)")
    a1.semilogy(bu, ord_, color="tab:red", label=r"$\rho_{ord}$ (LAGB walls)")
    a1.semilogy(bu, swept, color="tab:green", label=r"$\rho_{swept}$ (annihilated)")
    a1.set_ylabel(r"$\rho$ [m$^{-2}$]")
    a1.set_ylim(1e12, None)
    a1.set_title("[HBS] populations, Eq. (5)", fontsize=9)

    a2.stackplot(bu, free / tot, ord_ / tot, swept / tot,
                 colors=("tab:blue", "tab:red", "tab:green"), alpha=0.75,
                 labels=(r"$\rho_{free}$", r"$\rho_{ord}$", r"$\rho_{swept}$"))
    a2.set_ylabel(r"fraction of $\rho_{tot}$")
    a2.set_ylim(0.0, 1.0)
    ax = a2.twinx()
    ax.plot(bu, theta, "k--", lw=1.2, label=r"$\Theta$ [deg]")
    ax.axhline(THETA_HAGB, color="k", lw=0.8, ls=":")
    ax.set_ylabel(r"$\Theta$ [deg]")
    ax.legend(fontsize=7, loc="lower right")
    a2.set_title("partition and misorientation", fontsize=9)

    for a in (a1, a2):
        a.axvline(onset, color="grey", lw=0.8, ls=":")
        a.axvline(saturation, color="grey", lw=0.8, ls=":")
        a.set_xlabel("burnup [GWd/tU]")
        a.legend(fontsize=7, loc="upper left")
    fig.suptitle(f"[HBS] dislocation partition, T = {temperature:g} K "
                 f"(dotted: transition {onset:.0f} and saturation {saturation:.0f} GWd/tU)",
                 fontsize=10)
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")


def plot_partition(p, out, path, burnups=(40.0, 60.0, 80.0, 110.0), grid=None):
    """Where the dislocations produced by irradiation end up, vs [HBS].

    (a) domain means of the two reservoirs against what the source has put in:
        the gap rho_ref - (rho_free + rho_wall) is what the recovery of Eq. (28)
        has removed, the phase-field counterpart of rho_swept.
    (b) the same as fractions of the production, with the [HBS] partition dashed.
    (c) the profiles rho_free(x) and rho_wall(x).
    (d) the misorientation the walls carry, Theta_wall = b rho_wall D, against
        Theta(bu) of [HBS] and the ceiling Theta_HAGB of wall_saturation().
    """
    plt = _pyplot()
    grid = grid or Grid()
    n, x = grid.cells, grid.x * 1e6
    bu, ref = out.burnup, out.rho_ref
    free, wall = out.rho_mean, out.rho_wall_mean
    tot, ord_, swept, free_l, theta_l = landau_partition(bu, out.temperature)

    fig, axes = plt.subplots(1, 4, figsize=(19, 4.1))
    a1, a2, a3, a4 = axes
    a1.semilogy(bu, np.maximum(ref, 1e10), "k", lw=1.6,
                label=rf"$\int S$ = $\rho_{{{out.which}}}(bu) - \rho_{{{out.which}}}(bu_0)$")
    a1.semilogy(bu, np.maximum(free + wall, 1e10), color="tab:purple",
                label=r"$\rho_{free} + \rho_{wall}$ (still there)")
    a1.semilogy(bu, np.maximum(free, 1e10), color="tab:blue", label=r"$\rho_{free}$")
    a1.semilogy(bu, np.maximum(wall, 1e10), color="tab:red", label=r"$\rho_{wall}$")
    a1.semilogy(bu, np.maximum(ref - free - wall, 1e10), color="tab:green", ls="-",
                label=r"recovered, Eq. (28)")
    a1.set_ylim(1e12, None)
    a1.set_ylabel(r"$\rho$ [m$^{-2}$], domain mean")
    a1.set_title("where the production goes", fontsize=9)

    def share(q):
        """q as a fraction of what the source has produced (nan before it starts)."""
        return np.where(ref > 0.0, q / np.maximum(ref, 1e-30), np.nan)
    a2.plot(bu, share(free), color="tab:blue", label=r"$\rho_{free}$ / $\int S$")
    a2.plot(bu, share(wall), color="tab:red", label=r"$\rho_{wall}$ / $\int S$")
    a2.plot(bu, share(ref - free - wall), color="tab:green", label=r"recovered / $\int S$")
    a2.plot(bu, free_l / tot, "--", color="tab:blue", lw=1,
            label=r"[HBS] $\rho_{free}/\rho_{tot}$")
    a2.plot(bu, ord_ / tot, "--", color="tab:red", lw=1,
            label=r"[HBS] $\rho_{ord}/\rho_{tot}$")
    a2.plot(bu, swept / tot, "--", color="tab:green", lw=1,
            label=r"[HBS] $\rho_{swept}/\rho_{tot}$")
    a2.set_ylim(-0.05, 1.05)
    a2.set_ylabel("fraction")
    a2.set_title("partition: phase field (—) vs [HBS] (- -)", fontsize=9)

    colours = plt.cm.viridis(np.linspace(0.0, 0.85, len(burnups)))
    for b, colour in zip(burnups, colours):
        k = int(np.argmin(np.abs(bu - b)))
        y = out.solution.y[:, k]
        # log: the walls are one to two decades below the tangle
        a3.semilogy(x, np.maximum(y[2 * n:3 * n], 1e10), color=colour,
                    label=f"bu = {bu[k]:g}")
        a3.semilogy(x, np.maximum(y[3 * n:], 1e10), "--", color=colour)
    a3.set_ylim(1e11, None)
    a3.set_ylabel(r"$\rho_{free}$ (—), $\rho_{wall}$ (- -) [m$^{-2}$]")
    a3.set_title("profiles", fontsize=9)
    a3.set_xlabel(r"$x$ [µm]")

    a4.plot(bu, out.wall_misorientation_deg, color="tab:red",
            label=r"$\Theta_{wall}$ = $b\,\rho_{wall}D$ (median)")
    a4.plot(bu, out.wall_misorientation_max_deg, ":", color="tab:red",
            label=r"$\Theta_{wall}$ (max)")
    a4.plot(bu, theta_l, "k", label=r"[HBS] $\Theta(bu)$")
    a4.axhline(THETA_HAGB, color="grey", lw=1.0, ls="--",
               label=rf"ceiling $\Theta_{{HAGB}}$ = {THETA_HAGB:g}$^\circ$")
    a4.set_ylabel("misorientation [deg]")
    a4.set_title(rf"walls, $D$ = {out.subgrain_size * 1e6:.2f} µm", fontsize=9)

    for a in (a1, a2, a4):
        a.set_xlabel("burnup [GWd/tU]")
        if not math.isnan(out.bu_nucleation):
            a.axvline(out.bu_nucleation, color="grey", lw=0.8, ls=":")
    for a in axes:
        a.legend(fontsize=6.5)
    fig.suptitle(rf"UO$_2$ dislocation partition, $\Delta\theta$ = "
                 rf"{out.delta_theta_deg:g}$^\circ$, {out.event}"
                 + ("" if math.isnan(out.bu_nucleation)
                    else f" at {out.bu_nucleation:.1f} GWd/tU")
                 + rf", $c_3$ = {p.c3:.3f}", fontsize=10)
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")


def plot_diagnostics(p, outcomes, path, grid=None):
    """The comparison with [HBS] along the irradiation histories.

    What the removed step-protocol scan() used to draw, taken from the natural
    evolution instead of from one run per burnup: the widening depth, the
    misorientation, the capillarity that 1D cannot supply (limitation 1) and
    the restructured fraction.
    """
    plt = _pyplot()
    grid = grid or Grid()
    temperature, which = outcomes[0].temperature, outcomes[0].which
    bu = outcomes[0].burnup
    rho = np.array([dislocation_density(b, temperature, which) for b in bu])
    tot, ord_, swept, free_l, theta_l = landau_partition(bu, temperature)
    states = [hbs_state(b, temperature) for b in bu]

    fig, (a1, a2, a3, a4) = plt.subplots(1, 4, figsize=(19, 4.1))
    colours = plt.cm.viridis(np.linspace(0.0, 0.85, len(outcomes)))

    a1.plot(bu, eta_equilibrium(rho, p), "k", label=r"$\eta^{eq}(\rho)$, Eq. (42)")
    a1.axhline(0.0, color="grey", lw=0.8, ls=":")
    a1.set_ylabel(r"$\eta$")
    a1.set_title("widening depth vs the GB depth", fontsize=9)

    a2.plot(bu, theta_l, "k", label=r"[HBS] $\Theta(bu)$")
    a2.axhline(THETA_HAGB, color="grey", lw=0.8, ls="--")
    a2.set_ylabel("misorientation [deg]")
    a2.set_title("misorientation: output of the functional vs [HBS]", fontsize=9)

    a3.semilogy(bu, np.array([s.subgrain_radius_m for s in states]) * 1e6, "k",
                label=r"[HBS] sub-grain radius $r_n$")
    a3.set_ylabel("radius [µm]")
    a3.set_title("capillarity, missing in 1D (limitation 1)", fontsize=9)

    a4.plot(bu, [s.restructured_fraction for s in states], "k", label="[HBS] $X$")
    a4.set_ylabel("restructured fraction")
    a4.set_ylim(-0.05, 1.05)
    a4.set_title("restructured fraction", fontsize=9)

    for out, colour in zip(outcomes, colours):
        label = rf"$\Delta\theta$ = {out.delta_theta_deg:g}$^\circ$, {out.event}"
        relaxed = out.solution.y[:, 0]
        eta_gb0 = float(relaxed[:grid.cells].min())
        gamma = gb_energy(p, grid, relaxed)
        a1.plot(bu, out.eta_centre, color=colour, lw=1.0, label=label + " (centre)")
        a1.axhline(eta_gb0, color=colour, lw=0.8, ls="--")
        a2.plot(bu, out.wall_misorientation_deg, color=colour, lw=1.0, ls="-.")
        a3.semilogy(bu, critical_radius(p, gamma, np.maximum(rho, 1e10)) * 1e6,
                    color=colour, lw=1.0, label=label)
        a4.plot(bu, out.recrystallised, color=colour, lw=1.0, label=label)
        if not math.isnan(out.bu_nucleation):
            for a in (a1, a2, a3, a4):
                a.axvline(out.bu_nucleation, color=colour, lw=0.6, ls=":")
            a2.plot([out.bu_nucleation], [out.nucleus_misorientation_deg], "*",
                    color=colour, ms=11,
                    label=rf"$\Delta\theta$ = {out.delta_theta_deg:g}$^\circ$: "
                          r"$\Delta\theta_{nucleus}$")
    a2.plot([], [], "-.", color="grey", label=r"$\Theta_{wall}$ (3.6)")
    for a in (a1, a2, a3, a4):
        a.set_xlabel("burnup [GWd/tU]")
        a.legend(fontsize=6.5)
    fig.suptitle(rf"UO$_2$ irradiation vs [HBS], $c_3$ = {p.c3:.3f}, "
                 rf"$\rho_{{{which}}}$, T = {temperature:g} K "
                 r"(dashed in the first panel: $\eta^{GB}$)", fontsize=10)
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")

# ###########################################################################
#
#   RUN CONFIGURATION  (edit here)
#
# ###########################################################################

TEMPERATURE = REFERENCE_TEMPERATURE                     # K
FIGURES_DIR = "figures"
RHO_KIND = "tot"                    # rho fed to the SSD term: "tot" (Nogita) or "free" (Landau)
GRAIN_MISORIENTATION_DEG = 20.0     # deg, as-fabricated HAGB (Fig. 2 calibrated up to 30)
IRRADIATION_ANGLES = [15.0, 20.0, 30.0]                 # deg, parent HAGB misorientations
BU_RANGE = (0.0, 110.0)                                 # GWd/tU
BU_RATE = 10.0 / (365.25 * 86400.0)     # GWd/tU per s: ~10 GWd/tU per year (LWR average)
# Diffuse interface width (docstring 3.5): nu/sqrt(alpha) is set on the layer
# of enriched dislocation density that [ON25] measures next to the original
# grain boundaries, 500-600 nm.  nu = 2.5 um with alpha = 20 gives 0.56 um.
# f0 and c MUST be refitted whenever this changes (gamma scales with nu).
UO2_NU = 2.5e-6                         # m
# GB energy (docstring 3.5): f0 and c fitted by fit_uo2_interface() on the
# random-axis / random-plane average of the 5-DOF function of [Z21], over the
# 0-30 deg window of Fig. 2, at dx = 10 nm and nu = UO2_NU.
# rms 0.015 J/m^2, max 3.9%; gamma saturates at ~1.53 J/m^2 against 0.71 J/m^2
# at 15 deg for Cu.  c is large because the optimal c moves with nu (c = 7 at
# the Cu nu = 1 um, 16 at 1.5 um, 34 at 2.5 um).
UO2_F0 = 671.7e3                        # Pa
UO2_C = 34.0                            # -
# Phase-field clock of UO2 GBs (docstring 3.4).  NOT calibrated: this value
# makes 10 GWd/tU/yr equivalent to the former compressed run (1e-3 GWd/tU per
# t0 = 1 s), i.e. UO2 GBs ~3e3 times less mobile than the Cu values of Table 1.
# See the TODO of docstring 3.4.
UO2_T0 = 1.0e-3 / BU_RATE               # s
# c3 is calibrated so that recrystallisation starts at the experimental HBS onset,
# ~60-75 GWd/tU (Rondinella & Wiss, Mater. Today 13 (2010) 24); the lower bound
# is used.  UO2_C3 is what calibrate_c3() returns for THIS configuration (uniform
# source, the GB energy of 3.5, Delta theta = 20 deg, the Grid() default).  The
# bisection is a dozen irradiation histories, so it is not run on every call:
# set CALIBRATE_C3 = True after changing nu, f0, c, C_D, the geometry or the
# burnup protocol, and copy the value it prints back into UO2_C3.
# It is run on the SAME grid as the figures: on a coarser one (400 cells, where
# the theta core is ~4 cells) onset(c3) stops being monotone, the bisection
# returns a c3 an order of magnitude too small, and each history costs 4-10
# times MORE because the stiff solver cannot take a step.
CALIBRATE_C3 = False
UO2_C3 = 1.183                          # -
HBS_ONSET_BURNUP = 60.0                                 # GWd/tU
# Recovery (docstring 3.4): weaker than the Cu value of Table 1.  C_D = 100 is
# not usable here - it makes the problem so stiff that one irradiation history
# does not finish in 17 min, against ~1 min at 10.
UO2_C_D = 10.0
# Polygonisation (docstring 3.6): the transfer rho_free -> rho_wall.  Its
# mobility M0 is not measured (limitation 9), so it is given here as the
# TRANSFER TIME in years at rho_free = 1e14 m^-2 and main() turns it into M0
# with mobility_from_timescale() (3 years -> M0 = 4.1e2 with the parameters
# above).  None switches the transfer off and the model is then exactly [T26].
#
# OFF BY DEFAULT, and not because of the physics: the equations are verified
# (verify_mathematics(), 13/13) but a single irradiation history does not
# integrate in reasonable time.  At 200 cells, 0-110 GWd/tU, c3 = 1:
# M0 = 41 fails after 36 min ("factor is exactly singular"), M0 = 410 was still
# running after 1 h 40, against ~1 min with the transfer off.  See the OPEN
# paragraph of docstring 3.6.
POLYGONISATION_YEARS = None             # years, or None
# Chosen to straddle the nucleation window: the new grain lives between two
# boundaries for ~1 GWd/tU before it has eaten both parents, so a list that
# jumps 60 -> 65 -> 80 draws the domain before and after it and never the
# nucleus itself.
SNAPSHOT_BURNUPS = [0.0, 20.0, 40.0, 55.0, 60.0, 62.5, 64.0, 66.0, 80.0, 110.0]

def main():
    os.makedirs(FIGURES_DIR, exist_ok=True)

    print("\n=== Cu: checks against [T26] ===")
    ok = selftest()

    print("\n=== Cu: figures of [T26] ===")

    p = CU

    print("Fig. 1 and state functions")

    plt = _pyplot()
    eta = np.linspace(0.0, 0.999, 2000)
    g, dg = coupling_g(eta, p.c, g_offset(p.c))
    fig, axes = plt.subplots(1, 4, figsize=(16, 3.6))

    axes[0].plot(eta, 0.5 * (1.0 - eta) ** 2, label=r"$V$")
    axes[0].plot(eta, potential_derivative(eta), label=r"$V'$")
    axes[0].set_title("potential, Eq. (35)", fontsize=9)

    axes[1].semilogy(eta, g, label=r"$g$")
    axes[1].semilogy(eta, np.abs(dg), "--", label=r"$|g'|$")
    axes[1].axhline(0.01, color="grey", lw=0.8, ls=":")
    axes[1].set_title(rf"coupling, Eqs. (35)-(36), $c$ = {p.c:g}, $C_0$ = {g_offset(p.c):.3f}",
                      fontsize=9)

    # Fig. 1 of [T26]: c1 = 100, c2 = 0.9, normalised by c3, phi(1)/c3 = 1
    fig1 = replace(p, c1=100.0, c2=0.9)
    raw = eta - np.log(np.cosh(fig1.c1 * (fig1.c2 - eta))) / fig1.c1       # Eq. (37) / (c3/2)
    raw_at_1 = 1.0 - np.log(np.cosh(fig1.c1 * (fig1.c2 - 1.0))) / fig1.c1
    axes[2].plot(eta, 0.5 * (raw - raw_at_1) + 1.0, label=r"$\phi/c_3$, Eq. (37)")
    axes[2].plot(eta, phi_derivative(eta, fig1) / fig1.c3, label=r"$\phi'/c_3$, Eq. (38)")
    axes[2].set_title(r"Fig. 1: $c_1$ = 100, $c_2$ = 0.9", fontsize=9)

    axes[3].semilogy(eta, np.maximum(recovery_localiser(eta), 1e-12), label=r"$A$")
    axes[3].set_title("recovery localiser, Eq. (29)", fontsize=9)
    for ax in axes:
        ax.set_xlabel(r"$\eta$")
        ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(f"{FIGURES_DIR}/tandogan_cu_fig1_state_functions.png", dpi=150)
    print(f"wrote {FIGURES_DIR}/tandogan_cu_fig1_state_functions.png")

    print("Fig. 2")
    plot_gb_energy(CU, f"{FIGURES_DIR}/tandogan_cu_fig2.png", grid=CU_GRID)

    print("Fig. 4")

    plt = _pyplot()
    grid = CU_GRID
    n = grid.cells
    times = [0.0, 1e1, 1e2, 1e3, 2e3, 5e3, 1e4]
    colours = ["tab:red", "tab:blue", "tab:green", "tab:orange", "tab:purple",
               "tab:brown", "tab:pink"]
    fig, axes = plt.subplots(1, 3, figsize=(13, 3.8))

    out = run_case(p, CU_DELTA_THETA, CU_RHO0, t_end=times[-1], grid=grid, samples=times)
    print(f"  C_D = {p.c_d:g}: {out.event}, "
          f"t_recovery = {out.t_recovery / p.t0:g} t0")
    x = grid.x * 1e6
    for t, colour in zip(times, colours):
        y = state_at(out, t * p.t0)
        axes[0].plot(x, y[:n], color=colour, label=f"$C_D$={p.c_d:g}, t={t:g} $t_0$")
        axes[1].plot(x, np.degrees(y[n:2 * n]), color=colour)
        axes[2].plot(x, y[2 * n:3 * n], color=colour)

    axes[0].axhline(eta_equilibrium(CU_RHO0, p), color="grey", lw=0.8)
    axes[0].set_ylabel(r"$\eta_T$")
    axes[1].set_ylabel(r"$\theta$ [deg]")
    axes[2].set_ylabel(r"$\rho$ [m$^{-2}$]")
    for ax in axes:
        ax.set_xlabel(r"$x$ [µm]")
    axes[0].legend(fontsize=6)
    fig.suptitle(r"Cu, $\Delta\theta=15^\circ$, $\rho_0=2.5\times10^{15}$ m$^{-2}$")
    fig.tight_layout()
    fig.savefig(f"{FIGURES_DIR}/tandogan_cu_fig4.png", dpi=150)
    print(f"wrote {FIGURES_DIR}/tandogan_cu_fig4.png")

    print("Fig. 5: misorientation")
    _plot_family(CU, [(rf"$\Delta\theta$ = {a:g}$^\circ$", a, CU_RHO0)
                      for a in (2.5, 5.0, 10.0, 15.0, 20.0)],
                 [0.0, 2.0e3, 4.0e3, 1.0e4], f"{FIGURES_DIR}/tandogan_cu_fig5.png",
                 r"Fig. 5, Cu, $\rho_0 = 2.5\times10^{15}$ m$^{-2}$.")

    # Eq. (43): the nucleus orientation follows the rho ratio across the GB
    print("Fig. 6: rho left/right of the GB")
    pairs = [(0.0, 2.5), (1.0, 2.5), (1.5, 2.5), (2.0, 2.5), (2.3, 2.5), (2.5, 2.5), (2.5, 2.0)]
    _plot_family(CU, [(rf"$\rho_1, \rho_2$ = {a:g}, {b:g}", CU_DELTA_THETA, (a * 1e15, b * 1e15))
                      for a, b in pairs],
                 [0.0, 3.0e3, 1.0e4], f"{FIGURES_DIR}/tandogan_cu_fig6.png",
                 r"Fig. 6, Cu, $\Delta\theta = 15^\circ$, $\rho$ in $10^{15}$ m$^{-2}$.")
    
    print("\n=== UO2 parameters ===")
    
    p = uo2_parameters(TEMPERATURE)

    print("\n=== [HBS]: the dislocation partition to be reproduced ===")
    plot_landau_partition(f"{FIGURES_DIR}/hbs_dislocation_partition.png", TEMPERATURE)

    print(f"\n=== UO2: GB energy, f0 = {p.f0 / 1e3:.0f} kPa, c = {p.c:g}, "
          f"nu = {p.nu * 1e6:g} um ===")
    try:
        targets = uo2_gamma_targets()
    except (ImportError, FileNotFoundError) as error:
        print(f"  no [Z21] table ({error}); run: python3 uo2_gb_energy.py")
        targets = None
    plot_gb_energy(p, f"{FIGURES_DIR}/tandogan_uo2_gb_energy.png",
                   angles=FIT_ANGLES, targets=targets)

    # >>> POLYGONISATION: M0 from the transfer time (docstring 3.6)
    polygonisation = (0.0 if POLYGONISATION_YEARS is None
                      else mobility_from_timescale(POLYGONISATION_YEARS, 1.0e14, p))

    if CALIBRATE_C3:
        print(f"\n=== UO2: c3 for onset at {HBS_ONSET_BURNUP:g} GWd/tU, "
              f"Delta theta = {GRAIN_MISORIENTATION_DEG:g} deg ===")
        # on the production grid: see the comment at CALIBRATE_C3
        p = replace(p, c3=calibrate_c3(p, GRAIN_MISORIENTATION_DEG, HBS_ONSET_BURNUP,
                                       BU_RANGE[0], BU_RATE, TEMPERATURE, RHO_KIND,
                                       polygonisation=polygonisation))
        print(f"  -> copy this into UO2_C3: {p.c3:.3f}")

    print("\n=== UO2: c3 in use and what it implies (docstring 3.3) ===")
    grid = Grid()
    # G at the onset burnup: f_eta4 of Eq. (32) is what c3 is calibrated against
    _, p = uo2_at(HBS_ONSET_BURNUP, TEMPERATURE, p, RHO_KIND)
    c3 = p.c3

    relaxed = initial_state(p, grid, GRAIN_MISORIENTATION_DEG)
    eta_gb0 = float(relaxed[:grid.cells].min())
    gamma = gb_energy(p, grid, relaxed)

    print(f"c3 (docstring 3.3), Delta theta = {GRAIN_MISORIENTATION_DEG:g} deg, "
          f"rho_{RHO_KIND}, onset = {HBS_ONSET_BURNUP:g} GWd/tU: eta_GB0 = {eta_gb0:.3f}, "
          f"gamma = {gamma:.3f} J/m^2")
    print(f"{'rule':>11} {'c3':>7} {'eta_eq(60)':>10} {'eta_eq(80)':>10} "
          f"{'eta_eq(110)':>11} {'bu(eta_GB0)':>11} {'bu(eta=0)':>9}")
    eq = [eta_equilibrium(dislocation_density(b, TEMPERATURE, RHO_KIND), p)
          for b in (60.0, 80.0, 110.0)]
    print(f"{'onset':>11} {c3:7.3f} {eq[0]:10.3f} {eq[1]:10.3f} {eq[2]:11.3f} "
          f"{burnup_at_eta_eq(eta_gb0, p, TEMPERATURE, RHO_KIND):11.1f} "
          f"{burnup_at_eta_eq(0.0, p, TEMPERATURE, RHO_KIND):9.1f}")

    # eta_eq(bu) for the c3 in use: nucleation needs eta_eq > eta_GB0
    plt = _pyplot()
    bu = np.linspace(20.0, 120.0, 201)
    rho = np.array([dislocation_density(b, TEMPERATURE, RHO_KIND) for b in bu])

    fig, ax = plt.subplots(figsize=(5.6, 4.0))
    ax.plot(bu, eta_equilibrium(rho, p), color="k",
            label=f"onset rule, $c_3$ = {c3:.3f}")
    ax.axhline(eta_gb0, color="tab:blue", ls="--", lw=1,
               label=rf"$\eta^{{GB}}$ = {eta_gb0:.3f} ($\Delta\theta$ = "
                     rf"{GRAIN_MISORIENTATION_DEG:g}$^\circ$)")
    ax.axhline(0.0, color="grey", lw=0.8, ls=":")
    ax.axvline(HBS_ONSET_BURNUP, color="grey", lw=0.8, ls=":",
               label=f"onset = {HBS_ONSET_BURNUP:g} GWd/tU")
    ax.set_ylabel(r"$\eta^{eq}$, Eq. (42)")
    ax.set_ylim(-0.6, 1.05)
    ax.set_xlabel("burnup [GWd/tU]")
    ax.legend(fontsize=7)
    ax.set_title(r"widening depth: nucleation needs $\eta^{eq} > \eta^{GB}$"
                 f"\n$\\rho_{{{RHO_KIND}}}$, T = {TEMPERATURE:g} K", fontsize=9)
    fig.tight_layout()
    fig.savefig( f"{FIGURES_DIR}/tandogan_uo2_c3_rules.png", dpi=150)
    print(f"wrote {FIGURES_DIR}/tandogan_uo2_c3_rules.png")

    print("\n=== UO2, irradiation source (natural evolution) ===")
    outcomes = irradiation(p, IRRADIATION_ANGLES, *BU_RANGE, BU_RATE, TEMPERATURE,
                           RHO_KIND,
                           f"{FIGURES_DIR}/tandogan_uo2_irradiation_{RHO_KIND}.png",
                           SNAPSHOT_BURNUPS,
                           f"{FIGURES_DIR}/tandogan_uo2_snapshots_{{angle:g}}deg.png",
                           polygonisation=polygonisation,
                           partition_path=f"{FIGURES_DIR}/tandogan_uo2_partition_"
                                          "{angle:g}deg.png")

    print("\n=== UO2, irradiation vs [HBS] ===")
    plot_diagnostics(p, outcomes, f"{FIGURES_DIR}/tandogan_uo2_diagnostics.png")

    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
