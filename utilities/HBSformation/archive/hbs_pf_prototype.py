"""1D phase-field prototype of HBS formation: HMP orientation field + dislocation budget.

Henry-Mellenthin-Plapp (HMP) orientation phase field, Tandogan, Budnitzki & Sandfeld,
J. Mech. Phys. Solids 206 (2026) 106325 [T26], without mechanics, coupled to a nonlocal
GND/SSD dislocation budget that ties into the mean-field Landau model of this folder.

How to read the tags:
  [P] taken from the paper [T26] as written
  [R] REDUCTION / modelling choice made here
  [N] NUMERICS: an implementation choice
  [?] UNKNOWN: not measured or calibrated; a placeholder
  [E] known problem / deviation
  [T] checked in selftest()

    python3 hbs_pf_prototype.py                                        # selftest + demo
    python3 -c "import hbs_pf_prototype as pf; pf.selftest()"          # checks only


1. ASSUMPTIONS
===============
H1  2D reduced to 1D, Theta = [0, 0, theta]^T, small rotations                     [P]
H2  isotropic grain boundaries                                                     [P]
H3  no mechanics: u fixed, no slip, skew elastic curvature e*_x = 0                [R]
H3b bulk lattice rotation (plasticity in Tandogan) represented by a finite
    rotational mobility g_max instead                                           [R][?]
H4  one scalar dislocation population (no slip-system resolution)                  [R]
H5  isothermal                                                                     [R]
H6  GND at scale ell: rho_g = (n/b)|grad thetabar|                                 [R]
H7  n slip-system families per wall                                             [P][?]
H8  free-dislocation line energy A1(rho_av) G b^2                               [P][R]
H9  no explicit gamma*S/V term: wall energy = the HMP energy calibrated on
    Read-Shockley (Sec. 6)                                                         [R]
H10 the dislocation source is energy supplied from outside (irradiation)           [R]
H11 recovery acts only on free dislocations, only where d(eta)/dt > 0              [P]
H12 wall dislocations that disappear are annihilated                               [R]
H13 periodic boundary                                                              [P]
H14 phi(eta) in [0, 1]: SSD cost nothing in fully disordered material (phi(0)=0)
    and the full line energy in the bulk (phi(1)=1)                                [R]

2. KINEMATICS
=============
Averaged orientation, implicit gradient (spectral symbol K_ell(k) = 1/(1+k^2 ell^2)):
    thetabar - ell^2 Lap thetabar = theta
GND density, its unit vector, and the regularisation:
    rho_g = (n/b) |grad thetabar|_eps,   ebar = grad thetabar / |grad thetabar|_eps,
    |p|_eps = sqrt(p^2 + eps^2)
eps is not purely numerical: it must satisfy eps <= 0.01 R or results depend on it
near the wall/no-wall threshold [E, T3b].

3. POPULATIONS AND CONSTRAINT
==============================
    rho_s = rho_av - rho_g   (free),   rho_sw = rho_tot - rho_av   (annihilated),
    rho_g + rho_s + rho_sw = rho_tot
    rho_s >= 0,   lambda >= 0,   lambda * rho_s = 0                     (KKT)

lambda = 0 whenever d(rho_g)/dt <= 0: constraint only resists agrowing wall; 
without this rule, complementarity leaves lambda undetermined whenever 
d(rho_s)/dt = 0 exactly, and the multiplier could make dissipation (Sec. 9)
negative. Implemented in `Model.step` as `_gate`: the penalty contribution to the
theta force is retained only where |thetabar'| grew relative to the previous
accepted step [N] -- a one-step-lagged approximation to the true (path-dependent,
non-variational) KKT flow, not an exact active-set/semismooth-Newton solve [E, still
open]. `Model.energy` and the finite-difference force check in `selftest()` always
use the UNGATED penalty (a well-defined state function); the gating only changes the
force actually used to advance theta in `Model.step`, exactly as intended: it is a
genuinely non-conservative (dissipative, loading/unloading-type) correction to an
otherwise-variational flow, so during "loading" (rho_g growing) it must reduce to,
and is checked against, the plain gradient-flow force.

4. FREE ENERGY
===============
    psi = f0 [ alpha V(eta) + nu^2/2 |grad eta|^2 + mu^2/2 g(eta) |grad theta|^2 ]
          + phi(eta) A1(rho_av) G b^2 rho_s

    A1 = f(nuP)/(4 pi) ln( rho_av^(-1/2) / b ),   f(nuP) = (1 - nuP/2) / (1 - nuP)

nuP is the Poisson ratio.

    V(eta)  = (1 - eta)^2 / 2
    g(eta)  = (7 eta^3 - 6 eta^4)/(1 - eta)^3 + c_g ln(1 - eta) + C0,  c_g = 0 [T1], C0 = 0.01
    phi(eta) = c3/2 [ eta - ln cosh(c1 (c2 - eta)) / c1 ] + c0,   c1 = 100, c2 = 0.9 [P]
    phi'(eta) = c3/2 [ 1 - tanh(c1 (eta - c2)) ]

c3, c0 are fixed by phi(0) = 0, phi(1) = 1 (H14) [R]: c3 = 1.111, c0 = 0.496 (checked
in `selftest()`, computed once at import as `_PHI_C3`, `_PHI_C0`).

5. CALIBRATION (T1)
====================
Exact reduction over theta at fixed eta gives the invariance (checked to 1e-5)
    gamma_HMP = f0 nu sqrt(alpha) * Gamma_hat(m * dtheta; c_g),   m = mu/nu
alpha only rescales; the GB width is ~ 2 nu, nearly independent of dtheta. Mean-field
Read-Shockley target at 600 K, porosity 0.05:
    gamma_target = gamma_0 * dtheta * ln(1/dtheta),  gamma_0 = n f(nuP) G b/(4 pi) = 5.22 J/m^2
Fit result (`calibrate_m`): c_g = 0, m = 3.24, f0 nu sqrt(alpha/20) = 0.542 J/m^2 (so
f0 = 10.8 MPa at nu = 50 nm, alpha = 20); relative error 0.15% over 1-10 deg (fit
range), +2% at 15 deg, +7% at 20 deg, +29% at 30 deg [E, deviates above 20 deg].

6. DYNAMICS
============
Variational derivatives of L = F - integral(lambda rho_s), periodic boundary:
    dL/deta  = f0 [ alpha V' + mu^2/2 g' |grad theta|^2 ] - f0 nu^2 Lap eta
               + phi' A1 G b^2 rho_s
    dL/dtheta = -div(f0 mu^2 g grad theta) + K_ell[ div( (phi A1 G b^2 - lambda) (n/b) ebar ) ]
    dpsi/d(rho_av) = phi G b^2 (A1 + rho_s A1')
                    = phi G b^2 f(nuP)/(4 pi) [ ln(rho_av^(-1/2)/b) - rho_s/(2 rho_av) ] >= 0
    (>= 0 because phi >= 0, H14, and the log term exceeds 1/2 in the range of interest)

Evolution, with the NEW finite rotational mobility (H3b) replacing Tandogan's tau_hat*g:
    tau_eta d(eta)/dt = -dL/deta
    tau_*   d(theta)/dt = -dL/dtheta,   tau_* = tau_hat * min(g(eta), g_max)
Why the change: with tau_* = tau_hat*g (Tandogan Eq. 23) grains cannot rotate in the
bulk (g ~ 1e12 there, README T3): the misorientation stays at its initial value and
the budget constraint never engages. With g_max finite, wall misorientation grows by
grain rotation until the budget binds (Sec. 8).

7. rho_av EVOLUTION (T4)
==========================
    d(rho_av)/dt = dS/dt - C_D A(eta) <d eta/dt>_+ rho_s
                   - <-(n/b) ebar . grad(d thetabar/dt)>_+
    A(eta) = (7 eta^3-6 eta^4)/(1-eta)   [P, T26 Eq. 29]
Only the first two terms are implemented (`Model.evolve_rho_av`, `Model._step_rho_av`)
[R4]. The third ("sweep") term is deliberately dropped, on the following reasoning
rather than left unresolved:

rho_g here has NO memory of its own -- unlike the mean-field's rho_ord, it is
recomputed from theta at every call (`(n/b)|grad thetabar|_eps`), not advected as a
state. So when a wall migrates away from a point, rho_g there collapses to ~0 the
instant thetabar relaxes, with no explicit removal needed; and because the SAME
energy functional pins eta down only where |grad theta| is large (the mu^2 g'(eta)
theta'^2 and phi' A1 G b^2 rho_s terms), eta at that point recovers toward 1 at
essentially the same time thetabar relaxes there -- which is exactly when the
ALREADY-IMPLEMENTED recovery term (H11) switches on. In the regime where the KKT
constraint holds (rho_s >= rho_g, Sec. 3), recovery therefore already does what the
sweep term would: remove the free population precisely where and when a wall
recedes. It is NOT redundant, and the gap is real, in the OPPOSITE regime -- where
the constraint is already violated (rho_g > rho_s, Sec. 12 issue #8) when the wall
recedes: recovery only ever depletes rho_s, so a population that was counted as
rho_g right up to the moment the wall vanished is not annihilated, it simply
reclassifies as rho_s (rho_g -> 0 in the ap - rho_av split) and becomes available
again -- an unphysical "recharge" of the local budget with dislocations that should
have annihilated with the wall. This is the same residual soft-penalty violation of
issue #8, not a separate physics gap [R]: fixing the KKT enforcement would close
most of what the sweep term was for; adding the sweep term on top of a still-soft
penalty would be patching one symptom of #8 rather than #8 itself.

`source_rate` and `c_d` are supplied directly in the model's own nondimensional
code-time unit (tau_eta/f0 is not fixed anywhere in this prototype, just as the UO2
GB mobility is unknown in phasefield_tandogan_1d.py, tags Q1/Q2 there) [?].
`dislocation_source_rate` gives the PHYSICAL dS/dt (Nogita-Une source gated by
rho_crit) as a reference for when that timescale is fixed.

`Model._step_rho_av` integrates the recovery term EXACTLY (rho_s decays as
exp(-c_d A(eta) <eta_dot>_+ dt), rho_g untouched, H11) rather than by forward
Euler: a first version used forward Euler and could remove more free
dislocations than existed in a single step whenever c_d A(eta) <eta_dot>_+ dt > 1,
crashing rho_av (1e15 -> 5e13 m^-2 measured at c_d = 20) and, downstream, R = rho_av
b nu/n -- which the constraint of Sec. 3 checks the (unrelated) GND field against --
producing a spurious rho_g/rho_av up to 7.7x above the KKT bound. exp() cannot
overshoot for any dt, which is also why `Model.dt_limit` no longer needs a separate
recovery-stability term [E, fixed].

8. RESULTS DERIVED FROM THE TESTS
===================================
- [T2, dropped] the local model (ell = 0) is ill posed: zig-zags form at grid scale
  (6/21/34 sign changes at dx = 0.1/0.05/0.025 nu).
- [T3] the nonlocal model is well posed: mesh- and eps-converged (2 walls, dtheta
  2.24-2.27 deg); the ordered bulk is metastable (small noise decays).
- [T3b] misorientation bound from grain rotation until the budget binds:
    dtheta_max = 2 ell R = 2 ell b rho_av / n
  measured ~40% above it (soft penalty); matches the mean-field bound (7b),
  theta_bal = beta b sqrt(rho_av)/(3n), if ell = beta/(6 sqrt(rho_av)) ~ 113 nm at
  rho_av = 1e15 m^-2.
- [T3b] wall threshold (ell = 250 nm) between 5e13 and 3e14 m^-2; not eps-converged
  near the threshold [E, still open].
- [T3b] an original high-angle GB is reduced by grain rotation down to the cap
  dtheta_max: the budget needs to exclude jumps above theta_HAGB [E, confirmed, high
  severity].

9. ENERGY BALANCE AND DISSIPATION
===================================
    dF/dt = P_irr - D,   P_irr = integral( psi_,rho_av dS/dt )
    D = integral[ tau_eta eta_dot^2 + tau_* theta_dot^2 + lambda rho_g_dot
                  + psi_,rho_av (R_rec + R_ann) ]  >= 0
Every term is nonnegative by construction: tau_eta, tau_* > 0 (H3b does not change
their sign); lambda*rho_g_dot >= 0 by the Sec. 3 selection rule; psi_,rho_av >= 0 by
H14 (Sec. 6); R_rec, R_ann >= 0 by construction (Sec. 7). Not asserted analytically
here (the gated theta force is not a pure gradient flow, Sec. 3); monitored
empirically instead, via the energy-increase counter `run()` returns.

10. NONDIMENSIONAL FORM USED IN THE CODE
==========================================
x in units of nu, time in tau_eta/f0, energy density in f0:
    K = A1 G b n / (f0 nu),   R = rho_av b nu / n,   S_av = K R,   r = tau_hat/tau_eta
    d(eta)/dt = eta'' + alpha(1-eta) - m^2/2 g' theta'^2 - phi'(S_av - K|thetabar'|_eps)
    r min(g,g_max) d(theta)/dt = (m^2 g theta')' - K_ell[ ((phi K - lambda~) ebar)' ]
with |thetabar'| <= R by penalty, lambda~ -> P <|thetabar'|_eps - R>_+ (gated by the
selection rule of Sec. 3), P = 10 K/R [N]. At rho_av = 1e15 m^-2, nu = 50 nm:
K = 42.4, R = 9.7e-3.

Numerical scheme [N, T3]: convex splitting (Eyre 1998) -- eta linearly implicit;
theta's HMP part implicit, its concave -K|thetabar'| part explicit (energy-stable for
any dt), its penalty part explicit with dt <= 0.3 tau_min/(P max_k k^2 Khat^2);
thetabar by FFT. Checked to 1e-7/1e-9 (discrete force = -(1/dx) dE/d(field)); energy
never increases in the archived T3/T3b runs (Sec. 8), monitored (not re-proven) here.

11. PARAMETERS
================
b, G, nuP        0.389 nm, 69.6 GPa, 0.299 at 600 K       [P] (hbs_formation_landau)
n (N_FAMILIES)   2                                        [?]
c_g, m           0, 3.240806767151237                     [T1]
f0 nu sqrt(alpha/20)  0.5421577755484295 J/m^2             [T1]
alpha, nu        20, 50 nm                                [N] set width (~2 nu) and f0
c1, c2, c3, c0   100, 0.9, 1.111, 0.496                    [P]/[R], H14
ell              5 nu = 250 nm in the tests                [?] <-> beta (Sec. 8)
g_max            1e3                                       [?]
r = tau_hat/tau_eta   1                                    [?]
c_d (C_D)        --                                        [?] (T4)
eps              <= 0.01 R                                 [N]/[E]
rho_crit         from the mean-field model                 [E] probably redundant

12. KNOWN ISSUES (updated)
=============================
1.  ell is constitutive; it fixes the bound 2 ell b rho_av/n. ell ~ rho_av^-1/2
    would reproduce theta ~ sqrt(rho) [?]
2.  eps-dependence confirmed near the wall threshold; eps <= 0.01 R [E]
3.  rho_av is not advected: a boundary cannot enter a recovered region [E]
4.  budget applied to ALL boundaries, confirmed to reduce an original high-angle GB
    by rotation down to the cap: needs excluding jumps above theta_HAGB [E, high]
5.  onset needs seeds/finite perturbations; phi' is weak with c3 fixed by H14 [E]
6.  rho_crit: two thresholds already emerge from the phase field without it; whether
    to drop it from the source is undecided [?]
7.  g_max is new and not calibrated; sets the grain-rotation rate [?]
8.  soft penalty, not exact KKT: ~30-40% bound violation originally reported
    (archived T3b); ~1.0-1.1x measured here after the exact-decay fix of Sec. 7 --
    the Sec. 3 selection rule fixes the SIGN of the dissipation, not the violation
    magnitude. This residual is also what stands in for the dropped "sweep" term
    of Sec. 7: a wall receding WHILE the constraint is violated (rho_g > rho_s)
    reclassifies rho_g as rho_s instead of annihilating it, an unphysical local
    budget "recharge". Needs an exact active-set/semismooth-Newton solve or a much
    stiffer penalty (at the cost of dt) [E, high: also closes #15]
9.  c_g = 0: Staublin et al. (2022) conditions on g not checked [?]
10. gamma_HMP deviates above 20 deg (+7% at 20, +29% at 30) [E]
11. no mechanics (replaced by g_max, H3b); no temperature dependence except G(T) [R]
12. parameter identifiability: many parameters, few data [E, high]
13. 1D only: wall topology changes in 2D [E, high]
14. source_rate/c_d (T4) are in code-time units, not real time: no UO2 GB mobility
    is fixed anywhere in this prototype [?]
15. the "sweep" term of the T4 design equation (Sec. 7) is deliberately not
    implemented: rho_g is derived from theta, not advected, so the already-
    implemented recovery term (H11) removes the free population where and when a
    wall recedes AS LONG AS the KKT constraint holds there; the residual gap is
    exactly issue #8's violation, not separate unresolved physics [R]
"""


# ###########################################################################
#   PLAIN-LANGUAGE GUIDE (read this first)
# ###########################################################################
#
# WHAT THIS FILE SIMULATES
#   In UO2 at high burnup a large grain breaks up into many small sub-grains (the
#   High Burnup Structure, HBS). Here this is described along ONE line (1D) that
#   crosses the material.
#
# THE TWO UNKNOWN FIELDS LIVING ON THE LINE
#   eta(x)   crystalline order, between 0 and 1.
#              eta = 1  -> perfect, ordered crystal (the inside of a grain)
#              eta < 1  -> disordered material (a grain boundary, "the wall")
#              A grain boundary is therefore a narrow dip in the eta profile.
#   theta(x) orientation of the crystal lattice (radians). A grain boundary is
#              also a jump in theta: on one side the lattice is rotated by a
#              different angle than on the other (the "misorientation").
#
# THE THIRD QUANTITY: DISLOCATIONS
#   Irradiation creates dislocations (line defects). There are two kinds:
#     rho_g  (GND) : geometrically necessary dislocations: the ones that MAKE UP
#                    the grain boundary (a wall of dislocations). If theta
#                    changes quickly in space there are many of them.
#                    They are computed from theta:  rho_g = (n/b) * |d theta/dx|.
#     rho_s  (SSD) : "free" dislocations scattered inside the grain. They cost
#                    energy and are the "fuel" that lets new walls form.
#     rho_av       : the total available = rho_g + rho_s  (the budget).
#   CONSTRAINT: a wall cannot be built from more dislocations than exist, i.e.
#   rho_g <= rho_av. This becomes a cap on the misorientation (see "budget" and
#   "penalty" below).
#
# HOW IT EVOLVES
#   A free energy F[eta, theta, rho] is defined. Each field slides down the
#   gradient of F (gradient flow): d(field)/dt = - dF/d(field) / mobility.
#   Time is advanced with a "convex splitting" scheme (energy-stable).
#
# HOW TO READ THE CODE (order of the blocks)
#   1. functions g, phi, A of eta   -> the energy "pieces" taken from the paper
#   2. solve_gb / calibrate_m       -> calibrate parameters on a flat wall
#   3. physical_KR                  -> map physical units to code units
#   4. class Model                  -> energy, forces, time step
#   5. run                          -> time loop with checkpointing
#   6. plot_*                       -> figures (the first four are the
#                                      "explanatory" ones, section 6b)
#   7. selftest                     -> automatic correctness checks
#
# KEYWORDS
#   nonlocal: theta is averaged over a length ell (theta_bar) before being
#   differentiated. Without this average the model is ill posed (zig-zags at
#   the grid scale).
#   penalty: a term that "pushes back" when rho_g exceeds the budget.
#   gate: the penalty acts only where rho_g is growing (see Sec. 3 above).
#   code units: x in units of nu (wall width), energy in units of f0.
# ###########################################################################

import math
import os
import sys
import time
from collections import namedtuple

import numpy as np
import scipy.sparse as sp
import scipy.sparse.linalg as spla

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
from hbs_formation_landau import (
    BURGERS,
    FABRICATION_POROSITY,
    N_FAMILIES,
    NOGITA_SLOPE,
    REFERENCE_TEMPERATURE,
    RHO_CRIT,
    dislocation_density_nogita,
    poisson_ratio,
    shear_modulus,
)

# ###########################################################################
#   Calibrated constants: flat-GB energy fit to the mean-field Read-Shockley
#   form, c_g = 0, alpha = 20. Reproduce with calibrate_m() below.
# ###########################################################################
# g and A diverge as eta -> 1 (they divide by (1-eta)^3): to avoid infinities
# their values are 'frozen' for eta > 1 - 1e-4.
ETA_CUTOFF = 1.0 - 1.0e-4        # -    [P3] g (and A) frozen above this
# ALPHA weights the potential V(eta): large = narrow, 'stiff' wall.
ALPHA = 20.0                     # -    weight of V(eta) [P4, Tandogan Table 1 shape]
# m = mu/nu: ratio of the model's two length parameters, chosen so that the wall
# energy follows Read-Shockley (see calibrate_m).
M_CALIBRATED = 3.240806767151237      # -    mu/nu [T]
F0NU_CALIBRATED = 0.5421577755484295  # J/m^2  f0*nu [T]

# ---------------------------------------------------------------------------
# 1. HMP functions of eta: g (c_g = 0, Eq. 35-36), A (Eq. 29), phi (Eq. 37-38)
# ---------------------------------------------------------------------------

# g(eta) is the "cost of a theta gradient". In the bulk (eta -> 1) g is huge: 
# rotating the lattice of a perfect crystal is very expensive. 
# Inside the wall (small eta) g is ~0.01: theta can vary freely. 
# This is why the orientation jump is confined to the boundary.
# (plot_model_functions shows it.)
def g_fun(eta):
    """g(eta), c_g = 0: min over [0, eta_cutoff) of the raw form is 0 at eta = 0,
    so C0 = 0.01 exactly (no grid search needed, unlike the general-c case in
    `make_g` below) [T]."""
    e = np.minimum(eta, ETA_CUTOFF)
    return (7.0 * e ** 3 - 6.0 * e ** 4) / (1.0 - e) ** 3 + 0.01


# first and second derivative of g w.r.t. eta
def g_d1(eta):
    e = np.minimum(eta, ETA_CUTOFF)
    u, du = 7.0 * e ** 3 - 6.0 * e ** 4, 21.0 * e ** 2 - 24.0 * e ** 3
    return du / (1.0 - e) ** 3 + 3.0 * u / (1.0 - e) ** 4

def g_d2(eta):
    e = np.minimum(eta, ETA_CUTOFF)
    u, du, ddu = 7.0 * e ** 3 - 6.0 * e ** 4, 21.0 * e ** 2 - 24.0 * e ** 3, 42.0 * e - 72.0 * e ** 2
    return ddu / (1.0 - e) ** 3 + 6.0 * du / (1.0 - e) ** 4 + 12.0 * u / (1.0 - e) ** 5


# A(eta) is the 'recovery localiser' (annihilation of free dislocations), 
# which is concentrated where eta is healing towards 1.
def recovery_localiser(eta):
    """A(eta), Eq. (29), frozen above eta_cutoff like g [N, same convention as
    recovery_localiser in phasefield_tandogan_1d.py]."""
    e = np.minimum(eta, ETA_CUTOFF)
    return (7.0 * e ** 3 - 6.0 * e ** 4) / (1.0 - e)

# phi(eta) is a smooth switch between 0 and 1. It says how much line
# energy the free dislocations pay: phi = 0 in disordered material (inside the
# wall they cost nothing), phi = 1 in the crystal (they cost the full amount).
# This (tanh) step is what makes it favourable to 'put' dislocations in the wall.
_PHI_C1, _PHI_C2 = 100.0, 0.9    # [P] Tandogan Fig. 4 style step

def _phi_tilde(e):
    """Antiderivative of 0.5*(1 - tanh(c1*(e - c2))), i.e. of phi'(eta) Eq. (38),
    via a numerically stable ln cosh."""
    z = _PHI_C1 * (_PHI_C2 - e)
    return e - (np.abs(z) + np.log1p(np.exp(-2.0 * np.abs(z))) - math.log(2.0)) / _PHI_C1

# c3 and c0 are chosen so that phi(0) = 0 and phi(1) = 1 exactly (assumption H14)
_PHI_C3 = 2.0 / (_phi_tilde(1.0) - _phi_tilde(0.0))         # [R] normalised so
_PHI_C0 = 1.0 - 0.5 * _PHI_C3 * _phi_tilde(1.0)             # phi(0) = 0, phi(1) = 1

def phi(eta):
    return 0.5 * _PHI_C3 * _phi_tilde(eta) + _PHI_C0

def dphi(eta):
    return 0.5 * _PHI_C3 * (1.0 - np.tanh(_PHI_C1 * (eta - _PHI_C2)))


# ---------------------------------------------------------------------------
# 2. flat grain-boundary energy (exact reduction over theta), calibration
# ---------------------------------------------------------------------------

def _g_general(eta, c):
    """g(eta) with a free Read-Shockley coefficient c, used only to reproduce
      (`calibrate_m`); production code uses g_fun (c = 0)."""
    return (7.0 * eta ** 3 - 6.0 * eta ** 4) / (1.0 - eta) ** 3 + c * np.log(1.0 - eta)


def _make_g_general(c):
    grid = np.linspace(0.0, ETA_CUTOFF, 200001)
    c0 = -np.min(_g_general(grid, c)) + 0.01               # [E2] evident intent of Eq. 36

    def g(eta):
        e = np.minimum(eta, ETA_CUTOFF)
        return _g_general(e, c) + c0

    def dg(eta):
        e = np.minimum(eta, ETA_CUTOFF)
        num, dnum = 7.0 * e ** 3 - 6.0 * e ** 4, 21.0 * e ** 2 - 24.0 * e ** 3
        val = dnum / (1.0 - e) ** 3 + 3.0 * num / (1.0 - e) ** 4 - c / (1.0 - e)
        return np.where(eta < ETA_CUTOFF, val, 0.0)

    return g, dg


# solve_gb: finds the equilibrium profile of one flat wall (a single boundary)
# with an imposed orientation jump dtheta. Trick: at fixed eta, theta minimises
# the energy with theta' = J/g, so theta can be eliminated and only eta is left
# to minimise (with L-BFGS). Returns eta(x), theta(x) and the wall energy gamma.
# Used for the calibration: gamma(dtheta) must resemble Read-Shockley.
def solve_gb(dtheta, alpha, m, c, length=20.0, n=4001, eta_init=None):
    """Equilibrium flat GB, Dirichlet eta = bulk at both ends [N, avoids the GB
    migrating onto a Neumann boundary]. Exact reduction over theta (README T1):
    at fixed eta, (g theta')' = 0 so theta' = J/g with int theta' dx = dtheta, and
    min_theta int m^2/2 g theta'^2 dx = m^2 dtheta^2 / (2 I), I = int dx/g(eta).
    Returns x, eta, theta, gamma (excess energy per area, units f0*nu)."""
    from scipy.optimize import minimize

    g, dg = _make_g_general(c)
    x = np.linspace(-length / 2, length / 2, n)
    dx = x[1] - x[0]
    if eta_init is None:
        eta_init = 1.0 - 0.5 * np.exp(-x ** 2)
    eta0 = np.clip(eta_init, 0.0, ETA_CUTOFF)

    def energy_and_grad(eta):
        V, dV = 0.5 * (1.0 - eta) ** 2, -(1.0 - eta)
        w = np.full_like(eta, dx); w[0] = w[-1] = 0.5 * dx
        deta = np.diff(eta) / dx
        ginv = 1.0 / g(eta)
        I = np.sum(w * ginv)
        E = alpha * np.sum(w * V) + 0.5 * np.sum(deta ** 2) * dx + 0.5 * m ** 2 * dtheta ** 2 / I
        grad = alpha * w * dV
        grad[:-1] -= deta; grad[1:] += deta
        grad += 0.5 * m ** 2 * dtheta ** 2 / I ** 2 * w * dg(eta) * ginv ** 2
        return E, grad

    res = minimize(energy_and_grad, eta0, jac=True, method="L-BFGS-B",
                    bounds=[(ETA_CUTOFF, ETA_CUTOFF)] + [(0.0, ETA_CUTOFF)] * (n - 2)
                           + [(ETA_CUTOFF, ETA_CUTOFF)],
                    options=dict(maxiter=20000, ftol=1e-15, gtol=1e-11, maxcor=30))
    eta = res.x
    E, _ = energy_and_grad(eta)
    e_bulk = alpha * 0.5 * (1.0 - ETA_CUTOFF) ** 2 * length
    ginv = 1.0 / g(eta)
    w = np.full_like(eta, dx); w[0] = w[-1] = 0.5 * dx
    I = np.sum(w * ginv)
    theta = np.concatenate([[0.0], np.cumsum(0.5 * (ginv[1:] + ginv[:-1]) * dx)]) / I * dtheta
    return dict(x=x, eta=eta, theta=theta, gamma=E - e_bulk, success=res.success, nit=res.nit)


# calibrate_m: searches the value of m (and of the scale f0*nu) for which the
# model's gamma(dtheta) reproduces the Read-Shockley curve of the mean-field model.
# Slow: not run by default; its results are the *_CALIBRATED constants.
def calibrate_m(angles_deg=(1, 2, 3, 5, 7, 10), c=0.0, temperature=REFERENCE_TEMPERATURE,
                 porosity=FABRICATION_POROSITY):
    """Reproduce M_CALIBRATED, F0NU_CALIBRATED:
    fit m = mu/nu on the Read-Shockley target of the mean-field model, at fixed
    c_g = c and alpha = ALPHA. Expensive (many L-BFGS solves); not run by default."""
    from scipy.optimize import minimize_scalar

    G = shear_modulus(temperature, porosity)
    nu = poisson_ratio(temperature, porosity)
    g0 = N_FAMILIES * (1.0 - nu / 2.0) / (1.0 - nu) * G * BURGERS / (4.0 * math.pi)
    th = np.radians(np.asarray(angles_deg, dtype=float))
    target = g0 * th * np.log(1.0 / th)

    def rel_rms(log_m):
        gam = np.array([solve_gb(t, ALPHA, math.exp(log_m), c, length=20.0, n=1001)["gamma"] for t in th])
        scale = np.sum(gam * target) / np.sum(gam ** 2)
        return float(np.sqrt(np.mean(((scale * gam - target) / target) ** 2))), scale

    r = minimize_scalar(lambda lm: rel_rms(lm)[0], bracket=(math.log(2.5), math.log(3.2), math.log(4.0)),
                         tol=1e-4)
    err, scale = rel_rms(r.x)
    return dict(m=math.exp(r.x), f0_nu=scale, rel_rms=err)


# ---------------------------------------------------------------------------
# 3. Physical mapping rho_av -> (K, R): dislocation line energy, mean-field units
# ---------------------------------------------------------------------------

# physical_KR: translates the dislocation density rho_av [m^-2] into the two
# dimensionless quantities of the code:
#   K = dislocation line energy / wall energy
#   R = maximum slope of theta allowed by the budget: |theta'| <= R
#       (because rho_g = (n/b)|theta'| cannot exceed rho_av)
def physical_KR(rho_av, nu_len=5.0e-8, temperature=REFERENCE_TEMPERATURE, porosity=FABRICATION_POROSITY):
    """K = A1 G b n / (f0 nu), R = rho_av b nu / n, vectorised over rho_av [m^-2].
    G, nuP (Poisson ratio) from the SAME UO2 correlations as the mean-field model
    (`hbs_formation_landau.shear_modulus/poisson_ratio`) [P]."""
    rho_av = np.asarray(rho_av, dtype=float)
    G = shear_modulus(temperature, porosity)
    nuP = poisson_ratio(temperature, porosity)
    f_nu = (1.0 - nuP / 2.0) / (1.0 - nuP)
    a1 = f_nu / (4.0 * math.pi) * np.log(rho_av ** -0.5 / BURGERS)
    K = a1 * G * BURGERS * N_FAMILIES / F0NU_CALIBRATED
    R = rho_av * BURGERS * nu_len / N_FAMILIES # [Q: what is nu_len?]
    return K, R

# dislocation_source_rate: how many new dislocations irradiation creates per second
# (Nogita-Une law). Reference only: not connected to the code's time unit.
def dislocation_source_rate(burnup, dbu_dt, rho_crit=RHO_CRIT):
    """dS/dt [m^-2/s] of the README T4 spec: only active once the Nogita-Une
    density exceeds rho_crit [P]. NOT wired to `Model`'s code-time units: doing
    that needs a UO2 GB mobility (tau_eta/f0), unknown here just as it is in
    phasefield_tandogan_1d.py ([Q1]/[Q2] there) [?]. Given for reference / for
    when that timescale is fixed."""
    rho_n = dislocation_density_nogita(burnup)
    if rho_n <= rho_crit:
        return 0.0
    return NOGITA_SLOPE * math.log(10.0) * rho_n * dbu_dt


# ---------------------------------------------------------------------------
# 4. Model: nonlocal GND budget, finite rotational mobility, phi coupling
# ---------------------------------------------------------------------------

_Budget = namedtuple("Budget", "pb ap viol pc S hprime K_c R_c P_c")

# Model = the core of the code: it holds the grid, energy, forces and time step.
#   grid: periodic ring of n cells. eta and theta live on the NODES; derivatives
#   (theta') live halfway between two nodes (the 'faces' i+1/2).
class Model:
    """Periodic 1D ring, `n` cells, spacing `dx` (units of nu). See the module
    docstring, Sec. 4 (free energy) through 7 (rho_av evolution), for the
    equations this class implements."""

    def __init__(self, n, dx, ell, rho_av, nu_len=5.0e-8, temperature=REFERENCE_TEMPERATURE,
                 porosity=FABRICATION_POROSITY, m=M_CALIBRATED, alpha=ALPHA, eps=1.0e-4, r=1.0,
                 P_factor=10.0, g_max=1.0e3, use_phi=True, c_d=100.0, evolve_rho_av=False):
        self.n, self.dx, self.ell = n, dx, ell
        self.m, self.alpha, self.eps, self.r = m, alpha, eps, r
        self.P_factor, self.g_max, self.use_phi, self.c_d = P_factor, g_max, use_phi, c_d
        self.nu_len, self.temperature, self.porosity = nu_len, temperature, porosity
        self.evolve_rho_av = evolve_rho_av
        self.rho_av = np.broadcast_to(np.asarray(rho_av, dtype=float), (n,)).copy()
        self._ap_prev = None          # [R] lambda-selection rule, module docstring Sec. 3
        self._refresh_KR()

        # Nonlocal averaging filter theta_bar = K_hat * theta (in Fourier space):
        # damps oscillations shorter than ell, avoiding grid-scale zig-zags.
        k = 2.0 * np.pi * np.fft.rfftfreq(n, d=dx)
        lap_symbol = (2.0 - 2.0 * np.cos(k * dx)) / dx ** 2
        self.Khat = 1.0 / (1.0 + ell ** 2 * lap_symbol)     # thetabar smoothing kernel [R]
        # L = discrete Laplacian matrix (finite differences, periodic)
        e = np.ones(n)
        L = sp.diags([e[:-1], -2 * e, e[:-1]], [-1, 0, 1], format="lil")
        L[0, n - 1] = 1; L[n - 1, 0] = 1
        self.L = L.tocsc() / dx ** 2
        self.I = sp.identity(n, format="csc")

    def _refresh_KR(self):
        self.K, self.R = physical_KR(self.rho_av, self.nu_len, self.temperature, self.porosity)

    def _phi(self, e):  return phi(e) if self.use_phi else np.ones_like(e)
    def _dphi(self, e): return dphi(e) if self.use_phi else np.zeros_like(e)

    # three basic operators: nonlocal average, gradient (nodes->faces), divergence
    def smooth(self, f):  return np.fft.irfft(np.fft.rfft(f) * self.Khat, n=self.n)
    def grad(self, f):    return (np.roll(f, -1) - f) / self.dx        # node -> cell i+1/2
    def div_T(self, v):   return (np.roll(v, 1) - v) / self.dx         # cell -> node

    # ---- energetics -----------------------------------------------------
    # budget: computes everything about the dislocations, on the faces:
    #   pb = theta_bar'  (slope of the averaged orientation)
    #   ap = |pb|        (proportional to the GND: rho_g)
    #   S  = K*R - K*ap  (proportional to the free dislocations rho_s = rho_av - rho_g)
    #   viol = how far ap exceeds R (constraint violation) -> penalty
    def budget(self, eta, theta, growing=None):
        """Budget(pb, ap, viol, pc, S, hprime, K_c, R_c, P_c) at cell centres i+1/2.
        pb = thetabar', ap = |pb|_eps, S = rho_s in nondim energy units, hprime the
        derivative of the dislocation-energy density h(pb) = phi_c(S_av - K ap) +
        P/2 <ap - R>_+^2 w.r.t. pb.

        `growing`: the lambda-selection rule of Sec. 3 (None = ungated, i.e. the
        plain penalty everywhere ap > R). `Model.energy` and `_force_theta` (used
        by selftest's finite-difference check) always call this with growing=None,
        so they stay a consistent state-function/gradient pair; only `Model.step`
        passes an actual mask, making the resulting theta force NON-conservative
        by design where the mask is False [N, see Sec. 3]."""
        pb = self.grad(self.smooth(theta))
        ap = np.sqrt(pb ** 2 + self.eps ** 2)   # regularised |pb| (avoids division by 0)
        K_c = 0.5 * (self.K + np.roll(self.K, -1))
        R_c = 0.5 * (self.R + np.roll(self.R, -1))          # [N] arithmetic mean to faces
        viol = np.maximum(ap - R_c, 0.0)
        if growing is not None:
            viol = np.where(growing, viol, 0.0)             # [R] lambda = 0 if d(rho_g)/dt <= 0
        P_c = self.P_factor * K_c / R_c
        pn = self._phi(eta); pc = 0.5 * (pn + np.roll(pn, -1))
        S = K_c * R_c - K_c * ap                             # rho_s (nondim energy)
        hprime = (-K_c * pc + P_c * viol) * pb / ap
        return _Budget(pb, ap, viol, pc, S, hprime, K_c, R_c, P_c)

    # energy_density: the FIVE parts of the free energy, point by point:
    #   landau    = alpha*V(eta)       prefers eta = 1 (order)
    #   grad_eta  = 1/2 (eta')^2       makes the wall smooth, not a step
    #   hmp_theta = m^2/2 g theta'^2   cost of varying the orientation
    #   ssd       = phi * S            energy of the free dislocations
    #   penalty   = P/2 viol^2         punishes rho_g > rho_av
    def energy_density(self, eta, theta, growing=None):
        """Per-term energy DENSITY (not yet integrated over dx), value-for-value the
        SAME arrays `energy` used to sum -- landau/grad_eta at nodes, hmp_theta/ssd/
        penalty at cell centres i+1/2 (both (n,)-arrays on this periodic grid, so
        dx*sum gives the same total either way). Used by both `energy` (summed) and
        `plot_energy_contributions` (kept spatial)."""
        p = self.grad(theta); de = self.grad(eta)
        gn = g_fun(eta); gc = 0.5 * (gn + np.roll(gn, -1))
        b = self.budget(eta, theta, growing=growing)
        return dict(landau=self.alpha * 0.5 * (1.0 - eta) ** 2,
                    grad_eta=0.5 * de ** 2,
                    hmp_theta=0.5 * self.m ** 2 * gc * p ** 2,
                    ssd=b.pc * b.S,
                    penalty=0.5 * b.P_c * b.viol ** 2)

    def energy(self, eta, theta):
        parts = {k: self.dx * np.sum(v) for k, v in self.energy_density(eta, theta).items()}
        return sum(parts.values()), parts

    # ---- discrete forces, -(1/dx) dE/d(field); shared by step() and selftest() ----
    # _force_eta: force on eta = -dE/d eta (so that eta_dot = force).
    # Also returns J, the derivative of the force, used by the implicit step.
    def _force_eta(self, eta, theta):
        m2 = self.m ** 2
        p = self.grad(theta); p2n = np.roll(p, 1) ** 2 + p ** 2
        S = self.budget(eta, theta).S
        Sn = np.roll(S, 1) + S
        F = (self.L @ eta + self.alpha * (1.0 - eta) - 0.25 * m2 * g_d1(eta) * p2n
             - 0.5 * self._dphi(eta) * Sn)
        J = -self.alpha - 0.25 * m2 * g_d2(eta) * p2n        # diagonal Jacobian of F
        return F, J

    # _theta_operator_and_budget: builds the linear part of the force on theta
    # (diffusion operator with coefficient g(eta), matrix Dm) and the dislocation
    # part f_budget, which is treated explicitly.
    def _theta_operator_and_budget(self, eta, theta, growing=None):
        n, dx, m2 = self.n, self.dx, self.m ** 2
        gn = g_fun(eta); gc = 0.5 * (gn + np.roll(gn, -1))
        up, lo = gc, np.roll(gc, 1)
        Dm = sp.diags([-(up + lo)], [0], format="lil")
        Dm.setdiag(up[:-1], 1); Dm.setdiag(lo[1:], -1)
        Dm[n - 1, 0] = up[n - 1]; Dm[0, n - 1] = lo[0]
        Dm = Dm.tocsc() * (m2 / dx ** 2)
        f_budget = -self.smooth(self.div_T(self.budget(eta, theta, growing=growing).hprime))
        return Dm, f_budget, gn

    def _force_theta(self, eta, theta):
        Dm, f_budget, _ = self._theta_operator_and_budget(eta, theta)
        return Dm @ theta + f_budget

    # ---- time stepping (convex splitting, Eyre 1998) ---------------------
    # step: ONE time step. Sequence:
    #   1. eta is advanced (linearly implicit)
    #   2. decide where the penalty is active (the 'gate')
    #   3. theta is advanced with the new eta and mobility tau = r*min(g, g_max)
    #   4. (optional) rho_av is updated: recovery + source
    def step(self, eta, theta, dt, source_rate=0.0):
        # 1) step for eta: (I - dt*(L+J)) d_eta = dt*F, then eta is kept in [0, cutoff]
        F, J = self._force_eta(eta, theta)
        A = self.I - dt * (self.L + sp.diags(J))
        eta_new = np.clip(eta + spla.spsolve(A, dt * F), 0.0, ETA_CUTOFF)

        # lambda-selection rule (Sec. 3): retain the penalty only where the GND
        # grew relative to the LAST accepted step -- a one-step-lagged, explicit
        # stand-in for "lambda = 0 if d(rho_g)/dt <= 0" [N]
        ap_now = np.sqrt(self.grad(self.smooth(theta)) ** 2 + self.eps ** 2)
        growing = np.ones(self.n, dtype=bool) if self._ap_prev is None else (ap_now > self._ap_prev)
        self._ap_prev = ap_now

        # 3) step for theta: linear system (tau/dt - Dm) theta_new = tau/dt*theta + force
        # phi/budget weighted at the NEW eta [N, matches the archived T3b choice]
        Dm, f_budget, gn = self._theta_operator_and_budget(eta_new, theta, growing=growing)
        tau = self.r * np.minimum(gn, self.g_max)             # [R] finite rotational mobility
        M = sp.diags(tau / dt) - Dm
        theta_new = spla.spsolve(M.tocsc(), tau / dt * theta + f_budget)

        if self.evolve_rho_av:
            self._step_rho_av(eta, eta_new, theta_new, dt, source_rate)
        return eta_new, theta_new

    # _step_rho_av: updates rho_av. Only the FREE dislocations decay, at rate
    # k = c_d*A(eta)*(d eta/dt)_+ (only where eta is growing, i.e. healing);
    # then the source is added. The GND rho_g are untouched (they sit in the wall).
    def _step_rho_av(self, eta_old, eta_new, theta_new, dt, source_rate):
        """[R4] rho_av evolves by recovery of the FREE population only (H11) plus
        the source, on top of the unaffected GND: rho_av = rho_g + rho_s.

        rho_s decays EXACTLY, rho_s_new = rho_s * exp(-k dt) with k = c_d A(eta)
        <eta_dot>_+ [N]: a forward-Euler update (rho_s -= k dt rho_s) can remove
        MORE than rho_s in one step whenever k dt > 1 (large c_d, or a fast eta
        transient), overshooting past zero before the dt=1e15->5e13 crash first
        seen in a c_d=20 demo run -- exp() cannot overshoot for any dt, so this is
        unconditionally stable and no longer needs its own dt limit [E, fixed].
        The source is added on top (purely additive, safe under forward Euler)."""
        pb = self.grad(self.smooth(theta_new))
        ap = np.sqrt(pb ** 2 + self.eps ** 2)                  # rho_g, nondim (= R units)
        ap_node = 0.5 * (np.roll(ap, 1) + ap)                  # faces -> nodes [N]
        rho_g_node = ap_node * N_FAMILIES / (BURGERS * self.nu_len)
        rho_s_node = np.maximum(self.rho_av - rho_g_node, 0.0)
        growth = np.maximum((eta_new - eta_old) / dt, 0.0)
        k = self.c_d * recovery_localiser(eta_new) * growth   # decay rate, 1/code-time
        rho_s_new = rho_s_node * np.exp(-k * dt)
        self.rho_av = np.maximum(rho_g_node + rho_s_new + dt * source_rate, 1.0)  # floor: log(rho_av) in K
        self._refresh_KR()

    # dt_limit: largest stable time step (the penalty is explicit, so it limits dt)
    def dt_limit(self, eta, safety=0.3):
        """Explicit-penalty stability limit (as in the archived T3b) [N]. No
        separate limit is needed for rho_av: `_step_rho_av`'s recovery is
        integrated exactly (exponential decay), so it is unconditionally stable."""
        k = 2.0 * np.pi * np.fft.rfftfreq(self.n, self.dx)
        k2K2 = np.max((2.0 - 2.0 * np.cos(k * self.dx)) / self.dx ** 2 * self.Khat ** 2)
        K_c = 0.5 * (self.K + np.roll(self.K, -1)); R_c = 0.5 * (self.R + np.roll(self.R, -1))
        P_c = self.P_factor * K_c / R_c
        tau_min = self.r * np.minimum(g_fun(eta), self.g_max).min()
        return min(0.02, safety * tau_min / (P_c.max() * k2K2))


# count_walls: counts walls in two ways: (dips) regions where eta fell below
# 1-0.02, (flips) sign changes of the slope of theta.
def count_walls(model, eta, theta, thr_eta=0.02):
    """Walls = connected runs where 1 - eta > thr_eta, and sign changes of thetabar'."""
    dip = (1.0 - eta) > thr_eta
    runs = int(np.sum(dip & ~np.roll(dip, 1))) if dip.any() and not dip.all() else (1 if dip.all() else 0)
    pb = model.grad(model.smooth(theta))
    big = np.abs(pb) > 0.1 * model.R.mean()
    s = np.sign(pb[big])
    flips = int(np.sum(s != np.roll(s, 1))) if s.size > 1 else 0
    return runs, flips


# ---------------------------------------------------------------------------
# 5. Checkpointed driver (merges the archived t3_run.py / t3b_run.py)
# ---------------------------------------------------------------------------

# Named fields for the run() history, so plot_* and callers read hist["field"]
# instead of a positional hist[:, k] -- the latter silently breaks if a column is
# ever added or reordered, the former just works.
_HIST_FIELDS = ["t", "E", "dips", "flips", "eta_min", "rho_av_mean", "theta_range_deg",
                "dt", "energy_increases",
                "E_landau", "E_grad_eta", "E_hmp_theta", "E_ssd", "E_penalty"]
_HIST_INT_FIELDS = {"dips", "flips", "energy_increases"}
HIST_DTYPE = np.dtype([(name, "i4" if name in _HIST_INT_FIELDS else "f8") for name in _HIST_FIELDS])


# run: the full time loop. Starts from random noise in theta ('noise') or from a
# bicrystal with an equilibrium wall ('bicrystal'), advances to t_end and saves a
# .npz checkpoint (call again with the same tag to resume).
def run(tag, n=800, dx=0.1, ell=5.0, rho_av=1.0e15, t_end=300.0, init="noise", amp=0.1,
        dtheta_deg=10.0, seed=0, source_rate=0.0, evolve_rho_av=False, checkpoint_dir=".",
        wall_limit=230.0, **model_kwargs):
    """Run/resume to `t_end`, checkpointing every `wall_limit` s of wall clock [N,
    the 230 s default matches the sandbox the original prototype was built in].
    `source_rate` may be a constant or a callable of t. Returns a summary dict;
    call again with the same tag to resume. `hist` in the checkpoint is a
    structured array (`HIST_DTYPE`): read it as `hist["t"]`, `hist["E_ssd"]`, etc."""
    ck = os.path.join(checkpoint_dir, "run_%s.npz" % tag)
    mdl = Model(n, dx, ell, rho_av, evolve_rho_av=evolve_rho_av, **model_kwargs)

    if os.path.exists(ck):
        d = np.load(ck)
        eta, theta, t = d["eta"], d["theta"], float(d["t"])
        hist = [tuple(row) for row in d["hist"]]        # structured rows -> plain tuples
        mdl.rho_av = d["rho_av"]; mdl._refresh_KR()
        profiles = list(d["profiles"]) if "profiles" in d else []
        profiles_t = list(d["profiles_t"]) if "profiles_t" in d else []
    else:
        rng = np.random.default_rng(seed)
        if init == "noise":
            kk = np.fft.rfftfreq(n, d=dx) * 2 * np.pi
            mask = (kk > 0) & (kk < 2 * np.pi)
            spec = np.zeros(kk.size, complex)
            spec[mask] = rng.standard_normal(mask.sum()) + 1j * rng.standard_normal(mask.sum())
            theta = np.fft.irfft(spec, n=n); theta *= amp / np.abs(theta).max()
            eta = np.full(n, 0.85)
        elif init == "bicrystal":
            dth = math.radians(dtheta_deg)
            gb = solve_gb(dth, ALPHA, mdl.m, 0.0, length=n * dx / 2, n=n // 2 + 1)
            eta = np.concatenate([gb["eta"][:-1], gb["eta"][:-1]])
            theta = np.concatenate([gb["theta"][:-1], dth - gb["theta"][:-1]])
            eta = np.roll(eta, n // 4); theta = np.roll(theta, n // 4)
            theta += 1.0e-4 * rng.standard_normal(n)
        else:
            raise ValueError("init must be 'noise' or 'bicrystal'")
        t, hist, profiles, profiles_t = 0.0, [], [eta.copy()], [0.0]

    wall = time.time(); E_prev, _ = mdl.energy(eta, theta); increases = 0
    while t < t_end and time.time() - wall < wall_limit:
        dt = min(mdl.dt_limit(eta), t_end - t)
        s = source_rate(t) if callable(source_rate) else source_rate
        eta, theta = mdl.step(eta, theta, dt, source_rate=s); t += dt
        E, parts = mdl.energy(eta, theta)
        if E > E_prev + 1.0e-9 * max(1.0, abs(E_prev)):
            increases += 1
        E_prev = E
        if not hist or t - hist[-1][0] >= t_end / 300:
            dips, flips = count_walls(mdl, eta, theta)
            hist.append((t, E, dips, flips, eta.min(), mdl.rho_av.mean(),
                         math.degrees(theta.max() - theta.min()), dt, increases,
                         parts["landau"], parts["grad_eta"], parts["hmp_theta"],
                         parts["ssd"], parts["penalty"]))
            profiles.append(eta.copy()); profiles_t.append(t)   # eta(x,t) snapshot [N]

    hist_arr = np.array(hist, dtype=HIST_DTYPE)
    np.savez(ck, eta=eta, theta=theta, rho_av=mdl.rho_av, t=t, hist=hist_arr,
              profiles=np.array(profiles), profiles_t=np.array(profiles_t),
              n=n, dx=dx, ell=ell, eps=mdl.eps, nu_len=mdl.nu_len,
              temperature=mdl.temperature, porosity=mdl.porosity)   # so plot_* can rebuild Model
    last = hist_arr[-1]
    return dict(tag=tag, t=round(t, 2), E=round(float(last["E"]), 4),
                dips=int(last["dips"]), flips=int(last["flips"]),
                eta_min=round(float(last["eta_min"]), 4), rho_av_mean=float(last["rho_av_mean"]),
                theta_range_deg=round(float(last["theta_range_deg"]), 3),
                energy_increases=int(last["energy_increases"]), done=bool(t >= t_end))


# ---------------------------------------------------------------------------
# 6. Plotting (merges the archived t1_plot.py / t3_plot.py / t3b_plot.py)
# ---------------------------------------------------------------------------

def plot_gb_energy(angles_deg=(1, 2, 3, 5, 7, 10, 15, 20, 30), c=3.0, m=0.80, alpha=ALPHA,
                    path="hbs_pf_gamma.png"):
    """T1 diagnostic: gamma_HMP(dtheta), Tandogan Cu parameters by default (c = 3,
    m = 0.80, README T1: "Read-Shockley-like, ~10% relative error"). Pass
    m=M_CALIBRATED, c=0.0 for the fit calibrated on the mean-field Read-Shockley
    target instead. Saves and returns `path`."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    angles = np.asarray(angles_deg, dtype=float)
    gammas = [solve_gb(math.radians(d), alpha, m, c, length=20.0, n=1001)["gamma"] for d in angles]
    fig, ax = plt.subplots(figsize=(5, 4))
    ax.plot(angles, gammas, "o-")
    ax.set_xlabel(r"$\Delta\theta$ (deg)")
    ax.set_ylabel(r"$\gamma_{HMP}$ ($f_0 \nu$)")
    ax.set_title("T1: flat-GB energy vs misorientation (c=%.1f, m=%.3f)" % (c, m))
    fig.tight_layout()
    fig.savefig(path, dpi=110)
    plt.close(fig)
    return path

# [Q: it is a bit weird that the misorientation range decreases, it should be the amis2mean which increases with time?]
def plot_run(checkpoint_path, path=None, n_profile_snapshots=4):
    """T3b/T4 diagnostic for a checkpoint written by `run()`:
      - eta(x) at a few times
      - eta(x, t) as a space-time heatmap: this is what shows wall NUCLEATION
        (a new dark stripe starting mid-run, away from t = 0) vs. COARSENING
        (two stripes drifting together and merging into one) -- the scalar
        "number of walls" panel alone cannot tell those apart.
      - mean rho_av, energy and wall count vs. t.
    Saves and returns `path` (default: checkpoint_path with '_plot.png' in place
    of '.npz')."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    d = np.load(checkpoint_path)
    eta, theta, hist = d["eta"], d["theta"], d["hist"]
    x = np.arange(eta.size)
    fig, ax = plt.subplots(2, 3, figsize=(15, 7))

    # profile at a few times, not just the final one
    a = ax[0, 0]
    if "profiles" in d and len(d["profiles"]) > 1:
        profiles, profiles_t = d["profiles"], d["profiles_t"]
        idx = np.unique(np.linspace(0, len(profiles) - 1, n_profile_snapshots).astype(int))
        for i in idx:
            a.plot(x, profiles[i], label="t=%.3g" % profiles_t[i], alpha=0.4 + 0.6 * i / idx[-1])
        a.legend(fontsize=7, title="eta(x) at:")
        a.set_title("profile at several times")
    else:
        a.plot(x, eta, "k-")
        a.set_title("final profile only (no snapshots saved)")
    a.set_xlabel("cell"); a.set_ylabel(r"$\eta$")

    # space-time heatmap: THE plot for nucleation vs. coarsening
    b = ax[0, 1]
    if "profiles" in d and len(d["profiles"]) > 1:
        profiles, profiles_t = d["profiles"], d["profiles_t"]
        im = b.imshow(profiles, aspect="auto", origin="lower", cmap="viridis",
                       extent=[0, eta.size, profiles_t[0], profiles_t[-1]])
        fig.colorbar(im, ax=b, label=r"$\eta$")
        b.set_xlabel("cell"); b.set_ylabel("t")
        b.set_title(r"$\eta(x,t)$: new stripe mid-run = nucleation,"
                    "\nstripes merging = coarsening")
    else:
        b.axis("off"); b.text(0.5, 0.5, "no profile history saved", ha="center")

    c = ax[0, 2]
    c.plot(hist["t"], hist["rho_av_mean"]); c.set_yscale("log")
    c.set_xlabel("t"); c.set_ylabel(r"mean $\rho_{av}$ (m$^{-2}$)")
    c.set_title("dislocation budget")

    ax[1, 0].plot(hist["t"], hist["E"])
    ax[1, 0].set_xlabel("t"); ax[1, 0].set_ylabel("energy (f0 nu)")
    ax[1, 0].set_title("energy (should never increase, convex splitting)")

    ax[1, 1].step(hist["t"], hist["dips"], where="post")
    ax[1, 1].set_xlabel("t"); ax[1, 1].set_ylabel("number of walls")
    ax[1, 1].set_title("wall count (drop = coarsening, rise = nucleation)")

    ax[1, 2].plot(hist["t"], hist["theta_range_deg"])
    ax[1, 2].set_xlabel("t"); ax[1, 2].set_ylabel(r"$\theta_{max}-\theta_{min}$ (deg)")
    ax[1, 2].set_title("misorientation range")

    fig.tight_layout()
    path = path or checkpoint_path.replace(".npz", "_plot.png")
    fig.savefig(path, dpi=110)
    plt.close(fig)
    return path


def plot_dislocation_partition(checkpoint_path, path=None):
    """The LOCAL analogue of `hbs_dislocation_partition.png` (mean-field model,
    rho_ordered/rho_free/rho_swept vs. burnup): here rho_g (GND, in the walls) and
    rho_s (free) vs. POSITION, at the final state of a `run()` checkpoint, since
    this prototype has no burnup axis of its own (module docstring Sec. 7, [?]
    "code-time, not real time"). Left: populations [m^-2]. Right: fraction of
    rho_av, plus rho_g/rho_av where it exceeds 1 -- the KKT constraint rho_s >= 0
    is VIOLATED there (soft penalty, Sec. 12 issue #8). Saves and returns `path`."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    d = np.load(checkpoint_path)
    n, dx, ell, eps = int(d["n"]), float(d["dx"]), float(d["ell"]), float(d["eps"])
    nu_len, temperature, porosity = float(d["nu_len"]), float(d["temperature"]), float(d["porosity"])
    eta, theta, rho_av, t = d["eta"], d["theta"], d["rho_av"], float(d["t"])

    mdl = Model(n, dx, ell, rho_av, nu_len=nu_len, temperature=temperature, porosity=porosity, eps=eps)
    pb = mdl.grad(mdl.smooth(theta))
    ap = np.sqrt(pb ** 2 + eps ** 2)
    ap_node = 0.5 * (np.roll(ap, 1) + ap)                       # faces -> nodes, as in _step_rho_av
    rho_g = ap_node * N_FAMILIES / (BURGERS * nu_len)
    rho_s = np.maximum(rho_av - rho_g, 0.0)

    x = np.arange(n) * dx
    fig, (a1, a2) = plt.subplots(1, 2, figsize=(11, 4.0))
    a1.semilogy(x, rho_av, "k", lw=1.6, label=r"$\rho_{av}$ (not yet annihilated)")
    a1.semilogy(x, rho_s, color="tab:blue", label=r"$\rho_s$ (free/SSD)")
    a1.semilogy(x, rho_g, color="tab:red", label=r"$\rho_g$ (GND/wall)")
    a1.set_xlabel(r"$x/\nu$"); a1.set_ylabel(r"$\rho$ (m$^{-2}$)")
    a1.set_title("dislocation populations", fontsize=9); a1.legend(fontsize=7)

    frac_g = rho_g / np.maximum(rho_av, 1.0)
    a2.plot(x, frac_g, color="tab:red", label=r"$\rho_g/\rho_{av}$")
    a2.plot(x, 1.0 - np.minimum(frac_g, 1.0), color="tab:blue", label=r"$\rho_s/\rho_{av}$")
    a2.axhline(1.0, color="k", lw=0.8, ls=":", label="KKT bound")
    a2.set_xlabel(r"$x/\nu$"); a2.set_ylabel(r"fraction of $\rho_{av}$")
    a2.set_title(r"partition ($\rho_g/\rho_{av}>1$: constraint violated)", fontsize=9)
    a2.legend(fontsize=7)

    fig.suptitle("T3b/T4: local dislocation partition, t = %.3g" % t)
    fig.tight_layout()
    path = path or checkpoint_path.replace(".npz", "_partition.png")
    fig.savefig(path, dpi=110)
    plt.close(fig)
    return path


def plot_energy_contributions(checkpoint_path, path=None):
    """Energy diagnostic (module docstring Sec. 4/9): each of the five terms of
    `Model.energy` vs. t (left, from the run() history) and vs. position at the
    FINAL state (right, from `Model.energy_density`) -- shows not just that the
    total never increases, but WHERE it sits (grain-boundary vs. dislocation vs.
    KKT-penalty energy) and how that shifts over the run. Saves and returns
    `path` (default: checkpoint_path with '_energy.png' in place of '.npz')."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    d = np.load(checkpoint_path)
    hist = d["hist"]
    n, dx, ell, eps = int(d["n"]), float(d["dx"]), float(d["ell"]), float(d["eps"])
    nu_len, temperature, porosity = float(d["nu_len"]), float(d["temperature"]), float(d["porosity"])
    eta, theta, rho_av = d["eta"], d["theta"], d["rho_av"]

    fig, (a1, a2) = plt.subplots(1, 2, figsize=(12, 4.5))

    terms = ["E_landau", "E_grad_eta", "E_hmp_theta", "E_ssd", "E_penalty"]
    colors = ["tab:gray", "tab:green", "tab:orange", "tab:blue", "tab:red"]
    for term, color in zip(terms, colors):
        a1.plot(hist["t"], hist[term], color=color, label=term[2:])
    a1.plot(hist["t"], hist["E"], "k--", lw=1.2, label="total")
    a1.set_xlabel("t"); a1.set_ylabel(r"energy ($f_0 \nu$)")
    a1.set_title("energy contributions vs. t"); a1.legend(fontsize=7)

    mdl = Model(n, dx, ell, rho_av, nu_len=nu_len, temperature=temperature, porosity=porosity, eps=eps)
    dens = mdl.energy_density(eta, theta)
    x = np.arange(n) * dx
    for term, color in zip(["landau", "grad_eta", "hmp_theta", "ssd", "penalty"], colors):
        a2.plot(x, dens[term], color=color, label=term)
    a2.axhline(0.0, color="k", lw=0.6)
    a2.set_xlabel(r"$x/\nu$"); a2.set_ylabel(r"energy density ($f_0$)")
    a2.set_title("spatial energy density, final state"); a2.legend(fontsize=7)

    fig.tight_layout()
    path = path or checkpoint_path.replace(".npz", "_energy.png")
    fig.savefig(path, dpi=110)
    plt.close(fig)
    return path


# ---------------------------------------------------------------------------
# 6b. EXPLANATORY plots: meant to help understand the model, not to validate it
# ---------------------------------------------------------------------------

def plot_model_functions(path="explain_model_functions.png"):
    """The four functions of eta that define the model.
      V(eta)   : potential, minimum at eta = 1 (ordered crystal).
      g(eta)   : cost of varying theta. LOGARITHMIC scale: in the bulk it is huge,
                 in the wall ~0.01 -> orientation can only change inside the wall.
      phi(eta) : 0 -> 1 switch of the free-dislocation energy.
      A(eta)   : recovery localiser (where free dislocations disappear)."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    e = np.linspace(0.0, 0.999, 2000)
    fig, ax = plt.subplots(1, 4, figsize=(17, 3.8))
    ax[0].plot(e, 0.5 * (1 - e) ** 2, "k")
    ax[0].set_title(r"$V(\eta)=(1-\eta)^2/2$" "\nminimum in the crystal (eta=1)")
    ax[1].semilogy(e, g_fun(e), "tab:orange")
    ax[1].set_title(r"$g(\eta)$: cost of rotating the lattice" "\n(huge in the bulk, small in the wall)")
    ax[2].plot(e, phi(e), "tab:blue")
    ax[2].axvline(_PHI_C2, color="gray", ls=":", label="c2 = %.1f (step)" % _PHI_C2)
    ax[2].set_title(r"$\phi(\eta)$: 0 in the wall, 1 in the crystal" "\n(free dislocations only cost energy in the crystal)")
    ax[2].legend(fontsize=8)
    ax[3].semilogy(e[e > 0.05], recovery_localiser(e[e > 0.05]), "tab:green")
    ax[3].set_title(r"$A(\eta)$: where recovery acts")
    for a in ax:
        a.set_xlabel(r"$\eta$  (0 = disordered, 1 = crystal)")
    fig.tight_layout(); fig.savefig(path, dpi=110); plt.close(fig)
    return path


def plot_wall_anatomy(dtheta_deg=5.0, path="explain_wall_anatomy.png"):
    """Anatomy of ONE grain boundary at equilibrium (solved with solve_gb):
      top    : eta(x)   -> the boundary is a 'hole' in the order
      middle : theta(x) -> the orientation jumps by dtheta, only inside the hole
      bottom : theta'(x) -> proportional to the geometrically necessary
               dislocations rho_g = (n/b)|theta'|: the wall IS made of dislocations.
    x is in units of nu (wall width). No dislocation budget is imposed here."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    r = solve_gb(math.radians(dtheta_deg), ALPHA, M_CALIBRATED, 0.0, length=20.0, n=1001)
    x, eta, theta = r["x"], r["eta"], r["theta"]
    dth = np.gradient(theta, x)                       # theta' in units of 1/nu
    rho_g = dth * N_FAMILIES / (BURGERS * 5.0e-8)     # m^-2 for nu = 50 nm
    fig, ax = plt.subplots(3, 1, figsize=(6.5, 8), sharex=True)
    ax[0].plot(x, eta, "k"); ax[0].set_ylabel(r"$\eta$")
    ax[0].set_title("1) order: the boundary is a disordered zone")
    ax[1].plot(x, np.degrees(theta), "tab:orange"); ax[1].set_ylabel(r"$\theta$ (deg)")
    ax[1].set_title("2) orientation: %.0f degree jump inside the boundary" % dtheta_deg)
    ax[2].plot(x, rho_g, "tab:red"); ax[2].set_ylabel(r"$\rho_g$ (m$^{-2}$)")
    ax[2].set_title(r"3) wall dislocations: $\rho_g=(n/b)|\theta'|$ (no budget imposed)")
    ax[2].set_xlabel(r"$x/\nu$")
    fig.tight_layout(); fig.savefig(path, dpi=110); plt.close(fig)
    return path


def plot_nonlocal_budget(ell_over_nu=(0.0, 2.0, 5.0), rho_av=1.0e15, dtheta_deg=2.0, nu_len=5.0e-8,
                          path="explain_nonlocal_budget.png"):
    """Why the cap on the misorientation (the 'budget') exists:
      left : a sharp jump of theta is AVERAGED over a length ell (theta_bar).
             The larger ell, the more the jump is smeared out.
      right: |theta_bar'| (proportional to rho_g) must stay below R (proportional
             to rho_av, the dashed line). If the jump is too large for the budget,
             rho_g > rho_av: impossible -> the penalty reduces dtheta.
             Hence dtheta_max = 2 ell R.
    The ring is periodic: one jump up at L/4 and one jump down at 3L/4."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    n, dx = 800, 0.1
    x = np.arange(n) * dx
    theta = np.where((x > x.max() / 4) & (x < 3 * x.max() / 4), math.radians(dtheta_deg), 0.0)
    _, R = physical_KR(rho_av, nu_len)
    fig, ax = plt.subplots(1, 2, figsize=(12, 4))
    for ell in ell_over_nu:
        if ell == 0.0:
            tb = theta
            pb = np.abs(np.diff(theta, append=theta[0])) / dx
        else:
            m = Model(n, dx, ell, rho_av, nu_len=nu_len)
            tb = m.smooth(theta)
            pb = np.abs(m.grad(tb))
        ax[0].plot(x, np.degrees(tb), label="ell = %g nu" % ell)
        ax[1].plot(x, pb, label="ell = %g nu" % ell)
    ax[1].axhline(float(R), color="k", ls="--", label="cap R (budget rho_av = %.0e)" % rho_av)
    ax[0].set_xlabel(r"$x/\nu$"); ax[0].set_ylabel(r"$\bar\theta$ (deg)")
    ax[0].set_title("averaged orientation (nonlocal)"); ax[0].legend(fontsize=8)
    ax[1].set_xlabel(r"$x/\nu$"); ax[1].set_ylabel(r"$|\bar\theta'|$  (~ GND)")
    ax[1].set_yscale("log"); ax[1].set_ylim(1e-6, None)
    ax[1].set_title("GND required vs budget: above the line = constraint violated")
    ax[1].legend(fontsize=8)
    fig.tight_layout(); fig.savefig(path, dpi=110); plt.close(fig)
    return path


def plot_calibration(angles_deg=(1, 2, 3, 5, 7, 10, 15, 20, 30), path="explain_calibration.png"):
    """Calibration: wall energy of the phase-field model (dots) against the
    Read-Shockley curve of the mean-field model (line), with m and f0*nu
    calibrated. Good agreement up to ~20 degrees, then the model rises above the
    target."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    G = shear_modulus(REFERENCE_TEMPERATURE, FABRICATION_POROSITY)
    nuP = poisson_ratio(REFERENCE_TEMPERATURE, FABRICATION_POROSITY)
    g0 = N_FAMILIES * (1.0 - nuP / 2.0) / (1.0 - nuP) * G * BURGERS / (4.0 * math.pi)
    a = np.asarray(angles_deg, dtype=float)
    th = np.radians(a)
    gam = np.array([solve_gb(t, ALPHA, M_CALIBRATED, 0.0, length=20.0, n=1001)["gamma"] for t in th])
    gam_phys = gam * F0NU_CALIBRATED                    # f0*nu [J/m^2] * (gamma in f0*nu)
    fine = np.radians(np.linspace(0.5, 30, 200))
    fig, ax = plt.subplots(figsize=(6, 4.2))
    ax.plot(np.degrees(fine), g0 * fine * np.log(1 / fine), "k-", label="Read-Shockley (mean field)")
    ax.plot(a, gam_phys, "o", color="tab:orange", label="phase field (m = %.2f)" % M_CALIBRATED)
    ax.set_xlabel("misorientation (deg)"); ax.set_ylabel(r"boundary energy $\gamma$ (J/m$^2$)")
    ax.set_title("flat-wall calibration"); ax.legend()
    fig.tight_layout(); fig.savefig(path, dpi=110); plt.close(fig)
    return path


# ---------------------------------------------------------------------------
# 7. Self-test: reuses the production force/energy functions -- no duplicated formulas
# ---------------------------------------------------------------------------

def selftest(verbose=True):
    ok = True

    def report(name, value, threshold, passed):
        nonlocal ok
        ok &= passed
        if verbose:
            print("%-28s %10.2e  (threshold %.0e)  %s" % (name, value, threshold, "OK" if passed else "FAIL"))

    # phi normalisation [T]
    e0, e1 = abs(phi(0.0)), abs(phi(1.0) - 1.0)
    report("phi(0)", e0, 1e-9, e0 < 1e-9)
    report("phi(1)-1", e1, 1e-9, e1 < 1e-9)

    # phi constants match the documented model (Sec. 4): c3 = 1.111, c0 = 0.496 [T]
    ec3, ec0 = abs(_PHI_C3 - 1.111), abs(_PHI_C0 - 0.496)
    report("phi c3 vs 1.111", ec3, 1e-3, ec3 < 1e-3)
    report("phi c0 vs 0.496", ec0, 1e-3, ec0 < 1e-3)

    # g'(eta), g''(eta) by finite differences [T]
    e = np.linspace(0.1, 0.99, 50); h = 1e-6
    err_g1 = np.max(np.abs((g_fun(e + h) - g_fun(e - h)) / (2 * h) / g_d1(e) - 1.0))
    err_g2 = np.max(np.abs((g_d1(e + h) - g_d1(e - h)) / (2 * h) / g_d2(e) - 1.0))
    report("g' finite-difference", err_g1, 1e-4, err_g1 < 1e-4)
    report("g'' finite-difference", err_g2, 1e-4, err_g2 < 1e-4)

    # discrete forces = -(1/dx) dE/d(field), both with rho_av frozen and evolving [T]
    for evolve in (False, True):
        mdl = Model(64, 0.25, ell=2.0, rho_av=1.0e15, eps=1e-3, evolve_rho_av=evolve)
        rng = np.random.default_rng(0)
        eta = 0.6 + 0.35 * rng.random(64); theta = 0.05 * rng.standard_normal(64)
        F_eta, _ = mdl._force_eta(eta, theta)
        F_theta = mdl._force_theta(eta, theta)
        we = wt = 0.0
        for i in rng.choice(64, 12, replace=False):
            hh = 1e-6
            a, b = eta.copy(), eta.copy(); a[i] += hh; b[i] -= hh
            fd = -(mdl.energy(a, theta)[0] - mdl.energy(b, theta)[0]) / (2 * hh) / mdl.dx
            we = max(we, abs(fd - F_eta[i]) / max(abs(fd), 1e-6))
            a, b = theta.copy(), theta.copy(); a[i] += hh; b[i] -= hh
            fd = -(mdl.energy(eta, a)[0] - mdl.energy(eta, b)[0]) / (2 * hh) / mdl.dx
            wt = max(wt, abs(fd - F_theta[i]) / max(abs(fd), 1e-6))
        report("force check eta (evolve=%s)" % evolve, we, 1e-4, we < 1e-4)
        report("force check theta (evolve=%s)" % evolve, wt, 1e-4, wt < 1e-4)

    # lambda-selection rule (Sec. 3): growing=None must reproduce the plain penalty
    # (energy()/the force check above rely on this); growing=False must zero it out
    # exactly wherever it would otherwise be active [T]
    mdl = Model(48, 0.15, ell=2.0, rho_av=5e13, eps=1e-3)     # low rho_av: R small, easy to violate
    rng = np.random.default_rng(3)
    eta = 0.5 + 0.4 * rng.random(48); theta = 0.3 * np.cumsum(rng.standard_normal(48))
    b_plain = mdl.budget(eta, theta)
    b_true = mdl.budget(eta, theta, growing=np.ones(48, bool))
    b_false = mdl.budget(eta, theta, growing=np.zeros(48, bool))
    e_gate_true = (np.max(np.abs(b_true.viol - b_plain.viol))
                   + np.max(np.abs(b_true.hprime - b_plain.hprime)))
    report("gate growing=True == ungated", e_gate_true, 1e-12, e_gate_true < 1e-12)
    active = b_plain.viol > 0
    gated_off = active.any() and np.all(b_false.viol[active] == 0.0)
    report("gate growing=False zeroes active penalty",
           0.0 if gated_off else 1.0, 0.5, gated_off)

    # step() actually wires the gate: with a decreasing GND field at a violated face,
    # the theta update must NOT feel the penalty there (Sec. 3 selection rule) [T]
    mdl = Model(48, 0.15, ell=2.0, rho_av=5e13, eps=1e-3)
    eta = np.full(48, 0.9)
    theta = 0.02 * np.arange(48)                         # constant slope, well above R
    dt = 1e-6
    mdl.step(eta, theta, dt)                              # primes self._ap_prev (first call, all True)
    theta_decaying = 0.6 * theta                          # GND now smaller everywhere: not growing
    ap_now = np.sqrt(mdl.grad(mdl.smooth(theta_decaying)) ** 2 + mdl.eps ** 2)
    all_decreasing = np.all(ap_now < mdl._ap_prev)
    report("synthetic case is actually decreasing everywhere",
           0.0 if all_decreasing else 1.0, 0.5, all_decreasing)

    eta_gated, theta_gated = mdl.step(eta, theta_decaying, dt)     # uses the real gate internally
    Dm, f_budget_ungated, gn = mdl._theta_operator_and_budget(eta_gated, theta_decaying, growing=None)
    tau = mdl.r * np.minimum(gn, mdl.g_max)
    M = sp.diags(tau / dt) - Dm
    theta_ungated = spla.spsolve(M.tocsc(), tau / dt * theta_decaying + f_budget_ungated)
    differ = np.max(np.abs(theta_gated - theta_ungated)) > 1e-8 * max(1.0, np.max(np.abs(theta_ungated)))
    report("step() gate changes the theta update vs. the ungated force",
           0.0 if differ else 1.0, 0.5, differ)

    # T4 regression gate: evolve_rho_av=False must leave rho_av/K/R exactly untouched,
    # i.e. this file's step() reduces EXACTLY to the archived T3b when T4 is off [T]
    mdl = Model(32, 0.2, ell=1.5, rho_av=1e15, eps=1e-3, evolve_rho_av=False)
    eta = np.full(32, 0.8); theta = 0.05 * np.random.default_rng(1).standard_normal(32)
    rho_before = mdl.rho_av.copy()
    for _ in range(5):
        eta, theta = mdl.step(eta, theta, mdl.dt_limit(eta))
    frozen = np.array_equal(mdl.rho_av, rho_before)
    report("rho_av frozen when evolve_rho_av=False", 0.0 if frozen else 1.0, 0.5, frozen)

    # T4 rate check: independent, non-vectorised recomputation of _step_rho_av at a
    # few nodes, against the production (vectorised) result [T]
    mdl = Model(32, 0.2, ell=1.5, rho_av=1e15, eps=1e-3, evolve_rho_av=True, c_d=50.0)
    rng = np.random.default_rng(2)
    eta_old = 0.5 + 0.4 * rng.random(32); theta = 0.1 * rng.standard_normal(32)
    dt = 1e-4
    F, J = mdl._force_eta(eta_old, theta)
    A = mdl.I - dt * (mdl.L + sp.diags(J))
    eta_new = np.clip(eta_old + spla.spsolve(A, dt * F), 0.0, ETA_CUTOFF)
    rho_before = mdl.rho_av.copy()
    mdl._step_rho_av(eta_old, eta_new, theta, dt, source_rate=1e10)
    pb = mdl.grad(mdl.smooth(theta)); ap = np.sqrt(pb ** 2 + mdl.eps ** 2)
    max_err = 0.0
    for i in rng.choice(32, 8, replace=False):
        ap_node_i = 0.5 * (ap[(i - 1) % 32] + ap[i])
        rho_g_i = ap_node_i * N_FAMILIES / (BURGERS * mdl.nu_len)
        rho_s_i = max(rho_before[i] - rho_g_i, 0.0)
        growth_i = max((eta_new[i] - eta_old[i]) / dt, 0.0)
        e_i = np.minimum(eta_new[i], ETA_CUTOFF)
        A_i = (7 * e_i ** 3 - 6 * e_i ** 4) / (1 - e_i)
        k_i = 50.0 * A_i * growth_i
        expect_i = max(rho_g_i + rho_s_i * math.exp(-k_i * dt) + dt * 1e10, 1.0)
        max_err = max(max_err, abs(expect_i - mdl.rho_av[i]) / max(abs(expect_i), 1.0))
    report("T4 rate check (scalar vs vectorised)", max_err, 1e-8, max_err < 1e-8)

    # exact decay never overshoots: even with an extreme c_d*dt, the FREE part
    # rho_s must stay in [0, rho_s_before] -- a forward-Euler update could remove
    # more than rho_s_before, sending it negative (rho_g itself is untouched by
    # recovery and independent of rho_av, so it is rho_s, not rho_av, that must
    # be bounded here) [T]
    mdl = Model(16, 0.2, ell=1.5, rho_av=1e14, eps=1e-3, evolve_rho_av=True, c_d=1.0e6)
    eta_old = np.full(16, 0.5); theta = 0.1 * np.random.default_rng(4).standard_normal(16)
    dt = 1.0e-3
    F, J = mdl._force_eta(eta_old, theta)
    A = mdl.I - dt * (mdl.L + sp.diags(J))
    eta_new = np.clip(eta_old + spla.spsolve(A, dt * F), 0.0, ETA_CUTOFF)
    pb0 = mdl.grad(mdl.smooth(theta)); ap0 = np.sqrt(pb0 ** 2 + mdl.eps ** 2)
    rho_g_before = 0.5 * (np.roll(ap0, 1) + ap0) * N_FAMILIES / (BURGERS * mdl.nu_len)
    rho_s_before = np.maximum(mdl.rho_av - rho_g_before, 0.0)
    mdl._step_rho_av(eta_old, eta_new, theta, dt, source_rate=0.0)
    rho_s_after = np.maximum(mdl.rho_av - rho_g_before, 0.0)   # theta unchanged by _step_rho_av
    overshoot = np.any(rho_s_after < -1.0e-6) or np.any(rho_s_after > rho_s_before.max() + 1.0)
    report("exact decay never overshoots (extreme c_d*dt)",
           1.0 if overshoot else 0.0, 0.5, not overshoot)

    if verbose:
        print("selftest: %s" % ("ALL OK" if ok else "FAILURES ABOVE"))
    return ok


if __name__ == "__main__":
    selftest()
    figures_dir = os.path.join(os.path.dirname(__file__) or ".", "figures")
    os.makedirs(figures_dir, exist_ok=True)

    print("\nExplanatory figures:")
    for fn in (plot_model_functions, plot_wall_anatomy, plot_nonlocal_budget, plot_calibration):
        print("  ", fn(path=os.path.join(figures_dir, fn.__name__.replace("plot_", "explain_") + ".png")))

    print("\nT1 -- gamma_HMP(dtheta), Tandogan Cu parameters (c = 3, m = 0.80):")
    for deg in (1, 2, 5, 10, 20):
        r = solve_gb(math.radians(deg), ALPHA, 0.80, 3.0, length=20.0, n=1001)
        print("  %5.1f deg  gamma = %.6f f0*nu  eta_min = %.4f" % (deg, r["gamma"], r["eta"].min()))
    gamma_png = plot_gb_energy(path=os.path.join(figures_dir, "t1_gamma.png"))
    print("  figure ->", gamma_png)

    print("\nT3b+T4 demo: small ring, rho = 1e15 m^-2, source on, a few steps")
    ck = os.path.join(figures_dir, "run_demo.npz")
    if os.path.exists(ck):
        os.remove(ck)                          # fresh demo each run, not a resumable study
    summary = run("demo", n=200, dx=0.1, ell=5.0, rho_av=1.0e15, t_end=0.5, init="noise",
                   evolve_rho_av=True, source_rate=5.0e12, c_d=20.0, wall_limit=20.0,
                   checkpoint_dir=figures_dir)
    print(" ", summary)
    run_png = plot_run(ck)
    print("  figure ->", run_png)
    partition_png = plot_dislocation_partition(ck)
    print("  figure ->", partition_png)
    energy_png = plot_energy_contributions(ck)
    print("  figure ->", energy_png)
