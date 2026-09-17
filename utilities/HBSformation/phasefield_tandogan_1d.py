"""1D orientation phase field for dislocation-driven grain nucleation,
reproduced for Cu and then driven by the HBS Landau model for UO2.

    python3 phasefield_tandogan_1d.py

References
==========
[T26]  I.T. Tandogan, M. Budnitzki, S. Sandfeld, "A multi-physics model for
       dislocation driven spontaneous grain nucleation and microstructure
       evolution in polycrystals", J. Mech. Phys. Solids 206 (2026) 106325.
       Equation, table and figure numbers in this file are those of [T26].
[PFRP] N. Ofori-Opoku, J. Warren, P.-C. Simon, "Phase Field Method Recommended
       Practices - Model Formulation".
[HBS]  hbs_formation_landau.py in this folder (Landau model of the HBS).


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
* Geometry.  x in [0, L], L = 10 um, 400 cells, one GB at L/2 (Sec. 3.2).  The
  paper's periodic 20 um bicrystal (Fig. 3) is symmetric about the grain
  centres, so zero-flux walls at x = 0 and x = L are exact.
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

* Mobilities: Table 1 "recrystallisation" values, tau_eta = 1e4 f0 t0,
  tau_hat = 1e1 f0 t0, t0 = 1 s.  The initial GB is relaxed with
  tau_eta = 1e2 f0 t0 only to get to equilibrium faster.

3. UO2 APPLIED
==============
3.1 What the functional does by itself, and what it does not
-------------------------------------------------------------
* Delta theta is the misorientation of the ORIGINAL (as-fabricated) grains
  and is imposed only once, in initial_state().  It is a usual high-angle
  boundary (GRAIN_MISORIENTATION_DEG, 20 deg by default: HAGB, and inside the
  0-30 deg range where Fig. 2 is calibrated), NOT the Landau Theta(bu).
* The orientation of the new grain is an OUTPUT of Eqs. (32)-(34): theta
  of the nucleus lies between the two parents and, by Eq. (43), it is their
  average if rho is the same on both sides and moves towards the grain with
  the lower rho otherwise (Fig. 6).  Its misorientation to the closer
  parent, `nucleus_misorientation_deg`, is what can be compared with the
  Landau Theta(bu): it is printed next to it, it is not an input.
* In this reduction the functional does NOT produce misorientation from
  dislocations.  rho is a scalar SSD density: it carries stored energy only,
  no orientation.  In [T26] the geometrically necessary content is the
  Cosserat curvature kappa = grad theta (Eq. 3), and new lattice rotation
  comes only from plastic slip (Eqs. 24-26).  Without mechanics (no slip,
  and irradiation produces no slip here) theta can only be redistributed
  between the values already present: with a uniform rho a nucleus, when
  it forms, sits at Delta theta / 2; otherwise the GB migrates (theta of the
  centre goes to a parent's value, event "migration").  
  The Landau split rho_tot -> rho_ord (walls, GND, misorientation) + 
  rho_free (random) is therefore NOT done by the phase field: it has no ingredient for it.
* Consequence for rho: since the phase field does not know about the split,
  the natural input is the total density of Nogita-Une, rho_tot
  (RHO_KIND = "tot"); rho_free (RHO_KIND = "free") would presuppose the
  Landau model.  The same RHO_KIND is used in both protocols and for c3.

3.2 Where the inputs enter  (grep for ">>> UO2 INPUT" / ">>> DISLOCATION SOURCE")
----------------------------------------------------------------------------------
  Delta theta   GRAIN_MISORIENTATION_DEG (step protocol) or IRRADIATION_ANGLES
                -> initial_state(), height of the theta step at L/2.
  rho(bu)       hbs_state(bu).rho_tot (or .rho_free)
                -> PROTOCOL 1, step (as Sec. 3.2): run_case() sets it
                   uniformly at t = 0 after relaxing the GB.
  S(bu)         d rho_ref/d bu * bu_rate, rho_ref = the same RHO_KIND
                -> PROTOCOL 2, irradiation: run_irradiation() builds S and
                   _right_hand_side() adds it to d rho/dt, in place of the
                   Kocks-Mecking production of Eq. (28) (irradiation instead
                   of slip).  Uniform in x and independent of the local rho,
                   so a recovered nucleus is loaded again.
  G(bu, T)      hbs_state(bu).shear_modulus
                -> stored_energy_per_rho, i.e. f_eta4 in Eq. (32).
  b             BURGERS of [HBS].
  c3            uo2_parameters(), see 3.3.
  Landau Theta, r_n, X are used only for COMPARISON (tables and plots).

3.3 What c3 is and how it is chosen
-----------------------------------
c3 is the height of phi'(eta) inside the GB (Eq. 38): it multiplies the
stored-energy force f_eta4 = -c3 * lambda/2 G b^2 rho.  It is a pure
calibration knob that fixes HOW MUCH rho is needed to widen the GB, through
Eq. (42):

    eta_eq = 1 - c3 * (lambda/2 G b^2 rho) / (f0 alpha)

Too large a c3 drives eta_eq below 0 (not allowed); too small a c3 and the
GB never widens enough to nucleate.  [T26] chooses c3 = 1.7 so that at the
saturation density of their Kocks-Mecking law (2.5e15 m^-2) eta_eq = 0.579.
Here the same rule is applied: c3 is such that eta_eq = 0.579 at rho(bu_sat),
bu_sat being the burnup where Landau Theta saturates (HBS complete).
TODO (later): A more physical alternative (not implemented): choose c3 so that nucleation
starts at the experimental HBS onset burnup.


4. LIMITATIONS
==============
1. 1D: no GB curvature, hence no capillary pressure; a nucleus of any size
   survives.  critical_radius() estimates r* = 2 gamma / (lambda/2 G b^2 rho)
   a posteriori, to be compared with the Landau sub-grain radius.
2. No mechanics: no slip, no stress, no Kocks-Mecking production; tau_eta is
   constant (the switch of Eq. 39 is not used).  Hence no new lattice
   rotation: see 3.1.
3. One slip system (h = 1), isotropic GB energy (a(n_gb, theta) = 1).
4. The GB energy is calibrated (Fig. 2) only up to 30 deg for Cu <100> tilt
   boundaries; no HAGB cusps.
   TODO (later): calibrate gamma(Delta theta) on UO2 values.
5. For UO2 the interface parameters are still the Cu values of Table 1; only
   G, b and c3 are changed.  With t0 = 1 s the times are not physical.
   Physical meaning of each parameter for UO2:

   parameter        role                                   UO2 counterpart
   ---------------  -------------------------------------  -----------------------------
   G, b, lambda     line energy of a dislocation           PHYSICAL: G(T, porosity, bu)
                    (lambda/2 G b^2)                       and b from [HBS]; lambda ~ 0.3
   f0, c            GB energy vs misorientation (Fig. 2)   PHYSICAL target: UO2
                                                           gamma(Delta theta), fitted
   nu, alpha, mu    GB width and shape (width ~            NUMERICAL: diffuse width >>
                    nu/sqrt(alpha)); with f0 also gamma    real GB, chosen for resolution
   tau_eta,         inverse GB / rotation mobilities       PHYSICAL in principle (UO2 GB
   tau_hat                                                 mobility, Arrhenius in T);
                                                           their ratio is numerical
   c1, c2           shape and position of phi' (Eq. 38)    NUMERICAL (GB velocity, Fig. 7)
   c3               rho needed to nucleate (Eq. 42)        CALIBRATION, see 3.3
   C_D              efficiency of recovery (Eq. 28)        CALIBRATION (large enough for
                                                           full recovery)
   Delta theta      misorientation of parent grains        PHYSICAL: as-fabricated HAGB
   L, cells         domain, resolution                     NUMERICAL (L ~ grain size)

6. Irradiation protocol: the burnup rate is compressed (the phase field
   relaxes in ~1e2 s, irradiation takes years; only rate << relaxation
   matters); the source is uniform; no recovery other than Eq. (28). 
   TODO (later): natural evolution
7. The "nucleus" criteria (rho < 0.1 rho0 at the centre, ends still above
   c2, centre back above 0.95; nucleus vs migration from theta_centre) are
   thresholds chosen in this script, not in [T26].


5. NUMERICS  (checked against [PFRP])
======================================
* Finite volumes on a uniform grid, zero-flux faces at the walls; g at the
  faces by harmonic (series) average; theta'^2 at the cell centres as the
  mean of the two face values.  Method of lines, stiff BDF (scipy) with
  adaptive time step and a sparse Jacobian pattern ([PFRP] item 8).
* Interface resolution: GB width ~ nu/sqrt(alpha) = 0.22 um, dx = 25 nm,
  i.e. ~9 cells across the GB ([PFRP] item 3 asks for 5-10).
* g and A are frozen above eta_cutoff = 1 - 1e-4 (Sec. 2.2.2), to control
  the singularity.  As [PFRP] recommends ("plot your state functions"),
  plot_state_functions() draws V, g, g', phi, phi' (Fig. 1) and A, and
  selftest() checks g >= 0.01 and Eq. (42).
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
from scipy.sparse import diags

from hbs_formation_landau import (
    BURGERS,
    REFERENCE_TEMPERATURE,
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
T0 = 1.0                            # s      time unit of the mobilities, Table 1


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
    on a fine grid; selftest() checks min g = 0.01, and
    plot_state_functions() shows g.
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


def phi_derivative(eta, p):
    """phi'(eta), Eq. (38): ~c3 in the GB, ~0 in the bulk.

    This is what confines the stored-energy force f_eta4 to the GB.
    """
    return 0.5 * p.c3 * (1.0 - np.tanh(p.c1 * (eta - p.c2)))


def recovery_localiser(eta):
    """A(eta), Eq. (29), frozen above eta_cutoff."""
    e = np.minimum(eta, ETA_CUTOFF)
    return (7.0 * e ** 3 - 6.0 * e ** 4) / (1.0 - e)


def eta_equilibrium(rho, p):
    """eta_eq of a widened GB, Eq. (42), with phi'(eta_eq) = c3.

    Nucleation (Sec. 3.2.3) needs eta_eq above the initial GB depth.
    """
    return 1.0 - p.c3 * p.stored_energy_per_rho * rho / (p.f0 * p.alpha)


# ---------------------------------------------------------------------------
# A.4  1D solver
# ---------------------------------------------------------------------------

@dataclass
class Grid:
    length: float = 10.0e-6         # m      grain-centre to grain-centre, Fig. 3
    cells: int = 400                # -      as in Sec. 3.2

    @property
    def dx(self):
        return self.length / self.cells

    @property
    def x(self):
        """Cell centres."""
        return (np.arange(self.cells) + 0.5) * self.dx


def _right_hand_side(p, grid, offset, source=None):
    """d/dt of the state y = [eta (n), theta (n), rho (n)].

    source(t) [m^-2/s]: dislocation production, uniform in x.  
    None means no production, as in [T26] after loading.
    """
    n, dx = grid.cells, grid.dx
    tau_eta = p.tau_eta * p.f0 * T0             # J s m^-3
    tau_hat = p.tau_hat * p.f0 * T0             # J s m^-3
    energy = p.stored_energy_per_rho

    def rhs(t, y):
        eta, theta, rho = y[:n], y[n:2 * n], y[2 * n:]
        g, dg = coupling_g(eta, p.c, offset)

        # gradients on the n + 1 faces; the two wall faces stay 0 (zero flux)
        dtheta = np.zeros(n + 1)
        dtheta[1:-1] = np.diff(theta) / dx
        deta = np.zeros(n + 1)
        deta[1:-1] = np.diff(eta) / dx

        g_face = np.zeros(n + 1)
        g_face[1:-1] = 2.0 * g[:-1] * g[1:] / (g[:-1] + g[1:])     # harmonic average
        grad_theta_sq = 0.5 * (dtheta[:-1] ** 2 + dtheta[1:] ** 2)  # at cell centres

        # Eq. (34): orientation, tau_hat g theta_dot = f0 (mu^2 g theta')'
        flux = p.mu ** 2 * g_face * dtheta
        theta_dot = p.f0 * np.diff(flux) / dx / (tau_hat * g)

        # Eq. (32): order parameter, forces of Eq. (41)
        # (f_eta2 and f_eta3 kept grouped: the irradiation protocol is
        #  sensitive to the round-off of this sum)
        f_eta1 = p.f0 * p.nu ** 2 * np.diff(deta) / dx               # gradient
        f_eta23 = p.f0 * (p.alpha * potential_derivative(eta)        # potential
                          + p.mu ** 2 * dg * grad_theta_sq)          # orientation
        f_eta4 = phi_derivative(eta, p) * energy * rho               # stored dislocations
        eta_dot = (f_eta1 - f_eta23 - f_eta4) / tau_eta

        # Eq. (28): recovery, active only where eta grows (<.> Macaulay bracket)
        rho_dot = -rho * p.c_d * recovery_localiser(eta) * np.maximum(eta_dot, 0.0)

        # >>> DISLOCATION SOURCE: irradiation production (UO2, Part C.3).
        #     Replaces the Kocks-Mecking term of Eq. (28).
        if source is not None:
            rho_dot = rho_dot + source(t)

        return np.concatenate([eta_dot, theta_dot, rho_dot])

    return rhs


def _sparsity(n):
    """Jacobian pattern: nearest-neighbour coupling within and between fields."""
    band = diags([1.0, 1.0, 1.0], [-1, 0, 1], shape=(n, n)).toarray()
    return np.block([[band, band, band]] * 3)


def evolve(p, grid, y0, t_end, times, source=None, max_step=np.inf):
    """Integrate from 0 to t_end, output at `times` (stiff BDF, adaptive)."""
    rhs = _right_hand_side(p, grid, g_offset(p.c), source)
    sol = solve_ivp(rhs, (0.0, t_end), y0, method="BDF", t_eval=times, max_step=max_step,
                    jac_sparsity=_sparsity(grid.cells), rtol=1e-5, atol=1e-8)
    if not sol.success:
        raise RuntimeError(sol.message)
    return sol


# ---------------------------------------------------------------------------
# A.5  Initial condition, Sec. 3.2
# ---------------------------------------------------------------------------

def initial_state(p, grid, delta_theta_deg, eta0=0.99):
    """Relaxed GB without dislocations.

    eta = eta0 everywhere, theta a sharp step of height delta_theta at L/2,
    then relaxed with rho = 0 (tau_eta = 1e2 f0 t0 to get there faster).
    """
    n = grid.cells
    # >>> UO2 INPUT: delta_theta_deg is the misorientation of the PARENT
    #     grains (a fixed HAGB for UO2), imposed only here.  Every later
    #     orientation, including the nucleus', comes from Eq. (34).
    theta = math.radians(delta_theta_deg) * 0.5 * (
        1.0 + np.tanh((grid.x - 0.5 * grid.length) / (2.0 * grid.dx)))
    y0 = np.concatenate([np.full(n, eta0), theta, np.zeros(n)])
    relax = evolve(replace(p, tau_eta=1.0e2), grid, y0, 2.0e4, [2.0e4])
    return relax.y[:, -1]


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


# ###########################################################################
#
#   PART B - Cu: REPRODUCTION OF [T26], SECTION 3.2 (Figs. 1, 2, 4, 5, 6)
#
#   run_case() and plot_profiles() implement the paper's step protocol and
#   are reused for UO2 in Part C.2.
#
# ###########################################################################

CU = PhaseFieldParameters()                 # Table 1, c1, c2, c3 of Fig. 4
CU_RHO0 = 2.5e15                            # m^-2   Sec. 3.2
CU_DELTA_THETA = 15.0                       # deg    Fig. 4


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


def run_case(p, delta_theta_deg, rho0, t_end=1.0e4, grid=None, samples=(), every=50.0):
    """Step protocol of Sec. 3.2: relax the GB, switch on rho0, evolve.

    rho0 is uniform (Figs. 4, 5) or a pair (rho_left, rho_right) with a jump
    at the GB (Fig. 6); the orientation of the nucleus then follows Eq. (43).

    Nucleus (criterion of this script, limitation 7): the recovery at the GB
    centre starts while the two grains are still crystalline (eta at the
    walls above c2) and the centre becomes crystalline again (eta > 0.95).
    If the widened GB first fills the whole domain, the orientation gradient
    vanishes and the uniform eta_eq state recovers everywhere at once: that
    is recorded as t_recovery but not as a nucleus.
    """
    grid = grid or Grid()
    n, centre = grid.cells, grid.cells // 2
    y = initial_state(p, grid, delta_theta_deg)
    eta_gb0 = y[:n].min()
    relaxed = y.copy()

    # >>> UO2 INPUT (step protocol): rho0 is hbs_state(bu).rho_tot (or rho_free)
    rho_left, rho_right = rho0 if isinstance(rho0, tuple) else (rho0, rho0)
    y[2 * n:] = np.where(grid.x < 0.5 * grid.length, rho_left, rho_right)
    rho_scale = max(rho_left, rho_right)

    times = np.union1d(np.arange(every, t_end + 0.5 * every, every), samples)
    sol = evolve(p, grid, y, t_end, times)

    # the thresholds below use the larger of the two densities
    rho0 = rho_scale
    rho_c = sol.y[2 * n + centre]
    hit = np.flatnonzero(rho_c < 0.1 * rho0) if rho0 > 0 else np.array([], int)
    t_recovery = float(sol.t[hit[0]]) if hit.size else math.nan
    ends_crystalline = hit.size and min(sol.y[0, hit[0]], sol.y[n - 1, hit[0]]) > p.c2
    # recovery alone is not a nucleus: the centre must also become crystalline
    centre_ordered = hit.size and sol.y[centre, hit[0]:].max() > 0.95

    theta_c = math.degrees(sol.y[n + centre, -1])
    rho = sol.y[2 * n:, -1]
    recrystallised = float(np.mean(rho < 0.1 * rho0)) if rho0 > 0 else 0.0
    recovered = bool(ends_crystalline and centre_ordered)
    # a nucleus keeps an orientation between the two parents (Eq. 43); if the
    # centre has rotated onto one of them, the GB has migrated (SIBM) instead
    inside = 0.1 * delta_theta_deg < theta_c < 0.9 * delta_theta_deg
    nucleated = recovered and inside
    event = "nucleus" if nucleated else ("migration" if recovered else "none")
    misorientation = nucleus_misorientation(theta_c, delta_theta_deg) if nucleated else math.nan
    return Outcome(delta_theta_deg, (rho_left, rho_right) if rho_left != rho_right else rho_left,
                   eta_equilibrium(rho0, p), eta_gb0, nucleated, t_recovery, recrystallised,
                   theta_c, relaxed, sol, event, misorientation)


def plot_profiles(p, delta_theta_deg, rho0, path, title, grid=None):
    """eta, theta, rho profiles at several times, as Fig. 4 (a)-(c)."""
    plt = _pyplot()
    grid = grid or Grid()
    n = grid.cells
    times = [0.0, 1e1, 1e2, 1e3, 2e3, 5e3, 1e4]
    colours = ["tab:red", "tab:blue", "tab:green", "tab:orange", "tab:purple",
               "tab:brown", "tab:pink"]
    fig, axes = plt.subplots(1, 3, figsize=(13, 3.8))

    out = run_case(p, delta_theta_deg, rho0, t_end=times[-1], grid=grid, samples=times)
    print(f"  C_D = {p.c_d:g}: {out.event}, "
          f"t_recovery = {out.t_recovery:g} s")
    x = grid.x * 1e6
    for t, colour in zip(times, colours):
        y = state_at(out, t)
        axes[0].plot(x, y[:n], color=colour, label=f"$C_D$={p.c_d:g}, t={t:g} s")
        axes[1].plot(x, np.degrees(y[n:2 * n]), color=colour)
        axes[2].plot(x, y[2 * n:], color=colour)

    axes[0].axhline(eta_equilibrium(rho0, p), color="grey", lw=0.8)
    axes[0].set_ylabel(r"$\eta_T$")
    axes[1].set_ylabel(r"$\theta$ [deg]")
    axes[2].set_ylabel(r"$\rho$ [m$^{-2}$]")
    for ax in axes:
        ax.set_xlabel(r"$x$ [µm]")
    axes[0].legend(fontsize=6)
    fig.suptitle(title)
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")


def plot_state_functions(p, path):
    """V, g, g', phi, phi' (Fig. 1 of [T26]) and A, as [PFRP] recommends."""
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
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")


def plot_gb_energy(p, path, angles=(1.0, 2.5, 5.0, 7.5, 10.0, 15.0, 20.0, 25.0, 30.0)):
    """Fig. 2 of [T26]: GB energy of the relaxed 1D profile vs misorientation."""
    plt = _pyplot()
    grid = Grid()
    gammas = [gb_energy(p, grid, initial_state(p, grid, a)) for a in angles]
    for a, gamma in zip(angles, gammas):
        print(f"  Delta theta = {a:5.1f} deg: gamma = {gamma:.3f} J/m^2")
    fig, ax = plt.subplots(figsize=(5, 3.8))
    ax.plot(angles, gammas, "x-", label="1D, numerical")
    ax.axvline(30.0, color="grey", lw=0.8, ls=":")
    ax.set_xlabel(r"$\Delta\theta$ [deg]")
    ax.set_ylabel(r"$\gamma$ [J/m$^2$]")
    ax.set_title(f"Fig. 2: GB energy, c = {p.c:g}", fontsize=9)
    ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")


def _plot_family(p, cases, times, path, title):
    """eta, theta, rho at `times` for several (label, delta_theta, rho0) cases."""
    plt = _pyplot()
    grid = Grid()
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
            y = state_at(out, t)
            axes[0].plot(x, y[:n], style, color=colour,
                         label=label if t == times[0] else None)
            axes[1].plot(x, np.degrees(y[n:2 * n]), style, color=colour)
            axes[2].plot(x, y[2 * n:], style, color=colour)
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


def plot_paper_figures(figures_dir):
    """Cu figures of [T26] redone in 1D: Figs. 1, 2, 4, 5, 6."""
    print("Fig. 1 and state functions")
    plot_state_functions(CU, f"{figures_dir}/tandogan_cu_fig1_state_functions.png")

    print("Fig. 2")
    plot_gb_energy(CU, f"{figures_dir}/tandogan_cu_fig2.png")

    print("Fig. 4")
    plot_profiles(CU, CU_DELTA_THETA, CU_RHO0, f"{figures_dir}/tandogan_cu_fig4.png",
                  r"Cu, $\Delta\theta=15^\circ$, $\rho_0=2.5\times10^{15}$ m$^{-2}$")

    print("Fig. 5: misorientation")
    _plot_family(CU, [(rf"$\Delta\theta$ = {a:g}$^\circ$", a, CU_RHO0)
                      for a in (2.5, 5.0, 10.0, 15.0, 20.0)],
                 [0.0, 2.0e3, 4.0e3, 1.0e4], f"{figures_dir}/tandogan_cu_fig5.png",
                 r"Fig. 5, Cu, $\rho_0 = 2.5\times10^{15}$ m$^{-2}$.")

    # Eq. (43): the nucleus orientation follows the rho ratio across the GB
    print("Fig. 6: rho left/right of the GB")
    pairs = [(0.0, 2.5), (1.0, 2.5), (1.5, 2.5), (2.0, 2.5), (2.3, 2.5), (2.5, 2.5), (2.5, 2.0)]
    _plot_family(CU, [(rf"$\rho_1, \rho_2$ = {a:g}, {b:g}", CU_DELTA_THETA, (a * 1e15, b * 1e15))
                      for a, b in pairs],
                 [0.0, 3.0e3, 1.0e4], f"{figures_dir}/tandogan_cu_fig6.png",
                 r"Fig. 6, Cu, $\Delta\theta = 15^\circ$, $\rho$ in $10^{15}$ m$^{-2}$.")


def selftest():
    """Cu checks against [T26]; returns True if all pass.

    The corresponding figures are drawn by plot_paper_figures().
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
    no_rec = run_case(replace(CU, c_d=0.0), 15.0, rho0, t_end=1.0e4)
    eta_c = no_rec.solution.y[Grid().cells // 2, -1]
    check("Cu 15 deg, C_D = 0: GB widens to eta_eq", abs(eta_c - eq) < 0.03,
          f"eta(centre) = {eta_c:.3f}, no nucleus = {not no_rec.nucleated}")

    # Sec. 3.2.2: with recovery a nucleus forms
    rec = run_case(CU, 15.0, rho0, t_end=1.0e4)
    check("Cu 15 deg, C_D = 100: nucleus", rec.nucleated,
          f"theta_centre = {rec.theta_centre_deg:.2f} deg, recrystallised {rec.recrystallised:.2f}")

    # Fig. 2
    gamma = gb_energy(CU, Grid(), no_rec.initial)
    check("Cu 15 deg GB energy vs Fig. 2", 0.6 < gamma < 0.9, f"gamma = {gamma:.3f} J/m^2 (Fig. 2: ~0.75)")

    # Fig. 5: misorientation threshold
    for angle, expected, paper in [(10.0, True, "immediate"),
                                   (5.0, True, "delayed, ~4e3 s"),
                                   (2.5, False, "never")]:
        out = run_case(CU, angle, rho0, t_end=1.0e4)
        check(f"Cu {angle:g} deg: nucleus = {expected} (paper: {paper})",
              out.nucleated == expected,
              f"t_recovery = {out.t_recovery:g} s")
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


def uo2_parameters(temperature=REFERENCE_TEMPERATURE, which="tot", rho_saturation=None,
                   **changes):
    """Cu interface of Table 1 (limitation 5) + UO2 G, b and c3.

    c3 (docstring 3.3): Eq. (42) returns the paper's 0.579 at rho_saturation,
    by default rho(bu_sat), bu_sat = burnup where Landau Theta saturates.
    G here is the one at bu_sat; callers replace it with G(bu) (uo2_at).
    """
    _, _, bu_saturation = regime_boundaries()
    state = hbs_state(bu_saturation, temperature)
    if rho_saturation is None:
        rho_saturation = dislocation_density(bu_saturation, temperature, which)
    p = replace(CU, shear_modulus=state.shear_modulus, burgers=BURGERS)
    # >>> UO2 INPUT: c3 from the dislocation density at HBS saturation
    eta_eq_paper = eta_equilibrium(CU_RHO0, CU)
    c3 = (1.0 - eta_eq_paper) * p.f0 * p.alpha / (p.stored_energy_per_rho * rho_saturation)
    return replace(p, c3=c3, **changes)


def uo2_at(burnup, temperature=REFERENCE_TEMPERATURE, p=None, which="tot"):
    """Landau state at `burnup` and the parameters with G(burnup)."""
    state = hbs_state(burnup, temperature)
    p = p or uo2_parameters(temperature, which)
    # >>> UO2 INPUT: shear modulus G(bu, T), enters f_eta4 of Eq. (32)
    return state, replace(p, shear_modulus=state.shear_modulus)


# ---------------------------------------------------------------------------
# C.2  Protocol 1: step in rho (as Sec. 3.2), one run per burnup
#
#      Parent grains with a fixed HAGB misorientation; at t = 0 the
#      dislocation density of burnup bu is switched on.  The misorientation
#      of the nucleus is an output, compared with the Landau Theta(bu).
# ---------------------------------------------------------------------------

def run_burnup(burnup, delta_theta_deg, temperature=REFERENCE_TEMPERATURE, p=None,
               which="tot", **keywords):
    """Paper protocol with the dislocation density of one burnup."""
    state, p = uo2_at(burnup, temperature, p, which)
    rho = dislocation_density(burnup, temperature, which)
    # >>> UO2 INPUT: parent misorientation (fixed) and rho(bu)
    return state, rho, run_case(p, delta_theta_deg, rho, **keywords)


def plot_burnup(burnup, delta_theta_deg, temperature, which, path):
    """Fig. 4-like profiles for one burnup."""
    state, p = uo2_at(burnup, temperature, which=which)
    rho = dislocation_density(burnup, temperature, which)
    plot_profiles(p, delta_theta_deg, rho, path,
                  f"UO$_2$, bu = {burnup:g} GWd/tU: "
                  rf"$\Delta\theta$ = {delta_theta_deg:g}$^\circ$, "
                  rf"$\rho_{{{which}}}$ = {rho:.2e} m$^{{-2}}$, $c_3$ = {p.c3:.3f}  "
                  rf"(Landau $\Theta$ = {state.theta_deg:.2f}$^\circ$)")


def scan(burnups, delta_theta_deg, temperature, which, path):
    """Nucleation, nucleus misorientation and capillarity against burnup."""
    plt = _pyplot()
    grid = Grid()
    p = uo2_parameters(temperature, which)
    # the relaxed GB depends only on Delta theta and the interface parameters
    initial = initial_state(p, grid, delta_theta_deg)
    eta_gb0 = initial[:grid.cells].min()
    gamma = gb_energy(p, grid, initial)
    print(f"UO2, T = {temperature:g} K, rho_{which}, c3 = {p.c3:.3f}, "
          f"Delta theta = {delta_theta_deg:g} deg: eta_GB = {eta_gb0:.3f}, "
          f"gamma = {gamma:.3f} J/m^2")
    print(f"{'bu':>6} {'rho':>9} {'eta_eq':>6} {'event':>9} {'t_rec':>6} "
          f"{'dth_nuc':>7} {'Theta_L':>7} {'r*':>8} {'r_n':>8} {'X':>5}")
    rows = []
    for bu in burnups:
        state, rho, out = run_burnup(bu, delta_theta_deg, temperature, p, which, grid=grid)
        r_star = critical_radius(p, gamma, rho)
        rows.append((bu, rho, out.eta_eq, out.nucleated, out.t_recovery,
                     out.nucleus_misorientation_deg, state.theta_deg, r_star,
                     state.subgrain_radius_m, state.restructured_fraction))
        print(f"{bu:6.1f} {rho:9.2e} {out.eta_eq:6.3f} {out.event:>9} "
              f"{out.t_recovery:6.0f} {out.nucleus_misorientation_deg:7.2f} "
              f"{state.theta_deg:7.2f} {r_star:8.2e} {state.subgrain_radius_m:8.2e} "
              f"{state.restructured_fraction:5.2f}")
    r = np.array(rows, dtype=float)
    nucl = r[:, 3] > 0

    fig, (a1, a2, a3) = plt.subplots(1, 3, figsize=(16, 3.9))
    a1.plot(r[:, 0], r[:, 2], "o-", label=rf"$\eta^{{eq}}(\rho_{{{which}}})$, Eq. (42)")
    a1.axhline(eta_gb0, color="tab:orange", label=r"initial GB depth $\eta^{GB}$")
    a1.plot(r[nucl, 0], r[nucl, 2], "k*", ms=9, label="1D nucleus")
    a1.set_ylabel(r"$\eta$")
    a1.set_title(rf"nucleation, $\Delta\theta$ = {delta_theta_deg:g}$^\circ$", fontsize=9)

    a2.plot(r[:, 0], r[:, 6], "s-", label=r"Landau $\Theta(bu)$")
    a2.plot(r[nucl, 0], r[nucl, 5], "k*", ms=9, label="nucleus, from the functional")
    a2.set_ylabel("misorientation [deg]")
    a2.set_title("misorientation: output of the functional vs Landau", fontsize=9)

    a3.semilogy(r[:, 0], r[:, 7] * 1e6, "o-", label=r"critical radius $r^*=2\gamma/(\lambda G b^2\rho/2)$")
    a3.semilogy(r[:, 0], r[:, 8] * 1e6, "s-", label=r"sub-grain radius $r_n$ (Landau)")
    a3.set_ylabel("radius [µm]")
    a3.set_title("capillarity, missing in 1D", fontsize=9)
    ax = a3.twinx()
    ax.plot(r[:, 0], r[:, 9], "k--", lw=1, label="Landau $X$")
    ax.set_ylabel("$X$")
    ax.set_ylim(0, 1.05)
    ax.legend(fontsize=8, loc="upper right")
    for a in (a1, a2, a3):
        a.set_xlabel("burnup [GWd/tU]")
        a.legend(fontsize=8, loc="lower left")
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")
    return rows


# ---------------------------------------------------------------------------
# C.3  Protocol 2: dislocations produced by irradiation
#
#      d rho/dt = S(bu) - rho C_D A(eta) <d eta/dt>,   S = d rho_ref/d bu * bu_rate
#      bu(t)    = bu_start + bu_rate t
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


def run_irradiation(p, delta_theta_deg, bu_start, bu_end, bu_rate,
                    temperature=REFERENCE_TEMPERATURE, which="tot", grid=None,
                    samples_per_gwd=4):
    """Relaxed GB at bu_start with rho = 0, then irradiate up to bu_end.

    The nucleus criterion is the one of run_case, with rho0 replaced by the
    density produced so far.
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

    # >>> UO2 INPUT: fixed parent misorientation
    y = initial_state(p, grid, delta_theta_deg)
    t_end = (bu_end - bu_start) / bu_rate
    times = np.linspace(0.0, t_end, int(samples_per_gwd * (bu_end - bu_start)) + 1)
    sol = evolve(p, grid, y, t_end, times, source=source,
                 max_step=0.25 / bu_rate)        # do not step over the source

    bu = bu_start + bu_rate * sol.t
    ref = np.interp(bu, bu_table, rho_table)
    rho_c = sol.y[2 * n + centre]
    eta_end = np.minimum(sol.y[0], sol.y[n - 1])
    hit = np.flatnonzero((ref > 0) & (rho_c < 0.1 * ref) & (eta_end > p.c2))
    bu_nuc = float(bu[hit[0]]) if hit.size else math.nan

    # a nucleus keeps an orientation between the two grains (Eq. 43);
    # if the centre rotates onto one of them, the GB has migrated (SIBM)
    event, misorientation = "none", math.nan
    if hit.size:
        later = min(np.searchsorted(bu, bu_nuc + 5.0), bu.size - 1)
        theta_later = math.degrees(sol.y[n + centre, later])
        inside = 0.1 * delta_theta_deg < theta_later < 0.9 * delta_theta_deg
        event = "nucleus" if inside else "migration"
        if inside:
            misorientation = nucleus_misorientation(theta_later, delta_theta_deg)
    return IrradiationOutcome(delta_theta_deg, bu_nuc, event, misorientation, bu, rho_c,
                              sol.y[2 * n:].mean(axis=0), ref, sol.y[centre], eta_end,
                              np.degrees(sol.y[n + centre]), sol)


def irradiation(angles, bu_start, bu_end, bu_rate, temperature, which, path):
    """run_irradiation for several parent misorientations, table and figure."""
    plt = _pyplot()
    _, p = uo2_at(bu_start, temperature, which=which)   # G at bu_start, kept constant
    print(f"UO2, T = {temperature:g} K, c3 = {p.c3:.3f}, source d rho_{which}/d bu, "
          f"bu {bu_start:g} -> {bu_end:g} GWd/tU at {bu_rate:g} GWd/tU/s")
    print(f"{'dtheta':>7} {'bu_rec':>7} {'event':>10} {'dth_nuc':>8} "
          f"{'Theta_Landau':>13} {'rho(bu_rec)':>12}")
    outcomes = []
    for angle in angles:
        out = run_irradiation(p, angle, bu_start, bu_end, bu_rate, temperature, which)
        outcomes.append(out)
        if math.isnan(out.bu_nucleation):
            print(f"{angle:7.2f} {'--':>7} {out.event:>10}")
        else:
            s = hbs_state(out.bu_nucleation, temperature)
            print(f"{angle:7.2f} {out.bu_nucleation:7.2f} {out.event:>10} "
                  f"{out.nucleus_misorientation_deg:8.2f} {s.theta_deg:13.2f} "
                  f"{np.interp(out.bu_nucleation, out.burnup, out.rho_ref):12.3e}")

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
    fig.suptitle(f"irradiation source, bu rate {bu_rate:g} GWd/tU/s (compressed), "
                 f"T = {temperature:g} K", fontsize=10)
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    print(f"wrote {path}")
    return outcomes


# ###########################################################################
#
#   RUN CONFIGURATION  (edit here)
#
# ###########################################################################

TEMPERATURE = REFERENCE_TEMPERATURE                     # K
FIGURES_DIR = "figures"
RHO_KIND = "tot"                    # rho fed to the SSD term: "tot" (Nogita) or "free" (Landau)
GRAIN_MISORIENTATION_DEG = 20.0     # deg, as-fabricated HAGB (Fig. 2 calibrated up to 30)
CASE_BURNUPS = [50.0, 70.0, 80.0, 100.0]                # GWd/tU, profile plots
SCAN_BURNUPS = [40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0, 110.0]
IRRADIATION_ANGLES = [15.0, 20.0, 30.0]                 # deg, parent HAGB misorientations
BU_RANGE = (0.0, 110.0)                                 # GWd/tU
BU_RATE = 1.0e-3                                        # GWd/tU per s, compressed


def main():
    os.makedirs(FIGURES_DIR, exist_ok=True)

    print("\n=== Cu: checks against [T26] ===")
    ok = selftest()

    print("\n=== Cu: figures of [T26] ===")
    plot_paper_figures(FIGURES_DIR)

    print("\n=== UO2, step protocol: profiles ===")
    for bu in CASE_BURNUPS:
        print(f"bu = {bu:g} GWd/tU")
        plot_burnup(bu, GRAIN_MISORIENTATION_DEG, TEMPERATURE, RHO_KIND,
                    f"{FIGURES_DIR}/tandogan_uo2_bu{bu:g}.png")

    print("\n=== UO2, step protocol: burnup scan ===")
    scan(SCAN_BURNUPS, GRAIN_MISORIENTATION_DEG, TEMPERATURE, RHO_KIND,
         f"{FIGURES_DIR}/tandogan_uo2_scan.png")

    print("\n=== UO2, irradiation source ===")
    irradiation(IRRADIATION_ANGLES, *BU_RANGE, BU_RATE, TEMPERATURE, RHO_KIND,
                f"{FIGURES_DIR}/tandogan_uo2_irradiation_{RHO_KIND}.png")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
