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
        + phi(eta) * sum_a lambda/2 G r_a^2,         r_a^2 = b^2 sum_b h_abFirst t rho_b   (21)

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
==========================================================================
For a burnup bu, hbs_state(bu, T) of [HBS] provides:

  Delta theta = state.theta_deg  (Landau Eq. 8), sub-grain misorientation
      -> initial_state(): height of the theta step at L/2.
         Used by run_burnup() (profiles, scan).  In the irradiation protocol
         the angle is NOT taken from hbs_state: it is a fixed input
         (IRRADIATION_ANGLES) that does not follow Theta(bu).
  rho = state.rho_free  (Landau Eq. 5), dislocations still in a random array,
        i.e. what the SSD term of [T26] represents
      -> PROTOCOL 1, step (as Sec. 3.2): run_case() sets rho = rho_free
         uniformly at t = 0 after relaxing the GB. 
         TBD: i am not sure about this choice the fact is that i have the total dislocation density of nogita
         and the model i developed was based on the total dislocation density to recover the free and the one not free SSD that
         probably gives GND because it piles up at the GB giving misorinetation. But does the tandogan model do this automatically?
         Does the model automatically calculate the suborientation without having my model to do the split?
  S(bu) = d rho_ref/d bu * bu_rate,   rho_ref = rho_free or rho_tot
      -> irradiation: run_irradiation() builds S, and _right_hand_side() 
         adds it to d rho/dt.  It takes the place of the
         Kocks-Mecking production term of Eq. (28) (irradiation instead of
         plastic slip).  Uniform in x and independent of the local rho, so a
         recovered nucleus is loaded again.
  G = state.shear_modulus
      -> PhaseFieldParameters.stored_energy_per_rho, i.e. f_eta4 in Eq. (32).
  c3 (not from hbs_state directly) TBD -> not clear to me
      -> uo2_parameters(): chosen as the paper does (c3 = 1.7 <-> eta_eq =
         0.579 at the saturation rho = 2.5e15 of Cu).  Here: Eq. (42) gives
         0.579 at the rho_free of the burnup where Theta saturates.


4. LIMITATIONS
==============
1. 1D: no GB curvature, hence no capillary pressure; a nucleus of any size
   survives.  critical_radius() estimates r* = 2 gamma / (lambda/2 G b^2 rho)
   a posteriori, to be compared with the Landau sub-grain radius.
2. No mechanics: no slip, no stress, no Kocks-Mecking production; tau_eta is
   constant (the switch of Eq. 39 is not used).
3. One slip system (h = 1), isotropic GB energy (a(n_gb, theta) = 1).
4. The GB energy is calibrated (Fig. 2) only up to 30 deg for Cu <100> tilt
   boundaries; no HAGB cusps. Landau Theta up to the HAGB cap is outside
   the calibrated range. Need to be calibrated for UO2 values. TBD: later on, leave this comment
5. For UO2 the interface parameters (f0, nu, alpha, mu, c, tau_eta, tau_hat)
   are still the Cu values of Table 1; only G, b and c3 are changed.  With
   t0 = 1 s the times are not physical. TBD-> maybe we should identify which are pure calibration and which have a physical correspondance for uo2
6. Irradiation protocol: the burnup rate is compressed (the phase field
   relaxes in ~1e2 s, irradiation takes years; only rate << relaxation
   matters); the source is uniform; no recovery other than Eq. (28); the
   misorientation is fixed and does not evolve with Theta(bu).
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
  the singularity.  The state functions are checked in selftest() (g >= 0.01,
  Eq. 42) as [PFRP] recommends ("plot your state functions"). TBD: do the plots!
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

    The minimum is found on a fine grid; if g_raw is already positive, only
    the 0.01 is added.  selftest() checks min g = 0.01.
    """
    eta = np.linspace(0.0, ETA_CUTOFF, 200001)
    return -min(_g_raw(eta, c).min(), 0.0) + 0.01 # TBD: why the minus?


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
    # >>> UO2 INPUT: delta_theta_deg is the misorientation; for UO2 it is
    #     hbs_state(bu).theta_deg (run_burnup) or a fixed angle (irradiation).
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
#   PART B - Cu: REPRODUCTION OF [T26], SECTION 3.2 (Figs. 2, 4, 5)
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
    rho0: float
    eta_eq: float                   # Eq. (42)
    eta_gb0: float                  # GB depth before rho is switched on
    nucleated: bool
    t_recovery: float               # s, first time rho(centre) < 0.1 rho0 (nan: never)
    recrystallised: float           # fraction of cells with rho < 0.1 rho0 at t_end
    theta_centre_deg: float
    initial: np.ndarray             # relaxed GB, before rho is switched on
    solution: object


def run_case(p, delta_theta_deg, rho0, t_end=1.0e4, grid=None, samples=(), every=50.0):
    """Step protocol of Sec. 3.2: relax the GB, switch on a uniform rho0, evolve.

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

    # >>> UO2 INPUT (step protocol): rho0 is hbs_state(bu).rho_free for UO2
    y[2 * n:] = rho0

    times = np.union1d(np.arange(every, t_end + 0.5 * every, every), samples)
    sol = evolve(p, grid, y, t_end, times)

    rho_c = sol.y[2 * n + centre]
    hit = np.flatnonzero(rho_c < 0.1 * rho0) if rho0 > 0 else np.array([], int)
    t_recovery = float(sol.t[hit[0]]) if hit.size else math.nan
    ends_crystalline = hit.size and min(sol.y[0, hit[0]], sol.y[n - 1, hit[0]]) > p.c2
    # recovery alone is not a nucleus: the centre must also become crystalline
    centre_ordered = hit.size and sol.y[centre, hit[0]:].max() > 0.95

    theta_c = math.degrees(sol.y[n + centre, -1])
    rho = sol.y[2 * n:, -1]
    recrystallised = float(np.mean(rho < 0.1 * rho0)) if rho0 > 0 else 0.0
    return Outcome(delta_theta_deg, rho0, eta_equilibrium(rho0, p), eta_gb0,
                   bool(ends_crystalline and centre_ordered), t_recovery, recrystallised, theta_c,
                   relaxed, sol)


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
    print(f"  C_D = {p.c_d:g}: nucleated = {out.nucleated}, "
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


def selftest(): # TBD fai anche i plit dei paper
    """Cu checks against [T26]; returns True if all pass."""
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
# C.1  Parameters
# ---------------------------------------------------------------------------

def uo2_parameters(temperature=REFERENCE_TEMPERATURE, rho_saturation=None, **changes):
    """Cu interface of Table 1 (limitation 5) + UO2 G, b and c3.

    c3 is set the way [T26] sets it (c3 = 1.7 <-> eta_eq = 0.579 at the
    saturation rho of Cu): Eq. (42) returns 0.579 at rho_saturation, by
    default rho_free at the burnup where Landau Theta reaches the HAGB cap.
    G here is the one at that burnup; callers replace it with G(bu).
    """
    _, _, bu_saturation = regime_boundaries()
    state = hbs_state(bu_saturation, temperature)
    if rho_saturation is None:
        rho_saturation = state.rho_free
    p = replace(CU, shear_modulus=state.shear_modulus, burgers=BURGERS)
    # >>> UO2 INPUT: c3 from the saturation dislocation density
    eta_eq_paper = eta_equilibrium(CU_RHO0, CU)
    c3 = (1.0 - eta_eq_paper) * p.f0 * p.alpha / (p.stored_energy_per_rho * rho_saturation)
    return replace(p, c3=c3, **changes)


def uo2_at(burnup, temperature=REFERENCE_TEMPERATURE, p=None):
    """Landau state at `burnup` and the parameters with G(burnup)."""
    state = hbs_state(burnup, temperature)
    p = p or uo2_parameters(temperature)
    # >>> UO2 INPUT: shear modulus G(bu, T), enters f_eta4 of Eq. (32)
    return state, replace(p, shear_modulus=state.shear_modulus)


# ---------------------------------------------------------------------------
# C.2  Protocol 1: step in rho (as Sec. 3.2), one run per burnup
# ---------------------------------------------------------------------------

def run_burnup(burnup, temperature=REFERENCE_TEMPERATURE, p=None, **keywords):
    """Paper protocol with the Landau inputs of one burnup."""
    state, p = uo2_at(burnup, temperature, p)
    # >>> UO2 INPUT: misorientation Theta(bu) and rho_free(bu) from hbs_state
    return state, run_case(p, state.theta_deg, state.rho_free, **keywords)


def plot_burnup(burnup, temperature, path):
    """Fig. 4-like profiles for one burnup."""
    state, p = uo2_at(burnup, temperature)
    plot_profiles(p, state.theta_deg, state.rho_free, path,
                  f"UO$_2$, bu = {burnup:g} GWd/tU: "
                  rf"$\Theta$ = {state.theta_deg:.2f}$^\circ$, "
                  rf"$\rho_{{free}}$ = {state.rho_free:.2e} m$^{{-2}}$, $c_3$ = {p.c3:.2f}")


def scan(burnups, temperature, path):
    """Nucleation criterion and capillarity estimate against burnup."""
    plt = _pyplot()
    p = uo2_parameters(temperature)
    print(f"UO2, T = {temperature:g} K, c3 = {p.c3:.3f}")
    print(f"{'bu':>6} {'Theta':>6} {'rho_free':>9} {'eta_eq':>6} {'eta_gb':>6} "
          f"{'nucl':>5} {'t_rec':>6} {'gamma':>6} {'r*':>8} {'r_n':>8} {'X':>5}")
    rows = []
    grid = Grid()
    for bu in burnups:
        state, out = run_burnup(bu, temperature, p, grid=grid)
        gamma = gb_energy(p, grid, out.initial)
        r_star = critical_radius(p, gamma, state.rho_free)
        rows.append((bu, state.theta_deg, state.rho_free, out.eta_eq, out.eta_gb0,
                     out.nucleated, out.t_recovery, gamma, r_star,
                     state.subgrain_radius_m, state.restructured_fraction))
        print(f"{bu:6.1f} {state.theta_deg:6.2f} {state.rho_free:9.2e} {out.eta_eq:6.3f} "
              f"{out.eta_gb0:6.3f} {out.nucleated!s:>5} {out.t_recovery:6.0f} "
              f"{gamma:6.3f} {r_star:8.2e} {state.subgrain_radius_m:8.2e} "
              f"{state.restructured_fraction:5.2f}")
    r = np.array(rows, dtype=float)

    fig, (a1, a2) = plt.subplots(1, 2, figsize=(11, 3.8))
    a1.plot(r[:, 0], r[:, 3], "o-", label=r"$\eta_T^{eq}(\rho_{free})$, Eq. (42)")
    a1.plot(r[:, 0], r[:, 4], "s-", label=r"GB depth $\eta_T^{GB}(\Theta)$")
    nucl = r[:, 5] > 0
    a1.plot(r[nucl, 0], r[nucl, 4], "k*", ms=9, label="1D nucleus")
    a1.set_xlabel("burnup [GWd/tU]")
    a1.set_ylabel(r"$\eta_T$")
    a1.set_title("Tandogan criterion: nucleus if $\\eta_T^{GB} < \\eta_T^{eq}$", fontsize=9)
    a1.legend(fontsize=8)

    a2.semilogy(r[:, 0], r[:, 8] * 1e6, "o-", label=r"critical radius $r^*=2\gamma/(\lambda G b^2\rho/2)$")
    a2.semilogy(r[:, 0], r[:, 9] * 1e6, "s-", label=r"sub-grain radius $r_n$ (Landau)")
    a2.set_xlabel("burnup [GWd/tU]")
    a2.set_ylabel("radius [µm]")
    a2.set_title("capillarity, missing in 1D", fontsize=9)
    ax = a2.twinx()
    ax.plot(r[:, 0], r[:, 10], "k--", lw=1, label="Landau $X$")
    ax.set_ylabel("$X$")
    ax.set_ylim(0, 1.05)
    a2.legend(fontsize=8, loc="lower left")
    ax.legend(fontsize=8, loc="upper right")
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
#      stored energy builds up only through S.  The misorientation is a fixed
#      input angle (limitation 6).
# ---------------------------------------------------------------------------

def reference_density(temperature=REFERENCE_TEMPERATURE, which="free"):
    """rho_ref(bu): rho_free (Landau, Eq. 5) or rho_tot (Nogita-Une, Eq. 1) of [HBS]."""
    key = {"free": "rho_free", "tot": "rho_tot"}[which]

    def rho_ref(bu):
        return getattr(hbs_state(bu, temperature), key)
    return rho_ref


@dataclass
class IrradiationOutcome:
    delta_theta_deg: float
    bu_nucleation: float            # GWd/tU, first recovery at the GB centre (nan: none)
    event: str                      # "nucleus", "migration" (SIBM) or "none"
    burnup: np.ndarray              # sampled burnups
    rho_centre: np.ndarray
    rho_mean: np.ndarray
    rho_ref: np.ndarray             # rho produced since bu_start, without recovery
    eta_centre: np.ndarray
    eta_end: np.ndarray
    theta_centre_deg: np.ndarray
    solution: object


def run_irradiation(p, delta_theta_deg, bu_start, bu_end, bu_rate,
                    temperature=REFERENCE_TEMPERATURE, which="free", grid=None,
                    samples_per_gwd=4):
    """Relaxed GB at bu_start with rho = 0, then irradiate up to bu_end.

    The nucleus criterion is the one of run_case, with rho0 replaced by the
    density produced so far.
    """
    grid = grid or Grid()
    n, centre = grid.cells, grid.cells // 2
    rho_ref = reference_density(temperature, which)

    # >>> DISLOCATION SOURCE: S(bu) = d rho_ref/d bu * bu_rate, tabulated once
    bu_table = np.linspace(bu_start, bu_end, int(20 * (bu_end - bu_start)) + 2)
    rho_table = np.array([rho_ref(b) for b in bu_table])
    rate_table = np.gradient(rho_table, bu_table) * bu_rate

    def source(t):
        return float(np.interp(bu_start + bu_rate * t, bu_table, rate_table))

    rho_table = rho_table - rho_table[0]          # produced since bu_start

    # >>> UO2 INPUT: fixed misorientation, not Theta(bu)
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

    # a nucleus keeps an orientation between the two grains (Sec. 3.2.2);
    # if the centre rotates onto one of them, the GB has migrated (SIBM)
    event = "none"
    if hit.size:
        later = min(np.searchsorted(bu, bu_nuc + 5.0), bu.size - 1)
        theta_later = math.degrees(sol.y[n + centre, later])
        inside = 0.1 * delta_theta_deg < theta_later < 0.9 * delta_theta_deg
        event = "nucleus" if inside else "migration"
    return IrradiationOutcome(delta_theta_deg, bu_nuc, event, bu, rho_c,
                              sol.y[2 * n:].mean(axis=0), ref, sol.y[centre], eta_end,
                              np.degrees(sol.y[n + centre]), sol)


def irradiation(angles, bu_start, bu_end, bu_rate, temperature, which, path):
    """run_irradiation for several misorientations, table and figure."""
    plt = _pyplot()
    _, p = uo2_at(bu_start, temperature)          # G at bu_start, kept constant
    print(f"UO2, T = {temperature:g} K, c3 = {p.c3:.3f}, source d rho_{which}/d bu, "
          f"bu {bu_start:g} -> {bu_end:g} GWd/tU at {bu_rate:g} GWd/tU/s")
    print(f"{'dtheta':>7} {'bu_rec':>7} {'event':>10} {'Theta_Landau':>13} {'rho(bu_rec)':>12}")
    outcomes = []
    for angle in angles:
        out = run_irradiation(p, angle, bu_start, bu_end, bu_rate, temperature, which)
        outcomes.append(out)
        if math.isnan(out.bu_nucleation):
            print(f"{angle:7.2f} {'--':>7} {out.event:>10}")
        else:
            s = hbs_state(out.bu_nucleation, temperature)
            print(f"{angle:7.2f} {out.bu_nucleation:7.2f} {out.event:>10} {s.theta_deg:13.2f} "
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
CASE_BURNUPS = [50.0, 70.0, 80.0, 100.0]                # GWd/tU, profile plots
SCAN_BURNUPS = [40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0, 110.0]
IRRADIATION_ANGLES = [0.5, 1.0, 2.0, 5.0, 10.0]         # deg, fixed misorientations
BU_RANGE = (0.0, 110.0)                                 # GWd/tU
BU_RATE = 1.0e-3                                        # GWd/tU per s, compressed
SOURCE = "tot"                                         # rho_ref: "free" or "tot"


def main():
    os.makedirs(FIGURES_DIR, exist_ok=True)

    print("\n=== Cu: checks against [T26] ===")
    ok = selftest()

    print("\n=== Cu: Fig. 4 ===")
    plot_profiles(CU, CU_DELTA_THETA, CU_RHO0, f"{FIGURES_DIR}/tandogan_cu_fig4.png",
                  r"Cu, $\Delta\theta=15^\circ$, $\rho_0=2.5\times10^{15}$ m$^{-2}$")

    print("\n=== UO2, step protocol: profiles ===")
    for bu in CASE_BURNUPS:
        print(f"bu = {bu:g} GWd/tU")
        plot_burnup(bu, TEMPERATURE, f"{FIGURES_DIR}/tandogan_uo2_bu{bu:g}.png")

    print("\n=== UO2, step protocol: burnup scan ===")
    scan(SCAN_BURNUPS, TEMPERATURE, f"{FIGURES_DIR}/tandogan_uo2_scan.png")

    print("\n=== UO2, irradiation source ===")
    irradiation(IRRADIATION_ANGLES, *BU_RANGE, BU_RATE, TEMPERATURE, SOURCE,
                f"{FIGURES_DIR}/tandogan_uo2_irradiation_{SOURCE}.png")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
