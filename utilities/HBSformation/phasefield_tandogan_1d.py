"""1D prototype of the Tandogan et al. HMP orientation phase field, driven by the
HBS Landau model.

Reference: I.T. Tandogan, M. Budnitzki, S. Sandfeld, "A multi-physics model for
dislocation driven spontaneous grain nucleation and microstructure evolution in
polycrystals", J. Mech. Phys. Solids 206 (2026) 106325.  Section 2 is followed;
equation numbers below are the paper's.

Reduction to what is needed here
--------------------------------
* 1D, x in [0, L], one grain boundary in the middle, zero-flux ends (mirror
  images of the neighbours instead of the paper's periodic cell).
* No mechanical loading: u = 0, no slip, so the Cosserat part only enforces the
  constraint e^e_skew = 0 (large mu_c).  With the eigenrotation initialised as
  in Eq. (27), theta = -e*, and Eq. (34) becomes an equation for theta:

      tau_hat * g(eta) * dtheta/dt = f0 * d/dx [ mu^2 g(eta) dtheta/dx ]      (34)

* Order parameter, Eq. (32), single slip system (r^2 = b^2 rho, h = 1):

      tau_eta * deta/dt = f0 nu^2 eta'' - f0 [alpha V' + mu^2 g' theta'^2]
                          - phi'(eta) * lambda/2 * G b^2 rho                  (32)

* Dislocations, Eq. (28) with only the recovery term (no plastic slip):

      drho/dt = - rho C_D A(eta) <deta/dt>                                     (28)

  In the fuel the source is irradiation, not |gamma_dot|.  Two protocols:
  --scan switches rho on as a step, like the paper does in Section 3.2;
  --irradiation replaces the Kocks-Mecking production by an irradiation source

      drho/dt = S(bu) - rho C_D A(eta) <deta/dt>,   S = d rho_ref/d bu * bu_rate

  uniform in x and independent of the local rho, so a recovered nucleus is
  loaded again.  rho_ref(bu) is rho_free of the Landau model (or rho_tot,
  Nogita-Une).  bu_rate is compressed: the phase field relaxes in ~1e2 s with
  the Cu mobilities, irradiation takes years; only rate << relaxation matters.

* V, g, C0, phi, A: Eqs. (35), (36), (37)-(38), (29); g and A are frozen above
  eta_cutoff = 1 - 1e-4, as in the paper.

Coupling to hbs_formation_landau.py
-----------------------------------
For a burnup bu, `hbs_state(bu, T)` gives
    Delta theta = state.theta_deg   the sub-grain misorientation (Landau Eq. 8)
    rho         = state.rho_free    the dislocations still in a random array,
                                    i.e. what Tandogan's SSD term represents
    G           = state.shear_modulus
and the phase field decides whether that boundary nucleates a dislocation-free
grain.  The interface parameters (f0, nu, alpha, mu, c, mobilities) are the Cu
values of Table 1: they are NOT calibrated for UO2.  c3 is set the way the
paper sets it, by the saturation value of rho: Eq. (42) gives the paper's
eta_eq = 0.579 at the rho_free of the burnup where Theta saturates.

Usage
-----
    python3 phasefield_tandogan_1d.py --selftest      Cu checks against the paper
    python3 phasefield_tandogan_1d.py --cu            Fig. 4 of the paper, redone
    python3 phasefield_tandogan_1d.py --case 100      profiles for one burnup
    python3 phasefield_tandogan_1d.py --scan          nucleation against burnup
    python3 phasefield_tandogan_1d.py --irradiation   rho produced by irradiation
"""

import argparse
import math
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

ETA_CUTOFF = 1.0 - 1.0e-4           # -      Section 2.2.2
T0 = 1.0                            # s      time unit of the mobilities


# ---------------------------------------------------------------------------
# PARAMETERS
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class PhaseFieldParameters:
    """Table 1 of the paper (Cu), plus the dislocation-energy inputs."""

    f0: float = 371.0e3             # Pa     normalization coefficient
    nu: float = 1.0e-6              # m
    alpha: float = 20.0             # -
    mu: float = 2.5e-6 / math.pi    # m
    c: float = 3.0                  # -      Read-Shockley log term, Eq. (35)
    tau_eta: float = 1.0e4          # f0*t0  heat-treatment value
    tau_hat: float = 1.0e1          # f0*t0  heat-treatment value
    lam: float = 0.3                # -      lambda
    shear_modulus: float = 75.0e9   # Pa     mu^e
    burgers: float = 0.2556e-9      # m
    c1: float = 100.0               # -      Fig. 4 of the paper
    c2: float = 0.95                # -      Fig. 4 of the paper
    c3: float = 1.7                 # -      Fig. 4 of the paper
    c_d: float = 100.0              # -      recovery, Eq. (28), fig. 4 of the paper

    @property
    def stored_energy_per_rho(self):
        """lambda/2 * G * b^2 [J/m]: multiplies rho in Eq. (15)."""
        return 0.5 * self.lam * self.shear_modulus * self.burgers ** 2


CU = PhaseFieldParameters()


def _g_raw(eta, c):
    return (7.0 * eta ** 3 - 6.0 * eta ** 4) / (1.0 - eta) ** 3 + c * np.log(1.0 - eta)


def g_offset(c): #TBC
    """Eq. (36): shift so that g >= 0.01 on [0, 1)."""
    eta = np.linspace(0.0, ETA_CUTOFF, 200001)
    return -min(_g_raw(eta, c).min(), 0.0) + 0.01


# ---------------------------------------------------------------------------
# EQ. (29), (35), (37)-(38): THE FUNCTIONS OF eta
# ---------------------------------------------------------------------------

def potential_derivative(eta):
    """V'(eta), V = (1 - eta)^2 / 2."""
    return -(1.0 - eta)


def coupling_g(eta, c, offset):
    """g(eta) and g'(eta), frozen above ETA_CUTOFF."""
    e = np.minimum(eta, ETA_CUTOFF)
    g = _g_raw(e, c) + offset
    dg = ((21.0 * e ** 2 - 24.0 * e ** 3) / (1.0 - e) ** 3
          + 3.0 * (7.0 * e ** 3 - 6.0 * e ** 4) / (1.0 - e) ** 4
          - c / (1.0 - e))
    dg = np.where(eta < ETA_CUTOFF, dg, 0.0)
    return g, dg


def phi_derivative(eta, p):
    """Eq. (38): the step that confines the stored-energy force to the GB."""
    return 0.5 * p.c3 * (1.0 - np.tanh(p.c1 * (eta - p.c2)))


def recovery_localiser(eta):
    """Eq. (29), A(eta), frozen above ETA_CUTOFF."""
    e = np.minimum(eta, ETA_CUTOFF)
    return (7.0 * e ** 3 - 6.0 * e ** 4) / (1.0 - e)


def eta_equilibrium(rho, p):
    """Eq. (42), with phi'(eta_eq) = c3."""
    return 1.0 - p.c3 * p.stored_energy_per_rho * rho / (p.f0 * p.alpha)


# ---------------------------------------------------------------------------
# THE 1D SOLVER
# ---------------------------------------------------------------------------

@dataclass
class Grid:
    length: float = 10.0e-6         # m
    cells: int = 400

    @property
    def dx(self):
        return self.length / self.cells

    @property
    def x(self):
        return (np.arange(self.cells) + 0.5) * self.dx


def _right_hand_side(p, grid, offset, source=None):
    """source(t) [m^-2/s]: dislocation production, uniform in x (None: none)."""
    n, dx = grid.cells, grid.dx
    tau_eta = p.tau_eta * p.f0 * T0
    tau_hat = p.tau_hat * p.f0 * T0
    energy = p.stored_energy_per_rho

    def rhs(_t, y):
        eta, theta, rho = y[:n], y[n:2 * n], y[2 * n:]
        g, dg = coupling_g(eta, p.c, offset)

        # zero-flux ends: face gradients at the two walls are zero
        dtheta = np.zeros(n + 1)
        dtheta[1:-1] = np.diff(theta) / dx
        deta = np.zeros(n + 1)
        deta[1:-1] = np.diff(eta) / dx

        g_face = np.zeros(n + 1)
        g_face[1:-1] = 2.0 * g[:-1] * g[1:] / (g[:-1] + g[1:])     # series average
        grad_theta_sq = 0.5 * (dtheta[:-1] ** 2 + dtheta[1:] ** 2)

        # Eq. (34)
        flux = p.mu ** 2 * g_face * dtheta
        theta_dot = p.f0 * np.diff(flux) / dx / (tau_hat * g)

        # Eq. (32)
        eta_dot = (p.f0 * p.nu ** 2 * np.diff(deta) / dx
                   - p.f0 * (p.alpha * potential_derivative(eta)
                             + p.mu ** 2 * dg * grad_theta_sq)
                   - phi_derivative(eta, p) * energy * rho) / tau_eta

        # Eq. (28), recovery only
        rho_dot = -rho * p.c_d * recovery_localiser(eta) * np.maximum(eta_dot, 0.0)
        if source is not None:
            rho_dot = rho_dot + source(_t)

        return np.concatenate([eta_dot, theta_dot, rho_dot])

    return rhs


def _sparsity(n):
    """Nearest-neighbour coupling inside and between the three fields."""
    band = diags([1.0, 1.0, 1.0], [-1, 0, 1], shape=(n, n)).toarray()
    return np.block([[band, band, band]] * 3)


def evolve(p, grid, y0, t_end, times, source=None, max_step=np.inf):
    rhs = _right_hand_side(p, grid, g_offset(p.c), source)
    sol = solve_ivp(rhs, (0.0, t_end), y0, method="BDF", t_eval=times, max_step=max_step,
                    jac_sparsity=_sparsity(grid.cells), rtol=1e-5, atol=1e-8)
    if not sol.success:
        raise RuntimeError(sol.message)
    return sol


def initial_state(p, grid, delta_theta_deg, eta0=0.99):
    """eta = 0.99, theta a sharp step at L/2, then relaxed without dislocations."""
    n = grid.cells
    theta = math.radians(delta_theta_deg) * 0.5 * (
        1.0 + np.tanh((grid.x - 0.5 * grid.length) / (2.0 * grid.dx)))
    y0 = np.concatenate([np.full(n, eta0), theta, np.zeros(n)])
    relax = evolve(replace(p, tau_eta=1.0e2), grid, y0, 2.0e4, [2.0e4])
    return relax.y[:, -1]


@dataclass
class Outcome:
    delta_theta_deg: float
    rho0: float
    eta_eq: float
    eta_gb0: float                  # GB depth before rho is switched on
    nucleated: bool
    t_recovery: float               # s, first time rho(centre) < 0.1 rho0 (nan: never)
    recrystallised: float           # fraction of cells with rho < 0.1 rho0 at t_end
    theta_centre_deg: float
    initial: np.ndarray             # relaxed GB, before rho is switched on
    solution: object


def run_case(p, delta_theta_deg, rho0, t_end=1.0e4, grid=None, samples=(), every=50.0):
    """Section 3.2 protocol: relax the GB, switch on a uniform rho0, evolve.

    Nucleation, as in the paper, means that the recovery at the GB centre starts
    while the two grains are still crystalline (eta at the ends above c2).  If
    the widened GB first fills the whole domain, the orientation gradient
    vanishes and the uniform eta_eq state recovers everywhere at once: that is
    recorded as t_recovery but not as a nucleus.
    """
    grid = grid or Grid()
    n, centre = grid.cells, grid.cells // 2
    y = initial_state(p, grid, delta_theta_deg)
    eta_gb0 = y[:n].min()
    relaxed = y.copy()
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


def gb_energy(p, grid, y):
    """GB energy [J/m^2] of a profile without dislocations: Eq. (15), first line.

    The bulk contributes nothing (V(1) = 0, no gradients), so the integral over
    the domain is the energy per unit GB area; this is what Fig. 2 calibrates.
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
    """Capillarity, absent in 1D: a nucleus of radius r survives if the stored
    energy beats the curvature pressure, lambda/2 G b^2 rho > 2 gamma / r."""
    return 2.0 * gamma / (p.stored_energy_per_rho * rho)


def state_at(outcome, t):
    """Column of the solution closest to time t."""
    return outcome.solution.y[:, int(np.argmin(np.abs(outcome.solution.t - t)))]


# ---------------------------------------------------------------------------
# COUPLING TO THE LANDAU MODEL
# ---------------------------------------------------------------------------

def uo2_parameters(temperature=REFERENCE_TEMPERATURE, rho_saturation=None, **changes):
    """Table 1 interface, UO2 dislocation energy, c3 set by the saturation rho.

    rho_saturation defaults to rho_free at the burnup where Theta reaches the
    HAGB cap, and c3 is chosen so that Eq. (42) returns the paper's 0.579 there.
    """
    _, _, bu_saturation = regime_boundaries()
    state = hbs_state(bu_saturation, temperature)
    if rho_saturation is None:
        rho_saturation = state.rho_free
    p = replace(CU, shear_modulus=state.shear_modulus, burgers=BURGERS)
    eta_eq_paper = eta_equilibrium(2.5e15, CU)
    c3 = (1.0 - eta_eq_paper) * p.f0 * p.alpha / (p.stored_energy_per_rho * rho_saturation)
    return replace(p, c3=c3, **changes)


def run_burnup(burnup, temperature=REFERENCE_TEMPERATURE, p=None, **keywords):
    state = hbs_state(burnup, temperature)
    p = p or uo2_parameters(temperature)
    p = replace(p, shear_modulus=state.shear_modulus)
    return state, run_case(p, state.theta_deg, state.rho_free, **keywords)


# ---------------------------------------------------------------------------
# IRRADIATION SOURCE
# ---------------------------------------------------------------------------

def reference_density(temperature=REFERENCE_TEMPERATURE, which="free"):
    """rho_ref(bu): rho_free (Landau, Eq. 5) or rho_tot (Nogita-Une, Eq. 1)."""
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
    rho_ref: np.ndarray
    eta_centre: np.ndarray
    eta_end: np.ndarray
    theta_centre_deg: np.ndarray
    solution: object


def run_irradiation(p, delta_theta_deg, bu_start, bu_end, bu_rate,
                    temperature=REFERENCE_TEMPERATURE, which="free", grid=None,
                    samples_per_gwd=4):
    """Relaxed GB at bu_start with rho = 0, then irradiate.

    bu(t) = bu_start + bu_rate t, and without recovery rho(bu) = rho_ref(bu) -
    rho_ref(bu_start): no step, the stored energy builds up only through S.
    The nucleus criterion is the one of run_case, with rho0 replaced by that
    produced density.
    """
    grid = grid or Grid()
    n, centre = grid.cells, grid.cells // 2
    rho_ref = reference_density(temperature, which)

    # the source, tabulated once: S(bu) = d rho_ref / d bu * bu_rate
    bu_table = np.linspace(bu_start, bu_end, int(20 * (bu_end - bu_start)) + 2)
    rho_table = np.array([rho_ref(b) for b in bu_table])
    rate_table = np.gradient(rho_table, bu_table) * bu_rate

    def source(t):
        return float(np.interp(bu_start + bu_rate * t, bu_table, rate_table))

    rho_table = rho_table - rho_table[0]          # produced since bu_start
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

    # a nucleus keeps an orientation between the two grains (Eq. 43); if the
    # centre rotates onto one of them, the GB has migrated instead (rule 3)
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
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    p = uo2_parameters(temperature)
    p = replace(p, shear_modulus=hbs_state(bu_start, temperature).shear_modulus)
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


# ---------------------------------------------------------------------------
# OUTPUT
# ---------------------------------------------------------------------------

def plot_profiles(p, delta_theta_deg, rho0, path, title, grid=None):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    grid = grid or Grid()
    n = grid.cells
    times = [0.0, 1e1, 1e2, 1e3, 2e3, 5e3, 1e4]
    colors = ["tab:red", "tab:blue", "tab:green", "tab:orange", "tab:purple", "tab:brown", "tab:pink"]
    fig, axes = plt.subplots(1, 3, figsize=(13, 3.8))
#    for c_d, colour in [(0.0, "tab:red"), (p.c_d, "tab:blue")]:
    for c_d, colour in [(p.c_d, "tab:blue")]:
        out = run_case(replace(p, c_d=c_d), delta_theta_deg, rho0,
                       t_end=times[-1], grid=grid, samples=times)
        print(f"  C_D = {c_d:g}: nucleated = {out.nucleated}, "
              f"t_recovery = {out.t_recovery:g} s")
        x = grid.x * 1e6
        for k, colour in enumerate(colors):
            y = state_at(out, times[k])
            label = f"$C_D$={c_d:g}, t={times[k]:g} s"
            axes[0].plot(x, y[:n], color=colour, label=label)
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


def scan(burnups, temperature, path):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

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


def selftest():
    ok = True

    def check(name, cond, detail):
        nonlocal ok
        ok &= bool(cond)
        print(f"[{'PASS' if cond else 'FAIL'}] {name}: {detail}")

    rho0 = 2.5e15
    eq = eta_equilibrium(rho0, CU)
    check("Eq. (42), Cu", abs(eq - 0.579) < 1e-3, f"eta_eq = {eq:.4f} (paper 0.579)")

    g, _ = coupling_g(np.linspace(0, ETA_CUTOFF, 1001), CU.c, g_offset(CU.c))
    check("g >= 0.01", g.min() > 0.0099, f"min g = {g.min():.4f}")

    no_rec = run_case(replace(CU, c_d=0.0), 15.0, rho0, t_end=1.0e4)
    eta_c = no_rec.solution.y[Grid().cells // 2, -1]
    check("Cu 15 deg, C_D = 0: GB widens to eta_eq", abs(eta_c - eq) < 0.03,
          f"eta(centre) = {eta_c:.3f}, no nucleus = {not no_rec.nucleated}")

    rec = run_case(CU, 15.0, rho0, t_end=1.0e4)
    check("Cu 15 deg, C_D = 100: nucleus", rec.nucleated,
          f"theta_centre = {rec.theta_centre_deg:.2f} deg, recrystallised {rec.recrystallised:.2f}")

    gamma = gb_energy(CU, Grid(), no_rec.initial)
    check("Cu 15 deg GB energy vs Fig. 2", 0.6 < gamma < 0.9, f"gamma = {gamma:.3f} J/m^2 (Fig. 2: ~0.75)")

    for angle, expected, paper in [(10.0, True, "immediate"),
                                   (5.0, True, "delayed, ~4e3 s"),
                                   (2.5, False, "never")]:
        out = run_case(CU, angle, rho0, t_end=1.0e4)
        check(f"Cu {angle:g} deg: nucleus = {expected} (paper: {paper})",
              out.nucleated == expected,
              f"t_recovery = {out.t_recovery:g} s")
    return ok


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--selftest", action="store_true", help="Cu checks against the paper")
    parser.add_argument("--cu", action="store_true", help="redo Fig. 4 of the paper")
    parser.add_argument("--case", type=float, metavar="BU", help="profiles at one burnup")
    parser.add_argument("--scan", action="store_true", help="nucleation against burnup")
    parser.add_argument("--burnups", type=float, nargs="+",
                        default=[40, 50, 60, 70, 80, 90, 100, 110])
    parser.add_argument("--irradiation", action="store_true",
                        help="rho produced by irradiation, for several misorientations")
    parser.add_argument("--angles", type=float, nargs="+", default=[0.5, 1.0, 2.0, 5.0, 10.0])
    parser.add_argument("--bu-range", type=float, nargs=2, default=[0.0, 110.0],
                        metavar=("START", "END"))
    parser.add_argument("--bu-rate", type=float, default=1.0e-3,
                        help="GWd/tU per s, compressed (default 1e-3)")
    parser.add_argument("--source", choices=["free", "tot"], default="free",
                        help="rho_ref for the source: rho_free (Landau) or rho_tot (Nogita-Une)")
    parser.add_argument("--temperature", type=float, default=REFERENCE_TEMPERATURE)
    parser.add_argument("--figures-dir", default="figures", metavar="DIR")
    args = parser.parse_args(argv)

    if not (args.selftest or args.cu or args.case is not None or args.scan
            or args.irradiation):
        parser.print_help()
        return 0
    status = 0
    if args.selftest:
        status |= 0 if selftest() else 1
    if args.cu:
        plot_profiles(CU, 15.0, 2.5e15, f"{args.figures_dir}/tandogan_cu_fig4.png",
                      r"Cu, $\Delta\theta=15^\circ$, $\rho_0=2.5\times10^{15}$ m$^{-2}$")
    if args.case is not None:
        p = uo2_parameters(args.temperature)
        state = hbs_state(args.case, args.temperature)
        p = replace(p, shear_modulus=state.shear_modulus)
        plot_profiles(p, state.theta_deg, state.rho_free,
                      f"{args.figures_dir}/tandogan_uo2_bu{args.case:g}.png",
                      f"UO$_2$, bu = {args.case:g} GWd/tU: "
                      rf"$\Theta$ = {state.theta_deg:.2f}$^\circ$, "
                      rf"$\rho_{{free}}$ = {state.rho_free:.2e} m$^{{-2}}$, $c_3$ = {p.c3:.2f}")
    if args.scan:
        scan(args.burnups, args.temperature, f"{args.figures_dir}/tandogan_uo2_scan.png")
    if args.irradiation:
        irradiation(args.angles, *args.bu_range, args.bu_rate, args.temperature,
                    args.source, f"{args.figures_dir}/tandogan_uo2_irradiation_{args.source}.png")
    return status


if __name__ == "__main__":
    sys.exit(main())
