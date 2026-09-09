"""1D exploratory prototype of a phase-field functional for HBS sub-grain nucleation.

This is NOT a phase-field solver.  It is a scratch pad to look at the pieces of a
possible functional before anything gets near a real PDE code (MOOSE, Sciantix).

The functional follows Muramatsu et al., Comput. Mater. Sci. 87 (2014) 112-122,
Eq. (2) for the total free energy and Eq. (7) for the local free energy density
f(phi), but f(phi) is rewritten so its driving force and its nucleation
threshold are the ones ALREADY calibrated in `hbs_formation_landau.py`:

    g(phi) = h(phi) = phi^2                       Eq. (7)/Appendix A, same as the paper
    q(phi) = phi^2 (1-phi)^2                       double-well barrier, same as the paper
    p(phi) = phi^3 (10 - 15 phi + 6 phi^2)         monotone interpolation, same as the paper

    f_paper(phi; E_s) = a_ub * p(phi) * E_s + a_wg * W_b * q(phi)         Eq. (7), f_m=0

    state = hbs_state(bu, T)            # hbs_formation_landau.py, Eqs. (1)-(11)
    f_new(phi; bu, T) = state.c0 * (1 - p(phi) * state.eta) + a_wg * W_b * q(phi)

`state.c0` is already rho_tot*A1*G*b^2 (the free-dislocation energy of the HBS
Landau model); `state.eta` is already the solution of that Landau functional in
the misorientation angle, zero below `BU_THRESHOLD` and rising smoothly above
it.  Reusing them means the phase-field threshold IS the HBS threshold, not a
new independent one.

At phi=0 (matrix): f_new = state.c0, always.
At phi=1 (sub-grain): f_new = state.c0 * (1 - state.eta).
Below BU_THRESHOLD, eta=0, the two are equal: no driving force, phi=0 is the
only minimum.  Above BU_THRESHOLD, eta>0 opens a second, deepening minimum near
phi=1 -- the nucleation threshold asked for.

theta0(bu, T) = state.theta_deg is reported as a diagnostic only (the
orientation a nucleus would be seeded with); this first prototype freezes the
orientation field theta uniform, so the paper's grain-boundary term
g(phi)*||grad theta|| vanishes identically and only phi is evolved.

Parameters M_PHI (mobility), ALPHA2 (gradient-energy coefficient) and
A_WG_WB (barrier height) are illustrative, NOT calibrated: they were picked
for a numerically well-behaved 1D test, not fitted to data.
"""

import argparse
import sys

from hbs_formation_landau import (
    BU_SATURATION,
    BU_THRESHOLD,
    REFERENCE_TEMPERATURE,
    hbs_state,
    regime_boundaries,
)

# ---  constants
A_UB = 4.0                  # m^3/J
A_WG_WB = 30*400*1e3        # -
M_PHI = 4*1e9               # s-1
ALPHA2 = (30*1e-6)          # m

# --- Eq. (7)/Appendix A building blocks

def g_of_phi(phi):
    """g(phi) = phi^2, Eq. (7)/Appendix A."""
    return phi * phi

def q_of_phi(phi):
    """Double-well barrier, q(phi) = phi^2 (1-phi)^2."""
    return phi * phi * (1.0 - phi) * (1.0 - phi)

def p_of_phi(phi):
    """Monotone interpolation, p(phi) = phi^3 (10 - 15 phi + 6 phi^2).
    p(0)=0, p(1)=1, p'(0)=p'(1)=0.
    """
    return phi ** 3 * (10.0 - 15.0 * phi + 6.0 * phi * phi)

def dp_dphi(phi):
    return 30.0 * phi**2 - 60.0 * phi**3 + 30.0 * phi**4

def dq_dphi(phi):
    return 2.0 * phi * (1.0 - phi)**2 - phi**2

# --- free energy densities

def f_paper(phi, e_s, a_ub=A_UB, a_wg_wb=A_WG_WB):
    """Eq. (7) of the paper."""
    return a_ub *(1 - p_of_phi(phi)) * e_s + a_wg_wb * q_of_phi(phi)

def df_paper_dphi(phi, e_s, a_ub=A_UB, a_wg_wb=A_WG_WB):
    """d f_paper / d phi, closed form (no finite differences needed)."""
    return -a_ub * e_s * dp_dphi(phi) + a_wg_wb * dq_dphi(phi)

def f_new(phi, burnup, temperature=REFERENCE_TEMPERATURE, a_wg_wb=A_WG_WB):
    """The proposed f(phi), driven and gated by the HBS Landau model."""
    state = hbs_state(burnup, temperature)
    return state.c0 * (1.0 - p_of_phi(phi) * state.eta)  + a_wg_wb * q_of_phi(phi)


def df_new_dphi(phi, burnup, temperature=REFERENCE_TEMPERATURE, a_wg_wb=A_WG_WB):
    """d f_new / d phi, closed form (no finite differences needed)."""
    state = hbs_state(burnup, temperature)
    return - state.c0 * state.eta * dp_dphi(phi) + a_wg_wb * dq_dphi(phi)


# --- Figure 1: the building blocks

def plot_building_blocks(path="figures/phasefield_building_blocks.png"):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    phi = [i / 400.0 for i in range(401)]

    figure, axes = plt.subplots(1, 3, figsize=(13.0, 4.0))

    axes[0].plot(phi, [g_of_phi(p) for p in phi], "-", color="k")
    axes[0].set_title(r"$g(\phi) = h(\phi) = \phi^2$", fontsize=10)

    axes[1].plot(phi, [q_of_phi(p) for p in phi], "-", color="k")
    axes[1].plot(phi, [dq_dphi(p) for p in phi], "--", color="tab:blue", lw=1.2,
                label=r"$dq/d\phi$")
    axes[1].set_title(r"double well  $q(\phi) = \phi^2(1-\phi)^2$", fontsize=10)

    axes[2].plot(phi, [p_of_phi(p) for p in phi], "-", color="k", label=r"$p(\phi)$")
    axes[2].plot(phi, [dp_dphi(p) for p in phi], "--", color="tab:blue", lw=1.2,
                label=r"$dp/d\phi$")
    axes[2].set_title(r"interpolation  $p(\phi) = \phi^3(10-15\phi+6\phi^2)$", fontsize=10)
    axes[2].legend(fontsize=8)

    for axis in axes:
        axis.set_xlabel(r"$\phi$")
        axis.set_xlim(0.0, 1.0)
    figure.suptitle("phase-field building blocks")
    figure.tight_layout()
    figure.savefig(path, dpi=140)
    print("written: %s" % path)


# --- Figure 2: local free energy

def plot_free_energy(path="figures/phasefield_free_energy.png",
                     temperature=REFERENCE_TEMPERATURE):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    phi = [i / 400.0 for i in range(401)]

    figure, axes = plt.subplots(1, 2, figsize=(11.0, 4.4))

    for e_s, colour in ((1.0e6, "tab:blue"), (5.0e6, "tab:orange"), (2.0e7, "tab:green")):
        axes[0].plot(phi, [f_paper(p, e_s) for p in phi], "-", color=colour,
                    label=r"$E_s=%.1e$ J/m$^3$" % e_s)
    axes[0].set_title("Eq. (7), Muramatsu 2014", fontsize=10)
    axes[0].legend(fontsize=8)

    burnups = (20.0, BU_THRESHOLD, 70.0, BU_SATURATION)
    labels = ("20 (below threshold)", "%.1f (threshold)" % BU_THRESHOLD,
             "70", "%.1f (saturation)" % BU_SATURATION)
    colours = ("tab:blue", "0.3", "tab:orange", "tab:green")
    for burnup, label, colour in zip(burnups, labels, colours):
        axes[1].plot(phi, [f_new(p, burnup, temperature) for p in phi], "-", color=colour,
                    label="bu = %s" % label)
    axes[1].set_title("proposed  $f_{new}(\\phi)$", fontsize=10)
    axes[1].legend(fontsize=8)

    for axis in axes:
        axis.set_xlabel(r"$\phi$")
        axis.set_ylabel(r"free energy density  (J/m$^3$)")
        axis.set_xlim(0.0, 1.0)
    figure.suptitle("local free energy density, T = %g K" % temperature)
    figure.tight_layout()
    figure.savefig(path, dpi=140)
    print("written: %s" % path)


# --- Figure 3: the threshold gate

def plot_threshold_gate(path="figures/phasefield_threshold_gate.png",
                        temperature=REFERENCE_TEMPERATURE):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    burnups = [1.0 + 0.25 * i for i in range(800)]
    states = [hbs_state(b, temperature) for b in burnups]

    figure, axes = plt.subplots(1, 3, figsize=(13.5, 4.0))

    axes[0].plot(burnups, [s.eta for s in states], "-", color="k")
    axes[0].set_ylabel(r"$\eta = -c2 / (2.0 * c4)**0.5$")
    c2max = max([abs(s.c2) for s in states])
    c4max = max([abs(s.c4) for s in states])
    axes[0].plot(burnups, [s.c2 / c2max for s in states], "-", color="tab:orange", label="c2 / c2absmax")
    axes[0].plot(burnups, [s.c4 /c4max for s in states], "-", color="tab:green", label="c4 / c4absmax")
    axes[0].legend()

    c0max = max([abs(s.c0) for s in states])
    axes[1].plot(burnups, [s.c0 /c0max for s in states], "-", color="tab:blue", label="c0 / c0absmax")
    axes[1].set_ylabel(r"$c_o(bu)$  (J/m$^3$)")
    axes[1].set_yscale("log")
    axes[1].legend()


    axes[2].plot(burnups, [s.c0 * s.eta for s in states], "-", color="k")
    axes[2].set_yscale("log")
    axes[2].set_ylabel(r"nucleation driving force  $c_0 \cdot \eta$  (J/m$^3$)")

    figure.suptitle("nucleation onset derived from the Landau Model, T = %g K" % temperature)
    figure.tight_layout()
    figure.savefig(path, dpi=140)
    print("written: %s" % path)


# --- Figure 4: 1D Allen-Cahn

def relax_step_profile(burnup, r0, temperature=REFERENCE_TEMPERATURE,
                       length=200.0e-9, n_points=201, t_end=2.0, n_times=120):
    """Integrate d(phi)/dt = -M_phi*(df/dphi - alpha2*d2phi/dx2) from a step IC.

    phi=1 on |x| < r0, phi=0 elsewhere, Neumann BCs (Eq. 4 of the paper).  The
    reaction term (barrier height A_WG_WB) is much stiffer than the diffusive
    term, so this needs an implicit solver ("BDF"), not an explicit one.
    `state.c0` and `state.eta` depend only on (burnup, temperature), fixed for
    the whole run, so they are looked up once instead of once per grid point
    per call.

    Caveat: this is a PLANAR 1D geometry (two independent flat interfaces),
    not a radial one.  Unlike a spherical nucleus, a planar slab has no
    curvature term competing against the bulk driving force, so above
    threshold any well-separated slab grows regardless of r0 -- there is no
    curvature-driven critical radius here.  What CAN be seen in 1D is (a) the
    threshold gate itself (nothing moves below BU_THRESHOLD, any r0), and
    (b) the two interfaces attracting and collapsing the slab when r0 is
    comparable to the interface width, even above threshold, because the
    short-range wall-wall interaction outweighs a driving force too weak to
    separate them.  A genuine curvature-limited critical radius needs a
    radial (spherical) 1D formulation -- left for a follow-up script.

    Returns (x, t, phi[len(t), len(x)]).
    """
    import numpy as np
    from scipy.integrate import solve_ivp

    x = np.linspace(-length / 2.0, length / 2.0, n_points)
    dx = x[1] - x[0]
    phi0 = np.where(np.abs(x) < r0, 1.0, 0.0)

    def laplacian(phi):
        lap = np.empty_like(phi)
        lap[1:-1] = (phi[2:] - 2.0 * phi[1:-1] + phi[:-2]) / dx ** 2
        lap[0] = (phi[1] - phi[0]) / dx ** 2 * 2.0        # Neumann: mirror ghost node
        lap[-1] = (phi[-2] - phi[-1]) / dx ** 2 * 2.0
        return lap

    def rhs(_t, phi):
        phi = np.clip(phi, 0.0, 1.0)
        return -M_PHI * (df_new_dphi(phi, burnup, temperature, A_WG_WB) - ALPHA2 * laplacian(phi))

    t_eval = np.linspace(0.0, t_end, n_times)
    solution = solve_ivp(rhs, (0.0, t_end), phi0, t_eval=t_eval, method="BDF",
                         rtol=1e-6, atol=1e-9)
    return x, solution.t, np.clip(solution.y.T, 0.0, 1.0)


def plot_1d_relaxation(path="figures/phasefield_1d_relaxation.png",
                       temperature=REFERENCE_TEMPERATURE):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    # bu=52.5 is just above BU_THRESHOLD (~49.56): eta and the driving force
    # c0*eta are still small there (weak compared to the barrier A_WG_WB), so
    # the two interfaces of a narrow slab can still win and collapse it, while
    # a wide, well-separated slab is still pulled forward by the (small but
    # nonzero) bulk driving force -- this is where the two behaviours can
    # actually be told apart.  At bu=70 the driving force already exceeds the
    # barrier and any r0 grows near-instantly (tried first; not shown here).
    cases = (
        ("bu=52.5, r0=3 nm (~interface width: collapses)", 52.5, 1),
        ("bu=52.5, r0=30 nm (well separated: grows)", 52.5, 100),
        ("bu=20 (below threshold), r0=30 nm: static", 20.0, 100),
    )

    figure, axes = plt.subplots(2, len(cases), figsize=(4.3 * len(cases), 7.4))

    for column, (label, burnup, r0) in enumerate(cases):
        x, t, phi = relax_step_profile(burnup, r0, temperature)

        image = axes[0, column].pcolormesh(x * 1e9, t, phi, shading="auto",
                                           cmap="viridis", vmin=0.0, vmax=1.0)
        axes[0, column].set_title(label, fontsize=9)
        axes[0, column].set_xlabel("x  [nm]")
        axes[0, column].set_ylabel("t  [s]")
        figure.colorbar(image, ax=axes[0, column], label=r"$\phi$")

        axes[1, column].plot(x * 1e9, phi[0], "--", color="0.6", lw=1.2, label="t=0")
        axes[1, column].plot(x * 1e9, phi[-1], "-", color="k", lw=1.6,
                             label="t=%.3g s" % t[-1])
        axes[1, column].set_xlabel("x  [nm]")
        axes[1, column].set_ylabel(r"$\phi$")
        axes[1, column].set_ylim(-0.05, 1.05)
        axes[1, column].legend(fontsize=8)

    figure.suptitle("1D Allen-Cahn relaxation of a step profile, "
                    r"$\theta$ frozen uniform  (T = %g K)" % temperature)
    figure.tight_layout()
    figure.savefig(path, dpi=140)
    print("written: %s" % path)


# --- symbolic self-check: the double-well bifurcation matches BU_THRESHOLD -

def selftest(verbose=True):
    """Check the symbolic shape of f_new(phi) and that its threshold is BU_THRESHOLD.

    p(phi) is a quintic smoothstep: p'(0)=p'(1)=p''(0)=p''(1)=0.  That flatness
    means phi=0 and phi=1 are BOTH stationary points of f_new for any (c0, eta)
    -- confirmed symbolically below -- and both are local minima of f_new
    (curvature set by q(phi) alone, A_WG_WB*q''(0)=A_WG_WB*q''(1)=2*A_WG_WB>0,
    again independent of eta).  So eta does not create the phi=1 minimum out of
    nothing; it sets how much DEEPER phi=1 is than phi=0:

        f_new(0) - f_new(1) = c0 * eta

    which is the actual nucleation driving force.  By construction of
    `hbs_state`, eta is exactly zero for bu <= BU_THRESHOLD and strictly
    positive above it, so that is where this checks the bifurcation: the
    driving force must vanish below BU_THRESHOLD and turn on above it.
    """
    import sympy

    phi = sympy.symbols("phi", real=True)
    c0, eta = sympy.symbols("c0 eta", positive=True)
    p_expr = phi ** 3 * (10 - 15 * phi + 6 * phi * phi)
    q_expr = phi ** 2 * (1 - phi) ** 2
    f_expr = A_UB * c0 * (1 - p_expr * eta) + A_WG_WB * q_expr

    d1 = sympy.diff(f_expr, phi)
    d2 = sympy.diff(f_expr, phi, 2)
    d1_at_0 = sympy.simplify(d1.subs(phi, 0))
    d1_at_1 = sympy.simplify(d1.subs(phi, 1))
    d2_at_0 = sympy.simplify(d2.subs(phi, 0))
    d2_at_1 = sympy.simplify(d2.subs(phi, 1))
    driving_force = sympy.simplify(f_expr.subs(phi, 0) - f_expr.subs(phi, 1))

    if verbose:
        print("f_new(phi)              = %s" % f_expr)
        print("df_new/dphi             = %s" % sympy.simplify(d1))
        print("at phi=0: df/dphi=%s  d2f/dphi2=%s" % (d1_at_0, d2_at_0))
        print("at phi=1: df/dphi=%s  d2f/dphi2=%s" % (d1_at_1, d2_at_1))
        print("f_new(0) - f_new(1)     = %s   (driving force = c0*eta)" % driving_force)
        print()

    # phi=0 and phi=1 are stationary points of f_new, and are local minima,
    # for ANY (c0, eta) -- independent of the driving force, by construction
    # of the smoothstep p(phi).
    ok_wells = (d1_at_0 == 0 and d1_at_1 == 0
               and d2_at_0 == 2 * A_WG_WB and d2_at_1 == 2 * A_WG_WB)

    # the actual bifurcation: the driving force c0*eta must vanish (no
    # nucleation possible) at and below BU_THRESHOLD, and be strictly
    # positive immediately above it -- reusing hbs_state's own eta(bu).
    below = hbs_state(BU_THRESHOLD - 0.5, REFERENCE_TEMPERATURE)
    above = hbs_state(BU_THRESHOLD + 0.5, REFERENCE_TEMPERATURE)
    ok_threshold = below.eta == 0.0 and above.eta > 0.0

    if verbose:
        print("phi=0, phi=1 stationary local minima for any (c0,eta): %s" % ok_wells)
        print("eta(BU_THRESHOLD-0.5) = %.6g  (expect 0)" % below.eta)
        print("eta(BU_THRESHOLD+0.5) = %.6g  (expect > 0)" % above.eta)
        print("driving force switches on exactly at BU_THRESHOLD: %s" % ok_threshold)

    assert ok_wells, "phi=0/phi=1 are not both stationary local minima of f_new"
    assert ok_threshold, "the nucleation driving force does not switch on at BU_THRESHOLD"
    if verbose:
        print("\nselftest: ok")
    return True


def main(argv=None):
    parser = argparse.ArgumentParser(
        description="1D exploratory prototype of a phase-field functional for HBS "
                    "sub-grain nucleation, driven and threshold-gated by the HBS "
                    "Landau model of hbs_formation_landau.py.  Not a solver.",
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--selftest", action="store_true",
                        help="symbolic/numeric check: the double-well bifurcation "
                             "tracks BU_THRESHOLD")
    parser.add_argument("--plots", action="store_true",
                        help="write all four figures to --figures-dir")
    parser.add_argument("--figures-dir", default="figures", metavar="DIR",
                        help="output directory for --plots (default: figures)")
    parser.add_argument("--temperature", type=float, default=REFERENCE_TEMPERATURE,
                        metavar="K", help="temperature for --plots (default %g)"
                                          % REFERENCE_TEMPERATURE)
    arguments = parser.parse_args(argv)

    if not any((arguments.selftest, arguments.plots)):
        parser.print_help()
        return 0

    if arguments.selftest:
        selftest()

    if arguments.plots:
        import os
        os.makedirs(arguments.figures_dir, exist_ok=True)
        print("note: M_PHI, ALPHA2 and A_WG_WB are illustrative, not calibrated.")
        plot_building_blocks(os.path.join(arguments.figures_dir,
                                          "phasefield_building_blocks.png"))
        plot_free_energy(os.path.join(arguments.figures_dir, "phasefield_free_energy.png"),
                        arguments.temperature)
        plot_threshold_gate(os.path.join(arguments.figures_dir,
                                         "phasefield_threshold_gate.png"),
                           arguments.temperature)
        plot_1d_relaxation(os.path.join(arguments.figures_dir,
                                        "phasefield_1d_relaxation.png"),
                          arguments.temperature)
    return 0


if __name__ == "__main__":
    sys.exit(main())
