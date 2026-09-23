"""1D orientation phase field for dislocation-driven grain nucleation.

What this script does, in three steps
=====================================
  1. Solves a 1D reduction of the model of [T26], Sec. 2 (phase field only, no
     mechanics).
  2. Checks it against the paper's Cu bicrystal (Sec. 3.2, Figs. 1, 2, 4, 5, 6).
  3. Applies the same equations to UO2: the dislocation density rises with
     burnup (source S(bu) taken from the Landau model [HBS]), and the script
     reports whether and when a new grain nucleates at an as-fabricated grain
     boundary.

    python3 phasefield_tandogan_1d.py                                 # everything (slow)
    python3 -c "import phasefield_tandogan_1d as pf; pf.selftest()"   # checks only

Every setting is in the RUN CONFIGURATION block right after the imports.

References
==========
[T26]  I.T. Tandogan, M. Budnitzki, S. Sandfeld, J. Mech. Phys. Solids 206
       (2026) 106325.  Equation numbers "(n)" below refer to this paper.
[HBS]  hbs_formation_landau.py in this folder (Landau model of the HBS):
       rho_tot(bu), G(T), b, Theta(bu), r_n(bu), X(bu).
[Z21]  J. Zhang et al., J. Am. Ceram. Soc. (2021): 5-DOF GB energy of UO2,
       reduced to gamma(Delta theta) by uo2_gb_energy.py.
[ON25] M.-L. Onofri et al.: in irradiated UO2 the dislocation density is higher
       over the 500-600 nm next to the original GBs.
[PFRP] Ofori-Opoku, Warren, Simon, "Phase Field Method Recommended Practices".

How to read the tags
====================
Every statement below carries a tag, and the code comments repeat the tag:

  [P]  taken from the paper [T26] as written
  [R]  REDUCTION: my choice to go from the 2D Cosserat model to this 1D one
  [U]  UO2: my choice to apply the model to UO2 (not in [T26])
  [N]  NUMERICS: an implementation choice
  [?]  UNKNOWN: a value nobody has measured or calibrated; a placeholder
  [E]  ERROR / DEVIATION: something that is wrong, inconsistent, or does not
       reproduce the paper


1. EQUATIONS SOLVED
===================
Fields (each an array over the cells of the 1D grid):

    eta    in [0, 1]  crystalline order: 1 in a grain, < 1 inside a diffuse GB
    theta  [rad]      lattice orientation (= Cosserat microrotation)
    rho    [m^-2]     SSD (statistically stored dislocation) density

Free energy, Eq. (15), with a(n_gb, theta) = 1, one slip system and h = 1, so
that r^2 = b^2 rho:

    psi = f0 [ alpha V(eta) + nu^2/2 eta'^2 + mu^2 g(eta) theta'^2 ]
        + phi(eta) lambda/2 G b^2 rho

    V(eta)    = (1 - eta)^2 / 2                                            (35)
    g(eta)    = (7 eta^3 - 6 eta^4)/(1 - eta)^3 + c ln(1 - eta) + C0       (35)
    C0        = |min g_raw| + 0.01                                         (36)
    phi'(eta) = c3/2 [1 - tanh(c1 (eta - c2))]                             (38)
                  ~ c3 inside the GB (eta < c2), ~ 0 in the grain

Evolution:

    tau_eta  d eta/dt = f_eta1 + f_eta2 + f_eta3 + f_eta4                  (32)
        f_eta1 =  f0 nu^2 eta''                       gradient             (41)
        f_eta2 = -f0 alpha V'(eta)                    potential            (41)
        f_eta3 = -f0 mu^2 g'(eta) theta'^2            orientation          (41)
        f_eta4 = -phi'(eta) lambda/2 G b^2 rho        stored dislocations  (41)

    tau_hat g(eta) d theta/dt = f0 d/dx [ mu^2 g(eta) d theta/dx ]         (34) + [R2]

    d rho/dt = - rho C_D A(eta) <d eta/dt>  +  S(t)                        (28) + [R3], [U3]
        A(eta) = (7 eta^3 - 6 eta^4) / (1 - eta)                           (29)
        <x>    = max(x, 0)   (recovery only where eta grows)

Nucleation mechanism, [T26] Secs. 2.3 and 3.2 [P]:
  (a) f_eta4 lowers eta ONLY inside the GB (phi' is a step at c2), so the GB
      widens towards eta_eq of Eq. (42):
          eta_eq = 1 - c3 (lambda/2 G b^2 rho) / (f0 alpha)                (42)
  (b) if eta_eq is ABOVE the initial GB depth, eta rises at the GB centre, the
      recovery term removes rho there, f_eta4 disappears, eta -> 1 and a
      dislocation-free grain appears with an orientation between the two
      parents; its two boundaries then move apart.


2. ASSUMPTIONS
==============
2.1 Taken from the paper [P]
----------------------------
P1  Eqs. (15), (28), (29), (32), (34)-(38), (41), (42) as written above.
P2  Isotropic interfaces, a(n_gb, theta) = 1 (Sec. 2.2.1).
P3  g frozen above eta_cutoff = 1 - 1e-4 (Sec. 2.2.2, end).
P4  Cu parameters: Table 1 (f0, nu, alpha, mu, c, lambda, b, C_D) and the
    phi' coefficients of the Fig. 4 caption (c1 = 100, c2 = 0.95, c3 = 1.7).
P5  Mobilities: the "recrystallisation" values of Table 1,
    tau_eta = 1e4 f0 t0 and tau_hat = 1e1 f0 t0, with t0 = 1 s.
P6  Cu protocol (Sec. 3.2): GB relaxed from eta0 = 0.99, then a uniform
    rho0 = 2.5e15 m^-2 switched on at t = 0 (or two values, one per grain,
    Fig. 6).
P7  Geometry: periodic bicrystal, grains 10 um wide (Fig. 3); 400 blocks over
    the 20 um period (dx = 50 nm).

2.2 Reduction from 2D Cosserat to 1D phase field [R]
----------------------------------------------------
R1  1D: x in [0, L], ONE GB at L/2, zero-flux walls at x = 0 and x = L.  The
    periodic bicrystal of Fig. 3 is mirror-symmetric about each grain centre,
    so [0, L] is exactly a half period (L = 10 um for Cu).
    Consequence: an antisymmetric mode (rigid drift of the GB pair) is
    forbidden.  With a uniform rho the problem is mirror-symmetric about L/2
    too, so a nucleus can only form at exactly Delta theta / 2 and
    migration cannot happen (see E5).
R2  No mechanics: u = 0, no slip, and mu_c -> infinity so that the skew
    elastic strain is 0 and theta = -e*.  With tau_* = tau_hat g(eta)
    (Eq. 23), Eq. (34) becomes the theta equation above.
    The Cosserat couple modulus, elasticity and plasticity parameters of
    Table 1 therefore do not appear.
R3  No slip means no Kocks-Mecking production in Eq. (28): only the recovery
    branch is left.  For Cu this is the paper's own setting (rho0 imposed,
    no loading).
R4  tau_eta is held constant; the switch of Eq. (39) is not used.
R5  G of Cu = 75 GPa: the paper writes mu^e without a value; I take
    C44 = 75 GPa from Table 1 (it is also the mu_c value).  With it
    Eq. (42) gives the paper's eta_eq = 0.579 exactly (checked in selftest).

R6  Polycrystal (Part 3b): N parent grains on a PERIODIC 1D domain (a ring),
    the 1D analogue of [T26]'s periodic Voronoi polycrystals (Sec. 3.3.2,
    6 and 32 grains, orientations 0-35 deg).  Orientations drawn uniformly
    in 0-30 deg, widths +-20%, fixed seed (make_ring).  No angle wrapping:
    theta is compared as a plain number (fine while all lie in 0-30 deg).

2.3 Application to UO2 [U]
--------------------------
U1  Inputs from [HBS]: G(T), b, rho(bu) (rho_tot by default, RHO_KIND).
    In [HBS] G depends on temperature only, not on burnup.
U2  Parent misorientation Delta theta: a fixed as-fabricated HAGB
    (GRAIN_MISORIENTATION_DEG = 20 deg, IRRADIATION_ANGLES for the scan),
    inside the 0-30 deg range where the GB energy is fitted.
U3  Dislocation source S(t) = d rho_ref/d bu * BU_RATE, with
    rho_ref = rho(bu) - rho(bu_start).  It stands in for the Kocks-Mecking
    production of Eq. (28) (irradiation instead of slip).  S is UNIFORM in x
    and does NOT depend on the local rho: a recovered nucleus is loaded
    again at the same rate.  Without recovery the domain average of rho is
    exactly rho_ref(bu).
U4  rho = 0 at bu_start: the density present before (as-fabricated plus
    anything produced before bu_start) is not included. 
    # TODO future: include pre-existing dislocations.
U5  GB energy: f0 and c refitted (fit_uo2_interface) on the "random axis and
    random plane" average of [Z21], over 0-30 deg, at nu = UO2_NU:
        target                     c     f0 [kPa]   rms [J/m^2]   max
        random axis and plane     34       672        0.015       3.9 %
        symmetric tilt <110>      30       716        0.017       3.1 %
        symmetric tilt <100>      45       818        0.029       3.6 %
    (values of the previous version of this script, NOT re-run for this
    revision; selftest only checks gamma at 5, 15 and 30 deg.)
    # TODO future: re-run the fit for the current revision.
U6  Diffuse width nu = 2.5 um, chosen so that the eta well nu/sqrt(alpha)
    = 0.56 um matches the 500-600 nm enriched layer of [ON25].
U7  Domain L = 25 um (~10 nu, the same ratio as Cu's 10 um at nu = 1 um):
    the GB first widens (Sec. 3.2.1) and only then splits; in a shorter
    domain the widened GB reaches the walls and the whole domain
    recrystallises as one grain ("collapse").
U8  c3 is calibrated (calibrate_c3) so that the first nucleation at
    Delta theta = 20 deg happens at HBS_ONSET_BURNUP = 60 GWd/tU.  UO2_C3 is
    the result for the default configuration.  Other angles use the same c3.
    !! It was calibrated with the [HBS] rho_tot(bu) BEFORE rho_crit was added
    (hbs_formation_landau.py, 2026-09-22): with the present rho_tot = 0
    below 47.1 GWd/tU the 20 deg onset moves to 68.5 GWd/tU.  Re-run
    CALIBRATE_C3 (~1 h) before quoting any UO2 onset.
U9  alpha, mu, c1, c2, lambda, tau_eta, tau_hat: Cu values of Table 1.
U10 C_D = 10 instead of 100: C_D = 100 makes one irradiation history take
    > 17 min instead of ~1 min.  A numerical reason, not a physical one.
    # TODO future: study several values to understand its effect.
U11 Temperature enters only through G(T); nothing is thermally activated.
U12 Source gating (SOURCE_IN_SWEPT = 0, ON by default): irradiation makes
    dislocations only in material that a boundary has NOT swept.  A fourth
    field m, the swept marker, starts at 1 and follows the same recovery law
    as rho,
        dm/dt = -m C_D A(eta) <d eta/dt>,
    and the source is multiplied by SOURCE_IN_SWEPT + (1 - SOURCE_IN_SWEPT) m
    (1 restores the strictly uniform source of [U3]).
    Why: HBS grains are dislocation-poor, the Nogita-Une rho_tot(bu) is read
    on the unrestructured matrix, and in [HBS] the new boundaries absorb what
    they sweep (rho_swept).  Neither [T26] nor the phase field has a
    mechanism of its own for this, because S is prescribed [U3].
    It is a constitutive assumption, not a measurement [?]: nothing says
    production STOPS in a new grain, only that its density stays far below
    the matrix value.
    It does not move the onset, because m = 1 everywhere until the first
    sweep: the 20 deg half period nucleates at 68.50 GWd/tU with and without
    it, so the c3 calibration is untouched.
    m is reset to 1 after the relaxation of [N3]: that relaxation raises eta
    from eta0 to ~1 and would otherwise "sweep" the whole domain (measured:
    m = 0.069 everywhere before irradiation, which switched the source off).
U13 Step B (Part 3b, LANDAU_ROTATION, OFF): the analogue of [T26]'s loading
    phase (Sec. 3.3.1).  Eq. (26) with u = 0, e^e = 0 gives
    theta = -e^slip - e*; e^slip is PRESCRIBED so that each subgrain turns at
    W = 1/2 s(x) dTheta_L/dt, Theta_L = [HBS] Theta(bu), scaled by the local
    share of the tangle rho / (rho_tot - rho_ord)_[HBS] (a recovered grain has
    nothing to polygonise):
        tau_hat g (d theta/dt - W) = f0 d/dx[ mu^2 g d theta/dx ]
    Theta_L is used as a LOCAL wall angle, while [HBS] Eq. (10) reads it as
    the mean of a two-phase mixture: a possible double count with X. [?]
U14 s(x) = +-1 alternating on subgrains of width D = 2 r_n(HBS_ONSET_BURNUP),
    an even number per parent grain, held fixed ([HBS] r_n shrinks).
U15 Step B dislocations: production d rho_tot/d bu (uniform) and transfer of
    the tangle into walls at the relative rate (d rho_ord/dt)/(rho_tot -
    rho_ord) of [HBS].  [HBS]'s rho_swept is NOT subtracted: annihilation is
    left to the recovery of Eq. (28), so it is not counted twice.
    # TODO future: dislocation model temperature dependent.

2.4 Numerics [N]
----------------
N1  Finite volumes on a uniform grid; zero flux on the two walls; g at the
    faces by harmonic mean; theta'^2 at a cell centre = mean of its two face
    values.  Method of lines, scipy BDF (tolerances: N6), sparse
    tridiagonal-block Jacobian pattern.  SI units throughout; t0 enters only
    through the mobilities.
N2  A(eta) is frozen above eta_cutoff like g.  The paper states the cutoff
    for g only; without it one UO2 history at 2500 cells goes from ~50 s to
    more than 5 min (measured in the previous version).
N3  Initial GB: sharp tanh step of theta (width 2 dx), eta = 0.99, relaxed
    for 2e4 t0 with rho = 0 and the most mobile values found in Table 1 for
    each field separately (tau_eta = 1e2, tau_hat = 1e1 f0 t0; this pair is
    not one of Table 1's rows).  The relaxed profile does not depend on the
    mobilities; it is cached.
N4  Grids.  Cu: 400 cells over 10 um (dx = 25 nm).  UO2: 2500 cells over
    25 um (dx = 10 nm): the theta transition narrows with Delta theta, and
    at 20 deg it is ~0.09 um, i.e. ~9 cells; at 30 deg ~3 cells, below the
    5-10 cells of [PFRP].  gamma still converges there because g >= 0.01.
    A COARSER grid is not cheaper: at 400 cells over 10 um one UO2 history
    costs 4-10 times more (stiff) and onset(c3) stops being monotone.
    (Numbers measured in the previous version, not re-measured here.)
N5  Event classification (classify_event), [T26]'s wording turned into three
    tests on the profiles:
      1. rho at the GB centre < 0.1 rho_ref (first time = birth);
      2. at that moment both grain centres still have eta > c2;
      3. later, eta at the centre > c2 with eta < c2 on both sides.
    1+2+3 = "nucleus";  1 and centre theta ended on a parent = "migration";
    1 without 2 = "collapse" (whole domain disordered first);  not 1 = "none".
    0.1 and c2 are thresholds I put on continuous fields.
N6  Tolerances: rtol = 1e-5; atol = 1e-8 for eta and theta, 1e4 m^-2 for
    rho.  The previous version used atol = 1e-8 for ALL fields, i.e.
    1e-8 m^-2 on a density that reaches 1e15: while rho ~ 0 (start of an
    irradiation) the error test then chases the on/off switching of
    <d eta/dt> in Eq. (28) and the step size collapses.  The old UO2 30 deg
    history passed by luck of the step sequence (with scipy Jacobian
    overflow warnings); after removing the always-zero 4th field it stalled
    at bu = 0.008 (> 30 min).  With atol_rho = 1e4 it takes ~50 s.


N7  Step B: the [HBS] tangle is floored at max(1e-3 rho_tot, 1e12 m^-2)
    (TANGLE_FLOOR).  Below rho_crit it is 0, and rho/tangle would turn
    solver noise (~ATOL_RHO) into rotation: with a floor of 1 m^-2 the B0
    test produced 9-32 deg spurious steps.
N8  Recovery dead zone: <d eta/dt> in Eq. (28) is replaced by
    <d eta/dt - 1e-9/t0> (RECOVERY_DEAD_ZONE).  In the grain A(eta) is frozen
    at ~1e4 and d eta/dt is solver noise that changes sign between Newton
    iterations; once rho > 0 (e.g. when the [HBS] source switches on at
    rho_crit, 47.1 GWd/tU) that kink stalls BDF: 46 -> 49 GWd/tU took
    > 400 s instead of 8 s, one 20 deg history 966 s instead of ~50 s.
    1e-9/t0 (3e-13 /s for UO2) is far below the physical rates (GB widening
    ~1e-9 /s, nucleation ~1e-8 /s).  A model change, although a small one.
N9  Jacobian: scipy's num_jac (what jac_sparsity does internally) with its
    relative column step capped at MAX_JAC_FACTOR.  Uncapped it overflows on
    a column whose derivative is exactly 0 - the swept marker before the
    first sweep [U12] - and BDF stops with "Factor is exactly singular".
    num_jac and group_columns are PRIVATE scipy functions (checked: 1.17.1).

3. UNKNOWNS / UNCALIBRATED [?] # TODO future.
==============================
Q1  UO2 GB mobility.  t0 = UO2_T0 = 1e-3 / BU_RATE s (~3.2e3 s) is a
    placeholder, i.e. UO2 GBs ~3e3 times less mobile than Cu's Table 1.
    It should come from M = M0 exp(-Q/RT): t0 = tau_eta / (f0 M).
Q2  Because t0 is DEFINED as 1e-3 / BU_RATE, the mobilities and the source
    both scale with BU_RATE: the evolution in burnup is EXACTLY independent
    of BU_RATE.  Changing BU_RATE changes nothing but the time axis.  The
    rate effect will only appear once t0 comes from Q1.
Q3  c3 for UO2: one number fitted to one datum (onset at 60 GWd/tU, 20 deg).
    It absorbs every other error in the stored-energy force (U9, U10, Q1).
Q4  alpha, mu, c1, c2 for UO2: no data; Cu values.
Q5  C_D for UO2: no data; chosen for run time (U10).
Q6  Critical nucleus size: 1D has no curvature (E1).  critical_radius()
    gives r* = 2 gamma / (lambda/2 G b^2 rho) after the fact, for comparison
    with the Landau sub-grain radius only.


4. KNOWN ERRORS, DEVIATIONS AND LIMITS [E]
=========================================
E1  1D: no GB curvature, no capillary pressure; a nucleus of any size
    survives.
E2  Eq. (36) as printed in [T26] reads "C0 = min(...) + 0.01"; with the
    minimum <= 0 this would make g negative.  I implement the evident
    intention, C0 = -min(g_raw) + 0.01, so that min g = 0.01 (selftest).
E3  Cu, Delta theta = 2.5 deg: [T26] report that rho is never recovered and
    eta settles at eta_eq = 0.579.  Here the orientation gradient does
    disappear and the whole domain recovers at once (t ~ 4.5e3 s).  It is
    classified "collapse", not "nucleus", which agrees with the paper's
    conclusion (no nucleus) but NOT with its profiles.
E4  UO2: with the calibrated c3, eta_eq of Eq. (42) drops below 0 at high
    burnup (printed by main, column bu(eta=0)).  Results beyond that burnup
    are outside the model ([T26] Sec. 2.3: c3 must keep eta in [0, 1]).
E5  UO2: migration (SIBM) is unreachable (R1 + uniform source).  The
    asymmetric branch of Eq. (43) can only be tried in run_case, where two
    rho values are imposed (Fig. 6), and even there it does not come out as
    in the paper (E8).  Under irradiation it needs the full period: the
    ring of Part 3b [R6] (results in E13).
E6  gamma(Delta theta) is monotone by construction (no HAGB cusps); above
    30 deg it is an extrapolation.  The UO2 symmetric-tilt curves have a
    maximum near 20-25 deg that is not reproduced.
E7  The model has no mechanism that CREATES misorientation from
    dislocations: rho is a scalar with energy only, and without slip theta
    can only be redistributed.  The nucleus orientation is always between
    the parents; the Landau Theta(bu) of [HBS] is not something this model
    can produce, and the comparison in plot_diagnostics is only a
    side-by-side.
E8  Cu, Fig. 6 (rho different in the two grains, Delta theta = 15 deg) is
    NOT reproduced.  [T26]: a nucleus for every pair between (0, 2.5) and
    (2.5, 2.5), closer to the less loaded grain (Eq. 43).  Here, in units
    of 1e15 m^-2:
        (0, 2.5) (1, 2.5) (1.5, 2.5) (2, 2.5) (2.5, 2)   migration
        (2.3, 2.5)                                        nucleus, but it
                                                          rotates to 0.3 deg
    Likely causes, NOT verified: the half-period domain (R1, 10 um: the
    nucleus is swept away before it separates), outputs every 50 s (a
    short-lived bulge can fall between two outputs, N5), no mechanics (R2).
E9  UO2 30 deg: "collapse" at ~68 GWd/tU, not a nucleus (the widened GB
    fills the domain first).  The 20 deg case is calibrated (U8), so the
    15 and 30 deg cases are predictions of the same c3.
E10 UO2: "recrystallised" is ~0 at the end of every history, because the
    source keeps loading the new grain (U3) and rho there climbs back above
    0.1 rho_ref.  It measures dislocation-free volume, not restructured
    volume; compare it with [HBS] X with that in mind.
E11 Under the uniform source the mirror symmetry about L/2 is broken only by
    round-off, but nucleation amplifies it: at 15 deg the theta asymmetry
    grows from 1e-8 deg (bu 50) to 7e-3 deg (bu 62), and keeps growing.
    The nucleus misorientation is read at the LAST output where the bulge
    still exists (N5), i.e. late: there it is 7.18 deg (6.79 in the previous
    version), while at bu 62 theta(L/2) = 7.498 deg = Delta theta / 2.
    Under this protocol read it as Delta theta / 2; the rest is numerical
    drift.  Onsets are robust (55.5, 59.75, 68.25 GWd/tU at 15, 20, 30 deg,
    identical in the previous and this version).
E12 The polygonisation extension (tangle -> walls -> subgrain rotation) of
    the previous version is removed from this file: it was off by default
    and did not polygonise (HMP relaxes a subgrain rotation away in ~1 day
    with Table 1's tau_hat).  Kept, unmaintained, in
    archive/phasefield_tandogan_1d_with_polygonisation.py.
E13 Half period: every history ends as ONE flat crystal (eta = 1, theta =
    Delta theta / 2).  After nucleation the two new boundaries run into the
    parents, driven by lambda/2 G b^2 (rho_parent - rho_nucleus), with no
    curvature to stop them (E1), and at the walls each meets its own mirror
    image (R1): same orientation on both sides, no boundary left.  With no
    GB, f_eta4 is ~0 everywhere (phi' ~ 0 for eta > c2) and nothing else can
    happen, whatever the burnup.  [T26] show the same full expansion
    (Figs. 4, 6).
    The ring [R6] removes the mirror but NOT the flattening; it only delays
    it.  5 grains (make_ring seed 0: 27.4, 18.2, 21.9, 16.3, 28.1 deg; the
    last GB is only 0.67 deg), c3 = 1.183, new [HBS] rho_tot:
        grains [count/theta spread]   bu 60     65      75      85     100
        25 um grains (2498 s)         5/11.7   8/11.7  4/4.6   2/1.7  0/1.0
        10 um grains (1008 s)         5/11.7   5/9.8   3/9.2   2/0.3  0/0.0
    Nuclei at 58.5-63.75 GWd/tU at 4 (25 um) and 3 (10 um) of the 5 GBs,
    then a sweep: the region swept last holds the least rho (the uniform
    source reloads everything equally), so it always pushes into its
    neighbours; a grain shrunk to zero merges its two boundaries and the
    ring loses a grain.  Nothing opposes it in 1D: no curvature, no triple
    junctions, and fewer boundaries is always lower energy.  By ~85-95
    GWd/tU one orientation fills the ring; above ~90 the GB-free bulk flips
    (phi' grows as eta drops, eta_eq < 0, E4) and the "0 grains" rows are
    that flip.  In [T26]'s 2D polycrystals growth is stopped and reversed
    by curvature (Sec. 3.3.2); a 1D model has no equivalent.
    The 10 um grains do not collapse before nucleating (the U7 worry does
    not apply at this nu).  Stencil check: a ring of 2 grains (0/20 deg,
    25 um) gives the half-period result exactly (nucleus at 68.50 GWd/tU,
    10.00 deg at both GBs).
    WITH the source gating of [U12] (SOURCE_IN_SWEPT = 0) the ring no longer
    coarsens: once a region is swept it is not reloaded, so rho -> 0 there,
    the driving force disappears and the structure FREEZES.  25 um grains:
        bu                  60      65      70      75      80     110
        grains              5       8       6       4       4       4
        theta spread [deg]  11.7    11.7    9.2     1.5     1.5     1.5
        rho mean [m^-2]     6.1e14  7.4e14  5.0e14  2.2e14  7.4e12  4.0e13
    Nuclei at 57.25-63.25 GWd/tU at the four real GBs, the domain fully
    swept by ~73, then nothing moves up to 110 GWd/tU.  E4 is no longer
    reached either, because rho stays bounded (eta min 0.52 at 110 against
    a bulk flip without the gating).
    What it does NOT fix: the surviving grains are as wide as the original
    ones (~20-30 um, i.e. the nucleation sites), and they differ by ~1.5 deg
    in all.  HBS sub-grains are ~0.2-0.8 um at 10 deg.  Nucleation only
    happens at pre-existing GBs in this model (E7), so the final grain count
    is set by the parent microstructure, not by the physics of the HBS.
E14 Step B fails the B0 go/no-go test (one 25 um grain on a ring, no GB,
    0 -> 80 GWd/tU):
        tau_hat = 1e1 (Table 1): lattice step 0.0005 deg vs Theta_L 0.7-4.2 deg
        tau_hat = 1e4:           0.54 deg (76 %) at 50, 0.64 deg (15 %) at 80
    and eta at the subgrain walls stays 1.0000: g' = 0 above eta_cutoff,
    so a theta step inside a perfect grain exerts no force on eta, no eta
    dip forms to pin it, and e* relaxes it (Eq. 34).  In [T26] the same
    happens to gradients that do not localise (kink band, subgrains B1/B2
    "rotate back").  Kept behind LANDAU_ROTATION = False; making it work
    needs a model change (lower cutoff or a bulk plateau of phi', which the
    archived option A showed flips the grain to the GB branch near
    58 GWd/tU).
"""

import math
import os
import sys
from dataclasses import dataclass, replace

import numpy as np
from scipy.integrate import solve_ivp
from scipy.integrate._ivp.common import num_jac      # private, see [N9]
from scipy.optimize._numdiff import group_columns    # private, see [N9]
from scipy.sparse import diags, kron

from hbs_formation_landau import (
    BURGERS,
    REFERENCE_TEMPERATURE,
    THETA_HAGB,
    hbs_state,
)


# ###########################################################################
#   RUN CONFIGURATION  (edit here)
# ###########################################################################

FIGURES_DIR = "phasefield"
TEMPERATURE = REFERENCE_TEMPERATURE     # K        [U11] enters only through G(T)
RHO_KIND = "tot"                        # -        rho fed to f_eta4: "tot" (Nogita-Une)
                                        #          or "free" (Landau tangle) [U1]

# --- irradiation protocol [U3] --------------------------------------------
GRAIN_MISORIENTATION_DEG = 20.0         # deg      parent HAGB used for the c3 calibration [U2]
IRRADIATION_ANGLES = [15.0, 20.0, 30.0] # deg      parent HAGBs of the scan [U2]
BU_RANGE = (0.0, 110.0)                 # GWd/tU
SECONDS_PER_YEAR = 365.25 * 86400.0
BU_RATE = 10.0 / SECONDS_PER_YEAR       # GWd/tU/s ~10 GWd/tU per year; irrelevant, see [Q2]
SNAPSHOT_BURNUPS = [0.0, 20.0, 40.0, 55.0, 60.0, 62.5, 64.0, 66.0, 80.0, 110.0]
                                        #          dense around 60-66: the nucleus lives
                                        #          ~1 GWd/tU between its two boundaries

# --- UO2 phase-field parameters -------------------------------------------
UO2_NU = 2.5e-6                         # m        [U6]  nu/sqrt(alpha) = 0.56 um ~ [ON25]
UO2_F0 = 671.7e3                        # Pa       [U5]  fitted at nu = UO2_NU; refit if nu changes
UO2_C = 34.0                            # -        [U5]
UO2_T0 = 1.0e-3 / BU_RATE               # s        [Q1] placeholder, [Q2]
UO2_C_D = 10.0                          # -        [U10], [Q5]
UO2_C3 = 1.183                          # -        [U8]  output of calibrate_c3 for THIS
                                        #          configuration; re-run after changing nu,
                                        #          f0, c, C_D, geometry or protocol
HBS_ONSET_BURNUP = 60.0                 # GWd/tU   [U8]  lower end of 60-75 GWd/tU
                                        #          (Rondinella & Wiss, Mater. Today 13 (2010) 24)
CALIBRATE_C3 = False                    # True: re-run the bisection (~1 h) and print c3.
                                        #       Always on the production grid [N4].

# --- polycrystal ring, Part 3b [R6] ----------------------------------------
N_GRAINS = 5                            # -        parent grains on the ring
RING_GRAIN_WIDTHS = (25.0e-6, 10.0e-6)  # m        both tested: 25 um keeps ~10 nu per grain
                                        #          [U7], 10 um is a real UO2 grain
RING_SEED = 0                           # -        orientations and widths (make_ring)
LANDAU_ROTATION = False                 # -        Step B: lattice rotation from [HBS]
                                        #          [U13]-[U15]; see E14 before switching on


# ###########################################################################
#   PART 1 - THE MODEL (material independent)
# ###########################################################################

ETA_CUTOFF = 1.0 - 1.0e-4               # -        [P3] g (and A, [N2]) frozen above


@dataclass(frozen=True)
class PhaseFieldParameters:
    """Model parameters.  Defaults: Cu, Table 1 of [T26] and Fig. 4 caption [P4]."""

    # phase field, Eq. (15)
    f0: float = 371.0e3                 # Pa     normalisation coefficient
    nu: float = 1.0e-6                  # m      eta gradient coefficient
    alpha: float = 20.0                 # -      weight of V(eta)
    mu: float = 2.5e-6 / math.pi        # m      theta gradient coefficient
    c: float = 3.0                      # -      log (Read-Shockley) term of g, Eq. (35)
    # inverse mobilities, Eqs. (23), (30), in units of f0*t0 [P5]
    tau_eta: float = 1.0e4              # f0*t0
    tau_hat: float = 1.0e1              # f0*t0
    t0: float = 1.0                     # s      time unit of the mobilities
    # stored dislocation energy, Eq. (15) last term
    lam: float = 0.3                    # -      lambda
    shear_modulus: float = 75.0e9       # Pa     mu^e = C44 of Table 1 [R5]
    burgers: float = 0.2556e-9          # m      b
    # phi'(eta), Eq. (38)
    c1: float = 100.0                   # -      steepness of the step
    c2: float = 0.95                    # -      position of the step
    c3: float = 1.7                     # -      height of the step
    # recovery, Eq. (28)
    c_d: float = 100.0                  # -      C_D

    @property
    def stored_energy_per_rho(self):
        """lambda/2 G b^2 [J/m]: multiplies rho in Eq. (15) (h = 1)."""
        return 0.5 * self.lam * self.shear_modulus * self.burgers ** 2


# ---------------------------------------------------------------------------
# 1.1  Functions of eta: Eqs. (29), (35)-(38), (42)
# ---------------------------------------------------------------------------

def potential_derivative(eta):
    """V'(eta), V = (1 - eta)^2 / 2, Eq. (35)."""
    return -(1.0 - eta)


def _g_raw(eta, c):
    """g(eta) of Eq. (35) without C0."""
    return (7.0 * eta ** 3 - 6.0 * eta ** 4) / (1.0 - eta) ** 3 + c * np.log(1.0 - eta)


def g_offset(c):
    """C0 of Eq. (36): the shift that makes min g = 0.01 on [0, eta_cutoff].

    g_raw(0) = 0 and the log term makes it negative at intermediate eta, so
    C0 = -min(g_raw) + 0.01.  The paper prints "min(...) + 0.01" [E2].
    The minimum is taken on a fine grid.
    """
    eta = np.linspace(0.0, ETA_CUTOFF, 200001)
    return -min(_g_raw(eta, c).min(), 0.0) + 0.01


def coupling_g(eta, c, offset):
    """g(eta) and g'(eta), Eq. (35), with g -> g(min(eta, eta_cutoff)) [P3]."""
    e = np.minimum(eta, ETA_CUTOFF)
    g = _g_raw(e, c) + offset
    dg = ((21.0 * e ** 2 - 24.0 * e ** 3) / (1.0 - e) ** 3
          + 3.0 * (7.0 * e ** 3 - 6.0 * e ** 4) / (1.0 - e) ** 4
          - c / (1.0 - e))
    dg = np.where(eta < ETA_CUTOFF, dg, 0.0)      # g constant above the cutoff
    return g, dg


def phi_derivative(eta, p):
    """phi'(eta), Eq. (38): ~c3 inside the GB, ~0 in the grain.

    This is what confines f_eta4 to the GB.  phi itself (Eq. 37) is not needed:
    without plasticity only phi' enters the dynamics.
    """
    return 0.5 * p.c3 * (1.0 - np.tanh(p.c1 * (eta - p.c2)))


def recovery_localiser(eta):
    """A(eta), Eq. (29), frozen above eta_cutoff like g [N2]."""
    e = np.minimum(eta, ETA_CUTOFF)
    return (7.0 * e ** 3 - 6.0 * e ** 4) / (1.0 - e)


def eta_equilibrium(rho, p):
    """eta_eq of a widened GB, Eq. (42), with phi'(eta_eq) = c3.

    Nucleation needs eta_eq ABOVE the initial GB depth (Sec. 3.2.3).
    """
    return 1.0 - p.c3 * p.stored_energy_per_rho * rho / (p.f0 * p.alpha)


# ---------------------------------------------------------------------------
# 1.2  Grid, right-hand side, time integration [N1]
# ---------------------------------------------------------------------------

@dataclass
class Grid:
    """1D grid of `cells` finite volumes over `length`.

    periodic = False: half period, grain centre to grain centre, GB at L/2,
    zero-flux walls [R1].  Default = UO2 [U7], [N4]; Cu uses CU_GRID.
    periodic = True: a ring (x = 0 and x = L are the same point), used for the
    polycrystal of Part 3b [R6].
    """
    length: float = 25.0e-6             # m
    cells: int = 2500                   # -      dx = 10 nm
    periodic: bool = False

    @property
    def dx(self):
        return self.length / self.cells

    @property
    def x(self):
        """Cell centres."""
        return (np.arange(self.cells) + 0.5) * self.dx


def split(y, n):
    """State vector -> (eta, theta, rho, m).

    y = [eta (n), theta (n), rho (n), m (n)].  m in [0, 1] is the SWEPT
    MARKER [U12]: 1 in as-fabricated material, decaying by the same recovery
    law as rho wherever a boundary sweeps.  The irradiation source is
    multiplied by it, so a new grain is no longer reloaded with dislocations.
    """
    return y[:n], y[n:2 * n], y[2 * n:3 * n], y[3 * n:]


def theta_at_gb(y, n, cell=None):
    """theta [rad] on the face between cells `cell - 1` and `cell` (default:
    x = L/2): the mean of the two cells either side of that face.

    Cell n//2 alone sits half a cell off L/2; inside a steep theta profile the
    two differ.  Works on y or sol.y.
    """
    cell = n // 2 if cell is None else cell
    return 0.5 * (y[n + (cell - 1) % n] + y[n + cell])


def _right_hand_side(p, grid, source=None, sink=None, rotation=None):
    """d/dt [eta, theta, rho], Eqs. (32), (34), (28).

    source(t) [m^-2/s]: uniform dislocation production [U3]; None = no
    production, as in the Cu protocol of [T26] [R3].
    sink(t) [1/s] and rotation = (s(x), rate(t)) [-, rad/s]: the transfer of
    the tangle into walls and the lattice rotation it makes, prescribed by
    [HBS] (Part 3b, Step B, [U13]-[U15]).  None = off.
    """
    n, dx = grid.cells, grid.dx
    offset = g_offset(p.c)
    tau_eta = p.tau_eta * p.f0 * p.t0            # J s m^-3
    tau_hat = p.tau_hat * p.f0 * p.t0            # J s m^-3
    energy = p.stored_energy_per_rho             # J/m
    dead_zone = RECOVERY_DEAD_ZONE / p.t0        # 1/s

    def injected(marker):
        """Share of the source that still reaches this point [U12].
        SOURCE_IN_SWEPT = 1 restores the uniform source of [U3]."""
        return SOURCE_IN_SWEPT + (1.0 - SOURCE_IN_SWEPT) * np.clip(marker, 0.0, 1.0)

    def growth(eta_dot):
        """<d eta/dt> of Eq. (28) with a dead zone [N8]: 0 below dead_zone."""
        return np.maximum(eta_dot - dead_zone, 0.0)

    def rhs(t, y):
        eta, theta, rho, marker = split(y, n)
        g, dg = coupling_g(eta, p.c, offset)
        if grid.periodic:
            return _periodic_rates(t, eta, theta, rho, marker, g, dg)

        # gradients on the n + 1 faces; the two wall faces stay 0 (zero flux)
        dtheta = np.zeros(n + 1)
        dtheta[1:-1] = np.diff(theta) / dx
        deta = np.zeros(n + 1)
        deta[1:-1] = np.diff(eta) / dx
        g_face = np.zeros(n + 1)
        g_face[1:-1] = 2.0 * g[:-1] * g[1:] / (g[:-1] + g[1:])     # harmonic mean

        # Eq. (34) [R2]: tau_hat g theta_dot = f0 d/dx [ mu^2 g theta' ]
        flux = p.mu ** 2 * (g_face * dtheta)
        theta_dot = p.f0 * np.diff(flux) / dx / (tau_hat * g)

        # Eq. (32) with the forces of Eq. (41), same signs as the paper.
        # f_eta2 and f_eta3 are summed inside ONE bracket on purpose: the
        # irradiation runs are sensitive to the round-off of this sum, and
        # this is the grouping UO2_C3 was calibrated with.
        theta_prime_sq = 0.5 * (dtheta[:-1] ** 2 + dtheta[1:] ** 2)  # at cell centres
        f_eta1 = p.f0 * p.nu ** 2 * np.diff(deta) / dx
        f_eta23 = -p.f0 * (p.alpha * potential_derivative(eta)
                           + p.mu ** 2 * dg * theta_prime_sq)
        f_eta4 = -phi_derivative(eta, p) * energy * rho
        eta_dot = (f_eta1 + f_eta23 + f_eta4) / tau_eta

        # Eq. (28), recovery branch only [R3]: active where eta grows
        swept = p.c_d * recovery_localiser(eta) * growth(eta_dot)
        rho_dot = -rho * swept
        marker_dot = -marker * swept             # [U12] same law, m in [0, 1]
        if source is not None:                   # [U3] uniform over the material
            rho_dot = rho_dot + source(t) * injected(marker)   # that is still there

        return np.concatenate([eta_dot, theta_dot, rho_dot, marker_dot])

    def _periodic_rates(t, eta, theta, rho, marker, g, dg):
        """The same equations on a ring [R6]: n faces, face i between cells
        i - 1 and i (face 0 closes the ring).  theta itself is periodic,
        because the parent orientations return to the first one (Part 3b)."""
        dtheta = (theta - np.roll(theta, 1)) / dx
        deta = (eta - np.roll(eta, 1)) / dx
        g_left = np.roll(g, 1)
        g_face = 2.0 * g_left * g / (g_left + g)                # harmonic mean
        flux = p.mu ** 2 * (g_face * dtheta)
        theta_dot = p.f0 * (np.roll(flux, -1) - flux) / dx / (tau_hat * g)
        theta_prime_sq = 0.5 * (dtheta ** 2 + np.roll(dtheta, -1) ** 2)
        f_eta1 = p.f0 * p.nu ** 2 * (np.roll(deta, -1) - deta) / dx
        f_eta23 = -p.f0 * (p.alpha * potential_derivative(eta)
                           + p.mu ** 2 * dg * theta_prime_sq)
        f_eta4 = -phi_derivative(eta, p) * energy * rho
        eta_dot = (f_eta1 + f_eta23 + f_eta4) / tau_eta
        swept = p.c_d * recovery_localiser(eta) * growth(eta_dot)
        rho_dot = -rho * swept
        marker_dot = -marker * swept             # [U12]
        if source is not None:
            rho_dot = rho_dot + source(t) * injected(marker)
        if sink is not None:
            # [U15] the tangle goes into walls at the relative rate [HBS] asks
            rho_dot = rho_dot - sink(t) * rho
        if rotation is not None:
            # (A4): tau_hat g (theta_dot - W) = f0 d/dx[mu^2 g theta'],
            # W = 1/2 s(x) dTheta_L/dt, scaled by the local share of the tangle
            # (a recovered grain has nothing to polygonise) [U13], [U14]
            sign, rate, tangle = rotation
            theta_dot = theta_dot + 0.5 * sign * rate(t) * rho / tangle(t)
        return np.concatenate([eta_dot, theta_dot, rho_dot, marker_dot])

    return rhs


RTOL = 1.0e-5                           # -      BDF relative tolerance [N1]
ATOL = 1.0e-8                           # -, rad absolute tolerance on eta and theta
ATOL_RHO = 1.0e4                        # m^-2   absolute tolerance on rho [N6]
RECOVERY_DEAD_ZONE = 1.0e-9              # 1/t0   <d eta/dt> ignored below this [N8]
SOURCE_IN_SWEPT = 0.0                   # -      share of the source that still reaches
                                        #        material a boundary has swept [U12]:
                                        #        0 = new grains are not reloaded,
                                        #        1 = the uniform source of [U3]


MAX_JAC_FACTOR = 1.0e-2                 # -      cap on scipy's relative Jacobian step [N9]


def _capped_jacobian(rhs, pattern, atol):
    """scipy's own sparse finite-difference Jacobian, with its step capped [N9].

    This is what solve_ivp(jac_sparsity=...) does internally (num_jac with the
    same column grouping and threshold, scipy 1.17), with ONE change: scipy
    multiplies the relative step of a column by 10 whenever that column's
    derivative looks ~0, without any cap, until it overflows.  The Jacobian
    then contains NaN and BDF fails with "Factor is exactly singular".  Here
    that happens on the swept marker of [U12], whose derivative is exactly 0
    until the first boundary sweeps.
    """
    sparsity = (pattern, group_columns(pattern))
    state = {"factor": None}

    def columns(t, y):
        return np.column_stack([rhs(t, y[:, k]) for k in range(y.shape[1])])

    def jac(t, y):
        J, factor = num_jac(columns, t, y, rhs(t, y), atol, state["factor"], sparsity)
        state["factor"] = np.minimum(factor, MAX_JAC_FACTOR)
        return J

    return jac


def evolve(p, grid, y0, t_end, times, source=None, max_step=np.inf, sink=None,
           rotation=None):
    """Integrate from 0 to t_end, output at `times` (stiff BDF, adaptive) [N1]."""
    n = grid.cells
    offsets = [-1, 0, 1] + ([-(n - 1), n - 1] if grid.periodic else [])  # ring: cell 0
    neighbours = diags([1.0] * len(offsets), offsets, shape=(n, n))     # and n-1 touch
    pattern = kron(np.ones((4, 4)), neighbours, format="csr")   # every field of a cell
    atol = np.concatenate([np.full(2 * n, ATOL), np.full(n, ATOL_RHO),  # sees every
                           np.full(n, ATOL)])
    rhs = _right_hand_side(p, grid, source, sink, rotation)              # field of the
    sol = solve_ivp(rhs, (0.0, t_end), y0,                               # cell and its
                    method="BDF", t_eval=times, max_step=max_step,       # neighbours
                    jac=_capped_jacobian(rhs, pattern.tocsc(), atol),
                    rtol=RTOL, atol=atol)
    if not sol.success:
        raise RuntimeError(sol.message)
    return sol


# ---------------------------------------------------------------------------
# 1.3  Initial condition [P6], [N3]
# ---------------------------------------------------------------------------

_RELAXED = {}


def initial_state(p, grid, delta_theta_deg, eta0=0.99):
    """Relaxed GB without dislocations: y = [eta, theta, rho = 0].

    theta: a step of height delta_theta at L/2 (smoothed over 2 dx);
    eta = eta0; then relaxed for 2e4 t0 with rho = 0 [N3].

    Cached: only the parameters in `key` enter (rho = 0, so c3, C_D, lambda,
    G, b do not).  A copy is returned because callers write rho into it.
    """
    key = (p.f0, p.nu, p.alpha, p.mu, p.c, p.t0,
           eta0, delta_theta_deg, grid.length, grid.cells)
    if key not in _RELAXED:
        n = grid.cells
        theta = math.radians(delta_theta_deg) * 0.5 * (
            1.0 + np.tanh((grid.x - 0.5 * grid.length) / (2.0 * grid.dx)))
        y0 = np.concatenate([np.full(n, eta0), theta, np.zeros(n), np.ones(n)])
        t_relax = 2.0e4 * p.t0
        relax = evolve(replace(p, tau_eta=1.0e2, tau_hat=1.0e1), grid, y0, t_relax, [t_relax])
        _RELAXED[key] = relax.y[:, -1]
        _RELAXED[key][3 * n:] = 1.0     # the relaxation is a preparation step [N3]:
                                        # eta rises from eta0 to ~1 everywhere, which
                                        # would "sweep" the whole domain [U12]
    return _RELAXED[key].copy()


# ---------------------------------------------------------------------------
# 1.4  Post-processing
# ---------------------------------------------------------------------------

def gb_energy(p, grid, y):
    """GB energy [J/m^2] of a profile WITHOUT dislocations: Eq. (15), line 1,
    integrated over the domain (the grains contribute 0).  Fig. 2 calibrates it.
    """
    n, dx = grid.cells, grid.dx
    eta, theta, _, _ = split(y, n)
    g, _ = coupling_g(eta, p.c, g_offset(p.c))
    g_face = 2.0 * g[:-1] * g[1:] / (g[:-1] + g[1:])
    cells = p.f0 * p.alpha * 0.5 * (1.0 - eta) ** 2
    faces = p.f0 * (0.5 * p.nu ** 2 * (np.diff(eta) / dx) ** 2
                    + p.mu ** 2 * g_face * (np.diff(theta) / dx) ** 2)
    return float(cells.sum() * dx + faces.sum() * dx)


def critical_radius(p, gamma, rho):
    """r* = 2 gamma / (lambda/2 G b^2 rho): capillarity, absent in 1D [E1], [Q6]."""
    return 2.0 * gamma / (p.stored_energy_per_rho * rho)


def grain_statistics(p, grid, y, rho_ref):
    """(recrystallised fraction, number of grains, number of new grains).

    grain = run of cells with eta > c2;  new grain = grain with mean
    rho < 0.1 rho_ref;  recrystallised = cells with eta > c2 AND rho < 0.1 rho_ref.
    """
    n = grid.cells
    eta, _, rho, _ = split(y, n)
    bulk = eta > p.c2
    fresh = bulk & (rho < 0.1 * rho_ref) if rho_ref > 0 else np.zeros(n, bool)
    edges = np.flatnonzero(np.diff(np.concatenate([[0], bulk.astype(int), [0]])))
    grains = [np.arange(a, b) for a, b in zip(edges[::2], edges[1::2])]
    if grid.periodic and len(grains) > 1 and grains[0][0] == 0 and grains[-1][-1] == n - 1:
        grains = [np.concatenate([grains[-1], grains[0]])] + grains[1:-1]   # wraps
    new = sum(1 for cells in grains if rho_ref > 0 and rho[cells].mean() < 0.1 * rho_ref)
    return float(fresh.mean()), len(grains), new


def classify_event(p, grid, sol, rho_ref, delta_theta_deg):
    """[N5] for the single GB of the half period: see classify_site."""
    n = grid.cells
    return classify_site(p, grid, sol, rho_ref, n // 2, 0, n - 1, 0.0, delta_theta_deg)


def _cells_between(a, b, n):
    """Cells a, a+1, ..., b-1 going forward, wrapping around a ring of n."""
    return np.arange(a, b) if a <= b else np.concatenate([np.arange(a, n), np.arange(0, b)])


def classify_site(p, grid, sol, rho_ref, gb, left, right, theta_left, theta_right):
    """(event, output index of the event, nucleus misorientation [deg])  [N5].

    One GB: `gb` is the cell just right of the GB plane, `left` and `right`
    the centre cells of the two parent grains, theta_left/right [deg] their
    initial orientations.  rho_ref: a scalar (run_case) or one value per
    output time (irradiation).

      1. rho at the GB falls below 0.1 rho_ref; the FIRST time is the birth
         ("a dislocation-free new grain", Sec. 3.2.2);
      2. at that time both parent centres still have eta > c2 (the nucleus is
         born inside a GB between two existing grains; this separates
         [T26]'s 5 deg case from the 2.5 deg one, Sec. 3.2.3);
      3. at that time or later, eta at the GB > c2 while eta < c2 somewhere
         between the GB and EACH parent centre: "the bulge of eta" and the
         two new boundaries (Sec. 3.2.3).

    The nucleus misorientation is its angle to the closer parent, read at the
    LAST output where condition 3 holds (Eq. 43 concerns the settled
    orientation, not the one at birth).
    """
    n = grid.cells
    ref = np.broadcast_to(np.asarray(rho_ref, dtype=float), (sol.t.size,))
    eta, rho_gb = sol.y[:n], sol.y[2 * n + gb]
    theta_gb = np.degrees(theta_at_gb(sol.y, n, gb))
    low, high = min(theta_left, theta_right), max(theta_left, theta_right)

    recovered = np.flatnonzero((ref > 0.0) & (rho_gb < 0.1 * ref))      # test 1
    if not recovered.size:
        return "none", -1, math.nan
    k = int(recovered[0])

    born_in_a_grain = min(eta[left, k], eta[right, k]) > p.c2           # test 2
    bulge = ((eta[gb] > p.c2)                                           # test 3
             & (eta[_cells_between(left, gb, n)].min(axis=0) < p.c2)
             & (eta[_cells_between(gb + 1, right + 1, n)].min(axis=0) < p.c2))
    grown = np.flatnonzero(bulge)
    grown = grown[grown >= k]
    if born_in_a_grain and grown.size:
        theta_nuc = theta_gb[int(grown[-1])]
        return "nucleus", k, min(abs(theta_nuc - theta_left), abs(theta_nuc - theta_right))

    spread = high - low
    if not low + 0.1 * spread < theta_gb[-1] < high - 0.1 * spread:
        return "migration", k, math.nan
    return "collapse", k, math.nan


def _pyplot():
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    return plt


def _save(fig, path, dpi=150):
    fig.tight_layout()
    fig.savefig(path, dpi=dpi)
    print(f"wrote {path}")


# ###########################################################################
#   PART 2 - Cu: REPRODUCTION OF [T26], SEC. 3.2 (step protocol [P6])
# ###########################################################################

CU = PhaseFieldParameters()
CU_RHO0 = 2.5e15                                # m^-2   Sec. 3.2
CU_DELTA_THETA = 15.0                           # deg    Fig. 4
CU_GRID = Grid(length=10.0e-6, cells=400)       # [P7]: half of the 20 um period;
                                                # dx = 25 nm, half the paper's


@dataclass
class Outcome:
    delta_theta_deg: float
    rho0: object                    # m^-2, scalar or (left, right) as in Fig. 6
    eta_eq: float                   # Eq. (42) at the larger rho
    eta_gb0: float                  # GB depth before rho is switched on
    nucleated: bool
    t_recovery: float               # s, first time rho(centre) < 0.1 rho0 (nan: never)
    recrystallised: float           # at t_end, see grain_statistics
    theta_centre_deg: float         # at t_end
    initial: np.ndarray             # relaxed GB, before rho is switched on
    solution: object
    event: str                      # "nucleus", "migration", "collapse", "none" [N5]
    nucleus_misorientation_deg: float   # to the closer parent (nan: no nucleus)


def run_case(p, delta_theta_deg, rho0, t_end=1.0e4, grid=None, samples=(), every=50.0):
    """Step protocol of Sec. 3.2 [P6]: relax the GB, switch on rho0, evolve.

    rho0: scalar (Figs. 4, 5) or (rho_left, rho_right) with a jump at the GB
    (Fig. 6).  t_end, samples, every are in units of t0 (= s for Cu).
    """
    grid = grid or Grid()
    n, centre = grid.cells, grid.cells // 2
    y = initial_state(p, grid, delta_theta_deg)
    relaxed = y.copy()

    rho_left, rho_right = rho0 if isinstance(rho0, tuple) else (rho0, rho0)
    y[2 * n:3 * n] = np.where(grid.x < 0.5 * grid.length, rho_left, rho_right)
    rho_max = max(rho_left, rho_right)          # thresholds use the larger one

    t_end, every = t_end * p.t0, every * p.t0
    times = np.union1d(np.arange(every, t_end + 0.5 * every, every),
                       np.asarray(samples, dtype=float) * p.t0)
    sol = evolve(p, grid, y, t_end, times)

    hit = np.flatnonzero(sol.y[2 * n + centre] < 0.1 * rho_max) if rho_max > 0 else []
    event, _, misorientation = classify_event(p, grid, sol, rho_max, delta_theta_deg)
    return Outcome(
        delta_theta_deg=delta_theta_deg,
        rho0=(rho_left, rho_right) if rho_left != rho_right else rho_left,
        eta_eq=eta_equilibrium(rho_max, p),
        eta_gb0=float(relaxed[:n].min()),
        nucleated=event == "nucleus",
        t_recovery=float(sol.t[hit[0]]) if len(hit) else math.nan,
        recrystallised=grain_statistics(p, grid, sol.y[:, -1], rho_max)[0],
        theta_centre_deg=math.degrees(theta_at_gb(sol.y, n)[-1]),
        initial=relaxed, solution=sol, event=event,
        nucleus_misorientation_deg=misorientation)


def state_at(outcome, t):
    """Output column closest to time t."""
    return outcome.solution.y[:, int(np.argmin(np.abs(outcome.solution.t - t)))]


def gamma_curve(p, angles, grid=None):
    """gamma(Delta theta) [J/m^2] of the relaxed 1D profiles (Fig. 2)."""
    grid = grid or Grid()
    return np.array([gb_energy(p, grid, initial_state(p, grid, a)) for a in angles])


def plot_state_functions(path):
    """Fig. 1 of [T26] plus V, g and A: the state functions ([PFRP] advice)."""
    plt = _pyplot()
    eta = np.linspace(0.0, 0.999, 2000)
    g, dg = coupling_g(eta, CU.c, g_offset(CU.c))
    fig, axes = plt.subplots(1, 4, figsize=(16, 3.6))
    axes[0].plot(eta, 0.5 * (1.0 - eta) ** 2, label=r"$V$")
    axes[0].plot(eta, potential_derivative(eta), label=r"$V'$")
    axes[0].set_title("potential, Eq. (35)", fontsize=9)
    axes[1].semilogy(eta, g, label=r"$g$")
    axes[1].semilogy(eta, np.abs(dg), "--", label=r"$|g'|$")
    axes[1].axhline(0.01, color="grey", lw=0.8, ls=":")
    axes[1].set_title(rf"coupling, Eqs. (35)-(36), $c$ = {CU.c:g}, "
                      rf"$C_0$ = {g_offset(CU.c):.3f}", fontsize=9)
    # Fig. 1 of [T26]: c1 = 100, c2 = 0.9, phi shifted to 1 at eta = 1 as in the caption
    fig1 = replace(CU, c1=100.0, c2=0.9)
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
    _save(fig, path)


def plot_gb_energy(p, path, angles=(1.0, 2.5, 5.0, 7.5, 10.0, 15.0, 20.0, 25.0, 30.0),
                   targets=None, grid=None):
    """Fig. 2: gamma of the relaxed 1D profile vs misorientation.

    targets: optional {label: gamma at `angles`} drawn dashed (e.g. [Z21]).
    """
    plt = _pyplot()
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
    _save(fig, path)


def plot_profiles(p, cases, times, path, title, grid=CU_GRID):
    """eta, theta, rho at `times` [t0] for (label, delta_theta, rho0) cases (Figs. 4-6)."""
    plt = _pyplot()
    n, x = grid.cells, grid.x * 1e6
    styles = ["-", "--", "-.", ":", (0, (1, 3)), (0, (5, 1)), (0, (3, 1, 1, 1))]
    colours = plt.cm.tab10(np.arange(len(cases)))
    fig, axes = plt.subplots(1, 3, figsize=(15, 4))
    for (label, angle, rho0), colour in zip(cases, colours):
        out = run_case(p, angle, rho0, t_end=times[-1], grid=grid, samples=times)
        print(f"  {label}: {out.event}, theta_centre = {out.theta_centre_deg:.2f} deg, "
              f"nucleus misorientation = {out.nucleus_misorientation_deg:.2f} deg")
        for t, style in zip(times, styles):
            eta, theta, rho, _ = split(state_at(out, t * p.t0), n)
            axes[0].plot(x, eta, ls=style, color=colour, label=label if t == times[0] else None)
            axes[1].plot(x, np.degrees(theta), ls=style, color=colour)
            axes[2].plot(x, rho, ls=style, color=colour)
    if len(cases) == 1:
        axes[0].axhline(eta_equilibrium(max(np.atleast_1d(cases[0][2])), p),
                        color="grey", lw=0.8, label=r"$\eta^{eq}$, Eq. (42)")
    for ax, label in zip(axes, (r"$\eta$", r"$\theta$ [deg]", r"$\rho$ [m$^{-2}$]")):
        ax.set_ylabel(label)
        ax.set_xlabel(r"$x$ [µm]")
    axes[0].legend(fontsize=7)
    fig.suptitle(title + "   lines: " + ", ".join(
        f"{s if isinstance(s, str) else '·'} t={t:g} s" for t, s in zip(times, styles)),
        fontsize=9)
    _save(fig, path)


# ###########################################################################
#   PART 3 - UO2 DRIVEN BY THE HBS LANDAU MODEL
# ###########################################################################

RHO_KEYS = {"tot": "rho_tot", "free": "rho_free"}
FIT_ANGLES = (2.5, 5.0, 7.5, 10.0, 15.0, 20.0, 25.0, 30.0)
UO2_GAMMA_LABELS = {"random": "[Z21], random axis and plane",
                    "st110": r"[Z21], sym. tilt $\langle 110\rangle$",
                    "st100": r"[Z21], sym. tilt $\langle 100\rangle$"}


def dislocation_density(burnup, temperature=TEMPERATURE, which=RHO_KIND):
    """rho(bu) [m^-2] from [HBS] [U1]."""
    return getattr(hbs_state(burnup, temperature), RHO_KEYS[which])


def uo2_parameters(temperature=TEMPERATURE, **changes):
    """Cu parameters with the UO2 values of the RUN CONFIGURATION.

    Changed: f0, c [U5]; nu [U6]; G(T), b [U1]; t0 [Q1]; C_D [U10]; c3 [U8].
    Kept from Cu: alpha, mu, c1, c2, lambda, tau_eta, tau_hat [U9].
    G is taken at HBS_ONSET_BURNUP, but in [HBS] it does not depend on burnup.
    """
    shear_modulus = hbs_state(HBS_ONSET_BURNUP, temperature).shear_modulus
    return replace(CU, f0=UO2_F0, c=UO2_C, nu=UO2_NU, shear_modulus=shear_modulus,
                   burgers=BURGERS, t0=UO2_T0, c_d=UO2_C_D, c3=UO2_C3, **changes)


def uo2_gamma_targets(angles=FIT_ANGLES, which=UO2_GAMMA_LABELS):
    """{label: gamma [J/m^2]} of [Z21] from uo2_gb_energy (builds its table if needed)."""
    import uo2_gb_energy
    return {UO2_GAMMA_LABELS[key]: uo2_gb_energy.target(angles, key) for key in which}


def fit_uo2_interface(angles=FIT_ANGLES, which=UO2_GAMMA_LABELS, grid=None,
                      c_grid=(3.0, 5.0, 7.0, 9.0, 12.0, 16.0, 20.0, 26.0, 30.0, 34.0,
                              38.0, 45.0)):
    """(f0, c) reproducing each [Z21] curve at nu = UO2_NU [U5].  Not run by main().

    Eq. (15) is linear in f0 at fixed (nu, alpha, mu, c), so for each c the
    best f0 is closed-form, f0 = f0_base (gamma . target) / (gamma . gamma);
    only c is scanned.  The optimal c moves with nu (nu = 1, 1.5, 2.5 um give
    c = 7, 16, 34), hence the wide c_grid.

    Returns {label: (f0, c, rms, table)}, table rows = (c, f0, rms, max rel. error).
    """
    base = replace(CU, nu=UO2_NU)
    grid = grid or Grid()
    print(f"GB energy fit on [Z21] ({grid.cells} cells, nu = {base.nu * 1e6:g} um):")
    curves = {c: gamma_curve(replace(base, c=c), angles, grid) for c in c_grid}
    fits = {}
    for label, target in uo2_gamma_targets(angles, which).items():
        target = np.asarray(target, dtype=float)
        table = []
        for c in c_grid:
            gamma = curves[c]
            scale = float(gamma @ target / (gamma @ gamma))
            residual = scale * gamma - target
            table.append((c, base.f0 * scale, float(np.sqrt(np.mean(residual ** 2))),
                          float(np.max(np.abs(residual / target)))))
        c, f0, rms, worst = min(table, key=lambda row: row[2])
        print(f"  {label}: f0 = {f0 / 1e3:.1f} kPa, c = {c:g} "
              f"(rms = {rms:.4f} J/m^2, max = {100 * worst:.1f} %)")
        fits[label] = (f0, c, rms, table)
    return fits


@dataclass
class IrradiationOutcome:
    delta_theta_deg: float
    bu_nucleation: float            # GWd/tU, first recovery at the GB centre (nan: none)
    event: str                      # "nucleus", "migration", "collapse", "none" [N5]
    nucleus_misorientation_deg: float   # to the closer parent, see classify_event
    burnup: np.ndarray              # GWd/tU at the outputs
    rho_ref: np.ndarray             # m^-2, produced since bu_start, without recovery [U3]
    rho_centre: np.ndarray          # m^-2, at the GB centre
    rho_mean: np.ndarray            # m^-2, domain mean
    eta_centre: np.ndarray
    theta_centre_deg: np.ndarray
    recrystallised: np.ndarray      # see grain_statistics
    new_grains: np.ndarray
    solution: object
    temperature: float
    which: str


def run_irradiation(p, delta_theta_deg, bu_start, bu_end, bu_rate,
                    temperature=TEMPERATURE, which=RHO_KIND, grid=None, samples_per_gwd=4):
    """Relaxed GB with rho = 0 at bu_start [U4], then irradiated to bu_end [U3].

        d rho/dt = S(t) - rho C_D A(eta) <d eta/dt>,   S = d rho_ref/d bu * bu_rate
        bu(t)    = bu_start + bu_rate t
    """
    grid = grid or Grid()
    n, centre = grid.cells, grid.cells // 2

    # S(bu), tabulated once every 0.05 GWd/tU
    bu_table = np.linspace(bu_start, bu_end, int(20 * (bu_end - bu_start)) + 2)
    rho_table = np.array([dislocation_density(b, temperature, which) for b in bu_table])
    rate_table = np.gradient(rho_table, bu_table) * bu_rate

    def source(t):
        return float(np.interp(bu_start + bu_rate * t, bu_table, rate_table))

    y = initial_state(p, grid, delta_theta_deg)
    t_end = (bu_end - bu_start) / bu_rate
    times = np.linspace(0.0, t_end, int(samples_per_gwd * (bu_end - bu_start)) + 1)
    sol = evolve(p, grid, y, t_end, times, source=source,
                 max_step=0.25 / bu_rate)        # [N] never step over 0.25 GWd/tU of source

    bu = bu_start + bu_rate * sol.t
    ref = np.interp(bu, bu_table, rho_table - rho_table[0])
    stats = np.array([grain_statistics(p, grid, sol.y[:, k], ref[k]) for k in range(bu.size)])
    event, k_event, misorientation = classify_event(p, grid, sol, ref, delta_theta_deg)
    return IrradiationOutcome(
        delta_theta_deg=delta_theta_deg,
        bu_nucleation=float(bu[k_event]) if k_event >= 0 else math.nan,
        event=event, nucleus_misorientation_deg=misorientation,
        burnup=bu, rho_ref=ref,
        rho_centre=sol.y[2 * n + centre], rho_mean=sol.y[2 * n:3 * n].mean(axis=0),
        eta_centre=sol.y[centre], theta_centre_deg=np.degrees(theta_at_gb(sol.y, n)),
        recrystallised=stats[:, 0], new_grains=stats[:, 2].astype(int),
        solution=sol, temperature=temperature, which=which)


def calibrate_c3(p, delta_theta_deg, bu_target, bu_start, bu_rate,
                 temperature=TEMPERATURE, which=RHO_KIND,
                 bracket=(0.05, 10.0), tol=0.5, margin=10.0, grid=None):
    """c3 such that the first event happens at bu_target [U8].

    Larger c3 = stronger f_eta4 = earlier onset, so bisection on log c3 inside
    `bracket`; each run stops at bu_target + margin; stops within `tol`.
    ~12 histories on the production grid: about an hour.  Do NOT coarsen
    `grid` to make it cheaper [N4].
    """
    lo, hi = bracket
    for _ in range(12):
        c3 = math.sqrt(lo * hi)
        out = run_irradiation(replace(p, c3=c3), delta_theta_deg, bu_start,
                              bu_target + margin, bu_rate, temperature, which, grid=grid)
        bu = out.bu_nucleation if not math.isnan(out.bu_nucleation) else math.inf
        print(f"  c3 = {c3:.4f}: onset = {bu:6.2f} GWd/tU ({out.event})")
        if abs(bu - bu_target) <= tol:
            break
        if bu > bu_target:
            lo = c3                     # too late: more force needed
        else:
            hi = c3
    print(f"  -> c3 = {c3:.4f}")
    return c3


def burnup_at_eta_eq(target, p, temperature=TEMPERATURE, which=RHO_KIND,
                     bu_range=(1.0, 200.0), samples=4000):
    """First burnup where eta_eq(rho(bu)) of Eq. (42) drops below `target` (nan: never).

    target = eta_GB0: from there on the GB widens instead of nucleating;
    target = 0: from there on the model is outside its range [E4].
    """
    bu = np.linspace(bu_range[0], bu_range[1], samples)
    eq = np.array([eta_equilibrium(dislocation_density(b, temperature, which), p) for b in bu])
    below = np.flatnonzero(eq < target)
    return float(bu[below[0]]) if below.size else math.nan


def plot_widening_depth(p, eta_gb0, path, temperature=TEMPERATURE, which=RHO_KIND):
    """eta_eq(bu) of Eq. (42) for the c3 in use vs the initial GB depth."""
    plt = _pyplot()
    bu = np.linspace(20.0, 120.0, 201)
    rho = np.array([dislocation_density(b, temperature, which) for b in bu])
    fig, ax = plt.subplots(figsize=(5.6, 4.0))
    ax.plot(bu, eta_equilibrium(rho, p), color="k", label=f"$c_3$ = {p.c3:.3f}")
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
                 f"\n$\\rho_{{{which}}}$, T = {temperature:g} K", fontsize=9)
    _save(fig, path)


def plot_snapshots(p, out, burnups, path, grid=None):
    """eta, theta, rho profiles at several burnups of ONE history."""
    plt = _pyplot()
    grid = grid or Grid()
    n, x = grid.cells, grid.x * 1e6
    fig, axes = plt.subplots(1, 3, figsize=(15, 4))
    colours = plt.cm.viridis(np.linspace(0.0, 0.9, len(burnups)))
    for bu, colour in zip(burnups, colours):
        k = int(np.argmin(np.abs(out.burnup - bu)))
        eta, theta, rho, _ = split(out.solution.y[:, k], n)
        days = out.solution.t[k] / 86400.0
        axes[0].plot(x, eta, color=colour, label=f"bu = {out.burnup[k]:g} GWd/tU, t = {days:.0f} d")
        axes[1].plot(x, np.degrees(theta), color=colour)
        axes[2].plot(x, rho, color=colour)
    for ax, label in zip(axes, (r"$\eta$", r"$\theta$ [deg]", r"$\rho$ [m$^{-2}$]")):
        ax.set_ylabel(label)
        ax.set_xlabel(r"$x$ [µm]")
    axes[0].legend(fontsize=7)
    fig.suptitle(rf"UO$_2$ irradiation, $\Delta\theta$ = {out.delta_theta_deg:g}$^\circ$, "
                 f"{out.event} at bu = {out.bu_nucleation:.2f} GWd/tU, "
                 f"c3 = {p.c3:.3f}, t0 = {p.t0:.3g} s", fontsize=9)
    _save(fig, path)


def irradiation_scan(p, angles, bu_start, bu_end, bu_rate, temperature, which, path,
                     snapshot_burnups=(), snapshot_path=None, grid=None):
    """run_irradiation for several parent misorientations: table and figures."""
    plt = _pyplot()
    print(f"UO2, T = {temperature:g} K, c3 = {p.c3:.3f}, t0 = {p.t0:.3g} s, C_D = {p.c_d:g}, "
          f"source d rho_{which}/d bu, bu {bu_start:g} -> {bu_end:g} GWd/tU at "
          f"{bu_rate * SECONDS_PER_YEAR:g} GWd/tU/yr")
    print(f"{'dtheta':>7} {'bu_event':>8} {'event':>10} {'dth_nuc':>8} "
          f"{'Theta_Landau':>13} {'rho(bu_ev)':>11} {'max_new':>8}")
    outcomes = []
    for angle in angles:
        out = run_irradiation(p, angle, bu_start, bu_end, bu_rate, temperature, which, grid=grid)
        outcomes.append(out)
        if snapshot_path and snapshot_burnups:
            plot_snapshots(p, out, snapshot_burnups, snapshot_path.format(angle=angle), grid=grid)
        if math.isnan(out.bu_nucleation):
            print(f"{angle:7.2f} {'--':>8} {out.event:>10} {'':>8} {'':>13} {'':>11} "
                  f"{out.new_grains.max():8d}")
        else:
            landau = hbs_state(out.bu_nucleation, temperature).theta_deg
            print(f"{angle:7.2f} {out.bu_nucleation:8.2f} {out.event:>10} "
                  f"{out.nucleus_misorientation_deg:8.2f} {landau:13.2f} "
                  f"{np.interp(out.bu_nucleation, out.burnup, out.rho_ref):11.3e} "
                  f"{out.new_grains.max():8d}")

    fig, axes = plt.subplots(1, 3, figsize=(14, 3.9))
    colours = plt.cm.viridis(np.linspace(0.0, 0.85, len(outcomes)))
    for out, colour in zip(outcomes, colours):
        label = rf"$\Delta\theta$ = {out.delta_theta_deg:g}$^\circ$, {out.event}"
        axes[0].semilogy(out.burnup, np.maximum(out.rho_centre, 1e10), color=colour, label=label)
        axes[0].semilogy(out.burnup, out.rho_mean, "--", color=colour, lw=0.8)
        axes[1].plot(out.burnup, out.eta_centre, color=colour, label=label)
        axes[2].plot(out.burnup, out.theta_centre_deg, color=colour, label=label)
        if not math.isnan(out.bu_nucleation):
            for ax in axes:
                ax.axvline(out.bu_nucleation, color=colour, lw=0.6, ls=":")
    axes[0].semilogy(outcomes[0].burnup, outcomes[0].rho_ref, "k", lw=1.5,
                     label=rf"$\int S = \rho_{{{which}}}(bu) - \rho_{{{which}}}(bu_0)$")
    axes[0].set_ylim(1e12, None)
    axes[0].set_ylabel(r"$\rho$ [m$^{-2}$]: centre (—), domain mean (- -)")
    axes[1].set_ylabel(r"$\eta$ at the GB centre")
    axes[2].set_ylabel(r"$\theta$ at the GB centre [deg]")
    for ax in axes:
        ax.set_xlabel("burnup [GWd/tU]")
    axes[0].legend(fontsize=7)
    fig.suptitle(f"irradiation source, t0 = {p.t0:.3g} s, c3 = {p.c3:.3f}, "
                 f"T = {temperature:g} K", fontsize=10)
    _save(fig, path)
    return outcomes


def plot_diagnostics(p, outcomes, path, grid=None):
    """Phase field next to [HBS] along the irradiation histories.

    (1) GB depth vs eta_eq; (2) nucleus misorientation vs Landau Theta [E7];
    (3) critical radius r* vs Landau r_n [Q6]; (4) recrystallised fraction vs X.
    """
    plt = _pyplot()
    grid = grid or Grid()
    temperature, which = outcomes[0].temperature, outcomes[0].which
    bu = outcomes[0].burnup
    rho = np.array([dislocation_density(b, temperature, which) for b in bu])
    states = [hbs_state(b, temperature) for b in bu]

    fig, (a1, a2, a3, a4) = plt.subplots(1, 4, figsize=(19, 4.1))
    a1.plot(bu, eta_equilibrium(rho, p), "k", label=r"$\eta^{eq}(\rho)$, Eq. (42)")
    a1.axhline(0.0, color="grey", lw=0.8, ls=":")
    a1.set_ylabel(r"$\eta$")
    a1.set_title("widening depth vs the GB depth (dashed)", fontsize=9)
    a2.plot(bu, [s.theta_deg for s in states], "k", label=r"[HBS] $\Theta(bu)$")
    a2.axhline(THETA_HAGB, color="grey", lw=0.8, ls="--")
    a2.set_ylabel("misorientation [deg]")
    a2.set_title("nucleus misorientation vs [HBS] (side by side only, E7)", fontsize=9)
    a3.semilogy(bu, np.array([s.subgrain_radius_m for s in states]) * 1e6, "k",
                label=r"[HBS] sub-grain radius $r_n$")
    a3.set_ylabel("radius [µm]")
    a3.set_title("capillarity, missing in 1D (E1)", fontsize=9)
    a4.plot(bu, [s.restructured_fraction for s in states], "k", label="[HBS] $X$")
    a4.set_ylabel("restructured fraction")
    a4.set_ylim(-0.05, 1.05)
    a4.set_title("restructured fraction", fontsize=9)

    colours = plt.cm.viridis(np.linspace(0.0, 0.85, len(outcomes)))
    for out, colour in zip(outcomes, colours):
        label = rf"$\Delta\theta$ = {out.delta_theta_deg:g}$^\circ$, {out.event}"
        relaxed = out.solution.y[:, 0]
        gamma = gb_energy(p, grid, relaxed)
        a1.plot(bu, out.eta_centre, color=colour, lw=1.0, label=label + " (centre)")
        a1.axhline(float(relaxed[:grid.cells].min()), color=colour, lw=0.8, ls="--")
        a3.semilogy(bu, critical_radius(p, gamma, np.maximum(rho, 1e10)) * 1e6,
                    color=colour, lw=1.0, label=label)
        a4.plot(bu, out.recrystallised, color=colour, lw=1.0, label=label)
        if not math.isnan(out.bu_nucleation):
            for a in (a1, a2, a3, a4):
                a.axvline(out.bu_nucleation, color=colour, lw=0.6, ls=":")
            a2.plot([out.bu_nucleation], [out.nucleus_misorientation_deg], "*",
                    color=colour, ms=11, label=label + r": $\Delta\theta_{nucleus}$")
    for a in (a1, a2, a3, a4):
        a.set_xlabel("burnup [GWd/tU]")
        a.legend(fontsize=6.5)
    fig.suptitle(rf"UO$_2$ irradiation vs [HBS], $c_3$ = {p.c3:.3f}, "
                 rf"$\rho_{{{which}}}$, T = {temperature:g} K", fontsize=10)
    _save(fig, path)


# ###########################################################################
#   PART 3b - UO2 POLYCRYSTAL ON A RING (1D analogue of [T26] Sec. 3.3.2)
#
#   Step A: N parent grains with different orientations on a periodic 1D
#           domain [R6].  A new grain now meets GBs to OTHER grains instead of
#           its own mirror image, so the domain cannot end as one flat crystal
#           (E13) and migration is no longer forbidden by symmetry (E5).
#   Step B: optional lattice rotation prescribed by [HBS], the analogue of
#           [T26]'s loading phase (Sec. 3.3.1) running during irradiation
#           [U13]-[U15].  LANDAU_ROTATION switches it on.
# ###########################################################################

RING_DX = 10.0e-9                       # m      same resolution as Grid() [N4]
TANGLE_FLOOR = 1.0e12                   # m^-2   floor of the [HBS] tangle in Step B [N7]:
                                        #        below rho_crit it is 0, and rho/tangle
                                        #        would turn solver noise into rotation


@dataclass(frozen=True)
class Ring:
    """N parent grains on a ring; grain 0 is centred on x = 0 (= x = L).

    GB j separates grain j from grain j + 1 (mod N); the last one closes the
    ring back to grain 0, so theta returns to its first value and is periodic.
    """
    orientations_deg: tuple
    widths: tuple                       # m

    @property
    def length(self):
        return float(sum(self.widths))

    def gb_positions(self):
        """x of GB j, j = 0 .. N-1 [m]."""
        w = np.asarray(self.widths)
        return 0.5 * w[0] + np.concatenate([[0.0], np.cumsum(w[1:])])   # last: L - w0/2

    def centre_positions(self):
        """x of the centre of grain j [m]; grain 0 at x = 0."""
        w = np.asarray(self.widths)
        return np.concatenate([[0.0], 0.5 * w[0] + np.cumsum(w[1:]) - 0.5 * w[1:]])

    def grid(self):
        return Grid(length=self.length, cells=int(round(self.length / RING_DX)), periodic=True)


def make_ring(n_grains, width, seed=0, spread=0.2, max_angle=30.0):
    """Random parent microstructure: widths width (1 +- spread), orientations
    uniform in [0, max_angle] deg ([T26] Sec. 3.3.2 use 0-35 deg; 30 is the
    range of the GB-energy fit, E6)."""
    rng = np.random.default_rng(seed)
    widths = width * (1.0 + spread * rng.uniform(-1.0, 1.0, n_grains))
    angles = np.round(rng.uniform(0.0, max_angle, n_grains), 2)
    return Ring(tuple(float(a) for a in angles), tuple(float(w) for w in widths))


def ring_sites(ring, grid):
    """[(gb cell, left centre cell, right centre cell, theta_left, theta_right)]
    for every GB with a non-zero misorientation."""
    cell = lambda x: int(round(x / grid.dx)) % grid.cells        # noqa: E731
    centres = [cell(x) for x in ring.centre_positions()]
    angles, count = ring.orientations_deg, len(ring.widths)
    sites = []
    for j, x in enumerate(ring.gb_positions()):
        k = (j + 1) % count
        if angles[j] != angles[k]:
            sites.append((cell(x), centres[j], centres[k], angles[j], angles[k]))
    return sites


def ring_initial_state(p, ring, eta0=0.99):
    """Relaxed ring without dislocations: a staircase of tanh steps, one per GB
    (smoothed over 2 dx), relaxed like initial_state() [N3].  Cached."""
    grid = ring.grid()
    key = ("ring", p.f0, p.nu, p.alpha, p.mu, p.c, p.t0, eta0,
           ring.orientations_deg, ring.widths, grid.cells)
    if key not in _RELAXED:
        n, angles = grid.cells, np.radians(ring.orientations_deg)
        theta = np.full(n, angles[0])
        for j, x in enumerate(ring.gb_positions()):
            jump = angles[(j + 1) % angles.size] - angles[j]
            theta += jump * 0.5 * (1.0 + np.tanh((grid.x - x) / (2.0 * grid.dx)))
        y0 = np.concatenate([np.full(n, eta0), theta, np.zeros(n), np.ones(n)])
        t_relax = 2.0e4 * p.t0
        relax = evolve(replace(p, tau_eta=1.0e2, tau_hat=1.0e1), grid, y0, t_relax, [t_relax])
        _RELAXED[key] = relax.y[:, -1]
        _RELAXED[key][3 * n:] = 1.0     # the relaxation is a preparation step [N3]:
                                        # eta rises from eta0 to ~1 everywhere, which
                                        # would "sweep" the whole domain [U12]
    return _RELAXED[key].copy()


def subgrain_signs(ring, grid, size):
    """s(x) = +-1 [U14]: each parent grain tiled from its left GB with an EVEN
    number of subgrains of width ~size, signs alternating, so neighbouring
    subgrains turn opposite ways and every interior wall gains dTheta_L/dt."""
    sign = np.ones(grid.cells)
    edges = np.concatenate([[ring.gb_positions()[-1] - ring.length], ring.gb_positions()])
    for j, width in enumerate(ring.widths):
        m = max(2, 2 * int(round(width / (2.0 * size))))
        start = edges[j]                             # left GB of grain j
        offset = (grid.x - start) % ring.length
        inside = offset < width
        sign[inside] = np.where((offset[inside] // (width / m)) % 2 == 0, 1.0, -1.0)
    return sign


def _tabulated(function, bu_start, bu_end, bu_rate, step=0.05):
    """(value(bu), d value/dt(t)) of a burnup function, tabulated every `step`."""
    bu_table = np.linspace(bu_start, bu_end, int((bu_end - bu_start) / step) + 2)
    values = np.array([function(b) for b in bu_table])
    rates = np.gradient(values, bu_table) * bu_rate
    return (lambda bu: np.interp(bu, bu_table, values),
            lambda t: float(np.interp(bu_start + bu_rate * t, bu_table, rates)))


@dataclass
class RingOutcome:
    ring: Ring
    grid: Grid
    burnup: np.ndarray
    sites: list                     # [(event, bu, nucleus misorientation, theta_l, theta_r)]
    grains: np.ndarray              # number of grains (eta > c2 runs) vs burnup
    theta_spread_deg: np.ndarray    # max - min of theta vs burnup: 0 = one flat crystal
    lattice_step_deg: np.ndarray    # median theta step between subgrain centres (Step B)
    theta_landau_deg: np.ndarray    # [HBS] Theta(bu)
    eta_wall_min: np.ndarray        # min eta over the subgrain walls (Step B)
    rotation: bool
    solution: object


def run_ring(p, ring, bu_start, bu_end, bu_rate, temperature=TEMPERATURE, which=RHO_KIND,
             rotation=False, subgrain_size=None, samples_per_gwd=4):
    """Irradiate a ring of parent grains, rho = 0 at bu_start [U4].

    rotation = False (Step A): source d rho_which/d bu, as run_irradiation.
    rotation = True (Step B): production d rho_tot/d bu, transfer of the tangle
    into walls at the relative rate [HBS] asks (sink, [U15]) and the lattice
    rotation W = 1/2 s(x) dTheta_L/dt that those walls make [U13], [U14].
    """
    grid = ring.grid()
    n = grid.cells
    state = lambda b: hbs_state(b, temperature)                    # noqa: E731
    sink = rot = None
    if rotation:
        _, source = _tabulated(lambda b: state(b).rho_tot, bu_start, bu_end, bu_rate)
        _, ordered_rate = _tabulated(lambda b: state(b).rho_ordered, bu_start, bu_end, bu_rate)
        tangle_bu, _ = _tabulated(lambda b: max(state(b).rho_tot - state(b).rho_ordered,
                                                1.0e-3 * state(b).rho_tot, TANGLE_FLOOR),
                                  bu_start, bu_end, bu_rate)
        _, theta_rate = _tabulated(lambda b: math.radians(state(b).theta_deg),
                                   bu_start, bu_end, bu_rate)

        def tangle(t):          # Landau tangle rho_tot - rho_ord [m^-2], floored [N7]
            return float(tangle_bu(bu_start + bu_rate * t))

        def sink(t):            # relative transfer rate into walls [1/s]
            return ordered_rate(t) / tangle(t)

        if subgrain_size is None:
            subgrain_size = 2.0 * state(HBS_ONSET_BURNUP).subgrain_radius_m
        rot = (subgrain_signs(ring, grid, subgrain_size), theta_rate, tangle)
    else:
        _, source = _tabulated(lambda b: dislocation_density(b, temperature, which),
                               bu_start, bu_end, bu_rate)

    y = ring_initial_state(p, ring)
    t_end = (bu_end - bu_start) / bu_rate
    times = np.linspace(0.0, t_end, int(samples_per_gwd * (bu_end - bu_start)) + 1)
    sol = evolve(p, grid, y, t_end, times, source=source, max_step=0.25 / bu_rate,
                 sink=sink, rotation=rot)
    bu = bu_start + bu_rate * sol.t

    # reference for the recovery tests: the density a grain without recovery
    # would hold (the tangle), from the same tables
    if rotation:
        ref = np.array([tangle(t) for t in sol.t])
    else:
        ref = np.array([dislocation_density(b, temperature, which) for b in bu]) \
            - dislocation_density(bu_start, temperature, which)
    sites = []
    for gb, left, right, th_l, th_r in ring_sites(ring, grid):
        event, k, mis = classify_site(p, grid, sol, ref, gb, left, right, th_l, th_r)
        sites.append((event, float(bu[k]) if k >= 0 else math.nan, mis, th_l, th_r))
    grains = np.array([grain_statistics(p, grid, sol.y[:, k], ref[k])[1] for k in range(bu.size)])
    theta = np.degrees(sol.y[n:2 * n])

    step = np.full(bu.size, math.nan)
    wall_eta = np.full(bu.size, math.nan)
    if rotation:
        sign = rot[0]
        walls = np.flatnonzero(sign != np.roll(sign, 1))           # cells right of a wall
        gbs = {site[0] for site in ring_sites(ring, grid)}
        walls = np.array([w for w in walls if min(abs(w - g) for g in gbs | {-10 ** 9}) > 5])
        if walls.size > 1:
            middles = ((walls + np.roll(walls, -1) + n * (np.roll(walls, -1) < walls)) // 2) % n
            steps = np.abs(theta[middles] - theta[np.roll(middles, 1)])
            step = np.median(steps, axis=0)
            wall_eta = sol.y[walls].min(axis=0)
    return RingOutcome(ring, grid, bu, sites, grains, theta.max(axis=0) - theta.min(axis=0),
                       step, np.array([state(b).theta_deg for b in bu]), wall_eta,
                       rotation, sol)


def print_ring(out, tag):
    print(f"{tag}: {len(out.ring.widths)} grains, widths "
          f"{', '.join(f'{w * 1e6:.1f}' for w in out.ring.widths)} um, orientations "
          f"{', '.join(f'{a:g}' for a in out.ring.orientations_deg)} deg")
    print(f"  {'GB':>3} {'parents [deg]':>15} {'event':>10} {'bu':>7} {'dth_nuc':>8}")
    for j, (event, bu, mis, th_l, th_r) in enumerate(out.sites):
        print(f"  {j:3d} {th_l:7.2f}->{th_r:<7.2f} {event:>10} {bu:7.2f} {mis:8.2f}")
    n = out.grid.cells
    print(f"  end of history: {out.grains[-1]} grains, theta spread "
          f"{out.theta_spread_deg[-1]:.2f} deg (0 = one flat crystal), "
          f"rho {out.solution.y[2 * n:3 * n, -1].mean():.2e} m^-2, "
          f"unswept fraction {out.solution.y[3 * n:, -1].mean():.3f} [U12]")
    if out.rotation:
        print(f"  lattice step between subgrains at bu = {out.burnup[-1]:g}: "
              f"{out.lattice_step_deg[-1]:.3f} deg vs [HBS] Theta {out.theta_landau_deg[-1]:.2f} deg,"
              f" min eta at the walls {out.eta_wall_min[-1]:.4f}")


def plot_ring(p, out, path):
    """eta, theta, rho over (x, bu) for one ring history, GBs dotted."""
    plt = _pyplot()
    n, x = out.grid.cells, out.grid.x * 1e6
    y, bu = out.solution.y, out.burnup
    extent = (x[0], x[-1], bu[0], bu[-1])
    fig, axes = plt.subplots(1, 3 + out.rotation, figsize=(5.2 * (3 + out.rotation), 4.6))
    for ax, data, label, cmap in (
            (axes[0], y[:n].T, r"$\eta$", "viridis"),
            (axes[1], np.degrees(y[n:2 * n]).T, r"$\theta$ [deg]", "twilight"),
            (axes[2], np.log10(np.maximum(y[2 * n:3 * n], 1e10)).T, r"log$_{10}\rho$", "magma")):
        im = ax.imshow(data, origin="lower", aspect="auto", extent=extent, cmap=cmap)
        fig.colorbar(im, ax=ax, label=label)
        for gb in out.ring.gb_positions():
            ax.axvline(gb * 1e6, color="w", lw=0.5, ls=":")
        ax.set_xlabel(r"$x$ [µm]")
        ax.set_ylabel("burnup [GWd/tU]")
    if out.rotation:
        axes[3].plot(bu, out.lattice_step_deg, label="lattice step between subgrains")
        axes[3].plot(bu, out.theta_landau_deg, "k", label=r"[HBS] $\Theta$ (imposed)")
        axes[3].set_xlabel("burnup [GWd/tU]")
        axes[3].set_ylabel("misorientation [deg]")
        axes[3].legend(fontsize=7)
    events = ", ".join(f"{e}" + ("" if math.isnan(b) else f"@{b:.1f}") for e, b, *_ in out.sites)
    fig.suptitle(f"UO$_2$ ring, {len(out.ring.widths)} grains, rotation = {out.rotation}, "
                 f"c3 = {p.c3:.3f}, tau_hat = {p.tau_hat:g}: {events}", fontsize=9)
    _save(fig, path)


# ###########################################################################
#   PART 4 - CHECKS AND MAIN
# ###########################################################################

def selftest():
    """Fast checks against [T26] (Cu) and the [Z21] fit (UO2).  True if all pass."""
    ok = True

    def check(name, cond, detail):
        nonlocal ok
        ok &= bool(cond)
        print(f"[{'PASS' if cond else 'FAIL'}] {name}: {detail}")

    eq = eta_equilibrium(CU_RHO0, CU)
    check("Eq. (42), Cu", abs(eq - 0.579) < 1e-3, f"eta_eq = {eq:.4f} (paper 0.579)")

    g, _ = coupling_g(np.linspace(0, ETA_CUTOFF, 1001), CU.c, g_offset(CU.c))
    check("Eq. (36): min g = 0.01 [E2]", abs(g.min() - 0.01) < 1e-4, f"min g = {g.min():.4f}")

    # Sec. 3.2.1: without recovery the GB only widens, to eta_eq
    no_rec = run_case(replace(CU, c_d=0.0), CU_DELTA_THETA, CU_RHO0, grid=CU_GRID)
    eta_c = no_rec.solution.y[CU_GRID.cells // 2, -1]
    check("Cu 15 deg, C_D = 0: GB widens to eta_eq (Sec. 3.2.1)",
          abs(eta_c - eq) < 0.03 and not no_rec.nucleated,
          f"eta(centre) = {eta_c:.3f}, event = {no_rec.event}")

    gamma = gb_energy(CU, CU_GRID, no_rec.initial)
    check("Cu 15 deg GB energy vs Fig. 2", 0.6 < gamma < 0.9,
          f"gamma = {gamma:.3f} J/m^2 (Fig. 2: ~0.75)")

    # Sec. 3.2.2 and Fig. 5.  2.5 deg: paper "never recovered", here "collapse" [E3]
    for angle, expected, paper in [(15.0, True, "nucleus at the mean orientation"),
                                   (10.0, True, "immediate"),
                                   (5.0, True, "delayed, ~4e3 s"),
                                   (2.5, False, "never")]:
        out = run_case(CU, angle, CU_RHO0, grid=CU_GRID)
        check(f"Cu {angle:g} deg: nucleus = {expected} (paper: {paper})",
              out.nucleated == expected,
              f"{out.event}, t_recovery = {out.t_recovery:g} s, "
              f"theta_centre = {out.theta_centre_deg:.2f} deg")

    # [U5]: gamma of the UO2 parameters on the [Z21] random average (values frozen
    # here from uo2_gb_energy.target); tolerance = the fit residual
    uo2 = replace(CU, f0=UO2_F0, c=UO2_C, nu=UO2_NU)
    for angle, expected in [(5.0, 0.797), (15.0, 1.368), (30.0, 1.531)]:
        gamma = gb_energy(uo2, Grid(), initial_state(uo2, Grid(), angle))
        check(f"UO2 {angle:g} deg GB energy vs [Z21]", abs(gamma - expected) < 0.05,
              f"gamma = {gamma:.3f} J/m^2 (target {expected:.3f})")
    return ok


def main():
    os.makedirs(FIGURES_DIR, exist_ok=True)

    print("\n=== Cu: checks against [T26] ===")
    ok = selftest()

    print("\n=== Cu: figures of [T26] ===")
    plot_state_functions(f"{FIGURES_DIR}/tandogan_cu_fig1_state_functions.png")
    plot_gb_energy(CU, f"{FIGURES_DIR}/tandogan_cu_fig2.png", grid=CU_GRID)
    print("Fig. 4")
    plot_profiles(CU, [(r"$\Delta\theta$ = 15$^\circ$", CU_DELTA_THETA, CU_RHO0)],
                  [0.0, 1e1, 1e2, 1e3, 2e3, 5e3, 1e4], f"{FIGURES_DIR}/tandogan_cu_fig4.png",
                  r"Fig. 4, Cu, $\rho_0 = 2.5\times10^{15}$ m$^{-2}$, $C_D$ = 100.")
    print("Fig. 5: misorientation")
    plot_profiles(CU, [(rf"$\Delta\theta$ = {a:g}$^\circ$", a, CU_RHO0)
                       for a in (2.5, 5.0, 10.0, 15.0, 20.0)],
                  [0.0, 2.0e3, 4.0e3, 1.0e4], f"{FIGURES_DIR}/tandogan_cu_fig5.png",
                  r"Fig. 5, Cu, $\rho_0 = 2.5\times10^{15}$ m$^{-2}$.")
    print("Fig. 6: rho left/right of the GB")
    pairs = [(0.0, 2.5), (1.0, 2.5), (1.5, 2.5), (2.0, 2.5), (2.3, 2.5), (2.5, 2.5), (2.5, 2.0)]
    plot_profiles(CU, [(rf"$\rho_1, \rho_2$ = {a:g}, {b:g}", CU_DELTA_THETA, (a * 1e15, b * 1e15))
                       for a, b in pairs],
                  [0.0, 3.0e3, 1.0e4], f"{FIGURES_DIR}/tandogan_cu_fig6.png",
                  r"Fig. 6, Cu, $\Delta\theta = 15^\circ$, $\rho$ in $10^{15}$ m$^{-2}$.")

    p = uo2_parameters(TEMPERATURE)
    print(f"\n=== UO2: GB energy, f0 = {p.f0 / 1e3:.0f} kPa, c = {p.c:g}, "
          f"nu = {p.nu * 1e6:g} um ===")
    plot_gb_energy(p, f"{FIGURES_DIR}/tandogan_uo2_gb_energy.png",
                   angles=FIT_ANGLES, targets=uo2_gamma_targets())

    if CALIBRATE_C3:
        print(f"\n=== UO2: c3 for onset at {HBS_ONSET_BURNUP:g} GWd/tU, "
              f"Delta theta = {GRAIN_MISORIENTATION_DEG:g} deg ===")
        p = replace(p, c3=calibrate_c3(p, GRAIN_MISORIENTATION_DEG, HBS_ONSET_BURNUP,
                                       BU_RANGE[0], BU_RATE, TEMPERATURE, RHO_KIND))
        print(f"  -> copy this into UO2_C3: {p.c3:.3f}")

    print("\n=== UO2: what the c3 in use implies (Eq. 42) ===")
    relaxed = initial_state(p, Grid(), GRAIN_MISORIENTATION_DEG)
    eta_gb0 = float(relaxed[:Grid().cells].min())
    eq = [eta_equilibrium(dislocation_density(b), p) for b in (60.0, 80.0, 110.0)]
    print(f"Delta theta = {GRAIN_MISORIENTATION_DEG:g} deg: eta_GB0 = {eta_gb0:.3f}, "
          f"gamma = {gb_energy(p, Grid(), relaxed):.3f} J/m^2")
    print(f"{'c3':>7} {'eta_eq(60)':>10} {'eta_eq(80)':>10} {'eta_eq(110)':>11} "
          f"{'bu(eta_GB0)':>11} {'bu(eta=0)':>9}")
    print(f"{p.c3:7.3f} {eq[0]:10.3f} {eq[1]:10.3f} {eq[2]:11.3f} "
          f"{burnup_at_eta_eq(eta_gb0, p):11.1f} {burnup_at_eta_eq(0.0, p):9.1f}")
    plot_widening_depth(p, eta_gb0, f"{FIGURES_DIR}/tandogan_uo2_c3_rules.png")

    print("\n=== UO2: irradiation histories ===")
    outcomes = irradiation_scan(p, IRRADIATION_ANGLES, *BU_RANGE, BU_RATE, TEMPERATURE,
                                RHO_KIND, f"{FIGURES_DIR}/tandogan_uo2_irradiation_{RHO_KIND}.png",
                                SNAPSHOT_BURNUPS,
                                f"{FIGURES_DIR}/tandogan_uo2_snapshots_{{angle:g}}deg.png")
    plot_diagnostics(p, outcomes, f"{FIGURES_DIR}/tandogan_uo2_diagnostics.png")

    print(f"\n=== UO2: polycrystal ring (Part 3b), source in swept material = "
          f"{SOURCE_IN_SWEPT:g} [U12], Landau rotation = {LANDAU_ROTATION} [U13] ===")
    for width in RING_GRAIN_WIDTHS:
        ring = make_ring(N_GRAINS, width, RING_SEED)
        out = run_ring(p, ring, *BU_RANGE, BU_RATE, TEMPERATURE, RHO_KIND,
                       rotation=LANDAU_ROTATION)
        print_ring(out, f"{width * 1e6:g} um grains")
        plot_ring(p, out, f"{FIGURES_DIR}/tandogan_uo2_ring_{width * 1e6:g}um.png")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
