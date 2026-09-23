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
  TODO: Implement the effect of polygonisation on the phase field, 
  ensuring that the transfer of dislocations from rho_free to rho_wall is 
  correctly accounted for in the evolution equations. Polygonisation (3.6) 
  adds one: option A, a plastic rotation source, is implemented there 
  but does not polygonise yet (see its STATUS).
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

3.6 Polygonisation as a plastic rotation source (option A)
----------------------------------------------------------

STATUS  IMPLEMENTED, OFF BY DEFAULT (POLYGONISATION_YEARS = None), because
        it does not polygonise yet.  The equations are verified - M1 of
        verify_mathematics() finds A_k of (A7) equal to -dPsi per dislocation
        moved to 1e-16 in every subgrain - but the dynamics shows that the
        HMP energy of [T26] does not let a wall live inside a grain:

        * Hard grain (c4 = 0, eta = 1 in the bulk).  Turning an interior
          subgrain by 1e-13 rad already costs more than the whole tangle
          (g = 1e12), so A_k < 0 at once, and nothing lowers eta there:
          g' = 0 above the cutoff and phi' = 5e-5.  R flips on and off over
          1e-13 rad and the BDF matrix goes singular.  The two-reservoir
          version failed on the same fact (a wall capacity of 1.6e12 m^-2 in
          the grains): it is the functional, not the bookkeeping.
        * Soft grain (the plateau c4 of phi', below).  The walls do take
          dislocations (22% of the tangle in a year at c4 = 0.65, i.e. ~10 deg
          by Frank's rule), but the lattice keeps only ~0.01 deg of it: in a
          region of uniform eta, e* relaxes theta like a diffusion with
          D_theta = mu^2 / (tau_hat t0) ~ 2e-17 m^2/s, which flattens a 1.56 um
          subgrain in ~1.4 days (M4 of verify_mathematics() fails: 58% of the
          cells turn the way s(x) says, i.e. chance).  Table 1 of [T26] has no
          slower tau_hat (1e2 f0 t0 at most).  And once eta_bulk falls below
          c2 the c3 step switches on in the bulk too, the grain drops to the GB
          value (0.72) and the GB/bulk contrast that nucleation needs is gone.

        What does make walls: a slower reorientation.  One year at rho(60),
        no source, in a domain WITHOUT the original GB (all of it grain):

            c4    tau_hat [f0 t0]  eta grain  lattice step between subgrains
            0     1e4              1          (did not finish in 700 s)
            0.1   1e4              0.984      0.001 deg
            0.3   1e1              0.770      0.005 deg   (ledger 11 deg)
            0.3   1e4              0.770      2.7 deg, up to 4.3, eta dips 0.74
            0.65  1e4              0.716      3.5 deg, up to 5.0

        Lowering nu to 1 um changes none of this (it only narrows the dips).
        Two consequences.  Walls appear only once the grain has FLIPPED to the
        disordered branch eta ~ 1 - (c3 + c4) lambda/2 G b^2 rho/(f0 alpha): as
        soon as eta_bulk drops below ~c2 the c3 step switches on in the bulk as
        well (with c4 = 0.3 near rho ~ 1e15, ~58 GWd/tU).  After the flip the
        grain is GB-like, so the eta > c2 tests (grains, nucleus criterion)
        stop meaning what they say.  And a full irradiation history with
        polygonisation on does not integrate in reasonable time: below the
        flip the grain is hard and R switches on and off over 1e-13 rad; the
        run was still below 20 GWd/tU after 10 minutes, and a smoothed switch
        (AFFINITY_SMOOTHING at 1% of mu_f) did not help.  (Earlier probes run
        with C_D = 0 looked better only because the original GB had widened
        across the whole domain - [T26] Sec. 3.2.1 - making it all GB-like.)

        In HMP a theta step survives only where an eta dip holds it, and the
        dip only forms where eta is already low.

        The previous two-reservoir version (rho_wall with its own energy
        f0 mu^2 g b^2 rho_wall^2, the 1/g mobility, ETA_WALL_CUTOFF and the
        Theta_HAGB ceiling) is removed.

Idea.  Polygonisation takes dislocations out of the tangle and turns them
into lattice ROTATION; the phase field then charges the boundaries that
rotation creates with its own gamma(Delta theta).  No wall energy is added.

Fields: eta, theta, rho (the tangle, rho_free), rho_w (a LEDGER of the
dislocations stored in walls: no energy of its own).

Energy, Eq. (15) of [T26] unchanged:

    psi = f0 [ alpha V + nu^2/2 (dx eta)^2 + mu^2 g(eta) (dx theta)^2 ]
        + phi(eta) lambda/2 G b^2 rho                                     (A1)

A wall costs the mu^2 g theta'^2 of the boundary it produces, i.e. the
Read-Shockley energy fitted on [Z21] in 3.5: nothing is counted twice.

Kinematics.  Eq. (26) with u = 0 and mu_c -> infinity (e^e = 0) gives
theta = -e^slip - e*.  Slip is the only channel [T26] has for NEW rotation;
polygonisation is fed into it:

    d e^slip/dt = -W(x, t)                                                (A2)
    tau_hat g(eta) d e*/dt = -f0 dx[ mu^2 g dx theta ]      Eq. (34)      (A3)
    =>  tau_hat g(eta) (d theta/dt - W) = f0 dx[ mu^2 g(eta) dx theta ]   (A4)

In the bulk (g large) theta follows W; inside a boundary e* relaxes it.
W = 0 is the present model exactly.

Subgrains.  The domain is cut into subgrains Z_k of width D (2 r_n at the
onset burnup), with an edge on the original GB at L/2 (subgrain_pattern(),
sign only), s_k = +-1 alternating.  Each subgrain rotates RIGIDLY:

    W(x, t) = 1/2 s_k b D Rbar_k(t),   x in Z_k                           (A5)
    Rbar_k  = (1/D) int_{Z_k} R dx

Neighbours turn opposite ways, so the wall between k and k + 1 gains
misorientation at d Theta/dt = 1/2 b D (Rbar_k + Rbar_k+1): Frank's rule
Theta = b rho_w D, each wall fed by its two half subgrains.  The rotation is
concentrated at the zone edges and the interiors stay unbent - polygonisation,
not bending - and HMP localises each jump into a LAGB through
f_eta3 = -f0 mu^2 g' theta'^2.

Transfer rate and its driving force, exact (variational):

    R(x, t) = M rho(x) < A_k >+,   x in Z_k                               (A6)
    A_k = mubar_f,k + b f0 s_k [ mu^2 g dx theta ]_{a_k}^{b_k}            (A7)
    mubar_f,k = int_{Z_k} rho phi(eta) lambda/2 G b^2 dx / int_{Z_k} rho dx

(the rho-weighted mean: with R proportional to rho it is what makes A_k
exactly -dPsi per unit length of dislocation moved; a plain mean is not)

A_k [J/m] is the energy released per unit length of dislocation moved from
the tangle into the walls of Z_k: the stored energy of the tangle, minus the
work of turning Z_k against its two walls.  The second term is the flux
mu^2 g theta' at the two edge faces a_k, b_k of the zone - the same `flux`
array the theta equation builds.  Derivation: W is uniform on Z_k, so
int_{Z_k} (delta psi/delta theta) W dx, with delta psi/delta theta =
-2 f0 dx(mu^2 g theta'), is a boundary term; discretely the sum telescopes, so
(A7) is exact on the grid too.  Sign check: once Z_k (s_k = +1) stands above
both neighbours, theta' > 0 at a_k and < 0 at b_k, the bracket is negative and
the walls resist further rotation.

Softening of the grain (optional, c4 = 0 by default = [T26]).  phi' gets a
bulk plateau next to its GB step,

    phi'(eta) = c3/2 [1 - tanh(c1 (eta - c2))] + c4                      (A11)

so the tangle lowers eta in the grain to eta_bulk = 1 - c4 lambda/2 G b^2
rho / (f0 alpha) as long as eta_bulk > c2, and Eq. (42) becomes
eta_eq = 1 - (c3 + c4) lambda/2 G b^2 rho / (f0 alpha).  It is what makes
walls affordable in the grain at all; see the STATUS for what it costs.

Dislocation balances:

    d rho/dt   = S(bu) - rho C_D A(eta) <d eta/dt> - R                    (A8)
    d rho_w/dt =       - rho_w C_D A(eta) <d eta/dt> + R                  (A9)
    eta: Eq. (32) unchanged, f_eta4 = -phi'(eta) lambda/2 G b^2 rho       (A10)

Recovery (Eq. 28) removes wall dislocations too, as it does every SSD in
[T26] (the previous version recovered rho_free only).  A nucleus sweeping a
region takes its walls with it; there rho -> 0, so R -> 0 and W -> 0.
Correspondence with [HBS]: rho <-> rho_free, rho_w <-> rho_ord, recovered
<-> rho_swept (plot_partition()).

Guarantees.  Without source and recovery rho + rho_w is conserved.  The
transfer dissipates sum_k D Rbar_k A_k >= 0 (Macaulay bracket), so with (A3)
and (A10) Psi never increases apart from the source.  verify_mathematics()
M2-M4 carry over; M1 then checks (A7) against a complex-step dPsi/dW.
Removed with respect to the previous version: ETA_WALL_CUTOFF, the 1/g
mobility, the Theta_HAGB ceiling, the rho_wall energy, the signed rate.  A wall
grown past ~10-15 deg simply IS a HAGB under gamma(Delta theta): that is the
HBS subdivision, not something to cap.  Still imposed: s(x) and D ([G03],
limitation 9); one new parameter, M.

Open points for the review:

  1. Subgrain size vs boundary width (the main one).  At nu = 2.5 um a ~5 deg
     boundary has a theta transition ~0.83 um wide (NUMERICS) and a ~3 deg one
     is wider still, against D = 1.56 um: neighbouring walls overlap, and HBS
     subgrains (r_n ~ 0.1-0.3 um at saturation) are far below what this nu
     resolves.  Either accept a qualitative model at D = 2 r_n(onset), or lower
     nu - refitting f0 and c (3.5) and bringing L back towards ~10 nu.
  2. Irreversibility.  <A>+ never gives dislocations back to the tangle; only
     recovery removes walls.  Alternative: a signed rate drawing on rho_w when
     A < 0.
  3. Calibration of M: on [HBS] Theta(bu) (Theta_w = b rho_w D) or on rho_ord
     at one burnup, or from a transfer time (mobility_from_timescale()).
  4. Rigid subgrains are non-local: W on Z_k depends on all of Z_k through
     Rbar_k.  The Jacobian gains one D x D block per subgrain (16 blocks of
     ~156 cells): still sparse, no longer banded.
  5. c3 in the driving force: mu_f carries phi(eta), whose bulk value is
     c2 c3 lambda/2 G b^2 (limitation 9), so polygonisation depends on the
     nucleation calibration.
  6. The ledger has no stored energy: wall dislocations do not push eta down
     through f_eta4, only through theta' (f_eta3).  Consistent with (A1), but
     a choice.

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
6. Irradiation protocol: the result depends on BU_RATE * t0 and t0 is not 
   calibrated for UO2; no recovery other than Eq. (28).
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
9. [Option A of 3.6 keeps s(x) and D as inputs and adds M; it has no
   ceiling and no 1/g mobility.]
   Polygonisation (3.6) gives the magnitude of the misorientation, not its
   SIGN [the rest of this item describes the removed two-reservoir version
   and is kept for the record]: s(x) is imposed on the subgrain pattern, because a scalar rho_free
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
from types import SimpleNamespace

import numpy as np
from scipy.integrate import solve_ivp
from scipy.sparse import coo_matrix, diags, kron

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
    c4: float = 0.0                 # -      bulk plateau of phi' (docstring 3.6): the
                                    #        tangle softens the grain.  0 = [T26]
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
    return 0.5 * p.c3 * ((eta - log_cosh / p.c1) + log_cosh0 / p.c1) + p.c4 * eta


def phi_derivative(eta, p):
    """phi'(eta), Eq. (38): ~c3 in the GB, ~0 in the bulk.

    This is what confines the stored-energy force f_eta4 to the GB.
    """
    return 0.5 * p.c3 * (1.0 - np.tanh(p.c1 * (eta - p.c2))) + p.c4


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
    return 1.0 - (p.c3 + p.c4) * p.stored_energy_per_rho * rho / (p.f0 * p.alpha)


# ---------------------------------------------------------------------------
# A.7  Polygonisation: tangle -> rigid subgrain rotation (docstring 3.6, option A)
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Polygonisation:
    """The transfer of docstring 3.6: tangle -> walls -> rotation of the subgrains.

    mobility  M of Eq. (A6) [m J^-1 s^-1].  0 switches polygonisation off and
              the model is exactly [T26]; mobility_from_timescale() sets it.
    sign      s(x), array (n) of +-1.  The subgrains Z_k are the runs of
              constant sign, and s_k is the way Z_k turns (Eq. A5).  The one
              ingredient that does NOT come from the free energy (limitation 9).
    subgrain  D [m], for the post-processing only (Theta_w = b rho_w D): the
              rotation uses the actual width of every zone, so the two partial
              zones at the walls are handled exactly.
    """

    mobility: float = 0.0
    sign: object = None
    subgrain: float = 1.0e-6


def subgrain_zones(sign):
    """(zone of every cell, first cell of every zone + [n], number of zones).

    Zone k spans the cells first[k] .. first[k + 1] - 1, so its two edges are
    the faces first[k] (a_k) and first[k + 1] (b_k) of the n + 1 face array -
    the two faces whose flux enters Eq. (A7).
    """
    sign = np.asarray(sign)
    change = np.flatnonzero(np.diff(sign) != 0) + 1
    first = np.concatenate([[0], change, [sign.size]])
    zone = np.repeat(np.arange(first.size - 1), np.diff(first))
    return zone, first, first.size - 1


def tangle_potential(eta, p):
    """mu_f = d psi / d rho [J/m], energy per unit length of a tangle dislocation.

    The last term of Eq. (15) with r^2 = b^2 rho and h = 1, differentiated
    with respect to rho: phi(eta) lambda/2 G b^2.
    """
    return coupling_phi(eta, p) * p.stored_energy_per_rho


def polygonisation_affinity(eta, rho, flux, sign, zones, p):
    """A_k [J/m] of Eq. (A7), one value per subgrain.

        A_k = mubar_f,k + b f0 s_k (flux_{b_k} - flux_{a_k})

    flux = mu^2 g d theta/dx on the n + 1 faces: the array the theta equation
    builds, zero on the two walls.  The first term is what a dislocation leaving
    the tangle releases, the second the work of turning Z_k against its two
    walls.  mubar_f,k is the RHO-WEIGHTED mean of mu_f over Z_k: with R
    proportional to rho that makes A_k exactly -dPsi per unit length of
    dislocation moved (verify_mathematics() M1), where a plain mean would not be
    wherever rho or eta vary inside a subgrain.
    """
    zone, first, count = zones
    mu_f = tangle_potential(eta, p)
    weight = np.bincount(zone, rho, count)
    plain = np.bincount(zone, mu_f, count) / np.diff(first)
    weighted = np.bincount(zone, rho * mu_f, count) / np.where(weight > 0.0, weight, 1.0)
    s_k = np.asarray(sign, dtype=float)[first[:-1]]
    return (np.where(weight > 0.0, weighted, plain)
            + p.burgers * p.f0 * s_k * (flux[first[1:]] - flux[first[:-1]]))


def mobility_from_timescale(years, p):
    """M giving a transfer time `years` for the bulk tangle of a free subgrain.

    With A_k = mu_f (no wall has formed yet, eta = 1), Eq. (A6) empties the
    tangle at the rate R / rho = M mu_f, so M = 1 / (tau mu_f(1)).  M is not
    measured: choosing it from a time is the honest way to set it
    (limitation 9).
    """
    return 1.0 / (years * SECONDS_PER_YEAR * float(tangle_potential(np.array([1.0]), p)[0]))


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
    """d/dt of the state y = [eta (n), theta (n), rho (n), rho_w (n)].

    rho is the tangle (rho_free), rho_w the ledger of the dislocations stored
    in walls (docstring 3.6), which carries no energy of its own.

    source(t) [m^-2/s]: dislocation production, uniform in x - irradiation makes
    dislocations everywhere in the grain, so S has no shape at all (docstring
    3.2).  None means no production, as in [T26] after loading.

    polygonisation: a Polygonisation, docstring 3.6.  None (or mobility 0) is
    exactly [T26]: W = 0, R = 0, and rho_w stays 0.
    """
    n, dx = grid.cells, grid.dx
    tau_eta = p.tau_eta * p.f0 * p.t0           # J s m^-3
    tau_hat = p.tau_hat * p.f0 * p.t0           # J s m^-3
    energy = p.stored_energy_per_rho

    active = polygonisation is not None and polygonisation.mobility > 0.0
    if active:
        sign = np.asarray(polygonisation.sign, dtype=float)
        zones = subgrain_zones(sign)
        zone, _, count = zones

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

        # Eq. (34): tau_hat g theta_dot = f0 d/dx [ mu^2 g d theta/dx ]
        flux = p.mu ** 2 * (g_face * dtheta)
        theta_dot = p.f0 * np.diff(flux) / dx / (tau_hat * g)

        # Eq. (32): order parameter, forces of Eq. (41)
        # (f_eta2 and f_eta3 kept grouped: the irradiation protocol is
        #  sensitive to the round-off of this sum)
        curvature_sq = 0.5 * (dtheta[:-1] ** 2 + dtheta[1:] ** 2)    # at cell centres
        f_eta1 = p.f0 * p.nu ** 2 * np.diff(deta) / dx               # gradient
        f_eta23 = p.f0 * (p.alpha * potential_derivative(eta)        # potential
                          + p.mu ** 2 * dg * curvature_sq)           # orientation
        f_eta4 = phi_derivative(eta, p) * energy * rho               # stored dislocations
        eta_dot = (f_eta1 - f_eta23 - f_eta4) / tau_eta

        # Eq. (28): recovery, active only where eta grows (<.> Macaulay
        # bracket).  It removes every SSD, the ones stored in walls too (A9).
        rho_dot = -rho * p.c_d * recovery_localiser(eta) * np.maximum(eta_dot, 0.0)
        rho_wall_dot = np.zeros(n)
        if active:
            # only when polygonisation is on: with it off rho_w is identically
            # 0, and a term in it lets the finite-difference Jacobian grow its
            # step on that column until it overflows (singular BDF matrix)
            rho_wall_dot = -rho_wall * p.c_d * recovery_localiser(eta) \
                * np.maximum(eta_dot, 0.0)

        # >>> POLYGONISATION (docstring 3.6).  The tangle of subgrain Z_k goes
        #     into its walls at R = M rho <A_k>+ (A6)-(A7), and Z_k turns
        #     rigidly at W = 1/2 s_k b int_{Z_k} R dx (A5), which Eq. (A4) adds
        #     to theta_dot: the e^slip channel of Eq. (26).
        if active:
            affinity = polygonisation_affinity(eta, rho, flux, sign, zones, p)
            if AFFINITY_SMOOTHING > 0.0:
                # <A>+ with the kink at 0 rounded off: A^2 / (A + eps) for A > 0,
                # still exactly 0 for A <= 0, so R A >= 0 is untouched
                positive = np.maximum(affinity, 0.0)
                positive = positive ** 2 / (positive + AFFINITY_SMOOTHING)
            else:
                positive = np.maximum(affinity, 0.0)
            transfer = polygonisation.mobility * np.maximum(rho, 0.0) * positive[zone]
            rho_dot = rho_dot - transfer
            rho_wall_dot = rho_wall_dot + transfer
            theta_dot = theta_dot + 0.5 * sign * p.burgers * dx \
                * np.bincount(zone, transfer, count)[zone]

        # >>> DISLOCATION SOURCE: irradiation production (UO2, Part C.2).
        #     Replaces the Kocks-Mecking term of Eq. (28); uniform in x.
        if source is not None:
            rho_dot = rho_dot + source(t)

        return np.concatenate([eta_dot, theta_dot, rho_dot, rho_wall_dot])

    return rhs


def _sparsity(n, sign=None):
    """Jacobian pattern: nearest-neighbour coupling within and between the four
    fields [eta, theta, rho, rho_w], plus one block per subgrain when the
    polygonisation of 3.6 is on.

    The blocks are the price of rigid subgrains (review point 4 of 3.6): R and
    W on Z_k depend on eta and rho over the whole of Z_k through A_k, and on
    theta and eta one cell beyond each edge through the edge fluxes.  Still
    sparse, no longer banded.

    Kept SPARSE: the dense 4n x 4n pattern is 128 MB at the default grid and it
    is rebuilt at every evolve() call, which is what made the gamma fit of 3.5
    grow to several GB.
    """
    band = diags([1.0, 1.0, 1.0], [-1, 0, 1], shape=(n, n))
    pattern = kron(np.ones((4, 4)), band, format="csr")
    if sign is None:
        return pattern
    _, first, count = subgrain_zones(sign)
    rows, cols = [], []
    for k in range(count):
        cells = np.arange(first[k], first[k + 1])
        near = np.arange(max(first[k] - 1, 0), min(first[k + 1] + 1, n))
        rows.append(np.repeat(cells, near.size))
        cols.append(np.tile(near, cells.size))
    block = coo_matrix((np.ones(sum(r.size for r in rows)),
                        (np.concatenate(rows), np.concatenate(cols))), shape=(n, n))
    return (pattern + kron(np.ones((4, 4)), block, format="csr")).sign()


def evolve(p, grid, y0, t_end, times, source=None, max_step=np.inf,
           polygonisation=None, events=None):
    """Integrate from 0 to t_end, output at `times` (stiff BDF, adaptive).

    events: passed to solve_ivp (run_irradiation uses one to switch the
    polygonisation on when the grain flips, docstring 3.6)."""
    rhs = _right_hand_side(p, grid, g_offset(p.c), source, polygonisation)
    active = polygonisation is not None and polygonisation.mobility > 0.0
    sparsity = _sparsity(grid.cells, polygonisation.sign if active else None)
    sol = solve_ivp(rhs, (0.0, t_end), y0, method="BDF", t_eval=times, max_step=max_step,
                    jac_sparsity=sparsity, rtol=1e-5, atol=1e-8, events=events)
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
    key = (p.f0, p.nu, p.alpha, p.mu, p.c, p.t0,
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
        # both mobilities at the fast values of Table 1: the relaxed profile
        # does not depend on them, and the slow UO2 tau_hat of 3.6 would leave
        # theta unrelaxed in t_relax
        relax = evolve(replace(p, tau_eta=1.0e2, tau_hat=1.0e1), grid, y0, t_relax, [t_relax])
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

def total_energy(p, grid, y, face="harmonic"):
    """Psi [J/m^2]: Eq. (15) of [T26], i.e. (A1) of docstring 3.6.

        Psi = int [ f0 (alpha V + nu^2/2 (d eta/dx)^2 + mu^2 g(eta) (d theta/dx)^2)
                    + phi(eta) lambda/2 G b^2 rho ] dx

    The ledger rho_w does not appear: a wall costs the mu^2 g theta'^2 of the
    boundary it produces.  gb_energy() is the same thing without dislocations;
    this one is what the dynamics has to decrease.

    face: how g is averaged at the faces of the orientation term.  The theta
    flux and gb_energy() use the HARMONIC mean, the eta force is written as if
    each cell carried the mean of its two faces (ARITHMETIC).  The two agree to
    O(dx) - verify_mathematics() measures it - and both are offered so the
    comparison can be made.

    Accepts a complex state: every expression here is analytic, which is what
    makes the complex step of _energy_gradient() possible.
    """
    n, dx = grid.cells, grid.dx
    eta, theta, rho = y[:n], y[n:2 * n], y[2 * n:3 * n]
    offset = g_offset(p.c)
    if np.iscomplexobj(y):
        g = _g_raw(eta, p.c) + offset            # below the cutoff by construction
    else:
        g, _ = coupling_g(eta, p.c, offset)
    cell = (p.f0 * p.alpha * 0.5 * (1.0 - eta) ** 2
            + coupling_phi(eta, p) * p.stored_energy_per_rho * rho)
    if face == "harmonic":
        g_face = 2.0 * g[:-1] * g[1:] / (g[:-1] + g[1:])
    else:
        g_face = 0.5 * (g[:-1] + g[1:])
    faces = p.f0 * (0.5 * p.nu ** 2 * (np.diff(eta) / dx) ** 2
                    + p.mu ** 2 * g_face * (np.diff(theta) / dx) ** 2)
    return (cell.sum() + faces.sum()) * dx


_FIELDS = {"eta": 0, "theta": 1, "rho": 2}


def _energy_gradient(p, grid, y, field, face, step=1e-30):
    """d Psi / d y_i per unit volume, by the COMPLEX STEP.

    Im[Psi(y + i h e_k)] / h is exact to machine precision: there is no
    difference of two nearly equal numbers, which matters because g reaches
    1e12 at the cutoff - a finite difference of Psi loses every digit of the
    small terms against it.
    """
    n = grid.cells
    start = _FIELDS[field] * n
    out = np.zeros(n)
    base = y.astype(complex)
    for i in range(n):
        probe = base.copy()
        probe[start + i] += 1j * step
        out[i] = total_energy(p, grid, probe, face).imag / step
    return out / grid.dx


def _test_state(grid, seed=3):
    """A smooth non-equilibrium state with all four fields switched on.

    eta is kept inside [0.35, 0.85], below the cutoff of g, so that the complex
    step sees the analytic g everywhere.
    """
    x = grid.x / grid.length
    return np.concatenate([
        0.6 + 0.25 * np.cos(2.0 * np.pi * x),
        math.radians(20.0) * 0.5 * (1.0 + np.tanh((x - 0.5) / 0.05)),
        5.0e14 * (1.0 + 0.3 * np.sin(4.0 * np.pi * x)),
        2.0e13 * (1.0 + 0.5 * np.cos(6.0 * np.pi * x)) ** 2])


def _relaxation(p, grid, polygonisation, rho0=5.0e14, c_d=None, samples=40):
    """Relaxed GB loaded with a uniform rho, then left alone: no source.

    eta is first let settle under rho with polygonisation off, then the
    transfer is switched on.  Switched on at once, the walls start in a bulk at
    eta = 1, where turning a subgrain by 1e-13 rad already costs more than the
    whole tangle (g = 1e12): R flips on and off over that range and the BDF
    Jacobian goes singular.  That is the physics of docstring 3.6, not a test.
    """
    n = grid.cells
    p = p if c_d is None else replace(p, c_d=c_d)
    y = initial_state(p, grid, 20.0)
    y[2 * n:3 * n] = rho0
    y = evolve(p, grid, y, 1.0e3 * p.t0, [1.0e3 * p.t0]).y[:, -1]
    t_end = 1.0e3 * p.t0
    return p, evolve(p, grid, y, t_end, np.linspace(0.0, t_end, samples),
                     polygonisation=polygonisation)


def verify_mathematics(p=None, cells=(60, 120), verbose=True):
    """M1-M4: is the model the gradient flow it claims to be?

    Returns True if every check passes.  Nothing here uses a calibration, a
    burnup or an experimental number: it asks only whether the implemented
    right-hand side is the variational derivative of (A1), whether the
    polygonisation affinity (A7) is the energy it says it is, whether Psi
    decreases, and whether the transfer conserves dislocations.  A failure is a
    bug in the equations; a model that passes can still be the wrong model.
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
        n, dx = grid.cells, grid.dx
        sign = np.sign(subgrain_pattern(grid, subgrain, jitter=0.0))
        y = _test_state(grid)
        d = _right_hand_side(p, grid, g_offset(p.c))(0.0, y)
        g, _ = coupling_g(y[:n], p.c, g_offset(p.c))
        # the equations are tau * d(field)/dt = -d Psi / d field
        force_eta = -(p.tau_eta * p.f0 * p.t0) * d[:n]
        force_theta = -(p.tau_hat * p.f0 * p.t0) * g * d[n:2 * n]

        exact = _energy_gradient(p, grid, y, "eta", "arithmetic")
        check(f"M1 eta = dPsi/deta at {n_cells} cells",
              np.abs(exact - force_eta).max() / np.abs(exact).max() < 1e-12,
              f"{np.abs(exact - force_eta).max() / np.abs(exact).max():.1e}")

        # Eq. (34) of [T26] carries no factor 2 against Eq. (15): the theta
        # mobility is half the variational one, which is a convention, not an
        # error - the flow still descends the same Psi.
        exact = _energy_gradient(p, grid, y, "theta", "harmonic")
        check(f"M1 theta = dPsi/dtheta / 2 at {n_cells} cells",
              np.abs(0.5 * exact - force_theta).max() / np.abs(0.5 * exact).max() < 1e-12,
              f"{np.abs(0.5 * exact - force_theta).max() / np.abs(0.5 * exact).max():.1e} "
              f"(Eq. 34 convention)")

        exact = _energy_gradient(p, grid, y, "rho", "harmonic")
        mu_f = tangle_potential(y[:n], p)
        check(f"M1 mu_f at {n_cells} cells",
              np.abs(exact - mu_f).max() / np.abs(exact).max() < 1e-12,
              f"{np.abs(exact - mu_f).max() / np.abs(exact).max():.1e}")

        # (A7): move epsilon of the tangle of Z_k into its walls, exactly as
        # the rhs does (R proportional to rho, W uniform on Z_k), and compare
        # -dPsi/depsilon per unit length of dislocation moved with A_k
        zones = subgrain_zones(sign)
        zone, first, count = zones
        dtheta = np.zeros(n + 1)
        dtheta[1:-1] = np.diff(y[n:2 * n]) / dx
        g_face = np.zeros(n + 1)
        g_face[1:-1] = 2.0 * g[:-1] * g[1:] / (g[:-1] + g[1:])
        affinity = polygonisation_affinity(y[:n], y[2 * n:3 * n], p.mu ** 2 * g_face * dtheta,
                                           sign, zones, p)
        worst = 0.0
        for k in range(count):
            inside = zone == k
            rho_k = np.where(inside, y[2 * n:3 * n], 0.0)
            moved = rho_k.sum() * dx
            probe = y.astype(complex)
            probe[2 * n:3 * n] -= 1e-30j * rho_k
            probe[n:2 * n] += 1e-30j * 0.5 * sign * p.burgers * moved * inside
            exact_k = -total_energy(p, grid, probe).imag / 1e-30 / moved
            worst = max(worst, abs(exact_k - affinity[k]) / abs(affinity).max())
        check(f"M1 A_k = -dPsi per dislocation moved, {count} subgrains at {n_cells} cells",
              worst < 1e-10, f"{worst:.1e}")

        exact = _energy_gradient(p, grid, y, "eta", "harmonic")
        harmonic[n_cells] = np.abs(exact - force_eta).max() / np.abs(exact).max()

    # the eta force and the theta flux average g at the faces differently; the
    # difference has to vanish with dx, or the scheme is not consistent
    coarse, fine = cells[0], cells[-1]
    check("M1 arithmetic vs harmonic face average is O(dx)",
          harmonic[fine] < 0.75 * harmonic[coarse],
          f"{harmonic[coarse]:.1e} at {coarse} cells -> {harmonic[fine]:.1e} at {fine}")

    # --- M2 to M4: the dynamics --------------------------------------------
    # with the bulk softened (c4, docstring 3.6): with c4 = 0 the walls cannot
    # start in the grain at all, see _relaxation()
    p = replace(p, c4=0.65)
    grid = Grid(cells=cells[-1])
    n = grid.cells
    pp = Polygonisation(mobility=mobility_from_timescale(3.0, p),
                        sign=np.sign(subgrain_pattern(grid, subgrain, jitter=0.0)),
                        subgrain=subgrain)
    for tag, transfer in (("polygonisation off", None), ("polygonisation on", pp)):
        q, sol = _relaxation(p, grid, transfer, c_d=10.0)
        psi = np.array([total_energy(q, grid, sol.y[:, k]) for k in range(sol.t.size)])
        rise = float(np.diff(psi).max())
        check(f"M2 Psi does not increase, {tag}", rise <= 1e-12 * abs(psi[0]),
              f"{psi[0]:.4f} -> {psi[-1]:.4f} J/m^2, largest step {rise:+.1e}")

    q, sol = _relaxation(p, grid, pp, c_d=0.0)
    total = sol.y[2 * n:3 * n].sum(axis=0) + sol.y[3 * n:].sum(axis=0)
    moved = sol.y[3 * n:].sum(axis=0)[-1] / total[0]
    drift = abs(total[-1] - total[0]) / total[0]
    check("M3 the transfer conserves rho + rho_w",
          drift < 1e-10 and moved > 1e-3 and sol.y[3 * n:].min() >= 0.0,
          f"drift {drift:.1e}, {100 * moved:.1f}% moved into the walls, "
          f"min rho_w {sol.y[3 * n:].min():.1e}")

    # M4: the walls really turn the subgrains, in the sense s(x) asks for
    turned = sol.y[n:2 * n, -1] - sol.y[n:2 * n, 0]
    interior = np.abs(grid.x - 0.5 * grid.length) > 2.0e-6     # away from the old GB
    agree = float(np.mean(np.sign(turned[interior]) == pp.sign[interior]))
    check("M4 the subgrains turn the way s(x) says", agree > 0.95,
          f"{100 * agree:.0f}% of the cells, max |delta theta| = "
          f"{np.degrees(np.abs(turned).max()):.3f} deg")
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
CU_GRID = Grid(length=10.0e-6, cells=400)

FIT_ANGLES = (2.5, 5.0, 7.5, 10.0, 15.0, 20.0, 25.0, 30.0)



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
        nuc_mis = min(theta_nuc, delta_theta_deg - theta_nuc)
        return "nucleus", k, nuc_mis

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

def gamma_curve(p, angles, grid=None):
    """gamma(Delta theta) [J/m^2] of the relaxed 1D profiles, Fig. 2."""
    grid = grid or Grid()
    return np.array([gb_energy(p, grid, initial_state(p, grid, a)) for a in angles])


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
                   c_d=UO2_C_D, c3=UO2_C3, c4=UO2_C4, tau_hat=UO2_TAU_HAT, **changes)


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

    (f0, c) reproducing each target curve [J/m^2] at `angles`, docstring 3.5.

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
    keywords.setdefault("base", replace(CU, nu=UO2_NU))
    print(f"GB energy fit on [Z21] (Grid.cells = "
          f"{(keywords.get('grid') or Grid()).cells}, nu = {keywords['base'].nu * 1e6:g} um):")

    c_grid=(3.0, 5.0, 7.0, 9.0, 12.0, 16.0, 20.0, 26.0, 30.0, 34.0, 38.0, 45.0)


    targets = uo2_gamma_targets(angles, which)

    base = CU
    curves = {}
    for c in c_grid:
        curves[c] = gamma_curve(replace(base, c=c), angles, None)

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
    bu_flip: float = math.nan           # GWd/tU, grain flipped, polygonisation on (3.6)
    wall_misorientation_deg: np.ndarray = None   # b rho_w D (median), vs burnup (3.6)
    rho_wall_mean: np.ndarray = None    # m^-2, domain mean of the ledger rho_w, vs burnup
    lattice_misorientation_deg: np.ndarray = None   # median theta step between the
                                        #   centres of neighbouring subgrains, vs burnup
    subgrain_size: float = math.nan     # m, D of Frank's rule
    temperature: float = math.nan       # K
    which: str = "tot"                  # rho fed to the SSD term


def subgrain_pattern(grid, size, jitter=0.1, seed=0):
    """s(x): the way each subgrain turns, docstring 3.6 (callers take the sign).

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

    polygonisation: mobility M of docstring 3.6 [m J^-1 s^-1].  0 disables it
    and the model is exactly [T26]; otherwise the tangle of each subgrain goes
    into its walls, Eqs. (A6)-(A9), and the subgrain turns rigidly the way s(x)
    says, Eq. (A5).
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

    # >>> POLYGONISATION (docstring 3.6): subgrains of width D turning +-1.
    #     No jitter on the pattern: only its sign is used, and the source is
    #     strictly uniform too (limitation 8).
    transfer, sign = None, np.sign(subgrain_pattern(grid, subgrain_size, jitter=0.0))
    if polygonisation > 0.0:
        transfer = Polygonisation(mobility=polygonisation, sign=sign,
                                  subgrain=subgrain_size)

    def grain_flipped(t, y):
        """> 0 while both grain centres (the walls x = 0, L) are above c2."""
        return min(y[0], y[n - 1]) - p.c2
    grain_flipped.terminal, grain_flipped.direction = True, -1

    # >>> UO2 INPUT: fixed parent misorientation
    y = initial_state(p, grid, delta_theta_deg)
    t_end = (bu_end - bu_start) / bu_rate
    times = np.linspace(0.0, t_end, int(samples_per_gwd * (bu_end - bu_start)) + 1)
    sol = evolve(p, grid, y, t_end, times, source=source,
                 max_step=0.25 / bu_rate,        # do not step over the source
                 events=None if transfer is None else grain_flipped)

    # >>> POLYGONISATION, second stage (docstring 3.6).  While the grain is hard
    #     (eta above c2 at both grain centres) a subgrain cannot turn by more
    #     than ~1e-13 rad before A_k < 0, so R is zero to that precision - and
    #     integrating it is what makes the history crawl.  The first stage
    #     therefore runs without the transfer and stops when the grain flips;
    #     the second restarts from there with it.
    bu_flip = math.nan
    if transfer is not None and sol.t_events[0].size:
        t_flip = float(sol.t_events[0][0])
        bu_flip = bu_start + bu_rate * t_flip
        later = times[times > t_flip]
        rest = evolve(p, grid, sol.y_events[0][0], t_end - t_flip, later - t_flip,
                      source=lambda t: source(t + t_flip), max_step=0.25 / bu_rate,
                      polygonisation=transfer)
        sol = SimpleNamespace(t=np.concatenate([sol.t, rest.t + t_flip]),
                              y=np.hstack([sol.y, rest.y]))

    bu = bu_start + bu_rate * sol.t
    ref = np.interp(bu, bu_table, rho_table)
    rho_c = sol.y[2 * n + centre]
    eta_end = np.minimum(sol.y[0], sol.y[n - 1])
    # the event, and the burnup at which it happens, read off the profiles
    # exactly as [T26] read them off theirs: see classify_event()
    stats = np.array([grain_statistics(p, grid, sol.y[:, k], ref[k]) for k in range(bu.size)])
    event, k_event, misorientation = classify_event(p, grid, sol, ref, delta_theta_deg)
    bu_nuc = float(bu[k_event]) if k_event >= 0 else math.nan
    # >>> POLYGONISATION OUTPUT (3.6), two measures of the same thing:
    #     - what the ledger has put into the walls, Theta_w = b rho_w D (Frank),
    #       the median over the domain;
    #     - what the lattice actually shows: the theta step between the centres
    #       of neighbouring subgrains, the median over the walls, leaving out the
    #       pair across the original GB.  The two differ by whatever HMP has
    #       relaxed at the walls.

    # Theta_w = b rho_w D [deg], Frank's rule over one subgrain
    wall = np.degrees(p.burgers * np.abs(np.median(sol.y[3 * n:], axis=0)) * subgrain_size)

    _, first, _ = subgrain_zones(sign)
    middle = (first[:-1] + first[1:]) // 2
    steps = np.abs(np.diff(sol.y[n + middle], axis=0))
    keep = first[1:-1] != n // 2                     # not the pair across L/2
    lattice = np.degrees(np.median(steps[keep], axis=0)) if keep.any() \
        else np.zeros(bu.size)
    return IrradiationOutcome(delta_theta_deg, bu_nuc, event, misorientation, bu, rho_c,
                              sol.y[2 * n:3 * n].mean(axis=0), ref, sol.y[centre], eta_end,
                              np.degrees(sol.y[n + centre]), sol,
                              stats[:, 0], stats[:, 1].astype(int), stats[:, 2].astype(int),
                              bu_flip, wall, sol.y[3 * n:].mean(axis=0), lattice, subgrain_size,
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
        Theta(bu) of [HBS]; and the theta step the lattice actually shows.
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
            label=r"ledger: $b\,\rho_w D$ (median)")
    a4.plot(bu, out.lattice_misorientation_deg, "--", color="tab:red",
            label=r"lattice: $\theta$ step between subgrains (median)")
    a4.plot(bu, theta_l, "k", label=r"[HBS] $\Theta(bu)$")
    a4.axhline(THETA_HAGB, color="grey", lw=1.0, ls=":",
               label=rf"$\Theta_{{HAGB}}$ = {THETA_HAGB:g}$^\circ$ (reference)")
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


def plot_polygonisation(p, out, path, grid=None, snapshots=6):
    """Does the polygonisation of docstring 3.6 work?  One history, six panels.

    (a) eta(x, bu): walls should appear as thin dark lines at the subgrain
        edges (dotted), after the flip (dashed).
    (b) theta(x, bu) - theta(x, bu_flip): the rotation made since the flip.
        Polygonisation = each subgrain one flat colour block, the two
        neighbours of opposite sign; bending or smoothing = stripes that
        ignore the edges.
    (c), (d) theta(x) and eta(x) at a few burnups after the flip, edges dotted:
        a staircase in theta with an eta dip under every step.
    (e) misorientation against burnup: what the ledger rho_w has put into the
        walls (b rho_w D), what the lattice shows (theta step between the
        centres of neighbouring subgrains, median and max), [HBS] Theta.
    (f) eta in the grain (median), at the subgrain walls (min over the interior
        edges) and at the original GB; on the right axis the share of the
        dislocations sitting in walls, rho_w / (rho + rho_w), against [HBS]
        rho_ord / rho_tot.
    """
    plt = _pyplot()
    grid = grid or Grid()
    n, x = grid.cells, grid.x * 1e6
    bu, y = out.burnup, out.solution.y
    sign = np.sign(subgrain_pattern(grid, out.subgrain_size, jitter=0.0))
    _, first, _ = subgrain_zones(sign)
    edges = first[1:-1]
    interior = edges[edges != n // 2]
    middle = (first[:-1] + first[1:]) // 2
    flip = out.bu_flip
    k0 = 0 if math.isnan(flip) else int(np.searchsorted(bu, flip))
    theta = np.degrees(y[n:2 * n])
    tot, ord_, _, _, theta_l = landau_partition(bu, out.temperature)

    fig, axes = plt.subplots(3, 2, figsize=(15, 13))
    (a1, a2), (a3, a4), (a5, a6) = axes
    extent = (x[0], x[-1], bu[0], bu[-1])
    im = a1.imshow(y[:n].T, origin="lower", aspect="auto", extent=extent, cmap="viridis")
    fig.colorbar(im, ax=a1, label=r"$\eta$")
    a1.set_title(r"(a) $\eta(x, bu)$", fontsize=10)
    turned = theta - theta[:, [k0]]
    lim = max(float(np.abs(turned[:, k0:]).max()), 1e-6)
    im = a2.imshow(turned.T, origin="lower", aspect="auto", extent=extent,
                   cmap="RdBu_r", vmin=-lim, vmax=lim)
    fig.colorbar(im, ax=a2, label=r"$\theta - \theta(bu_{flip})$ [deg]")
    a2.set_title("(b) rotation since the flip: one flat block per subgrain = polygonisation",
                 fontsize=10)
    for a in (a1, a2):
        for e in edges:
            a.axvline(x[e], color="w", lw=0.4, ls=":")
        if not math.isnan(flip):
            a.axhline(flip, color="w", lw=1.0, ls="--")
        a.set_xlabel(r"$x$ [µm]")
        a.set_ylabel("burnup [GWd/tU]")

    picks = np.unique(np.linspace(k0, bu.size - 1, snapshots).astype(int))
    colours = plt.cm.viridis(np.linspace(0.0, 0.9, picks.size))
    for k, colour in zip(picks, colours):
        a3.plot(x, theta[:, k], color=colour, label=f"bu = {bu[k]:.1f}")
        a4.plot(x, y[:n, k], color=colour)
    for a in (a3, a4):
        for e in edges:
            a.axvline(x[e], color="grey", lw=0.4, ls=":")
        a.set_xlabel(r"$x$ [µm]")
    a3.set_ylabel(r"$\theta$ [deg]")
    a3.set_title("(c) theta: a staircase, a step at every dotted edge", fontsize=10)
    a3.legend(fontsize=7)
    a4.set_ylabel(r"$\eta$")
    a4.set_title("(d) eta: a dip under every step", fontsize=10)

    steps = np.abs(np.diff(theta[middle], axis=0))
    keep = edges != n // 2
    a5.plot(bu, out.wall_misorientation_deg, color="tab:red",
            label=r"ledger $b\,\rho_w D$ (median)")
    if keep.any():
        a5.plot(bu, np.median(steps[keep], axis=0), "--", color="tab:blue",
                label="lattice step between subgrains (median)")
        a5.plot(bu, steps[keep].max(axis=0), ":", color="tab:blue", label="lattice step (max)")
    a5.plot(bu, theta_l, "k", label=r"[HBS] $\Theta(bu)$")
    a5.set_ylabel("misorientation [deg]")
    a5.set_title("(e) walls: dislocations stored vs rotation the lattice keeps", fontsize=10)

    a6.plot(bu, np.median(y[:n], axis=0), color="tab:green", label=r"$\eta$ grain (median)")
    if interior.size:
        a6.plot(bu, y[interior].min(axis=0), color="tab:blue", label=r"$\eta$ at walls (min)")
    a6.plot(bu, y[n // 2], color="tab:orange", label=r"$\eta$ at the original GB")
    a6.axhline(p.c2, color="grey", lw=0.8, ls=":")
    a6.set_ylabel(r"$\eta$")
    a6.set_ylim(-0.05, 1.05)
    twin = a6.twinx()
    stored = y[3 * n:].mean(axis=0)
    twin.plot(bu, stored / np.maximum(stored + y[2 * n:3 * n].mean(axis=0), 1e-30), "--",
              color="tab:red", label=r"$\rho_w/(\rho+\rho_w)$")
    twin.plot(bu, ord_ / tot, ":", color="k", label=r"[HBS] $\rho_{ord}/\rho_{tot}$")
    twin.set_ylabel("share in walls")
    twin.set_ylim(-0.05, 1.05)
    twin.legend(fontsize=7, loc="center right")
    a6.set_title("(f) order of grain, walls and GB; share of dislocations in walls",
                 fontsize=10)
    for a in (a5, a6):
        a.set_xlabel("burnup [GWd/tU]")
        a.legend(fontsize=7, loc="upper left")
        if not math.isnan(flip):
            a.axvline(flip, color="grey", lw=0.8, ls="--")
        if not math.isnan(out.bu_nucleation):
            a.axvline(out.bu_nucleation, color="tab:purple", lw=0.8, ls=":")
    fig.suptitle(rf"UO$_2$ polygonisation, $\Delta\theta$ = {out.delta_theta_deg:g}$^\circ$, "
                 rf"$D$ = {out.subgrain_size * 1e6:.2f} µm, $c_4$ = {p.c4:g}, "
                 rf"$\hat\tau$ = {p.tau_hat:g} $f_0 t_0$, flip at {flip:.1f} GWd/tU "
                 f"(dashed), event: {out.event} (purple)", fontsize=11)
    fig.tight_layout()
    fig.savefig(path, dpi=130)
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
# Polygonisation needs two changes to [T26] for UO2 (docstring 3.6, STATUS):
# - a slower lattice reorientation.  With Table 1's tau_hat = 1e1 f0 t0 a
#   subgrain rotation is smoothed away in ~1 day (D_theta = mu^2/(tau_hat t0));
#   with 1e4 the walls keep it and grow their own eta dips.
# - a bulk plateau c4 of phi' (Eq. A11), so that the tangle can soften the
#   grain: with c4 = 0 a wall cannot start in a grain at all.  With 0.3 the
#   grain flips to the disordered branch near rho ~ 1e15 (~58 GWd/tU).
# The two values below are the [T26] ones, so the default run is unchanged; the
# polygonisation probes of 3.6 use UO2_TAU_HAT = 1e4 and UO2_C4 = 0.3.
UO2_TAU_HAT = 1.0e1                     # f0*t0   (polygonisation: 1e4)
UO2_C4 = 0.0                            # -       (polygonisation: 0.3)
AFFINITY_SMOOTHING = 0.0                # J/m, rounds the kink of <A>+ (0: none;
                                        #   tried at 1% of mu_f: no help)
# Recovery (docstring 3.4): weaker than the Cu value of Table 1.  C_D = 100 is
# not usable here - it makes the problem so stiff that one irradiation history
# does not finish in 17 min, against ~1 min at 10.
UO2_C_D = 10.0
# Polygonisation (docstring 3.6, option A): the tangle goes into the walls and
# turns the subgrains.  Its mobility M is not measured (limitation 9) and not
# calibrated yet (review point 3 of 3.6), so it is given as the time in years
# in which a free subgrain would empty its tangle, and main() turns it into M
# with mobility_from_timescale().  None switches polygonisation off and the
# model is then exactly [T26] (and UO2_C3 was calibrated in that case).
POLYGONISATION_YEARS = None             # years (e.g. 3.0), or None (then exactly [T26])
# Chosen to straddle the nucleation window: the new grain lives between two
# boundaries for ~1 GWd/tU before it has eaten both parents, so a list that
# jumps 60 -> 65 -> 80 draws the domain before and after it and never the
# nucleus itself.
SNAPSHOT_BURNUPS = [0.0, 20.0, 40.0, 55.0, 60.0, 62.5, 64.0, 66.0, 80.0, 110.0]

def main():
    
    print("\n=== Make figure directory ===")
    os.makedirs(FIGURES_DIR, exist_ok=True)

    print("\n=== Cu: checks against [T26] ===")

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

    print("\n=== Cu: figures of [T26] ===")

    print("Fig. 1")

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
    axes[1].set_title(rf"coupling, Eqs. (35)-(36), $c$ = {CU.c:g}, $C_0$ = {g_offset(CU.c):.3f}",
                      fontsize=9)

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
    fig.tight_layout()
    fig.savefig(f"{FIGURES_DIR}/tandogan_cu_fig1_state_functions.png", dpi=150)
    print(f"wrote {FIGURES_DIR}/tandogan_cu_fig1_state_functions.png")

    print("Fig. 2")
    plot_gb_energy(CU, f"{FIGURES_DIR}/tandogan_cu_fig2.png", grid=CU_GRID)

    print("Fig. 4")

    plt = _pyplot()
    n = CU_GRID.cells
    times = [0.0, 1e1, 1e2, 1e3, 2e3, 5e3, 1e4]
    colours = ["tab:red", "tab:blue", "tab:green", "tab:orange", "tab:purple",
               "tab:brown", "tab:pink"]
    fig, axes = plt.subplots(1, 3, figsize=(13, 3.8))

    out = run_case(CU, CU_DELTA_THETA, CU_RHO0, t_end=times[-1], grid=CU_GRID, samples=times)
    print(f"  C_D = {CU.c_d:g}: {out.event}, "
          f"t_recovery = {out.t_recovery / CU.t0:g} t0")
    x = CU_GRID.x * 1e6
    for t, colour in zip(times, colours):
        y = state_at(out, t * CU.t0)
        axes[0].plot(x, y[:n], color=colour, label=f"$C_D$={CU.c_d:g}, t={t:g} $t_0$")
        axes[1].plot(x, np.degrees(y[n:2 * n]), color=colour)
        axes[2].plot(x, y[2 * n:3 * n], color=colour)

    axes[0].axhline(eta_equilibrium(CU_RHO0, CU), color="grey", lw=0.8)
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
    plot_gb_energy(p, f"{FIGURES_DIR}/tandogan_uo2_gb_energy.png",
                   angles=FIT_ANGLES, targets=uo2_gamma_targets())

    # >>> POLYGONISATION: M0 from the transfer time (docstring 3.6)
    polygonisation = (0.0 if POLYGONISATION_YEARS is None
                      else mobility_from_timescale(POLYGONISATION_YEARS, p))

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
