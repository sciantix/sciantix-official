# Five metal particles (FMP) in UO2, LWR conditions

Model and validation of the five metal particles (Mo-Tc-Ru-Rh-Pd precipitates, `iFMP = 1`,
`src/models/FiveMetalParticles.C`). Model of V. Bellani (branch `development/metallicinclusions`, thesis
2026), port to SCIANTIX 2.5 and changes by E. Cappellari, marked **EC November 2026** below and in the code.

## Changes with respect to the model of V. Bellani (EC November 2026)

| Item | V. Bellani | EC November 2026 |
|---|---|---|
| Name, switch | 5MP / MFP, `iFMP` and `iCm` (same model, different outputs) | FMP, `iFMP` only |
| Parameters | 9 scaling factors `sf_mfp_*` on reference values (`input_scaling_factors.txt`) | values written in the code |
| Production | lumped yield y = 0.578 at/fiss (PRODHEL composition) | sum of the Mo, Tc, Ru, Rh, Pd systems, ENDF/B-VIII.0 LWR yields (0.5666 at/fiss) |
| Matrix | one matrix, dissolved metal C_m | two zones coupled by diffusion: zone 1 = grain interior (intragranular particles), zone 2 = grain-boundary zone, 100 nm on each side of the boundaries (grain-boundary particles) |
| Supersaturation | S = C_m / C_eq for both populations | S_k = c_k / C_eq of the zone of each population |
| Intragranular sites | f_d rho_d / lambda + f_b N_b | the same, times the volume fraction of zone 1 |
| Mass limit of nucleation | both requests reduced in proportion to the dissolved metal | each population limited by the metal of its zone |
| Implicit system | 3x3, (C_m, C_intra, C_GB) | 4x4, (C_m1, C_m2, C_intra, C_GB), with the exchange term |
| Intragranular / grain-boundary parameters | common k_0 and B | free: k_gb_0 = 3.06 k_intra_0, B_gb != B_intra (new calibration) |
| Outputs | Cm, Metal produced, atoms per particle | radii, metal dissolved in zone 2, volume fraction of zone 2, oxygen in the Mo oxides |
| OpenCalphad at the grain boundary | whole production of each metal (counted also in the FMP reservoirs) | grain-boundary inventory of the model, x_X C_GB (single inventory) |
| Mo oxidation | none | metallic share 1 - x_Mo f_ox of all the particles, f_ox from OpenCalphad |
| Oxygen | not coupled | balance between the fuel and the Mo oxides (Kleykamp excess oxygen, Mo/MoO2 buffer) |
| Grain-boundary area fraction | N_GB per unit fuel volume (100-700 times below TEM, not in J) | N_GB per unit volume of zone 2 (the TEM region), in the objective J4 |
| Calibration | 2 parameters, J3 = 0.614 | 4 parameters, J4 = 0.554 (J3 = 0.598) |

## Model equations

All concentrations are per unit fuel volume (at/m3 for the metal, FMP/m3 for the particles); c = C / phi is
the local concentration in a zone. The five elements are one lumped species: the particles have the
production composition, x_X.

### Production

**EC November 2026**: the production is the sum of the five element systems `X in UO2` (`SetSystem.C`),

$$ \frac{dC_{\mathrm{FMP}}}{dt} = \sum_X y_X F, \qquad X = \mathrm{Mo, Tc, Ru, Rh, Pd}, $$

with F the fission rate and the elemental yields (ENDF/B-VIII.0, fission fractions of an LWR cycle as in
https://arxiv.org/pdf/1808.10836) y = 0.2370 (Mo), 0.0614 (Tc), 0.1442 (Ru), 0.0469 (Rh), 0.0771 (Pd),
total 0.5666 at/fiss. V. Bellani: one lumped yield, y = 0.578 at/fiss. The particle composition is
$x_X = C_X^{\mathrm{produced}} / \sum_X C_X^{\mathrm{produced}}$ (42Mo-11Tc-25Ru-8Rh-14Pd at.%).

The inventory is conserved:

$$ \sum_X C_X^{\mathrm{produced}} = C_{m1} + C_{m2} + C^{\mathrm{intra}} + C^{\mathrm{GB}} $$

(V. Bellani: $C_m + C^{\mathrm{intra}} + C^{\mathrm{GB}}$).

### Two zones (EC November 2026)

For a spherical grain of radius a, zone 2 is the shell of thickness delta inside the grain, so that each
boundary has a layer delta on both sides:

$$ \phi_2 = 1 - \left(1 - \frac{\delta}{a}\right)^3 \approx \frac{3\delta}{a}, \qquad \phi_1 = 1 - \phi_2, \qquad
\delta = 100\ \mathrm{nm}, $$

($\phi_2 \le 0.999$). delta is the half-width of the grain-boundary regions of the TEM images of the
calibration (strips of about 200 nm across the boundary, V. Bellani, thesis, Figs. 5.3-5.16); with
a = 12.3 um, phi_2 = 0.024. The production is uniform: $\phi_k P$ in zone k. Local concentrations:
$c_k = C_{mk} / \phi_k$.

### Equilibrium concentration and supersaturation

$$ C_{\mathrm{eq}}(T) = C_{\mathrm{eq,ref}} \exp\left[-\frac{H}{k_B}\left(\frac{1}{T} - \frac{1}{T_{\mathrm{ref}}}\right)\right],
\qquad C_{\mathrm{eq,ref}} = x_{\mathrm{eq}} \frac{4}{a_{\mathrm{UO_2}}^3} = 1.462\times10^{24}\ \mathrm{at/m^3}, $$

with $x_{\mathrm{eq}} = 6\times10^{-5}$ (Mo solubility in UO2 at 1900 degC, Kleykamp), $a_{\mathrm{UO_2}} = 547.5$ pm,
H = 1.14 eV (DFT solution energy of Mo in UO2), $T_{\mathrm{ref}} = 2173.15$ K.

$$ S_k = \frac{c_k}{C_{\mathrm{eq}}}, \qquad \Delta\mu_k = k_B T \ln S_k \quad (S_k > 1), $$

with c_k after the production of the time step. **EC November 2026**: per zone, S_1 for the intragranular
particles and S_2 for the grain-boundary particles (V. Bellani: $S = C_m / C_{\mathrm{eq}}$ for both).

### Nucleation (CNT)

$$ \Delta G_j^* = \frac{B_j}{T^2 (\ln S)^2}, \qquad \nu_j = \nu_{0,j} \exp\left(-\frac{\Delta G_j^*}{k_B T}\right),
\qquad j = \mathrm{intra}, \mathrm{GB}, $$

with $\nu_{0,j} = 1$ 1/s and S of the zone of the population. Nucleation sites:

$$ N_{\mathrm{site,intra}} = \phi_1 \left( f_d \frac{\rho_d}{\lambda} + f_b N_{b,\mathrm{intra}} \right), \qquad
\rho_d = 6.545\times10^{12}\, b^{1.151} f_T, \qquad
f_T = 0.608 + \frac{1 - 0.608}{1 + \exp[(T - 1109)/25.8]}, $$

$$ N_{\mathrm{site,GB}} = \frac{3}{2 a \lambda^2}, $$

with b the burnup (MWd/kgUO2), $f_d = 0.67$, $f_b = 0.33$ (image analysis), $N_{b,\mathrm{intra}}$ the
intragranular bubble concentration, lambda = 15 nm and 3/(2a) the boundary area per unit volume (each
boundary shared by two grains). **EC November 2026**: the factor $\phi_1$ (V. Bellani: no factor).

Particle number, implicit in time, never decreasing:

$$ \frac{dN_j}{dt} = \nu_j \left( N_{\mathrm{site},j} - N_j \right), \qquad
\Delta N_j = \max\left[ \frac{N_j^{\mathrm{old}} + N_{\mathrm{site},j} \nu_j \Delta t}{1 + \nu_j \Delta t} - N_j^{\mathrm{old}}, 0 \right]. $$

Atoms per nucleus and mass taken from the matrix:

$$ n_j^* = \max\left( 2, \frac{2 \Delta G_j^*}{\Delta\mu} \right), \qquad
\Delta C_{\mathrm{nucl},j} = \min\left( n_j^* \Delta N_j, C_{mk}^{\mathrm{old}} + \phi_k P \Delta t \right), \qquad
N_j = N_j^{\mathrm{old}} + \frac{\Delta C_{\mathrm{nucl},j}}{n_j^*}, $$

**EC November 2026**: each population takes the atoms from its zone (k = 1 for intra, k = 2 for GB);
V. Bellani: both requests limited together by $C_m$, reduced in proportion.

### Growth

$$ k_j = D_j \, 4\pi N_j R_j, \qquad D_j = k_{j,0} \exp\left(-\frac{\Delta G_j}{k_B T}\right), $$

diffusion-limited growth with the sink strength $4\pi N_j R_j$, computed from the state at the beginning of
the time step (lagged coefficients, no iteration on the feedback R -> k -> C). Growth is active in a zone
only if $c_k > C_{\mathrm{eq}}$ after production and nucleation; there is no thermal dissolution.

### Exchange between the zones (EC November 2026)

Flux $K_{\mathrm{ex}}(c_1 - c_2)$ per unit volume. The conductance is the grain-boundary sink strength of a
sphere with the intragranular particles as internal sinks (Brailsford and Bullough), in series with the
half-thickness of zone 2 (interface area 3/a per unit volume):

$$ K_{\mathrm{ex}} = \frac{D_{\mathrm{intra}}}{1/k_{\mathrm{GB}}^2 + a \delta / 6}, \qquad
k_{\mathrm{GB}}^2 = \max\left( k_1^2 \frac{F}{1 - F}, \frac{15}{a^2} \right), \qquad
F = \frac{3}{z}\left(\coth z - \frac{1}{z}\right), \qquad z = k_1 a, $$

with $k_1^2 = 4\pi (N_{\mathrm{intra}}/\phi_1) R_{\mathrm{intra}}$. No new parameter.

#### Derivation of $K_{\mathrm{ex}}$

The metal dissolved in zone 1 diffuses until it is captured by an intragranular particle or reaches zone 2.
$K_{\mathrm{ex}}$ is the conductance between the mean concentration of zone 1, $c_1$, and that of zone 2, $c_2$,
made of two resistances in series through the concentration $c_i$ at the interface between the zones.

**Step 1, sphere with internal sinks.** Grain of radius a, uniform source P, the intragranular particles as
uniform sinks of strength $k_1^2 = 4\pi n_1 R_{\mathrm{intra}}$ ($n_1 = N_{\mathrm{intra}}/\phi_1$, particles per
unit volume of zone 1), concentration $c_i$ at r = a, quasi-steady state. With $u = c - c_i$:

$$ D \frac{1}{r^2}\frac{d}{dr}\left(r^2 \frac{du}{dr}\right) - D k_1^2 u + S = 0, \qquad u'(0) = 0, \qquad u(a) = 0, $$

where S is uniform (production, minus $D k_1^2 c_i$; the same with the term $C_{\mathrm{eq}}$ of the growth law).
Since $\frac{1}{r^2}\frac{d}{dr}\left(r^2 \frac{d}{dr}\frac{\sinh kr}{r}\right) = k^2 \frac{\sinh kr}{r}$, the
solution regular at r = 0 is

$$ u(r) = \frac{S}{D k_1^2}\left[1 - \frac{a \sinh(k_1 r)}{r \sinh(k_1 a)}\right]. $$

**Step 2, mean concentration.** With $\int_0^a r \sinh(k_1 r)\,dr = \frac{a \cosh z}{k_1} - \frac{\sinh z}{k_1^2}$, $z = k_1 a$:

$$ \bar u = \frac{3}{a^3}\int_0^a u\, r^2 dr = \frac{S}{D k_1^2}\left[1 - \frac{3}{a^2 \sinh z}\left(\frac{a \cosh z}{k_1} -
\frac{\sinh z}{k_1^2}\right)\right] = \frac{S}{D k_1^2}(1 - F), \qquad F = \frac{3}{z}\left(\coth z - \frac{1}{z}\right). $$

**Step 3, boundary sink strength.** In steady state the source goes either to the particles or across r = a:
$S = D k_1^2 \bar u + D k_{\mathrm{GB}}^2 \bar u$, which defines $k_{\mathrm{GB}}^2$:

$$ k_{\mathrm{GB}}^2 = \frac{S}{D \bar u} - k_1^2 = \frac{k_1^2}{1 - F} - k_1^2 = k_1^2 \frac{F}{1 - F}. $$

The flux to zone 2 per unit volume of the grain is $D k_{\mathrm{GB}}^2 (c_1 - c_i)$, with $c_1 = \bar c$. The
result does not depend on S, so it holds with $c_i \neq 0$ and with the term $C_{\mathrm{eq}}$.

**Step 4, limits.**
- No particles, $z \to 0$: $\coth z = 1/z + z/3 - z^3/45 + \dots$, $F = 1 - z^2/15$, $k_{\mathrm{GB}}^2 \to 15/a^2$.
  Directly: $u = S (a^2 - r^2)/6D$, $\bar u = S a^2/15 D$ (Booth sphere).
- Particles dominate, $z \gg 1$: $\coth z \to 1$, $F \approx 3/z$, $k_{\mathrm{GB}}^2 \to 3 k_1 / a$. Only the metal
  produced within about $1/k_1$ of the boundary reaches it; the rest is captured by the particles.

In the code $k_{\mathrm{GB}}^2 = \max(k_1^2 F/(1-F), 15/a^2)$: the two are equal in exact arithmetic
($k_1^2 F/(1-F) \geq 15/a^2$); the max avoids the cancellation in $1 - F$ at small z.

**Step 5, half-thickness of zone 2.** From the interface to the middle of zone 2, diffusion over $\delta/2$ through
the interface area per unit volume $4\pi a^2 / (\frac{4}{3}\pi a^3) = 3/a$:

$$ J = \frac{3}{a} \frac{D (c_i - c_2)}{\delta/2} = \frac{6 D}{a \delta}(c_i - c_2). $$

**Step 6, series.** $c_1 - c_i = J / (D k_{\mathrm{GB}}^2)$ and $c_i - c_2 = J a\delta/(6D)$; summing:

$$ J = \frac{D (c_1 - c_2)}{1/k_{\mathrm{GB}}^2 + a\delta/6} = K_{\mathrm{ex}} (c_1 - c_2). $$

**Fick form.** $1/k_{\mathrm{GB}}^2 + a\delta/6 = \frac{a}{3}(L_1 + \delta/2)$ with $L_1 = 3/(a k_{\mathrm{GB}}^2)$:

$$ K_{\mathrm{ex}} = \frac{3}{a} \frac{D}{L_1 + \delta/2}: $$

area times D over a distance. $L_1 = a/5$ = 2.5 um without particles, $L_1 \approx 1/k_1$ (distance travelled
before the capture by a particle) when the particles dominate. r000: $L_1$ = 107 nm at 14 and 42 nm at 54.1
MWd/kgUO2, against $\delta/2$ = 50 nm.

**Numerical check** (`check_exchange.py`, figure `figures/exchange_check.png`): finite-volume solution of the
problem of step 1 for z = 0.1-293; $k_{\mathrm{GB}}^2$ agrees with the expression of step 3 within 6e-8 and tends
to the two limits of step 4.

**Exact and approximate parts.** Steps 1-4 are exact for quasi-steady diffusion with uniform production and uniform
sinks in a sphere. Approximations: zone 2 is a well-mixed layer with the mean concentration at $\delta/2$ from the
interface (step 5, its sinks and production are not resolved); the radius of zone 1 is taken as a instead of
$a - \delta$ ($\delta/a$ = 0.8%); $D_{\mathrm{intra}}$ also in zone 2; quasi-steady state at every time step.

References: A.D. Brailsford, R. Bullough, *The theory of sink strengths*, Phil. Trans. R. Soc. Lond. A 302 (1981)
87-137, https://doi.org/10.1098/rsta.1981.0158 (sink strengths, grain boundary with internal sinks); A.H. Booth,
*A method of calculating fission gas diffusion from UO2 fuel and its application to the X-2-f loop test*,
AECL-496 (CRDC-721), Atomic Energy of Canada Limited, 1957 (sphere with uniform source, mean concentration
$S a^2/15D$). The derivation above is self-contained.

### Re-solution

$$ J_{\mathrm{res},j} = k_{\mathrm{res}} C^j, \qquad k_{\mathrm{res}} = 3.085078\times10^{-6}\ \mathrm{s^{-1}} \, \frac{F}{1.48\times10^{19}\ \mathrm{fiss/(m^3 s)}}, $$

from the FMP shrinkage under ion irradiation of Sakib et al. (R from 0.91 to 0.80 between 0.30 and 1.75 dpa,
about 1 dpa/day, $n \propto R^3$). The re-solved metal returns to the zone of the particles.

### Mass balance, implicit 4x4 system

$$ \begin{aligned}
\frac{dC_{m1}}{dt} &= \phi_1 P - k_{\mathrm{intra}} (c_1 - C_{\mathrm{eq}}) - K_{\mathrm{ex}} (c_1 - c_2) + k_{\mathrm{res}} C^{\mathrm{intra}} \\
\frac{dC_{m2}}{dt} &= \phi_2 P - k_{\mathrm{GB}} (c_2 - C_{\mathrm{eq}}) + K_{\mathrm{ex}} (c_1 - c_2) + k_{\mathrm{res}} C^{\mathrm{GB}} \\
\frac{dC^{\mathrm{intra}}}{dt} &= k_{\mathrm{intra}} (c_1 - C_{\mathrm{eq}}) - k_{\mathrm{res}} C^{\mathrm{intra}} \\
\frac{dC^{\mathrm{GB}}}{dt} &= k_{\mathrm{GB}} (c_2 - C_{\mathrm{eq}}) - k_{\mathrm{res}} C^{\mathrm{GB}}
\end{aligned} $$

solved with backward Euler after production and nucleation (`Solver::Laplace`). Every column of the matrix
sums to one: the total metal is conserved. V. Bellani: 3x3 system with one matrix,

$$ \frac{dC_m}{dt} = P - (k_{\mathrm{intra}} + k_{\mathrm{GB}}) (C_m - C_{\mathrm{eq}}) + k_{\mathrm{res}} (C^{\mathrm{intra}} + C^{\mathrm{GB}}). $$

### Radius

$$ R_j = \left( \frac{3 \phi_{\mathrm{met}} C^j \Omega}{4\pi N_j} \right)^{1/3}, \qquad \Omega = 1.44123\times10^{-29}\ \mathrm{m^3/at}, $$

zero while $N_j < 1$ FMP/m3. **EC November 2026**: radius as output (V. Bellani: atoms per particle) and only
from the metallic part $\phi_{\mathrm{met}}$ of the inventory (below; $\phi_{\mathrm{met}} = 1$ without thermochemistry).

### Coupling with OpenCalphad at the grain boundary (EC November 2026)

With `iThermochimica` active, OpenCalphad at the grain boundary receives, for each metal, the grain-boundary
inventory of the model,

$$ n_X^{\mathrm{OC}} = x_X C^{\mathrm{GB}}, $$

instead of the whole production. `JOGFormation` stores the fraction of this Mo in oxide phases,
$f_{\mathrm{ox}}$ (`Mo in oxide fraction`); the oxygen potential is uniform in the grain, so $f_{\mathrm{ox}}$ holds for all
the particles. The metallic share of the particle inventory, with one time step of lag, is

$$ \phi_{\mathrm{met}} = 1 - x_{\mathrm{Mo}} f_{\mathrm{ox}}. $$

The oxidised Mo stays in the particle inventory (and is re-solved with it). A step where OpenCalphad gives
no result keeps the previous $f_{\mathrm{ox}}$.

### Oxygen balance between the fuel and the Mo oxides (EC November 2026)

UO2 with `iStoichiometryDeviation = 7` (Kleykamp 1979, x increasing by 0.0013 per % FIMA), standalone
SCIANTIX (`SetPhaseDiagram.C`). The excess oxygen E (at/m3), released by fission and not taken by the other
oxide-forming fission products, is shared between the fuel and the Mo oxides of the particles:

$$ E = x M + O_{\mathrm{ox}}, \qquad \mathrm{Mo}_p = x_{\mathrm{Mo}} (C^{\mathrm{intra}} + C^{\mathrm{GB}}), \qquad
\mathrm{Mo}_{\mathrm{GB}} = x_{\mathrm{Mo}} C^{\mathrm{GB}}, $$

with M the heavy-metal atoms per unit volume.

- $E < 2\,\mathrm{Mo}_p$: the Mo/MoO2 equilibrium of the particles buffers the fuel. OpenCalphad at the grain
  boundary is solved with the fixed amount of oxygen $E\,\mathrm{Mo}_{\mathrm{GB}}/\mathrm{Mo}_p$ (share of the
  grain-boundary particles) and gives the oxygen potential $\mu_\mathrm{O}$; x from the inverse of the Blackburn
  relation (`Solver::NewtonBlackburn`),

  $$ \ln\frac{p_{\mathrm{O_2}}}{1\ \mathrm{atm}} = 2\ln\frac{x(2+x)}{1-x} + 108 x^2 - \frac{32700}{T} + 9.92
  = \frac{2\mu_\mathrm{O}}{RT} + \ln\frac{1\ \mathrm{bar}}{1\ \mathrm{atm}}, \qquad
  x \le \frac{E}{M}, \qquad O_{\mathrm{ox}} = E - x M; $$

- $E \ge 2\,\mathrm{Mo}_p$: all the Mo of the particles is MoO2, $O_{\mathrm{ox}} = 2\,\mathrm{Mo}_p$,
  $x = (E - 2\,\mathrm{Mo}_p)/M$; OpenCalphad is solved at the oxygen potential of the fuel.

Then the stoichiometry deviation, oxygen partial pressure and potential of the fuel are updated with x
(Blackburn), and $\Delta O_{\mathrm{ox}}$ is removed from `O content`. Without the balance, x was the Kleykamp value
and the grain-boundary Mo was oxidised without taking oxygen from the fuel. Only the Mo of the particles is
an oxygen getter (the dissolved metal is not oxidised).

### Parameters

| Parameter | Value | Origin |
|---|---|---|
| $k_{\mathrm{intra},0}$ | 9.834342e-16 m2/s | EC November 2026 calibration (V. Bellani: 4.980333299845e-15, common) |
| $k_{\mathrm{GB},0}$ | 3.009503e-15 m2/s | EC November 2026 calibration (V. Bellani: 4.980333299845e-15, common) |
| $B_{\mathrm{intra}}$ | 2.711740e8 eV K2 | EC November 2026 calibration (V. Bellani: 2.665764962509e8, common) |
| $B_{\mathrm{GB}}$ | 2.641409e8 eV K2 | EC November 2026 calibration (V. Bellani: 2.665764962509e8, common) |
| $\Delta G_{\mathrm{intra}} = \Delta G_{\mathrm{GB}}$ | 1.0 eV | V. Bellani |
| $\nu_{0,\mathrm{intra}} = \nu_{0,\mathrm{GB}}$ | 1.0 1/s | V. Bellani |
| delta | 100 nm | EC November 2026, TEM grain-boundary regions |
| $\Omega$ | 1.44123e-29 m3/at | V. Bellani |
| $n_{\min}$ | 2 at | V. Bellani (metallic dimers) |
| lambda | 15 nm | V. Bellani |
| $f_d$, $f_b$ | 0.67, 0.33 | V. Bellani (image analysis) |
| $k_{\mathrm{res,ref}}$ | 3.085078e-6 1/s at 1.48e19 fiss/(m3 s) | V. Bellani (Sakib et al.) |
| $x_{\mathrm{eq}}$, H, $T_{\mathrm{ref}}$ | 6e-5, 1.14 eV, 2173.15 K | V. Bellani (Kleykamp, DFT) |

### Order in the time step

`FiveMetalParticles` runs after `FissionProductProduction` and `FissionProductDecay`; with thermochemistry,
`SetPhaseDiagram` then solves OpenCalphad at the grain boundary with the oxygen balance, and `JOGFormation`
updates $f_{\mathrm{ox}}$ for the next time step.

## Calibration

### V. Bellani (`calibration/FMPcalibration.md`)

Sobol sampling of two common factors on the four TEM cases: precipitation prefactor $k_0$ and CNT coefficient
B, same values for the two populations. Objective J3 = mean |ln(model/exp)| over A_intra, R_intra and R_GB
(12 observables): J3 = 0.614 (0.611 with the per-element yields, 0.616 with the two zones and the same values).

### Two-zone model, EC November 2026 (`calibration_two_zones/`)

With common parameters the exchange keeps the two zones at nearly the same concentration and R_GB / R_intra
= 1.003-1.011. Four free parameters, $k_{\mathrm{intra},0}$, $B_{\mathrm{intra}}$, $k_{\mathrm{GB},0}$, $B_{\mathrm{GB}}$ (Delta G and
nu_0 fixed), 4096 Sobol points and five Nelder-Mead refinements from the best points, all ending at the same
point. Objective J4 = mean |ln(model/exp)| over R_intra, R_GB, A_intra and A_GB (16 observables), with A on
the zone volume of each population (below). See `calibration_two_zones/README.md`.

| | $k_{\mathrm{intra},0}$ | $B_{\mathrm{intra}}$ | $k_{\mathrm{GB},0}$ | $B_{\mathrm{GB}}$ | J4 | J3 |
|---|---|---|---|---|---|---|
| V. Bellani (common) | 4.980e-15 | 2.666e8 | 4.980e-15 | 2.666e8 | 0.869 | 0.616 |
| Two zones, calibrated | 9.83e-16 (x0.197) | 2.712e8 (x1.017) | 3.010e-15 (x0.604) | 2.641e8 (x0.991) | 0.554 | 0.598 |

D_GB / D_intra = 3.06 (same activation energy). B_intra, B_GB and k_GB,0 are well identified, k_intra,0 less
(x0.155-0.197 for the same J4).

## Cases

| Group | Cases | Purpose |
|---|---|---|
| `--metallicinclusions` | `test_FMP_UO2_r000/r030/r056/r075` | calibration: TEM data at four radial positions, 54.1 MWd/kgUO2 |
| `--metallicinclusions` | `test_FMP_UO2_Buck2015_r035/r061/r079/r094` | independent validation: ATM-109, Buck et al. (2015) |
| `--metallicinclusions-oc` | `thermochemistry/test_FMP_UO2_OC_r000/r030/r056/r075` | calibration cases with OpenCalphad at the grain boundary |

### Calibration cases

UO2 irradiated in LWR conditions to 54.1 MWd/kgUO2; the four cases are the radial positions used for the
calibration. They share settings and initial conditions; only the temperature history changes.

| Case | r/R | Temperature (K) | TEM foil thickness (nm) |
|---|---|---|---|
| `test_FMP_UO2_r000` | 0.00 | 1111-1210 | 50 (nominal) |
| `test_FMP_UO2_r030` | 0.30 | 1068-1158 | 50 (nominal) |
| `test_FMP_UO2_r056` | 0.56 | 961-1029 | 70 |
| `test_FMP_UO2_r075` | 0.75 | 842-886 | 40 |

Common conditions: 30000 h, same fission-rate history at the four positions (1.31-1.60e19 fiss/(m3 s)),
grain radius 12.3 um, density 10421.5 kg/m3, 4% U-235, no helium, no HBS, stoichiometric UO2
(`iStoichiometryDeviation = 0`), `iThermochimica = 0`.

Observables: R from the output, projected area fraction $A = 1 - \exp(-n \pi R^2 t)$ with t the TEM foil
thickness and n the particle density in the zone of the population, $N_{\mathrm{intra}}/\phi_1$ and
$N_{\mathrm{GB}}/\phi_2$ (EC November 2026; V. Bellani: per unit fuel volume).

Results with the calibrated parameters (54.1 MWd/kgUO2):

| Case | R_intra exp / model (nm) | R_GB exp / model (nm) | A_intra exp / model (%) | A_GB exp / model (%) | Metal dissolved | Metal in GB particles |
|---|---|---|---|---|---|---|
| r000 | 7.58 / 7.58 | 14.28 / 13.24 | 1.37 / 5.24 | 7.21 / 3.59 | 14.2% | 2.45% |
| r030 | 3.59 / 6.84 | 11.95 / 11.95 | 0.56 / 5.58 | 3.62 / 3.62 | 17.6% | 2.23% |
| r056 | 3.45 / 5.29 | 5.58 / 9.26 | 3.25 / 7.86 | 1.76 / 4.72 | 35.2% | 1.62% |
| r075 | 3.10 / 3.55 | 6.68 / 6.21 | 2.62 / 1.19 | 0.72 / 0.72 | 88.8% | 0.28% |

J4 = 0.554, J3 = 0.598. With the parameters of V. Bellani: R_intra 13.5 / 12.0 / 9.0 / 6.4 nm, R_GB = R_intra,
A_GB 0.32-0.74%, J4 = 0.869.

### Independent validation: Buck et al. (2015), ATM-109

E.C. Buck et al., *Nanostructure of metallic particles in light water reactor used nuclear fuel*,
J. Nucl. Mater. 461 (2015) 236-243. Approved Testing Material ATM-109, Quad Cities I BWR: GE UO2,
3.0% U-235, initial grain size 30 um, specimens maximum burnup 70.80 GWd/MTU, in reactor from February 1979
to September 1987 and from November 1989 to September 1992. The metallic particles were recovered from the
residues of two dissolutions (carbonate-peroxide and nitric acid) and examined by SEM/TEM: diameters of about
10-300 nm, mean size 43 nm (carbonate-peroxide) and 58 nm (nitric), hcp epsilon-Ru structure, variable
composition. These data were not used in the calibration.

The paper gives no power or temperature history (it would need the ATM-109 characterisation reports,
Guenther et al. and Wolf et al.), so the cases use an **assumed** history:

- in reactor continuously over the two periods (1 Feb 1979 - 1 Sep 1987, 1 Nov 1989 - 1 Sep 1992;
  100056 h), out of the reactor in between (300 K, no fissions);
- constant fission rate 5.003e18 fiss/(m3 s), which gives 62.41 MWd/kgUO2 = 70.8 GWd/tHM at the end;
  since 70.80 GWd/MTU is the maximum burnup of the specimens, this is an upper bound;
- particles from dissolved fuel have no radial information, so the pellet is split into four equal-volume
  rings, at (r/R)^2 = 0.125, 0.375, 0.625, 0.875, with a parabolic profile from 900 degC at the centre to
  450 degC at the surface (Buck et al.: LWR centreline below 1200 degC, rim 400-500 degC):

| Case | r/R | Temperature (K) | d_intra / d_GB (nm) | Metal dissolved |
|---|---|---|---|---|
| `test_FMP_UO2_Buck2015_r035` | 0.354 | 1117 | 20.3 / 35.5 | 8% |
| `test_FMP_UO2_Buck2015_r061` | 0.612 | 1004 | 15.0 / 26.2 | 14% |
| `test_FMP_UO2_Buck2015_r079` | 0.791 | 892 | 11.3 / 19.7 | 34% |
| `test_FMP_UO2_Buck2015_r094` | 0.935 | 779 | 7.2 / 12.6 | 93% |

Same settings as the calibration cases; grain radius 15 um, 3.0% U-235, uniform fission rate (no rim
burnup peak).

The comparison uses the pellet-averaged population (equal ring volumes, intragranular and grain-boundary
particles weighted by number). Fig. 6 of the paper is the cumulative **number** distribution of the particle
volume; it was digitized (`data/buck2015/fig6_*.txt`, binned marker pixels, approximate) and fitted with
log-normals of ln V (`data/buck2015/summary.txt`). The fits give median diameters of 13 nm
(carbonate-peroxide) and 28 nm (nitric) and number-mean diameters of 50 and 62 nm, close to the reported
43 and 58 nm, which therefore look like number-mean sizes.

| | Median d (nm) | Number-mean d (nm) | Mean ln V (nm3) |
|---|---|---|---|
| Buck et al., carbonate-peroxide | 13 | 50 (reported 43) | 7.07 |
| Buck et al., nitric acid | 28 | 62 (reported 58) | 9.30 |
| SCIANTIX, calibrated two-zone parameters | 11 | 12.5 | 6.78 |
| SCIANTIX, parameters of V. Bellani | 19 | 18.5 | 7.95 |

With the new parameters the intragranular particles, which dominate the count, are smaller: the median falls
just below the carbonate-peroxide series and the number-mean is 3.5-5 times too low. The grain-boundary
particles (26-36 nm in the two inner rings) are closer to the measured sizes but are 2% of the particles. The
size distribution is much narrower than the measured one (one mean size per ring and population), and the
large particles (100-300 nm) are missing. The particle composition of the model (42Mo-11Tc-25Ru-8Rh-14Pd at.%)
is richer in Mo than the dissolution residue quoted by Buck et al. (20Mo-0.5/5Tc-50/60Ru-10Rh-10Pd).

### Thermochemistry cases

`thermochemistry/test_FMP_UO2_OC_*`: the calibration cases with `iThermochimica = 2` (OpenCalphad at every
time step), `iStoichiometryDeviation = 7` (Kleykamp 1979, same x(burnup) at the four positions, no radial
redistribution of oxygen) and the Blackburn oxygen potential, system pressure 1e5 Pa (fifth history column).
OpenCalphad runs at the grain boundary with `BaMoO_CsMoO_MoPdRhRuTc_merged.TDB` (Mo, O, Pd, Rh, Ru, Tc). The
group is skipped when the OpenCalphad library or the database is missing.

Results with the calibrated parameters (54.1 MWd/kgUO2):

| Case | mu_O2 (kJ/mol) | x | Mo oxidised | R_intra with / without OC (nm) | R_GB with / without OC (nm) | R_GB TEM (nm) |
|---|---|---|---|---|---|---|
| r000 | -375 | 1.4e-5 | 32% | 7.1 / 7.6 | 12.4 / 13.2 | 14.3 |
| r030 | -384 | 6.6e-6 | 33% | 6.4 / 6.8 | 11.2 / 12.0 | 12.0 |
| r056 | -402 | 1.0e-6 | 43% | 4.8 / 5.3 | 8.5 / 9.3 | 5.6 |
| r075 | -265 | 5.8e-3 | 100% | 2.7 / 3.6 | 4.8 / 6.2 | 6.7 |

At r/R = 0-0.56 the fuel is buffered at Mo/MoO2 and x stays far below the Kleykamp value (8.7e-3, -255 kJ/mol,
all the Mo oxidised, without the balance). At r/R = 0.75, 89% of the metal is still dissolved: the Mo of the
particles is not sufficient, it is all oxidised and x rises with burnup (1.6e-3 at 10, 5.8e-3 at 54
MWd/kgUO2). OpenCalphad finds MoO2 and HCP_A3, BCC_A2 (metal) at the grain boundary, Mo4O11 at some steps; at
a few steps it does not converge and the previous Mo oxide fraction is kept. In `test_FMP_UO2_OC_r000` the
convergence at one step changes from one run to the next (OpenCalphad behaviour): the comparison with the gold
can fail although the final values agree within 1e-5.

## Data

- `data/intra/exp_intra_<case>.txt`, `data/inter/exp_inter_<case>.txt`: one row per position with the
  projected area fraction (`Inclusions`, -), mean radius and its standard deviation (um), and `SP`. TEM data
  of the calibration in V. Bellani's thesis.
- `data/buck2015/`: digitized Fig. 6 and summary of Buck et al. (2015).

## Run

```bash
python3 -m testing.runner --metallicinclusions                 # run + compare with output_gold.txt
python3 -m testing.runner --metallicinclusions-oc --oc         # needs OpenCalphad
python3 validation/metallicinclusions/plot.py                  # tables and figures in figures/
python3 validation/metallicinclusions/calibration_two_zones/calibrate.py   # two-zone calibration, ~10 min
python3 validation/metallicinclusions/check_exchange.py   # check of the exchange term, figures/exchange_check.png
```

`plot.py` writes:

- `metallicinclusions_UO2_LWR.png`: calibration cases vs TEM, radius and projected area fraction, J;
- `buck2015_ATM109.png`: Buck et al. (2015), particle volume distribution, diameter per ring, growth in time;
- `fmp_partition.png`: produced metal split into dissolved in the grain interior, dissolved in the
  grain-boundary zone, intragranular and grain-boundary particles vs burnup;
- `fmp_elements.png`: production of the five elements and metallic particle composition without and with
  OpenCalphad;
- `fmp_thermochemistry.png`: oxygen potential with the oxygen balance and with Kleykamp only, x, Mo oxide
  fraction, grain-boundary phases, grain-boundary radius with and without OpenCalphad, share of the excess
  oxygen in the Mo oxides.

`check_exchange.py` writes `exchange_check.png`: concentration profile in the grain without particles and with the
particles of r000 at 54.1 MWd/kgUO2, grain-boundary sink strength from finite volumes against the analytical
expression and its limits.

## Known limits

- Intragranular area fraction: 4-10 times the TEM value at r/R = 0-0.30, 2 times too low at 0.75. With the
  slower intragranular growth of the calibration the particles are smaller but more numerous: the
  intragranular number density is the main remaining discrepancy.
- R_GB is too large at r/R = 0.56 (9.3 vs 5.6 nm), where A_GB is also too high.
- At low temperature most of the metal stays dissolved (89% at r/R = 0.75, 93% in the outer ATM-109 ring).
- One mean size per population: the measured size distribution (Buck et al., 10-300 nm) cannot be
  reproduced, and the mean size of ATM-109 is underestimated.
- The grain-boundary zone has a fixed thickness (100 nm, from the TEM regions); it follows the grain radius.
- The five metals are one lumped species: the particles have the production composition, apart from the
  oxidised Mo, while the measured particles are poorer in Mo and richer in Ru.
- Oxygen balance: only the Mo of the particles takes oxygen (dissolved metal not oxidised), the excess oxygen
  comes from the Kleykamp relation (no radial O/M profile), and Cs and Ba are not in the UO2 systems.
- The calibration does not include OpenCalphad (the Mo oxidation reduces the radii by 5-25%).
- The MOX systems keep fast-reactor yields: the model is validated only for UO2 in LWR conditions.
