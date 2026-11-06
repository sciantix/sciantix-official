# Five metal particles (FMP) in UO2, LWR conditions

Model and validation of the five metal particles (Mo-Tc-Ru-Rh-Pd precipitates, `iFMP = 1`,
`src/models/FiveMetalParticles.C`). Model of V. Bellani (thesis 2026); port to SCIANTIX 2.5, three-zone model
and literature parameters by E. Cappellari, marked **EC November 2026** below and in the code.

The model has three zones: **intra1** (grain interior), **intra2** (near-boundary zone, 100 nm on each side of
the grain boundaries) and **gb** (particles on the boundary). All parameters come from the literature; the TEM
data of the four radial positions are used only for validation.

## Changes with respect to the model of V. Bellani (EC November 2026)

| Item | V. Bellani | EC November 2026 |
|---|---|---|
| Name, switch | 5MP / MFP, `iFMP` and `iCm` | FMP, `iFMP` only |
| Parameters | 9 scaling factors `sf_mfp_*` (`input_scaling_factors.txt`) | values written in the code |
| Production | lumped yield y = 0.578 at/fiss (PRODHEL) | sum of the Mo, Tc, Ru, Rh, Pd systems, ENDF/B-VIII.0 LWR yields (0.5666 at/fiss) |
| Zones | one matrix, intragranular and grain-boundary particles | intra1, intra2 (each with its own dissolved metal and particles) and gb, fed from intra2 |
| Supersaturation | S = C_m / C_eq for both populations | S of the zone of each population |
| Nucleation sites | f_d rho_d / lambda + f_b N_b (intra), 3/(2 a lambda^2) (GB) | intra1: phi_1 f_d rho_d / lambda + f_b N_b; intra2: phi_2 f_d rho_d / lambda (no bubbles); gb unchanged |
| Diffusivity | k_0 exp(-1 eV / k_B T), k_0 fitted | Turnbull et al. (1988), as for Xe |
| CNT barrier | B = 2.666e8 eV K2, fitted | from the interface energy, gamma = 1.0 J/m2; lens factor at the boundary |
| CNT frequency | nu_0 = 1 1/s | Z beta* |
| Mass limit of nucleation | both requests reduced in proportion to the dissolved metal | each population limited by the metal of its zone |
| Implicit system | 3x3, (C_m, C_intra, C_GB) | 5x5, (C_m1, C_m2, C_p1, C_p2, C_gb) |
| Outputs | Cm, Metal produced, atoms per particle | radii, dissolved metal and particles of intra2, volume fraction of intra2, oxygen in the Mo oxides |
| OpenCalphad at the grain boundary | whole production of each metal | gb inventory of the model, x_X C_gb |
| Mo oxidation, oxygen | none | metallic share 1 - x_Mo f_ox of all the particles; oxygen balance between the fuel and the Mo oxides |
| TEM data | used to fit k_0 and B | validation only |
| Grain radius of the TEM cases | 12.3 um | 4.4 um (mean grain size 8.8 um, Angleraud 2024) |

## Model

All concentrations are per unit fuel volume (at/m3 for the metal, FMP/m3 for the particles); c = C / phi is
the local concentration in a zone. The five elements are one lumped species: the particles have the
production composition x_X.

### Production

**EC November 2026**: the production is the sum of the five element systems `X in UO2` (`SetSystem.C`),

$$ \frac{dC_{\mathrm{FMP}}}{dt} = \sum_X y_X F, \qquad X = \mathrm{Mo, Tc, Ru, Rh, Pd}, $$

with F the fission rate and the elemental yields (ENDF/B-VIII.0, fission fractions of an LWR cycle as in
https://arxiv.org/pdf/1808.10836) y = 0.2370 (Mo), 0.0614 (Tc), 0.1442 (Ru), 0.0469 (Rh), 0.0771 (Pd),
total 0.5666 at/fiss. The particle composition is $x_X = C_X^{\mathrm{produced}} / \sum_X C_X^{\mathrm{produced}}$
(42Mo-11Tc-25Ru-8Rh-14Pd at.%). The inventory is conserved:

$$ \sum_X C_X^{\mathrm{produced}} = C_{m1} + C_{m2} + C_{p1} + C_{p2} + C_{gb}. $$

### Zones (EC November 2026)

For a spherical grain of radius a, intra2 is the shell of thickness delta inside the grain, so that each
boundary has a layer delta on both sides:

$$ \phi_2 = 1 - \left(1 - \frac{\delta}{a}\right)^3, \qquad \phi_1 = 1 - \phi_2, \qquad \delta = 100\ \mathrm{nm}, $$

($\phi_2 \le 0.999$). delta is the bubble-free zone of about a hundred nanometres on each side of every grain
boundary, with large metallic precipitates on or near the boundary (Angleraud 2024, ch. 3). With a = 4.4 um,
$\phi_2$ = 0.067. The production is uniform, $\phi_k P$ in zone k; $c_k = C_{mk} / \phi_k$. gb has no volume:
its particles sit on the boundary and take the metal that reaches it from intra2.

### Equilibrium concentration and supersaturation

$$ C_{\mathrm{eq}}(T) = C_{\mathrm{eq,ref}} \exp\left[-\frac{H}{k_B}\left(\frac{1}{T} - \frac{1}{T_{\mathrm{ref}}}\right)\right],
\qquad C_{\mathrm{eq,ref}} = x_{\mathrm{eq}} \frac{4}{a_{\mathrm{UO_2}}^3} = 1.462\times10^{24}\ \mathrm{at/m^3}, $$

with $x_{\mathrm{eq}} = 6\times10^{-5}$ (Mo solubility in UO2 at 1900 degC, Kleykamp 1985),
$a_{\mathrm{UO_2}}$ = 547.5 pm, H = 1.14 eV (solution energy of Mo in UO2, Brillant et al. 2009),
$T_{\mathrm{ref}}$ = 2173.15 K. Supersaturation of each population, after the production of the time step:

$$ S_1 = \frac{c_1}{C_{\mathrm{eq}}}, \qquad S_2 = \frac{c_2}{C_{\mathrm{eq}}}, \qquad S_{gb} = \frac{c_b}{C_{\mathrm{eq}}}, $$

with $c_b$ the concentration at the boundary (below).

### Nucleation (CNT, EC November 2026)

Per site (Kashchiev), $\Delta\mu = k_B T \ln S > 0$:

$$ \Delta G^* = \frac{16\pi\gamma^3\Omega^2 \Phi}{3\Delta\mu^2}, \qquad
n^* = \max\left(n_{\min}, \frac{2\Delta G^*}{\Delta\mu}\right), \qquad r^* = \frac{2\gamma\Omega}{\Delta\mu}, $$

$$ \nu = Z \beta^* \exp\left(-\frac{\Delta G^*}{k_B T}\right), \qquad \beta^* = 4\pi r^* D c, \qquad
Z = \frac{1}{n^*}\sqrt{\frac{\Delta G^*}{3\pi k_B T}}, $$

with c the local concentration of the zone. $\Phi = 1$ in intra1 and intra2; at the boundary the nucleus is a
lens,

$$ \Phi_{gb} = \tfrac{1}{2}(2 + \cos\theta)(1 - \cos\theta)^2, \qquad \cos\theta = \frac{\gamma_{gb}}{2\gamma}, \qquad
\gamma_{gb} = 2\gamma_s\cos\theta_b, $$

with the UO2 surface tension and semi-dihedral angle already in SCIANTIX ($\gamma_s$ = 0.7 J/m2,
$\theta_b$ = 50 deg): $\gamma_{gb}$ = 0.90 J/m2, $\Phi_{gb}$ = 0.37. In the form of V. Bellani,
$\Delta G^* = B / (T^2 \ln^2 S)$ with $B = 16\pi\gamma^3\Omega^2\Phi / (3 k_B^2)$ = 1.14e8 eV K2 (intra) and
4.2e7 eV K2 (gb).

Nucleation sites:

$$ N_{\mathrm{site},1} = \phi_1 f_d \frac{\rho_d}{\lambda} + f_b N_b, \qquad
N_{\mathrm{site},2} = \phi_2 f_d \frac{\rho_d}{\lambda}, \qquad N_{\mathrm{site},gb} = \frac{3}{2 a \lambda^2}, $$

$$ \rho_d = 6.545\times10^{12}\, b^{1.151} f_T, \qquad f_T = 0.608 + \frac{1 - 0.608}{1 + \exp[(T - 1109)/25.8]}, $$

with b the burnup (MWd/kgUO2, Veshchunov and Shestak 2009), $f_d$ = 0.67, $f_b$ = 0.33, $N_b$ the
intragranular bubble concentration, lambda = 15 nm and 3/(2a) the boundary area per unit volume. intra2 has no
bubble sites: the near-boundary zone has no nanometric bubbles and no bubble/precipitate pairs (Angleraud 2024).

Particle number, implicit in time, never decreasing; each nucleus takes $n^*$ atoms from the dissolved metal of
its zone, within the metal available there:

$$ \Delta N = \max\left[ \frac{N^{\mathrm{old}} + N_{\mathrm{site}} \nu \Delta t}{1 + \nu \Delta t} - N^{\mathrm{old}}, 0 \right], \qquad
\Delta C_{\mathrm{nucl}} = \min\left( n^* \Delta N, C_{\mathrm{available}} \right), \qquad
N = N^{\mathrm{old}} + \frac{\Delta C_{\mathrm{nucl}}}{n^*}. $$

intra1 takes from $C_{m1}$; gb and then intra2 take from $C_{m2}$.

### Growth

$$ k_j = 4\pi D N_j R_j, \qquad j = 1, 2, gb, $$

diffusion-limited growth (Ham) with D the single-atom diffusivity of Turnbull et al. (1988),
$D = D_1 + D_2 + D_3$ (thermal, irradiation-enhanced, athermal), the same in all zones (**EC November 2026**;
V. Bellani: $k_0 \exp(-\Delta G / k_B T)$, $k_0$ fitted). Coefficients from the state at the beginning of the
time step. Growth is active in a zone only if $c_k > C_{\mathrm{eq}}$ after production and nucleation; there is no
thermal dissolution.

### Exchange between the zones (EC November 2026)

**intra1 -> intra2**, flux $K_{12}(c_1 - c_2)$. The conductance is the grain-boundary sink strength of a sphere
with the intra1 particles as internal sinks (Brailsford and Bullough 1981), in series with the half-thickness
of intra2 (interface area 3/a per unit volume):

$$ K_{12} = \frac{D}{1/k_{\mathrm{GB}}^2 + a \delta / 6}, \qquad
k_{\mathrm{GB}}^2 = \max\left( k_1^2 \frac{F}{1 - F}, \frac{15}{a^2} \right), \qquad
F = \frac{3}{z}\left(\coth z - \frac{1}{z}\right), \qquad z = k_1 a, $$

with $k_1^2 = 4\pi (N_1/\phi_1) R_1$. Derivation: for quasi-steady diffusion in a grain of radius a with uniform
source S and uniform sinks $k_1^2$, and $u = c - c_i$ with $c_i$ the concentration at r = a,

$$ D \frac{1}{r^2}\frac{d}{dr}\left(r^2 \frac{du}{dr}\right) - D k_1^2 u + S = 0, \quad u(a) = 0
\quad\Rightarrow\quad u(r) = \frac{S}{D k_1^2}\left[1 - \frac{a \sinh(k_1 r)}{r \sinh(k_1 a)}\right],
\quad \bar u = \frac{S}{D k_1^2}(1 - F). $$

The source goes to the particles or across r = a, $S = D (k_1^2 + k_{\mathrm{GB}}^2) \bar u$, which gives
$k_{\mathrm{GB}}^2 = k_1^2 F / (1 - F)$, independent of S. Limits: $15/a^2$ without particles (Booth sphere,
$\bar u = S a^2 / 15 D$), $3 k_1 / a$ when the particles dominate (only the metal produced within about $1/k_1$
of the boundary reaches it). The max in the code avoids the cancellation in $1 - F$ at small z. From the
interface to the middle of intra2, $J = (3/a) D (c_i - c_2) / (\delta/2)$; in series:

$$ K_{12} = \frac{3}{a} \frac{D}{L_1 + \delta/2}, \qquad L_1 = \frac{3}{a k_{\mathrm{GB}}^2}, $$

$L_1$ = a/5 without particles, $L_1 \approx 1/k_1$ with them (16 nm at r000, 54.1 MWd/kgUO2).
`check_exchange.py` solves the sphere problem with finite volumes for z = 0.1-267: $k_{\mathrm{GB}}^2$ agrees with
the expression within 6e-8 (`figures/exchange_check.png`).

**intra2 -> gb**: diffusion from the middle of intra2 to the boundary, $K_{23} = 6D / (a\delta)$, in series with
the capture by the gb particles $k_{gb}$. With the quasi-steady concentration at the boundary,
$K_{23}(c_2 - c_b) = k_{gb}(c_b - C_{\mathrm{eq}})$:

$$ c_b = \frac{K_{23} c_2 + k_{gb} C_{\mathrm{eq}}}{K_{23} + k_{gb}}, \qquad
J_{23} = k_{23} (c_2 - C_{\mathrm{eq}}), \qquad k_{23} = \frac{K_{23} k_{gb}}{K_{23} + k_{gb}}. $$

Before the first gb particle $c_b = c_2$. No grain-boundary width or diffusivity is needed.

Approximations: intra2 is a well-mixed layer (its sinks and production are not resolved); the radius of intra1 is
taken as a instead of $a - \delta$; quasi-steady state at every time step.

### Re-solution

$$ J_{\mathrm{res},j} = k_{\mathrm{res}} C_{pj}, \qquad k_{\mathrm{res}} = 3.085078\times10^{-6}\ \mathrm{s^{-1}} \, \frac{F}{1.48\times10^{19}\ \mathrm{fiss/(m^3 s)}}, $$

from the FMP shrinkage under ion irradiation of Sakib et al. (2025) (R from 0.91 to 0.80 between 0.30 and
1.75 dpa, about 1 dpa/day, $n \propto R^3$). The re-solved metal returns to the zone of the particles (intra2
for gb).

### Mass balance, implicit 5x5 system (EC November 2026)

$$ \begin{aligned}
\frac{dC_{m1}}{dt} &= \phi_1 P - k_1 (c_1 - C_{\mathrm{eq}}) - K_{12} (c_1 - c_2) + k_{\mathrm{res}} C_{p1} \\
\frac{dC_{m2}}{dt} &= \phi_2 P - (k_2 + k_{23}) (c_2 - C_{\mathrm{eq}}) + K_{12} (c_1 - c_2) + k_{\mathrm{res}} (C_{p2} + C_{gb}) \\
\frac{dC_{p1}}{dt} &= k_1 (c_1 - C_{\mathrm{eq}}) - k_{\mathrm{res}} C_{p1} \\
\frac{dC_{p2}}{dt} &= k_2 (c_2 - C_{\mathrm{eq}}) - k_{\mathrm{res}} C_{p2} \\
\frac{dC_{gb}}{dt} &= k_{23} (c_2 - C_{\mathrm{eq}}) - k_{\mathrm{res}} C_{gb}
\end{aligned} $$

solved with backward Euler after production and nucleation (`Solver::Laplace`). Every column of the matrix sums
to one: the total metal is conserved.

### Radius

$$ R_j = \left( \frac{3 \phi_{\mathrm{met}} C_{pj} \Omega}{4\pi N_j} \right)^{1/3}, \qquad \Omega = 1.44123\times10^{-29}\ \mathrm{m^3/at}, $$

zero while $N_j < 1$ FMP/m3; $\phi_{\mathrm{met}}$ is the metallic part of the inventory (below, 1 without
thermochemistry).

### Coupling with OpenCalphad at the grain boundary (EC November 2026)

With `iThermochimica` active, OpenCalphad at the grain boundary receives, for each metal, the gb inventory of the
model, $n_X^{\mathrm{OC}} = x_X C_{gb}$. `JOGFormation` stores the fraction of this Mo in oxide phases,
$f_{\mathrm{ox}}$ (`Mo in oxide fraction`); the oxygen potential is uniform in the grain, so $f_{\mathrm{ox}}$ holds for
all the particles, and the metallic share of the inventory (one time step of lag) is
$\phi_{\mathrm{met}} = 1 - x_{\mathrm{Mo}} f_{\mathrm{ox}}$. The oxidised Mo stays in the particle inventory. A step
where OpenCalphad gives no result keeps the previous $f_{\mathrm{ox}}$.

### Oxygen balance between the fuel and the Mo oxides (EC November 2026)

UO2 with `iStoichiometryDeviation = 7` (Kleykamp 1979, x increasing by 0.0013 per % FIMA), standalone SCIANTIX
(`SetPhaseDiagram.C`). The excess oxygen E (at/m3), released by fission and not taken by the other oxide-forming
fission products, is shared between the fuel and the Mo of the particles:

$$ E = x M + O_{\mathrm{ox}}, \qquad \mathrm{Mo}_p = x_{\mathrm{Mo}} (C_{p1} + C_{p2} + C_{gb}), \qquad
\mathrm{Mo}_{gb} = x_{\mathrm{Mo}} C_{gb}, $$

with M the heavy-metal atoms per unit volume.

- $E < 2\,\mathrm{Mo}_p$: the Mo/MoO2 equilibrium buffers the fuel. OpenCalphad at the grain boundary is solved
  with the oxygen $E\,\mathrm{Mo}_{gb}/\mathrm{Mo}_p$ and gives $\mu_\mathrm{O}$; x from the inverse of the Blackburn
  relation (`Solver::NewtonBlackburn`),

  $$ \ln\frac{p_{\mathrm{O_2}}}{1\ \mathrm{atm}} = 2\ln\frac{x(2+x)}{1-x} + 108 x^2 - \frac{32700}{T} + 9.92
  = \frac{2\mu_\mathrm{O}}{RT} + \ln\frac{1\ \mathrm{bar}}{1\ \mathrm{atm}}, \qquad
  x \le \frac{E}{M}, \qquad O_{\mathrm{ox}} = E - x M; $$

- $E \ge 2\,\mathrm{Mo}_p$: all the Mo of the particles is MoO2, $O_{\mathrm{ox}} = 2\,\mathrm{Mo}_p$,
  $x = (E - 2\,\mathrm{Mo}_p)/M$; OpenCalphad is solved at the oxygen potential of the fuel.

Then the stoichiometry deviation, oxygen partial pressure and potential of the fuel are updated with x
(Blackburn), and $\Delta O_{\mathrm{ox}}$ is removed from `O content`. Only the Mo of the particles takes oxygen.

### Parameters

None is fitted to the TEM data of the validation.

| Parameter | Value | Source |
|---|---|---|
| D | $D_1 + D_2 + D_3$ | Turnbull et al. (1988), single-atom Xe diffusivity used as a surrogate (no Mo data found) |
| gamma (metal/UO2) | 1.0 J/m2 | incoherent interfaces 0.5-1 J/m2 (Porter and Easterling); no Mo/UO2 value found |
| $\gamma_s$, $\theta_b$ | 0.7 J/m2, 50 deg | UO2 matrix in SCIANTIX |
| delta | 100 nm | bubble-free zone on each side of the boundaries (Angleraud 2024) |
| a (TEM cases) | 4.4 um | mean grain size 8.8 um (Angleraud 2024, ch. 2) |
| $\Omega$ | 1.44123e-29 m3/at | atomic radii, hcp packing (V. Bellani) |
| $n_{\min}$ | 2 at | stable metallic dimers (Hong et al. 2012) |
| $\rho_d$ | $6.545\times10^{12} b^{1.151} f_T$ | Veshchunov and Shestak (2009) |
| lambda | 15 nm | V. Bellani |
| $f_d$, $f_b$ | 0.67, 0.33 | V. Bellani, image analysis |
| $k_{\mathrm{res,ref}}$ | 3.085078e-6 1/s at 1.48e19 fiss/(m3 s) | Sakib et al. (2025) |
| $x_{\mathrm{eq}}$, H, $T_{\mathrm{ref}}$ | 6e-5, 1.14 eV, 2173.15 K | Kleykamp (1985), Brillant et al. (2009) |

lambda, $f_d$ and $f_b$ come from the images of the same fuel (geometry and EDX of bubble/precipitate pairs), not
from a fit of R or A.

### Order in the time step

`FiveMetalParticles` runs after `FissionProductProduction` and `FissionProductDecay`; with thermochemistry,
`SetPhaseDiagram` then solves OpenCalphad at the grain boundary with the oxygen balance, and `JOGFormation`
updates $f_{\mathrm{ox}}$ for the next time step.

### Outputs (`iFMP = 1`)

| Output | Zone |
|---|---|
| `FMP in matrix (at/m3)` | dissolved metal, intra1 + intra2 |
| `FMP in near-boundary zone matrix (at/m3)` | dissolved metal, intra2 |
| `FMP in intragranular particles (at/m3)`, `Intragranular FMP concentration (FMP/m3)`, `Intragranular FMP radius (m)` | intra1 |
| `FMP in near-boundary particles (at/m3)`, `Near-boundary FMP concentration (FMP/m3)`, `Near-boundary FMP radius (m)` | intra2 |
| `FMP at grain boundary (at/m3)`, `Intergranular FMP concentration (FMP/m3)`, `Intergranular FMP radius (m)` | gb |
| `FMP near-boundary zone volume fraction (/)` | $\phi_2$ |
| `FMP oxygen in Mo oxides (at/m3)` | $O_{\mathrm{ox}}$, with thermochemistry |

## Cases

| Group | Cases | Purpose |
|---|---|---|
| `--metallicinclusions` | `test_FMP_UO2_r000/r030/r056/r075` | validation: TEM data at four radial positions, 54.1 MWd/kgUO2 |
| `--metallicinclusions` | `test_FMP_UO2_Buck2015_r035/r061/r079/r094` | validation: ATM-109, Buck et al. (2015) |
| `--metallicinclusions-oc` | `thermochemistry/test_FMP_UO2_OC_r000/r030/r056/r075` | TEM cases with OpenCalphad at the grain boundary |

## Validation on the TEM data (54.1 MWd/kgUO2)

Standard UO2 irradiated five cycles in a PWR to 61.4 GWd/tU (54.1 MWd/kgUO2), TEM of Angleraud (2024), image
analysis of V. Bellani (thesis 2026). The four cases share settings and initial conditions; only the
temperature history changes.

| Case | r/R | Temperature (K) | TEM foil thickness (nm) |
|---|---|---|---|
| `test_FMP_UO2_r000` | 0.00 | 1111-1210 | 50 (nominal) |
| `test_FMP_UO2_r030` | 0.30 | 1068-1158 | 50 (nominal) |
| `test_FMP_UO2_r056` | 0.56 | 961-1029 | 70 |
| `test_FMP_UO2_r075` | 0.75 | 842-886 | 40 |

Common conditions: 30000 h, same fission-rate history at the four positions (1.31-1.60e19 fiss/(m3 s)),
grain radius 4.4 um, density 10421.5 kg/m3, 4% U-235, no helium, no HBS, stoichiometric UO2
(`iStoichiometryDeviation = 0`), `iThermochimica = 0`.

Comparison:

- the TEM intragranular data with intra1, density $n = N_1/\phi_1$;
- the TEM grain-boundary data, taken in strips about 200 nm wide across the boundaries, with the strip
  intra2 + gb: $n = (N_2 + N_{gb})/\phi_2$, $R = (N_2 R_2 + N_{gb} R_{gb}) / (N_2 + N_{gb})$;
- projected area fraction $A = 1 - \exp(-\pi t \sum_j n_j R_j^2)$, t the foil thickness;
- J = mean |ln(model/exp)| over R and A of the two regions at the four positions (16 observables).

| Case | R intra exp / model (nm) | R strip exp / model (nm) | A intra exp / model (%) | A strip exp / model (%) | R_intra2, R_gb (nm) | Metal dissolved |
|---|---|---|---|---|---|---|
| r000 | 7.58 / 3.26 | 14.28 / 2.15 | 1.37 / 13.9 | 7.21 / 2.33 | 3.34, 1.79 | 4.1% |
| r030 | 3.59 / 3.15 | 11.95 / 2.15 | 0.56 / 14.3 | 3.62 / 2.43 | 3.23, 1.75 | 4.6% |
| r056 | 3.45 / 3.07 | 5.58 / 2.13 | 3.25 / 19.6 | 1.76 / 3.38 | 3.16, 1.72 | 5.8% |
| r075 | 3.10 / 3.28 | 6.68 / 2.15 | 2.62 / 10.8 | 0.72 / 1.84 | 3.36, 1.80 | 7.3% |

J = 1.17. Figure: `figures/metallicinclusions_UO2_LWR.png`.

- intra1: R within 15% of the TEM value at r/R = 0.30-0.75, 2.3 times too small at the centre. The density is
  4-60 times too high (model 8.5e22-1.05e23 /m3, TEM 1.5e21-2.2e22 /m3 from A and R), so A is 3-25 times too
  high. About half of the sites nucleate, and 92-95% of the metal is in intra1 particles at the end.
- Strip: R 2.6-6.6 times too small, density 12-24 times too high. All the gb sites nucleate early
  (1.52e21 /m3) at the lower barrier and stay at about 1.8 nm; intra2 holds 1.1-1.3% of the metal in 6.7% of the
  volume, because it has fewer sinks than intra1 (no bubble sites) and its metal diffuses into intra1.
- The radial trend of the TEM data (larger particles at the centre) is not reproduced: the radii depend little
  on the temperature.

Sensitivity to the only free literature value, gamma (J/m2): J = 1.32 (0.8), 1.17 (1.0), 0.94 (1.2),
0.97 (1.33); the strip radius stays at 2-5 nm. gamma = 1.33 J/m2 gives the B of V. Bellani (2.67e8 eV K2).

## Validation on Buck et al. (2015), ATM-109

E.C. Buck et al., *Nanostructure of metallic particles in light water reactor used nuclear fuel*,
J. Nucl. Mater. 461 (2015) 236-243. ATM-109, Quad Cities I BWR: GE UO2, 3.0% U-235, initial grain size 30 um,
maximum burnup of the specimens 70.80 GWd/MTU, in reactor from February 1979 to September 1987 and from November
1989 to September 1992. The particles were recovered from the residues of two dissolutions (carbonate-peroxide
and nitric acid) and examined by SEM/TEM: diameters of about 10-300 nm, mean size 43 nm (carbonate-peroxide)
and 58 nm (nitric), hcp epsilon-Ru structure.

The paper gives no power or temperature history, so the cases use an **assumed** history:

- in reactor continuously over the two periods (100056 h), out of the reactor in between (300 K, no fissions);
- constant fission rate 5.003e18 fiss/(m3 s), 62.41 MWd/kgUO2 = 70.8 GWd/tHM at the end (upper bound);
- four equal-volume rings, (r/R)^2 = 0.125, 0.375, 0.625, 0.875, parabolic profile from 900 degC at the centre
  to 450 degC at the surface; grain radius 15 um, 3.0% U-235.

| Case | r/R | Temperature (K) | d_intra1 / d_intra2 / d_gb (nm) | Metal dissolved |
|---|---|---|---|---|
| `test_FMP_UO2_Buck2015_r035` | 0.354 | 1117 | 7.2 / 7.3 / 3.8 | 3% |
| `test_FMP_UO2_Buck2015_r061` | 0.612 | 1004 | 6.5 / 6.6 / 3.6 | 5% |
| `test_FMP_UO2_Buck2015_r079` | 0.791 | 892 | 6.6 / 6.7 / 3.6 | 6% |
| `test_FMP_UO2_Buck2015_r094` | 0.935 | 779 | 7.3 / 7.4 / 3.9 | 8% |

The comparison uses the pellet-averaged population (equal ring volumes, the three populations weighted by
number). Fig. 6 of the paper (cumulative number distribution of the particle volume) was digitized
(`data/buck2015/fig6_*.txt`, approximate) and fitted with log-normals of ln V (`data/buck2015/summary.txt`).

| | Median d (nm) | Number-mean d (nm) | Mean ln V (nm3) |
|---|---|---|---|
| Buck et al., carbonate-peroxide | 13 | 50 (reported 43) | 7.07 |
| Buck et al., nitric acid | 28 | 62 (reported 58) | 9.30 |
| SCIANTIX | 6.6 | 6.8 | 5.10 |

The model particles are 7-9 times too small in number-mean diameter, consistent with the TEM cases. The size
distribution is much narrower than the measured one (one size per ring and population) and the large particles
(100-300 nm) are missing. The particle composition of the model (42Mo-11Tc-25Ru-8Rh-14Pd at.%) is richer in Mo
than the dissolution residue (20Mo-0.5/5Tc-50/60Ru-10Rh-10Pd). Figure: `figures/buck2015_ATM109.png`.

## Thermochemistry cases

`thermochemistry/test_FMP_UO2_OC_*`: the TEM cases with `iThermochimica = 2` (OpenCalphad at every time step),
`iStoichiometryDeviation = 7` (Kleykamp 1979, same x(burnup) at the four positions) and the Blackburn oxygen
potential, system pressure 1e5 Pa. OpenCalphad runs at the grain boundary with
`BaMoO_CsMoO_MoPdRhRuTc_merged.TDB` (Mo, O, Pd, Rh, Ru, Tc). The group is skipped when the OpenCalphad library
or the database is missing.

| Case | mu_O2 (kJ/mol) | x | Mo oxidised | R intra1 with / without OC (nm) | R strip with / without OC (nm) | Steps with an OC result |
|---|---|---|---|---|---|---|
| r000 | -376 | 1.3e-5 | 29% | 3.08 / 3.26 | 2.07 / 2.15 | 498 / 501 |
| r030 | -256 | 8.4e-3 | 84% | 2.61 / 3.15 | 1.86 / 2.15 | 22 / 501 |
| r056 | -404 | 9.1e-7 | 29% | 2.90 / 3.07 | 2.04 / 2.13 | 498 / 501 |
| r075 | -260 | 8.0e-3 | 100% | 2.61 / 3.28 | 1.80 / 2.15 | 40 / 501 |

At r000 and r056 the fuel is buffered at Mo/MoO2 and x stays far below the Kleykamp value (8.7e-3). At r030 and
r075 OpenCalphad stops converging after 2.6 and 4.5 MWd/kgUO2: the oxygen of the gb particles approaches
O/Mo = 2, the oxygen in the oxides stays at its last value and x follows the Kleykamp relation; the end values
of these two cases are not OpenCalphad results. Figure: `figures/fmp_thermochemistry.png`.

## Data

- `data/intra/exp_intra_<case>.txt`, `data/inter/exp_inter_<case>.txt`: one row per position with the projected
  area fraction (`Inclusions`, -), mean radius and its standard deviation (um), and `SP`. TEM images of
  Angleraud (2024), image analysis of V. Bellani (thesis 2026).
- `data/buck2015/`: digitized Fig. 6 and summary of Buck et al. (2015).

## Run

```bash
python3 -m testing.runner --metallicinclusions             # run + compare with output_gold.txt
python3 -m testing.runner --metallicinclusions-oc --oc     # needs OpenCalphad
python3 validation/metallicinclusions/plot.py              # tables and figures in figures/
python3 validation/metallicinclusions/check_exchange.py    # check of K_12, figures/exchange_check.png
```

`plot.py` writes:

- `metallicinclusions_UO2_LWR.png`: TEM cases, radius and projected area fraction, J;
- `buck2015_ATM109.png`: Buck et al. (2015), particle volume distribution, diameter per ring, growth in time;
- `fmp_partition.png`: produced metal split into dissolved (intra1, intra2) and particles (intra1, intra2, gb)
  vs burnup;
- `fmp_elements.png`: production of the five elements and particle composition without and with OpenCalphad;
- `fmp_thermochemistry.png`: oxygen potential with the oxygen balance and with Kleykamp only, x, Mo oxide
  fraction, grain-boundary phases, strip radius with and without OpenCalphad, share of the excess oxygen in the
  Mo oxides.

## Known limits

- Too many and too small particles in all zones (J = 1.17): the CNT barrier with gamma = 1 J/m2 is low and the
  nucleation sites fill up. The large particles on and near the boundaries seen by TEM and the radial trend are
  not reproduced.
- gamma, D for Mo in UO2 and the nucleation sites (lambda, $f_d$, $f_b$) are the weakest inputs: no direct data
  for the metal/UO2 interface and Mo diffusion.
- intra2 is well mixed, with a fixed thickness (100 nm); gb has no volume and no grain-boundary diffusion.
- One mean size per population: the measured size distribution (Buck et al., 10-300 nm) cannot be reproduced.
- The five metals are one lumped species: the particles have the production composition, apart from the oxidised
  Mo, while the measured particles are poorer in Mo and richer in Ru.
- TEM cases: 4% U-235 and 10421.5 kg/m3 (about 5% porosity), while the fuel of Angleraud has 4.48% U-235 and
  3.95% porosity (not changed).
- Oxygen balance: only the Mo of the particles takes oxygen, the excess oxygen comes from the Kleykamp relation
  (no radial O/M profile), Cs and Ba are not in the UO2 systems; OpenCalphad does not converge at r030 and r075
  after a few MWd/kgUO2.
- The MOX systems keep fast-reactor yields: the model is validated only for UO2 in LWR conditions.

## References

- M. Angleraud, *Etude par microscopie électronique en transmission des mécanismes de grossissement/croissance
  des bulles de gaz de fission dans le combustible nucléaire sous irradiation*, PhD thesis, Université de
  Toulouse, 2024, https://theses.hal.science/tel-05351500.
- V. Bellani, *Modelling of nucleation and growth of metallic fission product inclusions in irradiated UO2 fuel
  with neutronics-based source terms for SCIANTIX implementation*, MSc thesis, Politecnico di Milano, 2026.
- A.D. Brailsford, R. Bullough, *The theory of sink strengths*, Phil. Trans. R. Soc. Lond. A 302 (1981) 87-137,
  https://doi.org/10.1098/rsta.1981.0158.
- G. Brillant, F. Gupta, A. Pasturel, J. Phys.: Condens. Matter 21 (2009) 285602,
  https://doi.org/10.1088/0953-8984/21/28/285602.
- E.C. Buck et al., J. Nucl. Mater. 461 (2015) 236-243.
- F.S. Ham, *Theory of diffusion-limited precipitation*, J. Phys. Chem. Solids 6 (1958) 335-351.
- M. Hong et al., Phys. Rev. B 85 (2012) 144110, https://doi.org/10.1103/PhysRevB.85.144110.
- D. Kashchiev, *Nucleation: Basic Theory with Applications*, Butterworth-Heinemann, 2000.
- H. Kleykamp, *The chemical state of the fission products in oxide fuels*, J. Nucl. Mater. 131 (1985) 221-246,
  https://doi.org/10.1016/0022-3115(85)90460-X.
- D.A. Porter, K.E. Easterling, *Phase Transformations in Metals and Alloys*, Chapman & Hall.
- S. Sakib et al., *In-situ ion irradiation of fission products in a spent UO2 fuel*, J. Nucl. Mater. 614 (2025)
  155859, https://doi.org/10.1016/j.jnucmat.2025.155859.
- J.A. Turnbull, R.J. White, C. Wise, IWGFPT-32, Preston, UK, 1988.
- M.S. Veshchunov, V.E. Shestak, J. Nucl. Mater. 384 (2009) 12-18, https://doi.org/10.1016/j.jnucmat.2008.09.024.
