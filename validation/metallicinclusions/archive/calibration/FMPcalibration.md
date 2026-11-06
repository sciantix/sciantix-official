# Calibration of the five metal particles (FMP) model

English translation of the calibration report of V. Bellani (`MFPcalibration.md` on
`development/metallicinclusions`).

> **EC November 2026.** This report documents how the calibrated values were obtained. Since the port to
> SCIANTIX 2.5 the model has no scaling factors: the calibrated values are written in
> `src/models/FiveMetalParticles.C`, and the calibration script (`calibrate_5MP.py`) was removed with
> the `sf_mfp_*` keys. Sections 4-6, 16, 18 and 19 describe a preliminary four-parameter set-up. The final
> calibration, whose values are in the code, has **two common free parameters**:
>
> | Parameter | Meaning | Range, sampling | Best value |
> |---|---|---|---|
> | $k_{intra,0} = k_{GB,0}$ | precipitation prefactor (m²/s) | 2.5e-15 to 8.0e-15, log | 4.980333299845e-15 |
> | $B_{intra} = B_{GB}$ | CNT coefficient, $B_{ref}\,sf_B$ with $B_{ref}$ = 1e6 eV K² | $sf_B$ 245 to 285, linear | 2.665764962509e8 eV K² |
>
> with $\Delta G_{intra} = \Delta G_{GB} = 1$ eV and $k_{nucl,0}$ = 1 s⁻¹ (512 Sobol points). It gives J = 0.614
> with the lumped yield 0.578 at/fiss and J = 0.611 with the per-element LWR yields (EC November 2026).
> The objective function, the observables and the TEM thicknesses (sections 7-15) are unchanged and are
> computed by `validation/metallicinclusions/plot.py`.

## 1. Purpose

The model is calibrated simultaneously at the four radial positions

$$ r/R = 0;\ 0.30;\ 0.56;\ 0.75 $$

to find a single parameter set that describes the precipitate populations in the different thermal and
microstructural conditions along the pellet radius. The calibration is a global problem: every parameter
combination is used, unchanged, in four SCIANTIX simulations, one per radial position. A parameter set is
judged on how well it describes the whole data set, not one position.

## 2. Model parameters

The model includes intragranular nucleation, intragranular precipitation, grain-boundary nucleation,
grain-boundary precipitation and irradiation-induced re-solution. The temperature-dependent kinetic
coefficients have Arrhenius form

$$ k(T)=k_0\exp\left(-\frac{\Delta G}{k_BT}\right) $$

with $k_0$ the prefactor, $\Delta G$ the activation energy (eV), $k_B$ the Boltzmann constant and $T$ the
absolute temperature. For precipitation the prefactor is an effective transport coefficient,
$[k_{intra,0}]=[k_{GB,0}]=\mathrm{m^2\,s^{-1}}$; the nucleation prefactors are in $\mathrm{s^{-1}}$.

In the original implementation the parameters were multiplicative factors on unit reference values, so the
factor had the numerical value of the parameter in its units, e.g.
$k_{intra,0}=\underbrace{1}_{\mathrm{m^2/s}}\times sf_{k,intra}$ and
$\Delta G_{intra}=\underbrace{1}_{\mathrm{eV}}\times sf_{\Delta G,intra}$. They are effective kinetic
parameters of the model, not necessarily measurable microscopic coefficients.

Two physical quantities were fixed and not calibrated:

- the effective lumped production yield of Mo, Tc, Ru, Rh and Pd, $y_{\mathrm{5MP}}=0.578$ at/fiss
  (an aggregated effective yield, not a universal independent fission yield). *EC November 2026: replaced
  by the per-element LWR yields of the Mo, Tc, Ru, Rh, Pd systems (total 0.5666 at/fiss).*
- the reference re-solution coefficient $k_{res,ref}=3.085078\times10^{-6}\,\mathrm{s^{-1}}$, scaled as
  $k_{res}=k_{res,ref}(F/F_{ref})$ with $F_{ref}=1.48\times10^{19}\,\mathrm{fiss\,m^{-3}\,s^{-1}}$.

## 3. Nominal configuration

Before calibrating, the nominal configuration with all factors equal to one ($sf_{k,intra}$, $sf_{k,GB}$,
$sf_{k,nucl}$, $sf_{res}$, $sf_{\Delta G,nucl}$, $sf_{\Delta G,intra}$, $sf_{\Delta G,GB}$,
$sf_{\Delta G,nucl,GB}$, $sf_{k,nucl,GB}$ = 1) is evaluated. It is not a calibrated set but the reference
for the improvement. With the final formulation ($y_{\mathrm{5MP}}=0.578$ and the corrected $k_{res,ref}$)
it gives

$$ J_{baseline}=1.627431 $$

## 4. Calibrated parameters (preliminary four-parameter set-up)

A diagnostic screening of the sensitivity of the observables and of the identifiability of the
coefficients showed strong correlations:

- $k_{intra,0}$ and $k_{GB,0}$ give strongly correlated responses;
- $\Delta G_{intra}$ and $\Delta G_{GB}$ show a similar correlation;
- $k_{nucl,GB,0}$ changes some internal variables of the GB population but the objective function only
  negligibly;
- $\Delta G_{nucl,GB}$ belongs to the same GB nucleation channel, poorly observable with the available targets;
- the re-solution coefficient is kept at its independently derived reference value.

To avoid calibrating poorly identifiable directions together, this set-up had four free parameters:

| Free parameter | Meaning | Unit |
|---|---|---|
| $k_{GB,0}$ | precipitation prefactor at the grain boundary | $\mathrm{m^2/s}$ |
| $k_{nucl,0}$ | intragranular nucleation prefactor | $\mathrm{s^{-1}}$ |
| $\Delta G_{nucl}$ | intragranular nucleation energy barrier | eV |
| $\Delta G_{GB}$ | activation energy of precipitation at the grain boundary | eV |

and fixed $sf_{k,intra}$, $sf_{\Delta G,intra}$, $sf_{k,nucl,GB}$, $sf_{\Delta G,nucl,GB}$, $sf_{res}$ = 1. The
unit value of the fixed parameters is a normalisation of the model, not a proof that the data identify
those values.

## 5. Calibration ranges

The ranges come from the diagnostic screening. Prefactors are sampled on a log scale and energies linearly:

| Parameter | Min | Max | Sampling |
|---|---|---|---|
| $k_{GB,0}$ | $1$ | $10^{2}$ | log |
| $k_{nucl,0}$ | $10^{-6}$ | $10^{-4}$ | log |
| $\Delta G_{nucl}$ | 0.50 eV | 1.50 eV | linear |
| $\Delta G_{GB}$ | 0.75 eV | 1.50 eV | linear |

The screening of the intragranular nucleation prefactor showed a non-monotonic objective function:

| $k_{nucl,0}$ | $J$ |
|---|---|
| $10^{-2}$ | 1.430716 |
| $10^{-3}$ | 1.256555 |
| $10^{-4}$ | 1.029047 |
| $10^{-5}$ | 0.985899 |
| $10^{-6}$ | 1.204709 |

so the range $10^{-6}$-$10^{-4}$ contains the most promising region without extending to orders of
magnitude that performed worse. For $k_{GB,0}$ the diagnostics improved from the nominal value towards
larger values, with a useful region within $1$-$10^{2}$. The energy ranges are wide enough to include the
favourable regions of the diagnostics and to allow a compromise between the four positions. The nominal
configuration is always evaluated separately as reference.

## 6. Sobol sampling

The four-dimensional space is explored with a scrambled Sobol low-discrepancy sequence (seed 42), which
covers a multidimensional space more uniformly than random sampling. It is used only as a sampling
technique, not as a Sobol-index sensitivity analysis. With $N_{Sobol}=4096=2^{12}$ points plus the nominal
configuration, $N_{set}=4097$ sets are evaluated, each at the four positions:
$N_{runs}=4(N_{Sobol}+1)=16388$ SCIANTIX runs.

## 7. Experimental data

The TEM micrographs are segmented. For each precipitate population the analysis gives the area fraction
occupied by the inclusions, the mean equivalent radius, the standard deviation of the radius distribution
and the shape factor. The experimental fraction `Inclusions` is defined in the image-processing code as

$$ A_A^{exp} = \frac{A_{FMP}}{A_{image}} $$

i.e. the fraction of image pixels classified as metallic precipitates. It is an area fraction measured on
the micrograph, not a volume fraction from the total metallic fission-product inventory.

## 8. Intragranular population

SCIANTIX gives the particle concentration $N_{intra}$, the precipitated metal and the mean number of atoms
per particle (*EC November 2026: now the radius directly*). The mean equivalent radius assumes spherical
particles,

$$ R_{intra} = \left( \frac{ 3n_{intra}V_{eff} }{ 4\pi } \right)^{1/3}, \qquad V_{eff} = 1.44123\times10^{-29}\ \mathrm{m^3/atom}. $$

### 8.1 Projected area fraction

TEM observations are a projection through the thickness of the foil, so the modelled volume fraction is not
compared directly with the experimental area fraction. With $N_{intra}$, $R_{intra}$ and the local foil
thickness $t$, $\lambda_{intra} = N_{intra} \pi R_{intra}^{2} t$; for low coverage
$A_{A,intra}^{model} \simeq N_{intra}\pi R_{intra}^{2}t$. To account to first order for overlapping
projections,

$$ A_{A,intra}^{model} = 1- \exp \left( - N_{intra} \pi R_{intra}^{2} t \right), $$

which also guarantees $0 \leq A_{A,intra}^{model} \leq 1$. $A_{A,intra}^{model}$ is compared directly with
$A_{A,intra}^{exp}$. This is a projection operator chosen to make the modelled volumetric population
comparable with the coverage observed on the micrograph, not a general stereological relation between area
and volume fractions.

## 9. TEM foil thickness

| $r/R$ | TEM thickness |
|---|---|
| 0 | 50 nm |
| 0.30 | 50 nm |
| 0.56 | 70 nm |
| 0.75 | 40 nm |

For $r/R=0.56$ and $0.75$ the thicknesses of the available experimental conditions are used. For $r/R=0$ and
$0.30$, without a local measurement for the analysed region, a nominal 50 nm is used. This assumption is kept
explicit when interpreting the results, since the thickness directly affects the projected coverage.

## 10. Grain-boundary precipitates

In this set-up, intergranular nucleation used the intergranular bubbles as preferential heterogeneous sites.
In SCIANTIX the intergranular bubble concentration is an areal density of the grain boundary,
$N_{b,GB}^{A}$ (m⁻²), while the FMP model uses a volumetric particle concentration $N_{GB}$ (m⁻³). The areal
density was converted with the grain-boundary surface-to-volume ratio of SCIANTIX, $S_{GB}/V = 3/(2a)$
($a$ grain radius): $N_{sites,GB} = N_{b,GB}^{A}\,3/(2a)$, ($\mathrm{m^{-2}}\cdot\mathrm{m^{-1}}=\mathrm{m^{-3}}$), and
the source term was $S_{GB} = N_{sites,GB}\,k_{nucl,GB,0}\exp(-\Delta G_{nucl,GB}/k_BT)$.

*EC November 2026: in the final model the grain-boundary sites are $3/(2a\lambda^2)$ with $\lambda$ = 15 nm,
independent of the intergranular bubbles (see `docs/source/models/five_metal_particles.rst`).*

The experimental GB area fraction needs a separate treatment. The GB micrographs are selected on purpose at
grain boundaries, so the measured quantity is the fraction of the region of interest occupied by
precipitates, not the fractional coverage of the boundary surface. Converting the modelled volumetric
concentration into this observable would need the geometry and orientation of the boundary in the foil. To
avoid a geometric relation that the data do not constrain, the GB area fraction is a diagnostic only and is
not in the objective function.

## 11. Grain-boundary precipitate radius

The mean radius of the intergranular precipitates is used in the calibration:
$n_{GB} = C_{prec,GB}/N_{GB}$ and $R_{GB} = (3n_{GB}V_{eff}/4\pi)^{1/3}$, compared with the equivalent radius
from the micrographs. Sphericity is a simplification: metallic precipitates can be irregular, faceted or
elongated, especially at grain boundaries. The comparison is in terms of equivalent radius.

## 12. Observables

At each radial position three experimental observables are used: the intragranular area fraction, the
intragranular mean equivalent radius and the grain-boundary mean equivalent radius. The GB area fraction is a
diagnostic only. With 3 observables at 4 positions, $N_{obs} = 3\times4 = 12$.

## 13. Error definition

The error must compare positive observables with different units and magnitudes. The conventional relative
error $e_i^{rel}=|(M_i-E_i)/E_i|$ ($M_i$ model, $E_i$ experiment) is not used: the experimental value in the
denominator lets small targets dominate, and the error is asymmetric, $M=2E$ gives $e^{rel}=1$ while
$M=E/2$ gives $0.5$, although both differ by a factor of two. The absolute log ratio is used instead,

$$ \boxed{e_i=\left|\ln\left(\frac{M_i}{E_i}\right)\right|} $$

which is dimensionless and symmetric: $M=2E$ and $M=E/2$ both give $|\ln 2|$, and in general $M=\alpha E$
gives $|\ln\alpha|$. The error depends on the multiplicative factor between model and experiment, without
overweighting small targets, and keeps growing for strongly discordant predictions. All the quantities are
strictly positive, as required.

## 14. Global objective function

$$ \boxed{J=\frac{1}{12}\sum_{j=1}^{4}\left[\left|\ln\left(\frac{A_{A,intra,j}^{model}}{A_{A,intra,j}^{exp}}\right)\right|+\left|\ln\left(\frac{R_{intra,j}^{model}}{R_{intra,j}^{exp}}\right)\right|+\left|\ln\left(\frac{R_{GB,j}^{model}}{R_{GB,j}^{exp}}\right)\right|\right]} $$

Each position contributes three terms and the 12 observables have the same explicit weight. The set with
the minimum $J$ among the sampled points is the best calibrated set. $J$ is not a percentage error: it is the
mean absolute log discrepancy, $J=0$ is perfect agreement, and $\exp(J)$ is the geometric mean of the
multiplicative factors of the single terms. $J$ is always analysed together with the single error terms and
the radial trends, so that one synthetic number does not hide local discrepancies.

## 15. Diagnostic quantities

For each simulation, besides the observables in $J$, the following are stored: the experimental GB area
fraction, the GB coverage indicator, the modelled intragranular and GB volume fractions, the intragranular
and GB precipitated metal, the intragranular and intergranular particle concentrations, the mean atoms per
particle of both populations and the burnup actually selected in the SCIANTIX output. The modelled volume
fractions are $f_{V,intra} = C_{prec,intra}V_{eff}$ and $f_{V,GB} = C_{prec,GB}V_{eff}$; they are not compared with
the experimental area fraction but check the metallic inventory. The GB coverage indicator is a diagnostic
only: it is not strictly equivalent to the area fraction measured on the TEM region and is not in $J$.

## 16. Selection of the best set

For each Sobol point: (1) generate the free parameters; (2) set the fixed ones to their reference values;
(3) write the input files; (4) run the four SCIANTIX simulations; (5) select the output point at the
experimental burnup; (6) compute the modelled quantities; (7) compute the 12 log discrepancies; (8) compute
$J$; (9) store the diagnostics. The combination with the minimum $J$ is selected. The result is

$$ \boxed{\text{the best set among the sampled points}} $$

not necessarily the continuous global minimum of the objective function. The low discrepancy of the Sobol
sequence gives uniform coverage of the domain; more points increase the resolution of the exploration.

## 17. Post-calibration analysis

The best set is not judged on $J$ alone. The following are analysed separately: the log discrepancy of the
intragranular area fraction, of the intragranular radius and of the GB radius; the radial trend of the three
observables; the diagnostic behaviour of the GB population; the intragranular and GB precipitated
concentrations; the number of particles; the mean atoms per particle; and the position of the best set with
respect to the limits of the parameter space. The last check is particularly important: an optimum close to
a range limit may indicate that the space does not contain the whole optimal region, while an interior
solution with a good description of the single observables is a more robust indication of the quality of the
calibration.

## 18. Sensitivity and identifiability

The reduction to few free parameters comes from the preliminary screening, not only from the search for the
minimum. For $k_{nucl,0}$, the variation over several orders of magnitude showed a minimum within the range
used in the calibration. The two precipitation channels are strongly correlated: reciprocal variations of
$k_{intra,0}$ and $k_{GB,0}$ gave practically identical objective functions,

$$ sf_{k,intra}=10^{-1}\;\Longleftrightarrow\;sf_{k,GB}=10\qquad J=1.261132 $$

$$ sf_{k,intra}=10^{-2}\;\Longleftrightarrow\;sf_{k,GB}=10^{2}\qquad J=1.388780 $$

$$ sf_{k,intra}=10^{-3}\;\Longleftrightarrow\;sf_{k,GB}=10^{3}\qquad J=1.739208 $$

A similar correlation holds between $\Delta G_{intra}$ and $\Delta G_{GB}$:

$$ \Delta G_{intra}=0.5\;\mathrm{eV}\;\Longleftrightarrow\;\Delta G_{GB}=1.5\;\mathrm{eV}\qquad J=1.271638 $$

while the opposite combination gives

$$ \Delta G_{intra}=1.5\;\mathrm{eV}\;\Longleftrightarrow\;\Delta G_{GB}=0.5\;\mathrm{eV}\qquad J=1.945598 $$

These equivalences show that the data cannot identify both parameters of each pair independently. In the
four-parameter set-up $k_{intra,0}$ and $\Delta G_{intra}$ were kept at the reference value, with $k_{GB,0}$ and
$\Delta G_{GB}$ free (in the final set-up, both prefactors and both barriers are tied to common values).

The GB nucleation prefactor has almost no effect on the objective function:

| $sf_{k,nucl,GB}$ | $J$ |
|---|---|
| $10^{-2}$ | 1.627455 |
| $10^{-4}$ | 1.627614 |
| $10^{-6}$ | 1.627661 |

Although it changes the internal GB population, the effect on the targets is negligible, so it is considered
poorly identifiable and fixed to one, together with its energy barrier. A good objective function does not
imply that each parameter is identified independently; after the final calibration the best sets, their
distribution in the domain and their position with respect to the range limits are analysed as well.

## 19. Summary of the procedure

$$ \boxed{\text{nominal configuration}\rightarrow\text{diagnostic screening}\rightarrow\text{reduction of the free parameters}\rightarrow\text{Sobol points}\rightarrow\text{4 simulations per set}\rightarrow\text{12 model-experiment comparisons}\rightarrow J\rightarrow\text{best set}} $$

The calibration is global over the four radial positions and uses a single parameter set for the whole
data set. It does not calibrate the production yield, the re-solution coefficient or the parameters fixed
during the screening: these are defined by the final formulation of the model and by the normalisation
choices.
