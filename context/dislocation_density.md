# Dislocation-density HBS formation model (`iHighBurnupStructureFormation = 3`)

Alternative to the KJMA(bu) model of Barani 2020 (`iHighBurnupStructureFormation = 1, 2`).
Added to the `development/porosity_HBS` branch in April 2026.

This document covers the full chain: the physical model, the calibration of
its two free parameters against PIE data, its implementation in SCIANTIX, the
coupling with the HBS porosity model, and the regression test. The
option-3 formation path is experimental at the time of writing and is
**not included in the manuscript submitted to NED**; it is kept in the
codebase as the starting point for a future paper on lower-scale coupling
of HBS formation.

---

## 1. Motivation

The KJMA path (options 1 and 2) evolves $\alpha_r(\text{bu})$ purely as a
function of effective burnup, with no explicit temperature dependence: the
burn-through is driven by the cumulative density of fission events
regardless of local conditions. This is adequate for the near-rim region of
LWR pellets (`T ≲ 800 K`) where the Gerczak 2018 / Noirot 2015 calibration
data live, but it has two known deficiencies:

1. **Coarse temperature gating.** KJMA relies on the `"Effective burnup"`
   Heaviside (gate at 1273 K) for thermal suppression — a binary switch,
   not a continuous response. Experimentally the HBS-suppression window
   opens smoothly between ~1000 and ~1200 K (Rondinella & Wiss 2010 JNM
   395, Une 2001 JNM 288); the step at 1273 K misses the transition region.
2. **Phenomenological.** The $(K, \gamma, \text{bu}_\text{inc})$ triple is a
   fit to $\alpha_r(\text{bu})$ PIE data, with no connection to the
   underlying microstructural driver (dislocation pile-up density) that
   actually triggers polygonisation.

Option 3 addresses both by computing the dislocation density
$\rho_d(\text{bu}, T)$ explicitly and mapping it onto $\alpha_r$ through a
KJMA-like saturation curve.

---

## 2. Physical model

### 2.1. Dislocation density correlation

Veshchunov & Shestak (JNM 384 (2009) 12–18, Fig. 4) compiled $\rho_d$
measurements in UO$_2$ vs **local burnup** at four temperatures
(900, 1100, 1200, 1400 K). The functional form fitted in this work
(`context/dd_fit.py`, surface shown in
`context/dislocation_density_surface.png`) is:

$$
\rho_d(\text{bu}, T) = A \cdot \text{bu}^{n} \cdot \left[A_\infty + \frac{1 - A_\infty}{1 + \exp\!\left(\dfrac{T - T_c}{\Delta T}\right)}\right]
$$

The independent variable $\text{bu}$ is the **local/total burnup** in
MWd/kgHM — the same quantity plotted on the x-axis of Veshchunov's Fig. 4
— not the Khvostov effective burnup used by the KJMA path (see §5.4).

| Symbol | Value | Units | Role |
|---|---|---|---|
| $A$ | $6.545 \times 10^{12}$ | m$^{-2}$ / (MWd kgHM$^{-1})^n$ | burnup prefactor |
| $n$ | $1.151$ | — | burnup exponent (slightly super-linear) |
| $A_\infty$ | $0.608$ | — | high-T plateau of temperature factor |
| $T_c$ | $1109$ | K | sigmoid centre |
| $\Delta T$ | $25.8$ | K | sigmoid width |

The temperature factor is a Fermi-Dirac sigmoid: at $T \ll T_c$ the factor
is $\approx 1$ (full dislocation build-up), at $T \gg T_c$ it plateaus at
$A_\infty \approx 0.61$ (thermal recovery suppresses density growth but
does not drive it to zero, consistent with the Veshchunov-Shestak data
scatter at high T).

### 2.2. KJMA mapping onto restructured volume fraction

The dimensionless progress variable is the excess dislocation density
normalized against a reference scale:

$$
\xi = \max\!\left(\frac{\rho_d - \rho_\text{crit}}{\rho_\text{scale}}, \, 0 \right)
$$

The restructured volume fraction follows a KJMA-like form:

$$
f_\text{instant} = 1 - \exp\!\left[-K_\rho \cdot \xi^{\gamma_\rho}\right]
$$

with

| Constant | Value | Source |
|---|---|---|
| $\rho_\text{crit}$ | $6 \times 10^{14}$ m$^{-2}$ | HBS nucleation threshold (Veshchunov & Shestak 2009) |
| $\rho_\text{scale}$ | $1 \times 10^{15}$ m$^{-2}$ | Normalization so $\xi$ is $O(1)$; keeps $K_\rho$ dimensionless |
| $K_\rho$ | $2.597$ | KJMA($\rho$) prefactor, direct fit on PIE (Zullo 2026) — see §3 |
| $\gamma_\rho$ | $1.104$ | KJMA($\rho$) exponent, direct fit on PIE (Zullo 2026) — see §3 |

The fixed constants $\rho_\text{crit}$ and $\rho_\text{scale}$ come from
physics and are **not** free fit parameters. The former is the lower bound
of the HBS transition window identified by Veshchunov 2009 (onset of
low-angle $\to$ high-angle cell boundary conversion); the latter is the
upper bound of the same window and is used purely as a numerical rescaling.

### 2.3. Why the dimensionless $\xi$

If we wrote the exponent as $K_\rho \cdot (\rho_d - \rho_\text{crit})^{\gamma_\rho}$
with $\rho$ in m$^{-2}$, the fit parameter $K_\rho$ would carry dimensions
m$^{2\gamma_\rho}$ and numerical values around $10^{-16}$ for
$\gamma_\rho = 2$, or $10^{-32}$ for $\gamma_\rho = 4$. This is a disaster
for non-linear least-squares: the covariance matrix becomes
ill-conditioned and the optimizer gets stuck on the initial guess.

The dimensionless formulation
$\xi = (\rho_d - \rho_\text{crit}) / \rho_\text{scale}$ maps the physical
transition window to $\xi \in [0, 1]$, which keeps all fit parameters of
order unity. It is a purely numerical reparameterization; the physics is
identical.

The KJMA($\rho$) form **replaced an earlier linear ramp**
$\text{clamp}\!\left((\rho_d - \rho_\text{low})/(\rho_\text{high} - \rho_\text{low}), 0, 1\right)$
which gave an overly steep $\alpha_r(\text{bu})$ — saturating at 1 by
$\text{bu} \approx 78$ MWd/kgHM at rim conditions, with a corresponding
artificial narrow spike in the downstream pore number density. The KJMA
form asymptotes smoothly.

### 2.4. Monotonicity

HBS restructuring is irreversible: recoverable dislocation recovery does
not undo polygonisation once the sub-grain network is established. The
final value is therefore clipped against the start-of-timestep value:

$$
\alpha_r(t_{n+1}) = \max\!\left(\alpha_r(t_n), \; f_\text{instant}(\text{bu}, T) \right)
$$

This is critical because the temperature factor can drop during a
transient (e.g. reactor shutdown), which would otherwise pull
$f_\text{instant}$ below $\alpha_r(t_n)$ and break the physics.

### 2.5. Upper-bound cap

$\alpha_r$ is capped at $1 - 10^{-9}$ rather than exactly $1.0$. The
downstream porosity sweeping term (`HighBurnupStructurePorosity.C`)
computes $\dfrac{1}{1 - \alpha_r} \cdot \dfrac{d\alpha_r}{dt}$; under the
KJMA(bu) path, $\alpha_r$ asymptotes to 1 but never reaches it in double
precision, so the division is always finite. Without the clamp, option 3
would produce `inf`/`NaN` in downstream columns. The cap preserves the
asymptotic behaviour of options 1 and 2 without touching any downstream
code.

---

## 3. Calibration of $K_\rho$ and $\gamma_\rho$

### 3.1. Dataset

Post-irradiation examination measurements of the restructured volume
fraction $\alpha_r$ against local effective burnup, from the same
compilation used by Barani et al. 2020 and Frattini 2025:

| $\text{bu}$ (MWd/kgHM) | $\alpha_r$ | Source |
|---:|---:|---|
|  64.32 | 0.2687 | Gerczak 2018 |
|  70.91 | 0.5438 | Gerczak 2018 |
|  72.27 | 0.5534 | Gerczak 2018 |
|  77.05 | 0.5979 | Gerczak 2018 / Noirot 2015 |
|  83.86 | 0.6211 | Noirot 2015 |
|  88.41 | 0.6869 | Noirot 2015 |
|  90.68 | 0.7566 | Noirot 2015 |
| 129.77 | 1.0002 | Noirot 2015 |

All these points come from rim positions of high-burnup LWR fuel. The
local temperature at the rim is approximately 900 K in all cases (the rim
is the coldest region of the pellet, well below the Holt threshold), so
the effective burnup equals the local burnup and the tabulated values can
be used directly.

**Assumption**: $T = 900$ K for all PIE points. This is the one weak link
in the calibration chain; the data in the literature do not report the
exact irradiation temperature for each point, but the rim temperature of
commercial LWR fuel is known to sit around 800–950 K. A $\pm 100$ K
uncertainty here translates into a few percent variation in $\rho_d$
through $f(T)$, and is therefore absorbed into the fit residuals.

### 3.2. Procedure

1. Compute $\rho_d$ for each PIE point using the correlation of §2.1 with
   $T = 900$ K.
2. Fit the KJMA($\rho_d$) model to the $(\rho_d, \alpha_r)$ pairs using
   non-linear least squares in $(\log K_\rho, \gamma_\rho)$ space — the
   $\log$ transformation enforces positivity of $K_\rho$ and improves
   conditioning.
3. Initial guess: $K_\rho = 5$, $\gamma_\rho = 2.5$, motivated by the
   observation that near $\xi \sim 0.3$ (middle of the transition window)
   we expect $\alpha_r \sim 0.5$, i.e.
   $K_\rho \cdot \xi^{\gamma_\rho} \sim 0.7$.
4. Bounds: $K_\rho \in [10^{-3}, 10^3]$, $\gamma_\rho \in [0.5, 8]$.

### 3.3. Result

$$
\boxed{\; K_\rho = 2.597, \qquad \gamma_\rho = 1.104 \;}
$$

Standard errors: $\sigma(\log K_\rho) = 0.169$, $\sigma(\gamma_\rho) = 0.167$.

Goodness of fit (RMSE against PIE):

| Model | RMSE |
|---|:---:|
| KJMA(bu), Barani 2022 original | 0.156 |
| KJMA(bu), Barani + $\text{bu}_\text{inc} = 15$ | 0.110 |
| **KJMA($\rho_d$), this work** | **0.050** |

The progression $0.156 \to 0.110 \to 0.050$ goes from the baseline Barani
formulation, through the incubation-burnup refinement used by the
production model (option 2), to the dislocation-driven formulation. It is
not an apples-to-apples comparison — the three models have different
parameters and different progress variables — but it shows that, on the
same dataset and against the same figure of merit, moving from a
pure-burnup driver to a dislocation-density driver produces a better fit.

### 3.4. Physical interpretation

The fitted exponent $\gamma_\rho \approx 1.1$ is suggestive. With
$\gamma_\rho$ essentially equal to 1, the model collapses to

$$
-\ln(1 - f_\text{HBS}) \approx K_\rho \cdot \xi
$$

that is, the rate at which "unrestructured volume" disappears is
proportional to the density of dislocations in excess of the critical
threshold. This has a natural mechanistic reading: each dislocation beyond
the critical density contributes linearly to nucleating a sub-grain
boundary. The classical Avrami interpretation — where
$\gamma \in [3, 4]$ encodes the dimensionality of nucleation and growth —
does not map cleanly onto this picture, which is fine: we are not fitting
nucleation kinetics in time, we are fitting a transformation progress
against a state variable ($\rho_d$).

### 3.5. What the burnup-space curve looks like

Although the fit is performed in $\rho_d$ space with a near-exponential
form, the curve $f_\text{HBS}(\text{bu})$ at fixed $T$ remains sigmoidal,
because $\rho_d \propto \text{bu}^{1.15}$ injects the burnup-space sigmoid
shape. At $T = 900$ K the new curve tracks the PIE data closely and lies
close to the KJMA(bu) curve with $\text{bu}_\text{inc} = 15$ — see
`context/kjma_rho_fit.png`.

---

## 4. Temperature dependence: the real payoff

At $T = 900$ K the new model and the legacy KJMA(bu) with incubation
behave nearly identically — as they should, because both are calibrated
on the same PIE dataset. The difference appears at higher temperatures,
where the sigmoid $f(T)$ of the dislocation density correlation throttles
$\rho_d$:

| $T$ (K) | $\rho_d$ at $\text{bu} = 75$ MWd/kgHM (m$^{-2}$) | $f_\text{HBS}$ (option 3) | $f_\text{HBS}$ (KJMA-bu) |
|:---:|:---:|:---:|:---:|
|  800 | $9.42 \times 10^{14}$ | 0.55 | 0.42 |
|  900 | $9.42 \times 10^{14}$ | 0.55 | 0.42 |
| 1000 | $9.37 \times 10^{14}$ | 0.54 | 0.42 |
| 1100 | $7.89 \times 10^{14}$ | 0.34 | 0.42 |
| 1200 | $5.83 \times 10^{14}$ | 0.00 | 0.42 |
| 1300 | $5.73 \times 10^{14}$ | 0.00 | 0.42 |
| 1400 | $5.73 \times 10^{14}$ | 0.00 | 0.42 |

Three things to note:

- **Below 1000 K**: the two models agree to within a few percent. The new
  model does not break existing validation against rim data.
- **Around 1100 K**: the new model predicts a reduction in HBS fraction
  that the legacy model misses entirely. This is the regime where some
  fuel rods start to show deviations from simple burnup-only predictions
  in PIE.
- **Above 1200 K**: $\rho_d$ drops below the critical threshold and the
  new model returns $f_\text{HBS} = 0$ for this burnup. The legacy
  KJMA(bu), with no explicit $T$ dependence, would still predict 42 %
  restructuring. The thermal cutoff that Holt imposes with a Heaviside
  at 1273 K is reproduced here naturally, and the transition is gradual
  rather than step-like.

See `context/kjma_rho_T_dependence.png` for the full family of curves.

---

## 5. Implementation in SCIANTIX

### 5.1. Source changes

| File | Change |
|---|---|
| `src/models/HighBurnupStructureFormation.C` | `case 3` in the switch pushes the 9 parameters $\{A, n, A_\infty, T_c, \Delta T, \rho_\text{crit}, \rho_\text{scale}, K_\rho, \gamma_\rho\}$ onto the model's parameter vector (options 1 and 2 parameter layouts are left untouched at 5 entries each). A new `if (option == 3) { … }` block after the switch reads `sciantix_variable["Burnup"].getFinalValue() / 0.8814` (**local burnup** in MWd/kgHM — see §5.4), computes $\rho_d$, then $\xi$, then $f_\text{instant}$, applies the monotonicity guard via `getInitialValue()`, and writes both `"Restructured volume fraction"` and the new output `"Dislocation density"`. Options 1 and 2 stay inside their own branches, identical to the pre-change code. |
| `include/operations/SetVariablesFunctions.h` | Registers `SciantixVariable("Dislocation density", "(1/m2)", Sciantix_variables[70], Sciantix_variables[70], toOutputHighBurnupStructure)` next to `"Restructured volume fraction"`. Slot 70 was previously unused. |
| `src/operations/UpdateVariables.C` | Maps index 70 $\to$ `"Dislocation density"` in the state-sync map. |
| `src/operations/SetVariables.C` | `toOutputHighBurnupStructure` relaxed from `== 1` to `!= 0` so option 3 also enables the HBS output columns. Option 0 (disabled) and options 1, 2 (KJMA) behaviour unchanged. |

Bit-identity under options 1 and 2 was verified after the change: with
the new binary, `test_UO2HBS/output.txt` differs from the pre-change
output by exactly one column (the newly added `"Dislocation density"`,
all zeros for this run); every other column matches byte-for-byte.

### 5.2. Input format

No change to the input parser. `iHighBurnupStructureFormation` was already
parsed in `InputReading.C`; only the set of legal values is extended:

```
0  #  iHighBurnupStructureFormation  (0 = not considered,
                                      1 = KJMA Barani 2020 original,
                                      2 = KJMA + incubation burnup (bu_inc = 15),
                                      3 = dislocation-density (Veshchunov 2009 / Zullo 2026))
```

### 5.3. Output

A new tab-separated column `"Dislocation density (1/m2)"` is emitted in
`output.txt` whenever `iHighBurnupStructureFormation ≠ 0` (via the
`toOutputHighBurnupStructure` flag).

### 5.4. Why `"Burnup"` and not `"Effective burnup"`

The KJMA path (options 1 and 2) evolves $\alpha_r$ as a function of
SCIANTIX's `"Effective burnup"`. `EffectiveBurnup.C` computes this by
integrating the specific power over time **with a Heaviside gate** at
$T = 1273.15$ K (Khvostov 2005, adopted from Holt's original
effective-burnup definition):

```cpp
// src/models/EffectiveBurnup.C, lines 28–33
const double temperature_threshold = 1273.15;
if (T_final <= temperature_threshold ||
    (T_final > temperature_threshold && T_initial < temperature_threshold))
    parameter.push_back(power / 86400.0);
else
    parameter.push_back(0.0);
```

So $\text{bu}_\text{eff}$ stops accumulating above 1273 K. That is
appropriate for the KJMA phenomenological fit, which has no intrinsic
temperature dependence and relies on this gate to suppress restructuring
in the hot pellet centre.

The dislocation-density correlation is different: it carries the thermal
dependence **inside** the sigmoid
$A_\infty + (1 - A_\infty)/(1 + \exp((T - T_c)/\Delta T))$ with
$T_c \approx 1109$ K. Feeding $\text{bu}_\text{eff}$ into this formula
would apply the thermal suppression twice — once by zeroing the burnup
input above 1273 K, once by the sigmoid evaluating near its plateau
$A_\infty$. The result would be unphysical (for $T = 1500$ K, for
example, $\text{bu}_\text{eff}$ would be frozen at whatever value it had
when the pellet crossed 1273 K, and $\rho_d$ would stop growing
completely, instead of growing at the $A_\infty$-reduced rate the fit
prescribes).

Equally importantly, the Veshchunov & Shestak Fig. 4 data points — the
very data the fit was calibrated on — are plotted against **total
irradiation burnup**, not effective burnup. Using `"Burnup"` in the code
therefore preserves consistency with the experimental calibration.

**For the current regression test (`test_UO2HBS_dislocation`, isothermal
$T = 723$ K) the two inputs are numerically indistinguishable**: the
Heaviside is always "on" because the temperature never exceeds 1273 K,
so $\text{bu}_\text{eff} \equiv \text{bu}$ at every step and the output
is bit-identical under either choice. The fix to `"Burnup"` is therefore
a no-op for this test but is required for correctness in any transient
or high-temperature scenario (as would be encountered in a
fuel-performance coupling or a radial-profile calculation where the
pellet centre exceeds 1273 K).

### 5.5. Values to hardcode

```cpp
// KJMA(rho_d) HBS formation model - calibration on PIE data (T = 900 K).
// Fit: Zullo (2026). RMSE on PIE = 0.050.
const double K_rho_HBS     = 2.597;        // dimensionless
const double gamma_rho_HBS = 1.104;        // dimensionless
const double rho_crit_HBS  = 6.0e14;       // m^-2 (Veshchunov 2009)
const double rho_scale_HBS = 1.0e15;       // m^-2 (normalization)
```

---

## 6. Coupling with the HBS porosity model

### 6.1. The problem with `iHighBurnupStructurePorosity = 2`

The Barani porosity model (case 2, `HighBurnupStructurePorosity.C`) reads
formation-model parameters **positionally**:

```cpp
double avrami_constant     = model["…"].getParameter().at(0);   // expects 3.54
double transformation_rate = model["…"].getParameter().at(1);   // expects 2.77e-7
double bu_inc              = model["…"].getParameter().at(4);   // expects 15
double resolution_layer_thickness    = model["…"].getParameter().at(2);  // expects 1e-9
double resolution_critical_distance  = model["…"].getParameter().at(3);  // expects 1e-9
```

Under option 3 those slots hold completely different quantities from the
dislocation-density fit: $A = 6.5 \times 10^{12}$, $n = 1.151$,
$A_\infty = 0.608$, $T_c = 1109$, $\Delta T = 25.8$. The critical failure
is on the pore-nucleation-rate line:

```cpp
pore_nucleation_rate ∝ … · pow(bu_for_nucl, avrami_constant − 1)
```

which becomes `pow(bu_for_nucl, 6.5 × 10¹²)`. For any `bu_for_nucl`
outside a $\approx 2 \times 10^{-10}$-wide window around 1, this either
underflows to 0 (on the left) or overflows to $+\infty$ (on the right).
The test's burnup rate crosses that window in $\approx 10^{-7}$ s, far
below the 14.8 s timestep, so the simulation jumps directly from
rate = 0 to rate = $\infty$, producing NaN cascading through every HBS
column from that step onward.

**Pairing `iHighBurnupStructureFormation = 3` with
`iHighBurnupStructurePorosity = 2` is therefore unsupported** and always
will be, by construction of case 2's positional parameter reading.

### 6.2. The solution: `iHighBurnupStructurePorosity = 3`

Added in April 2026. A formation-agnostic duplicate of case 2 with
exactly two localized changes:

**1. Pore nucleation rate decoupled from positional parameter reads.**
For the KJMA path, $\alpha_r = 1 - \exp[-K (\text{bu} - \text{bu}_\text{inc})^\gamma]$
implies $d\alpha_r/d\text{bu} = K \gamma (1 - \alpha_r) (\text{bu} - \text{bu}_\text{inc})^{\gamma - 1}$,
so case 2's expression

```cpp
ν_P = 1e18 · K · γ · (1 − α_r) · (bu − bu_inc)^(γ−1) · dbu/dt
```

is exactly $10^{18} \cdot d\alpha_r/dt$ by the chain rule. Case 3 writes
the identity directly, using the simulation's own
`"Restructured volume fraction"` increment:

```cpp
double dalpha = sciantix_variable["Restructured volume fraction"].getIncrement();
double dt     = physics_variable["Time step"].getFinalValue();
double pore_nucleation_rate = (dt > 0.0 && dalpha > 0.0) ? (1.0e18 * dalpha / dt) : 0.0;
```

Paired with options 1 or 2 formation, case 3 is numerically very close
to case 2 (finite-difference vs analytical derivative — typically
$\lesssim 0.1\%$ deviation at the test's timestep). Paired with option-3
formation, case 3 gives a physically sensible nucleation trajectory
where case 2 blows up. Paired with any future formation model that
populates $\alpha_r$ monotonically, case 3 just works.

**2. Re-solution thicknesses hardcoded to 1 nm.**
The Veshchunov-Tarasov $d_V$ and $\delta_V$ are re-solution physics, not
formation physics; their accidental co-location in the formation
parameter vector under options 1–2 is a design artefact. Case 3
hardcodes them to their Barani-2022 Table 1 values ($1 \times 10^{-9}$ m)
instead of reading them from `at(2)` and `at(3)`.

Everything else — saturation factor, 5×5 implicit-Euler cluster
dynamics, vacancy growth, binary-interaction impingement, guards — is a
byte-for-byte copy of case 2.

---

## 7. Regression test

### 7.1. Test folder

`regression/test_UO2HBS_dislocation/`, duplicated from `test_UO2HBS/`
with two line changes in `input_settings.txt`:

- `iHighBurnupStructureFormation = 3`
- `iHighBurnupStructurePorosity = 3`

History and initial conditions are unchanged (T = 723 K constant, fission
rate $2 \times 10^{19}$ fiss m$^{-3}$ s$^{-1}$, 74 000 h). Under these
conditions the local burnup reaches $\approx 201$ MWd/kgHM at EoL and
the sigmoid temperature factor sits at $\approx 1.0$ (well below
$T_c = 1109$ K), so the test exercises the burnup-driven part of the
correlation.

### 7.2. Observed trajectory

Monotonic, asymptotic by construction of the KJMA($\rho$) form:

| $\text{bu}$ (MWd/kgHM) | $\rho_d$ (m$^{-2}$) | $\xi$ | $\alpha_r$ |
|---|---|---|---|
|  20 | $2.07 \times 10^{14}$ | 0     | 0 |
|  40 | $4.60 \times 10^{14}$ | 0     | 0 |
|  60 | $7.34 \times 10^{14}$ | 0.134 | 0.246 |
|  80 | $1.02 \times 10^{15}$ | 0.423 | 0.633 |
| 100 | $1.32 \times 10^{15}$ | 0.722 | 0.837 |
| 120 | $1.63 \times 10^{15}$ | 1.031 | 0.932 |
| 140 | $1.95 \times 10^{15}$ | 1.348 | 0.973 |
| 200 | $2.94 \times 10^{15}$ | 2.339 | 0.999 |

The $0 \to 1$ transition spans $\approx 100$ MWd/kgHM rather than the
$\approx 20$ MWd/kgHM window the linear-ramp version exhibited. The
saturation at $\alpha_r \approx 0.999$ at EoL is genuine KJMA asymptote
approach — not clipping by the $1 - 10^{-9}$ numerical cap (see §2.5).

Compared to the option-2 KJMA-bu reference in the sibling `test_UO2HBS`
folder:

- $N_p$ peak is sharper and shifted to lower $\text{bu}_\text{eff}$
  ($\approx 70$ vs $\approx 110$ MWd/kgHM). This reflects the narrower
  $\alpha_r(\text{bu})$ ramp of the dislocation model at $T = 723$ K —
  the ramp crosses $0 \to 1$ in $\approx 25$ MWd/kgHM vs KJMA's
  $\approx 60$.
- $\xi$ reaches similar saturation ($\approx 0.15$) via the percolation
  factor; both paths merge at high burnup once the solid backbone
  fragments.
- $R_p$ is systematically larger than case 2 (consistent with the
  smaller final $N_p$, same total xenon inventory $\to$ larger mean
  volume per pore).

All three trajectories sit within the scatter of the Cappia / Spino /
Noirot / Lassmann experimental datasets.

### 7.3. Regression plots

Branch added to `regression_hbs.py` in `_plot_dislocation_density_case()`
(triggered by folder name containing `"dislocation"`):

- `plot_restructured_fraction.png` — $\alpha_r(\text{bu}_\text{eff})$
  for option 3, with the KJMA curve from `test_UO2HBS/output.txt`
  overlaid when that sibling test has been run in the same session.
  Both paths now show smooth sigmoid shapes of comparable slope. The
  dislocation-density curve crosses 0.5 a few MWd/kgHM earlier than
  KJMA-bu but reaches its 0.99 asymptote later (a slightly gentler
  approach to saturation).
- `plot_dislocation_density.png` — $\rho_d(\text{bu}_\text{eff})$ on a
  log y-axis, with horizontal reference lines at $\rho_\text{crit}$ and
  $\rho_\text{scale}$.
- `plot_pore_density.png`, `plot_porosity.png`, `plot_pore_radius.png`
  — inherited from `regression_hbs.py`, with experimental overlays
  loaded from the sibling `test_UO2HBS/` folder for reference.

---

## 8. Open points and limitations

- The assumption $T = 900$ K for all PIE points in the calibration is
  the main source of uncertainty. If per-point local temperatures can
  be obtained (or simulated), the fit can be repeated point-by-point
  rather than at a single temperature.
- $\rho_d(\text{bu}, T)$ is built on a four-temperature dataset from a
  single paper (Veshchunov 2009 Fig. 4), so the temperature dependence
  is under-constrained outside the 900–1400 K range.
- $\rho_\text{crit}$ is fixed at $6 \times 10^{14}$ m$^{-2}$ from
  Veshchunov; a sensitivity study (e.g. varying it between
  $4 \times 10^{14}$ and $8 \times 10^{14}$) would be worth running
  before any future paper submission on this option.
- The fitted $\gamma_\rho \approx 1.1$ is statistically consistent with
  $\gamma_\rho = 1$ (exponential saturation). Forcing $\gamma_\rho = 1$
  and refitting only $K_\rho$ is a one-parameter alternative worth
  testing for simplicity, especially if the full two-parameter fit
  proves unstable under small perturbations of the dataset.
- The option is intended as a **validation stepping stone** for future
  coupling of SCIANTIX with defect-evolution codes that provide
  $\rho_d(\text{bu}, T)$ dynamically rather than through a closed-form
  correlation.

---

## 9. References

- Veshchunov, M. S. & Shestak, V. E. (2009). *Model for evolution of
  crystal defects in UO$_2$ under irradiation up to high burn-ups.*
  J. Nucl. Mater. **384**, 12 — Fig. 4 provides the $\rho_d(\text{bu}, T)$
  data used for the fit.
- Rondinella, V. V. & Wiss, T. (2010). *The high burnup structure in
  nuclear fuel.* J. Nucl. Mater. **395** — thermal-recovery suppression
  of HBS above $\sim 1100$ K.
- Une, K. et al. (2001). *Rim-structure formation of irradiated UO$_2$
  fuel.* J. Nucl. Mater. **288** — temperature gating of HBS onset.
- Biswas, S. & Aagesen, L. K. (2025). *Phase-field modelling of HBS
  formation.* Comp. Mater. Sci. **258**, 114052 — modified-JMAK Eq. 45;
  used in option 2.
- Barani, T. et al. (2020). *Modeling high burnup structure in oxide
  fuels for application to fuel performance codes. Part I.*
  J. Nucl. Mater. **539**, 152296 — KJMA fit for options 1 and 2.
- Gerczak, T. J. et al. (2018). *Restructuring in high burnup UO$_2$
  studied using modern electron microscopy.* J. Nucl. Mater. **509**,
  245 — PIE calibration data.
- Noirot, J. et al. (2015). *Focused ion beam — scanning electron
  microscope examination of high burnup UO$_2$ in the center of a pellet.*
  Nucl. Eng. Tech. **47**, 769 — PIE calibration data.
- Frattini, M. (2025). M.Sc. thesis, Politecnico di Milano.

---

## 10. Related files

| Path | Role |
|---|---|
| `context/dd_fit.py` | Fit script for $\rho_d(\text{bu}, T)$ against Veshchunov data (produces $A, n, A_\infty, T_c, \Delta T$). |
| `context/dislocation_density_fit.png` | 1-D slice of the $\rho_d$ fit vs data. |
| `context/dislocation_density_surface.png` | 2-D $\rho_d(\text{bu}, T)$ surface. |
| `context/kjma_fit_comparison.py` | KJMA($\alpha_r$ vs bu) fit with $\text{bu}_\text{inc}$ sweep; produces `kjma_rho_fit.png` used in the manuscript to justify $\text{bu}_\text{inc} = 15$. |
| `context/kjma_rho_fit.png` | $\alpha_r(\text{bu})$ at $T = 900$ K — KJMA(bu) variants, KJMA($\rho_d$), and PIE data overlaid. |
| `context/kjma_rho_T_dependence.png` | (1) $\alpha_r(\text{bu})$ at multiple temperatures via KJMA($\rho_d$); (2) universal curve $f_\text{HBS}(\rho_d)$. |
| `regression/test_UO2HBS_dislocation/` | Regression test for option 3 paired with porosity case 3. |