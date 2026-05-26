# Intragranular coarsening extensions relative to Barani 2019

This note summarizes the SCIANTIX branch changes built on top of the
Barani-type intragranular coarsening model. The reference model is the
two-population treatment in which the standard nanometric intragranular
bubbles coexist with a coarsened population nucleated on dislocations.

## Baseline retained from Barani 2019

The Barani configuration is kept as the reference option:

- `iCoarseningDislocationDensity = 0`
- fixed dislocation density, $\rho_d = 4.0 \times 10^{13}$ m/m3
- `iCoarseningKModel = 0`
- constant bubbles-per-dislocation prefactor, $K_0 = 10^6$

The Barani path is therefore still available without requiring additional
input parameters.

## Variable dislocation-density options

The fixed Barani value can now be replaced by burnup-dependent correlations
through `iCoarseningDislocationDensity`.

### Option 1: Veshchunov 2009

`iCoarseningDislocationDensity = 1` activates a temperature- and
burnup-dependent correlation:

$$
\rho_d = A\,\mathrm{Bu}^{n}\,f_T
$$

with

$$
f_T = A_\infty + \frac{1 - A_\infty}{1 + \exp((T - T_c)/\Delta T)}
$$

as implemented in the source. The option is labelled Veshchunov 2009
because the correlation originates from that reference.

### Option 2: Nogita 1995

`iCoarseningDislocationDensity = 2` activates the burnup-only Nogita 1995
correlation:

$$
\log_{10} \rho_d = 2.2 \times 10^{-2}\,\mathrm{Bu} + 13.8
$$

with $\rho_d$ in m/m3 and burnup in MWd/kgUO2. This option is useful for
testing whether the dislocation population should be driven by accumulated
irradiation damage alone, without an explicit thermal damping term.

## Effective bubbles-per-dislocation models

The branch also keeps three alternatives for `iCoarseningKModel`.

### Option 0: Barani 2019

`iCoarseningKModel = 0` uses the constant Barani value for $K$.

### Option 1: Nicodemo 2026 algebraic weighting

`iCoarseningKModel = 1` computes an effective value:

$$
K_\mathrm{eff} = K_0 f_\mathrm{Bu} f_T
$$

where

$$
f_\mathrm{Bu} = 1 - \exp(-\mathrm{Bu}/B_\mathrm{sat})
$$

and

$$
f_T = \frac{1}{1 + \exp((T_\mathrm{max} - T_\mathrm{sat})/100)}
$$

The current tuned defaults are:

- $K_0 = 8.0 \times 10^5$
- $B_\mathrm{sat} = 16$ MWd/kgUO2
- $T_\mathrm{sat} = 1850$ K

These values were chosen as a radius/density compromise on the White
coarsening data, not as a final physical calibration.

### Option 2: Nicodemo 2026 kinetic activation

`iCoarseningKModel = 2` uses the same saturation functions as option 1 but
applies them incrementally to activate available dislocation-bubble sites.
The intent is to avoid instantaneous seeding of the entire
$K_0\rho_d$ population at the beginning of the transient.

## Regression plotting

The standard White regression workflow now calls `regression/white/parity_plot.py`
after the White cases are run or compared. The script creates parity plots
against the White experimental data for:

- intragranular bubble radius
- intragranular bubble concentration
- intragranular bubble swelling
- intergranular bubble swelling

The White coarsening regression folder is intentionally left untouched by
this plotting update.
