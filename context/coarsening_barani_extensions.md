# Subsequent developments

The selected analytic_rho sources are now integrated unchanged in this repository.
Other historical campaigns remain separate research-workspace source trees.

## Development sequence

- Vergani-type re-solution/trapping, embryo survival, conservative coalescence
  and vacancy reservoirs were studied independently; they were not all promoted.
- Reversible vacancy exchange improved selected endpoints but was not promoted.
- Hart/Murphy weighting with a=1.5b became the frozen production reference.
- vesh_hart_1p5b added accumulated dose-driven density, gas-limited bubble
  births and variable Hart geometry.
- analytic_rho, subsequently selected in the research workspace, replaced
  accumulated density with the direct fitted rho(T,B).

## Analytic-density model

The [fit](dislocation_density.md) is evaluated at current temperature and burnup,
without a rho0 offset. At B=0, rho=0. Births use

```text
Delta_rho = max[rho(B_new,T_new)-rho(B_old,T_new), 0]
Delta_N_requested = K*Delta_rho, K=1e6 m^-1
```

Actual births consume solution gas: two atoms per embryo, zero initial vacancies.
Cooling at constant burnup alone does not cause births; decreasing density does
not automatically remove bubbles. The one-off seed K*4e13 remains independent
of the fit. Coverage is diagnostic only. This development adds no
coverage-triggered release, KJMA kinetics or loop punching.

## Evidence and limits

White RMSE log10 for R/Nd/IG/GB:
0.225432 / 0.636547 / 0.594226 / 0.375000. Only radius improves relative to
the constant-density Hart reference under these four aggregate metrics.

TU v1m1j24 comparisons against the accumulated-density development are mixed:
REGATE improves diameter increment but worsens absolute diameter; CONTACT improves
FGR modestly; AN3 still underpredicts the burst; HATAC approaches final FGR but
worsens history RMSE.

Fit/coupling checks and targeted grid refinements do not establish general
timestep convergence. Extrapolation beyond approximately 3-93 GWd/tHM and
900-1400 K, uptake caps and structural/SDA lag remain limitations.
Source-report locations are recorded in SOURCE_MANIFEST.json.
