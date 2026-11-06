# Calibration of the two-zone FMP model (EC November 2026)

The calibrated parameters of V. Bellani (`../calibration/FMPcalibration.md`) are common to the intragranular
and grain-boundary particles. With the two-zone model (zone 1 = grain interior, zone 2 = grain-boundary zone,
coupled by diffusion) the exchange keeps the two zones at nearly the same concentration, so with common
parameters the two populations have the same radius (R_GB / R_intra = 1.003-1.011). Here the grain-boundary
parameters are free.

## Parameters

| Parameter | Nominal | Range (factor on the nominal value) | Sampling |
|---|---|---|---|
| `k_intra_0`, growth prefactor, intragranular | 4.980333e-15 m2/s | 1e-2 - 1e2 | log |
| `B_intra`, CNT barrier coefficient, intragranular | 2.665765e8 eV K2 | 0.5 - 1.5 | linear |
| `k_gb_0`, growth prefactor, grain boundary | 4.980333e-15 m2/s | 1e-1 - 1e3 | log |
| `B_gb`, CNT barrier coefficient, grain boundary | 2.665765e8 eV K2 | 0.5 - 1.5 | linear |

Fixed as in the previous calibration: `dG_intra = dG_gb = 1 eV`, `nu0_intra = nu0_gb = 1 1/s`. A factor on
`k_gb_0` is a factor on the grain-boundary diffusion coefficient, D_GB = k_gb_0 exp(-dG_gb / kB T).

## Objective

J4 = mean |ln(model / exp)| over R_intra, R_GB, A_intra and A_GB at r/R = 0, 0.30, 0.56 and 0.75 (16
observables), at 54.1 MWd/kgUO2. A = 1 - exp(-n pi R^2 t) with the particle density n of the zone of the
population and the TEM foil thickness t, as in `../plot.py`. J3 (without A_GB, 12 observables) is the
objective of the previous calibration and is reported for comparison.

## Method

1. `build/fmp_calibration/code/` (in `build/` of the repository, ignored by git; `calibrate.py` rebuilds it
   when the executable is missing, the runs go in `build/fmp_calibration/work/`): copy of `src/`, `include/`,
   `tests/` and `CMakeLists.txt` of this checkout, where
   `FiveMetalParticles.C` reads the four parameters from the environment (`FMP_K_INTRA_0`, `FMP_B_INTRA`,
   `FMP_K_GB_0`, `FMP_B_GB`; the values written in the source when not set). Built without OpenCalphad. The
   sources of the repository are not modified; the ranges are factors on the nominal values of V. Bellani,
   fixed in `calibrate.py`. With the nominal values the copy gave the same `output.txt` as `build/sciantix.x`
   (checked on r000).
2. Scrambled Sobol sampling (seed 42), 4096 points plus the nominal one, the four cases of each point run at
   the same time (calibration cases, `iThermochimica = 0`).
3. Nelder-Mead refinement from the best Sobol point, in the unit hypercube (log scale for the prefactors),
   400 evaluations at most.

```bash
python3 calibrate.py --n-sobol 4096 --maxfev 400    # about 10 minutes on 16 cores
```

`results/sobol.csv`: all the points (parameters, J3, J4, the 16 observables); `results/best.txt`: nominal,
best Sobol point and refined point with the observables against the TEM data.

## Results

4 Nov 2026, 4096 Sobol points + 5 Nelder-Mead refinements from the five best points (`results/best.txt`,
figure `results/calibration_two_zones.png`, from `plot_calibration.py`).

| | `k_intra_0` | `B_intra` | `k_gb_0` | `B_gb` | J4 | J3 |
|---|---|---|---|---|---|---|
| Nominal (V. Bellani) | 4.980e-15 | 2.666e8 | 4.980e-15 | 2.666e8 | 0.869 | 0.616 |
| Best Sobol point | ×0.610 | ×1.048 | ×0.835 | ×0.987 | 0.627 | 0.646 |
| Calibrated | 9.83e-16 (×0.197) | 2.712e8 (×1.017) | 3.010e-15 (×0.604) | 2.641e8 (×0.991) | 0.554 | 0.598 |

The five refinements end at the same point (J4 = 0.5541-0.5544): `k_gb_0` ×0.60, `B_gb` ×0.991 and `B_intra`
×1.00-1.02 are well identified, `k_intra_0` less (×0.155-0.197 for the same J4). The grain-boundary diffusion
coefficient is about 3 times the intragranular one (D_GB / D_intra = 3.06, same activation energy).

| r/R | R_intra TEM / nominal / calibrated (nm) | R_GB (nm) | A_intra (%) | A_GB (%) |
|---|---|---|---|---|
| 0 | 7.58 / 13.46 / 7.58 | 14.28 / 13.50 / 13.24 | 1.37 / 3.25 / 5.24 | 7.21 / 0.45 / 3.59 |
| 0.30 | 3.59 / 11.99 / 6.84 | 11.95 / 12.04 / 11.95 | 0.56 / 3.57 / 5.58 | 3.62 / 0.47 / 3.62 |
| 0.56 | 3.45 / 9.00 / 5.29 | 5.58 / 9.06 / 9.26 | 3.25 / 5.89 / 7.86 | 1.76 / 0.74 / 4.72 |
| 0.75 | 3.10 / 6.44 / 3.55 | 6.68 / 6.51 / 6.21 | 2.62 / 2.56 / 1.19 | 0.72 / 0.32 / 0.72 |

- R_GB / R_intra goes from 1.00 to 1.75 at all positions: the faster grain-boundary transport separates the two radii, and
  R_intra is now 1.0-1.9 times the TEM value (was 1.8-3.3); at r/R = 0.75 A_intra is 2 times too low.
- A_GB is within a factor 2 at three positions (was 2-16 times too low); it is too high at r/R = 0.56,
  where R_GB is also too large (9.3 vs 5.6 nm).
- A_intra gets worse at r/R = 0-0.30 (4-10 times the TEM value): with a slower intragranular growth the
  particles are smaller but more numerous. The intragranular number density is the main remaining
  discrepancy.

The calibrated values are in `src/models/FiveMetalParticles.C` (EC November 2026); the gold files of the
`--metallicinclusions` and `--metallicinclusions-oc` cases and the figures of `../plot.py` are regenerated with
them. Model equations and results: `../README.md`.
