# MOX Oxygen Potential Verification

Verifies SCIANTIX's two oxygen-potential paths for MOX fuel against independent
references, entirely in oxygen potential (kJ/mol):

- **Kato analytic correlation** (NEA/NSC/R(2024)1), `Solver::BisectionKato`
  (`src/classes/Solver.C`);
- **OpenCalphad coupling** (`src/coupling/OCUtilsCoupling.C`), Gibbs-energy
  minimization on `upuo-v21.TDB`, enabled by `iThermochimica = 2`.

Setup: `iFuelMatrix = 2`, `iStoichiometryDeviation = 8` (O/M ramps linearly in
time), so each case sweeps O/M at fixed T and q. Comparison domain:
`753-2550 K`, `Pu/M = 0.10-0.32`, `O/M = 1.92-2.08`.

```bash
python3 -m testing.runner --mox-po2
```

## Acceptance criteria

- Kato path: max abs oxygen-potential error < 0.05 kJ/mol over the whole
  domain (pure numerics: bisection + interpolation).
- CALPHAD path: mean abs oxygen-potential error < 2 kJ/mol per (q, T) group,
  for T >= 1000 K. Skipped if OpenCalphad/`upuo-v21.TDB` isn't available.

## OpenCalphad re-equilibration cache

`input_thermochemistry_settings.txt` disables the lazy re-equilibration cache
(`coupling.max_stale_steps/composition_tolerance/temperature_tolerance = 0`).
The O/M ramp moves the composition by only ~3e-5/step, so with the default
cache the CALPHAD oxygen potential is held constant for ~13 steps and the
curve becomes a staircase.

## Workflow

`sciantix_verification/compare_sciantix_with_kato.py` and
`compare_sciantix_with_oc_csv.py`: read `temperature_sweep_summary.tsv` (the
latter also reads `TEMPERATURES_THERMOCALC_Q_*`), keep only the comparison
domain above, interpolate onto the reference's O/M grid, and write comparison
tables, residual summaries, and plots. Run standalone to regenerate reports
and plots from the existing `temperature_sweep_summary.tsv` without rerunning
SCIANTIX:

```bash
python3 sciantix_verification/compare_sciantix_with_kato.py
python3 sciantix_verification/compare_sciantix_with_oc_csv.py
```

Points are dropped from the comparison before interpolation if they fall
outside the domain above, outside the reference's O/M range, are duplicated
(Thermo-Calc `O/M` points for a given `(T, q)` are averaged first), or are
non-finite -- including non-positive pressures, since the oxygen potential
(proportional to `ln(pO2/p_ref)`) is undefined there.

## Outputs

- `sciantix_verification/sciantix_vs_kato_summary.txt`
- `sciantix_verification/sciantix_vs_oc_csv_summary.txt`
- Paper figures (oxygen potential; combined absolute/relative error), per q,
  per path, in `plots/`
