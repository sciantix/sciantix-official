# MOX pO2 Verification

This directory runs a MOX oxygen partial pressure and oxygen-potential
verification sweep for SCIANTIX.

**Two ways to run this, don't confuse them:**

- **Routine (fast, ~2 min)**: `python3 -m testing.runner --mox-po2` runs the 9
  persistent, committed `T_<T>K_q_<Pu>/` cases (1400/1800/2200 K x
  0.10/0.20/0.30 -- see `generate_cases.py`) through the ordinary gold-diff,
  then the accuracy check below via `testing/core/mox_po2_runner.py`. This is
  what's described in `testing/README_oxygenpotential_VV.md`.
- **Full/exploratory (slow, ~15 min with OC)**: everything described in this
  file below (`run_temperature_sweep.py`, the full 800-2600 K x 3-q grid,
  ephemeral case generation) -- useful for re-examining the low-T CALPHAD
  mismatch or producing paper figures, unrelated to the routine path above.

## Scope

The default sweep covers:

- temperatures from `800 K` to `2600 K` in `200 K` steps
- plutonium contents `q = 0.10, 0.20, 0.30`
- evolving `O/M` through `iStoichiometryDeviation = 8`
- minor actinides set to zero

Two comparisons are produced:

- SCIANTIX Kato output against the explicit analytical NEA Kato equation
- SCIANTIX OpenCalphad output against the Thermo-Calc CSV tables in
  `TEMPERATURES_THERMOCALC_Q_*`

The verification comparisons are restricted to the requested reference domain:

- `753 K <= T <= 2550 K`
- `0.10 <= Pu/M <= 0.32`
- `1.92 <= O/M <= 2.08`

The lower `Pu/M` bound is applied with a small numerical tolerance so that a
nominal `10%` Pu/M SCIANTIX output is not removed only because it is printed as
slightly below `0.10`.

## Usage

Run the full SCIANTIX sweep and post-processing:

```bash
python3 run_temperature_sweep.py
```

Regenerate plots and comparison reports from existing case outputs:

```bash
python3 run_temperature_sweep.py --plot-only
```

Keep generated case directories after a full run:

```bash
python3 run_temperature_sweep.py --keep-cases
```

Limit the sweep:

```bash
python3 run_temperature_sweep.py --temperatures 1200,1600,2000 --q-values 0.10,0.20
```

## Workflow

`run_temperature_sweep.py`:

- clears previously generated PNG plots from the test directory and `figures/`
- copies the template `input_*` files into one case directory per
  `(temperature, q)` pair
- updates the temperature in `input_history.txt`
- substitutes `__Q_VALUE__` in `input_initial_conditions.txt`
- runs `sciantix.x`
- collects all case `output.txt` files into `temperature_sweep_summary.tsv`
- creates overview plots directly in `figures/`
- runs the Kato and OpenCalphad CSV comparison scripts
- removes generated case directories unless `--keep-cases` is used

`sciantix_verification/compare_sciantix_with_kato.py`:

- reads `temperature_sweep_summary.tsv`
- keeps only `753 K <= T <= 2550 K`, `0.10 <= Pu/M <= 0.32`, and
  `1.92 <= O/M <= 2.08`
- samples the explicit NEA Kato equation over oxygen partial pressure
- interpolates the explicit reference onto the SCIANTIX `O/M` trajectory
- writes comparison tables, residual summaries, and plots

`sciantix_verification/compare_sciantix_with_oc_csv.py`:

- reads `temperature_sweep_summary.tsv`
- reads Thermo-Calc CSV tables from `TEMPERATURES_THERMOCALC_Q_*`
- keeps only `753 K <= T <= 2550 K`, `0.10 <= Pu/M <= 0.32`, and
  `1.92 <= O/M <= 2.08`
- interpolates SCIANTIX OpenCalphad values onto the Thermo-Calc `O/M` grid
- writes comparison tables, residual summaries, and plots

## Why Points Are Excluded

Some points are removed from the comparison tables before metrics are computed.
These removals are bookkeeping choices required by the interpolation/logarithm
operations.

Kato comparison:
- SCIANTIX points outside the comparison domain
  `753-2550 K`, `Pu/M = 0.10-0.32`, `O/M = 1.92-2.08` are dropped
- the explicit Kato curve is sampled over a finite pressure range
- SCIANTIX points outside the resulting explicit Kato `O/M` range cannot be
  interpolated and are dropped
- non-finite explicit values are dropped

OpenCalphad CSV comparison:
- Thermo-Calc and SCIANTIX points outside the comparison domain
  `753-2550 K`, `Pu/M = 0.10-0.32`, `O/M = 1.92-2.08` are dropped
- non-positive pressures are dropped because `log10(pO2 / p_ref)` is undefined
- Thermo-Calc points outside the SCIANTIX `O/M` interpolation range are dropped
- duplicate Thermo-Calc `O/M` points for a given `(T, q)` are averaged before
  interpolation

## Outputs

Primary reports:

- `sciantix_verification/sciantix_vs_kato_summary.txt`
- `sciantix_verification/sciantix_vs_oc_csv_summary.txt`

Plots are written to:

- `figures/`
