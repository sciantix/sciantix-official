# UN Notebook-8 Regression Test Plan

## Read-only branch audit

- Current local branch: `development/nitride`.
- Local/remotes include `chore/un-model-fast-demo`, `origin/development/nitride`,
  `origin/chore/un-model-fast-demo`, and `origin/GPregression`.
- GitHub was used read-only to search branches. Branches relevant to this audit:
  `chore/un-model-fast-demo` for UN calibration/demo work and `GPregression` for
  historical regression-related work.
- `development/nitride` contains the `UN_model/notebooks/8test_UN_intergranular`
  reference results and the modern `regression/core` framework.
- `chore/un-model-fast-demo` contains `un_calibration/model/*` and the same
  regression framework, but no standalone `regression/un_notebook8` suite.

## How SCIANTIX Regression Works

The current regression runner is Python-based:

- `regression/runner.py` discovers known regression groups and calls
  `regression.core.generic_runner.run_group`.
- A regression group is a directory under `regression/`, with case directories
  named by prefix, for example `test_Baker...` or `test_UO2HBS`.
- `regression/core/common.py` provides:
  - `run_sciantix(case_dir)`, which runs `build/sciantix.x <case_dir>/`;
  - `load_output(case_dir)` and `load_gold(case_dir)`;
  - cleanup helpers for generated run files.
- `regression/core/parser.py` reads tab-separated `output.txt` into a
  `SciantixOutput` object with header lookup and `get_last` / `get_all`.
- `regression/core/compare.py` compares full numeric arrays against
  `output_gold.txt`.
- `regression/core/plot.py` provides parity/history plotting helpers.
- `regression/hbs/plot.py` is a useful example for loading `output.txt`, finding
  columns by exact header name, tolerating missing input data, and generating
  figures.

## Templates To Reuse

- Use `regression/core/common.py` for the executable convention:
  `build/sciantix.x <case_dir>/`.
- Use the parsing style from `regression/core/parser.py`, but keep the UN suite
  standalone so missing columns can be warnings instead of hard failures.
- Use plotting patterns from `regression/core/plot.py` and `regression/hbs/plot.py`.
- Do not add the new UN suite to `regression/runner.py` yet. This avoids changing
  established regression behavior while the UN model is still being validated.

## How To Launch Many Cases Automatically

The standalone runner will:

1. Define the notebook-8 validation grid in Python.
2. Generate SCIANTIX input files in `regression/un_notebook8/cases/test_*`.
3. Run `build/sciantix.x <case_dir>/` for each case.
4. Parse the last row of `output.txt`.
5. Compare requested columns against
   `regression/un_notebook8/reference/python8_reference_points.csv`.
6. Write `regression/un_notebook8/results/un_notebook8_summary.csv`.
7. Write figures under `regression/un_notebook8/figures/`.

## Output Columns To Use

Compare these columns when present:

- `FIMA (%)`
- `Intragranular bulk gas bubble swelling`
- `Dislocation gas bubble swelling`
- `Intragranular gas bubble swelling`
- `Dislocation bubble radius`
- `Dislocation bubble concentration`
- `Dislocation bubble pressure`
- `Dislocation bubble equilibrium pressure`
- `UN grain-face gas`
- `UN released gas`
- `UN fission gas release`
- `Grain-face fractional coverage`
- `UN bulk nucleation rate`

If a column is missing, the standalone suite must write a warning in the summary
and continue. This is important because some SCIANTIX outputs are gated by input
switches.

## SCIANTIX vs Notebook-8 Comparison

The notebook-8 reference file stores the Python model outputs in SCIANTIX units:

- swelling values are fractions, converted from notebook percent columns;
- radii are meters, converted from notebook nm columns;
- pressures are MPa, converted from notebook Pa columns;
- gas inventories and rates use the notebook values directly when units match.

The first pass uses pointwise comparisons at:

- `T = 900, 1200, 1600, 1800, 2000 K` at `1.3% FIMA`;
- `T = 1600 K` at `1.1% FIMA`;
- `T = 1600 K` at `3.2% FIMA`;
- a history case at `T = 1600 K` plotted versus `FIMA (%)`.

## FIMA vs Burnup

SCIANTIX `Burnup` is `MWd/kgUO2`. It is not FIMA.

For Rizk/Ronchi/notebook-8 comparisons, the burnup coordinate must be
`FIMA (%)`. The notebook column named `burnup` is treated here as FIMA percent.
The standalone runner computes irradiation end time from target FIMA percent:

```text
time_h = FIMA_percent * U_atom_density / (fission_rate * 3.6e5)
```

This mirrors the SCIANTIX `Burnup.C` FIMA update and does not use
`Burnup (MWd/kgUO2)` as a substitute. The generated inputs set
`iChromiumSolubility = 1` only to expose the `FIMA (%)` output column; for
`iFuelMatrix = 2`, the UN execution path skips Chromium physics.

For optional Storms FGR, the burnup variable `B` is the atom-percent burnup,
therefore the suite uses `FIMA (%)` and never `Burnup (MWd/kgUO2)`.

## Integration Choice

Create a standalone `regression/un_notebook8` suite first. It should not be
wired into `regression/runner.py` until:

- the notebook-8 reference tolerances are agreed;
- SCIANTIX vs Python deltas are understood;
- the UN model output columns are stable.

This keeps existing regression tests unchanged and makes the UN validation suite
easy to run manually during development.
