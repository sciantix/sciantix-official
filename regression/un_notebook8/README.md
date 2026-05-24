# UN Notebook-8 Regression Suite

Standalone validation suite for the SCIANTIX UN notebook-8 model.

This suite is intentionally not wired into the top-level `regression/runner.py`
yet. It generates its own UN input decks, runs `build/sciantix.x <case_dir>/`,
parses `output.txt`, compares the final timestep against notebook-8 reference
points, and creates CSV/PNG artifacts under this directory.

## Run

From the repository root:

```bash
python3 regression/un_notebook8/run_un_notebook8.py --exe build/sciantix.x
```

Then, to regenerate figures from an existing summary:

```bash
python3 regression/un_notebook8/plot_un_notebook8.py
```

The runner writes:

- `regression/un_notebook8/results/un_notebook8_summary.csv`
- `regression/un_notebook8/figures/*.png`

## Cases

The minimum validation grid is:

- `T = 900, 1200, 1600, 1800, 2000 K` at `1.3% FIMA`;
- `T = 1600 K` at `1.1% FIMA`;
- `T = 1600 K` at `3.2% FIMA`;
- `test_UN_history_T1600`, a history case plotted versus `FIMA (%)`.

Input files are generated into `cases/test_*` at runtime. The case directories
are kept in git with `.gitkeep` files so the layout is visible before running.

## FIMA vs Burnup

SCIANTIX `Burnup` is in `MWd/kgUO2`; it is not FIMA. Notebook-8 and
Rizk/Ronchi comparisons use atom-percent burnup, therefore this suite uses only
the `FIMA (%)` output column for the burnup coordinate.

The generated end time is computed as:

```text
time_h = FIMA_percent * U_atom_density / (fission_rate * 3.6e5)
```

This mirrors the SCIANTIX FIMA update. The generated settings set
`iChromiumSolubility = 1` only to expose the `FIMA (%)` column in the standard
output. For `iFuelMatrix = 2`, Chromium physics is not run by the UN execution
path.

Optional Storms FGR comparisons, when added, must use `FIMA (%)` as the
atom-percent burnup `B` and must not use SCIANTIX `Burnup`.

