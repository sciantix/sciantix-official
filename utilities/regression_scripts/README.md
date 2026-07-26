# Legacy regression scripts — superseded, not runnable

Nothing in this directory runs any more. It is the regression driver that predates
`regression/runner.py`, kept for reference while its plotting logic is ported.

## Why it cannot run

`regression.py` resolves its working directory as the folder containing itself, then

- copies `../build/sciantix.x` — from here that resolves to `utilities/build/sciantix.x`,
  which does not exist, so `main()` fails on its first statement; and
- scans that same folder for `test_*` case directories, of which there are none.

Both assumptions belong to the old flat layout, where the cases sat next to the scripts
under `regression/test_<case>/`. `main` reorganised the cases into thematic groups
(`regression/baker/`, `regression/white/`, …) and the drivers were never updated.

## What replaced it

    python3 -m regression.runner --all -j $(nproc)

with the shared logic in `regression/core/` and one optional plotting script per group
(`regression/<group>/plot.py` or `parity_plot.py`, all run by `plotter.sh` at the repo
root). See §7 of `utilities/CONTEXT.md`.

## What has not been ported

These scripts are substantially larger than their replacements — 1132 lines against 487
for Kashibe, 463 against 119 for Baker, 654 against 424 for White — and the difference is
mostly comparison against experimental data. Worth a look before writing new plotting
code:

- `regression_kashibe1990.py`, `regression_kashibe1991.py`, `regression_kashibe1991_2.py`,
  `regression_kashibe1993.py` — the new suite has a single `kashibe` group covering all
  23 cases, and `regression/kashibe/plot.py` covers the 1990 series only.
- `regression_baker_GPR.py`, `regression_baker_porosityevolution.py` — the latter drives
  a case, `test_Baker1977_1273K_porosityevolution`, that is not in the repo at all.
- `Summary_of_results.py`, `globalSensitivityAnalysis.py` — reporting and sensitivity
  utilities with no equivalent under `regression/`. Compare with
  `utilities/singleSensitivityAnalysis.py`, which is separate and still current.

## Before deleting this directory

Port what is worth porting first. Everything here is in git history either way, but the
plotting recipes are easier to lift from a file on disk than from an archaeological dig.
