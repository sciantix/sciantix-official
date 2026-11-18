# Archive: what was on the `verification_MMS` branch

This records what existed on the (now-deleted) `verification_MMS` branch
before it was removed, for reference — not as a rebuild guide. Unlike
`METHODOLOGY.md` (which documents the approach actually carried forward
into this suite), the `verification_MMS` approach is not being repeated:
it worked by permanently modifying SCIANTIX's production solver code to
compute a fixed manufactured-solution problem, which made those files
unusable for real physics as long as the modification was in place — not
a state that could be merged into a release branch. This suite exists
because that approach needed replacing, not extending.

Commits (`81f1ae2f`..`5e7fb332`, March-September 2025, 8 commits): the
branch never merged into `pre-release/SCIANTIXv2.5` or `main` — it
diverged from an old common ancestor and the two lines drifted
independently by hundreds of commits before this suite was built fresh
off the current `pre-release/SCIANTIXv2.5` instead.

## What it actually did

**Hardcoded the manufactured solution directly into the production
solvers**, rather than reproducing them externally:

- `src/classes/Solver.C` — `Solver::SpectralDiffusion`'s signature was
  changed to take `time` and `D` directly, and its body was rewritten to
  compute the modal source for one specific fixed manufactured solution,
  `C(r,t) = (a²-r²)·exp(0.005t)`, `D(t) = exp(-0.001t)` — the same family
  this suite's case 02 later re-verified independently, in Python, without
  touching the solver.
- `src/models/GasDiffusion.C` and `src/models/GasProduction.C` — both
  hardcoded a second family, `C_M = 0.4a²sin(t)`,
  `S_M = 0.4a²cos(t) + 6sin(t)` (this suite's case 01/07 family). In
  `GasProduction.C`, the real line `productionRate =
  system.getProductionRate();` was commented out and replaced with the
  manufactured source — meaning a normal run on this branch computed the
  manufactured problem unconditionally, not real fission-gas production.
  Case 07's docstring already flagged finding this exact hardcoded
  constant while building the new suite, which is what first suggested a
  combined production+diffusion mass-balance check was intended but never
  packaged as one.
- `include/file_manager/TimeManager.h` / `src/file_manager/TimeManager.C`
  (new files, own comment: *"dedicated to organize the inputs and outputs
  for the verification"*) — three helpers: read a time column from
  `input_history.txt`, interpolate it into `N` sub-steps, and write a
  two-column `(time, value)` file back out. `GasDiffusion.C`/
  `GasProduction.C` used this to write `manufactured_solution.txt` and
  `gasproductionrate_solution.txt` as a side effect of a normal run, for
  external plotting/comparison scripts to read.
- `src/MainSCIANTIX.C` — whitespace-only changes (no functional diff).
- `regression/test_Baker1977__1273K/` — an existing regression test's
  golden `output.txt`/`overview.txt` were regenerated against the modified
  solvers above (204 changed lines), and three new solution files were
  added (`manufactured_solution.txt`, `gasproductionrate_solution.txt`,
  `gas_produced_solution.txt`) alongside a new `input_check.txt` and
  `execution.txt`.

## The exploratory scripts (`utilities/MMS_verification/MMS_Solver/`)

Eight standalone scripts, not integrated with each other or reused as a
shared kernel — each run/edited independently:

- `source_function.py`, `source_projection.py` — `sympy` derivations:
  differentiate the manufactured solution to get the required source term,
  then symbolically project it onto the solver's spherical eigenbasis
  (`ψ_k(r) = sin(kπr/a)/(r√(2πa))`) to get the modal source. This is the
  same derivation this suite's `common/spectral_diffusion_mms.py` module
  states in its own docstring and reuses across six cases — here it
  existed only as a one-off symbolic scratch calculation.
- `manufactured_solution(t).py`, `manufactured_solution(r).py`,
  `manufactured_solution(r,t).py` — plot the manufactured solution's time
  slice, radial slice, and full 3D surface respectively (the source of the
  report's Figure 15).
- `MMS_solver_verification.py` — a standalone script (not a function/
  module) that reimplements the backward-Euler modal update inline and
  compares it to the manufactured solution at one fixed `dt`/mode count.
- `order_of_convergence.py` — sweeps `dt` (own docstring: *"since it is
  backward euler, the order of convergence should be 1"*) to check the
  observed order — the direct precursor to this suite's case 03.
- `volume_averaged_manufactured_solution.py` (plus a stray
  `... copy.py` duplicate) — plots the volume-averaged concentration
  comparison (the report's Figure 16/19 style).

## Why this was retired rather than carried forward

The core problem: hardcoding the manufactured solution into
`Solver::SpectralDiffusion`, `GasDiffusion.C`, and `GasProduction.C`
directly means those files could no longer do real physics while the
modification was in place — verification and normal operation were
mutually exclusive on that branch, in the same files. Merging it into a
release branch would have meant either reverting the hardcoding (losing
the verification capability) or shipping a solver that always computes a
fixed manufactured problem instead of the physics it's supposed to model.

This suite's approach avoids that entirely: every `Solver::` function is
reproduced faithfully in a standalone Python module
(`common/*_mms.py`), and the real C++ source is never modified — SCIANTIX
stays fully usable for physics, and verification runs as an independent
check alongside it rather than a mode switch inside it.
