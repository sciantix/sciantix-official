# SCIANTIX — Code Context

> Code-architecture reference for the SCIANTIX codebase. Read this before doing
> substantive work on the code. It describes *how the code is structured and runs*;
> physics-model rationale and validation belong in the papers and `references/`.
>
> Repo: `/home/giovanni/sciantix-official` · Branch: `main` · Code version: **2.2.1** (2025)
> Maintainers: G. Zullo, E. Cappellari, G. Nicodemo, A. Zayat, D. Pizzocri, L. Luzzi
> (Politecnico di Milano, Nuclear Engineering Division).
>
> **Last audit: 2026-07-25** (previous: 2026-07-02, 2026-06-11) — clean rebuild OK with
> `-Wall -Wextra` (zero warnings); full regression suite **110/110 PASS** (atol 1e-8 /
> rtol 1e-6); unit tests pass (`ctest --test-dir build`). §9 records what each audit
> fixed and what remains open.

---

## 1. What SCIANTIX is

An **open-source 0D simulation code** that models the behaviour of a *single grain*
of nuclear fuel, focused on **fission gas behaviour** (Xe, Kr, He), high burnup
structure (HBS) formation/porosity, swelling, and release. It uses **physics-based
rate-theory models** rather than empirical correlations, so it couples cleanly to
lower-length-scale calculations and runs both standalone and as an embedded module
in fuel performance codes (TRANSURANUS, FRAPCON/FRAPTRAN, OFFBEAT).

Language: **C++17**. Build: **CMake ≥ 3.10**. Regression suite: **Python 3.8+**.

---

## 2. Directory map

```
src/                 implementation (.C)
  MainSCIANTIX.C       standalone entry point + time loop
  MainVariables.C      global C-array state (history/variables/modes)
  Sciantix.C           Sciantix() — one physics call per time step
  classes/             Simulation, System, Matrix, Solver, SciantixVariable
  models/              one .C per physics model (see §6)
  operations/          Set*/Update* — wire C arrays <-> SciantixArray objects
  file_manager/        I/O: InputReading, InputInterpolation, Output, TimeStepCalculation, Initialization
  coupling/            TUSrcCoupling.C — TRANSURANUS coupling glue
  namespaces/          ErrorMessages
include/               headers, mirrors src/ layout (several classes are header-only)
regression/            Python regression suite + validation cases (§7)
tests/                 unit tests (unit_tests.C, run via CTest — see §7)
utilities/             InputExplanation.md + input-template generators + this file
docs/                  Sphinx/Doxygen source for the online docs
references/            references.md (bibliography of the underlying models)
build/                 cmake build dir → sciantix.x (created by Allmake.sh)
```

---

## 3. Architecture & data model

SCIANTIX is built around a small set of named-collection objects rather than ad-hoc
variables. The central container is a template:

- **`SciantixArray<T>`** (`include/classes/SciantixArray.h`) — a `std::vector<T>` plus
  an internal `std::map<std::string,int>` for O(log n) **name→index** lookup. Supports
  both `array[i]` and `array["name"]`, `push()` (add-or-replace), `isElementPresent()`,
  and iteration. Everything named in the simulation lives in one of these.

Class hierarchy (all under `include/classes/`):

| Class | Role | Key members |
|---|---|---|
| `Variable` | base | `name` |
| `SciantixVariable` | a physical state variable | `uom`, `initial_value`, `final_value`, `to_output`; `getIncrement()` = final−initial, `resetValue()`, `rescale*` |
| `InputVariable` | scalar input/scaling factor | `value` |
| `Material` | base for physical objects | `name`, `reference` |
| `Gas` : Material | fission gas species | `atomic_number`, `mass_number`, `van_der_waals_volume`, `decay_rate`, `precursor_factor` |
| `Matrix` : Material | fuel matrix (UO₂) | ~40 props: density, lattice param, grain radius, GB mobility, surface tension, Schottky volume, nucleation rates, elastic/poisson/shear, Cr chemistry |
| `System` : Material | a gas-in-matrix system | holds a `Gas` + `Matrix`; `yield`, `diffusivity`, `bubble_diffusivity`, `resolution_rate`, `trapping_rate`, `nucleation_rate`, `production_rate`, `modes`, `restructured_matrix` |
| `Model` : Material | a physics model record | `overview` string, `parameter` vector<double> |

**Rate-theory pattern:** `System`/`Matrix` setters (e.g. `setFissionGasDiffusivity`,
`setResolutionRate`) take `SciantixArray` references to the current state + scaling
factors, compute a scalar rate from temperature/burnup/microstructure, and store it.
The `Solver` then uses those rates as ODE/PDE coefficients.

**`Simulation`** (`include/classes/Simulation.h`, `src/classes/Simulation.C`) is a
**singleton** (`getInstance()`, private ctor, Meyers function-local static). It owns every
collection — `sciantix_variable`, `history_variable`, `physics_variable`, `model`,
`sciantix_system`, `matrices`, `gas`, `input_variable`, `scaling_factors`, one
`Solver`, and the diffusion-mode state (`n_modes = 40`, `modes_initial_conditions`
sized 720). Each physics model is a **member method** of `Simulation` (not a separate
class), implemented in its own file under `src/models/`.

### Solver (`include/classes/Solver.h`, `src/classes/Solver.C`)

Analytical/semi-analytical per-step integrators (each advances one variable over `dt`):

- `Integrator` — y′ = k
- `LimitedGrowth` — y′ = k/y + S (quadratic)
- `Decay` — y′ = −Λy + S → (y+S·dt)/(1+Λ·dt)
- `BinaryInteraction` — y′ = −k·y² → y/(1+k·y·dt)
- `SpectralDiffusion` / `…2equations` / `…3equations` — spatially-averaged
  dy/dt = D∇²y + S − Λy via **spectral eigenfunction (mode) expansion**; params are
  `[N_modes, D, radius, production, loss_rate]`
- `Laplace2x2` / `Laplace3x3` / `Laplace(N,A,b)` — linear solves (Cramer)
- `QuarticEquation`, `NewtonBlackburn`, `NewtonLangmuirBasedModel` — Newton iterations
  for thermochemistry/sorption
- helpers: `dotProduct1D/2D`, `modeInitialization`

**Diffusion modes:** the intragranular diffusion problem is solved spectrally with
**40 modes per gas**. The 720-long mode array is partitioned by `getDiffusionModes`,
`…Solution`, `…Bubbles` for Xe, Kr, He, Xe133, Kr85m, and "Xe in HBS".

---

## 4. Execution flow

**Standalone** (`src/MainSCIANTIX.C`):

1. Resolve input path (CLI arg or cwd).
2. `InputReading()` → fill the global C arrays (`Sciantix_options/history/variables/
   scaling_factors`).
3. `Initialization()` → project initial gas concentrations onto diffusion modes.
4. **Time loop** until `Time_h > Time_end_h`:
   - linearly interpolate Temperature, Fission rate, Hydrostatic stress, Steam pressure
     at the current time into `Sciantix_history[]`;
   - call `Sciantix(options, history, variables, scaling_factors, diffusion_modes)`;
   - `simulation->output()` appends the step to `output.txt`;
   - `TimeStepCalculation()` picks the next `dt` (each input interval is split into
     `Number_of_time_steps_per_interval`, default 100).
5. Write wall-clock timing to `execution.txt`.

**Per-step physics** — `Sciantix()` (`src/Sciantix.C`) drives the singleton:
`initialize → execute → update → output`. `execute()` (`src/classes/Simulation.C:46`)
calls the models **in this fixed order**:

```
Burnup → EffectiveBurnup → Densification                 (skipped under COUPLING_TU)
GapPartialPressure → UO2Thermochemistry → StoichiometryDeviation
HighBurnupStructureFormation → HighBurnupStructurePorosity → Microstructure
ChromiumSolubility → GrainGrowth → GrainBoundarySweeping
GasProduction → GasDecay → IntraGranularBubbleBehavior → GasDiffusion
GrainBoundaryMicroCracking → GrainBoundaryVenting → InterGranularBubbleBehavior → GasRelease
```

Under `-DCOUPLING_TU=ON`, Burnup/EffectiveBurnup/Densification are supplied by the
host fuel-performance code, so they are compiled out here.

---

## 5. Input / Output

All files are read from / written to the run directory (`utilities/InputExplanation.md`
is the authoritative syntax reference). Templates are generated by the scripts in
`utilities/inputExample/`.

**Inputs:**

| File | Contents |
|---|---|
| `input_settings.txt` | integer model-selection flags (one per line, inline-commented): grain-growth model, diffusivity model, intra/inter-granular bubble model, HBS options, chemistry, output mode `iOutput`, etc. |
| `input_initial_conditions.txt` | grain radius, initial Xe/Kr/He inventories (intragranular / GB / released), bubble props, burnup, irradiation time, fuel density, U-isotopics, stoichiometry |
| `input_history.txt` | time (h), temperature (K), fission rate (fiss/m³·s), hydrostatic stress (MPa), optional steam pressure — **linearly interpolated** between rows |
| `input_scaling_factors.txt` *(optional)* | multiplicative scaling factors (resolution, trapping, nucleation, diffusivity, temperature, fission rate, He production…); default 1.0 if absent |

**Outputs:**

| File | Contents |
|---|---|
| `output.txt` | tab-separated; header row of `name (uom)`, one row per time step. `iOutput`=1 selected variables, =2 all variables |
| `overview.txt` | written once; lists active models/matrices/systems/settings with references |
| `execution.txt` | wall-clock time, CLOCKS_PER_SEC, ticks, total steps |
| `input_check.txt` | echo of parsed inputs for verification |

Global state lives in `src/MainVariables.C` / `include/MainVariables.h`. Array sizes
are named constants in `MainVariables.h` (`SCIANTIX_OPTIONS_SIZE` 40,
`SCIANTIX_VARIABLES_SIZE` 300, `SCIANTIX_DIFFUSION_MODES_SIZE` = `N_MODE_BLOCKS` 18 ×
`N_DIFFUSION_MODES` 40 = 720), plus `Time_h`, `dTime_h`, `Time_end_h` and the
history-input vectors (`Time_input` etc.), which grow dynamically with the number of
history rows (no fixed cap). An out-of-range model flag in `input_settings.txt` is a
**fatal error** (`ErrorMessages::Switch` → log + stderr + exit 1).

---

## 6. Physics models (`src/models/`)

One file per model; each is a `Simulation` method. Selected via `input_settings.txt`
flags.

| File | Models |
|---|---|
| `Burnup.C`, `EffectiveBurnup.C` | local burnup (MWd/kgUO₂, FIMA), effective burnup |
| `Densification.C` | fuel densification |
| `GasProduction.C`, `GasDecay.C`, `GasRelease.C` | gas source from fission, radioactive decay, release to free volume |
| `GasDiffusion.C` | intragranular diffusion (spectral) |
| `GrainGrowth.C`, `GrainBoundarySweeping.C` | grain growth, GB sweeping of gas |
| `IntraGranularBubbleBehavior.C` | intra-granular bubbles (3 model variants — nucleation/trapping/resolution; White-Tucker correlation; similarity-ratio) |
| `InterGranularBubbleBehavior.C` | lenticular GB bubbles: nucleation, vacancy-driven growth, coalescence |
| `GrainBoundaryMicroCracking.C`, `GrainBoundaryVenting.C` | crack/heal of GB faces under transients; venting release |
| `HighBurnupStructureFormation.C`, `HighBurnupStructurePorosity.C` | HBS restructuring and porosity evolution |
| `StoichiometryDeviation.C`, `UO2Thermochemistry.C`, `GapPartialPressure.C` | O/M deviation, urania thermochemistry, gap oxygen partial pressure |
| `ChromiumSolubility.C`, `Microstructure.C` | Cr-doped fuel: solubility vs T & pO₂; lattice parameter / theoretical density |

Model literature is catalogued in `references/references.md` and the headers' Doxygen
`@ref` tags. Key code papers: Zullo et al. JNM 587 (2023) 154744; Pizzocri et al. JNM
532 (2020) 152042.

---

## 7. Build & regression

**Build** (`Allmake.sh` → `build/` → `cmake .. && make -j`):

- Default target: executable `build/sciantix.x`. C++17 is set explicitly; `-Wall
  -Wextra` are enabled (GNU/Clang) and the tree is warning-clean — keep it that way.
- **Unit tests**: `tests/unit_tests.C` (plain asserts, no framework) covers the
  `Solver` integrators and `SciantixArray`; built as `build/unit_tests.x` and run via
  `ctest --test-dir build`.
- `-DCOUPLING_TU=ON` → static library `libsciantix.a` (omits Burnup/EffectiveBurnup/
  Densification — see §4).
- `Allclean.sh` removes `build/`, `obj/`, the executable, and `__pycache__`.
- `CMakeLists.txt` globs all `src/**/*.C` and `include/**`; object files copied to `obj/`.

**Regression** (`runRegression.sh` = clean + build + run; or `python3 -m
regression.runner --all -j $(nproc)`):

- Each case is a directory (e.g. `regression/white/test_White2004_4000-1/`) holding the
  three `input_*.txt` files plus a golden reference `output_gold.txt`.
- Runner executes `sciantix.x <case>/`, then compares `output.txt` vs `output_gold.txt`
  element-wise with **atol 1e-8 / rtol 1e-6**.
- `--mode-gold`: `0` run+compare (default), `1` run+rewrite gold, `2` compare only,
  `3` rewrite gold only. **Regenerate gold deliberately**, never to paper over a diff.
- Core logic: `regression/core/{generic_runner,compare,parser,common,report}.py`;
  an HTML report is written to `regression/report.html`. Plots via `plotter.sh`.

**Validation groups** (each ↔ an experimental dataset / phenomenon):

| Group | Dataset / phenomenon |
|---|---|
| `baker`, `cornell` | early FGR datasets (Baker 1977; Cornell 1969) |
| `white` | White (2004) GB bubble / FGR model validation |
| `kashibe` | Kashibe (1990s) burnup/restructuring experiments |
| `talip` | Talip (2014) He behaviour / annealing |
| `vercors` | VERCORS severe-accident release campaign |
| `hbs` | UO₂ high burnup structure porosity (`test_UO2HBS_*`) |
| `oxidation` | UO₂ oxidation / stoichiometry deviation |
| `chromium` | Cr-doped fuel solubility + microstructure |
| `contact` | contact / mechanics case |
| `analytics`/`gpr` | analytic power-pulse checks (`pulse` is an alias for `analytics`); GPR series |

`regression/white/bias.py` is a parameter-selection utility (not a test): it sweeps
scaling-factor combinations over the White (2004) cases and reports parity statistics
(BIAS/RMSE/MAD) to support sensitivity-guided effective-parameter selection.

**CI** (`.github/workflows/`): `ci.yml` builds `sciantix.x` and runs the regression
suite on push/PR; `clang-format-auto.yml` auto-formats C++; `pages.yml` deploys the
Sphinx docs; `paper.yml` builds the JOSS paper. Since 2026-07, `ci.yml` also triggers
on `pull_request`, runs `ctest` after the build, and has job timeouts.

---

## 8. Conventions when changing the code

- **Follow `.clang-format`** at the repo root; match surrounding style.
- A new physics model = a new `Simulation` method + `src/models/<Name>.C` + a flag in
  `input_settings.txt`, wired into the `execute()` order in `src/classes/Simulation.C`.
- Add/expose state via `SciantixVariable` (set `to_output`/`uom`) in the `operations/`
  setup, not as loose globals.
- **Any physics change must be reflected in the regression gold** — run the suite,
  inspect diffs, and only regenerate gold when the change is intended and understood.
- Prose / documentation style for the HBS paper follows the locked-in rules recorded in
  memory (UK English; "high burnup" no hyphen; porosity symbol **ξ**; "parameter
  selection guided by sensitivity analysis", never "calibration"). See `MEMORY.md`.

---

## 9. Audit findings & remediation (2026-06-11)

The 2026-06-11 audit found the weaknesses below; most were fixed the same day with
the full regression suite staying at 109/109 PASS (all fixes are gold-neutral) and
the TU-coupling library build verified.

### 9.1 Fixed

- **History file > 1000 rows was undefined behaviour** — the read loop wrote past
  vectors pre-sized to 1000. Now `InputReading.C` reads into locals and `push_back`s
  (no cap); an empty/malformed `input_history.txt` is a fatal error. When the
  steam-pressure column is absent the vector is zero-filled to match (the time loop
  interpolates it unconditionally — that dependency caused a segfault during the fix).
- **Out-of-range model flags only logged a warning** that was *never flushed to disk*
  (`writeErrorLog()` had no call sites) and the run continued with uninitialised model
  state. `ErrorMessages::Switch` is now `[[noreturn]]`: error log + stderr + exit 1.
  New helpers `ErrorMessages::Fatal/Warning` write immediately to both.
- **Newton solvers failed silently.** `NewtonBlackburn`, `NewtonLangmuirBasedModel`,
  and `QuarticEquation` now log a warning on non-convergence. `NewtonBlackburn` checks
  its argument *before* taking `log()` (returns the initial value on a non-positive
  argument). **`QuarticEquation` had a real bug** — `if (function < tol)` accepted any
  negative residual as converged, returning a wrong root after one step when Newton
  approaches from below (caught by the new unit test); now `fabs(function) < tol`,
  `max_iter` 5 → 50. No regression case changed (the bug never fired on validated
  paths). Tolerances stay at 1e-3 to keep gold unchanged.
- **NaN zeroing in `InterGranularBubbleBehavior.C`** now emits a once-per-run warning
  instead of being fully silent.
- **Unguarded division by grain radius** in `GasRelease.C` (intergranular swelling) —
  now guarded, returns 0 swelling for a degenerate radius.
- **`getDiffusionModes*` returned unchecked `nullptr`** for unknown gas names — now a
  fatal error via `ErrorMessages::Fatal`.
- **Singleton leak** — `Simulation::getInstance()` is now a Meyers singleton
  (function-local static; no `new`, destructor runs at exit).
- **Magic sizes named** — `MainVariables.h` defines `SCIANTIX_*_SIZE`,
  `N_DIFFUSION_MODES`, `N_MODE_BLOCKS`; `Simulation`'s constructor and the
  `SetVariables.C` mode-copy loop use them (the `j <= 17` literal is gone).
- **Shared physics constants named** — Blackburn (1973) `32700`/`9.92` are
  `blackburn_enthalpy`/`blackburn_entropy` in `Constants.h`, used by `Solver.C` and
  `StoichiometryDeviation.C`. The U-mass-fraction near-duplicates are now named
  `uranium_mass_fraction` locally with cross-referencing comments (values kept
  bit-identical: 0.8815 in `GrainGrowth.C`, 0.8814 in `GrainBoundaryMicroCracking.C`
  as published in Barani 2017 — unifying them would change gold).
- **Build hygiene** — `-Wall -Wextra` enabled and all 12 pre-existing warnings fixed;
  C++17 set explicitly; `${PROJECTSOUR}` typo fixed.
- **No unit tests** — added `tests/unit_tests.C` (Solver integrators, Cramer solves,
  QuarticEquation, SpectralDiffusion sanity, SciantixArray semantics), wired into
  CTest.

### 9.2 Fixed in the 2026-07-02 review

A four-agent review (models / numerical core / I-O wiring / tooling) found and fixed:

- **Resolution-rate scaling factor applied twice (sf²)** — `System::setResolutionRate`
  multiplied by `scaling_factors["Resolution rate"]` inside every switch case *and*
  again after the switch. Any run with sf ≠ 1 (i.e. `bias.py` sweeps) actually used
  sf². Post-switch duplicate removed; gold-neutral (all regression sf = 1).
  **Resolution-rate sensitivity results produced before this fix used sf² and must be
  re-run.**
- **"He at grain boundary" final value wired to `Sciantix_variables[71]`** (fabrication
  porosity) instead of `[17]` — digit-swap typo in `SetVariablesFunctions.C`. Masked on
  all validated paths (GasDiffusion overwrites the final value before consumers read
  it), hence gold-neutral, but a live state-corruption hazard.
- **Grain-boundary venting applied twice to each gas with the HBS matrix active** —
  the venting loop lacked the `getRestructuredMatrix() == 0` guard every sibling loop
  has, so with `iFuelMatrix = 1` the shared `[gas] at grain boundary` variable was
  vented once per system. Guard added. Gold-neutral (no regression case combines
  venting with HBS) — but it affected HBS + venting runs, e.g. on the porosity branch.
- **Scaling-factor slot layout unified** — `InputReading.C`/`TUSrcCoupling.C` labelled
  slots 4/5/6 `sf_diffusivity2`/`sf_temperature`/`sf_fission_rate` while every input
  file (and the index-based consumption) used temperature / fission rate /
  diffusion-based release; `bias.py` and the input generator used legacy
  "screw/span/cent parameter" names. All unified on the file convention (code labels,
  `getScalingFactorsNames()`, `bias.py`, generator, `InputExplanation.md`). The Cr
  diffusivity pre-exponential no longer reads the phantom "Diffusivity2" knob (it was
  actually reading the temperature factor); it is now unscaled. Note: the
  "diffusion-based release" factor (slot 6, Cappellari et al. 2025) is read but
  **consumed nowhere** in this branch.
- **`57 / 2` integer division** in `StoichiometryDeviation.C` cases 5 and 6 — the
  Massih/Langmuir exponent α was 28 instead of 28.5. **Gold regenerated** for the two
  Cox oxidation cases and Vercors5 (stoichiometry deviation shifts ~1.8%, oxygen
  partial pressure up to ~20%).
- **HBS pore-variance equation used `matrices["UO2"]` rates** while the mean used
  `matrices["UO2HBS"]` — copy-paste slip. **Gold regenerated** for `test_UO2HBS`
  (variance columns shift ~2%; means unchanged).
- **`System.h` shadowed `Material::name`/`reference`** — every system reference string
  was written to the shadow and lost; `overview.txt` printed empty references. Shadow
  members removed; overview now shows full per-system references (not gold-compared).
- **NaN silently passed the regression comparison** — `compare.py`'s tolerance mask is
  False for NaN. One-sided NaN is now a failure (both-NaN stays equal — the trailing
  empty output column parses to NaN on both sides); column headers are now compared too.
- **CI hardening** — `pull_request` trigger, `ctest` step, `timeout-minutes`,
  checkout/setup-python action bumps.
- `GrainBoundarySweeping.C` mode offsets use `n_modes` instead of literal `40`
  (behaviour-identical).

### 9.3 Still open (2026-06 + 2026-07 findings)

New in 2026-07 (see the review conversation for file/line detail):

- Input-parsing robustness (partly closed on 2026-07-25, see §9.5): short lines in
  `input_initial_conditions.txt` cause out-of-bounds vector reads; a 4-column history
  with `iStoichiometryDeviation > 0` interleaves times/pressures; a history not
  starting at t = 0 causes a dt = 0 infinite loop; `InputInterpolation.C` uses `short`
  indices (UB past 32 767 rows) and NaNs on a duplicated final time point.
- Solver guards: `Laplace` (4×4) lacks the determinant check its 2×2/3×3 siblings
  have (and the caller's NaN sweep passes ±inf); `BinaryInteraction` denominator can
  cross zero for negative increments; Newton solvers can return NaN after the
  non-convergence warning; unqualified `abs()` on doubles at `Solver.C:448,483`;
  C-style VLAs in `Laplace`/`det` (GCC extension).
- `iReleaseMode` switch has no `default:` error (invalid values silently freeze GB
  bubble state); the T < 1000 K pO2 cutoff in `StoichiometryDeviation.C` is dead code
  (unconditionally overwritten); `GapPartialPressure` model never `model.push`ed;
  unguarded division by total U in `Burnup.C`; wrong lookup key (missing `i` prefix)
  in the HBS porosity error branch.
- Time loop: FP accumulation can skip the final sub-step (1-ulp overshoot of
  `Time_end_h`); `TimeStepCalculation` returns a negative dt past the last history
  point (currently unreachable).
- TU coupling: settings read from CWD but scaling factors from `TestPath`; history
  slots 7/8 double-booked (Time/step-number vs Burnup) under `COUPLING_TU`.
- Tooling: missing test groups vanish silently; no subprocess timeout in `common.py`;
  the auto-format workflow pushes commits that never run CI; `error_log.txt` is not
  cleaned between runs; legacy `utilities/` scripts ignore exit codes.

### 9.4 Still open (from 2026-06)

- **Initial conditions are wired by hardcoded index** (e.g. `Sciantix_variables[54]`,
  `[66]`, `[150]` in `InputReading.C`) with matching hardcoded indices in
  `src/operations/`. No single source of truth for the layout; adding/reordering
  variables risks silent index drift. A proper fix is an index-constants header (or
  name-based wiring) used by both sides.
- **Vercors5 hits a NewtonBlackburn divergence** — the new non-convergence warning
  fires once in `regression/vercors/test_Vercors5` with `|f| = nan` (Blackburn
  thermochemistry outside its validity range during the severe-accident transient).
  The case still matches gold; the divergence is pre-existing physics behaviour now
  made visible. Worth investigating before extending the oxidation models.
- Newton tolerances (1e-3, absolute) are loose and unscaled — kept for gold
  compatibility; revisit alongside a deliberate gold regeneration.
- The regression suite remains end-to-end; unit tests cover `Solver`/`SciantixArray`
  only, not the models or the input parser.
- `CMakeLists.txt` still uses `GLOB_RECURSE` for sources and has no `install()`
  target (deliberate project conventions, low risk).

### 9.5 Fixed in the 2026-07-25 review

Full suite 110/110 PASS after each change (the suite grew from 109 to 110 with
`analytics/test_openPorosity`). Every fix below is gold-neutral except the
densification one, which changed the physics and required a deliberate gold
regeneration.

- **The grain-boundary venting guard added in §9.2 had been lost again.** The
  `getRestructuredMatrix() == 0` guard in `GrainBoundaryVenting.C` was reintroduced in
  commit `c21e418a` and dropped by the merge `8f66f7dd`, so with `iFuelMatrix = 1` the
  shared `[gas] at grain boundary` variable was again vented once per system. Restored,
  this time with a comment stating why it is there. Measured on `test_UO2HBS` with
  `iGrainBoundaryVenting = 1`: Xe at grain boundary +18%, intergranular swelling +18%,
  intergranular atoms per bubble +45%, 14 of 55 output columns affected. **Results
  produced on the porosity branch with venting active must be re-run.**
- **`ReadOneSetting`/`ReadOneParameter` truncated the inline comment** at 256
  characters (`ignore(256, '\n')`). A longer comment left its tail in the stream and
  every following entry was read as zero, with no diagnostic: a 400-character comment
  on `test_openPorosity` silently disabled `iDensification` and shifted the final FGR by
  12 % while exiting 0. The skip is now unbounded.
- **A malformed value is now a fatal error** naming the entry, instead of latching
  failbit and zeroing everything after it. The check is deliberately `fail() && !eof()`:
  reaching the end of the file stays legal, because no case in the validation database
  supplies all 14 initial-condition blocks (95 supply 12, 15 supply 13) and the trailing
  defaults of zero come precisely from that tolerance. Making plain `fail()` fatal would
  break all 110 cases.
- `comment` and `variable` are now initialised in the three read helpers; the
  `comment == '#'` test previously read an indeterminate value when extraction failed.
- **The input generators no longer emit files the parser misreads.**
  `print_input_settings.py` was missing the trailing newlines and the
  `iChromiumSolubility`/`iReleaseMode` entries (so `iDensification` landed in the wrong
  slot); `print_input_initial_conditions.py` was missing the `Chromium content` block.
  Both now assert their own completeness, and `utilities/InputExplanation.md` documents
  templates verified byte-identical to their output.
- `--pulse` / `--analytics` now run only the analytics group: `analytics` was excluded
  from the runner's group discovery, so selecting it left `explicit_selection` False and
  the whole suite ran.
- `UpdateVariables.C` uses `N_MODE_BLOCKS` instead of the literal `j <= 17`, completing
  the §9.1 cleanup (behaviour-identical).
- Documentation aligned with the code: the regression invocations quoted in
  `index.rst`, `installation.rst` and `CONTRIBUTING.md` pointed at a `regression.py`
  that no longer exists; `regression.rst` mis-described four validation suites (White is
  intergranular swelling from White 2004, Talip is helium annealing, CONTACT is the
  Xe133/Kr85m R/B experiment, Cornell is intragranular bubbles) and `--mode-gold 3`;
  `conf.py` still declared release 2.1.
- **Densification depended on the number of time steps, not on burnup** (the §9.3 item
  is now closed; **gold regenerated**). `Densification.C` reduced the fabrication
  porosity by applying `(1 - f_dens)` to its *running* value once per step, so the
  excess over the residual porosity decayed as `x0 * (1 - f_eq)^N` with `N` the step
  count. It was not an inaccurate integration but a scheme with no continuum limit: as
  the step size goes to zero `f_eq` stays finite, so the porosity collapses onto the
  residual floor instantly, whatever the burnup. Changing only
  `Number_of_time_steps_per_interval` from 25 to 400 moved the final porosity by 11.9 %
  and the FGR by 10 %; refining the history rows gave the same. `f_dens` is cumulative,
  so the porosity is now evaluated in closed form from the as-fabricated value, obtained
  from `Residual porosity` — the only quantity of that family that is written once and
  never modified. The ODE for `f_dens` is unchanged, and the reference constants are
  unaffected because they were fitted on the experimental points outside the code.
  Final porosity is now identical to 1e-8 across step counts. Gold for
  `test_openPorosity`: porosity +30.9 % (it no longer collapses onto the floor), venting
  probability +14.8 %, FGR +11.8 %. Two divergences from Pagani et al. (2026) are
  recorded in the source: Eq. (8) is printed as a growth equation, and the text calls
  `f_dens` the fraction of the original fabrication porosity whereas it is the fraction
  of the densifiable part — a factor four.
- **GB sweeping acting on the He mode blocks only is intended** (the §9.3 item is closed
  as a decision, not a defect): confirmed as physically correct by the maintainers.
  `docs/source/models/grain_boundary_sweeping.rst` still describes a generic fission-gas
  mechanism and is worth aligning.

---

*Maintenance: update this file when the model `execute()` order, the I/O file set, the
class architecture, or the regression layout changes. Re-run the §9 audit (build +
unit tests + full regression + spot checks) when touching Solver, InputReading, or
MainVariables.*
