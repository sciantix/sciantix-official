# SCIANTIX — Code Context

> Code-architecture reference for the SCIANTIX codebase. Read this before doing
> substantive work on the code. It describes *how the code is structured and runs*;
> physics-model rationale and validation belong in the papers and `references/`.
>
> Repo: `/home/giovanni/sciantix-official` · Branch: `main` · Code version: **2.2.1** (2025)
> Maintainers: G. Zullo, E. Cappellari, G. Nicodemo, A. Zayat, D. Pizzocri, L. Luzzi
> (Politecnico di Milano, Nuclear Engineering Division).
>
> **Last audit: 2026-06-11** — clean rebuild OK with `-Wall -Wextra` (zero warnings);
> full regression suite **109/109 PASS** (atol 1e-8 / rtol 1e-6); unit tests pass
> (`ctest --test-dir build`). Most audit findings were fixed the same day; §9 records
> what was fixed and what remains open.

---

## 1. What SCIANTIX is

An **open-source 0D simulation code** that models the behaviour of a *single grain*
of nuclear fuel, focused on **fission gas behaviour** (Xe, Kr, He), high burnup
structure (HBS) formation/porosity, swelling, and release. It uses **physics-based
rate-theory models** rather than empirical correlations, so it couples cleanly to
lower-length-scale calculations and runs both standalone and as an embedded module
in fuel performance codes (TRANSURANUS, FRAPCON/FRAPTRAN, OFFBEAT).

Language: **C++17**. Build: **CMake ≥ 3.6**. Regression suite: **Python 3.8+**.

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
utilities/             InputExplanation.md + input-template generators
docs/                  Sphinx/Doxygen source for the online docs
references/            references.md (bibliography of the underlying models)
build/                 cmake build dir → sciantix.x (created by Allmake.sh)
context/               this file
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
Sphinx docs; `paper.yml` builds the JOSS paper. There are **no unit tests** — the
regression suite is the only automated verification (see §9.4).

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

### 9.2 Still open

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

---

*Maintenance: update this file when the model `execute()` order, the I/O file set, the
class architecture, or the regression layout changes. Re-run the §9 audit (build +
unit tests + full regression + spot checks) when touching Solver, InputReading, or
MainVariables.*
