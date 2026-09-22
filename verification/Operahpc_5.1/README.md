# SCIANTIX verification cases (method of manufactured solutions)

This folder collects self-contained MMS verification cases for SCIANTIX,
modelled on the verification approach described in **OperaHPC WP5-D5.1,
Section 4.3 "Focus on OFFBEAT-SCIANTIX scheme"** (and Section 4.1's
description of separate-effect verification tests, `Table 4`).

As in the report, each case:

1. defines a manufactured solution `c_M` (and, where relevant, a manufactured
   diffusion coefficient `D_M`),
2. derives the corresponding manufactured source term analytically,
3. runs the SCIANTIX numerical scheme (reproduced in Python, matching the
   discretisation in `src/classes/Solver.C`) against that source term,
4. compares the numerical solution `c_N` to `c_M` and reports **all four**
   error metrics used in the report — L1 (average), L2 (RMSE), Linf (max) and
   the final-value error — plus, where applicable, observed order of
   convergence and wall-clock runtime scaling with time step and mode count
   (Fig. 18's four panels).

See `VERIFICATION_TAXONOMY.md` for what *type* of thing each case verifies
(standalone solver, convergence/cost, interface, outer coupling-scheme,
two-way feedback, ...) and why that type matters specifically for the
SCIANTIX-OFFBEAT coupling, rather than for SCIANTIX run standalone.

This mirrors how OFFBEAT's own separate-effect tests are organised under a
top-level `Cases` folder (per the report, Section 4.1) — each subfolder here
is one independent, runnable verification case with its own `run_case.py`.

## Cases

| # | Case | Equation verified | Manufactured solution | Report reference |
|---|------|--------------------|------------------------|-------------------|
| 01 | `case_01_spectral_diffusion_sinusoidal` | Intra-granular spectral diffusion, Eq. (10) | `c_M(r,t) = α(a²−r²)sin(εt)`, `D_M = γ` | Eq. (11)-(14), Fig. 15-17 |
| 02 | `case_02_spectral_diffusion_exponential` | Intra-granular spectral diffusion, Eq. (10) | `c_M(r,t) = (a²−r²)exp(0.005t)`, `D_M(t) = exp(-0.001t)` | Same solver, alternate MMS; matches the case wired into `Solver.C`/`GasDiffusion.C` on this branch |
| 03 | `case_03_convergence_study` | Same as case 01 | Same as case 01 | Fig. 18 (temporal + spatial convergence) |
| 04 | `case_04_grain_boundary_release` | Grain-boundary gas balance, Eq. (15): `dc_GB/dt = J_in − R` | `c_GB,M(t) = C0 + A·sin(ωt)` (fast-oscillating) | Fig. 19 |
| 05 | `case_05_hybrid_mode_allocation` | Same as case 01, run for 5 coupled OFFBEAT-node species | `c_M` as in case 01, at 40 modes vs. hybrid (20 modes stable / 40 modes non-stable) | Table 5, §4.3 text on the `development/sciantix2` hybrid allocation |
| 06 | `case_06_coupled_precursor_daughter_diffusion` | Two-species coupled diffusion (`Solver::SpectralDiffusion2equations`-style production/decay coupling) | `c1_M=(a²−r²)α₁sin(ε₁t)` (daughter), `c2_M=(a²−r²)α₂sin(ε₂t)` (precursor), coupled by decay constant λ | Not in the report — added for OFFBEAT-coupling relevance, see below |
| 07 | `case_07_fission_gas_release` | Fission gas release by mass balance: `released = produced − decayed − in_grain` (`GasDiffusion.C`, `iGrainBoundaryBehaviour=0` path) | `c_M` as in case 01 (in-grain term) + independent `N_M(t)` integrated with `Solver::Integrator` (produced term) | Fig. 19's release quantity, verified end-to-end via the exact mass-balance formula in `src/models/GasDiffusion.C`/`GasProduction.C` |
| 08 | `case_08_macro_step_restart` | Restart consistency of the spectral diffusion solve under SCIANTIX's ASCII state hand-off (`output.txt`/`input_initial_conditions.txt`) | Same `c_M`, `D_M` as case 01, integrated with repeated restarts instead of one continuous run | Not in the report — added for OFFBEAT-coupling relevance; see caveat below |
| 09 | `case_09_outer_iteration_relaxation` | OFFBEAT's outer-iteration relaxation/staleness scheme (`relax`, `nFrequency`) applied to the SCIANTIX exchange variable | Same `c_M` as case 01, with `D` a function of a manufactured temperature `T_M(t)`, integrated through a manufactured outer-iteration loop | Not in the report — grounded in `offbeat-official`'s real coupling wrapper, see below |
| 10 | `case_10_stress_coupled_bubble_growth` | Grain-boundary bubble growth under a manufactured hydrostatic stress (`Solver::LimitedGrowth`, `InterGranularBubbleBehavior.C`) | `y_M(t) = y0(1+ε sin(ωt))`, inverted to a manufactured stress `σ_H^M(t)` through SCIANTIX's own equilibrium-pressure algebra | Not in the report — verifies OFFBEAT's real stress→SCIANTIX input, see below |
| 11 | `case_11_grain_growth` | Grain radius evolution (`Solver::QuarticEquation`, Ainscough grain-growth model) — the `a` every other case holds fixed | Saturating `R_M(t)`, exact per-step source derived from the real quadratic form SCIANTIX solves | Not in the report — surfaces a real convergence-check defect in the shipped solver, see below |
| 12 | `case_12_grain_boundary_bubble_coalescence` | Grain-boundary bubble coalescence (`Solver::BinaryInteraction`) and the full downstream intergranular swelling algebra | Saturating vacancies/Xe-atoms-per-bubble inputs → exact volume/radius/area/concentration/swelling chain | Not in the report — completes case 10's coverage of the intergranular bubble model, see below |
| 13 | `case_13_stoichiometry_deviation` | Oxygen-to-metal ratio: equilibrium root-find (`Solver::NewtonBlackburn`) and approach to it (`Solver::NewtonLangmuirBasedModel`) | Manufactured target `x_eq` (Part A); exact logistic solution of the same governing ODE (Part B) | Not in the report — verifies a second OFFBEAT-exchanged field, `oxygenMetalRatio_`, see below |

Cases 01-04 (plus 05, which reuses case 01's MMS) were introduced in the report's
Section 4.3. Cases 06-13 are new and target verification gaps relevant to the
OFFBEAT-SCIANTIX coupling, to the release quantity actually reported by
fuel-performance codes, and (case 11) to a solver defect found while
extending this suite, all explained below.

## Grounding cases 08-10 in the real OFFBEAT coupling

Cases 08 and 09 were designed from SCIANTIX's own standalone-mode assumptions
first, then checked against the actual coupling code in
[`offbeat-official`](https://gitlab.com/AlessandroScolaro/offbeat-official)
(`offbeatLib/fissionGasRelease/fgrSCIANTIX.C/.H`). That check corrected the
picture:

- **No ASCII restart in the real coupling.** OFFBEAT calls SCIANTIX in
  process, per cell, and exchanges state as plain C++ doubles
  (`Sciantix_variables[...]`, `grainModes_[i][cellI]`,
  `fgrSCIANTIX.C:456-521`) — never through `output.txt`/
  `input_initial_conditions.txt`. So case 08's finding (ASCII restart at
  SCIANTIX's own 7/10-digit precision is effectively free relative to the
  scheme's own discretisation error) is true but describes a mechanism the
  real coupling doesn't use; the honest restart risk there is OpenFOAM's own
  checkpoint/restart, governed by OpenFOAM's `writePrecision`, not
  SCIANTIX's `setprecision`. Kept as a defensive check and documented as
  such.
- **`grainModes_.setSize(20)`**: OFFBEAT hardcodes exactly 20 spectral modes
  per cell for every species (`fgrSCIANTIX.C:398`), not the report's hybrid
  20/40 split (case 05). Case 03's existing mode-convergence sweep already
  has the relevant error-vs-*k* data for *k*=20.
- **`relax`/`nFrequency` are real, used knobs** (`fgrSCIANTIX.C:606,
  660-670,692`), but only ever damp/stagger the value *exposed* to the rest
  of OFFBEAT's physics during a time step's outer iterations
  (`correct()`); the value permanently retained by SCIANTIX is always a
  clean, unrelaxed recompute at the time step's converged fields
  (`updateVariables()`, `fgrSCIANTIX.C:698-774`). Case 09 verifies this
  claim directly and quantifies the iteration cost of the damping/staleness
  choices, and shows the only real committed-answer risk is an
  under-converged outer *temperature* loop, not `relax`/`nFrequency`
  themselves.
- **`sigma_` (hydrostatic stress) is a genuine SCIANTIX input**
  (`Hydrostaticstress_input`, `fgrSCIANTIX.C:614-619,652-653`), feeding
  `InterGranularBubbleBehavior.C`'s grain-boundary bubble model
  (`Solver::LimitedGrowth`), whose swelling output is applied as an
  isotropic eigenstrain (`Sciantix_variables[18]*I`, `fgrSCIANTIX.C:551`)
  back in OFFBEAT's mechanics — a genuine two-way stress ↔ swelling
  feedback loop. Case 10 verifies the stress-driven half of that loop, which
  nothing else in this suite touches.

## Solver being verified

SCIANTIX describes intra-granular fission gas diffusion with the Booth
equivalent-sphere model and the quasi-stationary (Speight) assumption:

```
∂c(r,t)/∂t = D(T,Ḟ) · (1/r²) ∂/∂r( r² ∂c/∂r )  +  β
```

solved with the PolyPole-2 spectral algorithm: the PDE is projected onto the
eigenfunctions of the spherical Laplacian, each modal coefficient is advanced
in time with first-order backward Euler, and the concentration is
reconstructed by summing the modes. `common/spectral_diffusion_mms.py`
implements this projection/solve once (with the eigenbasis normalisation
audited against `utilities/MMS_verification/MMS_Solver/source_projection.py`
and cross-checked to machine precision against the manufactured solution
already validated in `utilities/MMS_verification/MMS_Solver/MMS_solver_verification.py`),
and cases 01, 03 and 05 reuse it by supplying different `(f(t), D(t))` pairs
for the `c_M(r,t) = (a²−r²)f(t)` family (case 02 uses the same kernel with its
own pair). Case 04 verifies the separate grain-boundary ODE balance with the
same backward-Euler pattern as `Solver::Decay`. Case 06 extends the kernel to
two coupled species (`common/coupled_diffusion_mms.py`), reusing the same
source-projection operator for a production/decay-coupled pair, matching
`Solver::SpectralDiffusion2equations`. Case 07 combines case 01's in-grain
content with a second, independently manufactured cumulative-production
quantity (integrated with `Solver::Integrator`'s exact update rule) to verify
the release mass balance `produced − decayed − in_grain` itself — i.e. that
subtracting two separately-convergent quantities doesn't introduce extra
error, which is exactly what `src/models/GasDiffusion.C` (lines 184-202) and
`src/models/GasProduction.C` compute for a system with no grain-boundary
bubble behaviour. Notably, `defineSpectralDiffusion1Equation` in
`GasDiffusion.C` and `GasProduction.C` already hardcode the *same*
manufactured source `0.4*a^2*cos(t) + 6*sin(t)` for their respective MMS
scaffolding — evidence this exact combined check was intended but not yet
exercised as a standalone case; case 07 is that case, generalised to
reusable parameters instead of the branch's hardcoded test values. Case 08
reuses case 01's `c_M`/`D_M` pair but replaces the shared kernel's single
continuous loop with `run_with_restart(...)`
(`common/spectral_diffusion_mms.py`), which periodically round-trips the
modal coefficients through `truncate_precision(...)` to reproduce SCIANTIX's
actual ASCII restart interface between driver calls.

## Cases relevant to the OFFBEAT coupling specifically

Verifying the standalone solver (cases 01-04) is necessary but not sufficient
for the coupled use case: SCIANTIX runs *inside* OFFBEAT, called at every
finite-element node and every time step, for multiple isotopes at once. Two
gaps stood out and are covered here; a third is flagged for future work:

- **Per-node computational cost** (case 05): the report's own Table 5 case —
  reducing spectral modes for the stable-nuclide role trades a small,
  quantified accuracy cost for a real per-call runtime reduction. This is
  the single most coupling-relevant number in the report, since SCIANTIX's
  cost is paid once per node per OFFBEAT step.
- **Multi-species production/decay coupling** (case 06): OFFBEAT-SCIANTIX
  exchanges several isotopes per node, including short-lived precursors
  decaying into stable daughters while both diffuse (the report's own "3
  stable + 2 non-stable" element split). None of cases 01-05 exercise the
  coupling term between two simultaneously-diffusing species
  (`Solver::SpectralDiffusion2equations`); case 06 verifies it directly.
- **Macro-step restart consistency** (case 08): OFFBEAT drives SCIANTIX
  through short, repeated calls rather than one continuous in-process
  integration from t=0, and the state SCIANTIX needs to resume from is
  handed back and forth as ASCII text — written to `output.txt`
  (`std::setprecision(7)` for `sciantix_variable` fields, `setprecision(10)`
  for `history_variable` fields, `src/file_manager/Output.C:57-97`) and read
  back from `input_initial_conditions.txt`
  (`src/file_manager/InputReading.C:115-149`) at the next call, rather than
  kept in memory. `Solver::SpectralDiffusion` (`src/classes/Solver.C:40`)
  itself already advances the modal state by exactly one step per call given
  a passed-in `initial_condition` array, so restarting is mathematically
  lossless *in principle* (the update is Markovian in the modal
  coefficients) — case 08 checks that this holds in practice and quantifies
  what the actual finite-precision ASCII hand-off costs. It finds: (1)
  restarting at full double precision reproduces the continuous integration
  exactly, at any call cadence; (2) at SCIANTIX's real output precision (7
  significant digits), the restart-induced error is indistinguishable from
  the scheme's own dt-driven discretisation error across the whole dt range
  tested (i.e. the ASCII restart is effectively "free"); (3) that holds
  regardless of how many micro-steps occur between driver calls, so there is
  no sign of error accumulating over many restarts across a long irradiation
  history. As noted above, the real coupling doesn't actually use this ASCII
  mechanism — see "Grounding cases 08-10" below. A manufactured, kinked
  `T(t)`/`Ḟ(t)` power history remains a natural further extension, flagged
  for future work.
- **Outer-iteration relaxation/staleness** (case 09): the real
  OFFBEAT-SCIANTIX coupling re-solves each time step through several outer
  (segregated nonlinear) iterations, damping and occasionally staggering the
  SCIANTIX-computed swelling/FGR fields it exposes to the rest of OFFBEAT's
  physics via `relax`/`nFrequency`. Case 09 verifies this can't bias the
  physics SCIANTIX permanently retains (only an under-converged outer
  temperature loop can), and quantifies how many outer iterations the
  damping/staleness choices cost.
- **Stress-driven grain-boundary bubble growth** (case 10): OFFBEAT passes
  its own hydrostatic stress field into SCIANTIX every call, driving the
  grain-boundary bubble model (`Solver::LimitedGrowth`) whose output swelling
  feeds back into OFFBEAT's mechanics as an eigenstrain — a real two-way
  mechanical feedback loop untouched by any other case. Case 10 verifies
  `Solver::LimitedGrowth` (unverified elsewhere in this suite) against a
  manufactured stress history and quantifies how accuracy degrades as the
  stress transient gets faster/larger.
- **Grain radius itself** (case 11): every case above (01-10) treats grain
  radius as a fixed constant. In a real coupled run it evolves every call
  via OFFBEAT's default grain-growth model, and case 11 verifies that model
  — finding a genuine defect in the solver it uses. See below.
- **Grain-boundary bubble coalescence and the swelling it feeds** (case 12):
  completes case 10's coverage of the intergranular bubble model
  (`InterGranularBubbleBehavior.C`) — case 10 verified vacancies-per-bubble
  (`Solver::LimitedGrowth`, stress-driven); case 12 verifies bubble
  *concentration* (`Solver::BinaryInteraction`, coalescence-driven) and the
  downstream algebra that combines concentration and volume into the
  swelling OFFBEAT actually consumes. Because `BinaryInteraction` is the
  *exact* solution of its own governing ODE (not a backward-Euler
  approximation), the case shows error at floating-point precision
  regardless of step size — a useful contrast to every O(Δt) solver
  elsewhere in this suite — and includes a deliberate-miscoding panel
  showing how sharply this check would catch a real coding defect.
- **The other OFFBEAT-exchanged thermochemistry field** (case 13): the
  wrapper exchanges `oxygenMetalRatio_` alongside swelling/FGR
  (`Sciantix_variables[85]`, `fgrSCIANTIX.C:485,531`). Case 13 verifies
  both solvers behind it — `Solver::NewtonBlackburn` (equilibrium
  root-find) and `Solver::NewtonLangmuirBasedModel` (approach to that
  equilibrium). Both converge correctly for a target-value sweep at a
  representative temperature — recovery is provably independent of
  temperature itself, since T cancels exactly in the manufactured round
  trip, so sweeping it (an earlier version of this case did) only produces
  redundant overlapping curves. The genuinely informative axis turned out
  to be the initial guess: `NewtonBlackburn` has a surprisingly narrow
  basin of convergence, diverging to NaN once the starting guess is only
  ~3x the true root — safe in practice only because
  `UO2Thermochemistry.C` always warm-starts it from the previous time
  step's converged value, never from a poor guess.

## A solver defect found by case 11

`Solver::QuarticEquation` (`src/classes/Solver.C:346-376`), which grain
growth uses to advance grain radius, is a Newton iteration capped at
`max_iter=5`, with an early-exit check `if (function < tol) return y1;`
(`tol=1e-3`) evaluated on the **raw, signed residual** rather than
`abs(function)` compared against a **scale-appropriate** tolerance. For the
grain-growth equation that residual is a length-squared quantity
(~1e-16 to 1e-8 m² for realistic grain radii and mobility×Δt products) —
always many orders of magnitude below `1e-3`. The practical effect: the
check is satisfied after the very first Newton step regardless of how
accurate that step is, so **`max_iter=5` never actually engages beyond
iteration 1** for this equation, no matter the time-step size.

Case 11 quantifies the consequence directly: at a 30-day macro time step
(T=1900 K), the shipped solver's error is already ~4.4% of the grain
radius; running the same Newton recursion to genuine convergence (instead
of exiting after one step) reduces that error to floating-point zero,
confirming the discrete scheme itself is sound — only the termination
logic is broken. A secondary check with a well-scaled, order-1 quartic
(so the fixed tolerance *is* dimensionally appropriate) shows the same
early-exit risk is not unique to grain growth's units: some initial
guesses still exit after one step with 15-60% error, whenever that step's
residual happens to land below the threshold. This is reported as a
finding, not patched here — `src/classes/Solver.C:370` is the line to fix
(compare `abs(function)` against a tolerance appropriate to the equation's
own scale, or simply don't exit early).

## Running the cases

Each case is a standalone script; run it from the repository root or from
within the case folder:

```bash
python3 verification/Operahpc_5.1/case_01_spectral_diffusion_sinusoidal/run_case.py
python3 verification/Operahpc_5.1/case_02_spectral_diffusion_exponential/run_case.py
python3 verification/Operahpc_5.1/case_03_convergence_study/run_case.py
python3 verification/Operahpc_5.1/case_04_grain_boundary_release/run_case.py
python3 verification/Operahpc_5.1/case_05_hybrid_mode_allocation/run_case.py
python3 verification/Operahpc_5.1/case_06_coupled_precursor_daughter_diffusion/run_case.py
python3 verification/Operahpc_5.1/case_07_fission_gas_release/run_case.py
python3 verification/Operahpc_5.1/case_08_macro_step_restart/run_case.py
python3 verification/Operahpc_5.1/case_09_outer_iteration_relaxation/run_case.py
python3 verification/Operahpc_5.1/case_10_stress_coupled_bubble_growth/run_case.py
python3 verification/Operahpc_5.1/case_11_grain_growth/run_case.py
python3 verification/Operahpc_5.1/case_12_grain_boundary_bubble_coalescence/run_case.py
python3 verification/Operahpc_5.1/case_13_stoichiometry_deviation/run_case.py
```

Each run prints error norms to stdout and writes comparison/convergence
plots (PNG, styled via `common/plot_style.py`) into its own case folder.
Requires `numpy` and `matplotlib`. The style is deliberately matched to the
actual figures in OperaHPC WP5-D5.1 (plain matplotlib defaults — sans-serif
font, tab10 colors, full box spines, visible grid — not a custom
"publication" theme), so a plot from this suite and a plot from the report
read as the same figure family.

## Relationship to `utilities/MMS_verification`

This folder does not replace `utilities/MMS_verification/` (the original
exploratory MMS scripts for the spectral diffusion solver, the Newton-Blackburn
solver, the Decay/Integrator ODE solvers, and the symbolic derivations in
`MMS_Solver/`). It packages that work as independent, documented, reproducible
**verification cases** cross-referenced to the OperaHPC deliverable, following
the same case-per-folder convention OFFBEAT uses for its own verification
suite. Additional cases (e.g. for `Solver::NewtonBlackburn`, or the
three-equation spectral diffusion solver) can be added the same way: one
folder, one `run_case.py`, one manufactured solution derived and documented
in the header docstring.
