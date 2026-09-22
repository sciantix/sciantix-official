# Verification case taxonomy

This suite doesn't contain ten instances of the same kind of check. Each case
targets a *different class* of thing that can be wrong, and a bug in one
class is invisible to the others — a solver that is exactly correct in
isolation can still corrupt every OFFBEAT simulation that calls it, if the
interface handing it state is lossy, or if the outer coupling scheme wrapping
it is biased, or if it's simply never exercised with the inputs the coupling
actually drives it with. This document groups the cases by what they
actually verify and, for each group, why that class of correctness matters
specifically for the SCIANTIX-OFFBEAT coupling — not for SCIANTIX run
standalone.

The grouping is a pyramid: each layer assumes the one below it holds, and
each layer can hide bugs invisible from every layer below it.

```
   two-way feedback           case 10
   outer coupling-scheme      case 09
   interface / data exchange  case 08
   downstream aggregation     cases 07, 12
   multi-species coupling     case 06
   convergence & cost         cases 03, 05
   standalone solver (MMS)    cases 01, 02, 04, 06*, 07*, 10*, 11, 12*, 13
```

This is also the layer where a verification case earns its keep the most
directly: it's cheap to write, and it's where this suite has found genuine,
pre-existing gaps rather than just confirming correctness — case 07 found a
mass-balance combination that was never checked end-to-end (§4), and case 11
found a convergence-check defect in `Solver::QuarticEquation` that silently
limits grain growth to a single, unrefined Newton step regardless of
time-step size (§1 below). Neither would show up in a normal SCIANTIX run:
both solvers execute without error, they just aren't as accurate as their
own `max_iter`/structure implies. Case 13 is the counterpoint: it checks two
more Newton-type solvers the same way and finds both correctly implemented
— evidence this suite is a genuine check, not just a bug hunt with the
answer assumed in advance.
(`*` = the case also belongs to a layer above; the modal ODE it drives is
still verified against an exact solution either way.)

## 1. Standalone solver verification (cases 01, 02, 04, 06, 07, 10, 11, 12, 13)

**What it checks.** A single SCIANTIX numerical solver
(`Solver::SpectralDiffusion`, `Solver::Decay`, `Solver::LimitedGrowth`,
`Solver::Integrator`, `Solver::QuarticEquation`, `Solver::BinaryInteraction`,
`Solver::NewtonBlackburn`, `Solver::NewtonLangmuirBasedModel`) reproduces an
exact manufactured solution, in isolation, to the expected order of
accuracy.

**Why it matters for the coupling.** SCIANTIX is not run once — inside
OFFBEAT it's called at *every* fuel-material finite-element cell, at *every*
outer iteration, of *every* time step
(`offbeatLib/fissionGasRelease/fgrSCIANTIX.C:634-688`). A systematic error in
one solver call (wrong sign, wrong projection coefficient, off-by-one in a
discretisation) is not a rare event in the coupled system — it's the
*majority* of the computational work OFFBEAT does, repeated at thousands of
points in space and time. This is the precondition layer: nothing above it
means anything if the solver being called that often isn't right on its own.
It's also the only layer cheap enough to run at high mode/step counts and
check formal order of accuracy directly (cases 01, 02, 04, 10 all report
observed order alongside error norms), which the coupled system itself never
lets you isolate cleanly (see §2).

Case 11 (grain growth, `Solver::QuarticEquation`) is the one case in this
layer whose "expected order of accuracy" the solver doesn't actually
deliver: every other case above treats grain radius `a` as a fixed
constant, but it's OFFBEAT's default-active evolving quantity, and its
Newton solver terminates after exactly one step regardless of `max_iter=5`
because the convergence check compares a naturally tiny residual against a
fixed, inappropriate absolute tolerance (`src/classes/Solver.C:370`). At a
30-day macro time step the resulting error is already ~4% of the grain
radius — and since `a` feeds both the diffusion length scale (cases 01-03,
05-09) and the intergranular swelling formula (case 10's `3/a_grain`), an
unverified grain radius is a crack running under every other case's own
foundation.

Case 12 (`Solver::BinaryInteraction`, grain-boundary bubble coalescence) is
the opposite case from 11 in one specific sense: it's the exact, not merely
first-order-accurate, solution of its own governing ODE, so its error stays
at floating-point precision across every step size tested — a genuinely
different result from every backward-Euler solver above it, and a reminder
that "expected order of accuracy" isn't the same number for every solver in
this codebase. Case 13 (`Solver::NewtonBlackburn`,
`Solver::NewtonLangmuirBasedModel`, UO2 stoichiometry deviation) checks two
more Newton-type solvers the same way case 11 checked `QuarticEquation` —
same failure mode to look for (an inappropriately-scaled convergence check)
— and finds neither one has it: both use `abs(function) < tol` correctly,
with a `max_iter` generous enough to actually be used. It finds a different
sensitivity instead: `NewtonBlackburn`'s basin of convergence in its
initial guess is surprisingly narrow (diverges to NaN once the guess is
only ~3x the true root) — safe today only because its one caller always
warm-starts from the previous time step's converged value, never from an
arbitrary guess. Together, cases
11-13 are less about any single solver and more about the fact that this
codebase has at least four independent hand-rolled Newton/root-finding
implementations (`QuarticEquation`, `NewtonBlackburn`,
`NewtonLangmuirBasedModel`, and the coalescence closed form), each free to
get its own convergence check right or wrong independently — verifying one
tells you nothing about the others.

## 2. Convergence and cost characterisation (cases 03, 05)

**What it checks.** How error and runtime scale with the discretisation
parameters (`dt`, number of spectral modes `k`) SCIANTIX is actually run at
— not just *that* it converges, but *how fast*, and what a given operating
point costs.

**Why it matters for the coupling.** OFFBEAT hardcodes `k=20` spectral
modes per cell for every species, always
(`grainModes_.setSize(20)`, `fgrSCIANTIX.C:398`) — not a value tuned per
run, not the report's proposed hybrid 20/40 allocation. Case 03's
mode-convergence sweep is what tells you what accuracy that fixed choice
actually buys, and case 05 quantifies the report's proposed alternative
against it. Because SCIANTIX's cost is paid *per cell, per outer iteration,
per time step*, a runtime difference that looks trivial in one call (a
fraction of a millisecond) is multiplied by the total number of coupled
calls in a full 3D transient — this is the only layer that speaks to whether
a discretisation choice is affordable at that scale, not just accurate.

## 3. Multi-species coupling (case 06)

**What it checks.** The cross-species production/decay coupling term in
`Solver::SpectralDiffusion2equations`, verified with two simultaneously
diffusing, mutually coupled manufactured species.

**Why it matters for the coupling.** OFFBEAT exchanges several isotopes per
cell in one call — stable and radioactive gas species diffusing together,
one decaying into another. Every other diffusion case (01, 02, 03) exercises
exactly one species with no cross-term; a bug specific to the coupling term
itself (as opposed to either species' own diffusion) would pass all of them
and only appear once OFFBEAT actually requests a multi-species solve, which
is every real call.

## 4. Downstream aggregation / mass-balance (cases 07, 12)

**What it checks.** That combining several separately-integrated internal
quantities (produced, decayed, retained, released) into the one number
SCIANTIX actually reports reproduces the exact conservation identity, not
just that each piece is individually accurate.

**Why it matters for the coupling.** The released-gas quantity is the
number that leaves SCIANTIX and becomes physically meaningful outside it —
it's what `fgrSCIANTIX::gasMols()` hands to OFFBEAT's gap-gas model as a
mass source (`fgrSCIANTIX.H:399-402`). A bug in how the pieces are
subtracted/combined (sign error, stale operand, double-counting) can exist
even when every underlying solver is individually verified by layer 1 — this
is a distinct failure mode, an aggregation bug, that only a case checking the
*reported* quantity against its own conservation law can catch. It's also
concrete evidence this exact gap was real: `GasDiffusion.C` and
`GasProduction.C` already hardcode matching manufactured source terms for
their own internal MMS scaffolding, never previously wired into a standalone
check of the combined output (see case 07's docstring).

## 5. Coupling-interface / data-exchange verification (case 08)

**What it checks.** Whether the mechanism used to hand SCIANTIX's state
between calls preserves it, and at what cost if it doesn't.

**Why it matters for the coupling.** A perfectly verified solver (layer 1)
can still be corrupted by *how its state crosses the process/call boundary*.
This case was originally built around SCIANTIX's own ASCII restart files
(`output.txt`/`input_initial_conditions.txt`), and cloning
`offbeat-official` to check corrected that assumption: the real coupling
exchanges state as plain in-memory C++ doubles
(`Sciantix_variables[...]`, `grainModes_[i][cellI]`, `fgrSCIANTIX.C:456-521`)
— no text round-trip at all during a run. The case is kept as a defensive
check (relevant to OpenFOAM's own checkpoint/restart, or to any future
coupling that *does* serialize through text), with that correction now
documented in `README.md`. The general point stands regardless of the
specific mechanism: this layer exists to verify the wire protocol, not the
physics, and the two can fail independently.

## 6. Outer coupling-scheme verification (case 09)

**What it checks.** The numerical behaviour of the iterative scheme OFFBEAT
wraps *around* SCIANTIX to converge the segregated nonlinear multiphysics
solve — specifically `relax` (under-relaxation) and `nFrequency` (staleness)
in `fgrSCIANTIX::correct()` (`fgrSCIANTIX.C:606,660-670,692`).

**Why it matters for the coupling.** This is a class of error that
*cannot exist* in a standalone SCIANTIX run — it's introduced entirely by
the fact that OFFBEAT calls SCIANTIX repeatedly per time step while
temperature and stress are still converging, and damps/staggers what it
exposes to the rest of the physics in between. No amount of solver
verification (layer 1) touches it, because the solver is doing exactly what
it's told each time; the question is whether what it's told, and how the
answer is blended in, biases the coupled result. Case 09 shows the answer is
structural, not incidental: the committed physics is provably independent of
`relax`/`nFrequency` *given* outer convergence, and the real risk is
localized to an under-budgeted outer loop — a specific, actionable
conclusion this layer alone can produce.

## 7. Two-way feedback verification (case 10)

**What it checks.** A solver driven by the *other* direction of the
coupling — OFFBEAT's mechanical state (hydrostatic stress) as a genuine
input to SCIANTIX's physics (`Solver::LimitedGrowth`, grain-boundary bubble
growth), rather than SCIANTIX's output being verified in isolation.

**Why it matters for the coupling.** Every other case treats SCIANTIX as
consuming a temperature/fission-rate history and producing swelling/FGR —
one direction of a two-way loop. OFFBEAT also passes its own stress field
into SCIANTIX every call (`Hydrostaticstress_input`, `fgrSCIANTIX.C:614-619,
652-653`), which drives bubble growth, whose swelling output is applied back
as an eigenstrain in OFFBEAT's own mechanics
(`fgrSCIANTIX.C:542,551`) — closing the loop: stress → SCIANTIX → swelling →
stress. Nothing in layers 1-6 touches the stress-in half of that loop, or
the solver (`LimitedGrowth`) it exercises, at all. A verification suite that
never manufactures the mechanical input is only checking one arm of a
mechanism that is, by construction, two-way.

## Case index by type

| Case | Type(s) | What would silently break without it |
|---|---|---|
| 01, 02 | Standalone solver (§1) | Wrong intra-granular diffusion at every cell/step |
| 03 | Convergence & cost (§2) | No evidence for what accuracy/cost OFFBEAT's fixed `k=20` actually buys |
| 04 | Standalone solver (§1) | Wrong grain-boundary gas balance |
| 05 | Convergence & cost (§2) | Report's hybrid mode-allocation claim untested against its own baseline |
| 06 | Standalone solver (§1) + multi-species (§3) | Wrong precursor/daughter cross-coupling, invisible to single-species cases |
| 07 | Standalone solver (§1) + aggregation (§4) | Reported FGR silently violates its own mass balance |
| 08 | Interface/data exchange (§5) | State corruption at the SCIANTIX↔driver call boundary going undetected |
| 09 | Outer coupling-scheme (§6) | Relaxation/staleness biasing the physics OFFBEAT retains, undetected because it can't appear in a standalone run |
| 10 | Standalone solver (§1) + two-way feedback (§7) | Stress-driven bubble growth, and the mechanical feedback arm of the coupling, entirely unverified |
| 11 | Standalone solver (§1) | Grain radius `a` — the parameter every other case holds fixed — silently wrong by ~4%+ at coarse macro time steps, from a real convergence-check defect in `Solver::QuarticEquation` |
| 12 | Standalone solver (§1) + aggregation (§4) | Bubble-coalescence concentration and the volume/radius/area/swelling algebra it feeds — the other half of case 10's model — unverified |
| 13 | Standalone solver (§1) | `oxygenMetalRatio_`, the second OFFBEAT-exchanged field after swelling/FGR, and `NewtonBlackburn`'s narrow (~3x) basin of convergence going uncharacterized |

See `README.md` for how to run each case and its manufactured-solution
details; this document is about *why the set is shaped the way it is*, not
how any individual case works.
