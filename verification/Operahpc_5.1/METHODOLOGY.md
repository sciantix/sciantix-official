# How this suite was built

`README.md` documents what each case verifies. `VERIFICATION_TAXONOMY.md`
documents why the set is shaped the way it is. This document is neither —
it's the recipe: the actual process used to go from "SCIANTIX has an
unverified solver" to a working case, so the same process can be repeated
to extend the suite, or rebuilt from scratch if this folder is ever lost.

## 1. The core idea: method of manufactured solutions (MMS)

Don't compare SCIANTIX's output against experimental data (which has its
own uncertainty) or a "gold" reference run (which conflates the numerical
scheme with the physical model — a wrong model can still converge cleanly).
Instead: invent an exact solution, work out analytically what forcing term
the *continuous* equation would need to produce it exactly, feed that
forcing into the *discrete* scheme as actually coded, and check whether the
discrete output reproduces the invented solution to the order of accuracy
the scheme should have. This isolates one question — is the discretisation
correct? — from everything else (mesh quality, physical realism, material
data) that a comparison against real data would conflate it with.

## 2. Why the solvers are reproduced in Python instead of calling the C++

Every case is a small, standalone Python script, not a wrapper around a
SCIANTIX build. That's deliberate: it avoids needing a build environment
for a folder that should be runnable with just `numpy`/`matplotlib`, and it
forces the verification to state, in one place, exactly what algorithm is
being checked (`common/*_mms.py`), rather than trusting a compiled binary's
behavior implicitly.

The tradeoff, and the rule that makes it safe: **every reproduction is a
literal transcription of the actual C++ — same loop structure, same
coefficients, same convergence-check logic, bugs included.** The point is
to verify what's actually shipped, not an idealized version of it. Case 11
is the clearest example: `common/quartic_solver_mms.py`'s `quartic_step`
deliberately reproduces `Solver::QuarticEquation`'s real termination check,
because the whole finding *is* that the check is wrong — a "corrected"
Python version would have hidden the defect instead of exposing it.

## 3. The recipe for one case

1. **Pick a target.** Cross-reference `include/classes/Solver.h` against
   the case index in `README.md`/`VERIFICATION_TAXONOMY.md` — anything not
   listed there is unverified. (`Solver::BisectionKato`, added to
   `StoichiometryDeviation.C` for MOX/Pu-Am fuel oxygen potential, is the
   next one: it didn't exist when this suite was built and isn't covered
   yet — see §6.)
2. **Read the solver and its one caller.** Not just the `Solver::` function
   in `src/classes/Solver.C` — the model file that calls it
   (`src/models/*.C`), to learn the actual discrete update formula, what
   the parameters mean physically, and what units/scale the residual
   naturally lives at (this last one matters more than it sounds — see §5).
3. **Check `offbeat-official`'s coupling wrapper before claiming OFFBEAT
   relevance.** `offbeatLib/fissionGasRelease/fgrSCIANTIX.C` is the actual
   source of truth for what's exchanged, how often, and in which direction
   — not assumption, not the report. This has corrected real mistakes
   mid-suite: case 08 was originally built around SCIANTIX's own ASCII
   restart files, and only cloning `offbeat-official` revealed the real
   coupling exchanges state as in-memory C++ doubles, never through text.
4. **Match the MMS technique to the solver's actual mathematical
   character** — this is the design decision that most determines whether
   a case is informative:
   - *Time-integration with an explicit forcing term*
     (`SpectralDiffusion`, `Decay`): pick a smooth manufactured trajectory,
     derive the exact source term algebraically so the discrete step lands
     on it exactly, sweep `dt` and check the observed order of accuracy.
     Cases 01, 02, 04.
   - *A solver that is the exact closed-form solution of its own governing
     ODE* (`BinaryInteraction`'s recursion, `NewtonLangmuirBasedModel`'s
     logistic ODE): verify against the *exact* solution of that ODE
     directly, no artificial source term. Expect floating-point-level
     agreement regardless of step size — if you see genuine `O(dt)` error
     here, first check whether the "manufactured" solution was actually an
     approximation rather than the true solution. Cases 12, 13(B).
   - *Pure algebraic root-finding, no time dependence*
     (`QuarticEquation`, `NewtonBlackburn`): manufacture a target root,
     invert the model's own forward formula (if one exists;
     `BlackburnThermochemicalModel` is `NewtonBlackburn`'s) to get the
     input that makes it exact, and check recovery. Then, *separately*,
     probe robustness — initial-guess sensitivity, whether `max_iter` is
     ever actually used, whether the tolerance is scaled appropriately for
     the equation's natural residual size. Accuracy-at-the-answer and
     reliability-of-getting-there are different questions; cases 11 and
     13(A) both turned out to hinge on the second one, not the first.
5. **Write the shared kernel first, then the case.** One `common/*_mms.py`
   module per solver family, reused by every case touching it
   (`spectral_diffusion_mms.py` alone backs cases 01, 02, 03, 05, 08, 09).
   The case's own `run_case.py` then just supplies parameters, the
   manufactured solution, the 4-panel figure, and a console report with
   all four error norms (L1/L2/Linf/final — the report's own convention).
6. **Look hard at the plot before calling it done**, not just the numbers:
   - A parameter that's provably cancelled out by the manufacturing
     process (case 13's original temperature sweep — `p_O2` was derived
     from `(x_target, T)` via the same formula being inverted, so `T`
     could never show a difference) produces redundant, misleading
     panels. If a sweep axis isn't changing anything, that's often more
     interesting than it looks — it means something cancels structurally
     — but the panel needs to say so, not silently show three overlapping
     lines.
     Redesign it to sweep something that actually varies the answer.
   - NaN or divergent points vanish silently from default matplotlib
     autoscaling — a failure mode can disappear from the plot entirely
     instead of being shown as one. Case 13's initial-guess sweep needed
     an explicit shaded "diverges" region once this was caught.
   - Log-scale axes spanning under ~2 decades pack in overlapping minor
     tick labels by default (`common/plot_style.clean_log_ticks` exists
     specifically for this).
   - A legend placed automatically can land on top of the data it's
     explaining, especially for anything oscillating
     (`common/plot_style.inset_legend` gives it an opaque backing).
7. **Run the full regression before moving on**:
   `for d in case_*; do python3 $d/run_case.py; done` — every case must
   still produce its plot and print without error.

## 4. Grounding discipline: check the source the claim will actually live on

The single biggest source of correction across this suite's construction
was re-checking a claim against real source *at the point it mattered*,
not trusting an earlier session's grounding to still hold:

- Case 08's restart mechanism was corrected only after cloning
  `offbeat-official` and reading the real wrapper (§3.3).
- Before moving this folder to `pre-release/SCIANTIXv2.5`, the same
  solvers were re-diffed against that branch's actual `Solver.C`. Case
  11's defect turned out to be **unchanged** there (confirmed by rerunning
  the check against the branch's own algorithm, not assumed). Case 13's
  finding turned out to be **partially stale** — the target branch now
  catches the NaN divergence and returns a finite (if wrong) value with a
  logged warning, which is a real behavior change from what was verified.

The rule this suggests for extending the suite: before asserting what a
solver does, `git show <target-branch>:<path>` it and read the current
version, even if a very similar check was already done recently. Branches
drift.

## 5. Plot style

`common/plot_style.py` is matched to the actual figures in OperaHPC
WP5-D5.1 (Figures 15-19), not an arbitrary "nice" theme: sans-serif font,
matplotlib's default tab10 colors, full box spines, visible grid, plain
non-bold titles. The figures were rendered from the report PDF with
PyMuPDF (`pip install pymupdf`; `poppler-utils`/`pdftoppm` was not
available in this environment and would be the usual route) to check
directly against the source rather than guessing at "publication style."
Every case routes color/style choices through this one module's constants
(`MANUFACTURED`, `NUMERICAL`, `LINE_CYCLE`, `ERROR_STYLE`) rather than
hardcoding them, so the whole suite's look changes from one file.

## 6. Where this could go next

Solvers in `include/classes/Solver.h` not yet covered, as of this
branch:

- `Solver::BisectionKato` — new on this branch, not on the branch this
  suite was originally built against. A bisection (not Newton) solve of
  Kato's MOX oxygen-potential correlation
  (`StoichiometryDeviation.C:600`), including a Pu/Am-content-dependent
  temperature adjustment (NEA 2024, eq. 8.4-8.5). Bisection brackets by
  construction, so the interesting question is different from the Newton
  solvers already checked — likely bracket validity and convergence rate
  vs. `max_iter=100`, not a `tol`-scaling defect.
- `Solver::SpectralDiffusion3equations` — natural extension of case 06 to
  a three-species decay chain, matching the report's "3 stable + 2
  non-stable" element split. Lowest novelty of what's left (same solver
  family verified twice already), but completes the family.
- The linear-algebra utilities (`Laplace2x2`, `Laplace3x3`, `det`,
  `Laplace`) — generic solvers rather than physical models; lower priority
  unless a specific model caller turns up an OFFBEAT-relevant use.

To add a case: follow §3, then update `README.md`'s case table and
`VERIFICATION_TAXONOMY.md`'s classification and case index — both are
meant to be extended, not just read.
