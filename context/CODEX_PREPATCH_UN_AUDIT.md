# CODEX Prepatch UN Audit

## Target clarified

The implementation target is the full physical model already prototyped in
`UN_model/notebooks/8test_UN_intergranular.ipynb`, not a Patch 1 skeleton.
The source equations are `UN_model/reports/UNmodel.md` and
`UN_model/reports/UNintergranular_theory.md`.

The UO2 coarsening branch is used only as a software architecture reference
for variable plumbing, output flags, and persistence. Its UO2 constants and
physics are not copied.

## Existing UN support

- `iFuelMatrix = 2` selects `UN` in `src/operations/SetMatrix.C`.
- `src/operations/SetSystem.C` already registers `Xe_in_UN`, `Kr_in_UN`, and
  `He_in_UN`. Xe has yield `0.24`, radius in lattice `0.21e-9`, bulk
  nucleation factor, UN resolution, and UN trapping hooks.
- `src/classes/System.C` already has `iFissionGasDiffusivity = 11` for Xe in
  UN and UN-specific `setResolutionRatesUN` / `setTrappingRatesUN`.
- `src/models/GasDiffusion.C` already has `iDiffusionSolver = 4`, calling the
  three-equation exchange solver.

## Solver status

The 3x3 backward-Euler solver for `c`, `m_b`, and `m_d` already exists in
`src/classes/Solver.C` as `SpectralDiffusion3equationsExchange`. It solves:

```text
dc/dt   = D_g laplacian(c) - (g_b + g_d)c + b_b m_b + b_d m_d + beta
dm_b/dt = g_b c - b_b m_b
dm_d/dt = g_d c - b_d m_d
```

This solver will be reused. It will not be rewritten.

## Current issues

- `IntraGranularBubbleBehavior.C` has no UN case implementing the notebook 8
  two-population intragranular model.
- `Dislocation bubble concentration` and `Dislocation bubble radius` are
  initialized from legacy indices `19` and `20`, which are already used for
  bulk intragranular bubbles.
- `Xe in dislocation bubbles` is initialized to zero and lacks a dedicated
  persistent `Sciantix_variables` index, so external time stepping can lose
  `m_d`.
- Dislocation-bubble diffusion modes are internal-only and are not written to
  the public diffusion mode array. This is acceptable for this standalone
  SCIANTIX executable path but is a coupling limitation to document.
- The reference `development/CoarseningUO2` branch was fetched only for
  inspection and contains a conflict marker in its intragranular model file,
  so it is not a copy source.

## Exact implementation plan

Modify:

- `src/models/IntraGranularBubbleBehavior.C`
  - add `iIntraGranularBubbleBehavior = 5`;
  - implement notebook 8 ordering: gas growth, vacancy absorption,
    dislocation coalescence, dynamic `rho_d(T,F)` nucleation/update,
    recompute volumes/radii;
  - implement grain-face bubbles and FGR using `UNintergranular_theory.md`.
- `src/operations/SetVariablesFunctions.C`
  - add optional UN switch names and dedicated UN state variables at free
    indices `170+`;
  - stop initializing dislocation states from indices `19/20`.
- `src/operations/UpdateVariables.C`
  - write back all new UN variables.
- `src/file_manager/InputReading.C`, `include/file_manager/InputReading.h`,
  `src/coupling/TUSrcCoupling.C`
  - add optional setting reader and default UN switches.
- `context/UN_SCIANTIX_IMPLEMENTATION_NOTES.md`
  - final implementation notes, equations, variables, switches, and tests.

Dedicated variable block:

```text
170 Xe in dislocation bubbles
171 Dislocation bubble concentration
172 Dislocation bubble radius
173 Dislocation bubble volume
174 Dislocation gas atoms per bubble
175 Dislocation vacancies per bubble
176 Dislocation gas bubble swelling
177 Intragranular bulk gas bubble swelling
178 Bulk vacancies per bubble
179 Bulk bubble pressure
180 Bulk bubble equilibrium pressure
181 Dislocation bubble pressure
182 Dislocation bubble equilibrium pressure
183 Dislocation density
184 UN gas to grain boundary diagnostic
185 Grain-face bubble concentration
186 Grain-face atoms per bubble
187 Grain-face vacancies per bubble
188 Grain-face bubble radius
189 Grain-face bubble area
190 Grain-face bubble volume
191 Grain-face fractional coverage
192 Grain-face gas swelling
193 UN grain-face gas
194 UN released gas
195 UN fission gas release
196 UN total gas swelling
```

Technical defaults:

- Dynamic dislocation density uses the smooth notebook 8 law by default.
- Bulk/dislocation vacancy absorption uses the implicit pressure equation from
  `UNmodel.md`.
- Grain-face geometry uses the `cos^3(theta)` lenticular factor.
- Grain-face vacancy diffusion uses `D_v^gb = 1e6 D_v1`.
- Bulk-to-dislocation capture remains off, matching notebook 8 default.
