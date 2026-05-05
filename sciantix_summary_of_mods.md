# SCIANTIX engine modifications — `development/nitride`

Tracking file for the C++ engine delta only (include/, src/, regression/, docs/, references/).
Standalone Python work, notebooks, calibration scripts, optuna runs, and result trees at the repo root are out of scope here.

- Base: `origin/main`
- Branch: `development/nitride`
- Engine delta: **21 files, +817 / −217**
- All UN additions are tagged in-source with `// AD UN URANIUMNITRIDE` (or `// AD URANIUMNITRIDE`)

---

## File-level summary

| File | Δ | Nature |
|---|---:|---|
| `include/classes/Matrix.h` | +27 | new fields `dislocation_density`, `dislocation_core_radius` + getters/setters |
| `include/classes/Simulation.h` | +30 | 200-slot `modes_initial_conditions_dislocation_bubbles` + `getDiffusionModesDislocationBubbles(gas)` |
| `include/classes/Solver.h` | +32 | declaration of `SpectralDiffusion3equationsExchange` |
| `include/classes/System.h` | +60/−9 | new fields & methods for UN resolution / trapping / nucleation factor |
| `include/models/GasDiffusion.h` | +9 | declaration of `defineSpectralDiffusion3EquationsExchange` |
| `include/operations/SetMatrix.h` | +6 | declaration of `UN(...)` matrix factory |
| `include/operations/SetSystem.h` | +22 | declarations of `Xe_in_UN`, `Kr_in_UN`, `He_in_UN` |
| `include/operations/SetVariables.h` | −6 | doxygen comment cleanup only (no signature change) |
| `src/classes/Solver.C` | +78/−1 | `SpectralDiffusion3equationsExchange` implementation |
| `src/classes/System.C` | +226 | `setResolutionRatesUN`, `setTrappingRatesUN`, `setBulkNucleationFactor`, getters; `setFissionGasDiffusivity` case 11 |
| `src/models/GasDiffusion.C` | +70 | `iDiffusionSolver==4` branch; `defineSpectralDiffusion3EquationsExchange` factory |
| `src/models/IntraGranularBubbleBehavior.C` | +4 | placeholder comment only |
| `src/operations/SetMatrix.C` | +61 | `iFuelMatrix==2` branch; `UN(...)` factory |
| `src/operations/SetSystem.C` | +158/−1 | `iFuelMatrix==2` branch; `Xe_in_UN`, `Kr_in_UN`, `He_in_UN` factories |
| `src/operations/SetVariablesFunctions.C` | +21 | new SciantixVariables for dislocation bubbles |
| `src/operations/UpdateVariables.C` | +8 | output IDs 163 / 164 wired |
| `references/references.md` | +1/−1 | doxygen anchor `{#references}` only |
| `docs/source/conf.py` | −22 | removed sphinx_design / copybutton / myst extensions, html_theme_options, logo |
| `docs/source/index.rst` | +6/−66 | landing page trimmed back to plain toctree |
| `docs/source/references.md` | −109 | file deleted |
| `docs/source/_static/sciantix_logo.jpg` | bin | file deleted |

The `docs/` and `include/operations/SetVariables.h` changes are unrelated to UN — likely incidental or from a merge.

---

## New entry points (switch-case wiring)

| Input variable | New value | Effect |
|---|---:|---|
| `iFuelMatrix` | `2` | push `UN(...)` matrix |
| (driven by `iFuelMatrix==2`) | — | push `Xe_in_UN`, `Kr_in_UN`, `He_in_UN` systems |
| `iFissionGasDiffusivity` | `11` | Xe-in-UN: `D₁₀·exp(−Q₁/kT) + A₃₀·Ḟ`  (Rizk et al., JNM 606 (2025) 155604) |
| `iResolutionRate` | `4` | UN mechanistic bulk + dislocation resolution (Rizk) |
| `iTrappingRate` | `2` | UN bulk + dislocation line-sink trapping (Rizk) |
| `iDiffusionSolver` | `4` | spectral 3-equation with exchange (c, m_b, m_d) |

---

## New physics — per area

### Matrix (`UN`)
- Properties seeded from Rizk et al., JNM 606 (2025) 155604: theoretical density 14 300 kg/m³, lattice parameter 4.889e-10 m, dislocation density 3.0e13 m⁻², dislocation core radius 3.46e-10 m, surface tension 1.11 N/m, grain-boundary thickness 4.0e-10 m, semidihedral angle 59°, FF range 6 µm.
- TODO/placeholder values: Schottky volume (copied from UO₂), octahedral interstitial site (0.0), healing temperature threshold (1273.15 K), elastic modulus (2.0e5).

### Systems (Xe / Kr / He in UN)
- `Xe_in_UN`, `Kr_in_UN`: yield 0.24 / 0.30, radius in lattice 0.21e-9 m, Henry constant 0, bulk nucleation factor 1.0e-6 (Rizk range 1e-2…1e-7).
- `He_in_UN`: parameters copied from UO₂ — formal placeholder, source-tagged as such.
- All three call `setNucleationRate` via the existing UO₂ signature; the in-source comment notes Rizk's nucleation actually depends on c² and "non puo essere chiamato qui" — flagged for revision.

### Resolution (`setResolutionRatesUN`, case 4)
Two channels share the same Rizk form `b = F·1e-25·(2.64 − 2.02·exp(−2.61e-9/R_b))`:
- `resolution_rate_intra` uses `R_b = R_intra + r_lattice`
- `resolution_rate_disl` uses `R_b = R_disl + r_lattice`

### Trapping (`setTrappingRatesUN`, case 2)
- Bulk: `g_b = 4π D_g (R_b + r_l) N_b`
- Dislocation: `g_d = 4π D_g (R_d + r_l) N_d + (2π D_g / (ln(Γ_d / (Z_d r_d)) − 3/5)) · (ρ_d − 2 R_d N_d)` with `Γ_d = 1/√(π ρ_d)`, `Z_d = 5`, numeric guards on the log denominator and on negative free dislocation length.

### Diffusivity (case 11)
- Xe in UN: `D = D₁₀·exp(−Q₁/kT) + A₃₀·Ḟ` with `D₁₀ = 1.56e-3 m²/s`, `Q₁ = 4.94 eV`, `A₃₀ = 1.85e-39 m⁵`. Irradiation-enhanced term commented out as negligible.

### Solver (`SpectralDiffusion3equationsExchange`)
Spatially spectral on the gas-in-solution `c` (sphere, sin-basis), backward Euler per mode for `[c, m_b, m_d]`:

```
dc/dt   = D_g ∇²c − (g_b + g_d) c + b_b m_b + b_d m_d + β
dm_b/dt = g_b c − b_b m_b
dm_d/dt = g_d c − b_d m_d
```

3×3 linear system per mode solved by `Laplace3x3`. Parameter vector: `[N_modes, D_g, R, β, g_b, g_d, b_b, b_d]`.

### Variables
New SciantixVariables (output flag = 0 by default):
- `Xe in dislocation bubbles`, `Kr in dislocation bubbles`, `He in dislocation bubbles`, `Xe133 in dislocation bubbles`, `Kr85m in dislocation bubbles`
- `Dislocation bubble concentration`, `Dislocation bubble radius`, `Dislocation bubble volume`

Output IDs wired in `UpdateVariables.C`: `163 → Dislocation bubble concentration`, `164 → Dislocation bubble radius`. Placeholder IDs 161/162 for "Intragranular bulk bubble *" are commented out — the existing intragranular variables are reused instead.

### Diffusion mode storage
`Simulation` gets a parallel 200-element vector `modes_initial_conditions_dislocation_bubbles` and accessor `getDiffusionModesDislocationBubbles(gas)`. Indexing pattern is `[0, 40, 80, 120, 160]` for Xe / Kr / He / Xe133 / Kr85m. Sized for 5 gases × 40 modes.

---

## Regression coverage
**None.** No new regression case under `regression/` for UN. The standard `baker / white / talip / kashibe / ...` tree is untouched. Validation is currently happening through the standalone Python harness at the repo root.

---

## Open items flagged in source

| Where | Note (verbatim or paraphrased) |
|---|---|
| `Simulation.h` `getDiffusionModesDislocationBubbles` | "NON SO SE VA BENE QUEL 0*40" — indexing convention to verify |
| `SetSystem.C` Xe/Kr/He factories | "TUTTI I PARAMETRI SONO INVENTATI" on yield, radius, bulk_nucleation_factor |
| `SetSystem.C` `Xe_in_UN` | nucleation rate depends on c²; current call via standard signature is a placeholder |
| `SetMatrix.C` UN | TODO on theoretical density source, Schottky volume, octahedral site, healing T, elastic modulus |
| `System.C` `setTrappingRatesUN` | "USANO Intragranular bubble radius + radius_in_lattice ?????" — radius convention to confirm |
| `System.C` `setTrappingRatesUN` | "scaling factor per dislocation manca" — no separate scaling factor for dislocation trapping |
| `System.C` Rizk `g_d` | "nel Ritzk a numeratore c'era un *rho_d in piu, sembra per errore di stampa" — student's reading of the paper, worth cross-checking |
| `IntraGranularBubbleBehavior.C` | empty placeholder for a future case 3 ("bolle piccole + bolle medie") |

---

## Out of scope (root-level sandbox)
Not tracked here: `UN_M7_optuna_calibration_v{2..14}*.py`, `UN_M7_codex_*`, `un_model.py`, `un_data.py`, `calibrate_un.py`, all `UN_M7_*_results/` and `results_*` trees, all `*.ipynb`, run logs, shell drivers, and the thesis-side markdown reports. Update this section if any of that work is folded back into the engine.
