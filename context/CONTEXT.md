# SCIANTIX — HBS porosity branch working context

This file is the project-level context for the `development/porosity_HBS`
branch. It is intended to be loaded at the start of every AI-assisted session
so that the agent has full awareness of the code state, physical rationale,
and open issues. Update it when the branch state changes materially.

## 1. Project context

**Branch:** `development/porosity_HBS`
**Parent paper:** Frattini MSc thesis (PoliMi, 2025), which is itself a
refinement of Barani et al. JNM 563 (2022) and Barani et al. JNM 539 (2020).
**Goal:** physics-based HBS inter-granular porosity model (case 2 of
`iHighBurnupStructurePorosity`) in SCIANTIX, calibrated against Cappia 2016,
Spino 2006, Noirot 2008, Lassmann 2003, Une 2001 radial pore data.
**Main developer:** Giovanni Zullo (Politecnico di Milano). Collaborators:
Davide Pizzocri (PoliMi), Tommaso Barani (CEA), Paul Van Uffelen (JRC).

## 2. Current parameter baseline

The branch intentionally diverges from Barani 2022 Table 1 on several
parameters, following Frattini's sensitivity analysis against Cappia data.
Do not "correct" these values back to the Barani 2022 paper without
understanding the rationale in Section 4.

| Parameter | Current value | Location | Source |
|---|---|---|---|
| `D_gb^SA` (Xe single-atom GB diff.) | Xia 2022: `2.0e-8·exp(-1.4/(8.62e-5·T))` | `Matrix.C::setGrainBoundarySingleAtomDiffusivity` case 1 | Frattini 2025 Tab 2 (empirical for cluster-dynamics convergence) |
| `D_gb^v` (vacancy GB diff.) | White 2004 + `5e-41·Ḟ` (no tilt in matrix) | `Matrix.C::setGrainBoundaryVacancyDiffusivity` case 3 | Frattini 2025 App. A sensitivity |
| Tilt correction on `D_gb^v` | α-weighted `sin(4°(1-α)+40°α)/sin(4°)` applied locally | `HighBurnupStructurePorosity.C` case 2 | Barani 2022 Eq. 7 |
| `d_V`, `δ_V` (Veshchunov-Tarasov re-solution) | 1 nm each | `HighBurnupStructureFormation.C` case 1 params 2,3 | Barani 2022 Tab 1 / Veshchunov-Tarasov 2013 |
| `ν_P` prefactor | **`1.0e18`** | `HighBurnupStructurePorosity.C` line ~110 | 2× Barani 2022 5e17, calibrated on Cappia N_p peak magnitude |
| HBS incubation burnup `bu_inc` | **20 MWd/kgU** | `HighBurnupStructureFormation.C` param 4 | Biswas-Aagesen 2025 Eq. 45 (modified JMAK) |
| KJMA `K, n` (Barani 2020) | `2.77e-7`, `3.54` | `HighBurnupStructureFormation.C` params 0,1 | Barani 2020 fit on Gerczak data, unchanged |
| `0.8814` (M_U/M_UO₂) | hardcoded in both formation and porosity modules | `HighBurnupStructureFormation.C`, `HighBurnupStructurePorosity.C` | Stoichiometric unit conversion MWd/kgUO₂→MWd/kgU |
| Xe yield scale factor `sf` | 1.25 for `iFuelMatrix=1` | `System.C::setProductionRate` case 1 & 5 | Converts base 0.24 → 0.30 true cumulative Xe yield |
| Saturation cap on pore growth | `(1−ξ_old/ξ_sat)²`, `ξ_sat = 0.22` | `HighBurnupStructurePorosity.C` | Percolation theory (Stauffer-Aharony t=2); kinetic saturation of vacancy-driven pore growth |

## 3. Code changes vs parent commit (logical diff)

### `src/classes/Matrix.C`
- `setGrainBoundaryVacancyDiffusivity` case 3: removed hardcoded `hbs_correction = sin(40°)/sin(4°) ≈ 9.17`. The base value is now untilted; the α-weighted tilt correction is applied locally in `HighBurnupStructurePorosity` (Barani 2022 Eq. 7 applied symmetrically to `D_gb^SA` and `D_gb^v`).
- `setGrainBoundarySingleAtomDiffusivity` case 1: active formula is Xia 2022 (`2e-8·exp(-1.4/kT_eV)`). Olander-Van Uffelen "low D" (`1.3e-7·exp(-2.82/kT_eV)`) is commented with a note explaining why (catch-22 at T=723 K with 2-atom nucleation).

### `src/classes/System.C`
- `setProductionRate` case 1: added comment documenting that `sf = 1.25` rescales yield 0.24 → 0.30 when `iFuelMatrix=1`, to match cumulative Xe yield used for HBS calculations.

### `src/models/HighBurnupStructureFormation.C`
- Added `hbs_incubation_burnup = 20.0` as parameter[4] of the model.
- Replaced the Decay-solver integration of `dα_r/dbu` with an **analytic** modified-KJMA:
  ```
  α_r = 1 - exp[-K · (bu_U - bu_inc)^n]   for bu_U > bu_inc
  α_r = 0                                  otherwise
  ```
  where `bu_U = bu_eff_UO₂ / 0.8814`. Robust across the `bu_inc` crossing; equivalent to the ODE integration for the no-threshold case.

### `src/models/HighBurnupStructurePorosity.C` case 2

#### Sensitivity flags (top of case 2)
Four `constexpr bool` flags for separate-effect analysis of the 5×5 system:
- `flag_sweeping` (SW1): NR→HBS gas sweeping. When false, `sweeping_term = 0`.
- `flag_partitioning` (SW2): direct grain→pore flux. When false, `f_p = 0, f_gb = 1`.
- `flag_implicit_coupling` (SW3): implicit A ↔ c_gb,HBS coupling in the matrix. When false, off-diagonal terms `coeff_matrix[9,21]` are zeroed and trapping is explicit.
- `flag_variance_source` (SW4): direct source to variance B from grain flux. When false, B grows only via nucleation and β·Np (consistent with Frattini Eq. 12).

All default `true` (full model). Setting all to `false` gives the decoupled Frattini Eq. 12 system.

#### Nucleation rate
- `pore_nucleation_rate` reads `bu_inc` from formation model parameter[4] and applies the incubation threshold: `ν_P = 0` for `bu_U ≤ bu_inc`, otherwise proportional to `(bu_U - bu_inc)^(n-1)`. Prefactor bumped 5e17 → 1e18 (factor 2) to recover N_p peak magnitude consistent with Cappia 2016 data.

#### Tilt correction
- Added **α-weighted tilt correction** on `D_gb^v` locally (was hardcoded at max in Matrix.C).

#### Saturation cap (two-pathway)
The saturation factor `(1 − ξ_old/ξ_sat)²` is computed **outside** the `if(DimensionlessFactor)` block so it is in scope for both pathways:

1. **Vacancy pathway**: `volume_flow_rate *= saturation_factor` inside `if(DimensionlessFactor)`. This throttles the Speight-Beere vacancy absorption ODE.
2. **Total pore volume**: after V_pore is computed from gas + vacancy contributions, the **total increment** `dV = V_new − V_old_step` is throttled: `V_pore = V_old_step + saturation_factor · dV` (only for dV > 0; shrinkage is never blocked).

`V_pore_old_step` is saved at the start of each time step (before `setInitialValue` overwrites the value).

#### Interconnection (coalescence) uses uncapped increment
`BinaryInteraction` receives `V_pore_increment_uncapped` (saved before the cap), NOT `getIncrement()` (which would return the capped dV ≈ 0 at high ξ). This ensures coalescence kinetics are not frozen by the saturation cap — pores still merge at their "natural" expansion rate even when the stored volume is capped. Without this fix, N_p plateaus instead of declining at bu > 125 MWd/kgHM.

#### Existing guards preserved
`ψ > 0.7` cap, `PackingFraction > 0.65` cap, `isinf/isnan` on sweeping.

### `regression/regression_hbs.py`
- Added `effectiveBurnup` variable from `Effective burnup (MWd/kgUO₂)` column ÷ 0.8814.
- All main plots use `effectiveBurnup` on x-axis.
- Removed redundant "simple" and "sensitivity" plots.
- All 3 main plots (pore density, porosity, pore radius) now include **Barani 2022** curve (sienna `#8b4513`) alongside SCIANTIX 2.0 (blue) and SCIANTIX 2.2.1 (green).
- **Porosity** and **pore radius** plots include **±σ shaded bands** from the Fokker-Planck second moment B:
  - `σ_R = R_p · CV / 3` where `CV = √M₂ / n̄` (first-order Taylor, `R ∝ n^{1/3}`)
  - `σ_ξ = ξ · CV` (propagated from σ_R)
  - Note: both approximations neglect the vacancy contribution to pore volume, so they slightly underestimate the true spread.
- `plt.show()` replaced with `plt.savefig()` for non-interactive execution.
- **7 plots** produced total:
  1. `plot_pore_density.png` — N_p vs bu_eff + Cappia/Spino/Barani/SCIANTIX 2.0/2.2.1
  2. `plot_porosity.png` — ξ vs bu_eff + ±σ_ξ band + 6 experimental datasets + 3 model curves
  3. `plot_pore_radius.png` — R_p vs bu_eff + ±σ_R band + Cappia/Spino + 3 model curves
  4. `plot_xe_depletion.png` — Xe in grains vs Walker 1999 + Lassmann fit + α_r twin axis
  5. `plot_fuel_swelling.png` — matrix swelling breakdown vs Spino 2005 + α_r twin axis
  6. `plot_pore_variance.png` — B raw (at²/m³) vs bu_eff (diagnostic, not for paper)
  7. `plot_CV.png` — coefficient of variation σ_n/n̄ vs bu_eff (diagnostic, U-shape test)

### `regression/regression_functions.py`
- Removed 4 stale variable entries from `sciantix_dictionary()` that caused warnings:
  `Intergranular fractional intactness`, `Intergranular vented fraction`,
  `Intergranular venting probability`, `Intergranular S/V`. These variables are
  not produced by the current SCIANTIX output.

### `regression/test_UO2HBS/output_gold.txt`
- Regenerated from current output to match the variable set actually produced.

### `regression/test_UO2HBS/radial_plots/run_radial.py`
- TUBRNP parameters corrected to mm units: `p1=3.45, p2=3.0, p3=0.45`.
- Introduced `fission_rate_radial = fission_rate * f_r / f_avg_volumetric` for self-shielding.
- Multi-burnup sweep: `bu_avg_list = [40, 67, 97]` with common fission rate and scaled duration.
- Non-uniform radial grid: `rim_clustering = 3.0` (power law, more points at rim).
- Volumetric f_avg via trapezoidal integration (grid-independent normalization).
- 5 plots generated: input profiles, HBS results, radial porosity/radius/density vs experimental data.
- Experimental data overlaid with dashed connector lines for visual clarity.

## 4. Physical rationale — key decisions

### Why Xia and not Olander for `D_gb^SA`
At T=723 K, Olander gives `D_gb^SA ≈ 3e-27 m²/s`. Trapping rate β per pore is
`~10⁻⁷ s⁻¹`, while re-solution rate on 2-atom nuclei is `α_n ≈ 3e-4 s⁻¹`.
Ratio 10⁴ → every new nucleus is destroyed before gaining atoms. The
cluster-dynamics 5×5 system collapses. Frattini documents in thesis Table 2
(p. 14) that Xia 2022 is substituted as an empirical workaround.

### Why `D_gb^v = 5e-41·Ḟ` and not Barani's `1e-39·Ḟ`
Frattini thesis Appendix A (pp. 22-23): weighted deviation minimization
against Cappia 2016 data. With 1e-39, porosity at 200 MWd/kgU overshoots to
~0.40. With 5e-41, best compromise at ~0.20.

### Why incubation burnup `bu_inc = 20` on α_r AND ν_P
Baron et al. 2009 describe HBS in three stages: (1) dislocation network,
(2) Xe depletion, (3) pore nucleation. Cappia 2022 and Gerczak 2018 show a
lag of ~15-25 MWd/kgU between polygonization and pore nucleation.
Biswas-Aagesen 2025 (CMS 258 114052, Eq. 45) derives a modified JMAK
`α = 1 - exp[-k(β - β_th)^n]` from a phase-field dislocation-energy vs
subgrain-formation-energy balance. Applied consistently to both α_r and ν_P
to avoid "gas-reservoir burst" that occurs when applied to ν_P alone.

### Why ν_P prefactor bumped 5e17 → 1e18
The incubation threshold reduces the integral of ν_P. Recalibrating to 1e18
restores peak ~5×10¹⁷ pores/m³, consistent with Cappia/Spino range.

### Why saturation cap `(1 - ξ/0.22)²` on BOTH vacancy flow AND total dV
**Important:** HBS pores do NOT release gas by venting in steady-state
operation. Gas stays trapped at 30-100+ MPa overpressure (Hiernaut 2008
JNM 377, Noirot 2008 JNM 372). Gas is released only by fragmentation during
transients (Kulacsy 2015 JNM 466, Jernkvist 2019/2020). Therefore zero FGR
from HBS pores in isothermal tests is **physically correct**.

The experimentally observed porosity saturation at ξ ≈ 0.15-0.20
(Spino 2005, Cappia 2016) is a **kinetic saturation**, not a release. At
high ξ the remaining solid matrix supports the load on a smaller
cross-section; local effective stress grows as ~1/(1-ξ), suppressing
vacancy sources. The quadratic cap models this via percolation theory
(Stauffer & Aharony 1994, critical exponent t=2 in 3D). Gas continues to
accumulate in pores after saturation (trapping unaffected), driving up
pressure — consistent with experimental over-pressurization data.

The cap must apply to the **total** pore volume increment (gas + vacancy),
not just the vacancy flow rate, because `V_gas = n_Xe · gasVolumeInPore`
grows uncapped otherwise and `V_gas ≈ V_vacancy` in magnitude, so capping
only the vacancy pathway leaves ~half the growth uncontrolled.

The interconnection (coalescence) uses the **uncapped** increment so that
pore merging continues at high ξ. Without this, N_p plateaus instead of
declining — inconsistent with experimental bell-shaped N_p(bu) curve.

### Why `(bu_eff/0.8814)` conversion
`0.8814 = M(U) / M(UO₂)`. Converts MWd/kgUO₂ → MWd/kgU. Stoichiometry,
not calibration. Do not remove.

## 5. Open issues / known limitations

### A. HBS pores are a closed system by design (physically correct in steady state)
`GrainBoundaryMicroCracking` and `GrainBoundaryVenting` operate on non-HBS
intergranular bubbles. In isothermal T=723 K test, Xe released saturates at
~2.94×10²⁵ at/m³ around bu=75 (exhausting NR GB bubbles). Thereafter FGR=0.
All subsequent gas accumulates in HBS pores. Mass balance conserved ±0%.
This is experimentally correct for steady-state (Hiernaut, Noirot).
Extension for transients (fragmentation/cracking module) is out of scope.

### B. Residual gap vs Cappia ~×1.2-1.3 in mid-burnup
Even with all calibrations, porosity at bu=100-150 MWd/kgHM is ~20-30% above
the upper envelope of Cappia 2016. Same level as Frattini thesis Fig 6.
Acceptable scatter.

### C. N_p peak position at ~96 MWd/kgHM
With bu_inc=20 and ν_P=1e18, peak is at 96, slightly below Cappia ~110.
Within scatter of experimental histograms. Acceptable.

### D. Catch-22 with Olander `D_gb^SA` — documented, not resolved
The strict Barani 2022 Table 1 parameters cannot be reproduced at T=723 K.
Requires either lower R cutoff on re-solution, non-dimer nucleation, or
different gas-source mechanism. None implemented.

### E. σ_R approximation neglects vacancy contribution
The ±σ_R band on the pore radius plot uses `σ_R = R·CV/3` from `R ∝ n^{1/3}`.
This ignores the vacancy contribution to pore volume (`n_vp·Ω_Schottky`), so
it slightly underestimates the true distribution width. Acceptable for the
paper but should be noted.

## 6. Testing protocol

### Standard regression (isothermal T=723 K)
```bash
cd regression
printf '1\n6\n0\n0\n' | python3 regression.py   # no plots
printf '1\n6\n0\n1\n' | python3 regression.py   # with plots (7 PNGs)
```

### Expected baseline values
| bu_eff (MWd/kgHM) | porosity | N_p (1/m³) | R_p (m) |
|---|---|---|---|
| 50 | ~0.007 | ~6×10¹⁶ | ~300 nm |
| 75 | ~0.07 | ~3.5×10¹⁷ | ~360 nm |
| 96-100 | ~0.13-0.16 | ~5×10¹⁷ (peak) | ~420 nm |
| 125 | ~0.16-0.19 | ~3.6×10¹⁷ | ~500 nm |
| 150 | ~0.17-0.21 | ~2.7×10¹⁷ | ~600 nm |
| 175 | ~0.18-0.23 | ~2.0×10¹⁷ | ~700 nm |

### Radial test
```bash
cd regression/test_UO2HBS/radial_plots
python3 run_radial.py
```
Default: `bu_avg_list = [40, 67, 97]`, `temp_center=1400K`, `rim_clustering=3.0`,
30 points. Sweep over 3 burnup values with common fission rate and scaled duration.

## 7. File layout & key code locations

| File | Role |
|---|---|
| `src/classes/Matrix.C` | D_gb^SA (case 1), D_gb^v (case 3), pore rates |
| `src/classes/System.C` | Production rate with 1.25 yield factor |
| `src/models/HighBurnupStructureFormation.C` | KJMA α_r with incubation burnup, parameters[0-4] |
| `src/models/HighBurnupStructurePorosity.C` | **Core**: sensitivity flags, cluster-dynamics 5×5, nucleation with bu_inc, tilt correction, two-pathway saturation cap, uncapped-increment interconnection |
| `src/operations/SetMatrix.C` | Matrix setup; UO2HBS uses `iGrainBoundaryVacancyDiffusivity=3` hardcoded |
| `regression/regression_hbs.py` | HBS regression with 7 plots (density, porosity+σ, radius+σ, Xe depletion, swelling, variance B, CV) |
| `regression/regression_functions.py` | Shared regression utilities; `sciantix_dictionary()` variable list |
| `regression/test_UO2HBS/radial_plots/run_radial.py` | Multi-burnup radial sweep with 5 comparison plots |

## 8. Key references

- Barani et al., JNM 539 (2020) 152296 — HBS formation KJMA
- Barani et al., JNM 563 (2022) 153627 — HBS porosity cluster dynamics
- Zullo et al., NED 429 (2024) 113602 — 3-eq spectral solver for sweeping
- Frattini MSc thesis (2025) — mechanistic revision, calibration
- Biswas & Aagesen, CMS 258 (2025) 114052 — phase-field HBS, modified JMAK Eq. 45
- Ge et al., Nanomaterials 15 (2025) 325 — review, 3-stage Baron mechanism
- Veshchunov & Tarasov, JNM 437 (2013) 250 — size-dependent re-solution
- White, JNM 325 (2004) 61 — D_gb^v base formula
- Stauffer & Aharony, Introduction to Percolation Theory, 1994 — t=2 exponent
- Cappia et al., JNM 480 (2016) 138; JNM 569 (2022) 153881 — HBS data
- Spino et al., JNM 354 (2006) 66; JNM 346 (2005) 131 — HBS data
- Hiernaut et al., JNM 377 (2008) 313 — annealing, pore overpressure
- Kulacsy, JNM 466 (2015) 409 — fragmentation model

All PDFs except Stauffer-Aharony are in repo root.

## 9. Manuscript

The manuscript is at `/home/giovanni/research-manuscripts/Zullo_et_al__HBS/main.tex`
with bibliography at `bibliography.bib`. Target journal: Journal of Nuclear Materials.

New plots generated during this session are in:
- `regression/test_UO2HBS/plot_pore_density.png` (replaces `Images/Np_finale.png`)
- `regression/test_UO2HBS/plot_porosity.png` (replaces `Images/Porosity_PPT.png`)
- `regression/test_UO2HBS/plot_pore_radius.png` (replaces `Images/R_finale.png`)
- `regression/test_UO2HBS/plot_pore_variance.png` (new, diagnostic)
- `regression/test_UO2HBS/plot_CV.png` (new, diagnostic)
- `regression/test_UO2HBS/radial_plots/plot_radial_porosity.png` (replaces `Images/SepEff_Porosity.png`)
- `regression/test_UO2HBS/radial_plots/plot_radial_pore_radius.png` (replaces `Images/SepEff_R.png`)
- `regression/test_UO2HBS/radial_plots/plot_radial_pore_density.png` (replaces `Images/SepEff_Np.png`)
