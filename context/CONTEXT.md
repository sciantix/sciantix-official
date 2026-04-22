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
| Percolation saturation factor | `(1−ξ_old/ξ_sat)²`, `ξ_sat = 0.22` | `HighBurnupStructurePorosity.C` | Stauffer-Aharony `t=2`. Applied to **both** `D_gb^SA` (trapping) and `D_gb^v` (vacancy flow). No post-hoc cap on ΔV. |
| Cluster-dynamics time discretisation | Pure **implicit Euler** on the 5×5 system | `HighBurnupStructurePorosity.C` | Coeff matrix carries all implicit couplings; no residual explicit α·A term on the RHS |

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

#### Nucleation rate
- `pore_nucleation_rate` reads `bu_inc` from formation model parameter[4] and applies the incubation threshold: `ν_P = 0` for `bu_U ≤ bu_inc`, otherwise proportional to `(bu_U - bu_inc)^(n-1)`. Prefactor bumped 5e17 → 1e18 (factor 2) to recover N_p peak magnitude consistent with Cappia 2016 data.

#### Tilt correction
- α-weighted tilt correction `sin(angle_deg)/sin(4°)` with `angle_deg = 4°(1−α_r) + 40°α_r` is applied **locally** to both `D_gb^SA` (for `β_n`) and `D_gb^v` (for Speight-Beere), consistent with Barani 2022 Eq. 7. The base matrix values returned by the getters are **untilted**; the tilt is combined with the percolation saturation factor in a single multiplicative chain at the point of use. No other routine applies the tilt (the NR intergranular bubble model uses UO₂ matrix boundaries at 4° by definition).

#### Cluster-dynamics 5×5 linear system — fully implicit Euler
The 5 unknowns are `Np, A, B, c_gb^NR, c_gb^HBS`. The scheme is **pure implicit Euler**: every coupling (re-solution, trapping, sweeping) appears only through `coeff_matrix`, and the RHS `initial_conditions[]` contains only previous-step values and explicit sources (nucleation).

Mass balance encoded by the matrix:
- Row `A`: `(1 + α_n dt) A_new − β_n N_p dt · c_gb^HBS_new = A_old + 2ν_P dt`
- Row `c_gb^HBS`: `(1 + β_n N_p dt) c_gb^HBS_new − α_n dt · A_new − w dt · c_gb^NR_new = c_gb^HBS_old + gas_from_grain − 2ν_P dt`
- The `−2ν_P dt` RHS term on `c_gb^HBS` is the nucleation sink (two xenon atoms leave the HBS grain-boundary reservoir at every nucleation event); its counterpart `+2ν_P dt` on `A` is the source.
- Overall `d(A + c_gb^HBS)/dt = S_HBS + w c_gb` is conserved exactly by the solver.

Previously the RHS of `c_gb^HBS` also carried `+ pore_resolution_rate · A_old · dt`, which, combined with the implicit `−α_n dt` coupling in `coeff_matrix[21]`, turned the re-solution coupling into a Crank-Nicolson-like scheme with over-counting. **Removed**: only the implicit coupling remains, so the scheme is now pure implicit Euler.

#### Percolation saturation factor — two-pathway modulation of diffusivities
The saturation factor `(1 − ξ_old/ξ_sat)²` is computed **once** at the top of case 2 (before `trapping_coeff_HBS` is formed). It is applied by the same `t = 2` percolation exponent (Stauffer-Aharony) to **both** grain-boundary transport coefficients:

1. **Trapping pathway (`β_n`)**: `trapping_coeff_HBS = 4π · (D_gb · saturation_factor) · R_p · (1 + 1.8 ξ^{1.3})`. At `ξ → ξ_sat`, `β_n → 0` and newly produced gas piles up on `c_gb^HBS` instead of being absorbed by pores.
2. **Vacancy pathway (Speight-Beere)**: `D_gb_v_eff = D_gb^v · tilt_factor · saturation_factor`, used in `volume_flow_rate = 2π ρ_P D_gb_v_eff / ζ(ψ)`. At `ξ → ξ_sat`, vacancy inflow shuts down.

**No post-hoc cap on ΔV.** The earlier `V_pore = V_old + saturation_factor · dV` block has been removed. Saturation now acts entirely on the upstream diffusivities, so the pore volume follows the EoS identity `V_p = n_Xe·Ω_Xe + n_vac·Ω_Schottky` without any post-hoc manipulation. Carnahan-Starling packing-fraction cap (0.65) handles the thermodynamic limit on over-pressurisation naturally.

#### Coalescence / interconnection
`BinaryInteraction` now receives the **physical** `V_pore_increment = V_new − V_old_step` (no longer distinguished as "uncapped" vs "capped" because no cap is applied to ΔV). The bell-shaped `N_p(bu)` profile is preserved because saturation acts on diffusivities upstream, not on ΔV downstream.

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
- **8 plots** produced total:
  1. `plot_pore_density.png` — N_p vs bu_eff + Cappia/Spino/Barani/SCIANTIX 2.0/2.2.1
  2. `plot_porosity.png` — ξ vs bu_eff + ±σ_ξ band + 6 experimental datasets + 3 model curves
  3. `plot_pore_radius.png` — R_p vs bu_eff + ±σ_R band + Cappia/Spino + 3 model curves
  4. `plot_xe_depletion.png` — Xe in grains vs Walker 1999 + Lassmann fit + α_r twin axis
  5. `plot_fuel_swelling.png` — matrix swelling breakdown vs Spino 2005 + α_r twin axis
  6. `plot_pore_variance.png` — B raw (at²/m³) vs bu_eff (diagnostic, not for paper)
  7. `plot_CV.png` — coefficient of variation σ_n/n̄ vs bu_eff (diagnostic, U-shape test)
  8. `plot_xe_inventory.png` — **xenon mass balance** across the six reservoirs (NR grain → NR GB → HBS grain → HBS GB → HBS pores → released). Two-panel stacked area: top = absolute inventory (10²⁶ at/m³) with total production dashed-line overlay for conservation check; bottom = fractional share (%) with `α_r` on twin axis. Directly validates the two-phase sweeping framework and exposes the `c_gb^HBS` backup at `ξ → ξ_sat` (signature of the percolation saturation on `β_n`).

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

### Why percolation saturation `(1 - ξ/0.22)²` on both diffusivities (not on ΔV)
**Important:** HBS pores do NOT release gas by venting in steady-state
operation. Gas stays trapped at 30-100+ MPa overpressure (Hiernaut 2008
JNM 377, Noirot 2008 JNM 372). Gas is released only by fragmentation during
transients (Kulacsy 2015 JNM 466, Jernkvist 2019/2020). Therefore zero FGR
from HBS pores in isothermal tests is **physically correct**.

The experimentally observed porosity saturation at ξ ≈ 0.15-0.20
(Spino 2005, Cappia 2016) is a **percolation transition of the solid
grain-boundary backbone**. Both single-atom gas diffusion (`D_gb^SA`,
feeding `β_n`) and vacancy diffusion (`D_gb^v`, feeding the Speight-Beere
flux) travel along the same connected network of grain boundaries. As ξ
approaches `ξ_sat`, the backbone fragments and both diffusive pathways
close simultaneously. From 3D lattice percolation theory (Stauffer &
Aharony 1994), any transport coefficient along the solid phase scales near
the critical porosity as `D_eff = D (1 − ξ/ξ_sat)^t` with `t ≈ 2`.

**Implementation**: the factor is applied as a multiplicative modulation
of the two diffusivities *in situ* in `HighBurnupStructurePorosity.C`:
- `trapping_coeff_HBS = 4π · D_gb^SA · saturation_factor · R_p · (1 + 1.8 ξ^{1.3})`
- `D_gb_v_eff = D_gb^v · tilt_factor · saturation_factor` (used inside Speight-Beere)

The earlier ad-hoc post-hoc cap on the total pore-volume increment
(`V_pore = V_old + saturation_factor · dV`) has been **removed**. That cap
was physically inconsistent: it throttled the gas contribution `n_Xe Ω_Xe`
in addition to the vacancy contribution, even though trapping and vacancy
transport are distinct physical channels. By moving the factor upstream to
the diffusivities, both channels are suppressed at the correct rate, the
EoS identity `V_p = n_Xe Ω_Xe + n_vac Ω_Schottky` is preserved, and the
bell-shaped `N_p(bu)` profile emerges naturally from the percolation-modulated
β_n without needing an "uncapped-increment" special case for coalescence.

**Physical signature in the output** (visible in `plot_xe_inventory.png`):
as β_n → 0 at `ξ → ξ_sat`, newly produced xenon cannot enter the pores and
backs up on `c_gb^HBS`, which develops a plateau at high burnup. This is a
direct, visual validation of the two-pathway percolation formulation.

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
printf '1\n6\n0\n1\n' | python3 regression.py   # with plots (8 PNGs)
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
| `src/models/HighBurnupStructurePorosity.C` | **Core**: fully implicit Euler 5×5 cluster dynamics, nucleation with `bu_inc`, local tilt correction on both `D_gb^SA` and `D_gb^v`, percolation saturation factor applied to the two diffusivities (no ΔV cap) |
| `src/operations/SetMatrix.C` | Matrix setup; UO2HBS uses `iGrainBoundaryVacancyDiffusivity=3` hardcoded. UO2HBS pore surface energy `γ = 1.0 N/m`; UO2 matrix uses `γ = 0.7 N/m`. |
| `regression/regression_hbs.py` | HBS regression with 8 plots (density, porosity+σ, radius+σ, Xe depletion, swelling, variance B, CV, xenon inventory mass balance) |
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
with bibliography at `HBS.bib`. Target journal: Journal of Nuclear Materials.

### Equations aligned with the code

| Paper | Form | Notes |
|---|---|---|
| Eq. 12 (definition of `β_n`) | `β_n = 4π D_gb^SA c_gb R_n^p (1 + 1.8 ξ^{1.3})` | `β_n` **already includes** `c_gb^HBS` (atoms/pore·s). Frattini convention. |
| Eq. 15 (`dA/dt`) | `dA/dt = 2ν_P − α_n A + β_n N_p` | Previously had `β_n^{tot} c_gb^HBS` which double-counted `c_gb^HBS`. Now matches Frattini thesis exactly. |
| Eq. 18 (`dc_gb^HBS/dt`) | `dc_gb^HBS/dt = S_HBS + w c_gb − 2ν_P − β_n N_p + α_n A` | `−2ν_P` nucleation sink added (two atoms leave the reservoir per pore nucleated). Mass balance `d(A + c_gb^HBS)/dt = S_HBS + w c_gb` is now exact. |
| Eq. 22 (percolation saturation) | `D_eff = D (1 − ξ/ξ_sat)^t, t ≈ 2` with separate equations for `D_gb^SA,eff` and `D_gb^v,eff` | Rewritten as symmetric modulation of both diffusivities. The previous formulation "applied to total ΔV" has been removed. |
| §3.9 text | "saturation acts on upstream transport coefficients" | Rewritten. No more "capping only the vacancy pathway leaves half the growth uncontrolled" — that paragraph was the old (incorrect) justification for the ΔV cap and has been replaced. |
| §3.4 text | `β_n N_p` | Last residual `β_n^{tot}` removed. |
| Table 2 | Added `γ = 1.0 N/m` (UO₂-HBS pore surface energy) and effective cumulative yield `y = 0.30 at/fiss` | Explanatory paragraph on the 1.25 precursor factor (iodine + tellurium decay chains feeding stable Xe). |
| §5.5 (new) | "Xenon inventory and mass balance" | New results subsection built around `plot_xe_inventory.png`. Numbered observations: (1) conservation to 10⁻³ relative, (2) onset of HBS reservoir at `bu_eff ≈ 40–50 MWd/kgHM`, (3) closed-system behaviour (`c_r ≈ 0` throughout), plus the `c_gb^HBS` plateau as visual signature of the percolation saturation on `β_n`. |

### Plots in `Images/` for the manuscript
- `plot_pore_density.png` (Fig. 3 — N_p vs bu_eff)
- `plot_porosity.png` (Fig. 4 — ξ vs bu_eff with ±σ_ξ band)
- `plot_pore_radius.png` (Fig. 5 — R_p vs bu_eff with ±σ_R band)
- `plot_pore_variance.png` (Fig. 6 — diagnostic, B moment)
- `plot_CV.png` (Fig. 7 — diagnostic, coefficient of variation)
- `plot_xe_inventory.png` (Fig. 8 — xenon mass balance, §5.5)
- `plot_radial_porosity.png`, `plot_radial_pore_radius.png`, `plot_radial_pore_density.png` (radial section)

## 10. Recent-session change log (2026-04-22)

The material changes applied in this session, relative to the `e2f88a9b` tip, are:

1. **Implicit Euler on the 5×5 cluster-dynamics solver.** Removed the residual `+ pore_resolution_rate · A_old · dt` from `initial_conditions[4]`; the `−α_n A` coupling to `c_gb^HBS` is now carried purely by `coeff_matrix[21]`, eliminating a Crank-Nicolson-like over-count.
2. **Nucleation sink on `c_gb^HBS`.** Added `−2·ν_P·dt` to `initial_conditions[4]` (was previously missing in the paper; in the code it is now explicit and matches the sink/source balance with `A`).
3. **Percolation saturation factor moved onto the diffusivities.** The factor `(1 − ξ/ξ_sat)²` is now applied to both `D_gb^SA` (inside `trapping_coeff_HBS`) and `D_gb^v` (inside the Speight-Beere block), instead of being applied twice (once on `volume_flow_rate` and once on the total ΔV). The post-hoc `V_pore = V_old + saturation_factor·dV` block was removed; `V_pore_increment` is now the physical EoS-consistent ΔV.
4. **`BinaryInteraction` simplification.** Uses the physical `V_pore_increment` (no more "uncapped vs capped" distinction).
5. **Paper alignment.** Eq. 15 rewritten with Frattini's `β_n N_p` notation; Eq. 18 gained the `−2ν_P` sink; §3.9 rewritten as a two-pathway percolation modulation of `D_gb^SA` and `D_gb^v`; Table 2 gained the `γ_HBS = 1.0 N/m` and effective-yield rows; new §5.5 "Xenon inventory and mass balance".
6. **Regression plot 8.** `plot_xe_inventory.png` added to `regression_hbs.py`: two-panel stacked area visualising the six xenon reservoirs and the `α_r`-driven redistribution, doubling as a visual conservation check.
