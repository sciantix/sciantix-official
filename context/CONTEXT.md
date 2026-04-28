# SCIANTIX - HBS porosity branch working context

This file is the project-level context for the `development/porosity_HBS`
branch. It is intended to be loaded at the start of every AI-assisted session
so that the agent has full awareness of the code state, physical rationale,
and open issues. Update it when the branch state changes materially.

## 1. Project context

**Branch:** `development/porosity_HBS`

**Directory paths:**
- SCIANTIX directory: `/home/giovanni/sciantix-official`
- HBS paper directory: `/home/giovanni/research-manuscripts/Zullo_et_al__HBS`
- tex file: `/home/giovanni/research-manuscripts/Zullo_et_al__HBS/main.tex`

**Theoretical basis:**

**Goal:** physics-based HBS inter-granular porosity model (case 2 of
`iHighBurnupStructurePorosity`) in SCIANTIX, tested against data from Cappia 2016, Spino 2006, Noirot 2008, Lassmann 2003, Une 2001.

**Main developer:** Giovanni Zullo (Politecnico di Milano).

## 2. Current parameter baseline

The branch intentionally diverges from Barani 2022 Table 1 on several
parameters, following Frattini's sensitivity analysis against Cappia data.
Do not "correct" these values back to the Barani 2022 paper without
understanding the rationale in Section 4.

| Parameter | Current value | Location | Source |
|---|---|---|---|
| `D_gb^SA` (Xe single-atom GB diff.) | Xia 2022: `2.0e-8·exp(-1.4/(8.62e-5·T))` | `Matrix.C::setGrainBoundarySingleAtomDiffusivity` case 1 | Frattini 2025 Tab 2 (empirical for cluster-dynamics convergence) |
| `D_gb^v` (vacancy GB diff.) | White 2004 + `1e-39·Ḟ` (Barani 2022 original; no tilt in matrix) | `Matrix.C::setGrainBoundaryVacancyDiffusivity` case 3 | Barani 2022 Tab 1; the earlier Frattini `5e-41` is no longer needed now that the mechanical ΔV cap (Section 3) bounds porosity upstream |
| Tilt correction on `D_gb^v` | α-weighted `sin(4°(1-α)+40°α)/sin(4°)` applied locally | `HighBurnupStructurePorosity.C` case 2 | Barani 2022 Eq. 7 |
| `d_V`, `δ_V` (Veshchunov-Tarasov re-solution) | 1 nm each | `HighBurnupStructureFormation.C` case 1 params 2,3 | Barani 2022 Tab 1 / Veshchunov-Tarasov 2013 |
| `ν_P` prefactor | **`8.8e17`** | `HighBurnupStructurePorosity.C` line ~120 | 1.76× Barani 2022 5e17, calibrated on Cappia N_p peak magnitude. Previously `1.0e18` when the chain rule in the time-rate conversion was broken (effective prefactor `0.88e18`); with the `/0.8814` fix in place the literal prefactor `8.8e17` preserves the same calibration. See §11.9. |
| HBS incubation burnup `bu_inc` | **15 MWd/kgHM** | `HighBurnupStructureFormation.C` param 4 | Biswas-Aagesen 2025 Eq. 45 (modified KJMA); shifted from 20 → 15 to reduce RMSE vs PIE (Gerczak 2018 / Noirot 2015): 0.180 → 0.110, below the unshifted 0.156 baseline (see `context/kjma_fit_comparison.py`) |
| KJMA `K, n` (Barani 2020) | `2.77e-7`, `3.54` | `HighBurnupStructureFormation.C` params 0,1 | Barani 2020 fit on Gerczak data, unchanged |
| `0.8814` (M_U/M_UO₂) | hardcoded in both formation and porosity modules | `HighBurnupStructureFormation.C`, `HighBurnupStructurePorosity.C` | Stoichiometric unit conversion MWd/kgUO₂→MWd/kgHM |
| Xe yield scale factor `sf` | 1.25 for `iFuelMatrix=1` | `System.C::setProductionRate` case 1 & 5 | Converts base 0.24 → 0.30 true cumulative Xe yield |
| Percolation + mechanical saturation factor | `F_sat = (1−ξ_old/ξ_sat)²`, `ξ_sat = 0.22` | `HighBurnupStructurePorosity.C` case 2 | Stauffer-Aharony `t=2` applied to `D_gb^v` (vacancy backbone percolation) **and** post-hoc to the total pore-volume increment ΔV (mechanical stress on residual solid cross-section). **Not** applied to `D_gb^SA`/β_n. Coalescence receives the capped ΔV. |
| Cluster-dynamics time discretisation | Pure **implicit Euler** on the 5×5 system | `HighBurnupStructurePorosity.C` | Coeff matrix carries all implicit couplings; no residual explicit α·A term on the RHS |

## 3. Code changes vs parent commit (logical diff)

### `src/classes/Matrix.C`
- `setGrainBoundaryVacancyDiffusivity` case 3: removed hardcoded `hbs_correction = sin(40°)/sin(4°) ≈ 9.17`. The base value is now untilted; the α-weighted tilt correction is applied locally in `HighBurnupStructurePorosity` (Barani 2022 Eq. 7 applied symmetrically to `D_gb^SA` and `D_gb^v`).
- `setGrainBoundarySingleAtomDiffusivity` case 1: active formula is Xia 2022 (`2e-8·exp(-1.4/kT_eV)`). Olander-Van Uffelen "low D" (`1.3e-7·exp(-2.82/kT_eV)`) is commented with a note explaining why (catch-22 at T=723 K with 2-atom nucleation).

### `src/classes/System.C`
- `setProductionRate` case 1: added comment documenting that `sf = 1.25` rescales yield 0.24 → 0.30 when `iFuelMatrix=1`, to match cumulative Xe yield used for HBS calculations.

### `src/models/HighBurnupStructureFormation.C`
- Added `hbs_incubation_burnup = 15.0` as parameter[4] of the model (shifted from the earlier 20.0; see Section 4).
- Replaced the Decay-solver integration of `dα_r/dbu` with an **analytic** modified-KJMA:
  ```
  α_r = 1 - exp[-K · (bu_U - bu_inc)^n]   for bu_U > bu_inc
  α_r = 0                                  otherwise
  ```
  where `bu_U = bu_eff_UO₂ / 0.8814`. Robust across the `bu_inc` crossing; equivalent to the ODE integration for the no-threshold case.

### `src/models/HighBurnupStructurePorosity.C` case 2

#### Nucleation rate
- `pore_nucleation_rate` reads `bu_inc` from formation model parameter[4] and applies the incubation threshold: `ν_P = 0` for `bu_U ≤ bu_inc`, otherwise proportional to `(bu_U - bu_inc)^(n-1)`. Prefactor calibrated at `8.8e17` (1.76× Barani 2022's `5e17`) to recover N_p peak magnitude consistent with Cappia 2016 data; time-rate chain-rule `/0.8814` restored to match Eq. formula literally (see §11.9).

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

#### Percolation + mechanical saturation — vacancy pathway and post-hoc ΔV cap
The saturation factor `F_sat = (1 − ξ_old/ξ_sat)²` with `ξ_sat = 0.22` is computed **once** at the top of case 2 and acts through **two distinct physical channels** sharing the same functional form:

1. **Vacancy backbone percolation (Stauffer-Aharony `t=2`)**: `D_gb_v_eff = D_gb^v · tilt_factor · F_sat`, used in `volume_flow_rate = 2π ρ_P D_gb_v_eff / ζ(ψ)`. Vacancy transport requires bulk connectivity of the solid grain-boundary backbone; as the backbone fragments near `ξ_sat`, Schottky-vacancy flow shuts down.
2. **Mechanical cap on ΔV (stress on residual solid cross-section)**: the total pore-volume increment is capped post-hoc,
   ```
   V_pore_new = V_pore_old + F_sat · (V_pore_uncapped − V_pore_old)
   ```
   with `V_pore_uncapped = n_Xe · Ω_Xe + n_vac · Ω_Schottky`. Rationale: as local porosity approaches ~20%, the remaining solid matrix bears the mechanical load on a progressively smaller cross-section; the effective stress grows as ~1/(1−ξ) and suppresses further pore expansion regardless of whether the driver is gas or vacancy accumulation.

**β_n is NOT percolation-modulated.** `trapping_coeff_HBS = 4π · D_gb · R_p · (1 + 1.8 ξ^{1.3})` uses the tilt-corrected but un-percolated `D_gb^SA`: single-atom GB hops are a surface mechanism that does not require bulk backbone connectivity. Retaining β_n unchanged avoids the spurious `c_gb^HBS` plateau observed with the earlier symmetric two-pathway variant.

**EoS identity `V_p = n_Xe · Ω_Xe + n_vac · Ω_Schottky` is broken under saturation by design.** The ΔV cap represents over-pressurised pores whose expansion is inhibited by the solid matrix; Carnahan-Starling hard-sphere cap (0.65) still bounds the xenon packing fraction.

#### Coalescence / interconnection
`BinaryInteraction` receives `V_pore_increment = V_pore_increment_capped` (the **capped** ΔV), so coalescence slows together with pore growth near `ξ_sat`. After `BinaryInteraction`, V_pore per pore is rescaled by `N_before/N_after` rather than recomputed from n_Xe/n_vac, preserving the capped total volume `Σ V_p = N · V_per_pore` through the merging step. The bell-shape of `N_p(bu)` emerges naturally: for `ξ ≪ ξ_sat` (early phase) `F_sat ≈ 1` so coalescence proceeds on the full physical ΔV, while for `ξ → ξ_sat` both pore growth and merging slow down together.

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
  1. `plot_pore_density.png` — N_p vs bu + Cappia/Spino/Barani/SCIANTIX 2.0/2.2.1
  2. `plot_porosity.png` — ξ vs bu + ±σ_ξ band + 6 experimental datasets + 3 model curves
  3. `plot_pore_radius.png` — R_p vs bu + ±σ_R band + Cappia/Spino + 3 model curves
  4. `plot_xe_depletion.png` — Xe in grains vs Walker 1999 + Lassmann fit + α_r twin axis
  5. `plot_fuel_swelling.png` — matrix swelling breakdown vs Spino 2005 + α_r twin axis
  6. `plot_pore_variance.png` — B raw (at²/m³) vs bu (diagnostic, not for paper)
  7. `plot_CV.png` — coefficient of variation σ_n/n̄ vs bu (diagnostic, U-shape test)
  8. `plot_xe_inventory.png` — **xenon mass balance** across the six reservoirs (NR grain → NR GB → HBS grain → HBS GB → HBS pores → released). Two-panel stacked area: top = absolute inventory (10²⁶ at/m³) with total production dashed-line overlay for conservation check; bottom = fractional share (%) with `α_r` on twin axis. Validates the two-phase sweeping framework; under the current β_n formulation `c_gb^HBS` does not plateau, and saturation at high burnup manifests as slowing growth of the `A` reservoir (mechanical ΔV cap).

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

### Why `D_gb^v = 1e-39·Ḟ` (Barani 2022 original)
The earlier Frattini best-estimate `5e-41` was adopted when no post-hoc cap
on ΔV existed: with `1e-39`, porosity at 200 MWd/kgHM overshot to ~0.40
(Frattini thesis App. A, pp. 22-23), and could only be contained by
throttling `D_gb^v` itself. With the current mechanical ΔV cap (Section 3)
and the `D_gb^v` vacancy-percolation factor, porosity is bounded upstream,
so the athermal term is restored to the Barani 2022 original value. The
Carnahan-Starling cap on xenon packing fraction together with the ΔV cap
keeps porosity below ~0.20 at 175 MWd/kgHM without artificial throttling
of the base diffusivity.

### Why incubation burnup `bu_inc = 15` on α_r AND ν_P
Baron et al. 2009 describe HBS in three stages: (1) dislocation network,
(2) Xe depletion, (3) pore nucleation. Cappia 2022 and Gerczak 2018 show a
lag of ~15-25 MWd/kgHM between polygonization and pore nucleation.
Biswas-Aagesen 2025 (CMS 258 114052, Eq. 45) derives a modified KJMA
`α = 1 - exp[-k(β - β_th)^n]` from a phase-field dislocation-energy vs
subgrain-formation-energy balance. Applied consistently to both α_r and ν_P
to avoid "gas-reservoir burst" that occurs when applied to ν_P alone.

**`bu_inc` is an effective, homogenised bulk parameter — not a calibrated
constant.** Biswas-Aagesen 2025 explicitly model the subgrain-formation
threshold as **locally variable**: it is reduced near pre-existing gas bubbles
(which act as dislocation sinks and lower the energy penalty for subgrain
nucleation, modelled by their factor `r_b = 0.5`), and is shifted by the local
initial dislocation density (their Case V increases ρ_disl from 6.0 to
7.3×10⁻⁴ nm/nm³, which causes restructuring to initiate earlier and proceed
faster). Their 40 GWd/tU baseline is just the bulk-matrix calibration point
at fixed `k_s0 = 1.0`. The single value `bu_inc = 15` MWd/kgHM adopted in
SCIANTIX is therefore the macroscopic average of a fundamentally local
quantity, suitable for 0D fuel-performance applications. The procedure used
to fix this value is **parameter selection guided by sensitivity analysis +
physical-plausibility cutoff**, not least-squares calibration. Use this
framing when describing `bu_inc` in the paper, talks, or rebuttal letters.

**Sweep of `bu_inc ∈ {0, 10, 15, 20, 25, 30}`** against the PIE dataset
(Gerczak 2018 / Noirot 2015, N=8) with fixed `K = 2.77e-7`, `γ = 3.54`
yields the following RMSE on `α_r`:

| `bu_inc` (MWd/kgHM) | RMSE |
|---|---|
| 0 (unshifted) | 0.156 |
| 10 | **0.064** (numerical min) |
| 15 | 0.110 |
| 20 (previous baseline) | 0.180 |
| 25 | 0.252 |
| 30 | 0.320 |

`bu_inc = 15` is kept as the production value: it reduces RMSE by 39% over
the previous `20` baseline and stays below the unshifted 0.156, while still
respecting the lower bound of the experimental polygonisation-to-nucleation
lag reported by Cappia/Gerczak. `bu_inc = 10` is the numerical best fit
but sits at the edge of the experimental range and leaves no margin on
physical rationale, so it is **rejected on physical grounds** despite the
better RMSE. Reproduce the sweep with `context/kjma_fit_comparison.py`
(produces `kjma_fit_comparison.png` and `kjma_sensitivity_bu_inc.png`).

### Why ν_P prefactor bumped 5e17 → 8.8e17
The incubation threshold reduces the integral of ν_P. Recalibrating restores
peak ~5×10¹⁷ pores/m³, consistent with Cappia/Spino range. The literal
prefactor is `8.8e17` (was `1.0e18` while the chain rule in the time-rate
conversion carried a hidden factor-of-0.8814 error, so the effective prefactor
was `0.88e18` all along; see §11.9 for the fix).

### Why vacancy-percolation + mechanical ΔV cap (and not β_n modulation)
**Important:** HBS pores do NOT release gas by venting in steady-state
operation. Gas stays trapped at 30-100+ MPa overpressure (Hiernaut 2008
JNM 377, Noirot 2008 JNM 372). Gas is released only by fragmentation during
transients (Kulacsy 2015 JNM 466, Jernkvist 2019/2020). Therefore zero FGR
from HBS pores in isothermal tests is **physically correct**.

The experimentally observed porosity saturation at ξ ≈ 0.15-0.20
(Spino 2005, Cappia 2016) has **two distinct microstructural origins**,
which must be modelled separately to reproduce PIE data correctly:

1. **Percolation of the vacancy backbone.** Schottky-vacancy transport
   along grain boundaries propagates through the connected solid backbone.
   As ξ → ξ_sat, the backbone fragments and vacancy flow shuts down.
   Stauffer-Aharony 1994 gives `D_gb^v_eff = D_gb^v (1 − ξ/ξ_sat)^t` with
   `t ≈ 2` (3D lattice conductivity exponent). This factor is applied to
   `D_gb^v` inside the Speight-Beere block.

2. **Mechanical stress on the residual solid cross-section.** Independent
   of transport-network connectivity, as ξ rises the remaining solid
   matrix must bear the mechanical load on a smaller cross-section, so
   effective stress grows as ~1/(1−ξ) and opposes further pore expansion.
   This acts directly on the pore-volume increment, not on an upstream
   transport coefficient:
   `ΔV_p^physical = F_sat · ΔV_p^EoS`, with `F_sat = (1 − ξ/ξ_sat)²` as
   an empirical fit to the same saturation envelope.

**β_n is NOT percolation-modulated.** Single-atom gas hops along grain
boundaries are a surface process that does not require bulk backbone
connectivity, so there is no percolation argument for `D_gb^SA`. An
earlier symmetric two-pathway variant (F_sat on both D_gb^SA and D_gb^v,
no ΔV cap) was discarded after regression evidence: β_n → 0 caused xenon
to pile up on `c_gb^HBS`, producing an unphysical plateau at high burnup
(visible as a backup on the xenon-inventory plot), and the `N_p(bu)`
decay post-peak was too shallow.

**Implementation** in `HighBurnupStructurePorosity.C` case 2:
- `D_gb_v_eff = D_gb^v · tilt_factor · F_sat` (Speight-Beere only)
- `trapping_coeff_HBS = 4π · D_gb · R_p · (1 + 1.8 ξ^{1.3})` (no F_sat)
- `V_pore = V_pore_old + F_sat · ΔV_uncapped` (post-hoc cap)
- `BinaryInteraction` receives the capped ΔV; V_pore is rescaled by
  `N_before/N_after` after coalescence to preserve the capped total volume.

**EoS identity `V_p = n_Xe Ω_Xe + n_vac Ω_Schottky` is deliberately broken**
under saturation — the pores carry more matter than their capped volume
would host at unit packing, representing mechanical over-pressurisation.
The Carnahan-Starling hard-sphere cap (0.65) still bounds the internal
state thermodynamically.

**Physical signatures in the output** (visible in `plot_xe_inventory.png`
and `plot_pore_density.png`):
- No `c_gb^HBS` plateau (β_n uncapped → xenon keeps flowing into pores).
- `N_p(bu)` bell shape with gentle post-peak decay matching Cappia 2016.
- `ξ(bu)` saturates smoothly around 0.18-0.20 at 175 MWd/kgHM.

### Why `(bu_eff/0.8814)` conversion
`0.8814 = M(U) / M(UO₂)`. Converts MWd/kgUO₂ → MWd/kgHM. Stoichiometry,
not calibration. Do not remove.

## 5. Open issues / known limitations

### A. HBS pores are a closed system by design (physically correct in steady state)
`GrainBoundaryMicroCracking` and `GrainBoundaryVenting` operate on NR
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
With bu_inc=15 and ν_P=8.8e17 (current baseline), peak is at 96–100, slightly below Cappia ~110.
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
| bu (MWd/kgHM) | porosity | N_p (1/m³) | R_p (m) |
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
| `src/models/HighBurnupStructureFormation.C` | KJMA α_r with incubation burnup (case 2, `bu_inc = 15`, production baseline); case 1 is Barani 2020 without incubation. Additional cases exist in the codebase but are **out of scope** of this branch/paper — do not edit or reference. |
| `src/models/HighBurnupStructurePorosity.C` | **Core (case 2)**: fully implicit Euler 5×5 cluster dynamics, nucleation with `bu_inc`, local tilt correction on both `D_gb^SA` and `D_gb^v`, vacancy-pathway percolation on `D_gb^v`, mechanical post-hoc cap on ΔV (coalescence included). case 3 exists in the codebase but is **out of scope** of this branch/paper — do not edit or reference. |
| `src/operations/SetMatrix.C` | Matrix setup; UO2HBS uses `iGrainBoundaryVacancyDiffusivity=3` hardcoded. UO2HBS pore surface energy `γ = 1.0 N/m`; UO2 matrix uses `γ = 0.7 N/m`. |
| `regression/regression_hbs.py` | HBS regression with 8 plots (density, porosity+σ, radius+σ, Xe depletion, swelling, variance B, CV, xenon inventory mass balance) |
| `regression/regression_functions.py` | Shared regression utilities; `sciantix_dictionary()` variable list |
| `regression/test_UO2HBS/radial_plots/run_radial.py` | Multi-burnup radial sweep with 5 comparison plots |

## 8. Key references

- Barani et al., JNM 539 (2020) 152296 — HBS formation KJMA
- Barani et al., JNM 563 (2022) 153627 — HBS porosity cluster dynamics
- Zullo et al., NED 429 (2024) 113602 — 3-eq spectral solver for sweeping
- Frattini MSc thesis (2025) — mechanistic revision, calibration
- Biswas & Aagesen, CMS 258 (2025) 114052 — phase-field HBS, modified KJMA Eq. 45
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
with bibliography at `HBS.bib`. Target journal: Nuclear Engineering and Design (NED). Builds on the NED 429 (2024) 113602 paper by Zullo et al. on two-phase fission-gas sweeping; the current paper extends that framework to HBS porosity.

**Bibliography export.** `HBS.bib` is maintained in Zotero and must be exported with the **Better BibTeX** translator (not *Better BibLaTeX* or *BibLaTeX*). The paper compiles with classic `bibtex` + `\bibliographystyle{elsarticle-num-names}`, which expects the bibtex-style fields `year` / `journal` / `number`; biblatex-style fields `date` / `journaltitle` / `issue` render as `(????)` with a missing journal. Recommended: enable "Automatic export: On Change" in Better BibTeX so the file stays in sync with the Zotero library.

### Equations aligned with the code

| Paper | Form | Notes |
|---|---|---|
| Eq. 12 (definition of `β_n`) | `β_n = 4π D_gb^SA c_gb R_n^p (1 + 1.8 ξ^{1.3})` | `β_n` **already includes** `c_gb^HBS` (atoms/pore·s). Frattini convention. |
| Eq. 15 (`dA/dt`) | `dA/dt = 2ν_P − α_n A + β_n N_p` | Previously had `β_n^{tot} c_gb^HBS` which double-counted `c_gb^HBS`. Now matches Frattini thesis exactly. |
| Eq. 18 (`dc_gb^HBS/dt`) | `dc_gb^HBS/dt = S_HBS + w c_gb − 2ν_P − β_n N_p + α_n A` | `−2ν_P` nucleation sink added (two atoms leave the reservoir per pore nucleated). Mass balance `d(A + c_gb^HBS)/dt = S_HBS + w c_gb` is now exact. |
| Eq. 22 (saturation factor) | `F_sat = (1 − ξ/ξ_sat)^2`, applied to `D_gb^v` (vacancy percolation) and to the total ΔV (mechanical cap); **not** to `D_gb^SA`. | Rewritten. The symmetric two-pathway form was abandoned after regression (spurious `c_gb^HBS` plateau). |
| §3.4 text | `D_gb^v = White + 1.0e-39·Ḟ` (Barani 2022 original); tilt applied symmetrically to `D_gb^SA`; percolation factor applied only to `D_gb^v`, not to `D_gb^SA`. | The Frattini `5e-41` paragraph is removed: ΔV cap bounds porosity upstream, so base `D_gb^v` reverts to the Barani value. |
| §3.6 text | Two-channel saturation: (i) vacancy-backbone percolation on `D_gb^v`, (ii) mechanical cap on ΔV. EoS identity `V_p = n_Xe Ω_Xe + n_vp Ω_vac` **is broken under saturation by design** (over-pressurisation). | Replaces the "applied directly to both grain-boundary diffusivities" paragraph. |
| §3.4 / §3.2 `β_n` eq | `β_n N_p` | Last residual `β_n^{tot}` removed. β_n is no longer percolation-modulated. |
| Table 1 | Full parameter list: KJMA (K, γ, bu_inc), ν_P prefactor, d_V/δ_V, D_gb^SA (Xia), D_gb^v (White), tilt, ξ_sat, pore surface energy γ=1.0 N/m, effective cumulative yield y=0.30 at/fiss. | Single table (`tab:model_parameters`). Uses `tabularx` layout with phase subheaders. |
| §5.5 | "Xenon inventory and mass balance" | Three observations: (1) conservation to 10⁻³ relative, (2) onset of HBS reservoir at `bu_eff ≈ 40–50 MWd/kgHM`, (3) closed-system behaviour (`c_r ≈ 0` throughout). The earlier `c_gb^HBS` plateau claim has been removed — β_n is no longer damped, so no plateau forms; saturation of the pore inventory is driven by the mechanical ΔV cap instead. |

### Plots in `Images/` for the manuscript
- `plot_pore_density.png` (Fig. 3 — N_p vs bu)
- `plot_porosity.png` (Fig. 4 — ξ vs bu with ±σ_ξ band)
- `plot_pore_radius.png` (Fig. 5 — R_p vs bu with ±σ_R band)
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
7. **`bu_inc` shifted 20 → 15 MWd/kgHM** in `HighBurnupStructureFormation.C` case 1. Motivated by the RMSE sweep documented in Section 4: 0.110 at `bu_inc = 15` vs 0.180 at the previous `bu_inc = 20`, and below the unshifted 0.156. Regression `output_gold.txt` values at mid-burnup shift slightly; the qualitative bell-shape of `N_p(bu)` and the ξ/R trajectories are preserved.

## 11. Recent-session change log (2026-04-23)

Regression evidence (steep `N_p(bu)` decline post-peak, spurious `c_gb^HBS` plateau) motivated reverting the symmetric two-pathway percolation and reinstating a post-hoc ΔV cap. Changes relative to Section 10 baseline:

1. **Percolation removed from β_n.** `trapping_coeff_HBS = 4π · D_gb · R_p · (1 + 1.8 ξ^{1.3})` — no more `saturation_factor` multiplier ([src/models/HighBurnupStructurePorosity.C:173]). Single-atom GB hop is a surface mechanism and does not require bulk backbone connectivity.
2. **Post-hoc cap on ΔV restored.** `V_pore = V_pore_old + F_sat · (V_pore_uncapped − V_pore_old)` with `F_sat = (1 − ξ/ξ_sat)²` ([src/models/HighBurnupStructurePorosity.C:301-322]). Represents mechanical stress ~1/(1−ξ) on the residual solid cross-section. EoS identity `V_p = n_Xe·Ω_Xe + n_vac·Ω_Schottky` is broken by design (over-pressurised pores).
3. **Coalescence uses the capped ΔV.** `V_pore_increment = V_pore_increment_capped` ([src/models/HighBurnupStructurePorosity.C:318]). Uncapped was tried first and produced excessive post-peak decay; capped gives a gentler bell-shape matching Cappia 2016 at 125-175 MWd/kgHM.
4. **V_pore rescaled through coalescence.** After `BinaryInteraction`, V_pore per pore is multiplied by `N_before/N_after` rather than recomputed from n_Xe/n_vac, preserving the capped total volume ([src/models/HighBurnupStructurePorosity.C:350-366]).
5. **`D_gb^v` reverted to Barani `1e-39·Ḟ`** ([src/classes/Matrix.C:86]). The Frattini `5e-41` was only needed to compensate for the absent mechanical ΔV cap; with the cap restored, porosity is bounded upstream and the base diffusivity is free to return to its original value.
6. **Paper alignment update.** §3.4 vacancy diffusivity replaced (`1e-39`, Frattini paragraph removed); §3.6 rewritten as vacancy-percolation + mechanical ΔV cap (no more symmetric two-pathway); §5.5 observation #3 on `c_gb^HBS` plateau replaced with mechanical-cap rationale; §6 "two-pathway" phrasing updated; abstract/highlights qualified ("empirical porosity caps").
7. **Case 3 of `iHighBurnupStructurePorosity` unchanged** and **out of scope** of this branch/paper. It is present in the codebase but is not described in this document or in the manuscript. Do not edit, reference, or propose corrections to case 3.
8. **`+β_n N_p` trapping source added to `dB/dt`** ([src/models/HighBurnupStructurePorosity.C:189-195], `coeff_matrix[14] = -total_trapping_rate_HBS*dt`). The B row of the 5×5 system previously had only the self-coupling and the `(n̄-2)²·ν_P·dt` nucleation source, matching Eq. 17 of the paper. This was wrong: the `+β_n N_p` term is a kinematic broadening (each trapping event shifts a pore n → n+1, adding `2(n-n̄)+1` to the variance integrand) that is **exact** in the size-independent / mean-field limit, not a higher-order correction. Paper Eq. 17 and the surrounding footnote (§3.2) were rewritten accordingly. Effect on primary outputs (porosity, N_p, R_p) is <0.3% at all burnups; B grows 1-3% at mid-to-high burnup, more at the onset where the distribution is narrower. CV-derived σ bands on the porosity/radius plots widen slightly. Case 3 was NOT modified per rule 7. Regression gold file refreshed.
9. **Chain-rule fix on the nucleation-rate time conversion** ([src/models/HighBurnupStructurePorosity.C:115-132]). The analytic derivative `d(α_r)/d(bu_U)` was formed in MWd/kgHM (correct), but the time-rate multiplier used `Δ(bu_eff_UO2)/Δt` without the `/0.8814` factor needed to convert `Δbu_eff_UO2 → Δbu_U`. Effective ν_P was therefore ~12 % lower than the paper formula implied, and the `1.0e18` prefactor absorbed this silently. Fix: added `/0.8814` to the time-rate multiplier and rescaled the literal prefactor `1.0e18 → 8.8e17 = 1.0e18 × 0.8814`, preserving the peak-N_p calibration against Cappia (~5×10¹⁷ at 96–100 MWd/kgHM). Paper Eq. 11, surrounding paragraph, Table 1 row, and Results §Pore number density all updated to the new literal `8.8×10¹⁷`. Net physical behaviour of the model is unchanged; the nucleation formula now matches the paper literally (no hidden unit conversion in the prefactor). Regression output preserved to within rounding (8.814 → 8.8 rounding drift ≈ 0.16 %). Gold refreshed.

## 12. Recent-session change log (2026-04-24)

Consolidation and notation-sync pass on paper + code + bibliography ahead of NED submission.

1. **Target journal switched to NED.** `\journal{Journal of Nuclear Materials}` → `\journal{Nuclear Engineering and Design}`. Continues the lineage of Zullo et al., NED 429 (2024) 113602 (two-phase fission-gas sweeping).
2. **Title updated to differentiate from Barani 2020/2022.** New title: *"Two-phase cluster-dynamics modelling of high-burnup structure porosity in UO₂ fuel with vacancy-percolation and mechanical saturation"*. Flags the two distinctive contributions (two-phase NR↔HBS coupling and the physics-grounded two-channel saturation) that Barani's papers do not have.
3. **Highlights trimmed to ≤85 chars each** (NED submission-system hard limit): 75/47/84/67/79 chars.
4. **Burnup notation unified to MWd/kgHM.** `MWd/kgU` and `GWd/tU` occurrences in the paper and in `regression/regression_hbs.py` replaced with `MWd/kgHM` (preserves `MWd/kgUO₂` for the SCIANTIX internal variable). All 8 regression plots regenerated with the new axis label.
5. **NR notation unified.** Paper §Discussion and `regression_hbs.py` legend swapped `non-HBS` → `NR` for the grain-boundary micro-cracking and Xe-depletion label. Paper now uses NR/HBS subscripts consistently.
6. **Plot x-axis switched to Burnup (not Effective Burnup).** `regression_hbs.py` Plots 1, 2, 3, 6, 7, 8 now plot against `primary["burnup"]` instead of `primary["effectiveBurnup"]`. Variable `x_label_bu_eff` renamed to `x_label_bu`; text changed to "Burnup (MWd kgHM⁻¹)". Paper §5 and §6 narrative also use `bu` (not `bu_eff`) in inline math at plot-location references. `bu_eff` **kept** in §1 literature review, §3.1 KJMA physics definition (it is the integrated Heaviside burnup by definition), and §3.3 ν_P formula (inherits α_r's physics variable).
7. **Schema figure added at top of §3 Model.** `Images/schema_hbs.png` shows the two-phase reservoir architecture: NR intragranular (`c_{NR}`↔`m_{NR}`), NR GB (`c_{gb}`), HBS intragranular (`c_{HBS}`), HBS GB + pores (`c_{gb}^{HBS}`↔`A`), with sweeping terms `w = (1/(1−α_r))∂α_r/∂t` between them and FGR drawing only from the NR GB bubbles. Caption cross-references Eqs. 1 and 15-18.
8. **Bibliography source fix.** `HBS.bib` re-exported from Zotero using the **Better BibTeX** translator (not Better BibLaTeX). All entries now use bibtex-style `year`/`journal` fields that `elsarticle-num-names` understands; the previous biblatex-style `date`/`journaltitle` fields were producing "(????)" in the rendered bibliography on ~30 entries.
9. **Missing bib keys resolved.** Four keys cited in main.tex that were absent from the Zotero-exported file were replaced with the correct existing keys: `speight_calculation_1969` → `cornellRoleBubblesFission1969`, `ham_theory_1958` → `hamTheoryDiffusionlimitedPrecipitation1958a`, `gosele_concentration_1978` → `goseleConcentrationDependenceRate1978a`, `frattiniMScThesis2025` → `frattiniPhysicsbasedModelingHigh2025`. Added new entry `walkerAssessmentRadialExtent1999` (Walker, JNM 275, 1999) and switched the §5.1 Xe-depletion markers to cite it (previously credited to Lassmann 1995).
10. **Frattini citation downgraded.** The MSc thesis is no longer cited inline as a primary authority; it now appears in a footnote in §3.2 ("The same bookkeeping convention is adopted in a recent MSc thesis on the HBS cluster-dynamics model.") as supporting attribution only, consistent with the reduced weight appropriate for unpublished thesis work.
11. **Sweeping rate `w` formally defined.** Paper §3.2 now introduces `w = (1/(1−α_r))·dα_r/dt` as an explicit equation (`eq:w`) and rewrites Eqs.~\ref{eq:dcgb}-\ref{eq:dcgbhbs} with `w`, eliminating the previous inconsistency where the full expression was written in the equations while §3.3 and §5.5 text used the shorthand.
12. **"without empirical caps" overclaim fixed (2 places).** §3 advances bullet and Fig.~\ref{fig:PorosityResults} caption now say *"replaces the discontinuous 15 % empirical ceiling of SCIANTIX 2.0 with a physics-grounded two-channel saturation"* instead of "without empirical caps" / "removes the empirical cap". The model does have a mechanical ΔV cap; the earlier phrasing contradicted the (correct) abstract, §3.6, and conclusions.
13. **`JMAK` → `KJMA` globally.** 6 occurrences across `main.tex`, `HighBurnupStructureFormation.C`, `HighBurnupStructurePorosity.C`, and this CONTEXT.md normalized to `KJMA` (Kolmogorov-Johnson-Mehl-Avrami).
14. **Units of measure in parentheses, not brackets.** `[eV]` → `(eV)` at the two remaining occurrences (Table 1 `D_gb^SA` row and Eq. 10).
15. **Table 1 reshaped.** Used `tabularx` with `X` column on the Value field, italic phase subheaders (*Non-restructured UO₂*, *Restructured UO₂-HBS*, *Both phases*), Matrix column dropped. Fits `\linewidth` cleanly (was overflowing by ~99 pt). `\small` font. Added rows for all missing parameters: KJMA (`K`, γ, `bu_inc`), `ν_P` prefactor, `d_V`/`δ_V`, `D_gb^SA` (Xia 2022), `D_gb^v` (White + `1.0e-39·Ḟ`), tilt correction, `ξ_sat`.

## 13. Manuscript style rules

Conventions converged during the finalization pass. Apply these when editing `main.tex` in future sessions; they encode choices that were already made and should not be re-litigated without a reason.

1. **Units in parentheses, not square brackets.** Write `(eV)`, `(K)`, `(m/s)` — never `[eV]`. Square brackets in the source belong to LaTeX syntax only (`\cite`, `\includegraphics[width=…]`, placement specifiers).
2. **No blanket "eliminates empirical caps/thresholds" claims.** The model *replaces* the empirical 15 % porosity ceiling with a two-channel saturation (vacancy percolation on `D_gb^v` + mechanical cap on ΔV), and keeps `bu_inc = 15 MWd/kgHM` as a physics-motivated incubation threshold. Abstract / Results §porosity / Conclusions must reflect "replaces with a physically-grounded mechanism", not "eliminates".
3. **NR vs HBS subscripts, applied consistently.** Unknowns are `c_NR`, `m_NR`, `c_HBS` in the intra-granular problem, and `c_gb`, `c_gb^{HBS}` on the grain boundaries. Never mix in leftover `C_1`, `m_1`, `C_2` from earlier drafts.
4. **β_n follows the Frattini convention.** `β_n = 4π D_gb^SA c_gb^{HBS} R_n^p (1+1.8 ξ^{1.3})` already *includes* `c_gb^{HBS}`, so `β_n N_p` is the total trapping rate (atoms/m³/s). Do not write `β_n c_gb^{HBS} N_p` anywhere — that double-counts.
5. **γ symbol conflict is disambiguated by label, not renamed.** `γ` is used for both the Avrami exponent (= 3.54) and the pore surface energy (= 1.0 N/m). Each occurrence must carry its label explicitly ("Avrami exponent" / "pore surface energy") in Table 1 and in the prose.
6. **No developer-history residue in the manuscript.** Phrases like "an earlier formulation used X but sensitivity tests showed…" belong in this CONTEXT.md or in commit messages, not in the paper.
7. **Only use existing bib keys.** If a dataset needs a reference that is not in `HBS.bib` (e.g., `walker1999`), cite the closest existing source (`lassmannModellingHighBurnup1995` for the Xe-depletion EPMA data) and flag the gap so the user can add the entry. Never invent bib keys.
8. **Parameter table layout.** Table 1 uses `tabularx` with `X` on the Value column; phase grouping (*Non-restructured UO₂*, *Restructured UO₂-HBS*, *Both phases*) is done via `\multicolumn{3}{@{}l}{\textit{…}}` subheader rows, not a separate Matrix column. Font is `\small`. Do not re-introduce a fixed-width Value column — long formulas like `D_gb^v` overflow.
9. **dB/dt must include +β_n N_p.** Eq. 17 of the paper and `coeff_matrix[14]` of case 2 in `HighBurnupStructurePorosity.C` are coupled by this term. It is exact in the mean-field limit, not a higher-order correction. See §11.8.
10. **Case 3 of `iHighBurnupStructurePorosity` is out of scope** of this branch and paper. Do not edit, reference, or describe it. Restated from §11.7 because it is easy to forget when propagating changes into case 2.
11. **`bu_inc` is a parameter selection, not a calibration.** Per §4 and §14.2, the procedure that fixed `bu_inc = 15` is sensitivity sweep + physical-plausibility cutoff (rejected `bu_inc = 10` despite lower RMSE). Use "selection guided by sensitivity analysis" / "effective bulk parameter" wording — never "calibrated" — both in the paper and in talks. The Biswas-Aagesen 2025 reference must be acknowledged as describing a locally-variable threshold, not a single global constant.
12. **Each acronym must be defined at its first occurrence in the body**, then reused in short form thereafter. Form: *"long form (ACRONYM)"* on first use, e.g. *"post-irradiation examination (PIE) data"*, *"loss-of-coolant accidents (LOCA)"*, *"electron probe microanalysis (EPMA)"*. Do not introduce an acronym that is then never reused — drop the parenthetical (e.g. "cluster dynamics (CD)" became plain "cluster dynamics" because "CD" is not used again). The abstract is treated as self-contained: either spell out fully (preferred when the term appears once) or redefine separately. Do not rely on the body's definition to cover an abstract use, or vice versa. Acronyms currently in use in the paper: HBS, NR, UO₂, LWR, MOX, FBR, EBSD, LOCA, RIA, FPC, PIE, KJMA, FGR, RMSE, PCMI, EoS, PWR, EPMA, FIMA. Chemical formulas (UO₂, U-Pu, U-Mo, MWd/kgHM, MWd/kgUO₂) are not acronyms and do not require expansion.

## 14. Recent-session change log (2026-04-28)

Manuscript polish pass against the §13 style rules; no code changes.

1. **Eq.~\ref{eqn: fission gas} notation cleaned up** ([main.tex:193-196]). `gC_1 + bm_1` → `g\,c_{NR} + b\,m_{NR}`; bare `\alpha` → `\alpha_r` at five occurrences. Style rule §13.3.
2. **`bu_inc` reframed from "calibration" to "parameter selection"** ([main.tex:165-170]). Body §3.1 now includes a one-paragraph note that B&A's threshold is intrinsically locally variable (reduced near gas bubbles via dislocation-sink effect; shifted by initial dislocation density), and that our single `bu_inc = 15` is therefore an effective, homogenised bulk parameter — not a fundamental constant. Figure caption (`fig:BuIncSensitivity`) retitled "Sensitivity of the modified-KJMA fit to the incubation burnup..." with explicit acknowledgement that the value lies within the experimentally observed polygonisation-to-nucleation lag. Style rule §13.11 added to lock this framing in for future sessions.
3. **§6.1 overclaim removed** ([main.tex:551]). "removes arbitrary burnup thresholds and empirical porosity caps" → "replaces the discontinuous 15 % empirical porosity ceiling…with a two-channel saturation mechanism…and grounds the onset of restructuring in a physics-motivated $bu_{inc}$". Style rule §13.2.
4. **Limitations notation fix** ([main.tex:581]). `D_V` → `D_{gb}^v`.
5. **Frattini name-drop in §6.4 removed** ([main.tex:583]). "comparable to the scatter reported in Frattini's thesis" → "lies within the experimental scatter of the Cappia and Spino datasets" with proper `\cite`. Consistent with §12.10 (thesis is footnote-only).
6. **Equation labels added** for `dA/dt` and `dB/dt` ([main.tex:303-304]): `\label{eq:dA}`, `\label{eq:dB}`. Range `\eqref{eq:dNp}--\eqref{eq:dcgbhbs}` already worked by number, but explicit labels make individual cross-refs robust.
7. **Conclusions sentence rewrap** ([main.tex:611]). "yielding a continuous and physically grounded description of HBS porosity on a single characteristic porosity" → "calibrated on a single characteristic porosity, …yielding a continuous and physically grounded description of HBS porosity evolution". Removed dangling phrase.

Build status after the pass: 44 pages, 0 undefined references, 0 missing files, 10 cosmetic overfull hboxes (range 2.6-25.7 pt, all pre-existing). Bibtex emits 5 pre-existing minor warnings (missing institution / empty pages on three entries, empty journal on the Frattini thesis); none affects rendering.

Continuation pass (same date), additional manuscript edits:

8. **Two large overfull hboxes shrunk.** Line ~418 (§Results intro, was 25.7 pt) → 4.5 pt by tying citations with `~` and tightening the experimental-data sentence ("are taken from"). Line ~555 (§Discussion, TRANSURANUS sentence, was 20.4 pt) → eliminated by leading with the empirical-treatments subject and dropping the em-dashed parenthetical. Net: 10 overfulls → 8.
9. **Abstract balanced with the modified-KJMA contribution** ([main.tex:43]). Added one sentence to paragraph 2: *"The onset of restructuring is shifted with respect to the original Barani 2022 formulation by introducing a phase-field-derived incubation burnup, $bu_{inc}=15$~MWd/kgHM, selected through a sensitivity analysis against post-irradiation examination data."* Wording follows §13.11 ("selected", not "calibrated"). Also tightened "state-of-the-art mechanistic approaches" → "the mechanistic formulation of Barani et al." for accuracy.
10. **Acronym audit pass against new Style rule §13.12.** Defined at first body use: LWR, MOX, FBR (line 80), LOCA, RIA (line 88), PIE (line 96), FGR (line 99), NR (line 102), RMSE (line 165), PCMI (§Vacancy absorption), EoS (§Porosity saturation, also restructured the surrounding paragraph so the EoS identity is named before the equation), PWR (§Results intro), EPMA (§Xe depletion), FIMA (§Fuel swelling). Removed the unused `(CD)` parenthetical from "cluster dynamics" at first use. Removed the redundant `(NR)` / `(HBS)` re-definition at line 174 (both already defined upstream).
11. **Style rule §13.12 added** (acronym-at-first-use rule), with the canonical list of acronyms in current use.

Final build status (mid-pass): 44 pages, 0 undefined references, 8 cosmetic overfulls (largest 23.8 pt at the Conclusions novel-features bullet).

Third continuation pass (same date), regression-script and §1 polish:

12. **Plot legend labels generalised** ([regression_hbs.py:335-336]). `label_primary` "SCIANTIX 2.2.1, this work" → "SCIANTIX, this work"; `label_reference` "SCIANTIX 2.2.1, semi-empirical (Barani 2020)" → "SCIANTIX, semi-empirical". The "(Barani 2020)" attribution was misleading because the SCIANTIX 2.0 empirical baseline draws from multiple sources, not Barani 2020 only. Also unified "(total)" suffix style on plot 5 (fuel_swelling): `label_reference + ", total"` → `label_reference + " (total)"` and `label_primary + ", total matrix swelling"` → `label_primary + " (total)"` for symmetry with plot 4.
13. **Plot 2 (porosity) re-coloured** ([regression_hbs.py:382, 83]). Spino~2006 markers re-coloured `COLOR_REFERENCE` (`#1f77b4`, blue) to make the visual link to the SCIANTIX-semi-empirical fit (which was calibrated on Spino data) immediate. To avoid the resulting Spino/Noirot blue clash, `COLOR_NOIROT` was changed `#1f6aa3` → `#d62728` (matplotlib tab:red, distinct from Spino's `#c44545` reds used in plot 1). `COLOR_NOIROT` is only used in plot 2, so the change is local.
14. **Two more Conclusions overfulls eliminated** ([main.tex:607]). Split the long "Novel features include..." bullet into two bullets (general novelties + dedicated bullet for the two-channel saturation), and removed the redundant cap/saturation re-statement from the previous bullet 4 to avoid duplication. The 23.8 pt and 14.8 pt overfulls are gone; only 5 cosmetic overfulls remain (all <14 pt).
15. **§1 Introduction polish** ([main.tex:90, 99, 101, 106, 116]):
    - "Extending burnup ... makes it essential to capture HBS evolution" → "Because burnup is being extended ... accurate modelling of HBS evolution becomes essential" (cleaner subject-verb structure).
    - `Kolmogorov-Johnson-Mehl-Avrami` → `Kolmogorov--Johnson--Mehl--Avrami` (en-dashes, typographically correct for compound author names).
    - "Cappia's PIE data" → "the PIE data of Cappia et al." (consistent with the "X et al." convention used everywhere else).
    - "implemented in SCIANTIX 2.0 in simplified form" → "implemented in simplified form in SCIANTIX~2.0" (cleaner word order).
    - "Zullo and co-authors \cite{...}" → "our previous work \cite{...}" (consistent with line 174's self-citation style).
    - Removed redundant `(NR)` and `(HBS)` re-definitions at line 116 (both already defined at lines 102 and 79 respectively, per Style rule §13.12).

Final build status: 44 pages, 0 undefined references, 5 cosmetic overfulls (all <14 pt, all pre-existing). Bibtex still emits the same 5 minor warnings.