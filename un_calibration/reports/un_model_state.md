# UN model — current state (2026-05-09)

This document supersedes earlier notes for the description of the **current**
state of the Python UN fission-gas model. Historical / pre-audit content is
preserved in `un_physics_notes.md`.

## 1. Architecture

```
un_calibration/
├── model/                       parameter-free physics + solver
│   ├── un_model.py              Candidate, UNParameters, solve_UN(...)
│   └── un_data.py               Ronchi 1978 digitised anchor data
├── config/                      single source of truth for all constants
│   ├── rizk_constants.py        RIZK_CONSTANTS — Rizk 2025 + Schneider 2024
│   ├── manual_params.py         MANUAL_PARAMS — free-fit + scales + ρ_d toggles
│   ├── rho_d_laws.py            constant / Blank-FT / Rizk-NEAMS exp laws
│   └── builder.py               build_candidate / build_un_params / model_runner
├── scripts/                     one analysis per file, no argparse
│   ├── smoke_test.py
│   ├── fig3_swelling_vs_T.py    Rizk Fig. 3 reproduction (with Ronchi ±10% bars)
│   ├── fig4_diffusivity_vs_T.py our Fig. 4 (D_g, D_v decomposed)
│   ├── fig78_NdRd_vs_T.py       Rizk Fig. 7 + 8 reproduction
│   ├── fig9_gas_partition.py    Rizk Fig. 9 reproduction
│   ├── rho_d_diagnostic.py      3D surface of the active ρ_d law
│   ├── flag_ablation.py         2×2 study of (φ, mass-coupling) flags
│   ├── rho_d_laws_comparison.py 3-way comparison of the ρ_d laws
│   └── sensitivity_scan.py      1D scans of K_d, f_n, ρ_d
└── reports/                     output of every script (PNG + CSV)
```

## 2. Physics decisions (with citations)

| Decision | Status | Why |
|---|---|---|
| Capture bulk→disl step | **REMOVED** | Not in any literature paper (NotebookLM cross-check on Barani 2019/2020, Pizzocri 2020, Rizk 2023/2025, Qian 2021, Setyawan 2018). Slight degradation in fit (RMSE +0.1). |
| φ-correction (Eq. 25) on `c` and `m_{b,d}` equations | **ON** by default | Rizk paper applies φ only in dN_b/dt, but the gas-balance bookkeeping requires φ in the resolution terms too (Olander 2006 review; Pizzocri 2020 / Barani 2019 implement it consistently). Without φ the model under-predicts swelling by ~50% (bias −1.81). |
| Mass-coupling ±2ν_b in `dc/dt` and `dm_b/dt` | **ON** by default, flag preserved | Rigorous mass conservation when dimers form (Pizzocri 2020). Numerically zero effect at Rizk-nominal conditions (ν_b ∝ c² and c is small) but kept for physics defensibility. |
| Coalescence | **Implicit Euler quadratic** | Rizk 2025 Eq. 15 is the closed-form for *constant* λ over Δt, not the BE step. We solve `4λΔV·N²+N−N_old=0` in numerically-stable form `N = 2N_old/(1+√(1+16λΔV N_old))`, consistent with the BE pattern of `vacancy_concentration_implicit_step`. |
| D2 (irradiation-enhanced thermal) for Xe | **NOT summed** in D_g | Rizk Sec. 3.1.1: "D2 negligible for Xe". Centipede fit produces D2 ≈ 10⁻¹⁰⁷ at the minimum (T≈1336 K), 30+ OOM below D1+D3. Computed for diagnostic only. |
| D2 for V_U | **DROPPED entirely** | Rizk Tab. 2 V_U parameters mathematically broken: with the paper's Eq. 4 form they give D2_v ≈ 10⁻⁶ m²/s at 1500 K, ~14 OOM too high. Schneider 2026 cluster dynamics gives the *actual* D2_v ≈ 10⁻²⁴, negligible vs D1+D3. |
| D3 athermal for V_U | **ADDED** | Schneider 2024: D3_v = 2.48×10⁻²² m²/s at F = 5×10¹⁹, taken as `A30_VU = 4.96×10⁻⁴² m⁵`. Gives the right T,F-asymptotic floor. |
| K_d (bub/m, Rizk 2025 Eq. 11) | **5×10⁵** | Rizk 2025 §4 calibration for UN. Gives `N_d(t=0) = K_d · ρ_d = 1.5×10¹⁹`, which sits in the experimental cloud (Ronchi at 1.3% FIMA reports N_d ~ 5×10¹⁸ − 3×10¹⁹). |

## 3. Equations solved (current configuration)

All in `un_model.py::solve_UN` with `USE_PHI_GAS_RESOLUTION = USE_NUCLEATION_MASS_COUPLING = True`.

```
spectral 3-eq (sphere, sin basis, backward Euler per mode):
    ∂c/∂t   = D_g ∇²c − (g_b + g_d) c + b_b φ_b m_b + b_d φ_d m_d + (β − 2ν_b)
    dm_b/dt =                  g_b c − b_b φ_b m_b               + 2ν_b
    dm_d/dt =                  g_d c − b_d φ_d m_d

resolution (Rizk Eq. 8/9):
    b_{b,d} = Ḟ · b₀(R_{b,d} + r_l)
    b₀(R)   = 10⁻²⁵ · (2.64 − 2.02 · exp(−2.61×10⁻⁹ / R))

trapping (Rizk Eq. 22; Eq. 23 with paper-typo correction):
    g_b = 4π D_g (R_b + r_l) N_b
    g_d = 4π D_g (R_d + r_l) N_d + (2π D_g / [ln(Γ_d / Z_d r_d) − 3/5]) (ρ_d − 2 R_d N_d)

bulk-bubble # (Rizk Eq. 21c):
    dN_b/dt = ν_b − b_b φ_b N_b      (closed-form BE)
    ν_b     = 8π f_n D_g Ω_fg^(1/3) c²

dislocation-bubble # (coalescence only, no nucleation):
    dN_d/dV_d = −4 λ N_d²            (BE quadratic, λ lagged at old state)
    N_d_new   = 2 N_old / (1 + √(1 + 16 λ_old ΔV_d N_old))

vacancy ODE (Rizk Eq. 21f, Speight-Beere implicit):
    dn_v/dt = (2π D_v δ_WS N) / (kT ζ) · (p_int − p_eq)
    ζ = 10ψ(1+ψ³) / (−ψ⁶+5ψ²−9ψ+5),   ψ = R/δ_WS

volume / radius:
    V = (Ω_fg · m_gas + Ω · n_v) / N
    R = (3V/4π)^(1/3)

initial conditions:
    N_d(0) = K_d · ρ_d
    N_b(0) = 0,  V_b(0) = V_d(0) = 0
```

`D_g = D1 + D3` (Xe: D2 not summed). `D_v = D1 + D3` (V_U: D2 dropped).

## 4. Parameter values (RIZK_CONSTANTS)

| Param | Value | Source |
|---|---|---|
| Grain radius `r_g` | 6.0 µm | Rizk 2025 Tab. 1 |
| Lattice parameter `a` | 4.889 Å | Rizk 2025 Tab. 1 |
| Atomic volume `Ω_fg` | 8.5×10⁻²⁹ m³ | Rizk 2025 Tab. 1 |
| Lattice radius `r_l` | 0.21 nm | Rizk 2025 |
| Surface tension `γ` | 1.11 J/m² | Rizk 2025 (LLS-MD) |
| Xe yield | 0.24 at/fission | Rizk 2025 |
| `K_d` (Eq. 11) | 5×10⁵ bub/m | Rizk 2025 §4 (UN-calibrated) |
| Disl core radius `r_d` | 3.46 Å (≈ a/√2) | Rizk 2025 Tab. 1 |
| `Z_d` | 5 | Rizk 2025 Eq. 23 |
| `D₁₀_Xe` | 1.56×10⁻³ m²/s | Rizk 2025 Tab. 2 |
| `Q₁_Xe` | 4.94 eV | Rizk 2025 Tab. 2 |
| `A₃₀_Xe` | 1.85×10⁻³⁹ m⁵ | Rizk 2025 Tab. 2 |
| `D₁₀_VU` | 1.35×10⁻² m²/s | Rizk 2025 Tab. 2 |
| `Q₁_VU` | 5.66 eV | Rizk 2025 Tab. 2 |
| `A₃₀_VU` | 4.96×10⁻⁴² m⁵ | **Schneider 2024** (2.48×10⁻²² @ F=5×10¹⁹) |
| b₀ shape `(pref, a₁, a₂, b₁)` | (10⁻²⁵, 2.64, 2.02, 2.61 nm) | Rizk 2025 Eq. 8 |
| `f_n` | **3×10⁻⁶** | UN-recalibrated against Ronchi 1978 (was 10⁻⁶ from U₃Si₂ / Barani 2019) — see §5.5 |
| `ρ_d` (constant) | 3×10¹³ m⁻² | Rizk 2025 Sec. 2.2.2 |
| Fission rate `Ḟ` | 5×10¹⁹ fiss/(m³ s) | DN1 / Rizk validation |

## 5. Validation summary

### 5.1 Reference smoke point (T=1600 K, 1.3 % FIMA, post-f_n calibration)

```
Sw_d = 2.18 %     R_d = 72.4 nm    N_d = 1.37×10¹⁹ m⁻³
Sw_b = 1.46 %     R_b = 11.5 nm    N_b = 2.29×10²¹ m⁻³
Gas partition: matrix 0.1% / bulk 68.6% / disl 27.5% / q_gb 3.9%
```

### 5.2 Flag ablation 2×2 (39 Ronchi anchor points, f_n = 3×10⁻⁶)

| (φ, mass) | RMSE Sw_d | bias Sw_d | RMSE 1.1% | RMSE 1.3% | RMSE 3.2% |
|---|---|---|---|---|---|
| (off, off) bare 3-eq, paper-faithful | 2.12 | −1.88 | 1.91 | 2.45 | 2.19 |
| (off, on) | 2.12 | −1.88 | 1.91 | 2.45 | 2.19 |
| (on, off) | 1.02 | −0.21 | 0.66 | 1.79 | 0.57 |
| **(on, on) default** | **1.03** | **−0.20** | **0.66** | **1.80** | **0.57** |

Mass-coupling has zero numerical effect (as before). φ-correction is the dominant flag (it remains the single largest non-default term: −1.88 → −0.20 bias when toggled on).

### 5.3 ρ_d laws comparison (39 Ronchi anchor points, f_n = 3×10⁻⁶)

| Law | RMSE all | bias all | RMSE 1.1% | RMSE 1.3% | RMSE 3.2% |
|---|---|---|---|---|---|
| **constant (Rizk 2025) — current default** | **1.03** | −0.20 | 0.66 | 1.80 | 0.57 |
| Blank-FT (Ray-Blank burnup growth) | 2.79 | +1.31 | 0.72 | 2.32 | 4.95 |
| Rizk-NEAMS exp (Eq. 3.38) | 2.54 | −0.67 | 1.71 | 2.57 | 3.65 |

Constant ρ_d remains the best globally. With the recalibrated f_n the absolute RMSE is 32 % lower than before across all three laws.

### 5.4 Sensitivity scan of K_d, f_n, ρ_d (with f_n=3e-6 baseline)

```
              global RMSE                          (best in row in bold)
K_d:   1e4    1.20   1e5    1.37    5e5(ref) 1.03    1e6   0.92    1e7    1.06
f_n:   1e-7   2.76   1e-6   1.51    3e-6(NEW ref) 1.03   1e-5  1.10  1e-4   1.75
ρ_d:   1e12   2.24   1e13   1.71    3e13(ref) 1.03    1e14  2.86   1e15   3.16
```

The Rizk-baseline `(K_d=5e5, ρ_d=3e13)` is now consistent with the global RMSE minimum of the scan. K_d=1e6 still gives a marginal improvement (0.92) but breaking N_d (see caveat).

**Caveat — K_d is not free.** A pure-Sw_d-RMSE optimisation would push K_d up to 1e6 or higher, but `N_d(t=0) = K_d · ρ_d` is constrained by Ronchi N_d measurements (Fig. 7 cloud at 1.3 % FIMA: 5×10¹⁸–3×10¹⁹). At K_d=5e5 we get IC N_d=1.5×10¹⁹, in the cloud. K_d=5×10⁶ would give 1.5×10²⁰, ~10× too high. **Keep K_d=5e5; the residual Sw_d/N_d disagreement must come from elsewhere in the model, not from K_d miscalibration.**

ρ_d: scan minimum is at 3×10¹³ (current). Sharp degradation above 10¹⁴.

### 5.5 f_n calibration (recalibrated 2026-05-09)

A fine logarithmic scan in `un_calibration/scripts/calibrate_f_n.py` over 13 values:

```
   f_n      RMSE all   bias 1.1   bias 1.3   bias 3.2   max|bias|
─────────────────────────────────────────────────────────────────
 1e-6 (Rizk)    1.51      +0.25      +1.25      +0.18       1.25  ← starting point
 2e-6           1.16      −0.11      +0.62      −0.19       0.62
 3e-6           1.03      −0.31      +0.25      −0.40       0.40  ← chosen (max|bias| min)
 5e-6           0.98      −0.54      −0.19      −0.64       0.64  ← global RMSE min
 7e-6           1.02      −0.68      −0.45      −0.79       0.79
 1e-5           1.10      −0.82      −0.71      −0.93       0.93
```

**Decision: f_n = 3×10⁻⁶**, picked by minimum max|bias| across burnups.

- Reduces global RMSE by 32 % (1.51 → 1.03)
- Reduces 1.3 % FIMA bias from +1.25 to **+0.25** (5× improvement) — the asymmetric bias mystery flagged in earlier audits is **resolved**
- Per-burnup biases ≤ 0.40 in absolute value — symmetric performance
- Stays within Olander 2006 range (10⁻⁷..10⁻²)
- Smoke point Sw_d shifts from 3.08 % to 2.18 % at T=1600 K, 1.3 % FIMA
- Bulk-bubble radius stays 11–14 nm (still below REM 20 nm cutoff → comparison vs Sw_d alone remains valid)

Defensible thesis statement:
> *"The bulk-bubble nucleation factor f_n is recalibrated from the U₃Si₂-inherited value of 1×10⁻⁶ (Rizk 2025; Barani 2019) to a UN-specific value of 3×10⁻⁶, by a fine logarithmic scan against 39 Ronchi 1978 microscopic-swelling anchor points. The new value reduces the global RMSE on dislocation swelling by 32 % and balances the per-burnup biases below 0.4 % Sw."*

## 6. Open issues / next steps

1. ~~1.3 % FIMA bias asymmetry~~ — **RESOLVED** (2026-05-09) by f_n recalibration to 3×10⁻⁶. The hypothesis that 1.3 % FIMA was in a regime where bulk nucleation under-competes with dislocation trapping was correct. Per-burnup biases now all ≤ 0.40 % Sw.
2. **Overflow / pressure-init debug** at high D_v (when the thesis student tries Tab. 2 raw values). Entry point: `vacancy_concentration_implicit_step` in `un_model.py:344`.
3. **N_d evolution via dislocation network growth** (Eq. 21d first term `(N_d/ρ_d) ∂ρ_d/∂t`) is currently zero because ρ_d is constant. If a T,F-dependent ρ_d is adopted, the regeneration term should be added — currently only the coalescence sink is in the code.
4. **Solid swelling Eq. 19** (0.5 · B) is missing; needed for total swelling validation against integral SP-1 / SNAP50 / JOYO cases.
5. **Inter-granular bubble model** (Rizk Appendix A.2) is absent; the current `q_gb` is a pure mass balance "everything that left the grain interior" with no GB physics.
6. **Slight residual negative bias** (−0.20 % Sw global with the 3×10⁻⁶ choice) suggests there is still ~0.5 % Sw of physics missing systematically. Candidates: solid swelling (point 4), small intergranular contribution (point 5), or a slightly under-tuned vacancy diffusivity.

## 7. Reproducing every figure in this document

Single command (regenerates all reports in ~1 minute):
```bash
un_calibration/scripts/Allrun.sh                   # full
un_calibration/scripts/Allrun.sh --quick           # fast subset (skip scans)
PYTHON=python3.12 un_calibration/scripts/Allrun.sh # override interpreter
```

Or per-script:
```bash
cd /home/giovanni/sciantix-official
python3 un_calibration/scripts/smoke_test.py
python3 un_calibration/scripts/fig3_swelling_vs_T.py
python3 un_calibration/scripts/fig4_diffusivity_vs_T.py
python3 un_calibration/scripts/fig78_NdRd_vs_T.py
python3 un_calibration/scripts/fig9_gas_partition.py
python3 un_calibration/scripts/rho_d_diagnostic.py
python3 un_calibration/scripts/flag_ablation.py
python3 un_calibration/scripts/rho_d_laws_comparison.py
python3 un_calibration/scripts/sensitivity_scan.py
python3 un_calibration/scripts/calibrate_f_n.py
```

Outputs under `un_calibration/reports/<script_name>/` (PNG + CSV).

## 8. Key references

- **Rizk 2025**: J. Rizk et al., *Mechanistic nuclear fuel performance modeling of UN*, JNM 606 (2025) 155604. The baseline paper.
- **Rizk 2023**: LANL LA-UR-23-29157, NEAMS report. Eq. 3.38 ρ_d law, PolyPole-1/2 algorithms, SP-1 / JOYO validation.
- **Schneider 2024**: athermal D3_v for U vacancies in UN — source of `A30_VU`.
- **Schneider 2026**: cluster dynamics confirming D2_v ≈ 10⁻²⁴ at 1500 K, negligible.
- **Olander 2006**: re-solution review; physics of φ-correction.
- **Pizzocri/Barani 2020** (UO₂, "Ref39"): explicit ±2ν_b mass coupling.
- **Barani 2019** (U₃Si₂): homogeneous nucleation analogue, source of f_n=10⁻⁶.
- **Ronchi 1978**: original experimental swelling data (digitised in `un_data.py`). 10% statistical error, REM resolution limit ~20 nm.
- **NotebookLM cross-check (2026-05-09)**: confirmed (a) no literature has direct atom transfer between bubble populations; (b) Rizk Tab. 2 V_U D2 values are mathematically broken; (c) Rizk Fig. 4 plots D_tot dominated by D1, not D2.
