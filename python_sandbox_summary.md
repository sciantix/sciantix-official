# UN calibration — Python sandbox tracker

Companion to `sciantix_summary_of_mods.md`. That file tracks the C++ engine; this one tracks the standalone Python work the student is now iterating on. Source-of-truth files quoted below live at the repo root.

- Branch: `development/nitride`
- Scope: pure-Python UN model + Optuna calibration line `v2 → v14`
- Engine dependency: from `v11` onward the script files carry a `_STANDALONE` suffix and no longer require the SCIANTIX C++ build

---

## 1 — Standalone Python twin

| File | Lines | Role |
|---|---:|---|
| `un_model.py` | 718 | Pure-Python reimplementation of the UN intragranular gas behaviour model: gas-in-solution `c`, bulk bubbles `m_b`, dislocation bubbles `m_d`, capture/re-solution/coalescence terms, vacancy absorption, gas-to-grain-face accumulator `q_gb` |
| `un_data.py` | ~93 | Experimental anchors and Rizk reference values (swelling, R_d, N_d, partition) |
| `calibrate_un.py` | ~930 | Top-level driver wiring `un_model.py` to a parameter set and running point evaluations |
| `UN_M7_optuna_calibration_v8_core.py` | ~2000 | The `m.run_model_point` / `Candidate` / score-helper module that all v8+ calibrators import |

The student's work happens on top of these — the v* calibrators are *score-and-search* wrappers, not separate model copies.

---

## 2 — Calibration evolution v2 → v14 (one-line digest)

Sources: `UN_M7_calibration_lessons_report.md` covers v2/v3 in depth (Italian); the rest is read directly from the script docstrings, `optuna_summary_*.md` files in each `_results/` tree, and the score wiring in each script.

| Version | Theme | Δ vs previous | Standalone? |
|---|---|---|:---:|
| **v2** | First serious capture_only sweep | Codex/PC2 fixed-`D_v` sweep over 7 values × ~100 trials each. Best balanced: `D_v=1.0`, score 1.337. **Failure:** premature `q_gb` loss at 1600 K, bulk fraction too low | no |
| **v3** | + gas-partition & q_gb scoring | Adds explicit gas-partition score and q_gb constraint, harsher pressure term, weak high-T radius guardrail. **Win:** Rizk-Fig.9-like partition. **New failure:** `R_d(2000 K) ~ 2 µm` runaway | no |
| **v4** | + high-T radius hard guard | Explicit `R_d(2000 K) < 800 nm` cap | no |
| **v5** | Radius saturation/flattening | Adds `R_d(1900)` cap (~700 nm), `R_d(post-1800)` Δ cap (~350 nm), 1900→2000 ratio cap (1.35) | no |
| **v6** | rhoFT diagnostic fork | Splits dislocation-radius physics into `v6_core.py` to enable T-dependent ρ_d later. Score unchanged | no |
| **v7** | Stronger Nd / bulk / pressure | Adds three penalty terms: `W_ND_DROP_TARGET=0.55`, `W_BULK_PLATEAU=0.55`, `W_HIGHT_PRESSURE=0.45` | no |
| **v8** | Radius-band consolidation | `W_RADIUS_BAND=0.75` keeps `R_d` in a tight band at high T; consolidates v7 scoring; `v8_rhoFT.py` is a diagnostic toggle | no |
| **v9** | Score-only rebalancing | Pure scoring pivot: data-fit weights up (swelling 1.45, R_d 1.05, N_d 0.95, drop 1.25), structural guards down to ~0.1–0.2 | no |
| **v10** | rhoFT + Rizk-Fig.6 anchors | Diagnostic enables Ray–Blank `ρ_d(F,T)` scaling; soft burnup anchors at 1.1 / 3.2 / 6.0 % FIMA | no |
| **v10c** | rhoFT2 — two-knee scaling | `ρ_low_scale` (≤1300 K) + `ρ_high_scale` (high-T plateau), linear ramp between. q_gb tightened | no |
| **v11** | + Rizk-Fig.8 R_d(T) anchors | Adds 7 anchored points across 1033–1961 K from manually digitised Rizk Fig.8. `W_PARTITION=0.85`, `W_RIZK_FIG8_DISL_RADIUS=0.35` | **yes** |
| **v12** | rhoFT slope variant | Replaces v10c's two-knee scaling with `ρ_scale × ρ_0(F) × [1 + slope·(f_RB(T)−1)]` — single global scale + slope | yes |
| **v13** | rhoSat — saturating shape | Replaces dual/slope with **one** global scale `ρ_scale × ρ_0(F) × f_sat(T)`, where `f_sat(T) = ρ_∞ − (ρ_∞ − ρ_940)·exp(−(T−940)/τ)` (concave, monotone, saturating). Strengthens Fig.8 anchors, weakens point-by-point N_d match, adds explicit high-T N_d drop target | yes |
| **v13b** | + qgbStrict | Same physics; tightens q_gb constraint to make grain-boundary inventory more disciplined | yes |
| **v14** | + NdAnchors (current) | Replaces ratio/log-drop N_d targets with two absolute anchor families: early-T fits to experimental N_d cloud, high-T upper bounds at 1800/1900/2000 K | yes |

**Cross-version trends.** `f_n` drifts down (10⁻⁷ → 10⁻⁸), `K_d` collapses to ~3×10⁵ (the Ray–Blank reference), `ρ_d` hits the 3×10¹³ ceiling (saturation works), and `D_v / D_g / b` get pinned at scale = 1.0 in winning candidates — meaning the search is increasingly constrained, with the partition and rhoSat shape carrying the work.

---

## 3 — v14 deep dive

File: `UN_M7_optuna_calibration_v14_rhoSat_qgbStrict_NdAnchors_STANDALONE.py` (968 lines).
Inherits all v13 / v10c / v8 score terms by import; replaces only the N_d-shape scoring.

### 3.1 ρ_d(F,T) law (rhoSat, inherited from v13)
```
ρ_d(F, T) = ρ_scale · ρ_0(F) · f_sat(T)
ρ_0(F)    = max(ρ_FAB, C₁·(F − F₀))             # Ray–Blank burnup
f_sat(T)  = [ρ_∞ − (ρ_∞ − ρ_940)·exp(−(T−940)/τ)] / f_sat(1025 K)
```
Constants: `ρ_FAB = 3·10¹³ m⁻²`, `C₁ = 1.6·10¹⁴ m⁻²/(a/o)`, `F₀ = 2.4 a/o`, `T_ref = 1025 K`, `ρ_940 = 6.357`, `ρ_∞ = 9.104`, `τ = 203.76 K`. Only `ρ_scale ∈ [0.5, 2.0]` is fitted; the shape is fixed.

### 3.2 New v14 N_d scoring (the actual delta)
Two terms replace the old log-drop ratio:

- `nd_early_exp_score_v14` (weight **1.20**) — point-by-point log-RMSE against experimental N_d for `T ≤ 1507.5 K` only. Anchors the high-density region without coupling to the high-T tail.
- `nd_highT_abs_anchor_score_v14` (weight **1.40**) — log-error penalised only when `N_d` *exceeds* the cap:

| T (K) | max N_d (m⁻³) |
|---:|---:|
| 1800 | 8·10¹⁸ |
| 1900 | 4·10¹⁸ |
| 2000 | 1.5·10¹⁸ |

Rationale (verbatim from the docstring): *"the intention is not to punish whether the drop occurs slightly earlier or later, but to require that by 1800–2000 K the dislocation-bubble concentration has actually decreased."*

### 3.3 q_gb anchors (qgbStrict, inherited from v13b)
Burnup-dependent caps applied at multiple temperatures, plus flatness penalties:

| Burnup | low/mid-T cap | high-T cap |
|---:|---:|---:|
| 1.1 % FIMA | 5 % | 5 % |
| 3.2 % FIMA | 8 % | 12 % |

`W_V13_QGB = 2.50` makes this the single heaviest non-data score term. Flatness term: `(q_gb(T) − q_gb(T_prev) − 8)/25` triggered between 1200/1600 and 1600/2000 K.

### 3.4 Rizk Fig. 8 R_d(T) anchors
7-point subset of the digitised dislocation curve at 1.3 % FIMA, log-RMSE on `R_d(T)` predictions.
- `W_RIZK_FIG8_DISL_RADIUS = 0.75` (strong)
- `W_RIZK_FIG8_BULK_RADIUS = 0.04` (very weak — the bulk shape is largely free)

### 3.5 High-T dislocation-gas plateau anchors
Lower bounds on `dislocation_gas_percent` (so the dislocation channel must dominate at high T):

| Burnup | T (K) | min disl. gas |
|---:|---:|---:|
| 1.1 % FIMA | 1800 / 1900 / 2000 | 55 / 78 / 84 % |
| 3.2 % FIMA | 1800 / 1900 / 2000 | 45 / 70 / 80 % |

Plus monotonicity penalties on `Δd_{1800→1900}`, `Δd_{1900→2000}`.

### 3.6 Full v14 score weight table

| Term | Source | Weight |
|---|---|---:|
| Swelling RMSE | v8 core | `W_SWELLING` |
| R_d direct exp. fit | v8 core | `W_RD` |
| N_d level (point) | v8 core | `W_ND_LEVEL` |
| N_d drop | v8 core | `W_ND_DROP` |
| Pressure | v8 core | `W_PRESSURE` |
| Fission-rate prior | v8 core | `W_FDOT_PRIOR` |
| Rizk parameter prior | v8 (v5) | `W_RIZK_PRIOR` |
| **Gas partition** | v13 | **0.95** |
| **q_gb constraint** | v13/v13b | **2.50** |
| Radius high-T guard | v8 | `W_RADIUS_GUARD` |
| N_d coalescence shape | v8 (v7) | `W_ND_DROP_TARGET` |
| N_d high-T low target | v13 | 0.80 |
| **N_d early experimental fit** | **v14 NEW** | **1.20** |
| **N_d high-T abs. anchor** | **v14 NEW** | **1.40** |
| High-T pressure | v8 (v7) | `W_HIGHT_PRESSURE` |
| Rizk Fig.8 R_d | v11 | 0.75 |
| Rizk Fig.8 R_b | v13 | 0.04 |
| Rizk Fig.6 burnup-S_d | v10 | 0.02 |
| Base high-T guard | core | 0.04 |

### 3.7 v14 winning candidate
From `UN_M7_optuna_v14_rhoSat_qgbStrict_NdAnchors_results/capture_only/optuna_summary_capture_only.md`:

| field | rank-1 value |
|---|---:|
| score | 2.351 |
| `f_n` | 1.81·10⁻⁷ |
| `K_d` | 3.54·10⁵ |
| `ρ_d` | 3.0·10¹³ (ceiling) |
| `Ḟ` | 7.0·10¹⁹ |
| `D_v / D_g / D2_Xe` scales | 1, 1, 1 |
| `b_scale` | 1 |
| `g_b` | 1.65 |
| `g_d` | 1 |
| `coalescence_d` | 0.98 |
| `capture` | 0.25 |
| swelling_d @ 1600 K, 1.3% | 2.87 % |
| `R_d` @ 1600 K | 100.7 nm |
| `N_d` log-drop | −0.12 |

The candidate label is `..._v13_trial_00066`, i.e. v14 is *re-scoring v13's persisted Optuna study* (shared SQLite DB) rather than running a fresh search — v14's role is the new objective, not a new trial run.

---

## 4 — Open items (continuation of the lessons-report v3b plan)

The student's `UN_M7_calibration_lessons_report.md` (dated 2026-05-01) ends at v3 with a plan for v3b that became the `v4..v14` line. Items still open relative to that plan:

- **Parameter audit** still pending: `D_v`, `b_0(R)`, `f_n`, `K_d`, `ρ_d`, `g_d`, coalescence law, capture bulk→dislocation. The lessons report flags these as "verify, not just look up". Several of v14's winning values sit at search-space boundaries (`ρ_d` at ceiling, `f_n` very low) — a sign the *prior* range may be the binding constraint, not physics.
- **`R_d(2000 K)` blow-up:** mitigated structurally by rhoSat + Fig.8 anchors, but the lessons-report's `R_d(2000)/R_d(1800)` ratio guardrail is not in v14's score (only Fig.8 log-error). Worth checking whether v14's winners pass the implicit ratio test.
- **Codex three-level strategy** (nominal-near / wide exploratory / one-family escape) was proposed for v3b but the v4–v14 line is mostly nominal-near with growing constraint count. The wide / family-escape sweeps haven't been re-run on the v14 score.
- **Family choice:** all of v4..v14 have run almost exclusively on `capture_only`. `M7_no_phi`, `M7_full`, and `baseline` haven't been retried under the v14 objective.

---

## 5 — Out of scope (here)
The C++ engine delta is tracked separately in `sciantix_summary_of_mods.md`. Notebooks (`UNmodel.ipynb`, `UNpython_tests.ipynb`, `UN_Barani_model.ipynb`, `b_g_nu_comparison.ipynb`, ...), the diagnostic scripts (`UN_M7_*_diagnostics.py`), the `UN_M7_codex_*` and `UN_M7_v5_*` Codex-driven sweeps, and the run logs are not tracked here. Promote items into this file as they become relevant to the calibration line.
