# UN calibration — Python sandbox tracker

Companion to `sciantix_summary_of_mods.md`. Engine work is in `include/` and `src/`; this file tracks the standalone-Python sandbox.

- **Branch**: `development/nitride`
- **Last updated**: 2026-05-06 (after repo reorganisation, v1..v13b purge, and UN_clean.ipynb consolidation)

---

## 1 — Repo layout (current, post-reorg)

```
sciantix-official/
├── include/, src/, regression/, docs/, references/   ← C++ engine (untouched)
├── context/
│   ├── CONTEXT.md                          (thesis-side state file)
│   ├── AI_UN_calibration_instructions.md   (Codex-style instructions)
│   ├── sciantix_summary_of_mods.md         (engine delta tracker)
│   ├── python_sandbox_summary.md           (THIS file)
│   └── SESSION_NOTES.md                    (latest session log)
├── tools/                                  ← migration / build helpers
│   ├── build_un_clean_notebook.py          (regenerate UN_clean.ipynb)
│   ├── fix_paths.py                        (one-shot: applied during reorg)
│   ├── fix_future_imports.py               (one-shot: applied during reorg)
│   └── refactor_notebooks.py               (one-shot: stripped duplicated model code)
└── un_calibration/                         ← all student calibration work
    ├── _pathsetup.py                       (sys.path + chdir helper, imported by every script)
    ├── model/
    │   ├── un_model.py                     (reference scalar Python model — 718 lines)
    │   ├── un_model_fast.py                (vectorised twin — np.linalg.solve on (n_modes,3,3))
    │   ├── un_model_parity.py              (regression test, max diff ~1e-14)
    │   ├── un_data.py                      (Rizk experimental anchors)
    │   └── calibrate_un.py
    ├── optuna/                             (only v14 line + its dependencies remain)
    │   ├── UN_M7_optuna_calibration_v8.py
    │   ├── UN_M7_optuna_calibration_v8_core.py        (model engine + Candidate dataclass)
    │   └── UN_M7_optuna_calibration_v14_rhoSat_qgbStrict_NdAnchors_STANDALONE.py
    ├── notebooks/
    │   ├── UN_clean.ipynb                  ← MAIN CURATED NOTEBOOK (8 sections, 5 plots)
    │   ├── UNmodel.ipynb                   (legacy, has cached PNGs)
    │   ├── UNpython_tests.ipynb            (legacy, kitchen-sink iterations)
    │   ├── 2UNpython_tests.ipynb           (legacy, model-variants comparison)
    │   ├── UN_Barani_model.ipynb           (legacy, sensitivity sweeps)
    │   └── b_g_nu_comparison.ipynb         (legacy, parametric (b,g,ν) study)
    ├── runners/
    │   ├── run_v14_rhoSat_qgbStrict_NdAnchors_block.sh
    │   └── run_sweep_parallel.py           (joblib demo over un_model_fast)
    ├── logs/
    │   └── v14_rhoSat_qgbStrict_NdAnchors_run.log
    ├── results/
    │   ├── UN_M7_optuna_v14_rhoSat_qgbStrict_NdAnchors_results/   (active)
    │   └── UN_M7_global_sensitivity_fullcell/                    (legacy, pre-existing)
    └── reports/
        ├── parameter_audit.md              ← FULL LITERATURE AUDIT (Rizk, Olander, Blank)
        ├── UN_M7_calibration_lessons_report.md
        ├── UN_Thesis_M7_context_for_new_chat.md
        ├── UN_model_variants_report_thesis.md
        ├── UN_sensitivity_analysis_summary.md
        ├── UNcoalescence_comparison.md
        ├── UNcode.md
        ├── UNmodel.md
        ├── b_g_nu_comparison.md
        └── deep-research-report.md
```

Disk usage: `un_calibration/` total = ~15 MB (was 300 MB+ before purge).

---

## 2 — What changed during the audit / cleanup session (2026-05-06)

### Reorganisation
- All student `.py` scripts, notebooks, results, logs, reports moved from repo root into `un_calibration/<subfolder>/`. Engine (`include/`, `src/`) untouched.
- `tools/fix_paths.py` patched every `.py` in `un_calibration/` with a `_pathsetup` import (adds sibling subfolders to `sys.path` and chdirs to `un_calibration/`). Hardcoded result-folder string literals rewritten to `results/...`.

### Aggressive purge (v1 → v13b removed)
**Deleted scripts**: `UN_M7_optuna_calibration.py` (v1), `..._v2.py` … `..._v13b_*.py`, `..._v6_core.py`, `..._v7_core.py`, `..._v8_rhoFT.py` (18 files).
**Deleted result trees**: every `UN_M7_optuna_v{1..13b}_*_results/` (16 trees).
**Deleted logs**: `v8..v13b *.log` (7 files).
**Deleted runners**: `run_v12.sh`, `run_v13.sh`, `run_v13b.sh`.
**Deleted entire**: `un_calibration/codex/` and `un_calibration/diagnostics/` folders (their scripts imported the now-deleted v1/v5/v6 modules).
**Deleted obsolete reports**: `CODEX_*_TASK.md`, `UN_M7_codex_report.md`, `UN_M7_v5_codex_report.md`, `UN_M7_v6_capture_only_physics_notes.md`.

### Kept
- v14 calibrator + its v8 / v8_core dependencies (so v14 can still be re-run)
- v14 result tree + log + runner shell wrapper
- All thesis-level reports in `un_calibration/reports/`
- Notebooks (with cached outputs preserved)

### Additions
- **`un_calibration/model/un_model_fast.py`** — NumPy-vectorised twin of `un_model.py`. Inner spectral mode loop replaced by `np.linalg.solve` on `(n_modes, 3, 3)`. Per-point speedup ~1.65×.
- **`un_calibration/runners/run_sweep_parallel.py`** — joblib demo. ~4× wall-clock speedup over 144-point grid (combined with vectorisation: ~6× end-to-end vs original).
- **`un_calibration/notebooks/UN_clean.ipynb`** — main curated notebook with 8 sections, 5 plots, working with manual-params toggle (see §4 below).
- **`un_calibration/reports/parameter_audit.md`** — literature audit cross-checking every constant in the codebase against Rizk 2025, Olander 2006, Blank 1984. Includes v15 search-space recommendations.
- **`tools/build_un_clean_notebook.py`** — programmatic regeneration of `UN_clean.ipynb` (run after edits to the build script).

### Notebook refactor (`tools/refactor_notebooks.py`)
- 12 cells across 3 notebooks (`UNmodel`, `UNpython_tests`, `UN_Barani_model`) had bit-identical copies of `un_model.py` functions; those were removed and replaced with `from un_model import *` + `from un_model_fast import run_model_point, solve_UN_fast, clear_run_cache`.
- 99 function/class definitions removed across the three notebooks.
- Cell-specific helpers and tweaked variants preserved.
- Cached PNG outputs cleared on touched cells (cells need re-run after refactor).

### v15 attempt (rolled back)
- A v15 calibrator was prototyped with widened K_d range `[10⁶, 5×10⁷]`, locked theoretical scaling factors, and a thermal sigmoid gate on coalescence (`T_coal_onset`, `T_coal_width`).
- 50-trial study completed; rank-1 fitted `T_coal_onset = 1433 K` (within ~70 K of Rizk's stated 1500 K transition) and `K_d = 1.02×10⁶` (within Blank 1984's measured range).
- The student didn't like the resulting curves (high-T N_d crash was less dramatic than v14's), so the v15 calibrator, results, log, and v8_core gate edits were **reverted**.
- `parameter_audit.md` still documents the v15 search-space recommendations as a future option.

---

## 3 — Active parameter set: v14 rank-1

Located at `un_calibration/results/UN_M7_optuna_v14_rhoSat_qgbStrict_NdAnchors_results/capture_only/optuna_final_top_capture_only.csv`.

Top candidate: **`FINAL_rank1_capture_only_capture_only_v13_trial_00066`**, score = 2.351.

| Parameter | Value | Notes |
|---|---:|---|
| `f_n` | 1.81×10⁻⁷ | Low end of Olander's [10⁻⁷, 10⁻²] range |
| `K_d` | 3.53×10⁵ | **Pinned at lower bound** of v14 search [3×10⁵, 8×10⁵] |
| `rho_d` (base) | 3.0×10¹³ m⁻² | = `RHO_FAB`, Rizk + Blank floor |
| `fission_rate` | 7.03×10¹⁹ | s/(m³·s) |
| `rho_scale` | 0.577 | Multiplier on saturating Ray-Blank shape |
| `gb_scale` | 1.652 | Theoretical formula (Rizk Eq. 22), no literature basis to deviate |
| `gd_bubble_scale` | 0.094 | Theoretical formula (Rizk Eq. 23), pinned near zero |
| `gd_line_scale` | 5.457 | Theoretical formula (Rizk Eq. 23), pinned high |
| `b_bulk_scale` | 0.243 | Theoretical formula (Rizk Eq. 8), pinned low |
| `b_dislocation_scale` | 0.115 | Theoretical formula (Rizk Eq. 8), pinned low |
| `coalescence_d_scale` | 0.984 | Approximate hard-sphere formula (Rizk Eq. 15) |
| `capture_scale` | 0.247 | Barani-like UO₂ inheritance |

**Predicted at T=1600 K, 1.3 % FIMA**:
- swelling_d = 2.87 % (model 3.29 % at FAST settings — close)
- R_d = 100.7 nm
- N_d = 6.71×10¹⁸ m⁻³

**Known issues** (see `parameter_audit.md` for full discussion):
1. **Underdetermination**: 13 free scaling factors compete for ~7 data shapes. 5+ factors deviate sharply from 1.0 with no literature basis.
2. **Low-T N_d gap**: model can't reach Rizk's experimental ~5×10¹⁹ at 1153 K because `K_d × ρ_d_eff ≈ 4.9×10¹⁸` is the structural ceiling.
3. **K_d pinned at lower bound** despite Blank 1984 measuring K up to 1.9×10⁷.

The student also pointed to a 500-trial run not in this repo (trial 313, score 1.751) with `rho_scale = 1.635`, `coalescence_d_scale = 1.81`, `capture_scale = 0.065`. The full parameter set for that trial isn't here — to plug it in, see §4.

---

## 4 — Working with `UN_clean.ipynb`

The main curated notebook. Two toggles in section 1 control the candidate source:

```python
USE_MANUAL_PARAMS = False   # False = load v14 rank-1 from CSV
                            # True = use the MANUAL_PARAMS dict below

USE_RAW_RHO_D    = False    # Only meaningful when USE_MANUAL_PARAMS=True
                            # False = run through v14.run_model_point_rhoFT2 (rhoSat law)
                            # True  = run through m.run_model_point (constant cand.rho_d)
```

| `USE_MANUAL_PARAMS` | `USE_RAW_RHO_D` | Effect |
|:---:|:---:|---|
| False | (any) | CSV-loaded v14 rank-1, rhoFT2 wrapper |
| True | False | Manual candidate, rhoFT2 wrapper applies `ρ_0(F)·rho_scale·f_sat(T)`; `MANUAL_PARAMS["rho_d"]` ignored |
| True | True | Manual candidate, raw model; `MANUAL_PARAMS["rho_d"]` is the constant ρ_d |

**Notebook sections**:
1. Imports + load v14 candidate (or manual), with quick-look value summary
2. Equations solved (LaTeX, M7/capture_only family)
3. Smoke test at T=1600 K, 1.3 % FIMA
4. Swelling vs T at 1.1 / 1.3 / 3.2 % FIMA (3-panel, student-style labels)
5. Ray & Blank dislocation density vs T at 6.8 a/o (Blank Table 3 fit)
6. ρ_d(F, T) — the rhoSat law applied to the v14 candidate
7. R_d, N_d vs T at 1.3 % FIMA (Rizk Fig. 7 / 8)
8. Gas partition vs T (Rizk Fig. 9 style)
9. v14 best-candidate table (CSV mode only)

To regenerate the notebook from the build script:
```bash
cd /home/giovanni/sciantix-official
python3 tools/build_un_clean_notebook.py
cd un_calibration/notebooks
python3 -m jupyter nbconvert --to notebook --execute UN_clean.ipynb --output UN_clean.ipynb
```

---

## 5 — How to run v14 from scratch (regenerate the study)

```bash
cd un_calibration/runners
bash run_v14_rhoSat_qgbStrict_NdAnchors_block.sh 100   # 100 trials
```

Outputs land in `../results/UN_M7_optuna_v14_rhoSat_qgbStrict_NdAnchors_results/capture_only/`.

---

## 6 — How to use `un_model_fast` for sweeps

From `un_calibration/model/`:
```python
import un_model_fast as model
row = model.run_model_point(T=1600, burnup=1.3, f_n=1e-6)
```

Or the joblib parallel demo:
```bash
cd un_calibration/runners
python3 run_sweep_parallel.py            # 144 points, default 14 cores
python3 run_sweep_parallel.py --n-jobs 4 # cap workers
```

Parity vs the reference scalar `un_model.py` is verified by:
```bash
cd un_calibration/model
python3 un_model_parity.py
```
Max relative diff = ~1×10⁻¹⁴ across the test grid.

**Note**: `un_model_fast` is the **simpler 3-equation baseline** (no M7 capture, no φ-resolution). It's not a drop-in replacement for the v14 STANDALONE — that needs `v8_core.py`'s richer model with all the scaling factors. `un_model_fast` is for development / parametric studies of the basic model.

---

## 7 — Out of scope here

C++ engine work is tracked in `context/sciantix_summary_of_mods.md`. That file is independent of the Python sandbox and was not touched in the audit / cleanup session.

The pre-existing `un_calibration/results/UN_M7_global_sensitivity_fullcell/` was left in place — it's data, not code, and was already on the branch when the session started.
