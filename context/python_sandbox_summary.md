# UN calibration — Python sandbox tracker

Companion to `sciantix_summary_of_mods.md`. The C++ engine work is in `include/`
and `src/`; this file tracks the standalone-Python sandbox.

- **Branch**: `chore/un-model-fast-demo`
- **Last updated**: 2026-05-07 (after Optuna purge — notebook now self-contained)

---

## 1 — Current layout

```
sciantix-official/
├── include/, src/, regression/, docs/, references/   ← C++ engine (untouched)
├── context/                                          ← thesis-side state files
├── tools/
│   └── build_un_clean_notebook.py                    ← regenerates UN_clean.ipynb
└── un_calibration/
    ├── model/
    │   ├── un_M7_model.py        ← M7/capture_only physics, no optuna deps
    │   └── un_data.py            ← Rizk experimental anchors
    ├── notebooks/
    │   └── UN_clean.ipynb        ← single curated notebook
    └── reports/                  ← thesis-side markdown reports (preserved)
```

`un_calibration/` total: ~890 KB (was 300 MB+ before the audit purges; was 15 MB
before the Optuna purge). The previous v8/v14 Optuna scripts, result trees, runners,
logs, and `optuna/` / `runners/` / `results/` / `logs/` folders were deleted.
Only the M7 physics survives, extracted as a clean module.

---

## 2 — `un_M7_model.py`

Pure-physics module extracted from the deleted `UN_M7_optuna_calibration_v8_core.py`.
Stdlib-only imports (`math`, `dataclasses`, `typing`). Provides:

- `Candidate` (frozen dataclass — free fit parameters + scaling factors)
- `UNParameters` (mutable dataclass — every literature constant as a field)
- `solve_UN_M7(p, keep_history)` — the M7 / capture_only ODE solver
- helper functions: diffusivity, resolution, trapping, nucleation, coalescence,
  pressure, vacancy ODE, spectral 3-equation gas balance

Bit-identical to the deleted v8_core at the parity-check point (T=1600 K, 1.3 % FIMA).

Physics flags (module-level): `USE_PHI_GAS_RESOLUTION = USE_NUCLEATION_MASS_COUPLING = USE_BULK_DISLOCATION_CAPTURE = True`.

---

## 3 — `UN_clean.ipynb`

Single self-contained notebook. Every Rizk literature constant lives in
**`RIZK_CONSTANTS`** (Section 1); free-fit parameters in **`MANUAL_PARAMS`**.
Optional saturating + Ray-Blank ρ_d law in **`RHO_FT_PARAMS`** (toggle via
`MANUAL_PARAMS["USE_RHO_FT"]`).

**Sections (20 cells, 11 markdown + 9 code)**:

1. Imports + RIZK_CONSTANTS + RHO_FT_PARAMS + MANUAL_PARAMS + Candidate build + local model runner
2. Equations solved (LaTeX, M7 / capture_only)
3. Smoke test at T = 1600 K, 1.3 % FIMA
4. Swelling vs T at 1.1 / 1.3 / 3.2 % FIMA (Rizk Fig. 3)
5. ρ_d(F, T) law diagnostics (Blank Table 3 fit + 2-panel diagnostic + 3D surface in MWd/kgHM)
6. R_d, N_d vs T at 1.3 % FIMA (Rizk Fig. 7 / 8)
7. Gas partition vs T (Rizk Fig. 9 style)

The local `model_runner` builds `m.UNParameters` directly from `RIZK_CONSTANTS`
and monkey-patches `m.b0_resolution` so the b0(R) coefficients are also editable
from the notebook — no silent module-level defaults.

---

## 4 — Default Rizk-nominal output

With `MANUAL_PARAMS` at all-1.0 scales, `f_n = 1×10⁻⁶`, `K_d = 10⁶` (Rizk's
stated value), `ρ_d = 3×10¹³` constant (per Rizk Sec. 2.2.2), and
`USE_RHO_FT = False`:

| Quantity | Value |
|---|---:|
| N_d (1400 K, 1.3 % FIMA) | 2.90×10¹⁹ m⁻³ |
| N_d (2000 K, 1.3 % FIMA) | 8.46×10¹⁸ m⁻³ |
| R_d (2000 K, 1.3 % FIMA) | 186 nm |
| swelling_d (1600 K, 1.3 %) | 2.88 % |
| q_gb (1.1 % FIMA, 2000 K) | 6.15 % |

These match Rizk's experimental cloud (N_d ~ 1–5 × 10¹⁹ at 1.3 % FIMA in the
1100–1700 K range; R_d ~ 50–170 nm at the same conditions).

---

## 5 — Regenerating the notebook

```bash
cd /home/giovanni/sciantix-official
python3 tools/build_un_clean_notebook.py
python3 -m jupyter nbconvert --to notebook --execute \
    un_calibration/notebooks/UN_clean.ipynb --output UN_clean.ipynb
```

---

## 6 — What was removed (this session)

- `un_calibration/optuna/` — entire folder (v8 + v14 STANDALONE scripts)
- `un_calibration/runners/` — shell drivers and joblib demo
- `un_calibration/results/` — Optuna study CSVs (3 MB)
- `un_calibration/logs/` — Optuna run logs
- `un_calibration/_pathsetup.py` — sys.path helper for the old optuna scripts
- `un_calibration/model/un_model.py`, `un_model_fast.py`, `un_model_parity.py`,
  `calibrate_un.py` — preceded `un_M7_model.py`
- `tools/fix_paths.py`, `fix_future_imports.py`, `refactor_notebooks.py` —
  one-shot migration scripts already applied during the reorg

The only Python files left in `un_calibration/` are `model/un_M7_model.py`
and `model/un_data.py`.

---

## 7 — Out of scope here

C++ engine work is tracked in `context/sciantix_summary_of_mods.md`.
That file is independent of the Python sandbox and was not touched.
