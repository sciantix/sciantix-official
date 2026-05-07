# UN calibration — Python sandbox tracker

Companion to `sciantix_summary_of_mods.md`. The C++ engine work is in `include/`
and `src/`; this file tracks the standalone-Python sandbox.

- **Branch**: `chore/un-model-fast-demo`
- **Last updated**: 2026-05-07

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
    │   ├── un_model.py           ← capture_only physics, stdlib only
    │   └── un_data.py            ← Rizk experimental anchors
    ├── notebooks/
    │   └── UN_clean.ipynb        ← single curated notebook
    └── reports/
        └── un_physics_notes.md    ← consolidated thesis-side physics notes
```

`un_calibration/` total: ~890 KB. Only two Python files (`un_model.py`,
`un_data.py`) and one notebook (`UN_clean.ipynb`).

---

## 2 — `un_model.py`

Pure-physics module, stdlib-only (`math`, `dataclasses`, `typing`). Provides:

- `Candidate` (frozen dataclass — free fit parameters + scaling factors)
- `UNParameters` (mutable dataclass — every literature constant as a field)
- `solve_UN(p, keep_history)` — the capture_only ODE solver
- helpers: diffusivity, resolution, trapping, nucleation, coalescence,
  pressure, vacancy ODE, spectral 3-equation gas balance

Physics flags (module-level):
`USE_PHI_GAS_RESOLUTION = USE_NUCLEATION_MASS_COUPLING = USE_BULK_DISLOCATION_CAPTURE = True`.

---

## 3 — `UN_clean.ipynb`

Single self-contained notebook. Every Rizk literature constant lives in
**`RIZK_CONSTANTS`** (Section 1); free-fit parameters in **`MANUAL_PARAMS`**.
Two optional dislocation-density laws available, mutually exclusive:

- **`USE_RHO_FT = True`** → Blank-saturating temperature shape × Ray-Blank
  burnup growth (parameters in `RHO_FT_PARAMS`).
- **`USE_RHO_EXP = True`** → Rizk NEAMS 2023 exponential law
  (Eq. 3.38, Table 3.3 calibrated for UN; parameters in `RHO_EXP_PARAMS`).
  This matches Rizk's published BISON implementation.
- Both False → constant `rho_d = MANUAL_PARAMS['rho_d']` (Rizk 2025 paper choice, Sec. 2.2.2).

**Sections**:

1. Imports + RIZK_CONSTANTS + RHO_FT_PARAMS + RHO_EXP_PARAMS + MANUAL_PARAMS + Candidate build + local model runner
2. Equations solved (LaTeX, capture_only)
3. Smoke test at T = 1600 K, 1.3 % FIMA
4. Swelling vs T at 1.1 / 1.3 / 3.2 % FIMA (Rizk Fig. 3)
5. ρ_d(F, T) law diagnostics (Blank Table 3 anchors + 2-panel diagnostic + side-by-side 3D surfaces in MWd/kgHM)
6. R_d, N_d vs T at 1.3 % FIMA (Rizk Fig. 7 / 8)
7. Gas partition vs T (Rizk Fig. 9 style)

The local `model_runner` builds `m.UNParameters` directly from `RIZK_CONSTANTS`
and monkey-patches `m.b0_resolution` so the b0(R) coefficients are also editable
from the notebook — no silent module-level defaults.

---

## 4 — Default Rizk-nominal output

With `MANUAL_PARAMS` at all-1.0 scales, `f_n = 1×10⁻⁶`, `K_d = 10⁶` (Rizk's
stated value), `ρ_d = 3×10¹³` constant (per Rizk 2025 Sec. 2.2.2), and both
`USE_RHO_FT` and `USE_RHO_EXP` set to False:

| Quantity | Value |
|---|---:|
| N_d (1400 K, 1.3 % FIMA) | 2.90×10¹⁹ m⁻³ |
| N_d (2000 K, 1.3 % FIMA) | 8.46×10¹⁸ m⁻³ |
| R_d (2000 K, 1.3 % FIMA) | 186 nm |
| swelling_d (1600 K, 1.3 %) | 2.88 % |
| q_gb (1.1 % FIMA, 2000 K) | 6.15 % |

These match Rizk's experimental cloud (N_d ~ 1–5 × 10¹⁹ at 1.3 % FIMA in the
1100–1700 K range; R_d ~ 50–170 nm).

---

## 5 — Regenerating the notebook

```bash
cd /home/giovanni/sciantix-official
python3 tools/build_un_clean_notebook.py
python3 -m jupyter nbconvert --to notebook --execute \
    un_calibration/notebooks/UN_clean.ipynb --output UN_clean.ipynb
```

---

## 6 — Out of scope here

C++ engine work is tracked in `context/sciantix_summary_of_mods.md`.
Older session history is preserved in the git log (use `git log --follow`
on the relevant files to see the v8/v14 calibration era and its purge).
