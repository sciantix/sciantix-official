# Session notes — 2026-05-06

Consolidated log of the audit / reorg / v15-attempt session. Keep this with `python_sandbox_summary.md` as the entry-point handoff for the next chat.

## What this session produced

| Deliverable | Location | Status |
|---|---|---|
| **Repo reorganisation** into `un_calibration/<subfolder>/` | repo root | landed |
| **Path-fix tooling** | `tools/fix_paths.py`, `tools/fix_future_imports.py` | one-shot, applied |
| **Notebook refactor** (strip duplicated model code) | `tools/refactor_notebooks.py` | one-shot, applied |
| **Vectorised model + parallel demo** | `un_calibration/model/un_model_fast.py`, `un_model_parity.py`, `un_calibration/runners/run_sweep_parallel.py` | landed |
| **Aggressive purge** of v1..v13b + codex/diagnostics + orphan results | (deletions only) | landed |
| **Parameter audit** vs Rizk 2025 + Olander 2006 + Blank 1984 | `un_calibration/reports/parameter_audit.md` | landed |
| **Curated reference notebook** with MANUAL_PARAMS toggle | `un_calibration/notebooks/UN_clean.ipynb` (built by `tools/build_un_clean_notebook.py`) | landed |
| **v15 attempt** (widened K_d + thermal coalescence gate) | (deleted) | tried & reverted |

## Headline numbers

- `un_calibration/` disk usage: **300 MB → 15 MB** (after purge)
- Optuna scripts retained: **3** (v8, v8_core, v14) — were 21
- Notebook cells in `UN_clean.ipynb`: **21** (10 markdown + 8 code), 5 PNGs
- `un_model_fast.py` parity vs `un_model.py`: max relative diff = **1.1×10⁻¹⁴**
- joblib speedup over 144-point sweep: **4.0×** on 14 cores
- Combined vectorisation × joblib: **~6×** end-to-end vs original sequential

## What the audit said about v14's calibration

Looking at the v14 rank-1 candidate after cross-referencing every constant against the source papers:

- **Diffusivity constants & equations** in `un_model.py` and `v8_core.py` are exact matches to Rizk Table 2 and Appendix A. Two genuine paper findings:
  1. Rizk Eq. 23 has a dimensional typo — the student spotted it and the code is correct.
  2. Rizk Table 2's `A²⁰_VU = 1.32×10⁻¹⁹` doesn't reproduce Fig. 4; student refit to `4.63×10⁻²⁹` (~10⁻¹⁰× different) — likely a Rizk Table 2 unit/exponent typo.
- **9 of v14's 13 scaling factors should be locked at 1.0** because their underlying formulas (`b₀(R)`, `g_b`, `g_d`, all diffusivities) are pure literature with no UN-specific evidence to deviate.
- **5 free factors are enough** for the data: `f_n`, `K_d`, `rho_scale`, `coalescence_d_scale`, `capture_scale`.
- **`K_d` should be widened to `[10⁶, 5×10⁷]`** — Blank 1984 measured K up to `1.9×10⁷`; v14's `[3×10⁵, 8×10⁵]` is below all measured slopes. This is why the model's low-T N_d structurally undershoots Rizk's experimental cloud.

## What v15 tried and why it was reverted

The student didn't like the v15 output. v15:
- Widened `K_d` per the audit
- Locked the 9 theoretical scales at 1.0
- Added a fitted thermal sigmoid gate on coalescence: `s_coal(T) = 1 / (1 + exp(-(T - T_coal_onset)/T_coal_width))`

50-trial Optuna run produced a rank-1 with:
- `T_coal_onset = 1433 K` (within ~70 K of Rizk's stated 1500 K — independent confirmation)
- `K_d = 1.02×10⁶`, `rho_scale = 0.92`
- N_d at 1153 K: ~3×10¹⁹ (vs v14's 6×10¹⁸ — much closer to Rizk's 5×10¹⁹)
- N_d at 2000 K: stayed at ~2.4×10¹⁹ (didn't crash — this is what the student didn't like)

Reversion path: deleted the v15 STANDALONE script, the v15 result tree, the v15 log, the comparison plot script, and the four edits in `v8_core.py` (Candidate fields, UNParameters fields, sigmoid gate logic, plumbing). Verified v14 rank-1 numbers are unchanged after revert. The audit doc still documents the v15 search-space recommendations for any future attempt.

## Open items for the student

1. **The 500-trial trial 313** the student mentions (score 1.751, `rho_scale=1.635`, `coal=1.81`, `cap=0.065`) is not in this repo. To reproduce in `UN_clean.ipynb`, plug the full parameter row into section 1's `MANUAL_PARAMS` dict (with `USE_MANUAL_PARAMS = True`).
2. Either confirm or refute the suspected Rizk Table 2 typo on `A²⁰_VU` with the paper authors.
3. Decide whether to re-run a v15-style attempt with different scoring (the structural N_d gap is now understood; the missing piece is the high-T crash mechanism).

## How to pick up next session

Read in order:
1. `context/CONTEXT.md` — thesis-side state
2. `context/python_sandbox_summary.md` — full repo layout + active params + how-to
3. `context/SESSION_NOTES.md` — THIS file (chronological session log)
4. `un_calibration/reports/parameter_audit.md` — literature audit + v15 recommendations
5. `un_calibration/reports/UN_M7_calibration_lessons_report.md` — student's own retrospective (Italian)
6. `un_calibration/notebooks/UN_clean.ipynb` — main curated notebook (open in Jupyter to see plots)

To regenerate `UN_clean.ipynb` after any edit to the build script:
```bash
cd /home/giovanni/sciantix-official
python3 tools/build_un_clean_notebook.py
cd un_calibration/notebooks
python3 -m jupyter nbconvert --to notebook --execute UN_clean.ipynb --output UN_clean.ipynb
```

To re-run v14 calibration (~10 min for 100 trials + ~5 min FINAL):
```bash
cd un_calibration/runners
bash run_v14_rhoSat_qgbStrict_NdAnchors_block.sh 100
```
