# CODEX_UN_M7_V5_TASK — Overnight calibration with v5 saturation scoring

## Context

You are working in the repository:

```bash
~/sciantix-official
```

The project is a UN Thesis calibration of a Python 0D/one-grain fission gas model inspired by Rizk 2025 / SIFGRS.

The current important model family is:

```text
capture_only
```

Meaning:
- phi-resolution OFF;
- nucleation mass coupling OFF;
- bulk->dislocation capture ON;
- dislocation-dislocation coalescence ON;
- gas partition/q_gb diagnostics ON;
- pressure diagnostics ON.

The user has two PCs. PC1 will run Codex. Do not overwrite existing PC2/PC1 results.

## Files to read first

Read these files before doing anything substantial:

```text
AI_UN_calibration_instructions_UPDATED.md
UN_M7_optuna_calibration_v5.py
UN_M7_optuna_calibration_v4.py
UN_M7_calibration_lessons_report.md
UN_Thesis_M7_context_for_new_chat.md
UNmodel.md
UNcode.md
UNcoalescence_comparison.md
UN_model_variants_report_thesis.md
```

If some files are missing, continue but document it in `WORKLOG.md`.

## Strict rules

Do not modify:
- notebooks;
- original calibration scripts `UN_M7_optuna_calibration.py`, `UN_M7_optuna_calibration_v2.py`, `UN_M7_optuna_calibration_v3.py`, `UN_M7_optuna_calibration_v4.py`;
- `UN_M7_optuna_calibration_v5.py`, except only if the user explicitly asks.

Do not:
- git commit;
- git push;
- create branches;
- delete previous results;
- overwrite existing PC1/PC2 output folders;
- silently remove pressure, gas partition, q_gb, radius, or saturation scoring;
- silently disable coalescence or bulk->dislocation capture;
- silently change physical equations.

Allowed:
- create Codex-owned files:
  - `UN_M7_v5_codex_runner.py`
  - `UN_M7_v5_codex_analysis.py`
  - `UN_M7_v5_codex_report.md`
  - files under `UN_M7_v5_codex_results/`
- run `UN_M7_optuna_calibration_v5.py` with different command-line weights/output folders;
- create `WORKLOG.md` and update it after every block.

All Codex outputs must go under:

```text
UN_M7_v5_codex_results/
```

## First command

Start with:

```bash
git status --short
```

Record the result in:

```text
UN_M7_v5_codex_results/WORKLOG.md
```

## Scientific goal

Do not just minimize score. Determine whether v5 can find physically defensible candidates satisfying:

```text
- P2/dislocation swelling fit acceptable up to ~1700 K;
- R_d and N_d comparable to the experimental large-bubble data;
- gas partition qualitatively consistent with Rizk Fig. 9;
- q_gb moderate, interpreted as grain-boundary bubbles + fission gas release;
- p_b/p_eq and p_d/p_eq not extreme;
- R_d(2000 K, 1.3% FIMA) sub-micrometric, preferably <= 800-1000 nm;
- R_d(T) starts flattening after about 1800 K, not accelerating to micrometric values.
```

Important lessons from v4:
- top-score candidates still had `Rd(2000K) ~ 1.3-1.6 µm`;
- physical candidates existed:
  - `capture_only_v4_trial_00150`: `Rd(2000K) ~ 785 nm`;
  - `capture_only_v4_trial_00163`: `Rd(2000K) ~ 705 nm`;
  - `capture_only_v4_trial_00170`: `Rd(2000K) ~ 992 nm`;
- these physical candidates still had too little low-T bulk gas and dislocation pressure above equilibrium;
- therefore v5 adds an explicit saturation/flattening score.

## Standard v5 command block

Use a dedicated output directory, for example:

```bash
OUT="UN_M7_v5_codex_results/capture_only_v5_standard"
mkdir -p "$OUT"
```

Pilot:

```bash
python UN_M7_optuna_calibration_v5.py \
  --family capture_only \
  --dv-min 0.2 \
  --dv-max 2.0 \
  --partition-weight 0.8 \
  --qgb-weight 0.7 \
  --pressure-weight 0.7 \
  --pressure-free-factor 2 \
  --rizk-prior-weight 0.2 \
  --radius-guard-weight 0.55 \
  --radius-saturation-weight 0.75 \
  --rd2000-max-nm 800 \
  --rd1800-soft-max-nm 600 \
  --rd1900-soft-max-nm 700 \
  --rd-post1800-delta-max-nm 350 \
  --rd-last-increment-factor-max 0.75 \
  --rd-1900-2000-ratio-max 1.35 \
  --n-trials 300 \
  --n-top-final 0 \
  --no-plots \
  --output-dir "$OUT" \
  2>&1 | tee "$OUT/pilot_300_no_plots.log"
```

Then inspect `optuna_fast_trials_capture_only.csv`.

If the pilot has promising physical candidates, continue in the same folder:

```bash
python UN_M7_optuna_calibration_v5.py \
  --family capture_only \
  --dv-min 0.2 \
  --dv-max 2.0 \
  --partition-weight 0.8 \
  --qgb-weight 0.7 \
  --pressure-weight 0.7 \
  --pressure-free-factor 2 \
  --rizk-prior-weight 0.2 \
  --radius-guard-weight 0.55 \
  --radius-saturation-weight 0.75 \
  --rd2000-max-nm 800 \
  --rd1800-soft-max-nm 600 \
  --rd1900-soft-max-nm 700 \
  --rd-post1800-delta-max-nm 350 \
  --rd-last-increment-factor-max 0.75 \
  --rd-1900-2000-ratio-max 1.35 \
  --n-trials 1000 \
  --n-top-final 0 \
  --no-plots \
  --output-dir "$OUT" \
  2>&1 | tee "$OUT/continuation_1000_no_plots.log"
```

`n-trials 1000` means additional trials if the Optuna database already exists.

## Candidate selection

After each block, do not pick only the best `score_total`.

Create a script or analysis table that ranks:

```text
1. best score_total
2. best physical candidate
3. best radius-saturated candidate
4. best low-qgb candidate
5. best pressure-friendly candidate
6. best Rizk-near candidate
7. best partition candidate
```

A useful physical filter is:

```text
Rd_1p3_2000K <= 1000
Rd_ratio_2000_over_1800 <= 2.8
Rd_inc_1900_2000_nm <= 0.75 * max(Rd_inc_1800_1900_nm, 1)
p_d_over_eq_1p3_1600K <= 2.5
qgb_gas_1p1_1600 <= 12
bulk_gas_1p1_1200 >= 68
bulk_gas_1p1_1500 >= 65
swD_1p3_1600K between roughly 2 and 3.2
```

Also create a stricter version:

```text
Rd_1p3_2000K <= 800
p_d_over_eq_1p3_1600K <= 2.0
bulk_gas_1p1_1200 >= 70
```

If no candidates pass the strict filter, say so. Do not hide failure.

## Final reruns and plots

After pilot + continuation, choose a small set for exact plots:

```text
- top 3 by score_total;
- top 3 by physical filter;
- top 2 radius-saturated;
- top 2 pressure-friendly;
- top 2 Rizk-near;
- any candidate that is qualitatively interesting even if score is not best.
```

Use final settings:

```text
final_dt_h = 1
final_n_modes = 40
plots enabled
```

If the base script only plots top score candidates, create `UN_M7_v5_codex_runner.py` or `UN_M7_v5_codex_analysis.py` to rerun selected candidates from CSV using existing functions. Do not modify original physics.

## Weight adaptation protocol

You may adapt weights only if needed, and only after documenting the reason.

Examples:
- If no candidate can satisfy bulk gas at low T, try a block with slightly stronger `partition-weight` or looser saturation.
- If all promising candidates fail radius saturation, try a block with stronger `radius-saturation-weight`.
- If all candidates become too poor in P2 swelling, try a less severe saturation block.

Every changed-weight block must have its own output folder, for example:

```text
UN_M7_v5_codex_results/capture_only_v5_standard/
UN_M7_v5_codex_results/capture_only_v5_stronger_partition/
UN_M7_v5_codex_results/capture_only_v5_softer_saturation/
```

Never overwrite.

## Escape studies

If the standard run is inconclusive, run structured escape blocks.

Use the same v5 score philosophy, but classify which parameters move.

Suggested blocks:

```text
nominal_near:
  rizk_prior_weight = 0.5 or 0.8
  n_trials = 500

wide_standard:
  rizk_prior_weight = 0.1
  n_trials = 1000

strong_saturation:
  radius_saturation_weight = 1.2
  n_trials = 500

strong_partition:
  partition_weight = 1.1
  qgb_weight = 0.8
  n_trials = 500
```

If you need true one/two-parameter range escape studies, create a Codex-owned wrapper that monkey-patches `optuna_candidate_from_trial`, but do not modify the original v5 file.

## Required report

Create:

```text
UN_M7_v5_codex_report.md
UN_M7_v5_codex_results/best_candidates_all_categories.csv
UN_M7_v5_codex_results/physical_filtered_candidates.csv
UN_M7_v5_codex_results/score_component_summary.csv
UN_M7_v5_codex_results/escape_study_summary.csv
```

The report must answer:

1. Did v5 find a candidate with acceptable P2 fit, gas partition, pressure, and radius saturation?
2. Is the best-score candidate physically worse than another filtered candidate?
3. Which candidates are better than v4 `trial_00150`/`trial_00163`/`trial_00170`?
4. Does the saturation constraint conflict with P2 swelling?
5. Does low-temperature bulk gas remain too low?
6. Does dislocation pressure remain above equilibrium?
7. Which parameter groups are consistently driven away from Rizk?
8. Is the limiting issue likely:
   - missing grain-boundary/FGR model;
   - coalescence;
   - capture;
   - trapping/re-solution;
   - vacancy diffusivity;
   - single-size model limitation?
9. What should be tried next?

End by running:

```bash
git status --short
```

Report whether tracked files were modified. They should not be, except for newly created Codex-owned files if intentionally staged/untracked.
