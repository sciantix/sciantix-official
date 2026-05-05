# CODEX_UN_M7_V5_TASK_v2 — structured overnight calibration

You are working on the UN Thesis project in:

```text
~/sciantix-official
```

Read first:

```text
AI_UN_calibration_instructions.md
UN_M7_optuna_calibration_v5.py
UNmodel.md
UN_M7_calibration_lessons_report.md
UN_M7_codex_report.md, if present
```

Then follow this task.

---

## 0. Main scientific goal

We are calibrating the UN fission gas model using `capture_only` with v5 scoring.

The key question is not only:

```text
Can Optuna minimize score_total?
```

but:

```text
Can the model simultaneously reproduce P2 / dislocation swelling,
R_d, N_d, gas partition, pressure, and a physically acceptable
high-temperature R_d saturation/flattening?
```

Previous results:

```text
v3:
  improved gas partition,
  but R_d(2000 K) exploded to about 2 micrometers.

v4:
  reduced R_d blow-up,
  but top score candidates still had R_d(2000 K) ≈ 1.3–1.6 micrometers.
  More physical candidates trial_00150, 00163, 00170 had submicrometric
  or near-submicrometric R_d, but R_d(T) still did not clearly saturate
  and bulk gas at low/intermediate temperature was too low.

v5:
  adds radius saturation / flattening score.
```

Do not hide a failure by overfitting. If no candidate satisfies the physical constraints, say so clearly.

---

## 1. Strict rules

Do not:

```text
modify notebooks
modify original calibration scripts in place
modify UN_M7_optuna_calibration_v5.py unless explicitly required and documented
commit
push
create branch
delete or overwrite existing results
reuse PC2 output folders
silently disable pressure, q_gb, partition, or radius scoring
silently disable coalescence
silently disable capture in capture_only
```

Allowed Codex-owned files:

```text
UN_M7_v5_codex_runner.py
UN_M7_v5_codex_analysis.py
UN_M7_v5_codex_report.md
UN_M7_v5_codex_results/**
```

All outputs must go under:

```text
UN_M7_v5_codex_results/
```

Create and update:

```text
UN_M7_v5_codex_results/WORKLOG.md
```

Update `WORKLOG.md`:

```text
after every block
before changing score weights/ranges
before final reruns
if context gets low
```

Start by running:

```bash
git status --short
```

At the end, run it again and report changes.

---

## 2. Important methodology

Do not run one huge blind Optuna study.

Run **separate blocks**, analyze after each block, then decide the next block.

Reason:

```text
A single continuous Optuna run may over-exploit a non-physical basin.
We want to know which parameter families actually fix the model.
```

Therefore, use separate output directories / studies for each block.

Each block must produce:

```text
block_summary.csv
top_candidates.csv
score_component_summary.csv
short markdown note in WORKLOG.md
```

The note must say:

```text
what improved
what got worse
whether the best candidates are physical or only numerical
which parameter directions Optuna pushed
whether the next block should continue or change strategy
```

---

## 3. Candidate classification

Never select only by `score_total`.

For every block classify candidates as:

```text
best_score_total
best_physical
best_radius_saturated
best_partition
best_pressure
best_rizk_near
best_low_bulk_qgb
best_low_parameter_drift
rejected_by_radius
rejected_by_pressure
rejected_by_partition
rejected_by_absurd_parameters
```

A physically interesting candidate should preferably satisfy:

```text
R_d(2000 K, 1.3% FIMA) <= 800–1000 nm
R_d(T) after 1800 K should flatten or at least decelerate
R_d(1900→2000) increment should be smaller than or comparable to R_d(1800→1900)
p_d/p_eq around 1200–1800 K should not be extreme
bulk gas below about 1600 K should not be too low
q_gb should not dominate at low/intermediate temperature
P2 swelling, R_d, and N_d should remain reasonable
```

Do not reject a candidate solely because it is not the lowest score.

---

## 4. Base v5 command template

For a block, use commands equivalent to:

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
  --n-trials <N> \
  --n-top-final 0 \
  --no-plots \
  --output-dir <BLOCK_OUTPUT_DIR>
```

Final reruns only after selection:

```bash
python UN_M7_optuna_calibration_v5.py \
  ...same settings... \
  --n-trials 0 \
  --n-top-final <M> \
  --output-dir <BLOCK_OUTPUT_DIR>
```

If the base v5 CLI is not enough to implement a block-specific parameter range, create `UN_M7_v5_codex_runner.py` that imports/reuses the v5/base functions and changes only the Optuna sampling ranges for that block. Do not modify original scripts in place.

---

## 5. Overnight block plan — about 3000 trials total

The total target is approximately:

```text
3000 fast Optuna trials + final reruns
```

But they must be split into blocks.

### Block A — pilot nominal-near, 300 trials

Purpose:

```text
Check whether v5 scoring works in a Rizk-near region.
```

Use relatively conservative ranges. If v5 code cannot set all ranges from CLI, create a Codex-owned runner.

Suggested ranges:

```text
Dv_scale             0.5–1.5
Dg_scale             0.5–2.0
D2_xe_scale          0.3–3.0
b_scale              0.3–3.0
gb_scale             0.3–3.0
gd_scale             0.5–3.0
coalescence_d_scale  0.5–5.0
capture_scale        0.05–1.5
f_n                  1e-9–1e-6
K_d                  2e5–8e5
rho_d                1e13–6e13
```

Do not continue if every top candidate violates radius badly; analyze why.

### Block B — standard v5 wide, 700 trials

Purpose:

```text
Main v5 exploration with the default wider v5 ranges.
```

Use the base v5 range if no custom runner is needed.

This block should not blindly continue Block A’s database. Use a separate study.

### Block C — one-parameter-family escape studies, about 1000 trials total

Run separate sub-blocks, around 200 trials each.

Do not widen everything. In each sub-block, only one family is wide, others stay near nominal/moderate.

#### C1 — diffusion escape

Wide:

```text
Dv_scale, Dg_scale, D2_xe_scale
```

Keep other parameters moderate.

Purpose:

```text
Check whether wrong diffusivity scale can fix partition/radius/pressure.
```

#### C2 — resolution escape

Wide:

```text
b_scale
```

Optionally include `D2_xe_scale` moderate.

Purpose:

```text
Check whether re-solution scale controls pressure and gas partition.
```

#### C3 — trapping escape

Wide:

```text
gb_scale, gd_scale
```

Purpose:

```text
Check whether too-early gas transfer to dislocation is caused by trapping.
```

#### C4 — microstructure escape

Wide:

```text
f_n, K_d, rho_d
```

Purpose:

```text
Check whether the initial/dislocation bubble density explains R_d and N_d.
```

#### C5 — coalescence/capture escape

Wide:

```text
coalescence_d_scale, capture_scale
```

Purpose:

```text
Check whether R_d blow-up comes from coalescence/capture compensation.
```

### Block D — two-parameter-family escape studies, about 1000 trials total

Run separate sub-blocks, around 250 trials each.

Suggested pairs:

```text
D1: b_scale + gd_scale
D2: Dv_scale + coalescence_d_scale
D3: K_d + rho_d + coalescence_d_scale
D4: capture_scale + gd_scale
```

Purpose:

```text
Find physically interpretable two-family compensations.
```

Report whether the improvement is real or just parameter drift.

### Block E — all-wide final exploratory, 500 trials

Only after A–D have been analyzed.

Purpose:

```text
Allow all uncertain parameters to move widely, but keep Rizk prior nonzero.
```

This is not the candidate selection block by itself. It is an exploratory diagnostic.

If the best all-wide candidate is far from Rizk but much better, identify which parameters are suspicious and need reference audit.

---

## 6. Adaptive score-weight protocol

You may adapt weights only under this protocol:

1. Finish a block.
2. Read score component distributions.
3. Check top candidates’ plots/diagnostics if available.
4. If one score component dominates or all candidates fail in the same way, propose a new named block.
5. Write the reason in `WORKLOG.md`.
6. Run the revised block in a new output folder.

Never silently remove or weaken:

```text
pressure scoring
q_gb scoring
partition scoring
radius guard / saturation scoring
Rizk prior
```

Possible named variants:

```text
strong_saturation
soft_saturation
strong_partition
strong_prior
low_prior_escape
```

But the report must compare them fairly.

---

## 7. Final reruns

After all fast blocks:

Select about 8–12 candidates total, not just from the best score block.

Include:

```text
best_score_total
best_physical
best_radius_saturated
best_partition
best_pressure
best_rizk_near
best_escape_candidate from each promising escape family
```

Run final diagnostics with:

```text
final_dt_h = 1
final_n_modes = 40
plots enabled
```

Save final plots under:

```text
UN_M7_v5_codex_results/final_selected/
```

Required plots:

```text
swelling vs T at 1.1, 1.3, 3.2% FIMA
swelling vs burnup at 1600 K
R_d vs T at 1.3% FIMA
N_d vs T at 1.3% FIMA
pressure and pressure ratio at 3.2% FIMA
gas partition at 1.1 and 3.2% FIMA
capture diagnostics
```

---

## 8. Required final outputs

Create:

```text
UN_M7_v5_codex_report.md
UN_M7_v5_codex_results/WORKLOG.md
UN_M7_v5_codex_results/all_blocks_summary.csv
UN_M7_v5_codex_results/top_physical_candidates.csv
UN_M7_v5_codex_results/top_by_category.csv
UN_M7_v5_codex_results/parameter_escape_summary.csv
UN_M7_v5_codex_results/final_selected_candidates.csv
UN_M7_v5_codex_results/final_selected/**
```

The final report must answer:

1. Did v5 solve the high-T radius blow-up?
2. Did v5 produce a real R_d saturation/flattening or only lower R_d by hurting the fit?
3. Is the pressure problem in dislocation bubbles still present?
4. Is bulk gas below ~1600 K close to Rizk-like partition or still too low?
5. Which parameter family most improves the physical candidate?
6. Is there evidence for a suspicious Rizk parameter or implementation issue?
7. Is `capture_only` still the best thesis candidate?
8. Which model should be compared next: Rizk-base, M7_no_phi, or M7_full?
9. Which candidates should be kept for thesis plots?

---

## 9. Final message to user

At the end, print a concise summary:

```text
best physical candidate:
best score candidate:
best radius-saturated candidate:
whether v5 succeeded:
whether pressure is acceptable:
which parameter family mattered most:
files created:
commands run:
git status summary:
```
