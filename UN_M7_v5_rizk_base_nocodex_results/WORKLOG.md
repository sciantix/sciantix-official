# WORKLOG — v5 Rizk-base/no-capture structured run

Initial git status:

```text
?? UN_M7_v5_rizk_base_nocodex_runner.py
?? UN_M7_v5_rizk_base_nocodex_runner.py:Zone.Identifier
```

Trial scale: 1.0

Family: baseline

This run uses separate blocks, not one blind continuous Optuna study.

## Starting block A_nominal_near

Purpose: Rizk-near pilot: can the no-capture baseline work without moving far from nominal parameters?

Requested trials this run: 300

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/A_nominal_near`

## Block A_nominal_near completed
Purpose: Rizk-near pilot: can the no-capture baseline work without moving far from nominal parameters?
Rows: 300

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_A_nominal_near_trial_00299 | 2.047 | 258.2 | 1.728 | 1.081 | 59.73 | 8.558 |
| best_physical_metric | baseline_v5_A_nominal_near_trial_00299 | 2.047 | 258.2 | 1.728 | 1.081 | 59.73 | 8.558 |
| best_radius_saturated | baseline_v5_A_nominal_near_trial_00034 | 2.391 | 122.6 | 1.357 | 1.004 | 57.75 | 9.05 |
| best_partition_qgb | baseline_v5_A_nominal_near_trial_00140 | 2.079 | 189.9 | 1.449 | 1.07 | 56.14 | 8.017 |
| best_pressure | baseline_v5_A_nominal_near_trial_00139 | 2.083 | 209.1 | 1.565 | 1.046 | 61.35 | 8.161 |
| best_rizk_near | baseline_v5_A_nominal_near_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_A_nominal_near_trial_00299 | 2.047 | 258.2 | 1.728 | 1.081 | 59.73 | 8.558 |

## Starting block B_standard_wide

Purpose: Main wide v5 baseline/no-capture exploration.

Requested trials this run: 700

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/B_standard_wide`

## Block B_standard_wide completed
Purpose: Main wide v5 baseline/no-capture exploration.
Rows: 700

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_B_standard_wide_trial_00641 | 2.073 | 264.8 | 1.451 | 1.192 | 66.58 | 6.397 |
| best_physical_metric | baseline_v5_B_standard_wide_trial_00641 | 2.073 | 264.8 | 1.451 | 1.192 | 66.58 | 6.397 |
| best_radius_saturated | baseline_v5_B_standard_wide_trial_00544 | 2.088 | 220.7 | 1.336 | 1.304 | 64.93 | 5.369 |
| best_partition_qgb | baseline_v5_B_standard_wide_trial_00280 | 2.12 | 155.8 | 1.211 | 1.152 | 63.59 | 5.315 |
| best_pressure | baseline_v5_B_standard_wide_trial_00235 | 2.152 | 148.1 | 1.211 | 1.091 | 65.39 | 5.594 |
| best_rizk_near | baseline_v5_B_standard_wide_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_B_standard_wide_trial_00641 | 2.073 | 264.8 | 1.451 | 1.192 | 66.58 | 6.397 |

## Starting block C1_diffusion_escape

Purpose: One-family escape: diffusivity scales wide; other families moderate.

Requested trials this run: 200

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/C1_diffusion_escape`

## Block C1_diffusion_escape completed
Purpose: One-family escape: diffusivity scales wide; other families moderate.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_C1_diffusion_escape_trial_00173 | 2.149 | 237.4 | 1.907 | 1.141 | 51.34 | 9.332 |
| best_physical_metric | baseline_v5_C1_diffusion_escape_trial_00173 | 2.149 | 237.4 | 1.907 | 1.141 | 51.34 | 9.332 |
| best_radius_saturated | baseline_v5_C1_diffusion_escape_trial_00106 | 2.425 | 174.9 | 1.234 | 1.198 | 38.97 | 19.34 |
| best_partition_qgb | baseline_v5_C1_diffusion_escape_trial_00193 | 2.183 | 170.3 | 1.537 | 1.08 | 54.46 | 8.15 |
| best_pressure | baseline_v5_C1_diffusion_escape_trial_00032 | 2.264 | 291.3 | 1.658 | 1.057 | 49.49 | 20.55 |
| best_rizk_near | baseline_v5_C1_diffusion_escape_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_C1_diffusion_escape_trial_00173 | 2.149 | 237.4 | 1.907 | 1.141 | 51.34 | 9.332 |

## Starting block C2_resolution_escape

Purpose: One-family escape: re-solution scale wide.

Requested trials this run: 200

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/C2_resolution_escape`

## Block C2_resolution_escape completed
Purpose: One-family escape: re-solution scale wide.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_C2_resolution_escape_trial_00178 | 1.978 | 265.3 | 1.679 | 1.102 | 64.55 | 6.533 |
| best_physical_metric | baseline_v5_C2_resolution_escape_trial_00178 | 1.978 | 265.3 | 1.679 | 1.102 | 64.55 | 6.533 |
| best_radius_saturated | baseline_v5_C2_resolution_escape_trial_00065 | 2.399 | 117.8 | 1.163 | 1.169 | 40.8 | 8.199 |
| best_partition_qgb | baseline_v5_C2_resolution_escape_trial_00143 | 2.011 | 279.7 | 1.757 | 1.161 | 60.86 | 6.23 |
| best_pressure | baseline_v5_C2_resolution_escape_trial_00090 | 2.114 | 226.6 | 1.858 | 1.082 | 53.99 | 6.724 |
| best_rizk_near | baseline_v5_C2_resolution_escape_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_C2_resolution_escape_trial_00178 | 1.978 | 265.3 | 1.679 | 1.102 | 64.55 | 6.533 |

## Starting block C3_trapping_escape

Purpose: One-family escape: bulk/dislocation trapping scales wide.

Requested trials this run: 200

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/C3_trapping_escape`

## Block C3_trapping_escape completed
Purpose: One-family escape: bulk/dislocation trapping scales wide.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_C3_trapping_escape_trial_00184 | 2.12 | 315.7 | 1.776 | 1.162 | 64.54 | 8.003 |
| best_physical_metric | baseline_v5_C3_trapping_escape_trial_00184 | 2.12 | 315.7 | 1.776 | 1.162 | 64.54 | 8.003 |
| best_radius_saturated | baseline_v5_C3_trapping_escape_trial_00120 | 2.35 | 180 | 1.291 | 1.085 | 46.29 | 21.91 |
| best_partition_qgb | baseline_v5_C3_trapping_escape_trial_00047 | 2.321 | 652.7 | 3.052 | 1.327 | 56.18 | 6.319 |
| best_pressure | baseline_v5_C3_trapping_escape_trial_00108 | 2.368 | 227.7 | 1.617 | 1.037 | 67.45 | 10.87 |
| best_rizk_near | baseline_v5_C3_trapping_escape_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_C3_trapping_escape_trial_00184 | 2.12 | 315.7 | 1.776 | 1.162 | 64.54 | 8.003 |

## Starting block C4_microstructure_escape

Purpose: One-family escape: f_n, K_d, rho_d wide.

Requested trials this run: 200

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/C4_microstructure_escape`

## Block C4_microstructure_escape completed
Purpose: One-family escape: f_n, K_d, rho_d wide.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_C4_microstructure_escape_trial_00167 | 2.079 | 216.6 | 1.523 | 1.102 | 57.06 | 9.423 |
| best_physical_metric | baseline_v5_C4_microstructure_escape_trial_00167 | 2.079 | 216.6 | 1.523 | 1.102 | 57.06 | 9.423 |
| best_radius_saturated | baseline_v5_C4_microstructure_escape_trial_00136 | 2.103 | 162.4 | 1.315 | 1.062 | 56.68 | 9.084 |
| best_partition_qgb | baseline_v5_C4_microstructure_escape_trial_00153 | 2.14 | 188.3 | 1.349 | 1.152 | 54.68 | 7.978 |
| best_pressure | baseline_v5_C4_microstructure_escape_trial_00122 | 2.093 | 184.8 | 1.447 | 1.05 | 57.6 | 8.732 |
| best_rizk_near | baseline_v5_C4_microstructure_escape_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_C4_microstructure_escape_trial_00167 | 2.079 | 216.6 | 1.523 | 1.102 | 57.06 | 9.423 |

## Starting block C5_coalescence_escape

Purpose: One-family escape: dislocation coalescence scale wide; capture remains disabled.

Requested trials this run: 200

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/C5_coalescence_escape`

## Block C5_coalescence_escape completed
Purpose: One-family escape: dislocation coalescence scale wide; capture remains disabled.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_C5_coalescence_escape_trial_00158 | 2.093 | 229.2 | 1.553 | 1.061 | 59.26 | 8.914 |
| best_physical_metric | baseline_v5_C5_coalescence_escape_trial_00158 | 2.093 | 229.2 | 1.553 | 1.061 | 59.26 | 8.914 |
| best_radius_saturated | baseline_v5_C5_coalescence_escape_trial_00111 | 2.111 | 194.1 | 1.222 | 1.083 | 61.36 | 9.231 |
| best_partition_qgb | baseline_v5_C5_coalescence_escape_trial_00123 | 2.132 | 161 | 1.19 | 1.089 | 59.06 | 9.12 |
| best_pressure | baseline_v5_C5_coalescence_escape_trial_00158 | 2.093 | 229.2 | 1.553 | 1.061 | 59.26 | 8.914 |
| best_rizk_near | baseline_v5_C5_coalescence_escape_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_C5_coalescence_escape_trial_00158 | 2.093 | 229.2 | 1.553 | 1.061 | 59.26 | 8.914 |

## Starting block D1_resolution_trapping_pair

Purpose: Two-family escape: b_scale + trapping.

Requested trials this run: 200

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/D1_resolution_trapping_pair`

## Block D1_resolution_trapping_pair completed
Purpose: Two-family escape: b_scale + trapping.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_D1_resolution_trapping_pair_trial_00177 | 2.027 | 207.7 | 1.548 | 1.118 | 56.72 | 10.28 |
| best_physical_metric | baseline_v5_D1_resolution_trapping_pair_trial_00177 | 2.027 | 207.7 | 1.548 | 1.118 | 56.72 | 10.28 |
| best_radius_saturated | baseline_v5_D1_resolution_trapping_pair_trial_00009 | 3.718 | 121.8 | 1.366 | 1.002 | 40.73 | 29.21 |
| best_partition_qgb | baseline_v5_D1_resolution_trapping_pair_trial_00168 | 2.053 | 235.4 | 1.679 | 1.194 | 56.48 | 9.348 |
| best_pressure | baseline_v5_D1_resolution_trapping_pair_trial_00122 | 2.203 | 332.8 | 2.605 | 1.09 | 50.6 | 7.472 |
| best_rizk_near | baseline_v5_D1_resolution_trapping_pair_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_D1_resolution_trapping_pair_trial_00177 | 2.027 | 207.7 | 1.548 | 1.118 | 56.72 | 10.28 |

## Starting block D2_Dv_coalescence_pair

Purpose: Two-family escape: vacancy diffusivity + dislocation coalescence.

Requested trials this run: 200

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/D2_Dv_coalescence_pair`

## Block D2_Dv_coalescence_pair completed
Purpose: Two-family escape: vacancy diffusivity + dislocation coalescence.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_D2_Dv_coalescence_pair_trial_00164 | 2.084 | 172.9 | 1.21 | 1.018 | 62.52 | 9.538 |
| best_physical_metric | baseline_v5_D2_Dv_coalescence_pair_trial_00164 | 2.084 | 172.9 | 1.21 | 1.018 | 62.52 | 9.538 |
| best_radius_saturated | baseline_v5_D2_Dv_coalescence_pair_trial_00164 | 2.084 | 172.9 | 1.21 | 1.018 | 62.52 | 9.538 |
| best_partition_qgb | baseline_v5_D2_Dv_coalescence_pair_trial_00156 | 2.244 | 151.2 | 1.142 | 1.04 | 54.53 | 9.371 |
| best_pressure | baseline_v5_D2_Dv_coalescence_pair_trial_00164 | 2.084 | 172.9 | 1.21 | 1.018 | 62.52 | 9.538 |
| best_rizk_near | baseline_v5_D2_Dv_coalescence_pair_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_D2_Dv_coalescence_pair_trial_00164 | 2.084 | 172.9 | 1.21 | 1.018 | 62.52 | 9.538 |

## Starting block D3_microstructure_coalescence_pair

Purpose: Two-family escape: microstructure + coalescence.

Requested trials this run: 200

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/D3_microstructure_coalescence_pair`

## Block D3_microstructure_coalescence_pair completed
Purpose: Two-family escape: microstructure + coalescence.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_D3_microstructure_coalescence_pair_trial_00143 | 2.059 | 241.3 | 1.463 | 1.119 | 63.89 | 7.674 |
| best_physical_metric | baseline_v5_D3_microstructure_coalescence_pair_trial_00143 | 2.059 | 241.3 | 1.463 | 1.119 | 63.89 | 7.674 |
| best_radius_saturated | baseline_v5_D3_microstructure_coalescence_pair_trial_00182 | 2.086 | 208.5 | 1.343 | 1.094 | 62.34 | 9.19 |
| best_partition_qgb | baseline_v5_D3_microstructure_coalescence_pair_trial_00046 | 2.819 | 72.56 | 1.147 | 1.002 | 59.56 | 8.168 |
| best_pressure | baseline_v5_D3_microstructure_coalescence_pair_trial_00196 | 2.089 | 162.8 | 1.248 | 1.044 | 60.34 | 8.138 |
| best_rizk_near | baseline_v5_D3_microstructure_coalescence_pair_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_D3_microstructure_coalescence_pair_trial_00143 | 2.059 | 241.3 | 1.463 | 1.119 | 63.89 | 7.674 |

## Starting block D4_diffusion_resolution_pair

Purpose: Two-family escape: diffusion + resolution.

Requested trials this run: 200

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/D4_diffusion_resolution_pair`

## Block D4_diffusion_resolution_pair completed
Purpose: Two-family escape: diffusion + resolution.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_D4_diffusion_resolution_pair_trial_00199 | 1.964 | 279.9 | 1.956 | 1.078 | 62.23 | 5.358 |
| best_physical_metric | baseline_v5_D4_diffusion_resolution_pair_trial_00199 | 1.964 | 279.9 | 1.956 | 1.078 | 62.23 | 5.358 |
| best_radius_saturated | baseline_v5_D4_diffusion_resolution_pair_trial_00055 | 2.229 | 133.8 | 1.224 | 1.136 | 46.5 | 10.85 |
| best_partition_qgb | baseline_v5_D4_diffusion_resolution_pair_trial_00129 | 2.085 | 353.3 | 2.308 | 1.241 | 61.53 | 5.338 |
| best_pressure | baseline_v5_D4_diffusion_resolution_pair_trial_00174 | 2.149 | 350.9 | 2.389 | 1.076 | 65.89 | 6.027 |
| best_rizk_near | baseline_v5_D4_diffusion_resolution_pair_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_D4_diffusion_resolution_pair_trial_00199 | 1.964 | 279.9 | 1.956 | 1.078 | 62.23 | 5.358 |

## Starting block E_all_wide

Purpose: All-wide exploratory baseline/no-capture study with nonzero prior.

Requested trials this run: 500

Output: `UN_M7_optuna_v5_results/baseline_no_capture_structured/E_all_wide`

## Block E_all_wide completed
Purpose: All-wide exploratory baseline/no-capture study with nonzero prior.
Rows: 500

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_E_all_wide_trial_00497 | 1.988 | 226 | 1.405 | 1.1 | 67.16 | 6.918 |
| best_physical_metric | baseline_v5_E_all_wide_trial_00497 | 1.988 | 226 | 1.405 | 1.1 | 67.16 | 6.918 |
| best_radius_saturated | baseline_v5_E_all_wide_trial_00457 | 1.996 | 207.2 | 1.307 | 1.107 | 65.09 | 7.617 |
| best_partition_qgb | baseline_v5_E_all_wide_trial_00317 | 2.317 | 178.3 | 1.322 | 1.292 | 67.28 | 6.581 |
| best_pressure | baseline_v5_E_all_wide_trial_00274 | 2.025 | 214.8 | 1.347 | 1.087 | 65.95 | 8.632 |
| best_rizk_near | baseline_v5_E_all_wide_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_E_all_wide_trial_00497 | 1.988 | 226 | 1.405 | 1.1 | 67.16 | 6.918 |

## Starting final plots

Candidates: 5

Output: `UN_M7_v5_rizk_base_nocodex_results/final_selected`

## Final plots completed

Output: `UN_M7_v5_rizk_base_nocodex_results/final_selected`

## Run completed

Final git status:

```text
?? UN_M7_optuna_v5_results/
?? UN_M7_v5_rizk_base_nocodex_results/
?? UN_M7_v5_rizk_base_nocodex_runner.py
?? UN_M7_v5_rizk_base_nocodex_runner.py:Zone.Identifier
```

# WORKLOG — v5 Rizk-base/no-capture structured run

Initial git status:

```text
?? UN_M7_optuna_v5_results/
?? UN_M7_v5_rizk_base_nocodex_results/
?? UN_M7_v5_rizk_base_nocodex_runner.py
?? UN_M7_v5_rizk_base_nocodex_runner.py:Zone.Identifier
```

Trial scale: 1.0

Family: baseline

This run uses separate blocks, not one blind continuous Optuna study.

## Block A_nominal_near completed
Purpose: Rizk-near pilot: can the no-capture baseline work without moving far from nominal parameters?
Rows: 300

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_A_nominal_near_trial_00299 | 2.047 | 258.2 | 1.728 | 1.081 | 59.73 | 8.558 |
| best_physical_metric | baseline_v5_A_nominal_near_trial_00299 | 2.047 | 258.2 | 1.728 | 1.081 | 59.73 | 8.558 |
| best_radius_saturated | baseline_v5_A_nominal_near_trial_00034 | 2.391 | 122.6 | 1.357 | 1.004 | 57.75 | 9.05 |
| best_partition_qgb | baseline_v5_A_nominal_near_trial_00140 | 2.079 | 189.9 | 1.449 | 1.07 | 56.14 | 8.017 |
| best_pressure | baseline_v5_A_nominal_near_trial_00139 | 2.083 | 209.1 | 1.565 | 1.046 | 61.35 | 8.161 |
| best_rizk_near | baseline_v5_A_nominal_near_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_A_nominal_near_trial_00299 | 2.047 | 258.2 | 1.728 | 1.081 | 59.73 | 8.558 |

## Block B_standard_wide completed
Purpose: Main wide v5 baseline/no-capture exploration.
Rows: 700

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_B_standard_wide_trial_00641 | 2.073 | 264.8 | 1.451 | 1.192 | 66.58 | 6.397 |
| best_physical_metric | baseline_v5_B_standard_wide_trial_00641 | 2.073 | 264.8 | 1.451 | 1.192 | 66.58 | 6.397 |
| best_radius_saturated | baseline_v5_B_standard_wide_trial_00544 | 2.088 | 220.7 | 1.336 | 1.304 | 64.93 | 5.369 |
| best_partition_qgb | baseline_v5_B_standard_wide_trial_00280 | 2.12 | 155.8 | 1.211 | 1.152 | 63.59 | 5.315 |
| best_pressure | baseline_v5_B_standard_wide_trial_00235 | 2.152 | 148.1 | 1.211 | 1.091 | 65.39 | 5.594 |
| best_rizk_near | baseline_v5_B_standard_wide_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_B_standard_wide_trial_00641 | 2.073 | 264.8 | 1.451 | 1.192 | 66.58 | 6.397 |

## Block C1_diffusion_escape completed
Purpose: One-family escape: diffusivity scales wide; other families moderate.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_C1_diffusion_escape_trial_00173 | 2.149 | 237.4 | 1.907 | 1.141 | 51.34 | 9.332 |
| best_physical_metric | baseline_v5_C1_diffusion_escape_trial_00173 | 2.149 | 237.4 | 1.907 | 1.141 | 51.34 | 9.332 |
| best_radius_saturated | baseline_v5_C1_diffusion_escape_trial_00106 | 2.425 | 174.9 | 1.234 | 1.198 | 38.97 | 19.34 |
| best_partition_qgb | baseline_v5_C1_diffusion_escape_trial_00193 | 2.183 | 170.3 | 1.537 | 1.08 | 54.46 | 8.15 |
| best_pressure | baseline_v5_C1_diffusion_escape_trial_00032 | 2.264 | 291.3 | 1.658 | 1.057 | 49.49 | 20.55 |
| best_rizk_near | baseline_v5_C1_diffusion_escape_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_C1_diffusion_escape_trial_00173 | 2.149 | 237.4 | 1.907 | 1.141 | 51.34 | 9.332 |

## Block C2_resolution_escape completed
Purpose: One-family escape: re-solution scale wide.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_C2_resolution_escape_trial_00178 | 1.978 | 265.3 | 1.679 | 1.102 | 64.55 | 6.533 |
| best_physical_metric | baseline_v5_C2_resolution_escape_trial_00178 | 1.978 | 265.3 | 1.679 | 1.102 | 64.55 | 6.533 |
| best_radius_saturated | baseline_v5_C2_resolution_escape_trial_00065 | 2.399 | 117.8 | 1.163 | 1.169 | 40.8 | 8.199 |
| best_partition_qgb | baseline_v5_C2_resolution_escape_trial_00143 | 2.011 | 279.7 | 1.757 | 1.161 | 60.86 | 6.23 |
| best_pressure | baseline_v5_C2_resolution_escape_trial_00090 | 2.114 | 226.6 | 1.858 | 1.082 | 53.99 | 6.724 |
| best_rizk_near | baseline_v5_C2_resolution_escape_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_C2_resolution_escape_trial_00178 | 1.978 | 265.3 | 1.679 | 1.102 | 64.55 | 6.533 |

## Block C3_trapping_escape completed
Purpose: One-family escape: bulk/dislocation trapping scales wide.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_C3_trapping_escape_trial_00184 | 2.12 | 315.7 | 1.776 | 1.162 | 64.54 | 8.003 |
| best_physical_metric | baseline_v5_C3_trapping_escape_trial_00184 | 2.12 | 315.7 | 1.776 | 1.162 | 64.54 | 8.003 |
| best_radius_saturated | baseline_v5_C3_trapping_escape_trial_00120 | 2.35 | 180 | 1.291 | 1.085 | 46.29 | 21.91 |
| best_partition_qgb | baseline_v5_C3_trapping_escape_trial_00047 | 2.321 | 652.7 | 3.052 | 1.327 | 56.18 | 6.319 |
| best_pressure | baseline_v5_C3_trapping_escape_trial_00108 | 2.368 | 227.7 | 1.617 | 1.037 | 67.45 | 10.87 |
| best_rizk_near | baseline_v5_C3_trapping_escape_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_C3_trapping_escape_trial_00184 | 2.12 | 315.7 | 1.776 | 1.162 | 64.54 | 8.003 |

## Block C4_microstructure_escape completed
Purpose: One-family escape: f_n, K_d, rho_d wide.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_C4_microstructure_escape_trial_00167 | 2.079 | 216.6 | 1.523 | 1.102 | 57.06 | 9.423 |
| best_physical_metric | baseline_v5_C4_microstructure_escape_trial_00167 | 2.079 | 216.6 | 1.523 | 1.102 | 57.06 | 9.423 |
| best_radius_saturated | baseline_v5_C4_microstructure_escape_trial_00136 | 2.103 | 162.4 | 1.315 | 1.062 | 56.68 | 9.084 |
| best_partition_qgb | baseline_v5_C4_microstructure_escape_trial_00153 | 2.14 | 188.3 | 1.349 | 1.152 | 54.68 | 7.978 |
| best_pressure | baseline_v5_C4_microstructure_escape_trial_00122 | 2.093 | 184.8 | 1.447 | 1.05 | 57.6 | 8.732 |
| best_rizk_near | baseline_v5_C4_microstructure_escape_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_C4_microstructure_escape_trial_00167 | 2.079 | 216.6 | 1.523 | 1.102 | 57.06 | 9.423 |

## Block C5_coalescence_escape completed
Purpose: One-family escape: dislocation coalescence scale wide; capture remains disabled.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_C5_coalescence_escape_trial_00158 | 2.093 | 229.2 | 1.553 | 1.061 | 59.26 | 8.914 |
| best_physical_metric | baseline_v5_C5_coalescence_escape_trial_00158 | 2.093 | 229.2 | 1.553 | 1.061 | 59.26 | 8.914 |
| best_radius_saturated | baseline_v5_C5_coalescence_escape_trial_00111 | 2.111 | 194.1 | 1.222 | 1.083 | 61.36 | 9.231 |
| best_partition_qgb | baseline_v5_C5_coalescence_escape_trial_00123 | 2.132 | 161 | 1.19 | 1.089 | 59.06 | 9.12 |
| best_pressure | baseline_v5_C5_coalescence_escape_trial_00158 | 2.093 | 229.2 | 1.553 | 1.061 | 59.26 | 8.914 |
| best_rizk_near | baseline_v5_C5_coalescence_escape_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_C5_coalescence_escape_trial_00158 | 2.093 | 229.2 | 1.553 | 1.061 | 59.26 | 8.914 |

## Block D1_resolution_trapping_pair completed
Purpose: Two-family escape: b_scale + trapping.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_D1_resolution_trapping_pair_trial_00177 | 2.027 | 207.7 | 1.548 | 1.118 | 56.72 | 10.28 |
| best_physical_metric | baseline_v5_D1_resolution_trapping_pair_trial_00177 | 2.027 | 207.7 | 1.548 | 1.118 | 56.72 | 10.28 |
| best_radius_saturated | baseline_v5_D1_resolution_trapping_pair_trial_00009 | 3.718 | 121.8 | 1.366 | 1.002 | 40.73 | 29.21 |
| best_partition_qgb | baseline_v5_D1_resolution_trapping_pair_trial_00168 | 2.053 | 235.4 | 1.679 | 1.194 | 56.48 | 9.348 |
| best_pressure | baseline_v5_D1_resolution_trapping_pair_trial_00122 | 2.203 | 332.8 | 2.605 | 1.09 | 50.6 | 7.472 |
| best_rizk_near | baseline_v5_D1_resolution_trapping_pair_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_D1_resolution_trapping_pair_trial_00177 | 2.027 | 207.7 | 1.548 | 1.118 | 56.72 | 10.28 |

## Block D2_Dv_coalescence_pair completed
Purpose: Two-family escape: vacancy diffusivity + dislocation coalescence.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_D2_Dv_coalescence_pair_trial_00164 | 2.084 | 172.9 | 1.21 | 1.018 | 62.52 | 9.538 |
| best_physical_metric | baseline_v5_D2_Dv_coalescence_pair_trial_00164 | 2.084 | 172.9 | 1.21 | 1.018 | 62.52 | 9.538 |
| best_radius_saturated | baseline_v5_D2_Dv_coalescence_pair_trial_00164 | 2.084 | 172.9 | 1.21 | 1.018 | 62.52 | 9.538 |
| best_partition_qgb | baseline_v5_D2_Dv_coalescence_pair_trial_00156 | 2.244 | 151.2 | 1.142 | 1.04 | 54.53 | 9.371 |
| best_pressure | baseline_v5_D2_Dv_coalescence_pair_trial_00164 | 2.084 | 172.9 | 1.21 | 1.018 | 62.52 | 9.538 |
| best_rizk_near | baseline_v5_D2_Dv_coalescence_pair_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_D2_Dv_coalescence_pair_trial_00164 | 2.084 | 172.9 | 1.21 | 1.018 | 62.52 | 9.538 |

## Block D3_microstructure_coalescence_pair completed
Purpose: Two-family escape: microstructure + coalescence.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_D3_microstructure_coalescence_pair_trial_00143 | 2.059 | 241.3 | 1.463 | 1.119 | 63.89 | 7.674 |
| best_physical_metric | baseline_v5_D3_microstructure_coalescence_pair_trial_00143 | 2.059 | 241.3 | 1.463 | 1.119 | 63.89 | 7.674 |
| best_radius_saturated | baseline_v5_D3_microstructure_coalescence_pair_trial_00182 | 2.086 | 208.5 | 1.343 | 1.094 | 62.34 | 9.19 |
| best_partition_qgb | baseline_v5_D3_microstructure_coalescence_pair_trial_00046 | 2.819 | 72.56 | 1.147 | 1.002 | 59.56 | 8.168 |
| best_pressure | baseline_v5_D3_microstructure_coalescence_pair_trial_00196 | 2.089 | 162.8 | 1.248 | 1.044 | 60.34 | 8.138 |
| best_rizk_near | baseline_v5_D3_microstructure_coalescence_pair_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_D3_microstructure_coalescence_pair_trial_00143 | 2.059 | 241.3 | 1.463 | 1.119 | 63.89 | 7.674 |

## Block D4_diffusion_resolution_pair completed
Purpose: Two-family escape: diffusion + resolution.
Rows: 200

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_D4_diffusion_resolution_pair_trial_00199 | 1.964 | 279.9 | 1.956 | 1.078 | 62.23 | 5.358 |
| best_physical_metric | baseline_v5_D4_diffusion_resolution_pair_trial_00199 | 1.964 | 279.9 | 1.956 | 1.078 | 62.23 | 5.358 |
| best_radius_saturated | baseline_v5_D4_diffusion_resolution_pair_trial_00055 | 2.229 | 133.8 | 1.224 | 1.136 | 46.5 | 10.85 |
| best_partition_qgb | baseline_v5_D4_diffusion_resolution_pair_trial_00129 | 2.085 | 353.3 | 2.308 | 1.241 | 61.53 | 5.338 |
| best_pressure | baseline_v5_D4_diffusion_resolution_pair_trial_00174 | 2.149 | 350.9 | 2.389 | 1.076 | 65.89 | 6.027 |
| best_rizk_near | baseline_v5_D4_diffusion_resolution_pair_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_D4_diffusion_resolution_pair_trial_00199 | 1.964 | 279.9 | 1.956 | 1.078 | 62.23 | 5.358 |

## Block E_all_wide completed
Purpose: All-wide exploratory baseline/no-capture study with nonzero prior.
Rows: 500

| category | label | score | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | baseline_v5_E_all_wide_trial_00497 | 1.988 | 226 | 1.405 | 1.1 | 67.16 | 6.918 |
| best_physical_metric | baseline_v5_E_all_wide_trial_00497 | 1.988 | 226 | 1.405 | 1.1 | 67.16 | 6.918 |
| best_radius_saturated | baseline_v5_E_all_wide_trial_00457 | 1.996 | 207.2 | 1.307 | 1.107 | 65.09 | 7.617 |
| best_partition_qgb | baseline_v5_E_all_wide_trial_00317 | 2.317 | 178.3 | 1.322 | 1.292 | 67.28 | 6.581 |
| best_pressure | baseline_v5_E_all_wide_trial_00274 | 2.025 | 214.8 | 1.347 | 1.087 | 65.95 | 8.632 |
| best_rizk_near | baseline_v5_E_all_wide_trial_00000 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | baseline_v5_E_all_wide_trial_00497 | 1.988 | 226 | 1.405 | 1.1 | 67.16 | 6.918 |

## Starting final plots

Candidates: 5

Output: `UN_M7_v5_rizk_base_nocodex_results/final_selected`

## Final plots completed

Output: `UN_M7_v5_rizk_base_nocodex_results/final_selected`

## Run completed

Final git status:

```text
?? UN_M7_optuna_v5_results/
?? UN_M7_v5_rizk_base_nocodex_results/
?? UN_M7_v5_rizk_base_nocodex_runner.py
?? UN_M7_v5_rizk_base_nocodex_runner.py:Zone.Identifier
```

