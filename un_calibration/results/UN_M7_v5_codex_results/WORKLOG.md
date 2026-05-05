# UN M7 v5 Codex Worklog

## Setup

- Started from a clean `git status --short`.
- Read task file `CODEX_UN_M7_V5_TASK_v2.md`.
- Read required context: `AI_UN_calibration_instructions.md`, `UN_M7_optuna_calibration_v5.py`, `UNmodel.md`, and `UN_M7_calibration_lessons_report.md`.
- Confirmed strict constraints: no notebook edits, no original calibration script edits, no commits/branches/pushes, and all generated outputs under `UN_M7_v5_codex_results/`.

## Implementation Block 0 — Codex-Owned Tooling

- Creating `UN_M7_v5_codex_runner.py` because the base v5 CLI controls only `Dv_scale`; family-specific ranges for A/C/D/E need a wrapper.
- Creating `UN_M7_v5_codex_analysis.py` to classify candidates by physical categories after every block and write required block CSV summaries.
- The runner imports and reuses v5 scoring/equations; it changes only Optuna sampling ranges and output folders for named blocks.

## Starting A_nominal_near

- Trials requested: 300
- Output dir: `UN_M7_v5_codex_results/A_nominal_near`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Starting A_nominal_near

- Trials requested: 300
- Output dir: `UN_M7_v5_codex_results/A_nominal_near`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed A_nominal_near

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Decision After A_nominal_near

- Block A produced analyzable outputs and all top-50 candidates pass the current radius/partition/pressure filters.
- Best score candidate is also the current best physical candidate: `capture_only_v5_codex_trial_00264`, `score_total=2.04946`.
- Physical diagnostics for the best candidate are encouraging: `R_d(2000 K)=263 nm`, `R_d(1900->2000)=55 nm`, `p_d/p_eq(1600 K)=1.05`, `bulk_gas(1600 K)=62.8%`, `qgb(1600 K)=8.0%`.
- What improved: v5 saturation/radius scoring prevents the v3/v4 micrometric high-T radius blow-up in the nominal-near region.
- What got worse / remains weak: score is still limited by swelling and `N_d`/drop components; best `swD_1600` is high at about 2.87%.
- Parameter directions: top candidates push `b_scale` low, `gb_scale` and `gd_scale` high, `coalescence_d_scale` low, with `f_n` near the top of the conservative range.
- Strategy: continue to Block B standard-wide. This checks whether the good physical behavior is robust or whether the wide range finds a lower-score but less physical basin.

## Analysis A_nominal_near

- Best score: `capture_only_v5_codex_trial_00264` with `score_total=2.0495`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=263 nm`, `p_d/p_eq1600=1.049`, `bulk1600=62.83%`, `qgb1600=7.967%`.
- Parameter directions in top set: b_scale=0.482, gb_scale=1.72, gd_scale=2.9, coalescence_d_scale=0.554.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Starting B_standard_wide

- Trials requested: 700
- Output dir: `UN_M7_v5_codex_results/B_standard_wide`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed B_standard_wide

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis B_standard_wide

- Best score: `capture_only_v5_codex_trial_00629` with `score_total=1.99`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=261.2 nm`, `p_d/p_eq1600=1.12`, `bulk1600=65.78%`, `qgb1600=4.348%`.
- Parameter directions in top set: Dv_scale=1.54, Dg_scale=0.54, D2_xe_scale=0.177, b_scale=0.0591, gb_scale=1.57, gd_scale=3.72, coalescence_d_scale=0.545, capture_scale=2.7.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Decision After B_standard_wide

- Block B improved the best fast score from A (`2.04946`) to `1.99000`.
- The best B candidate is physically acceptable under the current filters: `R_d(2000 K)=261 nm`, `R_d(1900->2000)=52.9 nm`, `p_d/p_eq(1600 K)=1.12`, `bulk_gas(1600 K)=65.8%`, `qgb_gas(1600 K)=4.35%`.
- The best B candidate has better partition and pressure than A, with comparable high-T radius behavior, but reaches it through a more displaced parameter basin.
- Main escape signature: very low `b_scale` near the lower bound, low `Dg_scale` and `D2_xe_scale`, high `Dv_scale`, elevated `capture_scale`, and moderately high `gd_scale`.
- This suggests the next one-family studies should test whether the improvement comes mostly from diffusion, resolution/trapping, or coalescence/capture rather than accepting the all-wide basin blindly.
- Strategy: continue with all one-parameter-family escape blocks C1-C5, analyzing each block before moving on.

## Starting C1_diffusion_escape

- Trials requested: 200
- Output dir: `UN_M7_v5_codex_results/C1_diffusion_escape`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed C1_diffusion_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis C1_diffusion_escape

- Best score: `capture_only_v5_codex_trial_00185` with `score_total=2.0862`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=196.5 nm`, `p_d/p_eq1600=1.073`, `bulk1600=59.75%`, `qgb1600=6.947%`.
- Parameter directions in top set: D2_xe_scale=8.95, b_scale=0.202, gd_scale=2.62, coalescence_d_scale=0.595, capture_scale=0.161.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Decision After C1_diffusion_escape

- C1 did not reproduce the standard-wide improvement: best score `2.08617`, worse than A (`2.04946`) and B (`1.99000`).
- It did produce very good high-temperature radius behavior: `R_d(2000 K)=196.5 nm` and `R_d(1900->2000)=27.6 nm`.
- Gas partition remains acceptable but not superior to B: `bulk_gas(1600 K)=59.8%`, `qgb_gas(1600 K)=6.95%`.
- The C1 basin is scientifically distinct from B: top candidates push `D2_xe_scale` high and `capture_scale` low, while B's best basin had `D2_xe_scale` low and `capture_scale` high.
- Interpretation: diffusion-only escape mainly improves/saturates radius, but it does not explain the full score/partition gain seen in B.
- Strategy: continue to C2 resolution escape to test whether very low `b_scale` is the dominant driver.

## Analysis C1_diffusion_escape

- Best score: `capture_only_v5_codex_trial_00185` with `score_total=2.0862`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=196.5 nm`, `p_d/p_eq1600=1.073`, `bulk1600=59.75%`, `qgb1600=6.947%`.
- Parameter directions in top set: D2_xe_scale=8.95, b_scale=0.202, gd_scale=2.62, coalescence_d_scale=0.595, capture_scale=0.161.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Starting C2_resolution_escape

- Trials requested: 200
- Output dir: `UN_M7_v5_codex_results/C2_resolution_escape`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed C2_resolution_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis C2_resolution_escape

- Best score: `capture_only_v5_codex_trial_00047` with `score_total=2.0624`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=294.1 nm`, `p_d/p_eq1600=1.343`, `bulk1600=64.11%`, `qgb1600=6.014%`.
- Parameter directions in top set: Dv_scale=0.599, Dg_scale=2.36, D2_xe_scale=0.505, b_scale=0.0319, gb_scale=3.09, gd_scale=2.99, coalescence_d_scale=0.643, capture_scale=0.286.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Decision After C2_resolution_escape

- C2 confirms that very low `b_scale` can give acceptable physical candidates, but it does not recover the all-wide score improvement from B.
- Best C2 score is `2.06239`, worse than A (`2.04946`) and B (`1.99000`), although the candidate remains submicrometric: `R_d(2000 K)=294 nm`.
- Gas partition is acceptable (`bulk_gas(1600 K)=64.1%`, `qgb_gas(1600 K)=6.01%`), but pressure is less centered than A/B (`p_d/p_eq(1600 K)=1.34`) and swelling remains high.
- The block strongly pushes `b_scale` to the lower boundary and also raises `Dg_scale`, `gb_scale`, and `gd_scale`, so the resolution family alone is not a sufficient explanation.
- Strategy: continue to C3 trapping escape to test whether the high `gb_scale`/`gd_scale` direction is an independent driver or only useful when coupled to low resolution/capture changes.

## Starting C3_trapping_escape

- Trials requested: 200
- Output dir: `UN_M7_v5_codex_results/C3_trapping_escape`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed C3_trapping_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis C3_trapping_escape

- Best score: `capture_only_v5_codex_trial_00192` with `score_total=2.1628`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=361.8 nm`, `p_d/p_eq1600=1.129`, `bulk1600=56.47%`, `qgb1600=9.769%`.
- Parameter directions in top set: Dv_scale=0.68, Dg_scale=2.02, D2_xe_scale=2.17, gb_scale=2.73, gd_scale=3.37, capture_scale=0.319.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Decision After C3_trapping_escape

- C3 gives physical candidates but does not approach the best all-wide basin: best score `2.16283`, worse than B (`1.99000`) and also worse than C1/C2.
- Best C3 remains submicrometric, but high-temperature radius is less saturated than the best radius-specific candidates: `R_d(2000 K)=362 nm`, with `R_d(1900->2000)=133 nm`.
- Pressure is acceptable (`p_d/p_eq(1600 K)=1.13`), but partition is weaker than B: `bulk_gas(1600 K)=56.5%`, `qgb_gas(1600 K)=9.77%`.
- The useful signature is not low `b_scale`; top C3 has near-nominal `b_scale`, high `Dg_scale`, high `D2_xe_scale`, high `gb_scale/gd_scale`, and low `capture_scale`.
- Interpretation: trapping escape alone can produce physical, submicrometric candidates, but it does not solve the partition/fit trade-off. It likely needs coupling to the low-resolution or coalescence/capture directions.
- Strategy: continue to C4 microstructure escape to test whether the wide basin depends mainly on `f_n`, `K_d`, `rho_d`, or `Fdot` rather than the transport/trapping scalings.

## Starting C4_microstructure_escape

- Trials requested: 200
- Output dir: `UN_M7_v5_codex_results/C4_microstructure_escape`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed C4_microstructure_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis C4_microstructure_escape

- Best score: `capture_only_v5_codex_trial_00196` with `score_total=2.0532`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=225.8 nm`, `p_d/p_eq1600=1.01`, `bulk1600=63.28%`, `qgb1600=7.258%`.
- Parameter directions in top set: Dg_scale=0.666, D2_xe_scale=4.1, b_scale=0.251, gb_scale=2.59, coalescence_d_scale=0.644, capture_scale=1.59.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Decision After C4_microstructure_escape

- C4 is scientifically useful even though it does not beat B: best score `2.05325`, close to A/C2 but still above B (`1.99000`).
- Best C4 is a credible physical candidate: `R_d(2000 K)=226 nm`, `R_d(1900->2000)=40 nm`, `p_d/p_eq(1600 K)=1.01`, `bulk_gas(1600 K)=63.3%`, `qgb_gas(1600 K)=7.26%`.
- Compared with B, C4 has better pressure centering and smaller high-T radius, but worse total score and weaker partition.
- The top C4 basin is not purely microstructural: it combines high `rho_d`, low `K_d`, low `Dg_scale`, very low `b_scale`, high `gb_scale`, low coalescence, and high capture.
- Interpretation: moving microstructure alone is not enough, but it stabilizes a physically attractive basin when coupled with resolution/capture changes.
- Strategy: continue to C5 coalescence/capture escape. This is the natural next test because both B and C4 use coalescence/capture directions, but with different signs and magnitudes.

## Starting C5_coalescence_capture_escape

- Trials requested: 200
- Output dir: `UN_M7_v5_codex_results/C5_coalescence_capture_escape`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed C5_coalescence_capture_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis C5_coalescence_capture_escape

- Best score: `capture_only_v5_codex_trial_00172` with `score_total=2.0841`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=315.4 nm`, `p_d/p_eq1600=1.188`, `bulk1600=64.02%`, `qgb1600=6.901%`.
- Parameter directions in top set: Dg_scale=0.614, D2_xe_scale=3.57, b_scale=0.213, gb_scale=1.67, gd_scale=2.09, coalescence_d_scale=0.611, capture_scale=0.62.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Decision After C5_coalescence_capture_escape

- C5 gives physical candidates, but it does not beat the best basin: best score `2.08411`, worse than B (`1.99000`), A (`2.04946`), C4 (`2.05325`), and C2 (`2.06239`).
- Best C5 has acceptable pressure and partition balance, but a less attractive high-temperature radius than C4/B: `R_d(2000 K)=315 nm`, `R_d(1900->2000)=83 nm`, `p_d/p_eq(1600 K)=1.19`, `bulk_gas(1600 K)=64.0%`, `qgb_gas(1600 K)=6.90%`.
- The radius-saturated C5 candidate is cleaner in shape (`R_d(2000 K)=218 nm`, `R_d(1900->2000)=19 nm`) but pays with worse score (`2.13789`) and weaker fit terms.
- The top C5 direction is low `Dg_scale`, high `D2_xe_scale`, very low `b_scale`, high `gb_scale/gd_scale`, low coalescence, and low capture. This is close to the B/C4 physical basin, not a new capture-driven escape.
- Interpretation: coalescence/capture escape alone helps keep candidates physical, but it does not solve the score/partition/radius trade-off. The useful information is the low coalescence/low capture direction coupled to resolution and microstructure movement.
- Strategy: all one-family escape studies are complete. Proceed to two-parameter-family escape studies, prioritizing combinations that couple the strongest one-family signals: resolution + microstructure, resolution + coalescence/capture, microstructure + coalescence/capture, and diffusion + trapping.

## Starting D1_b_gd_escape

- Trials requested: 250
- Output dir: `UN_M7_v5_codex_results/D1_b_gd_escape`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed D1_b_gd_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis D1_b_gd_escape

- Best score: `capture_only_v5_codex_trial_00022` with `score_total=2.0911`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=206.6 nm`, `p_d/p_eq1600=1.121`, `bulk1600=62.48%`, `qgb1600=4.392%`.
- Parameter directions in top set: Dg_scale=0.504, D2_xe_scale=0.559, b_scale=0.377, gb_scale=2.88, gd_scale=5.75, coalescence_d_scale=0.673, capture_scale=0.128.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Decision After D1_b_gd_escape

- D1 does not improve the global trade-off: best score `2.09106`, worse than B (`1.99000`), A (`2.04946`), C4 (`2.05325`), C2 (`2.06239`), and C5 (`2.08411`).
- The best-score candidate has attractive high-temperature radius (`R_d(2000 K)=207 nm`, `R_d(1900->2000)=42 nm`) and good partition score, but it relies on a high nucleation prefactor (`f_n=9.63e-7`) and high `N_d(1600 K)=1.76e19`, so it is not the safest physical representative.
- The best physical D1 candidate is `trial_00228`: `score_total=2.09759`, `R_d(2000 K)=273 nm`, `R_d(1900->2000)=67 nm`, `p_d/p_eq(1600 K)=1.07`, `bulk_gas(1600 K)=63.1%`, `qgb_gas(1600 K)=6.36%`.
- Directionally, D1 reinforces low `Dg_scale`, low `D2_xe_scale`, low `b_scale`, high `gb_scale`, high `gd_scale`, low coalescence, and low capture. It does not support very large `gd_scale` as an independent escape; excessive `gd_scale` often worsened scores.
- Interpretation: `b+gd` can keep radius submicrometric and pressure acceptable, but by itself it does not recover the B-level score or the desired gas partition. The winning basin still needs coupling to transport/coalescence/capture and probably microstructure.
- Strategy: proceed to D2 (`Dv+coalescence`) because the strongest remaining uncertainty is whether transport resolution plus coalescence can preserve saturation while improving partition and swelling terms.

## Starting D2_Dv_coalescence_escape

- Trials requested: 250
- Output dir: `UN_M7_v5_codex_results/D2_Dv_coalescence_escape`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed D2_Dv_coalescence_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis D2_Dv_coalescence_escape

- Best score: `capture_only_v5_codex_trial_00235` with `score_total=2.0666`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=176.5 nm`, `p_d/p_eq1600=1.006`, `bulk1600=62.13%`, `qgb1600=6.442%`.
- Parameter directions in top set: Dv_scale=2.8, Dg_scale=0.402, D2_xe_scale=3.93, b_scale=0.43, gb_scale=3.95, gd_scale=3.79, coalescence_d_scale=0.359, capture_scale=0.236.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Decision After D2_Dv_coalescence_escape

- D2 is not the best score basin, but it is one of the most physically attractive radius/pressure candidates: best score `2.06660`, behind B (`1.99000`), A (`2.04946`), and C4 (`2.05325`), but better than C2/C5/D1.
- Best D2 has clear high-temperature radius saturation: `R_d(2000 K)=176 nm`, `R_d(1900->2000)=17 nm`, with centered pressure `p_d/p_eq(1600 K)=1.006`.
- The trade-off is that partition remains only moderate: `bulk_gas(1600 K)=62.1%`, `qgb_gas(1600 K)=6.44%`, and `N_d(1600 K)=1.37e19`. It improves radius/pressure more than gas partition.
- Top D2 direction is coherent and different from D1: high `Dv_scale`, very low `Dg_scale`, high `D2_xe_scale`, low `b_scale`, high `gb_scale/gd_scale`, low coalescence, and low capture.
- Interpretation: coupling `Dv` and coalescence can produce the cleanest saturating radius found so far, but still does not solve the low/intermediate-temperature bulk-gas deficit. This is a strong `best_radius_saturated` candidate to keep for final reruns.
- Strategy: proceed to D3 (`K_d+rho_d+coalescence`) to test whether the D2 physical basin can be improved by moving microstructure with coalescence, especially for partition without losing the radius plateau.

## Starting D3_Kd_rhod_coalescence_escape

- Trials requested: 250
- Output dir: `UN_M7_v5_codex_results/D3_Kd_rhod_coalescence_escape`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed D3_Kd_rhod_coalescence_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis D3_Kd_rhod_coalescence_escape

- Best score: `capture_only_v5_codex_trial_00206` with `score_total=2.1440`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=202.1 nm`, `p_d/p_eq1600=1.110`, `bulk1600=55.90%`, `qgb1600=9.162%`.
- Parameter directions in top set: Dv_scale=0.481, Dg_scale=1.33, D2_xe_scale=3.52, b_scale=0.309, gb_scale=3.76, gd_scale=1.50, coalescence_d_scale=0.717, capture_scale=0.123.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Decision After D3_Kd_rhod_coalescence_escape

- D3 is not competitive: best score `2.14398`, clearly worse than B (`1.99000`), A (`2.04946`), C4 (`2.05325`), D2 (`2.06660`), C2, C5, and D1.
- Best D3 remains physically acceptable and submicrometric (`R_d(2000 K)=202 nm`, `R_d(1900->2000)=32 nm`), but partition is weaker: `bulk_gas(1600 K)=55.9%`, `qgb_gas(1600 K)=9.16%`.
- Its best radius-saturated candidate is similar (`R_d(2000 K)=181 nm`, `R_d(1900->2000)=21 nm`) but still worse in score (`2.16228`) and does not repair partition.
- Directionally, D3 prefers low `Dv_scale`, mixed/high `Dg_scale`, high `D2_xe_scale`, low `b_scale`, high `gb_scale`, modest-high `gd_scale`, near/mixed coalescence, and low capture. This differs from D2 mainly by lowering `Dv_scale` and raising `Dg_scale`, which appears to hurt the global objective.
- Interpretation: moving `K_d/rho_d` with coalescence can preserve a small radius, but it does not improve the gas partition/score trade-off. D2 remains the better physical-radius candidate.
- Strategy: proceed to D4 (`capture+gd`) as the last two-family escape, because low capture has repeatedly appeared in top sets and may need coordinated `gd_scale` movement.

- Best score: `capture_only_v5_codex_trial_00206` with `score_total=2.144`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=202.1 nm`, `p_d/p_eq1600=1.11`, `bulk1600=55.9%`, `qgb1600=9.162%`.
- Parameter directions in top set: Dv_scale=0.481, D2_xe_scale=3.52, b_scale=0.309, gb_scale=3.76, gd_scale=1.5, capture_scale=0.123.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Starting D4_capture_gd_escape

- Trials requested: 250
- Output dir: `UN_M7_v5_codex_results/D4_capture_gd_escape`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed D4_capture_gd_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis D4_capture_gd_escape

- Best score: `capture_only_v5_codex_trial_00228` with `score_total=2.0971`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=310.8 nm`, `p_d/p_eq1600=1.062`, `bulk1600=62.30%`, `qgb1600=7.476%`.
- Parameter directions in top set: Dv_scale=1.47, Dg_scale=1.64, D2_xe_scale=3.94, b_scale=0.314, gb_scale=3.07, gd_scale=3.35, coalescence_d_scale=0.717, capture_scale=0.144.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Decision After D4_capture_gd_escape

- D4 is not competitive: best score `2.09709`, worse than D2 (`2.06660`), C4 (`2.05325`), A (`2.04946`), and B (`1.99000`).
- Best D4 is physical but less radius-saturated than D2: `R_d(2000 K)=311 nm`, `R_d(1900->2000)=90 nm`, `p_d/p_eq(1600 K)=1.06`, `bulk_gas(1600 K)=62.3%`, `qgb_gas(1600 K)=7.48%`.
- The best radius-saturated D4 candidate has a better plateau (`R_d(2000 K)=162 nm`, `R_d(1900->2000)=17 nm`) but worse score (`2.14415`) and high `N_d(1600 K)=1.96e19`.
- Directionally, D4 prefers high `Dv_scale/Dg_scale/D2_xe_scale`, low `b_scale`, high `gb_scale/gd_scale`, near-mixed coalescence, and low capture. It rejects the high-capture escape: large `capture_scale` appears mostly in weak candidates.
- Interpretation: capture must remain low/moderate, but moving capture with `gd_scale` alone does not solve the partition/radius trade-off. D2 remains the best radius-saturated physical block; B remains the best total-score block; C4 remains an important pressure/radius compromise.
- Strategy: all two-family escape studies are complete. Proceed to final all-wide exploratory study, using it as a last check for a combined basin that can beat B while keeping D2-like radius saturation.

- Best score: `capture_only_v5_codex_trial_00228` with `score_total=2.0971`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=310.8 nm`, `p_d/p_eq1600=1.062`, `bulk1600=62.3%`, `qgb1600=7.476%`.
- Parameter directions in top set: Dv_scale=1.47, Dg_scale=1.64, D2_xe_scale=3.94, b_scale=0.314, gb_scale=3.07, gd_scale=3.35, capture_scale=0.144.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Starting E_all_wide_exploratory

- Trials requested: 500
- Output dir: `UN_M7_v5_codex_results/E_all_wide_exploratory`
- Final reruns requested now: 0
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed E_all_wide_exploratory

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Analysis E_all_wide_exploratory

- Best score: `capture_only_v5_codex_trial_00496` with `score_total=2.0018`.
- Physical candidate found by current classifier: yes.
- Best score diagnostics: `Rd2000=213.2 nm`, `p_d/p_eq1600=1.105`, `bulk1600=63.2%`, `qgb1600=7.011%`.
- Parameter directions in top set: D2_xe_scale=0.255, b_scale=0.056, gb_scale=0.68, gd_scale=0.467, coalescence_d_scale=0.394.
- What improved / worsened must be judged against previous blocks in the aggregate report; local next step: continue only if radius/partition/pressure trade-off improved; otherwise change strategy.

## Starting B_standard_wide

- Trials requested: 0
- Output dir: `UN_M7_v5_codex_results/B_standard_wide`
- Final reruns requested now: 1
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed B_standard_wide

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Starting E_all_wide_exploratory

- Trials requested: 0
- Output dir: `UN_M7_v5_codex_results/E_all_wide_exploratory`
- Final reruns requested now: 1
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed E_all_wide_exploratory

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Starting D2_Dv_coalescence_escape

- Trials requested: 0
- Output dir: `UN_M7_v5_codex_results/D2_Dv_coalescence_escape`
- Final reruns requested now: 1
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed D2_Dv_coalescence_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Starting C4_microstructure_escape

- Trials requested: 0
- Output dir: `UN_M7_v5_codex_results/C4_microstructure_escape`
- Final reruns requested now: 1
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed C4_microstructure_escape

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Starting A_nominal_near

- Trials requested: 0
- Output dir: `UN_M7_v5_codex_results/A_nominal_near`
- Final reruns requested now: 1
- Plots enabled: False
- Range changes are limited to this Codex wrapper; v5 scoring/equations are reused.

## Completed A_nominal_near

- Fast Optuna run finished.
- Next step: run block analysis and decide whether to continue or change strategy.

## Final Wrap-Up

- All planned blocks were completed: A nominal-near, B standard wide, C one-parameter-family escapes, D two-parameter-family escapes, and E all-wide exploratory.
- Total fast trials completed: 3000.
- Final reruns were performed after all block analyses on selected physically interesting candidates only.
- Final ranking among selected candidates:
  - B/trial_00629: `score_total=1.9131`, `Rd2000=266.2 nm`, `Rd1900->2000=57.1 nm`.
  - E/trial_00496: `score_total=1.9187`, `Rd2000=212.8 nm`, `Rd1900->2000=28.4 nm`.
  - C4/trial_00196: `score_total=1.9517`, `Rd2000=222.1 nm`, `Rd1900->2000=38.8 nm`.
  - D2/trial_00235: `score_total=1.9561`, `Rd2000=173.6 nm`, `Rd1900->2000=16.3 nm`.
  - A/trial_00264: `score_total=2.0133`, `Rd2000=253.6 nm`, `Rd1900->2000=50.8 nm`.
- Final conclusion: B remains best by score, E is the most interesting all-wide physical alternative, and D2 is the cleanest radius-saturation/pressure candidate. v5 avoids the v3/v4 micrometric `R_d(2000 K)` blow-up for selected physical candidates, but the low/intermediate-T gas partition trade-off is still unresolved.
