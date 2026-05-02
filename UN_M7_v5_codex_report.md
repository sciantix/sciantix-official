# UN M7 v5 Codex Calibration Report

All Codex outputs for this run are under `UN_M7_v5_codex_results/`. Original calibration scripts and notebooks were not modified.

## Structured Blocks

| block | trials | best fast score | main read |
|---|---:|---:|---|
| A nominal-near | 300 | 2.0495 | good near-nominal physical reference |
| B standard wide | 700 | 1.9900 | best total score before final rerun |
| C1 diffusion | 200 | 2.0862 | smaller/saturating radius, weaker partition |
| C2 resolution | 200 | 2.0624 | physical but pressure/radius trade-off not as good |
| C3 trapping | 200 | 2.1628 | trapping alone not competitive |
| C4 microstructure | 200 | 2.0532 | strong pressure/radius compromise |
| C5 coalescence/capture | 200 | 2.0841 | useful low coalescence/capture direction |
| D1 b+gd | 250 | 2.0911 | small radius possible, partition still weak |
| D2 Dv+coalescence | 250 | 2.0666 | best radius saturation/pressure candidate |
| D3 Kd+rho_d+coalescence | 250 | 2.1440 | radius ok, partition weak |
| D4 capture+gd | 250 | 2.0971 | not enough alone |
| E all-wide | 500 | 2.0018 | new near-B family with flatter high-T radius |

Total fast trials: 3000.

## Final Reruns

| role | block/trial | final score | Rd2000 nm | dRd 1900-2000 nm | p_d/p_eq 1600 | bulk 1600 % | qgb 1600 % |
|---|---|---:|---:|---:|---:|---:|---:|
| best_score_total | B/trial_00629 | 1.9131 | 266.2 | 57.1 | 1.121 | 65.7 | 4.42 |
| best_all_wide | E/trial_00496 | 1.9187 | 212.8 | 28.4 | 1.104 | 61.3 | 9.17 |
| best_radius_saturated | D2/trial_00235 | 1.9561 | 173.6 | 16.3 | 1.007 | 60.8 | 8.04 |
| best_microstructure | C4/trial_00196 | 1.9517 | 222.1 | 38.8 | 1.011 | 61.3 | 9.52 |
| near_nominal_ref | A/trial_00264 | 2.0133 | 253.6 | 50.8 | 1.048 | 60.8 | 10.35 |

## Classification

- `best_score_total`: B/trial_00629. It remains the lowest final score, mainly because it keeps better bulk gas at 1600 K.
- `best_physical`: E/trial_00496 or B/trial_00629 depending priority. B wins score; E has a cleaner high-T radius profile.
- `best_radius_saturated`: D2/trial_00235. It has `Rd2000=173.6 nm` and only `16.3 nm` growth from 1900 to 2000 K.
- `best_partition`: B/trial_00629 among selected final reruns; B block also contains B/trial_00391 as a partition-focused fast candidate, but with larger radius.
- `best_pressure`: D2/trial_00235 and C4/trial_00196 are closest to pressure equilibrium.
- `best_rizk_near`: near-Rizk candidates were not selected because the aggregate classifier consistently found poor partition or high-T radius failures.
- `rejected_by_radius`: examples include E/trial_00043 and B/trial_00051, which go back toward micrometric high-T radius.
- `rejected_by_pressure`: examples include E/trial_00079 and B/trial_00039, with dislocation pressure far above equilibrium.
- `rejected_by_partition`: examples include E/trial_00119 and several near-Rizk candidates; they can keep radius small but partition is not acceptable.
- `rejected_by_absurd_parameters`: not a dominant final category, but extreme coalescence/capture/trapping combinations were indirectly filtered by radius, pressure, or partition penalties.

## Scaling Factor Takeaway

The useful calibrated scaling factors are not tiny perturbations around 1. Across the physically interesting candidates:

- `b_scale` wants to be low, typically `0.05-0.44`, often near the lower bound in B/E.
- `coalescence_d_scale` is low/moderate, about `0.36-0.60`.
- `capture_scale` ranges from low to high depending family, about `0.24-2.39`.
- `Dg_scale` is often reduced in B/D2/C4, about `0.40-1.29`.
- `Dv_scale` can be near/high, about `0.92-2.80` in the most useful families.
- `D2_xe_scale` is family-dependent: low in B/E (`0.22-0.27`), high in D2 (`3.97`), near in A/C4 (`0.68-1.31`).
- `gb_scale` and `gd_scale` are not universal: B/D2 need high trapping scales, E needs low trapping scales, C4 splits the difference.

So, for calibration, deviations of factors of `2-10` are scientifically appearing in the regression searches, and `b_scale` in particular is often about `20x` below nominal. The final physically selected candidates avoid the v3/v4 micrometric failure: `R_d(2000 K)` is about `174-266 nm`, not `1-2 micrometers`.

## Scientific Conclusion

v5 can produce acceptable P2 swelling/Rd/Nd + pressure + submicrometric saturating `R_d(T)`. The remaining unresolved trade-off is gas partition: candidates that improve high-T saturation and pressure tend to give lower bulk gas and higher qgb at low/intermediate temperature. B is still the best score candidate; E is the most interesting new candidate because it nearly matches B while flattening `R_d(T)` much more strongly.
