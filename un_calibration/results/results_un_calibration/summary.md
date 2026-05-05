# UN calibration summary

Pipeline generated from the long `# CONFIGURAZIONE GENERALE` notebook cell and the AI calibration instructions.

Fixed/default choices: `GRAIN_RADIUS = 6.0e-6 m`, `FISSION_RATE = 5.0e19`, `bulk_seed_radius_nm = 0.0` for the main sweep, `xe_yield = 0.24`, and SCIANTIX-like old-step gas-solver coefficients. `g_d_scale` multiplies only the dislocation trapping rate `g_d`, not `g_b`.

Scores use Fig. 3 against both `swelling_d_percent` and `swelling_ig_percent`, Fig. 7 against `Nd`, Fig. 8 against `Rd_nm`, a medium-temperature pressure penalty on `p_b/p_eq` and `p_d/p_eq`, and a high-temperature blow-up penalty.

## Baseline nominal

- `f_n`: 1e-06
- `K_d`: 500000
- `g_d_scale`: 1
- `seed_nm`: 0
- `score_main`: 3.42919
- `score_swd`: 2.02859
- `score_swig`: 1.11413
- `score_Nd`: 0.518587
- `score_Rd`: 0.488734
- `score_pressure`: 0
- `score_highT_penalty`: 0
- `probe_swelling_d_1600_13`: 0.281007
- `probe_swelling_ig_1600_13`: 1.43135
- `probe_Nd_1600_13`: 1.48289e+19
- `probe_Rd_nm_1600_13`: 35.632

## Best candidate if Fig. 3 is P2/dislocation swelling

- `f_n`: 1e-08
- `K_d`: 200000
- `g_d_scale`: 0.5
- `seed_nm`: 0
- `score_main`: 2.80725
- `score_swd`: 1.65577
- `score_swig`: 0.942223
- `score_Nd`: 0.571154
- `score_Rd`: 0.229964
- `score_pressure`: 0
- `score_highT_penalty`: 0.00438011
- `probe_swelling_d_1600_13`: 0.741398
- `probe_swelling_ig_1600_13`: 2.27211
- `probe_Nd_1600_13`: 5.82262e+18
- `probe_Rd_nm_1600_13`: 67.238

## Best candidate if Fig. 3 is total intragranular swelling

- `f_n`: 1e-08
- `K_d`: 200000
- `g_d_scale`: 0.5
- `seed_nm`: 0
- `score_alt_total_ig`: 1.75135
- `score_swd`: 1.65577
- `score_swig`: 0.942223
- `score_Nd`: 0.571154
- `score_Rd`: 0.229964
- `score_pressure`: 0
- `score_highT_penalty`: 0.00438011
- `probe_swelling_d_1600_13`: 0.741398
- `probe_swelling_ig_1600_13`: 2.27211
- `probe_Nd_1600_13`: 5.82262e+18
- `probe_Rd_nm_1600_13`: 67.238

## Notes

- `g_d_scale != 1` is an empirical trapping correction, not an original Rizk parameter.
- `q_gb` is only a grain-face balance term here; the model still lacks explicit grain-boundary bubbles, interconnection, and release.
- Results above about 1800 K should be read cautiously because missing release/intergranular physics can make dislocation bubbles grow too aggressively.
- PNG plots written: `True`.
