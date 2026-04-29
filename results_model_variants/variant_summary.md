# UN model variants summary

Run mode: `MODEL_VARIANTS`.

Fixed Rizk parameters were kept unchanged: `grain_radius = 6e-6`, `fission_rate = 5e19`, `xe_yield = 0.24`, `rho_d = 3e13`, `gamma_b = 1.11`, `omega_fg = 8.5e-29`.

Main score is P2-only: Fig. 3 is compared only with `swelling_d_percent` for `T <= 1700 K`; Fig. 7 and Fig. 8 constrain `Nd` and `Rd_nm`; pressure is penalized only from 1200 to 1700 K. Total intragranular swelling and high-temperature blow-up are diagnostics.

Best variant for Fig. 3 as dislocation swelling: `M6_phi_plus_capture` with `score_main = 1.26898`.

## Best By Model

| model | variant | score | sw_d | Rd log | Nd log | pressure | highT diag | f_n | K_d | g_d_scale | Rizk/coherence |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|
| M0 | M0_baseline | 1.33753 | 0.919747 | 0.107743 | 0.489089 | 0 | 3.30721 | 1e-10 | 300000 | 0.5 | yes |
| M1 | M1_rescore_baseline | 1.33753 | 0.919747 | 0.107743 | 0.489089 | 0 | 3.30721 | 1e-10 | 300000 | 0.5 | yes |
| M2 | M2_nucleation_mass | 1.33753 | 0.91975 | 0.107743 | 0.489089 | 0 | 3.30721 | 1e-10 | 300000 | 0.5 | diagnostic |
| M3 | M3_phi_resolution_bulk_and_dislocation | 1.27636 | 0.838865 | 0.131451 | 0.489885 | 0.0127869 | 4.83826 | 3e-07 | 300000 | 0.5 | diagnostic |
| M4 | M4_bulk_dislocation_capture | 1.28393 | 0.813516 | 0.186797 | 0.485222 | 0 | 0.282971 | 1e-09 | 500000 | 1 | barani-like |
| M5 | M5_nucleation_mass_plus_capture | 1.28393 | 0.813518 | 0.186797 | 0.485222 | 0 | 0.282971 | 1e-09 | 500000 | 1 | diagnostic+barani-like |
| M6 | M6_phi_plus_capture | 1.26898 | 0.835877 | 0.125199 | 0.488246 | 0.0184505 | 5.05507 | 3e-07 | 300000 | 0.5 | diagnostic+barani-like |

## Interpretation

1. The best P2/dislocation swelling variant is `M6_phi_plus_capture`. Its Fig. 3 term is `score_swd = 0.835877`.
2. Its microstructure terms are `score_Rd = 0.125199` and `score_Nd = 0.488246`; these should be read together because a good swelling fit alone can hide a bad radius-density partition.
3. Its pressure diagnostic is `score_pressure = 0.0184505`; lower means bulk and dislocation bubbles stay closer to near-equilibrium in the 1200-1700 K range.
4. Coherence label: `diagnostic+barani-like`. Variants marked `diagnostic` or `barani-like` are useful tests, but are not pure Rizk-base equations.

## Variant Notes

- `M0_baseline`: Rizk-coherent baseline: no nucleation gas mass source, no phi in gas re-solution, no bulk-dislocation capture.
- `M1_rescore_baseline`: Same physics as M0; this isolates the effect of the P2-only score.
- `M2_nucleation_mass`: Diagnostic mass-closure test: dc/dt gets -2 nu_b and dm_b/dt gets +2 nu_b; phi remains only in dN_b/dt.
- `M3_phi_resolution_bulk_only`: Diagnostic re-solution test: phi modifies gas re-solution from bulk bubbles, while N_b destruction still uses b_b phi_b.
- `M3_phi_resolution_bulk_and_dislocation`: Diagnostic re-solution test: phi modifies gas re-solution from bulk and dislocation bubbles.
- `M4_bulk_dislocation_capture`: Barani-like extension, not in Rizk base: growing dislocation-bubble capture volume removes bulk bubbles and transfers bulk gas/vacancies to dislocation bubbles.
- `M5_nucleation_mass_plus_capture`: Combined diagnostic mass-closure and Barani-like capture extension.
- `M6_phi_plus_capture`: Combined diagnostic phi re-solution and Barani-like capture extension.

PNG files are written as `M0_swelling.png`, `M1_swelling.png`, etc. For M3, the best of the two phi-resolution options is used for `M3_swelling.png`.
