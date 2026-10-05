# Benchmark gas partition separato

Benchmark Rizk di modello, non esperimenti. Il fronte con gas usa soltanto la RMS dei compartimenti matrice/bulk/dislocazioni. RMS completa, grain-face e FGR restano diagnostiche. Nessuna aggregazione con swelling o R_d. La curva 1.1% FIMA è condivisa AP3.2/AP3.8; a 1.3% non è disponibile. Le metriche aggregate derivano dai residui salvati, con conteggi uguali per pin e canale. Dettagli per caso/canale in model_benchmark_summary.csv.

| Candidato | RMS matrice/bulk/disl pp | Compartimenti / baseline | RMS cinque canali diagnostica pp | Cinque canali / baseline | benchmark_matrix_gas_percent_RMSE_pp | benchmark_bulk_gas_percent_RMSE_pp | benchmark_dislocation_gas_percent_RMSE_pp | benchmark_grainface_gas_percent_RMSE_pp | benchmark_release_gas_percent_RMSE_pp | gas_partition_RMSE_pp_AP3.2 | gas_partition_RMSE_pp_AP3.8 | gas_partition_RMSE_pp_ANP6 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| baseline | 3.077 | 1 | 2.998 | 1 | 3.825 | 2.574 | 2.672 | 3.764 | 1.542 | 3.286 | 3.268 | 2.342 |
| f_n_low | 53.24 | 17.3 | 41.63 | 13.89 | 3.393 | 70.37 | 59.49 | 10.98 | 6.536 | 44.13 | 44.13 | 36.12 |
| f_n_high | 7.832 | 2.546 | 6.158 | 2.054 | 3.881 | 9.317 | 9.063 | 2.174 | 0.94 | 7.167 | 7.311 | 2.994 |
| K_d_low | 3.492 | 1.135 | 3.254 | 1.085 | 3.826 | 3.495 | 3.119 | 3.744 | 1.538 | 3.64 | 3.558 | 2.421 |
| K_d_high | 2.822 | 0.9172 | 2.855 | 0.9522 | 3.825 | 1.336 | 2.733 | 3.8 | 1.554 | 3.059 | 3.145 | 2.281 |
| rho_d_low | 9.97 | 3.241 | 7.964 | 2.656 | 3.824 | 10.78 | 12.94 | 4.014 | 1.673 | 9.312 | 9.414 | 3.868 |
| rho_d_high | 18 | 5.852 | 14.04 | 4.683 | 3.829 | 22.13 | 21.64 | 3.288 | 1.488 | 16.53 | 16.23 | 7.396 |
| Dv_dislocation_scale_low | 2.991 | 0.9723 | 2.947 | 0.983 | 3.825 | 2.445 | 2.497 | 3.769 | 1.542 | 3.222 | 3.203 | 2.327 |
| Dv_dislocation_scale_high | 3.105 | 1.009 | 3.015 | 1.006 | 3.825 | 2.614 | 2.73 | 3.763 | 1.542 | 3.308 | 3.29 | 2.346 |
| Dg_dislocation_scale_low | 13.26 | 4.309 | 10.46 | 3.488 | 3.824 | 14.84 | 17.1 | 4.079 | 1.728 | 12.32 | 12.37 | 4.823 |
| Dg_dislocation_scale_high | 19.43 | 6.315 | 15.13 | 5.048 | 3.829 | 23.86 | 23.43 | 3.233 | 1.493 | 17.79 | 17.49 | 8.041 |
| N_gf0_factor_low | 3.077 | 1 | 3.02 | 1.007 | 3.825 | 2.574 | 2.672 | 3.92 | 1.356 | 3.296 | 3.278 | 2.399 |
| N_gf0_factor_high | 3.077 | 1 | 2.976 | 0.9926 | 3.825 | 2.574 | 2.672 | 3.605 | 1.7 | 3.258 | 3.242 | 2.333 |
