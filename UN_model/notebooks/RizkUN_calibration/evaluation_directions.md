# Direzioni della valutazione OAT aggiornata

Variazioni rispetto alla baseline: Δ negativo significa minore errore, non necessariamente miglioramento di tutti i pin. Ogni prova varia un solo parametro; non viene stimata la risposta delle combinazioni. Le direzioni fisiche restano nelle 720 righe di direction_table.csv.

| Parametro | Livello | Valore | Δ swelling NRMSE | Δ R_d ≤1600 RMSE nm | Δ gas RMS pp | Gas core / baseline |
| --- | --- | --- | --- | --- | --- | --- |
| f_n | low | 1e-07 | 2.425 | 3.25 | 38.63 | 17.3 |
| f_n | high | 0.01 | 0.3641 | 15.33 | 3.16 | 2.546 |
| K_d | low | 1e+05 | -0.01506 | -2.774 | 0.2563 | 1.135 |
| K_d | high | 1e+06 | 0.2675 | 26.19 | -0.1434 | 0.9172 |
| rho_d | low | 1e+13 | 0.4367 | 3.99 | 4.966 | 3.241 |
| rho_d | high | 6e+13 | 0.2273 | -3.491 | 11.04 | 5.852 |
| Dv_dislocation_scale | low | 1 | 0.03226 | 2.349 | -0.05092 | 0.9723 |
| Dv_dislocation_scale | high | 30 | -0.00313 | -0.1512 | 0.01685 | 1.009 |
| Dg_dislocation_scale | low | 1 | 0.5999 | 42.31 | 7.46 | 4.309 |
| Dg_dislocation_scale | high | 30 | 0.8501 | -15.3 | 12.14 | 6.315 |
| N_gf0_factor | low | 0.1 | 0 | 0 | 0.02189 | 1 |
| N_gf0_factor | high | 10 | 0 | 0 | -0.02217 | 1 |

