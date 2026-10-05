# Refinement monodimensionale di K_d

Eseguiti soltanto K_d=1.5e5, 2.0e5, 2.5e5: 3 × 4 pin × 19 temperature = **228 nuovi punti**. Baseline K_d=3e5 e controllo K_d=1e5 riutilizzati dai risultati OAT: **nessun rerun dei due controlli**. Ogni altro campo del candidato è identico alla baseline: f_n=5.5e-4, rho_d=3e13, Dv_dislocation_scale=10, Dg_dislocation_scale=13, N_gf0_factor=1. Tutti i switches, la formulazione e gli altri parametri restano quelli originali. DT_H=1 h, N_MODES=40, T=900–1800 K con passo 50 K; fission rate specifici dei quattro pin invariati.

Nessuno score composto. Swelling prioritario: stessa media delle NRMSE per pin usata nell’OAT, con tutti i dati sperimentali in dominio. R_d secondario forte: 5 punti sperimentali AP3.4 con T ≤1600 K. R_d >1600 K e N_d low/high-T sono **solo diagnostiche**, senza peso nella selezione. Gas core RMS = benchmark dei compartimenti matrice/bulk/dislocazioni (333 residui per candidato, unità pp); il benchmark completo a cinque frazioni (555 residui), FGR, R_gf, N_gf e pressioni restano diagnostiche/guardrail. Pareto esperimenti usa solo swelling + R_d low-T; Pareto con gas aggiunge soltanto il benchmark core. Nessuna somma pesata o ranking totale.

Le temperature sperimentali e benchmark sono interpolate sulla stessa griglia 50 K; non si eseguono punti aggiuntivi. Il punto AP3.8 a 899 K è escluso, come prima. Il benchmark 1.1% è condiviso AP3.2/AP3.8; non c’è una curva gas partition a 1.3%. Sono benchmark di modello, non esperimenti. Il CSV R_gf/N_gf Rizk Fig.7/8 resta non disponibile. La sottostima high-T del raggio o sovrastima high-T della density non sono obiettivi da eliminare.

| Candidato | K_d | Swelling NRMSE | R_d ≤1600 RMSE nm | Gas core RMS pp | Gas / baseline | R_d high-T diagnostica nm | N_d low-T diagnostica dex | N_d high-T diagnostica dex | pareto_experiments | pareto_with_gas |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| K_d_low | 1e+05 | 0.3685 | 21.89 | 3.492 | 1.135 | 21.84 | 0.9123 | 0.5173 | False | False |
| K_d_150000 | 1.5e+05 | 0.3057 | 11.83 | 3.341 | 1.086 | 18.87 | 0.7436 | 0.4715 | True | True |
| K_d_200000 | 2e+05 | 0.3132 | 14.65 | 3.232 | 1.05 | 37.05 | 0.6268 | 0.4849 | False | True |
| K_d_250000 | 2.5e+05 | 0.3451 | 19.96 | 3.146 | 1.022 | 49.88 | 0.5393 | 0.5193 | False | True |
| baseline | 3e+05 | 0.3836 | 24.66 | 3.077 | 1 | 59.31 | 0.471 | 0.5598 | False | True |

Swelling per pin: RMSE e bias in punti percentuali, sempre separati. Un miglioramento della media non cancella i casi peggiorati.

| K_d | swelling_rmse_pp_AP3.2 | swelling_rmse_pp_AP3.8 | swelling_rmse_pp_AP3.4 | swelling_rmse_pp_ANP6 | swelling_bias_pp_AP3.2 | swelling_bias_pp_AP3.8 | swelling_bias_pp_AP3.4 | swelling_bias_pp_ANP6 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1e+05 | 1.067 | 0.2691 | 1.593 | 0.4234 | 0.8306 | -0.1175 | 0.5217 | -0.4098 |
| 1.5e+05 | 0.6123 | 0.4562 | 1.099 | 0.6557 | 0.4614 | -0.41 | -0.02153 | -0.5913 |
| 2e+05 | 0.4256 | 0.6878 | 0.9815 | 0.8337 | 0.2251 | -0.6048 | -0.3487 | -0.7276 |
| 2.5e+05 | 0.4022 | 0.8588 | 1.008 | 0.9676 | 0.05694 | -0.7466 | -0.573 | -0.8338 |
| 3e+05 | 0.453 | 0.9886 | 1.078 | 1.072 | -0.07135 | -0.8562 | -0.7395 | -0.9191 |

Coerenza gas: tutti i canali riportati separatamente; FGR/grain-face fuori dal fronte di selezione.

| K_d | gas_core3_RMSE_pp | gas_partition5_RMSE_pp_diagnostic | benchmark_matrix_gas_percent_RMSE_pp | benchmark_bulk_gas_percent_RMSE_pp | benchmark_dislocation_gas_percent_RMSE_pp | benchmark_grainface_gas_percent_RMSE_pp | benchmark_release_gas_percent_RMSE_pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1e+05 | 3.492 | 3.254 | 3.826 | 3.495 | 3.119 | 3.744 | 1.538 |
| 1.5e+05 | 3.341 | 3.16 | 3.826 | 3.189 | 2.948 | 3.75 | 1.538 |
| 2e+05 | 3.232 | 3.092 | 3.826 | 2.95 | 2.827 | 3.756 | 1.539 |
| 2.5e+05 | 3.146 | 3.04 | 3.826 | 2.749 | 2.738 | 3.76 | 1.541 |
| 3e+05 | 3.077 | 2.998 | 3.825 | 2.574 | 2.672 | 3.764 | 1.542 |

Bias e pressioni sono diagnostiche: bias raggio positivo = sovrastima; bias log10 N_d positivo = sovrastima. Pressioni finite e bilancio conservato non certificano validità fisica.

| K_d | Rd_bias_le1600_nm_AP3.4 | Rd_bias_gt1600_diagnostic_nm_AP3.4 | Nd_log10_bias_le1600_diagnostic | Nd_log10_bias_gt1600_diagnostic | p_gf_max | p_gf_over_eq_max | max_abs_gas_balance_error_pp |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1e+05 | 21.04 | 19.13 | -0.8756 | -0.2246 | 8.868e+11 | 2.472e+04 | 4.263e-14 |
| 1.5e+05 | 5.378 | -15.31 | -0.697 | -0.02912 | 8.868e+11 | 2.473e+04 | 4.263e-14 |
| 2e+05 | -4.622 | -35.08 | -0.57 | 0.1068 | 8.868e+11 | 2.474e+04 | 5.684e-14 |
| 2.5e+05 | -11.74 | -48.25 | -0.4715 | 0.2109 | 8.868e+11 | 2.474e+04 | 4.263e-14 |
| 3e+05 | -17.17 | -57.81 | -0.3909 | 0.2953 | 8.868e+11 | 2.474e+04 | 4.263e-14 |

Lettura gerarchica: **K_d=2e5** migliora lo swelling su tutti e quattro i pin (RMSE AP3.2/AP3.8/AP3.4/ANP6: 0.4256/0.6878/0.9815/0.8337 pp, contro 0.4530/0.9886/1.0782/1.0718 pp della baseline), porta R_d low-T da 24.66 a **14.65 nm** e aumenta il RMS gas core soltanto da 3.077 a **3.232 pp** (+5.03%). È il valore più interessante per un miglioramento distribuito sui quattro pin, senza dover variare un secondo parametro.

K_d=2.5e5 è più conservativo: migliora anch’esso tutti i pin, ha gas core +2.25% e R_d low-T 19.96 nm, ma lascia un residuo maggiore sugli altri tre pin rispetto a 2e5. K_d=1.5e5 dà il minimo R_d low-T (11.83 nm) e la minima media swelling (0.3057), ma AP3.2 peggiora da 0.4530 a 0.6123 pp e AP3.4 da 1.0782 a 1.0988 pp. Il fronte sperimentale aggregato lo preferisce a 2e5, mentre il dettaglio per pin mostra perché la media non determina da sola la scelta. Il confronto non viene convertito in uno score composto.

Il bias R_d low-T varia in modo ordinato: +21.04 nm a 1e5, +5.38 nm a 1.5e5, −4.62 nm a 2e5, −11.74 nm a 2.5e5, −17.17 nm alla baseline. I valori intermedi riducono il grande sbilanciamento del controllo 1e5. Nessun vantaggio sul raggio high-T o su N_d viene usato per promuovere un candidato. Il controllo 1e5 è dominato da 1.5e5 sui tre criteri aggregati swelling/raggio low-T/gas core, ma conserva RMSE swelling più piccoli su AP3.8/ANP6: le priorità restano esplicite.

Tutti i nuovi valori rispettano N_gf_vol < N_d < N_b su tutti i 76 punti per candidato. Restano violazioni persistenti dell’ordine dei raggi nei tre pin AP3.2/AP3.8/AP3.4 a bassa T; ANP6 lo rispetta per tutti i nuovi valori. Le pressioni grain-face restano pressoché identiche alla baseline: nessuna conclusione di validità fisica viene dedotta dall’esito numerico positivo. Il bilancio di gas è conservato entro 5.68e-14 pp, senza valori finali non finiti o negativi nei campi fisici controllati.

Gli ordinamenti confrontano R_gf > R_d > R_b e N_gf_vol < N_d < N_b allo stesso burnup finale. N_gf_vol è la conversione volumetrica, non la densità areale. Persistente = almeno tre temperature consecutive, span ≥100 K. Non vengono controllate qui tutte le storie temporali.

| K_d | case | ordering | persistent_ranges |
| --- | --- | --- | --- |
| 1e+05 | AP3.2 | Rgf>Rd>Rb | 900-1250 K |
| 1e+05 | AP3.2 | Ngf_vol<Nd<Nb | 900-1250 K |
| 1e+05 | AP3.8 | Rgf>Rd>Rb | 900-1250 K |
| 1e+05 | AP3.8 | Ngf_vol<Nd<Nb | 900-1250 K |
| 1e+05 | AP3.4 | Rgf>Rd>Rb | 900-1250 K |
| 1e+05 | AP3.4 | Ngf_vol<Nd<Nb | 900-1250 K |
| 1e+05 | ANP6 | Rgf>Rd>Rb | 1000-1150 K |
| 1e+05 | ANP6 | Ngf_vol<Nd<Nb | 900-1150 K |
| 1.5e+05 | AP3.2 | Rgf>Rd>Rb | 900-1200 K |
| 1.5e+05 | AP3.8 | Rgf>Rd>Rb | 900-1200 K |
| 1.5e+05 | AP3.4 | Rgf>Rd>Rb | 900-1200 K |
| 2e+05 | AP3.2 | Rgf>Rd>Rb | 900-1200 K |
| 2e+05 | AP3.8 | Rgf>Rd>Rb | 900-1200 K |
| 2e+05 | AP3.4 | Rgf>Rd>Rb | 950-1200 K |
| 2.5e+05 | AP3.2 | Rgf>Rd>Rb | 950-1200 K |
| 2.5e+05 | AP3.8 | Rgf>Rd>Rb | 950-1200 K |
| 2.5e+05 | AP3.4 | Rgf>Rd>Rb | 1000-1150 K |
| 3e+05 | AP3.2 | Rgf>Rd>Rb | 1000-1150 K |
| 3e+05 | AP3.8 | Rgf>Rd>Rb | 1000-1150 K |
| 3e+05 | AP3.4 | Rgf>Rd>Rb | 1050-1150 K |

Le pressioni grain-face conservano le elevate sovrapressioni già presenti nella baseline. La soglia descrittiva p/p_eq>1000 serve solo a esporre intervalli ed estremi, non come obiettivo di fit o regola arbitraria di accettazione. I dettagli sono in pressure_diagnostics.csv e negli output completi.

Confronti rispetto a entrambi i controlli: reference_deltas.csv contiene tutte le differenze dei tre nuovi valori rispetto a baseline e K_d=1e5, inclusi i quattro swelling separati. I CSV candidate_summary.csv e comparison_summary.csv contengono rispettivamente i tre nuovi candidati e tutti e cinque i valori confrontati. comparison_runs.csv distingue i 228 punti nuovi dai 152 punti riutilizzati. comparison_points.csv conserva osservazioni, predizioni interpolate, residui e ruoli nella valutazione.

**Nessuna combinazione a due parametri viene proposta o eseguita.** Le precedenti proposte sono sospese in attesa della lettura di questo refinement. Non viene dedotta alcuna performance di una combinazione da questi risultati.
