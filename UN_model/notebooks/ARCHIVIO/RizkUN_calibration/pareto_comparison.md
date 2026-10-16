# Confronto separato e Pareto — valutazione corretta

Swelling: stesso score precedente, media delle NRMSE separate sui quattro pin. R_d secondario forte: RMSE su **5 punti sperimentali AP3.4 con T ≤1600 K**. R_d high-T: **4 punti**, sola diagnostica, peso zero nella selezione. N_d low/high-T: diagnostiche qualitative con RMSE log10 e bias separati, senza peso di selezione. La sottostima high-T di R_d e la sovrastima high-T di N_d sono trattate come comportamento della formulazione accettabile secondo il criterio interpretativo fornito dall’utente per la Fig.4.7 Matthews 2025; non si tenta di eliminarle.

Gas partition: RMS degli scarti dei **tre compartimenti matrice/bulk/dislocazioni** come asse separato di coerenza, senza combinazione con gli errori sperimentali. RMS delle cinque frazioni completa, canale grain-face e FGR sono riportati separatamente come diagnostiche e non entrano nel fronte di selezione. Sono 333 confronti core per candidato (3 pin × 3 canali × 37 temperature già interpolate), e 555 nella diagnostica completa a cinque canali. Stesse unità (pp), stesso numero di punti per canale/pin, nessun peso di fit. Il CSV riporta anche RMS completo a cinque frazioni, tutti i canali separati e i rapporti alla baseline. Questi benchmark sono di modello, non dati sperimentali. Le curve 1.1% FIMA sono condivise da AP3.2/AP3.8; non sono disponibili a 1.3% FIMA. Nessuna soglia arbitraria di accettazione gas viene applicata: i peggioramenti sono esposti numericamente.

Pareto esperimenti: minimizzazione delle sole colonne swelling e R_d ≤1600 K. Pareto con gas: le stesse due colonne più RMS dei tre compartimenti matrice/bulk/dislocazioni, con dominanza componente per componente e nessuna somma pesata. Nessun ordinamento totale è imposto. Un candidato è dominato se un altro non peggiora nessuna colonna e ne migliora almeno una (tolleranza relativa 1e-10). **N_d, R_d high-T, pressioni e altri guardrail non entrano nei fronti né nei ranghi.** Un punto sul fronte può avere swelling o gas partition inaccettabilmente peggiori: essere non dominato non significa essere raccomandato.

| Candidato | Swelling NRMSE | R_d ≤1600 K RMSE nm | Gas matrice/bulk/disl RMS pp | Gas core / baseline | R_d >1600 K diagnostica nm | N_d ≤1600 K diagnostica dex | N_d >1600 K diagnostica dex | Pareto esperimenti | Pareto con gas |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| baseline | 0.3836 | 24.66 | 3.077 | 1 | 59.31 | 0.471 | 0.5598 | False | True |
| f_n_low | 2.809 | 27.91 | 53.24 | 17.3 | 59.36 | 0.5092 | 0.4394 | False | False |
| f_n_high | 0.7477 | 39.99 | 7.832 | 2.546 | 87.27 | 0.4631 | 0.5808 | False | False |
| K_d_low | 0.3685 | 21.89 | 3.492 | 1.135 | 21.84 | 0.9123 | 0.5173 | True | True |
| K_d_high | 0.6511 | 50.85 | 2.822 | 0.9172 | 102.7 | 0.3001 | 0.9686 | False | True |
| rho_d_low | 0.8203 | 28.65 | 9.97 | 3.241 | 68.65 | 0.8946 | 0.5038 | False | False |
| rho_d_high | 0.6109 | 21.17 | 18 | 5.852 | 48.48 | 0.2785 | 0.7021 | True | True |
| Dv_dislocation_scale_low | 0.4159 | 27.01 | 2.991 | 0.9723 | 59.49 | 0.4666 | 0.5599 | False | True |
| Dv_dislocation_scale_high | 0.3805 | 24.51 | 3.105 | 1.009 | 59.29 | 0.4723 | 0.5598 | False | True |
| Dg_dislocation_scale_low | 0.9835 | 66.97 | 13.26 | 4.309 | 125.5 | 0.4578 | 0.5914 | False | False |
| Dg_dislocation_scale_high | 1.234 | 9.36 | 19.43 | 6.315 | 13.4 | 0.4882 | 0.4739 | True | True |
| N_gf0_factor_low | 0.3836 | 24.66 | 3.077 | 1 | 59.31 | 0.471 | 0.5598 | False | True |
| N_gf0_factor_high | 0.3836 | 24.66 | 3.077 | 1 | 59.31 | 0.471 | 0.5598 | False | True |
