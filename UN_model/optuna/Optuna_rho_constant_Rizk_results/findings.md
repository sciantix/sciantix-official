# Optuna_rho_constant_Rizk — calibrazione esplorativa

Campagna conclusa al limite autorizzato di 60 trial. Tre strategie distinte sono disponibili per ispezione; non viene scelto un modello finale.

## Configurazione e riproducibilità

Notebook corrente congelato (SHA-256 `014d602c96a2e9db5bf0fb2dae558f4187b5b870cc2da6391d09ca542ec8c7c5`), conservato in `source_notebook.ipynb`; originale verificato invariato. Optuna 4.8.0, NSGA-II, seed 20251005, popolazione 12, SQLite `study.db`. Ask/tell sequenziale; solo i punti fisici vengono distribuiti su 8 processi. Stato del sampler in `sampler.pkl`, checkpoint per trial e per mezzo pin. Lo stesso comando riprende lo studio senza superare 60 trial.

Screening: 900–1800 K, passo 50 K, quattro pin; rerun dei soli A/B/C: passo normale 25 K. In entrambi dt=1 h, N_MODES=40, solver e tolleranze originali. rho_d è costante per tutta la simulazione e per ogni T/burnup. Nessuno switch, scala o legge fisica aggiuntiva.

Verifica della baseline: i 76 punti comuni coincidono con il CSV esistente del notebook corrente su tutte le 129 colonne numeriche confrontate (`baseline_equivalence_validation.json`). Il ricalcolo indipendente e l’esclusione dei dati high-T dagli obiettivi sono verificati in `scoring_validation.json`.

L’adattamento dell’interfaccia aggiunge la densità iniziale grain-face a Candidate/UNParameters e la inoltra all’inizializzazione esistente: nessuna equazione modificata. Gli altri adattamenti riguardano solo esportazione e directory/limiti dei grafici.

| parametro | min | max | CURRENT_BASELINE | campionamento |
| --- | --- | --- | --- | --- |
| f_n | 1e-07 | 0.01 | 0.00055 | log |
| K_d | 100000 | 2e+06 | 300000 | log |
| rho_d | 1e+13 | 7e+13 | 3.5e+13 | log |
| Dv_dislocation_scale | 1 | 30 | 10 | log |
| Dg_dislocation_scale | 1 | 30 | 15 | log |
| NGF_AREAL_0 | 2e+12 | 2e+14 | 1e+13 | log |

| case | burnup | fission_rate | linear_power_kW_m | fuel_diameter_mm |
| --- | --- | --- | --- | --- |
| AP3.2 | 1.1 | 5.76784e+19 | 100 | 8.3 |
| AP3.8 | 1.1 | 6.86374e+19 | 119 | 8.3 |
| AP3.4 | 1.3 | 7.4982e+19 | 130 | 8.3 |
| ANP6 | 3.2 | 7.76068e+19 | 125 | 8 |

Trial totali: 60; completati: 59; falliti: 1. Curve screening salvate: 4560; rerun: 3 × 148 punti.

## Trial esclusi

| trial | solver_failures | nonfinite_values | negative_inventory_values | gas_balance_failures | gross_nonphysical_points |
| --- | --- | --- | --- | --- | --- |
| 26 | 0 | 0 | 0 | 0 | 1 |

Il trial 26 è escluso per ANP6 a 1800 K: swelling totale 105.35%, con grain-face bubble radius di circa 10 µm superiore al raggio del grano 6 µm. Il solver termina e il gas balance è conservato, ma questo stato è palesemente fuori dalla geometria del modello. Le sue curve restano nel CSV; gli obiettivi ricalcolabili sono diagnostici e non lo rendono eleggibile.

## Obiettivi fissi e dati

J_swelling = media dei quattro RMSE P2 in punti percentuali, ciascuno normalizzato dal massimo sperimentale del proprio pin. Non si mescolano i punti di pin diversi. Il punto AP3.8 a 899 K è escluso perché esterno al dominio; restano 10/9/9/10 punti. Gli anchor vicino a 1600 K sono mostrati nei grafici standard, senza duplicarli nello score.

E_R = RMSE dell’errore relativo del raggio; E_N = RMSE dell’errore log10 della densità. J_micro = 0.6 E_R + 0.4 E_N. Solo AP3.4 e T<=1600 K: 5 punti R, 17 punti N. I 4 punti R e 11 punti N sopra 1600 K sono diagnostiche separate e non entrano negli obiettivi.

J_FGR in punti percentuali: media degli errori quadratici dei due pin a 1.1% FIMA, poi media con quelli ANP6 a 3.2%, infine radice. Le curve Rizk digitalizzate native a 25 K vengono confrontate mediante interpolazione del modello solo a 900–1800 K. Stessa ponderazione per gli errori separati delle cinque gas partition. Core = radice della media dei quadrati dei tre RMSE matrix/bulk/dislocation. FGR e partition Rizk sono benchmark di modello, non dati sperimentali. AP3.4 non ha benchmark Fig.9 a 1.3% e non viene confrontato con un burnup inventato.

Non esiste uno score totale. I warning partition >10 pp e strong >20 pp, pressione, ordinamenti e geometria non modificano i tre obiettivi. FAIL è riservato a solver failure, valori principali non finiti, inventari negativi, bilancio gas oltre 0.1 pp o volume totale delle bolle >=100%. La geometria single-size resta un warning. Le verifiche numeriche utilizzano gli stati finali a ogni T; i guard/clipping interni del solver sono quelli originali.

## CURRENT_BASELINE

| trial | J_swelling | J_micro | J_FGR | E_R | E_N | gas_partition_core_RMS | Rd_highT_RMSE_nm | Nd_highT_log10_RMSE | warnings |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 0 | 0.179794 | 0.27505 | 1.69071 | 0.177323 | 0.421641 | 6.60499 | 49.1171 | 0.579111 | grainface_pressure_over_eq_gt100_diagnostic_only |

## Minimi separati e Pareto front

| obiettivo_minimizzato | trial | J_swelling | J_micro | J_FGR | gas_partition_core_RMS |
| --- | --- | --- | --- | --- | --- |
| J_swelling | 14 | 0.176683 | 0.472704 | 2.48819 | 19.135 |
| J_micro | 54 | 1.13102 | 0.153742 | 2.32647 | 36.9155 |
| J_FGR | 25 | 0.625934 | 0.368179 | 1.02901 | 13.038 |

Pareto front: 9 trial non dominati. La dominanza usa soltanto i tre obiettivi, senza partition/pressione/ordering.

| trial | J_swelling | J_micro | J_FGR | E_R | E_N | gas_partition_core_RMS |
| --- | --- | --- | --- | --- | --- | --- |
| 0 | 0.179794 | 0.27505 | 1.69071 | 0.177323 | 0.421641 | 6.60499 |
| 12 | 0.280995 | 0.56811 | 1.0529 | 0.575775 | 0.556611 | 8.77489 |
| 14 | 0.176683 | 0.472704 | 2.48819 | 0.467157 | 0.481025 | 19.135 |
| 25 | 0.625934 | 0.368179 | 1.02901 | 0.192142 | 0.632235 | 13.038 |
| 30 | 0.275718 | 0.258092 | 1.33549 | 0.258835 | 0.256979 | 15.8999 |
| 46 | 0.275718 | 0.258092 | 1.33549 | 0.258835 | 0.256979 | 15.8999 |
| 49 | 0.496764 | 0.225906 | 1.61133 | 0.207779 | 0.253096 | 21.3031 |
| 53 | 0.244862 | 0.273483 | 1.48127 | 0.283761 | 0.258066 | 15.7341 |
| 54 | 1.13102 | 0.153742 | 2.32647 | 0.094399 | 0.242756 | 36.9155 |

## Selezione di strategie diverse

Pool competitivo: J_swelling <= 0.282692, J_micro <= 0.493307, J_FGR <= 4.05574 pp. Sono soglie separate di selezione, applicate dopo lo screening; nessuna modifica allo scoring. Pool: [0, 2, 14, 30, 32, 34, 41, 46, 50, 53, 59]. Comprende punti Pareto e competitivi dominati. Nessun filtro sulla gas partition.

A rappresenta un buon compromesso: migliore swelling nel sottoinsieme con J_micro sotto la mediana del pool. B massimizza la distanza da A; C massimizza la distanza minima da A/B. La distanza euclidea usa tutte e sei le coordinate log10 normalizzate sui rispettivi intervalli [0,1].

| trial | f_n | K_d | rho_d | Dv_dislocation_scale | Dg_dislocation_scale | NGF_AREAL_0 | J_swelling | J_micro | J_FGR | E_R | E_N | gas_partition_core_RMS | pareto |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 0 | 0.00055 | 300000 | 3.5e+13 | 10 | 15 | 1e+13 | 0.179794 | 0.27505 | 1.69071 | 0.177323 | 0.421641 | 6.60499 | True |
| 41 | 9.51111e-05 | 1.62169e+06 | 2.14992e+13 | 1.06313 | 21.2532 | 1.8086e+13 | 0.253481 | 0.425214 | 3.01271 | 0.489351 | 0.329008 | 8.55715 | False |
| 2 | 6.26898e-05 | 1.88269e+06 | 1.89076e+13 | 7.78941 | 21.2532 | 2.36572e+12 | 0.277724 | 0.420505 | 2.77445 | 0.477109 | 0.335597 | 7.88114 | False |

### CANDIDATE_A: trial 0

Parametri relativamente bassi nel range logaritmico: NGF_AREAL_0; alti: f_n, Dv_dislocation_scale, Dg_dislocation_scale. Migliora rispetto alla baseline: nessun obiettivo; trade-off rispetto alla baseline: J_swelling, J_micro, J_FGR. Core partition 6.605 pp. Selezione: rappresentante competitivo con buon compromesso microstrutturale.

Distanze: A–B: 0.930, A–C: 0.791.

Miglioramenti di componenti specifiche rispetto alla baseline: baseline di riferimento.

| label | source_trial | J_swelling | J_micro | J_FGR | E_R | E_N | gas_partition_core_RMS | Rd_highT_RMSE_nm | Nd_highT_log10_RMSE | gas_balance_max_abs_pp | p_gf_over_eq_max | warnings |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CANDIDATE_A | 0 | 0.17814 | 0.275241 | 1.68765 | 0.177654 | 0.421623 | 6.58307 | 49.4966 | 0.579651 | 4.26326e-14 | 45328.3 | grainface_pressure_over_eq_gt100_diagnostic_only |

Raggio grain-face massimo: 1.762 µm, rispetto al raggio del grano congelato 6.0 µm. Anche la scala geometrica, soprattutto C vicino a tale dimensione, resta da ispezionare.

| label | swelling_error_AP3p2 | swelling_error_AP3p8 | swelling_error_AP3p4 | swelling_error_ANP6 | Rd_lowT_RMSE_nm | Nd_lowT_log10_RMSE | gas_FGR_RMSE_1p1FIMA_pp | gas_FGR_RMSE_3p2FIMA_pp |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CANDIDATE_A | 0.192646 | 0.102625 | 0.274942 | 0.142347 | 20.1953 | 0.421623 | 0.877025 | 2.21972 |

Grafici: [plots CANDIDATE_A](final_candidates/CANDIDATE_A/plots/). Risultati completi e parametri salvati nella stessa cartella. Le tre popolazioni R/N sono confrontate in unità coerenti (Ngf volumetrica); Ngf areale è conservata a parte.

### CANDIDATE_B: trial 41

Parametri relativamente bassi nel range logaritmico: Dv_dislocation_scale; alti: K_d, Dg_dislocation_scale. Migliora rispetto alla baseline: nessun obiettivo; trade-off rispetto alla baseline: J_swelling, J_micro, J_FGR. Core partition 8.557 pp. Selezione: massima distanza da A nel pool competitivo.

Distanze: B–A: 0.930, B–C: 0.739.

Miglioramenti di componenti specifiche rispetto alla baseline: swelling_error_AP3p2, E_N.

Questo candidato è dominato negli obiettivi aggregati e viene mantenuto come alternativa fisica competitiva, non come miglioramento globale. Migliora il pin AP3.2 e la componente N_d low-T, ma paga soprattutto R_d low-T e gli altri pin. La distanza nel parameter space motiva l’ispezione visiva; non implica superiorità del fit.

| label | source_trial | J_swelling | J_micro | J_FGR | E_R | E_N | gas_partition_core_RMS | Rd_highT_RMSE_nm | Nd_highT_log10_RMSE | gas_balance_max_abs_pp | p_gf_over_eq_max | warnings |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CANDIDATE_B | 41 | 0.255185 | 0.425309 | 3.01044 | 0.489495 | 0.329031 | 8.53624 | 93.9119 | 1.00691 | 5.68434e-14 | 34054.8 | grainface_pressure_over_eq_gt100_diagnostic_only |

Raggio grain-face massimo: 3.063 µm, rispetto al raggio del grano congelato 6.0 µm. Anche la scala geometrica, soprattutto C vicino a tale dimensione, resta da ispezionare.

| label | swelling_error_AP3p2 | swelling_error_AP3p8 | swelling_error_AP3p4 | swelling_error_ANP6 | Rd_lowT_RMSE_nm | Nd_lowT_log10_RMSE | gas_FGR_RMSE_1p1FIMA_pp | gas_FGR_RMSE_3p2FIMA_pp |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CANDIDATE_B | 0.13672 | 0.281298 | 0.291145 | 0.311576 | 46.336 | 0.329031 | 0.723954 | 4.1954 |

Grafici: [plots CANDIDATE_B](final_candidates/CANDIDATE_B/plots/). Risultati completi e parametri salvati nella stessa cartella. Le tre popolazioni R/N sono confrontate in unità coerenti (Ngf volumetrica); Ngf areale è conservata a parte.

### CANDIDATE_C: trial 2

Parametri relativamente bassi nel range logaritmico: rho_d, NGF_AREAL_0; alti: K_d, Dg_dislocation_scale. Migliora rispetto alla baseline: nessun obiettivo; trade-off rispetto alla baseline: J_swelling, J_micro, J_FGR. Core partition 7.881 pp. Selezione: massima separazione minima da A/B nel pool competitivo.

Distanze: C–A: 0.791, C–B: 0.739.

Miglioramenti di componenti specifiche rispetto alla baseline: swelling_error_AP3p2, E_N.

Questo candidato è dominato negli obiettivi aggregati e viene mantenuto come alternativa fisica competitiva, non come miglioramento globale. Migliora il pin AP3.2 e la componente N_d low-T, ma paga soprattutto R_d low-T e gli altri pin. La distanza nel parameter space motiva l’ispezione visiva; non implica superiorità del fit.

| label | source_trial | J_swelling | J_micro | J_FGR | E_R | E_N | gas_partition_core_RMS | Rd_highT_RMSE_nm | Nd_highT_log10_RMSE | gas_balance_max_abs_pp | p_gf_over_eq_max | warnings |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CANDIDATE_C | 2 | 0.279472 | 0.420591 | 2.77601 | 0.477236 | 0.335624 | 7.86164 | 96.4774 | 1.0192 | 4.26326e-14 | 217166 | grainface_pressure_over_eq_gt100_diagnostic_only |

Raggio grain-face massimo: 5.821 µm, rispetto al raggio del grano congelato 6.0 µm. Anche la scala geometrica, soprattutto C vicino a tale dimensione, resta da ispezionare.

| label | swelling_error_AP3p2 | swelling_error_AP3p8 | swelling_error_AP3p4 | swelling_error_ANP6 | Rd_lowT_RMSE_nm | Nd_lowT_log10_RMSE | gas_FGR_RMSE_1p1FIMA_pp | gas_FGR_RMSE_3p2FIMA_pp |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CANDIDATE_C | 0.169717 | 0.304397 | 0.31343 | 0.330344 | 46.6954 | 0.335624 | 0.991097 | 3.79871 |

Grafici: [plots CANDIDATE_C](final_candidates/CANDIDATE_C/plots/). Risultati completi e parametri salvati nella stessa cartella. Le tre popolazioni R/N sono confrontate in unità coerenti (Ngf volumetrica); Ngf areale è conservata a parte.

B e C hanno densità iniziali dislocation simili: K_d × rho_d = 3.486e+19 e 3.56e+19 m^-3. Differiscono però di 7.33 volte nella scala di trasporto delle vacanze sulla dislocation e di 7.65 volte nella densità grain-face iniziale. A usa K_d molto più basso e rho_d più alta, con f_n maggiore. È quindi un confronto fra strategie di densità iniziale, crescita e allocazione grain-face, non fra tre copie di uno stesso optimum.

## Correlazioni, compensazioni e limiti dei range

Correlazioni di Spearman descrittive sui trial completati: non dimostrano causalità, dato che NSGA-II modifica più parametri contemporaneamente. Con soli 60 trial le degenerazioni sono indicazioni, non identificazioni univoche.

| parametro | metrica | rho_Spearman |
| --- | --- | --- |
| f_n | J_FGR | -0.92768 |
| Dg_dislocation_scale | E_R | -0.604312 |
| Dg_dislocation_scale | J_micro | -0.59893 |
| Dg_dislocation_scale | J_FGR | -0.461168 |
| Dv_dislocation_scale | E_R | 0.415994 |
| Dv_dislocation_scale | J_swelling | 0.391886 |
| Dg_dislocation_scale | J_swelling | -0.36923 |
| Dv_dislocation_scale | J_micro | 0.334681 |
| Dg_dislocation_scale | gas_partition_core_RMS | 0.289173 |
| NGF_AREAL_0 | J_micro | 0.257798 |
| K_d | E_N | -0.254068 |
| rho_d | J_FGR | -0.251166 |

| coppia | Spearman_tutti | Spearman_pool_competitivo |
| --- | --- | --- |
| K_d / rho_d | 0.0378673 | -0.821023 |
| Dv_dislocation_scale / Dg_dislocation_scale | -0.538802 | -0.644795 |
| f_n / Dg_dislocation_scale | 0.279509 | 0.239081 |

K_d × rho_d imposta la densità iniziale delle dislocation bubbles: diversi prodotti e diverse scale di trasporto possono distribuire il gas in modi differenti. Dv_dislocation controlla la crescita tramite vacanze; Dg_dislocation modifica il trasporto sulla linea. f_n altera nucleazione bulk e competizione per il gas. Le correlazioni campionate sopra segnalano le possibili compensazioni, senza attribuirle automaticamente a un unico meccanismo.

| parametro | entro_10%_log_limite_basso | entro_10%_log_limite_alto | min_campionato | max_campionato |
| --- | --- | --- | --- | --- |
| f_n | 2 | 0 | 1.06629e-07 | 0.00293608 |
| K_d | 3 | 17 | 129673 | 1.96713e+06 |
| rho_d | 3 | 0 | 1.03365e+13 | 5.50698e+13 |
| Dv_dislocation_scale | 11 | 6 | 1.05028 | 23.2666 |
| Dg_dislocation_scale | 9 | 6 | 1.08322 | 23.7758 |
| NGF_AREAL_0 | 22 | 3 | 2.36572e+12 | 1.72626e+14 |

## Tensione tra swelling e microstruttura

Una ricostruzione diagnostica con la relazione sferica single-size già usata dal modello, 100 × N_exp × (4π/3) × R_exp³, usando R interpolato solo entro il dominio dei dati <=1600 K, dà swelling da 1.30 a 4.51 volte la curva P2 sperimentale interpolata. Si veda experimental_micro_swelling_consistency.csv. Non è un nuovo obiettivo né una nuova equazione nel solver. È un indizio di tensione fra target quando rappresentati con una popolazione single-size, non una prova di inconsistenza sperimentale: punti non coincidenti, definizioni statistiche del raggio/densità e distribuzioni reali possono differire. Questo aiuta a interpretare il trade-off osservato: migliorare R/N non garantisce migliorare lo swelling.

## Gas partition, FGR e diagnostiche

Core partition: min 3.909, mediana 10.496, max 60.088 pp. Warning >10: 33; strong >20: 8. Nessuno di questi warning ha causato esclusione automatica. FGR RMSE: min 1.029, mediana 3.453, max 13.615 pp.

| statistica | gas_matrix_RMSE_pp | gas_bulk_RMSE_pp | gas_dislocation_RMSE_pp | gas_grainface_RMSE_pp | gas_FGR_RMSE_pp |
| --- | --- | --- | --- | --- | --- |
| min | 2.96103 | 2.47137 | 2.5034 | 2.12908 | 1.02901 |
| median | 3.37431 | 13.3798 | 12.6068 | 6.21433 | 3.45287 |
| max | 3.49434 | 77.4656 | 69.4272 | 17.8078 | 13.6148 |

Bilancio massimo assoluto sui trial completi: 7.10543e-14 pp. Errore relativo massimo della rho costante: 0. Valori non finiti principali: 0; inventari negativi: 0.

High-T R_d sottostimato in media in 55/59 trial; high-T N_d sovrastimata in media in 56/59 trial. Le curve non vengono forzate a eliminare questo comportamento strutturale. La dispersione di N_d sperimentale e l’evoluzione high-T non sono un quarto obiettivo.

p_gf/p_eq massimo: mediana dei massimi 66532.6, massimo 463838; baseline 45328.3. Questi valori sono segnalati come limite del modello e non usati per scegliere il fit. Le pressioni assolute e i rapporti sono salvati per tutti i punti.

| case | trial_con_Rgf>Rd>Rb_violato_persistente | trial_con_Ngf<Nd<Nb_violato_persistente |
| --- | --- | --- |
| AP3.2 | 5 | 7 |
| AP3.8 | 5 | 7 |
| AP3.4 | 3 | 7 |
| ANP6 | 1 | 2 |

Persistente significa almeno tre temperature consecutive nello screening. Ngf < Nd < Nb usa tutte densità volumetriche. Violazioni dei raggi a basse T e pressioni grain-face elevate possono persistere anche nella baseline: restano guardrail per l’ispezione fisica. Non esistono curve numeriche Rgf/Ngf sperimentali o Rizk integrate nel notebook corrente, quindi non sono inventati RMSE per tali popolazioni.

## Artefatti e ricalcolo offline

`all_trial_curves.csv` contiene tutti i parametri, stati finali, gas, pressioni, numeri/raggi, swelling e diagnostiche per ogni trial/pin/T. `trial_metrics.csv`, `trials.csv` e `pareto_front.csv` conservano metriche separate. Gli esperimenti e la Fig.9 limitata al dominio sono esportati in CSV. `manifest.json` documenta formule, unità, pesi, impostazioni congelate e versione; i moduli ausiliari contengono scorer e plotting.

Ricalcolo senza solver: `.venv/bin/python UN_model/optuna/Optuna_rho_constant_Rizk.py --rescore`. Produce `offline_rescored_metrics.csv` e `offline_rescored_pareto.csv` senza sovrascrivere la campagna. Il normale comando riprende checkpoint/studio e resta limitato a 60 trial. La selezione e i rerun esistenti vengono riutilizzati.

Lo studio si ferma qui. Range, fisica e scoring restano quelli dichiarati. Nessun refinement, secondo studio o scelta definitiva viene eseguito.

I trial FAIL restano esclusi da Pareto/selezione. Per quelli con curve finite, le stesse formule degli obiettivi e dei benchmark vengono comunque riportate a scopo diagnostico (failed_trial_diagnostic_metrics.csv); lo stato e i valori Optuna non vengono modificati.

I conteggi NaN/inf di tutte le colonne numeriche sono salvati per ciascun trial in `numerical_diagnostics.csv` e nelle tabelle dei trial. I quattro campi Dg_lookup_base, Dv_lookup_base, Dv_nonthermal_effective e A20_vU_active sono placeholder NaN per rami di diffusività inattivi, già presenti nella baseline originale; non rappresentano un fallimento del solver. Le colonne vuote error/traceback non sono inventari numerici.

Inf in tutte le colonne numeriche: 0; trial con NaN inattesi: 0.
