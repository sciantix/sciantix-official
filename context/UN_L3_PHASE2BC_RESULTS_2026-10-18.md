# SCIANTIX UN L3 — Risultati FASI 2B e 2C, 18 ottobre 2026

## Esito e perimetro

**FASE 2B completata e verificata prima di procedere alla 2C. FASE 2C completata per la configurazione L3 approvata.** Tutti i nuovi test di componente passano. Nessuna FASE 2D, 3 o 4 avviata. Il modello non è ancora collegato al normale ciclo SCIANTIX.

Il perimetro è quello confermato dall'utente: 2B trasporto e intragranulare, inclusi seed, vacancy, coalescenza e cattura; 2C grain-face e rilascio; futura 2D integrazione nel ciclo normale. Le equazioni, le opzioni attive e l'ordine degli aggiornamenti derivano dal notebook approvato `UN_model/notebooks/finalRizkUN_L3_solver_unificato.ipynb`, SHA-256 `e29d798d3685edb827a43933b3dc4400087f1aff488ee4080650c1430e1c59ff`. Non è stato rieseguito o modificato per produrre nuovi valori attesi.

Riferimenti: [report 2A](UN_L3_PHASE2A_RESULTS_2026-10-18.md), [istruzioni e riproduzione](../UN_model/phase2bc/README.md), [riepilogo JSON](../UN_model/phase2bc/results/summary.json), [protezione workspace](../UN_model/phase2bc/preservation_check.json).

## Implementazione modulare

| Componente | File di produzione | Contenuto verificato |
|---|---|---|
| Parametri e stato | `include/models/UNParameters.h`, `UNState.h` | Parametri SI e opzioni L3 locali; stato proprietario dei tre vettori modali e flag; inventari e diagnostiche grain-face |
| Trasporto | `include/models/UNTransport.h`, `src/models/UNTransport.C` | Diffusività Xe/vacancy e contributi, diffusività grain boundary, produzione, nucleazione, trapping e risoluzione |
| Tabelle | `include/models/UNLookupTables.h` | Nodi ordinati/deduplicati copiati dai dati preparati congelati, senza rifit |
| Bolle | `include/models/UNBubblePhysics.h`, `src/models/UNBubblePhysics.C` | Geometria, pressioni, seed, fattori geometrici, vacancy implicite, coefficiente di coalescenza |
| Passo intragranulare | `include/models/UNIntraGranular.h`, `src/models/UNIntraGranular.C` | Sequenza fisica e chiamata al vero solver SCIANTIX; trasferimento dei modi nella cattura |
| Grain-face | `include/models/UNGrainFace.h`, `src/models/UNGrainFace.C` | Inizializzazione, geometria lenticolare, limited growth, predittore 3×3, saturazione e rilascio |

Tutto vive nel namespace `un_l3` e nell'architettura dei modelli SCIANTIX. Il driver di test è un harness separato che chiama questi stessi sorgenti; non è un programma di produzione alternativo. Il solver `Solver::SpectralDiffusion3equationsExchange` e `Solver.C` rimangono invariati.

Il passo intragranulare mantiene l'ordine Python: ricostruzione delle medie dai modi; coefficienti dallo stato precedente; aggiornamento del numero di bolle bulk; sorgenti con accoppiamento di massa della nucleazione; solver; eventuale ramo di ripristino delle concentrazioni negative; seed al primo gas con salto dell'aggiornamento vacancy per quella popolazione in quel passo; vacancy implicite; crescita e coalescenza delle bolle su dislocazione; ricostruzione della geometria; cattura con trasferimento di gas, vacancy, numero bulk e singoli coefficienti modali. La densità di dislocazioni resta quella costante della configurazione L3.

Il passo grain-face mantiene gli stadi Python: refresh dello stato; inventario di gas entrante; crescita vacancy; predittore 3×3; ramo presaturazione, attraversamento della soglia o ramo già saturo; sopravvivenza degli inventari e rilascio; refresh finale e tassi. Non viene introdotto un clamp di capacità alternativo.

Non sono modificati buffer globali, costanti condivise, dispatch di altri combustibili o percorso UN legacy. Il valore locale L3 di kB è mantenuto esattamente. `CMakeLists.txt` aggiunge le dipendenze trasporto/bolle alla libreria di test 2A già opzionale; l'opzione rimane disattivata per default. I nuovi `.C` rientrano nel normale build dei sorgenti del progetto, senza essere ancora richiamati dal ciclo di simulazione.

## Confronti numerici

Usate direttamente le tracce complete congelate della FASE 1 per AP3.2/900 K, AP3.4/1600 K, AP3.4/1800 K e ANP6/1600 K: rispettivamente 605, 550, 550 e 1.307 aggiornamenti, totale 3.012, passo nominale 3 h, 160 modi. L'ultimo passo ridotto è quello registrato. I risultati diagnostici brevi non entrano in questa verifica.

La verifica isolata riparte dallo stato registrato a ogni passo. La catena intragranulare conserva il proprio stato C++ tra tutti i passi e calcola i coefficienti con i componenti già verificati; non ricarica i modi attesi. La catena grain-face conserva lo stato C++, ma riceve gas entrante, dt e Dv1 dalle fixture per separare gli errori del componente da quelli dell'accoppiamento futuro.

La suite numerica 2A resta distinta: coefficienti, sorgenti e vettori modali sono quelli congelati, senza sostituzione con coefficienti fisici appena implementati. È stata ripetuta dopo la 2B e sul sorgente finale, insieme al test CTest di proprietà dello stato, flag e chiamate intercalate.

Per i nuovi componenti: errore normalizzato `|C++−Python|/|Python|` quando Python è diverso da zero, denominatore 1 per riferimento esattamente zero. Soglia fissata `1e-11`, controllo di finitezza obbligatorio. I CSV di ciascuna esecuzione riportano anche errore assoluto, campo, caso e indice del massimo; nessun superamento della soglia. Gli errori assoluti di concentrazioni o coefficienti modali molto grandi vanno letti con la rispettiva scala fisica.

| Test canonico in `UN_model/phase2bc/results/` | Valori confrontati | Errore normalizzato massimo |
|---|---:|---:|
| `B1_transport` | 60.240 | 2,051e-16 |
| `B2_seed` | 24 | 1,665e-16 |
| `B2_vacancy` | 12.032 | 1,361e-13 |
| `B3_intragranular_cached` — passi isolati | 2.999.952 | 3,118e-13 |
| `B3_intragranular_chain` — stato consecutivo | 2.999.952 | 4,644e-13 |
| `B4_solver_state_recheck` — solver/adapter 2A | 5.829.810 | 0 |
| `C1_limited` | 3.012 | 0 |
| `C1_3x3` | 9.036 | 0 |
| `C2_initial` | 80 | 0 |
| `C2_grainface` — passi isolati | 60.240 | 1,689e-13 |
| `C2_grainface_chain` — stato consecutivo | 60.240 | 2,983e-13 |
| `C3_lookup_nodes` | 776 | 1,616e-16 |
| `C3_ig_sanitized` — catena intragranulare | 2.999.952 | 4,644e-13 |
| `C3_gf_sanitized` — catena grain-face | 60.240 | 2,983e-13 |
| `C3_final_solver_state` — ricontrollo finale 2A | 5.829.810 | 0 |

I valori comprendono scalari e, nell'intragranulare, tutti i 480 modi dopo lo scambio e tutti i 480 modi dopo la cattura per ciascun aggiornamento. I test lookup coprono 194 nodi unici (111 Xe e 83 U), ripetuti nei quattro casi. Il ricontrollo finale 2A ricompila l'intero codice, passa CTest 1/1 e mantiene l'identità binaria fra solver diretto e adapter di stato.

Gli invarianti grain-face passano per ogni caso sia isolato sia consecutivo: bilancio `q_gf + q_rel` rispetto agli inventari precedenti più gas entrante con errore normalizzato massimo **0**; flag di saturazione identici a Python; inventari e numero areale non negativi; output finiti. Scarto assoluto massimo della copertura satura da 0,5: **3,331e-16**. Prima saturazione negli aggiornamenti con indice da zero 538 (AP3.4/1800 K) e 735 (ANP6/1600 K); nessuna negli altri due casi. Dettagli in [grainface_invariants.json](../UN_model/phase2bc/results/grainface_invariants.json).

AddressSanitizer e UndefinedBehaviorSanitizer non hanno segnalato errori nelle due catene complete. LeakSanitizer è disabilitato (`detect_leaks=0`) per l'incompatibilità con ptrace già osservata in 2A; non si dichiara verificata l'assenza di leak con quel tool.

## Build, regressione storica e preservazione

Ambiente esistente riutilizzato: g++ 13.3.0, C++11, build componenti `-O2 -Wall -Wextra`; sanitizer `-O1 -g -fsanitize=address,undefined -fno-omit-frame-pointer`; Python 3.12.3, NumPy 2.4.4, CMake privato 3.31.6 predisposto in FASE 1. Nessuna ulteriore installazione di dipendenze. HEAD invariato: `8f5bfc7e123b0241507c5d1e587a28d20e4dcb24`.

Il build SCIANTIX finale e la regressione sono eseguiti in una nuova copia isolata [C3_nonregression](../UN_model/phase2bc/results/C3_nonregression/manifest.json), modalità gold 0. **Suite standard: 109 PASS, 0 FAIL.** I 110 `output.txt`, incluso il caso supplementare, sono identici byte per byte alla baseline FASE 1. Anche il CSV degli esiti legacy UN è identico.

Anomalie preesistenti confermate e conservate:

- `analytics/test_Baker1977_1273K_porosityevolution`, escluso dal selettore standard: FAIL contro gold, 1.127 valori differenti, già presente nella baseline.
- `vercors/test_Vercors5`: tre NaN nelle colonne già documentate in FASE 1 (pressione parziale/potenziale di ossigeno alla riga dati 400, stechiometria di equilibrio alla 401); stesso output e stessi NaN del gold.
- UN legacy notebook8: 91 PASS con la tolleranza storica del 35%; resta assente il vecchio percorso del generatore notebook, non necessario per leggere i riferimenti già presenti e non rigenerato.

Per queste anomalie `audit_baseline.py` termina con codice 1, come prima del porting. Non è un fallimento introdotto dai componenti nuovi. [Confronto con FASE 1](../UN_model/phase2bc/results/C3_nonregression/comparison_to_phase1.json).

Controllati **4.099 file iniziali**: reference Python, gold, legacy, altri combustibili, precedenti report, risultati e build originali preservati. Solo cinque file già esistenti al momento di iniziare questa fase sono cambiati: `CMakeLists.txt`, `include/models/UNParameters.h`, `UNState.h`, `UNIntraGranular.h`, `src/models/UNIntraGranular.C`. Anche l'hash del diff staged e HEAD coincidono con lo stato iniziale. Le modifiche staged dell'utente a `BaseRizk2025.ipynb` e `BaseRizk2025_grid.csv` sono intatte. Nessuna cancellazione e nessun aggiornamento dei gold.

## Tempi e checkpoint

Tempi wall misurati, non stime; il tempo C++ è la somma dei quattro processi per caso ed esclude lettura/esportazione Python delle fixture:

| Esecuzione | Compilazione driver | Esecuzione C++ |
|---|---:|---:|
| Trasporto | 1,731 s | 0,014 s |
| Seed | 1,890 s | 0,007 s |
| Vacancy | 2,118 s | 0,017 s |
| Intragranulare isolato | 2,375 s | 0,501 s |
| Intragranulare consecutivo | 2,321 s | 0,528 s |
| Grain-face isolato | 2,841 s | 0,014 s |
| Grain-face consecutivo | 2,822 s | 0,016 s |
| Intragranulare sanitizer | 7,441 s | 1,650 s |
| Grain-face sanitizer | 7,579 s | 0,059 s |

Ricontrollo finale 2A completo, inclusi preparazione e build: 26,654 s. Baseline finale: configurazione 2,371 s, build 21,286 s, suite standard 19,530 s. Gli altri tempi sono nei rispettivi JSON; non è stato registrato un unico cronometro dell'intera attività interattiva.

Checkpoint locali documentati dopo i componenti significativi: `B1_transport`, `B2_seed_vacancy`, **`B_COMPLETE` prima della 2C**, `C1_limited_3x3`, `C2_grainface`, `C3_validated_components`, `C_COMPLETE`. Ogni directory contiene sorgenti ZIP e manifest SHA-256 con i test effettivamente passati. Non sono commit Git e non coinvolgono l'index dell'utente. [Checkpoint finale](../UN_model/phase2bc/checkpoints/C_COMPLETE/manifest.json).

Due tentativi di preparazione non canonici restano sul disco: un lettore cercava `Dv_dislocation` fuori dalla struttura annidata corretta; un successivo tentativo è stato interrotto per eliminare decompressioni NPZ ripetute. Corretti lettore e caching, rieseguiti tutti i quattro casi. Nessuna discrepanza ha richiesto modifiche alla reference o alle equazioni fisiche.

## File prodotti/modificati e lavoro residuo

Oltre ai cinque file preesistenti sopra elencati, aggiunti i nuovi header/sorgenti trasporto, tabelle, bolle e grain-face della tabella, e `UN_model/phase2bc/` con `component_driver.cpp`, `run_components.py`, `verify_results.py`, `checkpoint.py`, README, inventario iniziale, controllo preservazione, fixture derivate, output, metriche CSV/JSON, log e checkpoint. Aggiunto questo report. File e risultati delle FASI 0/1/2A non modificati; i loro runner sono stati riutilizzati con nuove directory di output sotto `phase2bc`.

La configurazione L3 attiva è verificata quantitativamente. Le formule per modalità alternative di diffusività sono trascritte, ma le fixture disponibili non validano indipendentemente tutte le loro curve; il test aggiuntivo lookup copre i nodi congelati. Le varianti inattive a densità di dislocazioni dinamica e a dimero in equilibrio non fanno parte del passo L3 implementato. Non si estende a queste varianti l'esito dei test L3.

Per una futura FASE 2D, soggetta a nuova autorizzazione:

1. Collegare selezione UN L3, parametri e inizializzazione completa al ciclo modulare SCIANTIX mantenendo il percorso legacy distinto.
2. Conservare internamente lo stato completo tra chiamate reali, compresi tre vettori modali e flag, senza modificare globalmente buffer o comportamenti UO2.
3. Orchestrare tempo, produzione cumulativa, bilancio/trasferimento del gas verso grain-face e aggiornamento degli output nell'esatto ordine Python; oggi i due passi sono deliberatamente verificati separatamente.
4. Eseguire durante l'integrazione completa la verifica end-to-end della persistenza e dell'accoppiamento. Le catene di componente qui superate non sostituiscono quella verifica.

Le regressioni accoppiate Python–C++ del modello completo, le verifiche delle fasi successive e l'eventuale checkpoint/restart esterno restano rinviati. Nessun blocco tecnico aperto per il perimetro 2B/2C; il lavoro si ferma qui come richiesto.
