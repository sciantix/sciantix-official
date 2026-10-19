# UN L3 — FASI 2B/2C

Completate per la configurazione L3 congelata: trasporto e intragranulare (2B), grain-face e rilascio (2C). Il ciclo normale SCIANTIX non richiama ancora questi componenti. Nessuna FASE 2D, 3 o 4 avviata.

Report: [UN_L3_PHASE2BC_RESULTS_2026-10-18.md](../../context/UN_L3_PHASE2BC_RESULTS_2026-10-18.md). Riepilogo verificabile: [results/summary.json](results/summary.json). Protezione del workspace: [preservation_check.json](preservation_check.json).

## Codice e riferimenti

Le implementazioni sono in `include/models/UN*.h` e `src/models/UN*.C`, namespace `un_l3`. `component_driver.cpp` è esclusivamente un harness: compila e richiama queste implementazioni e il vero `Solver.C` SCIANTIX. Non contiene un secondo solver fisico.

`run_components.py` legge manifest, tracce JSONL e vettori NPZ della FASE 1 in `../regression_reference/results/full/`; non esegue il notebook né ricalcola una reference Python. Il notebook approvato rimane `../notebooks/finalRizkUN_L3_solver_unificato.ipynb`, SHA-256 `e29d798d3685edb827a43933b3dc4400087f1aff488ee4080650c1430e1c59ff`.

Sono coperti i quattro casi AP3.2/900 K, AP3.4/1600 K, AP3.4/1800 K, ANP6/1600 K: 3.012 aggiornamenti complessivi, passo nominale 10.800 s (3 h), ultimo passo eventualmente ridotto, 160 modi per popolazione. Si usano tutte le tracce complete, non le esecuzioni diagnostiche brevi.

Nei test isolati ogni passo parte dallo stato Python registrato. Nei test `*_chain` lo stato è caricato solo all'inizio e poi conservato dal C++. La catena intragranulare calcola i propri coefficienti con i componenti già verificati in 2B. La catena grain-face usa invece ingressi di gas, dt e Dv1 congelati, per isolare il componente. Questi test non costituiscono ancora una simulazione accoppiata nel normale ciclo SCIANTIX.

## Ripetere i test

Da root del repository; scegliere sempre directory di output nuove. Lo script rifiuta di sovrascrivere risultati esistenti. Servono il Python della `.venv` con NumPy e g++.

```bash
.venv/bin/python UN_model/phase2bc/run_components.py --component transport --output UN_model/phase2bc/results/NEW_transport
.venv/bin/python UN_model/phase2bc/run_components.py --component intragranular_chain --output UN_model/phase2bc/results/NEW_ig
.venv/bin/python UN_model/phase2bc/run_components.py --component grainface_chain --output UN_model/phase2bc/results/NEW_gf
.venv/bin/python UN_model/phase2bc/run_components.py --component intragranular_chain --sanitize --output UN_model/phase2bc/results/NEW_ig_sanitized
```

Altri componenti disponibili: `lookup`, `seed`, `vacancy`, `intragranular`, `grainface_limited`, `grainface_3x3`, `grainface_initial`, `grainface`; anche `grainface_chain` supporta `--sanitize`. Ogni invocazione compila i moduli prima del test. I risultati conservano comando di build, tempi, hash sorgenti/binario, input serializzati, output binari, confronti NPZ, metriche JSON e CSV per campo. Gli archivi NPZ sono letti una volta per caso.

La metrica dei componenti è `abs(C++ - Python) / abs(Python)` per reference non nulla; per reference esattamente nulla il denominatore è 1. La soglia è `1e-11` e tutti i valori devono essere finiti. Gli errori assoluti e le posizioni dei massimi sono nei CSV. La suite 2A conserva la propria definizione di tolleranza, documentata nel suo report, e produce qui errore esattamente zero.

I sanitizer abilitano AddressSanitizer e UndefinedBehaviorSanitizer. `ASAN_OPTIONS=detect_leaks=0` evita l'incompatibilità LeakSanitizer/ptrace già documentata in 2A: non è stato eseguito un controllo delle perdite con LeakSanitizer.

Per ricompilare l'intero progetto e ripetere il solver numerico/contratto dello stato 2A:

```bash
.venv/bin/python UN_model/phase2a/run_phase2a.py --output UN_model/phase2bc/results/NEW_solver_state
```

Per la regressione storica senza aggiornare i gold:

```bash
.venv/bin/python UN_model/baseline_sciantix/prepare_baseline.py --cmake /tmp/sciantix-un-l3-phase1-tools/cmake/data/bin/cmake --output UN_model/phase2bc/results/NEW_baseline --jobs 2
.venv/bin/python UN_model/baseline_sciantix/audit_baseline.py UN_model/phase2bc/results/NEW_baseline
```

Il secondo comando restituisce 1 per le anomalie preesistenti riportate nel report; non aggiorna i gold. Il CMake privato indicato è quello già predisposto in FASE 1.

```bash
.venv/bin/python UN_model/phase2bc/verify_results.py
```

Quest'ultimo comando controlla i risultati canonici già salvati, gli invarianti grain-face, l'identità dei 110 output storici rispetto alla FASE 1, l'esito legacy UN e i 4.099 file inizialmente protetti. Rigenera soltanto i riepiloghi di questa fase. Non lancia nuovamente la fisica o la regressione. `protection_before.json` registra lo stato iniziale; le sole cinque modifiche ammesse a file già esistenti sono dichiarate nello script e nel report.

## Checkpoint e tentativi conservati

`checkpoints/` contiene `B1_transport`, `B2_seed_vacancy`, `B_COMPLETE`, `C1_limited_3x3`, `C2_grainface`, `C3_validated_components`, `C_COMPLETE`. Ogni checkpoint ha `source.zip` e un manifest con hash dei sorgenti e dei risultati effettivamente superati. Sono archivi locali, non commit Git; non includono le modifiche staged dell'utente. Il manifest delimita i test superati, anche se uno snapshot intermedio contiene altri sorgenti in lavorazione.

I risultati canonici sono elencati in `results/summary.json`. Sono conservati anche due tentativi di preparazione non canonici: `B3_intragranular` (chiave della traccia cercata al livello sbagliato, corretta nel lettore) e `B3_intragranular_v2` (interrotto per eliminare decompressioni NPZ ripetute). Il test completo corretto è `B3_intragranular_cached`. Nessuna reference o equazione è stata cambiata per risolvere questi problemi.

Per un ulteriore checkpoint usare `checkpoint.py NOME file_results.json ...` con soli risultati `status: pass`; il nome deve essere nuovo. Le directory `snapshot/` dei build e `mplconfig/` sono ignorate da Git ma conservate sul disco. I log e i manifest rimangono consultabili.
