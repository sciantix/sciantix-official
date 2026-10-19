# UN L3 — integrazione SCIANTIX e regressione completa

FASE 2D e FASE 3 completate il 19 ottobre 2026. FASE 4 non avviata. [Report completo](../../context/UN_L3_PHASE2D3_RESULTS_2026-10-19.md), [riepilogo verificabile](results/summary.json), [errori per variabile](results/variable_errors.csv), [controllo preservazione](preservation_check.json).

## Percorso di produzione

Selettore `iFuelMatrix = 3` (indice 11 di `input_settings.txt`): UN L3. Restano distinti `0` UO2, `1` UO2/HBS e `2` UN legacy. `UNL3Model.h/.C` è un modulo posseduto da `Simulation`; viene eseguito attraverso `MainSCIANTIX → Sciantix → Simulation.initialize/execute/update/output`. Non introduce un altro `main` o un solver separato.

Il profilo attualmente esposto è la configurazione L3 congelata: 160 modi, dt nominale 10.800 s, dislocazioni a densità costante, opzioni fisiche dei moduli 2B/2C. Temperatura, fission rate e stress provengono da `input_history.txt`; raggio del grano da `input_initial_conditions.txt`. Gli altri parametri sono i default SI in `UNParameters.h`, verificati esattamente contro tutti i manifest. Si richiedono storia costante da tempo zero, combustibile inizialmente senza gas e bolle intragranulari, burnup e tempo di irraggiamento iniziali nulli, fattori di scaling storici unitari. Le condizioni non supportate vengono rifiutate. Non è un'interfaccia generale per tutte le varianti del notebook.

L'inizializzazione locale crea Nd = Kd·rho e il seed geometrico grain-face senza gas. I tre vettori modali, microstruttura, inventari e flag restano nello stato del modulo per tutte le chiamate. Il buffer modale globale storico da 720 elementi non è ridimensionato, né usato per ricostruire L3. `update` esporta i principali scalari negli indici compatibili già esistenti. Il checkpoint/restart esterno rimane rinviato.

Sul percorso L3 vengono saltati tutti gli aggiornamenti fisici legacy. Il modulo chiama l'intragranulare verificato, accumula la produzione, calcola dal proprio stato il deficit di gas e l'incremento verso il bordo, quindi chiama il grain-face verificato e aggiorna il tempo. Nessun coefficiente, flusso o stato successivo è letto dalle fixture. Il vero eseguibile legge solo i normali tre file di input (e l'eventuale scaling storico).

Gli esempi pronti sono i tre `input_*.txt` originari di ogni directory caso in `results/P3_full/`; `input_check.txt` è generato dall'eseguibile. Usare copie in directory nuove per ulteriori esecuzioni.

## Output e confronto

Per L3, `output.txt` contiene colonne con i nomi delle quantità Python, a 17 cifre: 64 quantità di storia più inventari, flag e controlli temporali. Unità SI: gas in atomi/m³ di combustibile, vacancy totali in vacancy/m³ di combustibile, densità bolle in m⁻³ o m⁻² per Ngf_areal, raggi in m, volumi per bolla in m³, aree in m², pressioni in Pa, swelling e copertura adimensionali; il tempo è in s e le colonne `*_percent` sono percentuali.

- `un_l3_modes.bin`: float64 nativi, ordine `(riga_temporale, popolazione c/mb/md, modo)`, forma `(steps+1,3,160)`, inclusa l'inizializzazione. Nell'ambiente WSL verificato il formato è little-endian.
- `un_l3_exchange_modes.bin`: stessa struttura dopo lo scambio, `(steps,3,160)`.
- `un_l3_diagnostics.txt`: 32 campi intermedi per aggiornamento, inclusi coefficienti, sorgenti, medie dopo scambio, seed, vacancy, crescita, cattura e gas entrante autonomo.

I vettori finali vengono confrontati sia con `after_capture` sia con `step_end` congelati, per controllare che l'orchestrazione grain-face non alteri i modi. Il conteggio dei valori include queste due verifiche dello stesso vettore.

`results/P3_full/errors.csv` contiene gli errori per caso/stadio/variabile. `results/variable_errors.csv` aggrega i massimi sui quattro casi. In ogni caso, `boundary_checks.json` conserva confronti dettagliati di inizializzazione, primo/terzo passo, ingresso in saturazione e ultimo passo. `comparison.npz` conserva le curve scalari C++ e Python. I file originari delle FASI 1/2A/2BC sono soltanto letti.

Tolleranze definite prima della diagnostica: `rtol=1e-11`, termine additivo `64*epsilon*scala`, come principio numerico della 2A. La scala conserva le unità della variabile: massimo assoluto congelato per le storie; massimo per popolazione/riga per i modi. Inventari al bordo e residui di massa ottenuti sottraendo grandi quantità usano la produzione cumulativa come scala; le rispettive percentuali usano 100. Flag e contatori richiedono uguaglianza esatta. Per una curva identicamente zero il guard computazionale è `1e-300`, quindi gli zeri fisici sono di fatto richiesti esatti. Il file `tolerances.json` descrive il criterio; i manifest registrano la versione effettivamente usata. Durante la documentazione è stata precisata soltanto la descrizione della scala scalare, senza cambiare alcun numero o criterio.

## Riproduzione

Da root del repository, usando directory di output nuove. Servono l'ambiente `.venv`, g++, e il CMake privato già predisposto nella FASE 1. Non sono state installate nuove dipendenze.

```bash
.venv/bin/python UN_model/phase2d3/run_integration.py --suite diagnostic --output UN_model/phase2d3/results/NEW_diagnostic
.venv/bin/python UN_model/phase2d3/run_integration.py --suite full --output UN_model/phase2d3/results/NEW_full
.venv/bin/python UN_model/phase2d3/run_integration.py --suite full --sanitize --output UN_model/phase2d3/results/NEW_sanitized
.venv/bin/python UN_model/phase2d3/analyze_results.py --results UN_model/phase2d3/results/NEW_full --output UN_model/phase2d3/results/NEW_plots
```

Ogni esecuzione copia i sorgenti in uno snapshot nuovo, compila **sciantix.x**, genera esclusivamente i normali input dei quattro casi e lancia un processo per caso. Parametri/fine simulazione vengono dai manifest congelati; tutti i valori attesi dalle tracce/NPZ già salvati, senza eseguire Python fisico. La diagnostica esegue tre aggiornamenti per caso; `full` esegue tutte le storie. I log conservano comandi, tempi, hash, configurazione e binario. Il sanitizer usa Debug con `-O1 -g -fsanitize=address,undefined -fno-omit-frame-pointer`; `ASAN_OPTIONS=detect_leaks=0` per la nota incompatibilità LeakSanitizer/ptrace. Nessuna verifica LeakSanitizer dichiarata.

```bash
.venv/bin/python UN_model/phase2a/run_phase2a.py --output UN_model/phase2d3/results/NEW_solver_state
.venv/bin/python UN_model/baseline_sciantix/prepare_baseline.py --cmake /tmp/sciantix-un-l3-phase1-tools/cmake/data/bin/cmake --output UN_model/phase2d3/results/NEW_baseline --jobs 2
.venv/bin/python UN_model/baseline_sciantix/audit_baseline.py UN_model/phase2d3/results/NEW_baseline
```

L'audit storico restituisce 1 per Baker/Vercors preesistenti, non aggiorna i gold. I risultati completi canonici sono `D1_diagnostic_v3`, `P3_full`, `P3_sanitized`, `P3_solver_state`, `P3_nonregression`, `P3_independent_ANP6`, `P3_plots`. `D_parameter_audit/defaults.cpp` stampa i default reali dell'header C++, confrontati con i 208 valori nei manifest; sorgente e risultato sono conservati.

La ripetizione indipendente ANP6 usa il binario di `P3_full`, una nuova directory con i soli input e un nuovo processo. Tutti e quattro i file di output sono identici byte per byte; risultato in `P3_independent_ANP6/results.json`.

```bash
.venv/bin/python UN_model/phase2d3/verify_workspace.py
```

Questo comando verifica i risultati canonici già salvati, i bilanci, gli ultimi passi esatti, i flag persistenti, i 110 output storici rispetto alla baseline FASE 1 e i 6.741 file protetti; rigenera soltanto riepilogo e CSV aggregato di questa fase. Non esegue fisica. Il diff staged e HEAD sono verificati invariati.

## Checkpoint e ripresa

`checkpoints/D_COMPLETE`, `P3_FULL`, `FINAL_COMPLETE` contengono sorgenti ZIP e hash dei risultati effettivamente passati. Sono checkpoint locali, non commit Git. Per crearne un altro: `checkpoint.py NOME file_results.json ...`, con nome nuovo e soli JSON `status: pass`.

Restano conservati due tentativi di build iniziali: `D1_diagnostic` (include `<cmath>` mancante) e `D1_diagnostic_v2` (collisione del nuovo nome con `UNModel.h` legacy). Il vecchio header è stato ripristinato esattamente; i nuovi file si chiamano `UNL3Model.h/.C`. La verifica di preservazione conferma il ripristino. Nessuna equazione o reference è stata modificata per risolvere differenze numeriche.

Per riprendere: leggere il report finale e `FINAL_COMPLETE/manifest.json`, verificare `verify_workspace.py`, quindi lavorare solo sulla fase successivamente autorizzata. Non è necessario rifare l'analisi del porting. La FASE 4 e il restart esterno restano fuori dal lavoro svolto.
