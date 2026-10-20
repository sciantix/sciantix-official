# Campagna estesa: 128 LHS + 32 stati UN

Questa directory contiene esclusivamente la campagna numerica richiesta il 20 ottobre 2026, con script riproducibili, dati grezzi e consegna compatta. Nessun grafico, testo di tesi o studio per decidere N/dt finali.

## File di consegna

- `extended_convergence_160_compact.zip`: CSV, report breve e metadati da caricare per analisi indipendente; nessun grande file binario.
- `deliverables_final/REPORT_BREVE.md`: esito, tempi e anomalie.
- `deliverables_final/LEGGIMI_DATI.md`: significato dei CSV, unità, normalizzazioni e formulazione.
- `deliverables_final/metadata.json` e `deliverables_final/validation.json`: conteggi e controlli.
- `deliverables_final/temporal_order_review.csv`: confronto fra sequenza principale e raffinamento finale a ogni tempo fissato.
- `deliverables_final/temporal_population_assessment.csv`: valutazione separata di c/mb/md per tutti i casi.
- `package_info.json`: dimensione e SHA-256 dello ZIP.
- `checkpoints/final/manifest.json`: checkpoint conclusivo e hash della consegna.

Questi file conclusivi sono prodotti solo dopo la campagna e la validazione; lo stato progressivo è in `progress.json`.

`deliverables/` conserva la prima raccolta, precedente a due raffinamenti aggiuntivi mirati. Due sequenze md a 2 h mostravano p=0,8894/0,8800 dopo cinque dimezzamenti: un sesto dimezzamento sostenibile a 3,515625 s ha dato p=0,94843/0,94428, lasciando invariati criteri e dati precedenti. `targeted_extension_protocol.json`, `extensions_manifest.json` ed `extensions/` documentano questa estensione. La raccolta finale nello ZIP include **81 raffinamenti**, non soltanto i 79 della prima raccolta. I nuovi output grezzi sono aggiunti sotto gli stessi casi, senza riscrivere quelli esistenti.

## Input congelati

`cases.json` e `combinations_160.csv` sono congelati prima dei benchmark. Il seed è **20261020**, SciPy `qmc.LatinHypercube`, scrambling attivo, generatore NumPy `default_rng`. Le coordinate unitarie sono in `inputs/lhs_unit.csv`; ognuna delle cinque dimensioni occupa una volta ciascuno dei 128 strati. D (alias Dg), gd, bb e bd sono logaritmici; gb lineare sull'intero intervallo [0,2.042]. Nessun estremo è imposto artificialmente ai campioni LHS.

Le quattro storie complete dei coefficienti sono in `inputs/history_coefficients_complete.csv`, con tempo, intervallo, passo, numero di riga, file e hash della reference congelata. `inputs/history_selection_32.csv` documenta gli otto stati distinti di ciascuna storia. Si selezionano primo/ultimo stato disponibile, quattro tempi vicini al 20/40/60/80% dell'irraggiamento e due stati successivi alle maggiori variazioni relative dei coefficienti. Il primo record contiene i coefficienti effettivamente usati a t=0; l'ultimo quelli usati nell'ultimo intervallo, senza ricalcolarli al tempo finale. I valori sono copiati esattamente e non ritagliati agli intervalli Table B.1.

In tutti i benchmark i cinque coefficienti restano **costanti**, con R=6e-6 m, iniziali nulle e sorgente comune `(1.7995675661038836e19,0,0)` at/(m³ s), come nella 4B. Le sorgenti originarie delle storie restano registrate nelle tabelle di estrazione. Questa è una campagna del sistema lineare congelato, non una ripetizione delle storie microstrutturali UN.

## Esecuzione e caching

Si importano, senza modificarli, gli helper validati `phase4b/resumed/reference.py` e le matrici/proiezioni 4A. Il driver C++ 4A invariato è compilato con il vero `src/classes/Solver.C` in `build/`, separato dai build precedenti. La reference temporale usa il semigruppo expm con controllo diretto e dimezzamento del passo analitico; i confronti spaziali usano solo analitica N contro high-N. Il massimo high-N viene accresciuto secondo gli stessi due criteri della 4B, senza assumere un ordine universale.

La reference N40 viene riutilizzata per i cinque dt e gli eventuali raffinamenti. Le sei reference spaziali condividono gli stessi prefissi modali, dato che i modi sono indipendenti. Configurazioni 4B identiche sono cercate per tutti i coefficienti, R, sorgente, iniziali, N e dt, con controllo SHA-256 del solver e degli output. I tre casi pilota completati non vengono rieseguiti dalla campagna principale.

La cache NPZ dello helper 4B contiene un campo storico `case_sha256` che identifica i suoi dieci set originali. Questo è un metadato della dipendenza, non l'identità di un nuovo campione: ogni `reference_manifest.json` lega esplicitamente la cache ai coefficienti effettivi, ai percorsi e ai relativi hash. Non si confrontano hash di metadati di branch differenti come se fossero errori numerici.

Il budget è di **5400 s trascorsi dal primo controllo del benchmark**. È verificato per caso, per esecuzione C++ e prima di ogni batch expm, con riserva per arresto ordinato. `budget.json` registra la scadenza; non viene azzerata quando si riprende. Gli hash del solver, del driver e del binario, con il comando di compilazione, sono registrati nel manifest di build; ambiente e header sono identificati in `environment.json` e nella fotografia di preservazione.

## Grezzi e tempi osservati

Ogni output C++ conserva **4001 osservazioni orarie** da 0 a 4000 h. Il formato binario è little-endian float64, una riga di 124 valori: tempo, tre medie, 40 modi c, 40 modi mb e 40 modi md. Gli array `modal_and_mean_errors_dt*.npz` conservano gli errori con segno delle medie e di **tutti i modi a tutte le ore**, con la stessa griglia. Cache e output sono separati dalle tabelle compatte.

I CSV principali usano gli undici tempi fisici fissi già validati nella 4B: 0,1,2,4,8,24,100,500,1000,2000,4000 h. Contengono dati effettivi e non estrapolazioni. Gli errori relativi istantanei sono separati dai normalizzati e restano indefiniti quando il denominatore non è risolto. I p grezzi non vengono rimossi quando non sono vicini a 1. Le classificazioni di precisione/cancellazione/transitorio sono controlli numerici dichiarati, accompagnati dai dati per una revisione indipendente.

## Comandi da root del repository

Prima esecuzione, **solo in una directory nuova**, con questi script copiati in una nuova directory sorella sotto `UN_model/VERIFICATION/`:

```bash
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/setup_campaign.py
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/run_campaign.py benchmark
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/run_campaign.py run
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/collect_results.py
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/targeted_extension.py
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/collect_results.py --output UN_model/VERIFICATION/NEW_DIRECTORY/deliverables_final
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/validate_and_package.py --input-directory UN_model/VERIFICATION/NEW_DIRECTORY/deliverables_final
```

Prima della nuova esecuzione va salvata una nuova fotografia di preservazione in `preservation_before.json`, come quella della sessione consegnata: branch, HEAD, origin, hash del diff staged, numero di file staged, diff unstaged e hash dei file precedenti. Questo verbale non va copiato come se descrivesse una sessione futura.

Per riprendere **questa stessa campagna interrotta**, usare soltanto `run_campaign.py run`: i casi con `completed.json` sono saltati; le esecuzioni e le cache integre sono riutilizzate. Un processo C++ interrotto durante la scrittura viene conservato e ripetuto in una directory di tentativo nuova. Errori di integrità o assert scientifici non vengono aggirati. Se il budget è scaduto, fermarsi al checkpoint e consegnare i completati; non modificare `budget.json` per continuare oltre i 90 minuti senza una nuova istruzione.

`setup_campaign.py` rifiuta di rigenerare i campioni già congelati; raccolta, ZIP e checkpoint finale rifiutano la sovrascrittura della consegna. Perciò non rilanciare questi passi sopra i risultati conclusivi. I file precedenti della 4B, l'index Git, notebook, reference e sorgenti restano intatti.
