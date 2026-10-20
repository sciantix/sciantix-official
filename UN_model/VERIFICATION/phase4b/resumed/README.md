# FASE 4B ripresa — 110 configurazioni principali e 60 test rapidi completati

- [Report scientifico e limiti](REPORT_FASE4B_COMPLETA.md).
- [Proposta LHS, non eseguita](PROPOSTA_LHS.md).
- [Protocollo congelato e hash dei dieci set](configuration.json).
- [Risultati numerici principali](analysis_v2/summary.json), [risultati per caso](audit/case_findings.csv).
- [Validazione finale](validation.json), [checkpoint](checkpoints/completed/manifest.json).

Tutto il nuovo lavoro è qui. I file della 4A e del precedente checkpoint di arresto 4B sono immutati. Il vecchio report di costo descrive correttamente il suo protocollo originale; non viene aggiornato retroattivamente. La nuova campagna segue l'opzione di decomposizione approvata successivamente.

## Script e dati

| Script | Funzione |
|---|---|
| `reference.py` | Caching expm, controlli del semigruppo e reference high-N |
| `campaign.py` | Build distinta, 50+60 configurazioni, riusi con hash e 60 prove rapide |
| `analyze.py` | Errori con segno, norme, ordini, storie e limiti stazionari separati |
| `audit_results.py` | Check BE sugli altri N, ordini per modo e costi per caso |
| `rapid_modes.py` | Errori e ordini di ogni modo nelle prove brevi |
| `plot_results.py` | Otto grafici PNG/PDF della campagna principale |
| `validate_final.py` | Preservazione, risultati, hash e checkpoint della sessione |

`results/` contiene manifest e output C++ nuovi; per le esecuzioni riutilizzate `cpp.json` rimanda al percorso originale e ne registra SHA-256. `cache/` conserva le soluzioni continue, i controlli e tutte le espansioni high-N. Non viene riscritto alcun file delle cache precedenti. Le due popolazioni attive a gb=0 sono risolte nel sottospazio invariante; bulk resta zero senza clipping.

`analysis_v2/` è la sola analisi canonica. `analysis/` conserva un tentativo interrotto subito, prima di produrre risultati numerici, per una forma RHS non compatibile con NumPy 2; l'adattatore è corretto e il log è mantenuto. Non rappresenta un fallimento del solver.

Gli errori modali completi possono essere ricostruiti da output binari little-endian float64 e cache: righe di `4+3N` valori `[tempo,media_c,media_mb,media_md,modi_c,modi_mb,modi_md]`. I CSV compressi sono normali CSV gzip. Gli errori temporali orari e le decomposizioni spaziali agli undici tempi sono distinti. Le norme sono calcolate dopo la decomposizione vettoriale.

## Riproduzione senza sovrascrivere la consegna

Gli script importano la `.venv` e le dipendenze SciPy locali già presenti nella 4A. Non serve installare o modificare l'ambiente. Le directory dei nuovi output rifiutano la sovrascrittura; `campaign.py` può riprendere le esecuzioni usando manifest e hash già presenti. Non rilanciare `benchmark.py`, che rigenererebbe i file di setup della vecchia 4B.

Per una riproduzione numerica indipendente, copiare **solo i sette script elencati** in una nuova directory sorella sotto `UN_model/VERIFICATION/phase4b/`, per esempio `reproduction_new/`, lasciando lì esclusivamente file nuovi. Gli import trovano gli stessi dati congelati 4B e gli stessi sorgenti 4A. Da root del repository:

```bash
.venv/bin/python -B UN_model/VERIFICATION/phase4b/reproduction_new/campaign.py all
.venv/bin/python -B UN_model/VERIFICATION/phase4b/reproduction_new/analyze.py
.venv/bin/python -B UN_model/VERIFICATION/phase4b/reproduction_new/audit_results.py
.venv/bin/python -B UN_model/VERIFICATION/phase4b/reproduction_new/rapid_modes.py
.venv/bin/python -B UN_model/VERIFICATION/phase4b/reproduction_new/plot_results.py
```

Nessuna di queste istruzioni avvia un LHS o modifica Git. Il protocollo può riutilizzare i sei output C++ precedenti identici; per controllare una nuova esecuzione dello stesso caso, modificare soltanto la politica di riuso nella **copia degli script**, mantenendo i parametri e le reference.

`validate_final.py` è un verbale della sessione consegnata: richiede il suo `preservation_before.json` e un checkpoint finale non ancora esistente. Non è necessario rilanciarlo sopra il checkpoint consegnato; gli hash possono essere verificati dal manifest. Per una nuova sessione di preservazione occorre una nuova fotografia iniziale, prima delle scritture.

## Numeri da non confondere

110 etichette di configurazione principali = 94 nuove esecuzioni + 16 riusi, di cui 10 comuni ai due studi e 6 precedenti. Sono 100 IVP/griglie C++ distinti nelle campagne principali. Le 60 simulazioni rapide sono tutte aggiuntive e nuove. Tutti i 110 confronti richiesti hanno risultati effettivi; non sono stati sostituiti da estrapolazioni. Nessun campione LHS.
