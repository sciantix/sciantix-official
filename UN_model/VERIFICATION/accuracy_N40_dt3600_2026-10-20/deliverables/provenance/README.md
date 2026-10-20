# Accuratezza N=40, Δt=3600 s sui 160 casi congelati

Consegna canonica in `deliverables/`; archivio compatto `accuracy_N40_dt3600_compact.zip`, con dimensione e SHA-256 in `package_info.json`. Il report è `deliverables/REPORT_ACCURATEZZA.md`; la validazione finale è `deliverables/validation.json` e il checkpoint è `checkpoints/final/manifest.json`.

Sono riutilizzati i 160 output candidati e i 18 output N40 a 1800/900 s dei nove casi critici. Sono nuovi soltanto 18 output N80/N160 a 3600 s e 12 prove brevi/di prima ora su tre casi congelati. Nessun campione nuovo, nessuna ripetizione della campagna estesa o del candidato. Tutti i benchmark principali restano a coefficienti costanti per 4000 h, con gli stessi coefficienti, R, sorgente e iniziali della campagna estesa.

## File e definizioni

- `temporal_errors_hourly.csv.gz`: tabella lunga completa locale, **1.920.480 righe**, con riferimenti, errori con segno, assoluti, relativi istantanei, normalizzati e flag.
- `temporal_hourly_lossless.csv`: tabella compatta completa, **640.160 righe**, inclusa nello ZIP: tre riferimenti e tre errori con segno a tutte le 4001 ore di tutti i 160 casi. Non è una selezione temporale ridotta.
- `normalization_scales.csv`: scala e sensibilità numerica per caso e popolazione. Permette di ricostruire gli errori normalizzati dalla tabella compatta.
- `temporal_errors_common11.csv`: estratto leggibile alle undici ore comuni, dalla stessa reference oraria.
- `spatial_errors.csv`, `total_errors.csv`, `decomposition_errors.csv`: errori agli undici tempi originali e ai tempi arricchiti dei casi critici, con identità e dominanza delle componenti. Le percentuali non definite sono vuote; gli assoluti restano presenti.
- `observed_maxima.csv`: massimi assoluti, relativi istantanei e normalizzati, **ognuno con il proprio istante**, numero di campioni e definizione della griglia. Non sono massimi continui rigorosi.
- `worst_cases_common_sampling.csv`, `group_statistics_common_sampling.csv`: confronti uniformi separati per gruppo e popolazione. `worst_total_after_targeted_sampling.csv` aggiunge i picchi osservati nella griglia arricchita del sottoinsieme critico; non presume uguale densità per tutti i casi.
- `targeted_refinement_maxima.csv`, `targeted_refinement_errors.csv`, `targeted_costs.csv`: confronto fra N40/dt3600, N40/dt1800, N40/dt900, N80/dt3600 e N160/dt3600, sulle stesse osservazioni high-N per ciascun caso. I temporali sono confrontati sulle 4001 ore. Costi originali riutilizzati e costi nuovi sono distinti.
- `coefficient_regimes.csv`, `coefficient_associations.csv`: coefficienti, tempi caratteristici e associazioni di rango descrittive, non causali. Non sono una scelta automatica di N/dt.
- `rapid_short_errors.csv`, `rapid_short_maxima.csv`, `rapid_first_hour_comparison.csv`: prove a T=20τ e integrazione fine fino a 3600 s. Il candidato non riceve valori dentro il suo primo timestep.
- `sampling_coverage.csv`: ore effettivamente osservate per ogni caso, con rimando alla griglia arricchita.
- `timings.csv`, `metadata.json`, `validation.json`: misure, scopi e controlli.

Le concentrazioni e gli errori assoluti sono in at/m³; relativi e normalizzati sono rapporti adimensionali, non percentuali già moltiplicate per 100. Nelle righe `metric=relative_instantaneous`, il valore 0.2 significa 20%. `relative_valid_mask` nella tabella compatta usa bit **1=c, 2=mb, 4=md**; un bit zero indica percentuale non definita, non errore assoluto nullo. Per ogni popolazione:

```text
errore_assoluto = abs(error_population)
errore_relativo = abs(error_population / analytic_population), solo se il bit è presente
errore_normalizzato = abs(error_population) / temporal_N40_scale
```

Riferimenti ed errori con segno sono memorizzati esattamente in float64 leggibile CSV. La somma riferimento+errore ricostruisce la concentrazione C++ entro l'arrotondamento; il binario C++ originale è conservato per il dato bit per bit. Nello ZIP non si duplica la tabella lunga gzip da circa 100 MB: la tabella compatta conserva tutte le ore e consente di ricostruire tutte le misure di errore, non soltanto i massimi.

## Precisione e decomposizione

L'errore temporale su tutte le ore usa la cache expm N40 originale. Sulle griglie condivise si mantiene la decomposizione validata con il prefisso N40 della reference diretta high-N. Le due valutazioni numeriche dello stesso problema continuo differiscono soltanto per arrotondamento; `reference_form_gap` lo espone esplicitamente. Nessun file della reference è modificato. La scala temporale candidata è il massimo della sua popolazione nella cache N40; spazio e totale usano la scala high-N. I confronti dei raffinamenti impiegano un'unica scala high-N candidata per non confrontare normalizzazioni diverse.

I nuovi tempi high-N sono **ore intere**, per confrontarli con output candidati esistenti, e sono calcolati soltanto per i nove casi selezionati. L'identità dei prefissi modali consente di controllare il raddoppio N/2→N senza duplicare i calcoli degli stessi modi. Gli undici riferimenti originali restano invariati. Le osservazioni aggiunte sono controllate con entrambi i criteri 4B, ≤1% per l'incremento della media rispetto al troncamento N320 e per l'incremento relativo dell'energia della coda. Non è una soglia di accettazione del candidato.

Le prove brevi usano i tempi analitici previsti e il più vicino output fine, con scarto di clock float ≤2e-12 della durata, come nella 4B. La validazione registra lo scarto di campionamento e verifica l'effetto sul riferimento. Non vengono interpolati o mantenuti costanti valori del candidato fra 0 e 3600 s. Il confronto candidato/fine della prima ora è sempre a 3600 s.

## Riproduzione e checkpoint

Gli script importano in sola lettura la formulazione 4A e gli helper 4B validati. Build, nuove cache, output e documenti vivono soltanto qui. I grezzi già esistenti sono referenziati mediante percorso e checksum, non copiati o modificati.

Da root, in una **nuova directory sorella** sotto `UN_model/VERIFICATION/`, copiare gli script e registrare una nuova fotografia Git/file in `preservation_before.json` e un nuovo budget `budget.json` da 5400 s prima del benchmark. Non copiare il verbale della sessione come se descrivesse uno stato futuro. Comandi:

```bash
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/baseline.py benchmark
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/baseline.py all
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/targeted.py benchmark
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/targeted.py all
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/collect.py
.venv/bin/python -B UN_model/VERIFICATION/NEW_DIRECTORY/validate_package.py
```

I primi due passi non lanciano C++. I casi già completati e gli output numerici integri sono riutilizzati. `baseline/<case>/completed.json`, `targeted/<case>/completed.json` e `rapid/<case>/completed.json` sono checkpoint progressivi. Non rilanciare raccolta e confezionamento sopra la consegna conclusiva. La selezione avviene dai picchi per gruppo/popolazione/tipo/normalizzazione, senza accettazione o rifiuto all'1%/3%. Il budget non viene azzerato durante una ripresa; a scadenza si consegnano i checkpoint completati.

Il primo tentativo del benchmark mirato ha completato i calcoli ma si è fermato nel CSV dei costi per colonne eterogenee. La correzione dell'esportazione e la ripresa con cache sono documentate nei log e nel benchmark; nessuna simulazione identica è stata ripetuta.
