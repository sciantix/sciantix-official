# Estrazione delle storie dei coefficienti UN

Il file da caricare è `UN_coefficients_histories.csv` (UTF-8, separatore virgola).
Sono conservate tutte le chiamate temporali al solver disponibili o generate
per i quattro punti AP3.2/900 K, AP3.4/1600 K, AP3.4/1800 K e ANP6/1600 K.

- `BaseRizk2025_uncalibrated`: configurazione di riferimento esattamente come
  presente in `UN_model/notebooks/BaseRizk2025.ipynb`, comprese le scelte
  numeriche e fisiche attive del notebook. Non è una ricostruzione autonoma
  delle equazioni dell'articolo. Esecuzione del runner case-specific originale.
- `M7_no_phi_nominal`, `M7_full_nominal`: famiglie disponibili nello script
  M7 v14, attivate tramite la funzione originale `set_model_family`, con i
  parametri nominali originali di `UNParameters`. Non sono nuove calibrazioni
  né risultati di un candidato calibrato recuperato: non sono stati trovati
  candidati calibrati salvati con questi nomi. Temperatura, burnup finale e
  fission rate del caso sono quelli degli `EXPERIMENT_CASES` di BaseRizk2025;
  Δt=1 h e 40 modi sono i valori `FINAL_*` dello script M7 originale.
- `M7_v14_capture_only_fixed_rank1`: candidato esplicito del notebook M7 v14
  fixed-parameter, eseguito attraverso il suo wrapper `run_model_point_rhoFT2`.
  Il fission rate calibrato del candidato è mantenuto, anche quando differisce
  da quello specifico del pin: è riportato nella colonna `fission_rate_m3_s`.
- `UN_L3_frozen`: lettura delle quattro tracce complete già congelate della
  FASE 1, senza rilanciare il modello L3.

`Dg` è in m²/s; `gb`, `gd`, `bb`, `bd` in s⁻¹. `bb` e `bd` sono sempre gli
argomenti **effettivi** `b_b_gas`, `b_d_gas` passati alla funzione di exchange,
quindi comprendono φ quando attivo. Non sono le sole rate grezze `b_b`, `b_d`.

`tempo_s` è l'inizio dell'intervallo cui si applicano i coefficienti;
`tempo_fine_s` è il termine effettivo, incluso l'ultimo passo parziale.
Il primo intervallo comincia a zero. L'ultima riga rappresenta i coefficienti
effettivamente usati nell'ultimo passo: non viene inventato un aggiornamento
ulteriore dei coefficienti all'istante finale. `dt_s` rende esplicita la durata.
`n_modes`, fission rate, percorso sorgente e SHA-256 accompagnano ogni riga.
Gli identificativi `AP3p2`, `AP3p4` corrispondono ad AP3.2 e AP3.4.

`manifest.json` conserva parametri completi, provenienza, numero di righe e
checksum. `raw/` conserva le estrazioni per serie, riutilizzate al rilancio.
`extract.py` osserva gli argomenti e richiama la stessa funzione originale con
gli stessi argomenti; non sostituisce la fisica. Esegue soltanto definizioni e
runner puntuali, senza sweep, optimizer o grafici. L'unica impostazione
rilocalizzata durante il caricamento è la directory di output. I file sorgenti
e lo stato Git iniziale sono verificati senza modifiche.

Riproduzione dalla root: `.venv/bin/python -B UN_model/VERIFICATION/coefficient_histories_2026-10-20/extract.py`.
Nessun confronto scientifico o con i dati LHS è stato effettuato.
