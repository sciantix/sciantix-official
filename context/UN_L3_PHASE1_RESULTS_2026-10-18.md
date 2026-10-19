# UN L3 — riferimento Python e baseline SCIANTIX

Data: 18 ottobre 2026. FASE 1 completata, baseline pre-porting eseguita.
**FASE 2 non iniziata; nessuna modifica alla fisica o ai sorgenti C++.**

Il riferimento confermato dall'utente è
`UN_model/notebooks/finalRizkUN_L3_solver_unificato.ipynb`, SHA-256
`e29d798d3685edb827a43933b3dc4400087f1aff488ee4080650c1430e1c59ff`.
L'utente ha confermato l'identità con il file originariamente chiamato `(3)`;
la questione del nome è chiusa. Branch `feature/un-l3-porting`, HEAD invariato
`8f5bfc7e123b0241507c5d1e587a28d20e4dcb24`.

## 1. Artefatti e istruzioni

| Artefatto | Percorso |
|---|---|
| Runner isolato, verificatore e istruzioni | [`UN_model/regression_reference/README.md`](../UN_model/regression_reference/README.md) |
| Riferimento completo canonico, quattro casi | [`results/full/manifest.json`](../UN_model/regression_reference/results/full/manifest.json) |
| Diagnostici a durata ridotta, con trace dei componenti | [`results/diagnostic_with_function_trace/manifest.json`](../UN_model/regression_reference/results/diagnostic_with_function_trace/manifest.json) |
| Ripetizione completa indipendente senza osservazione | [`results/reproducibility_full_plain/manifest.json`](../UN_model/regression_reference/results/reproducibility_full_plain/manifest.json) |
| Ripetizione strumentata AP3.4/1800 K | [`results/reproducibility_full_observed/manifest.json`](../UN_model/regression_reference/results/reproducibility_full_observed/manifest.json) |
| Risultati delle verifiche Python | [`results/verification.json`](../UN_model/regression_reference/results/verification.json) |
| Sintesi CSV dei quattro casi | [`results/full_case_summary.csv`](../UN_model/regression_reference/results/full_case_summary.csv) |
| Grafico delle storie salvate | [`results/reference_overview.pdf`](../UN_model/regression_reference/results/reference_overview.pdf) |
| Baseline: istruzioni e inventario | [`UN_model/baseline_sciantix/README.md`](../UN_model/baseline_sciantix/README.md) |
| Build/configurazione/provenienza baseline | [`baseline manifest`](../UN_model/baseline_sciantix/runs/pre_port_2026-10-18/manifest.json) |
| Esiti storici standard | [`report.html`](../UN_model/baseline_sciantix/runs/pre_port_2026-10-18/report.html), [`case_results.json`](../UN_model/baseline_sciantix/runs/pre_port_2026-10-18/case_results.json) |
| Audit esteso, caso supplementare e NaN | [`audit.json`](../UN_model/baseline_sciantix/runs/pre_port_2026-10-18/audit.json), [`preexisting_issues.json`](../UN_model/baseline_sciantix/runs/pre_port_2026-10-18/preexisting_issues.json) |
| Conservazione del workspace | [`workspace_preservation_check.json`](../UN_model/baseline_sciantix/workspace_preservation_check.json) |

Ogni caso Python include parametri effettivi completi, condizioni iniziali, storia CSV
e NPZ float64, stato finale, rate finali, warning/output, hash degli artefatti.
I casi strumentati includono `trace.jsonl.gz` e `modes.npz`: coefficienti di tutte e tre
le popolazioni iniziali, post-exchange, post-cattura e a ogni fine passo.
I trace includono anche variabili interne di seed, vacancy implicite, LimitedGrowth,
grain-face 3×3, attraversamento della saturazione e rilascio.

Le istruzioni specificano unità, ordinamento dei modi, interpretazione temporale degli
stadi e comandi effettivamente eseguiti. I file di output non sovrascrivono dati preesistenti.

## 2. Metodo e configurazione Python

Il runner carica in un nuovo modulo soltanto le celle necessarie, senza richiamare
la griglia completa o utilizzare lo stato del kernel Jupyter. Il factory originale
case-specific costruisce tutti i parametri. L'osservazione aggiunge solo chiamate
di lettura dei locali: rimuoverle restituisce esattamente l'AST originale.
Una seconda modalità `--plain` esegue il solver originale senza alcuna osservazione.

Configurazione preservata: dt=10800 s, N=160, Rgrain=6e-6 m, fn=4.2e-4,
Kd=4.1e5 bolle/m, rho=3.5e13 m^-2 costante, scala gas line sink 17,
scala vacancy dislocazioni 10, Ngf0=1e13 m^-2, D2 Xe con peso zero,
diffusività `rizk2025_refit_plot`/`rizk2025_refit_full`, GB `rizk_legacy_1e6_Dv1`.
Cattura, accoppiamento massa nucleazione, grain-face e first-gas seed attivi;
phi nella re-solution gas, seed iniziale dimero e densità dinamica disattivati.
Vacancy absorption-only False, sia intragranulari sia GF.
Il manifest conserva anche tutti gli altri valori, le costanti e le tabelle.

I diagnostici terminano dopo tre passi di 3 h, in cartelle distinte: non sono risultati
al burnup finale. È stato eseguito prima il controllo diagnostico; solo dopo il suo
confronto esatto con un altro processo sono stati eseguiti i quattro casi completi.
È stato poi esteso il trace ai ritorni dei componenti e ripetuto il controllo diagnostico.

## 3. Risultati completi e tempi misurati

Tempi wall-clock del solver con osservazione/compressione del trace incluse:

| Caso | Fdot [fissioni m^-3 s^-1] | Passi | Tempo finale [h] | Ultimo dt [s] | Solver osservato [s] |
|---|---:|---:|---:|---:|---:|
| AP3.2 / 900 K | 5.767844763153473e19 | 605 | 1813.328555743312 | 4782.800675924 | 2.224 |
| AP3.4 / 1600 K | 7.498198192099515e19 | 550 | 1648.480505221193 | 5329.818796294 | 1.975 |
| AP3.4 / 1800 K | 7.498198192099515e19 | 550 | 1648.480505221193 | 5329.818796294 | 1.965 |
| ANP6 / 1600 K | 7.760680190110210e19 | 1307 | 3920.555174560026 | 9198.628416095 | 4.746 |

Il processo dei quattro casi completi, incluso caricamento/salvataggio, ha richiesto
12.923 s. I diagnostici strumentati richiedono circa 0.011–0.014 s per caso.
La ripetizione indipendente senza osservazione ha richiesto 2.948 s complessivi:
tempi solver 0.401, 0.438, 0.415 e 0.985 s rispettivamente.
L'ulteriore ripetizione strumentata AP3.4/1800 K ha richiesto 2.139 s di solver,
3.039 s di processo. I tempi sono misure di questa esecuzione, non prestazioni del futuro C++.

| Caso | Swelling bulk [%] | Dislocazioni [%] | GF [%] | Totale [%] | Gas rilasciato [% prodotto] |
|---|---:|---:|---:|---:|---:|
| AP3.2 / 900 K | 0.567059407 | 0.364194282 | 0.031704147 | 0.962957836 | 0 |
| AP3.4 / 1600 K | 0.733118927 | 2.707517783 | 0.904603100 | 4.345239810 | 0 |
| AP3.4 / 1800 K | 0.445226383 | 10.385587950 | 1.861212153 | 12.692026486 | 0.159272954 |
| ANP6 / 1600 K | 2.261114715 | 3.039092518 | 2.779742811 | 8.079950044 | 2.087261727 |

Questi sono output del riferimento approvato; non è stata effettuata calibrazione
o modifica per avvicinarli agli esperimenti. Il primo stato saturo è a 1617 h
per AP3.4/1800 K e a 2208 h per ANP6/1600 K. Gli altri due casi restano sotto saturazione.
Il rate AP3.8, pur senza un quinto caso completo richiesto, rimane distinto e registrato
nelle impostazioni originali: 6.863735268152632e19 fissioni m^-3 s^-1.

## 4. Verifiche eseguite

- Tutte le variabili storiche di tutti e quattro i casi completi coincidono **esattamente
  in float64**, a ogni timestep, con una nuova esecuzione indipendente del solver originale plain.
- Lo stesso confronto è passato per tutti i diagnostici, sia con il primo osservatore
  sia con il trace esteso dei componenti.
- La ripetizione strumentata AP3.4/1800 K ha prodotto `history.csv`, `trace.jsonl.gz`,
  `modes.npz` e `last_rates.json` identici byte per byte.
- Nessun warning numerico, nessuna variabile storica non finita e nessuna media
  negativa che attivi il reset. Un solo first-gas seed per popolazione in ciascun caso.
- Ricostruzione delle medie dai modi salvati esattamente uguale alla storia.
  Cattura: modi c invariati; errore normalizzato di conservazione mb+md per modo ≤2.22e-16.
- Residuo massimo gas normalizzato alla produzione finale ≤1.961e-15.
  `q_gb=q_gf+q_rel`; copertura del ramo saturo rispettata entro la soglia originale 5e-10.
- Hash del notebook, delle celle caricate e di ogni output di caso verificati.

I JSON dei rate contengono anche NaN diagnostici di componenti non applicabili,
esplicitamente etichettati; non sono stati trasformati in zeri. Le storie fisiche
complete non contengono NaN/Inf.

Un primo tentativo si è interrotto prima della simulazione durante la serializzazione
di un ndarray delle impostazioni. Il serializzatore è stato corretto; il tentativo
è conservato separatamente in `diagnostic_setup_attempt`. Nessuna equazione cambiata.

## 5. Baseline SCIANTIX pre-porting

Installazione privata della sola dipendenza mancante CMake 3.31.6 sotto
`/tmp/sciantix-un-l3-phase1-tools`. Nessuna modifica a `.venv`, pacchetti di sistema
o strumenti già presenti. Il primo accesso alla rete nel sandbox non risolveva il DNS;
la ripetizione autorizzata fuori sandbox ha installato il wheel richiesto.
SciPy resta non installato: non necessario a questi run; la verifica analitica è rinviata.

Build in una snapshot nuova, separata da `build/` dell'utente:

- C/C++ GNU 13.3.0, CMake 3.31.6, `CMAKE_BUILD_TYPE=Release`;
- `COUPLING_TU=OFF`, flag effettivi e comandi in cache/compile_commands;
- configure 2.195 s, build 18.054 s, entrambi exit 0;
- warning di deprecazione del requisito minimo CMake, nessun errore di compilazione;
- CTest non registra test nell'HEAD corrente; inventario salvato, nessun falso conteggio di test unitari.

La suite storica standard (`--all --mode-gold 0 -j 2`) ha richiesto 18.023 s:

| Gruppo | PASS | FAIL |
|---|---:|---:|
| baker | 9 | 0 |
| cornell | 9 | 0 |
| white | 43 | 0 |
| kashibe | 23 | 0 |
| talip | 5 | 0 |
| oxidation | 4 | 0 |
| chromium | 3 | 0 |
| contact | 1 | 0 |
| hbs | 1 | 0 |
| vercors | 1 | 0 |
| analytics selezionato da `--all` | 1 | 0 |
| gpr | 9 | 0 |
| **Totale standard** | **109** | **0** |

La FASE 0 aveva inventariato 110 directory caso. L'esecuzione dimostra che `--all`
ne seleziona soltanto 109. Il caso restante è stato eseguito separatamente (0.032 s):
**`analytics/test_Baker1977_1273K_porosityevolution` FAIL** contro il gold corrente.
Presenta 1127 celle oltre le soglie storiche, con primi scarti già alla riga iniziale:
probabilità venting 0.05874837 contro 0.02712685; porosità aperta 0.001455292 contro
0.001455359. Il prefisso analytics del runner è `test_powerPulse`, quindi questo
caso Baker non era incluso. Nessuna modifica per risolverlo in questa fase.

Un audit aggiuntivo di finitudine ha rilevato tre NaN in `vercors/test_Vercors5`,
presenti anche nel gold: pressione parziale ossigeno e potenziale ossigeno alla riga 400,
deviazione stechiometrica di equilibrio alla riga 401. Il comparatore storico li lascia
passare. Perciò i 109 PASS standard non equivalgono a 109 output interamente finiti;
108 dei 110 casi inventariati superano anche il controllo aggiuntivo.
Il dettaglio è conservato senza cambiare il comparatore storico.

Separatamente, suite UN notebook-8 legacy: 7 casi puntuali, **91 confronti PASS**,
più esecuzione del caso storia; 0.724 s, exit 0 con `--strict --no-plot`.
La tolleranza relativa legacy è 35%: questo esito non prova equivalenza L3.
Non è stato rigenerato il riferimento notebook-8, il cui generatore punta ancora
al vecchio percorso del notebook ora archiviato.

Tutti i gold della snapshot sono immutati. Tutti i sorgenti/header/build copiati
coincidono con quelli correnti verificati. Il binario costruito, gli input, gli output
dei casi e gli oggetti restano nella snapshot locale; i report principali sono esterni
alla snapshot e quindi visibili anche come nuovi artefatti Git.

## 6. Protezione dei file dell'utente

Verificati SHA-256 di **2350 file preesistenti**, inclusi tutti i file tracciati
presenti e il binario originale in `build/`: nessuna modifica o cancellazione.
Hash dello staged diff invariato:
`ee2a381549a8c1c6999dcfcb959b10ad1be72fc55be45f3ed433406e38ad116e`.
Restano staged soltanto le due modifiche iniziali dell'utente relative a BaseRizk2025.
Nessun commit, pulizia del repository o modifica alla tesi Word.

I nuovi file sono runner e strumenti Python, documentazione e risultati sotto
`UN_model/regression_reference/`, `UN_model/baseline_sciantix/` e questo report.
Nessun nuovo programma fisico C++ è stato introdotto.

## 7. Piano aggiornato per la FASE 2, da autorizzare separatamente

1. Congelare questa reference come oracolo numerico. Usare inizialmente i tre passi
   diagnostici con medesimi parametri, poi le quattro storie complete; nessun nuovo fit.
2. Aggiungere configurazione tipizzata e **stato interno L3 posseduto da Simulation**:
   tre vettori dinamici, inventari, flag seed/saturazione, cumulata produzione e gas.
   Mantenere esplicito il nuovo percorso UN e separato il comportamento legacy.
3. Prima tranche: trasporto e lookup, trapping/re-solution/nucleazione, proiezione e
   ricostruzione. Testare direttamente il vero solver exchange SCIANTIX contro modi/rate
   salvati; nessun cambio globale a buffer 40/720 o costanti UO2.
4. Seconda tranche: seed/vacancy, Nb, crescita/coalescenza e cattura modo per modo.
   Usare le tracce per individuare il primo aggiornamento divergente, mantenendo l'ordine Python.
5. Terza tranche: GF LimitedGrowth, 3×3, primo crossing e ramo già saturo, verificando
   separatamente qgb/qgf/qrel. Entrambi i rami saturi sono coperti dalle storie complete.
6. Integrare nel normale percorso `Sciantix`/`Simulation`, senza doppio GasDiffusion/GasRelease.
   Dimostrare la persistenza di modi e flag attraverso successive chiamate temporali.
   **Rinviare l'API avanzata di checkpoint/restart esterno**, come precisato dall'utente;
   la persistenza interna è requisito della prima integrazione.
7. Eseguire confronti temporali e modali e poi ripetere la baseline UO2/legacy, mantenendo
   visibili il FAIL Baker e i NaN Vercors già presenti. Non aggiornare i gold.
8. Verifica analitica e cartella `UN_model/Verification/` soltanto dopo la regressione C++–Python.

La riproducibilità Python è dimostrata. Il porting C++–Python **non è ancora stato
implementato né verificato**. Il lavoro si arresta qui al confine autorizzato.
