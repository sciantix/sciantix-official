# SCIANTIX UN L3 — integrazione e regressione completa, 19 ottobre 2026

## Esito

**FASE 2D completata e verificata; FASE 3 completata.** Il modello UN L3 è integrato nel normale ciclo del vero eseguibile SCIANTIX e supera il confronto con il notebook sui quattro casi congelati, per tutti i **3.012 timestep**. Nessuna FASE 4 avviata.

Il primo blocco ha verificato i primi tre passi di ciascun caso: **20.652 valori, errore zero**. Dopo il checkpoint `D_COMPLETE` è partita automaticamente la regressione completa: **4.637.652 valori confrontati, zero fallimenti**, errore normalizzato massimo **1,80918e-12**. Le configurazioni iniziali, i modi, le variabili microstrutturali, gli inventari e le transizioni di saturazione passano. Il gas verso il grain-face è calcolato autonomamente dal C++.

Riferimento immutato: `UN_model/notebooks/finalRizkUN_L3_solver_unificato.ipynb`, SHA-256 `e29d798d3685edb827a43933b3dc4400087f1aff488ee4080650c1430e1c59ff`. Nessun notebook fisico rieseguito per generare nuovi valori attesi. Utilizzati manifest, tracce e NPZ congelati nella FASE 1.

Artefatti principali: [README e riproduzione](../UN_model/phase2d3/README.md), [riepilogo JSON](../UN_model/phase2d3/results/summary.json), [errori aggregati per variabile](../UN_model/phase2d3/results/variable_errors.csv), [errori dettagliati per caso/stadio](../UN_model/phase2d3/results/P3_full/errors.csv), [checkpoint finale](../UN_model/phase2d3/checkpoints/FINAL_COMPLETE/manifest.json).

## Integrazione nel ciclo SCIANTIX

Nuova selezione esplicita **`iFuelMatrix=3`** per L3; rimangono invariati `0` UO2, `1` UO2/HBS e `2` UN legacy. `MainSCIANTIX` richiama la stessa funzione `Sciantix`, che attraversa `Simulation.initialize`, `execute`, `update`, `output`. `Simulation` possiede il modulo `un_l3::Model` definito nei nuovi `UNL3Model.h/.C`.

Lo stato completo è mantenuto internamente: tre vettori da 160 modi, medie, numero/raggio/volume delle popolazioni bulk e dislocazione, vacancy, produzione cumulativa, inventari di gas e grain-face, tempo, flag di primo gas e saturazione. Il buffer globale storico da 720 elementi e il numero storico di modi non vengono ampliati. I vettori L3 non vengono ricostruiti dagli scalari esportati tra due chiamate. Il test di persistenza riguarda ora l'effettivo ciclo dell'eseguibile, oltre al contratto di stato separato della 2A.

Il dispatch L3 ritorna prima degli aggiornamenti legacy di produzione, diffusione, bolle e rilascio. Il passo chiama i componenti già verificati nella sequenza Python: trasporto/nucleazione e scambio intragranulare, seed, vacancy, crescita/coalescenza, cattura modale; poi produzione cumulativa, deficit del gas rispetto al gas iniziale più prodotto, incremento entrante autonomo al bordo, grain-face/rilascio, aggiornamento del tempo e output. Il saldo verso il bordo usa le medie scalari del solver/cattura come Python; l'output ricostruisce le medie dai modi come Python. Questa distinzione evita di introdurre una differenza di arrotondamento nel bilancio.

Il codice fisico di 2B/2C e il solver non sono stati riscritti. L'unica aggiunta all'intragranulare è una diagnostica del numero di bolle catturate, usata per il cumulativo. Le altre modifiche orchestrano selezione, inizializzazione, tempo e I/O.

Temperatura, fission rate e stress sono letti dalla normale storia, il raggio del grano dalle condizioni iniziali. I quattro fission rate sono rispettivamente `5.767844763153473e19`, `7.498198192099515e19`, `7.498198192099515e19`, `7.76068019011021e19` fissioni/(m³·s). Gli altri 52 parametri predefiniti coincidono esattamente con ogni manifest: **208 confronti esatti** con i default stampati da un programma C++ che include lo stesso header di produzione.

Per questo profilo sono esposte le opzioni L3 congelate, 160 modi e dt nominale 10.800 s. Il Main conserva esattamente i valori delle storie costanti evitando il passaggio aritmetico moltiplicazione/divisione dell'interpolazione storica; l'ultimo passo è `min(10800, tempo_finale−tempo_corrente)`. Solo il nuovo selettore usa questo comportamento.

## Confronto completo

| Caso | Aggiornamenti | Errore normalizzato massimo | Errore modale normalizzato massimo | Massimo residuo massa / produzione |
|---|---:|---:|---:|---:|
| AP3.2 / 900 K | 605 | 1,293e-16 | 0 | 2,565e-15 |
| AP3.4 / 1600 K | 550 | 2,033e-15 | 0 | 2,026e-15 |
| AP3.4 / 1800 K | 550 | 1,809e-12 | 3,068e-15 | 2,498e-15 |
| ANP6 / 1600 K | 1.307 | 1,919e-16 | 0 | 3,336e-15 |

Confrontate tutte le **64 quantità di storia**, più inventario vacancy grain-face e due flag seed, **32 diagnostiche intermedie**, e tutti i vettori modali iniziali, dopo scambio, dopo cattura e a fine passo. Il conteggio comprende il confronto dello stesso vettore finale con due stadi congelati distinti, per verificare che il grain-face non alteri i modi. Non si tratta di 4,6 milioni di variabili indipendenti.

Errori massimi aggregati sui quattro casi, per alcune quantità principali:

| Variabile | Errore assoluto massimo, unità SI | Errore relativo massimo su riferimento non nullo |
|---|---:|---:|
| c, gas in soluzione [at/m³] | 6,291e6 | 6,015e-16 |
| mb, gas bulk [at/m³] | 3,436e10 | 1,000e-15 |
| md, gas dislocazione [at/m³] | 6,013e10 | 9,842e-16 |
| Nb [m⁻³] | 2,517e7 | 6,026e-16 |
| Nd [m⁻³] | 1,434e4 | 1,604e-15 |
| Rb [m] | 8,272e-25 | 2,954e-16 |
| Rd [m] | 1,588e-22 | 1,136e-15 |
| nvb, vacancy bulk [m⁻³] | 4,295e10 | 1,144e-15 |
| nvd, vacancy dislocazione [m⁻³] | 4,398e12 | 1,320e-15 |
| p_b [Pa] | 1,907e-6 | 6,163e-16 |
| p_d [Pa] | 2,794e-8 | 1,760e-15 |
| Swelling totale [-] | 8,188e-16 | 6,760e-15 |
| Gas totale giunto al bordo [at/m³] | 6,872e10 | 1,307e-14 |
| Gas trattenuto grain-face [at/m³] | 2,394e11 | 4,100e-14 |
| Gas rilasciato [at/m³] | 2,738e11 | 4,405e-12 |
| Vacancy grain-face [m⁻³] | 2,879e13 | 5,296e-14 |
| Copertura grain-face [-] | 1,077e-14 | 2,267e-14 |

Gli errori assoluti degli inventari vanno rapportati ai valori dell'ordine 1e23–1e27. Per il caso AP3.4/1800 K il massimo errore modale assoluto è 1,203e11 at/m³, con errore relativo massimo 3,068e-15; gli altri tre casi hanno modi esattamente uguali a Python. Tutti i campi e gli indici dei massimi sono nei CSV collegati, senza omissione delle quantità non riportate nella tabella.

### Tolleranze e vicinanza allo zero

Criterio definito prima dei test: `|C++−Python| ≤ 1e-11·|Python| + 64·epsilon·scala`, con epsilon float64. Sono mantenuti rtol e moltiplicatore di arrotondamento motivati nella 2A; l'integrazione esplicita il termine additivo per quantità ottenute sottraendo grandi inventari. La 2B/2C conserva i propri criteri originari.

Per le storie la scala ha le unità della variabile ed è il massimo assoluto congelato; per i modi è il massimo per riga/popolazione, almeno 1 at/m³. Gli inventari al bordo e il residuo di massa usano la produzione cumulativa come scala; le rispettive percentuali usano 100. Zeri identici, flag e contatori vengono controllati separatamente. Il guard di una curva scalare identicamente nulla è 1e-300. La formulazione del campo descrittivo nel JSON è stata precisata nella documentazione finale; **nessun numero o criterio è cambiato dopo i confronti**.

L'errore normalizzato riportato è `errore / max(|reference|,64·epsilon·scala/rtol)`. Il CSV include anche errore relativo puro e assoluto. Escludendo il residuo di massa, il massimo errore relativo puro è già **4,4051e-12**, inferiore a 1e-11: l'esito non dipende dall'allargamento di una tolleranza per far passare un campo fisico. Il residuo di massa è vicino allo zero e il suo errore relativo puro può raggiungere 2, dato non significativo; il bilancio è valutato rispetto alla produzione.

### Primo scarto e accumulo

Non esiste un primo superamento della tolleranza: tutti i confronti passano. Sono stati comunque localizzati i primi scarti non identici bit per bit, con indici di storia che includono la riga iniziale zero:

- AP3.2/900 K, riga 246: nucleazione `nu_b`, scarto 8 su circa 6,19e16; modi e stato restano esatti.
- AP3.4/1600 K, riga 544: geometria derivata grain-face, `A_gf` con scarto 1,262e-29 m², poi copertura/zeta; modi intragranulari esatti.
- AP3.4/1800 K, riga 5: `nu_b`, scarto 0,015625, quindi geometria bulk a livello di arrotondamento. La propagazione resta ai livelli quantitativi delle tabelle; il rilascio, ottenuto per differenza dopo saturazione, è il campo più sensibile.
- ANP6/1600 K, riga 594: `nu_b`, scarto 16 su circa 8,34e16; modi e stato restano esatti.

Nessuno scarto richiede un cambiamento fisico. L'analisi per quarti della storia e i grafici degli errori sono in [analysis.json](../UN_model/phase2d3/results/P3_plots/analysis.json). Le tracce congelate restano intatte.

## Inizializzazione, saturazione, passo finale e persistenza

Valori iniziali e primi tre passi sono esatti nei quattro casi. I contatori coincidono con il numero reale di chiamate e la produzione cumulativa con quella Python: nessun doppio aggiornamento. I tre vettori completi sono confrontati dopo ogni passo; non viene usata una fixture per reimpostarli. I flag di primo gas e saturazione sono monotoni e coincidono con quelli congelati. Il bilancio locale grain-face `(q_gf+q_rel)new − ((q_gf+q_rel)old + ingresso_C++)` ha errore normalizzato **zero** in tutti i casi.

Prima saturazione: AP3.4/1800 K alla riga di storia **539** (1617 h), ANP6/1600 K alla **736** (2208 h), esattamente come Python. Gli altri due casi non saturano. Controllate le righe prima dell'attraversamento, quella di attraversamento e la prosecuzione, inclusi rilascio e conservazione della massa.

Gli ultimi dt coincidono **esattamente** con i manifest, anche nei bit float64:

| Caso | Ultimo dt [s] |
|---|---:|
| AP3.2/900 K | 4782.800675923936 |
| AP3.4/1600 K | 5329.81879629381 |
| AP3.4/1800 K | 5329.81879629381 |
| ANP6/1600 K | 9198.628416094929 |

Per ciascun caso `boundary_checks.json` conserva i valori C++/Python iniziali, primo/terzo passo, ultimo passo e bordi di saturazione. Il residuo globale massimo è 3,33615e-15 della produzione, contro il limite 1e-11. La verifica controlla anche tempo finale esatto e singola inizializzazione dello stato.

ANP6 completo è stato ripetuto in un **processo indipendente**, con nuova directory contenente solo gli input: `output.txt`, diagnostiche e i due file modali sono identici byte per byte alla prima esecuzione. Non è una prova di restart esterno, che resta rinviato.

## Grafici

Otto figure, ognuna in PNG e PDF: per ogni caso un confronto a 16 pannelli e un grafico dell'accumulo degli errori scalari/modali. Linea continua Python, tratteggiata SCIANTIX. Produzione e grafici utilizzano soltanto risultati salvati, senza ricalcolo della fisica Python.

- [AP3.2/900 K — confronto](../UN_model/phase2d3/results/P3_plots/AP3p2_900K_comparison.png), [errori](../UN_model/phase2d3/results/P3_plots/AP3p2_900K_errors.png).
- [AP3.4/1600 K — confronto](../UN_model/phase2d3/results/P3_plots/AP3p4_1600K_comparison.png), [errori](../UN_model/phase2d3/results/P3_plots/AP3p4_1600K_errors.png).
- [AP3.4/1800 K — confronto](../UN_model/phase2d3/results/P3_plots/AP3p4_1800K_comparison.png), [errori](../UN_model/phase2d3/results/P3_plots/AP3p4_1800K_errors.png), [PDF](../UN_model/phase2d3/results/P3_plots/AP3p4_1800K_comparison.pdf).
- [ANP6/1600 K — confronto](../UN_model/phase2d3/results/P3_plots/ANP6_1600K_comparison.png), [errori](../UN_model/phase2d3/results/P3_plots/ANP6_1600K_errors.png).

## Altri test, baseline storica e tempi

Ripetuta la suite separata 2A sul sorgente integrato: **5.829.810 valori, errore zero**, identità fra solver diretto e adapter, CTest di stato **1/1 PASS**. Coefficienti/sorgenti di questa prova sono ancora quelli congelati: la verifica del solver resta distinta dalla nuova regressione fisica accoppiata.

AddressSanitizer e UndefinedBehaviorSanitizer sul vero eseguibile, su tutti i casi completi: **PASS**, stessi errori numerici della build ordinaria. LeakSanitizer disabilitato per la nota incompatibilità ptrace (`detect_leaks=0`); non si dichiara verificata l'assenza di leak con quel tool.

Baseline storica, in copia nuova e gold mode 0: **109/109 PASS**; legacy UN **91/91 PASS** alla sua tolleranza storica del 35%. I **110 output.txt** (109 standard più supplementare) coincidono byte per byte con la FASE 1; il CSV legacy coincide anch'esso. [Confronto alla baseline](../UN_model/phase2d3/results/P3_nonregression/comparison_to_phase1.json).

Problemi preesistenti confermati: Baker `analytics/test_Baker1977_1273K_porosityevolution` fallisce contro gold (1.127 discrepanze) ed è escluso dal selettore standard; Vercors5 conserva i tre NaN documentati alla riga dati 400 nelle colonne pressione parziale/potenziale ossigeno e alla 401 nella stechiometria di equilibrio. I relativi output sono identici ai precedenti. Il vecchio percorso del generatore notebook8 resta assente e non è stato rigenerato. L'audit termina con codice 1 per queste anomalie note; nessuna nuova regressione.

Ambiente: g++ 13.3.0, CMake privato 3.31.6, Python 3.12.3, NumPy 2.4.4; nessuna nuova dipendenza installata. Build normale CMake Release; sanitizer Debug con O1 e ASan/UBSan. Tempi wall misurati:

| Blocco | Tempo |
|---|---:|
| Configurazione/build FASE 3 Release | 1,864 / 22,034 s |
| Esecuzione AP3.2/900 K, inclusi output | 0,105 s |
| Esecuzione AP3.4/1600 K | 0,087 s |
| Esecuzione AP3.4/1800 K | 0,087 s |
| Esecuzione ANP6/1600 K | 0,203 s |
| Ripetizione indipendente ANP6 | 0,239 s |
| Build sanitizer / quattro esecuzioni | 66,052 / 1,526 s |
| Build baseline storica / suite standard | 21,353 / 18,451 s |

I confronti, la lettura delle tracce e la generazione dei grafici non sono inclusi nei tempi dei processi C++. Comandi, log e hash sono nei manifest.

## File modificati e protezione

Nove file preesistenti modificati:

- `include/classes/Simulation.h`: proprietà del modulo L3 e storia della chiamata.
- `include/models/UNState.h`: commento aggiornato sull'integrazione dello stato.
- `src/MainSCIANTIX.C`: storia costante esatta e gestione locale dei passi L3.
- `src/classes/Simulation.C`: dispatch e ciclo L3 separato.
- `src/file_manager/Initialization.C`: delega dell'inizializzazione L3 al modulo.
- `src/file_manager/InputReading.C`: validazione delle storie L3 supportate.
- `src/file_manager/Output.C`: output completo L3.
- `src/models/UNIntraGranular.C`: sola diagnostica `captured_bubbles`.
- `src/operations/UpdateVariables.C`: export scalare L3 senza toccare il buffer modale storico.

Nuovi file di produzione: `include/models/UNL3Model.h`, `src/models/UNL3Model.C`. Nuovi strumenti/artefatti in `UN_model/phase2d3/`: runner, analisi/grafici, verificatore, checkpoint, tolleranze, README, inventario iniziale, controlli e risultati. Aggiunto questo report. Nessuna modifica necessaria a CMake, che include già i sorgenti dei modelli.

[Verificati 6.741 file iniziali](../UN_model/phase2d3/preservation_check.json): notebook e reference Python, gold, UN legacy, altri combustibili, build e risultati precedenti e documenti della tesi preservati, salvo le nove modifiche autorizzate ai sorgenti. HEAD iniziale/finale invariato `016b4d25688354623e5d630efcc20b06967d0f59`; diff staged identico allo stato all'avvio. Nessuna cancellazione di dati o build originali, nessun commit o aggiornamento dell'index.

Due build iniziali fallite sono conservate: include `<cmath>` mancante, poi collisione del nome proposto `UNModel.h` con l'header legacy. L'header legacy è stato ripristinato e verificato byte per byte prima del test valido; il nuovo modulo usa il nome distinto `UNL3Model`. Nessuna modifica a reference, parametri fisici o soglie per correggere discrepanze.

Checkpoint locali con sorgenti ZIP, hash e test effettivamente passati: `D_COMPLETE`, `P3_FULL`, `FINAL_COMPLETE`. I primi due delimitano rispettivamente il passaggio sicuro alla FASE 3 e il confronto completo; il finale include anche memoria, baseline, grafici e preservazione. Le istruzioni del README permettono la ripresa senza ripetere l'analisi.

## Limiti e attività residue

L'equivalenza numerica è dimostrata per i quattro casi e la configurazione L3 congelata. Il selettore attuale richiede storie costanti, inizializzazione fresh-fuel e scaling storico unitario; storie arbitrarie, inizializzazioni diverse e varianti fisiche inattive non sono dichiarate verificate. Il restart/checkpoint esterno resta rinviato; la persistenza interna nel ciclo reale è invece implementata e verificata.

Restano aperti soltanto i problemi storici sopra elencati e le attività fuori perimetro. Questa regressione non costituisce verification analitica né validazione sperimentale aggiuntiva. La **FASE 4 non è iniziata**. Il lavoro autorizzato si ferma qui, senza blocchi scientifici ambigui emersi.
