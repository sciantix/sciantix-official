# Campagna L3 OAT — registro decisionale

## Protocollo

Fisica e numerica del notebook consegnato: 3 h, 160 modi, 900–1800 K ogni 25 K, quattro casi con i fission rate a piena precisione originali. Nessun optimizer e nessuna funzione di merito aggregata. Ogni batch viene definito dopo aver letto i risultati del precedente. AP3.2 è escluso dalla selezione. Per ogni osservabile si calcolano errori separati, interpolando linearmente i risultati sulla griglia alle temperature misurate. Il punto AP3.8 a 899 K è confrontato con il bordo a 900 K, senza alterare la griglia originale; è marcato nel CSV dei residui.

La soglia 1500 K serve a separare le metriche low/high, senza limitare la valutazione del raggio: tutti i suoi punti fino a 1722.22 K entrano nel confronto. ANP6 oltre 1590 K è estrapolazione, senza target fittizi. Il gas partition e FGR sono diagnostiche, non misure sperimentali. N_d non guida la scelta dei candidati.

## Verifica iniziale

Letto integralmente `RizkUNcalibrazione(1).md`. Copiati i tre originali in `runs/baseline/originals`; salvati SHA256. Le celle eseguibili 1, 2, 6, 8, 10 sono caricate testualmente, senza patch fisiche. Eseguita la funzione `main()` del notebook, conservando CSV e la suite completa dei grafici originali. Le 148 righe e 139 colonne coincidono con lo ZIP: tolleranza relativa 1e-12, assoluta zero, NaN identici. Il report precedente riporta valori indicativi leggermente diversi (p.es. S_d AP3.4 a 1600 K circa 2.68%; il notebook corrente e ZIP concordano su 2.707518%): il riferimento di questa campagna è il notebook corrente riprodotto.

I dati N_d sono i 28 punti incorporati nel notebook (1156.143345 K, 1.67973237e19 m^-3 come primo punto), con etichetta Rizk Fig. 7. La tabella obsoleta in `UN_model/src/un_data.py` inizia da 1153 K e 5.22e19 m^-3, e non viene importata. Non viene effettuata una nuova digitalizzazione del paper: la provenienza operativa resta il notebook fornito dall'utente. I blocchi markdown storici del notebook descrivono anche impostazioni Matthews ormai superate: fanno fede i valori eseguiti della cella 1, che attivano Rizk.

Bilancio finale baseline: massimo residuo relativo 1.70e-15; nessuna grandezza essenziale negativa/non finita e nessuna violazione del criterio single-size del notebook. La chiusura del gas al bordo usa il complemento del gas trattenuto nel solver: il solo residuo di massa verifica il bookkeeping, non dimostra da solo convergenza del flusso diffusivo.

## Batch 1 — sensitività locali attorno a L3

Domanda: quale leva aumenta il raggio senza amplificare lo swelling high-T già eccessivo di AP3.4 e l'estrapolazione ANP6?

Otto candidati: f_n ±10%, K_d ±15%, Dg_d ±10%, rho_d -10%, Dv_d +20%. I test Dv e rho sono unilaterali; non si pretende una derivata centrale per questi due parametri. N_gf,0 resta fisso: nessun problema intergranulare sperimentale giustifica usarlo come leva di fit. Si verificano localmente alcune direzioni già note, senza ripetere gli sweep estesi del vecchio report.

Ipotesi: K_d ridotto può aumentare il raggio tramite riduzione del numero; f_n aumentato o Dg ridotto potrebbero compensare lo swelling. Le direzioni inverse vengono misurate, non assunte. Tutte le simulazioni salvano diagnostiche lungo l'intera storia; le storie complete a 1200, 1600, 1800 K sono archiviate in NPZ.

Risultati: K_d -15% abbassa RMSE raggio high-T da 49.86 a 41.27 nm e ANP6 da 0.348 a 0.232 punti percentuali, ma peggiora AP3.4 da 1.267 a 1.518 e aumenta S_ANP6(1800) da 12.185% a 14.175%. Dg -10% riduce S_ANP6(1800) a 9.778% e RMSE AP3.4 a 1.009, ma peggiora AP3.8 (0.425) e ANP6 (0.584), e peggiora il raggio. Dg +10% fa il contrario; non è una soluzione del problema high-T. rho -10% riduce swelling ma il raggio ha segno variabile: +0.35% a 1200 K, -1.22% a 1600 K, -3.21% a 1800 K per AP3.4. Dv +20% cambia S_d AP3.4 a 1200 K di +3.90%, a 1600 K solo +0.056%: la precedente insensibilità vale soprattutto high-T e non va estesa alla banda fredda.

Tutte le otto simulazioni sono numericamente regolari. La gas partition resta più vicina a L3 per K_d che per Dg; quest'ultima cambia il sink delle dislocazioni. La riduzione f_n non distingue abbastanza il recupero intorno a 1590 K dall'aumento a 1700–1800 K.

Decisione: esplorare K_d ridotto con Dg ridotto o f_n aumentato, verificando anche la correzione Dv limitata alla bassa temperatura. Evitare ulteriore Dg isolato crescente e sweep Dv estesi: non affrontano il conflitto osservato.

## Batch 2 — compensazioni guidate dalle sensitività locali

Sei candidati dichiarati in `scripts/batch_02.json`. K_d -15% con Dg -6%, oppure f_n +25%, più una combinazione distribuita f_n +10% / Dg -2%. Un candidato aggiunge Dv +20% al primo per recuperare il raggio low-T; uno estende K_d -30% / Dg -12% per verificare i limiti della compensazione; uno usa rho -5% al posto del correttore di trasporto.

Domanda: la riduzione del numero di bolle può conservare il guadagno del raggio mentre un secondo parametro riporta lo swelling vicino a L3? Nessuna delle combinazioni è pre-selezionata come migliore. Si confronteranno separatamente le tre curve di swelling, le bande del raggio e la partition.

Risultati: la compensazione K_d -15% / Dg -6% mantiene quasi inalterate le curve di swelling (RMSE AP3.8/AP3.4/ANP6 = 0.2245/1.2794/0.3571 contro 0.2263/1.2672/0.3482 L3), con raggio high-T RMSE 44.85 anziché 49.86 nm. Il punto K_d -30% / Dg -12% porta il raggio high-T a 38.12 nm, ma AP3.4/ANP6 salgono a 1.3169/0.3555 e S_ANP6(1800) a 12.411%. Il recupero del raggio è maggiore, ma non risolve né gli ultimi punti ANP6 né la curvatura high-T.

K_d -15% / f_n +25% offre raggio high-T simile (44.49 nm), ma distanza D_cal maggiore (0.0489 contro 0.0308) e peggioramento AP3.4 maggiore; non viene approfondito. Aggiungere Dv +20% al K_d-Dg cambia il raggio high-T di soli 0.023 nm in RMSE: beneficio insufficiente a giustificare un terzo parametro. La combinazione a tre parametri f_n-K_d-Dg non prevale sul piano dei compromessi. K_d -15% / rho -5% migliora AP3.8 e ANP6 (0.2105/0.3166), raggio high-T 42.10 nm, ma peggiora AP3.4 a 1.3510 e l'estrapolazione ANP6(1800) a 12.753%; merita un solo raffinamento, riducendo ulteriormente rho per frenare high-T.

Decisione: terzo batch di sei prove, con due piccoli spostamenti Dg attorno al punto K_d -30%, un punto intermedio più conservativo K_d -20%, un raffinamento K_d-rho. Chiudere anche le sensibilità diagnostiche Dv (verso opposto) e N_gf,0 (controllo unilaterale). Non estendere f_n o Dv per inseguire il raggio high-T.

## Batch 3 — fine tuning e controlli diagnostici

Domanda: esiste, vicino alle compensazioni B2, un punto che preservi il raggio migliore senza peggiorare apprezzabilmente AP3.4 o ANP6? I due controlli OAT non sono candidati preferiti per il fit high-T: servono a completare la mappa. Nessuna selezione usa N_d, FGR o AP3.2 come target.

Risultati: il fine tuning Dg attorno a K_d -30% conferma lo scambio, senza risolverlo. Dg=14.875 porta RMSE raggio high-T a 38.47 nm con RMSE swelling AP3.8/AP3.4/ANP6 = 0.2245/1.2970/0.3686; Dg=15.045 migliora ulteriormente il raggio a 37.78 nm e ANP6 a 0.3425, ma peggiora AP3.4 a 1.3373 e S_ANP6(1800) a 12.574%. Il punto intermedio K_d -20% / Dg -7.7% offre RMSE raggio high-T 42.65 nm, con errori swelling 0.2199/1.2996/0.3507 e D_cal=0.0420.

La coppia K_d -15% / rho -7% offre un raggio high-T molto simile (42.43 nm), D_cal=0.0316 e RMSE swelling 0.2224/1.2894/0.3571. È il candidato scelto per il confronto finale conservativo: i piccoli scarti sullo swelling rispetto al punto intermedio non giustificano una distanza maggiore. L'alternativa K_d -15% / Dg -6% rimane ancora un poco più vicina a L3 (D_cal=0.0308), ma recupera meno raggio (44.85 nm). Queste valutazioni sono giudizi separati sui compromessi, non un ordinamento da score.

Il controllo N_gf,0 +20% lascia identici gli output intragranulari; varia grain-face/FGR. Il test Dv -20% conferma che non è una leva del raggio high-T. Nessun quarto o quinto batch: le direzioni di fine tuning si sono stabilizzate e non emerge una nuova ipotesi parametrica locale che migliori gli ultimi punti ANP6 e contemporaneamente freni l'estrapolazione, conservando AP3.8 e AP3.4. Questo non dimostra impossibilità globale fuori dalla regione esplorata.

## Decisione finale

Conservare L3 come candidato corrente. `B3_K085_rho093` è una valida alternativa microstrutturale da esaminare: raggio high-T RMSE -14.90%, low-T -48.46%, ma RMSE swelling AP3.4 +1.75%, ANP6 +2.54%; MAPE ANP6 da 9.39% a 11.25%. Gli ultimi punti ANP6 e l'accelerazione 1600–1800 K restano irrisolti. Non si adotta automaticamente il nuovo set e non lo si dichiara miglioramento concorde dei target principali.

Completati 3 batch (8+6+6 candidati), 2960 punti finali, più 148 baseline e 16 riesecuzioni baseline con storia. Le diagnostiche percorrono tutte le storie dei candidati. Massimo residuo relativo di massa: 4.52e-15. Nessuna anomalia rilevata dagli indicatori registrati; nessuna modifica agli originali, alle celle fisiche congelate o agli input sperimentali. Verifica automatica documentata in `verification.json`.
