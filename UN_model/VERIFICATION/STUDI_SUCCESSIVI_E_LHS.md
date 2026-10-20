# Proposta degli studi successivi — nessun LHS eseguito

Questa proposta richiede la scelta successiva dell'utente. Non sono stati generati punti Latin Hypercube, seed di campionamento o simulazioni LHS.

## Durate e osservabili prima del campionamento

1. Separare verification dei transitori da verification spaziale finale. I pilot spaziali attuali usano uno stato matematico molto lungo del sistema congelato; non verificano la traiettoria intermedia con il loro dt grande.
2. Scegliere, per ciascuna famiglia di coefficienti, osservazioni tra 0,01 e 5 τ_rapido, attorno a 0,1–10 volte i tempi di scambio, e a 1–10 τ_lento. I tempi propri sono già in `results/pilots_v2/cases.json`. Usare griglie logaritmiche di osservazione e verificare separatamente l'ultimo passo parziale.
3. Per vedere i rapidi transitori del caso stiff servono passi inferiori a quelli del pilot: n=1 ha τ_rapido≈33 s, e i modi più alti possono essere ancora più rapidi. Un primo criterio da discutere è dt≤τ_rapido/20, accompagnato da una misura dell'errore dell'osservabile, senza imporre inutilmente questo limite a una prova del solo stato finale.
4. Ampliare i casi deterministici prima del LHS: iniziali non uniformi compatibili con la base, initial layer uniforme, sorgenti in ciascuna popolazione e trasferimento interno costante `(-s,+s,0)`. Mantenere separata la rinormalizzazione dell'inizializzatore storico dalla proiezione ortogonale.
5. Aggiungere un confronto spaziale nei transitori con budget dedicato. Le code non devono necessariamente avere gli ordini del regime parabolico; la regolarità iniziale e il dato al bordo possono cambiare le pendenze. Se l'errore temporale non è trascurabile al N massimo, segnalarlo e restringere casi/tempi invece di correggere artificialmente la curva.

Per il sistema completo nonlineare mantenere durate legate al burnup e alle condizioni sperimentali; i τ di decenni del sottosistema congelato non sono durate fisiche raccomandate del modello completo. Non è stata decisa una durata definitiva.

## Dimensioni e range da approvare

Per il solver lineare la combinazione D/R² determina il rate diffusivo: variare D e R indipendentemente conta due coordinate fisiche ma una sola direzione dinamica. Un primo disegno può usare cinque rate indipendenti: κ₁=π²D/R², g_b, g_d, b_b, b_d, preferibilmente in logaritmo per i rate positivi. Gli zeri esatti vanno trattati come famiglie deterministiche separate, senza inserirli in un logaritmo. L'ampiezza di una singola sorgente si elimina per linearità; rapporti fra sorgenti e fra popolazioni iniziali sono eventuali dimensioni aggiuntive, da introdurre solo quando le corrispondenti prove deterministiche sono stabili.

I range non sono ancora fissati. Proposta: costruirli dalle tracce congelate delle quattro storie e da margini dichiarati, rispettando le correlazioni fisiche tra rate. Per una prova puramente algebrica del solver si possono campionare rate indipendenti, ma non presentare quei punti come microstrutture UN fisicamente realizzabili. Parametri calibrati del modello di produzione e reference restano immutati.

## Dimensionamento progressivo proposto

- **32 punti** per un primo pilot del disegno, controllo di casi estremi, condizionamento e costo effettivo.
- **128 punti** come prima campagna da valutare, dopo aver approvato durate/range/osservabili.
- Estensione a **256**, eventualmente **512**, solo se distribuzioni degli errori, ordini e regioni problematiche non risultano stabili. Questi numeri non garantiscono copertura probabilistica o errore massimo in cinque dimensioni.
- I casi esatti e i limiti deterministici restano separati dal conteggio LHS. Una futura replica del disegno con altro seed può misurare la sensibilità del riepilogo al campionamento; non è stata eseguita ora.

I costi misurati di un singolo confronto temporale N=40, su osservazioni orarie, sono mediana **0,0241 s** e percentile 95 **0,1080 s**, includendo processo C++ e reference/comparazione ma non compilazione, grafici e conservazione estesa degli output. Stima aritmetica per cinque dt per punto:

| Punti proposti | Stima con costo mediano | P95 con margine ×3 |
|---|---:|---:|
| 32 | 3,85 s | 51,9 s |
| 64 | 7,71 s | 103,7 s |
| 128 | 15,4 s | 207,4 s |
| 256 | 30,8 s | 414,9 s |
| 512 | 61,7 s | 829,7 s |

[CSV dei budget](results/figures/lhs_budget_proposal.csv). Sono stime condizionate ai pilot attuali, **non** preventivi validati per traiettorie dense, N=160, reference ad alto N, coefficienti estremi o durate nuove. I controlli reference spaziali hanno costi ulteriori. Il primo lotto da 32 punti, se autorizzato, servirà a ricalibrare il budget. Nessun valore di questa tabella deriva da un campionamento già svolto.

Prima di procedere occorrono quindi decisioni su: famiglie/range, T e griglia di osservazione per famiglia, metriche e accuratezza richiesta, numero iniziale/estensioni e politica di conservazione degli output. La FASE 4A si ferma senza iniziare il LHS.
