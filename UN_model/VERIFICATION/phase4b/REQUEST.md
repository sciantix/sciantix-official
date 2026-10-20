# SCIANTIX UN L3 — FASE 4B: Verification estesa del solver 3×3

La FASE 4A è completata. I risultati preliminari confermano la convergenza temporale di ordine 1 e la convergenza spaziale del solver.

Lavora direttamente su `development/nitride`, esclusivamente in:

`UN_model/VERIFICATION/`

## 1. Obiettivo

Estendere la verification analitica del vero solver C++ `SpectralDiffusion3equationsExchange`, confrontandolo con la soluzione indipendente `scipy.linalg.expm`.

Vogliamo verificare la convergenza numerica per diverse combinazioni dei coefficienti di diffusione, trapping e re-solution.

Riutilizza gli script, la reference analitica e i controlli sviluppati nella FASE 4A. Non riscrivere inutilmente quanto è già funzionante.

## 2. Prima: benchmark cronometrato

Esegui 5 simulazioni di prova, con:

- Durata fisica: 4000 h
- N = 40 modi
- Δt = 225 s (64.000 timestep)
- Cinque combinazioni rappresentative dei coefficienti

Misura i tempi effettivi del solver C++, dell'analisi Python e del processo completo, inclusi I/O.

Stima il costo totale dei 110 test successivi.

Se emerge un costo eccessivo, fermati e proponi una soluzione prima di lanciare l'intera campagna.

## 3. Selezione dei parametri: 10 combinazioni

Seleziona:

**4 combinazioni realistiche**, estratte dalle reference congelate delle simulazioni UN:

- AP3.2 — 900 K
- AP3.4 — 1600 K
- AP3.4 — 1800 K
- ANP6 — 1600 K

Per ciascuna, documenta il timestep di provenienza e i coefficienti effettivamente utilizzati.

**6 combinazioni estreme deterministiche**, che esplorino:

- Diffusione minima e massima.
- Trapping bulk e dislocation debole e forte.
- Re-solution debole e forte.
- Regimi stiff e combinazioni con tempi caratteristici molto diversi.
- Almeno un caso con g_b = 0.

Utilizza esclusivamente gli intervalli già definiti nella **Table B.1 dell'Appendix B della tesi**:

| Coefficiente | Minimo | Massimo |
|---|---|---|
| Dg [m²/s] | 6.860e-20 | 2.814e-16 |
| gb [1/s] | 0 | 2.042 |
| gd [1/s] | 7.230e-5 | 3.086e-1 |
| bb [1/s] | 1.024e-6 | 2.049e-4 |
| bd [1/s] | 3.790e-7 | 2.049e-4 |

Mantieni Rg = 6e-6 m.

Non modificare questi intervalli. Le combinazioni estreme servono alla verifica numerica e non devono necessariamente rappresentare microstrutture UN fisicamente realizzabili.

Salva un CSV con le 10 combinazioni, la loro provenienza e i tempi caratteristici.

## 4. Convergenza temporale — 50 simulazioni

Per ciascuna delle 10 combinazioni:

- N = 40 modi.
- Δt = 3600, 1800, 900, 450, 225 s.
- Durata fisica principale: 4000 h.
- Stesse condizioni iniziali e stessa sorgente.
- Confronto con `scipy.linalg.expm` allo stesso N.

Totale: **10 × 5 = 50 simulazioni**.

Calcola gli errori per c, mb e md, sui modi e sulle medie, e gli ordini di convergenza osservati.

Conserva la separazione fra errore temporale e spaziale.

Oltre all'errore al tempo finale, valuta l'accumulo dell'errore durante la storia.

**Attenzione ai transitori iniziali:** alcuni coefficienti hanno tempi caratteristici molto inferiori a 225 s. Non dichiararli risolti dalle griglie principali. Dove necessario, esegui benchmark brevi aggiuntivi con Δt adeguatamente ridotto e osservazioni ravvicinate; documenta separatamente il numero e il costo di queste prove.

## 5. Convergenza spaziale — 60 simulazioni

Per ciascuna delle 10 combinazioni:

N = 10, 20, 40, 80, 160, 320.

Totale: **10 × 6 = 60 simulazioni**.

Utilizza una soluzione analitica di riferimento ad alto N, verificandone la convergenza.

Scegli Δt attraverso una verifica preliminare che garantisca un errore temporale trascurabile rispetto a quello spaziale. Mantieni fisso Δt nei sei test della stessa combinazione.

Analizza la convergenza:

- A tempi fisici rappresentativi, fino a 4000 h.
- Nel regime stazionario matematico, quando utile, separatamente dalla storia fisica.
- Per le tre popolazioni di gas e per i coefficienti modali.

Non presumere che ogni combinazione mostri lo stesso ordine spaziale. Controlla la convergenza effettiva della reference e distingui i casi non ancora nel regime asintotico.

## 6. Risultati e grafici

Produci:

1. CSV dei 10 set di parametri.
2. CSV degli errori temporali e degli ordini osservati.
3. CSV degli errori spaziali.
4. Grafici log-log della convergenza in Δt e N.
5. Grafici di evoluzione temporale dell'errore.
6. Analisi dei casi peggiori e dei regimi stiff.
7. Tempi computazionali effettivi.
8. Report scientifico che distingua errori temporali, spaziali e limiti della verification.

Tutti i risultati devono essere riproducibili e organizzati sotto `UN_model/VERIFICATION/`, senza sovrascrivere i risultati della FASE 4A.

## 7. Vincoli

- Non modificare il solver C++ di produzione.
- Non modificare il modello fisico UN L3.
- Non modificare le reference congelate, i gold, i notebook o i documenti della tesi.
- Non spostare o eliminare le vecchie directory `phase2a`, `phase2bc`, `phase2d3` ecc.
- Non cambiare branch.
- Non fare commit, merge o push su GitHub.
- Non iniziare il campionamento LHS.

## 8. Conclusione

Alla fine delle **110 simulazioni principali**, più eventuali controlli aggiuntivi sui transitori veloci, prepara un report con:

- Accuratezza e convergenza del solver.
- Configurazioni numeriche che garantiscono gli errori desiderati.
- Regimi nei quali N = 40 e Δt = 1 h risultano insufficienti.
- Tempi effettivi della campagna.
- Proposta motivata per il successivo Latin Hypercube Sampling.

Fermati prima dell'LHS: il numero di campioni e la configurazione definitiva verranno approvati successivamente.