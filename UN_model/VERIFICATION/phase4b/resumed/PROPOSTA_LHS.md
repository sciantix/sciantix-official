# Proposta successiva — nessun LHS eseguito

## Decisioni necessarie

La campagna deterministica è completata. Prima di un LHS occorre scegliere osservabili e soglie: medie, ciascun coefficiente modale o norma del campo; errore assoluto nelle popolazioni piccole e relativo sulle scale dichiarate; regione iniziale rapida, storia 0–4000 h o limite stazionario. Sono obiettivi diversi.

I dati mostrano che N=40 e dt=1 h non possono essere certificati uniformemente: massimo temporale 3,75% in E2, massimo spaziale circa 1,47% sulle medie e 12,1% in L2 in E3. A N=320 il troncamento E3 scende a 0,163% sulle medie ma rimane 3,90% in L2. Un ipotetico obiettivo di 1% sul campo richiederebbe quindi ulteriori modi almeno in quella regione. Non si approvano qui nuove griglie o soglie e non si estrapola una garanzia dal solo ordine locale.

## Disegno proposto

1. **Pilot futuro da 32 punti**, con seed e versione del generatore registrati prima della generazione. Congelare i punti e verificarne la stratificazione soltanto dopo approvazione del disegno.
2. Eventuale campagna da **128 punti**; incremento a **256** se quantili, casi peggiori e frontiere di accuratezza non sono stabili. Conservare i dati del pilot e documentare il metodo di estensione: un LHS ordinario non è automaticamente annidabile mantenendo ogni vecchio punto.
3. Intervalli esclusivamente Table B.1 e Rg=6e-6 m. Proporre coordinate logaritmiche per Dg, gd, bb e bd positivi, e **gb lineare su [0,2.042]**. Non inventare un minimo positivo di gb per poter usare il logaritmo. Il limite gb=0 resta anche nei test deterministici separati; non è garantito che un LHS continuo tocchi un estremo.
4. Distinguere questo campionamento numerico dei coefficienti da un campionamento di microstrutture fisicamente realizzabili. I quattro set UN congelati restano controlli esterni, non vengono sostituiti.
5. Per ogni punto conservare la stessa decomposizione firmata e reference high-N convergente. Usare le griglie principali per la storia fisica e test brevi quando τ_rapido lo richiede. Se N massimo non risolve la norma scelta, segnalare la regione e fare uno studio deterministico aggiuntivo; non cambiare la fisica o la soglia dopo aver visto il risultato.

## Budget misurato, ancora condizionale

La proiezione usa, per ogni nuovo set, l'equivalente dell'intero protocollo corrente: **10 configurazioni C++ distinte** (11 etichette temporali/spaziali con un'intersezione), cache expm ai sei N e reference high-N. Non sono punti LHS già generati.

| Punti proposti | Compute medio stimato | Inviluppo dei dieci casi attuali |
|---|---:|---:|
| 32 | circa 3,0 min | 2,2–5,3 min |
| 128 | circa 12,0 min | 8,8–21,2 min |
| 256 | circa 23,9 min | 17,5–42,4 min |

Questi tempi escludono analisi/CSV, I/O, grafici, controlli di preservazione e prove rapide. L'intera FASE 4B corrente, per i blocchi misurati principali, richiede circa due minuti sui dieci set grazie al caching. Per una prima pianificazione completa conviene riservare un margine rispetto al solo compute: orientativamente 10–20 min per il pilot e 30–60 min per 128 punti **solo se le griglie restano quelle attuali**. Sono stime di pianificazione, non budget garantiti. Nuove soglie, N maggiori o regioni più difficili possono cambiare il costo e vanno preceduti da un piccolo benchmark.

[CSV delle estrapolazioni](audit/lhs_budget_proposal_only.csv), [costi osservati per set](audit/costs_per_case.csv). Nessun algoritmo LHS è stato chiamato, nessun seed o punto è stato estratto e nessun file di campionamento è stato creato.
