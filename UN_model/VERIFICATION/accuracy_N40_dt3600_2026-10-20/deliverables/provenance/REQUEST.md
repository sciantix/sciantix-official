# SCIANTIX UN L3 — Verifica dell'accuratezza con N=40 e Δt=1 h

La campagna estesa di convergenza del 20 ottobre è completata: 128 combinazioni LHS + 32 stati estratti dalle storie UN.

Adesso voglio valutare **quanto sia accurata la configurazione N=40, Δt=3600 s**, senza eseguire nuovamente la campagna di convergenza.

Lavora sul branch `development/nitride`.

Leggi:
- `UN_model/VERIFICATION/extended_convergence_2026-10-20/`
- `UN_model/VERIFICATION/phase4b/resumed/`
- I relativi report, protocolli, CSV, manifest e script validati.

## 1. Riutilizza i 160 casi esistenti

Non generare nuovi campioni.

Considera le 128 combinazioni LHS e i 32 stati UN già congelati, mantenendo invariati coefficienti, sorgenti, condizioni iniziali e durata fisica di 4000 h.

Tutti i benchmark restano a coefficienti costanti.

Riutilizza gli output C++ esistenti per N=40, Δt=3600 s, e le soluzioni analitiche già disponibili.

Non ripetere simulazioni identiche.

## 2. Calcola gli errori per c, mb e md

Per ogni combinazione e popolazione analizza le concentrazioni medie nel grano.

Separa:

**Errore temporale**
- C++ Backward Euler con N=40 contro soluzione analitica `expm` con N=40.

**Errore spaziale**
- Soluzione analitica con N=40 contro soluzione analitica ad alto N, verificata e convergente.

**Errore totale**
- C++ con N=40 e Δt=3600 s contro soluzione analitica high-N.

Conserva la decomposizione con segno dell'errore già validata.

Per ogni errore riporta:
- Errore assoluto.
- Errore relativo istantaneo rispetto alla soluzione analitica allo stesso tempo.
- Errore normalizzato sulla scala della singola popolazione, per confronto con i risultati precedenti.
- Tempo al quale si verifica il massimo osservato.

Quando la concentrazione analitica è nulla o numericamente troppo piccola, non calcolare percentuali ingannevoli. Segnala esplicitamente tali casi, senza alterare i risultati né nascondere gli errori assoluti.

Mantieni distinti c, mb e md.

## 3. Accuratezza durante le 4000 h

Non limitarti all'errore finale.

Per l'errore temporale, analizza tutte le **4001 osservazioni orarie** già conservate.

Per l'errore spaziale e totale, riutilizza inizialmente gli undici tempi fisici già verificati. Aggiungi osservazioni intermedie quando necessario, soprattutto nei casi peggiori, senza ricostruire inutilmente tutte le reference high-N.

Calcola sempre l'errore numerico confrontando soluzioni allo stesso istante fisico.

Identifica i massimi osservati, precisando la risoluzione temporale del campionamento.

**Attenzione ai transitori rapidi:** se il tempo caratteristico è molto minore di un'ora, non affermare che Δt=1 h risolva il transitorio. Non attribuire una soluzione numerica a istanti interni al timestep senza una procedura giustificata.

Usa prove mirate con timestep più piccoli nei casi necessari a caratterizzare la perdita di accuratezza nei transitori.

Non dichiarare un massimo continuo rigoroso se è stato calcolato soltanto su tempi discreti.

## 4. Analizza i casi peggiori

Confronta separatamente:
- Le 128 combinazioni LHS.
- I 32 stati ricavati dalle storie UN.
- Le tre popolazioni.

Determina quali coefficienti e quali regimi producono gli errori più elevati.

Distingui la dominanza dell'errore temporale da quella dell'errore spaziale.

Non stabilire automaticamente che un risultato supera o fallisce una soglia dell'1% o del 3%: questa decisione verrà presa successivamente.

## 5. Raffinamenti mirati

Soltanto per un sottoinsieme motivato dei casi più critici, confronta:

- N=40, Δt=3600 s — configurazione candidata.
- N=40, Δt=1800 s.
- N=40, Δt=900 s.
- N=80, Δt=3600 s.
- N=160, Δt=3600 s.

Riutilizza le simulazioni già esistenti e crea nuovi output C++ soltanto quando necessari.

Se N=160 non basta per un caso particolare, segnala il limite; non estendere indefinitamente i raffinamenti.

Voglio capire **quanto migliora l'accuratezza riducendo Δt o aumentando N**, e quale dei due interventi è più efficace nelle diverse regioni dei coefficienti.

## 6. Output da consegnare

Crea una nuova directory sotto:

`UN_model/VERIFICATION/`

Prepara:
1. CSV degli errori temporali, spaziali e totali per tutte le 160 combinazioni.
2. CSV dei massimi osservati, con popolazione, istante e definizione dell'errore.
3. CSV dei casi peggiori e dei relativi coefficienti.
4. CSV dei raffinamenti mirati, con confronto dei costi computazionali.
5. Report tecnico con risultati, limitazioni e anomalie.
6. README e validazione finale.
7. ZIP compatto con CSV, report e metadati, pronto da caricare in ChatGPT.

**Non creare grafici per la tesi.** Li prepareremo separatamente analizzando i dati.

Non scrivere ancora conclusioni definitive sulla scelta di N e Δt: presenta i risultati quantitativi e le alternative.

## 7. Vincoli

- Nessuna modifica al solver C++ o alla fisica UN.
- Nessuna modifica alle 160 combinazioni.
- Nessuna modifica alle reference congelate o ai precedenti risultati.
- Nessuna modifica a notebook o documenti della tesi.
- Preserva il branch e lo stato Git iniziale, compreso l'index.
- Nessun commit, push, merge, riordino o eliminazione.
- Usa caching, checksum e checkpoint per evitare lavoro duplicato.

Esegui un benchmark preliminare e mantieni la nuova campagna entro **90 minuti** complessivi.

Se il costo stimato supera il budget, fermati ordinatamente e consegna i risultati già completati.

Procedi autonomamente senza chiedere conferma intermedia, salvo problemi scientifici sostanziali.

**Fermati dopo questa campagna.** La scelta finale di N e Δt verrà discussa separatamente.