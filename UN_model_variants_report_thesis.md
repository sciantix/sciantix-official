# Analisi comparativa delle varianti del modello di swelling intragranulare in UN

## 1. Scopo dell'analisi

Lo scopo di questa analisi è valutare se la discrepanza osservata tra il modello UN implementato e i dati sperimentali di swelling microscopico possa essere risolta mediante una semplice calibrazione dei parametri oppure richieda una modifica della formulazione fisica del modello. In particolare, il confronto è stato impostato considerando i punti sperimentali di Fig. 3 di Rizk et al. come rappresentativi della popolazione intragranulare grande, indicata come popolazione P2. In accordo con l'interpretazione fisica dell'articolo, tale popolazione è associata alle **dislocation bubbles**. Di conseguenza, il target principale della calibrazione non è lo swelling intragranulare totale, ma lo swelling dovuto alla popolazione di bolle su dislocazione:

\[
\left(\frac{\Delta V}{V}\right)_d = N_d V_d .
\]

Le curve di bulk bubbles e grain-boundary bubbles sono quindi trattate come informazioni diagnostiche, non come grandezze direttamente confrontabili con i dati sperimentali di Fig. 3.

L'analisi è stata eseguita mediante la modalità `MODEL_VARIANTS` della pipeline Python, confrontando diverse formulazioni del modello intragranulare. I parametri fisici fissi di Rizk sono stati mantenuti invariati:

| Parametro | Valore |
|---|---:|
| Grain radius | \(6.0\times10^{-6}\) m |
| Fission rate | \(5.0\times10^{19}\) fiss m\(^{-3}\) s\(^{-1}\) |
| Xe yield | 0.24 atom/fission |
| Dislocation density \(\rho_d\) | \(3.0\times10^{13}\) m\(^{-2}\) |
| Surface energy \(\gamma_b\) | 1.11 J m\(^{-2}\) |
| Fission-gas atomic volume \(\Omega_{fg}\) | \(8.5\times10^{-29}\) m\(^3\)/atom |

## 2. Grandezze sperimentali usate come target

Il confronto quantitativo è stato effettuato su tre insiemi di dati digitalizzati:

1. **Fig. 3**: swelling microscopico associato alla popolazione P2, confrontato con `swelling_d_percent`.
2. **Fig. 7**: concentrazione numerica delle bolle grandi, confrontata con `N_d`.
3. **Fig. 8**: raggio medio delle bolle grandi, confrontato con `R_d`.

La pressione delle bolle non è un dato sperimentale diretto, ma è stata usata come controllo fisico. Il modello è considerato più coerente quando le pressioni interne delle bolle bulk e dislocation restano prossime alle pressioni di equilibrio:

\[
\frac{p_b}{p_{b,eq}} \simeq 1, \qquad \frac{p_d}{p_{d,eq}} \simeq 1 .
\]

Il comportamento ad alta temperatura, oltre circa 1800 K, è stato trattato come diagnostico e non come vincolo primario, poiché in questa regione la mancanza di una descrizione completa di interconnessione, grain-boundary bubble evolution e release può alterare la risposta del modello.

## 3. Varianti fisiche analizzate

Sono state confrontate sette formulazioni del modello. Le varianti non modificano i parametri materiali fissi, ma modificano la forma di alcuni termini fisici del solver.

### 3.1 M0: baseline Rizk-like

La variante `M0_baseline` corrisponde alla formulazione attuale del modello:

- nessun termine esplicito di trasferimento di massa gas dovuto alla nucleazione;
- fattore \(\phi_b\) usato solo nell'equazione per la densità numerica delle bulk bubbles;
- nessun termine di cattura bulk-dislocation;
- coalescenza tra dislocation bubbles inclusa.

Questa è la variante più coerente con la formulazione Rizk-base implementata.

### 3.2 M1: baseline con score P2-only

La variante `M1_rescore_baseline` ha la stessa fisica di M0, ma usa esplicitamente lo score P2-only. Serve a separare l'effetto della funzione obiettivo dall'effetto della fisica del modello. In altre parole, M1 risponde alla domanda: *quanto migliora il modello base se viene valutato correttamente contro le sole dislocation bubbles?*

### 3.3 M2: nucleation mass coupling

La variante `M2_nucleation_mass` introduce un termine di bilancio di massa associato alla nucleazione bulk. Se la nucleazione di una nuova bolla bulk viene interpretata come formazione di un dimero, allora due atomi devono essere sottratti dalla soluzione e aggiunti alla popolazione bulk:

\[
\left(\frac{dc}{dt}\right)_\nu = -2\nu_b,
\qquad
\left(\frac{dm_b}{dt}\right)_\nu = +2\nu_b .
\]

Questa variante è principalmente un test di consistenza del bilancio di massa.

### 3.4 M3: phi nella re-solution gas

La variante `M3_phi_resolution` testa l'ipotesi che il fattore \(\phi\), già presente nella distruzione della densità numerica delle bulk bubbles, possa entrare anche nel termine di re-solution atomica. La forma diagnostica testata è:

\[
b_{eff} = b\phi .
\]

Nel caso migliore selezionato automaticamente, la modalità risultante è `bulk_and_dislocation`, cioè \(\phi\) modifica la re-solution sia delle bulk bubbles sia delle dislocation bubbles. Questa variante non è trattata come Rizk-base, ma come test diagnostico di sensibilità alla formulazione della re-solution.

### 3.5 M4: cattura bulk-dislocation di tipo Barani

La variante `M4_bulk_dislocation_capture` introduce un termine di cattura delle bulk bubbles da parte delle dislocation bubbles in crescita. L'idea fisica è che una bolla su dislocazione, aumentando il proprio raggio, spazzi un volume di cattura e possa incorporare bulk bubbles presenti in quel volume:

\[
V_{cap} = \frac{4}{3}\pi (R_d + R_b)^3 .
\]

La frazione catturata viene usata per trasferire gas e vacanze dalla popolazione bulk alla popolazione dislocation:

\[
m_b \rightarrow m_b(1-f_{cap}),
\qquad
m_d \rightarrow m_d + f_{cap}m_b,
\]

con procedura analoga per le vacanze. Questa variante è Barani-like e quindi non rappresenta il modello Rizk-base puro.

### 3.6 M5 e M6

Le ultime due varianti combinano gli effetti precedenti:

- `M5_nucleation_mass_plus_capture`: M2 + M4;
- `M6_phi_plus_capture`: M3 bulk-and-dislocation + M4.

M6 è la variante più aggressiva: combina riduzione della re-solution effettiva mediante \(\phi\) e trasferimento bulk-dislocation. Per questo motivo è utile come test diagnostico, ma non deve essere interpretata come formulazione Rizk-base.

## 4. Parametri variati

Per ogni variante sono stati variati i seguenti parametri:

| Parametro | Griglia |
|---|---|
| \(f_n\) | \(10^{-10}, 3\times10^{-10}, 10^{-9}, 3\times10^{-9}, 10^{-8}, 3\times10^{-8}, 10^{-7}, 3\times10^{-7}, 10^{-6}\) |
| \(K_d\) | \(1\times10^5, 2\times10^5, 3\times10^5, 5\times10^5, 8\times10^5\) bubble/m |
| \(g_{d,scale}\) | 0.5, 1.0, 2.0, 3.0, 5.0 |
| bulk seed radius | 0.0 nm |

Il parametro \(g_{d,scale}\) moltiplica solo il trapping verso dislocazioni:

\[
g_d^{eff} = g_{d,scale} g_d .
\]

Non è un parametro originale di Rizk; è stato introdotto come correzione empirica diagnostica.

## 5. Definizione dello score

Lo score principale è stato costruito come score P2-only. La componente principale confronta lo swelling sperimentale di Fig. 3 con `swelling_d_percent` per punti con \(T \leq 1700\) K. I dati Fig. 7 e Fig. 8 vincolano rispettivamente la concentrazione numerica \(N_d\) e il raggio medio \(R_d\). La pressione entra come penalità solo nell'intervallo 1200--1700 K. Lo swelling intragranulare totale e il blow-up ad alta temperatura sono invece mantenuti come diagnostiche.

In forma qualitativa:

\[
S_{main} = S_{sw,d} + S_{N_d} + S_{R_d} + S_p .
\]

Dove:

- \(S_{sw,d}\) misura l'errore su Fig. 3 rispetto allo swelling delle dislocation bubbles;
- \(S_{N_d}\) misura l'errore logaritmico sulla concentrazione delle bolle grandi;
- \(S_{R_d}\) misura l'errore logaritmico sul raggio medio;
- \(S_p\) penalizza deviazioni significative da \(p/p_{eq}\simeq1\).

## 6. Risultati finali per variante

La tabella seguente riporta i migliori candidati finali per ogni modello. La valutazione finale è stata eseguita con `dt_h = 6 h` e `n_modes = 30`.

| Modello | Variante | Score main | Score swelling P2 | Score Rd | Score Nd | Pressione | High-T diag | \(f_n\) | \(K_d\) | \(g_{d,scale}\) | Coerenza |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|
| M0 | M0_baseline | 1.33753 | 0.91975 | 0.10774 | 0.48909 | 0.00000 | 3.30721 | 1.0e-10 | 3.0e5 | 0.5 | Rizk-base |
| M1 | M1_rescore_baseline | 1.33753 | 0.91975 | 0.10774 | 0.48909 | 0.00000 | 3.30721 | 1.0e-10 | 3.0e5 | 0.5 | Rizk-base |
| M2 | M2_nucleation_mass | 1.33753 | 0.91975 | 0.10774 | 0.48909 | 0.00000 | 3.30721 | 1.0e-10 | 3.0e5 | 0.5 | Diagnostic |
| M3 | M3_phi_resolution_bulk_and_dislocation | 1.27636 | 0.83887 | 0.13145 | 0.48989 | 0.01279 | 4.83826 | 3.0e-7 | 3.0e5 | 0.5 | Diagnostic |
| M4 | M4_bulk_dislocation_capture | 1.28393 | 0.81352 | 0.18680 | 0.48522 | 0.00000 | 0.28297 | 1.0e-9 | 5.0e5 | 1.0 | Barani-like |
| M5 | M5_nucleation_mass_plus_capture | 1.28393 | 0.81352 | 0.18680 | 0.48522 | 0.00000 | 0.28297 | 1.0e-9 | 5.0e5 | 1.0 | Diagnostic + Barani-like |
| M6 | M6_phi_plus_capture | 1.26898 | 0.83588 | 0.12520 | 0.48825 | 0.01845 | 5.05507 | 3.0e-7 | 3.0e5 | 0.5 | Diagnostic + Barani-like |

Il miglior score complessivo è ottenuto da M6, con \(S_{main}=1.26898\). Tuttavia, la differenza rispetto a M3 e M4 è limitata. In particolare, M4 ha uno score principale solo leggermente peggiore, ma presenta una diagnostica high-temperature molto più stabile.

## 7. Confronto puntuale a 1.3% FIMA e 1600 K

Per interpretare meglio il significato fisico dei risultati, è utile confrontare i valori modello a 1.3% FIMA e circa 1600 K con i punti sperimentali digitalizzati più vicini:

- Fig. 3b: swelling sperimentale circa 3.53% a 1595 K;
- Fig. 8: raggio sperimentale circa 120.83 nm a 1594 K;
- Fig. 7: concentrazione sperimentale circa \(5.34\times10^{18}\) m\(^{-3}\) a 1599 K.

| Modello | \(sw_d\) [%] | \(sw_{ig}\) [%] | \(R_d\) [nm] | \(N_d\) [m\(^{-3}\)] | \(p_b/p_{b,eq}\) | \(p_d/p_{d,eq}\) |
|---|---:|---:|---:|---:|---:|---:|
| M0 | 2.75072 | 4.44437 | 93.51 | \(8.03\times10^{18}\) | 1.011 | 1.103 |
| M1 | 2.75072 | 4.44437 | 93.51 | \(8.03\times10^{18}\) | 1.011 | 1.103 |
| M2 | 2.75072 | 4.44437 | 93.51 | \(8.03\times10^{18}\) | 1.011 | 1.103 |
| M3 | 2.19892 | 4.17559 | 86.11 | \(8.22\times10^{18}\) | 1.003 | 1.151 |
| M4 | 2.64471 | 3.39548 | 77.72 | \(1.35\times10^{19}\) | 1.001 | 1.039 |
| M5 | 2.64470 | 3.39548 | 77.72 | \(1.35\times10^{19}\) | 1.001 | 1.039 |
| M6 | 2.31369 | 4.25371 | 87.69 | \(8.19\times10^{18}\) | 1.003 | 1.177 |

Questo confronto evidenzia un punto importante: il miglior score globale non implica necessariamente il miglior accordo nel singolo punto a 1600 K. Infatti, M0/M1 predicono uno swelling P2 più vicino al punto sperimentale di Fig. 3b a 1600 K rispetto a M6, mentre M6 ottiene uno score globale migliore grazie al comportamento sull'intero insieme di punti e a un compromesso più favorevole tra swelling, raggio e concentrazione.

In generale, tutti i modelli sottostimano il raggio medio a 1600 K rispetto al punto di circa 120 nm, mentre alcuni modelli compensano parzialmente mediante una concentrazione numerica più elevata. Questo è tipico di una difficoltà nella ripartizione raggio-densità: lo swelling può risultare ragionevole anche se la microstruttura non è perfettamente corretta.

## 8. Discussione per variante

### 8.1 M0/M1: modello Rizk-base

M0 e M1 danno risultati identici, come previsto, perché hanno la stessa fisica. Il miglior candidato Rizk-base è:

\[
f_n = 10^{-10}, \qquad K_d = 3\times10^5\;\mathrm{bubble/m}, \qquad g_{d,scale}=0.5 .
\]

Questo risultato indica che, nella nostra implementazione, il modello Rizk-base richiede una nucleazione bulk estremamente bassa e una riduzione empirica del trapping verso dislocazioni per avvicinarsi ai dati P2. Rispetto al nominale iniziale, il valore di \(K_d\) preferito è inferiore a \(5\times10^5\) bubble/m, ma non drasticamente lontano. Il valore di \(f_n\), invece, è molto più basso di quello nominale usato nei primi test.

Dal punto di vista fisico, M1 è la variante più difendibile come base di tesi, perché conserva la forma Rizk-like del modello. Il suo limite principale è che il raggio medio resta sottostimato e lo score high-T diagnostico è relativamente elevato.

### 8.2 M2: effetto del termine di massa della nucleazione

M2 produce risultati praticamente identici a M0/M1. Questo indica che, nell'intervallo di parametri selezionato dal fit, il termine di massa \(-2\nu_b\) in \(c\) e \(+2\nu_b\) in \(m_b\) ha un effetto trascurabile sulle grandezze target. Il motivo è coerente con il best-fit: il parametro \(f_n\) scelto è molto basso, quindi la nucleazione bulk non è il processo dominante nel candidato ottimo.

La conclusione è che il termine di massa della nucleazione è importante per completezza formale del bilancio, ma non risolve il problema di fitting della popolazione P2.

### 8.3 M3: effetto di \(\phi\) nella re-solution gas

M3 migliora lo score principale da 1.33753 a 1.27636. La riduzione è circa il 4.6% rispetto a M1. Lo score dello swelling P2 migliora più sensibilmente, passando da 0.91975 a 0.83887, cioè circa il 8.8%.

Tuttavia, M3 introduce una penalità di pressione non nulla e aumenta notevolmente la diagnostica high-temperature. Questo suggerisce che l'uso di \(b\phi\) nella re-solution atomica può migliorare la forma della curva P2, ma tende anche a rendere il modello più aggressivo ad alta temperatura. Inoltre, questa modifica non è parte del modello Rizk-base pubblicato e deve quindi essere presentata come test diagnostico.

### 8.4 M4: cattura bulk-dislocation

M4 è fisicamente interessante perché introduce un meccanismo di trasferimento da bulk bubbles a dislocation bubbles. Il miglior candidato M4 ha:

\[
f_n = 10^{-9}, \qquad K_d = 5\times10^5\;\mathrm{bubble/m}, \qquad g_{d,scale}=1 .
\]

Questo è notevole perché M4 non richiede la riduzione empirica del trapping verso dislocazioni e recupera il valore Rizk-like di \(K_d=5\times10^5\) bubble/m. Inoltre, la diagnostica high-temperature è molto bassa rispetto a M1 e M6. La pressione resta prossima all'equilibrio.

Il limite di M4 è la microstruttura a 1600 K: \(N_d\) è sovrastimato e \(R_d\) è sottostimato. Ciò significa che la cattura bulk-dislocation migliora lo swelling P2, ma produce una popolazione troppo numerosa e con raggio medio troppo piccolo rispetto ai dati Fig. 7--8.

Nonostante questo, M4 è probabilmente la variante estesa più fisicamente difendibile, perché introduce un meccanismo Barani-like plausibile senza modificare artificialmente la re-solution atomica.

### 8.5 M5: nucleation mass + capture

M5 è praticamente indistinguibile da M4. Questo conferma che il termine di massa della nucleazione non ha effetto significativo quando è presente la cattura bulk-dislocation nel regime di parametri ottimo. Pertanto, M5 non aggiunge informazione fisica rilevante rispetto a M4.

### 8.6 M6: phi + capture

M6 fornisce il miglior score complessivo:

\[
S_{main}=1.26898 .
\]

Tuttavia, il miglioramento rispetto a M4 è modesto:

\[
\frac{1.28393 - 1.26898}{1.28393} \approx 1.2\% .
\]

Inoltre, M6 presenta la più alta diagnostica high-temperature tra i candidati migliori. Ciò indica che l'accoppiamento tra \(\phi\)-resolution e cattura bulk-dislocation migliora leggermente il compromesso globale, ma al costo di una formulazione meno pulita e più instabile ad alta temperatura.

M6 non deve quindi essere interpretato come modello finale Rizk-base. È utile per mostrare che modifiche alla re-solution e trasferimento bulk-dislocation possono migliorare il fitting, ma la sua coerenza fisica richiede ulteriori verifiche.

## 9. Interpretazione complessiva

I risultati mostrano che nessuna variante riproduce perfettamente e simultaneamente:

1. lo swelling P2/dislocation di Fig. 3;
2. il raggio medio \(R_d\) di Fig. 8;
3. la concentrazione numerica \(N_d\) di Fig. 7;
4. il comportamento near-equilibrium delle pressioni;
5. la stabilità ad alta temperatura.

Il modello base Rizk-like, se riscorato correttamente come P2-only, è già competitivo. Il best M1 ha score 1.33753, mentre il best globale M6 ha score 1.26898. Il miglioramento relativo di M6 rispetto a M1 è circa 5.1%, quindi non è abbastanza grande da giustificare automaticamente l'adozione di M6 come modello fisico finale.

La cattura bulk-dislocation M4 è una variante particolarmente interessante perché migliora lo score dello swelling e riduce drasticamente la diagnostica high-temperature. Tuttavia, la microstruttura risultante mostra una densità di bolle troppo elevata e un raggio troppo basso. Questo suggerisce che il meccanismo di cattura può essere rilevante, ma la sua formulazione semplice single-size non è ancora sufficiente a riprodurre correttamente la distribuzione P2.

Il termine di massa della nucleazione M2 non modifica i risultati, mentre l'introduzione di \(\phi\) nella re-solution atomica M3/M6 migliora il fitting globale ma aumenta la sensibilità high-temperature. Questo rende il sospetto sul termine \(\phi\) interessante, ma non conclusivo.

## 10. Scelta raccomandata per la tesi

Per una tesi, la distinzione più chiara è la seguente.

### Modello principale difendibile

Usare M1 come modello principale:

\[
f_n = 10^{-10}, \qquad K_d = 3\times10^5\;\mathrm{bubble/m}, \qquad g_{d,scale}=0.5 .
\]

Motivazione:

- conserva la formulazione Rizk-base;
- usa lo score corretto P2-only;
- riproduce qualitativamente la transizione di swelling;
- mantiene le pressioni vicine all'equilibrio;
- evita di introdurre termini diagnostici non pubblicati.

Limiti da dichiarare:

- sottostima il raggio medio P2 a circa 1600 K;
- richiede un \(f_n\) molto basso;
- richiede un fattore empirico \(g_{d,scale}=0.5\);
- presenta una crescita high-temperature diagnostica ancora significativa.

### Variante estesa più promettente

Usare M4 come estensione fisicamente motivata:

\[
f_n = 10^{-9}, \qquad K_d = 5\times10^5\;\mathrm{bubble/m}, \qquad g_{d,scale}=1 .
\]

Motivazione:

- introduce un meccanismo bulk-dislocation fisicamente plausibile;
- recupera valori più vicini ai parametri nominali per \(K_d\) e \(g_d\);
- riduce fortemente la diagnostica high-temperature;
- mantiene la pressione near-equilibrium.

Limiti:

- non è Rizk-base puro;
- sottostima \(R_d\);
- sovrastima \(N_d\);
- richiede una formulazione più rigorosa della cattura e probabilmente una descrizione non single-size.

### Variante diagnostica migliore in senso numerico

M6 può essere presentata come best numerical diagnostic:

\[
f_n = 3\times10^{-7}, \qquad K_d = 3\times10^5\;\mathrm{bubble/m}, \qquad g_{d,scale}=0.5 .
\]

Motivazione:

- migliore score globale;
- mostra che re-solution modificata e capture possono migliorare il fit.

Limiti:

- combina due modifiche non Rizk-base;
- high-temperature diagnostic elevata;
- interpretazione fisica meno pulita.

## 11. Lavoro successivo raccomandato

Prima di fissare un set finale per la tesi, sono raccomandati tre controlli:

1. **Verifica numerica fine**: rilanciare M1, M4 e M6 con `dt_h = 1 h` e `n_modes = 40`, almeno per 1.3% FIMA.
2. **Verifica locale a 1600 K**: confrontare direttamente swelling, \(R_d\), \(N_d\), \(p_d/p_{eq}\) e gas partition per i tre candidati.
3. **Analisi del termine capture**: nel caso M4, verificare se il termine di cattura può essere formulato con un coefficiente di efficienza \(\eta_{cap}\), evitando eccessiva riduzione del raggio medio.

Un possibile passo successivo è quindi introdurre:

\[
dN_{b,cap} = -\eta_{cap} N_d N_b \frac{dV_{cap}}{dt},
\]

con \(0 < \eta_{cap} \leq 1\), e valutare se esiste un valore fisicamente ragionevole che migliori simultaneamente swelling, \(R_d\) e \(N_d\).

## 12. Conclusione

La model-form sensitivity mostra che il problema non è riconducibile a un solo parametro, come \(f_n\). La formulazione Rizk-base, riscorata correttamente rispetto alla popolazione P2, fornisce un risultato ragionevole ma richiede una nucleazione bulk estremamente bassa e una correzione empirica del trapping verso dislocazioni. Le estensioni fisiche migliorano il fit, ma in modo moderato.

La conclusione più robusta è che il modello intragranulare P2 è sensibile alla descrizione della re-solution e del trasferimento tra popolazioni bulk e dislocation. Tra le varianti testate, M6 fornisce il miglior score numerico, ma M4 rappresenta l'estensione fisicamente più interpretabile. Per una formulazione di tesi, M1 dovrebbe essere mantenuto come modello di riferimento Rizk-base, mentre M4 e M6 dovrebbero essere discusse come estensioni diagnostiche per identificare i limiti della formulazione corrente.
