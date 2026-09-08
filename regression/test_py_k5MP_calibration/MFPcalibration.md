# Calibrazione del modello dei prodotti di fissione metallici (5MP)
## 1. Obiettivo della calibrazione
La calibrazione del modello dei prodotti di fissione metallici viene eseguita simultaneamente sulle quattro posizioni radiali considerate:

$$ r/R = 0; 0.30; 0.56; 0.75 $$

L'obiettivo è identificare un unico set di parametri in grado di descrivere il comportamento delle popolazioni di precipitati metallici nelle differenti condizioni termiche e microstrutturali presenti lungo il raggio della pastiglia di combustibile.

La calibrazione viene pertanto formulata come un problema globale: ogni combinazione di parametri viene utilizzata, senza modifiche, per eseguire quattro simulazioni SCIANTIX, una per ciascuna posizione radiale.

In questo modo, la bontà di un set di parametri non viene valutata sulla capacità di riprodurre una singola posizione, ma sulla sua capacità di descrivere simultaneamente l'intero insieme di dati sperimentali.

---
## 2. Parametri del modello
Il modello comprende i processi di:

- nucleazione intragranulare;

- precipitazione intragranulare;

- nucleazione al bordo di grano;

- precipitazione al bordo di grano;

- re-solution sotto irraggiamento.

I coefficienti cinetici dipendenti dalla temperatura sono descritti mediante una relazione di tipo Arrhenius:

$$ k(T)=k_0\exp\left(-\frac{\Delta G}{k_BT}\right) $$

dove:

- $k_0$ è il prefattore cinetico;

- $\Delta G$ è l'energia di attivazione;

- $k_B$ è la costante di Boltzmann;

- $T$ è la temperatura assoluta.

Per i processi di precipitazione, il prefattore cinetico assume dimensionalmente il ruolo di un coefficiente efficace di trasporto:

$$ [k_{intra,0}]=[k_{GB,0}]=\mathrm{m^2\,s^{-1}} $$

mentre i prefattori associati alla nucleazione hanno dimensioni:

$$ [k_{nucl,0}]=[k_{nucl,GB,0}]=\mathrm{s^{-1}} $$

Le energie di attivazione sono espresse in eV.

Nell'implementazione numerica i parametri vengono introdotti attraverso fattori moltiplicativi applicati a valori di riferimento unitari. Di conseguenza, il valore numerico del fattore coincide con il valore numerico del corrispondente parametro efficace, espresso nelle rispettive unità.

Ad esempio:

$$ k_{intra,0}=\underbrace{1}_{\mathrm{m^2/s}}\times sf_{k,intra} $$

e:

$$ \Delta G_{intra}=\underbrace{1}_{\mathrm{eV}}\times sf_{\Delta G,intra} $$

Tali coefficienti devono pertanto essere interpretati come parametri cinetici efficaci del modello e non necessariamente come coefficienti microscopici direttamente misurabili.

Due quantità fisiche vengono invece mantenute fisse e non fanno parte della calibrazione:

- l'effective lumped production yield dei cinque elementi Mo, Tc, Ru, Rh e Pd, pari a $y_{\mathrm{5MP}}=0.578$ atomi/fissione;

- il coefficiente di re-solution di riferimento, pari a $k_{res,ref}=3.085078\times10^{-6}\,\mathrm{s^{-1}}$, scalato con il fission rate secondo: 

$$k_{res}=k_{res,ref}(F/F_{ref})sf_{res}$$

con:

$$F_{ref}=1.48\times10^{19}\,\mathrm{fissions\,m^{-3}\,s^{-1}}$$ e $$sf_{res}=1$$.

Il valore $y_{\mathrm{5MP}}$ rappresenta un yield efficace aggregato per i cinque elementi considerati nel modello e non un independent fission yield universale.

---
## 3. Configurazione nominale
Prima della calibrazione viene valutata una configurazione nominale del modello ponendo tutti i fattori moltiplicativi pari all'unità:

$$ sf_i=1 $$

Il set nominale è quindi:

$$ sf_{k,intra}=1 $$

$$ sf_{k,GB}=1 $$

$$ sf_{k,nucl}=1 $$

$$ sf_{res}=1 $$

$$ sf_{\Delta G,nucl}=1 $$

$$ sf_{\Delta G,intra}=1 $$

$$ sf_{\Delta G,GB}=1 $$

$$ sf_{\Delta G,nucl,GB}=1 $$

$$ sf_{k,nucl,GB}=1 $$

Questa configurazione non rappresenta un set calibrato, ma costituisce il riferimento nominale rispetto al quale valutare il miglioramento introdotto dalla procedura di calibrazione.

Con la formulazione finale del modello, inclusi $y_{\mathrm{5MP}}=0.578$ atomi/fissione e il valore corretto di $k_{res,ref}$, la configurazione nominale fornisce una funzione obiettivo pari a circa:

$$ J_{baseline}=1.627431 $$

---
## 4. Parametri calibrati
Prima della calibrazione finale è stato eseguito uno screening diagnostico per valutare la sensibilità delle osservabili e l'identificabilità dei diversi coefficienti.

Lo screening ha mostrato forti correlazioni tra alcuni parametri. In particolare:

- $k_{intra,0}$ e $k_{GB,0}$ producono risposte fortemente correlate nel confronto finale;

- $\Delta G_{intra}$ e $\Delta G_{GB}$ mostrano una correlazione analoga;

- $k_{nucl,GB,0}$ modifica sensibilmente alcune variabili interne della popolazione GB ma produce variazioni trascurabili della funzione obiettivo;

- $\Delta G_{nucl,GB}$ appartiene allo stesso canale di nucleazione GB scarsamente osservabile con i target disponibili;

- il coefficiente di re-solution è mantenuto al valore di riferimento ricavato indipendentemente e non viene utilizzato come grado di libertà della calibrazione.

Per evitare di calibrare simultaneamente direzioni parametriche scarsamente identificabili, la calibrazione finale viene quindi ridotta a quattro parametri liberi:

| Parametro libero | Significato | Unità |
|---|---|---|
| $k_{GB,0}$ | prefattore cinetico della precipitazione al bordo di grano | $\mathrm{m^2/s}$ |
| $k_{nucl,0}$ | prefattore della nucleazione intragranulare | $\mathrm{s^{-1}}$ |
| $\Delta G_{nucl}$ | barriera energetica della nucleazione intragranulare | eV |
| $\Delta G_{GB}$ | energia di attivazione della precipitazione al bordo di grano | eV |

I parametri mantenuti fissi sono:

| Parametro fisso | Valore |
|---|---|
| $sf_{k,intra}$ | 1 |
| $sf_{\Delta G,intra}$ | 1 |
| $sf_{k,nucl,GB}$ | 1 |
| $sf_{\Delta G,nucl,GB}$ | 1 |
| $sf_{res}$ | 1 |

Il valore unitario dei parametri fissati deve essere interpretato come riferimento di normalizzazione del modello e non come dimostrazione che tali valori siano stati identificati univocamente dai dati sperimentali.

---
## 5. Intervalli di calibrazione
Gli intervalli finali non vengono scelti come domini generici molto ampi, ma vengono definiti sulla base dello screening diagnostico preliminare.

I prefattori cinetici vengono campionati su scala logaritmica, mentre le energie di attivazione vengono campionate linearmente.

Il dominio finale è:

| Parametro | Minimo | Massimo | Campionamento |
|---|---|---|---|
| $k_{GB,0}$ | $1$ | $10^{2}$ | logaritmico |
| $k_{nucl,0}$ | $10^{-6}$ | $10^{-4}$ | logaritmico |
| $\Delta G_{nucl}$ | $0.50$ eV | $1.50$ eV | lineare |
| $\Delta G_{GB}$ | $0.75$ eV | $1.50$ eV | lineare |

Lo screening del prefattore di nucleazione intragranulare ha mostrato una risposta non monotona della funzione obiettivo:

| $k_{nucl,0}$ | $J$ |
|---|---|
| $10^{-2}$ | 1.430716 |
| $10^{-3}$ | 1.256555 |
| $10^{-4}$ | 1.029047 |
| $10^{-5}$ | 0.985899 |
| $10^{-6}$ | 1.204709 |

Il dominio $10^{-6}$--$10^{-4}$ include quindi la regione più promettente individuata dallo screening senza estendersi inutilmente verso ordini di grandezza che hanno mostrato prestazioni peggiori.

Per $k_{GB,0}$, i test diagnostici hanno mostrato un miglioramento passando dal valore nominale verso valori maggiori, con una regione utile compresa nel dominio $1$--$10^{2}$.

Le energie vengono mantenute su intervalli sufficientemente ampi da includere le regioni favorevoli emerse dai diagnostics e da consentire il compromesso tra le quattro posizioni radiali.

La configurazione nominale con tutti i fattori pari a uno viene comunque valutata separatamente come riferimento, anche quando non coincide con il centro del dominio finale.

---
## 6. Campionamento mediante sequenza di Sobol
L'esplorazione dello spazio parametrico finale, di dimensione quattro, viene effettuata mediante una sequenza di Sobol a bassa discrepanza.

La sequenza di Sobol permette una copertura più uniforme dello spazio multidimensionale rispetto a un campionamento puramente casuale ed è pertanto utilizzata come strategia di esplorazione per la calibrazione.

Nel presente lavoro la sequenza di Sobol viene utilizzata esclusivamente come tecnica di campionamento dello spazio parametrico e non deve essere confusa con un'analisi di sensitività basata sugli indici di Sobol.

Il campionamento è eseguito con sequenza Sobol scrambled, seed 42, utilizzando:

$$ N_{Sobol}=4096=2^{12} $$

punti nel dominio dei quattro parametri liberi.

Per ciascun punto campionato viene utilizzato lo stesso set parametrico alle quattro posizioni radiali:

$$ r/R=0,\;0.30,\;0.56,\;0.75 $$

Viene inoltre valutata separatamente la configurazione nominale con tutti i fattori pari all'unità.

Il numero complessivo di set valutati è quindi:

$$ N_{set}=N_{Sobol}+1=4097 $$

Poiché ciascun set viene simulato alle quattro posizioni radiali, il numero totale di simulazioni SCIANTIX è:

$$ N_{runs}=4\left(N_{Sobol}+1\right)=16388 $$

---
## 7. Dati sperimentali utilizzati
Le immagini sperimentali vengono analizzate mediante segmentazione delle micrografie TEM.

Per ogni popolazione di precipitati vengono ricavate principalmente:

- la frazione areale occupata dalle inclusioni;

- il raggio equivalente medio;

- la deviazione standard della distribuzione dei raggi;

- lo shape factor.

La frazione sperimentale \`Inclusions\` è definita nel codice di elaborazione delle immagini come:

$$ A_A^{exp} = \frac{A_{5MP}}{A_{image}} $$

e corrisponde quindi alla frazione dei pixel dell'immagine classificati come precipitati metallici.

Si tratta pertanto di una frazione areale misurata direttamente sulla micrografia e non di una frazione volumica calcolata a partire dalla quantità totale di prodotti di fissione metallici.

---
## 8. Confronto della popolazione intragranulare
SCIANTIX fornisce per la popolazione intragranulare:

- la concentrazione numerica delle particelle $N_{intra}$;

- la quantità di metalli precipitati;

- il numero medio di atomi per particella.

Il raggio medio equivalente viene ricavato assumendo particelle sferiche.

Indicando con $n_{intra}$ il numero medio di atomi per particella e con $V_{eff}$ il volume atomico efficace della fase metallica:

$$ R_{intra} = \left( \frac{ 3n_{intra}V_{eff} }{ 4\pi } \right)^{1/3}. $$

Il volume atomico efficace utilizzato nel modello è:

$$ V_{eff} = 1.44123\times10^{-29}\ \mathrm{m^3/atom}. $$

### 8.1 Frazione areale proiettata
Poiché le osservazioni TEM rappresentano una proiezione attraverso lo spessore della lamella, la frazione volumica modellata non viene confrontata direttamente con la frazione areale sperimentale.

Per ottenere una quantità modellata maggiormente coerente con l'osservabile sperimentale, viene definita una copertura proiettata.

Indicando con:

- $N_{intra}$ la concentrazione numerica delle particelle;

- $R_{intra}$ il loro raggio medio equivalente;

- $t$ lo spessore locale della lamella TEM;

si definisce:

$$ \lambda_{intra} = N_{intra} \pi R_{intra}^{2} t. $$

Per basse frazioni di copertura:

$$ A_{A,intra}^{model} \simeq N_{intra}\pi R_{intra}^{2}t. $$

Per tenere conto in prima approssimazione della possibile sovrapposizione delle proiezioni delle particelle viene utilizzata la forma:

$$ A_{A,intra}^{model} = 1- \exp \left( - N_{intra} \pi R_{intra}^{2} t \right). $$

Questa formulazione garantisce inoltre:

$$ 0 \leq A_{A,intra}^{model} \leq 1. $$

La quantità:

$$ A_{A,intra}^{model} $$

viene quindi confrontata direttamente con:

$$ A_{A,intra}^{exp}. $$

È importante sottolineare che questa relazione rappresenta un operatore di proiezione adottato per rendere confrontabile la popolazione volumetrica modellata con la copertura osservata nella micrografia TEM.

Non viene interpretata come una relazione stereologica generale tra frazione areale e frazione volumica.

---
## 9. Spessore delle lamelle TEM
La conversione della popolazione intragranulare in frazione areale proiettata richiede la conoscenza dello spessore della lamella TEM.

Nell'implementazione corrente vengono utilizzati i seguenti valori:

| $r/R$ | Spessore TEM |
|---|---|
| 0 | 50 nm |
| 0.30 | 50 nm |
| 0.56 | 70 nm |
| 0.75 | 40 nm |

Per $r/R=0.56$ e $r/R=0.75$ vengono utilizzati gli spessori associati alle condizioni sperimentali disponibili. Per $r/R=0$ e $r/R=0.30$, in assenza di una misura locale specifica per la ROI analizzata, viene adottato un valore nominale di 50 nm.

Questa approssimazione viene mantenuta esplicita nell'interpretazione dei risultati, poiché lo spessore della lamella influenza direttamente la trasformazione della popolazione volumetrica modellata nella corrispondente copertura proiettata.

---
## 10. Trattamento dei precipitati al bordo di grano
La nucleazione dei precipitati intergranulari viene descritta assumendo le bolle intergranulari come siti preferenziali di nucleazione eterogenea.

In SCIANTIX, la concentrazione delle bolle intergranulari è definita come densità areale del bordo di grano:

$$ N_{b,GB}^{A} \quad [\mathrm{m^{-2}}]. $$

Il modello dei 5MP è invece formulato utilizzando una concentrazione volumetrica delle particelle:

$$ N_{GB} \quad [\mathrm{m^{-3}}]. $$

Per rendere dimensionalmente coerente il termine sorgente della nucleazione, la densità areale delle bolle viene convertita in una densità volumetrica equivalente di siti mediante il rapporto tra superficie dei bordi di grano e volume del materiale.

Assumendo la geometria utilizzata in SCIANTIX:

$$ \frac{S_{GB}}{V} = \frac{3}{2a}, $$

dove $a$ rappresenta il raggio del grano, la densità volumetrica equivalente dei siti intergranulari è:

$$ N_{sites,GB} = N_{b,GB}^{A} \frac{3}{2a}. $$

Dimensionalmente:

$$ [\mathrm{m^{-2}}] [\mathrm{m^{-1}}] = [\mathrm{m^{-3}}]. $$

Il termine sorgente associato alla nucleazione intergranulare diventa quindi:

$$ S_{GB} = N_{sites,GB} k_{nucl,GB,0} \exp\left( -\frac{\Delta G_{nucl,GB}}{k_BT} \right). $$

Questa formulazione permette di mantenere la descrizione volumetrica della popolazione dei 5MP intergranulari pur tenendo esplicitamente conto della natura superficiale dei siti localizzati al bordo di grano.

La frazione areale sperimentale dei precipitati intergranulari richiede tuttavia un trattamento distinto.

Le micrografie utilizzate per la caratterizzazione dei precipitati GB sono selezionate intenzionalmente in corrispondenza dei bordi di grano; la quantità misurata rappresenta quindi la frazione dell'area della ROI occupata dai precipitati e non direttamente la fractional coverage della superficie del bordo di grano.

Una conversione della concentrazione volumetrica modellata in una quantità direttamente confrontabile con tale osservabile richiederebbe informazioni aggiuntive sulla geometria e sull'orientazione del bordo rispetto alla lamella TEM.

Per evitare di introdurre un'ulteriore relazione geometrica non direttamente vincolata dai dati sperimentali, la frazione areale GB viene mantenuta come quantità diagnostica e non contribuisce alla funzione obiettivo.

---
## 11. Raggio dei precipitati al bordo di grano
Il raggio medio dei precipitati intergranulari viene invece mantenuto tra le quantità utilizzate nella calibrazione.

Il raggio modellato viene ricavato dal rapporto tra quantità precipitata e concentrazione numerica delle particelle:

$$ n_{GB} = \frac{ C_{prec,GB} }{ N_{GB} } $$

e quindi:

$$ R_{GB} = \left( \frac{ 3n_{GB}V_{eff} }{ 4\pi } \right)^{1/3}. $$

Tale raggio equivalente può essere confrontato con il raggio equivalente ricavato dalle micrografie sperimentali.

L'assunzione di sfericità rappresenta una semplificazione geometrica: i precipitati metallici possono infatti presentare forme irregolari, facettate o allungate, in particolare in prossimità dei bordi di grano.

Il confronto viene pertanto effettuato in termini di raggio equivalente.

---
## 12. Osservabili utilizzate nella calibrazione
Per ciascuna posizione radiale vengono utilizzate tre osservabili sperimentali:

1\. frazione areale dei precipitati intragranulari;

2\. raggio medio equivalente dei precipitati intragranulari;

3\. raggio medio equivalente dei precipitati al bordo di grano.

La frazione areale dei precipitati al bordo di grano viene invece mantenuta esclusivamente come quantità diagnostica.

La calibrazione utilizza quindi:

$$ 3 $$

osservabili per ciascuna delle:

$$ 4 $$

posizioni radiali.

Il numero totale di termini della funzione obiettivo è pertanto:

$$ N_{obs} = 3\times4 = 12. $$

---
## 13. Definizione dell'errore
La scelta della funzione di errore deve permettere di confrontare in modo coerente osservabili positive con unità e ordini di grandezza differenti.

Una possibile scelta sarebbe l'errore relativo assoluto convenzionale:

$$ e_i^{rel}=\left|\frac{M_i-E_i}{E_i}\right| $$

dove $M_i$ rappresenta il valore ottenuto dal modello ed $E_i$ il corrispondente valore sperimentale.

Questa definizione non viene adottata nella calibrazione finale perché il valore sperimentale compare direttamente al denominatore. Di conseguenza, target caratterizzati da valori assoluti piccoli possono produrre contributi molto grandi e dominare la funzione obiettivo.

L'errore relativo convenzionale è inoltre asimmetrico rispetto a sovrastima e sottostima. Ad esempio:

$$ M=2E\quad\Rightarrow\quad e^{rel}=1 $$

mentre:

$$ M=\frac{E}{2}\quad\Rightarrow\quad e^{rel}=0.5 $$

In entrambi i casi modello ed esperimento differiscono invece dello stesso fattore moltiplicativo pari a due.

Per questi motivi viene adottato l'errore assoluto del rapporto logaritmico:

$$ \boxed{e_i=\left|\ln\left(\frac{M_i}{E_i}\right)\right|} $$

che è adimensionale e tratta in modo simmetrico sovrastime e sottostime dello stesso fattore moltiplicativo. Infatti:

$$ M=2E\quad\text{e}\quad M=\frac{E}{2} $$

producono entrambi:

$$ e_i=|\ln 2| $$

Più in generale, se:

$$ M=\alpha E $$

si ottiene:

$$ e_i=|\ln\alpha| $$

La discrepanza dipende quindi dal fattore moltiplicativo che separa modello ed esperimento, senza assegnare automaticamente un peso eccessivo ai target caratterizzati da valori sperimentali piccoli.

La forma logaritmica mantiene inoltre una penalizzazione progressiva per previsioni fortemente discordanti: l'errore continua ad aumentare all'aumentare del rapporto tra modello ed esperimento.

Tutte le quantità utilizzate nella calibrazione sono strettamente positive, condizione necessaria per l'applicazione del rapporto logaritmico.

---
## 14. Funzione obiettivo globale
La funzione obiettivo globale è definita come la media delle discrepanze logaritmiche assolute associate alle 12 osservabili considerate:

$$ \boxed{J=\frac{1}{12}\sum_{j=1}^{4}\left[\left|\ln\left(\frac{A_{A,intra,j}^{model}}{A_{A,intra,j}^{exp}}\right)\right|+\left|\ln\left(\frac{R_{intra,j}^{model}}{R_{intra,j}^{exp}}\right)\right|+\left|\ln\left(\frac{R_{GB,j}^{model}}{R_{GB,j}^{exp}}\right)\right|\right]} $$

In questo modo ciascuna delle quattro posizioni radiali contribuisce con tre termini e tutte le 12 osservabili entrano nella funzione obiettivo con lo stesso peso esplicito.

L'implementazione utilizza quindi esplicitamente il denominatore 12, corrispondente al numero effettivo di target inclusi nella calibrazione.

Il set di parametri caratterizzato dal minimo valore di $J$ tra tutti i punti campionati viene identificato come miglior set calibrato.

Il valore di $J$ non deve essere interpretato come un errore percentuale. Esso rappresenta la discrepanza logaritmica assoluta media tra modello ed esperimento: $J=0$ corrisponde a un accordo perfetto, mentre valori crescenti indicano differenze moltiplicative progressivamente maggiori.

La quantità:

$$ \exp(J) $$

corrisponde alla media geometrica dei fattori moltiplicativi associati ai singoli termini di errore e può quindi essere utilizzata come indicatore sintetico della discrepanza globale.

Il valore della funzione obiettivo viene comunque analizzato insieme ai singoli termini di errore e agli andamenti radiali delle osservabili, in modo da evitare che un singolo indicatore sintetico nasconda eventuali discrepanze locali.

---
## 15. Quantità diagnostiche
Oltre alle osservabili utilizzate direttamente nella funzione obiettivo, per ciascuna simulazione vengono salvate ulteriori quantità utili per l'interpretazione fisica del risultato.

Tra queste:

- frazione areale sperimentale dei precipitati al bordo di grano;

- indicatore di copertura associato alla popolazione intergranulare;

- frazione volumica modellata dei precipitati intragranulari;

- frazione volumica modellata dei precipitati al bordo di grano;

- concentrazione di metalli precipitati intragranularmente;

- concentrazione di metalli precipitati al bordo di grano;

- concentrazione numerica delle particelle intragranulari;

- concentrazione numerica delle particelle intergranulari;

- numero medio di atomi per particella intragranulare;

- numero medio di atomi per particella intergranulare;

- burn-up effettivamente selezionato dall'output SCIANTIX.

Le frazioni volumiche modellate possono essere calcolate mediante:

$$ f_{V,intra} = C_{prec,intra}V_{eff} $$

e:

$$ f_{V,GB} = C_{prec,GB}V_{eff}. $$

Queste quantità non vengono confrontate direttamente con la frazione areale sperimentale, ma vengono mantenute come controllo dell'inventario fisico dei prodotti di fissione metallici.

L'indicatore di copertura GB viene utilizzato esclusivamente come quantità diagnostica. Esso non viene considerato equivalente, in senso stretto, alla frazione areale misurata sulla ROI TEM e non contribuisce alla funzione obiettivo.

---
## 16. Selezione del miglior set
Per ciascun punto della sequenza di Sobol vengono:

1\. generati i quattro parametri liberi del modello;

2\. assegnati i parametri mantenuti fissi ai rispettivi valori di riferimento;

3\. scritti i corrispondenti file di input;

4\. eseguite quattro simulazioni SCIANTIX;

5\. selezionato il punto dell'output corrispondente al burn-up sperimentale;

6\. calcolate le quantità modellate;

7\. calcolate le 12 discrepanze logaritmiche utilizzate nella funzione obiettivo;

8\. calcolato $J$;

9\. salvate le quantità diagnostiche.

La combinazione con il valore minimo di $J$ viene infine selezionata.

È importante precisare che il risultato ottenuto rappresenta:

$$ \boxed{\text{il miglior set tra i punti effettivamente campionati}} $$

e non costituisce necessariamente il minimo globale continuo della funzione obiettivo.

La natura a bassa discrepanza della sequenza di Sobol consente una copertura uniforme del dominio; l'aumento del numero di punti permette di aumentare progressivamente la risoluzione dell'esplorazione dello spazio parametrico.

---
## 17. Analisi successiva alla calibrazione
Una volta identificato il miglior set parametrico, il risultato non viene valutato esclusivamente sulla base del valore della funzione obiettivo.

Vengono analizzati separatamente:

- discrepanza logaritmica sulla frazione areale intragranulare;

- discrepanza logaritmica sul raggio intragranulare;

- discrepanza logaritmica sul raggio al bordo di grano;

- andamento radiale delle tre osservabili;

- comportamento diagnostico della popolazione GB;

- concentrazioni precipitate intra e GB;

- numero di particelle;

- numero medio di atomi per particella;

- posizione del miglior set rispetto ai limiti dello spazio parametrico.

Quest'ultimo controllo è particolarmente importante.

Se uno o più parametri ottimali risultano prossimi a un limite dell'intervallo di calibrazione, ciò può indicare che lo spazio parametrico selezionato non comprende completamente la regione ottimale.

Al contrario, una soluzione interna agli intervalli, accompagnata da una buona descrizione delle singole osservabili, costituisce un'indicazione più robusta della qualità della calibrazione.

---
## 18. Sensitività e identificabilità
La riduzione del problema finale a quattro parametri liberi deriva da uno screening diagnostico preliminare e non dalla sola ricerca del minimo della funzione obiettivo.

Per $k_{nucl,0}$, la variazione su più ordini di grandezza ha evidenziato una regione di minimo nell'intervallo utilizzato per la calibrazione finale.

Per i parametri di precipitazione è stata osservata una forte correlazione tra i due canali. In particolare, i test diagnostici hanno prodotto valori praticamente identici della funzione obiettivo per coppie reciproche di variazioni di $k_{intra,0}$ e $k_{GB,0}$, ad esempio:

$$ sf_{k,intra}=10^{-1}\;\Longleftrightarrow\;sf_{k,GB}=10\qquad J=1.261132 $$

$$ sf_{k,intra}=10^{-2}\;\Longleftrightarrow\;sf_{k,GB}=10^{2}\qquad J=1.388780 $$

$$ sf_{k,intra}=10^{-3}\;\Longleftrightarrow\;sf_{k,GB}=10^{3}\qquad J=1.739208 $$

Una correlazione analoga è stata osservata tra $\Delta G_{intra}$ e $\Delta G_{GB}$. Nel diagnostic energetico:

$$ \Delta G_{intra}=0.5\;\mathrm{eV}\;\Longleftrightarrow\;\Delta G_{GB}=1.5\;\mathrm{eV}\qquad J=1.271638 $$

mentre la combinazione opposta produce:

$$ \Delta G_{intra}=1.5\;\mathrm{eV}\;\Longleftrightarrow\;\Delta G_{GB}=0.5\;\mathrm{eV}\qquad J=1.945598 $$

Queste equivalenze indicano che i dati finali non consentono di identificare indipendentemente entrambi i parametri di ciascuna coppia. Per questo motivo $k_{intra,0}$ e $\Delta G_{intra}$ vengono mantenuti al valore di riferimento, mentre $k_{GB,0}$ e $\Delta G_{GB}$ restano liberi.

Il prefattore di nucleazione GB risulta invece quasi ininfluente sulla funzione obiettivo:

| $sf_{k,nucl,GB}$ | $J$ |
|---|---|
| $10^{-2}$ | 1.627455 |
| $10^{-4}$ | 1.627614 |
| $10^{-6}$ | 1.627661 |

Nonostante la variazione della popolazione interna GB, l'effetto sui target utilizzati nella calibrazione è quindi trascurabile. Il parametro viene pertanto considerato scarsamente identificabile e fissato a uno, insieme alla corrispondente barriera energetica.

La presenza di una buona soluzione della funzione obiettivo non implica comunque che ciascun parametro sia identificato in maniera indipendente. Dopo la calibrazione finale vengono pertanto analizzati anche i migliori set, la loro distribuzione nel dominio e la posizione dei parametri rispetto ai limiti degli intervalli.

---
## 19. Sintesi della procedura
La procedura complessiva può essere riassunta come:

$$ \boxed{\text{configurazione nominale}\rightarrow\text{screening diagnostico}\rightarrow\text{riduzione a 4 parametri liberi}\rightarrow\text{4096 punti Sobol}\rightarrow\text{4 simulazioni per set}\rightarrow\text{12 confronti modello-esperimento}\rightarrow J\rightarrow\text{selezione del best set}} $$

La calibrazione è quindi globale rispetto alle quattro posizioni radiali e utilizza un unico set parametrico per descrivere l'intero insieme dei dati sperimentali.

La procedura finale non calibra il production yield, il coefficiente di re-solution o i parametri fissati durante lo screening: tali quantità restano definite dalla formulazione finale del modello e dalle scelte di normalizzazione adottate.