# SCIANTIX UN L3 — FASE 4A: verification preliminare

## Esito e limiti

Completati il benchmark iniziale, i controlli della base sferica e **cinque casi pilota**, con **25 esecuzioni temporali e 25 spaziali**, oltre alle prove di supporto e scelta del passo. Il vero solver SCIANTIX è collegato senza modificarlo. L'ordine temporale osservato tende a uno, coerentemente con Backward Euler letto nel codice. La convergenza spaziale dipende dall'osservabile e dal caso limite; non emerge un unico ordine universale.

Questa verifica riguarda il sistema lineare a coefficienti costanti. Non sostituisce la regressione del modello completo delle FASI 2D/3 e non verifica analiticamente la microstruttura nonlineare. Le durate finali di tesi, il numero LHS e il riordino restano da approvare. Nessun LHS eseguito, nessuna directory storica spostata.

Branch e HEAD invariati: `development/nitride`, `971621968f8f077f51a6c0107732640923291303`; origin coincide con lo stato iniziale sincronizzato. Verificati **7.211 file preesistenti**, immutati. Tutti i nuovi artefatti sono dentro `UN_model/VERIFICATION/`.

## 1. Benchmark di velocità eseguito per primo

Configurazione: N=40, dt=3600 s, T=86400 s, 24 aggiornamenti. Coefficienti rappresentativi di AP3.4/1600 K, registrati al passo congelato 100 della FASE 1: D=1,3705602340796549e-18 m²/s, R=6e-6 m, g_b=3,691366838061218e-4 s⁻¹, g_d=1,4480265981793865e-3 s⁻¹, b_b=1,7178041769597167e-5 s⁻¹, b_d=5,310375031628281e-6 s⁻¹. Sorgente s_c=1,7995675661038836e19 at/(m³ s), s_b=s_d=0; gas iniziale nullo. Non viene applicato il termine nonlineare della nucleazione.

| Misura wall | Tempo |
|---|---:|
| Compilazione g++ del driver e Solver.C | 1,124019 s |
| Chiamate effettive al solver C++ | 2,9971e-5 s, circa 30 µs |
| Processo C++ complessivo, avvio e I/O inclusi | 0,004648 s |
| Soluzione analitica Python e confronto | 0,028475 s |

Il tempo del solver è la somma delle misure `steady_clock` intorno alle chiamate, inclusa la copia dei parametri prevista dall'API; esclude I/O e inizializzazione. Su soli 24 passi è una misura molto breve e soggetta a overhead/rumore: non è un benchmark definitivo delle prestazioni del modello completo. Il tempo Python esclude import delle librerie e salvataggio degli output. L'errore massimo sulle medie, normalizzato alla scala della storia analitica, è **1,16577e-3**: è errore di discretizzazione a passo finito, non un fallimento della regressione Python–C++ precedente.

Ambiente riutilizzato: g++ 13.3.0, `-std=c++11 -O2 -Wall -Wextra`, Python 3.12.3, NumPy 2.4.4. SciPy 1.16.2 è stato installato esclusivamente nel target locale `VERIFICATION/dependencies`; Matplotlib proviene dalla `.venv` esistente. Nessuna modifica alla `.venv` o installazione globale. [Benchmark completo](results/speed_initial/results.json), [grafico tempi](results/figures/initial_speed.png).

## 2. Problema indipendente e normalizzazione

Sia x=r/R. Per le tre concentrazioni c, m_b, m_d:

\[
\partial_t c=\frac{D}{R^2x^2}\partial_x(x^2\partial_x c)
 -(g_b+g_d)c+b_bm_b+b_dm_d+s_c,
\]
\[
\partial_t m_b=g_bc-b_bm_b+s_b,\qquad
\partial_t m_d=g_dc-b_dm_d+s_d.
\]

Per la componente mobile: c(1,t)=0, regolarità e derivata radiale nulla al centro. Le componenti immobili non hanno un proprio operatore diffusivo né una distinta condizione di flusso. Tutte sono rappresentate nella stessa base L2.

Base adimensionale ortonormale nella misura 4πx² dx:

\[
\phi_n(x)=\frac{\sin(n\pi x)}{\sqrt{2\pi}\,x},\quad
\phi_n(0)=\frac{n\pi}{\sqrt{2\pi}},\quad
q_n=\langle1,\phi_n\rangle=\sqrt{8/\pi}\frac{(-1)^{n+1}}n.
\]

La media di volume è `Σ q_n a_n / V`, con V=4π/3. La sorgente uniforme produce coefficienti q_n s. Per ciascun modo:

\[
\dot a_n=J_na_n+q_ns,\qquad
J_n=\begin{pmatrix}
-[D(n\pi/R)^2+g_b+g_d]&b_b&b_d\\
g_b&-b_b&0\\g_d&0&-b_d
\end{pmatrix}.
\]

`analytic.py` risolve questo IVP con `scipy.linalg.expm`. La matrice aumentata 4×4 evita J⁻¹ e gestisce anche matrici singolari, assenza di diffusione o tassi nulli. Prima di esponenziare viene scalata la concentrazione per evitare che la grande sorgente dimensionale domini artificialmente la norma della matrice aumentata. Non viene usato il solver Cramer o il kernel del notebook per generare la reference.

Per lo studio molto lungo dei due casi accoppiati si usa la forma equivalente `a_eq + expm(J*t)(a_0-a_eq)`. L'equilibrio è derivato dai bilanci stazionari; questa forma evita la propagazione di arrotondamenti associati all'autovalore zero della matrice aumentata. Il confronto tra le due forme ai tempi temporali pilota dà differenze relative 9,98e-12 e 7,44e-11; la forma traslata non viene usata nei brevi transitori, dove la sottrazione di grandi equilibri può perdere cifre. I controlli indipendenti dei limiti scalari e della soluzione stazionaria confermano la reference scelta.

Quadratura Gauss–Legendre a 1024 punti su n=1,2,3,10,40,80,160: errore massimo di ortonormalità **3,33e-14**, della proiezione uniforme **2,75e-14**; condizione al bordo e regolarità al centro verificate. [Controlli della base](basis_checks.json).

### Proiezione e inizializzatore SCIANTIX

Una proiezione ortogonale troncata della costante ha media `f_N C0`, dove

\[
f_N=\sum_{n=1}^N q_n^2/V=\frac6{\pi^2}\sum_{n=1}^N\frac1{n^2}<1.
\]

La sorgente troncata ha quindi media f_N s: questa coda deve essere inclusa nell'analisi spaziale e nei bilanci. Il driver usa l'inizializzazione ortogonale q_n C0, uguale alla reference, per confrontare lo stesso problema a ogni N. `Solver::modeInitialization`, controllato separatamente per tutti i N richiesti, ripete 20 correzioni e produce invece il fattore `[1-(1-f_N)^20]/f_N`, conservando la media iniziale. Non è la stessa inizializzazione; non è stato modificato il comportamento di produzione. I due casi L3 e il caso diffusivo partono comunque da gas nullo.

## 3. Casi, supporto e convergenza temporale

Il solver usa `unsigned short` per n+1. Il driver impone 1≤N≤65535; N=10,20,40,80,160 sono stati effettivamente eseguiti con array dinamici, risultati finiti e inizializzatore unitario entro 1e-13. Nessun buffer SCIANTIX globale modificato.

| Caso | Definizione | T temporale fisso |
|---|---|---:|
| ordinary_L3_1600K | Coefficienti congelati AP3.4/1600 K, passo 100 | 48 h |
| stiff_L3_1800K | Coefficienti congelati AP3.4/1800 K, passo 100, maggiore trapping/diffusione | 6 h |
| pure_diffusion | D e sorgente del caso 1800 K; tutti gli scambi nulli | 18 h |
| exchange_only | D=0; trapping del caso ordinario ×0,01; b invariati; iniziali 1e23,2e22,1e22 at/m³ | 48 h |
| source_only | D e scambi nulli; sola produzione uniforme | 12 h |

I tre limiti sono problemi sintetici di verification, non modifiche ai parametri calibrati UN L3. Configurazioni complete e hash della provenienza in [cases.json](results/pilots_v2/cases.json).

N fisso a 40; dt=3600,1800,900,450,225 s. Il C++ evolve a ogni passo, senza reset alle soluzioni analitiche. Confronto con `expm` **allo stesso N** sui tempi comuni ogni ora e al termine, con T invariato all'interno di ogni sweep. Misurate sia le medie sia la norma L2 dei coefficienti ortonormali. Gli errori sono normalizzati con la scala massima della stessa storia analitica; il CSV separato per popolazione conserva gli errori assoluti delle tre medie.

Il codice di produzione contiene esplicitamente `(I-dt*J)a_new=a_old+dt*s`: Backward Euler, ordine globale atteso uno per il sistema regolare a N fissato. Non è stato dedotto l'ordine dal solo fit numerico.

| Caso | Errore normalizzato medie, dt=225 s | Ultimo ordine osservato medie | Ultimo ordine L2 modi |
|---|---:|---:|---:|
| ordinary | 3,5972e-5 | 0,9980 | 0,9980 |
| stiff | 1,2920e-5 | 0,9969 | 0,9976 |
| pure diffusion | 6,9884e-4 | 0,9988 | 0,9986 |
| exchange only | 2,9761e-4 | 0,9967 | 0,9970 |
| source only | 2,4539e-15 | non stimabile | non stimabile |

Gli ordini sono `log2(E_h/E_h/2)`. Il caso source-only è esatto nel tempo salvo arrotondamenti; non gli viene attribuito un ordine fittizio. I risultati degli altri casi tendono a uno con errore monotono sulle griglie studiate. [CSV temporale](results/pilots_v2/temporal_errors.csv), [per popolazione](results/pilots_v2/temporal_errors_per_population.csv), [grafico log-log](results/figures/temporal_convergence.png), [ordini](results/figures/temporal_orders.png).

I campioni comuni orari non risolvono i transitori più rapidi del caso stiff. Lo studio dimostra l'ordine sulle osservabili campionate, non l'accuratezza dei primi secondi. Le prove successive dovranno introdurre osservazioni e passi coerenti con quei tempi, come proposto separatamente.

## 4. Convergenza spaziale separata dall'errore temporale

N=10,20,40,80,160. Lo studio spaziale valuta **lo stato finale lungo**, in un esperimento distinto dai transitori precedenti. Per ogni caso T=32 τ_lento, e dt iniziale=τ_lento/128; per source-only non esistono autovalori di decadimento e si usa la scala di accumulo 1e24/s_c. T e dt sono poi fissi mentre varia N. Nel pilot il primo dt proposto ha già superato il controllo, senza riduzioni.

Al N=160 è stata confrontata la soluzione C++ con `expm` allo stesso N: rapporto fra errore temporale e troncamento spaziale rispettivamente **1,71e-5, 2,40e-6, 1,95e-7, 1,68e-10, 3,90e-12**, tutti sotto il limite di separazione 0,05. Questi dt sono accurati **per lo stato finale scelto**; non descrivono accuratamente i transitori veloci. Il limite e gli eventuali tentativi sono registrati in [spatial_dt_selection.json](results/pilots_v2/spatial_dt_selection.json).

Reference analitica controllata a N=320,640,1280,2560, con possibilità predisposta fino a 5120. Per i tre casi diffusivi è sufficiente **N_ref=2560**: differenza fra reference successive <1e-8 della scala e <1% del minimo errore spaziale pilota. Anche la coda L2 residua è controllata tramite la serie esatta dei coefficienti stazionari, proporzionale alla zeta di Hurwitz `zeta(6,N+1)`: la coda oltre N_ref è meno dell'1% di quella oltre N=160. Per D=0 il troncamento grezzo converge lentamente come 1/N: non viene dichiarato sufficientemente convergente il solo valore grezzo a N=640. Poiché J è identico per ogni n, la coda è nota esattamente da `Σq_n²=V`; la reference ad alto N viene completata con quella coda, senza fit o spostamento di tolleranze. Le reference corrette convergono al livello dell'arrotondamento. Sono conservati sia i valori grezzi sia quelli completi in [reference_convergence.json](results/pilots_v2/reference_convergence.json).

| Classe | Ordine medio osservato 80→160 | Ordine L2 profilo 80→160 |
|---|---:|---:|
| D>0, tre casi | 2,9867 circa | 2,4887 circa |
| D=0, due casi limite | 0,9955 circa | 0,4977 circa |

Per sorgente uniforme a regime diffusivo, i coefficienti sono O(n⁻³): la coda della media è O(N⁻³), quella L2 O(N⁻⁵/²). Senza diffusione la distribuzione uniforme rappresentata in questa base ha coefficienti O(n⁻¹): media O(N⁻¹), L2 O(N⁻¹/²). I risultati misurati si avvicinano a questi limiti derivati, invece di presumere un ordine spaziale identico per qualsiasi dato. Le curve normalizzate dei tre casi diffusivi si sovrappongono perché il profilo stazionario è la stessa parabola riscalata; analogamente si sovrappongono i due limiti uniformi.

[CSV spaziale](results/pilots_v2/spatial_errors.csv), [grafico log-log](results/figures/spatial_convergence.png), [convergenza reference](results/figures/reference_convergence.png). N=160 non è una garanzia universale di accuratezza: in un limite D=0 la coda uniforme della media è ancora circa 0,00379; ciò è coerente con la proiezione spettrale, non un nuovo difetto introdotto nel solver.

## 5. Tempi caratteristici e scelta delle durate

I tempi propri sono ricavati dagli autovalori di J_n, separando gli autovalori nulli; sono riportati per n=1 e per l'ultimo modo a N=40/160. I tempi reciproci delle reazioni e il rate D(nπ/R)² spiegano i diversi regimi. Il [CSV dei tempi caratteristici](characteristic_times.csv) riporta anche 1/g_b, 1/g_d, 1/b_b, 1/b_d e R²/(D n²π²), lasciando vuoti i reciproci dei rate nulli. I valori n=1 sono:

| Caso | Tempi finiti principali [s] | T spaziale matematico |
|---|---|---:|
| ordinary | 548; 6,78e4; 7,86e8 | 2,5143e10 s, circa 796,7 anni |
| stiff | 33,3; 5,94e4; 7,35e8 | 2,3526e10 s, circa 745,5 anni |
| pure diffusion | 1,30e5 | 4,1523e6 s, circa 48,1 giorni |
| exchange only | 3,62e4; 7,69e4, più un autovalore nullo | 2,4617e6 s, circa 28,5 giorni |
| source only | nessun decadimento; accumulo 1e24/s_c≈5,56e4 s | 1,7782e6 s, circa 20,6 giorni |

Le durate di secoli sono **esclusivamente esperimenti del sottosistema lineare congelato** per isolare il troncamento spaziale finale; non sono una proposta di simulare per secoli il modello microstrutturale completo né una scelta delle durate definitive della tesi. Richiedono circa 4096 aggiornamenti con dt scalato, quindi non rendono costoso il test. Il rate limitante riflette il lento trasferimento diffusivo di gas fortemente intrappolato con coefficienti costanti; non autorizza a estrapolare la microstruttura fisica congelata per tali tempi.

Per studi futuri si propone una griglia di osservazione dimensionale ottenuta da finestre adimensionali: transitorio iniziale circa 0,01–5 τ_rapido; intermedio attorno ai tempi di scambio e fino a τ_lento; lungo 1–10 τ_lento per il solo sistema lineare. Per source-only usare tempi di accumulo rispetto a una concentrazione dichiarata. A N elevato τ_rapido diminuisce per la diffusione: scegliere dt rispetto al modo e all'osservabile che si intende risolvere. Non si assume più un'unica durata di 24 ore valida per ogni caso. [Grafico tempi](results/figures/characteristic_times.png), [proposta successiva](STUDI_SUCCESSIVI_E_LHS.md).

## 6. Controlli indipendenti, costi e stato del lavoro

`validate_results.py` verifica:

- expm contro la soluzione scalare con `expm1` per diffusione pura: scarto relativo 1,15e-15;
- expm contro crescita lineare per source-only: 2,16e-16;
- conservazione del gas nei due casi D=0, includendo f_N: residui 1,03e-13 e 2,10e-15;
- bilancio discreto Backward Euler del benchmark, con flusso valutato sui modi nuovi: 1,02e-14;
- reference spaziale contro la media della parabola stazionaria `s_c R²/(15D)`, con i rapporti di scambio all'equilibrio: scarto circa 1,84e-11, coerente con la coda finita a N_ref;
- integrità dei file, branch, HEAD, origin e index iniziali.

Tutti i controlli passano. Le soglie di questi controlli di identità/algebra non vengono usate per nascondere l'errore di discretizzazione, che è riportato integralmente nei CSV.

Costi aggregati misurati: 25 prove temporali, **0,00627 s nelle chiamate C++** e **0,891 s nella reference/comparazione Python**; 25 prove spaziali, **0,211 s nelle chiamate C++**; costruzione/controllo delle reference ad alto N, **3,333 s**. Sono misure di queste categorie, non un cronometro totale dell'intera sessione; selezione dt, avvii, compilazioni, import e grafici sono registrati o separati. Non è stato necessario ridurre i cinque casi per ragioni di costo. L'avvio dei processi e la reference dominano sul solver; conviene valutare un harness a lotti solo dopo aver fissato il disegno successivo.

Sei figure in PNG/PDF; CSV, configurazioni, output C++ e reference numeriche sono riproducibili tramite il [README](README.md). Un primo tentativo `results/pilots/` si è fermato nella preparazione del caso senza autovalori finiti: corretta la gestione della lista vuota e rieseguito tutto in `pilots_v2`; nessuna modifica alla fisica o ai risultati precedenti.

Il [checkpoint finale](checkpoints/complete/manifest.json) raccoglie script, documenti e hash. Restano da decidere durate finali, punti di osservazione iniziali più fitti, ulteriori condizioni iniziali/sorgenti e numero/range LHS. Il [riordino](PROPOSTA_RIORDINO.md) è soltanto una proposta. Il lavoro si ferma alla FASE 4A.
