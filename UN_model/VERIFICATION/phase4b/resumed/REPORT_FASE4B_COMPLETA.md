# UN L3 — FASE 4B completata con decomposizione spazio/tempo

## Esito e perimetro

Completate **50 configurazioni temporali e 60 spaziali**, tutte a **4000 h**, sui dieci set congelati. Aggiunte **60 simulazioni brevi** sui transitori rapidi. Nessun campionamento LHS. L'implementazione `Solver::SpectralDiffusion3equationsExchange` è quella di produzione, collegata direttamente al driver 4A invariato; non sono stati modificati sorgenti, fisica, parametri UN L3 o reference precedenti.

Le 110 configurazioni sono registrate in [main_ledger.json](main_ledger.json): **94 nuove esecuzioni C++ e 16 riusi verificati**. Dieci riusi corrispondono alle configurazioni N=40, dt=225 s comuni ai due studi; altri sei provengono dai cinque benchmark precedenti e dal precontrollo E2/N=320/dt=225 s. Quindi sono 100 configurazioni C++ distinte nella campagna principale, più 60 brevi, senza conteggiare due volte il costo dei dati riutilizzati. [Riepilogo numerico](analysis_v2/summary.json).

Branch `development/nitride`, HEAD `971621968f8f077f51a6c0107732640923291303`. Restano invariati i **321 file staged**, i risultati 4A, il precedente arresto di costo 4B e gli altri **10.008 file protetti**. Nessun commit, push, merge, cambio branch o riordino. La verifica finale è in [validation.json](validation.json), il checkpoint in [checkpoints/completed/manifest.json](checkpoints/completed/manifest.json).

## Protocollo effettivo e reference

- Temporale: N=40, dt=3600/1800/900/450/225 s, T=14.400.000 s, **4001 osservazioni orarie per configurazione**, inclusi t=0 e t=T. Tutti i coefficienti modali C++ sono conservati nei file binari, non soltanto le medie.
- Spaziale: N=10/20/40/80/160/320, dt pratico **225 s**, stesso T. Decomposizione a **0,1,2,4,8,24,100,500,1000,2000,4000 h**. Le storie C++ e la reference allo stesso N restano anche disponibili su griglia oraria.
- Sorgente comune `(1.7995675661038836e19,0,0)` at/(m³ s), iniziali nulle; Rg=6e-6 m. I dieci set, la provenienza dei quattro realistici e i limiti Table B.1 sono quelli precedentemente congelati, senza rigenerarli. [Configurazione e hash](configuration.json), [CSV originale dei parametri](../parameters_10.csv).
- Metodo C++: **Backward Euler** per ciascun modo, `(I-hJ)x_next=x+h*s`. L'ordine nominale 1 deriva dal codice, non da un'assunzione sul grafico.

La reference riusa le matrici, le proiezioni e la normalizzazione 4A. Per le storie orarie viene calcolato `scipy.linalg.expm` del sistema aumentato e composto il suo **semigruppo esatto**; questo non introduce una discretizzazione Backward Euler. L'accumulo usa `longdouble`. Il risultato a 1800 s è controllato rispetto al semigruppo a 3600 s e rispetto a expm diretta agli undici tempi sopra elencati. Le differenze vengono registrate, non cancellate o attribuite automaticamente al C++.

Per gb=0 e bulk inizialmente e forzatamente nullo si risolve il sottospazio invariante c/md, con mb esattamente zero per algebra: nessun clipping. I metadati delle cache riportano metodo, tempi e scarti. Il limite operativo di allarme reference è 1e-8 sulla scala della singola popolazione; gli scarti effettivi sono molto inferiori. Per N=40, lo scarto modale massimo fra le due formulazioni arriva a circa **4,46e-10** nei set stiff; la differenza fra i due passi del semigruppo è circa 1e-14–1e-13. I dati diretti e quelli del semigruppo sono due valutazioni numeriche della stessa soluzione continua.

### Reference spaziale effettivamente controllata

Si espande N=320,640,1280,2560,5120 e, quando necessario, 10240/20480. Si calcolano soltanto i nuovi modi; i prefissi già valutati restano identici e conservati. Controlli per ogni popolazione attiva e ogni tempo positivo osservato:

1. incremento della media al raddoppio ≤ **1%** dell'errore spaziale a N=320;
2. incremento relativo dell'**energia della coda modale** oltre N=320 ≤ **1%**.

Il secondo controllo riguarda una quantità scalare, la norma L2 al quadrato; non equivale a dire che la differenza fra due interi vettori modali sia inferiore all'1%. Gli incrementi finali sono in [reference_convergence.csv](analysis_v2/reference_convergence.csv):

| Set | N reference | Max incremento media / errore N320 | Max incremento relativo energia coda |
|---|---:|---:|---:|
| R1 | 5120 | 0,001777 | 0,0000323 |
| R2 | 5120 | 0,001770 | 0,0000320 |
| R3 | 5120 | 0,001755 | 0,0000314 |
| R4 | 5120 | 0,001771 | 0,0000321 |
| E1 | 5120 | 0,001911 | 0,0000385 |
| E2 | 5120 | 0,001716 | 0,0000298 |
| E3 | 20480 | 0,005985 | 0,002361 |
| E4 | 5120 | 0,001756 | 0,0000314 |
| E5 | 5120 | 0,002006 | 0,0000430 |
| E6 | 20480 | 0,005325 | 0,001880 |

Sono evidenze di convergenza per raddoppi successivi, non un limite rigoroso della coda infinita. Le reference ad alto N servono alla verifica del troncamento; il vero C++ è testato fino a N=320, con supporto verificato nel ciclo del solver. [Grafico della reference](figures/high_reference_convergence.png).

## Definizioni degli errori

Ogni media è normalizzata con il massimo analitico della **propria popolazione** nella storia osservata, con floor 1 at/m³. La norma modale usa il massimo della norma L2 analitica della propria popolazione. Un errore normalizzato 0,01 significa 1% di quella scala, non necessariamente 1% del valore istantaneo. I CSV contengono anche errori assoluti e relativi istantanei con denominatore `max(|valore|,1e-12*scala,1)`, esplicitamente registrato. Le componenti strutturalmente inattive sono identificate e non usate per dedurre ordini.

Per lo studio spaziale viene applicata, **prima delle norme**, l'identità con segno:

`Cpp − analitica_highN = (Cpp − analitica_stessoN) + (analitica_stessoN − analitica_highN)`.

È verificata per tutte le medie e i coefficienti mantenuti. Sui modi mantenuti il termine spaziale è zero: le equazioni modali sono indipendenti da N. Il troncamento del **campo completo** comprende invece tutti i modi omessi: il C++ viene esteso con zeri nello spazio high-N. Per ortogonalità, la norma totale soddisfa `L2_totale² = L2_temporale² + L2_coda²`. Le norme non vanno sommate come gli errori con segno. La norma è quella sulla sfera unitaria; il fattore costante R^(3/2) del dominio fisico si cancella nei rapporti normalizzati.

Il residuo normalizzato misurato dell'identità sulle medie è **0** ai valori float64 salvati. Il CSV riporta separatamente tutti e tre i termini. Su **204 campioni attivi** della griglia spaziale il termine temporale domina: il C++ grezzo non è presentato come puro errore spaziale.

## Convergenza temporale: risultati dei 50 test

Massimo sulla storia oraria, per popolazione. La tabella mostra i due estremi di dt; tutti i cinque valori, gli errori assoluti, le regioni temporali e gli ordini sono nei CSV.

| Set | Max temporale fra popolazioni, dt=1 h | Max temporale, dt=225 s |
|---|---:|---:|
| R1 AP3.2/900 K | 2,80309e-3 | 1,87600e-4 |
| R2 AP3.4/1600 K | 1,56248e-3 | 3,16099e-5 |
| R3 AP3.4/1800 K | 1,28360e-4 | 6,84758e-6 |
| R4 ANP6/1600 K | 1,44199e-3 | 2,56711e-5 |
| E1 | 6,83928e-3 | 4,68859e-4 |
| E2 | **3,75404e-2** | **2,68176e-3** |
| E3 | 6,47841e-3 | 5,02925e-4 |
| E4 | 1,40428e-4 | 1,40388e-7 |
| E5 | 2,73462e-5 | 1,71026e-6 |
| E6 | 7,16524e-8 | 4,22575e-9 |

Il caso peggiore è **E2, md, dt=3600 s**: errore assoluto massimo **2,03305e21 at/m³**, scala md=5,41564e22, normalizzato **3,754%**. A 4000 h la stessa prova ha errore relativo alla scala di circa **1,55e-16**: il solo finale nasconderebbe il transitorio. Con dt=225 s il massimo fra tutte le popolazioni scende a **0,2682%**, in c/E2.

Per il raffinamento 450→225 s, gli ordini sul massimo delle medie sopra il livello di riferimento vanno da **0,9693 a 1,4079**; sulle norme modali da **0,9693 a 1,4070**. I massimi orari non sono sempre nel regime asintotico: sull'intera sequenza compaiono ordini apparenti fino a circa 7,30, e su valori a un singolo istante anche ordini negativi. Per esempio mb/R2 a 1 h ha cancellazione dell'errore vicino a dt=1800 s; E4/c passa dall'errore del primo passo fortemente implicito a un transitorio rapido già smorzato con più passi. Questi numeri sono conservati, non filtrati per ottenere p=1. Non dimostrano un metodo di ordine superiore né una mancata convergenza dell'implementazione.

I CSV degli ordini segnalano i valori sotto o vicini alla sensibilità della reference. Il floor di screening è il maggiore fra 100 epsilon e 20 volte lo scarto fra reference: serve a evitare ordini privi di significato vicino al roundoff, non è una tolleranza applicativa o un cambiamento della fisica. Nelle prove brevi dedicate l'ordine 1 emerge senza l'ambiguità dei transitori persi dalla griglia oraria.

- [Errori, regioni 0–24/24–1000/1000–4000 h e scale per variabile](analysis_v2/temporal_summary.csv).
- [Errori orari completi](analysis_v2/temporal_history.csv.gz).
- [Errori di ciascun coefficiente modale](analysis_v2/temporal_modal_errors.csv), [ordini per modo](audit/temporal_orders_per_mode.csv).
- [Ordini sulle medie e norme](analysis_v2/temporal_orders.csv), [convergenza log-log](figures/temporal_convergence.png), [ordini](figures/temporal_orders.png), [storie degli errori](figures/error_histories.png).

## Convergenza spaziale: risultati dei 60 test

Massimo fra popolazioni e undici tempi osservati; solo termine analitico di troncamento, non il C++ grezzo.

| Set | Media N40 | Media N320 | Campo L2 N40 | Campo L2 N320 |
|---|---:|---:|---:|---:|
| R1 | 2,15343e-4 | 4,34545e-7 | 2,30232e-3 | 1,30736e-5 |
| R2 | 1,84006e-4 | 3,71310e-7 | 1,96336e-3 | 1,11489e-5 |
| R3 | 1,72664e-4 | 3,48423e-7 | 1,84073e-3 | 1,04525e-5 |
| R4 | 1,79639e-4 | 3,62499e-7 | 1,91616e-3 | 1,08808e-5 |
| E1 | 1,98992e-3 | 4,47613e-6 | 2,10622e-2 | 1,36823e-4 |
| E2 | 4,63473e-6 | 9,35253e-9 | 4,24598e-5 | 2,41106e-7 |
| E3 | **1,47375e-2** | **1,63401e-3** | **1,20874e-1** | **3,89646e-2** |
| E4 | 2,04565e-3 | 4,62646e-6 | 2,16293e-2 | 1,41436e-4 |
| E5 | 5,25352e-3 | 1,71061e-5 | 5,21366e-2 | 5,24914e-4 |
| E6 | 1,18496e-2 | 2,53935e-4 | 1,02636e-1 | 7,72631e-3 |

In **E3/mb a 4000 h**, N=320 ha errore spaziale medio **4,23315e23 at/m³** e L2 **2,06612e25** nelle coordinate modali della sfera unitaria: **0,1634%** e **3,896%** delle rispettive scale. In quel punto il termine temporale della media è solo **1,363e-10** normalizzato. L'errore L2 resta quindi significativo anche quando la media appare precisa.

All'ultimo raffinamento 160→320, considerando tempi positivi e componenti attive, gli ordini spaziali sono **1,034–2,994 per le medie**, **0,525–2,494 per il campo L2**. Non esiste un unico ordine su tutti i regimi. Quando le alte frequenze sono vicine al bilancio diffusive-stazionario, i coefficienti decadono come n^-3 e ci si avvicina a N^-3 per la media e N^-2,5 per L2. Nei regimi a bassa diffusione/trapping forte, il range testato può ancora avere coefficienti simili alla proiezione uniforme n^-1, con convergenza molto più lenta. Le curve analitiche mostrano la transizione senza confonderla con l'errore Backward Euler.

- [Tutti i termini, per popolazione/N/tempo](analysis_v2/spatial_errors.csv).
- [Ordini spaziali](analysis_v2/spatial_orders.csv).
- [Decomposizione di ogni coefficiente mantenuto](analysis_v2/spatial_retained_modal_decomposition.csv.gz); la coda è nelle cache high-N e nella norma L2 del CSV principale.
- [Convergenza delle medie](figures/spatial_mean_convergence.png), [convergenza L2](figures/spatial_L2_convergence.png), [esempio E2: contaminazione temporale](figures/error_decomposition_E2.png).

## Sessanta test rapidi aggiuntivi

Selezione deterministica: τ_rapido(N40)<225 s, cioè **R3, E2, E3, E4, E5, E6**. Per ognuno: N=40 e N=320, T=20τ_rapido del relativo N, dt=τ/10,/20,/40,/80,/160, osservazioni ogni 0,5τ. Parametri, iniziali e sorgente sono gli stessi dei set congelati. Non sono state introdotte iniziali artificiali o nuove microstrutture.

Le finestre vanno da **1,9511 a 472,1769 s**, dt minimo **0,00060973 s**. Ogni esecuzione usa nominalmente 200–3200 passi; alcune hanno un ulteriore passo finale minuscolo dovuto all'accumulo float del tempo nel driver 4A. Il campionamento viene ricondotto agli stessi 41 istanti entro 2e-12 della durata, senza alterare il solver. Sono documentati numero effettivo di passi e tempo effettivo campionato.

All'ultimo raffinamento, gli ordini sono **0,99626–0,999998** sulle medie e **0,99626–0,999998** sulle norme modali. Il massimo errore medio normalizzato scende da **1,766%** a τ/10 a **0,1147%** a τ/160. Le scale di queste percentuali sono quelle della finestra breve, diverse da quelle a 4000 h.

Si controlla inoltre **ogni singolo modo**, incluso il più rapido n=N, che potrebbe avere peso piccolo nelle medie: [errori modali](rapid_modal/errors_per_mode.csv), [ordini modali](rapid_modal/orders_per_mode.csv), [solo modo più rapido](rapid_modal/fastest_mode_errors.csv), [riepilogo](rapid_modal/summary.json), [storia del modo 320/E2](rapid_modal/fastest_mode_history.png). Questo evita di dedurre che il modo più rapido sia risolto soltanto dall'errore aggregato.

[Registro delle 60 prove e tempi](rapid_ledger.json), [errori e ordini aggregati](analysis_v2/rapid_summary.csv), [ordini](analysis_v2/rapid_orders.csv), [storie](analysis_v2/rapid_history.csv.gz), [grafico log-log](figures/fast_transient_convergence.png). Queste prove risolvono la scala rapida; non sostituiscono lo studio dei tempi lenti.

## Controlli dell'implementazione e del regime lungo

L'intero insieme delle **100 configurazioni principali C++ distinte** è confrontato anche con un aggiornamento BE indipendente: `numpy.linalg.solve(I-hJ,...)` e composizione affine, separato dalla reference continua expm. Il massimo scarto normalizzato in L2 modale è **6,62e-12**. Questo controllo distingue l'errore di discretizzazione da un'implementazione diversa del metodo. Non viene usato come soluzione analitica del problema continuo.

Verificati: iniziali nulle, passo finale a 4000 h, numero effettivo di passi, tutti i modi finiti, ricostruzione delle medie con le proiezioni 4A, positività dopo aver fattorizzato il segno della proiezione modale e nullità bulk quando gb=0. I vettori modali vengono mantenuti tra tutte le chiamate del vero solver nel driver; questa è la verification lineare, non una nuova regressione del modello UN completo.

[Controlli BE temporali](analysis_v2/implementation_checks.csv), [controlli degli altri N](audit/implementation_checks_other_N.csv). Le soglie operative 1e-8 per il confronto BE e 5e-14 per la ricostruzione controllano errori numerici accumulati in float64 e non sono tolleranze fisiche o modifiche ai test Python–SCIANTIX congelati. Gli scarti effettivi sono riportati per ciascuna configurazione.

Il limite stazionario matematico è studiato **separatamente** con la media esatta `S_c R²/(15D)`, rapporti gb/bb e gd/bd e somma finita n^-4. [CSV del limite stazionario](analysis_v2/stationary_limit_separate.csv). Non sono dichiarate simulazioni C++ stazionarie aggiuntive: a 4000 h quasi tutti i set sono ancora lontani dalla scala più lenta. Per esempio τ_lento(n1) è circa 5,31 h in E2, 2,04e5 h in R3 e 2,95e10 h in E3. La precisione finale E2 non è generalizzabile ai restanti set.

## Costi effettivi e riproducibilità

| Blocco | Tempo misurato [s] |
|---|---:|
| Preflight reference, inclusa compilazione | 3,882 |
| Compilazione, sottoparte del preflight | 1,152 |
| Campagna principale, C++ + cache + I/O | 70,013 |
| 60 prove rapide, reference e I/O | 2,740 |
| Analisi principale, CSV inclusi | 23,052 |
| Audit degli altri N e ordini per modo | 3,960 |
| Otto figure PNG/PDF | 14,102 |

Questi blocchi non sovrapposti sommano circa **117,75 s**, esclusi preparazione degli script, audit modale rapido aggiuntivo, controllo di preservazione finale e avvio dell'interprete. Le sotto-misure non devono essere sommate di nuovo ai blocchi completi:

- nuovi kernel principali: **12,302 s**; nuovi processi C++ con I/O: **14,643 s**;
- kernel delle 60 prove brevi: **0,475 s**; processi con I/O: **0,647 s**;
- costruzione cache analitiche orarie: **12,994 s** complessivi;
- reference high-N: **29,777 s**.

Il caching dell'esponenziale allo stesso N elimina la ricostruzione ripetuta per i cinque dt. Il precedente costo di mesi riguardava un protocollo diverso, con dt uniformemente minuscolo per annullare la contaminazione temporale del C++ grezzo. Non è stato ridotto cambiando parametri o tolleranze: ora i due errori vengono misurati separatamente, come approvato.

I tempi originali dei dati riutilizzati restano associati ai file originali, senza essere presentati come nuove esecuzioni. [Costi per caso](audit/costs_per_case.csv), [istruzioni riproducibili](README.md), [manifest di build](build_manifest.json).

## Limiti e seguito

La verification conferma l'aggiornamento BE del solver e la convergenza osservata sui regimi testati. **Non certifica un'unica coppia N/dt per ogni accuratezza desiderata**: manca una soglia applicativa concordata e N=40/dt=1 h non è sufficiente a descrivere tutti i transitori o tutti i campi spaziali estremi. Anche N=320 può lasciare quasi 4% in L2 per E3. Le statistiche principali sono su griglia oraria e gli errori spaziali su undici tempi: non sono massimi continui rigorosi. I transitori rapidi sono verificati separatamente e gli incrementi high-N non costituiscono un teorema sulla coda infinita.

Nessun nuovo problema di implementazione o costo incompatibile è emerso con il protocollo approvato. Il primo avvio del solo postprocessing si è interrotto per la forma del vettore RHS di `numpy.linalg.solve` in NumPy 2; è stato corretto l'adattatore locale. Il tentativo incompleto resta in `analysis/` e `analysis.log`; i dati canonici sono in `analysis_v2/`. Nessuna simulazione è stata alterata o ripetuta per questa correzione.

La [proposta LHS separata](PROPOSTA_LHS.md) usa i costi misurati e mantiene aperta la scelta delle soglie e delle griglie. **FASE 4B conclusa; arresto prima di generare qualsiasi campione LHS.**
