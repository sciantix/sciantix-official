# Proposta dopo l'arresto di costo — nessuna alternativa ancora eseguita

Il collo di bottiglia non è il solver a dt=225 s: è richiedere che il suo errore temporale sia molto più piccolo di un troncamento spaziale di poche parti in 1e8, lungo tutta una storia di 4000 h, con un solo dt uniforme. Non si propone di cambiare Table B.1, parametri UN L3 o soglie per ottenere artificialmente una convergenza.

## Opzione raccomandata: separare esplicitamente le verifiche

1. Eseguire i **50 test temporali** previsti a N=40, T=4000 h, stessi sorgente/iniziali e cinque dt. Una reference esatta per caso, N e griglia comune può essere riutilizzata per ridurre il costo Python, senza cambiare i risultati. Restano da verificare accuratezza/condizionamento della reference nei regimi più stiff.
2. Eseguire i **60 test spaziali** su un dt pratico dichiarato, conservando l'errore C++ grezzo e scomponendolo, senza correggerlo, nell'identità

   `Cpp − analytic_highN = (Cpp − analytic_sameN) + (analytic_sameN − analytic_highN)`.

   La seconda componente misura il solo troncamento spaziale; la prima misura la contaminazione temporale. Le curve raw non vanno presentate come convergenza spaziale isolata quando la prima domina. Nei transitori l'ordine spaziale si misura sulla soluzione spettrale esatta; il C++ è verificato separatamente rispetto a quella allo stesso N. Questa è una **modifica esplicita del criterio operativo originale**, che richiede una scelta prima di lanciare la campagna.
3. Conservare separati i controlli C++ spaziali finali a regime, con dt effettivamente verificato come nella 4A. Le loro durate matematiche non sostituiscono l'analisi fisica 0–4000 h e non possono essere spacciate per soluzione dei transitori richiesti.

Questa opzione mantiene l'obiettivo scientifico di distinguere spazio e tempo e tutti i 110 confronti deterministici, senza centinaia di miliardi di passi. Non comporta alcuna modifica al solver di produzione. Non è stata eseguita ora, perché cambierebbe il requisito di dt trascurabile su tutta la storia.

## Alternative da valutare

**Mantenere il requisito rigoroso dei 60 test C++:** ridisegnare la griglia temporale con segmenti/adattività, identica per tutti i N dello stesso set, e controlli locali/globali dell'errore. Il solver accetta già un incremento per chiamata; servirebbe un harness di controllo distinto, senza riscrivere il metodo. Non è garantito che l'adattività basti: le osservazioni nei primi minuti/ore possono ancora richiedere accuratezze molto alte. Necessario un nuovo piccolo benchmark prima di stimare il costo.

**Estrapolazione temporale diagnostica:** combinare soluzioni del vero solver a h e h/2 e verificarne il residuo con expm. Può aiutare a stimare il limite semidiscreto e l'errore spaziale, ma la combinazione non è la soluzione grezza Backward Euler. Non va usata per attribuire al solver di produzione un ordine superiore o per nascondere i suoi errori. Anche questa cambierebbe il protocollo e richiede una scelta esplicita.

**Accuracy target applicativo:** definire errori desiderati per c, mb, md, medie e profili, con scale fisiche e gestione dei componenti inattivi. Un vincolo rispetto a uno spettro già molto più accurato della soglia applicativa può essere inutilmente costoso. L'eventuale sostituzione del criterio deve essere motivata e approvata, non introdotta dopo il confronto per far passare i risultati.

## Transitori rapidi, ancora da eseguire

Per E3–E6 e altri set stiff, proporre benchmark brevi a N=40 e nel modo più rapido a N=320, con T circa 10–20 τ_rapido e dt=τ_rapido/10,/20,/40,/80,/160, osservazioni ravvicinate e confronto allo stesso N. Usare iniziali/sorgente dichiarate ed eventualmente un singolo modo fuori equilibrio per rendere visibile il transitorio. Queste prove sono aggiuntive rispetto ai 110 e non sono state lanciate dopo l'arresto.

## LHS successivo: proposta, non campionamento

Prima del LHS occorrono decisioni su protocollo temporale/spaziale, soglie, osservabili e prova del costo. Rg e i cinque intervalli Table B.1 restano quelli assegnati. gb=0 deve essere gestito come limite deterministico o categoria esplicita, non inserito in una trasformazione logaritmica. Le combinazioni matematiche estreme non vanno presentate come microstrutture realistiche.

Solo dopo il completamento della verification deterministica: un pilot futuro da **32 punti**, quindi eventuale campagna da **128**, con estensioni a **256** soltanto se i risultati e le regioni peggiori non sono stabili. Sono numeri proposti, non approvati e non eseguiti. Il costo non può essere estrapolato dal solo benchmark a dt=225 s finché il disegno spaziale resta aperto; i budget preliminari della 4A non coprono il nuovo vincolo uniforme mostrato qui.

La decisione richiesta prima della ripresa è quindi: mantenere il rigido isolamento sul C++ grezzo con un nuovo protocollo temporale, oppure approvare la decomposizione esplicita spazio/tempo con test grezzi e reference separate. Nessuna campagna principale o LHS verrà avviata automaticamente da questo checkpoint.
