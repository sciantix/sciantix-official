# UN L3 — FASE 4B: benchmark e arresto prima della campagna principale

## Esito

**FASE 4B non completata: arresto al controllo di costo richiesto dall'utente.** Eseguite cinque simulazioni cronometrate da 4000 h e due precontrolli C++ per scegliere il dt spaziale. **0/50 test temporali principali, 0/60 spaziali principali, nessun benchmark dei rapidi transitori e nessun LHS.** Le sette esecuzioni del vero solver sono terminate correttamente; il motivo dell'arresto è il costo necessario a soddisfare la separazione degli errori nei transitori, non un fallimento della fisica UN L3 o della regressione precedente.

Tutti i nuovi file sono in `UN_model/VERIFICATION/phase4b/`. Il branch resta `development/nitride`, HEAD `97162196`. I file 4A preesistenti, compresi quelli che l'utente ha messo in staging, non sono stati modificati. [Validazione e preservazione](validation.json), [checkpoint](checkpoints/cost_gate_stop/manifest.json), [riproduzione](README.md).

## 1. Dieci configurazioni selezionate

Gli intervalli della Table B.1 forniti nella richiesta sono conservati senza cambiamenti in [bounds.json](bounds.json): Dg da 6,860e-20 a 2,814e-16 m²/s; gb da 0 a 2,042 s⁻¹; gd da 7,230e-5 a 0,3086 s⁻¹; bb da 1,024e-6 a 2,049e-4 s⁻¹; bd da 3,790e-7 a 2,049e-4 s⁻¹. Rg=6e-6 m in tutti i casi. Non è stata consultata o modificata la tesi: la tabella è quella riportata integralmente nella [richiesta](REQUEST.md).

Le quattro configurazioni realistiche R1–R4 sono estratte dalle tracce congelate AP3.2/900 K, AP3.4/1600 K, AP3.4/1800 K e ANP6/1600 K, allo stadio `after_exchange`, **passo 100 con indice da zero**, coefficienti dallo stato a 300 h per l'aggiornamento fino a 303 h. Il CSV riporta percorso e SHA-256 della traccia, intervallo temporale e tutti i coefficienti effettivi.

Per isolare l'effetto dei coefficienti, tutti i dieci set usano la stessa sorgente di prova `(1,7995675661038836e19,0,0)` at/(m³ s) e iniziali `(0,0,0)`. Il CSV conserva anche la sorgente originaria delle quattro tracce: per R1/R4 è diversa dalla sorgente comune del benchmark. I cinque coefficienti realistici sono riprodotti esattamente; questa è una verification lineare congelata, non la ripetizione delle storie fisiche complete.

| Set estremo | D | gb | gd | bb | bd |
|---|---|---|---|---|---|
| E1 | minimo | 0 | minimo | minimo | minimo |
| E2 | massimo | 0 | minimo | massimo | massimo |
| E3 | minimo | massimo | minimo | minimo | massimo |
| E4 | massimo | 0 | massimo | massimo | minimo |
| E5 | massimo | massimo | massimo | minimo | minimo |
| E6 | minimo | massimo | massimo | massimo | massimo |

Sono coperti diffusione minima/massima, trapping e risoluzione deboli/forti, bulk nullo e rapporti di scala estremi. Sono configurazioni matematiche, senza imporre che corrispondano a microstrutture UN realizzabili. [CSV dei dieci set e tempi caratteristici](parameters_10.csv), [JSON completo](cases.json).

I tempi propri sono calcolati per n=1,40,320. Nei casi con trapping estremo il più lento autovalore è troppo piccolo rispetto agli altri per affidarsi soltanto a un eigensolver generico. Si risolve stabilmente la relazione secolare del rate lento `k = rho [1+gb/(bb-rho)+gd/(bd-rho)]`, con k=D(nπ/R)², e si ricavano gli altri rate per deflazione. Per gb=0 il rate bulk bb è separato. Non vengono alterati coefficienti, equazioni o solver di produzione.

## 2. Cinque benchmark richiesti, eseguiti prima della campagna

N=40, dt=225 s, T=14.400.000 s=4000 h: **64.000 timestep per ogni prova**, 4001 righe di osservazione orarie. Ricompilato il driver 4A invariato, collegato direttamente a `src/classes/Solver.C`, in un build distinto da quello 4A. Reference `scipy.linalg.expm` 4A allo stesso N; le nonlinearità di nucleazione e microstruttura non partecipano al problema lineare.

| Caso | Solo solver C++ [s] | Processo C++ con I/O [s] | Analisi Python [s] | Completo con I/O [s] |
|---|---:|---:|---:|---:|
| R1 AP3.2/900 K | 0,07966 | 0,09874 | 4,5000 | 4,6225 |
| R2 AP3.4/1600 K | 0,08898 | 0,10918 | 4,4489 | 4,5746 |
| R3 AP3.4/1800 K | 0,09338 | 0,11529 | 5,3821 | 5,5128 |
| E2 Dmax, gb=0, risoluzione forte | 0,08865 | 0,11126 | 5,5794 | 5,6999 |
| E5 trapping forte, risoluzione debole | 0,08687 | 0,11300 | 6,2493 | 6,3763 |

Compilazione separata: **1,4574 s**. Tempo totale del blocco benchmark, compilazione inclusa: **28,2503 s**. Il tempo Python include costruzione della reference e calcolo degli errori, esclude il successivo salvataggio NPZ; il completo include processo, analisi e salvataggio. Somme dei cinque casi: solver 0,4375 s, Python 26,1597 s, completo 26,7861 s. L'avvio/import iniziale dell'interprete non è incluso nel cronometro interno del blocco.

[Tempi effettivi CSV](results/benchmark_4000h/timings.csv). Gli errori massimi normalizzati delle medie rispetto a expm allo stesso N sono rispettivamente **1,174e-6, 5,806e-7, 5,999e-7, 2,682e-3, 6,360e-7**. Sono errori di discretizzazione osservati sui campioni orari, non tolleranze imposte o fallimenti della regressione con il notebook.

In particolare E2 mostra che una storia finale apparentemente assestata non garantisce accuratezza del transitorio. Δt=225 s produce già un errore massimo di circa 0,27% sulla scala globale della storia; alla prima ora gli errori relativi istantanei sono maggiori. Non è stata eseguita la griglia principale dt=3600 s: non le si attribuiscono risultati misurati. Il suo comportamento non può essere dichiarato sufficiente sulla sola base della stabilità implicita o della precisione finale.

## 3. Perché l'estrapolazione ingenua dei 110 test è fuorviante

I benchmark renderebbero economico il solo kernel. Per i 50 test temporali richiesti, la somma dei passi per set è 124.000: proiezione di circa **1,70 s di solo solver** complessivo, più **262 s** di analisi Python se ogni reference oraria è ricostruita cinque volte; con cache esatta per caso/N/tempi la parte di costruzione potrebbe scendere a circa **52 s**, senza cambiare la soluzione.

Per i 60 test spaziali, **ipotizzando erroneamente dt=225 s per tutti**, la proiezione è circa **13,8 s di solver**, più **824 s** per analisi allo stesso N su osservazioni orarie, senza cache. High-N, controlli, I/O e grafici sono aggiuntivi. Questi numeri suggerirebbero circa 18 minuti di analisi ordinaria, ma **non soddisfano ancora la richiesta di errore temporale trascurabile rispetto allo spaziale**. Sono scenari controfattuali, non preventivi accettati della campagna.

È stato quindi eseguito il precontrollo E2 a N=320, dt=225 e 112,5 s, sempre fino a 4000 h: rispettivamente **64.000 e 128.000 passi**. Tempi misurati solver **0,61853 e 1,24846 s**, processo con I/O **0,72193 e 1,37048 s**. Si confrontano le osservazioni a 1,24,100,1000,4000 h con expm allo stesso N e con una reference ad alto N.

Reference N=640,1280,2560,5120: a N=5120 la variazione rispetto a N=2560 è circa **0,00172 volte** l'errore spaziale N=320, sotto il criterio dell'1%. È controllata su ogni popolazione attiva e sui tempi scelti. La media a lungo termine è anche confrontata con quella parabolica indipendente `s_c R²/(15D)`. Non è stato usato un valore high-N non convergente per stimare un troncamento artificialmente piccolo.

## 4. Risultato del controllo temporale/spaziale

Riutilizzato il criterio 4A: errore temporale ≤ **5%** di quello spaziale, per ogni popolazione attiva. Alla prima ora:

| Popolazione | Errore spaziale relativo istantaneo N=320 | Errore temporale relativo dt=225 s | Errore temporale relativo dt=112,5 s | Rapporto temporale/spaziale, dt=112,5 s |
|---|---:|---:|---:|---:|
| c | 3,651e-8 | 1,0469e-2 | 5,2794e-3 | 144.600 |
| md | 5,891e-8 | 1,5027e-2 | 7,6780e-3 | 130.339 |

I relativi sono riferiti al valore istantaneo della rispettiva popolazione analitica; il rapporto tra errori non dipende dalla normalizzazione. Bulk è esattamente inattivo in E2. A 1000/4000 h, invece, il rapporto è già dell'ordine di 1e-6 e passa: **controllare solo il finale nasconderebbe il problema della storia fisica**.

[CSV del precontrollo](results/cost_accuracy_gate_v2/accuracy_precheck.csv). Per soddisfare lo stesso criterio a tutte queste osservazioni con un dt uniforme sull'intera simulazione:

- usando l'ordine locale osservato dai due passi, p≈0,969 nel punto vincolante md, si stima **dt≈2,68e-5 s**, circa **5,38e11 passi**, **60,7 giorni** per il solo N=320 e **119,5 giorni** per i sei N di E2;
- usando invece l'ordine asintotico teorico **p=1**, l'osservabile vincolante è c e si stimano **dt≈3,89e-5 s**, **3,70e11 passi**, **41,8 giorni** per N=320 e **82,3 giorni** per i sei N di E2.

**Questi sono costi estrapolati, non tempi eseguiti né garanzie sul dt finale.** L'estrapolazione su molti ordini non è una calibrazione del passo; il comportamento a quei dt andrebbe verificato e l'arrotondamento potrebbe diventare rilevante. Il modello di costo scala i tempi del kernel misurati per numero di passi e N, escludendo I/O, analisi e gli altri nove set. Anche l'ipotesi p=1, senza l'amplificazione dell'ordine locale preasintotico, rende costosa la sola parte spaziale E2 per mesi. Non è necessario simulare centinaia di miliardi di passi per constatare questa incompatibilità pratica.

Il limite interno cautelativo del controllo è un'ora di solo solver e il driver 4A ha già un limite di sicurezza di cinque milioni di passi. L'ora è una scelta operativa dichiarata, non un budget fornito dall'utente. Il superamento stimato è di ordini di grandezza; anche ignorando quel limite, il costo previsto non è adeguato a lanciare automaticamente l'intera campagna. **Arresto prima dei 110 test**, come richiesto. [Decisione e stima locale](results/cost_accuracy_gate_v2/decision.json), [stime con p=1](results/diagnostic_figures/asymptotic_p1_cost.csv).

## 5. Reference nel caso gb=0 e precisione numerica

Con gb=0, sorgente e iniziali bulk nulle, `mb'=-bb mb` implica mb identicamente zero. La expm aumentata 4×4 originale può lasciare rumore in questa componente dell'ordine di 1e7 at/m³ rispetto a scale attive 1e23, circa precisione macchina. Non è un segnale di gas bulk fisico o un errore di produzione.

Il primo postprocessing del controllo si è interrotto dividendo per una differenza spaziale bulk esattamente nulla mentre interpretava quel rumore come errore temporale. Le due simulazioni C++ erano già concluse. Nel controllo v2 è stato risolto il sottospazio invariante c/md con expm aumentata 3×3: bulk nullo per algebra, **nessun clipping**, nessuna modifica alla reference 4A o al solver. È verificato l'accordo delle componenti attive fra le due formulazioni. Gli output C++ originali sono riutilizzati con SHA-256, non rieseguiti o alterati. Il [tentativo iniziale](results/cost_accuracy_gate/ATTEMPT_STATUS.md) resta documentato.

## 6. Grafici e limiti delle conclusioni

- [Tempi dei cinque benchmark](results/diagnostic_figures/benchmark_times.png), [PDF](results/diagnostic_figures/benchmark_times.pdf).
- [Evoluzione dell'errore delle medie](results/diagnostic_figures/benchmark_error_histories.png), [CSV per popolazione](results/diagnostic_figures/benchmark_errors_per_population.csv).
- [Rapporto degli errori nel tempo, assi logaritmici](results/diagnostic_figures/spatial_accuracy_gate.png).
- [Convergenza della reference E2](results/diagnostic_figures/reference_convergence.png).

Non vengono presentati grafici di convergenza dei 50/60 test come se fossero stati eseguiti. Il benchmark conserva l'errore modale L2 aggregato per caso; non costituisce un'analisi completa per ogni modo e popolazione della campagna richiesta. Non sono determinati ordini spaziali universali dai soli due precontrolli.

I set stiff hanno tempi rapidi molto inferiori a 225 s: ad esempio E5 a N=40 ha τ_rapido≈0,404 s; E3≈0,490 s. Le storie orarie non li risolvono. I benchmark brevi ravvicinati restano proposti, **non eseguiti**, perché il lavoro è stato arrestato al controllo richiesto. Nessuna conclusione di accuratezza dei primi secondi viene tratta dalla precisione a 4000 h.

Non è stata specificata una soglia assoluta di errore desiderato per le applicazioni: il 5% qui controlla la separazione fra errori, non definisce l'accuratezza ingegneristica finale. Per esempio, a un ipotetico target 1e-3 sulla scala della storia E2 fallirebbe già con N=40 e dt=225 s per il suo errore temporale 2,68e-3. Questo non sostituisce la scelta di una soglia e una verifica completa. La configurazione N=40, dt=1 h non può essere certificata per tutti i regimi dalla sola 4A o dai benchmark correnti.

## 7. Ripresa proposta e preservazione

La [proposta separata](PROPOSTA_RIPRESA.md) distingue: test temporali allo stesso N, verifica analitica dello spazio e decomposizione dell'errore, eventuale nuovo disegno temporale o estrapolazione come diagnostica distinta. Nessuna alternativa è stata applicata tacitamente e nessuna soglia, intervallo o fisica è stata cambiata per far passare i test. L'eventuale LHS resta successivo alla decisione e alla verification completata.

La validazione finale controlla i dieci set e la provenienza, le sette esecuzioni C++, la reference e l'invariante bulk, tutti i file preesistenti, il branch, HEAD, origin e l'intero diff staged. **Nessun commit, merge, push, cambio branch o riordino.** La FASE 4A e i documenti della tesi restano intatti. Il checkpoint conserva dati, script e istruzioni per riprendere senza rifare i benchmark o le due simulazioni di selezione del passo.
