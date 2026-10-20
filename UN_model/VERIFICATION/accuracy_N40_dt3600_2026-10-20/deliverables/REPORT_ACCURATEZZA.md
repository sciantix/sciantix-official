# Accuratezza della configurazione N=40, Δt=3600 s

Analizzati i 160 casi congelati (128 LHS e 32 stati UN), con sorgente, iniziali, coefficienti costanti e durata 4000 h invariati. La campagna estesa non è stata ripetuta. Per il temporale sono usate tutte le 4001 osservazioni orarie; spazio e totale usano inizialmente gli undici tempi verificati e, nei nove casi critici, ore intermedie aggiuntive.

## Massimi relativi totali osservati

| Gruppo | Popolazione | Caso | Errore relativo istantaneo | Tempo [h] | Griglia |
|---|---|---|---:|---:|---|
| LHS | c | LHS_041 | 2.496% | 8 | common_11_verified |
| LHS | mb | LHS_041 | 3.1119% | 1 | common_11_verified |
| LHS | md | LHS_055 | 24.499% | 1 | common_11_verified |
| UN_history | c | UN_AP3p2_900K_S3 | 21.616% | 1 | common_11_verified |
| UN_history | mb | UN_AP3p2_900K_S3 | 8.6121% | 1 | common_11_verified |
| UN_history | md | UN_AP3p2_900K_S3 | 10.541% | 1 | common_11_verified |

Questi sono massimi discreti osservati, non massimi continui rigorosi. Le griglie sono più dense solo nei casi presele­zionati; i confronti statistici uniformi sui 160 casi restano separati in worst_cases_common_sampling.csv e group_statistics_common_sampling.csv. Nessuna soglia automatica dell'1% o del 3% è stata applicata.

## Separazione degli errori

La decomposizione con segno è `Cpp − highN = (Cpp − exact_N) + (exact_N − highN)`. Il temporale orario usa la cache expm N40 validata; nei confronti condivisi si usa il prefisso N40 della stessa reference diretta high-N, come nella 4B. La differenza di formulazione, di solo roundoff, è riportata esplicitamente come reference_form_gap e non è interpretata come nuova fisica. Tutte le reference originali restano immutate. L'errore spaziale confronta esclusivamente soluzioni analitiche. La dominanza è valutata sulle componenti assolute nelle stesse unità, prima della normalizzazione.

Le percentuali istantanee hanno per denominatore il riferimento della popolazione allo stesso tempo; sono diverse dagli errori sulla scala massima di storia. Percentuali non definite sono vuote e segnalate, mentre gli assoluti rimangono disponibili. Le scale e la stima di sensibilità numerica sono in normalization_scales.csv. Le otto popolazioni bulk strutturalmente inattive non ricevono percentuali fittizie.

## Raffinamenti mirati: esempi sul totale

| Caso | Pop. | N | Δt [s] | Max relativo osservato | Tempo [h] | Fattore miglioramento |
|---|---|---:|---:|---:|---:|---:|
| LHS_055 | md | 40 | 3600.0 | 24.499% | 1 | 1 |
| LHS_055 | md | 40 | 1800.0 | 11.576% | 1 | 2.116 |
| LHS_055 | md | 40 | 900.0 | 5.0996% | 1 | 4.804 |
| LHS_055 | md | 80 | 3600.0 | 25.435% | 1 | 0.9632 |
| LHS_055 | md | 160 | 3600.0 | 25.899% | 1 | 0.9459 |
| UN_AP3p2_900K_S3 | c | 40 | 3600.0 | 21.616% | 1 | 1 |
| UN_AP3p2_900K_S3 | c | 40 | 1800.0 | 12.353% | 1 | 1.75 |
| UN_AP3p2_900K_S3 | c | 40 | 900.0 | 6.7538% | 1 | 3.201 |
| UN_AP3p2_900K_S3 | c | 80 | 3600.0 | 21.237% | 1 | 1.018 |
| UN_AP3p2_900K_S3 | c | 160 | 3600.0 | 21.142% | 1 | 1.022 |

Tutte le configurazioni dello stesso caso sono confrontate sulla stessa griglia high-N arricchita; i temporali sono valutati separatamente sulle 4001 ore. Il fattore è errore massimo candidato / errore massimo raffinato sulla stessa griglia e con lo stesso denominatore. Un valore inferiore a uno segnala un totale maggiore, che può derivare dalla rimozione di una compensazione fra errore temporale e spaziale di segno opposto. Non è un motivo per alterare dati o fisica. Aumentare N agisce sul troncamento; dimezzare Δt sul Backward Euler. I residui a N160 sono conservati senza estendere i test oltre N160 e senza dichiarare sufficienza in assenza di una soglia concordata.

## Coefficienti, regimi e campionamento

worst_cases_common_sampling.csv riporta i coefficienti effettivi dei casi peggiori, distinti per gruppo e popolazione. coefficient_regimes.csv aggiunge scale rapide, intermedie e lente, rapporti di trapping/risoluzione e diffusione. coefficient_associations.csv fornisce correlazioni di rango descrittive; i 32 stati UN sono correlati lungo le storie e non permettono attribuzioni causali indipendenti ai singoli coefficienti. Le associazioni non vengono trasformate in una scelta definitiva di N o Δt.

LHS_055/md evidenzia un errore temporale positivo parzialmente compensato dal troncamento negativo: più modi possono quindi aumentare l'errore totale mentre ne diminuiscono la componente spaziale. I casi UN critici mostrano che una percentuale iniziale elevata può essere poco visibile nella normalizzazione sulla concentrazione massima a 4000 h. Il tempo rapido spettrale da solo non descrive tutte le popolazioni: sono presenti anche induzioni e scale intermedie dell'exchange.

## Transitori rapidi

Sono eseguite prove brevi su tre casi esistenti: il peggiore relativo temporale del gruppo veloce LHS, il peggiore del gruppo veloce UN e il caso con τ più rapido. Si usa T=20τ con dt=τ/10,/20,/40 e una integrazione risolta separata fino a 3600 s con dt=τ/20. Le prove brevi e quelle di 4000 h hanno scale di normalizzazione distinte. La selezione operativa veloce è 3600/τ≥20, senza usarla come soglia di accettazione dell'accuratezza.

Non si attribuisce alcun valore candidato dentro il primo timestep: il candidato ha output a 0 e 3600 s, non nei tempi interni delle prove brevi. Le concentrazioni risolte e quelle analitiche mostrano l'evoluzione non campionata dal candidato, ma non definiscono un suo errore continuo. Il confronto quantitativo del primo passo è fatto a 3600 s per entrambe le soluzioni. Gli scarti di campionamento float delle prove brevi sono controllati entro 2e-12 della loro durata, come nel protocollo 4B.

## Riproducibilità e anomalie operative

Gli output N40 a 3600/1800/900 s sono riutilizzati; sono nuovi soltanto N80/N160 a 3600 s sui nove casi e le dodici prove rapide/di prima ora. I numeri di modi non sono estesi oltre 160 nei confronti C++. Le reference high-N aggiuntive sono calcolate solo ai tempi nuovi e controllate con i due criteri 4B sui raddoppi dei prefissi modali. Non sono rigenerate tutte le reference high-N della campagna precedente.

Il primo benchmark mirato si è interrotto nell'esportazione dei metadati CSV, perché i record nuovi e quelli riutilizzati avevano colonne diverse. È stata corretta l'unione delle colonne e riutilizzato ogni output numerico già completo, senza ripetere C++. I tempi originali delle componenti restano conservati. Nessun problema scientifico ha richiesto modifiche di equazioni o coefficienti.

I CSV temporal_errors_hourly.csv.gz conservano l'analisi completa a ogni ora; per lo ZIP la stessa informazione numerica è resa compatta in temporal_hourly_lossless.csv con riferimenti ed errori con segno delle tre popolazioni e maschera dei relativi definiti. I massimi, le scale e le formule permettono di ricostruire esattamente assoluti, relativi e normalizzati senza i binari. La compattezza non elimina osservazioni orarie. I nuovi grezzi e tutte le reference restano separati. Nessun grafico o conclusione definitiva sulla scelta N/dt è prodotto.

## Coefficienti dei regimi critici osservati

- LHS_055/md: gb=0,50062 s⁻¹, gd=1,05562e-4 s⁻¹, bb=2,00871e-4 s⁻¹ e bd=1,26597e-6 s⁻¹. La risoluzione bulk è molto più rapida di quella dislocation e bb·3600≈0,723. Il modo più rapido ha τ≈1,993 s, ma la popolazione md presenta anche una fase di induzione alimentata dallo scambio con il reservoir bulk. Il dato osservato non può essere spiegato soltanto dal tempo spettrale più rapido.
- UN_AP3p2_900K_S3/c: D=6,85956e-19 m²/s, gb=5,48459e-8 s⁻¹, gd=7,47934e-4 s⁻¹; τ_rapido(N40)≈949 s e 3600/τ≈3,79. L'errore relativo iniziale è elevato anche senza uno stiff estremo.
- Il maggiore troncamento relativo LHS nella griglia comune è in LHS_042, con D=9,82560e-20 m²/s e gb=1,17823 s⁻¹: trapping/diffusione del modo 1≈4,37e7. Il maggiore troncamento UN è in ANP6/1600 K nell'ultimo stato selezionato (UN_ANP6_1600K_S8). I relativi massimi sulle popolazioni raggiungono rispettivamente circa 1,4867% e 1,1930%, osservati a 1 h; sono misure, non soglie di accettazione.

Fra i 128 LHS, la correlazione di rango fra massimo spaziale relativo di c e rapporto trapping/diffusione è ρ≈0,9983, mentre con D è ρ≈−0,9330. Per il massimo temporale relativo md, l'associazione con bb è moderata (ρ≈0,5371). Nei 32 stati UN, l'associazione del massimo temporale relativo c con τ_rapido(N40) è ρ≈0,9963; i coefficienti degli stati sono però correlati fra loro. Queste associazioni descrivono il campione e non costituiscono attribuzioni causali o extrapolazioni fuori dai casi congelati.

## Validazione e tempi misurati

Validazione PASS: 640,160 righe orarie controllate senza perdita, 1,920,480 osservazioni popolazione/ora, 160 reference high-N originali e nove griglie arricchite. L'identità con segno ha residuo massimo normalizzato 0; 54 controlli indipendenti della potenza affine Backward Euler sui nuovi output N80/N160 hanno scarto modale massimo normalizzato 4.77e-13. Lo scarto di clock massimo delle prove rapide è 2.32e-11 s, con effetto sul riferimento ≤1.82e-14 sulla scala di popolazione.

Compilazione 1.196 s; analisi candidato 56.746 s; kernel dei 30 nuovi run C++ 0.578 s; valutazioni high-N aggiunte 63.414 s; cache orarie N80/N160 4.421 s. Queste misure sono componenti, non somme dei tempi originali riutilizzati. Tempo trascorso prima del confezionamento 19.73 min (include preparazione, audit e report; limite 90 min).

Conservati byte per byte 19,589 file preesistenti, inclusi risultati ignorati da Git; branch, HEAD e index iniziali invariati. Dettagli in validation.json.
