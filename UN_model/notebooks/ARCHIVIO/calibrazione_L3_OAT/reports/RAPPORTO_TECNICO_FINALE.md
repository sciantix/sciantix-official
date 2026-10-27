# Continuazione L3 — calibrazione OAT del modello UN

## Esito e raccomandazione

**Raccomandazione: conservare L3 come candidato corrente.** La campagna individua un miglioramento del raggio fisicamente interpretabile con curve di swelling vicine alla baseline, ma non un miglioramento concorde dei target principali. Il problema degli ultimi punti ANP6 e della crescita 1600–1800 K resta aperto.

Il candidato alternativo scelto per la revisione è **B3_K085_rho093**, con K_d=3.485e5 bubble/m e rho_d=3.255e13 m^-2. Gli altri quattro parametri restano L3: f_n=4.2e-4, Dv_dislocation_scale=10, Dg_dislocation_scale=17, NGF_AREAL_0=1e13 m^-2. Il notebook originale non è stato sostituito o modificato.

![Confronto finale L3 e candidato](FINAL_L3_vs_candidate.png)

Sono stati eseguiti tre batch adattivi di 8, 6 e 6 candidati: **20 candidati, 2960 punti finali**, tutti sui quattro casi e sulla griglia originale. Le decisioni e le direzioni interrotte sono documentate nel [registro progressivo](registro_progressivo.md). Non sono stati usati Optuna, random search o un punteggio globale.

## Baseline riprodotta e dati utilizzati

I tre file consegnati sono conservati integralmente in `runs/baseline/originals`; gli hash SHA256 prima/dopo coincidono. Le celle eseguibili del notebook sono estratte testualmente e caricate senza patch al modello. La funzione `main()` originale è stata eseguita, ricreando CSV e suite completa dei grafici in `runs/baseline/RizkUN_L3_unificato`.

Le **148 righe e 139 colonne** del CSV coincidono con lo ZIP con rtol=1e-12, atol=0, trattando coerentemente i NaN. Il massimo scarto relativo finito è 8.04e-13, attribuibile alla rappresentazione/lettura del CSV. Parametri e fisica sono quelli eseguiti della cella 1; alcuni blocchi markdown storici del notebook descrivono impostazioni precedenti e non rappresentano la configurazione attiva. Anche i valori indicativi nel vecchio report differiscono leggermente dalla baseline corrente: per esempio AP3.4 a 1600 K è 2.707518%, coerente con lo ZIP, anziché circa 2.68%.

Configurazione invariata: 3 h, 160 modi, T=900–1800 K ogni 25 K; rho costante; capture e nucleation mass coupling ON; phi gas re-solution OFF; grain-face ON, Pastore 3x3/exact release; WS OFF; diffusività Xe `rizk2025_refit_plot`, vacancy `rizk2025_refit_full`, GB `rizk_legacy_1e6_Dv1`; D2_xe_scale=0. Tutti gli altri scale factor e le condizioni iniziali restano quelli del notebook. La configurazione completa è salvata per ogni run.

|Caso|Burnup [% FIMA]|Potenza [kW/m]|Fission rate [m^-3 s^-1]|
|---|---:|---:|---:|
|AP3.2|1.1|100|5.767844763153473e19|
|AP3.8|1.1|119|6.863735268152632e19|
|AP3.4|1.3|130|7.498198192099515e19|
|ANP6|3.2|125|7.760680190110210e19|

AP3.2 è sempre simulato e visualizzato, ma escluso dalla selezione. Lo swelling confrontato è `swelling_d_percent`, coerente con il target P2. Le osservazioni sono i dati incorporati nel notebook: 10 punti AP3.8, 9 AP3.4, 10 ANP6; per la microstruttura AP3.4, tutti i 9 punti Fig. 8 e i **28 punti corretti Fig. 7**. La vecchia tabella `src/un_data.py` non viene importata: il suo primo N_d è 5.22e19 a 1153 K, mentre il notebook contiene 1.67973237e19 a 1156.143345 K. Non è stata effettuata una nuova digitalizzazione indipendente del paper; la provenienza dei dati corretti resta il notebook fornito.

Le predizioni ai punti sperimentali sono interpolate linearmente dalla griglia originale. Il solo punto AP3.8 a 899 K usa il bordo 900 K, con flag esplicito in `residuals.csv`. Nessun punto del raggio viene eliminato oltre 1500 K: sei misure high-T fino a 1722.22 K entrano nelle metriche. La separazione low/high è ≤1500 e >1500 K. Non sono disponibili in questi input barre d'errore per attribuire significatività statistica ai piccoli scarti tra candidati.

## Cosa è stato appreso dai batch

Il batch 1 verifica le sensitività locali prima delle compensazioni: f_n ±10%, K_d ±15%, Dg_d ±10%, rho_d -10%, Dv_d +20%. K_d ridotto migliora il raggio ma aumenta lo swelling high-T; Dg ridotto frena le curve ma peggiora il raggio e gli ultimi punti ANP6. Aumentare Dg isolatamente recupera ANP6 e raggio, ma amplifica AP3.4 e l'estrapolazione. Dv ha un effetto misurabile nella banda fredda, quasi nullo high-T: la sua precedente insensibilità non è universale.

Il batch 2 testa K_d-Dg, K_d-f_n, K_d-rho e una compensazione distribuita. K_d -30% / Dg -12% riduce RMSE raggio high-T a 38.12 nm, ma lascia lo swelling quasi sovrapposto a L3 e non risolve ANP6. Il controllo Dv aggiuntivo cambia RMSE raggio high-T di soli 0.023 nm e non giustifica un terzo parametro. K_d -15% / f_n +25% recupera meno raggio con distanza maggiore; la direzione non viene estesa.

Il batch 3 affina le due famiglie promettenti e completa Dv -20% e N_gf,0 +20%. Il raffinamento Dg scambia ancora miglioramento ANP6/raggio contro peggioramento AP3.4 e crescita a 1800 K. La coppia K_d -15% / rho -7% raggiunge un raggio simile al punto K_d -20% / Dg -7.7% con distanza inferiore: viene scelta come alternativa conservativa per la contact sheet finale. La campagna si ferma dopo tre batch perché il fine tuning non cambia il compromesso fisico osservato; non si aggiungono prove prive di una nuova ipotesi locale.

La [mappa OAT aggiornata](OAT_sensitivities.md) contiene direzione e intensità, low/high, per swelling, R_d, N_d, matrix/bulk/dislocation gas, R_gf, N_gf e FGR. I [CSV per temperatura](OAT_response_by_temperature.csv) includono anche il controllo AP3.2. Le elasticità sono risposte locali, non leggi universali. Le grandi elasticità del FGR vicino allo zero devono essere lette insieme agli scarti assoluti in punti percentuali.

## Confronto quantitativo del candidato finale

RMSE swelling in punti percentuali; MAPE in %. La [tabella dei candidati migliori](best_candidates.md) riporta parametri e alternative; [tutte le metriche](all_metrics_long.csv) mantengono ogni osservabile, banda e caso separati.

|Target|L3 RMSE / MAPE|B3_K085_rho093 RMSE / MAPE|
|---|---:|---:|
|Swelling AP3.8|0.2263 / 13.57|0.2224 / 13.44|
|Swelling AP3.4|1.2672 / 37.66|1.2894 / 38.56|
|Swelling ANP6|0.3482 / 9.39|0.3571 / 11.25|
|R_d ≤1500 K [nm]|3.654 / 5.76|1.883 / 3.10|
|R_d >1500 K [nm]|49.863 / 34.36|42.433 / 28.90|

Il raggio migliora del **14.90% in RMSE high-T**, e del 48.46% low-T. Il miglioramento non avviene sacrificando la banda fredda. Lo swelling AP3.8 migliora leggermente, ma RMSE AP3.4 cresce dell'1.75% e ANP6 del 2.54%. Il MAPE ANP6 aumenta da 9.39% a 11.25%, quindi la sola stabilità dell'RMSE non basta a dichiarare equivalenti i fit.

|Caso, banda|L3 RMSE swelling|Candidato RMSE swelling|
|---|---:|---:|
|AP3.8 ≤1500 K|0.2696|0.2685|
|AP3.8 >1500 K|0.1725|0.1639|
|AP3.4 ≤1500 K|0.3247|0.3400|
|AP3.4 >1500 K|1.5349|1.5608|
|ANP6 ≤1500 K|0.0955|0.1250|
|ANP6 >1500 K|0.6188|0.6233|

Il punto intermedio K_d-Dg presenta un guadagno di raggio simile (42.65 nm high-T) ma D_cal=0.0420, contro 0.0316 del candidato scelto. Il candidato K_d -30% / Dg -12.5% spinge il raggio a 38.47 nm con D_cal=0.0675, ma peggiora maggiormente ANP6. Non si accetta automaticamente la maggiore calibrazione per qualche nm di recupero aggiuntivo.

Le sole modifiche K_d -15% e rho -7% sono entrambe lievi secondo le categorie del report precedente. Il loro prodotto riduce N_d iniziale del **20.95%**, da 1.435e19 a 1.1343675e19 m^-3: l'effetto microstrutturale combinato è quindi più rilevante dei due scarti singoli. D_cal è la RMS dei log10 dei sei rapporti, senza alcun uso come score del fit. L3 resta una baseline già calibrata, non il set nominale puro Rizk.

## N_d, gas partition, grain-face e FGR

N_d è controllato dopo la scelta basata su swelling/raggio, senza obiettivo diretto. Nel candidato RMSE diagnostico N_d passa da 7.89e18 a 5.72e18 m^-3; RMSE log10 da 0.6834 a 0.6085. Il MAPE high-T resta elevato, 610% contro 800% L3: il calo di N_d migliora la coerenza ma non riproduce il forte andamento decrescente delle osservazioni corrette.

A 1600 K AP3.4, N_d passa da 1.2830e19 a 1.0141e19 m^-3 e R_d da 79.57 a 86.10 nm, mentre S_d passa da 2.7075% a 2.7115%. È soprattutto una redistribuzione dello stesso volume su meno bolle. La relazione single-size è verificata numericamente; il miglioramento del raggio non costituisce una validazione indipendente della distribuzione delle dimensioni. Il raggio sperimentale è a sua volta ricavato indirettamente da grandezze microstrutturali.

Differenze della gas partition del candidato rispetto a L3, in punti percentuali del gas generato:

|Caso, T|Matrix|Bulk|Dislocation|Grain-face|FGR|
|---|---:|---:|---:|---:|---:|
|AP3.8, 1600 K|+0.00037|+2.22422|-2.25665|+0.03206|0|
|AP3.8, 1800 K|+0.00020|+3.33446|-3.44397|+0.10931|0|
|ANP6, 1600 K|+0.00005|+0.86070|-0.87153|+0.00014|+0.01063|
|ANP6, 1800 K|+0.00001|+1.76840|-1.80379|+0.00022|+0.03517|

Il volume dislocation quasi conservato non implica partition conservata: rho modifica anche i sink, e più gas resta nel bulk. Questa è una compensazione parametrica trasparente, non una nuova conferma sperimentale. Le frazioni non sono usate come dati di fit. Il FGR ANP6 a 1800 K passa da 4.0913% a 4.1264%. AP3.8 rimane a FGR nullo nel dominio; non viene interpretato come validazione integrale.

Per AP3.4 a 1800 K, R_gf passa da 277.43 a 280.02 nm e N_gf da 2.8144e12 a 2.7626e12 m^-2. Le curve bulk, grain-face, FGR e geometria sono mostrate nella [diagnostica aggiuntiva](../plots/B3_K085_rho093_additional_diagnostics.png); tutti gli scarti sono nel [CSV degli output](shortlist_all_output_changes.csv). La prova N_gf,0 +20% conferma il disaccoppiamento degli output intragranulari: non fornisce una leva per i target principali.

## ANP6 e discrepanze persistenti

A 1590 K il dato è 3.75%: L3 predice 2.9485%, il candidato 2.9467%. L'errore dell'ultimo punto è sostanzialmente immutato. Tra 1600 e 1800 K L3 cresce da 3.0391% a 12.1850% (+9.1459 punti); il candidato da 3.0381% a 12.2418% (+9.2037 punti). Il forte aumento non viene corretto. **T>1590 K resta estrapolazione**, senza target inventati; la regolarità numerica e il rispetto dei limiti geometrici non ne dimostrano l'accuratezza fisica.

AP3.4 rimane basso intorno a 1535–1641 K e alto dopo il calo delle misure a circa 1662 K. Le variazioni locali principalmente riscalano curve crescenti e non ricreano quel comportamento. AP3.2 resta un controllo discrepante e vicino ad AP3.8 nel modello; non è stato usato per decidere candidati o modificare i fission rate.

Il [controllo single-size sui dati](experimental_single_size_consistency.csv), accoppiando punti S e R distanti meno di 5 K, mostra un N_d implicito vicino a quello di L3 a 1220 K ma soltanto circa il 12.45% di L3 a 1722 K. Non è un target aggiunto: descrive la riduzione dipendente da T necessaria per conciliare i due osservabili in una popolazione single-size. Una riduzione costante moderata di K_d o rho recupera parte del raggio, ma non quella variazione di forma. La campagna supporta un limite locale della parametrizzazione; non prova un'impossibilità globale e non identifica univocamente una closure da modificare. Nessuna nuova legge è stata introdotta.

## Verifica numerica e riproducibilità

La [verifica conclusiva](verification.json) controlla integrità degli originali e delle celle estratte, impostazioni fisiche/numeriche fisse, parametri variati soltanto nei sei campi autorizzati, quattro casi e 37 temperature per ogni candidato, fission rate originali e completezza degli artefatti.

Su tutte le storie dei 20 candidati: massimo residuo relativo del bilancio del gas **4.52e-15**, nessuno scambio negativo o non finito prima del clipping rilevato dal monitor, nessuna frazione di capture grezza prossima a 1, nessuna violazione dei criteri single-size del notebook, nessun overflow geometrico rilevato, nessun incremento negativo del rilascio. La relazione S_d=(4π/3)N_d R_d^3 è soddisfatta entro arrotondamento. Per il candidato finale il residuo massimo lungo le storie è 3.69e-15 e psi_d massimo finale circa 0.497.

Per L3 sono state rieseguite 16 storie (quattro casi × 900/1200/1600/1800 K), ottenendo gli stessi output finali del CSV originale. Per ciascun candidato sono archiviate le diagnostiche dell'intera storia su tutti i 148 punti e le storie complete NPZ a 1200/1600/1800 K.

Il bilancio al bordo nel solver deriva dal complemento del gas trattenuto: la sua chiusura verifica il bookkeeping, non indipendentemente l'accuratezza del flusso. Questi controlli non sono un nuovo studio di convergenza; la discretizzazione richiesta è rimasta fissa, e l'audit preesistente 3 h/160 modi su AP3.4 non certifica ogni caso e candidato.

Per rieseguire e consultare gli artefatti, vedere il [README](../README.md). I risultati consentono di esaminare B3_K085_rho093 come alternativa microstrutturale, ma l'assenza di un miglioramento dello swelling AP3.4/ANP6 e la persistenza dell'estrapolazione rapida motivano **il mantenimento di L3** in questa fase.
