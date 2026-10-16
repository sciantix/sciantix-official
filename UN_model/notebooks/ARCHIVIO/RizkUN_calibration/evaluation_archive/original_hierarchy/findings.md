# RizkUN — risultati della sensitivity OAT controllata

Eseguite **988/988 simulazioni con esito numerico positivo**, esattamente 13 configurazioni × 4 pin × 19 temperature. Originale `finalRizkUN.ipynb` invariato, verificato tramite SHA-256. La baseline riproduce gli output preesistenti su 76 punti e 126 colonne numeriche (tolleranza 1e-12; differenze osservate nulle). I quattro fission rate restano quelli originali. DT_H=1 h, N_MODES=40; T=900–1800 K, passo 50 K. Nessuna ottimizzazione, combinazione o simulazione aggiuntiva alle temperature sperimentali.

La verifica di `source_changes.diff` ripristina esattamente il core originale rimuovendo soltanto il campo `Candidate.ngf0_factor`, il campo `UNParameters.ngf_areal_0` e la lettura del valore passato nell’inizializzazione grain-face. Nel runner sono aggiunti il passaggio `NGF_AREAL_0*cand.ngf0_factor` e le diagnostiche di stato finale, senza variazione delle equazioni. Tutti gli altri parametri, switches, diffusività, re-solution, coalescenza, capture, geometria e superficie coincidono con la sorgente.

Lo **swelling P2/dislocation** resta la priorità, valutato separatamente sui quattro pin. RMSE espressi in punti percentuali di volume. NRMSE per pin = RMSE / RMS dei dati sperimentali di quel pin; la media fra quattro pin assegna a ciascuno lo stesso peso. Questa è una descrizione delle 13 prove fisse, non un obiettivo di ottimizzazione. `R_d` è il confronto sperimentale secondario su AP3.4, 1.3% FIMA. Nessun punteggio include `N_d`, pressione o benchmark Rizk.

| Candidato | Swelling NRMSE media pin | AP3.2 RMSE pp | AP3.8 RMSE pp | AP3.4 RMSE pp | ANP6 RMSE pp | R_d AP3.4 RMSE nm |
| --- | --- | --- | --- | --- | --- | --- |
| baseline | 0.3836 | 0.453 | 0.9886 | 1.078 | 1.072 | 43.6 |
| f_n_low | 2.809 | 5.992 | 4.488 | 8.729 | 6.452 | 44.71 |
| f_n_high | 0.7477 | 1.271 | 1.854 | 2.003 | 1.761 | 65.37 |
| K_d_low | 0.3685 | 1.067 | 0.2691 | 1.593 | 0.4234 | 21.87 |
| K_d_high | 0.6511 | 1.036 | 1.63 | 1.738 | 1.611 | 78.25 |
| rho_d_low | 0.8203 | 1.419 | 1.998 | 2.186 | 1.947 | 50.5 |
| rho_d_high | 0.6109 | 1.839 | 0.7699 | 2.357 | 0.5463 | 35.97 |
| Dv_dislocation_scale_low | 0.4159 | 0.434 | 1.092 | 1.156 | 1.224 | 44.48 |
| Dv_dislocation_scale_high | 0.3805 | 0.476 | 0.9705 | 1.062 | 1.046 | 43.55 |
| Dg_dislocation_scale_low | 0.9835 | 1.802 | 2.346 | 2.63 | 2.25 | 97.42 |
| Dg_dislocation_scale_high | 1.234 | 3.292 | 1.927 | 4.605 | 1.402 | 11.34 |
| N_gf0_factor_low | 0.3836 | 0.453 | 0.9886 | 1.078 | 1.072 | 43.6 |
| N_gf0_factor_high | 0.3836 | 0.453 | 0.9886 | 1.078 | 1.072 | 43.6 |

`K_d=1e5` porta l’RMSE di R_d da **43.60 a 21.87 nm** e migliora lo swelling AP3.8/ANP6; peggiora però AP3.2 e AP3.4, soprattutto dove la curva cresce troppo ad alta T. La piccola riduzione della NRMSE media (0.3836 → 0.3685) non dimostra un miglioramento comune ai quattro pin. A 1600 K la baseline dà 1.642%, 1.602%, 1.611%, 1.801% per AP3.2, AP3.8, AP3.4, ANP6; gli ancoraggi sperimentali supplementari sono 1.64%, 2.90%, 3.51%, 3.72% a burnup prossimi ai nominali. La baseline è già vicina ad AP3.2, mentre gli altri tre pin richiedono più swelling: è un trade-off centrale del modello congelato.

`Dg_dislocation_scale=30` porta l’RMSE di R_d a **11.34 nm**, ma la NRMSE media swelling sale a **1.2337**. Quindi il solo accordo del raggio non è una ragione per selezionare questo estremo. `f_n=1e-7` trasferisce molto gas dal bulk alle dislocazioni e sovrastima pesantemente lo swelling; `f_n=1e-2` sopprime troppo swelling e R_d. `rho_d=6e13` migliora alcuni pin e peggiora altri; la risposta di R_d cambia con T e può invertire direzione fra i tre livelli. La variazione di Dv sulle dislocazioni ha un effetto assai più debole sulla gas partition; da baseline 10 a 30 dà benefici piccoli.

`N_d` resta una diagnostica separata, con RMSE dei residui log10 sotto/uguale a 1600 K e sopra 1600 K. Alzare K_d migliora N_d a bassa T (0.471 → 0.300 dex), ma peggiora raggio e swelling. Abbassarlo peggiora N_d a bassa T (0.471 → 0.912 dex), pur migliorando R_d. Questi conflitti e la dispersione ad alta T non vengono risolti forzando un accordo simultaneo di swelling, R e N.

La [tabella delle direzioni](direction_table.md) e il [CSV dettagliato](direction_table.csv) riportano tutte le osservabili per parametro, pin e tre intervalli di temperatura: 900–1800, 900–1600 e 1650–1800 K. Il CSV contiene medie low/baseline/high, differenze assolute, variazioni relative alla media baseline, frazioni di T con aumento/diminuzione e inversioni fra livelli. Le frecce descrivono low → high; non stimano una derivata locale. Una variazione relativa non definita quando la baseline è zero viene sostituita nel Markdown dalla differenza assoluta.

- Aumentare f_n: swelling e R_d diminuiscono, N_d aumenta; più gas nel bulk, meno gas in matrice/dislocazioni; R_gf diminuisce, N_gf aumenta, FGR diminuisce dove attiva.
- Aumentare K_d: R_d diminuisce, N_d aumenta; più gas bulk, meno gas dislocazioni. Lo swelling ha inversioni a bassa T e generalmente diminuisce nel resto della griglia. R_gf/FGR aumentano debolmente, N_gf diminuisce debolmente.
- Aumentare rho_d: swelling/gas dislocazioni aumentano, gas bulk diminuisce; R_d ha risposta mista; N_d cresce quasi ovunque. R_gf/FGR diminuiscono, N_gf aumenta.
- Aumentare Dv_dislocation_scale: swelling/R_d aumentano, N_d diminuisce debolmente; gas partition, FGR e grain-face cambiano poco, con alcune inversioni minori.
- Aumentare Dg_dislocation_scale: forte aumento di swelling/R_d/gas dislocazioni, meno gas bulk e N_d; R_gf/FGR diminuiscono, N_gf aumenta. Alcuni piccoli cambiamenti di gas matrice hanno segno diverso con T.
- Aumentare N_gf0_factor: P2 swelling, R_d, N_d e gas matrice/bulk/dislocazioni sono esattamente invariati; R_gf diminuisce, N_gf aumenta, FGR aumenta dove attiva e il gas grain-face trattenuto diminuisce.

Gas partition, FGR, R_gf e N_gf sono salvati per **ogni prova e ogni pin**, insieme alle pressioni. Il confronto numerico con le curve digitalizzate Rizk è in [model_benchmark_summary.csv](model_benchmark_summary.csv) e [comparison_points.csv](comparison_points.csv). Le curve Rizk 2025 sono **benchmark di modello**, non esperimenti; i loro scarti non entrano nelle metriche di priorità sperimentale. Le curve gas partition disponibili sono a 1.1 e 3.2% FIMA, confrontate a 37 temperature digitalizzate nel dominio mediante interpolazione della griglia 50 K. AP3.2/AP3.8 condividono il benchmark 1.1%, senza provenienza pin-specific verificata; non si modifica il fission rate per riprodurlo. Nessun benchmark gas partition a 1.3% FIMA è incorporato. Il CSV Fig.7/8 per R_gf/N_gf citato da `_16NgfOnly_plots_with_Rizk_intergranular.ipynb` non è presente nel workspace: i relativi confronti numerici sono marcati non disponibili e non sono stati inventati dati.

Il ruolo indipendente di N_gf0 è confermato numericamente: passando da fattore 0.1 a 10, R_gf medio sui 76 punti passa da circa **443.6 a 67.6 nm**, N_gf volumetrico medio da **2.98e17 a 1.87e19 m^-3**, e FGR media da **0.724 a 1.079%**. Il confronto FGR Rizk migliora sui pin 1.1% alzando N_gf0, ma peggiora su ANP6 (RMSE baseline 2.439 pp → 2.770 pp); abbassarlo migliora ANP6 (2.064 pp) e peggiora i pin 1.1%. Anche questa è una compensazione fra casi, non una calibrazione sperimentale del grain-face.

L’ordinamento **R_gf > R_d > R_b** è violato persistentemente già nella baseline: AP3.2/AP3.8 a 1000–1150 K e AP3.4 a 1050–1150 K; nessuna violazione di tale ordine su ANP6. L’ordinamento **N_gf < N_d < N_b** è rispettato dalla baseline su tutta la griglia. Per il confronto N_gf viene convertito in densità volumetrica; la densità areale rimane salvata separatamente. Persistente significa almeno 3 temperature consecutive (span ≥100 K), allo stato finale del burnup nominale. Non è una verifica di tutte le storie temporali. `K_d=1e5`, `rho_d=1e13` e `N_gf0_factor=10` introducono violazioni persistenti dell’ordine delle densità in tutti e quattro i pin. Il fattore grain-face 10 viola inoltre l’ordine dei raggi lungo tutta la griglia AP3.2/AP3.8/AP3.4. Intervalli e componenti violate sono in [ordering_diagnostics.csv](ordering_diagnostics.csv) e nel CSV completo.

I controlli numerici finali non rilevano valori non finiti/negativi, estinzioni delle popolazioni o perdita del bilancio di gas (scarto massimo **5.68e-14 pp**). Questo **non certifica validità fisica**. Le sovrapressioni grain-face sono molto elevate già nella baseline: p_gf massimo circa **8.87e11 Pa**, p_gf/p_eq fino a **2.47e4**; con N_gf0_factor=0.1 si raggiungono **3.25e12 Pa** e **1.89e5**. Sono anomalie fisiche potenziali da verificare nel campo di validità della formulazione congelata, pur senza crash. Non si altera la pressione o il solver per migliorare un fit. [pressure_diagnostics.csv](pressure_diagnostics.csv) contiene estremi e intervalli di sovrapressione; la soglia descrittiva p/p_eq>1000 non è una regola di accettazione fisica o un obiettivo di fit. Le pressioni assolute, di equilibrio e i rapporti sono anche nei risultati e nei quattro grafici dedicati.

Le temperature sperimentali sono interpolate linearmente sulla griglia prescritta, senza extrapolazione e senza altri run. Il punto AP3.8 a 899 K è escluso perché fuori dominio. I quattro ancoraggi swelling a 1600 K sono salvati come confronto supplementare al burnup nominale più vicino, fuori dal punteggio primario per evitare doppio conteggio. Mancano incertezze sperimentali utilizzabili per una likelihood pesata: le metriche sono descrittive. La griglia 50 K può smussare l’onset FGR; non sono state cambiate le impostazioni numeriche per risolverlo.

**Proposte per il prossimo round: tutte non eseguite e non approvate.** Ogni parametro non elencato rimane alla baseline; tutti i vincoli fisici e numerici rimangono validi. Sono ipotesi da testare: l’OAT non misura le interazioni e non giustifica una previsione quantitativa interpolata del fit.

C1. **K_d=200000, Dv_dislocation_scale=30**. Ridurre moderatamente K_d per aumentare R_d e swelling rispetto alla baseline, con un piccolo contributo della mobilità vacanze sulle dislocazioni. Evitare la sovracrescita ad alta T osservata con K_d=1e5. Possibile peggioramento AP3.2; il contributo di Dv da 10 a 30 è debole. Non è una previsione di miglioramento su tutti i pin.

C2. **K_d=100000, f_n=0.0008**. Conservare parte del guadagno di R_d visto a K_d basso; aumentare f_n per riportare gas al bulk e contenere lo swelling eccessivo di AP3.2/AP3.4. f_n può ridurre anche il raggio e lo swelling utile ad AP3.8/ANP6. N_d resta diagnostica, senza tentare di correggerlo con un peso artificiale.

C3. **K_d=200000, rho_d=4e+13**. Aumentare il trasferimento di gas alle dislocazioni con rho_d, e ridurre K_d per contenere N_d e favorire R_d. N_d iniziale=8e18 m^-3, vicino a 9e18 della baseline. R_d ha risposta mista/non monotona a rho_d: verificare direttamente tutti i pin. Possibile eccesso di swelling AP3.2/AP3.4.

C4. **Dg_dislocation_scale=18, f_n=0.0008**. Aumentare moderatamente il gas catturato sulle dislocazioni e R_d tramite Dg_dislocation_scale; compensare con più nucleazione bulk per limitare swelling e redistribuzione estrema del gas. Interazione competitiva non misurata dall’OAT. FGR e gas partition possono peggiorare in modo diverso per pin.

C5. **K_d=200000, N_gf0_factor=0.5**. Migliorare moderatamente R_d tramite K_d e, separatamente, aumentare R_gf/ridurre N_gf per favorire gli ordinamenti. Il fattore grain-face non cambia P2 nel presente modello. Una riduzione di N_gf0 diminuisce FGR e può migliorare il benchmark ANP6 ma peggiorare AP3.2/AP3.8; aumenta le già elevate sovrapressioni grain-face. Mancano le curve numeriche R_gf/N_gf per convalidare una scelta di fit.

I valori completi sono in [next_round_proposals.csv](next_round_proposals.csv). Nessuna di queste proposte è stata aggiunta al design OAT o eseguita. Lo screening si ferma qui, in attesa dell’approvazione di un eventuale round successivo.

Gli artefatti principali sono [all_OAT_runs.csv](all_OAT_runs.csv), [candidate_summary.csv](candidate_summary.csv), [direction_table.csv](direction_table.csv), [direction_table.md](direction_table.md). La cartella `candidates/` contiene i 13 CSV individuali e `plots/` contiene 24 figure OAT con dieci pannelli più quattro figure pressione. `manifest.json`, `candidate_design.csv`, `source_changes.diff`, `baseline_reproduction_check.csv` e `validation.json` rendono controllabili provenienza e vincoli. `checkpoints/` conserva 52 risultati per candidato/pin, riutilizzabili soltanto con la stessa firma di configurazione.
