# Stato attuale: refinement monodimensionale di K_d

Completati soltanto i tre valori autorizzati K_d=1.5e5, 2e5, 2.5e5, mantenendo tutti gli altri parametri alla baseline: **228 nuove simulazioni**, senza errori numerici. Baseline e K_d=1e5 riutilizzati dai risultati OAT. Nessuna combinazione a due parametri proposta o eseguita; le precedenti proposte non approvate sono archiviate e sospese fino alla lettura del refinement.

Confronto attuale dei cinque valori: [Kd_refinement/comparison.md](Kd_refinement/comparison.md) e [CSV](Kd_refinement/comparison_summary.csv). Nuovi risultati: [all_Kd_refinement_runs.csv](Kd_refinement/all_Kd_refinement_runs.csv). Swelling per ciascun pin, R_d ≤1600 K e coerenza gas sono separati; R_d high-T/N_d sono solo diagnostiche. Pressioni e ordinamenti restano guardrail.

K_d=2e5 migliora lo swelling rispetto alla baseline in tutti e quattro i pin, porta R_d low-T RMSE da 24.66 a 14.65 nm e mantiene la gas partition vicina alla baseline (RMS core +5.03%). K_d=1.5e5 ha raggio low-T e media swelling migliori, ma peggiora AP3.2 e leggermente AP3.4. K_d=2.5e5 è più conservativo su gas e AP3.2. Questi trade-off sono esposti senza uno score composto.

La precedente valutazione dei 13 candidati, sotto, rimane documentazione OAT. I CSV fisici originali e gli score dei due controlli sono conservati. Il notebook resta in modalità di consultazione dei risultati salvati: Run All non avvia il solver.

# RizkUN — rivalutazione dei 13 candidati dai soli risultati OAT salvati

**Nuove simulazioni: 0. Nuove combinazioni eseguite: 0.** Tutti i valori previsti derivano dai CSV già presenti: nessun solver è chiamato e nessuna interpolazione o nuova predizione di modello è introdotta. Le metriche sono ricalcolate dai residui sperimentali e benchmark già salvati. Gli output fisici delle 988 prove, checkpoint, grafici, design e manifest restano immutati, verificati tramite SHA-256. La valutazione precedente è conservata in `evaluation_archive/original_hierarchy/`.

Swelling: stesso score precedente, media delle NRMSE separate sui quattro pin. R_d secondario forte: RMSE su **5 punti sperimentali AP3.4 con T ≤1600 K**. R_d high-T: **4 punti**, sola diagnostica, peso zero nella selezione. N_d low/high-T: diagnostiche qualitative con RMSE log10 e bias separati, senza peso di selezione. La sottostima high-T di R_d e la sovrastima high-T di N_d sono trattate come comportamento della formulazione accettabile secondo il criterio interpretativo fornito dall’utente per la Fig.4.7 Matthews 2025; non si tenta di eliminarle.

Gas partition: RMS degli scarti dei **tre compartimenti matrice/bulk/dislocazioni** come asse separato di coerenza, senza combinazione con gli errori sperimentali. RMS delle cinque frazioni completa, canale grain-face e FGR sono riportati separatamente come diagnostiche e non entrano nel fronte di selezione. Sono 333 confronti core per candidato (3 pin × 3 canali × 37 temperature già interpolate), e 555 nella diagnostica completa a cinque canali. Stesse unità (pp), stesso numero di punti per canale/pin, nessun peso di fit. Il CSV riporta anche RMS completo a cinque frazioni, tutti i canali separati e i rapporti alla baseline. Questi benchmark sono di modello, non dati sperimentali. Le curve 1.1% FIMA sono condivise da AP3.2/AP3.8; non sono disponibili a 1.3% FIMA. Nessuna soglia arbitraria di accettazione gas viene applicata: i peggioramenti sono esposti numericamente.

Pareto esperimenti: minimizzazione delle sole colonne swelling e R_d ≤1600 K. Pareto con gas: le stesse due colonne più RMS dei tre compartimenti matrice/bulk/dislocazioni, con dominanza componente per componente e nessuna somma pesata. Nessun ordinamento totale è imposto. Un candidato è dominato se un altro non peggiora nessuna colonna e ne migliora almeno una (tolleranza relativa 1e-10). **N_d, R_d high-T, pressioni e altri guardrail non entrano nei fronti né nei ranghi.** Un punto sul fronte può avere swelling o gas partition inaccettabilmente peggiori: essere non dominato non significa essere raccomandato.

| Candidato | Swelling NRMSE | R_d ≤1600 K RMSE nm | Gas matrice/bulk/disl RMS pp | Gas core / baseline | R_d >1600 K diagnostica nm | N_d ≤1600 K diagnostica dex | N_d >1600 K diagnostica dex | Pareto esperimenti | Pareto con gas |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| baseline | 0.3836 | 24.66 | 3.077 | 1 | 59.31 | 0.471 | 0.5598 | False | True |
| f_n_low | 2.809 | 27.91 | 53.24 | 17.3 | 59.36 | 0.5092 | 0.4394 | False | False |
| f_n_high | 0.7477 | 39.99 | 7.832 | 2.546 | 87.27 | 0.4631 | 0.5808 | False | False |
| K_d_low | 0.3685 | 21.89 | 3.492 | 1.135 | 21.84 | 0.9123 | 0.5173 | True | True |
| K_d_high | 0.6511 | 50.85 | 2.822 | 0.9172 | 102.7 | 0.3001 | 0.9686 | False | True |
| rho_d_low | 0.8203 | 28.65 | 9.97 | 3.241 | 68.65 | 0.8946 | 0.5038 | False | False |
| rho_d_high | 0.6109 | 21.17 | 18 | 5.852 | 48.48 | 0.2785 | 0.7021 | True | True |
| Dv_dislocation_scale_low | 0.4159 | 27.01 | 2.991 | 0.9723 | 59.49 | 0.4666 | 0.5599 | False | True |
| Dv_dislocation_scale_high | 0.3805 | 24.51 | 3.105 | 1.009 | 59.29 | 0.4723 | 0.5598 | False | True |
| Dg_dislocation_scale_low | 0.9835 | 66.97 | 13.26 | 4.309 | 125.5 | 0.4578 | 0.5914 | False | False |
| Dg_dislocation_scale_high | 1.234 | 9.36 | 19.43 | 6.315 | 13.4 | 0.4882 | 0.4739 | True | True |
| N_gf0_factor_low | 0.3836 | 24.66 | 3.077 | 1 | 59.31 | 0.471 | 0.5598 | False | True |
| N_gf0_factor_high | 0.3836 | 24.66 | 3.077 | 1 | 59.31 | 0.471 | 0.5598 | False | True |

**La precedente enfasi sul raggio high-T viene rimossa.** Per K_d=1e5, il beneficio sull’RMSE di R_d pertinente alla selezione è **24.6645 → 21.8902 nm**, circa **11.25%**, mentre la precedente metrica su tutte le temperature indicava 43.602 → 21.868 nm. Quella riduzione di circa metà non è più una ragione di selezione. Inoltre il bias low-T cambia da −17.171 a **+21.041 nm**: il candidato passa dalla sottostima alla sovrastima, e un K_d intermedio è un’ipotesi più prudente.

Lo swelling rimane identico alla valutazione precedente: K_d basso migliora AP3.8/ANP6, ma peggiora AP3.2/AP3.4. L’RMSE swelling per pin baseline → K_d=1e5 è 0.453 → 1.067, 0.989 → 0.269, 1.078 → 1.593, 1.072 → 0.423 pp, nell’ordine AP3.2/AP3.8/AP3.4/ANP6. La media NRMSE 0.3836 → 0.3685 non autorizza a ignorare i pin peggiorati. Il benchmark gas cambia moderatamente (RMS cinque canali 2.998 → 3.255 pp, +8.55%; compartimenti matrice/bulk/dislocazioni 3.077 → 3.492 pp, +13.49%). È un compromesso da esplorare, non un candidato con accordo simultaneo.

Il fronte Pareto delle sole metriche sperimentali contiene K_d=1e5, rho_d=6e13 e Dg_dislocation_scale=30. Gli ultimi due hanno però RMS gas cinque canali rispettivamente **14.039 e 15.135 pp**, contro **2.998 pp** di baseline (4.68× e 5.05×). I soli compartimenti matrice/bulk/dislocazioni peggiorano a **18.005 e 19.431 pp**, contro **3.077 pp** (5.85× e 6.32×). Il raggio low-T di Dg=30 è migliore (9.359 nm), ma lo swelling peggiora a NRMSE 1.2337 e la redistribuzione bulk/dislocazioni si allontana fortemente dal benchmark: non viene promosso per inseguire R_d. Non serve penalizzare N_d o il raggio high-T per identificare questo problema.

Aumentare Dv_dislocation_scale da 10 a 30 porta swelling NRMSE 0.3836 → 0.3805 e R_d low-T 24.6645 → 24.5133 nm, con gas RMS 2.998 → 3.015 pp. È un effetto piccolo e coerente, non una soluzione dominante su tutti i pin. Dv=30 domina la baseline sulle sole due metriche sperimentali, ma peggiora lievemente gas partition e AP3.2; il fronte tridimensionale lo rende visibile. Il confronto resta gerarchico e separato.

Le perturbazioni estreme f_n=1e-7, Dg=1 e rho_d=1e13 peggiorano swelling e/o raggio low-T e gas partition. L’estremo f_n=1e-2 riporta più gas nel bulk ma peggiora swelling, raggio e benchmark; non è una soluzione di coerenza. Le proposte a f_n moderatamente più alto sono soltanto un’ipotesi per compensare K_d basso: la risposta locale e l’interazione non sono note dall’OAT.

N_gf0_factor=0.1 o 10 lascia esattamente invariati swelling e R_d low-T. Cambia FGR e grain-face: non deve essere usato per correggere una diagnostica high-T intragranulare. Il fattore 10 ha RMS gas a cinque canali lievemente più basso, ma il benchmark dei tre compartimenti di selezione è identico alla baseline; inoltre viola persistentemente gli ordinamenti delle densità in tutti i pin e quello dei raggi in tre pin lungo tutta la griglia. Le tre metriche del fronte con gas sono identiche per baseline e per entrambi i fattori N_gf0; questo non supera i guardrail e non giustifica preferire un fattore. Il fattore 0.1 favorisce gli ordinamenti ma accentua le sovrapressioni grain-face. Nessun nuovo fattore grain-face viene proposto in questo round.

Le sottostime di R_d sopra 1600 K e le sovrastime di N_d high-T sono riportate con RMSE, bias, frazioni di punti sotto/sovrastimati e rapporti mediani, senza penalità nella selezione. Non si reinterpreta la correzione della gerarchia come una necessità di aumentare K_d per far coincidere N_d. Il caso K_d=1e6 può avere una density low-T più vicina ma swelling/raggio low-T peggiori; l’accordo di N_d non lo promuove.

Gli ordinamenti, FGR, R_gf, N_gf e le pressioni conservano le diagnostiche originali. La baseline ha violazioni persistenti dell’ordine dei raggi a bassa T su AP3.2/AP3.8/AP3.4, mentre rispetta l’ordine delle densità; K_d=1e5 introduce violazioni persistenti delle densità in tutti i pin. Le pressioni grain-face sono molto elevate già alla baseline (p_gf fino a 8.87e11 Pa, p_gf/p_eq fino a 2.47e4). Questi segnali restano guardrail di plausibilità, separati dai punteggi sperimentali; la corretta tolleranza del bias high-T non li cancella. Non ci sono nuovi controlli dinamici o nuove simulazioni. Il confronto numerico R_gf/N_gf Rizk resta non disponibile perché manca il CSV digitalizzato Fig.7/8 citato nel notebook precedente.

Le direzioni fisiche OAT non cambiano: aumentare f_n riduce swelling/R_d e sposta gas verso il bulk; diminuire K_d aumenta R_d e spesso swelling ma può eccedere anche a bassa T; aumentare rho_d o Dg aumenta gas dislocazioni e swelling con forti scostamenti dal benchmark agli estremi; Dv ha effetti più piccoli. La nuova gerarchia cambia la scelta delle direzioni da esplorare: **K_d intermedio e Dv moderato/alto**, con una sola proposta di compensazione tramite f_n, senza aumenti di rho_d o Dg finalizzati al raggio high-T. `evaluation_directions.csv/.md` distingue gli scarti dagli errori fisici e riporta Δ per ciascun pin.


**Proposte a due parametri sospese.** Nessuna nuova proposta prima della lettura del refinement.
