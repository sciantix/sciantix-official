# Audit FASE 0 — porting modulare UN L3 Python → SCIANTIX

Data: 18 ottobre 2026. Stato: audit statico completato; piano da approvare.
Non è iniziata alcuna implementazione, generazione del riferimento numerico o verifica analitica.

## 1. Esito e riferimento proposto

Il riferimento locale coerente con la richiesta è
[`finalRizkUN_L3_solver_unificato.ipynb`](../UN_model/notebooks/finalRizkUN_L3_solver_unificato.ipynb).
Propongo di congelare questo file, con i parametri L3 già contenuti, **previa conferma dell'utente**.
Non è possibile certificare che coincida byte per byte con il candidato denominato
`finalRizkUN_L3_solver_unificato(3).ipynb`: quel nome non è presente nel repository,
negli allegati disponibili o nei percorsi corrispondenti della cronologia Git locale.
L'allegato della richiesta contiene testo, non il notebook con suffisso `(3)`.

Identità del candidato locale:

| Elemento | Valore |
|---|---|
| HEAD di partenza | `8f5bfc7e123b0241507c5d1e587a28d20e4dcb24` |
| Branch iniziale | `development/nitride` |
| Branch creato per il lavoro | `feature/un-l3-porting`, stesso HEAD |
| SHA-256 del notebook L3 | `e29d798d3685edb827a43933b3dc4400087f1aff488ee4080650c1430e1c59ff` |
| SHA-256 del codice concatenato | `a03c7680c3753f3b44c4f5e7fcc83cbd300cd427f1578a903d090301a2dc1b69` |
| Definizione di codice concatenato | sorgenti delle celle `code`, in ordine, separate da due newline, UTF-8 |
| Ultimo commit al percorso corrente | `8f5bfc7e`, 16 ottobre 2026, 12:20:28 +02:00 |
| mtime locale | 16 ottobre 2026, 09:56:21 Europe/Rome; informativo, non criterio di selezione |
| Stato delle celle | 23 celle totali; celle di codice senza execution count e senza output salvati |

L'introduzione del notebook dichiara una revisione del 15 ottobre 2026 e dice esplicitamente
che il notebook non è stato eseguito in quella sede. I commenti sulla convergenza
`3 h / 160 modi` non sono quindi prova di verifica eseguita in questo audit.

Sono stati inventariati staticamente **34 notebook UN**, comprese copie di calibrazione,
archivi e sensibilità. L'allegato
[`UN_L3_PHASE0_NOTEBOOK_INVENTORY.json`](UN_L3_PHASE0_NOTEBOOK_INVENTORY.json)
registra percorsi, hash, date, impostazioni selezionate e nomi delle funzioni.
Le impostazioni dell'inventario sono assegnazioni AST, non risultati di simulazioni.

### 1.1 Cronologia e varianti

`git log --follow` ricostruisce una famiglia partita dai notebook esplorativi di maggio,
passata per grain-face/FGR a settembre e aggiornata in ottobre:

| Versione/famiglia | Riscontro e differenza rispetto al riferimento proposto |
|---|---|
| Notebook 2–9 in `notebooks/ARCHIVIO` | varianti di seed, cattura, densità dislocazioni e grain-face; tipicamente 12 h/22 modi; non la configurazione L3 |
| `10test_UN*` | tipicamente 1 h/40 modi, densità costante e cattura; precede il grain-face Pastore corretto |
| `11test_UN_3x3_pastore_corrected` | introduce il grain-face 3×3/Pastore; 1 h/40 modi |
| `12testUN_millerDv` | diffusività GB aggiornata `miller2025_LAUR2528152`, distinta dal default L3 legacy |
| `13testUN_with_intergranular_pressure`, `_14SensitivityUN`, `_15NgfOnly*` | confronti e sensibilità intermedi, 1 h/40 modi; non selezionati |
| `16testUN_newD_D3_CORRETTO` | Xe/U lookup Matthews, GB Miller, 1 h/40 modi; formulazione/configurazione diversa |
| `c23528a9`, 4 ottobre | aggiunge `finalRizkUN.ipynb` con fission rate specifici; `f_n=5.5e-4`, `K_d=3e5`, `rho_d=3e13`, moltiplicatore gas dislocazioni 13, `Ngf0=2e13` |
| `308a0bad`, 5 ottobre | versione precedente `finalRizkUN.ipynb`: 1 h/40 modi, `f_n=5.5e-4`, `K_d=3e5`, `rho_d=3.5e13`, moltiplicatore gas 15; cattura con reset dei modi dalle medie |
| Sensibilità L3 del 7–9 ottobre in `optuna/Rizk Rho Constant` | le celle iniziali possono ancora riportare parametri intermedi; ulteriori celle variano phi, cluster, fission rate o parametri. Non usare il solo nome L3 come identificativo |
| `8f5bfc7e`, 16 ottobre | rinomina `finalRizkUN.ipynb` in `finalRizkUN_L3_solver_unificato.ipynb` (similarità Git 96%); introduce parametri L3, 3 h/160 modi, cattura modale e cache estesa |
| `BaseRizk2025.ipynb` | stesso codice delle funzioni del notebook L3; cambiano impostazioni: 1 h, `f_n=1e-6`, `K_d=5e5`, `rho_d=3e13`, scale dislocazioni gas/vacancy 1/1, `Ngf0=2e13`; non è L3 |
| `optuna/Matthews/UN_Matthews_F_comune_1e19.ipynb` | mantiene parametri L3 ma seleziona diffusività Matthews e GB Matthews; verificare il percorso del runner prima di dedurre i fission rate dal nome: `EXPERIMENT_CASES` contiene ancora quattro rate distinti |
| `UN_model/src/un_model.py` | estrazione storica da `2UNpython_tests`, altra cattura e altri default; non è una libreria equivalente al notebook L3 corrente |
| `UN_model/tests/UNpython_test.py` e primi notebook `UN*` | prototipi/test esplorativi, parametri anche placeholder; non sono un oracolo L3 |

I numeri nei nomi, gli mtime e i commit di spostamento non stabiliscono la correttezza fisica.
La scelta proposta è basata su impostazioni, corpi delle funzioni e differenze Git.
I riferimenti `finalRizkUN(5).ipynb` e
`UN_L3_unificato_Rizk_Matthews_solver_revisionato.ipynb` citati nel markdown non sono disponibili
come file locali per un confronto diretto.

### 1.2 Modifiche dell'utente preservate

All'avvio risultavano già in staging:

```text
M  UN_model/notebooks/BaseRizk2025.ipynb
A  UN_model/notebooks/BaseRizk2025/BaseRizk2025_grid.csv
```

Nessun reset, stash, commit o ripristino. Il codice delle celle di `BaseRizk2025`
è uguale tra HEAD, index e worktree; le differenze staged del notebook riguardano
contenuto non appartenente al codice delle celle. SHA-256 file corrente/staged:
`4115a35aadd5096ae7b0412bc30a8fac4a639cd279feacd0f223f1765cb21af3`;
SHA-256 file a HEAD: `53cc038a9ccfadf2292d3b49a882737f13b9f7fda36880f07c9fd1a9c3771dca`.
SHA-256 iniziale di `git diff --cached --binary`:
`ee2a381549a8c1c6999dcfcb959b10ad1be72fc55be45f3ed433406e38ad116e`.

## 2. Configurazione effettiva del percorso L3

L'analisi segue `main` → ultimo `simulate_grid` → `run_model_point_case` →
`candidate_for_experiment` → `run_model_point` → `UNParameters` → `solve_UN`.
Il notebook ridefinisce alcune funzioni del runner nella stessa cella: valgono le ultime
definizioni, con casi sperimentali separati. Non ho eseguito `main()`.

### 2.1 Default attivi

| Impostazione | Valore nel percorso benchmark |
|---|---|
| `DT_H`, `N_MODES`, `GRAIN_RADIUS` | 3 h = 10800 s; 160; 6e-6 m |
| `XE_DIFFUSIVITY_MODE` | `rizk2025_refit_plot` |
| `VU_DIFFUSIVITY_MODE` | `rizk2025_refit_full` |
| `DV_GB_MODE` | `rizk_legacy_1e6_Dv1` |
| `USE_NUCLEATION_MASS_COUPLING` | True; cluster effettivo `NUCLEATION_CLUSTER_SIZE_CURRENT=2` |
| `USE_BULK_DISLOCATION_CAPTURE` | True |
| `USE_INTERGRANULAR_MODEL` | True |
| `USE_PHI_GAS_RESOLUTION` | False; phi continua però a intervenire nell'equazione di `N_b` |
| `PHI_B_SCALE_CURRENT` | 1 |
| `RHO_MODE`, `USE_DYNAMIC_RHO_D_NUCLEATION` | `constant`; False |
| `USE_FIRST_GAS_PRESSURE_SEED` | True, per entrambe le popolazioni; pressione iniziale = 1/3 di quella di equilibrio |
| `USE_EQUILIBRIUM_DIMER_SEED` | False; `SEED_INITIAL_DISLOCATION_BUBBLES` e `SEED_NEW_BULK_BUBBLES` False |
| `INTERGRANULAR_POST_SAT_MODE` | `pastore_3x3_exact_release`; il corpo della funzione implementa direttamente questo algoritmo, senza dispatch sulla stringa |
| `USE_LENTICULAR_COS3` | True |
| `vacancy_absorption_only` intragranulare | **False**, default dataclass non sovrascritto dal runner |
| `GF_VACANCY_ABSORPTION_ONLY` | **False** |
| `update_bulk_vacancies` | True |
| `USE_WS_GAS_TRAPPING_BULK/DISLOCATION` | False/False; cap della variante WS = 10 |
| `USE_ZETA_OVERFLOW_GUARD` | True |

**Discrepanza documentale confermata:** il markdown ereditato afferma che siano selezionati
Matthews/Schneider e GB Matthews, ma le assegnazioni e tre `assert` nella cella delle
impostazioni impongono i refit Rizk e GB legacy. Nessun cambio di default è giustificato
da quei commenti. Analogamente, il commento che parla di default absorption-only
contraddice i due valori False realmente usati.

`UNParameters()` isolato ha ancora `dt=3600` e `n_modes=40`, mentre `Candidate`
ha scale dislocazioni di default 1 e `D2_xe_scale=1`. Il benchmark sovrascrive
questi valori mediante `DT_H`, `N_MODES` e **tutto** `MANUAL_PARAMS`.
Istanziare soltanto la dataclass con i default non riproduce L3.

### 2.2 Parametri calibrati e condizioni iniziali

| Parametro | Valore L3 |
|---|---:|
| `f_n` | 4.2e-4 |
| `K_d` | 4.1e5 bolle/m |
| `rho_d` | 3.5e13 m^-2 costante |
| `Dg_dislocation_scale` | 17, applicato al solo line sink |
| `Dv_dislocation_scale` | 10 |
| `D2_xe_scale` | 0 |
| `Dg_scale`, `Dv_scale` | 1, 1 |
| `Dg_D1_scale`, `Dg_D3_scale`, `Dv_D1_scale`, `Dv_D2_scale` | tutti 1 |
| scale re-solution, trapping e sue componenti, coalescenza, cattura | tutte 1; `gd_line_alpha=1` |
| Xe yield | 0.24 atomi/fissione |
| `gamma_b`, `omega_fg`, parametro reticolare | 1.11 J/m²; 8.5e-29 m³/atomo; 4.889e-10 m |
| `kB_J`, `kB_eV` | 1.380649e-23 J/K; 8.617333262e-5 eV/K |
| raggio in reticolo, core dislocazioni, `Z_d` | 0.21e-9 m; 3.46e-10 m; 5 |
| `NGF_AREAL_0`, `DELTA_GB`, `FC_SAT` | 1e13 bolle/m²; 4e-10 m; 0.5 |
| `THETA_GF_DEG`, `R_GF0` | 59°; 2.42e-10 m |
| `GAMMA_GB` | 1.1391 J/m², diagnostica; la pressione usa `gamma_b` |

Tutte le concentrazioni di gas iniziali sono zero; `N_b=R_b=R_d=0`.
`N_d=K_d*rho_d=1.435e19 bolle/m³`. Inventari vacancy intragranulari iniziali zero.
Le grain-face bubbles hanno geometria/vacancy iniziali, ma nessun gas iniziale.
Stress idrostatico zero nei casi proposti. Le tre popolazioni hanno vettori di 160 modi.

| Caso | FIMA finale (%) | Fission rate (fissioni m^-3 s^-1) |
|---|---:|---:|
| AP3.2 | 1.1 | 5.767844763153473e19 |
| AP3.8 | 1.1 | 6.863735268152632e19 |
| AP3.4 | 1.3 | 7.498198192099515e19 |
| ANP6 | 3.2 | 7.760680190110210e19 |

Il tempo finale è `(FIMA_percent/100)*(4/a^3)/Fdot`, in secondi.
L'ultimo passo è `min(dt, t_final-t)`; non si arrotonda il burnup per avere passi interi.
Il valore AP3.2 di `FISSION_RATE_NOMINAL` è soltanto un fallback: AP3.8 non può
essere fuso con AP3.2 perché ha lo stesso burnup.

### 2.3 Modalità alternative realmente presenti

- Xe: `matthews2025_fig42_lookup`, `rizk2025_table_D1D3`, `rizk2025_refit_plot`, `rizk2023_table_raw`.
- Vacancy/matrice: `matthews2025_DU_fig42_lookup`, `rizk2025_code_refit_old`, `rizk2025_refit_A20_only`, `rizk2025_refit_full`, `rizk2023_table_raw`.
- GB: `miller2025_LAUR2528152`, `rizk_legacy_1e6_Dv1`. Il nome letterale `miller...` resta quello accettato dalla funzione, anche se i commenti parlano di Matthews.
- Densità dislocazioni: `constant`, `rhoSat_RayBlank`, `rizk2023`, `rizk2023_fit`, `rizk2023_smooth`.

I lookup interpolano linearmente in `(1e4/T, log10(D))`, con estrapolazione del
segmento estremo; non diventano fit analitici. Nella revisione L3 la modalità Xe Matthews
aggiunge **l'intero** `A30*Fdot` al lookup, senza sottrarre il contributo al rate di riferimento.
La versione precedente usava il lookup da solo. Questa convenzione alternativa va
preservata e dichiarata, non reinterpretata come correzione fisica durante il porting.
La modalità U Matthews rappresenta trasporto efficace da autodiffusività totale,
non la mobilità microscopica della singola vacancy; i component scaling D1/D2 non vi sono applicati.
In GB legacy con bulk Matthews viene ricostruito il D1 termico Rizk indicato dal codice.

## 3. Contratto numerico e ordine esatto del timestep Python

Riferimento: cella 8 per core/solver, celle 1–2 per impostazioni/lookup,
cella 10 per runner e ridefinizioni case-specific. Gli indici di cella sono zero-based.

### 3.1 Proiezione e solver

Per modo con indice array `i=0..N-1`, `n=i+1`, peso `w_n=(-1)^n/n`:

```text
P = -2 sqrt(2/pi)
media(u) = somma_n P*w_n*u_n / (4*pi/3)
lambda_n = pi^2 Dg n^2 / Rgrain^2

A = [[1+dt*(lambda_n+gb+gd), -dt*bb_gas, -dt*bd_gas],
     [-dt*gb,                1+dt*bb_gas, 0],
     [-dt*gd,                0,          1+dt*bd_gas]]
rhs = vecchio_modo + dt*P*w_n*[source_c, source_mb, source_md]
A * nuovo_modo = rhs
```

L'inizializzazione delle medie usa `-sqrt(8/pi)` e **20 iterazioni di correzione
del residuo**, partendo da vettori nulli. Non basta una proiezione singola.
La diffusione spaziale riguarda solo `c`; le altre due popolazioni scambiano gas localmente.
Le tre sorgenti vanno proiettate con lo stesso segno/peso. Cramer Python rifiuta
`abs(det)<1e-300`; il solver condiviso C++ controlla invece solo `det!=0`.

### 3.2 Sequenza completa

1. Inizializzare geometria, `N_d`, vacancy totali, modi, produzione zero,
   `q_gb=q_gf=q_rel=0`, grain-face state e flag first-gas False. Salvare `t=0`.
2. Per ogni passo, determinare `dt` anche per l'ultimo passo parziale; calcolare
   burnup vecchio/nuovo e `rho_old`; ricostruire `c_old,mb_old,md_old` dai modi.
   Conservare `Nb_old,Nd_old,Vd_old,Rb_old,Rd_old`.
3. Calcolare `Dg`, `Dv`, `Dv_d`, re-solution e trapping dallo stato precedente.
   Trapping: bulk `4*pi*Dg*(Rb+r_lattice)*Nb`; dislocazioni = termine bolle più
   line sink con `Dg*17` e lunghezza libera `max(rho-2*Rd*Nd,0)`.
   Denominatore logaritmico non positivo → errore Python, non piccolo valore sostitutivo.
4. Calcolare `nu_b=8*pi*f_n*Dg*omega_fg^(1/3)*max(c_old,0)^2`;
   `phi_b` da `mb_old/Nb_old`, `phi_d` da `md_old/Nd_old`, con zero se atomi/bolla ≤1.
   I rate gas non vengono moltiplicati per phi nel default L3.
5. Aggiornare **prima** `Nb=(Nb_old+dt*nu_b)/(1+dt*bb*phi_b)`.
6. Impostare `source_c=beta-2*nu_b`, `source_mb=2*nu_b`, `source_md=0`;
   avanzare tutti i modi con backward Euler e ricostruire le medie.
7. Solo se almeno una media diventa negativa: clamp delle tre medie a zero e
   reset dei tre vettori mediante l'inizializzazione iterativa. È un ramo esplicito
   del riferimento, da registrare quando scatta. Non estendere quel reset alla cattura.
8. Calcolare incrementi del gas. Al primo ingresso di gas in ciascuna popolazione,
   risolvere per bisezione le vacancy per `p=p_eq/3`, aumentare l'inventario se necessario,
   segnare il flag e **saltare l'aggiornamento vacancy normale di quella popolazione nel passo**.
   La bisezione usa fino a 300 espansioni del bracket e 160 iterazioni geometriche.
9. Negli altri passi, aggiornare `nvb` con `Nb` già aggiornato e `nvd` con `Nd_old`.
   Raggio per vacancy: vecchio raggio se positivo, altrimenti raggio gas-only.
   Risolvere la quadratica implicita con ramo stabile per `B<0`; discriminante negativo → errore.
   Nel default **è ammesso anche il decremento delle vacancy**, senza clamp al valore vecchio.
10. Calcolare volumi di crescita dagli incrementi gas/vacancy. Il volume bulk
    predittore è calcolato ma poi ricostruito; il predittore dislocazioni serve alla coalescenza.
11. Calcolare `lambda_d` da `Vd_old*Nd_old`; ridurre `Nd_old` con
    `Nd=Nd_old/(1+4*lambda_d*Nd_old*max(Vd_growth-Vd_old,0))` e relativo scaling.
12. Applicare l'eventuale variazione siti da `rho_next/rho_old` dopo coalescenza;
    nel default L3 questo ramo è disabilitato. Ricostruire volumi/raggi dagli inventari totali.
13. Calcolare volume spazzato `4*pi*(Rd_old+Rb_old)^2*max(Rd-Rd_old,0)`;
    `f_cap=clip(capture_scale*Nd*volume_spazzato,0,1)`, con **Nd dopo coalescenza**.
14. Trasferire `f_cap` di gas bulk e vacancy bulk alle dislocazioni, ridurre `Nb`;
    **per ogni modo** sottrarre `f_cap*mb_mode` a `mb_mode` e aggiungerlo a `md_mode`.
    `c_mode` non cambia. Ricostruire geometria; nessun nuovo dimer/numero di bolle
    dislocazioni viene aggiunto dalla cattura.
15. Incrementare produzione `generated += beta*dt`. Calcolare
    `q_target=max(initial_gas+generated-(c+mb+md),0)` e
    `dq_to_gb=max(q_target-(q_gf+q_rel),0)`.
16. Calcolare Dv GB e avanzare grain-face; porre `q_gb=q_gf+q_rel`,
    `retained=c+mb+md+q_gf`. Aggiornare tempo, diagnostici e storia.

### 3.3 Grain-face: algoritmo da riprodurre

`Ngf_vol=kappa*Ngf_areal`, `kappa=3/(2*Rgrain)`.
Volume lenticolare = `(4*pi/3)*R^3*(1-1.5*cos(theta)+0.5*cos(theta)^3)`;
area = `pi*R^2*sin(theta)^2`. `qv_gf` è un inventario volumetrico totale,
`nv_gf` è per bolla: non sono intercambiabili.

1. Ricalcolare tutte le derivate geometriche dal vecchio stato. Saturazione
   riconosciuta dal flag persistente o da `Fc>=Fc_sat*(1-1e-12)`.
2. `q_trial=q_old+dq_to_gb`, gas per bolla temporaneo con **N vecchio**.
3. Vacancy per bolla con `LimitedGrowth`, usando vecchio raggio/copertura:
   `x=nv0+equilibrium_term*dt`, `nv1=(x+sqrt(x*x+4*growth_rate*dt))/2`.
   Nel default niente vincolo absorption-only.
4. Prima di saturazione, sistema 3×3 per `V,A,N`, con
   `alpha=gasvol0/(N0²*kappa)`, `beta=(2/3)*A0/V0`,
   `gamma=6*N0²/(3+4*Fc0)`. La soluzione dà `N_lin`; il codice conserva tutto
   `q_trial`, applica le vacancy per bolla e ricalcola geometria da composizione.
   **Non conserva semplicemente `V_lin,A_lin` come stato definitivo.**
5. Al primo attraversamento, `N_target=min(N_before,Fc_sat/A_cross)`;
   moltiplicare gas e vacancy totali per la frazione di sopravvivenza, rilasciare
   esattamente il gas perso; conservare composizione delle bolle sopravvissute.
6. Già saturo: chiamare anche il predittore 3×3 saturo (`alpha=0`,
   `gamma=N0/A0`), verificandone positività/finitudine. Poi composizione con
   `ng1=q_trial/(kappa*N0)` e `nv1`, ricalcolo geometria e
   `N1=min(N0,Fc_sat/A1)` se area crescente; se area diminuisce mantenere N0.
7. `q_gf1=q_trial*(N1/N0)`, `q_rel1=q_rel_old+(q_trial-q_gf1)`;
   `qv_gf1=nv1*kappa*N1`. Con area crescente verificare copertura a 0.5 entro 5e-10.
   Nessun ritorno al capacity clamp del C++ precedente.

### 3.4 Bilancio e diagnostica

Per Xe stabile:

```text
initial_gas + generated = c + mb + md + q_gf + q_rel + residuo
q_gb = q_gf + q_rel
retained = c + mb + md + q_gf
swelling_total = Nb*Vb + Nd*Vd + Ngf_vol*Vgf
```

Non sommare `q_gb` a `q_gf` e `q_rel`. Il residuo va calcolato indipendentemente:
`q_target` è già costruito da un bilancio e alcuni rami applicano `max`, quindi la
sola identità finale non dimostra l'esattezza del solver. Con N finito anche la
proiezione della sorgente ha un effetto da confrontare esattamente con Python.
Registrare anche ricostruzioni modali e inventari prima/dopo ogni trasferimento.

Il notebook restituisce storia delle variabili e **solo gli ultimi** rate in `last_rates`.
Per ottenere rate/modi a ogni passo nella FASE 1 servirà un osservatore diagnostico
esterno o inserito in memoria nella sorgente caricata, con corpi delle equazioni invariati
e confronto con l'esecuzione originale senza osservatore. Non trasformare la reference
in una riscrittura indipendente delle equazioni Python.

## 4. Architettura SCIANTIX disponibile e differenze

### 4.1 Interfacce e componenti riutilizzabili

- `MainSCIANTIX.C`: lettura input, interpolazione storia, chiamate temporali,
  log dei tempi. Il primo giro ha `dt=0`; i giri successivi usano il dt calcolato
  nell'iterazione precedente. L'allineamento temporale deve distinguere la riga iniziale
  da un vero avanzamento Python.
- `Sciantix.C` → singleton `Simulation::initialize/execute/update/output`.
  `initialize` ricostruisce variabili/materiali/sistemi a ogni chiamata.
- `SciantixArray` sostituisce gli elementi con lo stesso nome; `SciantixVariable`
  offre valori initial/final e unità; `Model` raccoglie parametri e riferimenti.
- `Matrix`, `System`, `Gas`: proprietà materiale, tassi e produzione. `iFuelMatrix=2`
  registra `UN` e attualmente **solo `Xe_in_UN`**. Le funzioni Kr/He UN esistono,
  ma non vengono registrate dal ramo attivo; i vecchi report che dicono il contrario
  non descrivono il codice corrente.
- `Solver::SpectralDiffusion3equationsExchange`: matrice, pesi, sorgenti e
  backward Euler coerenti strutturalmente con Python; da confrontare numericamente,
  non dichiarato già verificato. Accetta sorgenti opzionali mb/md.
- `Solver::modeInitialization`: stessa proiezione iterativa in 20 passaggi;
  richiede vettori inizialmente nulli per questa inizializzazione.
- `Solver::LimitedGrowth`, `Decay`, `Integrator`, `Laplace3x3`: strumenti disponibili.
  Riutilizzare LimitedGrowth solo dove coincida il comportamento ai bordi.
- `SetMatrix`, `SetSystem`, `SetVariablesFunctions`, `UpdateVariables`,
  `Initialization`, `Output`: plumbing materiale, opzioni, stati e output UN già presente.
- Stato scalare UN già assegnato agli indici **170–201**, inclusi md, densità,
  vacancy per bolla, grain-face gas e gas rilasciato. Non sono indici liberi.
- `GasProduction/GasDecay` e inizializzazione densità U UN `4/a³`: riutilizzabili
  con configurazione coerente; evitare una seconda integrazione della produzione.

### 4.2 Differenze accertate nel sorgente attuale

| Area | Python L3 | C++ corrente / conseguenza |
|---|---|---|
| Calibrazione | fn 4.2e-4, Kd 4.1e5, rho 3.5e13 | fn 5.5e-4 duplicato in GasDiffusion e IntraGranular; Kd 3e5; matrice rho 3e13 |
| Line sink | scala 17 | scala hardcoded 13 in `System.C` |
| Xe D2 | calcolato diagnostico, peso zero | `iFissionGasDiffusivity=11` somma D1+D2+D3 |
| Vacancy | refit full, scala dislocazioni 10 | opzione 2 compatibile nei coefficienti principali e scala 10; altre modalità mancanti |
| kB Joule | 1.380649e-23 | costante condivisa 1.380651e-23; differenza sistematica, non arrotondamento della stessa espressione |
| N_b e phi | phi da mb_old, Nb aggiornato prima del gas | `Simulation::execute` chiama gas prima delle bolle; phi usa mb appena aggiornato |
| Seed pressione | flag al primo gas, skip vacancy nello stesso passo | se gas>0 e vacancy≤0 esegue seed e subito vacancy; mancano flag equivalenti |
| Vacancy intragranulari | emissione ammessa (`absorption_only=False`) | helper ritorna il vecchio valore se p≤p_eq e impone `max(n_new,n_old)` |
| Coalescenza dislocazioni | predittore include anche incremento vacancy da seed | incremento vacancy C++ misurato dopo seed; possibile divergenza già al primo gas |
| Cattura bulk→dislocazioni | inventari e modi trasferiti | assente nel ramo UN corrente |
| Densità dinamica | disabilitata | default opzionale `iUNDislocationDensity=1`, attiva legge smooth e variazione Nd |
| Geometria GF | cos³, 59°, Ngf0=1e13 | helper cos³ riutilizzabile, Ngf0=2e13 |
| Vacancy GF | LimitedGrowth, anche emissione | passo esplicito con pressione motrice clampata positiva |
| Coalescenza/rilascio GF | sistema 3×3, proiezione e sopravvivenza Pastore | aggiornamento esplicito N e capacity clamp a saturazione |
| Gas GB iniziale | include `initial_gas` | target da prodotto-decayed-retained; va esplicitata la convenzione di prodotto iniziale per stati non nulli |
| Modi | 160 configurabili | n_modes=40, buffer pubblici 720 e offset hardcoded 40; md in buffer interno 200 |
| Restart/coupling | stato completo richiesto dal porting | scalare md esportato, modi md non esportati; manca inizializzazione equivalente da md iniziale e checkpoint completo |
| Pressioni zero | bulk p_eq usa raggio minimo, gas/vacancy=0 può dare inf | helper C++ p_eq(R≤0)=0, pressione senza vacancy=1e30 e output non finiti convertiti a zero |
| Errori | determinante/discriminante/domain error espliciti | vari clamp silenziosi, denominatore trapping forzato, Laplace3x3 lascia rhs se det=0 |
| Precisione output | float64 conservabile a 17 cifre | `Output.C` stampa variabili a 7 cifre significative; insufficiente per confronti stretti |

Il ramo attuale è soprattutto un porting notebook-8 di maggio, non un porting L3.
`include/models/UNModel.h` contiene una legge di densità, non il solver UN completo;
non esiste `src/models/UNModel.C`. La fisica UN è oggi dispersa in file comuni e
in un grande blocco di `IntraGranularBubbleBehavior.C`.

La sequenza `execute` attuale per il comportamento 5 è produzione/decadimento →
GasDiffusion → IntraGranularBubbleBehavior (che contiene anche grain-face) →
microcracking/venting; salta InterGranularBubbleBehavior e GasRelease generici.
L3 deve evitare quei doppi aggiornamenti e richiedere disattivati i modelli assenti dal notebook
(sweeping, cracking, venting, crescita grano, HBS ecc.) nei deck di regressione.

### 4.3 Build e suite presenti

`CMakeLists.txt` raccoglie ricorsivamente `src/*.C`, costruisce eseguibile oppure
libreria con `COUPLING_TU`, include CTest ma non definisce alcun `add_test`.
Non esiste una cartella `tests/` alla radice. I file in `UN_model/tests` sono
script esplorativi, non una suite unitaria L3 già pronta.
Il post-build copia gli oggetti in `obj/` dentro la sorgente anche con build fuori albero:
tenerne conto per i percorsi di verifica isolati. Non usare `runRegression.sh` durante
l'audit: richiama `Allclean.sh`, che rimuove build, oggetti e binari nel repository.

Il runner moderno è `python -m regression.runner --all --mode-gold 0`, lo stesso della CI.
Gestisce 12 famiglie e 110 directory caso con gold disponibili:

| Gruppo | Casi |
|---|---:|
| baker | 9 |
| cornell | 9 |
| white | 43 |
| kashibe | 23 |
| talip | 5 |
| oxidation | 4 |
| chromium | 3 |
| contact | 1 |
| hbs | 1 |
| vercors | 1 |
| analytics | 2 |
| gpr | 9 |

Il runner legacy in `utilities/regression_scripts` e `GPregression` resta disponibile;
il riferimento operativo corrente è quello usato dalla CI. Il comparatore moderno
usa nominalmente `abs_tol=1e-8`, `rel_tol=1e-6` e confronta array completi.
Il parser può generare NaN e il comparatore non applica un esplicito controllo globale
di finitudine; la nuova suite UN dovrà distinguerli da diagnostici non applicabili,
senza cambiare arbitrariamente il comportamento della suite storica.

`regression/un_notebook8` è separato e non entra in `--all`. Confronta solo il
valore finale con `rel_tol=0.35`, `abs_tol=1e-30`, 40 modi e `tfinal/100`.
Il generatore Python punta ancora a `UN_model/notebooks/8test_UN_intergranular.ipynb`,
ora in `ARCHIVIO`: riferimento non rigenerabile a quel percorso.
Usa fission rate comune 5.77e19 e densità smooth, non i quattro casi L3.
Questa suite non può certificare il porting richiesto; va mantenuta come storico,
segnalandone l'errore di percorso, senza sostituirne i gold per farla passare.

Ambiente ispezionato: Python 3.12.3, g++ 13.3.0, **cmake assente** dal PATH.
Nel `.venv`: numpy 2.4.4, pandas 3.0.2, matplotlib 3.10.9, nbformat 5.10.4;
**SciPy, pytest, nbclient non installati**. SciPy è necessario per la FASE 4 e può
essere richiesto da parte della suite storica; non è necessario al core Python L3 basato su math.
Esiste solo un binario `build/sciantix.x` di maggio senza cache CMake: non dimostra
la compilabilità dell'HEAD corrente. Nessuna installazione è stata effettuata.

## 5. Architettura proposta, da approvare

Usare un percorso L3 esplicito: proposta `iFuelMatrix=2` e nuova opzione
`iIntraGranularBubbleBehavior=6` (valore attualmente libero), con solver exchange 4.
Conservare il comportamento 5 e le sue impostazioni storiche, oltre ai percorsi UO2/HBS.
La nuova opzione richiede il preset L3 e permette di distinguere chiaramente risultati
precedenti e nuovi. Nomi e firme qui sotto sono proposte, non file già implementati.

| File previsto | Responsabilità |
|---|---|
| `include/models/UNParameters.h` | parametri e opzioni tipizzati, preset L3, costanti locali esatte, conversioni esplicite |
| `include/models/UNState.h` | stato indipendente: tre vettori modali, inventari totali, N/V/R, GF, flag seed/saturazione, produzione e diagnostici |
| `src/models/UNTransport.C` | diffusività e lookup, produzione, trapping, re-solution, densità dislocazioni |
| `src/models/UNBubblePhysics.C` | geometria, pressioni, seed, vacancy implicite, coalescenza e cattura conservativa |
| `src/models/UNIntraGranular.C` | ordine degli aggiornamenti intragranulari e chiamata al vero solver SCIANTIX |
| `src/models/UNGrainFace.C` | inizializzazione GF, LimitedGrowth, 3×3, attraversamento saturazione e rilascio |
| `src/models/UNL3.C` | `Simulation::UNL3` come orchestrazione sottile, mapping stato/input/output e bilanci |
| `include/classes/Simulation.h`, `src/classes/Simulation.C` | stato L3 posseduto dalla Simulation e dispatch esplicito del nuovo percorso |
| `src/file_manager/InputReading.C`, eventuale lettore UN dedicato | configurazione L3 nominata, validazione delle combinazioni e controlli dt/modi |
| `src/operations/SetSystem.C`, `SetMatrix.C` | adapter L3 verso materiale/sistema, evitando l'applicazione dei rate e della calibrazione hardcoded del percorso storico durante initialize |
| `src/operations/SetVariablesFunctions.C`, `UpdateVariables.C` | mapping scalari su campi già esistenti, aggiunta dei soli diagnostici/stati mancanti con indici documentati |
| `src/MainSCIANTIX.C`, `TimeStepCalculation.C` | dt L3 opzionale, ultimo passo esatto e riga t=0, confinati alla nuova configurazione |
| output UN dedicato | CSV/trace a 17 cifre, senza cambiare precisione e colonne UO2 |
| `CMakeLists.txt` e nuovi test | target piccoli con veri moduli di produzione, test CTest e driver per regressione |

`UNL3` deve collocarsi nella normale chiamata `Sciantix`, non essere un simulatore C++
esterno scollegato dall'eseguibile. Deve usare una sola sequenza intragranulare/GF,
evitando che GasDiffusion o GasRelease generici la eseguano nuovamente.
La produzione SCIANTIX può essere riutilizzata, ma produzione cumulata/initial gas
devono avere un unico proprietario e un contratto testato.

Il numero di modi UN va separato dai buffer legacy a stride 40: **non** sostituire
globalmente 40 con 160, né passare 160 a un buffer di 40 elementi.
Proposta: stato L3 con vettori dinamici, checkpoint completo e API opzionale per
coupling che esporti/importi anche modi md, inventari e flag. L'interfaccia legacy a
720 elementi resta utilizzabile con il suo contratto. Lo stato esteso è richiesto
per restart UN fedele; esportare solo le medie non lo consente.

La configurazione numerica/fisica UN può essere raccolta in un input nominato
dedicato `input_un_l3.txt` con preset esplicito, senza riempire l'array delle 40 opzioni
con decine di nuovi parametri. Il manifest della reference deve salvare i valori
effettivi, non affidarsi a default impliciti. Il lettore va attivato solo per L3.
Le proprietà placeholder della matrice UN, per esempio il volume Schottky 4.09e-29 m³,
non devono sostituire `a³/4` nelle equazioni L3. Non serve correggere globalmente
quei valori per ottenere il porting: il nuovo adapter deve usare i parametri del riferimento.

## 6. Mappa Python → C++ prevista e test per componente

Ogni funzione fisica del percorso `solve_UN` è inclusa qui, anche se raggruppata
con helper strettamente collegati. Runner, dati sperimentali e grafici rimangono in Python.

| Funzione/famiglia Python | Destinazione C++ proposta / riuso | Test richiesto |
|---|---|---|
| `Candidate`, `UNParameters`, `__post_init__` | `UNParameters`, preset e costruzione stato | manifest completo, Nd0, tempi finali e override dei default |
| `omega_matrix`, `uranium_atom_density_from_lattice` | `UNBubblePhysics`/costanti locali | conversioni SI e identità reciproca |
| `burnup_percent_to_time`, `time_to_burnup_percent` | `UNTransport` | quattro fission rate e round-trip |
| `sphere_volume`, `radius_from_volume` | `UNBubblePhysics` | zeri e round-trip su scale bubble |
| `_safe_exp` | helper UN locale | soglie -745/700, senza modificare helper globali |
| `_prepare_log_lookup`, `_piecewise_log_lookup` | `UNTransport`, tabelle fedeli | ordinamento/deduplica, nodi, interpolazione, estremi ed estrapolazione |
| `matthews2025_DU_lookup`, `matthews2025_DXe_lookup` | `UNTransport` | tutti i nodi e convenzione delle unità |
| `xe_diffusivity_UN` | `UNTransport`; riusare hook System, non la somma D2 attuale | quattro modalità, D1/D2/D3 e rate distinti |
| `vacancy_diffusivity_UN` | `UNTransport` | cinque modalità, componenti e scaling dislocazioni |
| `grain_boundary_vacancy_diffusivity` | `UNTransport` | entrambe modalità e caso bulk Matthews+GB legacy |
| `b0_resolution`, `resolution_rates_UN` | `UNTransport`, equazioni già in System | R=0, piccoli/grandi R, scale separate |
| `ws_gas_trapping_factor`, `trapping_rates_UN` | `UNTransport` | termini bolle/linea, clipping lunghezza libera, WS on/off, denominatore invalido |
| `beta_production` | adapter `GasProduction` e coefficienti UN | beta=yield*Fdot e cumulata senza doppio incremento |
| `nucleation_rate_bulk` | `UNTransport` | c nullo/non nullo, source_c+source_mb=beta |
| `phi_population` | `UNBubblePhysics` | atomi/bolla ≤1, >1, N nullo, uso del vecchio stato |
| `coalescence_lambda` | `UNBubblePhysics` | xi nullo e prossimo al cap 0.999999 |
| `pressure_internal`, `pressure_equilibrium` | `UNBubblePhysics` | vecchi/nuovi inventari, stress SI, zero/inf previsti |
| `seed_vacancies_for_pressure_factor` | `UNBubblePhysics` | bisezione, pressione p_eq/3, fallimento bracket come Python |
| `dimer_seed_for_population` | `UNBubblePhysics` | default off, variante on separata, nessuna massa inventata |
| `vacancies_for_pressure_factor_from_gas` | `UNBubblePhysics` | conservazione gas e conversione per-bolla/totale |
| `gas_only_radius_for_population`, `radius_for_vacancy_update` | `UNBubblePhysics` | raggio iniziale nullo e priorità del raggio vecchio |
| `wigner_seitz_delta`, `zeta_geometry` | `UNBubblePhysics` | psi<1, ≥1, overflow guard e cap |
| `vacancy_concentration_implicit_step` | `UNBubblePhysics` | rami B≥0/B<0, emissione, absorption-only opzionale, discriminante |
| `initialize_vacancy_concentration` | `UNState`/`UNBubblePhysics` | geometria iniziale con e senza gas |
| `initialize_modes_from_average` | `Solver::modeInitialization` su vettori nulli | 1/40/160 modi, 20 correzioni, medie non nulle |
| `reconstruct_average` | helper `UNIntraGranular` | medie firmate e ordine della somma |
| `det3`, `solve3x3_cramer` | `Solver::Laplace3x3` con controlli nel percorso UN | soluzioni/rhs identici, singolare e quasi singolare |
| `sciantix_3x3_exchange_step` | vero `Solver::SpectralDiffusion3equationsExchange` | tutti i modi, tutte le sorgenti, segni, dt=0 e più passi |
| `reset_modes_to_averages` | `UNIntraGranular` | init e solo ramo negativo; nessun richiamo durante cattura |
| `rho_d_for_time_burnup` | `UNTransport` | default costante e separazione dall'update siti |
| `rho_rizk2023`, `_interp_F_curve`, `rho_base_fig410`, `_interp_fig411`, `_correction_factor`, `_burnup_blend_weight`, `rho_d_rizk2023_centipede` | `UNTransport`, tabelle alternative | nodi, blend, cap, frazione/percentuale FIMA |
| `rho_rizk2023_smooth_monotone`, `rho_sat_shape10`, `rho_sat_factor_raw`, `rho_burnup_1025`, `effective_rho_d` | `UNTransport` | selezione delle cinque leggi, baseline costante esatta |
| `gf_theta_rad`, `gf_lens_factor`, `gf_volume_from_radius`, `gf_radius_from_volume`, `gf_area_from_radius` | `UNGrainFace` | cos³ e variante cos², conversioni deg/rad, round-trip |
| `gf_zeta_from_coverage`, `gf_pressure_internal`, `gf_pressure_equilibrium` | `UNGrainFace` | coperture limite e diagnostici iniziali |
| `initialize_grainface_state`, `_refresh_grainface_derived` | `UNState`/`UNGrainFace` | qv iniziale, conversione areale/volumetrico, niente gas iniziale |
| `_limited_growth_grainface` | `Solver::LimitedGrowth` dove equivalente | stessi argomenti ed estremi del codice Python |
| `_solve_grainface_3x3` | `UNGrainFace` | ramo pre/post, determinante 1-alpha*beta*gamma e casi invalidi |
| `update_grainface_state` | `UNGrainFace::advance` | pre-sat, primo crossing, già saturo, area decrescente, bilanci e Fc |
| blocco cattura/coalescenza in `solve_UN` | `UNIntraGranular` + helper `UNBubblePhysics` | fcap 0/intermedio/1, conservazione di ogni modo e vacancy, Nb=0 |
| `solve_UN`, `append_state`, `last_rates` | `UNL3`/`UNIntraGranular`, osservatore e mapping SCIANTIX | timestep completo con trace per fase e storia completa |
| `make_candidate`, `candidate_for_experiment`, `run_model_point`, `run_model_point_case`, `simulate_grid` | runner Python della suite, deck/config C++ | quattro case ID distinti, hash/config validati, niente griglia automatica |
| funzioni `plot_*`, dati sperimentali, CSV, cache, diagnostici guard e sweep | strumenti Python di regressione/postprocessing | corrispondenza colonne/unità, primo scarto, cache non condivisa tra configurazioni |

## 7. Piano di esecuzione dopo approvazione

### Passo A — congelamento reference Python (FASE 1)

1. Confermare notebook locale/hash e configurazione sopra descritti; copiare la
   sorgente originale approvata sotto `UN_model/regression_reference/` con manifest.
2. Runner in processo nuovo che carica **solo le celle 1, 2, 6, 8, 10** in ordine,
   registrando un vero modulo Python per le dataclass. Escludere cella 12 (`main`
   esegue la griglia), cella 14 (lancia AP3.4) e sweep opzionali. Assert sugli hash,
   funzioni e default; niente dipendenza dal kernel Jupyter.
3. Usare i factory originali case-specific. Eseguire prima solo pochi timestep
   diagnostici, dichiarando il tempo finale ridotto come caso diagnostico separato;
   poi i quattro casi completi richiesti con **3 h/160 modi e ultimo passo parziale**.
4. Salvare parametri dataclass effettivi, stato iniziale, versione Python/librerie,
   warning/errori, storia, ultimi rate e trace intermedi/modali selezionati.
   Rerun indipendente e confronto degli artefatti per riproducibilità.

### Passo B — porting incrementale (FASE 2)

1. Compilare e fotografare la baseline C++ prima di modificare la fisica; eseguire
   regressioni storiche in copia isolata per distinguere errori preesistenti.
2. Aggiungere strutture stato/config e fixture di componente; trasporto e conversioni.
3. Verificare proiezione/ricostruzione e solver exchange già esistente su vettori dinamici.
4. Seed, vacancy, numero bolle, coalescenza e cattura: verificare singolarmente e
   poi un passo intragranulare completo nell'ordine Python.
5. Grain-face: prima geometria/vacancy/3×3, poi crossing e ramo già saturo.
6. Integrare nuovo dispatch SCIANTIX, input, output e persistenza; ripetere i test
   rilevanti dopo ogni modifica. Implementare le modalità selezionabili presenti
   senza sostituirle ai default L3; fixture distinte anche per alternative inattive.

### Passo C — regressione temporale e non regressione (FASE 3)

I primi casi completi sono AP3.2/900 K, AP3.4/1600 K, AP3.4/1800 K e ANP6/1600 K.
Aggiungere una piccola fixture di configurazione AP3.8 per impedirne l'accorpamento
ad AP3.2, senza avviare l'intera griglia.

Confrontare a ogni timestep tutte le grandezze richieste: c/mb/md, Nb/Nd, Rb/Rd,
inventari vacancy totali e per bolla, pressioni e rapporti, swelling bulk/dislocazioni/GF/totale,
qgb/qgf/qrel, produzione e residuo. Includere N/A/V GF, copertura e flag saturazione.
Trace: rate, sorgenti, phi vecchi, stato dopo exchange, seed, vacancy, coalescenza,
cattura e GF; modi almeno ai primi passi e al primo scarto significativo.

Ogni confronto produce errori assoluti, relativi dove definiti, normalizzati,
massimo con tempo/variabile, primo timestep e prima fase divergente, CSV e grafici.
Nessun interpolatore temporale deve nascondere passi differenti.

Proposta di politica delle tolleranze, da fissare prima dei confronti:

- regola `abs(x_cpp-x_py) <= atol_j + rtol_j*abs(x_py)` per variabile;
- componenti regolari: obiettivo iniziale `rtol=1e-12`; exchange/modi `1e-11`;
  storia completa `1e-9`, con confronto separato degli eventi e delle branche;
- scala assoluta per ogni unità: `atol_j=epsilon_j*S_j`, con `epsilon_j` iniziale
  1e-12 e `S_j` fisso dichiarato nel manifest (per gas: produzione di un passo;
  per N: densità iniziale rappresentativa; per raggi/volumi: scale bubble della fixture;
  per pressioni: pressione capillare rappresentativa; per swelling/copertura: scala 1);
- riportare sempre lo scarto fisico assoluto: un valore vicino a zero non viene
  giudicato usando solo una percentuale o un pavimento 1e-300;
- per quantità nulle attese e bilanci di trasferimento usare anche invarianti dedicati;
  per diagnostici inf/NaN intenzionali registrare categoria e motivo, vietando non finiti
  negli stati evolutivi validi. Non trasformarli a zero;
- verificare sensibilità a condizionamento e somma modale; ogni variazione delle soglie
  richiede motivazione quantitativa, senza modificare parametri o branche fisiche.

Questi numeri sono **obiettivi proposti**, non accuratezze misurate. La presenza
di soglie e sottrazioni quasi cancellanti richiede metriche normalizzate per stadio;
un attraversamento di saturazione in un altro timestep non è automaticamente accettabile.

La suite storica sarà eseguita prima e dopo, stessi compilatore/config e gold immutati.
In aggiunta ai gold, confrontare baseline C++ e C++ modificato per i combustibili
esistenti; segnalare ogni fallimento, anche se già presente. Eseguire separatamente
la suite UN notebook-8, riportando il problema del generatore senza confonderlo con L3.

### Passo D — prototipo analitico soltanto dopo superamento regressione (FASE 4)

Creare `UN_model/Verification/` solo dopo che C++ e Python concordano.
Driver collegato allo **stesso** solver exchange di produzione; coefficienti costanti,
stati iniziali controllati e sorgenti senza trasferimento non lineare ±2nu.
Riferimento `scipy.linalg.expm`, confronto per modo e medie, misura wall-clock
separando avvio processo e calcolo. Prima poche combinazioni, ad esempio
N=8/40/160 e 1/10/100 passi, senza campioni casuali, senza cambiare le simulazioni normali.
Eventuali studi di convergenza/LHS saranno definiti dopo queste misure.

## 8. Rischi di regressione e contenimento proposto

| Rischio concreto | Contenimento / prova |
|---|---|
| cambio delle costanti comuni per imitare Python | costanti UN locali, lasciare intatti i numeri UO2; fixture kB |
| offset modali 40/720 corrotti | vettori L3 separati; test 40 e 160 e canary/controlli dimensione |
| modifica ordine esecuzione per tutti | dispatch solo combinazione esplicita L3; confronto baseline dei 110 casi |
| effetti UO2 applicati a UN | deck coerenti, controlli incompatibilità, nessuna sostituzione forzata delle equazioni |
| doppia produzione o doppio GF/FGR | proprietà unica dello stato e trace del bilancio a ogni stadio |
| reset dai soli valori medi | persistenza modi completa; cattura testata modo per modo |
| perdita di flag al restart | checkpoint con seed/saturazione e prova split-run vs uninterrupted |
| colonne/precisione output diverse nei gold UO2 | trace UN separato e output storico invariato |
| riscrittura involontaria dei gold | solo `--mode-gold 0`; esecuzione in copia isolata; hash prima/dopo |
| fiducia nei report storici | codice corrente come evidenza; vecchi documenti conservati con limiti dichiarati |

## 9. Ripetizione nel WSL e stato delle verifiche

Comandi di sola ispezione già usati: `git status`, `git log --follow`, `git show`,
parsing JSON/AST dei notebook e lettura dei sorgenti/build/runner.
Nessuna ricerca bibliografica esterna necessaria: questo audit descrive il codice locale,
non decide quale formulazione fisica della letteratura sia corretta.

Per le fasi successive occorrono CMake e le dipendenze Python mancanti.
Comandi ordinari previsti **dopo approvazione**, da eseguire in una copia di lavoro
di verifica per proteggere output preesistenti (i percorsi dei nuovi runner sono ancora proposti):

```bash
cmake -S . -B build
cmake --build build -j 2
ctest --test-dir build --output-on-failure
.venv/bin/python -m regression.runner --all --mode-gold 0 -j 2
.venv/bin/python regression/un_notebook8/run_un_notebook8.py --exe build/sciantix.x
```

Nell'HEAD attuale CTest non ha test registrati; il comando acquisterà significato
per i nuovi target unitari. Il runner moderno usa il percorso fisso `build/sciantix.x`.
I comandi di generazione reference L3 e confronto saranno documentati e provati
insieme ai relativi script, che non sono stati creati durante la FASE 0.

| Verifica | Stato in questo audit |
|---|---|
| Inventario notebook e hash | eseguito, 34 notebook parsati |
| Confronto L3/Base/cronologia e configurazioni | eseguito staticamente |
| Analisi solver Python, timestep, mapping C++ | eseguita staticamente |
| Creazione branch dedicato | eseguita |
| Modifiche a sorgenti, equazioni, notebook, tesi Word, gold | nessuna |
| Esecuzione del notebook/riferimenti Python | non eseguita, FASE 1 da approvare |
| Compilazione/test unitari/regressioni SCIANTIX | non eseguiti; cmake assente |
| Confronti numerici C++–Python | non eseguiti; porting non verificato |
| Benchmark analitico | non creato e non eseguito |

## 10. Conferme richieste prima di proseguire

1. Approvare come riferimento il notebook locale **senza `(3)`**, identificato dall'hash
   sopra, con refit Rizk/GB legacy, parametri L3, 3 h/160 modi e vacancy absorption-only
   False. La sua identità con un eventuale altro allegato `(3)` resta non dimostrata.
2. Approvare l'architettura modulare e il nuovo percorso L3 esplicito, preservando il
   comportamento 5 storico, con regressione Python–C++ precedente alla verifica analitica.

L'arresto qui è imposto dalla richiesta dell'utente: «Inizia esclusivamente con la
FASE 0 […] Attendi la mia approvazione prima di modificare l'implementazione».
