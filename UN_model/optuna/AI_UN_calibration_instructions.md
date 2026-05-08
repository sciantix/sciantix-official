# Istruzioni operative per un agente AI — calibrazione modello UN/Rizk

## 1. Scopo del lavoro

L'obiettivo è prendere il codice Python del modello UN intragranulare, eseguirlo automaticamente, confrontarlo con i dati sperimentali digitalizzati da Rizk/White, modificare i parametri incerti e produrre una o più soluzioni di compromesso che rappresentino bene:

1. lo swelling sperimentale delle large intragranular bubbles / popolazione P2;
2. la concentrazione numerica delle bolle grandi, da confrontare con `N_d`;
3. il raggio medio delle bolle grandi, da confrontare con `R_d`;
4. la coerenza fisica secondaria rispetto a pressioni, gas partition e andamento qualitativo delle curve Rizk.

La priorità è il fit ai dati sperimentali. Le curve modello di Rizk su bulk bubbles, grain-boundary bubbles, pressure e gas partition sono vincoli/diagnostiche secondarie, non il target principale di fitting.

---

## 2. Interpretazione fisica da mantenere

Nel modello Rizk per UN:

- la popolazione P1 è interpretata come bulk bubbles;
- la popolazione P2, cioè le large intragranular bubbles misurate sperimentalmente, è interpretata come dislocation bubbles;
- quindi Fig. 3, Fig. 6, Fig. 7 e Fig. 8 devono essere confrontate principalmente con la popolazione dislocation del codice: `swelling_d`, `N_d`, `R_d`.

Tuttavia, durante l’analisi conviene stampare anche `swelling_ig = swelling_b + swelling_d`, perché alcuni risultati numerici indicano che i punti sperimentali possono essere meglio approssimati dal totale intragranulare. Questo non deve sostituire il confronto principale con `swelling_d`, ma va riportato come diagnostica.

---

## 3. Parametri da considerare fissi / corretti

Questi parametri non devono essere usati per il fitting, salvo test di sensitività esplicitamente motivati:

```text
FISSION_RATE = 5.0e19 fiss / (m3 s)
GRAIN_RADIUS = 6.0e-6 m
lattice_parameter = 4.889e-10 m
omega_fg = 8.5e-29 m3/atom
gamma_b = 1.11 J/m2
rho_d = 3.0e13 m-2
r_d = 3.46e-10 m
Z_d = 5.0
radius_in_lattice = 0.21e-9 m
xe_yield = 0.24   # Xe-equivalent reference case
use_sciantix_D2_zero = True for Xe
```

Motivazione:

- `GRAIN_RADIUS = 6.0e-6 m` è il valore da usare per i confronti Rizk-style, non `5.0e-6 m`.
- `FISSION_RATE = 5.0e19` è il valore di riferimento usato per la validazione DN1/Rizk-style.
- Per Xe, il termine `D2` della diffusività va tenuto nullo nel caso di riferimento.

---

## 4. Equazioni e implementazione da preservare

### 4.1 Diffusività Xe

Usare:

```text
D_g = D1 + D3
D1 = D10 * exp(-Q1 / (kB*T))
D3 = A30 * Fdot
```

con:

```text
D10 = 1.56e-3 m2/s
Q1 = 4.94 eV
A30 = 1.85e-39 m5
```

`D2` Xe deve restare zero nel caso base.

### 4.2 Re-solution

Usare per bulk e dislocation:

```text
b = b0(R) * Fdot
b0(R) = 1e-25 * (2.64 - 2.02 * exp(-2.61e-9 / R))
```

Non modificare questa correlazione nel fit principale. Può essere testata solo come scenario separato, con giustificazione.

### 4.3 Trapping bulk

Usare:

```text
g_b = 4*pi*D_g*R_b*N_b
```

### 4.4 Trapping dislocation

Usare:

```text
g_d = 4*pi*D_g*R_d*N_d
      + (2*pi*D_g/den) * (rho_d - 2*R_d*N_d)

den = log(Gamma_d/(Z_d*r_d)) - 3/5
Gamma_d = 1/sqrt(pi*rho_d)
```

Nel codice è ammesso introdurre un fattore diagnostico:

```text
g_d_eff = g_d_scale * g_d
```

ma `g_d_scale = 1` resta il caso fisico nominale. Se il best fit richiede `g_d_scale != 1`, bisogna discuterlo come possibile correzione empirica del trapping verso dislocazioni.

### 4.5 Nucleazione bulk

Usare:

```text
nu_b = 8*pi*f_n*D_g*omega_fg^(1/3)*c^2
```

`f_n` è un parametro molto incerto. È uno dei principali parametri da calibrare.

### 4.6 Evoluzione `N_d`

Con densità di dislocazioni costante:

```text
dN_d/dt = -4*lambda*N_d^2*dV_d/dt
lambda = (2 - xi) / (2*(1 - xi)^3)
xi = V_d*N_d
```

Non aggiungere coalescenza bulk-bulk nel caso Rizk-base. Il termine di interazione bulk-dislocation di Barani/UO2 può essere esplorato solo come modello esteso, non come base.

### 4.7 Solver gas

Il solver 3x3 transiente va mantenuto. I test precedenti indicano che il problema principale non è il solver gas.

Per coerenza operator-splitting / SCIANTIX-like, nel caso base usare coefficienti old-step nel gas solver. Quindi `g_b` deve essere calcolato dallo stato noto all’inizio dello step, non ricalcolato dopo l’update di `N_b`, a meno che si stia facendo esplicitamente un test numerico semi-implicito.

---

## 5. Parametri da calibrare / esplorare

### 5.1 Parametri principali

Questi sono i parametri da esplorare prima:

```text
f_n        # nucleazione bulk, altamente incerto
K_d        # bubbles per dislocation length; N_d0 = K_d*rho_d
g_d_scale  # moltiplicatore diagnostico solo del trapping verso dislocazioni
```

Range raccomandati iniziali:

```text
f_n in [1e-10, 1e-6]  # scala logaritmica
K_d in [1e5, 8e5]     # m-1, con attenzione a 2e5-5e5
g_d_scale in [0.5, 10]
```

Valori nominali o già emersi:

```text
f_n nominale Rizk = 1e-6
K_d nominale Rizk = 5e5 m-1
K_d utile da test precedenti = circa 2e5-5e5 m-1
f_n utile da test precedenti = circa 1e-9-1e-8
```

### 5.2 Parametri secondari

Da esplorare solo dopo i principali:

```text
bulk_seed_radius_nm in [0, 1, 2, 3]
xe_yield in [0.24, 0.30, 0.475]
vacancy_diffusivity scale in [0.3, 1, 3]
resolution_sf in [0.3, 1, 3]
diffusivity_sf in [0.5, 1, 2]
```

Note:

- `bulk_seed_radius_nm` può migliorare la pressione bulk iniziale, ma non deve diventare un trucco per fit dello swelling. Il caso base deve usare `0`.
- `xe_yield = 0.24` è il riferimento Xe-equivalente. Usare `0.475` solo come sensitivity per includere gas/volatili più ampi, dichiarandolo esplicitamente.
- Non usare simultaneamente troppi scaling factor, altrimenti il fit diventa non identificabile.

---

## 6. Dati da fittare

L’agente deve usare i dati digitalizzati già inseriti nel codice, in particolare:

```text
EXP_SWELLING_T
EXP_SWELLING_BURNUP_1600
EXP_ND_T_13
EXP_RD_T_13
```

Priorità dei target:

1. `EXP_SWELLING_T` contro `swelling_d_percent`.
2. `EXP_ND_T_13` contro `Nd`.
3. `EXP_RD_T_13` contro `Rd_nm`.
4. `EXP_SWELLING_BURNUP_1600` contro `swelling_d_percent`.
5. Diagnostica alternativa: confrontare `EXP_SWELLING_T` anche con `swelling_ig_percent`, ma non usarlo come unico target.

Temperature sopra 1800 K vanno trattate con cautela, perché il modello Python corrente non include un modello completo di grain-boundary bubbles / interconnection / release. Se lo score globale è dominato da esplosioni a 1900-2000 K, fare anche uno score ristretto a:

```text
T <= 1800 K
```

---

## 7. Vincoli e diagnostiche fisiche

### 7.1 Pressioni

Usare le pressioni come diagnostica secondaria:

```text
p_b_over_eq = p_b / p_b_eq
p_d_over_eq = p_d / p_d_eq
```

Target qualitativo:

- bulk e dislocation bubbles dovrebbero restare circa vicino all’equilibrio, soprattutto a media temperatura;
- grain-boundary bubbles, non implementate nel Python attuale, sono quelle che in Rizk restano sovrapressurizzate.

Penalità suggerita:

```text
per 1200 K <= T <= 1800 K:
  penalizza se p_b/p_eq fuori da [0.3, 3]
  penalizza se p_d/p_eq fuori da [0.3, 3]

per T < 1200 K:
  tollerare maggiore deviazione, ma segnalarla.

per T > 1800 K:
  non usare la pressione come vincolo forte finché manca release/intergranulare.
```

### 7.2 Gas partition

Stampare sempre:

```text
matrix_gas_percent
bulk_gas_percent
dislocation_gas_percent
qgb_gas_percent
```

Diagnostica qualitativa da Rizk:

- sotto circa 1800 K, gran parte del gas è nelle bulk/intragranular bubbles;
- sopra circa 1800 K, il gas si sposta maggiormente verso dislocation bubbles, grain-boundary bubbles e release;
- nel Python attuale `q_gb` è solo gas arrivato al grain face per bilancio, non un modello completo di grain-boundary bubbles/release.

### 7.3 Microstruttura

Controllare sempre:

```text
Nd(T, 1.3% FIMA)
Rd(T, 1.3% FIMA)
Nb(T, 1.3% FIMA)
Rb(T, 1.3% FIMA)
```

Se lo swelling torna ma `R_d` e `N_d` sono molto sbagliati, il fit non è fisico.

---

## 8. Funzione obiettivo raccomandata

Implementare almeno quattro score separati:

```text
score_sw_d     = RMSE su swelling_d_percent vs EXP_SWELLING_T
score_sw_ig    = RMSE su swelling_ig_percent vs EXP_SWELLING_T, solo diagnostico
score_Nd_log   = RMSE log10(Nd / N_exp)
score_Rd_log   = RMSE log10(Rd_nm / R_exp_nm)
score_pressure = penalità su p_b/p_eq e p_d/p_eq
```

Score principale:

```text
score_main = score_sw_d + 0.7*score_Nd_log + 0.7*score_Rd_log + 0.2*score_pressure
```

Score alternativo:

```text
score_alt_total_ig = score_sw_ig + 0.7*score_Nd_log + 0.7*score_Rd_log + 0.2*score_pressure
```

L’agente deve riportare entrambi, perché il confronto `swelling_d` vs `swelling_ig` è uno dei punti scientifici aperti.

---

## 9. Workflow autonomo richiesto all’agente

L’agente deve lavorare in modo iterativo e riproducibile.

### Step 1 — Validazione codice

1. Eseguire il file Python originale senza modifiche.
2. Verificare che non ci siano errori runtime.
3. Salvare log e output in una cartella:

```text
results_un_calibration/run_YYYYMMDD_HHMMSS/
```

### Step 2 — Baseline

Eseguire il caso baseline:

```text
f_n = 1e-6
K_d = 5e5
g_d_scale = 1
bulk_seed_radius_nm = 0
GRAIN_RADIUS = 6e-6
```

Produrre CSV con colonne:

```text
f_n, K_d, g_d_scale, seed_nm, T, burnup,
sw_b_percent, sw_d_percent, sw_ig_percent,
Nb, Nd, Rb_nm, Rd_nm,
p_b_over_eq, p_d_over_eq,
matrix_gas_percent, bulk_gas_percent, dislocation_gas_percent, qgb_gas_percent,
score components
```

### Step 3 — Sweep logaritmico grossolano

Eseguire una griglia iniziale:

```text
f_n = [1e-10, 3e-10, 1e-9, 3e-9, 1e-8, 3e-8, 1e-7, 3e-7, 1e-6]
K_d = [1e5, 2e5, 3e5, 5e5, 8e5]
g_d_scale = [0.5, 1, 2, 3, 5, 10]
seed_nm = 0
```

Ordinare i risultati per `score_main` e `score_alt_total_ig`.

### Step 4 — Raffinamento locale

Prendere i migliori 10 candidati e fare uno sweep locale più fine attorno a ciascuno:

```text
f_n: mezzo ordine di grandezza attorno al candidato
K_d: +/- 50%
g_d_scale: +/- 50%
```

### Step 5 — Test seed bulk

Solo sui migliori 5 candidati, testare:

```text
seed_nm = [0, 1, 2, 3]
```

Non scegliere un candidato solo perché il seed migliora la pressione. Il seed deve migliorare anche lo score sperimentale o restare diagnostico.

### Step 6 — Report finale

Produrre:

1. `best_candidates.csv`
2. `all_runs.csv`
3. `summary.md`
4. grafici PNG:
   - swelling vs T per 1.1, 1.3, 3.2% FIMA;
   - swelling vs burnup a 1600 K;
   - `N_d` vs T a 1.3% FIMA;
   - `R_d` vs T a 1.3% FIMA;
   - pressioni a 3.2% FIMA;
   - gas partition a 1.1 e 3.2% FIMA.

Nel report finale, l’agente deve spiegare:

- quali parametri sono stati cambiati;
- quale candidato è migliore per `swelling_d`;
- quale candidato è migliore per `swelling_ig`;
- se i candidati hanno microstruttura fisica plausibile;
- se le pressioni restano ragionevoli;
- se il modello fallisce sistematicamente sopra 1800 K per mancanza di release/intergranulare.

---

## 10. Criteri di accettazione

Un candidato è accettabile solo se:

1. `score_sw_d` migliora rispetto al baseline nominale;
2. `R_d` a 1.3% FIMA è dello stesso ordine dei dati, idealmente 50-200 nm nel range 1000-1750 K;
3. `N_d` resta nello stesso ordine dei dati, circa 1e18-5e19 m^-3;
4. `p_d/p_eq` e `p_b/p_eq` non sono estremi tra 1200 e 1800 K;
5. non produce swelling intragranulare fisicamente assurdo sotto 1800 K;
6. tutte le modifiche parametriche sono dichiarate.

---

## 11. Punti aperti da non nascondere

L’agente deve indicare chiaramente questi limiti:

1. Il modello Python attuale non include grain-boundary bubbles vere, interconnection e release.
2. `q_gb` è solo un termine di bilancio verso il grain face.
3. Sopra 1800-2000 K il modello può sovrastimare la crescita delle dislocation bubbles perché manca lo svuotamento verso grain boundaries/release.
4. `f_n` è incerto e sembra dominante, ma un fit troppo basso rispetto a `1e-6` va discusso.
5. `K_d` modifica direttamente il compromesso tra raggio e numero di dislocation bubbles.
6. `g_d_scale != 1` è una correzione empirica, non un parametro originale Rizk.
7. Se `swelling_ig` fitta meglio di `swelling_d`, il report deve discuterlo: potrebbe indicare ambiguità della misura, oppure una partizione bulk/dislocation non corretta nel modello Python.

---

## 12. Prompt breve da dare all’agente AI

Usa questo prompt con Codex/agent:

```text
Hai un file Python che implementa un modello UN/Rizk per fission gas bubbles. Devi calibrare automaticamente i parametri incerti per rappresentare i dati sperimentali digitalizzati.

Leggi il codice, non cambiare le equazioni fisiche base senza motivazione, e crea una pipeline riproducibile che esegua sweep/ottimizzazione su f_n, K_d e g_d_scale. Usa GRAIN_RADIUS=6e-6 m, FISSION_RATE=5e19, xe_yield=0.24, bulk_seed_radius_nm=0 come baseline. La priorità è fittare i dati sperimentali: Fig. 3/Fig. 6 swelling P2 contro swelling_d, Fig. 7 contro N_d, Fig. 8 contro R_d. Calcola anche lo score alternativo usando swelling_ig per verificare se il totale intragranulare rappresenta meglio i punti.

Usa le pressioni p_b/p_eq e p_d/p_eq solo come diagnostica secondaria: in Rizk bulk e dislocation bubbles restano circa near-equilibrium, mentre le grain-boundary bubbles sono sovrapressurizzate. Non penalizzare eccessivamente T>1800 K perché nel codice manca un modello completo di grain-boundary bubbles/release.

Produci all_runs.csv, best_candidates.csv, summary.md e grafici PNG. Itera autonomamente: prima griglia grossolana, poi raffinamento locale sui migliori candidati, poi test seed bulk solo sui migliori. Spiega quali parametri sono robusti, quali sono incerti, quale candidato è migliore per swelling_d, quale per swelling_ig, e quali limiti fisici restano.
```


---

# 13. Aggiornamento operativo — calibrazione v5 dopo analisi v3/v4

## 13.1 Cosa abbiamo imparato dai run recenti

Sono stati eseguiti run `capture_only` con scoring progressivamente più fisico:

1. **v2 / Codex fixed-Dv profile**
   - `capture_only` riesce a fittare abbastanza bene P2.
   - I candidati a `Dv_scale` basso possono avere score numerico buono, ma spesso pressione dislocation troppo alta.
   - Il candidato più fisico emerso era vicino a `Dv_scale = 1`, ma aveva gas partition non soddisfacente.

2. **v3 / gas partition score**
   - Aggiungere vincoli su gas partition e `q_gb` ha funzionato: il modello ha spostato gas da `q_gb` verso bulk/dislocation in modo più simile a Rizk Fig. 9.
   - Però i migliori candidati v3 hanno pagato questo risultato con un raggio medio delle dislocation bubbles troppo grande:
     ```text
     Rd(2000 K, 1.3% FIMA) ~ 2 micrometri
     ```
   - Questo non è accettabile per un single-size average radius.

3. **v4 / radius guard**
   - Aggiungere il vincolo `Rd(2000 K) <= 800 nm` ha ridotto molto il problema.
   - I top-score finali restano però ancora attorno a:
     ```text
     Rd(2000 K) ~ 1.3-1.6 micrometri
     ```
   - Esistono candidati più fisici, non scelti dal ranking totale:
     ```text
     trial_00150: Rd(2000 K) ~ 785 nm
     trial_00163: Rd(2000 K) ~ 705 nm
     trial_00170: Rd(2000 K) ~ 992 nm
     ```
   - Questi candidati hanno raggio molto più ragionevole, ma mostrano ancora due problemi:
     - il raggio `R_d(T)` continua a crescere dopo 1800 K invece di tendere a saturare;
     - la pressione dislocation resta sopra equilibrio, mentre bulk pressure è quasi in equilibrio.

## 13.2 Nuovo obiettivo v5

Lo scopo di `v5` non è solo abbassare lo score totale, ma cercare candidati con forma fisica migliore:

```text
1. fit P2/dislocation swelling ragionevole fino a ~1700 K;
2. R_d e N_d nello stesso ordine di grandezza dei dati;
3. gas partition simile a Rizk:
   - bulk gas alto a bassa/intermedia T;
   - dislocation gas dominante ad alta T;
   - q_gb moderato, interpretato come GB bubbles + FGR;
4. pressione bulk e dislocation non estrema;
5. R_d(2000 K) sub-micrometrico;
6. R_d(T) con tendenza alla saturazione/flattening dopo ~1800 K, non crescita accelerata.
```

Il punto nuovo è il 6.

## 13.3 Scoring v5

Rispetto a v4, `UN_M7_optuna_calibration_v5.py` aggiunge:

```text
score_radius_saturation
```

che penalizza:

```text
- crescita eccessiva R_d(1800 -> 2000 K);
- crescita accelerata R_d(1900 -> 2000 K) rispetto a R_d(1800 -> 1900 K);
- rapporto R_d(2000)/R_d(1900) troppo alto;
- raggio 1900/2000 K sopra valori sub-micrometrici.
```

Default indicativi:

```text
rd2000_max_nm = 800
rd1900_soft_max_nm = 700
rd1800_soft_max_nm = 600
rd_post1800_delta_max_nm = 350
rd_last_increment_factor_max = 0.75
rd_1900_2000_ratio_max = 1.35
radius_guard_weight = 0.55
radius_saturation_weight = 0.75
```

Interpretazione:

- non è un fit rigido alla curva Rizk;
- è un guardrail fisico per impedire che il modello single-size compensi lo score con raggi micrometrici;
- se questo vincolo distrugge il fit P2, il report deve dirlo chiaramente.

## 13.4 Criteri di selezione fisica

Codex/agent non deve scegliere solo `score_total`.

Deve sempre classificare i candidati in almeno queste categorie:

```text
best_total_score
best_physical_score
best_radius_saturated
best_partition
best_pressure
best_rizk_near
best_exploratory_escape
```

Un candidato è **più fisico** se, indicativamente:

```text
Rd_1p3_2000K <= 800-1000 nm
Rd_ratio_2000_over_1800 <= 2.8
Rd_inc_1900_2000 <= 0.75 * Rd_inc_1800_1900  # flattening
p_d_over_eq_1p3_1600K <= ~2-2.5
qgb_gas_1p1_1600K <= ~12%
bulk_gas_1p1_1200K >= ~70%
bulk_gas_1p1_1500K >= ~68-70%
swD_1p3_1600K circa 2-3%
R_d 1000-1750 K confrontabile coi dati P2
N_d 1000-1750 K nello stesso ordine dei dati P2
```

Se nessun candidato soddisfa tutto, non nascondere il fallimento. Dire quale vincolo è in conflitto con gli altri.

## 13.5 Strategia Codex a step

Codex deve lavorare a blocchi, non lanciare una singola ottimizzazione cieca.

### Block A — pilot v5

Run consigliato:

```text
family = capture_only
n_trials = 250-400
no plots
score v5 standard
```

Obiettivo:
- verificare se v5 trova candidati non peggiori di v4;
- capire se il vincolo di saturazione è troppo duro;
- ispezionare componenti dello score.

### Block B — continuation

Se il pilot produce candidati ragionevoli, continuare nella stessa cartella/stesso database:

```text
+800/+1200 trial aggiuntivi
no plots
```

Non ripartire da zero.

### Block C — final rerun e plot

Selezionare non solo i primi per `score_total`, ma anche i migliori fisici:

```text
top 5 per score_total
top 5 per filtro fisico
top 3 vicino a Rizk
top 3 pressure-friendly
top 3 radius-saturated
```

Fare rerun finale con:

```text
final_dt_h = 1 h
final_n_modes = 40
plots enabled
```

### Block D — escape studies

Se i risultati non sono soddisfacenti, non cambiare subito le equazioni. Fare blocchi separati:

```text
D1: nominal-near
    prior più forte, parametri restano vicino a Rizk.

D2: diffusion escape
    Dv_scale, Dg_scale, D2_xe_scale più liberi.

D3: resolution/trapping escape
    b_scale, gb_scale, gd_scale più liberi.

D4: microstructure escape
    f_n, K_d, rho_d più liberi.

D5: coalescence/capture escape
    coalescence_d_scale, capture_scale più liberi.

D6: all-wide
    tutti larghi, ma prior non nullo.
```

Per ogni blocco, creare una sottocartella e un breve paragrafo in `WORKLOG.md`.

## 13.6 Regole sui pesi dello score

Codex può cambiare i pesi solo seguendo questo protocollo:

```text
1. completare un pilot;
2. leggere componenti score e plot diagnostici;
3. scrivere in WORKLOG.md perché un peso sembra troppo forte/debole;
4. creare un nuovo blocco con nome chiaro;
5. non sovrascrivere il blocco precedente.
```

Non è permesso:
- rimuovere pressure score;
- rimuovere qgb/gas partition score;
- rimuovere radius/saturation score;
- scegliere un candidato solo perché minimizza `score_total`;
- tacere se il candidato usa parametri molto lontani da Rizk.

## 13.7 Output richiesto per v5

Creare almeno:

```text
UN_M7_v5_codex_report.md
UN_M7_v5_codex_results/WORKLOG.md
UN_M7_v5_codex_results/best_candidates_all_categories.csv
UN_M7_v5_codex_results/physical_filtered_candidates.csv
UN_M7_v5_codex_results/score_component_summary.csv
UN_M7_v5_codex_results/escape_study_summary.csv
```

Per ogni candidato finale importante, riportare:

```text
label
score_total
score_swd
score_Rd
score_Nd
score_pressure
score_partition
score_qgb
score_radius_guard
score_radius_saturation
score_rizk_prior

f_n
K_d
rho_d
fission_rate
Dv_scale
Dg_scale
D2_xe_scale
b_scale
gb_scale
gd_scale
coalescence_d_scale
capture_scale

swD_1p3_1600K
swB_1p3_1600K
Rd_1p3_1600K
Rd_1p3_1800K
Rd_1p3_1900K
Rd_1p3_2000K
Rd_ratio_2000_over_1800
Rd_inc_1800_1900_nm
Rd_inc_1900_2000_nm
Nd_1p3_1600K
p_b_over_eq_1p3_1600K
p_d_over_eq_1p3_1600K
bulk_gas_1p1_1200
bulk_gas_1p1_1500
bulk_gas_1p1_1600
disl_gas_1p1_2000
qgb_gas_1p1_1600
```

## 13.8 Conclusione scientifica da verificare

La domanda centrale dopo v5 è:

```text
È possibile ottenere simultaneamente:
- gas partition ragionevole;
- pressione dislocation non estrema;
- R_d sub-micrometrico e tendente a saturare;
- fit P2 accettabile;
senza parametri assurdi?
```

Se sì, il candidato v5 è forte.

Se no, il report deve dire se il conflitto nasce principalmente da:
- coalescenza dislocation troppo aggressiva;
- trapping verso dislocation troppo forte;
- capture bulk→dislocation;
- mancanza del modello grain-boundary/FGR;
- problema nei parametri Rizk;
- limite del single-size model.
