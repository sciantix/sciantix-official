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
