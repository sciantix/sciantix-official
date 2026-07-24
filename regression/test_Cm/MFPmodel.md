# Modello Cm / 5MP (Metallic Fission Products) — Equazioni e domande per Vittoria

File di riferimento: `src/models/MetallicFissionProducts.C`
Flag di attivazione: `iCm` (in `input_settings.txt`)

### 1. Panoramica

Il modello segue il seguente percorso per i prodotti di fissione metallici (5MP):

```
produzione (fissione)
        │
        ▼
   Cm (totale, monotono crescente)          [riga 55-60]
        │
        ▼
   Cm_matrix (frazione libera in matrice) ──┬──► Cm_precipitated_intragranular
        │  ▲                                │         │
        │  │ k_res (rientro in soluzione)   │         ▼  (n atomi/particella)
        │  └────────────────────────────────┘   Intragranular atom per 5MP (n)
        └──────────────────────────────────► Cm_precipitated_grain_boundary

   Intragranular 5MPs concentration (N)
```

Variabili di stato in at/m³
- `Cm` — produzione cumulata totale
- `Cm matrix` — concentrazione libera in soluzione nella matrice
- `Cm precipitated intragranular` — precipitato dentro i grani
- `Cm precipitated grain boundary` — precipitato ai bordi grano
- `Intragranular 5MPs concentration` (N) — densità numerica di particelle 5MP nucleate, 1/m³
- `Intragranular atom per 5MP` (n) — atomi medi per particella, adimensionale

### 2. Equazioni

#### 2.1 Produzione totale — Cm

$$
\frac{dC_m}{dt} = y \cdot F
\qquad\Longrightarrow\qquad
C_m(t+\Delta t) = C_m(t) + y \, F \, \Delta t
$$

- $y = 0.6$ — fission yield efficace per una "5MP".
- $F$ = `history_variable["Fission rate"]` (fiss/m³/s).
- $\Delta t$ = `physics_variable["Time step"]`.

#### 2.2 Sistema Cm_matrix / precipitati — Eulero implicito 3×3  (righe 109-140)

$$
\begin{aligned}
\frac{dC_{m,\text{matrix}}}{dt} &= y F - (k_{intra}+k_{gb})\,C_{m,\text{matrix}} + k_{res}\left(C_{m,\text{intra}} + C_{m,gb}\right) \\
\frac{dC_{m,\text{intra}}}{dt}  &= k_{intra}\,C_{m,\text{matrix}} - k_{res}\,C_{m,\text{intra}} \\
\frac{dC_{m,gb}}{dt}            &= k_{gb}\,C_{m,\text{matrix}} - k_{res}\,C_{m,gb}
\end{aligned}
$$

risolto in forma implicita $A\,x = b$:

$$
\begin{bmatrix}
1+(k_{intra}+k_{gb})\Delta t & -k_{res}\Delta t & -k_{res}\Delta t \\
-k_{intra}\Delta t & 1+k_{res}\Delta t & 0 \\
-k_{gb}\Delta t & 0 & 1+k_{res}\Delta t
\end{bmatrix}
\begin{bmatrix} C_{m,\text{matrix}}^{n+1} \\ C_{m,\text{intra}}^{n+1} \\ C_{m,gb}^{n+1} \end{bmatrix}
=
\begin{bmatrix} C_{m,\text{matrix}}^{n} + yF\Delta t \\ C_{m,\text{intra}}^{n} \\ C_{m,gb}^{n} \end{bmatrix}
$$

risolto da `solver.Laplace3x3(A_matrix, b_matrix)`.

Coefficienti:

$$
k_{intra} = \underbrace{0.241313858 \times 1.100694 \times 10^{-1} \times 9.261187\times10^{-1} \times 1.047129}_{\text{= } 2.5758\times10^{-2}\ \text{s}^{-1}} \times sf_{intra}
$$

$$
k_{gb} = \underbrace{0.944860042 \times 4.641589\times10^{-2} \times 8.413951\times10^{-1} \times 9.120108\times10^{-1}}_{\text{= } 3.3654\times10^{-2}\ \text{s}^{-1}} \times sf_{gb}
$$

$$
k_{res} = \underbrace{6.799\times10^{-19}}_{k_{res,ref}} \cdot \frac{F}{\underbrace{1.48\times10^{19}}_{F_{ref}}} \times sf_{res}
$$

#### 2.3 Nucleazione eterogenea — `Intragranular 5MPs concentration` (N)

Siti di nucleazione (dislocazioni + bolle intragranulari), densità pesata:

$$
S_{siti} = \rho_{disl} + S_{bub}, \qquad S_{bub} = 4\pi \, n_{bub}\, r_{bub} \ \ (\text{m}^{-2})
$$

con densità di dislocazioni a gradino in funzione della temperatura.

Siti già "occupati" dalle particelle 5MP esistenti (raggio $R_{5M}$ dal volume atomico efficace $V_{eff,5M}$ e da $n$ al passo precedente):

$$
R_{5M} = \left(\frac{3}{4\pi} V_{eff,5M}\, n^{\,i}\right)^{1/3},
\qquad
S_{5MP} = 4\pi R_{5M}\, N^{\,i}
$$

$$
\text{disponibili} = \max\!\big(S_{siti} - S_{5MP},\ 0\big)
$$

Tasso di nucleazione e aggiornamento implicito di $N$:

$$
k_{nucl} = \underbrace{3.995668\times10^{18} \times 7.498942\times10^{-2} \times 7.498942\times10^{-1} \times 1.819783\times10^{-1}}_{\text{= } 4.0889\times10^{16}\ \text{atm/(m s)}} \times sf_{nucl}
$$

$$
\Delta G_{nucl} = \underbrace{2.9 \times 0.95}_{\text{= } 2.755\ \text{eV}} \times sf_{\Delta G}
$$

$$
k' = k_{nucl}\,\Delta t \, \exp\!\left(-\frac{\Delta G_{nucl}}{k_B T}\right)
$$

$$
N^{n+1} = \frac{k'\,\big(f_{disl}\,\rho_{disl} + f_{bub}\,S_{bub}\big)\cdot\text{disponibili} \;+\; N^{n}}{1 + k'}
\qquad (f_{disl}=0.67,\ f_{bub}=0.33)
$$

#### 2.4 Numero medio di atomi per particella — `Intragranular atom per 5MP` (n)

$$
n = \begin{cases} \dfrac{C_{m,\text{intra}}^{\,n+1}}{N^{\,n+1}} & N^{n+1} > 0 \\[4pt] 0 & N^{n+1} = 0 \end{cases}
$$

### 3. Scaling factors

| # | Nome |  | | Default |
|---|---|---|---|---|
| 9  | MFP nucleation rate | `sf_mfp_nucleation_rate` | $k_{nucl}$ | 1.0 |
| 10 | MFP precipitation rate intragranular | `sf_mfp_precipitation_rate_intragranular` | $k_{intra}$ | 1.0 |
| 11 | MFP precipitation rate grain boundary | `sf_mfp_precipitation_rate_grain_boundary` | $k_{gb}$ | 1.0 |
| 12 | MFP resolution rate | `sf_mfp_resolution_rate` | $k_{res}$ | 1.0 |
| 13 | MFP nucleation energy barrier | `sf_mfp_nucleation_energy_barrier` | $\Delta G_{nucl}$ | 1.0 |

---
### 4. Domande per Vittoria

#### D1. Riferimenti bibliografici mancanti
- $y = 0.6$
- $V_{eff,5M} = 1.44123\times10^{-29}\ \text{m}^3$

#### D2. Fattori di calibrazione concatenati
In $k_{intra}$, $k_{gb}$, $k_{nucl}$ e $\Delta G_{nucl}$ compaiono più fattori moltiplicativi in fila senza commento. Cosa rappresenta ciascun fattore? Sarebbe utile un commento (o un unico coefficiente consolidato, con riferimento al file di risultati corrispondente in `regression/test_py_k5MP_calibration/results/`) per poter tracciare e riprodurre la calibrazione in futuro.

#### D3. Densità di dislocazioni a gradini
$\rho_{disl}(T)$ è stata inserita come funzione a gradini. Nella referenza citata (*Modelling dislocation density evolution of UO2 under irradiation*, A. Djonovic) la dipendenza è una curva continua. Possiamo fornirti una funzione più continua e anche dipendente dal fission rate, se utile.

#### D4. Aggiornamento di N — denominatore e densità di siti — instabile
Nell'aggiornamento implicito di $N$:

$$
N^{n+1} = \frac{k'\,(f_{disl}\rho_{disl}+f_{bub}S_{bub})\cdot\text{disponibili} + N^{n}}{1+k'}
$$

- Il denominatore $(1+k')$ agisce come un termine di perdita per $N$, ma l'equazione dichiarata è solo $dN/dt = \text{tasso di nucleazione}$, senza meccanismo di perdita per $N$. È voluto? Corretto piu avanti.
- Nella stessa equazione, $S_{siti}$ compare due volte: non pesata dentro "disponibili" e   pesata ($f_{disl}$, $f_{bub}$) come prefattore del tasso. Forse era stato inserito per stabilizzare la soluzione (se i siti disponibili calano, cala anche il tasso di nucleazione, ricordo un nostro discorso a riguardo) ma vale la pena confermarlo.

**Oscillazioni** (`regression/test_Cm/output.txt`): $N$ e $n$ oscillano per tutta la durata della simulazione. Bisogna risolverlo prima di continuare la calibrazione. 

Due vincoli fisici mancanti nella formulazione:
1. **N non dovrebbe mai scendere** — cresce per nucleazione, e può scendere solo se un'intera particella si ridissolve (meccanismo non modellato).
2. **n non ha senso sotto 2 atomi/particella**.

Riscritta la fine di `MetallicFissionProducts.C` (dopo la soluzione del sistema 3×3, così il
cap può usare `Cm_precipitated_intragranular` appena calcolato):

```cpp
const double n_min = 2.0;
double N_candidate = N_iniziale +
    k_nucleazione * (f_dislocation*dislocation_density + f_bubbles*bubble_sink_strenght) * disponibili;
double N_cap   = b_matrix[1] / n_min;                       // Cm_precipitated_intragranular / n_min
double N_final = std::max(std::min(N_candidate, N_cap), N_iniziale);  // mai sotto N_iniziale
sciantix_variable["Intragranular 5MPs concentration"].setFinalValue(N_final);

double n_media = (N_final > 0.0) ? (b_matrix[1] / N_final) : 0.0;
sciantix_variable["Intragranular atom per 5MP"].setFinalValue(n_media);
```

N cresce solo per accumulo ed è comunque vincolata dal basso da `N_iniziale` (mai decrescente) e dall'alto da `Cm_precipitated_intragranular / n_min` (mai un numero medio di atomi/particella sotto 2). Il vecchio limite "siti disponibili" resta nel calcolo di `N_candidate` (quanto *vorrebbe* crescere N), ma il vero tetto ora è fisico e dimensionalmente coerente (entrambi in 5MP/m³).

Risultato di questo cambiamento: nuclea una popolazione nei primi momenti, poi la nucleazione si ferma del tutto (i siti si saturano) e tutta la produzione successiva fa crescere le particelle esistenti, non ne nuclea di nuove. Probabilmente riflette $k_{nucl}$ troppo grande: con $k_{nucl}$ calibrato più basso, ci si aspetterebbe una nucleazione più graduale nel tempo invece che quasi istantanea. 

#### D6. `S_siti` mischia due grandezze fisiche diverse — proposta

$S_{siti} = \rho_{disl} + S_{bub}$, con $S_{bub} = 4\pi\,n_{bub}\,r_{bub}$ (§2.3), non è
dimensionalmente/concettualmente corretto:

- $\rho_{disl}$ (m⁻²) è una densità di **lunghezza di linea** di dislocazione per volume.
- $S_{bub} = 4\pi R N$ è invece la **sink strength** della teoria del trapping per diffusione (F.S. Ham, 1958: `g = 4π·D_s·(R_b+r_reticolo)·N_b`). Nel
  modello Cm manca proprio `D_s`, quello che resta è solo il pezzo geometrico. Ma anche aggiungendo `D_s`, resterebbe concettualmente sbagliato, non descrive quanti siti di nucleazione offrono le bolle".

Proposta: portare entrambi i termini a una vera densità di siti per volume (m⁻³), così si possono sommare in modo dimensionalmente corretto:

$$
N_{siti} = f_{disl}\cdot\frac{\rho_{disl}}{\lambda} + f_{bub}\cdot n_{bub}
$$

- Per le bolle: usare direttamente `sciantix_variable["Intragranular bubble concentration"]` in bub/m³.
- Per le dislocazioni: dividere $\rho_{disl}$ (m⁻²) per una spaziatura caratteristica $\lambda$ (m) tra siti di nucleazione favorevoli lungo la linea di dislocazione, per ottenere anche qui un vero conteggio di siti per volume (m⁻²/m = m⁻³).

Nota: cambiando le unità dei due termini sommati, probabilmente anche $f_{disl}=0.67$ e $f_{bub}=0.33$ (e forse $k_{nucl}$ stesso) andranno ricalibrati? Non ancora implementato nel codice, fammi sapere cosa ne pensi.

#### D7. Script di calibrazione duplicati
`regression/test_py_k5MP_calibration/test2.py`, `test2_equal_weight_final_backup.py` e `test2_particle_fraction.py` sono tre script molto simili. Qual è la versione da mantenere? Le altre due possiamo toglierle?