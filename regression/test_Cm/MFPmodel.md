# Modello Cm / 5MP (Metallic Fission Products)

File di riferimento: `src/models/MetallicFissionProducts.C`
Flag di attivazione: `iCm` (in `input_settings.txt`)

### 1. Panoramica

Il modello segue il seguente percorso per i prodotti di fissione metallici (5MP):

```
produzione (fissione)
          │
          ▼
   Cm (totale, monotono crescente)
          |                                                                   
          └────────────────────────────┬──► Cm_matrix (frazione libera in matrice)
           ▲                           |                                                              
           |                           └────► Cm_precipitated_intragranular  --> Intragranular atom per 5MP (n)
           |                           |                                     --> Intragranular 5MPs concentration (N)
           |                           └────► Cm_precipitated_grain_boundary --> Intergranular atom per 5MP (n_gb)
           |                           |                                     --> Intragranular 5MPs concentration (N_gb)
           |                           |                              
           ▲                           │         
           │                           |
           └───────────────────────────┘   
            k_res (rientro in soluzione)

   
```

Variabili di stato 
- `Cm` — produzione cumulata totale, at/m³
- `Cm matrix` — concentrazione libera in soluzione nella matrice, at/m³
- `Cm precipitated intragranular` — precipitato dentro i grani, at/m³
- `Cm precipitated grain boundary` — precipitato ai bordi grano, at/m³
- `Intragranular 5MPs concentration` (N) — densità numerica di particelle 5MP nucleate in un grano, 1/m³
- `Intragranular atom per 5MP` (n) — atomi medi per particella intragranulare, adimensionale
- `Intragranular 5MPs concentration` (N_gb) — densità numerica di particelle 5MP nucleate a bordo grano, 1/m³
- `Intergranular atom per 5MP` (n_gb) — atomi medi per particella intergranulare, adimensionale

### 2. Equazioni

#### 2.1 Produzione totale — Cm

$$
\frac{dC_m}{dt} = y \cdot F
\qquad\Longrightarrow\qquad
C_m(t+\Delta t) = C_m(t) + y \, F \, \Delta t
$$

- $y = 0.2$ — fission yield efficace per una "5MP".
- $F$ = `history_variable["Fission rate"]` (fiss/m³/s).
- $\Delta t$ = `physics_variable["Time step"]`.

#### 2.2 Sistema Cm_matrix / precipitati — Eulero implicito 3×3

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
k_{intra} = \underbrace{1}_{\text{s}^{-1}} \times sf_{intra}
$$

$$
k_{gb} = \underbrace{1}_{\text{s}^{-1}} \times sf_{gb}
$$

$$
k_{res} = \underbrace{6.799\times10^{-19}}_{k_{res,ref;\text{s}^{-1}}} \cdot \frac{F}{\underbrace{1.48\times10^{19}}_{F_{ref}}} \times sf_{res}
$$



#### 2.3 Nucleazione eterogenea 
##### `Intragranular 5MPs concentration` (N_intra)

equazione

$$
\frac{dN_{intra}}{dt} = {k_{intra}} \times {Siti     \ disponibili}
$$

dove i siti disponibili sono aggiornati ogni volta sottraendo il numero di siti già occupati da una 5MP:

Siti disponibili per nuova nucleazione:
$$
{Siti     \ disponibili} = (Siti_{tot} - N_{iniziale_{intra}})
$$

- Siti di nucleazione (dislocazioni + bolle intragranulari), densità pesata con coefficienti ricavati da Imagej tramite analisi immagini sperimentali (sovrapposizione maschere Mo e Xe):

     $$
     Siti_{tot} = \rho_{disl}\times f_{disl} + \rho_{bubble_{intra}}\times f_{bubble_{intra}}, \ \ (\text{m}^{-3})
     $$
     $$
     (f_{disl}=0.67,\ f_{bub}=0.33)
     $$

     - densità di dislocazioni descritta da funzione continua in funzione di burnup e temperatura
       (*Modelling dislocation density evolution of UO2 under irradiation*, A. Djonovic) in \text{m}^{-2}, resa poi una densità effettiva dividendo per una lunghezza caratteristica \text lambda, \text {m}, (quante 5MPs possono nucleare lungo una dislocazione)

       $$ 
       \lambda = \text 15 \times {10}^{-9} 
       $$ 
       
       la lunghezza caratteristica è assunta 2 volte il raggio della 5MP più grande (ottenuto dalle osservazione sperimentali). \
       Il limite è l'impenetrabilità tra 2 5MPs
     - bubble density calcolata da sciantix

- Siti già "occupati" dalle particelle 5MP esistenti (numero 5MPs al passo precedente, indicato con n):

     $$
     N_{iniziale_{intra}} = N_{intra}^{n}
     $$


Il coefficiente di nucleazione ha forma Arrhenius

$$
k_{nucl} = \underbrace{1}_{\text{s}^{-1}} \times sf_{nucl}
$$

$$
\Delta G_{nucl} = \underbrace{1}_{\text{eV}} \times sf_{\Delta G}
$$

$$
k_{nucleazione} = k_{nucl}\,\Delta t \, \exp\!\left(-\frac{\Delta G_{nucl}}{k_B T}\right)
$$

Aggiornamento implicito di ${N_{intra}}$

$$
N_{intra}^{n+1} = \frac{k_{nucleazione} \times {Siti_{tot}} \times dt \;+\; N_{intra}^{n}}{1 + k_{nucleazione}\times dt}
$$

risolto da `solver.Decay`.

##### `Intergranular 5MPs concentration` (N_inter)

Stessa logica. Cambiano i isti disponibili
- Siti di nucleazione (bolle intergranulari, da sciantix):

     $$
     Siti_{tot} = \rho_{bubble_{inter}}, \ \ (\text{m}^{-3})
     $$

- Siti già occupati

     $$
     N_{iniziale_{inter}}=N_{inter}^{n}
     $$

Aggiornamento implicito di ${N_{inter}}$

$$
N_{inter}^{n+1} = \frac{k_{nucleazione} \times {Siti_{tot}} \times dt \;+\; N_{inter}^{n}}{1 + k_{nucleazione}\times dt}
$$

risolto da `solver.Decay`.

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


#### 4. Vincoli fisici

Sia per 5MPs intragranulari che per 5MPs a bordo grano:

1. **N non dovrebbe mai scendere** — cresce per nucleazione, e può scendere solo se un'intera particella si ridissolve (meccanismo non modellato).
2. **n non ha senso sotto 2 atomi/particella**.

```cpp
 const double n_min       = 2.0;
    double       N_candidate_intra = solver.Decay(
                                        N_iniziale_intra,
                                        k_nucleazione,
                                        k_nucleazione* (f_dislocation * dislocation_sites + f_bubbles * bubble_sites_intra),
                                        physics_variable["Time step"].getFinalValue()
    );

    double       N_candidate_inter = solver.Decay(
                                        N_iniziale_inter,
                                        k_nucleazione,
                                        k_nucleazione*bubble_sites_inter,
                                        physics_variable["Time step"].getFinalValue()
    );

    double N_cap_intra   = b_matrix[1] / n_min;  // minimo numero di atomi per 5MP
    double N_final_intra = std::max(std::min(N_candidate_intra, N_cap_intra), N_iniziale_intra);
    sciantix_variable["Intragranular 5MPs concentration"].setFinalValue(N_final_intra);

    double N_cap_inter   = b_matrix[2] / n_min;  // minimo numero di atomi per 5MP
    double N_final_inter = std::max(std::min(N_candidate_inter, N_cap_inter), N_iniziale_inter);
    sciantix_variable["Intergranular 5MPs concentration"].setFinalValue(N_final_inter);

    double n_media_intra = (N_final_intra > 0.0) ? (b_matrix[1] / N_final_intra) : 0.0;
    sciantix_variable["Intragranular atom per 5MP"].setFinalValue(n_media_intra);
    double n_media_inter = (N_final_inter > 0.0) ? (b_matrix[2] / N_final_inter) : 0.0;
    sciantix_variable["Intergranular atom per 5MP"].setFinalValue(n_media_inter);

```

N cresce solo per accumulo ed è comunque vincolata dal basso da `N_iniziale` (mai decrescente) e dall'alto da `Cm_precipitated_intragranular / n_min` (mai un numero medio di atomi/particella sotto 2). 

Risultato: nuclea una popolazione nei primi momenti, poi la nucleazione si ferma del tutto (i siti si saturano) e tutta la produzione successiva fa crescere le particelle esistenti, non ne nuclea di nuove. 
