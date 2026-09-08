# Modello Cm / 5MP (Metallic Fission Products)

File di riferimento: `src/models/MetallicFissionProducts.C`

Flag di attivazione: `iCm` (in `input_settings.txt`)

## 1. Obiettivo fisico del modello

Durante l'irraggiamento del combustibile UO$_2$, i prodotti di fissione metallici Mo, Tc, Ru, Rh e Pd vengono progressivamente generati con un tasso proporzionale al fission rate.

Nel modello i cinque elementi non vengono seguiti separatamente, ma sono rappresentati come un'unica specie metallica efficace, indicata come 5MP-forming inventory. Gli atomi prodotti possono:

- rimanere dispersi nella matrice di UO$_2$;
- formare precipitati intragranulari;
- formare precipitati ai bordi di grano;
- rientrare nella matrice attraverso irradiation-induced re-solution.

Il modello segue contemporaneamente:

1. il bilancio della massa metallica;
2. il numero di particelle;
3. la dimensione media delle particelle.

Queste quantità non sono ridondanti. A parità di massa precipitata, infatti, un'elevata densità numerica porta a molte particelle piccole, mentre una densità numerica più bassa porta a un numero minore di particelle più grandi.

Schema concettuale:

```text
produzione (fissione)
          │
          ▼
   Cm (totale, monotono crescente)
          |
          └────────────────────────────┬──► Cm_matrix (frazione libera in matrice)
           ▲                           |
           |                           └────► Cm_prec_intra --> Intragranular atom per 5MP (n)
           |                           |                    --> Intragranular 5MPs concentration (N)
           |                           |
           |                           └────► Cm_prec_gb --> Intergranular atom per 5MP (n_gb)
           |                           |                 --> Intergranular 5MPs concentration (N_gb)
           │                           |
           └───────────────────────────┘
           k_res (rientro in soluzione)
```

### Variabili principali

- `Cm` — inventario metallico totale prodotto, at/m³
- `Cm matrix` — concentrazione metallica libera nella matrice, at/m³
- `Cm precipitated intragranular` — concentrazione metallica precipitata intragranularmente, at/m³
- `Cm precipitated grain boundary` — concentrazione metallica precipitata ai bordi di grano, at/m³
- `Intragranular 5MPs concentration` — $N_{\mathrm{intra}}$, $m^{-3}$
- `Intergranular 5MPs concentration` — $N_{\mathrm{GB}}$, $m^{-3}$
- `Intragranular atom per 5MP` — $n_{\mathrm{intra}}$, atomi/particella
- `Intergranular atom per 5MP` — $n_{\mathrm{GB}}$, atomi/particella

---

## 2. Produzione e conservazione dell'inventario metallico

La produzione totale dei cinque elementi è descritta attraverso l'effective lumped production yield

$$
y_{\mathrm{5MP}} = 0.578 \qquad \mathrm{atoms/fission}.

$$

Il valore deriva dalla composizione PRODHEL del combustibile considerato:

$$
x_{\mathrm{5MP}}
=
x_{\mathrm{Mo}}+
x_{\mathrm{Tc}}+
x_{\mathrm{Ru}}+
x_{\mathrm{Rh}}+
x_{\mathrm{Pd}}
=
0.289,

$$

e, assumendo circa due fission-product atoms per fission,

$$
y_{\mathrm{5MP}}
=
2x_{\mathrm{5MP}}
=
0.578.

$$

Il termine di produzione è quindi

$$
\frac{dC_{\mathrm{5MP}}}{dt}
=
y_{\mathrm{5MP}}F.

$$

Durante un timestep:

$$
\Delta C_{\mathrm{prod}}
=
y_{\mathrm{5MP}}F\Delta t.

$$

L'inventario totale è suddiviso in:

$$
C_{\mathrm{5MP}}
=
C_m^{\mathrm{matrix}}
+
C_m^{\mathrm{intra}}
+
C_m^{\mathrm{GB}}.

$$

Produzione, nucleazione, precipitazione e re-solution vengono implementate in modo conservativo: l'unico termine che crea nuova massa metallica è la fissione. Tutti gli altri processi trasferiscono atomi tra matrice e precipitati.

---

## 3. Concentrazione di equilibrio e supersaturazione

La nucleazione non è descritta attraverso una barriera costante. Il driving force dipende dalla supersaturazione della matrice.

Si introduce una concentrazione di equilibrio efficace:

$$
C_{\mathrm{eq}}(T)
=
C_{\mathrm{eq,ref}}
\exp
\left[
-\frac{H_{\mathrm{sol}}^{\mathrm{eff}}}{k_B}
\left(
\frac{1}{T}
-
\frac{1}{T_{\mathrm{ref}}}
\right)
\right].

$$

Nel modello:

$$
T_{\mathrm{ref}}
=
2173.15\ \mathrm{K},

$$

$$
C_{\mathrm{eq,ref}}
=
1.462\times10^{24}
\ \mathrm{at\,m^{-3}},

$$

$$
H_{\mathrm{sol}}^{\mathrm{eff}}
=
1.14\ \mathrm{eV}.

$$

Il riferimento di concentrazione deriva dal limite superiore di solubilità del Mo in $UO_2$ stechiometrico a 1900 °C, mentre 1.14 eV è usato come effective solution enthalpy per la specie metallica lumped.

La supersaturazione è:

$$
S
=
\frac{C_m^{\mathrm{matrix}}}
{C_{\mathrm{eq}}(T)}.

$$

Se

$$
S\leq1,

$$

la nucleazione è posta uguale a zero.

Per

$$
S>1,

$$

il chemical driving force per atomo è

$$
\Delta\mu
=
k_BT\ln S.

$$

---

## 4. Nucleazione CNT-based

La barriera di nucleazione è descritta in forma efficace a partire dalla dipendenza della classical nucleation theory:

$$
\Delta G_j^*
=
\frac{B_j}
{T^2(\ln S)^2},
\qquad
j=\mathrm{intra},\mathrm{GB}.

$$

Nel codice:

$$
B_j
=
B_{\mathrm{ref}}\,sf_{B,j},

$$

con

$$
B_{\mathrm{ref}}
=
10^6\ \mathrm{eV\,K^2}.

$$

`B_ref` è una normalizzazione utilizzata dall'interfaccia degli scaling factors. Non va interpretato come parametro fisico misurato indipendentemente.

Il coefficiente di nucleazione è:

$$
k_{\mathrm{nucl},j}
=
k_{\mathrm{nucl},0,j}
\exp
\left(
-\frac{\Delta G_j^*}{k_BT}
\right).

$$

Nel modello finale i prefattori di nucleazione sono mantenuti al valore di riferimento:

$$
sf_{k_{\mathrm{nucl,intra}}}
=
sf_{k_{\mathrm{nucl,GB}}}
=
1.

$$

---

## 5. Siti di nucleazione intragranulari

Per la popolazione intragranulare vengono considerati due contributi:

- dislocazioni;
- bolle di gas intragranulari.

La densità di dislocazioni è calcolata in funzione di burnup e temperatura:

$$
\rho_d
=
AB^nf_T,

$$

con

$$
f_T
=
A_\infty
+
\frac{1-A_\infty}
{1+\exp[(T-T_c)/\Delta T]}.

$$

I parametri utilizzati sono:

$$
A=6.545\times10^{12},
\qquad
n=1.151,

$$

$$
A_\infty=0.608,
\qquad
T_c=1109\ \mathrm{K},
\qquad
\Delta T=25.8\ \mathrm{K}.

$$

Poiché $\rho_d$ ha dimensioni $m^{-2}$, viene convertita in densità volumetrica di siti attraverso una lunghezza caratteristica

$$
\lambda
=
15\times10^{-9}\ \mathrm{m}.

$$

Quindi:

$$
N_{\mathrm{site,disl}}
=
\frac{\rho_d}{\lambda}.

$$

La densità totale efficace di siti intragranulari è:

$$
N_{\mathrm{site,intra}}
=
f_d\frac{\rho_d}{\lambda}
+
f_bN_{\mathrm{b,intra}},

$$

con

$$
f_d=0.67,
\qquad
f_b=0.33.

$$

I coefficienti 0.67 e 0.33 sono effective weights ricavati dall'analisi dell'associazione tra precipitati metallici e bolle. Non devono essere interpretati come probabilità dirette di nucleazione sui due tipi di siti.

L'evoluzione della concentrazione numerica intragranulare è:

$$
\frac{dN_{\mathrm{intra}}}{dt}
=
k_{\mathrm{nucl,intra}}
\left(
N_{\mathrm{site,intra}}
-
N_{\mathrm{intra}}
\right).

$$

Il termine tra parentesi introduce la saturazione progressiva dei siti disponibili.

---

## 6. Siti di nucleazione ai bordi di grano

Nel modello attuale le bolle intergranulari non vengono utilizzate come popolazione di siti per la nucleazione delle 5MP ai bordi di grano.

Il grain boundary viene invece trattato direttamente come superficie eterogenea.

Per grani sferici di raggio $a$:

$$
\frac{A_{\mathrm{GB}}}{V}
=
\frac{3}{2a}.

$$

Il fattore $1/2$ tiene conto del fatto che ogni interfaccia è condivisa da due grani.

Assumendo una spaziatura caratteristica $\lambda$ tra siti sulla superficie bidimensionale del bordo di grano, la densità areale di siti è proporzionale a

$$
\frac{1}{\lambda^2}.

$$

La densità volumetrica equivalente diventa quindi:

$$
N_{\mathrm{site,GB}}
=
\frac{3}{2a\lambda^2}.

$$

Con

$$
\lambda=15\ \mathrm{nm}.

$$

L'evoluzione della popolazione intergranulare è:

$$
\frac{dN_{\mathrm{GB}}}{dt}
=
k_{\mathrm{nucl,GB}}
\left(
N_{\mathrm{site,GB}}
-
N_{\mathrm{GB}}
\right).

$$

---

## 7. Dimensione critica del nucleo

La massa assegnata a una nuova particella non è più fissata sempre a due atomi.

Per la forma capillare della CNT:

$$
n_j^*
=
\frac{2\Delta G_j^*}{\Delta\mu}.

$$

Nel codice viene utilizzato:

$$
n_{\mathrm{nucl},j}
=
\max
\left(
2,
n_j^*
\right).

$$

Il valore 2 rappresenta soltanto un floor numerico e atomistico. Non viene interpretato come dimensione critica universale del nucleo.

Se durante il timestep vengono richieste

$$
\Delta N_j

$$

nuove particelle, la massa metallica associata alla nucleazione è:

$$
\Delta C_{\mathrm{nucl},j}^{\mathrm{req}}
=
n_{\mathrm{nucl},j}
\Delta N_j.

$$

---

## 8. Vincolo imposto dalla massa disponibile

La nucleazione non può trasferire più atomi di quelli presenti nella matrice.

La quantità disponibile è:

$$
C_{\mathrm{available}}
=
C_m^{\mathrm{matrix,old}}
+
\Delta C_{\mathrm{prod}}.

$$

Deve essere rispettato:

$$
\Delta C_{\mathrm{nucl,intra}}
+
\Delta C_{\mathrm{nucl,GB}}
\leq
C_{\mathrm{available}}.

$$

Se la richiesta totale supera la massa disponibile, le due masse richieste vengono ridotte proporzionalmente.

Il numero effettivo di nuclei formati viene poi ricalcolato come:

$$
\Delta N_j^{\mathrm{actual}}
=
\frac{\Delta C_{\mathrm{nucl},j}^{\mathrm{actual}}}
{n_{\mathrm{nucl},j}}.

$$

In questo modo il bilancio di popolazione e il bilancio di massa rimangono coerenti.

---

## 9. Vincolo sulla diminuzione di N

Il modello attuale non include:

- coalescenza;
- total dissolution;
- esplicita scomparsa di singole particelle.

Per questo motivo il numero di particelle non può diminuire durante il timestep:

$$
\Delta N_j
=
\max
\left(
N_j^{\mathrm{candidate}}
-
N_j^{\mathrm{old}},
0
\right).

$$

La re-solution può diminuire la massa precipitata e quindi il raggio medio, ma non diminuisce direttamente $N_j$.

---

## 10. Precipitazione dei prodotti metallici

La precipitazione avviene solo quando la matrice è supersatura rispetto alla concentrazione di equilibrio efficace.

Per ciascuna popolazione:

$$
J_{\mathrm{prec},j}
=
k_j
\max
\left[
C_m^{\mathrm{matrix}}
-
C_{\mathrm{eq}}(T),
0
\right].

$$

Questa formulazione evita di far continuare la precipitazione quando la concentrazione in matrice è inferiore a $C_{\mathrm{eq}}$.

Non viene però introdotto un termine di thermal dissolution quando

$$
C_m^{\mathrm{matrix}}<C_{\mathrm{eq}}.

$$

La dissoluzione descritta nel modello resta quella irradiation-induced attraverso $k_{\mathrm{res}}$.

I coefficienti di precipitazione sono:

$$
k_{\mathrm{intra}}
=
k_{\mathrm{intra},0}
\exp
\left(
-\frac{\Delta G_{\mathrm{intra}}}{k_BT}
\right)
4\pi N_{\mathrm{intra}}R_{\mathrm{intra}},

$$

$$
k_{\mathrm{GB}}
=
k_{\mathrm{GB},0}
\exp
\left(
-\frac{\Delta G_{\mathrm{GB}}}{k_BT}
\right)
4\pi N_{\mathrm{GB}}R_{\mathrm{GB}}.

$$

Il termine

$$
4\pi NR

$$

rappresenta la dipendenza della sink strength da numero e dimensione delle particelle.

Poiché $NR$ ha dimensioni $m^{-2}$, il prefattore $k_0$ ha dimensioni $m^{-2}/s$, così che il coefficiente complessivo $k_j$ abbia dimensioni $s^{-1}$.

---

## 11. Irradiation-induced re-solution

La re-solution trasferisce atomi dai precipitati alla matrice:

$$
J_{\mathrm{res,intra}}
=
k_{\mathrm{res}}
C_m^{\mathrm{intra}},

$$

$$
J_{\mathrm{res,GB}}
=
k_{\mathrm{res}}
C_m^{\mathrm{GB}}.

$$

Il coefficiente di riferimento è:

$$
k_{\mathrm{res,ref}}
=
3.085078\times10^{-6}\ \mathrm{s^{-1}}.

$$

Il valore è ricavato dal restringimento delle 5MP osservato durante irraggiamento ionico, assumendo particelle sferiche e quindi

$$
n\propto R^3.

$$

La relazione utilizzata è:

$$
k_{\mathrm{res}}
=
-\frac{3}{\Delta t}
\ln
\left(
\frac{R_1}{R_0}
\right).

$$

Nel codice il coefficiente viene scalato con il fission rate:

$$
k_{\mathrm{res}}
=
k_{\mathrm{res,ref}}
\frac{F}{F_{\mathrm{ref}}}
sf_{\mathrm{res}},

$$

con

$$
F_{\mathrm{ref}}
=
1.48\times10^{19}
\ \mathrm{fissions\,m^{-3}\,s^{-1}}.

$$

Nella calibrazione finale:

$$
sf_{\mathrm{res}}=1.

$$

---

## 12. Numero medio di atomi e raggio equivalente

Per ogni popolazione:

$$
n_j
=
\frac{C_m^j}{N_j}.

$$

Il raggio sferico equivalente è:

$$
R_j
=
\left(
\frac{3n_j\Omega_{\mathrm{5MP}}}
{4\pi}
\right)^{1/3},

$$

con

$$
\Omega_{\mathrm{5MP}}
=
1.44123\times10^{-29}
\ \mathrm{m^3/atom}.

$$

Il raggio è una quantità equivalente utilizzata per descrivere una popolazione media. Non implica che tutte le particelle reali siano perfettamente sferiche.

---

## 13. Sistema implicito per precipitazione e re-solution

Dopo produzione e trasferimento di massa associato alla nucleazione si definiscono le concentrazioni intermedie:

$$
C_m^*,
\qquad
C_{\mathrm{intra}}^*,
\qquad
C_{\mathrm{GB}}^*.

$$

I processi di precipitazione e re-solution vengono risolti simultaneamente con uno schema backward Euler.

Il sistema è:

$$
\mathbf A
\mathbf C^{n+1}
=
\mathbf b,

$$

con

$$
\mathbf C^{n+1}
=
\begin{bmatrix}
C_m^{n+1}\\
C_{\mathrm{intra}}^{n+1}\\
C_{\mathrm{GB}}^{n+1}
\end{bmatrix},

$$

e

$$
\mathbf A
=
\begin{bmatrix}
1+(k_{\mathrm{intra}}+k_{\mathrm{GB}})\Delta t
&
-k_{\mathrm{res}}\Delta t
&
-k_{\mathrm{res}}\Delta t
\\
-k_{\mathrm{intra}}\Delta t
&
1+k_{\mathrm{res}}\Delta t
&
0
\\
-k_{\mathrm{GB}}\Delta t
&
0
&
1+k_{\mathrm{res}}\Delta t
\end{bmatrix}.

$$

Poiché la precipitazione è guidata da

$$
C_m-C_{\mathrm{eq}},

$$

il termine noto è:

$$
\mathbf b
=
\begin{bmatrix}
C_m^*
+
(k_{\mathrm{intra}}+k_{\mathrm{GB}})
C_{\mathrm{eq}}\Delta t
\\
C_{\mathrm{intra}}^*
-
k_{\mathrm{intra}}
C_{\mathrm{eq}}\Delta t
\\
C_{\mathrm{GB}}^*
-
k_{\mathrm{GB}}
C_{\mathrm{eq}}\Delta t
\end{bmatrix}.

$$

Se la precipitazione non è attiva, i coefficienti $k_{\mathrm{intra}}$ e $k_{\mathrm{GB}}$ vengono posti uguali a zero per quel timestep.

I termini contenenti $C_{\mathrm{eq}}$, precipitazione e re-solution si cancellano sommando le tre equazioni. Il sistema conserva quindi la massa metallica totale.

---

## 14. Trattamento lagged dei coefficienti di precipitazione

I coefficienti di precipitazione dipendono dalla popolazione di particelle:

$$
k_j
\propto
N_jR_j.

$$

Poiché $R_j$ dipende a sua volta dalla massa precipitata, il problema completo sarebbe non lineare.

Nel modello viene adottato uno schema lagged:

$$
k_j
=
k_j
\left(
C_j^{\mathrm{old}},
N_j^{\mathrm{old}}
\right).

$$

I coefficienti vengono valutati una volta all'inizio del timestep e mantenuti costanti durante la soluzione del sistema 3×3.

Questo evita una fixed-point iteration interna sul feedback:

$$
C_{\mathrm{prec}}
\uparrow
\Rightarrow
R
\uparrow
\Rightarrow
k
\uparrow
\Rightarrow
C_{\mathrm{prec}}
\uparrow.

$$

La formulazione può quindi essere descritta come semi-implicita con coefficienti laggati.

---

## 15. Scaling factors nel modello attuale

Gli scaling factors disponibili nell'interfaccia SCIANTIX restano:

| Parametro | Significato | Valore di riferimento |
|---|---|---:|
| `sf_mfp_nucleation_rate` | prefattore nucleazione intra | 1 |
| `sf_mfp_precipitation_rate_intragranular` | prefattore precipitazione intra | 1 |
| `sf_mfp_precipitation_rate_grain_boundary` | prefattore precipitazione GB | 1 |
| `sf_mfp_resolution_rate` | re-solution | 1 |
| `sf_mfp_nucleation_energy_barrier` | scaling del coefficiente CNT intra | 1 |
| `sf_mfp_intra_activation_energy` | energia di attivazione precipitazione intra | 1 |
| `sf_mfp_grain_boundary_activation_energy` | energia di attivazione precipitazione GB | 1 |
| `sf_mfp_nucleation_energy_barrier_grain_boundary` | scaling del coefficiente CNT GB | 1 |
| `sf_mfp_nucleation_rate_grain_boundary` | prefattore nucleazione GB | 1 |

Nel modello finale, però, solo due combinazioni sono lasciate libere durante la calibrazione.

### Parametro libero 1: precipitazione comune

$$
sf_{k,\mathrm{intra}}
=
sf_{k,\mathrm{GB}}
=
s_k.

$$

Dominio finale:

$$
2.5\times10^{-15}
\leq
s_k
\leq
8.0\times10^{-15}.

$$

Il campionamento è logaritmico.

### Parametro libero 2: barriera CNT comune

$$
sf_{B,\mathrm{intra}}
=
sf_{B,\mathrm{GB}}
=
s_B.

$$

Dominio finale:

$$
245
\leq
s_B
\leq
285.

$$

Il campionamento è lineare.

Tutti gli altri scaling factors sono mantenuti uguali a 1.

---

## 16. Sequenza finale dell'algoritmo

A ogni timestep il modello procede nel seguente ordine:

1. legge temperatura, fission rate, burnup, grain radius e variabili microstrutturali necessarie;
2. calcola la produzione metallica:

$$
\Delta C_{\mathrm{prod}}
=
y_{\mathrm{5MP}}F\Delta t;

$$

3. calcola $C_{\mathrm{eq}}(T)$;
4. valuta la supersaturazione:

$$
S
=
\frac{C_m^{\mathrm{matrix}}}
{C_{\mathrm{eq}}(T)};

$$

5. calcola i siti intragranulari e GB;
6. se $S>1$, calcola $\Delta G_j^*$ e $k_{\mathrm{nucl},j}$;
7. aggiorna $N_{\mathrm{intra}}$ e $N_{\mathrm{GB}}$;
8. calcola il critical nucleus size $n_j^*$;
9. trasferisce alla fase precipitata la massa richiesta dalla nucleazione, limitandola alla massa disponibile;
10. calcola $k_{\mathrm{intra}}$ e $k_{\mathrm{GB}}$ dallo stato all'inizio del timestep;
11. attiva la precipitazione solo se la matrice è sopra $C_{\mathrm{eq}}(T)$;
12. calcola $k_{\mathrm{res}}$ dal fission rate;
13. risolve il sistema implicito 3×3 per matrice, precipitati intra e precipitati GB;
14. calcola $n_{\mathrm{intra}}$ e $n_{\mathrm{GB}}$;
15. ricostruisce $R_{\mathrm{intra}}$ e $R_{\mathrm{GB}}$;
16. salva lo stato aggiornato per il timestep successivo.

---

## 17. Ipotesi e limiti attuali

Il modello attuale:

- tratta Mo, Tc, Ru, Rh e Pd come una singola specie efficace;
- usa un effective equilibrium concentration basato principalmente su dati del Mo;
- considera nucleazione eterogenea ma non nucleazione omogenea;
- non include coalescenza;
- non include total dissolution delle singole particelle;
- non evolve una particle size distribution;
- non consente a $N$ di diminuire;
- usa lo stesso fission-rate history nelle quattro posizioni radiali del dataset attuale;
- usa un solo valore efficace di $\lambda$;
- usa un'unica barriera CNT comune intra/GB nella calibrazione finale;
- usa un'unica scala comune di precipitazione intra/GB nella calibrazione finale.

Queste ipotesi mantengono il modello compatibile con una formulazione reduced-order e con la quantità di informazione sperimentale attualmente disponibile.