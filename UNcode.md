---
title: "SCIANTIX — UN (Uranium Nitride) code notes"
---

# SCIANTIX — UN (Uranium Nitride)

Questo file raccoglie, in un unico posto, **equazioni e parametri** effettivamente usati nel codice per il caso **UN** (tag nel codice: `// AD URANIUMNITRIDE` / `// AD UN URANIUMNITRIDE` / `// UN AD URANIUMNITRIDE`).

## Attivazione (input)

- **Matrice UN**: `iFuelMatrix = 2` (vedi `src/operations/SetMatrix.C`, `src/operations/SetSystem.C`)
- **Solver diffusione UN a 3 equazioni (exchange)**: `iDiffusionSolver = 4` (vedi `src/models/GasDiffusion.C`)
- **Diffusività Xe in UN (Rizk 2025)**: `iFissionGasDiffusivity = 11` (vedi `src/classes/System.C`)
- **Trapping UN (bulk + dislocazioni)**: `iTrappingRate = 2` (vedi `src/classes/System.C`)
- **Re-solution UN (bulk + dislocazioni)**: `iResolutionRate = 4` (vedi `src/classes/System.C`)

## Variabili UN aggiunte

### Gas in dislocation bubbles (concentrazione)
Inizializzate in `src/operations/SetVariablesFunctions.C`:
- `"Xe in dislocation bubbles"` `(at/m3)`
- `"Kr in dislocation bubbles"` `(at/m3)`
- `"He in dislocation bubbles"` `(at/m3)`
- `"Xe133 in dislocation bubbles"` `(at/m3)`
- `"Kr85m in dislocation bubbles"` `(at/m3)`

### Dislocation bubbles (microstruttura)
Inizializzate in `src/operations/SetVariablesFunctions.C` e mappate in `src/operations/UpdateVariables.C`:
- `"Dislocation bubble concentration"` `(bub/m3)`  (indice update 163)
- `"Dislocation bubble radius"` `(m)` (indice update 164)

Nota: nel codice sono **ri-usati** gli indici 19–20 (storicamente intragranular bubble conc/radius) come placeholder anche per le dislocation bubbles (vedi commenti in `src/operations/SetVariablesFunctions.C`).

## Parametri materiale UN (Matrix)

Definiti in `src/operations/SetMatrix.C` (funzione `UN(...)`) e accessibili via `Matrix`:

### Parametri UN-specifici (dislocazioni)
- `rho_d` = `matrix.getDislocationDensity()`  `[1/m^2]`
  - valore attuale nel codice: `3.0e13` (Rizk et al., JNM 606 (2025) 155604)
- `r_d` = `matrix.getDislocationCoreRadius()` `[m]`
  - valore attuale nel codice: `3.46e-10` (Rizk et al., JNM 606 (2025) 155604, ~ a/sqrt(2))

### Altri valori impostati (placeholder)
- `matrix_density = 14300.0` `(kg/m3)` (TODO nel codice: verificare fonte)
- `lattice_parameter = 4.889e-10` `(m)` (Rizk et al., JNM 606 (2025) 155604)

## Diffusività Xe in UN (iFissionGasDiffusivity = 11)

Implementata in `src/classes/System.C` (case 11).

### Formula
Sia `T` la temperatura (K) e `Ḟ` il fission rate density (1/(m³·s)).

Costanti:
- `kB = 8.617333262e-5` (eV/K)
- `D10 = 1.56e-3` (m²/s)
- `Q1  = 4.94` (eV)
- `A30 = 1.85e-39` (m⁵)

Componenti:
- `d1 = D10 * exp( -Q1 / (kB*T) )`
- `d2 = 0` (termine irradiation-enhanced presente come commento ma disattivato)
- `d3 = A30 * Ḟ`

Risultato:
- `D_g = (d1 + d2 + d3) * scaling_factors["Diffusivity"]`

## Re-solution UN: bulk + dislocazioni (iResolutionRate = 4)

Implementata in `src/classes/System.C` in `System::setResolutionRatesUN(...)`.

### Definizioni
- `Ḟ` = fission rate density (1/(m³·s)) = `history_variable["Fission rate"]`
- `r_lat` = `radius_in_lattice` (m)
- `R_b,intra = R_intra + r_lat`, con `R_intra = sciantix_variable["Intragranular bubble radius"].getInitialValue()`
- `R_b,disl  = R_disl  + r_lat`, con `R_disl  = sciantix_variable["Dislocation bubble radius"].getInitialValue()`

### Coefficiente b0 (Rizk 2025, come in codice)
Per entrambi (intra e disl):
- `b0(R) = 1.0e-25 * ( 2.64 - 2.02 * exp( -2.61e-9 / R ) )`

### Tassi di re-solution usati dal solver diffusione UN
- `b_b = resolution_rate_intra = Ḟ * b0(R_b,intra) * scaling_factors["Resolution rate"]`
- `b_d = resolution_rate_disl  = Ḟ * b0(R_b,disl)  * scaling_factors["Resolution rate"]`

## Trapping UN: bulk + dislocazioni (iTrappingRate = 2)

Implementata in `src/classes/System.C` in `System::setTrappingRatesUN(...)`.

### Trapping verso bulk bubbles (gb)
Definizioni:
- `N_b` = `"Intragranular bubble concentration"` (bub/m³)
- `R_b` = `"Intragranular bubble radius"` + `radius_in_lattice` (m)

Formula (Ham-like sink):
- se `N_b == 0`: `g_b = 0`
- altrimenti: `g_b = 4 * pi * D_g * R_b * N_b`
- `g_b *= scaling_factors["Trapping rate"]`

### Trapping verso dislocazioni (gd)
Somma di due contributi:
1) **Bolle su dislocazioni** (analogo a gb)
2) **Dislocazione “nuda” (line sink)**

Definizioni:
- `N_d` = `"Dislocation bubble concentration"` (bub/m³)
- `R_d` = `"Dislocation bubble radius"` (m)
- `rho_d` = dislocation density (1/m²) = `matrix.getDislocationDensity()`
- `r_d` = dislocation core radius (m) = `matrix.getDislocationCoreRadius()`
- `Z_d = 5.0` (costante nel codice)
- `Gamma_d = 1 / sqrt(pi * rho_d)` (m) (Wigner–Seitz radius per dislocazioni)
- `R_d,eff = R_d + radius_in_lattice` (m)

Termine bolle:
- `term_bubbles = 4 * pi * D_g * R_d,eff * N_d`

Termine line-sink:
- `den = ln( Gamma_d / (Z_d * r_d) ) - 3/5` (protetto numericamente nel codice)
- `free_dislocation = rho_d - 2 * R_d * N_d` (clamp a ≥ 0 nel codice)
- `term_dislocation = (2*pi*D_g/den) * free_dislocation`

Risultato:
- `g_d = term_bubbles + term_dislocation`
- `g_d *= scaling_factors["Trapping rate"]`

## Solver diffusione UN a 3 equazioni (iDiffusionSolver = 4)

### Variabili (spazialmente mediate sul grano)
- `c` = gas in solution (diffonde) `(at/m³)`
- `m_b` = gas in bulk bubbles `(at/m³)`
- `m_d` = gas in dislocation bubbles `(at/m³)`

### Sistema di equazioni (usato nel solver)
Implementato in `src/classes/Solver.C` (`Solver::SpectralDiffusion3equationsExchange`):

```
dc/dt   = D_g ∇²c − (g_b + g_d) c + b_b m_b + b_d m_d + β
dm_b/dt = g_b c − b_b m_b
dm_d/dt = g_d c − b_d m_d
```

### Vettore parametri passato al solver (Model: "Gas diffusion - <system>")
Costruito in `src/models/GasDiffusion.C` (`defineSpectralDiffusion3EquationsExchange`):

| idx | simbolo | descrizione | unità |
|---:|---|---|---|
| 0 | `N_modes` | numero modi spettrali | (/) |
| 1 | `D_g` | diffusività gas in soluzione (con precursor factor) | (m²/s) |
| 2 | `r` | raggio grano | (m) |
| 3 | `β` | produzione volumetrica gas | (at/m³·s) |
| 4 | `g_b` | trapping verso bulk bubbles | (1/s) |
| 5 | `g_d` | trapping verso dislocation bubbles | (1/s) |
| 6 | `b_b` | re-solution da bulk bubbles | (1/s) |
| 7 | `b_d` | re-solution da dislocation bubbles | (1/s) |

Note codice:
- `D_g` viene costruito come `system.getFissionGasDiffusivity() * system.getGas().getPrecursorFactor()`.
- Per sistemi non-UN, `g_b, g_d, b_b, b_d` sono attesi `0`.

---

## Codice Sciantix: caller + solver (3 eq UN)

### 1) Chi chiama il solver (iDiffusionSolver = 4)

In `src/models/GasDiffusion.C` il caso `iDiffusionSolver = 4` legge `c, m_b, m_d` dalle variabili Sciantix, chiama il
solver spettrale e poi riscrive i valori aggiornati:

```cpp
// src/models/GasDiffusion.C (case 4 - UN)
double c_solution =
    sciantix_variable[system.getGasName() + " in intragranular solution"].getFinalValue();
double m_bulk =
    sciantix_variable[system.getGasName() + " in intragranular bubbles"].getFinalValue();
double m_disl =
    sciantix_variable[system.getGasName() + " in dislocation bubbles"].getFinalValue();

solver.SpectralDiffusion3equationsExchange(
    c_solution,
    m_bulk,
    m_disl,
    getDiffusionModesSolution(system.getGasName()),
    getDiffusionModesBubbles(system.getGasName()),
    getDiffusionModesDislocationBubbles(system.getGasName()),
    model["Gas diffusion - " + system.getName()].getParameter(),
    physics_variable["Time step"].getFinalValue());

sciantix_variable[system.getGasName() + " in intragranular solution"].setFinalValue(c_solution);
sciantix_variable[system.getGasName() + " in intragranular bubbles"].setFinalValue(m_bulk);
sciantix_variable[system.getGasName() + " in dislocation bubbles"].setFinalValue(m_disl);
sciantix_variable[system.getGasName() + " in grain"].setFinalValue(c_solution + m_bulk + m_disl);
```

### 2) Il codice che risolve davvero (Backward Euler per modo + 3×3)

In `src/classes/Solver.C`, `Solver::SpectralDiffusion3equationsExchange(...)` fa un loop sui modi spettrali e, per ogni
modo, esegue **un passo implicito Backward Euler** risolvendo un **sistema lineare 3×3** accoppiato.

Estratto (matrice del passo implicito e solve):

```cpp
// src/classes/Solver.C (per ogni modo)
coeff_matrix[0] = 1.0 + (diffusion_rate + g_b + g_d) * increment;
coeff_matrix[1] = -b_b * increment;
coeff_matrix[2] = -b_d * increment;

coeff_matrix[3] = -g_b * increment;
coeff_matrix[4] = 1.0 + b_b * increment;
coeff_matrix[5] = 0.0;

coeff_matrix[6] = -g_d * increment;
coeff_matrix[7] = 0.0;
coeff_matrix[8] = 1.0 + b_d * increment;

initial_conditions[0] = modes_c[n] + source_rate * increment;
initial_conditions[1] = modes_m_b[n];
initial_conditions[2] = modes_m_d[n];

Solver::Laplace3x3(coeff_matrix, initial_conditions);
```

Poi aggiorna `modes_c[n], modes_m_b[n], modes_m_d[n]` e ricostruisce le medie sul grano (`c, m_b, m_d`) via proiezione
sommando il contributo di tutti i modi.

---

## Spiegazione formale del procedimento numerico

### A) Discretizzazione nello spazio: espansione spettrale su sfera

Si assume un grano sferico di raggio `R`. Le grandezze vengono espanse in autofunzioni del Laplaciano; nel codice questo
compare come una dipendenza `~ n²` del termine diffusivo:

- `diffusion_rate = (π² D_g / R²) * n²`

Ogni modo spettrale evolve quindi come un sistema ODE accoppiato fra `c_n, m_{b,n}, m_{d,n}`.

### B) Discretizzazione nel tempo: Backward Euler modo-per-modo

Per ogni modo `n` e per ogni time step `k → k+1` con `dt`, Sciantix risolve:

- `(I - dt * J_n) x_n^{k+1} = x_n^{k} + dt * s_n`

dove:
- `x_n = [c_n, m_{b,n}, m_{d,n}]^T`
- `J_n` è il jacobiano lineare (include diffusione + trapping + resolution)
- `s_n` è il termine sorgente del modo (derivato dalla sorgente volumetrica `β` tramite proiezione)

Operativamente, questo equivale a costruire e risolvere un sistema lineare 3×3 per ogni modo:
- righe/colonne 1..3 corrispondono a `[c_n, m_{b,n}, m_{d,n}]`
- i coefficienti sono quelli che vedi in `coeff_matrix[...]` (con termini `diffusion_rate`, `g_b`, `g_d`, `b_b`, `b_d`)

### C) Ricostruzione delle medie sul grano (quantità “lumped”)

Dopo l’update dei modi, Sciantix ricostruisce le quantità medie sul grano (`c, m_b, m_d`) come combinazione lineare dei
coefficienti modali tramite un fattore di proiezione (`projection_coeff`) e `n_coeff = (-1)^(n+1)/n`. Queste medie sono
quelle che poi vengono salvate nelle variabili Sciantix e usate dagli altri modelli.

---

---

# Approfondimento — Metodo Spettrale e Significato dei Modi (versione corretta)

Questa sezione chiarisce il significato matematico e fisico della decomposizione spettrale usata nel solver UN a 3 equazioni, in coerenza con l’implementazione SCIANTIX.

---

## 1. Espansione spettrale

La concentrazione del gas in soluzione è rappresentata come:

$$
c(r,t) = \sum_{n=1}^{N} c_n(t)\,\phi_n(r)
$$

dove:

- $ \phi_n(r) $ sono autofunzioni del Laplaciano nel grano sferico
- soddisfano:
  $$
  \nabla^2 \phi_n = -\lambda_n \phi_n
  $$
- con:
  $$
  \lambda_n = \left(\frac{n\pi}{R}\right)^2
  $$

Questa formulazione deriva dall’approccio classico di **Booth** per la diffusione intragranulare in una sfera, in cui la soluzione viene espressa come serie di modi radiali.

Espansioni analoghe valgono per:

$$
m_b(r,t), \quad m_d(r,t)
$$

---

## 2. Da PDE a ODE

Sostituendo l’espansione nella PDE e proiettando sulle autofunzioni:

$$
\int_V \phi_n \phi_m \, dV = 0 \quad (n \neq m)
$$

si ottiene, per ogni modo $ n $, il sistema:

$$
\begin{cases}
\frac{dc_n}{dt} = -D_g \lambda_n c_n - (g_b + g_d)c_n + b_b m_{b,n} + b_d m_{d,n} + S_n \\
\frac{dm_{b,n}}{dt} = g_b c_n - b_b m_{b,n} \\
\frac{dm_{d,n}}{dt} = g_d c_n - b_d m_{d,n}
\end{cases}
$$

Ogni modo evolve **indipendentemente dagli altri** una volta effettuata la proiezione.

---

## 3. Significato dei modi

- **Modo n = 1**
  - rappresenta la componente a più bassa frequenza spaziale
  - contribuisce in modo dominante alla media volumetrica
  - ma **non coincide esattamente con la media**

- **Modi n > 1**
  - rappresentano variazioni radiali più rapide nel grano
  - decadono rapidamente per diffusione:
    $$
    \lambda_n \propto n^2
    $$

> Nota: nella base spettrale di tipo Booth, nessun modo coincide esattamente con una funzione costante. La decomposizione non separa direttamente media e fluttuazioni.

---

## 4. Termine sorgente β (produzione da fissione)

### Definizione

$$
\beta = \dot{F} \cdot Y_{Xe}
$$

- funzione del tempo
- assunta **uniforme nello spazio**

---

## 5. Proiezione della sorgente (forma esplicita in SCIANTIX)

Nel formalismo continuo, la proiezione è:

$$
S_n = \int_V \beta\,\phi_n(r)\, dV
$$

Poiché $ \beta $ è costante:

$$
S_n = \beta \int_V \phi_n(r)\, dV
$$

La funzione uniforme viene quindi rappresentata come serie spettrale:

$$
\beta = \sum_{n=1}^{N} S_n \phi_n(r)
$$

---

### Forma implementata nel codice

Nel solver SCIANTIX, questa proiezione non è calcolata tramite integrazione numerica esplicita, ma è implementata direttamente tramite coefficienti analitici equivalenti derivati dalla base spettrale:

$$
S_n = p\,a_n\,\beta
$$

con:

$$
p = -2\sqrt{\frac{2}{\pi}}, \qquad a_n = \frac{(-1)^n}{n}
$$

ovvero:

$$
S_n = -2\sqrt{\frac{2}{\pi}} \cdot \frac{(-1)^n}{n} \cdot \beta
$$

---

### Proprietà dei coefficienti

- tutti i modi sono eccitati:
  $$
  S_n \neq 0 \quad \forall n=1,\dots,N
  $$

- ampiezza decrescente:
  $$
  S_n \sim \frac{1}{n}
  $$

- segno alternato:
  $$
  (-1)^n
  $$

---

### Correzione rispetto all’assunzione semplificata

Non vale:

$$
\beta_n =
\begin{cases}
\neq 0 & n = 1 \\
0 & n > 1
\end{cases}
$$

Questa proprietà sarebbe valida solo per basi che includono esplicitamente la funzione costante come primo modo, cosa che **non avviene nella base radiale usata (Booth / SCIANTIX)**.

---

### Interpretazione fisica corretta

- la produzione di gas è uniforme
- la base spettrale non contiene una componente costante pura
- quindi una funzione costante viene rappresentata come somma di modi:

$$
\beta = \sum_n S_n \phi_n(r)
$$

➡️ la sorgente alimenta **tutti i modi**, non solo il primo

---

## 6. Implementazione nel solver

Nel solver:

- β è passato come termine scalare
- viene distribuito sui modi tramite i coefficienti $S_n$

Per ogni modo si risolve:

$$
\dot{x}_n = A_n x_n + S_n
$$

con:

$$
x_n =
\begin{bmatrix}
c_n \\
m_{b,n} \\
m_{d,n}
\end{bmatrix}
$$

---

## 7. Risoluzione numerica

Il sistema è risolto con schema implicito (Backward Euler):

$$
(I - \Delta t A_n)\, x_n^{k+1} = x_n^k + \Delta t S_n
$$

Caratteristiche:

- schema implicito
- A-stable
- sistema lineare 3×3 per ogni modo

---

## 8. Ricostruzione delle variabili medie

Le quantità macroscopiche sono:

$$
c = \sum_{n=1}^{N} w_n c_n,
\quad
m_b = \sum_{n=1}^{N} w_n m_{b,n},
\quad
m_d = \sum_{n=1}^{N} w_n m_{d,n}
$$

con:

$$
w_n = \frac{p\,a_n}{(4/3)\pi}
$$

dove i pesi $w_n$ derivano dalla media volumetrica delle autofunzioni $\phi_n$ nel grano sferico.

---

## 9. Conseguenze pratiche

- la sorgente alimenta **tutti i modi**
- i modi alti:
  - sono debolmente eccitati ($\sim 1/n$)
  - decadono rapidamente ($\sim n^2$)
- il comportamento globale è dominato dai primi modi, ma non esclusivamente

---

## 10. Sintesi concettuale

Il metodo spettrale usato in SCIANTIX:

- rappresenta il campo tramite modi radiali (tipo Booth)
- non separa esplicitamente media e fluttuazioni
- distribuisce una sorgente uniforme su tutta la base

Pertanto, una sorgente costante non corrisponde a un singolo modo, ma a una combinazione di modi con coefficienti:

$$
S_n \propto \frac{(-1)^n}{n}
$$

---

## 11. Collegamento con la soluzione analitica (Pastore / Booth)

Nel caso di coefficienti costanti, il sistema modale può essere risolto analiticamente. Ogni modo evolve come combinazione di esponenziali:

$$
c_n(t) = A_n e^{-p_n t} + B_n e^{-q_n t} + C_n
$$

dove $p_n$ e $q_n$ sono gli autovalori del sistema accoppiato diffusione–trapping–resolution.

Nel solver SCIANTIX, questi autovalori non sono calcolati esplicitamente, ma il comportamento dinamico equivalente è ottenuto tramite integrazione numerica implicita.