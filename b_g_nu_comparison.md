# Modelli di Re-solution, Trapping e Nucleazione

## Rizk (2025) – Barani (2019, 2020)

---

# 1. Rizk (2025) – Uranium Nitride (UN)

## Meccanismo

Omogeneo (collisioni balistiche)

---

## Re-solution rate

$$
b = b_0(R)\dot{F} \quad [s^{-1}]
$$

$$
b_0(R)=10^{-25}\left(2.64 - 2.02 \exp\left[-\frac{2.61\times10^{-9}}{R}\right]\right)
$$

* $R$ in metri
* $\dot{F}\ \text{in fissioni}\ \mathrm{m}^{-3}\ \mathrm{s}^{-1}$

---

## Trapping rate

### Bulk

$$
g_b = 4\pi D_g R_b N_b  
\quad [\mathrm{s}^{-1}]
$$

### Dislocazioni

$$
g_d = 4\pi D_g R_d N_d
+\frac{2\pi D_g}{\ln\left(\frac{\Gamma_d}{ Z_d r_d }\right)-\frac{5}{3}}
  (\rho_d - 2R_d N_d)
  $$

$$
\Gamma_d = \frac{1}{\sqrt{\pi \rho_d}}
$$

---

## Nucleazione

$$
\nu_b = 8\pi f_n D_g \Omega_{fg}^{1/3} c^2
\quad [m^{-3}s^{-1}]
$$

---

## Costanti (UN)

* $f_n = 10^{-6}$
* $\Omega_{fg} = 8.5\times10^{-29}, m^3$
* $\rho_d = 3.0\times10^{13}, m^{-2}$
* $r_d = 3.46\times10^{-10}, m$
* $Z_d = 5.0$
* $k_B = 8.617\times10^{-5}, eV/K$

$$
D_g = \left(1.56\times10^{-3} e^{-4.94/(k_BT)} + 1.85\times10^{-39}\dot{F}\right) SF_D
$$



$$
N_d(0) = 3.6 \times 10^{19} \; \text{[bub/m}^3\text{]}
$$

*Valore iniziale del numero di bolle sulle dislocazioni.*


---

# 2. Barani (2019) – Uranium Silicide (U₃Si₂)

## Meccanismo

Omogeneo (metallo)

---

## Re-solution rate

$$
\alpha = \alpha_0(R)\dot{F}
\quad [\mathrm{s}^{-1}]
$$

$$
\alpha_0(R)=2.80\times10^{-25}\left(\frac{5\times10^{-10}}{R}\right)^{0.23}
$$

---

## Trapping rate

$$
b_n = 4\pi D R_n N
\quad [\mathrm{s}^{-1}]
$$

---

## Nucleazione

$$
n = 8\pi D R_{sg} f_n c_1^2
\quad [\mathrm{at}\cdot \mathrm{m}^{-3}\ \mathrm{s}^{-1}]
$$

---

## Costanti (U₃Si₂)

* $f_n = 10^{-4}$
* $R_{sg} = 2.41\times10^{-10}, m$
* $D = 5.91\times10^{-6} \exp\left[-\frac{4.41\times10^{-19}}{k_BT}\right]$
* Nota: qui l'energia è in Joule, quindi

$$
k_B = 1.38 \cdot 10^{-23}\ \mathrm{J/K}
$$
---

# 3. Barani (2020) – Uranium Dioxide (UO₂)

## Meccanismo

Eterogeneo / thermal spikes

---

## Re-solution rate

$$
\alpha =
\left[
a e^{-b_1 R}

+ \frac{b_0 - a}{1 + cR^2} e^{-dR^2}
  \right]\dot{F}
\quad [\mathrm{s}^{-1}]
$$

---

## Trapping rate

### Bulk

$$
b_b = 4\pi D R_b N_b
$$

### Dislocazioni

$$
b_d = 4\pi D R_b N_d+
\frac{2\pi D_d}{\ln\left(\frac{r_{ws,d}}{r_d}\right) - \frac{3}{5}}
\;\rho_d
$$

## Nucleazione

### Bulk

$$
h \cdot 2\dot{F}
$$

### Dislocazioni

$$
N_{d,initial} = K\rho_d
$$

---

## Costanti (UO₂)

### Re-solution fit

* $a = 9.49\times10^{-24}, m^3$
* $b_0 = 9.18\times10^{-23}, m^3$
* $b_1 = 7.07\times10^7, m^{-1}$
* $c = 7.982\times10^{18}, m^{-2}$
* $d = 3.71\times10^{16}, m^{-2}$

### Microstruttura

* $h = 25 \quad \mathrm{bolle/fission\ fragment}$ 
* $K = 10^6, m^{-1}$
* $\rho_d = 4.0\times10^{13}, m^{-2}$
* $r_d = 1.925\times10^{-9}, m$
* $
r_{ws,d} = \frac{1}{\sqrt{\pi \rho_d}}\ \text{(m)}
$


### Diffusività

$$
D_1 = 7.6\times10^{-10} e^{-4.86\times10^{-19}/(k_BT)}
$$

$$
D_2 = 5.64 \cdot 10^{-25}\ \sqrt{\dot{F}}\ e^{-\frac{1.91 \cdot 10^{-19}}{k_B T}}
$$

$$
D_3 = 2.0\times10^{-40}\dot{F}
$$


## Discussione e confronto tra i modelli

L’analisi comparativa dei modelli di Rizk (UN) e Barani (U₃Si₂, UO₂) evidenzia differenze strutturali rilevanti nei meccanismi fisici alla base dei processi di re-solution, trapping e nucleazione del gas di fissione. Tali differenze si riflettono direttamente nelle dipendenze funzionali osservate nei grafici e, più in generale, nel comportamento predetto dei sistemi.

### Confronto dei meccanismi di re-solution

Nei modelli di Rizk (UN) e Barani per U₃Si₂, la re-solution è descritta come un processo **omogeneo**, legato a collisioni balistiche che avvengono nel volume del materiale. Questo si traduce in una dipendenza debole dal raggio della bolla: nel caso di UN si osserva una saturazione a grande raggio, mentre per U₃Si₂ la dipendenza segue una legge di potenza molto blanda.

Al contrario, nel modello di Barani per UO₂, la re-solution è dominata da un meccanismo **eterogeneo**, associato ai *thermal spikes* indotti dagli eventi di fissione. La presenza di termini esponenziali in funzione di \( R \) e \( R^2 \) introduce una lunghezza caratteristica nel problema, determinando un’elevata efficienza di re-solution per bolle piccole e un rapido decadimento per bolle di dimensioni maggiori. Questo comportamento indica che, in UO₂, la re-solution è fortemente localizzata e sensibile alla scala spaziale del fenomeno.

L’introduzione del termine ( b *phi ), con \( phi= 1/m-1 ), amplifica ulteriormente il contributo delle bolle piccole, rendendo la dinamica della re-solution fortemente dominata dalle scale nanometriche.

---

### Confronto del trapping nel bulk

Il trapping nel bulk presenta una struttura formale analoga nei tre modelli, essendo proporzionale a \( 4\pi D R N \). Tuttavia, le differenze emergono chiaramente attraverso la dipendenza della diffusività \( D \):

- In UN (Rizk), la diffusività include sia un contributo termico sia uno dipendente dal fission rate, introducendo una debole dipendenza da \( F \).
- In U₃Si₂ (Barani), la diffusività è puramente termica, rendendo il trapping indipendente dal fission rate.
- In UO₂ (Barani), la diffusività comprende contributi proporzionali a \( \sqrt{F} \) e a \( F \), oltre al termine termico.

Ne consegue che, mentre UN e U₃Si₂ sono prevalentemente controllati dalla temperatura, UO₂ può entrare in un regime in cui il trapping è dominato dall’irraggiamento. Questo rappresenta una differenza fondamentale nella risposta del materiale alle condizioni operative.

---

### Confronto del trapping su dislocazioni

Per quanto riguarda il trapping sulle dislocazioni, sia UN che UO₂ includono due contributi: la cattura sulle bolle già presenti e un termine associato alla diffusione lungo la dislocazione (*pipe diffusion*).

Nel modello di Rizk (UN), il termine di diffusione lungo la dislocazione dipende dalla quantità di dislocazione disponibile, espressa come \( (\rho_d - 2 R_d N_d) \). Questo implica l’esistenza di un limite geometrico: all’aumentare della concentrazione di bolle sulle dislocazioni, la lunghezza disponibile si riduce fino a saturazione. Il modello incorpora quindi in modo esplicito la competizione per lo spazio sulla dislocazione.

Nel modello di Barani per UO₂, invece, il termine di diffusione lungo la dislocazione è proporzionale a \( \rho_d \) e non dipende da \( N_d \). Di conseguenza, non è presente un meccanismo di saturazione analogo, e la capacità di trapping delle dislocazioni non si esaurisce con l’aumentare delle bolle. Questo suggerisce che il modello per UO₂ adotta una descrizione più efficace e meno legata alla geometria microscopica.

---

### Confronto dei meccanismi di nucleazione

Le differenze più marcate emergono nell’analisi della nucleazione.

Nei modelli di Rizk (UN) e Barani (U₃Si₂), la nucleazione segue una legge proporzionale a \( c^2 \), tipica di un processo diffusivo di secondo ordine, in cui la formazione di nuove bolle è governata dalla collisione tra atomi di gas. Questo implica una forte dipendenza dalla concentrazione locale e, indirettamente, dalla diffusività, quindi dalla temperatura.

Nel modello di Barani per UO₂, invece, la nucleazione è espressa come \( \nu = 2h\dot{F} \), risultando indipendente sia dalla concentrazione di gas che dalla temperatura. In questo caso, la formazione di bolle è direttamente legata agli eventi di fissione, configurando un processo **event-driven**.

---

### Conclusioni sulla comparazione

Nel complesso, il confronto evidenzia una distinzione netta tra due classi di modelli:

- **Modelli omogenei (UN, U₃Si₂):**
  - Basati su cinetiche diffusivo-collisionali
  - Dipendenza forte dalla concentrazione di gas
  - Meccanismi sensibili alla diffusività e quindi alla temperatura
  - Presenza di vincoli geometrici realistici (es. saturazione delle dislocazioni)

- **Modello eterogeneo (UO₂):**
  - Dominato da processi indotti dagli eventi di fissione (thermal spikes)
  - Debole o nulla dipendenza dalla concentrazione locale per la nucleazione
  - Forte dipendenza dal fission rate
  - Assenza di meccanismi espliciti di saturazione microstrutturale

Questa distinzione riflette due approcci concettualmente diversi: da un lato una descrizione microscopica basata sul trasporto diffusivo e sulle interazioni tra specie, dall’altro una modellazione efficace in cui i fenomeni sono guidati direttamente dall’energia depositata dagli eventi di fissione.

In termini applicativi, ciò implica che i modelli per UN e U₃Si₂ risultano più adatti a descrivere l’evoluzione microstrutturale in condizioni in cui il trasporto diffusivo è dominante, mentre il modello per UO₂ è più rappresentativo di condizioni in cui i processi indotti dall’irraggiamento giocano un ruolo primario.