# Modello UN (Uranium Nitride) - Equazioni e Parametri

Questo documento descrive le equazioni e i parametri del modello UN basato su Rizk (2025) per il comportamento del gas di fissione in Uranium Nitride.

## Diffusività del Gas di Fissione (Xe)

La diffusività $D_g$ è data dalla somma di tre contributi:

$$
D_g = D_1 + D_2 + D_3
$$

dove:
- $D_1 = D_{10} \exp\left(-\frac{Q_1}{k_B T}\right)$ (diffusione termica)
- $D_2 = A_{20} \sqrt{\dot{F}} \exp\left( -\frac{B_{21}}{k_B T} - \frac{B_{22}}{(k_B T)^2} - \frac{B_{23}}{(k_B T)^3} \right)$ (irradiation-enhanced diffusion)
- $D_3 = A_{30} \dot{F}$ (radiation-induced mixing)

Valori dei parametri:
- $D_{10} = 1.56 \times 10^{-3}$ m²/s
- $Q_1 = 4.94$ eV
- $A_{20} = 1.21 \times 10^{-67}$ m⁷/²/s¹/²
- $B_{21} = 25.87$ eV
- $B_{22} = -1.49$ eV²
- $B_{23} = 0.0$ eV³
- $A_{30} = 1.85 \times 10^{-39}$ m⁵
- $k_B = 8.617 \times 10^{-5}$ eV/K
- $\dot{F}$ è il fission rate density (fissioni/m³·s)

Nota: Nel modello attuale, $D_2$ è trascurato per Xe secondo Rizk, ma è incluso qui per completezza.

## Re-solution Rate

Il tasso di re-solution $b$ per le bolle è:

$$
b = b_0(R) \dot{F}
$$

dove:

$$
b_0(R) = 10^{-25} \left(2.64 - 2.02 \exp\left(-\frac{2.61 \times 10^{-9}}{R}\right)\right)
$$

- $R$ è il raggio della bolla (m)
- $\dot{F}$ è il fission rate density (fissioni/m³·s)

Questo vale sia per le bolle bulk che per quelle su dislocazioni:

$$
b_b = b_0(R_b)\dot{F}
$$

$$
b_d = b_0(R_d)\dot{F}
$$

## Trapping Rate

### Trapping verso bolle bulk ($g_b$)

$$
g_b = 4\pi D_g R_b N_b
$$

- $D_g$ diffusività (m²/s)
- $R_b$ raggio bolle bulk (m)
- $N_b$ concentrazione bolle bulk (bub/m³)

### Trapping verso dislocazioni ($g_d$)

$$
g_d =
4\pi D_g R_d N_d
+
\frac{2\pi D_g}
{\ln\left(\frac{\Gamma_d}{Z_d r_d}\right) - \frac{3}{5}}
(\rho_d - 2 R_d N_d)
$$

dove:
- $R_d$ raggio bolle su dislocazioni (m)
- $N_d$ concentrazione bolle su dislocazioni (bub/m³)
- $\Gamma_d = \frac{1}{\sqrt{\pi \rho_d}}$ (Wigner-Seitz radius per dislocazioni, m)
- $\rho_d = 3.0 \times 10^{13}$ m⁻² (densità dislocazioni)
- $r_d = 3.46 \times 10^{-10}$ m (raggio core dislocazioni)
- $Z_d = 5.0$

Nota: $\Gamma_d$ è mantenuto nella forma dimensionalmente corretta:

$$
\Gamma_d = \frac{1}{\sqrt{\pi \rho_d}}
$$

perché deve essere una lunghezza, in modo che l’argomento del logaritmo sia adimensionale.

## Nucleazione

Il tasso di nucleazione per bolle bulk $\nu_b$:

$$
\nu_b = 8\pi f_n D_g \Omega_{fg}^{1/3} c^2
$$

- $f_n = 10^{-6}$
- $\Omega_{fg} = 8.5 \times 10^{-29}$ m³
- $c$ concentrazione gas in soluzione (at/m³)

## Sistema completo Rizk per bolle bulk e bolle su dislocazioni

Le variabili principali sono:
- $c$: gas in soluzione (at/m³)
- $m_b$: gas in bolle bulk (at/m³)
- $m_d$: gas in bolle su dislocazioni (at/m³)
- $N_b$: concentrazione bolle bulk (bub/m³)
- $N_d$: concentrazione bolle su dislocazioni (bub/m³)
- $V_b$: volume medio bolla bulk (m³/bub)
- $V_d$: volume medio bolla su dislocazione (m³/bub)
- $n_b$: concentrazione volumetrica di vacanze associate alle bolle bulk (vac/m³)
- $n_d$: concentrazione volumetrica di vacanze associate alle bolle su dislocazioni (vac/m³)

### Equazione per gas in soluzione

$$
\frac{\partial c}{\partial t}
=
D_g \nabla^2 c
-
(g_b + g_d)c
+
b_b m_b
+
b_d m_d
+
\beta
$$

### Equazioni per gas nelle bolle

Forma compatta scritta per bulk e dislocation bubbles:

$$
\frac{\partial m_{b,d}}{\partial t}
=
g_{b,d}c
-
b_{b,d}m_{b,d}
$$

cioè:

$$
\frac{\partial m_b}{\partial t}
=
g_b c
-
b_b m_b
$$

$$
\frac{\partial m_d}{\partial t}
=
g_d c
-
b_d m_d
$$

dove:
- $\beta$ produzione volumetrica di gas (at/m³·s)
- $b_b, b_d$ tassi di re-solution per bulk e dislocazioni

### Evoluzione della concentrazione di bolle bulk

La concentrazione numerica delle bolle bulk, $N_b$, evolve secondo:

$$
\frac{\partial N_b}{\partial t}
=
\nu_b
-
b_b \phi_b N_b
$$

dove:

$$
\phi_b = \frac{1}{m_b' - 1}
$$

e:

$$
m_b' = \frac{m_b}{N_b}
$$

è il numero medio di atomi di gas per bolla bulk.

Il termine $\phi_b$ corregge il fatto che la re-solution rimuove singoli atomi, mentre per distruggere una bolla servono più eventi di re-solution.

### Evoluzione della concentrazione di bolle su dislocazioni

L’equazione completa per la concentrazione numerica delle bolle su dislocazioni è:

$$
\frac{\partial N_d}{\partial t}
=
\frac{N_d}{\rho_d}
\frac{\partial \rho_d}{\partial t}
-
4 \lambda N_d^2
\frac{dV_d}{dt}
$$

dove:
- $\frac{N_d}{\rho_d}\frac{\partial \rho_d}{\partial t}$ descrive la variazione di $N_d$ dovuta all’evoluzione della densità di dislocazioni;
- $-4\lambda N_d^2 \frac{dV_d}{dt}$ descrive la diminuzione di $N_d$ dovuta alla coalescenza delle bolle su dislocazioni.

Nel modello di Rizk, la densità iniziale di bolle su dislocazioni è legata alla densità di dislocazioni:

$$
N_d = K \rho_d
$$

con:

$$
K = 5.0 \times 10^5 \ \mathrm{bubble\,m^{-1}}
$$

$$
\rho_d = 3.0 \times 10^{13} \ \mathrm{m^{-2}}
$$

quindi, usando questi valori:

$$
N_d(0) = K \rho_d = 1.5 \times 10^{19} \ \mathrm{bubble\,m^{-3}}
$$

Se si assume densità di dislocazioni costante:

$$
\frac{\partial \rho_d}{\partial t}=0
$$

allora il primo termine della 21d si annulla, ma rimane comunque il termine di coalescenza se si implementa l’evoluzione del volume:

$$
\frac{\partial N_d}{\partial t}
=
-
4 \lambda N_d^2
\frac{dV_d}{dt}
$$

Se invece nel modello numerico semplificato non si implementano né evoluzione di $\rho_d$ né coalescenza, allora si assume:

$$
\frac{\partial N_d}{\partial t}=0
$$

### Evoluzione del volume delle bolle

L’evoluzione del volume medio delle bolle bulk e delle bolle su dislocazioni è:

$$
\frac{\partial V_{b,d}}{\partial t}
=
\frac{\Omega_{fg}}{N_{b,d}}
\frac{\partial m_{b,d}}{\partial t}
+
\frac{\Omega}{N_{b,d}}
\frac{\partial n_{b,d}}{\partial t}
$$

dove:
- $\Omega_{fg}$ = volume atomico del gas di fissione;
- $\Omega$ = volume atomico della matrice UN;
- $m_{b,d}$ = concentrazione di gas nelle bolle;
- $n_{b,d}$ = concentrazione volumetrica di vacanze associata alle bolle;
- $N_{b,d}$ = concentrazione numerica delle bolle.

Scritta separatamente:

$$
\frac{\partial V_b}{\partial t}
=
\frac{\Omega_{fg}}{N_b}
\frac{\partial m_b}{\partial t}
+
\frac{\Omega}{N_b}
\frac{\partial n_b}{\partial t}
$$

$$
\frac{\partial V_d}{\partial t}
=
\frac{\Omega_{fg}}{N_d}
\frac{\partial m_d}{\partial t}
+
\frac{\Omega}{N_d}
\frac{\partial n_d}{\partial t}
$$

### Evoluzione delle vacanze nelle bolle

L’evoluzione della concentrazione volumetrica di vacanze associata alle bolle è:

$$
\frac{\partial n_{b,d}}{\partial t}
=
\frac{2 \pi D_v \delta_{b,d} N_{b,d}}
{k_B T \zeta_{b,d}}
\left(
p_{b,d}
-
p_{b,d}^{eq}
\right)
$$

Scritta separatamente:

$$
\frac{\partial n_b}{\partial t}
=
\frac{2 \pi D_v \delta_b N_b}
{k_B T \zeta_b}
\left(
p_b
-
p_b^{eq}
\right)
$$

$$
\frac{\partial n_d}{\partial t}
=
\frac{2 \pi D_v \delta_d N_d}
{k_B T \zeta_d}
\left(
p_d
-
p_d^{eq}
\right)
$$

dove:
- $n_{b,d}$ = concentrazione volumetrica di vacanze associate alle bolle $[\mathrm{vac/m^3}]$
- $D_v$ = diffusività delle vacanze (m²/s)
- $\delta_{b,d}$ = raggio della cella di Wigner-Seitz associata alla popolazione di bolle
- $\zeta_{b,d}$ = fattore geometrico
- $p_{b,d}$ = pressione interna della bolla
- $p_{b,d}^{eq}$ = pressione di equilibrio della bolla

## Numero medio di atomi per bolla

Per le bolle bulk:

$$
m_b' = \frac{m_b}{N_b}
$$

Per le bolle su dislocazioni:

$$
m_d' = \frac{m_d}{N_d}
$$

dove:
- $m_b$ = concentrazione di gas nelle bolle bulk $[\mathrm{at/m^3}]$
- $m_d$ = concentrazione di gas nelle bolle su dislocazioni $[\mathrm{at/m^3}]$
- $N_b$ = concentrazione numerica bolle bulk $[\mathrm{bub/m^3}]$
- $N_d$ = concentrazione numerica bolle su dislocazioni $[\mathrm{bub/m^3}]$

Nota: in questo documento si usa $m_i'$ per il numero medio di atomi di gas per bolla, per non confonderlo con $m_i$, che è la concentrazione volumetrica di gas nelle bolle.

## Raggio delle bolle

Assumendo bolle sferiche, il volume medio di una bolla è:

$$
V_i = \frac{4}{3}\pi R_i^3
$$

con $i=b,d$.

Il raggio si calcola dalla relazione:

$$
R_i =
\left(
\frac{3 V_i}{4 \pi}
\right)^{1/3}
$$

Nel caso semplificato in cui il volume della bolla sia calcolato tramite il volume atomico efficace del gas:

$$
V_i = m_i' \Omega_{fg}
$$

quindi:

$$
R_i =
\left(
\frac{3 m_i' \Omega_{fg}}{4 \pi}
\right)^{1/3}
$$

dove:

$$
\Omega_{fg} = 8.5 \times 10^{-29} \ \mathrm{m^3}
$$

## Swelling intragranulare da bolle bulk

Lo swelling volumetrico dovuto alle bolle bulk è:

$$
\left(\frac{\Delta V}{V}\right)_b
=
N_b V_b
$$

con:

$$
V_b = \frac{4}{3}\pi R_b^3
$$

quindi:

$$
\left(\frac{\Delta V}{V}\right)_b
=
\frac{4}{3}\pi R_b^3 N_b
$$

## Swelling intragranulare da bolle su dislocazioni

Lo swelling volumetrico dovuto alle bolle su dislocazioni è:

$$
\left(\frac{\Delta V}{V}\right)_d
=
N_d V_d
$$

con:

$$
V_d = \frac{4}{3}\pi R_d^3
$$

quindi:

$$
\left(\frac{\Delta V}{V}\right)_d
=
\frac{4}{3}\pi R_d^3 N_d
$$

## Swelling gassoso intragranulare totale

Lo swelling gassoso intragranulare totale è la somma dei contributi bulk e dislocation:

$$
\left(\frac{\Delta V}{V}\right)_{ig}
=
\left(\frac{\Delta V}{V}\right)_b
+
\left(\frac{\Delta V}{V}\right)_d
$$

cioè:

$$
\left(\frac{\Delta V}{V}\right)_{ig}
=
\frac{4}{3}\pi R_b^3 N_b
+
\frac{4}{3}\pi R_d^3 N_d
$$

## Swelling da prodotti di fissione solidi

Rizk usa una correlazione semplice per lo swelling da prodotti di fissione solidi:

$$
\left(\frac{\Delta V}{V}\right)_{solid}
=
0.5 B
$$

dove $B$ è il burnup espresso in FIMA.

Se $B$ è espresso in percento FIMA, la forma equivalente è:

$$
\left(\frac{\Delta V}{V}\right)_{solid}
=
0.005 B_{\%FIMA}
$$

Questa correlazione rappresenta circa $0.5\%$ di swelling per ogni $1\%$ FIMA.

## Swelling totale

Lo swelling totale del combustibile, escludendo l’espansione termica, è:

$$
\left(\frac{\Delta V}{V}\right)_{tot}
=
\left(\frac{\Delta V}{V}\right)_{solid}
+
\left(\frac{\Delta V}{V}\right)_{ig}
+
\left(\frac{\Delta V}{V}\right)_{gf}
$$

dove:
- $(\Delta V/V)_{solid}$ = swelling da prodotti solidi
- $(\Delta V/V)_{ig}$ = swelling gassoso intragranulare
- $(\Delta V/V)_{gf}$ = swelling da bolle ai bordi di grano

Nel modello SCIANTIX-UN attuale, se le bolle intergranulari non sono ancora implementate, si può porre temporaneamente:

$$
\left(\frac{\Delta V}{V}\right)_{gf} = 0
$$

## Parametri UN da Rizk

| Simbolo | Valore | Unità | Significato |
|---|---:|---|---|
| $D_{10}^{Xe}$ | $1.56 \times 10^{-3}$ | $\mathrm{m^2\,s^{-1}}$ | prefattore diffusione termica Xe |
| $Q_1^{Xe}$ | $4.94$ | $\mathrm{eV}$ | energia attivazione Xe |
| $A_{20}^{Xe}$ | $1.21 \times 10^{-67}$ | $\mathrm{m^{7/2}\,s^{-1/2}}$ | coefficiente irradiation-enhanced Xe |
| $B_{21}^{Xe}$ | $25.87$ | $\mathrm{eV}$ | parametro fit $D_2$ Xe |
| $B_{22}^{Xe}$ | $-1.49$ | $\mathrm{eV^2}$ | parametro fit $D_2$ Xe |
| $A_3^{Xe}$ | $1.85 \times 10^{-39}$ | $\mathrm{m^5}$ | coefficiente mixing irradiation-induced |
| $D_{10}^{V_U}$ | $1.35 \times 10^{-2}$ | $\mathrm{m^2\,s^{-1}}$ | prefattore diffusione termica vacanze U |
| $Q_1^{V_U}$ | $5.66$ | $\mathrm{eV}$ | energia attivazione vacanze U |
| $A_{20}^{V_U}$ | $1.32 \times 10^{-19}$ | $\mathrm{m^{7/2}\,s^{-1/2}}$ | coefficiente irradiation-enhanced vacanze |
| $B_{21}^{V_U}$ | $-0.62$ | $\mathrm{eV}$ | parametro fit $D_2$ vacanze |
| $B_{22}^{V_U}$ | $-0.04$ | $\mathrm{eV^2}$ | parametro fit $D_2$ vacanze |
| $\Omega_{fg}$ | $8.5 \times 10^{-29}$ | $\mathrm{m^3}$ | volume atomico gas fissione |
| $f_n$ | $10^{-6}$ | $-$ | fattore nucleazione omogenea |
| $a$ | $4.889 \times 10^{-10}$ | $\mathrm{m}$ | parametro reticolare UN |
| $\Omega$ | $a^3/4$ | $\mathrm{m^3}$ | volume atomico matrice UN |
| $\gamma$ | $1.11$ | $\mathrm{J\,m^{-2}}$ | energia superficiale UN-bolla |
| $D_v^{gb}$ | $10^6 D_1^U$ | $\mathrm{m^2\,s^{-1}}$ | diffusività vacanze al bordo grano |
| $\delta_{gb}$ | $4.0 \times 10^{-10}$ | $\mathrm{m}$ | spessore layer diffusivo bordo grano |
| $N_{gf,0}$ | $2.0 \times 10^{13}$ | $\mathrm{bub\,m^{-2}}$ | densità iniziale bolle grain-face |
| $\theta$ | $59^\circ$ | $\mathrm{gradi}$ | semi-angolo diedro |
| $R_{gf}$ | $2.42 \times 10^{-10}$ | $\mathrm{m}$ | raggio iniziale grain-face |
| $r_{gr}$ | $6.0 \times 10^{-6}$ | $\mathrm{m}$ | raggio grano |
| $F_{c,sat}$ | $0.5$ | $-$ | copertura grain-face a saturazione |
| $K$ | $5.0 \times 10^5$ | $\mathrm{bubble\,m^{-1}}$ | bolle per lunghezza di dislocazione |
| $r_d$ | $a/\sqrt{2}$ | $\mathrm{m}$ | raggio/core dislocazione |
| $r_d$ | $3.46 \times 10^{-10}$ | $\mathrm{m}$ | valore numerico |
| $\rho_d$ | $3.0 \times 10^{13}$ | $\mathrm{m^{-2}}$ | densità di dislocazioni |
| $Z_d$ | $5.0$ | $-$ | trapping radius factor dislocazioni |
| $k_B$ | $8.617333262 \times 10^{-5}$ | $\mathrm{eV\,K^{-1}}$ | costante di Boltzmann |

## Diffusività delle vacanze di Uranio

Per la crescita delle bolle tramite assorbimento di vacanze serve la diffusività delle vacanze di Uranio:

$$
D_{V_U} = D_1^{V_U} + D_2^{V_U}
$$

con:

$$
D_1^{V_U}
=
1.35 \times 10^{-2}
\exp\left(
-\frac{5.66}{k_B T}
\right)
$$

e:

$$
D_2^{V_U}
=
\sqrt{\dot{F}}
\left[
1.32 \times 10^{-19}
\exp\left(
-\frac{-0.62}{k_B T}
-
\frac{-0.04}{(k_B T)^2}
\right)
\right]
$$

Per le vacanze non si usa il termine $D_3$, perché $D_3$ rappresenta il mixing balistico usato per Xe.

## Diffusività delle vacanze ai bordi di grano

Rizk assume:

$$
D_v^{gb} = 10^6 D_1^U
$$

dove $D_1^U$ è la diffusività termica dei difetti di Uranio.

## Pressione interna della bolla

La pressione del gas nella bolla può essere calcolata con la relazione:

$$
p =
\frac{k_B T m}{n \Omega}
$$

dove:
- $p$ = pressione interna della bolla
- $T$ = temperatura
- $m$ = numero di atomi di gas nella bolla
- $n$ = numero di vacanze nella bolla
- $\Omega$ = volume atomico della matrice UN

Nota: questa è la forma riportata nel modello Rizk per il calcolo della pressione interna della bolla.

## Pressione di equilibrio della bolla

La pressione meccanica di equilibrio è:

$$
p_{eq}
=
\frac{2 \gamma}{R}
-
\sigma_h
$$

dove:
- $\gamma = 1.11 \ \mathrm{J\,m^{-2}}$
- $R$ = raggio bolla
- $\sigma_h$ = stress idrostatico

Se lo stress idrostatico non è disponibile nel modello 0D:

$$
\sigma_h = 0
$$

e quindi:

$$
p_{eq}
=
\frac{2 \gamma}{R}
$$

## Crescita per assorbimento di vacanze

La crescita delle bolle può essere guidata dall’assorbimento di vacanze quando:

$$
p > p_{eq}
$$

Il rate di assorbimento di vacanze nel modello Rizk è:

$$
\frac{\partial n_{b,d}}{\partial t}
=
\frac{2 \pi D_v \delta_{b,d} N_{b,d}}
{k_B T \zeta_{b,d}}
\left(
p_{b,d}
-
p_{b,d}^{eq}
\right)
$$

dove:
- $n_{b,d}$ = concentrazione volumetrica di vacanze associate a bolle bulk/dislocation
- $D_v$ = diffusività delle vacanze
- $\delta_{b,d}$ = raggio della cella di Wigner-Seitz
- $\zeta_{b,d}$ = fattore geometrico
- $p_{b,d}-p_{b,d}^{eq}$ = sovrapressione della bolla

La cella di Wigner-Seitz associata alla popolazione di bolle è:

$$
\delta_{b,d}
=
\left(
\frac{3}{4\pi N_{b,d}}
\right)^{1/3}
$$

Il fattore geometrico è:

$$
\zeta_{b,d}
=
\frac{
10 \psi_{b,d}
\left(
1+\psi_{b,d}^3
\right)
}{
-\psi_{b,d}^6
+
5\psi_{b,d}^2
-
9\psi_{b,d}
+
5
}
$$

con:

$$
\psi_{b,d} =
\frac{R_{b,d}}{\delta_{b,d}}
$$

Nota: questa parte è importante per descrivere il breakaway swelling, ma nel tuo stato attuale sembra non ancora implementata completamente in SCIANTIX-UN.

## Copertura delle dislocazioni da parte delle bolle

La frazione di dislocazioni occupata dalle bolle è:

$$
\kappa
=
\frac{2R_d N_d}{\rho_d}
$$

Questo termine è coerente con la parte libera della dislocazione usata nel trapping:

$$
\rho_d - 2R_d N_d
$$

## Loop punching / emissione di dislocazioni

La pressione soglia per il loop punching è:

$$
P_{dis}
=
\frac{G b}{R}
+
p^{eq}
$$

dove:

$$
G =
\frac{E}{2(1+\nu)}
$$

e:
- $G$ = modulo di taglio
- $E$ = modulo di Young
- $\nu$ = coefficiente di Poisson
- $b$ = modulo del vettore di Burgers
- $R$ = raggio bolla

## Bolle ai bordi di grano

Rizk include anche una popolazione intergranulare, indicata come grain-face bubbles.

La densità iniziale è:

$$
N_{gf,0} = 2.0 \times 10^{13} \ \mathrm{bub\,m^{-2}}
$$

La copertura frazionaria delle facce di grano è:

$$
F_c = N_{gf} A_{gf}
$$

dove $A_{gf}$ è l’area proiettata media di una bolla sul bordo di grano.

Per bolle lenticolari:

$$
A_{gf} = \pi R_{gf}^2 \sin^2 \theta
$$

Il raggio di curvatura della bolla grain-face è:

$$
R_{gf}
=
\left[
\frac{3 V_{gf}}
{4 \pi \left(1 - 1.5\cos\theta + 0.5\cos^3\theta\right)}
\right]^{1/3}
$$

con:

$$
\theta = 59^\circ
$$

La saturazione/interconnessione è assunta quando:

$$
F_c \ge F_{c,sat}
$$

con:

$$
F_{c,sat} = 0.5
$$

Quando la copertura raggiunge la saturazione, il gas che arriva successivamente ai bordi di grano può essere considerato rilasciato.

## Fission gas release

Il gas rilasciato può essere modellato come il gas che arriva alle bolle intergranulari dopo il raggiungimento della copertura critica:

$$
F_c \ge F_{c,sat}
$$

Prima della saturazione:

$$
\dot{R}_{FG} = 0
$$

Dopo la saturazione:

$$
\dot{R}_{FG} = \dot{q}_{gb}
$$

dove $\dot{q}_{gb}$ è il flusso/rate di gas che raggiunge il bordo di grano.

Nel modello SCIANTIX-UN attuale, questa parte può essere lasciata come estensione futura se il rilascio intergranulare non è ancora accoppiato.

## Note implementative importanti

1. Nel modello attuale SCIANTIX-UN sono già presenti tre concentrazioni:
   - $c$: gas in soluzione
   - $m_b$: gas in bolle bulk
   - $m_d$: gas in bolle su dislocazioni

2. Il sistema diffusivo a 3 equazioni è:

$$
\frac{\partial c}{\partial t}
=
D_g \nabla^2 c
-
(g_b+g_d)c
+
b_b m_b
+
b_d m_d
+
\beta
$$

$$
\frac{\partial m_b}{\partial t}
=
g_b c
-
b_b m_b
$$

$$
\frac{\partial m_d}{\partial t}
=
g_d c
-
b_d m_d
$$

3. Le equazioni aggiunte rispetto al modello minimo sono:
   - evoluzione $N_b$
   - evoluzione completa $N_d$ con termine di coalescenza
   - evoluzione $V_b$, $V_d$
   - evoluzione delle vacanze $n_b$, $n_d$
   - pressione interna bolla
   - pressione di equilibrio
   - fattore geometrico $\zeta$
   - copertura dislocazioni $\kappa$
   - pressione soglia per loop punching

4. Per una prima implementazione numerica minimale:
   - usare $N_d$ costante
   - usare $N_b$ evolutivo
   - calcolare il numero medio di atomi di gas per bolla come:

$$
m_i' = \frac{m_i}{N_i}
$$

   - calcolare $R_b$ e $R_d$ da:

$$
R_i =
\left(
\frac{3 m_i' \Omega_{fg}}{4\pi}
\right)^{1/3}
$$

   oppure, in forma equivalente, direttamente da $m_i$ e $N_i$:

$$
R_i =
\left(
\frac{3 m_i \Omega_{fg}}{4\pi N_i}
\right)^{1/3}
$$

   - calcolare swelling intragranulare come somma bulk + dislocation
   - lasciare grain-boundary bubbles/FGR come TODO

5. Per una implementazione completa Rizk:
   - risolvere anche $N_d$
   - risolvere anche $V_b$ e $V_d$
   - risolvere anche $n_b$ e $n_d$
   - aggiornare $R_b$ e $R_d$ da $V_b$ e $V_d$
   - usare $p-p^{eq}$ per la crescita vacancy-driven