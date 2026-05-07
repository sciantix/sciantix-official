# UN model — physics notes (consolidated)

Concatenation of the five physics-content reports that previously lived as separate files in `un_calibration/reports/`:

1. `UNmodel.md` — Rizk model equations and parameters (Italian)
2. `UNcode.md` — C++ SCIANTIX engine UN-specific tags and equations
3. `b_g_nu_comparison.md` — Re-solution / trapping / nucleation comparison: Rizk (UN), Barani (U₃Si₂), Barani (UO₂)
4. `UNcoalescence_comparison.md` — Coalescence terms across Barani, Rizk
5. `UN_model_variants_report_thesis.md` — Comparative analysis of model variants M0..M6 and thesis recommendation

Active executable physics is in `un_calibration/notebooks/UN_clean.ipynb` (Section 2 has the LaTeX equations) and `un_calibration/model/un_model.py` (the solver). This file preserves discussion / comparative analysis / design rationale that doesn't naturally live in code.

Source PDFs are in `references/pdf_link/`; bibliography in `references/un_bib.md`, `references/references.md`.

---

═══════════════════════════════════════════════════════════════
## Source: `UNmodel.md`
═══════════════════════════════════════════════════════════════

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

Per i calcoli di validazione rispetto agli esperimenti Rizk/DN1 si usa:

$$
\dot{F} \simeq 5.0 \times 10^{19} \ \mathrm{fissions\,m^{-3}\,s^{-1}}
$$

for Rizk validation experiments, corrispondente a un linear heat rate di circa 100 kW/m e a un rod diameter di 8.30 mm.

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

 Fattore di coalescenza per bolle su dislocazioni

Il fattore correttivo per la coalescenza tra bolle su dislocazioni è:

$$
\lambda =
\frac{2-\xi}{2(1-\xi)^3}
$$

dove:

$$
\xi =
V_d N_d
$$

cioè:

$$
\xi =
\frac{4}{3}\pi R_d^3 N_d
$$

Qui $\xi$ rappresenta la porosità intragranulare associata alla popolazione di bolle su dislocazioni.

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

Per la crescita delle bolle tramite assorbimento di vacanze serve la diffusività efficace delle vacanze di Uranio:

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
A_{20,\mathrm{fit}}^{V_U}
\exp\left(
\frac{-0.62}{k_B T}
+
\frac{-0.04}{(k_B T)^2}
\right)
\right]
$$

dove:

$$
A_{20,\mathrm{fit}}^{V_U}
=
4.6304523933553033 \times 10^{-29}
\ \mathrm{m^{7/2}s^{-1/2}}
$$

Per le vacanze non si usa il termine $D_3$, perché $D_3$ rappresenta il mixing balistico usato per Xe.

**Nota sul parametro $A_{20}^{V_U}$**  
La Table 2 di Rizk riporta per le vacanze di Uranio il valore:

$$
A_{20}^{V_U} = 1.32 \times 10^{-19}
$$

ma usando tale valore direttamente nella formula la diffusività ottenuta non riproduce la curva “Vacancies, bulk” riportata da Rizk. Per questo motivo, in questa implementazione si usa il valore:

$$
A_{20,\mathrm{fit}}^{V_U}
=
4.6304523933553033 \times 10^{-29}
$$

ottenuto come fit della diffusività delle vacanze di bulk riportata nella Fig. 4 di Rizk. Gli altri parametri della correlazione, cioè $D_{10}^{V_U}$, $Q_1^{V_U}$, $B_{21}^{V_U}$ e $B_{22}^{V_U}$, sono mantenuti invariati.

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
---
it is not utilized in the current bubble growth model, but is instructive for future model development
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

From MD calculations, where θ is semi-dihedral angle of the bubble, which is calculated using the ratio of grain boundary and surface energies.

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

   ---

## Struttura globale del modello: intragranular → grain face → gas release

L’obiettivo del modello è calcolare il trasporto del gas dalla matrice intragranulare verso i bordi di grano, la crescita delle bolle intergranulari e, quando viene raggiunta la saturazione della copertura di grain-face bubbles, il fission gas release.

La struttura è:

$$
\text{intragranular}
\rightarrow
\dot{q}_{gb}
\rightarrow
\text{grain-face bubbles}
\rightarrow
F_c \ge F_{c,sat}
\rightarrow
\text{fission gas release}
$$

---

## Variabili intragranulari principali

Le variabili primarie del modello intragranulare sono:

$$
c
$$

$$
m_b
$$

$$
m_d
$$

$$
N_b
$$

$$
N_d
$$

dove:
- $c$ = concentrazione di gas in soluzione;
- $m_b$ = concentrazione di gas nelle bolle bulk;
- $m_d$ = concentrazione di gas nelle bolle su dislocazioni;
- $N_b$ = concentrazione numerica delle bolle bulk;
- $N_d$ = concentrazione numerica delle bolle su dislocazioni.

---

## Equazioni intragranulari

Il sistema intragranulare è:

$$
\frac{\partial c}{\partial t}
=
D_g\nabla^2 c
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

$$
\frac{\partial N_b}{\partial t}
=
\nu_b
-
b_b \phi_b N_b
$$

$$
\frac{\partial N_d}{\partial t}
=
\frac{N_d}{\rho_d}
\frac{\partial \rho_d}{\partial t}
-
4\lambda N_d^2
\frac{dV_d}{dt}
$$

---

## Ipotesi su densità di dislocazioni

Nel modello attuale si assume densità di dislocazioni costante:

$$
\frac{\partial \rho_d}{\partial t}
=
0
$$

Sostituendo nella equazione di $N_d$:

$$
\frac{\partial N_d}{\partial t}
=
-
4\lambda N_d^2
\frac{dV_d}{dt}
$$

Quindi $N_d$ non è costante se si considera la coalescenza delle bolle su dislocazioni.

La variazione di $N_d$ è controllata dalla crescita del volume medio delle bolle su dislocazioni:

$$
\frac{dV_d}{dt}
$$

Se le bolle su dislocazioni crescono:

$$
\frac{dV_d}{dt} > 0
$$

allora:

$$
\frac{\partial N_d}{\partial t} < 0
$$

cioè la concentrazione numerica delle bolle su dislocazioni diminuisce per coalescenza.

---

## Volume delle bolle

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

Separatamente:

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

---

## Crescita per vacanze

La concentrazione volumetrica di vacanze associate alle bolle evolve come:

$$
\frac{\partial n_{b,d}}{\partial t}
=
\frac{2\pi D_v \delta_{b,d} N_{b,d}}
{k_B T \zeta_{b,d}}
\left(
p_{b,d}
-
p_{b,d}^{eq}
\right)
$$

Separatamente:

$$
\frac{\partial n_b}{\partial t}
=
\frac{2\pi D_v \delta_b N_b}
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
\frac{2\pi D_v \delta_d N_d}
{k_B T \zeta_d}
\left(
p_d
-
p_d^{eq}
\right)
$$

---

## Raggio delle bolle

Il raggio delle bolle viene calcolato dal volume medio:

$$
R_b =
\left(
\frac{3V_b}{4\pi}
\right)^{1/3}
$$

$$
R_d =
\left(
\frac{3V_d}{4\pi}
\right)^{1/3}
$$

---

## Condizione al bordo di grano

Il bordo di grano è trattato come sink per il gas in soluzione:

$$
c(r=a,t)=0
$$

Il flusso di gas verso il grain face è:

$$
J_{gb}
=
-
D_g
\left.
\frac{\partial c}{\partial r}
\right|_{r=a}
$$

Il rate volumetrico di gas che raggiunge il grain face può essere scritto come:

$$
\dot{q}_{gb}
=
-
\frac{3}{a}
D_g
\left.
\frac{\partial c}{\partial r}
\right|_{r=a}
$$

---

## Variabili intergranulari principali

Le variabili principali del modello grain-face sono:

$$
q
$$

$$
N_{gf}
$$

$$
V_{gf}
$$

$$
R_{gf}
$$

$$
F_c
$$

$$
R_{FG}
$$

dove:
- $q$ = gas arrivato al grain face;
- $N_{gf}$ = concentrazione numerica delle bolle grain-face;
- $V_{gf}$ = volume medio delle bolle grain-face;
- $R_{gf}$ = raggio medio delle bolle grain-face;
- $F_c$ = copertura frazionaria del grain face;
- $R_{FG}$ = gas rilasciato.

---

## Accumulo di gas al grain face

Prima della saturazione/interconnessione:

$$
\frac{dq}{dt}
=
\dot{q}_{gb}
$$

Dopo la saturazione/interconnessione:

$$
\frac{dq}{dt}
=
0
$$

e il gas che arriva viene rilasciato.

---

## Grain-face bubble coverage

La copertura del grain face è:

$$
F_c =
N_{gf} A_{gf}
$$

L’area proiettata della bolla grain-face è:

$$
A_{gf}
=
\pi R_{gf}^2 \sin^2\theta
$$

Il raggio della bolla grain-face è:

$$
R_{gf}
=
\left[
\frac{3V_{gf}}
{4\pi
\left(
1
-
1.5\cos\theta
+
0.5\cos^3\theta
\right)}
\right]^{1/3}
$$

---

## Condizione di saturazione/interconnessione

La saturazione avviene quando:

$$
F_c \ge F_{c,sat}
$$

con:

$$
F_{c,sat}=0.5
$$

---

## Fission gas release

Prima della saturazione:

$$
F_c < F_{c,sat}
$$

$$
\dot{R}_{FG}=0
$$

Dopo la saturazione:

$$
F_c \ge F_{c,sat}
$$

$$
\dot{R}_{FG}
=
\dot{q}_{gb}
$$

Quindi, dopo interconnessione, tutto il gas che arriva dal modello intragranulare al grain face viene considerato rilasciato.

---

## Sequenza logica di soluzione

1. Risolvere il modello intragranulare:

$$
c,\ m_b,\ m_d,\ N_b,\ N_d
$$

2. Calcolare il flusso verso il grain face:

$$
\dot{q}_{gb}
$$

3. Usare $\dot{q}_{gb}$ come sorgente del modello grain-face:

$$
\frac{dq}{dt}
=
\dot{q}_{gb}
$$

4. Calcolare crescita delle grain-face bubbles:

$$
V_{gf}
\rightarrow
R_{gf}
\rightarrow
A_{gf}
\rightarrow
F_c
$$

5. Quando:

$$
F_c \ge 0.5
$$

imporre:

$$
\dot{R}_{FG}
=
\dot{q}_{gb}
$$


═══════════════════════════════════════════════════════════════
## Source: `UNcode.md`
═══════════════════════════════════════════════════════════════

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

Valore di riferimento per i calcoli di validazione Rizk/DN1:
- `Ḟ ≈ 5.0e19` fiss/(m³·s), for Rizk validation experiments, ricavato da LHR ≈ 100 kW/m e rod diameter = 8.30 mm.

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


═══════════════════════════════════════════════════════════════
## Source: `b_g_nu_comparison.md`
═══════════════════════════════════════════════════════════════

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
+\frac{2\pi D_g}{\ln\left(\frac{\Gamma_d}{ Z_d r_d }\right)-\frac{3}{5}}
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
* $\dot{F} \approx 5.0\times10^{19}, \mathrm{fissions\,m^{-3}\,s^{-1}}$ for Rizk validation experiments (DN1, LHR ≈ 100 kW/m, rod diameter 8.30 mm)

$$
D_g = \left(1.56\times10^{-3} e^{-4.94/(k_BT)} + 1.85\times10^{-39}\dot{F}\right) SF_D
$$



$$
N_d(0) = K\rho_d = 5.0\times10^5 \cdot 3.0\times10^{13}
= 1.5 \times 10^{19} \; \text{[bub/m}^3\text{]}
$$

*Valore iniziale Rizk del numero di bolle sulle dislocazioni.*


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


═══════════════════════════════════════════════════════════════
## Source: `UNcoalescence_comparison.md`
═══════════════════════════════════════════════════════════════

## Confronto dei termini di coalescenza nei modelli

La variazione del numero di bolle nel tempo dovuta alla coalescenza viene trattata con diversi livelli di complessità nei modelli disponibili. Nei modelli recenti di Barani (2020) e Rizk (2025), la coalescenza intragranulare è descritta principalmente come impingement, cioè contatto diretto tra bolle dovuto alla loro crescita volumetrica, non come coalescenza per migrazione Browniana delle bolle.

---

### 1. Coalescenza tra bolle su dislocazioni

Questo termine descrive la diminuzione della densità numerica delle bolle su dislocazioni, $N_d$, quando le bolle crescono e si toccano.

In Barani (2020) e Rizk (2025), il termine di coalescenza è:

$$
\left(
\frac{\partial N_d}{\partial t}
\right)_{coals}
=
-4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

dove:

$$
\lambda =
\frac{2-\xi}{2(1-\xi)^3}
$$

e:

$$
\xi =
V_d N_d
=
\frac{4}{3}\pi R_d^3 N_d
$$

Qui $V_d$ è il volume medio della bolla su dislocazione e $\xi$ è la porosità intragranulare associata alla popolazione di bolle su dislocazioni.

Nel modello Rizk per UN, questo termine è incluso nella evoluzione di $N_d$:

$$
\frac{\partial N_d}{\partial t}
=
\frac{N_d}{\rho_d}
\frac{\partial \rho_d}{\partial t}
-
4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

Se si assume densità di dislocazioni costante:

$$
\frac{\partial \rho_d}{\partial t}=0
$$

rimane comunque:

$$
\frac{\partial N_d}{\partial t}
=
-
4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

quindi $N_d$ diminuisce se le bolle su dislocazioni crescono.

---

### 2. Coalescenza tra bolle su dislocazioni e bolle bulk

Barani (2020) include anche un termine di interazione tra bolle su dislocazioni e bolle bulk. L’idea è che una bolla su dislocazione, crescendo, catturi le bolle bulk contenute nel volume spazzato dalla sua espansione.

Il volume di cattura è:

$$
V_d^*
=
\frac{4}{3}\pi(R_d+R_b)^3
$$

La variazione della densità numerica delle bolle bulk dovuta a questa interazione è:

$$
\left(
\frac{\partial N_b}{\partial t}
\right)_{bulk-disloc}
=
-
N_d N_b
\frac{\partial V_d^*}{\partial t}
$$

Questo termine trasferisce gas dalle bolle bulk alle bolle su dislocazioni e riduce la densità numerica delle bolle bulk.

Nel modello Rizk (2025) per UN, questo termine non è incluso esplicitamente. Rizk include la coalescenza tra bolle su dislocazioni, ma non il termine di cattura bulk-dislocation nella equazione di $N_b$.

---

### 3. Coalescenza tra bolle bulk

Barani (2020) trascura esplicitamente la coalescenza tra bolle bulk nanometriche. Nel modello, le bolle bulk sono piccole e la loro evoluzione è dominata da nucleazione, trapping e re-solution.

Nel modello Rizk (2025), la densità numerica delle bolle bulk evolve come:

$$
\frac{\partial N_b}{\partial t}
=
\nu_b
-
b_b\phi_bN_b
$$

quindi non compare un termine di coalescenza bulk-bulk.

Nel modello SCIANTIX/Pizzocri standard per UO₂, la concentrazione di bolle intragranulari evolve nella forma:

$$
\frac{dN_{ig}}{dt}
=
\nu
-
bN_{ig}
$$

anche qui senza termine esplicito di coalescenza bulk-bulk.

---

### 4. Modelli classici con bolle mobili

Nei modelli classici, come quelli discussi nella letteratura di Olander, si possono considerare meccanismi di coalescenza legati alla migrazione delle bolle. In questo caso la coalescenza deriva dallo scontro tra bolle mobili, non dall’impingement di bolle immobili che crescono.

Questa forma rappresenta una coalescenza per migrazione/collisione, mentre Barani (2020) e Rizk (2025) usano una descrizione basata sulla crescita volumetrica e sull’impingement.

---



## Sintesi

La differenza principale è che Barani (2020) ha il modello più completo per la coalescenza intragranulare tra popolazioni diverse:

$$
N_d \rightarrow \text{coalescenza dislocation-dislocation}
$$

e:

$$
N_b \rightarrow \text{riduzione per cattura da bolle su dislocazioni}
$$

Rizk (2025), invece, mantiene solo il termine:

$$
-4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

nella evoluzione delle bolle su dislocazioni.

Quindi, per il modello UN attuale, la scelta coerente con Rizk è:

$$
\frac{\partial N_b}{\partial t}
=
\nu_b
-
b_b\phi_bN_b
$$

$$
\frac{\partial N_d}{\partial t}
=
\frac{N_d}{\rho_d}
\frac{\partial \rho_d}{\partial t}
-
4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

e, se $\rho_d$ è costante:

$$
\frac{\partial N_d}{\partial t}
=
-
4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

═══════════════════════════════════════════════════════════════
## Source: `UN_model_variants_report_thesis.md`
═══════════════════════════════════════════════════════════════

# Analisi comparativa delle varianti del modello di swelling intragranulare in UN

## 1. Scopo dell'analisi

Lo scopo di questa analisi è valutare se la discrepanza osservata tra il modello UN implementato e i dati sperimentali di swelling microscopico possa essere risolta mediante una semplice calibrazione dei parametri oppure richieda una modifica della formulazione fisica del modello. In particolare, il confronto è stato impostato considerando i punti sperimentali di Fig. 3 di Rizk et al. come rappresentativi della popolazione intragranulare grande, indicata come popolazione P2. In accordo con l'interpretazione fisica dell'articolo, tale popolazione è associata alle **dislocation bubbles**. Di conseguenza, il target principale della calibrazione non è lo swelling intragranulare totale, ma lo swelling dovuto alla popolazione di bolle su dislocazione:

\[
\left(\frac{\Delta V}{V}\right)_d = N_d V_d .
\]

Le curve di bulk bubbles e grain-boundary bubbles sono quindi trattate come informazioni diagnostiche, non come grandezze direttamente confrontabili con i dati sperimentali di Fig. 3.

L'analisi è stata eseguita mediante la modalità `MODEL_VARIANTS` della pipeline Python, confrontando diverse formulazioni del modello intragranulare. I parametri fisici fissi di Rizk sono stati mantenuti invariati:

| Parametro | Valore |
|---|---:|
| Grain radius | \(6.0\times10^{-6}\) m |
| Fission rate | \(5.0\times10^{19}\) fiss m\(^{-3}\) s\(^{-1}\) |
| Xe yield | 0.24 atom/fission |
| Dislocation density \(\rho_d\) | \(3.0\times10^{13}\) m\(^{-2}\) |
| Surface energy \(\gamma_b\) | 1.11 J m\(^{-2}\) |
| Fission-gas atomic volume \(\Omega_{fg}\) | \(8.5\times10^{-29}\) m\(^3\)/atom |

## 2. Grandezze sperimentali usate come target

Il confronto quantitativo è stato effettuato su tre insiemi di dati digitalizzati:

1. **Fig. 3**: swelling microscopico associato alla popolazione P2, confrontato con `swelling_d_percent`.
2. **Fig. 7**: concentrazione numerica delle bolle grandi, confrontata con `N_d`.
3. **Fig. 8**: raggio medio delle bolle grandi, confrontato con `R_d`.

La pressione delle bolle non è un dato sperimentale diretto, ma è stata usata come controllo fisico. Il modello è considerato più coerente quando le pressioni interne delle bolle bulk e dislocation restano prossime alle pressioni di equilibrio:

\[
\frac{p_b}{p_{b,eq}} \simeq 1, \qquad \frac{p_d}{p_{d,eq}} \simeq 1 .
\]

Il comportamento ad alta temperatura, oltre circa 1800 K, è stato trattato come diagnostico e non come vincolo primario, poiché in questa regione la mancanza di una descrizione completa di interconnessione, grain-boundary bubble evolution e release può alterare la risposta del modello.

## 3. Varianti fisiche analizzate

Sono state confrontate sette formulazioni del modello. Le varianti non modificano i parametri materiali fissi, ma modificano la forma di alcuni termini fisici del solver.

### 3.1 M0: baseline Rizk-like

La variante `M0_baseline` corrisponde alla formulazione attuale del modello:

- nessun termine esplicito di trasferimento di massa gas dovuto alla nucleazione;
- fattore \(\phi_b\) usato solo nell'equazione per la densità numerica delle bulk bubbles;
- nessun termine di cattura bulk-dislocation;
- coalescenza tra dislocation bubbles inclusa.

Questa è la variante più coerente con la formulazione Rizk-base implementata.

### 3.2 M1: baseline con score P2-only

La variante `M1_rescore_baseline` ha la stessa fisica di M0, ma usa esplicitamente lo score P2-only. Serve a separare l'effetto della funzione obiettivo dall'effetto della fisica del modello. In altre parole, M1 risponde alla domanda: *quanto migliora il modello base se viene valutato correttamente contro le sole dislocation bubbles?*

### 3.3 M2: nucleation mass coupling

La variante `M2_nucleation_mass` introduce un termine di bilancio di massa associato alla nucleazione bulk. Se la nucleazione di una nuova bolla bulk viene interpretata come formazione di un dimero, allora due atomi devono essere sottratti dalla soluzione e aggiunti alla popolazione bulk:

\[
\left(\frac{dc}{dt}\right)_\nu = -2\nu_b,
\qquad
\left(\frac{dm_b}{dt}\right)_\nu = +2\nu_b .
\]

Questa variante è principalmente un test di consistenza del bilancio di massa.

### 3.4 M3: phi nella re-solution gas

La variante `M3_phi_resolution` testa l'ipotesi che il fattore \(\phi\), già presente nella distruzione della densità numerica delle bulk bubbles, possa entrare anche nel termine di re-solution atomica. La forma diagnostica testata è:

\[
b_{eff} = b\phi .
\]

Nel caso migliore selezionato automaticamente, la modalità risultante è `bulk_and_dislocation`, cioè \(\phi\) modifica la re-solution sia delle bulk bubbles sia delle dislocation bubbles. Questa variante non è trattata come Rizk-base, ma come test diagnostico di sensibilità alla formulazione della re-solution.

### 3.5 M4: cattura bulk-dislocation di tipo Barani

La variante `M4_bulk_dislocation_capture` introduce un termine di cattura delle bulk bubbles da parte delle dislocation bubbles in crescita. L'idea fisica è che una bolla su dislocazione, aumentando il proprio raggio, spazzi un volume di cattura e possa incorporare bulk bubbles presenti in quel volume:

\[
V_{cap} = \frac{4}{3}\pi (R_d + R_b)^3 .
\]

La frazione catturata viene usata per trasferire gas e vacanze dalla popolazione bulk alla popolazione dislocation:

\[
m_b \rightarrow m_b(1-f_{cap}),
\qquad
m_d \rightarrow m_d + f_{cap}m_b,
\]

con procedura analoga per le vacanze. Questa variante è Barani-like e quindi non rappresenta il modello Rizk-base puro.

### 3.6 M5 e M6

Le ultime due varianti combinano gli effetti precedenti:

- `M5_nucleation_mass_plus_capture`: M2 + M4;
- `M6_phi_plus_capture`: M3 bulk-and-dislocation + M4.

M6 è la variante più aggressiva: combina riduzione della re-solution effettiva mediante \(\phi\) e trasferimento bulk-dislocation. Per questo motivo è utile come test diagnostico, ma non deve essere interpretata come formulazione Rizk-base.

## 4. Parametri variati

Per ogni variante sono stati variati i seguenti parametri:

| Parametro | Griglia |
|---|---|
| \(f_n\) | \(10^{-10}, 3\times10^{-10}, 10^{-9}, 3\times10^{-9}, 10^{-8}, 3\times10^{-8}, 10^{-7}, 3\times10^{-7}, 10^{-6}\) |
| \(K_d\) | \(1\times10^5, 2\times10^5, 3\times10^5, 5\times10^5, 8\times10^5\) bubble/m |
| \(g_{d,scale}\) | 0.5, 1.0, 2.0, 3.0, 5.0 |
| bulk seed radius | 0.0 nm |

Il parametro \(g_{d,scale}\) moltiplica solo il trapping verso dislocazioni:

\[
g_d^{eff} = g_{d,scale} g_d .
\]

Non è un parametro originale di Rizk; è stato introdotto come correzione empirica diagnostica.

## 5. Definizione dello score

Lo score principale è stato costruito come score P2-only. La componente principale confronta lo swelling sperimentale di Fig. 3 con `swelling_d_percent` per punti con \(T \leq 1700\) K. I dati Fig. 7 e Fig. 8 vincolano rispettivamente la concentrazione numerica \(N_d\) e il raggio medio \(R_d\). La pressione entra come penalità solo nell'intervallo 1200--1700 K. Lo swelling intragranulare totale e il blow-up ad alta temperatura sono invece mantenuti come diagnostiche.

In forma qualitativa:

\[
S_{main} = S_{sw,d} + S_{N_d} + S_{R_d} + S_p .
\]

Dove:

- \(S_{sw,d}\) misura l'errore su Fig. 3 rispetto allo swelling delle dislocation bubbles;
- \(S_{N_d}\) misura l'errore logaritmico sulla concentrazione delle bolle grandi;
- \(S_{R_d}\) misura l'errore logaritmico sul raggio medio;
- \(S_p\) penalizza deviazioni significative da \(p/p_{eq}\simeq1\).

## 6. Risultati finali per variante

La tabella seguente riporta i migliori candidati finali per ogni modello. La valutazione finale è stata eseguita con `dt_h = 6 h` e `n_modes = 30`.

| Modello | Variante | Score main | Score swelling P2 | Score Rd | Score Nd | Pressione | High-T diag | \(f_n\) | \(K_d\) | \(g_{d,scale}\) | Coerenza |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|
| M0 | M0_baseline | 1.33753 | 0.91975 | 0.10774 | 0.48909 | 0.00000 | 3.30721 | 1.0e-10 | 3.0e5 | 0.5 | Rizk-base |
| M1 | M1_rescore_baseline | 1.33753 | 0.91975 | 0.10774 | 0.48909 | 0.00000 | 3.30721 | 1.0e-10 | 3.0e5 | 0.5 | Rizk-base |
| M2 | M2_nucleation_mass | 1.33753 | 0.91975 | 0.10774 | 0.48909 | 0.00000 | 3.30721 | 1.0e-10 | 3.0e5 | 0.5 | Diagnostic |
| M3 | M3_phi_resolution_bulk_and_dislocation | 1.27636 | 0.83887 | 0.13145 | 0.48989 | 0.01279 | 4.83826 | 3.0e-7 | 3.0e5 | 0.5 | Diagnostic |
| M4 | M4_bulk_dislocation_capture | 1.28393 | 0.81352 | 0.18680 | 0.48522 | 0.00000 | 0.28297 | 1.0e-9 | 5.0e5 | 1.0 | Barani-like |
| M5 | M5_nucleation_mass_plus_capture | 1.28393 | 0.81352 | 0.18680 | 0.48522 | 0.00000 | 0.28297 | 1.0e-9 | 5.0e5 | 1.0 | Diagnostic + Barani-like |
| M6 | M6_phi_plus_capture | 1.26898 | 0.83588 | 0.12520 | 0.48825 | 0.01845 | 5.05507 | 3.0e-7 | 3.0e5 | 0.5 | Diagnostic + Barani-like |

Il miglior score complessivo è ottenuto da M6, con \(S_{main}=1.26898\). Tuttavia, la differenza rispetto a M3 e M4 è limitata. In particolare, M4 ha uno score principale solo leggermente peggiore, ma presenta una diagnostica high-temperature molto più stabile.

## 7. Confronto puntuale a 1.3% FIMA e 1600 K

Per interpretare meglio il significato fisico dei risultati, è utile confrontare i valori modello a 1.3% FIMA e circa 1600 K con i punti sperimentali digitalizzati più vicini:

- Fig. 3b: swelling sperimentale circa 3.53% a 1595 K;
- Fig. 8: raggio sperimentale circa 120.83 nm a 1594 K;
- Fig. 7: concentrazione sperimentale circa \(5.34\times10^{18}\) m\(^{-3}\) a 1599 K.

| Modello | \(sw_d\) [%] | \(sw_{ig}\) [%] | \(R_d\) [nm] | \(N_d\) [m\(^{-3}\)] | \(p_b/p_{b,eq}\) | \(p_d/p_{d,eq}\) |
|---|---:|---:|---:|---:|---:|---:|
| M0 | 2.75072 | 4.44437 | 93.51 | \(8.03\times10^{18}\) | 1.011 | 1.103 |
| M1 | 2.75072 | 4.44437 | 93.51 | \(8.03\times10^{18}\) | 1.011 | 1.103 |
| M2 | 2.75072 | 4.44437 | 93.51 | \(8.03\times10^{18}\) | 1.011 | 1.103 |
| M3 | 2.19892 | 4.17559 | 86.11 | \(8.22\times10^{18}\) | 1.003 | 1.151 |
| M4 | 2.64471 | 3.39548 | 77.72 | \(1.35\times10^{19}\) | 1.001 | 1.039 |
| M5 | 2.64470 | 3.39548 | 77.72 | \(1.35\times10^{19}\) | 1.001 | 1.039 |
| M6 | 2.31369 | 4.25371 | 87.69 | \(8.19\times10^{18}\) | 1.003 | 1.177 |

Questo confronto evidenzia un punto importante: il miglior score globale non implica necessariamente il miglior accordo nel singolo punto a 1600 K. Infatti, M0/M1 predicono uno swelling P2 più vicino al punto sperimentale di Fig. 3b a 1600 K rispetto a M6, mentre M6 ottiene uno score globale migliore grazie al comportamento sull'intero insieme di punti e a un compromesso più favorevole tra swelling, raggio e concentrazione.

In generale, tutti i modelli sottostimano il raggio medio a 1600 K rispetto al punto di circa 120 nm, mentre alcuni modelli compensano parzialmente mediante una concentrazione numerica più elevata. Questo è tipico di una difficoltà nella ripartizione raggio-densità: lo swelling può risultare ragionevole anche se la microstruttura non è perfettamente corretta.

## 8. Discussione per variante

### 8.1 M0/M1: modello Rizk-base

M0 e M1 danno risultati identici, come previsto, perché hanno la stessa fisica. Il miglior candidato Rizk-base è:

\[
f_n = 10^{-10}, \qquad K_d = 3\times10^5\;\mathrm{bubble/m}, \qquad g_{d,scale}=0.5 .
\]

Questo risultato indica che, nella nostra implementazione, il modello Rizk-base richiede una nucleazione bulk estremamente bassa e una riduzione empirica del trapping verso dislocazioni per avvicinarsi ai dati P2. Rispetto al nominale iniziale, il valore di \(K_d\) preferito è inferiore a \(5\times10^5\) bubble/m, ma non drasticamente lontano. Il valore di \(f_n\), invece, è molto più basso di quello nominale usato nei primi test.

Dal punto di vista fisico, M1 è la variante più difendibile come base di tesi, perché conserva la forma Rizk-like del modello. Il suo limite principale è che il raggio medio resta sottostimato e lo score high-T diagnostico è relativamente elevato.

### 8.2 M2: effetto del termine di massa della nucleazione

M2 produce risultati praticamente identici a M0/M1. Questo indica che, nell'intervallo di parametri selezionato dal fit, il termine di massa \(-2\nu_b\) in \(c\) e \(+2\nu_b\) in \(m_b\) ha un effetto trascurabile sulle grandezze target. Il motivo è coerente con il best-fit: il parametro \(f_n\) scelto è molto basso, quindi la nucleazione bulk non è il processo dominante nel candidato ottimo.

La conclusione è che il termine di massa della nucleazione è importante per completezza formale del bilancio, ma non risolve il problema di fitting della popolazione P2.

### 8.3 M3: effetto di \(\phi\) nella re-solution gas

M3 migliora lo score principale da 1.33753 a 1.27636. La riduzione è circa il 4.6% rispetto a M1. Lo score dello swelling P2 migliora più sensibilmente, passando da 0.91975 a 0.83887, cioè circa il 8.8%.

Tuttavia, M3 introduce una penalità di pressione non nulla e aumenta notevolmente la diagnostica high-temperature. Questo suggerisce che l'uso di \(b\phi\) nella re-solution atomica può migliorare la forma della curva P2, ma tende anche a rendere il modello più aggressivo ad alta temperatura. Inoltre, questa modifica non è parte del modello Rizk-base pubblicato e deve quindi essere presentata come test diagnostico.

### 8.4 M4: cattura bulk-dislocation

M4 è fisicamente interessante perché introduce un meccanismo di trasferimento da bulk bubbles a dislocation bubbles. Il miglior candidato M4 ha:

\[
f_n = 10^{-9}, \qquad K_d = 5\times10^5\;\mathrm{bubble/m}, \qquad g_{d,scale}=1 .
\]

Questo è notevole perché M4 non richiede la riduzione empirica del trapping verso dislocazioni e recupera il valore Rizk-like di \(K_d=5\times10^5\) bubble/m. Inoltre, la diagnostica high-temperature è molto bassa rispetto a M1 e M6. La pressione resta prossima all'equilibrio.

Il limite di M4 è la microstruttura a 1600 K: \(N_d\) è sovrastimato e \(R_d\) è sottostimato. Ciò significa che la cattura bulk-dislocation migliora lo swelling P2, ma produce una popolazione troppo numerosa e con raggio medio troppo piccolo rispetto ai dati Fig. 7--8.

Nonostante questo, M4 è probabilmente la variante estesa più fisicamente difendibile, perché introduce un meccanismo Barani-like plausibile senza modificare artificialmente la re-solution atomica.

### 8.5 M5: nucleation mass + capture

M5 è praticamente indistinguibile da M4. Questo conferma che il termine di massa della nucleazione non ha effetto significativo quando è presente la cattura bulk-dislocation nel regime di parametri ottimo. Pertanto, M5 non aggiunge informazione fisica rilevante rispetto a M4.

### 8.6 M6: phi + capture

M6 fornisce il miglior score complessivo:

\[
S_{main}=1.26898 .
\]

Tuttavia, il miglioramento rispetto a M4 è modesto:

\[
\frac{1.28393 - 1.26898}{1.28393} \approx 1.2\% .
\]

Inoltre, M6 presenta la più alta diagnostica high-temperature tra i candidati migliori. Ciò indica che l'accoppiamento tra \(\phi\)-resolution e cattura bulk-dislocation migliora leggermente il compromesso globale, ma al costo di una formulazione meno pulita e più instabile ad alta temperatura.

M6 non deve quindi essere interpretato come modello finale Rizk-base. È utile per mostrare che modifiche alla re-solution e trasferimento bulk-dislocation possono migliorare il fitting, ma la sua coerenza fisica richiede ulteriori verifiche.

## 9. Interpretazione complessiva

I risultati mostrano che nessuna variante riproduce perfettamente e simultaneamente:

1. lo swelling P2/dislocation di Fig. 3;
2. il raggio medio \(R_d\) di Fig. 8;
3. la concentrazione numerica \(N_d\) di Fig. 7;
4. il comportamento near-equilibrium delle pressioni;
5. la stabilità ad alta temperatura.

Il modello base Rizk-like, se riscorato correttamente come P2-only, è già competitivo. Il best M1 ha score 1.33753, mentre il best globale M6 ha score 1.26898. Il miglioramento relativo di M6 rispetto a M1 è circa 5.1%, quindi non è abbastanza grande da giustificare automaticamente l'adozione di M6 come modello fisico finale.

La cattura bulk-dislocation M4 è una variante particolarmente interessante perché migliora lo score dello swelling e riduce drasticamente la diagnostica high-temperature. Tuttavia, la microstruttura risultante mostra una densità di bolle troppo elevata e un raggio troppo basso. Questo suggerisce che il meccanismo di cattura può essere rilevante, ma la sua formulazione semplice single-size non è ancora sufficiente a riprodurre correttamente la distribuzione P2.

Il termine di massa della nucleazione M2 non modifica i risultati, mentre l'introduzione di \(\phi\) nella re-solution atomica M3/M6 migliora il fitting globale ma aumenta la sensibilità high-temperature. Questo rende il sospetto sul termine \(\phi\) interessante, ma non conclusivo.

## 10. Scelta raccomandata per la tesi

Per una tesi, la distinzione più chiara è la seguente.

### Modello principale difendibile

Usare M1 come modello principale:

\[
f_n = 10^{-10}, \qquad K_d = 3\times10^5\;\mathrm{bubble/m}, \qquad g_{d,scale}=0.5 .
\]

Motivazione:

- conserva la formulazione Rizk-base;
- usa lo score corretto P2-only;
- riproduce qualitativamente la transizione di swelling;
- mantiene le pressioni vicine all'equilibrio;
- evita di introdurre termini diagnostici non pubblicati.

Limiti da dichiarare:

- sottostima il raggio medio P2 a circa 1600 K;
- richiede un \(f_n\) molto basso;
- richiede un fattore empirico \(g_{d,scale}=0.5\);
- presenta una crescita high-temperature diagnostica ancora significativa.

### Variante estesa più promettente

Usare M4 come estensione fisicamente motivata:

\[
f_n = 10^{-9}, \qquad K_d = 5\times10^5\;\mathrm{bubble/m}, \qquad g_{d,scale}=1 .
\]

Motivazione:

- introduce un meccanismo bulk-dislocation fisicamente plausibile;
- recupera valori più vicini ai parametri nominali per \(K_d\) e \(g_d\);
- riduce fortemente la diagnostica high-temperature;
- mantiene la pressione near-equilibrium.

Limiti:

- non è Rizk-base puro;
- sottostima \(R_d\);
- sovrastima \(N_d\);
- richiede una formulazione più rigorosa della cattura e probabilmente una descrizione non single-size.

### Variante diagnostica migliore in senso numerico

M6 può essere presentata come best numerical diagnostic:

\[
f_n = 3\times10^{-7}, \qquad K_d = 3\times10^5\;\mathrm{bubble/m}, \qquad g_{d,scale}=0.5 .
\]

Motivazione:

- migliore score globale;
- mostra che re-solution modificata e capture possono migliorare il fit.

Limiti:

- combina due modifiche non Rizk-base;
- high-temperature diagnostic elevata;
- interpretazione fisica meno pulita.

## 11. Lavoro successivo raccomandato

Prima di fissare un set finale per la tesi, sono raccomandati tre controlli:

1. **Verifica numerica fine**: rilanciare M1, M4 e M6 con `dt_h = 1 h` e `n_modes = 40`, almeno per 1.3% FIMA.
2. **Verifica locale a 1600 K**: confrontare direttamente swelling, \(R_d\), \(N_d\), \(p_d/p_{eq}\) e gas partition per i tre candidati.
3. **Analisi del termine capture**: nel caso M4, verificare se il termine di cattura può essere formulato con un coefficiente di efficienza \(\eta_{cap}\), evitando eccessiva riduzione del raggio medio.

Un possibile passo successivo è quindi introdurre:

\[
dN_{b,cap} = -\eta_{cap} N_d N_b \frac{dV_{cap}}{dt},
\]

con \(0 < \eta_{cap} \leq 1\), e valutare se esiste un valore fisicamente ragionevole che migliori simultaneamente swelling, \(R_d\) e \(N_d\).

## 12. Conclusione

La model-form sensitivity mostra che il problema non è riconducibile a un solo parametro, come \(f_n\). La formulazione Rizk-base, riscorata correttamente rispetto alla popolazione P2, fornisce un risultato ragionevole ma richiede una nucleazione bulk estremamente bassa e una correzione empirica del trapping verso dislocazioni. Le estensioni fisiche migliorano il fit, ma in modo moderato.

La conclusione più robusta è che il modello intragranulare P2 è sensibile alla descrizione della re-solution e del trasferimento tra popolazioni bulk e dislocation. Tra le varianti testate, M6 fornisce il miglior score numerico, ma M4 rappresenta l'estensione fisicamente più interpretabile. Per una formulazione di tesi, M1 dovrebbe essere mantenuto come modello di riferimento Rizk-base, mentre M4 e M6 dovrebbero essere discusse come estensioni diagnostiche per identificare i limiti della formulazione corrente.


