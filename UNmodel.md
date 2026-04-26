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

Questo vale sia per le bolle bulk che per quelle su dislocazioni.

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
g_d = 4\pi D_g R_d N_d + \frac{2\pi D_g}{\ln\left(\frac{\Gamma_d}{Z_d r_d}\right) - \frac{3}{5}} (\rho_d - 2 R_d N_d)
$$

dove:
- $R_d$ raggio bolle su dislocazioni (m)
- $N_d$ concentrazione bolle su dislocazioni (bub/m³)
- $\Gamma_d = \frac{1}{\sqrt{\pi \rho_d}}$ (Wigner-Seitz radius per dislocazioni, m)
- $\rho_d = 3.0 \times 10^{13}$ m⁻² (densità dislocazioni)
- $r_d = 3.46 \times 10^{-10}$ m (raggio core dislocazioni)
- $Z_d = 5.0$

## Nucleazione

Il tasso di nucleazione per bolle bulk $\nu_b$:

$$
\nu_b = 8\pi f_n D_g \Omega_{fg}^{1/3} c^2
$$

- $f_n = 10^{-6}$
- $\Omega_{fg} = 8.5 \times 10^{-29}$ m³
- $c$ concentrazione gas in soluzione (at/m³)

## Sistema di Equazioni di Diffusione (3 equazioni)

Le variabili sono:
- $c$: gas in soluzione (at/m³)
- $m_b$: gas in bolle bulk (at/m³)
- $m_d$: gas in bolle su dislocazioni (at/m³)

Sistema:

$$
\frac{\partial c}{\partial t} = D_g \nabla^2 c - (g_b + g_d) c + b_b m_b + b_d m_d + \beta
$$

$$
\frac{\partial m_b}{\partial t} = g_b c - b_b m_b
$$

$$
\frac{\partial m_d}{\partial t} = g_d c - b_d m_d
$$

dove:
- $\beta$ produzione volumetrica di gas (at/m³·s)
- $b_b, b_d$ tassi di re-solution per bulk e dislocazioni

## Valori Iniziali

- Concentrazione iniziale bolle su dislocazioni: $N_d(0) = 3.6 \times 10^{19}$ bub/m³

## Riferimenti

- Rizk et al., JNM 606 (2025) 155604


---

## Evoluzione della concentrazione di bolle bulk

La concentrazione numerica delle bolle bulk, \(N_b\), evolve secondo:

$$
\frac{\partial N_b}{\partial t} = \nu_b - b_b \phi_b N_b
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

Il termine \(\phi_b\) corregge il fatto che la re-solution rimuove singoli atomi, mentre per distruggere una bolla servono più eventi di re-solution.

---

## Evoluzione della concentrazione di bolle su dislocazioni

Nel modello di Rizk, la densità di bolle su dislocazioni è legata alla densità di dislocazioni:

$$
N_d = K \rho_d
$$

con:

$$
K = 5.0 \times 10^5 \ \mathrm{bubble \ m^{-1}}
$$

$$
\rho_d = 3.0 \times 10^{13} \ \mathrm{m^{-2}}
$$

quindi:

$$
N_d(0) = K \rho_d = 1.5 \times 10^{19} \ \mathrm{bubble \ m^{-3}}
$$

Se si volesse modellare l’evoluzione dinamica di \(N_d\) tramite evoluzione della densità di dislocazioni:

$$
\frac{\partial N_d}{\partial t}
=
\frac{N_d}{\rho_d}
\frac{\partial \rho_d}{\partial t}
$$

Nel caso implementato in SCIANTIX-UN, però, si assume \(\rho_d\) costante, quindi normalmente:

$$
\frac{\partial \rho_d}{\partial t}=0
$$

e quindi:

$$
\frac{\partial N_d}{\partial t}=0
$$

salvo eventuali termini di coalescenza non ancora implementati.

---

## Numero medio di atomi per bolla

Per le bolle bulk:

$$
n_b = \frac{m_b}{N_b}
$$

Per le bolle su dislocazioni:

$$
n_d = \frac{m_d}{N_d}
$$

dove:
- \(m_b\) = concentrazione di gas nelle bolle bulk \([\mathrm{at/m^3}]\)
- \(m_d\) = concentrazione di gas nelle bolle su dislocazioni \([\mathrm{at/m^3}]\)
- \(N_b\) = concentrazione numerica bolle bulk \([\mathrm{bub/m^3}]\)
- \(N_d\) = concentrazione numerica bolle su dislocazioni \([\mathrm{bub/m^3}]\)

---

## Raggio delle bolle

Assumendo bolle sferiche, il volume medio di una bolla è:

$$
V_i = \frac{4}{3}\pi R_i^3
$$

con \(i=b,d\).

Il raggio si può calcolare dalla relazione:

$$
R_i =
\left(
\frac{3 V_i}{4 \pi}
\right)^{1/3}
$$

Nel caso semplificato in cui il volume della bolla sia calcolato tramite il volume atomico efficace del gas:

$$
V_i = n_i \Omega_{fg}
$$

quindi:

$$
R_i =
\left(
\frac{3 n_i \Omega_{fg}}{4 \pi}
\right)^{1/3}
$$

dove:

$$
\Omega_{fg} = 8.5 \times 10^{-29} \ \mathrm{m^3}
$$

---

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

---

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

---

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

---

## Swelling da prodotti di fissione solidi

Rizk usa una correlazione semplice per lo swelling da prodotti di fissione solidi:

$$
\left(\frac{\Delta V}{V}\right)_{solid}
=
0.5 B
$$

dove \(B\) è il burnup espresso in FIMA.

Se \(B\) è espresso in percento FIMA, la forma equivalente è:

$$
\left(\frac{\Delta V}{V}\right)_{solid}
=
0.005 B_{\%FIMA}
$$

Questa correlazione rappresenta circa \(0.5\%\) di swelling per ogni \(1\%\) FIMA.

---

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
- \((\Delta V/V)_{solid}\) = swelling da prodotti solidi
- \((\Delta V/V)_{ig}\) = swelling gassoso intragranulare
- \((\Delta V/V)_{gf}\) = swelling da bolle ai bordi di grano

Nel modello SCIANTIX-UN attuale, se le bolle intergranulari non sono ancora implementate, si può porre temporaneamente:

$$
\left(\frac{\Delta V}{V}\right)_{gf} = 0
$$

---

## Parametri UN da Rizk

| Simbolo | Valore | Unità | Significato |
|---|---:|---|---|
| \(D_{10}^{Xe}\) | \(1.56 \times 10^{-3}\) | \(\mathrm{m^2/s}\) | prefattore diffusione termica Xe |
| \(Q_1^{Xe}\) | \(4.94\) | eV | energia attivazione Xe |
| \(A_3^{Xe}\) | \(1.85 \times 10^{-39}\) | \(\mathrm{m^5}\) | coefficiente mixing irradiation-induced |
| \(D_{10}^{V_U}\) | \(1.35 \times 10^{-2}\) | \(\mathrm{m^2/s}\) | prefattore diffusione termica vacanze U |
| \(Q_1^{V_U}\) | \(5.66\) | eV | energia attivazione vacanze U |
| \(A_{20}^{V_U}\) | \(1.32 \times 10^{-19}\) | \(\mathrm{m^{7/2}/s^{1/2}}\) | coefficiente irradiation-enhanced vacanze |
| \(B_{21}^{V_U}\) | \(-0.62\) | eV | parametro fit \(D_2\) vacanze |
| \(B_{22}^{V_U}\) | \(-0.04\) | \(\mathrm{eV^2}\) | parametro fit \(D_2\) vacanze |
| \(\Omega_{fg}\) | \(8.5 \times 10^{-29}\) | \(\mathrm{m^3}\) | volume atomico gas fissione |
| \(f_n\) | \(10^{-6}\) | / | fattore nucleazione omogenea |
| \(a\) | \(4.889 \times 10^{-10}\) | m | parametro reticolare UN |
| \(\Omega\) | \(a^3/4\) | \(\mathrm{m^3}\) | volume atomico matrice UN |
| \(\gamma\) | \(1.11\) | \(\mathrm{J/m^2}\) | energia superficiale UN-bolla |
| \(D_v^{gb}\) | \(10^6 D_1^U\) | \(\mathrm{m^2/s}\) | diffusività vacanze al bordo grano |
| \(\delta_{gb}\) | \(4.0 \times 10^{-10}\) | m | spessore layer diffusivo bordo grano |
| \(N_{gf,0}\) | \(2.0 \times 10^{13}\) | \(\mathrm{bub/m^2}\) | densità iniziale bolle grain-face |
| \(\theta\) | \(59^\circ\) | gradi | semi-angolo diedro |
| \(R_{gf}\) | \(2.42 \times 10^{-10}\) | m | raggio iniziale grain-face |
| \(r_{gr}\) | \(6.0 \times 10^{-6}\) | m | raggio grano |
| \(F_{c,sat}\) | \(0.5\) | / | copertura grain-face a saturazione |
| \(K\) | \(5.0 \times 10^5\) | \(\mathrm{bubble/m}\) | bolle per lunghezza di dislocazione |
| \(r_d\) | \(a/\sqrt{2}\) | m | raggio/core dislocazione |
| \(r_d\) | \(3.46 \times 10^{-10}\) | m | valore numerico |
| \(\rho_d\) | \(3.0 \times 10^{13}\) | \(\mathrm{m^{-2}}\) | densità di dislocazioni |
| \(Z_d\) | \(5.0\) | / | trapping radius factor dislocazioni |
| \(k_B\) | \(8.617333262 \times 10^{-5}\) | eV/K | costante di Boltzmann |

---

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

Per le vacanze non si usa il termine \(D_3\), perché \(D_3\) rappresenta il mixing balistico usato per Xe.

---

## Diffusività delle vacanze ai bordi di grano

Rizk assume:

$$
D_v^{gb} = 10^6 D_1^U
$$

dove \(D_1^U\) è la diffusività termica dei difetti di Uranio.

---

## Pressione interna della bolla

La pressione del gas nella bolla può essere calcolata con l’equazione di stato hard-sphere tipo Carnahan-Starling:

$$
\frac{pV}{n k_B T}
=
\frac{1+\eta+\eta^2-\eta^3}{(1-\eta)^3}
$$

dove:
- \(p\) = pressione interna della bolla
- \(V\) = volume della bolla
- \(n\) = numero di atomi nella bolla
- \(T\) = temperatura
- \(k_B\) = costante di Boltzmann
- \(\eta\) = packing fraction

La packing fraction è:

$$
\eta =
\frac{\pi}{6} d_{HS}^3 \frac{n}{V}
$$

Il diametro hard-sphere dello Xe è:

$$
d_{HS}
=
4.45 \times 10^{-10}
\left[
0.8542
-
0.03996 \log\left(\frac{T}{231.2}\right)
\right]
$$

---

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
- \(\gamma = 1.11 \ \mathrm{J/m^2}\)
- \(R\) = raggio bolla
- \(\sigma_h\) = stress idrostatico

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

---

## Crescita per assorbimento di vacanze

La crescita delle bolle può essere guidata dall’assorbimento di vacanze quando:

$$
p > p_{eq}
$$

Il rate di assorbimento di vacanze può essere scritto nella forma Speight-Beere:

$$
\frac{dn_v}{dt}
=
\frac{2\pi D_v d}{k_B T \zeta}
\left(
p - p_{eq}
\right)
$$

dove:
- \(n_v\) = numero di vacanze nella bolla
- \(D_v\) = diffusività delle vacanze
- \(d\) = raggio della cella di Wigner-Seitz associata alla bolla
- \(\zeta\) = fattore geometrico
- \(p-p_{eq}\) = sovrapressione della bolla

Per le bolle su dislocazioni:

$$
d =
\frac{1}{\sqrt{\pi N_d/\rho_d}}
$$

oppure, in forma equivalente, se \(K=N_d/\rho_d\):

$$
d =
\frac{1}{\sqrt{\pi K}}
$$

Il fattore geometrico è:

$$
\zeta =
10\xi
\frac{
1+\xi^3
}{
-\xi^6 + 5\xi^2 - 9\xi + 5
}
$$

con:

$$
\xi = \frac{R_d}{d}
$$

Nota: questa parte è importante per descrivere il breakaway swelling, ma nel tuo stato attuale sembra non ancora implementata completamente in SCIANTIX-UN.

---

## Bolle ai bordi di grano

Rizk include anche una popolazione intergranulare, indicata come grain-face bubbles.

La densità iniziale è:

$$
N_{gf,0} = 2.0 \times 10^{13} \ \mathrm{bub/m^2}
$$

La copertura frazionaria delle facce di grano è:

$$
F_c = N_{gf} A_{gf}
$$

dove \(A_{gf}\) è l’area proiettata media di una bolla sul bordo di grano.

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

---

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

dove \(\dot{q}_{gb}\) è il flusso/rate di gas che raggiunge il bordo di grano.

Nel modello SCIANTIX-UN attuale, questa parte può essere lasciata come estensione futura se il rilascio intergranulare non è ancora accoppiato.

---

## Note implementative importanti

1. Nel modello attuale SCIANTIX-UN sono già presenti tre concentrazioni:
   - \(c\): gas in soluzione
   - \(m_b\): gas in bolle bulk
   - \(m_d\): gas in bolle su dislocazioni

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

3. Le equazioni mancanti da aggiungere al modello sono quindi soprattutto:
   - evoluzione \(N_b\)
   - definizione \(N_d = K\rho_d\)
   - calcolo \(R_b\), \(R_d\)
   - swelling bulk/dislocation
   - eventuale crescita tramite vacanze
   - eventuale popolazione intergranulare e FGR

4. Per ora, se si vuole rimanere coerenti con l’implementazione minima:
   - usare \(N_d\) costante
   - usare \(N_b\) evolutivo
   - calcolare \(R_b\) e \(R_d\) da \(m_i/N_i\)
   - calcolare swelling intragranulare come somma bulk + dislocation
   - lasciare grain-boundary bubbles/FGR come TODO