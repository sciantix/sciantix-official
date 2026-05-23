# Modello intergranulare UN — grain-face bubbles e fission gas release

Questo documento prosegue il modello intragranulare descritto in `UNmodel.md`, introducendo la popolazione di bolle intergranulari (*grain-face bubbles*) e il rilascio di gas di fissione. La formulazione segue il modello SIFGRS/BISON riassunto da Rizk et al. (2025) per UN e, dove Rizk rimanda alla formulazione precedente, le equazioni sono riportate nella forma di Pastore et al. (2013) / White-type grain-face bubble model.

Il documento contiene solo modello fisico, equazioni, definizioni e parametri. Non include indicazioni di codice, sensitivity analysis o workflow implementativo.

---

## 1. Collegamento con il modello intragranulare

Nel modello intragranulare UN si risolvono le popolazioni:

$$
c,\qquad m_b,\qquad m_d
$$

dove:

- $c$ è il gas in soluzione nella matrice;
- $m_b$ è il gas nelle bolle bulk intragranulari;
- $m_d$ è il gas nelle bolle su dislocazione.

La parte intragranulare fornisce un rate di gas che raggiunge i bordi di grano:

$$
\dot q_{gb}
\qquad [\mathrm{at\,m^{-3}\,s^{-1}}].
$$

Il modello intergranulare separa questo gas in:

$$
q_{gb}=q_{gf}+q_{rel}
$$

dove:

- $q_{gf}$ è il gas contenuto nelle bolle grain-face, espresso per volume di combustibile;
- $q_{rel}$ è il gas rilasciato, espresso per volume di combustibile.

La catena fisica è quindi:

$$
\text{gas intragranulare}
\rightarrow
\text{grain boundary}
\rightarrow
\text{grain-face bubbles}
\rightarrow
\text{coalescenza/interconnessione}
\rightarrow
\text{fission gas release}.
$$

---

## 2. Assunzioni del modello intergranulare

Il modello grain-face adotta le seguenti ipotesi:

1. le bolle intergranulari sono localizzate sulle facce di grano;
2. le peculiarità dei grain edges, dove si incontrano tre grani, sono trascurate;
3. esiste una densità iniziale di nucleation sites sul grain face;
4. la nucleazione successiva durante irraggiamento è trascurata;
5. tutte le bolle grain-face hanno, a ogni istante, la stessa dimensione media;
6. le bolle grain-face hanno forma lenticolare con proiezione circolare sul bordo di grano;
7. il gas assorbito dalle bolle grain-face è uguale al gas che arriva al grain boundary;
8. la re-solution dalle grain-boundary bubbles è trascurata;
9. la crescita delle bolle avviene per arrivo di gas e assorbimento/emissione di vacanze;
10. la coalescenza riduce il numero di bolle e aumenta la dimensione media;
11. il rilascio termico avviene quando la coverage raggiunge una soglia di saturazione.

---

## 3. Variabili principali

### 3.1 Numero di bolle grain-face

La densità numerica delle bolle intergranulari è areale:

$$
N_{gf}
\qquad [\mathrm{bub\,m^{-2}}].
$$

Rizk assume:

$$
N_{gf,0}
=
2.0\times10^{13}\ \mathrm{bub\,m^{-2}}.
$$

Questo valore è ereditato da modelli/dati precedenti su UO$_2$.

---

### 3.2 Densità volumetrica equivalente

Per collegare la popolazione areale al volume di combustibile, si usa il rapporto superficie/volume del grano.

Per grani sferici di raggio $r_{gr}$:

$$
\frac{S}{V}
=
\frac{3}{r_{gr}}.
$$

Poiché ogni grain boundary è condiviso da due grani, la densità volumetrica equivalente delle bolle grain-face è:

$$
\mathcal N_{gf}
=
\frac{1}{2}\frac{3}{r_{gr}}N_{gf}
=
\frac{3N_{gf}}{2r_{gr}}
\qquad [\mathrm{bub\,m^{-3}}].
$$

Quindi:

$$
q_{gf}=\mathcal N_{gf} n_g
$$

dove $n_g$ è il numero medio di atomi di gas per bolla grain-face.

---

### 3.3 Variabili per bolla

Per una bolla grain-face media:

$$
n_g
\qquad [\mathrm{at/bubble}]
$$

$$
n_v
\qquad [\mathrm{vac/bubble}]
$$

$$
V_{gf}
\qquad [\mathrm{m^3/bubble}]
$$

$$
R_{gf}
\qquad [\mathrm{m}]
$$

$$
A_{gf}
\qquad [\mathrm{m^2}]
$$

$$
F_c
\qquad [-]
$$

dove:

- $n_g$ è il numero medio di atomi di gas nella bolla;
- $n_v$ è il numero medio di vacanze nella bolla;
- $V_{gf}$ è il volume medio della bolla;
- $R_{gf}$ è il raggio di curvatura della lente;
- $A_{gf}$ è l'area proiettata della bolla sul grain face;
- $F_c$ è la frazione di faccia di grano coperta dalle bolle.

---

## 4. Parametri nominali per UN

| Simbolo | Valore | Unità | Significato |
|---|---:|---|---|
| $N_{gf,0}$ | $2.0\times10^{13}$ | $\mathrm{bub\,m^{-2}}$ | densità iniziale grain-face bubbles |
| $r_{gr}$ | $6.0\times10^{-6}$ | m | raggio medio del grano |
| $D_v^{gb}$ | $10^6D_{U,1}$ | $\mathrm{m^2\,s^{-1}}$ | diffusività vacanze lungo grain boundary |
| $\delta_{gb}$ | $4.0\times10^{-10}$ | m | spessore dello strato diffusivo al grain boundary |
| $F_{c,sat}$ | $0.5$ | - | saturation coverage |
| $\theta$ | $59^\circ$ | deg | semi-dihedral angle |
| $R_{gf,0}$ | $2.42\times10^{-10}$ | m | raggio iniziale grain-face bubble |
| $\gamma_b$ | $1.11$ | $\mathrm{J\,m^{-2}}$ | surface energy UN/gas |
| $\gamma_{GB}$ | $1.1391$ | $\mathrm{J\,m^{-2}}$ | grain-boundary energy media |
| $\Omega_{fg}$ | $8.5\times10^{-29}$ | $\mathrm{m^3\,at^{-1}}$ | volume atomico gas di fissione |
| $a$ | $4.889\times10^{-10}$ | m | parametro reticolare UN |
| $\Omega$ | $a^3/4$ | $\mathrm{m^3\,vac^{-1}}$ | volume atomico/vacancy della matrice |
| $k_B$ | $1.380649\times10^{-23}$ | $\mathrm{J\,K^{-1}}$ | costante di Boltzmann per termini meccanici |

Il volume atomico della matrice è:

$$
\Omega
=
\frac{a^3}{4}
=
\frac{(4.889\times10^{-10})^3}{4}
\simeq
2.92\times10^{-29}\ \mathrm{m^3}.
$$

---

## 5. Angolo diedro e forma lenticolare

Le bolle intergranulari non sono trattate come sfere, ma come lenti con proiezione circolare sulla faccia di grano.

Il semi-angolo diedro $\theta$ è ottenuto dal bilancio energetico alla linea di contatto:

$$
\cos\theta
=
\frac{\gamma_{GB}}{2\gamma_b}.
$$

Con:

$$
\gamma_{GB}=1.1391\ \mathrm{J\,m^{-2}},
$$

$$
\gamma_b=1.11\ \mathrm{J\,m^{-2}},
$$

si ottiene:

$$
\cos\theta
=
\frac{1.1391}{2\cdot1.11}
\simeq0.513,
$$

$$
\theta\simeq59^\circ.
$$

Definendo:

$$
f_\theta
=
1-\frac{3}{2}\cos\theta+\frac{1}{2}\cos^3\theta,
$$

il volume della bolla lenticolare è:

$$
V_{gf}
=
\frac{4}{3}\pi R_{gf}^3 f_\theta.
$$

Di conseguenza:

$$
R_{gf}
=
\left(
\frac{3V_{gf}}{4\pi f_\theta}
\right)^{1/3}.
$$

L'area proiettata sul grain face è:

$$
A_{gf}
=
\pi R_{proj}^2
=
\pi R_{gf}^2\sin^2\theta.
$$

La coverage è:

$$
F_c=N_{gf}A_{gf}.
$$

Nota: nel testo estratto di Rizk/Pastore la potenza del termine finale può comparire come $\cos^2\theta$ per problemi di parsing/OCR; la geometria della lente sferica e la formulazione SIFGRS/U$_3$Si$_2$ usano il termine $\cos^3\theta$.

---

## 6. Gas assorbito dalle grain-face bubbles

Il rate di assorbimento di gas nelle grain-boundary bubbles è assunto uguale al rate di gas che raggiunge i grain boundaries.

Prima della saturazione:

$$
\frac{dq_{gf}}{dt}
=
\dot q_{gb},
$$

$$
\frac{dq_{rel}}{dt}=0.
$$

In termini di atomi per bolla:

$$
\frac{dn_g}{dt}
=
\frac{1}{\mathcal N_{gf}}
\frac{dq_{gf}}{dt}.
$$

Poiché:

$$
\mathcal N_{gf}=\frac{3N_{gf}}{2r_{gr}},
$$

si ha:

$$
\frac{dn_g}{dt}
=
\frac{2r_{gr}}{3N_{gf}}
\frac{dq_{gf}}{dt}.
$$

Prima della saturazione, quindi:

$$
\frac{dn_g}{dt}
=
\frac{2r_{gr}}{3N_{gf}}
\dot q_{gb}.
$$

---

## 7. Re-solution dalle grain-boundary bubbles

La re-solution dalle grain-boundary bubbles è trascurata:

$$
b_{gf}=0.
$$

Il motivo fisico è che il gas eventualmente risolto da una bolla intergranulare è assunto come rapidamente ricatturato dal grain boundary.

Quindi, a differenza delle popolazioni intragranulari:

$$
b_b\ne0,
\qquad
b_d\ne0,
\qquad
b_{gf}=0.
$$

---

## 8. Diffusività delle vacanze al grain boundary

La crescita per assorbimento di vacanze usa una diffusività efficace lungo il grain boundary:

$$
D_v^{gb}=10^6D_{U,1}.
$$

Per UN:

$$
D_{U,1}
=
D_{U,1}^{0}
\exp\left(-\frac{Q_U}{k_BT}\right),
$$

con:

$$
D_{U,1}^{0}=1.35\times10^{-2}\ \mathrm{m^2\,s^{-1}},
$$

$$
Q_U=5.66\ \mathrm{eV},
$$

e:

$$
k_B=8.617333262\times10^{-5}\ \mathrm{eV\,K^{-1}}
$$

quando l'energia è espressa in eV.

Quindi:

$$
D_v^{gb}
=
10^6
\left[
1.35\times10^{-2}
\exp\left(
-\frac{5.66}{8.617333262\times10^{-5}T}
\right)
\right].
$$

---

## 9. Pressione interna e pressione di equilibrio

### 9.1 Pressione interna

La pressione interna della bolla è calcolata con la forma semplificata dell'equazione di stato di Van der Waals:

$$
p_{gf}
=
\frac{k_BT}{\Omega}
\frac{n_g}{n_v}.
$$

Equivalente:

$$
p_{gf}
=
\frac{k_BT n_g}{n_v\Omega}.
$$

Qui $k_B$ deve essere in unità SI:

$$
k_B=1.380649\times10^{-23}\ \mathrm{J\,K^{-1}}.
$$

---

### 9.2 Pressione di equilibrio

La pressione meccanica di equilibrio è:

$$
p_{gf}^{eq}
=
\frac{2\gamma_b}{R_{gf}}-\sigma_h.
$$

Dove:

- $\gamma_b$ è la surface energy UN/gas;
- $R_{gf}$ è il raggio di curvatura della bolla;
- $\sigma_h$ è la tensione idrostatica.

Con convenzione Pastore/SIFGRS, la compressione è negativa. Quindi, se il solido è in compressione:

$$
\sigma_h<0
$$

e il termine:

$$
-\sigma_h>0
$$

aumenta la pressione di equilibrio richiesta per la crescita.

Nel caso senza accoppiamento meccanico:

$$
\sigma_h=0,
$$

quindi:

$$
p_{gf}^{eq}
=
\frac{2\gamma_b}{R_{gf}}.
$$

---

## 10. Vacancy absorption / emission

La bolla grain-face cresce o si riduce assorbendo o emettendo vacanze dal grain boundary.

La legge Speight-Beere/SIFGRS è:

$$
\frac{dn_v}{dt}
=
\frac{2\pi D_v^{gb}\delta_{gb}}
{k_BT\,\zeta_{gf}}
\left(
p_{gf}-p_{gf}^{eq}
\right).
$$

Dove:

- $D_v^{gb}$ è la diffusività delle vacanze lungo grain boundary;
- $\delta_{gb}$ è lo spessore dello strato diffusivo al grain boundary;
- $\zeta_{gf}$ è il fattore geometrico;
- $p_{gf}-p_{gf}^{eq}$ è la driving force meccanica.

Se:

$$
p_{gf}>p_{gf}^{eq},
$$

la bolla è sovrapressurizzata e assorbe vacanze:

$$
\frac{dn_v}{dt}>0.
$$

Se:

$$
p_{gf}<p_{gf}^{eq},
$$

la forma teorica consente emissione di vacanze:

$$
\frac{dn_v}{dt}<0.
$$

---

## 11. Fattore geometrico per vacancy capture

Per grain-face bubbles il fattore geometrico è:

$$
\zeta_{gf}
=
-
\frac{
(3-F_c)(1-F_c)+2\ln F_c
}{4}.
$$

Dove:

$$
F_c=N_{gf}A_{gf}.
$$

Questa forma sostituisce il fattore geometrico intragranulare basato su Wigner-Seitz cell. La ragione è che il trasporto di vacanze verso una bolla grain-face è vincolato alla geometria bidimensionale del bordo di grano.

La formula è definita per:

$$
0<F_c<1.
$$

Nel modello fisico, $F_c$ evolve fino alla soglia:

$$
F_{c,sat}=0.5.
$$

---

## 12. Volume della bolla

Il volume della bolla è dato dal contributo del gas e delle vacanze:

$$
V_{gf}
=
\Omega_{fg}n_g
+
\Omega n_v.
$$

La variazione di volume dovuta alla sola crescita fisica è:

$$
\left(\frac{dV_{gf}}{dt}\right)_g
=
\Omega_{fg}\frac{dn_g}{dt}
+
\Omega\frac{dn_v}{dt}.
$$

Qui il pedice $g$ indica *growth*, cioè crescita per arrivo di gas e vacanze, non per coalescenza.

---

## 13. Swelling intergranulare

Lo swelling volumetrico dovuto alle bolle grain-face è:

$$
\left(\frac{\Delta V}{V}\right)_{gf}
=
\frac{1}{2}\frac{3}{r_{gr}}N_{gf}V_{gf}.
$$

Usando la densità volumetrica equivalente:

$$
\left(\frac{\Delta V}{V}\right)_{gf}
=
\mathcal N_{gf}V_{gf}.
$$

La formula contiene il fattore $1/2$ perché una bolla grain-face è condivisa da due grani.

---

## 14. Coalescenza delle grain-face bubbles

La coalescenza descrive il processo:

$$
2\ \text{bolle}
\rightarrow
1\ \text{bolla più grande}.
$$

Durante la coalescenza:

- il numero areale di bolle diminuisce;
- il volume medio per bolla aumenta;
- il contenuto totale di gas e vacanze sul grain face è conservato, finché non avviene release.

La coalescenza è legata all'aumento dell'area proiettata delle bolle sul grain face. Nel modello White, se una bolla aumenta la sua area proiettata di $dA_{gf}$, può interagire con le bolle vicine. Per bolle circolari uguali in una griglia quadrata, la perdita di densità numerica dovuta alla coalescenza è:

$$
\left(\frac{dN_{gf}}{dt}\right)_c
=
-2N_{gf}^2
\left(\frac{dA_{gf}}{dt}\right)_g.
$$

Il fattore 2 deriva dal fatto che il fattore geometrico iniziale 4 viene dimezzato per non contare due volte la stessa interazione.

---

## 15. Correzione Pastore al modello di White

Pastore et al. modificano il modello di White imponendo che la nuova bolla coalescente conservi il contenuto di gas e vacanze delle due bolle parenti. Quindi la coalescenza conserva il volume totale delle bolle per unità di superficie:

$$
\left[
N_{gf}V_{gf}
\right]_c
=
\mathrm{costante}.
$$

Di conseguenza:

$$
\left(\frac{dV_{gf}}{dt}\right)_c
=
-
\frac{V_{gf}}{N_{gf}}
\left(\frac{dN_{gf}}{dt}\right)_c.
$$

Il volume medio evolve come:

$$
\frac{dV_{gf}}{dt}
=
\left(\frac{dV_{gf}}{dt}\right)_g
+
\left(\frac{dV_{gf}}{dt}\right)_c.
$$

Poiché, a $\theta$ costante:

$$
V_{gf}\propto A_{gf}^{3/2},
$$

la variazione totale dell'area media può essere scritta come:

$$
\frac{dA_{gf}}{dt}
=
\left(\frac{dA_{gf}}{dt}\right)_g
+
\left(\frac{dA_{gf}}{dt}\right)_c.
$$

Combinando la perdita di numero per coalescenza con la conservazione del volume totale per unità di superficie, Pastore ottiene:

$$
\boxed{
\frac{dN_{gf}}{dt}
=
-
\frac{6N_{gf}^2}
{3+4N_{gf}A_{gf}}
\frac{dA_{gf}}{dt}
}
\qquad
\text{per }F_c<F_{c,sat}.
$$

Questa equazione descrive la riduzione di $N_{gf}$ per coalescenza prima della saturazione.

---

## 16. Grain-face saturation e interconnection

La frazione coperta dalle bolle è:

$$
F_c=N_{gf}A_{gf}.
$$

La saturazione avviene quando:

$$
F_c=F_{c,sat}.
$$

Rizk usa:

$$
F_{c,sat}=0.5.
$$

Fisicamente, questa soglia rappresenta l'interconnessione delle bolle grain-face e la formazione di canali verso porosità aperta/free volume.

Dopo la saturazione, la coverage viene mantenuta costante:

$$
\frac{dF_c}{dt}
=
\frac{d}{dt}
\left(
N_{gf}A_{gf}
\right)
=
0.
$$

Quindi:

$$
N_{gf}A_{gf}=F_{c,sat}.
$$

Derivando:

$$
A_{gf}\frac{dN_{gf}}{dt}
+
N_{gf}\frac{dA_{gf}}{dt}
=
0.
$$

Da cui:

$$
\boxed{
\frac{dN_{gf}}{dt}
=
-
\frac{N_{gf}}{A_{gf}}
\frac{dA_{gf}}{dt}
}
\qquad
\text{per }F_c=F_{c,sat}.
$$

Questo termine è associato al rilascio di gas: la crescita ulteriore delle bolle viene compensata dalla perdita di gas e dalla riduzione della densità numerica efficace, mantenendo costante la coverage.

---

## 17. Equazione completa per la densità di bolle grain-face

La legge per $N_{gf}$ è quindi:

$$
\frac{dN_{gf}}{dt}
=
-
\frac{6N_{gf}^2}
{3+4N_{gf}A_{gf}}
\frac{dA_{gf}}{dt}
\qquad
\text{se }N_{gf}A_{gf}<F_{c,sat},
$$

$$
\frac{dN_{gf}}{dt}
=
-
\frac{N_{gf}}{A_{gf}}
\frac{dA_{gf}}{dt}
\qquad
\text{se }N_{gf}A_{gf}=F_{c,sat}.
$$

Prima della saturazione la riduzione di $N_{gf}$ è controllata dalla coalescenza.

Dopo la saturazione la riduzione di $N_{gf}$ è controllata dal rilascio necessario a mantenere:

$$
F_c=F_{c,sat}.
$$

---

## 18. Fission gas release

Il rilascio è nullo prima della saturazione:

$$
\frac{dn_{fgr}^{surf}}{dt}
=
0
\qquad
\text{se }N_{gf}A_{gf}<F_{c,sat}.
$$

Dopo saturazione:

$$
\frac{dn_{fgr}^{surf}}{dt}
=
n_g
\frac{N_{gf}}{A_{gf}}
\frac{dA_{gf}}{dt}
\qquad
\text{se }N_{gf}A_{gf}=F_{c,sat}.
$$

Questa è la forma areale, cioè riferita alla superficie di grain boundary.

Per convertirla in rate volumetrico nel combustibile:

$$
\frac{dq_{rel}}{dt}
=
\frac{1}{2}\frac{3}{r_{gr}}
\frac{dn_{fgr}^{surf}}{dt}.
$$

Quindi:

$$
\boxed{
\frac{dq_{rel}}{dt}
=
\frac{3}{2r_{gr}}
n_g
\frac{N_{gf}}{A_{gf}}
\frac{dA_{gf}}{dt}
}
\qquad
\text{se }F_c=F_{c,sat}.
$$

La relazione esprime il fatto che, dopo interconnessione, l'ulteriore crescita geometrica delle bolle non si traduce in aumento indefinito della coverage, ma in perdita di gas verso il free volume.

---

## 19. Gas trattenuto nelle grain-face bubbles

Il gas contenuto nelle bolle grain-face per volume di combustibile è:

$$
q_{gf}
=
\mathcal N_{gf}n_g
=
\frac{3N_{gf}}{2r_{gr}}n_g.
$$

Il gas totale arrivato al grain boundary soddisfa:

$$
\dot q_{gb}
=
\frac{dq_{gf}}{dt}
+
\frac{dq_{rel}}{dt}.
$$

Prima della saturazione:

$$
\frac{dq_{rel}}{dt}=0,
\qquad
\frac{dq_{gf}}{dt}=\dot q_{gb}.
$$

Dopo la saturazione:

$$
\frac{dq_{rel}}{dt}>0,
\qquad
\frac{dq_{gf}}{dt}
=
\dot q_{gb}
-
\frac{dq_{rel}}{dt}.
$$

Quindi il gas nelle grain-face bubbles non è imposto costante in assoluto. È determinato dal bilancio tra gas arrivato dal grano e gas rilasciato dopo interconnessione.

---

## 20. Bilancio complessivo del gas

Il bilancio del gas del modello completo può essere scritto come:

$$
q_{prod}
=
c+m_b+m_d+q_{gf}+q_{rel}
$$

dove:

- $q_{prod}$ è il gas prodotto;
- $c$ è il gas in soluzione;
- $m_b$ è il gas nelle bolle bulk;
- $m_d$ è il gas nelle bolle su dislocazione;
- $q_{gf}$ è il gas nelle grain-face bubbles;
- $q_{rel}$ è il gas rilasciato.

Per la sola parte intergranulare:

$$
\dot q_{gb}
=
\dot q_{gf}
+
\dot q_{rel}.
$$

---

## 21. Loop punching threshold

Rizk calcola anche una soglia di pressione per dislocation loop punching:

$$
P_{dis}
=
\frac{Gb}{R}
+
p^{eq}.
$$

Dove:

$$
G=\frac{E}{2(1+\nu)}
$$

è il modulo di taglio, $b$ è il modulo del Burgers vector e $R$ è il raggio della bolla.

Per le bolle grain-face:

$$
P_{dis,gf}
=
\frac{Gb}{R_{gf}}
+
p_{gf}^{eq}.
$$

Se:

$$
p_{gf}>P_{dis,gf},
$$

la bolla è sufficientemente sovrapressurizzata da poter rilassare la pressione tramite emissione di un loop di dislocazione. Rizk nota che sotto circa $1300\ \mathrm{K}$ le grain-boundary bubbles possono superare questa soglia. Tuttavia, il loop punching non è incluso come meccanismo di crescita nel modello di base: è una diagnostica fisica del regime di sovrapressione.

---

## 22. Stato finale del modello intergranulare

Il modello intergranulare UN è descritto dal seguente insieme di equazioni:

Gas verso grain-face bubbles:

$$
\frac{dn_g}{dt}
=
\frac{1}{\mathcal N_{gf}}
\left(
\dot q_{gb}
-
\dot q_{rel}
\right).
$$

Vacanze:

$$
\frac{dn_v}{dt}
=
\frac{2\pi D_v^{gb}\delta_{gb}}
{k_BT\zeta_{gf}}
\left(
p_{gf}-p_{gf}^{eq}
\right).
$$

Volume:

$$
V_{gf}
=
\Omega_{fg}n_g+\Omega n_v.
$$

Geometria:

$$
R_{gf}
=
\left(
\frac{3V_{gf}}{4\pi f_\theta}
\right)^{1/3},
$$

$$
A_{gf}
=
\pi R_{gf}^2\sin^2\theta,
$$

$$
F_c=N_{gf}A_{gf}.
$$

Pressioni:

$$
p_{gf}
=
\frac{k_BTn_g}{n_v\Omega},
$$

$$
p_{gf}^{eq}
=
\frac{2\gamma_b}{R_{gf}}-\sigma_h.
$$

Fattore geometrico grain-boundary:

$$
\zeta_{gf}
=
-
\frac{
(3-F_c)(1-F_c)+2\ln F_c
}{4}.
$$

Evoluzione del numero di bolle:

$$
\frac{dN_{gf}}{dt}
=
-
\frac{6N_{gf}^2}
{3+4N_{gf}A_{gf}}
\frac{dA_{gf}}{dt}
\qquad
(F_c<F_{c,sat}),
$$

$$
\frac{dN_{gf}}{dt}
=
-
\frac{N_{gf}}{A_{gf}}
\frac{dA_{gf}}{dt}
\qquad
(F_c=F_{c,sat}).
$$

Release:

$$
\frac{dq_{rel}}{dt}=0
\qquad
(F_c<F_{c,sat}),
$$

$$
\frac{dq_{rel}}{dt}
=
\frac{3}{2r_{gr}}
n_g
\frac{N_{gf}}{A_{gf}}
\frac{dA_{gf}}{dt}
\qquad
(F_c=F_{c,sat}).
$$

Swelling intergranulare:

$$
\left(\frac{\Delta V}{V}\right)_{gf}
=
\frac{3N_{gf}V_{gf}}{2r_{gr}}.
$$

---

## 23. Note sui punti non completamente specifici in Rizk

1. Rizk riporta esplicitamente la forma di $\zeta_{gf}$, la geometria lenticolare, $N_{gf,0}$, $D_v^{gb}$, $\delta_{gb}$, $F_{c,sat}$ e la logica di saturation/release.

2. Rizk descrive la coalescenza come già stimata in lavori precedenti tramite griglia quadrata di proiezioni circolari; le equazioni esplicite per coalescenza e release sono quelle riportate da Pastore et al. nella formulazione SIFGRS/White-type.

3. La potenza del termine finale nel fattore geometrico della lente deve essere trattata con attenzione: la geometria della lente sferica usa $\cos^3\theta$, mentre alcuni testi estratti/OCR possono mostrare $\cos^2\theta$.

4. I parametri $N_{gf,0}$, $F_{c,sat}$ e $\delta_{gb}$ sono ereditati da formulazioni UO$_2$/UC, non da una misura diretta specifica UN.

---

## 24. Riferimenti

- J.T. Rizk et al., *Mechanistic nuclear fuel performance modeling of uranium nitride*, Journal of Nuclear Materials 606 (2025) 155604.
- G. Pastore, L. Luzzi, V. Di Marcello, P. Van Uffelen, *Physics-based modelling of fission gas swelling and release in UO$_2$ applied to integral fuel rod analysis*, Nuclear Engineering and Design 256 (2013) 75–86.
- R.J. White, *The development of grain-face porosity in irradiated oxide fuel*, Journal of Nuclear Materials 325 (2004) 61–77.
- M.V. Speight, W. Beere, *Vacancy potential and void growth on grain boundaries*, Metal Science 9 (1975) 190–191.
