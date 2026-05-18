# finalUNcalibration — UN M7 scoring targets

**Scopo:** definire in modo esplicito gli obiettivi di scoring per la prossima calibrazione Optuna/manuale del modello UN M7.

Questo file è scritto come specifica operativa v1. L'idea è separare:

1. **fit sperimentale vero**, da usare nello score;
2. **target qualitativi importanti**, soprattutto gas partition;
3. **guardie numeriche/fisiche**, da usare per evitare soluzioni non fisiche;
4. **diagnostiche**, da plottare ma non usare come target forte.

---

## 0. Regola generale

I target primari sono:

$$
\boxed{S_d(T,F)\ \text{e}\ R_d(T,F)}
$$

cioè swelling e raggio delle **dislocation bubbles**, interpretate come popolazione P2.

Il valore assoluto di $N_d$ **non** viene fittato punto per punto, perché sperimentalmente è incoerente con swelling e raggio:

$$
S_d = 100\,N_dV_d
=100\,N_d\frac{4}{3}\pi R_d^3
$$

Se imponiamo già $S_d$ e $R_d$, allora $N_d$ è quasi determinato:

$$
N_{d,\mathrm{implied}}
=
\frac{S_d/100}{\frac{4}{3}\pi R_d^3}
$$

Quindi $N_d$ entra solo come **trend qualitativo**: vogliamo un calo ad alta temperatura, non il fit del valore assoluto.

---

## 0.1 Range di temperatura usato nello scoring

Per evitare di ottimizzare regioni dove non abbiamo dati o dove il modello ridotto può perdere significato, lo scoring distingue tra **fit**, **guardie** e **diagnostica**.

### Fit sperimentale

Il fit sperimentale usa solo i punti disponibili:

- swelling P2 vs T: circa $900\text{--}1725\ \mathrm{K}$, secondo le tabelle in Sezione 4;
- raggio $R_d(T)$: circa $1045\text{--}1725\ \mathrm{K}$, secondo la tabella in Sezione 5;
- swelling vs burnup: solo $T=1600\ \mathrm{K}$.

### Gas partition e pressione

Le bande di gas partition e la penalità di pressione sono valutate su una griglia discreta fino a:

$$
T_{score,max}=2200\ \mathrm{K}
$$

Default consigliato:

$$
T_{score}=\{1200,1400,1600,1800,1900,2000,2200\}\ \mathrm{K}
$$

Per $1.1\%$ FIMA si può omettere $1900\ \mathrm{K}$ se non è presente nella griglia del run, ma se esiste è utile per la transizione.

### Diagnostica alta temperatura

Il modello può essere plottato fino a:

$$
T_{diag,max}=2500\ \mathrm{K}
$$

ma **non** va ottimizzato oltre $2200\ \mathrm{K}$, salvo scelta esplicita. Sopra $2200\ \mathrm{K}$ i risultati servono a vedere se il modello satura o esplode, non a costruire lo score principale.

Se compaiono NaN/Inf o guardie numeriche prima di $2200\ \mathrm{K}$, il trial è fortemente penalizzato. Se compaiono solo tra $2200$ e $2500\ \mathrm{K}$, il trial è segnalato come diagnosticamente sospetto, ma non necessariamente scartato dallo score base.

---

## 1. Definizioni modello

### 1.1 Volumi medi delle bolle

Per ogni popolazione $i=b,d$:

$$
V_i = \frac{4}{3}\pi R_i^3
$$

con:

- $b$ = bulk bubbles;
- $d$ = dislocation bubbles.

### 1.2 Swelling da bolle

$$
S_b = 100\,N_bV_b
$$

$$
S_d = 100\,N_dV_d
$$

Lo score sui dati P2 usa:

$$
\boxed{S_d}
$$

non lo swelling totale.

### 1.3 Gas partition

Nel modello ridotto usiamo quattro frazioni:

$$
f_m = 100\frac{c}{M_{tot}}
$$

$$
f_b = 100\frac{m_b}{M_{tot}}
$$

$$
f_d = 100\frac{m_d}{M_{tot}}
$$

$$
f_{qgb} = 100\frac{q_{gb}}{M_{tot}}
$$

con:

$$
M_{tot}=c+m_b+m_d+q_{gb}
$$

Nel confronto con Rizk 2025 Fig. 9:

$$
q_{gb}^{model}\approx \text{grain-boundary bubbles}+\text{FGR}
$$

perché il nostro modello ridotto non separa ancora grain-boundary bubbles e fission gas release.

---

## 2. Funzioni errore proposte

### 2.1 Log-error per grandezze positive

Per swelling e raggi:

$$
\mathrm{logerr}(x,x^*)=
\left[
\ln\left(\frac{\max(x,\epsilon)}{\max(x^*,\epsilon)}\right)
\right]^2
$$

con ad esempio:

$$
\epsilon=10^{-30}
$$

Per i raggi usare $R$ in nm o m indifferentemente, purché modello e target abbiano le stesse unità.

### 2.2 Band penalty per target a intervallo

Per gas partition e guardie:

$$
\mathrm{band}(x;x_{min},x_{max})=0
\quad\text{se}\quad
x_{min}\le x\le x_{max}
$$

Se $x<x_{min}$:

$$
\mathrm{band}(x;x_{min},x_{max})=
\left(\frac{x-x_{min}}{x_{max}-x_{min}}\right)^2
$$

Se $x>x_{max}$:

$$
\mathrm{band}(x;x_{min},x_{max})=
\left(\frac{x-x_{max}}{x_{max}-x_{min}}\right)^2
$$

Per percentuali si usa direttamente $x$ in percento.

---

## 3. Score totale proposto

Bozza:

$$
J_{tot}=
 w_S J_{S_d(T)}
+w_R J_{R_d(T)}
+w_P J_{partition}
+w_F J_{S_d(F,1600K)}
+w_N J_{N_d,drop}
+w_p J_{pressure}
+w_G J_{guards}
$$

Pesi iniziali proposti:

| Componente | Peso | Nota |
|---|---:|---|
| $J_{S_d(T)}$ | 1.0 | target primario: swelling P2/dislocation |
| $J_{R_d(T)}$ | 1.0 | target primario: raggio P2/dislocation |
| $J_{partition}$ | 1.0 | target importante: gas partition Rizk-like |
| $J_{S_d(F,1600K)}$ | 0.3 | target secondario: crescita con burnup |
| $J_{N_d,drop}$ | 0.2 | solo calo high-T, non valore assoluto |
| $J_{pressure}$ | 1.0 | penalità solo per sovrapressione $p>p_{eq}$ |
| $J_{guards}$ | 2.0 | penalità forte / quasi invalidazione |

Questi pesi sono la base operativa. In particolare, la pressione ha peso:

$$
w_{pressure}=1.0
$$

ma la penalità di pressione è **asimmetrica**: penalizza solo $p>p_{eq}$, non penalizza $p<p_{eq}$.

---

# 4. Target sperimentale: swelling P2 vs T

Grandezza modello:

$$
S_d(T,F)=100N_dV_d
$$

Errore:

$$
J_{S_d(T)}=
\frac{1}{N_S}\sum_k
\mathrm{logerr}\left(S_d^{model}(T_k,F_k),S_{P2,k}^{exp}\right)
$$

---

## 4.1 Experimental data fit — swelling vs T at 1.1 % FIMA, 100 kW/m

| $T$ [K] | $S_{P2}^{exp}$ [%] |
|---:|---:|
| 1127 | 0.68 |
| 1228 | 0.59 |
| 1312 | 0.43 |
| 1402 | 0.58 |
| 1485 | 1.22 |
| 1549 | 1.84 |
| 1598 | 1.66 |
| 1632 | 2.13 |
| 1669 | 3.60 |
| 1685 | 2.72 |

---

## 4.2 Experimental data fit — swelling vs T at 1.1 % FIMA, 119 kW/m

| $T$ [K] | $S_{P2}^{exp}$ [%] |
|---:|---:|
| 899 | 0.63 |
| 1154 | 1.17 |
| 1228 | 1.08 |
| 1325 | 1.28 |
| 1435 | 1.32 |
| 1514 | 2.10 |
| 1570 | 2.72 |
| 1608 | 2.91 |
| 1635 | 3.28 |
| 1656 | 3.75 |

---

## 4.3 Experimental data fit — swelling vs T at 1.3 % FIMA

| $T$ [K] | $S_{P2}^{exp}$ [%] |
|---:|---:|
| 1044 | 1.11 |
| 1220 | 1.22 |
| 1377 | 1.33 |
| 1534 | 2.83 |
| 1595 | 3.53 |
| 1639 | 3.86 |
| 1661 | 2.45 |
| 1709 | 2.93 |
| 1724 | 3.15 |

---

## 4.4 Experimental data fit — swelling vs T at 3.2 % FIMA

| $T$ [K] | $S_{P2}^{exp}$ [%] |
|---:|---:|
| 984 | 0.72 |
| 1056 | 1.06 |
| 1126 | 1.26 |
| 1247 | 1.58 |
| 1343 | 1.79 |
| 1420 | 2.08 |
| 1459 | 2.40 |
| 1511 | 2.83 |
| 1557 | 3.31 |
| 1590 | 3.75 |

---

# 5. Target sperimentale: dislocation-bubble radius $R_d(T)$

Grandezza modello:

$$
R_d(T,F=1.3\%\ \mathrm{FIMA})
$$

Errore:

$$
J_{R_d(T)}=
\frac{1}{N_R}\sum_k
\mathrm{logerr}\left(R_d^{model}(T_k),R_{d,k}^{exp}\right)
$$

Usare $R_d$ in nm nella tabella.

## 5.1 Experimental data fit — $R_d(T)$ at 1.3 % FIMA

| $T$ [K] | $R_d^{exp}$ [nm] |
|---:|---:|
| 1045 | 54.06 |
| 1219 | 60.37 |
| 1374 | 69.58 |
| 1535 | 104.85 |
| 1594 | 120.83 |
| 1641 | 143.73 |
| 1663 | 122.76 |
| 1710 | 157.99 |
| 1725 | 173.67 |

## 5.2 Anti-runaway radius penalty

Non vogliamo imporre un valore sperimentale ad alta T dove non lo conosciamo bene, però non vogliamo un raggio che esplode.

Proposta:

| Condizione | Penalità |
|---|---|
| $R_d > 500$ nm per $T\le 1800$ K | soft penalty |
| $R_d > 1000$ nm per $T\le 2000$ K | strong penalty |
| $R_d > 2000$ nm | quasi invalid trial |
| NaN / Inf in $R_d$ | invalid trial |

Formula possibile:

$$
J_{R,guard}=
\left[\max\left(0,\ln\frac{R_d}{R_{soft}}\right)\right]^2
$$

con soglie diverse a seconda del range di temperatura.

---

# 6. Target secondario: swelling vs burnup at 1600 K

Grandezza modello:

$$
S_d(F,T=1600K)
$$

Errore:

$$
J_{S_d(F,1600K)}=
\frac{1}{N_F}\sum_k
\mathrm{logerr}\left(S_d^{model}(F_k,1600K),S_{P2,k}^{exp}\right)
$$

## 6.1 Experimental data fit — $S_d(F)$ at 1600 K

| FIMA [%] | $S_{P2}^{exp}$ [%] | Nota |
|---:|---:|---|
| 1.12 | 1.64 | approx 100 kW/m |
| 1.11 | 2.90 | approx 119 kW/m |
| 1.31 | 3.51 | measurement |
| 3.18 | 3.72 | measurement |

Peso proposto più basso rispetto a swelling-vs-T e radius:

$$
w_F=0.3
$$

---

# 7. Target importante: gas partition, Rizk 2025 Fig. 9 style

Questo target è importante, ma per ora non va trattato come digitalizzazione punto-per-punto perfetta.

Usiamo bande numeriche manuali coerenti con Rizk 2025 Fig. 9.

Frazioni modello:

- $f_m$ = matrix / gas in solution [%];
- $f_b$ = bulk bubbles [%];
- $f_d$ = dislocation bubbles [%];
- $f_{qgb}$ = gas to grain face [%].

Nel nostro modello:

$$
f_{qgb}\approx f_{gb\ bubbles}+f_{FGR}
$$

Score:

$$
J_{partition}=
\frac{1}{N_{part}}
\sum_{k,j}
\mathrm{band}\left(f_j(T_k,F_k);f_{j,min},f_{j,max}\right)
$$

con $j\in\{b,d,qgb\}$.

La frazione matrix $f_m$ si può tenere come diagnostica o usare con peso debole.

---

## 7.1 Gas partition target bands — 1.1 % FIMA

Questi target rappresentano una banda numerica manuale coerente con lo stile della gas partition di Rizk. La frazione in matrice non deve essere zero a bassa temperatura: a bassa T deve restare circa 5%.

| $T$ [K] | $f_b$ bulk [%] | $f_d$ dislocation [%] | $f_{qgb}$ [%] | $f_m$ matrix [%] |
|---:|---:|---:|---:|---:|
| 1200 | 65 – 85 | 5 – 25 | 0 – 5 | 4 – 6 |
| 1400 | 65 – 85 | 5 – 25 | 0 – 5 | 4 – 6 |
| 1600 | 55 – 80 | 10 – 35 | 2 – 8 | 2 – 5 |
| 1800 | 30 – 55 | 35 – 60 | 5 – 10 | 0.5 – 3 |
| 1900 | 10 – 35 | 55 – 80 | 8 – 12 | 0 – 2 |
| 2000 | 0 – 15 | 75 – 88 | 10 – 15 | 0 – 1 |
| 2200 | 0 – 5 | 78 – 90 | 10 – 18 | 0 – 0.5 |

Interpretazione desiderata:

- a bassa temperatura la matrice deve contenere circa $5\%$ del gas generato;
- bulk dominante fino a circa $1500\text{--}1600\ \mathrm{K}$;
- transizione bulk/dislocation attorno a $1750\text{--}1900\ \mathrm{K}$;
- dislocation dominante sopra circa $1900\text{--}2000\ \mathrm{K}$;
- $q_{gb}$ non deve essere zero sopra $1800\ \mathrm{K}$;
- a $2000\ \mathrm{K}$ il target per $q_{gb}$ è circa $10\text{--}15\%$.

---

## 7.2 Gas partition target bands — 3.2 % FIMA

Anche a $3.2\%$ FIMA la frazione in matrice non deve essere zero a bassa temperatura: deve restare circa $5\%$ fino a circa $1400\ \mathrm{K}$, poi andare progressivamente verso zero.

| $T$ [K] | $f_b$ bulk [%] | $f_d$ dislocation [%] | $f_{qgb}$ [%] | $f_m$ matrix [%] |
|---:|---:|---:|---:|---:|
| 1200 | 80 – 90 | 3 – 12 | 0 – 5 | 4 – 6 |
| 1400 | 80 – 90 | 3 – 12 | 0 – 5 | 4 – 6 |
| 1600 | 75 – 88 | 5 – 20 | 2 – 8 | 2 – 5 |
| 1800 | 60 – 80 | 15 – 30 | 5 – 10 | 0.5 – 3 |
| 1900 | 35 – 55 | 35 – 55 | 8 – 12 | 0 – 2 |
| 2000 | 10 – 25 | 60 – 78 | 10 – 15 | 0 – 1 |
| 2200 | 0 – 8 | 75 – 90 | 10 – 18 | 0 – 0.5 |

Interpretazione desiderata:

- a $3.2\%$ FIMA la transizione bulk $\rightarrow$ dislocation può essere più tardiva rispetto al caso $1.1\%$ FIMA;
- bulk resta dominante fino a circa $1700\text{--}1800\ \mathrm{K}$;
- dislocation domina sopra circa $1900\text{--}2000\ \mathrm{K}$;
- $q_{gb}$ deve essere moderato ma non nullo sopra $1800\ \mathrm{K}$;
- a $2000\ \mathrm{K}$ il target per $q_{gb}$ è circa $10\text{--}15\%$.

---

## 7.3 Gas partition target — 1.3 % FIMA

Rizk Fig. 9 mostra esplicitamente 1.1 % e 3.2 % FIMA. Per 1.3 % FIMA si può:

1. non usare gas partition nello score;
2. oppure interpolare qualitativamente vicino al caso 1.1 % FIMA;
3. oppure usare solo guardie deboli.

Proposta iniziale: **non usare 1.3 % FIMA per gas partition score**, a meno che non decidiamo una tabella manuale dedicata.

---

# 8. $N_d(T)$: non fit del valore assoluto, solo calo ad alta T

I dati sperimentali $N_d$ sono diagnostici, non target primario.

Non usare:

$$
N_d^{model}(T_k)=N_d^{exp}(T_k)
$$

come obiettivo forte.

Usare invece un vincolo di trend:

$$
N_d(T_{high}) < N_d(T_{mid})
$$

Proposta numerica:

| Burnup [% FIMA] | $T_{mid}$ [K] | $T_{high,1}$ [K] | Target | $T_{high,2}$ [K] | Target |
|---:|---:|---:|---:|---:|---:|
| 1.1 | 1400 | 1800 | $N_d(1800)/N_d(1400)<0.8$ | 2000 | $N_d(2000)/N_d(1400)<0.5$ |
| 1.3 | 1400 | 1800 | $N_d(1800)/N_d(1400)<0.8$ | 2000 | $N_d(2000)/N_d(1400)<0.5$ |
| 3.2 | 1400 | 1800 | $N_d(1800)/N_d(1400)<0.8$ | 2000 | $N_d(2000)/N_d(1400)<0.5$ |

Score possibile:

$$
J_{N_d,drop}=
\left[\max\left(0,\ln\frac{N_d(T_{high})}{r_{max}N_d(T_{mid})}\right)\right]^2
$$

con $r_{max}=0.8$ o $0.5$.

Nota: non vogliamo collasso numerico di $N_d$ a zero; vogliamo un calo fisico.

Guardia aggiuntiva:

$$
N_d < 10^{16}\ \mathrm{m^{-3}}
\Rightarrow \text{warning / likely numerical collapse}
$$

Il valore assoluto di $N_d$ non entra nel fit sperimentale, ma non vogliamo che il modello faccia collassare la concentrazione sotto circa:

$$
N_{d,min}=10^{16}\ \mathrm{m^{-3}}
$$

nel range di scoring. Sotto questa soglia il calo non è più interpretato come trend fisico, ma come possibile collasso numerico o coalescenza eccessiva.

---

# 9. Pressure target / physical regularization

La pressione non è un dato sperimentale diretto. Serve a evitare soluzioni non fisiche.

La penalità di pressione è **solo per sovrapressione**:

$$
p_i>p_{i,eq}
$$

Non si applica penalità se:

$$
p_i\le p_{i,eq}
$$

Questo vale perché una bolla sottopressurizzata rispetto all'equilibrio non è il problema principale che vogliamo eliminare nello scoring. Il problema da evitare è la sovrapressione crescente/non fisica.

## 9.1 Grandezze

Per $i=b,d$:

$$
\Pi_i=\frac{p_i}{p_{i,eq}}
$$

con:

$$
p_{i,eq}=\frac{2\gamma}{R_i}-\sigma_h
$$

Nel modello attuale:

$$
\sigma_h=0
$$

quindi:

$$
p_{i,eq}=\frac{2\gamma}{R_i}
$$

## 9.2 Score di pressione

Per ogni punto di temperatura e burnup nello scoring:

$$
P_i(T,F)=\left[\max\left(0,\log_{10}\Pi_i(T,F)\right)\right]^2
$$

Quindi:

- se $\Pi_i\le 1$, allora $P_i=0$;
- se $\Pi_i=3$, allora $P_i=(\log_{10}3)^2$;
- se $\Pi_i=10$, allora $P_i=1$;
- se $\Pi_i=100$, allora $P_i=4$.

Lo score su una popolazione è la media sui punti valutati:

$$
J_{p,i}=\frac{1}{N_{p,i}}\sum_{F,T}P_i(T,F)
$$

Default operativo:

$$
F\in\{1.1,1.3,3.2\}\%\ \mathrm{FIMA}
$$

$$
T\in\{1200,1400,1600,1800,1900,2000,2200\}\ \mathrm{K}
$$

compatibilmente con i punti effettivamente calcolati nel notebook/codice.

## 9.3 Bulk vs dislocation pressure

La pressione sulle dislocation bubbles è più importante:

$$
J_{pressure}=0.7J_{p,d}+0.3J_{p,b}
$$

Se la popolazione bulk non esiste in un punto, quel punto bulk va escluso dalla media, non forzato con $R_{min}$.

Peso nello score totale:

$$
w_{pressure}=1.0
$$

## 9.4 Interpretazione pratica

La pressione funziona come hai detto: **più punti stanno sopra $p_{eq}$ e più sono sopra, peggiore è lo score**.

Non conta solo il numero di punti sopra $p_{eq}$, ma anche quanto sono sopra, tramite $\log_{10}(p/p_{eq})$.

---

# 10. Geometry guards: $\xi$, $\psi$, $\lambda$, $\zeta$

## 10.1 Definizioni

Porosità/volume fraction della popolazione dislocation:

$$
\xi_d=N_dV_d
$$

Wigner-Seitz radius:

$$
\delta_d=\left(\frac{3}{4\pi N_d}\right)^{1/3}
$$

Parametro geometrico:

$$
\psi_d=\frac{R_d}{\delta_d}
$$

Relazione:

$$
\xi_d=\psi_d^3
$$

Fattore di coalescenza:

$$
\lambda_d=\frac{2-\xi_d}{2(1-\xi_d)^3}
$$

Fattore geometrico per vacancy absorption:

$$
\zeta_d=
\frac{10\psi_d(1+
\psi_d^3)}
{-\psi_d^6+5\psi_d^2-9\psi_d+5}
$$

## 10.2 Guardie

| Grandezza | Soglia | Interpretazione |
|---|---:|---|
| $\psi_d < 0.8$ | ok | single-size ancora ragionevole |
| $0.8 \le \psi_d < 0.9$ | warning | geometria densa |
| $0.9 \le \psi_d < 1$ | strong penalty | quasi fuori validità |
| $\psi_d \ge 1$ | invalid / quasi invalid | $\xi_d\ge1$, modello non fisico |
| $\xi_d > 0.5$ | warning | swelling/porosità molto alta |
| $\xi_d > 0.8$ | strong penalty | vicino al limite geometrico |
| $\xi_d \ge 1$ | invalid / quasi invalid | volume bolle maggiore del volume disponibile |
| $\lambda_d > 10^2$ | warning | coalescenza molto forte |
| $\lambda_d > 10^4$ | strong penalty | near-singular coalescence |

Score guardia:

$$
J_{geom}=P(\psi_d)+P(\xi_d)+P(\lambda_d)
$$

con penalità crescenti sopra le soglie.

---

# 11. Capture bulk $\rightarrow$ dislocation guard

La legge di capture usata nel modello è:

$$
f_{cap}=\min\left(1,\max\left(0,s_{cap}N_d\Delta V_{cap}\right)\right)
$$

con:

$$
\Delta V_{cap}
\approx
\frac{4}{3}\pi(R_d+R_b)^3\Big|_{new}
-
\frac{4}{3}\pi(R_d+R_b)^3\Big|_{old}
$$

oppure nella forma differenziale equivalente:

$$
\Delta V_{cap}\approx 4\pi(R_d+R_b)^2\Delta R_d
$$

Target numerico/fisico:

| Grandezza | Soglia | Interpretazione |
|---|---:|---|
| $\max f_{cap,step}<0.1$ | ok | capture debole per step |
| $0.1\le\max f_{cap,step}<0.5$ | warning | capture significativa |
| $0.5\le\max f_{cap,step}<1$ | strong penalty | quasi saturazione |
| $\max f_{cap,step}=1$ | near invalid | in almeno uno step cattura 100% bulk bubbles |

Score:

$$
J_{capture}=\left[\max\left(0,\frac{\max f_{cap,step}-0.1}{0.9}\right)\right]^2
$$

più penalty addizionale se:

$$
\max f_{cap,step}=1
$$

La somma cumulativa:

$$
\sum f_{cap,step}
$$

è solo diagnostica; non rappresenta una vera frazione fisica cumulativa.

Se $\max f_{cap,step}=1$, il primo controllo numerico da fare è un rerun con timestep più piccolo, per esempio:

$$
\Delta t = 1\ \mathrm{h}
$$

Se il clipping sparisce o si sposta molto, il termine è time-step sensitive. Se resta anche con timestep piccolo, allora non è solo numerica: la capture/crescita delle bolle è fisicamente troppo aggressiva nella formulazione scelta.

Nel run Optuna standard, comunque, $\max f_{cap,step}=1$ va penalizzato fortemente, perché indica che il trial sta usando una dinamica di capture non controllata.

---

# 12. Vacancy absorption diagnostics

Non è ancora target di scoring principale, ma va sempre plottata.

Equazione:

$$
\frac{dn_{v,i}}{dt}=
\frac{2\pi D_v\delta_iN_i}{k_BT\zeta_i}
\left(p_i-p_{i,eq}\right)
$$

per $i=b,d$.

Grandezze da salvare/plottare:

| Grandezza | Uso |
|---|---|
| $D_v(T)$ | capire se la diffusività vacancy è troppo alta/bassa |
| $dn_{v,b}/dt$ | crescita volume bulk da vacanze |
| $dn_{v,d}/dt$ | crescita volume dislocation da vacanze |
| $n_{v,b}$ | inventario vacanze bulk |
| $n_{v,d}$ | inventario vacanze dislocation |
| $\Omega n_v/(\Omega_{fg}m)$ | volume vacancy vs volume gas |
| $p-p_{eq}$ | driving force per vacancy absorption |

Guardia qualitativa:

- se $dn_v/dt$ produce salti enormi in un singolo timestep, il run è probabilmente time-step sensitive;
- se il volume diventa dominato da vacancy absorption e $R_d$ esplode, bisogna penalizzare tramite $R_d$, $\psi$, $\xi$, pressure e capture.

---

# 13. Guardie numeriche da registrare nel CSV

Ogni trial dovrebbe salvare almeno:

| Nome diagnostica | Significato |
|---|---|
| `max_f_cap_step` | massimo clipping capture per step |
| `capture_fraction_sum` | somma diagnostica dei $f_{cap}$ clippati |
| `capture_raw_sum` | somma diagnostica del raw hazard |
| `psi_b`, `psi_d` | Wigner-Seitz occupancy radius ratio |
| `xi_b`, `xi_d` | volume fraction / porosity |
| `lambda_d` | fattore coalescenza |
| `zeta_b`, `zeta_d` | fattore vacancy absorption |
| `valid_single_size` | flag complessivo geometrico |
| `zeta_guard_count` | quante volte è intervenuto il guard su $\zeta$ |
| `psi_ge_1_count` | quante volte $\psi\ge1$ |
| `xi_clip_count` | quante volte $\xi$ è stato clippato |
| `N_floor_count` | quante volte $N$ è stato portato a floor |
| `pressure_Rmin_count` | quante volte si usa $R_{min}$ in $p_{eq}$ |
| `negative_clamp_count` | clamp a zero di quantità negative |
| `nan_or_inf_flag` | NaN/Inf nel run |

Se queste diagnostiche intervengono, il punto va interpretato come numericamente/fisicamente sospetto.

---

# 14. Cose da NON mettere nello score forte

Non usare come target forte:

1. $N_d$ assoluto punto per punto;
2. swelling ad alta T dove non abbiamo dati affidabili;
3. valore esatto di $R_d$ sopra il range sperimentale, salvo anti-runaway;
4. gas partition digitalizzata troppo finemente, finché non la digitalizziamo davvero;
5. bulk swelling come target primario;
6. matching perfetto $p/p_{eq}=1$;
7. risultati oltre il range dove il modello usa guard numerici.

---

# 15. Riassunto operativo

Lo score deve premiare:

1. buon fit di $S_d(T)$ sui dati P2;
2. buon fit di $R_d(T)$ sui dati disponibili;
3. gas partition coerente con Rizk 2025 Fig. 9, usando bande numeriche;
4. crescita con burnup a 1600 K ragionevole;
5. calo qualitativo di $N_d$ ad alta T;
6. pressione non sovrapressurizzata rispetto a $p_{eq}$;
7. nessun runaway geometrico;
8. nessun clipping capture violento;
9. nessun intervento silenzioso di guardie numeriche.

Lo score non deve cercare di forzare simultaneamente:

$$
S_d^{exp},\quad R_d^{exp},\quad N_d^{exp}
$$

perché i tre insiemi non sono perfettamente consistenti tra loro.
