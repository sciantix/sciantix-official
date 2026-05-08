# Diagnostica del modello UN per fission gas swelling: sensitivity analysis e stato del lavoro

## Scopo del documento

Questo documento riassume lo stato della diagnostica numerica svolta sul modello 0D/1-grain per il comportamento del gas di fissione in combustibile **uranium mononitride (UN)**, ispirato al modello di **Rizk et al. (2025)** e alla formulazione SIFGRS/SCIANTIX/BISON.

L'obiettivo pratico è capire perché l'implementazione Python corrente, pur usando parametri nominalmente coerenti con Rizk, non riproduce ancora in modo soddisfacente le curve pubblicate di:

- swelling da bolle bulk;
- swelling da bolle su dislocazioni;
- pressioni interne/equilibrio;
- densità numeriche e raggi medi delle bolle;
- partizione del gas tra matrice, bulk bubbles, dislocation bubbles e gas arrivato al grain face.

Il documento è scritto per essere leggibile in **VS Code Markdown Preview**. Le formule sono scritte in sintassi KaTeX/LaTeX tra `$$ ... $$`.

---

## Modello fisico implementato

Nel modello Python sono considerate, per ora, solo le popolazioni **intragranulari**:

1. gas in soluzione nella matrice, con concentrazione media $c$;
2. gas in bolle bulk intragranulari, con concentrazione $m_b$;
3. gas in bolle su dislocazioni, con concentrazione $m_d$.

Il modello intergranulare/grain-boundary completo non è ancora implementato. Il termine `q_gb` nel codice rappresenta per ora solo il gas trasferito al grain face per bilancio di massa, non un modello completo di grain-boundary bubbles né di fission gas release.

La produzione di gas è:

$$
\beta = Y_{fg}\,\dot{F}
$$

Nel modello attuale si è scelto:

$$
Y_{fg}=0.24
$$

interpretato come resa Xe-equivalente. È stata discussa la possibilità di usare $Y_{fg}=0.475$, perché Rizk menziona gas nobili più volatili nella sezione sullo swelling solido. Tuttavia, per il modello corrente Xe-only, non è stato adottato come default. Il valore 0.475 resta solo una possibile sensitivity futura.

---

## Sistema gas risolto numericamente

La parte gas viene risolta con un solver spettrale a tre equazioni, analogo allo stile SCIANTIX, per le variabili:

$$
(c,\,m_b,\,m_d)
$$

con termini di:

- produzione $\beta$;
- diffusione verso il grain boundary;
- trapping verso bolle bulk, $g_b$;
- trapping verso bolle su dislocazioni, $g_d$;
- re-solution bulk, $b_b$;
- re-solution da bolle su dislocazioni, $b_d$.

In forma schematica:

$$
\frac{\partial c}{\partial t}
= \nabla \cdot \left(D_g \nabla c\right)
+\beta
-g_b c-g_d c
+b_b m_b+b_d m_d
$$

$$
\frac{\partial m_b}{\partial t}=g_b c-b_b m_b
$$

$$
\frac{\partial m_d}{\partial t}=g_d c-b_d m_d
$$

Sono stati confrontati due ordinamenti numerici:

1. aggiornamento di $N_b$ dopo il gas solver;
2. aggiornamento di $N_b$ prima del gas solver, più vicino all'ordine SCIANTIX classico.

L'ordine finale usato nelle diagnostiche più recenti è:

```text
coefficienti old-step
→ update N_b usando c_old, m_b_old
→ gas diffusion 3x3 con coefficienti old-step
→ update vacancy absorption
→ update V_b, V_d, R_b, R_d
→ coalescenza dislocation bubbles
→ bilancio q_gb
```

La diagnostica ha mostrato che passare da solver 3x3 a una versione Polypole1-like non cambia in modo dominante i risultati. Quindi il problema principale non sembra essere il solver gas.

---

## Diffusività Xe

La diffusività Xe in UN è stata implementata come:

$$
D_g = D_1 + D_2 + D_3
$$

con:

$$
D_1 = D_{10}\exp\left(-\frac{Q_1}{k_B T}\right)
$$

$$
D_3 = A_{30}\dot{F}
$$

Nel caso Python corrente, coerentemente con la lettura di Rizk/SCIANTIX-UN, il termine $D_2$ di Xe è trascurato:

$$
D_2^{Xe}=0
$$

Parametri usati:

$$
D_{10}=1.56\times 10^{-3}\ \mathrm{m^2/s}
$$

$$
Q_1=4.94\ \mathrm{eV}
$$

$$
A_{30}=1.85\times 10^{-39}\ \mathrm{m^5}
$$

La diagnostica della curva $D_g(T)$ ha mostrato andamento coerente con la figura di Rizk: regime quasi atermico dominato da $D_3$ alle temperature più basse e crescita termica a temperature alte.

---

## Diffusività delle vacanze di Uranio: correzione effettuata

La crescita delle bolle tramite assorbimento di vacanze usa la diffusività delle vacanze di Uranio:

$$
D_{V_U}=D_1^{V_U}+D_2^{V_U}
$$

Il termine termico è stato mantenuto come in Rizk:

$$
D_1^{V_U}
=1.35\times10^{-2}\exp\left(-\frac{5.66}{k_B T}\right)
$$

La formula tabulata per $D_2^{V_U}$, se usata letteralmente con i segni della tabella, produceva una diffusività non coerente con la curva "Vacancies, bulk" di Rizk Fig. 4. È stata quindi introdotta una versione diagnostica corretta/rifittata:

$$
D_2^{V_U}
=\sqrt{\dot{F}}\,A_{20}^{refit}
\exp\left(
\frac{B_{21}}{k_B T}
+
\frac{B_{22}}{(k_B T)^2}
\right)
$$

con:

$$
B_{21}=-0.62\ \mathrm{eV}
$$

$$
B_{22}=-0.04\ \mathrm{eV^2}
$$

$$
A_{20}^{refit}=4.6304523933553033\times10^{-29}
\ \mathrm{m^{7/2}s^{-1/2}}
$$

Questa modifica è stata introdotta per riprodurre la scala della curva di diffusività delle vacanze bulk riportata graficamente da Rizk. La correzione è importante per evitare una crescita meccanica delle bolle eccessivamente rapida, ma **non risolve da sola** il disaccordo principale con le curve di swelling.

---

## Pressione interna ed equilibrio meccanico

La pressione interna viene calcolata come:

$$
p_i = \frac{k_B T\,m_i}{n_i\Omega}
$$

con:

- $m_i$ = concentrazione di gas nella popolazione di bolle $i$;
- $n_i$ = concentrazione di vacanze contenute nella popolazione di bolle $i$;
- $\Omega=a^3/4$ = volume atomico della matrice UN.

La pressione di equilibrio capillare è:

$$
p_{i,eq}=\frac{2\gamma}{R_i}-\sigma_h
$$

Nel modello 0D corrente si usa:

$$
\sigma_h=0
$$

quindi:

$$
p_{i,eq}=\frac{2\gamma}{R_i}
$$

con:

$$
\gamma=1.11\ \mathrm{J/m^2}
$$

### Problema numerico individuato

Con condizioni iniziali fresh:

$$
N_b(0)=0,\quad R_b(0)=0,\quad n_b(0)=0
$$

appena entra gas nelle bolle bulk, la pressione interna tende formalmente a infinito perché il volume vacancy disponibile è nullo. L'update implicito delle vacanze evita la divergenza numerica, ma tende a portare rapidamente:

$$
p_b\simeq p_{b,eq}
$$

mentre in Rizk Fig. 5 la curva bulk pressure risulta, in parte dell'intervallo, sotto la curva bulk equilibrium pressure:

$$
p_b < p_{b,eq}
$$

### Test sul seed bulk

È stato testato un seed di volume/vacanze per le bolle bulk appena nucleate:

$$
R_{b,seed}=1\text{--}3\ \mathrm{nm}
$$

Risultato qualitativo:

- il seed può effettivamente portare $p_b/p_{b,eq}<1$ alle basse temperature;
- il seed non modifica in modo significativo la crescita delle dislocation bubbles;
- sopra circa 1200--1300 K il sistema tende comunque a $p_b\approx p_{b,eq}$.

Conclusione: il seed migliora la fisica iniziale della pressione bulk, ma non è la causa principale del disaccordo sulle curve di swelling.

---

## Trapping bulk e dislocation

Il trapping verso bolle bulk è:

$$
g_b=4\pi D_g R_b N_b
$$

Il trapping verso dislocazioni è:

$$
g_d
=
4\pi D_g R_d N_d
+
\frac{2\pi D_g}{
\ln\left(\frac{\Gamma_d}{Z_d r_d}\right)-3/5
}
\left(\rho_d-2R_dN_d\right)
$$

con:

$$
\Gamma_d=\frac{1}{\sqrt{\pi\rho_d}}
$$

La diagnostica ha mostrato che il gas prodotto viene catturato troppo fortemente dalle bolle bulk. In molti casi a 3.2% FIMA il modello Python dava, a bassa e media temperatura:

```text
bulk_gas ≈ 80--90 % del gas generato
disl_gas ≈ 1--4 % del gas generato
```

Questo è il punto centrale: le dislocation bubbles nel modello Python ricevono troppo poco gas, quindi crescono troppo tardi rispetto alle curve di Rizk.

---

## Nucleazione bulk e parametro $f_n$

La nucleazione bulk è:

$$
\nu_b
=8\pi f_n D_g\Omega_{fg}^{1/3}c^2
$$

Rizk riporta:

$$
f_n=10^{-6}
$$

ma indica che il valore è ereditato da modelli precedenti e non è un parametro direttamente misurato per UN. È quindi un candidato naturale per una sensitivity analysis.

### Risultato della sensitivity su $f_n$

Sono stati testati:

$$
f_n=10^{-6},\ 3\times10^{-7},\ 10^{-7},\ 3\times10^{-8},\ 10^{-8},\ 3\times10^{-9},\ 10^{-9}
$$

con solver 3x3, $Y_{fg}=0.24$, $r_{gr}=6\ \mu\mathrm{m}$ e diffusività vacancy corretta.

Risultato chiave a 3.2% FIMA:

- con $f_n=10^{-6}$, lo swelling da bolle su dislocazioni cresce troppo tardi;
- con $f_n\approx 10^{-8}$, la crescita delle dislocation bubbles si anticipa sensibilmente e si avvicina molto di più alle curve Rizk;
- il seed bulk non cambia significativamente le temperature soglia dello swelling dislocation.

Valori indicativi trovati dalla diagnostica:

| $f_n$ | $T$ a cui $sw_d\approx1\%$ | $T$ a cui $sw_d\approx2\%$ | $T$ a cui $sw_d\approx5\%$ |
|---:|---:|---:|---:|
| $10^{-6}$ | circa 1765 K | circa 1862 K | circa 2005 K |
| $10^{-8}$ | circa 1530 K | circa 1630 K | circa 1740 K |

Conclusione provvisoria:

$$
\boxed{\text{il parametro dominante sembra essere } f_n}
$$

Ridurre $f_n$ riduce $N_b$, quindi riduce $g_b$, lasciando più gas disponibile per le bolle su dislocazioni.

---

## Test su solver 3x3 vs Polypole1

È stata eseguita una diagnostica confrontando:

1. solver 3x3 transiente;
2. solver Polypole1-like con diffusività efficace.

Il risultato è che le due versioni danno curve molto simili per la partizione bulk/dislocation. Quindi il disaccordo principale con Rizk **non** sembra essere dovuto al solver gas.

Conclusione:

$$
\boxed{\text{si mantiene il solver 3x3}}
$$

---

## Test su $K_d$ e densità iniziale di bolle su dislocazioni

Nel modello:

$$
N_d(0)=K_d\rho_d
$$

con:

$$
K_d=5\times10^5\ \mathrm{bubbles/m}
$$

$$
\rho_d=3\times10^{13}\ \mathrm{m^{-2}}
$$

quindi:

$$
N_d(0)=1.5\times10^{19}\ \mathrm{m^{-3}}
$$

Un valore alternativo emerso nei documenti di lavoro è:

$$
N_d(0)\approx3.6\times10^{19}\ \mathrm{m^{-3}}
$$

che corrisponde a:

$$
K_d\approx1.2\times10^6\ \mathrm{bubbles/m}
$$

È opportuno includere $K_d$ o $N_d(0)$ come ulteriore parametro di sensitivity, perché aumentando $N_d$ aumenta il sink verso le dislocation bubbles e si anticipa la loro crescita.

---

## Interpretazione rispetto alle figure di Rizk

Le figure Rizk usate come riferimento sono:

- Fig. 4: diffusività gas/vacanze;
- Fig. 5: pressioni interne ed equilibrium pressures a 3.2% FIMA;
- Fig. 6: swelling vs burnup a 1600 K;
- Fig. 7: concentrazioni numeriche delle bolle a 1.3% FIMA;
- Fig. 8: raggi medi delle bolle a 1.3% FIMA;
- Fig. 9: amount of gas in matrix, bulk bubbles, dislocation bubbles, grain-boundary bubbles e release.

Rizk interpreta:

- la popolazione piccola $P_1$ come bulk bubbles;
- la popolazione grande intragranulare $P_2$ come dislocation bubbles;
- le misure TEM di grandi bolle intragranulari sono confrontabili soprattutto con le dislocation bubbles, non con le bulk bubbles.

Nel nostro codice mancano ancora:

- grain-boundary bubbles;
- interconnection/release;
- possibile evoluzione di $\rho_d$;
- eventuale contributo di volatili non-Xe;
- eventuale calibrazione accurata di $f_n$ e/o $K_d$.

---

## Conclusione provvisoria della diagnostica

I test svolti finora indicano:

1. La diffusività Xe è sostanzialmente coerente.
2. La diffusività vacancy tabulata richiedeva una correzione/rifit per riprodurre la scala di Rizk Fig. 4.
3. La singolarità iniziale della pressione bulk è reale, ma non è la causa principale del disaccordo sulle curve di swelling.
4. Il passaggio solver 3x3 vs Polypole1-like non cambia in modo dominante i risultati.
5. Il problema principale è la competizione tra $g_b$ e $g_d$: con $f_n=10^{-6}$ il modello crea troppe bolle bulk, quindi $N_b$ e $g_b$ diventano troppo grandi.
6. Ridurre $f_n$ verso $10^{-8}$ sposta il gas verso le dislocation bubbles e anticipa lo swelling da dislocazioni, avvicinando il comportamento alle curve Rizk.

La prossima fase consigliata è una **calibrazione mirata** su:

$$
f_n,\quad K_d,\quad R_{b,seed}
$$

mantenendo:

$$
Y_{fg}=0.24
$$

come caso Xe-equivalente di riferimento.

---

## Roadmap dei prossimi test

1. Eseguire uno sweep fine di $f_n$ in scala logaritmica:

$$
f_n\in[10^{-9},10^{-6}]
$$

2. Ripetere lo sweep per due valori di $N_d(0)$:

$$
N_d(0)=1.5\times10^{19}\ \mathrm{m^{-3}}
$$

$$
N_d(0)=3.6\times10^{19}\ \mathrm{m^{-3}}
$$

3. Costruire una funzione obiettivo qualitativa rispetto alle curve Rizk digitizzate/approssimate:

$$
J=
\sum_i
\left[sw_{d,model}(T_i)-sw_{d,Rizk}(T_i)\right]^2
$$

4. Identificare il range migliore di $f_n$.

5. Solo dopo, valutare se introdurre il modello grain-boundary/intergranular per confrontare pienamente Fig. 9.

---

## Note operative per continuare in una nuova chat

Se questo file viene usato in una nuova chat, partire da queste conclusioni:

- Non rifare da zero il confronto 3x3 vs Polypole1: è già stato verificato che non domina.
- Non assumere $Y_{fg}=0.475$ nel modello principale; usarlo solo come sensitivity, perché il modello corrente è Xe-equivalente.
- La priorità è lo sweep fine su $f_n$ e, subito dopo, su $N_d(0)$ o $K_d$.
- Il valore nominale $f_n=10^{-6}$ produce troppo bulk trapping.
- Il range interessante emerso è circa:

$$
10^{-8}\lesssim f_n\lesssim3\times10^{-8}
$$

ma deve essere rifinito confrontando contemporaneamente Fig. 6, Fig. 7, Fig. 8 e Fig. 9 di Rizk.

