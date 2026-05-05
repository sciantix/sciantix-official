# UN M7 — note fisiche per variante `capture_only` v6

Data: 2026-05-02

## 1. Contesto

Dopo i run v5:

- `capture_only` PC1 ha trovato un candidato molto promettente, `B_standard_wide_trial_00629`;
- `baseline/no-capture` PC2 è risultato competitivo, ma leggermente meno convincente;
- in entrambi i casi emerge una richiesta forte di re-solution effettiva molto più bassa del nominale;
- v5 ha risolto il problema precedente del raggio micrometrico a 2000 K, ma lo ha fatto usando scale factor aggregati abbastanza grossolani.

Il punto non è più solo cercare un nuovo minimo Optuna, ma capire **quale pezzo fisico del modello sta venendo compensato dagli scale factor**.

---

## 2. Candidato B `capture_only` v5

Candidato:

```text
B_standard_wide_trial_00629
family = capture_only
```

Parametri principali:

```text
f_n                    = 2.0147e-06
K_d                    = 6.5339e+05
rho_d                  = 1.6459e+13 m^-2
fission_rate           = 6.9386e+19 fiss/(m^3 s)

Dv_scale               = 1.4996
Dg_scale               = 0.5578
D2_xe_scale            = 0.2181
b_scale                = 0.05268
gb_scale               = 1.6647
gd_scale               = 3.7672
coalescence_d_scale    = 0.5066
capture_scale          = 2.3875
```

Diagnostiche finali:

```text
score_final            = 1.9131
R_d(1600 K)            ≈ 85.7 nm
R_d(1800 K)            ≈ 162.3 nm
R_d(1900 K)            ≈ 209.1 nm
R_d(2000 K)            ≈ 266.2 nm
p_d/p_d_eq(1600 K)     ≈ 1.121
p_b/p_b_eq(1600 K)     ≈ 1.000
bulk gas 1600 K        ≈ 65.7 %
q_gb 1600 K            ≈ 4.4 %
```

Interpretazione:

```text
b_scale basso
+ coalescence_d_scale basso/moderato
+ Dv_scale alto
+ gd_scale alto
+ capture_scale alto
```

Il modello tiene gas nelle dislocation bubbles, ma evita il collasso di `N_d`, quindi il raggio medio non esplode.

---

## 3. Decisione: rimuovere `D2_xe_scale`

Rizk indica che per Xe il termine `D2` è trascurabile rispetto a `D1 + D3`, mentre è importante per le vacancy.

Quindi, per la prossima variante:

```text
D2_xe_scale va rimosso dall'ottimizzazione.
```

Non deve più rubare tempo a Optuna.

Per Xe, usare:

\[
D_g = s_{D1,g} D_{1,g} + s_{D3,g} D_{3,g}
\]

oppure, in una variante più conservativa:

\[
D_g = s_{D_g}(D_{1,g}+D_{3,g})
\]

ma senza parametro libero su `D2_xe`.

Preferenza v6:

\[
D_g = s_{D1,g} D_{1,g} + s_{D3,g} D_{3,g}
\]

perché `D1` e `D3` agiscono in zone fisiche diverse:

- `D1`: diffusione termica;
- `D3`: mixing/ballistic cascade, proporzionale a fission rate.

---

## 4. Diffusività vacancy separata

Attualmente:

\[
D_v = s_{D_v}(D_{1,v}+D_{2,v})
\]

Però `D1_v` e `D2_v` hanno significato diverso:

- `D1_v`: parte termica;
- `D2_v`: irradiation-enhanced vacancy diffusion.

Variante v6 raccomandata:

\[
D_v = s_{D1,v}D_{1,v}+s_{D2,v}D_{2,v}
\]

Motivo:

- la pressione delle bolle dipende molto dall'assorbimento di vacancy;
- separare i due contributi permette di capire se il problema è termico o irradiation-driven;
- un unico `Dv_scale` può nascondere compensazioni.

---

## 5. Phi-resolution: non inserirla ora

Per bolle nel range `R_d ≈ 50–300 nm`, una stima grossolana near-equilibrium dà:

\[
m \sim 10^6 - 10^7 \quad \text{atomi gas per bolla}
\]

Quindi:

\[
\phi \approx \frac{1}{m-1} \sim 10^{-6}-10^{-8}
\]

Questo è molto più piccolo del `b_scale ≈ 0.05` trovato da v5.

Conclusione:

```text
La phi-resolution pura è troppo aggressiva se applicata direttamente al gas re-solution
del single-size model. Non inserirla nella prossima variante capture_only v6.
```

Resta una variante futura, magari con:

\[
b_{\mathrm{eff}} = b\,\phi^\alpha
\]

oppure:

\[
b_{\mathrm{eff}} = b\,\max(\phi,\phi_{min})
\]

ma non adesso.

---

## 6. Re-solution separata bulk/dislocation

Attualmente:

\[
b_b = s_b \dot F b_0(R_b)
\]

\[
b_d = s_b \dot F b_0(R_d)
\]

con un unico:

\[
s_b = \texttt{b_scale}
\]

Questo è troppo grossolano.

Variante v6:

\[
b_b = s_{b,b}\dot F b_0(R_b)
\]

\[
b_d = s_{b,d}\dot F b_0(R_d)
\]

dove:

```text
b_bulk_scale        = s_{b,b}
b_dislocation_scale = s_{b,d}
```

Scopo:

- capire se la re-solution va ridotta per entrambe le popolazioni;
- oppure se il problema è soprattutto nelle dislocation bubbles;
- evitare che un solo `b_scale` compensi simultaneamente bulk, dislocation, partition e pressione.

---

## 7. Trapping dislocation: separare bubble sink e line sink

Forma attuale:

\[
g_d =
s_{g_d}
\left[
4\pi D_g R_d N_d
+
\frac{2\pi D_g}{\ln(\Gamma_d/(Z_dr_d))-3/5}
(\rho_d-2R_dN_d)_+
\right]
\]

Problemi:

1. `gd_scale` scala insieme due contributi fisicamente diversi.
2. Il termine `(\rho_d - 2 R_d N_d)` assume che le bolle su dislocazione rimuovano porzioni di line sink.
3. Barani usa una forma più vicina a `rho_d` pieno, senza sottrarre `2 R_d N_d`.

Variante v6:

\[
g_d =
s_{d,bub}\,g_{d,bub}
+
s_{d,line}\,g_{d,line}
\]

con:

\[
g_{d,bub}=4\pi D_g R_dN_d
\]

\[
g_{d,line}=
\frac{2\pi D_g}{\ln(\Gamma_d/(Z_dr_d))-3/5}
(\rho_d-\alpha\,2R_dN_d)_+
\]

Parametro geometrico:

\[
\alpha \in [0,1]
\]

Interpretazione:

```text
alpha = 1  -> modello attuale
alpha = 0  -> line sink pieno tipo Barani/rho_d
```

Questa è una delle modifiche più importanti, perché v5 spesso chiede `gd_scale > 1`. Forse non serve aumentare tutto `g_d`; forse serve solo correggere il line sink.

---

## 8. Coalescenza dislocation-dislocation

La coalescenza dislocation-dislocation resta:

\[
\left(\frac{dN_d}{dt}\right)_{coal}
=
-4\lambda N_d^2\frac{dV_d}{dt}
\]

con:

\[
\lambda=\frac{2-\xi}{2(1-\xi)^3}
\]

\[
\xi=N_dV_d
\]

Nel codice:

\[
\left(\frac{dN_d}{dt}\right)_{coal}
=
-s_{coal}
4\lambda N_d^2\frac{dV_d}{dt}
\]

dove:

```text
coalescence_d_scale = s_coal
```

Questo termine riduce `N_d` ma non rimuove gas dalle dislocation bubbles. Quindi, a parità di swelling, aumenta il volume medio e quindi il raggio medio.

Nota:

```text
Il candidato B usa coalescence_d_scale ≈ 0.51.
```

Quindi B probabilmente sottostima un po' la coalescenza per evitare raggio troppo alto. Una sensitivity mirata deve verificare se è possibile aumentare `coalescence_d_scale` senza distruggere `R_d`.

---

## 9. Capture bulk→dislocation: correggere geometria Barani

Formula Barani:

\[
V_d^*=\frac{4}{3}\pi(R_d+R_b)^3
\]

ma il differenziale usato è:

\[
dV_d^*=4\pi(R_d+R_b)^2dR_d
\]

Interpretazione fisica:

- la bolla su dislocazione, espandendosi, spazza nuovo volume;
- le bulk bubbles dentro quel volume vengono catturate;
- il raggio `R_b` conta perché definisce la distanza di contatto;
- però il differenziale deve essere legato a `dR_d`, non a `dR_b`.

### Problema nella formula precedente

La formula precedente usava, in pratica:

\[
\Delta V_{cap}
=
\max
\left[
\frac{4}{3}\pi(R_d^{new}+R_b^{new})^3
-
\frac{4}{3}\pi(R_d^{old}+R_b^{old})^3,
0
\right]
\]

Questa include anche l'effetto di \(\Delta R_b\). Ma quando una bulk bubble cresce, solo una piccola porzione della sua superficie cresce verso la bolla dislocation; la maggior parte cresce in direzioni non rilevanti per il volume spazzato dalla dislocation bubble.

Quindi la formula è geometricamente troppo generosa.

### Variante v6 Barani-like

Usare:

\[
\Delta R_d^+ = \max(R_d^{new}-R_d^{old},0)
\]

\[
\Delta V_{cap}^{Barani}
=
4\pi(R_d^{old}+R_b^{old})^2\Delta R_d^+
\]

oppure variante mid-step:

\[
\Delta V_{cap}^{Barani}
=
4\pi(\bar R_d+\bar R_b)^2\Delta R_d^+
\]

Prima scelta v6:

\[
\boxed{
\Delta V_{cap}^{Barani}
=
4\pi(R_d^{old}+R_b^{old})^2\max(R_d^{new}-R_d^{old},0)
}
\]

Poi:

\[
f_{cap}
=
\min(1,s_{cap}N_d\Delta V_{cap}^{Barani})
\]

senza esponenziale.

---

## 10. Perché resta il `max(0,...)`

Il `max(0,...)` non deve essere visto come trucco numerico, ma come irreversibilità fisica:

```text
se la bolla dislocation si contrae, non restituisce le bulk bubbles già catturate.
```

Quindi:

\[
\Delta R_d^+ = \max(\Delta R_d,0)
\]

è fisico.

Da evitare:

\[
f_{cap}=1-\exp(-\mu)
\]

Non usarlo in questa fase: vogliamo restare fedeli alla forma lineare Barani/Rizk-like, non introdurre una nuova legge.

---

## 11. Nucleazione: variante futura con massa iniziale efficace

Il termine di nucleazione bulk è:

\[
\nu_b=8\pi f_nD_g\Omega_{fg}^{1/3}c^2
\]

Nel modello con nucleation mass coupling si usa:

\[
S_c=\beta-2\nu_b
\]

\[
S_{m_b}=2\nu_b
\]

Ma nel single-size model, `2` come dimero può essere troppo rigido. Una variante futura più generale è:

\[
S_c=\beta-\eta_{nuc}\nu_b
\]

\[
S_{m_b}=\eta_{nuc}\nu_b
\]

dove:

```text
eta_nuc = massa iniziale efficace per evento di nucleazione
```

Questa variante non entra nella prossima `capture_only v6`, perché `capture_only` attuale non usa mass coupling. Va testata dopo, su `M7_no_phi` o variante dedicata.

---

## 12. Parametri v6 proposti

Rimuovere:

```text
D2_xe_scale
phi
```

Aggiungere:

```text
Dg_D1_scale
Dg_D3_scale

Dv_D1_scale
Dv_D2_scale

b_bulk_scale
b_dislocation_scale

gb_scale

gd_bubble_scale
gd_line_scale
gd_line_alpha

coalescence_d_scale

capture_scale
```

Mantenere:

```text
f_n
K_d
rho_d
fission_rate
```

con prior fisico, non totalmente libero.

---

## 13. Sensitivity immediata su B

Prima di un Optuna globale v6, fare OAT su `B_standard_wide_trial_00629`.

### 13.1 Re-solution

```text
b_bulk_scale:
0.03, 0.05, 0.1, 0.2, 0.5, 1

b_dislocation_scale:
0.03, 0.05, 0.1, 0.2, 0.5, 1
```

### 13.2 Coalescenza

```text
coalescence_d_scale:
0.25, 0.5, 0.75, 1, 2, 4
```

### 13.3 Capture

```text
capture_scale:
0.5, 1, 2.39, 4
```

### 13.4 Line sink dislocation

```text
gd_line_alpha:
0, 0.25, 0.5, 0.75, 1

gd_line_scale:
0.5, 1, 2, 4

gd_bubble_scale:
0.5, 1, 2, 4
```

### 13.5 Diffusività

```text
Dg_D1_scale:
0.3, 0.6, 1, 2

Dg_D3_scale:
0.3, 0.6, 1, 2

Dv_D1_scale:
0.5, 1, 2, 4

Dv_D2_scale:
0.5, 1, 2, 4
```

---

## 14. Mini 2D consigliate

Dopo OAT, fare solo poche mappe 2D:

```text
b_dislocation_scale × coalescence_d_scale
gd_line_alpha × gd_line_scale
capture_scale × coalescence_d_scale
Dg_D1_scale × Dg_D3_scale
Dv_D1_scale × Dv_D2_scale
```

Metriche da guardare:

```text
score_total
score_swd
score_Rd
score_Nd
score_pressure
score_partition
score_qgb
score_radius_guard
score_radius_saturation

R_d(1600,1800,1900,2000)
N_d(1600,1725)
p_d/p_eq(1600)
bulk gas 1600
q_gb 1600
```

---

## 15. Criterio di successo v6

La variante v6 è utile solo se migliora o chiarisce almeno una di queste cose:

1. permette `b_dislocation_scale` meno estremo di `0.05`;
2. migliora `N_d(T)` senza far esplodere `R_d`;
3. mantiene `p_d/p_eq` vicino a 1;
4. mantiene bulk gas a bassa/intermedia T ragionevole;
5. chiarisce se il problema è:
   - re-solution,
   - line sink dislocation,
   - capture geometry,
   - diffusività,
   - coalescenza.

Se v6 ottiene score migliore ma usando parametri ancora più estremi e senza chiarire la fisica, non è un progresso scientifico.

---

## 16. Conclusione operativa

La prossima versione non deve essere un semplice v5 con più trial.

Deve essere:

```text
capture_only v6 = v5 scoring + fisica scomposta meglio
```

Modifiche prioritarie:

```text
1. rimuovere D2_xe_scale;
2. non usare phi;
3. separare Dg in D1/D3;
4. separare Dv in D1/D2;
5. separare b_bulk e b_dislocation;
6. separare gd_bubble e gd_line;
7. introdurre alpha nel line sink;
8. correggere Delta V_cap alla forma Barani dR_d;
9. mantenere capture lineare clippata, senza esponenziale.
```

Questa variante serve per capire se il candidato B è un vero equilibrio fisico o solo un incastro empirico di scale factor aggregati.
