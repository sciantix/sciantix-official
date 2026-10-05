# 30/09/2026 — Nuove pubblicazioni per UN

## Scopo

Questo file raccoglie le principali novità bibliografiche emerse il 30/09/2026 e le loro conseguenze per il modello UN attualmente sviluppato in SCIANTIX/Python.

L'obiettivo è distinguere chiaramente tra:

- dati sperimentali;
- output di modello;
- nuove formulazioni / nuovi parametri;
- informazioni utili per calibrazione e validazione;
- punti ancora aperti da verificare prima della scrittura finale della tesi.

---

## 1. Rizk 2025: la gas partition di Fig. 9 non è un dato sperimentale

La Fig. 9 di Rizk et al. (2025), usata finora come riferimento per:

- gas in matrix / solution;
- bulk bubbles;
- dislocation bubbles;
- grain-boundary bubbles;
- released gas / FGR,

è un **output interno del modello UNSIFGRS/BISON**, non una misura sperimentale diretta della partizione del gas.

Quindi va usata principalmente come:

$$
\boxed{\text{benchmark di implementazione del modello Rizk}}
$$

e non come:

$$
\boxed{\text{target sperimentale di calibrazione}}
$$

### Conseguenza per il nostro modello

Se il nostro modello riproduce la Fig. 9 di Rizk, significa soprattutto che la nostra implementazione riproduce la logica interna del modello Rizk.

Non significa automaticamente che la partizione del gas sia sperimentalmente validata.

La gas partition di Rizk può quindi essere usata come controllo qualitativo / diagnostico, ma non dovrebbe avere lo stesso peso dei dati sperimentali P2.

### Reference

J. T. Rizk, M. W. D. Cooper, P. C. A. Simon, A. J. Schneider, D. A. Andersson, S. R. Novascone, C. Matthews,  
**“Mechanistic nuclear fuel performance modeling of uranium nitride”**,  
*Journal of Nuclear Materials* 606 (2025) 155604.  
DOI: 10.1016/j.jnucmat.2024.155604

---

## 2. Cosa è invece sperimentalmente utile in Rizk

Rizk confronta il modello con dati microstrutturali relativi alle large intragranular bubbles, indicate come popolazione P2 e interpretate come **dislocation bubbles**.

Le quantità più direttamente vincolate da dati sperimentali sono quindi:

$$
R_d,\qquad N_d,\qquad S_d
$$

dove:

- $R_d$ = raggio medio delle dislocation bubbles;
- $N_d$ = concentrazione numerica;
- $S_d$ = swelling associato.

### Conseguenza metodologica

Per l'intragranulare, questi restano i target principali di calibrazione.

La gas partition di Rizk va invece considerata soprattutto come benchmark di modello.

---

## 3. Miller 2025: test integrale del framework SIFGRS/Rizk

È stata trovata la dissertation completa di:

**Zachary Aaron Miller**,  
*Accelerated Fuel Qualification of Uranium Mononitride: Mechanistic Modeling, Bayesian Calibration, and Regime-Stratified Validation*,  
PhD Dissertation, University of California, Berkeley, 2025.

Permalink: https://escholarship.org/uc/item/5t55p5kx

Miller dichiara esplicitamente che usa il framework **SIFGRS sviluppato da Rizk et al.** all'interno di BISON.

La struttura concettuale è:

$$
\text{Rizk / UNSIFGRS}
\rightarrow
\text{modello locale FGR + swelling}
\rightarrow
\text{BISON}
$$

BISON risolve il fuel pin con campi spaziali e temporali:

$$
T(r,z,t),\quad \dot F(r,z,t),\quad BU(r,z,t),\quad \sigma(r,z,t),\ldots
$$

e aggiorna localmente il modello SIFGRS.

### Differenza rispetto al nostro notebook

Il nostro modello corrente è sostanzialmente un **single-material-point / separate-effects model**.

Miller usa invece BISON per simulazioni integrali del fuel pin.

---

## 4. Miller trova che il baseline FGR sottostima molti casi SNAP-50

Nella dissertation, Miller confronta il baseline SIFGRS con il database SNAP-50.

Il risultato generale è che il **FGR baseline viene sistematicamente sottostimato in molti casi**.

Nella regione circa:

$$
1400-1800\ \mathrm{K}
$$

alcuni dati sperimentali mostrano FGR dell'ordine di:

$$
5-7\%
$$

mentre molte predizioni baseline rimangono:

$$
<2\%.
$$

Sotto circa 1400 K, il modello spesso predice release quasi nullo.

### Alcuni esempi riportati da Miller

| Specimen | T [K] | Burnup [%FIMA] | FGR exp [%] | FGR BISON baseline [%] |
|---|---:|---:|---:|---:|
| 220-M | 1714 | 0.95 | 12.0 | 0.953 |
| 665-M | 1426 | 4.58 | 5.96 | 0.061 |
| 669-M | 1348 | 2.72 | 0.13 | ~0 |

### Interpretazione corretta

Questo NON significa:

> Miller dimostra che la specifica Fig. 9 di Rizk è sbagliata.

Significa invece:

$$
\boxed{\text{la parametrizzazione SIFGRS/Rizk non generalizza bene al FGR integrale SNAP-50}}
$$

### Reference

Z. A. Miller, 2025, PhD Dissertation, UC Berkeley, soprattutto Chapter 5.

---

## 5. Diagnosi di Miller: collo di bottiglia intragranulare

Miller mostra che il problema del FGR non è necessariamente solo nel grain-boundary model.

La sua diagnosi è che molto gas rimane intrappolato intragranularmente e troppo poco gas raggiunge il grain boundary.

Schema concettuale:

$$
\text{dislocation bubble}
\rightarrow
\text{re-solution}
\rightarrow
\text{matrix}
\rightarrow
\text{re-trapping intragranulare}
$$

invece di:

$$
\text{matrix}
\rightarrow
\text{grain boundary}
\rightarrow
\text{release}
$$

Tra i parametri esplorati da Miller compaiono:

- trapping;
- re-solution;
- intragranular diffusivity;
- saturation coverage;
- grain-boundary vacancy diffusivity;
- dislocation density.

Una combinazione aggressiva studiata è:

$$
\text{trapping}=0.1\times,
\qquad
\text{resolution}=10\times,
\qquad
D_{\rm intra}=10\times
$$

che migliora alcuni casi ma peggiora altri.

### Conseguenza

Il FGR non può essere corretto agendo solo sull'intergranulare se a monte arriva troppo poco gas.

---

## 6. Non è stato trovato un regression test esplicito di Miller contro le figure di Rizk

Nella dissertation non è stata trovata una sezione del tipo:

> “Abbiamo riprodotto Fig. 3–9 di Rizk con gli stessi input e ottenuto gli stessi risultati.”

Quindi non possiamo affermare che Miller abbia fatto un vero regression test point-by-point contro Rizk.

Possiamo invece affermare che:

- usa il framework SIFGRS/Rizk;
- lo implementa in BISON;
- lo testa su un database integrale SNAP-50 molto più ampio.

---

## 7. Conseguenza per la calibrazione FGR del nostro modello

Al momento il nostro modello non è una simulazione integrale del fuel pin.

Quindi:

$$
\boxed{\text{non calibrare il FGR direttamente sulla Fig. 9 di Rizk}}
$$

Per una vera validazione FGR servono casi integrali con:

- storia di temperatura;
- storia di potenza / fission rate;
- burnup;
- geometria;
- grain size;
- condizioni al contorno.

### Strategia attuale

Per ora:

- calibrare l'intragranulare su P2;
- usare Fig. 9 di Rizk come benchmark di implementazione;
- usare FGR del nostro separate-effects case come diagnostica;
- costruire in seguito uno o più benchmark SNAP-50.

---

## 8. Nuovo report LANL 2025: aggiornamento della baseline UN

Nuova fonte importante:

**C. Matthews, C. O. Galvin, A. J. Schneider, M. W. D. Cooper**,  
*Finish development, test and then demonstrate new baseline fuel performance capability for UN fuel swelling models under steady-state and transient conditions*,  
Los Alamos National Laboratory, **LA-UR-25-28152**, 2025.

Il report rappresenta una continuazione diretta della linea Rizk / UNSIFGRS / BISON.

### Punto importante

Il report afferma che i parametri di base vengono mantenuti dalla **hand-calibration dell'anno precedente**.

Quella calibrazione era guidata soprattutto da separate-effects swelling data.

Il report non fornisce un nuovo set completo di parametri calibrati del tipo:

$$
f_n,\quad K_d,\quad \rho_d,\quad b,\quad F_{c,sat},\ldots
$$

Le principali novità sono invece:

- diffusività lower-length-scale aggiornate;
- chemistry / stoichiometry;
- transient FGR / microcracking;
- aggiornamenti Centipede / BISON.

---

## 9. Nuova formula per la vacancy diffusivity al grain boundary

Il report LANL 2025 fornisce:

$$
\boxed{
D_v^{GB}
=
325
\exp\left(
-\frac{5.95}{k_BT}
\right)
\ \mathrm{m^2/s}
}
$$

e mantiene l'ipotesi:

$$
\boxed{
D_v^{GB}=10^6D_{v,\mathrm{thermal}}^{bulk}
}
$$

Quindi:

$$
\boxed{
D_{v,\mathrm{thermal}}^{bulk}
=
3.25\times10^{-4}
\exp\left(
-\frac{5.95}{k_BT}
\right)
\ \mathrm{m^2/s}
}
$$

### Attenzione

Questa non è automaticamente la diffusività vacancy bulk totale sotto irraggiamento.

In generale:

$$
D_v^{bulk,tot}=D_{v,1}+D_{v,2}+\ldots
$$

La formula sopra corrisponde alla componente termica usata come base per il percorso rapido al grain boundary.

### Conseguenza pratica

Per ora conviene:

1. lasciare invariata la $D_v^{bulk}$ del modello attuale;
2. testare separatamente la nuova $D_v^{GB}(T)$;
3. confrontare:
   - onset di $F_c=0.5$;
   - $R_{gf}$;
   - $N_{gf}$;
   - grain-face gas inventory;
   - FGR diagnostico.

---

## 10. Perché la nuova $D_v^{GB}$ è diversa dalla baseline Rizk

La vecchia linea era basata principalmente su:

**M. W. D. Cooper et al.**,  
“Simulations of self- and Xe diffusivity in uranium mononitride including chemistry and irradiation effects,”  
*Journal of Nuclear Materials* 587 (2023) 154685.  
DOI: 10.1016/j.jnucmat.2023.154685

Il nuovo report usa un dataset lower-length-scale aggiornato di Schneider et al., con informazioni ab-initio più estese su difetti e cluster.

La relazione:

$$
D_v^{GB}=10^6D_v^{bulk,thermal}
$$

rimane una **assunzione di modello**.

Quello che cambia è la diffusività bulk termica sottostante.

---

## 11. $D_{2,Xe}$: da trascurare

La situazione ora è più chiara.

### Cooper 2023

La vecchia lower-length-scale baseline includeva un contributo irradiation-enhanced Xe $D_2$.

### Rizk 2025

Rizk già tratta $D_{2,Xe}$ come trascurabile rispetto a:

$$
D_1+D_3
$$

nelle condizioni rilevanti.

### Schneider 2026

Nuova fonte:

**A. J. Schneider, C. Matthews, D. A. Andersson, M. W. D. Cooper**,  
“Ab-initio informed cluster dynamics simulation of self- and Xe diffusivity in uranium mononitride under irradiation,”  
*Journal of Nuclear Materials* 620 (2026) 156360.  
DOI: 10.1016/j.jnucmat.2025.156360

La nuova parametrizzazione ab-initio aumenta fortemente la barriera di migrazione del difetto Xe interstitial rispetto alla vecchia stima, circa:

$$
0.4\ \mathrm{eV}
\rightarrow
1.38\ \mathrm{eV}
$$

Il contributo irradiation-enhanced Xe diventa quindi piccolo rispetto al plateau atermico $D_3$.

### Decisione per il nostro modello

$$
\boxed{D_{2,Xe}=0}
$$

oppure, nella tesi:

> “The irradiation-enhanced Xe contribution $D_2$ is neglected because it is negligible relative to $D_1+D_3$ over the relevant conditions.”

### Attenzione

Questo non implica:

$$
D_{2,V_U}=0
$$

La vacancy diffusivity va trattata separatamente.

---

## 12. Chemistry / stoichiometry

Il report LANL 2025 mostra che la diffusività e lo swelling sono fortemente dipendenti dalla stechiometria.

La vecchia hand-calibration richiedeva una condizione molto N-rich per riprodurre bene alcuni separate-effects swelling data.

Con i nuovi dati lower-length-scale, il comportamento migliore viene invece ottenuto vicino allo stechiometrico:

$$
UN_{1+y},
\qquad
y\sim10^{-6}-10^{-5}
$$

### Implicazione

La stechiometria è una variabile fisica importante e potrebbe spiegare parte dei scale factor artificiali usati in vecchie calibrazioni.

Per la tesi attuale può essere trattata come:

- limite della baseline a composizione fissa;
- possibile estensione futura;
- non necessariamente da introdurre subito in `11test_UN`.

---

## 13. Charatsidou et al. 2024: irradiation-induced defects e cracking in UN

Nuova fonte:

**E. Charatsidou, M. Giamouridou, A. Fazi, et al.**,  
“Proton irradiation-induced cracking and microstructural defects in UN and (U,Zr)N composite fuels,”  
*Journal of Materiomics* 10(4) (2024) 906–918.  
DOI: 10.1016/j.jmat.2024.01.014

Il lavoro usa proton irradiation da 2 MeV fino a 100 dpa.

### Risultati utili per il nostro modello

Per UN osservano:

- dislocation loops;
- aumento della loop density con dose;
- tendenza alla saturazione ad alta dose;
- cracking da irraggiamento;
- componenti sia intergranulari sia transgranulari.

Densità di loop riportate per UN:

| Dose | Loop density |
|---|---:|
| 1 dpa plateau | $0.7\times10^{21}\ \mathrm{m^{-3}}$ |
| 10 dpa peak | $20\times10^{21}\ \mathrm{m^{-3}}$ |
| 10 dpa plateau | $11\times10^{21}\ \mathrm{m^{-3}}$ |
| 100 dpa peak | $26\times10^{21}\ \mathrm{m^{-3}}$ |

### Implicazione per $\rho_d$

Questo supporta qualitativamente l'idea che:

$$
\rho_d=\mathrm{const.}
$$

sia una semplificazione forte.

Ma i dati sono **number density di loops** $[\mathrm{m^{-3}}]$, non dislocation line density $[\mathrm{m^{-2}}]$.

Una conversione richiederebbe, ad esempio:

$$
\rho_d^{loops}
\sim
N_{loop}2\pi R_{loop}
$$

quindi non possiamo usare direttamente quei numeri come parametro $\rho_d$.

### Limiti

Non usare questi dati per calibrare direttamente FGR o bubble parameters perché:

- proton irradiation, non neutron irradiation;
- temperatura circa ambiente;
- forte H implantation;
- a 100 dpa l'H locale è molto elevato;
- swelling e cracking derivano da danno reticolare + H.

---

## 14. Grain size da Charatsidou

Il paper misura per UN circa:

$$
d_g=7.8\pm2.8\ \mu\mathrm{m}
$$

da SEM e:

$$
d_g=7.1\pm2.3\ \mu\mathrm{m}
$$

da EBSD.

Si tratta di **diametro equivalente**, non raggio.

Quindi conferma l'ordine di grandezza micrometrico, ma non giustifica direttamente il nostro:

$$
r_g=6\ \mu\mathrm{m}
$$

che corrisponde a circa 12 µm di diametro.

---

## 15. Microcracking e transient FGR

Charatsidou mostra cracking da irraggiamento con componenti intergranulari e transgranulari.

Il report LANL 2025 introduce invece una capacità transient/microcracking nel modello BISON/UNSIFGRS.

Le due cose sono coerenti qualitativamente, ma non equivalenti quantitativamente.

### Uso corretto nella tesi

Il paper Charatsidou può supportare una frase del tipo:

> “Irradiation-induced cracking, including grain-boundary cracking, has been experimentally observed in UN.”

Ma non può validare direttamente un modello quantitativo di transient FGR.

---

## 16. Chen et al. 2025 — ML swelling model

**R. Chen, Z. Miller, T. Gibson, V. K. Mehta, M. Fratoni, A. Levinsky, G. T. Craven**,  
“Machine learning models for volumetric swelling in uranium nitride,”  
*Journal of Nuclear Materials* 615 (2025) 155980.  
DOI: 10.1016/j.jnucmat.2025.155980

### Utilità

È utile per:

- dataset storici di swelling UN;
- trend con temperatura, burnup e power density;
- confronto esterno sullo swelling totale;
- possibile benchmark empirico.

Non va usato per fissare direttamente:

$$
f_n,\quad K_d,\quad D_v^{GB},\quad F_{c,sat}
$$

perché non è un modello mechanistic bubble/FGR.

---

## 17. Miller et al. NETS 2025

Reference identificata:

**Z. Miller, M. Fratoni, A. Levinsky, G. T. Craven, C. Matthews**,  
“Optimization of the BISON UN Fission Gas Release and Swelling Model,”  
*Proceedings of Nuclear and Emerging Technologies for Space (NETS 2025)*,  
Huntsville, Alabama, May 4–8, 2025, pp. 123–130.

### Stato

Full text non ancora disponibile nella bibliografia locale.

### Priorità

$$
\boxed{\text{ALTISSIMA}}
$$

perché riguarda esattamente il problema della calibrazione FGR/swelling UN in BISON.

La dissertation Miller 2025 è per ora la fonte più completa disponibile sullo stesso filone.

---

## 18. Decisioni operative aggiornate per il nostro modello

### Da fare subito / quasi subito

- documentare:
  $$
  D_{2,Xe}=0
  $$
  con Rizk 2025 + Schneider 2026;

- implementare come opzione la nuova:
  $$
  D_v^{GB}(T)
  =
  325\exp(-5.95/k_BT)
  $$

- confrontare il comportamento intergranulare con l'attuale baseline.

### Da NON fare subito

- non rifittare $f_n$ solo per migliorare il FGR;
- non usare Fig. 9 di Rizk come target sperimentale;
- non sostituire direttamente la nostra $D_v^{bulk}$ con la nuova formula GB / $10^6$;
- non usare Charatsidou per fissare numericamente $\rho_d$.

### Da fare dopo

- costruire uno o più benchmark SNAP-50 integrali;
- verificare se la calibrazione intragranulare rimane coerente con il FGR integrale;
- valutare $\rho_d(T,B)$;
- valutare chemistry/stoichiometry;
- recuperare il paper NETS 2025;
- rintracciare la documentazione della hand-calibration FY24.

---

## 19. Statements pronti da riusare nella tesi

### Gas partition

> The gas partition reported by Rizk et al. is a model prediction rather than a direct experimental measurement and is therefore used here primarily as an implementation benchmark.

**Ref:** Rizk et al. (2025).

### FGR integrale

> Subsequent integral validation of the UN SIFGRS framework against a broader SNAP-50 database revealed systematic underprediction of fission gas release in several operating regimes.

**Ref:** Miller (2025), Chapter 5.

### Trapping intragranulare

> Gas-partition diagnostics indicate that excessive intragranular trapping and limited transport to grain boundaries can constrain the predicted FGR more strongly than grain-boundary saturation alone.

**Ref:** Miller (2025), Chapter 5.

### Xe irradiation-enhanced diffusion

> Updated ab-initio-informed cluster-dynamics calculations strongly suppress the irradiation-enhanced Xe diffusion contribution, supporting the approximation $D_{Xe}\approx D_1+D_3$.

**Refs:** Rizk et al. (2025); Schneider et al. (2026).

### Grain-boundary vacancy diffusion

> The updated LANL UN baseline retains the assumption of grain-boundary vacancy diffusion being $10^6$ times faster than the corresponding thermal bulk contribution, while updating the underlying lower-length-scale diffusivity.

**Ref:** Matthews et al. (2025), LA-UR-25-28152.

### Dislocation evolution

> Proton-irradiation experiments on UN show a pronounced dose dependence of irradiation-induced dislocation-loop density, providing qualitative evidence that a constant dislocation-density assumption is not universally valid.

**Ref:** Charatsidou et al. (2024).

### Cracking

> Irradiated UN exhibits both intergranular and transgranular cracking initiated in highly damaged regions, although proton-implantation conditions preclude direct quantitative transfer to in-reactor FGR.

**Ref:** Charatsidou et al. (2024).

---

## 20. Bibliografia aggiornata

1. **Rizk, J. T., Cooper, M. W. D., Simon, P. C. A., Schneider, A. J., Andersson, D. A., Novascone, S. R., Matthews, C.**  
   “Mechanistic nuclear fuel performance modeling of uranium nitride.”  
   *Journal of Nuclear Materials* 606 (2025) 155604.  
   DOI: 10.1016/j.jnucmat.2024.155604

2. **Miller, Z. A.**  
   *Accelerated Fuel Qualification of Uranium Mononitride: Mechanistic Modeling, Bayesian Calibration, and Regime-Stratified Validation.*  
   PhD Dissertation, University of California, Berkeley, 2025.  
   https://escholarship.org/uc/item/5t55p5kx

3. **Matthews, C., Galvin, C. O., Schneider, A. J., Cooper, M. W. D.**  
   *Finish development, test and then demonstrate new baseline fuel performance capability for UN fuel swelling models under steady-state and transient conditions.*  
   Los Alamos National Laboratory, LA-UR-25-28152, 2025.

4. **Schneider, A. J., Matthews, C., Andersson, D. A., Cooper, M. W. D.**  
   “Ab-initio informed cluster dynamics simulation of self- and Xe diffusivity in uranium mononitride under irradiation.”  
   *Journal of Nuclear Materials* 620 (2026) 156360.  
   DOI: 10.1016/j.jnucmat.2025.156360

5. **Cooper, M. W. D., Rizk, J., Matthews, C., Kocevski, V., Craven, G., Gibson, T., Andersson, D.**  
   “Simulations of self- and Xe diffusivity in uranium mononitride including chemistry and irradiation effects.”  
   *Journal of Nuclear Materials* 587 (2023) 154685.  
   DOI: 10.1016/j.jnucmat.2023.154685

6. **Charatsidou, E., Giamouridou, M., Fazi, A., et al.**  
   “Proton irradiation-induced cracking and microstructural defects in UN and (U,Zr)N composite fuels.”  
   *Journal of Materiomics* 10(4) (2024) 906–918.  
   DOI: 10.1016/j.jmat.2024.01.014

7. **Chen, R., Miller, Z., Gibson, T., Mehta, V. K., Fratoni, M., Levinsky, A., Craven, G. T.**  
   “Machine learning models for volumetric swelling in uranium nitride.”  
   *Journal of Nuclear Materials* 615 (2025) 155980.  
   DOI: 10.1016/j.jnucmat.2025.155980

8. **Miller, Z., Fratoni, M., Levinsky, A., Craven, G. T., Matthews, C.**  
   “Optimization of the BISON UN Fission Gas Release and Swelling Model.”  
   *Proceedings of Nuclear and Emerging Technologies for Space (NETS 2025)*, Huntsville, Alabama, May 4–8, 2025, pp. 123–130.

---

## 21. Open questions

1. Recuperare il full text del paper NETS 2025.
2. Trovare il report/documento FY24 della hand-calibration citata da Matthews et al. 2025.
3. Testare la nuova $D_v^{GB}(T)$ nell'11test.
4. Consolidare $D_{2,Xe}=0$ nel codice.
5. Decidere come costruire un benchmark integrale SNAP-50 compatibile con il nostro framework.
6. Riesaminare in futuro $f_n$, trapping, re-solution e $\rho_d(T,B)$ senza compromettere il fit P2.
7. Mantenere sempre separati:
   - dati sperimentali;
   - benchmark Rizk;
   - nuove lower-length-scale physics;
   - parametri di calibrazione.

---

## Commento per la tesi — dalla lower-length-scale physics a BISON

Il modello UN segue una struttura multiscala. Alla scala atomistica, calcoli **DFT** e, dove necessario, simulazioni di **molecular dynamics (MD)** forniscono proprietà fondamentali dei difetti del reticolo, come energie di formazione, energie di legame e barriere di migrazione. Queste quantità descrivono quanto sia energeticamente favorevole creare, muovere o associare vacancy, interstiziali, atomi di Xe e relativi cluster.

Le informazioni atomistiche vengono poi utilizzate nel codice di **cluster dynamics Centipede**. Centipede non segue direttamente tutti gli atomi del cristallo, ma risolve l'evoluzione delle concentrazioni delle diverse specie di difetto e dei cluster. Tiene quindi conto della produzione di difetti da irraggiamento, della loro ricombinazione, del clustering e dell'assorbimento ai sink. Le velocità di reazione dipendono dalle diffusività dei difetti e dalle differenze di energia libera tra reagenti e prodotti, in modo da rispettare la termodinamica delle reazioni. Per UN vengono trattati esplicitamente il sottoreticolo dell'uranio, quello dell'azoto e i siti interstiziali, rendendo possibile descrivere anche la dipendenza dalla stechiometria.

Il risultato di Centipede è una descrizione efficace della mobilità dei difetti e dello Xe nelle diverse condizioni di temperatura, fission rate e composizione. Da queste simulazioni vengono ricavate le **self-diffusivities** e la **Xe diffusivity** utilizzate dal modello di fission gas.

Queste diffusività costituiscono quindi l'input lower-length-scale del modello **UNSIFGRS/SIFGRS**, che descrive a scala di grano trapping, re-solution, nucleazione e crescita delle bolle intragranulari e su dislocazione, migrazione del gas verso il grain boundary, crescita delle bolle intergranulari e fission gas release.

Infine, **BISON** incorpora UNSIFGRS all'interno della simulazione fuel-performance del pin. BISON fornisce localmente le condizioni termo-meccaniche e di irraggiamento, mentre il modello di fission gas restituisce swelling e release. In questo modo informazioni ottenute alla scala elettronica e atomistica vengono propagate fino alla scala ingegneristica del combustibile.

In forma sintetica, la catena multiscala è:

**DFT / MD → defect energetics → Centipede cluster dynamics → diffusività efficaci di self-defects e Xe → UNSIFGRS/SIFGRS → swelling e FGR → BISON fuel-performance simulation.**

**Riferimenti principali:** Cooper et al. (2023); Rizk et al. (2025); Matthews et al. (2025), LA-UR-25-28152; Schneider et al. (2026).

---

## Commento per la tesi — interstiziali non esplicitamente considerati nella bubble-growth law

Nel modello UN attuale la crescita delle bolle considera esplicitamente l'assorbimento di **Xe** e di **vacanze**, ma non introduce un termine esplicito di assorbimento dei self-interstitials. Fisicamente i due difetti hanno effetti opposti sulla cavità: l'assorbimento di una vacanza aggiunge volume libero e tende quindi a **far crescere la bolla**, mentre l'assorbimento di un interstiziale tende a **ridurre la cavità**, perché l'atomo interstiziale può ricostituire un sito reticolare alla superficie della bolla. In una descrizione completa sotto irraggiamento, la crescita dipenderebbe quindi dal bilancio tra flussi di vacanze e interstiziali, insieme a produzione di coppie di Frenkel, ricombinazione e assorbimento ai diversi sink.

L'assenza degli interstiziali espliciti non implica che questa fisica sia inesistente: nei modelli ridotti di evoluzione delle bolle è comune descrivere soltanto gas + vacanze e rappresentare in modo efficace il bilancio vacancy–interstitial tramite una produzione netta di vacanze o termini source/sink calibrati. Aagesen (2024) mostra esplicitamente che un modello *vacancy-only* opportunamente parametrizzato può approssimare la crescita ottenuta con una descrizione completa vacancy–interstitial. Questo costituisce un riferimento utile per motivare, nella tesi, la semplificazione adottata nel modello corrente; non giustifica però l'aggiunta arbitraria di un termine di interstitial absorption senza introdurre coerentemente anche concentrazioni, diffusività, ricombinazione e sink bias degli interstiziali.

**Reference:** L. K. Aagesen, “Parameterization of vacancy production rate in phase-field models of fission gas bubble evolution in nuclear fuel,” *Journal of Nuclear Materials* 601 (2024) 155311. DOI: 10.1016/j.jnucmat.2024.155311.

---

## Nota sul ruolo degli interstiziali nella diffusività vacancy e nella crescita delle bolle

Nel cluster dynamics vengono prodotti vacancy + interstiziali come Frenkel pairs e vengono trattati ricombinazione, clustering e assorbimento ai sink. Quindi la concentrazione steady-state delle vacancy è già influenzata dalla presenza degli interstiziali.

Fisicamente potresti avere:

$$
V \rightarrow \text{bubble}
$$

che aggiunge spazio libero e quindi fa crescere la bolla, ma anche:

$$
I \rightarrow \text{bubble}
$$

dove l'interstiziale arriva alla superficie della bolla e **annichila parte del volume vacante**.

Concettualmente quindi il bilancio corretto sarebbe qualcosa come

$$
\boxed{
\dot n_v = J_v^{\rm bubble} - J_i^{\rm bubble}
}
$$

non semplicemente

$$
\dot n_v\propto D_v(\ldots).
$$

Per questo una diffusività efficace del tipo

$$
D_v^{\rm eff}\sim c_v\,M_v
$$

può già contenere **indirettamente** gli effetti della ricombinazione vacancy–interstitial nella matrice.

---

## Evidenze aggiuntive — vacancy-rich matrix non implica flusso vacancy-dominante

I lavori di **Matthews et al. (2019)** e successivi sulla cluster dynamics di $UO_2$ mostrano che sotto irraggiamento può verificarsi un accumulo di uranium vacancies nella matrice. **Cooper et al. (2025)** richiama esplicitamente risultati precedenti in cui l'accumulo di $V_U$ porta il bulk $UO_2$ verso una condizione più iperstechiometrica.

Tuttavia, una maggiore concentrazione di vacancy non implica automaticamente che il flusso di vacancy verso una bolla domini quello degli interstiziali:

$$
\boxed{c_v>c_i\ \not\Rightarrow\ J_v>J_i}
$$

perché, in prima approssimazione, il flusso dipende sia dalla concentrazione sia dalla mobilità:

$$
J\sim cD.
$$

È quindi possibile avere:

$$
c_i\ll c_v
$$

ma contemporaneamente:

$$
D_i\gg D_v,
$$

così che il termine $c_iD_i$ resti importante o possa persino superare $c_vD_v$.

Nel caso di $UO_2$, **Cooper et al. (2025)** mostra proprio che, a bassa temperatura, pochi self-interstitials molto mobili possono avere un ruolo dominante nell'evoluzione della pressione delle fission-gas bubbles. Questo significa che la sola osservazione di una matrice vacancy-rich non è sufficiente, da sola, a giustificare il neglect del flusso di interstiziali verso le bolle.

Per UN, il quadro qualitativo va nella stessa direzione. Un lavoro del 2026 sui **dislocation loops in UN** riporta un'elevata mobilità degli uranium interstitials, associata a barriere di migrazione relativamente basse e a una crescita anomala dei loop sotto irraggiamento. Inoltre, **Schneider et al. (2026)** include esplicitamente sia vacancy clusters sia self-interstitial clusters nella cluster dynamics utilizzata per calcolare le diffusività sotto irraggiamento.

Di conseguenza, almeno qualitativamente:

$$
\boxed{\text{in UN non possiamo giustificare a priori }J_i^{\rm bubble}\approx0}
$$

soprattutto nei regimi in cui gli interstiziali sono molto mobili.

### References

- C. Matthews, R. Perriot, M. W. D. Cooper, C. R. Stanek, D. A. Andersson, **“Cluster dynamics simulation of uranium self-diffusion during irradiation in $UO_2$,”** *Journal of Nuclear Materials* 527 (2019) 151787. DOI: 10.1016/j.jnucmat.2019.151787.
- M. W. D. Cooper et al., **“The role of irradiation-enhanced interstitial diffusion in over-pressurizing fission gas bubbles in $UO_2$,”** *Journal of Nuclear Materials* (2025). ScienceDirect PII: S002231152400552X.
- A. J. Schneider et al., **“Ab-initio informed cluster dynamics simulation of self- and Xe diffusivity in uranium mononitride under irradiation,”** *Journal of Nuclear Materials* 620 (2026) 156360. DOI: 10.1016/j.jnucmat.2025.156360.
- 2026 study on irradiation-induced dislocation-loop evolution in UN, ScienceDirect PII: S1359646226001582. **Nota:** recuperare la citazione bibliografica completa prima della stesura finale della tesi.

---

## Commento per la tesi — incertezza sul fattore di nucleazione delle bolle sulle dislocazioni

Schneider et al. (2026) descrive la nucleazione delle dislocation bubbles come proporzionale alla densità di dislocazioni, con un fattore tipico

$$
K\sim10^6\ \mathrm{bubble\,m^{-1}},
$$

richiamando Rizk et al. (2025) e Ray & Blank (1984). È però importante non interpretare questo valore come una misura diretta della **nucleazione iniziale nel combustibile fresco**.

La base sperimentale di Ray & Blank (1984) deriva da **osservazioni TEM (Transmission Electron Microscopy) effettuate su combustibile mixed-carbide già irradiato**, con burnup compresi circa tra 1.8 e 11 at.%: vengono osservati la struttura di dislocazioni e le bolle di gas di fissione associate al loro ambiente microstrutturale. Si tratta quindi di una microstruttura **evoluta durante l'irraggiamento e osservata post-irradiation**, non della configurazione iniziale del pellet.

Di conseguenza, quando nel modello si usa una relazione del tipo

$$
N_{d,0}=K\rho_d,
$$

il parametro $K$ va considerato come un **fattore efficace di modello**, motivato/constraining da osservazioni microstrutturali post-irraggiamento, e non come un parametro sperimentale direttamente misurato a $t=0$. Questo introduce un'incertezza fisica importante: durante l'irraggiamento evolvono sia la densità di dislocazioni sia il numero di bolle associate ad esse, mentre il modello semplificato può trasferire tale informazione finale in una condizione iniziale equivalente.

Questo punto è coerente anche con Barani et al. (2020), dove $K=10^6\ \mathrm{bubble\,m^{-1}}$ è esplicitamente indicato come **model parameter** (“Present work”), rappresentativo del numero di bolle nucleate per unità di dislocazione, piuttosto che come una misura indipendente della microstruttura iniziale.

### References

- A. J. Schneider et al., **“Integrating mechanistic models with separate effects and integral tests through neural network-enabled Bayesian inference: Application to uranium nitride fuel swelling,”** *Journal of Nuclear Materials* 632 (2026) 156895. DOI: 10.1016/j.jnucmat.2026.156895.
- J. T. Rizk et al., **“Mechanistic nuclear fuel performance modeling of uranium nitride,”** *Journal of Nuclear Materials* 606 (2025) 155604. DOI: 10.1016/j.jnucmat.2024.155604.
- I. L. F. Ray, H. Blank, **“Microstructure and fission gas bubbles in irradiated mixed carbide fuels at 2 to 11 a/o burnup,”** *Journal of Nuclear Materials* 124 (1984) 159–174. DOI: 10.1016/0022-3115(84)90020-5.
- T. Barani, G. Pastore, A. Magni, D. Pizzocri, P. Van Uffelen, L. Luzzi, **“Modeling intra-granular fission gas bubble evolution and coarsening in uranium dioxide during in-pile transients,”** *Journal of Nuclear Materials* 538 (2020) 152195. DOI: 10.1016/j.jnucmat.2020.152195.


---

## Commenti per la tesi — Schneider et al. 2026: Bayesian UQ, sensitivity e possibili estensioni del nostro modello

### Riassunto breve del paper

**A. J. Schneider et al.**, “Integrating mechanistic models with separate effects and integral tests through neural network-enabled Bayesian inference: Application to uranium nitride fuel swelling,” *Journal of Nuclear Materials* 632 (2026) 156895, applica una metodologia di **uncertainty quantification (UQ) e calibrazione bayesiana** alla catena multiscala UN:

$$
\text{DFT / lower-length-scale parameters}
\rightarrow
\text{Centipede}
\rightarrow
D_U,\ D_N,\ D_{Xe}
\rightarrow
\text{UNSIFGRS/BISON}
\rightarrow
\text{swelling}.
$$

Il problema computazionale viene reso trattabile tramite una **rete neurale surrogate** addestrata sui risultati del modello fisico Centipede+BISON, non sui dati sperimentali. La rete va quindi interpretata come una funzione multivariata approssimata della response surface del modello: serve a riprodurre velocemente gli output del modello all'interno del dominio di training e permette di eseguire decine di milioni di valutazioni durante la MCMC.

Il workflow è, in forma sintetica:

1. partire da 232 parametri complessivi del framework, di cui 160 considerati esattamente definiti e 72 trattati come incerti;
2. eseguire una sensitivity analysis di Sobol sui parametri incerti;
3. selezionare un sottoinsieme di parametri che cattura quasi tutta la varianza degli output;
4. addestrare una neural-network surrogate del modello fisico;
5. calibrare bayesianamente i parametri rispetto ai dati sperimentali;
6. propagare la posterior attraverso il modello per ottenere distribuzioni probabilistiche di swelling e diffusività (*pushforward posterior*).

Per lo screening Sobol il paper considera quattro output:

$$
\boxed{D_U,\quad D_N,\quad D_{Xe},\quad S_d}
$$

con $S_d$ definito come **swelling da dislocation bubbles**. I 15 parametri selezionati dalla sensitivity catturano circa il 99% della varianza totale dei quattro output. La calibrazione bayesiana vera e propria viene poi descritta come effettuata su 16 parametri. Il paper presenta quindi una piccola ambiguità numerica tra “15 selected for inference” e “16 calibrated parameters”, che non va nascosta nella tesi.

Importante: i quattro output della sensitivity non coincidono esattamente con i target sperimentali della likelihood. La calibrazione usa principalmente:

$$
\boxed{D_U,\quad D_N,\quad S}
$$

mentre $D_{Xe}$ non viene calibrato direttamente contro i dati disponibili, perché le condizioni sperimentali — in particolare la pressione parziale di azoto — non sono documentate in modo sufficiente. La diffusività Xe viene usata successivamente come **verifica indipendente** del modello calibrato.

Analogamente, UNSIFGRS produce anche raggi, concentrazioni e gas partition delle diverse popolazioni di bolle, ma queste grandezze non sono tutte target diretti della sensitivity/calibrazione principale. Il raggio delle bolle viene poi usato come verifica microstrutturale indipendente nella discussione sulla localizzazione delle bolle.

### Parametri incerti più importanti nella sensitivity — ciò che interessa direttamente il nostro modello

Il risultato più rilevante per il nostro lavoro è che lo swelling da dislocation bubbles è **estremamente sensibile alla dislocation density**. Schneider et al. sottolineano però che parte di questa enorme sensitività deriva dal fatto che per $\rho_d$ viene usata una prior molto larga, a causa della scarsa conoscenza sperimentale della densità di dislocazioni in UN. Il valore del Sobol index non va quindi interpretato come una proprietà universale del materiale: dipende anche dal range di incertezza assegnato al parametro.

I principali gruppi di parametri sensibili identificati nel paper sono:

| Gruppo | Parametri / proprietà | Effetto principale | Rilevanza per il nostro modello |
|---|---|---|---|
| Microstruttura dislocazionale | densità di dislocazioni $\rho_d$ | parametro dominante per $S_d$ | **molto alta** |
| Evoluzione dislocazionale | dipendenza di $\rho_d$ dal burnup | unica sensitività che cambia in modo evidente con il burnup | **alta** |
| Nucleazione su dislocazione | dislocation-bubble nucleation factor/rate $K_d$ | effetto marginale ma non nullo | **alta**, perché nel nostro modello $N_{d,0}=K_d\rho_d$ |
| Vacanze U | $H_{V_U},\ S_{V_U},\ Q_{V_U}$ | controllano la mobilità/concentrazione vacancy e quindi la crescita delle bolle | **molto alta** |
| Xe–vacancy cluster | $H_{\{Xe:2V_U\}},\ S_{\{Xe:2V_U\}},\ Q_{\{Xe:2V_U\}}$ | importanti per $D_{Xe}$ soprattutto ad alta $T$ | **alta**, ma non sostituibili direttamente con i nostri parametri efficaci $D_g$ |
| Interstiziali N | $H_{N_i},\ S_{N_i},\ Q_{N_i}$ | forti effetti sulle diffusività a bassa $T$ | indiretta per il nostro modello ridotto |
| Chimica / $N_2$ | $H_{N_2},\ S_{N_2}$ e parametri della $p_{N_2}(T)$ | influenzano diffusività, stechiometria e quindi swelling | potenziale estensione futura |

Per la calibrazione il paper considera 11 parametri atomistici informati da calcoli *ab initio* e 5 parametri di microstruttura/ambiente. Gli 11 atomistici sono associati a $N_2$, $N_i$, $V_U$ e al cluster $\{Xe:2V_U\}$ attraverso entalpie, entropie e barriere di migrazione. I 5 parametri di scala superiore riguardano la densità di dislocazioni e la sua dipendenza dal burnup, il fattore di nucleazione delle dislocation bubbles e due parametri che definiscono la pressione parziale dell'azoto.

La sensitivity dello swelling resta quasi costante con il burnup fino a circa:

$$
3.5\%\ \mathrm{FIMA},
$$

con l'eccezione della dipendenza della densità di dislocazioni dal burnup. Questo suggerisce che, **all'interno della struttura di modello assunta da Schneider**, i meccanismi dominanti dello swelling non cambino drasticamente in quel range di burnup.

### Nota importante su priors e posterior

Nel paper vanno distinti due passaggi:

- per lo **screening Sobol** i parametri incerti sono esplorati su intervalli assegnati;
- nella **calibrazione bayesiana** vengono usate prior normali troncate ai limiti inferiori/superiori prescritti.

La posterior non viene quindi “scelta” dagli autori: deriva da

$$
p(\theta\mid D)\propto p(D\mid\theta)p(\theta).
$$

Campionando la posterior e propagandola attraverso il modello si ottiene la distribuzione probabilistica dello swelling. Questo è concettualmente diverso dal nostro uso di Optuna, che cerca soprattutto combinazioni di parametri con score minimo.

---

## Commento per la tesi — dislocation loops, trapping del gas e significato fisico di $\rho_d$

Durante l'irraggiamento si formano **dislocation loops**, principalmente attraverso l'accumulo di self-interstitials generati dalle cascate balistiche. Le dislocazioni e i loop costituiscono sink/trap per gli atomi di gas di fissione.

Una maggiore densità di dislocazioni può quindi aumentare la ritenzione del gas all'interno della matrice, ridurne la mobilità effettiva verso i grain boundaries e ritardarne l'accumulo nelle bolle intergranulari. Per questo $\rho_d$ influenza contemporaneamente:

- lo swelling da gas di fissione;
- la partizione del gas tra bulk, dislocation e grain boundary;
- il fission gas release.

Questo fornisce una motivazione fisica, oltre che statistica, all'elevata sensitività dello swelling rispetto a $\rho_d$ trovata da Schneider et al.

---

## Commento per la tesi — ciò che Schneider non esplora esplicitamente: $f_n$ e model-form sensitivity

Nel paper Schneider 2026 compare esplicitamente la sensitivity al **fattore/tasso di nucleazione delle dislocation bubbles**, ma non compare un parametro chiaramente identificabile con il nostro:

$$
f_n
$$

che controlla la nucleazione omogenea delle **bulk bubbles**.

Non è possibile stabilire dal paper se $f_n$:

- sia stato mantenuto fisso al valore nominale;
- sia incluso nel gruppo “Others”;
- oppure non sia stato incluso tra i 72 parametri incerti.

Questo è rilevante perché nel nostro modello $f_n$ è un parametro fenomenologico molto incerto e controlla:

$$
\nu_b=8\pi f_nD_g\Omega_{fg}^{1/3}c^2.
$$

Una possibile estensione originale rispetto al lavoro Schneider è quindi distinguere tra **parameter sensitivity** e **model-form sensitivity**. Schneider esplora soprattutto l'incertezza dei parametri mantenendo sostanzialmente fissata la struttura delle closure UNSIFGRS. Nel nostro lavoro possiamo invece chiedere quanto cambino i risultati modificando direttamente alcune ipotesi del single-size model.

---

## Commento per la tesi — sensitivity alla closure $\phi_b$ di distruzione delle bulk bubbles

Nel nostro modello la densità numerica delle bulk bubbles evolve con un termine di distruzione da re-solution del tipo:

$$
\frac{dN_b}{dt}=\nu_b-b_b\phi_bN_b,
$$

con

$$
\boxed{\phi_b=\frac{1}{n_b-1}}
$$

oppure, nella notazione del modello corrente,

$$
n_b=m_b'=\frac{m_b}{N_b}.
$$

Il significato di $\phi_b$ è quello di una **probabilità efficace che la re-solution porti alla distruzione completa di una bolla**: quando una bolla contiene molti atomi, un singolo evento di re-solution non ne causa normalmente la scomparsa.

Schneider et al. non eseguono una sensitivity specifica su questa closure. Una possibile analisi della tesi è quindi confrontare:

$$
\phi_b=\frac{1}{n_b-1}
$$

con formulazioni alternative o con scale factor controllati, per verificare esplicitamente quanto la legge di distruzione delle bolle influenzi $N_b$, $R_b$, trapping, re-solution e swelling.

Questo punto va mantenuto distinto dal test diagnostico già fatto nel nostro modello in cui $\phi$ veniva moltiplicato direttamente per il rate atomico di re-solution, $b_{eff}=b\phi$: quella era una modifica aggiuntiva della closure, non il significato originario di $\phi_b$ nell'equazione di $N_b$.

---

## Commento per la tesi — nucleazione come dimeri e accoppiamento con il raggio medio

Nel single-size model la nucleazione viene rappresentata in modo estremamente compatto. Se una nuova bolla viene trattata come un dimero e si forza il bilancio di massa:

$$
\left(\frac{dc}{dt}\right)_{\nu}=-2\nu_b,
\qquad
\left(\frac{dm_b}{dt}\right)_{\nu}=+2\nu_b,
$$

la creazione continua di nuove bolle tende, a gas totale fissato, ad aumentare $N_b$ e a ridurre il numero medio di atomi per bolla. Di conseguenza può ridursi il raggio medio rappresentativo:

$$
R_b\downarrow.
$$

Questo non modifica soltanto la microstruttura, perché nel modello trapping e re-solution dipendono dal raggio. Per esempio:

$$
g_b=4\pi D_gR_bN_b,
$$

mentre:

$$
b_b=b_0(R_b)\dot F.
$$

Quindi la scelta “nucleo = dimero” può propagarsi indirettamente nei coefficienti di scambio del gas. Una possibile **model-form sensitivity** consiste nel confrontare la nucleazione dimerica con una nucleazione efficace in cui la nuova popolazione nasce con un numero iniziale di atomi $n_0>2$, oppure con altre closure fisicamente motivate.

L'obiettivo non è necessariamente ottenere un fit migliore, ma capire se questa semplificazione ha un impatto trascurabile oppure misurabile sui risultati finali.

---

## Commento per la tesi — limite del single-size model e distribuzione reale dei raggi

Le osservazioni sperimentali di Ronchi mostrano distribuzioni di bubble size molto larghe, soprattutto alle temperature elevate. La Fig. 10 di Ronchi, ad esempio, mostra che la distribuzione evolve da una popolazione molto concentrata nelle classi piccole a distribuzioni estese su più classi dimensionali all'aumentare della temperatura.

Il nostro modello è invece un **single-size model**: a ogni popolazione associa un singolo $R$ rappresentativo. Le closure di trapping, re-solution, pressione e distruzione vengono quindi valutate a quel raggio efficace e non mediando esplicitamente sulla distribuzione reale $n(R)$.

Una possibile estensione per la tesi è valutare quanto questa approssimazione conti. Per il trapping bulk, se si disponesse della distribuzione completa:

$$
g_b^{dist}
=
4\pi D_g\int R\,n(R)\,dR
=
4\pi D_gN_b\langle R\rangle_N.
$$

Se il raggio del single-size model coincidesse esattamente con il number-average radius $\langle R\rangle_N$, una legge lineare in $R$ avrebbe lo stesso valore medio. Tuttavia il raggio efficace del modello deriva dal volume/massa media della popolazione e non è necessariamente identico a quel momento della distribuzione.

Il problema è ancora più importante per closure **non lineari**. Per esempio:

$$
\langle b(R)\rangle\neq b(\langle R\rangle),
$$

e analogamente una distribuzione di $n$ non è equivalente a valutare:

$$
\phi_b=\frac{1}{\langle n\rangle-1}.
$$

Quindi una possibile analisi aggiuntiva è confrontare:

- closure valutate al raggio single-size;
- closure mediate su una distribuzione sperimentale o parametrica di raggi.

Questo costituisce una **model-form uncertainty** che non viene esplorata nella sensitivity analysis di Schneider.

### Sensitivity esplicita al trapping $g$

In parallelo, conviene eseguire anche una sensitivity esplicita a un fattore moltiplicativo del trapping:

$$
g_b\rightarrow s_{g_b}g_b,
\qquad
 g_d\rightarrow s_{g_d}g_d,
$$

per separare l'effetto della closure geometrica/single-size dall'incertezza complessiva sull'intensità del trapping.

---

## Commento per la tesi — Ronchi 1978: cosa viene realmente misurato

Le misure sperimentali di microscopic swelling di Ronchi et al. **non sono una misura diretta della variazione geometrica dell'intero pellet**.

Ronchi analizza sezioni trasversali post-irraggiamento tramite **replica technique in Transmission Electron Microscopy (TEM)**. La procedura misura la densità numerica e la distribuzione dimensionale delle fission-gas bubbles; da questi dati viene poi calcolato il microscopic swelling in funzione del raggio del pin e della temperatura.

La replica technique usata da Ronchi è limitata approssimativamente a bolle di dimensione:

$$
\boxed{200\ \text{Å}\approx20\ \mathrm{nm}}
$$

fino a circa:

$$
\boxed{1\ \mu\mathrm{m}},
$$

oltre la quale le bolle diventano difficili da distinguere dalla porosità intrinseca di sinterizzazione. Ronchi osserva che per ottenere lo spettro completo fino a circa $20$ Å ($\sim2$ nm) sarebbero necessarie osservazioni dirette di campioni assottigliati in TEM, ma che le bolle più piccole danno un contributo ridotto allo swelling volumetrico.

Ronchi osserva inoltre che, a basso burnup, le fission-gas bubbles con dimensioni inferiori a circa $1\ \mu$m sono relativamente distinguibili dalla sintering porosity, tipicamente maggiore di $1\ \mu$m e fino a circa $10\ \mu$m, localizzata preferenzialmente ai grain boundaries. Ad alto burnup questa distinzione diventa più difficile.

Questi limiti sperimentali sono particolarmente importanti per interpretare la popolazione bulk del modello. Rizk sottolinea esplicitamente che la tecnica di Ronchi **non era sufficientemente raffinata per catturare le piccole bulk bubbles** e che le misure di densità e dimensione sono direttamente confrontabili soprattutto con le large intragranular/dislocation bubbles. Nel testo Rizk usa una soglia di circa 20 nm per la replica microscopy, mentre nelle caption delle sue Fig. 7–8 viene anche indicato che le bolle con raggio inferiore a 10 nm non sono incluse nei dati. Questa differenza di soglia va verificata e riportata con precisione nella stesura finale.

### Open question sperimentale da chiarire

Va controllato con attenzione nel paper originale di Ronchi:

- come vengono identificate/separate le bolle intragranulari dalle bolle o porosità ai grain boundaries;
- quale contributo delle grain-boundary bubbles entra effettivamente nel “microscopic swelling” ricostruito;
- se la classificazione sperimentale permette davvero di separare in modo univoco large intragranular bubbles e grain-face porosity.

Questo punto è utile per capire non soltanto ciò che il modello predice, ma **quale popolazione sia realmente osservabile con la tecnica sperimentale usata come benchmark**.

---

## Commento per la tesi — interpretazione delle large intragranular bubbles come dislocation bubbles

Rizk confronta le misure di large intragranular bubble swelling $\mu_2$ di Ronchi con le tre popolazioni del modello, ma specifica che **solo le dislocation bubbles sono direttamente comparabili alle misure $\mu_2$**. Bulk e intergranular swelling vengono mostrati per informazione, ma non sono trattati come la stessa osservabile sperimentale.

La logica è coerente con il limite sperimentale appena discusso: le bulk bubbles previste dal modello rimangono per la maggior parte delle condizioni sotto la soglia di risoluzione della replica microscopy, mentre la popolazione grande osservata è compatibile dimensionalmente con le dislocation bubbles.

Schneider 2026 verifica ulteriormente questa interpretazione. Poiché Ronchi non specifica direttamente se le bolle osservate siano bulk, dislocation o grain-boundary, Schneider ripete la calibrazione sotto tre ipotesi alternative e poi usa ciascun modello calibrato per predire il **raggio delle bolle**. Il raggio non è il principale target usato per la calibrazione dello swelling e quindi costituisce un controllo microstrutturale aggiuntivo.

A $1\sigma$ la banda delle dislocation bubbles è quella che mostra il miglior accordo con i raggi sperimentali; a $3\sigma$ le bande delle tre ipotesi si sovrappongono e la discriminazione non è più conclusiva. Pertanto l'identificazione P2/large intragranular $\leftrightarrow$ dislocation bubbles è ben supportata ma non matematicamente univoca.

È inoltre visibile in Fig. 10 di Schneider che diversi punti sperimentali, soprattutto alle temperature più alte, si collocano nella parte alta della banda $1\sigma$ delle dislocation bubbles. Questo ricorda la nostra tendenza a ottenere raggi leggermente bassi e concentrazioni di bolle relativamente alte. Non va però scritto come “sottostima sistematica” senza una quantificazione dedicata: i punti restano in larga parte compatibili con la banda di incertezza.

---

## Commento per la tesi — microscopic Ronchi vs integral fuel-pin swelling

È fondamentale non confondere due tipi diversi di benchmark sperimentale.

### Ronchi — microscopic swelling

I dati di Ronchi usati da Rizk/Schneider derivano dalla ricostruzione microstrutturale tramite conteggio e misura delle bolle osservabili. Sono quindi dati di **microscopic swelling**, particolarmente utili per vincolare:

$$
R_d,\qquad N_d,\qquad S_d.
$$

Non sono una misura macroscopica diretta dell'intero volume del pellet.

### SP1 e SNAP50 — integral fuel-pin assessment

I due casi integrali usati da Rizk non sono esperimenti di Ronchi:

- **SNAP50**: S. C. Weaver, J. L. Scott, R. L. Senn, B. H. Montgomery, *Effects of Irradiation on Uranium Nitride under Space-Reactor Conditions*, ORNL-4461 (1969); Rizk seleziona la capsula 57-642;
- **SP1 / SP-100**: R. Matthews, K. Chidester, C. Hoth, R. Mason, R. Petty, *Fabrication and testing of uranium nitride fuel for space power reactors*, *Journal of Nuclear Materials* 151 (1988) 345; Rizk seleziona il pin NBU-3.

In questi casi BISON calcola e somma:

$$
S_{solid}
+
S_{bulk}
+
S_{dislocation}
+
S_{GB},
$$

insieme alla thermal expansion durante l'irraggiamento, e confronta il risultato con le misure full-pin/post-irradiation. Alla fine della simulazione la componente di thermal expansion scompare e il valore residuo viene confrontato con il PIE swelling. Rizk riporta che il fission-gas swelling costituisce il contributo dominante allo swelling finale.

Per SNAP50 la letteratura originale determina il fuel swelling tramite variazione di densità/volume post-irraggiamento, includendo misure di massa e volume con pycnometry a mercurio; si tratta quindi di un benchmark macroscopico molto diverso dalla ricostruzione TEM di Ronchi.

### Conseguenza per l'interpretazione P2 = dislocation bubbles

Il fatto che la stessa struttura di modello che interpreta la popolazione large intragranular di Ronchi come dislocation bubbles riesca anche a fornire un accordo ragionevole con lo swelling integrale di SP1/SNAP50 rende questa interpretazione più credibile.

Non è comunque una dimostrazione univoca: un total swelling corretto può in principio derivare anche da compensazioni tra errori nei contributi bulk, dislocation e grain-boundary. La forza dell'interpretazione deriva quindi dalla **combinazione** di:

- $R_d$, $N_d$ e $S_d$ da Ronchi;
- confronto del raggio in Schneider Fig. 10;
- assessment integrale SP1/SNAP50.

---

## Commento complessivo per la tesi — cosa resta originale rispetto a Schneider 2026

Schneider et al. 2026 svolge già una sensitivity analysis e una Bayesian UQ molto più avanzate di una semplice calibrazione dei parametri. In questo senso esiste una forte sovrapposizione con una parte dell'idea iniziale della tesi: il lavoro ha già mostrato quali parametri della struttura UNSIFGRS fissata sono più importanti e ha già propagato le loro distribuzioni fino allo swelling.

Questo non rende inutile il nostro lavoro, ma suggerisce di spostare chiaramente l'enfasi da:

$$
\text{“quale valore dei parametri fitta meglio?”}
$$

verso:

$$
\boxed{\text{“quanto dipendono i risultati dalle closure e dalle ipotesi strutturali del single-size model?”}}
$$

Le estensioni più interessanti da valutare sono quindi:

1. sensitivity esplicita a $f_n$ della nucleazione omogenea bulk;
2. sensitivity/model-form test della closure $\phi_b=1/(n_b-1)$;
3. confronto tra nucleazione di dimeri e nuclei iniziali con $n_0>2$;
4. sensitivity ai coefficienti di trapping $g_b$ e $g_d$;
5. effetto della distribuzione reale dei raggi rispetto alla single-size approximation;
6. effetto indiretto della nucleazione sul raggio medio e quindi sui rate radius-dependent di trapping e re-solution;
7. verifica se la tendenza a sottostimare leggermente $R_d$ e sovrastimare $N_d$ è legata a queste closure piuttosto che soltanto alla calibrazione dei parametri.

L'obiettivo può essere anche semplicemente dimostrare che alcune di queste ipotesi hanno un effetto trascurabile. Sapere **esplicitamente** che una closure non influenza i risultati nel dominio di interesse è comunque un risultato utile e difendibile per la tesi.

### References principali di questo blocco

- A. J. Schneider et al., **“Integrating mechanistic models with separate effects and integral tests through neural network-enabled Bayesian inference: Application to uranium nitride fuel swelling,”** *Journal of Nuclear Materials* 632 (2026) 156895. DOI: 10.1016/j.jnucmat.2026.156895.
- J. T. Rizk et al., **“Mechanistic nuclear fuel performance modeling of uranium nitride,”** *Journal of Nuclear Materials* 606 (2025) 155604. DOI: 10.1016/j.jnucmat.2024.155604.
- C. Ronchi, I. L. F. Ray, H. Thiele, J. Van De Laar, **“Swelling analysis of highly-rated MX-type LMFBR fuels: II. Microscopic swelling behaviour,”** *Journal of Nuclear Materials* 74 (1978) 193–211.
- S. C. Weaver, J. L. Scott, R. L. Senn, B. H. Montgomery, **“Effects of Irradiation on Uranium Nitride under Space-Reactor Conditions,”** ORNL-4461, 1969.
- R. Matthews, K. Chidester, C. Hoth, R. Mason, R. Petty, **“Fabrication and testing of uranium nitride fuel for space power reactors,”** *Journal of Nuclear Materials* 151 (1988) 345. DOI: 10.1016/0022-3115(88)90029-3.


---

## Nota sulla diffusività dello Xe in Schneider et al. (2026)

Schneider et al. (2026) confrontano la **pushforward posterior** della diffusività dello Xe con diversi dati sperimentali disponibili in letteratura. Tuttavia, tali dati **non vengono utilizzati direttamente nella calibrazione bayesiana**.

La ragione è che, per molti degli esperimenti storici, non sono riportate con sufficiente dettaglio alcune condizioni necessarie per riprodurre correttamente la diffusività con il modello lower-length-scale. Gli autori citano esplicitamente, come esempio, la **pressione parziale di azoto**:

$$
p_{N_2}
$$

che non è nota per diversi dataset sperimentali. Inoltre, non sono sempre sufficientemente documentate le condizioni specifiche di irraggiamento e del campione.

Di conseguenza, Schneider et al. non interpretano la curva di Fig. 7 come una correlazione universale:

$$
D_{Xe}=D_{Xe}(T)
$$

valida indipendentemente dalle condizioni del combustibile. La diffusività dello Xe dipende infatti anche dallo stato difettuale e chimico del materiale, che a sua volta dipende da parametri quali:

$$
T,\qquad p_{N_2},\qquad \dot F,\qquad \text{chemistry / stoichiometry},
$$

oltre che dai parametri atomistici che controllano la formazione e la mobilità dei difetti e dei cluster contenenti Xe.

Per questo motivo, la pushforward posterior mostrata in Fig. 7 viene calcolata assumendo condizioni simili a quelle del campione **ANP6**, e va interpretata come una **predizione condizionata a quel particolare regime**, non come una nuova legge generale della diffusività dello Xe in UN.

Un altro risultato importante della figura è la notevole ampiezza delle bande di incertezza, soprattutto alle temperature più elevate. La larghezza delle bande non rappresenta direttamente l'errore sperimentale, ma l'incertezza predittiva ottenuta propagando attraverso il modello le distribuzioni posteriori dei parametri:

$$
p(\boldsymbol{\theta}\mid D)
\longrightarrow
p(D_{Xe}\mid D).
$$

L'aumento dell'ampiezza delle bande ad alta temperatura indica quindi che, in quel regime, la diffusività dello Xe rimane fortemente sensibile all'incertezza dei parametri lower-length-scale e dei meccanismi diffusivi dominanti.

### Implicazione per il presente lavoro

Per questo motivo, la pushforward posterior di Schneider et al. non viene adottata direttamente come nuova correlazione di diffusività nel modello corrente.

Viene invece utilizzata come riferimento per evidenziare:

- la forte dipendenza della diffusività dello Xe dalle condizioni fisiche e chimiche del combustibile;
- l'incertezza ancora significativa della diffusività, soprattutto ad alta temperatura;
- la necessità di interpretare con cautela le correlazioni semplici del tipo:

$$
D_{Xe}=D_{Xe}(T).
$$

I dati sperimentali riportati nella stessa Fig. 7 possono invece essere utilizzati come confronto indipendente con la diffusività adottata nel presente modello, mantenendo esplicitamente la cautela dovuta alle condizioni sperimentali non completamente note.

**Reference:** A. J. Schneider et al., *Integrating mechanistic models with separate effects and integral tests through neural network-enabled Bayesian inference: Application to uranium nitride fuel swelling*, Journal of Nuclear Materials 632 (2026) 156895.
---

## Commento per la tesi — andamento della dislocation density con burnup/dose e temperatura

La densità di dislocazioni non dovrebbe essere considerata necessariamente costante durante l'irraggiamento. Le evidenze disponibili, pur provenendo da materiali e condizioni diverse, suggeriscono qualitativamente una dipendenza sia dalla dose/burnup sia dalla temperatura.

### Evidenze sperimentali con il burnup

Nel report **Rizk et al. (2023), LA-UR-23-29157**, la Fig. 4.12 confronta l'evoluzione della dislocation line density calcolata con dati sperimentali di **UC/mixed carbide** e **UO$_2$** a circa 1025 K. I dati UC provengono da **Ray & Blank (1984)**, ottenuti mediante TEM su combustibile mixed carbide $U_{0.8}Pu_{0.2}C$ irradiato fino a circa 11% FIMA. Rizk osserva che, a temperatura fissata, tali dati mostrano una relazione approssimativamente lineare tra densità di dislocazioni e burnup. Nella stessa figura i dati UO$_2$ di **Nogita & Une (1994)** mostrano invece una crescita leggermente più curvata/esponenziale.

Per i dati UC è inoltre evidente una fase iniziale di incubazione: Rizk mostra che traslando la previsione di circa 3% FIMA verso burnup maggiori si ottiene una migliore corrispondenza con i punti sperimentali. Il punto importante per il presente lavoro non è il valore numerico della traslazione, ma il fatto che le misure sperimentali supportino qualitativamente:

$$
\boxed{\rho_d \uparrow \ \text{con il burnup/dose}}
$$

almeno nel regime osservato, con un andamento quasi lineare nei dati UC.

Rizk (2023) aveva inizialmente costruito anche una correlazione empirica lineare $\rho_d(T,F)$ sulla base dei dati carbide, ma successivamente adottò una formulazione più complessa perché forniva risultati migliori per lo swelling. Il report sottolinea quindi già che la base sperimentale per $\rho_d(T,F)$ è limitata e che una descrizione veramente meccanicistica dell'evoluzione dislocazionale sarebbe preferibile.

### Evidenze recenti su UN

**Schneider et al. (2026)** introduce una forma semplificata dipendente dal burnup,

$$
\rho_d=\rho_d^0+\beta\,\rho_d^\beta,
$$

e mostra che la densità di dislocazioni è uno dei parametri più importanti per lo swelling delle dislocation bubbles. Gli autori sottolineano però che non esistono misure dirette sufficienti della dislocation line density negli specifici campioni UN usati per la calibrazione e indicano esplicitamente la necessità di un modello più robusto della sua evoluzione.

Le osservazioni sperimentali di **Kosmidou et al. (2025)** su UN irradiato *in situ* con Kr tra 700 e 1100 °C mostrano inoltre che la loop density diminuisce all'aumentare della temperatura. Con la dose, la densità dei loop cresce inizialmente fino a un massimo e poi diminuisce quando iniziano a formarsi segmenti e reti di dislocazioni. Questi dati non sono direttamente convertibili nel burnup da fissione e misurano una loop number density, non direttamente la line density $\rho_d$ usata nel modello, ma supportano qualitativamente un'evoluzione del tipo:

$$
\boxed{
\text{dose/burnup}\uparrow:
\ \rho_d \text{ cresce inizialmente e può poi saturare/evolvere}
}
$$

e

$$
\boxed{
T\uparrow:
\ \text{a temperatura elevata la densità numerica dei loop tende a diminuire}
}
$$

Questo quadro è coerente anche con **Charatsidou et al. (2024)**, che osserva in UN proton-irradiato un aumento della densità di loop con la dose e una tendenza alla saturazione alle dosi più alte.

### Implicazione per il modello corrente

L'assunzione

$$
\rho_d=\mathrm{costante}
$$

resta quindi utile come baseline, ma è una semplificazione. Per una futura estensione del modello è fisicamente più plausibile una funzione:

$$
\boxed{\rho_d=\rho_d(T,B)}
$$

capace di descrivere almeno:

- crescita iniziale con burnup/dose;
- eventuale saturazione/evoluzione della rete dislocazionale;
- riduzione della densità di loop alle temperature elevate.

I dati UC di Ray & Blank sono particolarmente utili come supporto qualitativo della dipendenza quasi lineare con burnup, ma non devono essere trattati come una calibrazione quantitativa diretta per UN.

### References

- J. T. Rizk, A. J. Schneider, M. W. D. Cooper, D. A. Andersson, C. Matthews, **“Development of Mechanistic Fission Gas Release and Swelling Models for UN Fuels in BISON,”** Los Alamos National Laboratory, LA-UR-23-29157, 2023. In particolare Fig. 4.12 e la discussione sulla dislocation-density evolution.
- I. L. F. Ray, H. Blank, **“Microstructure and fission gas bubbles in irradiated mixed carbide fuels at 2 to 11 a/o burnup,”** *Journal of Nuclear Materials* 124 (1984) 159–174. DOI: 10.1016/0022-3115(84)90020-5.
- K. Nogita, K. Une, **“Radiation-induced microstructural change in high burnup UO$_2$ fuel pellets,”** *Nuclear Instruments and Methods in Physics Research Section B* 91 (1994) 301–306.
- A. J. Schneider et al., **“Integrating mechanistic models with separate effects and integral tests through neural network-enabled Bayesian inference: Application to uranium nitride fuel swelling,”** *Journal of Nuclear Materials* 632 (2026) 156895. DOI: 10.1016/j.jnucmat.2026.156895.
- M. Kosmidou et al., **“Temperature dependency of dislocation evolution in Kr-irradiated Uranium mononitride (UN) through in-situ TEM observation and modeling,”** *Acta Materialia* 288 (2025) 120824. DOI: 10.1016/j.actamat.2025.120824.
- E. Charatsidou et al., **“Proton irradiation-induced cracking and microstructural defects in UN and (U,Zr)N composite fuels,”** *Journal of Materiomics* 10(4) (2024) 906–918. DOI: 10.1016/j.jmat.2024.01.014.
---

## Commento per la tesi — uranium self-diffusion dominata dalle uranium vacancies

Una nuova conferma indipendente viene da **Antropov et al. (2026)**, che analizzano la self-diffusion di U e N in UN mediante DFT e un machine-learning interatomic potential di tipo SNAP. Sulla base della dipendenza sperimentale della uranium self-diffusion dalla pressione parziale di azoto, gli autori concludono che, nelle condizioni considerate, la self-diffusion dell'uranio avviene principalmente tramite **uranium vacancies**.

Questo è coerente con **Rizk et al. (2025)**, che assume la diffusione dei difetti di U come rate-limiting rispetto ai più rapidi difetti di N e afferma che, tra i difetti dell'uranio, la vacancy $V_U$ costituisce il meccanismo di diffusione intrinseca dominante per la maggior parte delle condizioni chimiche, con eccezione delle condizioni fortemente U-rich. Per questo Rizk usa la diffusività delle uranium vacancies come contributo dominante nel calcolo della crescita delle bolle.

### Attenzione: Antropov non fornisce una nuova diffusività quantitativamente più affidabile

Il risultato qualitativo sul **meccanismo dominante** è robusto, ma il valore assoluto della diffusività rimane incerto. Antropov et al. ottengono infatti una uranium self-diffusivity circa **due ordini di grandezza più alta** dei dati sperimentali disponibili.

Questo non significa semplicemente che abbiano "sbagliato i calcoli". Il paper mostra piuttosto una forte **model-form / parameter uncertainty** nella descrizione atomistica della diffusione. Gli autori identificano come possibili cause soprattutto:

- migration energy della uranium vacancy non sufficientemente accurata;
- prefattore diffusivo;
- trattamento dello stato magnetico durante la migrazione del difetto;
- sensibilità delle free energies dei difetti alla formulazione termodinamica adottata.

Un punto particolarmente importante è il confronto con **Cooper et al. (2023)**. La formulazione originale di Cooper risulta abbastanza vicina ai dati sperimentali di uranium self-diffusion; tuttavia Antropov sostiene che la free-energy expression di Cooper dovrebbe essere corretta includendo esplicitamente la degenerazione configurazionale dei difetti. Applicando questa correzione, anche la previsione di Cooper si allontana sensibilmente dagli esperimenti.

Quindi la lettura corretta è:

$$
\boxed{\text{meccanismo: } U\text{-vacancy dominated}}
$$

ma

$$
\boxed{\text{magnitudine assoluta di }D_U\text{ / }D_{V_U}\text{ ancora incerta}}
$$

Il paper di Antropov non giustifica quindi la sostituzione diretta della nostra $D_v$ con la loro correlazione. È invece utile come ulteriore supporto alla scelta fisica del meccanismo vacancy-dominated e come evidenza che la diffusività vacancy in UN resta una delle quantità lower-length-scale più incerte.

### References

- A. Antropov, E. Lobashev, V. Stegailov, **“Deciphering the physics of point defects in uranium mononitride with a machine-learning interatomic potential,”** *Journal of Nuclear Materials* 633 (2026) 157009. DOI: 10.1016/j.jnucmat.2026.157009.
- J. T. Rizk, M. W. D. Cooper, P.-C. A. Simon, A. J. Schneider, D. A. Andersson, S. R. Novascone, C. Matthews, **“Mechanistic nuclear fuel performance modeling of uranium nitride,”** *Journal of Nuclear Materials* 606 (2025) 155604. DOI: 10.1016/j.jnucmat.2024.155604.
- M. W. D. Cooper, J. Rizk, C. Matthews, V. Kocevski, G. T. Craven, T. Gibson, D. A. Andersson, **“Simulations of self- and Xe diffusivity in uranium mononitride including chemistry and irradiation effects,”** *Journal of Nuclear Materials* 587 (2023) 154685. DOI: 10.1016/j.jnucmat.2023.154685.

---

## Commento per la tesi — incertezza sulla surface energy $\gamma$ e sensitivity dedicata

**AbdulHameed et al. (2026)** calcolano per UN privo di ossigeno una surface energy di circa:

$$
\gamma_{\mathrm{clean}} = 1.59\ \mathrm{J\,m^{-2}},
$$

maggiore del valore costante adottato da **Rizk et al. (2025)**:

$$
\gamma_{\mathrm{Rizk}} = 1.11\ \mathrm{J\,m^{-2}}.
$$

Il lavoro mostra inoltre che la segregazione di ossigeno può ridurre la surface energy efficace in modo dipendente da temperatura e raggio della cavità, con effetto più forte per bolle piccole. Tuttavia, l'analisi quantitativa è estesa solo fino a circa $R=50$ nm e non fornisce una correlazione direttamente applicabile alle dislocation bubbles più grandi ($\sim100$–$300$ nm) osservate/predette alle alte temperature.

Per questo il lavoro non viene usato per imporre una legge $\gamma(R,T)$ nel modello corrente, ma costituisce una motivazione fisica per trattare $\gamma$ come parametro incerto ed includerlo in una sensitivity analysis.

**Reference:** M. AbdulHameed et al., *Journal of Nuclear Materials* 633 (2026) 156983.

---

## Integrazioni finali da bibliografia — note utili per la stesura della tesi

### Barani et al. (2019) — homogeneous vs heterogeneous re-solution e limite single-size

Il lavoro su $U_3Si_2$ va **mantenuto come riferimento metodologico**, anche se non riguarda quantitativamente UN. È particolarmente utile perché distingue esplicitamente due rappresentazioni della re-solution intragranulare:

- **homogeneous re-solution**, in cui il gas viene espulso dalle bolle come processo distribuito nella popolazione;
- **heterogeneous re-solution**, in cui l'interazione del fission fragment con la bolla viene trattata come evento localizzato e può portare alla distruzione completa della bolla interessata.

Il paper deriva inoltre un modello single-size a partire da una descrizione di cluster dynamics e sottolinea due aspetti direttamente collegati alle nostre sensitivity di model form:

1. la nucleazione viene trattata coerentemente come **formazione di dimeri**, evitando di far nascere artificialmente le nuove bolle già alla dimensione media della popolazione;
2. nell'equazione della bubble number density compare un termine di **bubble destruction** associato alla re-solution.

Quindi Barani et al. è utile per motivare nella tesi il confronto tra diverse closure di re-solution/nucleazione e per spiegare perché il single-size model contiene assunzioni strutturali non equivalenti alla descrizione completa della distribuzione di cluster.

**Uso corretto:** riferimento teorico/model-form; **non** sorgente di parametri quantitativi per UN.

**Reference:** T. Barani et al., *Multiscale modeling of fission gas behavior in $U_3Si_2$ under LWR conditions*, 2019.

---

### Schneider et al. (2024) — sorgente diretta della diffusività atermica $D_{3,Xe}$

Il paper **Radiation induced athermal diffusivity in uranium mononitride** costituisce la sorgente diretta del contributo atermico usato nel modello UN.

Per una fission-rate density:

$$
\dot F=5\times10^{18}\ \mathrm{m^{-3}\,s^{-1}},
$$

gli autori calcolano:

$$
D_{3,Xe}=9.27\times10^{-21}\ \mathrm{m^2\,s^{-1}}.
$$

Poiché $D_3$ è lineare con la fission-rate density, questo corrisponde a:

$$
\boxed{
D_{3,Xe}\simeq1.85\times10^{-39}\dot F
}
$$

con $D_{3,Xe}$ in $\mathrm{m^2\,s^{-1}}$ e $\dot F$ in $\mathrm{m^{-3}\,s^{-1}}$.

Questo paper va quindi citato direttamente quando nella tesi viene introdotto il termine di **radiation-induced ballistic mixing / athermal diffusion**. È particolarmente importante nel regime a bassa temperatura, dove questo contributo può dominare la mobilità efficace dello Xe.

**Reference:** A. Schneider, J. Rizk, M. Kosmidou, C. Matthews, D. A. Andersson, M. W. D. Cooper, **“Radiation induced athermal diffusivity in uranium mononitride,”** *Journal of Nuclear Materials* 601 (2024) 155313.

---

### Tanaka et al. (2004) — dato sperimentale di FGR e partizione dello Xe in $(U,Pu)N$

Questo lavoro va mantenuto perché fornisce una **vera misura sperimentale**, ottenuta mediante PIE su mixed nitride irradiato nel reattore JOYO fino a circa $4.3\%$ FIMA.

Gli autori riportano:

$$
\mathrm{FGR}\approx3.3\%-5.2\%,
$$

e swelling rates di circa:

$$
1.6-1.8\%/\%\mathrm{FIMA}.
$$

Dalle distribuzioni radiali di Xe misurate mediante EPMA stimano inoltre che circa:

$$
\boxed{80\%}
$$

del gas rimanga nella regione intragranulare e circa:

$$
\boxed{15\%}
$$

sia associato alle fission-gas bubbles.

Questo è utile perché costituisce un riferimento sperimentale indipendente sulla **gas partition**, a differenza della Fig. 9 di Rizk che è un output di modello.

**Limite importante:** il combustibile è $(U,Pu)N$, non UN puro; i numeri non devono quindi essere imposti come target diretto della calibrazione del nostro modello, ma possono essere utilizzati come confronto qualitativo/integrale.

**Reference:** K. Tanaka et al., 2004, irradiation behaviour / fission gas release and swelling of $(U,Pu)N$ fuel in JOYO.

---

### Qian et al. (2021) — distribuzione delle bubble sizes e ruolo della re-solution

Il lavoro di Qian et al. è utile soprattutto per la parte di **model-form uncertainty**. A differenza del nostro single-size model, usa una kinetic rate theory che segue esplicitamente una distribuzione di bubble sizes.

Gli autori osservano una distribuzione **bimodale** delle bolle e identificano la **gas-bubble re-solution** come il principale meccanismo che genera tale bimodalità nel loro modello. Eseguono inoltre una sensitivity esplicita rispetto a nucleation factor e re-solution coefficient.

Nel loro caso vengono adottati valori dell'ordine di:

$$
F_N\sim10^{-4},
\qquad
b_0\sim10^{-18}\ \mathrm{cm^3},
$$

ma tali valori **non devono essere trasferiti direttamente** nel nostro modello: la struttura delle equazioni, il dataset e la parametrizzazione sono differenti.

La vera utilità per la tesi è mostrare che una distribuzione reale può sviluppare più popolazioni dimensionali e che:

$$
\boxed{\text{single-size approximation + re-solution closure}}
$$

costituiscono una sorgente concreta di model-form uncertainty.

**Reference:** Qian et al., 2021, kinetic rate-theory modelling of fission-gas behaviour and swelling in nitride fuel.

---

### Wallenius (2022) — possibile effetto dell'ossigeno sulla migrazione del gas

Wallenius rianalizza dati storici di fission gas release in nitride fuels mediante una correlazione semi-empirica basata su una barriera efficace di migrazione del gas.

Dall'analisi dei dati propone che una concentrazione dell'ordine di:

$$
1000\ \mathrm{ppm\ O}
$$

possa essere associata a una riduzione della barriera efficace per la migrazione dei gas di fissione di circa:

$$
\boxed{7\pm2\%}.
$$

Questo risultato non fornisce una nuova legge fondamentale per $D_{Xe}$ e non deve essere usato per sostituire direttamente la diffusività del nostro modello. È però un supporto indipendente all'idea che la **chemistry, e in particolare l'ossigeno, possa influenzare anche il trasporto del gas**, oltre agli effetti sulla defect chemistry e sulla surface energy discussi da Matthews/Watson/AbdulHameed.

**Uso corretto nella tesi:** evidenza semi-empirica / indicazione qualitativa, non parametro diretto di calibrazione.

**Reference:** J. Wallenius, 2022, correlation/review of fission gas release in nitride fuels.

---

### AbdulHameed et al. (2024) — cautela nell'uso dei potenziali interatomici UN

Questo lavoro confronta il potenziale ADP di Tseplyaev-Starikov con il potenziale EAM di Kocevski per diverse proprietà di UN e delle altre fasi U-N.

Il risultato importante per il nostro lavoro è che **nessuno dei due potenziali è universalmente affidabile**:

- Kocevski descrive meglio diverse proprietà strutturali, thermal expansion e stabilità di fase;
- Tseplyaev descrive meglio diversi aspetti energetici e le point-defect formation energies;
- il potenziale Kocevski non stabilizza correttamente $\alpha$-U ed è quindi indicato dagli autori come non adatto al calcolo delle formation energies di point defects non-stechiometrici.

Questo costituisce un ulteriore caveat quando parametri lower-length-scale derivati da un singolo interatomic potential vengono trasferiti nel fuel-performance model. In particolare, rafforza la scelta di trattare quantità come $\gamma$ e le defect energetics come soggette a incertezza, invece di considerarle valori esatti.

**Attenzione:** questo paper non calcola direttamente la surface energy usata nel nostro modello; non deve quindi essere presentato come una misura alternativa di $\gamma$.

**Reference:** M. AbdulHameed, B. Beeler, C. O. T. Galvin, M. W. D. Cooper, **“Assessment of uranium nitride interatomic potentials,”** *Journal of Nuclear Materials* 600 (2024) 155247.

---

### Grachev et al. (2020) — dataset sperimentale FGR su mixed nitride

Il lavoro riporta dati di fission gas release da combustibile mixed uranium-plutonium nitride irradiato in BN-600 fino a circa:

$$
7.5\%\ \text{heavy-atom burnup}.
$$

Gli autori osservano che il release inizia a diventare significativo sopra circa:

$$
2\%\ \text{burnup}
$$

e cresce con il burnup. Un FGR più elevato è associato a temperature del combustibile maggiori di circa $1500\ ^\circ\mathrm{C}$ oppure a particolari caratteristiche microstrutturali.

È quindi un possibile **benchmark integrale esterno** per trend di FGR ad alto burnup/alta temperatura, ma non un target diretto per il nostro modello UN puro.

**Reference:** A. F. Grachev et al., **“Fission gas release from irradiated uranium-plutonium nitride fuel,”** *Atomic Energy* 129 (2020).

---

### Carvajal-Núñez et al. (2014) — melting point di UN e confronto con $UO_2$

Questo paper va mantenuto come riferimento per l'introduzione e per il confronto generale UN/$UO_2$.

Gli autori misurano, in atmosfera di azoto a:

$$
p_{N_2}=0.25\ \mathrm{MPa},
$$

un melting point congruente di UN pari a:

$$
\boxed{T_m(UN)=3120\pm30\ \mathrm{K}}
$$

ossia circa:

$$
2847\pm30\ ^\circ\mathrm{C}.
$$

Nel paper viene richiamato per $UO_2$ un valore di circa:

$$
T_m(UO_2)=3130\pm20\ \mathrm{K}.
$$

Quindi, sulla base di questi valori, **UN e $UO_2$ hanno melting points sostanzialmente comparabili entro le incertezze**; non è corretto usare questa fonte per affermare che UN abbia un melting point nettamente superiore a $UO_2$.

Per $(U_{0.8}Pu_{0.2})N$ gli autori misurano invece:

$$
T_m=3045\pm25\ \mathrm{K}.
$$

**Reference:** R. Carvajal-Núñez et al., 2014, high-temperature melting behaviour of UN and mixed uranium-plutonium nitrides.

---

### El Jamal et al. (2023) — aqueous stability di UN e rilevanza per LWR/safety

Questo paper è utile per la parte introduttiva sui **limiti di UN in sistemi raffreddati ad acqua** e sulla stabilità chimica in condizioni incidentali.

La letteratura discussa nel lavoro indica che UN presenta **reattività molto bassa con acqua o vapore sotto circa 250 °C**, mentre a temperatura maggiore la reazione accelera. Per condizioni fino a circa 350 °C sono riportate reazioni di idrolisi del tipo:

$$
3UN+2H_2O\rightarrow UO_2+U_2N_3+2H_2,
$$

oppure:

$$
UN+2H_2O\rightarrow UO_2+NH_3+\frac{1}{2}H_2.
$$

La bassa reattività a temperatura relativamente bassa viene attribuita alla formazione di uno strato superficiale protettivo, con $UO_2$ all'esterno e $U_2N_3$ all'interfaccia.

Una frase utile per la tesi è quindi:

> **Sotto circa 250–350 °C la reattività di UN con acqua/vapore è molto bassa grazie alla formazione di uno strato superficiale protettivo di ossido/nitruro, ma non è nulla; a temperature maggiori la reazione accelera.**

Il lavoro mostra inoltre che, in ambiente acquoso irradiato, la radiolisi dell'acqua introduce un secondo meccanismo importante. In particolare $H_2O_2$ è un forte ossidante e gli esperimenti mostrano che:

$$
\boxed{\text{oxidative dissolution indotta da }H_2O_2
>\text{ semplice hydrolysis}}
$$

nelle condizioni studiate.

Questa è una limitazione importante da citare parlando dell'impiego di UN in LWR: la compatibilità con acqua/steam non è equivalente a quella di $UO_2$, soprattutto se si considera un contatto diretto in condizioni off-normal o accidentali.

**Attenzione:** gli esperimenti di El Jamal sono orientati principalmente alla stabilità acquosa / spent-fuel repository; non rappresentano un test integrale di LOCA o di severe accident. Il paper va usato per documentare la **chimica di degradazione**, non per quantificare direttamente un accident transient.

**Reference:** S. El Jamal, Y. Mishchenko, M. Jonsson, **Journal of Nuclear Materials** 578 (2023) 154334.

---

### Galvin et al. (2016) — pipe diffusion lungo le dislocazioni

Galvin et al. studiano mediante molecular dynamics la diffusione di He in $UO_2$ in presenza di dislocazioni e grain boundaries, nel range 2300–3000 K.

Il risultato più utile per il nostro modello è che la diffusività aumenta in una regione estesa circa:

$$
\boxed{20\ \text{Å}=2\ \mathrm{nm}}
$$

attorno al core delle dislocazioni o al grain boundary.

A 2300 K l'incremento può arrivare fino a circa **due ordini di grandezza** rispetto al bulk. Inoltre, per alcune edge dislocations, la diffusione è anisotropa lungo la linea di dislocazione; per la $\{100\}\langle110\rangle$ edge dislocation la diffusività lungo la linea è circa due volte quella nel piano perpendicolare.

Questa osservazione supporta direttamente il concetto di **pipe diffusion**:

> **Dislocations can act as fast diffusion pathways, enhancing gas transport along their cores.**

Per la nostra tesi questo riferimento è utile quando si motiva fisicamente:

- il trapping/capture del gas verso le dislocazioni;
- la possibilità che la regione del core abbia proprietà di trasporto differenti dal bulk;
- una futura estensione in cui il trasporto associato alle dislocazioni non sia rappresentato soltanto come sink locale, ma anche come percorso rapido lungo la linea.

**Limite fondamentale:** Galvin studia **He in $UO_2$**, non Xe in UN. Il risultato deve quindi essere utilizzato come evidenza del **meccanismo fisico generale**, non per assegnare quantitativamente un coefficiente di diffusione o un capture rate nel nostro modello.

**Reference:** C. O. T. Galvin, M. W. D. Cooper, P. C. M. Fossati, C. R. Stanek, R. W. Grimes, D. A. Andersson, **“Pipe and grain boundary diffusion of He in $UO_2$,”** *Journal of Physics: Condensed Matter* 28 (2016) 405002.

---

## Verification e validation del modello UN in SCIANTIX

### Oberkampf & Trucano — distinzione metodologica da adottare nella tesi

Oberkampf & Trucano forniscono la distinzione metodologica da usare esplicitamente:

$$
\boxed{\text{Verification: il modello matematico è risolto correttamente dal codice?}}
$$

$$
\boxed{\text{Validation: il modello rappresenta correttamente la realtà fisica?}}
$$

La verification riguarda quindi l'identificazione, quantificazione e riduzione degli errori nella **soluzione numerica e nell'implementazione**. La validation confronta invece i risultati computazionali con dati sperimentali, idealmente considerando anche errori e incertezze di entrambi.

Di conseguenza:

- il confronto con **Ronchi/P2** è validation/separate-effects assessment;
- il confronto con **SNAP-50 / SP1 / altri pin irradiati** è integral validation;
- il confronto con una vecchia curva di Rizk è soprattutto regression/implementation benchmarking;
- la **verification formale** richiede invece soluzioni esatte/manufactured o test analitici controllati.

**Reference:** W. L. Oberkampf, T. G. Trucano, 2002, verification and validation methodology for computational models.

---

### Strategia concreta di verification per la nostra implementazione SCIANTIX

La verification deve essere costruita sul **vero eseguibile SCIANTIX**, non sul notebook Python usato per la calibrazione. Gli input di SCIANTIX possono essere utilizzati per creare limit cases e regression tests, ma per la verifica numerica più rigorosa conviene seguire il metodo già presente nel repository SCIANTIX: **Method of Manufactured Solutions (MMS)**.

Nel repository esiste già:

```text
utilities/MMS_verification/
```

con test MMS per `Integrator`, `Decay`, `NewtonBlackburn` e per il spectral diffusion algorithm. Questo permette di adottare la stessa metodologia per la parte introdotta dal modello UN.

#### MMS del solver a tre popolazioni

Il target naturale è il solver usato per le tre popolazioni intragranulari, cioè il sistema di exchange tra:

$$
c,\qquad m_b,\qquad m_d.
$$

Si può costruire un nuovo test del tipo:

```text
mms_SpectralDiffusion3equationsExchange.py
```

scegliendo soluzioni manufactured note, ad esempio:

$$
c(t)=1+t^2,
\qquad
m_b(t)=2+t,
\qquad
m_d(t)=3+2t,
$$

quindi:

$$
\dot c=2t,
\qquad
\dot m_b=1,
\qquad
\dot m_d=2.
$$

I source terms vengono costruiti per sostituzione nelle stesse equazioni discretizzate utilizzate dal solver. Eseguendo poi il test con:

$$
\Delta t,\quad\Delta t/2,\quad\Delta t/4,\quad\Delta t/8,
$$

si misura l'errore rispetto alla soluzione manufactured e si ricava l'ordine di convergenza:

$$
p=\log_2\left(\frac{E_{\Delta t}}{E_{\Delta t/2}}\right).
$$

Per uno schema temporale del primo ordine ci si aspetta:

$$
\boxed{p\approx1}.
$$

Questo costituisce **code/numerical verification**, indipendente dal fatto che i parametri fisici siano corretti.

#### Test analitici e di conservazione complementari

L'MMS non sostituisce test più semplici ma molto utili per l'implementazione UN. Vanno mantenuti almeno:

1. **Xe mass conservation**

$$
N_{Xe}^{produced}
=
N_{matrix}+N_{bulk}+N_{dislocation}+N_{GB}+N_{released}
$$

entro l'errore numerico.

2. **Nucleation mass coupling**

Se la nucleazione crea dimeri:

$$
\left(\frac{dc}{dt}\right)_{\nu}=-2\nu_b,
\qquad
\left(\frac{dm_b}{dt}\right)_{\nu}=+2\nu_b,
$$

la nucleazione deve ridistribuire il gas senza crearne/distruggerne.

3. **Coalescence analytical update**

Per la closure implementata:

$$
N_d^{new}
=
\frac{N_d}{1+4\lambda N_d\Delta V_d^{+}},
$$

un caso a singolo timestep con input controllati può essere confrontato direttamente con il risultato analitico.

4. **Time-step / numerical convergence**

I risultati principali devono essere controllati rispetto alla riduzione del timestep / incremento del numero di step, separando l'errore numerico dalle differenze dovute alla fisica del modello.

### Nota sulla dislocation density

Nel modello corrente la baseline scelta per la tesi resta:

$$
\boxed{\rho_d=\text{costante}}
$$

quindi **non è necessario introdurre adesso un verification test della vecchia legge dinamica $\rho_d(T,F)$**. Tale estensione resta eventualmente un problema futuro e non va confusa con la verification del modello attualmente adottato.

### Distinzione finale da mantenere nella tesi

$$
\boxed{
\text{MMS / analytical tests}
\rightarrow
\text{verification}
}
$$

$$
\boxed{
\text{Ronchi / P2}
\rightarrow
\text{separate-effects validation}
}
$$

$$
\boxed{
\text{SNAP-50 / SP1 / integral irradiation cases}
\rightarrow
\text{integral validation}
}
$$

Questa separazione evita di chiamare "verification" un semplice fit ai dati sperimentali o una regression contro una precedente implementazione.



---

## Commento per la tesi — relazione tra swelling, raggio medio e concentrazione delle bolle P2

Nel confronto con i dati microstrutturali P2 è importante ricordare che **swelling, raggio medio e concentrazione numerica non costituiscono necessariamente tre osservabili indipendenti e perfettamente autoconsistenti quando la popolazione reale presenta una distribuzione di dimensioni**.

Per una popolazione reale di bolle con raggi differenti $R_i$, lo swelling volumetrico associato è proporzionale alla somma dei volumi di tutte le bolle:

$$
S_d
=
\sum_i \frac{4}{3}\pi R_i^3
$$

per unità di volume di combustibile. Introducendo la concentrazione numerica totale $N_d$ e la media del terzo momento della distribuzione, la stessa relazione può essere scritta come:

$$
\boxed{
S_d
=
N_d\frac{4}{3}\pi \langle R^3\rangle
}
$$

dove:

$$
\langle R^3\rangle
=
\frac{1}{n}\sum_i R_i^3.
$$

Se invece si dispone soltanto di un raggio medio riportato sperimentalmente, $\bar R$, e si ricostruisce lo swelling tramite:

$$
S_d^{rec}
=
N_d\frac{4}{3}\pi \bar R^3,
$$

si sta implicitamente assumendo:

$$
\bar R^3
=
\langle R^3\rangle.
$$

Questa uguaglianza in generale **non è valida**. Per una distribuzione non monodispersa:

$$
\boxed{
\langle R^3\rangle
\neq
\langle R\rangle^3
}
$$

e, per distribuzioni di raggio sufficientemente larghe, il contributo volumetrico delle bolle grandi pesa molto di più perché il volume scala con $R^3$.

Questo punto è particolarmente rilevante per i dati di **Ronchi et al. (1978)**, perché le osservazioni sperimentali mostrano distribuzioni di bubble size ampie, soprattutto alle temperature elevate. Il microscopic swelling viene ricostruito a partire dalla distribuzione dimensionale e dalla densità numerica delle bolle osservabili mediante replica TEM, mentre il modello corrente è un **single-size model**, cioè rappresenta ciascuna popolazione con un unico raggio efficace.

Di conseguenza, una discrepanza tra:

$$
S_d^{exp}
$$

e il valore ricostruito semplicemente come:

$$
N_d^{exp}\frac{4}{3}\pi \left(R_d^{exp}\right)^3
$$

non deve essere interpretata automaticamente come incoerenza dei dati sperimentali. Può derivare dal fatto che lo swelling sperimentale dipende dal **terzo momento della distribuzione reale dei raggi**, mentre il valore $R_d^{exp}$ riportato nei grafici è un raggio medio/rappresentativo. La definizione statistica esatta del raggio medio riportato nelle sorgenti originali va comunque verificata prima della stesura finale.

### Implicazione per la calibrazione del modello single-size

Per il modello corrente, il target sperimentale principale resta lo **swelling P2/dislocation**:

$$
\boxed{S_d(T)}
$$

perché integra volumetricamente l'effetto dell'intera popolazione osservabile ed è anche la grandezza utilizzata principalmente nelle attività di calibrazione/validazione UN più recenti.

Come secondo target microstrutturale conviene privilegiare il **raggio medio/rappresentativo delle bolle P2**:

$$
\boxed{R_d(T)}
$$

poiché permette di verificare che la singola taglia rappresentativa del modello sia compatibile con la scala dimensionale delle large intragranular bubbles osservate sperimentalmente.

La concentrazione numerica:

$$
N_d(T)
$$

rimane un confronto importante, soprattutto per verificare il trend con la temperatura e l'effetto della coalescenza, ma **non deve necessariamente essere forzata con lo stesso peso di swelling e raggio**. In particolare, con una rappresentazione single-size e con una densità di dislocazioni assunta costante, differenze quantitative in $N_d$ possono essere accettabili se lo swelling e la scala dimensionale delle bolle sono riprodotti in modo soddisfacente e il trend di $N_d(T)$ resta fisicamente ragionevole.

La gerarchia preliminare adottata per la calibrazione può quindi essere:

$$
\boxed{
S_d \;>\; R_d \;>\; N_d
}
$$

senza interpretarla come una regola rigida: il peso relativo verrà rivalutato dopo aver osservato i risultati della calibrazione preliminare.

### Plot diagnostico da includere nella tesi

Per rendere visibile questa relazione si prevede di aggiungere un plot che confronti:

$$
N_d^{exp}
$$

con la concentrazione ricostruita a partire dallo swelling sperimentale e dal raggio medio sperimentale:

$$
\boxed{
N_d^{rec}
=
\frac{S_d^{exp}}
{\frac{4}{3}\pi\left(R_d^{exp}\right)^3}
}
$$

alle temperature per cui sono disponibili dati compatibili.

Il confronto:

$$
N_d^{exp}
\quad \text{vs} \quad
N_d^{rec}
$$

servirà a mostrare quantitativamente quanto l'approssimazione:

$$
\langle R^3\rangle
\approx
\langle R\rangle^3
$$

sia o meno adeguata per la popolazione P2. Una differenza tra le due curve non verrà interpretata automaticamente come errore sperimentale, ma come possibile conseguenza combinata di:

- distribuzione reale delle bubble sizes;
- differenza tra raggio medio e raggio volumetricamente equivalente;
- limiti di risoluzione della replica TEM;
- correzioni stereologiche e procedure di ricostruzione della densità numerica;
- approssimazione single-size del modello.

### Conseguenza interpretativa

Il confronto con $N_d$ resta quindi necessario, ma il modello non verrà calibrato sacrificando un buon accordo su swelling e raggio medio soltanto per riprodurre esattamente la concentrazione numerica. L'obiettivo è ottenere una rappresentazione fisicamente coerente della popolazione P2 entro i limiti intrinseci della single-size approximation, discutendo esplicitamente le differenze residue come **model-form uncertainty** e come possibile effetto della distribuzione sperimentale delle dimensioni.


---

## Commento per la tesi — Tanaka et al. (2004) come supporto sperimentale qualitativo alla gas partition

La **gas partition** mostrata da Rizk et al. (2025), e utilizzata come benchmark per il modello corrente, è un output di modello e non una misura sperimentale diretta. Tuttavia, esiste un'evidenza sperimentale indipendente utile a sostenere **qualitativamente** il quadro fisico di forte ritenzione intragranulare del gas nei combustibili nitruro.

**Tanaka et al. (2004)** hanno esaminato mediante PIE due fuel pins di **mixed nitride uranium–plutonium, \((U,Pu)N\)**, irradiati nel reattore veloce sperimentale **JOYO** fino a circa:

\[
4.3\%\ \mathrm{FIMA}
\]

a una linear heating rate di circa:

\[
75\ \mathrm{kW/m}.
\]

Gli autori misurano fission gas release pari a circa:

\[
\boxed{3.3\%-5.2\%}
\]

e, dalle distribuzioni radiali della concentrazione di Xe ottenute mediante **EPMA**, stimano che circa:

\[
\boxed{80\%}
\]

del gas di fissione rimanga nella **regione intragranulare**, mentre circa:

\[
\boxed{15\%}
\]

sia associato alle **fission-gas bubbles**.

Questi risultati sono qualitativamente coerenti con il quadro generale mostrato dalla gas partition di Rizk et al. e con quello ricercato nel presente modello: nella maggior parte del dominio di interesse il gas resta prevalentemente trattenuto all'interno del grano, mentre solo una frazione relativamente piccola raggiunge il rilascio esterno.

Il dato di Tanaka **non valida quantitativamente** la suddivisione specifica di Rizk tra:

\[
\text{matrix},
\qquad
\text{bulk bubbles},
\qquad
\text{dislocation bubbles},
\qquad
\text{grain-face bubbles},
\qquad
\text{FGR},
\]

e non deve essere interpretato come evidenza sperimentale di una particolare percentuale nelle sole bulk bubbles. Rimane però un importante **supporto sperimentale qualitativo** alla plausibilità della forte ritenzione intragranulare mostrata sia da Rizk Fig. 9 sia dalla corrispondente figura di gas partition del presente lavoro.

### Limite di trasferibilità

Il combustibile studiato da Tanaka et al. è:

\[
\boxed{(U,Pu)N}
\]

e non UN puro. Inoltre burnup, power history, microstruttura e condizioni di irraggiamento non coincidono con i nostri separate-effects calculations. Per questo il dato non viene usato come target quantitativo di calibrazione, ma come **conferma sperimentale quasi-qualitativa / order-of-magnitude** del fatto che nei combustibili nitruro una grande frazione del gas può rimanere intragranulare.

### Frase utilizzabile nella tesi

> Although the detailed gas partition reported by Rizk et al. is a model prediction, independent PIE measurements by Tanaka et al. on irradiated \((U,Pu)N\) fuel provide qualitative experimental support for the same general picture of strong intragranular gas retention. Approximately 80% of the fission gas was estimated to remain in the intragranular region, while the measured integral FGR was only about 3.3–5.2%. Because the experiment concerns mixed uranium–plutonium nitride under different irradiation conditions, these values are used here as qualitative supporting evidence rather than as direct calibration targets.

### Reference

K. Tanaka, K. Maeda, K. Katsuyama, M. Inoue, T. Iwai, Y. Arai,  
**“Fission gas release and swelling in uranium–plutonium mixed nitride fuels,”**  
*Journal of Nuclear Materials* **327** (2004) 77–87.  
DOI: **10.1016/j.jnucmat.2004.01.002**.


---

# Integrazioni 04/10/2026 — evoluzione delle diffusività UN, closure della bubble growth e fission-rate specifici dei casi Ronchi

## Sequenza storica della lower-length-scale physics: Cooper → Rizk → Schneider/Matthews

Per la stesura della tesi è utile presentare esplicitamente l'evoluzione storica della parametrizzazione delle diffusività, perché il modello di Rizk et al. (2025) non rappresenta semplicemente una copia immutata di Cooper et al. (2023), ma una **baseline di transizione** che incorpora già una conclusione preliminare del successivo lavoro Schneider/Matthews.

La sequenza concettuale è:

\[
\boxed{
\text{Cooper 2023 defect/CD dataset}
\rightarrow
\text{Rizk 2023 UNSIFGRS}
\rightarrow
\text{preliminary Schneider/Matthews update (2024)}
\rightarrow
\text{Rizk 2025 baseline}
\rightarrow
\text{Schneider updated CD dataset}
\rightarrow
\text{Matthews 2025 updated BISON/UNSIFGRS}
\rightarrow
\text{Schneider Bayesian 2026}
}
\]

Più precisamente:

1. **Cooper et al. (2023)** fornisce il dataset atomistico/cluster-dynamics originario per la self-diffusion di U e N e per la diffusività dello Xe in UN.
2. **Rizk et al. (2023)** usa ancora il contributo irradiation-enhanced dello Xe \(D_{2,Xe}\) e attribuisce ad esso parte del comportamento intermedio in temperatura del modello.
3. Nel 2024 **Schneider e Matthews** dispongono già di risultati atomistici preliminari che indicano una mobilità molto più bassa dello Xe interstitial e quindi un contributo \(D_{2,Xe}\) trascurabile.
4. **Rizk et al. (2025)** recepisce già questa conclusione e omette \(D_{2,Xe}\), citando esplicitamente **A. Schneider and C. Matthews, personal communication (2024)**, pur mantenendo ancora gran parte della formulazione diffusiva Cooper-based.
5. Il successivo lavoro di **Schneider et al.**, inizialmente submitted nel 2025 e poi pubblicato in *Journal of Nuclear Materials* 620 (2026) 156360, fornisce il nuovo dataset ab-initio-informed/cluster-dynamics completo.
6. **Matthews et al. (2025)** usa questo nuovo dataset all'interno di Centipede+BISON e introduce lookup tables dipendenti da temperatura, fission rate e stoichiometry.
7. **Schneider et al. (2026)**, nel lavoro Bayesian, usa la stessa catena multiscala Centipede+BISON ma tratta probabilisticamente diversi parametri lower-length-scale e microstrutturali.

### Frase pronta per la tesi

> Although the original Cooper et al. cluster-dynamics dataset predicted a significant irradiation-enhanced Xe diffusivity contribution \(D_2\), this contribution was omitted in the Rizk et al. baseline. This choice was motivated by preliminary updated atomistic calculations indicating a much lower Xe-interstitial mobility, and was also found to improve agreement with microscopic swelling data. Subsequent ab-initio-informed cluster-dynamics calculations by Schneider et al. provided a firmer physical basis for this approximation.

Una seconda frase utile è:

> Consequently, the Rizk et al. model can be regarded as a transitional baseline: it retained much of the Cooper et al. diffusivity formulation while already incorporating the preliminary conclusion of the later Schneider/Matthews work that the irradiation-enhanced Xe contribution is negligible.

---

## Cooper raw Centipede, analytical fit e lookup: non sono la stessa cosa

È importante distinguere:

\[
\boxed{
\text{Cooper raw Centipede results}
\neq
\text{analytical diffusivity adopted in Rizk/FY24}
}
\]

Nel dataset Centipede originario di Cooper compare una regione di irradiation-enhanced Xe diffusion \(D_{2,Xe}\) significativa. Quando Matthews et al. (2025) usa il **Cooper N-rich Lookup**, BISON interpola direttamente i valori tabulati prodotti da Centipede e quindi conserva quella regione \(D_2\).

La formulazione **Cooper N-rich Analytical (FY24)**, invece, è una correlazione analitica ridotta costruita per l'uso in BISON. In questa versione il contributo \(D_{2,Xe}\) era stato esplicitamente rimosso. La successiva pubblicazione Rizk et al. (2025) usa quindi, per lo Xe:

\[
\boxed{
D_{Xe}^{Rizk}=D_1+D_3
}
\]

mentre continua a mantenere il contributo irradiation-enhanced per la vacancy/self diffusion, dove \(D_2\) resta significativo.

Quindi non è corretto dire che il fit analitico di Rizk "faceva diventare \(D_2\) quasi zero". Più precisamente:

\[
\boxed{
\text{il vecchio Cooper raw prediceva un }D_{2,Xe}\text{ significativo,}
}
\]

ma

\[
\boxed{
\text{Rizk/FY24 lo omette deliberatamente alla luce dei nuovi risultati atomistici.}
}
\]

### Significato di lookup

Con **lookup** non si usa una singola correlazione chiusa \(D(T)\). Centipede viene eseguito su una griglia di condizioni:

\[
T,\qquad \dot F,\qquad y=\frac{N}{U}-1,
\]

e i valori risultanti vengono salvati in tabelle. BISON interpola quindi direttamente:

\[
\boxed{
(T,\dot F,y)\longrightarrow D
}
\]

preservando caratteristiche non-Arrhenius che possono essere perse da un fit analitico semplice.

---

## Perché cambiano \(D_{Xe}\) e \(D_U\) da Cooper a Schneider

### Xe

La modifica più importante riguarda la mobilità dello Xe interstitial. Nel nuovo dataset Schneider la migration barrier passa approssimativamente da:

\[
0.4\ \mathrm{eV}
\rightarrow
1.38\ \mathrm{eV}.
\]

Questo sopprime fortemente la vecchia regione irradiation-enhanced dello Xe. Nel regime rilevante il contributo \(D_{2,Xe}\) viene quindi mascherato dai contributi termico e soprattutto atermico:

\[
\boxed{
D_{Xe}\simeq D_1+D_3.
}
\]

La scelta di Rizk 2025 di trascurare \(D_{2,Xe}\) anticipava quindi il risultato poi giustificato più solidamente dal dataset Schneider.

### Uranium self-diffusion

Per l'uranio la differenza non deriva semplicemente da una nuova singola migration barrier. Schneider aggiorna le free energies / formation entropies dei difetti e quindi le concentrazioni delle diverse specie che trasportano U.

Per un difetto \(d\), il contributo alla self-diffusivity della specie \(X\) ha schematicamente la forma:

\[
\boxed{
D_{X,d}
=
\frac{f\,x_d\,D_d}{x_X}
}
\]

dove:

- \(D_d\) è la mobilità/diffusività del difetto;
- \(x_d\) è la sua concentrazione;
- \(f\) è un correlation factor;
- \(x_X\) è la frazione della specie trasportata.

Quindi la total uranium self-diffusivity è:

\[
\boxed{
D_U^{tot}
=
\sum_d D_{U,d}.
}
\]

Il valore finale può cambiare molto anche senza cambiare drasticamente tutte le migration barriers, perché cambiano le concentrazioni di \(V_U\), interstiziali, antisiti e cluster.

---

## Collegamento tra \(D_U\), vacancy transport e bubble growth

Non va scritto che la mobilità microscopica della uranium vacancy sia identica alla uranium self-diffusivity:

\[
D_{V_U}^{mob}\neq D_U.
\]

Fisicamente, in un meccanismo vacancy-mediated, ogni salto della vacancy corrisponde al salto di un atomo U nella direzione opposta. Tuttavia la self-diffusivity macroscopica dell'U dipende anche da **quante vacancies sono presenti**:

\[
D_U^{(V_U)}
\sim
x_{V_U}D_{V_U}^{mob}.
\]

Nel framework Rizk/Matthews il punto chiave è invece la **closure efficace** utilizzata per la crescita delle bolle:

1. i self-defects dell'azoto sono molto più mobili;
2. la uranium self-diffusion è quindi rate limiting per il riarrangiamento della matrice;
3. la uranium self-diffusion è, nelle condizioni rilevanti, largamente vacancy-mediated;
4. il coefficiente efficace che controlla la bubble growth viene quindi ricondotto alla uranium self-diffusion.

Per la trasposizione nel nostro modello è quindi appropriato indicare:

\[
\boxed{
D_v^{eff}
\leftarrow
D_U^{self}
}
\]

specificando però che si tratta di una **effective U self-diffusivity controlling vacancy-mediated matrix transport and bubble growth**, non della mobilità microscopica di una singola vacancy.

### Nota sul ruolo di \(D_N\)

Il fatto che \(D_N\gg D_U\) non significa che "l'azoto aspetti l'uranio" per una ragione puramente termodinamica. La termodinamica limita l'accumulo indefinito di non-stechiometria, mentre cineticamente il sottoreticolo N può riaggiustarsi rapidamente. Il processo globale di riarrangiamento della matrice rimane quindi controllato dal trasporto più lento sul sottoreticolo U.

---

## Limite della current bubble-growth closure: flusso netto vacancies–interstitials

Matthews et al. (2025) riconosce esplicitamente che una descrizione più completa della crescita/riduzione delle bolle dovrebbe utilizzare il **net flux of vacancies and interstitials to the bubbles**, invece di rappresentare il processo unicamente tramite una self-diffusivity efficace.

La descrizione fisica più completa sarebbe quindi concettualmente:

\[
\boxed{
\dot n_v
\propto
J_V^{bubble}-J_I^{bubble}
}
\]

con concentrazioni, diffusività, ricombinazione e sink bias coerentemente trattati.

Questo costituisce una reference diretta molto utile per la discussione della tesi: il neglect del flusso interstitial esplicito non è soltanto una limitazione identificata nel presente lavoro, ma viene riconosciuto anche nella linea di sviluppo BISON/UNSIFGRS.

### Frase pronta per la tesi

> The current effective self-diffusion closure does not explicitly resolve the competing vacancy and self-interstitial fluxes to the bubble. Matthews et al. identified a direct treatment of the net vacancy–interstitial flux as a desirable future improvement of the UN fission-gas model.

---

## Matthews 2025: scelta della stechiometria \(UN_{1.000001}\)

Matthews et al. esplora deterministicamente diverse stoichiometries e confronta le predizioni di microscopic swelling con i dati DN1/Ronchi. Il miglior accordo viene ottenuto molto vicino alla stechiometria perfetta, sul lato leggermente N-rich:

\[
10^{-6}\lesssim y\lesssim10^{-5},
\qquad
\frac{N}{U}=1+y.
\]

Per le simulazioni finali viene adottato:

\[
\boxed{
y=10^{-6}
}
\]

cioè:

\[
\boxed{
UN_{1.000001}.
}
\]

Questa scelta è effettuata **a posteriori rispetto al confronto con i dati di swelling**, ma non è una Bayesian inference: è una selezione/calibrazione deterministica mediante sweep della stoichiometry.

Questo punto va esplicitato nella tesi perché la nuova diffusività "Schneider/Matthews" non è una proprietà universale \(D(T)\), ma una predizione condizionata anche alla chemistry:

\[
\boxed{
D=D(T,\dot F,y).
}
\]

### Frase pronta per la tesi

> Matthews et al. inferred a near-stoichiometric, slightly N-rich composition by comparing stoichiometry-dependent model predictions with the microscopic swelling measurements, and subsequently adopted \(UN_{1.000001}\) for the updated simulations.

---

## Curve Matthews da utilizzare per l'aggiornamento delle diffusività

Per riprodurre la scelta finale Matthews, le curve rilevanti da digitalizzare sono quelle della **Fig. 4.2** corrispondenti alla composizione leggermente hyper-stoichiometric:

\[
\boxed{y=10^{-6}}.
\]

In particolare:

- **Fig. 4.2(b):** uranium self-diffusivity \(D_U^{tot}\), da usare come candidato \(D_v^{eff}\) per la bubble growth;
- **Fig. 4.2(f):** Xe diffusivity \(D_{Xe}\).

Attenzione: la Fig. 4.2 è mostrata a:

\[
\boxed{
\dot F=10^{19}\ \mathrm{fissions\,m^{-3}\,s^{-1}}
}
\]

mentre le simulazioni finali dei casi DN1/Ronchi usano i fission rates specifici dei singoli pin. La digitalizzazione della Fig. 4.2 è quindi utile come primo test/ricostruzione della nuova physics, ma non deve essere presentata come una legge universale indipendente da \(\dot F\).

Il vero lookup Matthews è tridimensionale:

\[
\boxed{
D(T,\dot F,y).
}
\]

---

## Grain-boundary vacancy diffusivity: confrontare Rizk e Matthews

Matthews et al. mantiene la closure:

\[
D_v^{GB}=10^6D_{v,\mathrm{thermal}}^{bulk},
\]

ma aggiorna la thermal vacancy-mediated U self-diffusion sottostante. Per il caso stechiometrico:

\[
\boxed{
D_v^{GB,Matthews}
=
325
\exp\left(-\frac{5.95}{k_BT}\right)
\ \mathrm{m^2\,s^{-1}}
}
\]

e quindi:

\[
\boxed{
D_{v,\mathrm{thermal}}^{bulk,Matthews}
=
3.25\times10^{-4}
\exp\left(-\frac{5.95}{k_BT}\right)
\ \mathrm{m^2\,s^{-1}}.
}
\]

Questa quantità è meglio interpretata come **thermal vacancy-mediated U self-diffusion contribution**, cioè concentrazione termica del difetto × mobilità, non come pura vacancy mobility.

Per la tesi non conviene scegliere a priori tra la closure GB di Rizk e quella di Matthews. Poiché nel modello corrente l'impatto del GB vacancy transport sembra secondario, è più trasparente mostrare una sensitivity dedicata:

\[
\boxed{
D_v^{GB,Rizk}
\quad\text{vs}\quad
D_v^{GB,Matthews}.
}
\]

### Nessun fattore \(10^6\) per lo Xe al grain boundary

Non va introdotta una relazione:

\[
D_{Xe}^{GB}=10^6D_{Xe}^{bulk}.
\]

Nel framework Rizk/Matthews il trasporto dello Xe **verso** il grain boundary è governato dalla diffusività intragranulare dello Xe. Una diffusività separata dello Xe **lungo** il grain boundary non viene introdotta nella stessa forma della vacancy diffusion.

---

# Correzione fondamentale: fission-rate density specifica per ciascun caso Ronchi/DN1

Una singola fission-rate density costante per tutti i benchmark Ronchi non è corretta.

Rizk et al. (2025) specifica che, nei simplified/separate-effects calculations usati per confrontarsi con DN1, la fission-rate density viene calcolata dalla **linear heat rating** del pin e da un fuel diameter di 8.30 mm. Ronchi et al. (1978) mostra però che i pin hanno ratings differenti.

Per una linear heat rate \(q'\), diametro del fuel \(d\) ed energia per fissione \(E_f\):

\[
\boxed{
\dot F
=
\frac{q'}
{\pi(d/2)^2E_f}.
}
\]

## Energia per fissione da usare nella ricostruzione

Per una conversione nominale di:

\[
200\ \mathrm{MeV/fission}
\]

si ottiene, usando il valore esatto dell'elettronvolt:

\[
\boxed{
E_f
=
3.20435313\times10^{-11}\ \mathrm{J/fission}.
}
\]

Questo è anche il valore riportato in test/documentazione BISON come conversione di 200 MeV/fission. In diversi input BISON viene usata anche la versione arrotondata:

\[
3.2\times10^{-11}\ \mathrm{J/fission}.
\]

La differenza è inferiore allo 0.2% e non è significativa per il presente benchmark. Per evitare ambiguità, nella ricostruzione seguente viene usato il valore esplicito di **200 MeV/fission**, cioè \(3.20435313\times10^{-11}\) J/fission.

## Geometria e ratings direttamente da Ronchi 1978

La Table 1 originale di Ronchi riporta per i pin nitruro DN1/AP3:

\[
(U_{0.8}Pu_{0.2})N,
\qquad
d=8.3\ \mathrm{mm},
\]

con He bonding e radial gap di circa 150 \(\mu\)m.

I tre casi AP3 rilevanti sono:

| Caso | Burnup | Linear heat rate | Fuel diameter |
|---|---:|---:|---:|
| AP3.2 | 1.1% FIMA | 100 kW/m | 8.3 mm |
| AP3.8 | 1.1% FIMA | 119 kW/m | 8.3 mm |
| AP3.4 | 1.3% FIMA | 130 kW/m | 8.3 mm |

Il caso DN2/ANP6 è invece:

| Caso | Burnup | Linear heat rate | Fuel diameter |
|---|---:|---:|---:|
| ANP6 | 3.2% FIMA | 125 kW/m | 8.0 mm |

Quindi il diametro 8.3 mm è corretto per i casi AP3, ma **Ronchi riporta 8.0 mm per ANP6**.

## Fission-rate densities ricostruite dai dati Ronchi

Usando \(E_f=3.20435313\times10^{-11}\) J/fission:

\[
\boxed{
\dot F_{\mathrm{AP3.2}}
=
5.77\times10^{19}\ \mathrm{m^{-3}s^{-1}}
}
\]

\[
\boxed{
\dot F_{\mathrm{AP3.8}}
=
6.86\times10^{19}\ \mathrm{m^{-3}s^{-1}}
}
\]

\[
\boxed{
\dot F_{\mathrm{AP3.4}}
=
7.50\times10^{19}\ \mathrm{m^{-3}s^{-1}}
}
\]

e, usando il diametro originale Ronchi \(d=8.0\) mm:

\[
\boxed{
\dot F_{\mathrm{ANP6}}
=
7.76\times10^{19}\ \mathrm{m^{-3}s^{-1}}.
}
\]

Se invece per ANP6 si applica anche lì il diametro semplificato di 8.30 mm indicato genericamente da Rizk per i simplified calculations, si ottiene:

\[
\dot F_{\mathrm{ANP6},\,8.3mm}
\simeq
7.21\times10^{19}\ \mathrm{m^{-3}s^{-1}}.
\]

Questa differenza va trattata come una piccola **model-input ambiguity** tra la geometria originale Ronchi e la semplificazione descritta da Rizk.

## Conseguenza fondamentale per il codice

Il precedente uso di un unico valore rappresentativo, ad esempio:

\[
\dot F=5\times10^{19}\ \mathrm{m^{-3}s^{-1}},
\]

per tutti i benchmark microscopic swelling deve essere sostituito da:

\[
\boxed{
\dot F=\dot F(\text{experimental pin}).
}
\]

Questo è particolarmente importante perché \(\dot F\) entra contemporaneamente in più meccanismi:

- produzione di Xe;
- athermal Xe diffusivity \(D_3\);
- irradiation-enhanced diffusivity / lookup Centipede;
- re-solution;
- tempo necessario a raggiungere un dato burnup.

Di conseguenza, usare lo stesso \(\dot F\) per AP3.2, AP3.8, AP3.4 e ANP6 può alterare non soltanto il tempo di irraggiamento ma anche la cinetica del modello.

### Nota specifica sul benchmark 1.3% FIMA

Il caso sperimentale di riferimento a:

\[
1.3\%\ \mathrm{FIMA}
\]

corrisponde a:

\[
\boxed{
\text{AP3.4, }130\ \mathrm{kW/m}
}
\]

e quindi:

\[
\boxed{
\dot F\simeq7.50\times10^{19}\ \mathrm{m^{-3}s^{-1}}.
}
\]

Questo valore deriva direttamente dalla linear heat rating e dal diametro riportati da Ronchi; non è un parametro ottenuto tramite calibrazione del nostro modello.

---

## Commento metodologico per la tesi — Fig. 3 di Rizk e i due dataset a 1.1% FIMA

Nel pannello a \(1.1\%\) FIMA della Fig. 3 di Rizk compaiono due serie sperimentali perché corrispondono a due pin DN1 diversi:

\[
\boxed{
\text{AP3.2: }100\ \mathrm{kW/m}
}
\]

e

\[
\boxed{
\text{AP3.8: }119\ \mathrm{kW/m}.
}
\]

Le figure agli altri burnup non devono quindi essere interpretate come calcoli eseguiti tutti allo stesso linear power. Il benchmark \(1.3\%\) FIMA deriva da AP3.4 a 130 kW/m, mentre quello \(3.2\%\) FIMA deriva da ANP6 a 125 kW/m.

Questo dettaglio va esplicitato quando nella tesi si descrive la ricostruzione dei separate-effects calculations di Rizk, perché chiarisce che temperatura e burnup non sono gli unici input sperimentali: anche la fission-rate density è **case-specific**.

---

## Decisioni operative aggiornate prima della digitalizzazione

Per il modello finale da esplorare:

1. mantenere una simulazione **baseline Rizk** con i parametri/diffusività originali per mostrare il punto di partenza prima della calibrazione;
2. aggiornare \(D_{Xe}\) verso il dataset Schneider/Matthews coerente con la composizione scelta \(UN_{1.000001}\);
3. testare come coefficiente efficace di bubble growth la \(D_U^{tot}\) Schneider/Matthews, dichiarandola correttamente come **effective U self-diffusivity controlling vacancy-mediated matrix transport and bubble growth**;
4. confrontare separatamente:
   \[
   D_v^{GB,Rizk}
   \quad\text{e}\quad
   D_v^{GB,Matthews};
   \]
5. usare fission rates specifici per AP3.2, AP3.8, AP3.4 e ANP6;
6. mantenere esplicita nella discussione la limitazione dovuta all'assenza del flusso interstitial nella current bubble-growth law;
7. nel prossimo passaggio, digitalizzare le curve Matthews selezionate di \(D_U\) e \(D_{Xe}\) per \(y=10^{-6}\), verificando attentamente il fission rate associato alla figura e distinguendolo dai fission rates dei casi Ronchi.

### References principali di questo blocco

- M. W. D. Cooper, J. Rizk, C. Matthews, V. Kocevski, G. T. Craven, T. Gibson, D. A. Andersson, **“Simulations of self- and Xe diffusivity in uranium mononitride including chemistry and irradiation effects,”** *Journal of Nuclear Materials* 587 (2023) 154685.
- J. T. Rizk, A. J. Schneider, M. W. D. Cooper, D. A. Andersson, C. Matthews, **“Development of Mechanistic Fission Gas Release and Swelling Models for UN Fuels in BISON,”** LA-UR-23-29157, 2023.
- A. Schneider, J. Rizk, M. Kosmidou, C. Matthews, D. A. Andersson, M. W. D. Cooper, **“Radiation induced athermal diffusivity in uranium mononitride,”** *Journal of Nuclear Materials* 601 (2024) 155313.
- J. T. Rizk, M. W. D. Cooper, P.-C. A. Simon, A. J. Schneider, D. A. Andersson, S. R. Novascone, C. Matthews, **“Mechanistic nuclear fuel performance modeling of uranium nitride,”** *Journal of Nuclear Materials* 606 (2025) 155604.
- C. Matthews, C. O. Galvin, A. J. Schneider, M. W. D. Cooper, **“Finish development, test and then demonstrate new baseline fuel performance capability for UN fuel swelling models under steady-state and transient conditions,”** LA-UR-25-28152, 2025.
- A. J. Schneider, C. Matthews, D. A. Andersson, M. W. D. Cooper, **“Ab-initio informed cluster dynamics simulation of self- and Xe diffusivity in uranium mononitride under irradiation,”** *Journal of Nuclear Materials* 620 (2026) 156360.
- A. J. Schneider et al., **“Integrating mechanistic models with separate effects and integral tests through neural network-enabled Bayesian inference: Application to uranium nitride fuel swelling,”** *Journal of Nuclear Materials* 632 (2026) 156895.
- C. Ronchi, I. L. F. Ray, H. Thiele, J. Van De Laar, **“Swelling analysis of highly-rated MX-type LMFBR fuels: II. Microscopic swelling behaviour,”** *Journal of Nuclear Materials* 74 (1978) 193–211.
- BISON documentation / Burnup Action regression example: `energy_per_fission = 3.20435313e-11 J/fission (200 MeV)`. Several BISON examples alternatively use the rounded \(3.2\times10^{-11}\) J/fission.
