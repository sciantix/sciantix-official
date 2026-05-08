# Contesto operativo — progetto UN Thesis / modello M7 per swelling da fission gas in UN

Questo file serve come **memoria di contesto** per una nuova chat del progetto *UN Thesis*.  
L’obiettivo è permettere all’IA di capire subito a che punto siamo, senza dover ricostruire tutta la storia.

---

## 1. Obiettivo generale

Stiamo sviluppando e calibrando un modello meccanicistico per il comportamento dello xeno in combustibile **uranium nitride (UN)**, seguendo principalmente il modello e i parametri riportati da **Rizk et al.** per SCIANTIX-UN, con confronto verso dati sperimentali P2.

Il target pratico adesso è:

1. migliorare al massimo il modello corrente **M7**;
2. capire quali parametri/termini controllano:
   - swelling da bolle su dislocazioni;
   - swelling da bolle bulk;
   - raggio delle bolle;
   - concentrazione delle bolle;
   - coalescenza / riduzione di concentrazione ad alta temperatura;
   - pressione interna vs pressione di equilibrio;
   - partizione del gas fra matrice, bulk bubbles, dislocation bubbles, grain boundary/release;
3. decidere se M7 è fisicamente difendibile oppure se conviene passare ad altri modelli/varianti;
4. eventualmente rifare lo stesso processo su modelli alternativi.

**Importante:** per ora la priorità è il fit dei dati sperimentali P2, soprattutto swelling e microstruttura delle bolle. Il confronto con le curve di Rizk è utile come diagnostica, ma se Rizk non coincide con i dati o sembra avere parametri sospetti, non va seguito ciecamente.

---

## 2. Vincolo metodologico importante

L’utente ha espresso chiaramente che:

- non vuole che vengano cambiate “a caso” le equazioni di Rizk/Barani;
- se si modifica una legge fisica bisogna dirlo esplicitamente e motivarlo;
- preferisce prima usare **scaling factors** davanti ai termini fisici, per fare sensitivity/calibrazione, non sostituire le equazioni;
- la coalescenza delle bolle su dislocazioni **non deve essere disattivata**;
- la capture bulk-dislocation non va sostituita con legge esponenziale o poissoniana se non richiesto;
- per ora bisogna restare su M7 e cercare di capire se può funzionare.

Formula pratica da ricordare:  
**non cambiare equazioni, calibrare tramite scale factors e parametri incerti.**

---

## 3. Modello corrente M7

Il modello M7 usato finora include:

1. **produzione gas in matrice**
   \[
   \beta = Y_{\mathrm{Xe}}\dot F
   \]
   con \(Y_{\mathrm{Xe}} = 0.24\) Xe/fissione.

2. **diffusione intragranulare dello Xe**
   \[
   D_g = D_1 + D_3
   \]
   con:
   \[
   D_1 = D_{10}\exp\left(-\frac{Q_1}{k_BT}\right)
   \]
   \[
   D_3 = A_{30}\dot F
   \]
   Valori nominali usati:
   - \(D_{10}=1.56\times10^{-3}\,\mathrm{m^2/s}\)
   - \(Q_1=4.94\,\mathrm{eV}\)
   - \(A_{30}=1.85\times10^{-39}\,\mathrm{m^5}\)

   Nota: nel codice corrente il termine \(D_2\) per Xe non è incluso/è implicitamente zero, coerentemente con la versione che stavamo usando.

3. **diffusività delle vacancy di uranio**

   È stata usata una legge rifittata da Fig. 4 di Rizk perché era emerso un problema con i numeri originali:
   \[
   D_v = D_{v,1}+D_{v,2}
   \]
   con:
   \[
   D_{v,1}=D_{10}^{vU}\exp\left(-\frac{Q_1^{vU}}{k_BT}\right)
   \]
   \[
   D_{v,2}=\sqrt{\dot F}\,A_{20}^{vU}\exp\left(\frac{B_{21}}{k_BT}+\frac{B_{22}}{(k_BT)^2}\right)
   \]
   Valori usati nel codice:
   - \(D_{10}^{vU}=1.35\times10^{-2}\,\mathrm{m^2/s}\)
   - \(Q_1^{vU}=5.66\,\mathrm{eV}\)
   - \(B_{21}=-0.62\)
   - \(B_{22}=-0.04\)
   - \(A_{20}=4.6304523933553033\times10^{-29}\)

   Attenzione: questo è uno dei punti già sospetti/ritoccati rispetto a Rizk.

4. **nucleazione bulk**
   \[
   \nu_b = 8\pi f_n D_g \Omega_{fg}^{1/3} c^2
   \]
   con valore Rizk nominale:
   - \(f_n=10^{-6}\)

   Nel modello M7 la nucleazione è accoppiata alla massa gas:
   \[
   S_c = \beta - 2\nu_b
   \]
   \[
   S_{m_b} = 2\nu_b
   \]

5. **trapping verso bolle bulk e dislocation bubbles**

   Bulk:
   \[
   g_b = 4\pi D_g R_b N_b
   \]
   con raggio efficace nel codice:
   \[
   R_b + r_{\mathrm{lattice}}
   \]
   dove \(r_{\mathrm{lattice}}=0.21\,\mathrm{nm}\).

   Dislocazioni/dislocation bubbles:
   \[
   g_d = 4\pi D_g R_d N_d +
   \frac{2\pi D_g}{\ln(\Gamma_d/(Z_d r_d))-3/5}\rho_{d,\mathrm{free}}
   \]
   con:
   \[
   \Gamma_d = \frac{1}{\sqrt{\pi\rho_d}}
   \]
   \[
   \rho_{d,\mathrm{free}}=\max(\rho_d - 2R_d N_d,0)
   \]

   Valori nominali:
   - \(Z_d=5\)
   - \(r_d=3.46\times10^{-10}\,\mathrm{m}\)
   - \(\rho_d=3\times10^{13}\,\mathrm{m^{-2}}\)
   - \(K_d=5\times10^5\,\mathrm{bubble/m}\)
   - \(N_{d0}=K_d\rho_d=1.5\times10^{19}\,\mathrm{m^{-3}}\)

   Nota: il termine \(-3/5\) nel denominatore è stato controllato su Barani e considerato corretto.

6. **resolution**

   La legge base usata:
   \[
   b = b_0(R)\dot F
   \]
   \[
   b_0(R)=10^{-25}\left(2.64 - 2.02\exp\left(-\frac{2.61\times10^{-9}}{R}\right)\right)
   \]

   In M7 è stata usata la correzione con \(\phi\):
   \[
   \phi=\frac{1}{m/N - 1}
   \]
   e quindi nei termini gas:
   \[
   b_{\mathrm{gas}} = b\phi
   \]

   Tuttavia, questo punto è ancora aperto: **potremmo abbandonare la \(\phi\) nella resolution** se risulta che peggiora o non è coerente con Rizk/SCIANTIX/Barani.  
   Per ora va considerato come scelta M7, non verità definitiva.

7. **vacancy absorption**

   Viene usato uno step implicito per aggiornare \(n_v\), basato su:
   \[
   \frac{dn_v}{dt}
   =
   \frac{2\pi D_v\delta N}{k_BT\zeta}(p-p_{eq})
   \]
   con l’opzione `vacancy_absorption_only=True`, cioè le vacancy vengono assorbite solo se \(p>p_{eq}\).

   Pressione interna:
   \[
   p=\frac{k_BT\,m}{n_v\Omega_m}
   \]

   Pressione di equilibrio:
   \[
   p_{eq}=\frac{2\gamma}{R}-\sigma_h
   \]
   con:
   - \(\gamma=1.11\,\mathrm{J/m^2}\)
   - \(\sigma_h=0\)

   Volume bubble:
   \[
   V = \frac{\Omega_{fg}m+\Omega_m n_v}{N}
   \]

8. **coalescenza dislocation bubbles**

   Deve restare attiva. Formula usata:
   \[
   \lambda_d = \frac{2-\xi}{2(1-\xi)^3}
   \]
   con:
   \[
   \xi = V_d N_d
   \]
   Aggiornamento:
   \[
   N_d^{new} = \frac{N_d^{old}}{1 + 4\lambda_d N_d^{old}\Delta V_d}
   \]
   se \(\Delta V_d>0\).

   Nel codice più recente è stato introdotto anche:
   \[
   \mathrm{coalescence\_d\_scale}
   \]
   come fattore moltiplicativo, ma **non deve essere messo a zero**. Può essere esplorato, ad esempio 0.3, 1, 3, ecc.

9. **bulk-dislocation capture**

   La legge implementata è quella lineare/clippata tipo Barani/Rizk:
   \[
   f_{\mathrm{cap}} = \min(1,\max(0,\mathrm{capture\_scale}\,N_d\Delta V_{\mathrm{cap}}))
   \]
   con:
   \[
   V_{\mathrm{cap}}=\frac{4}{3}\pi(R_b+R_d)^3
   \]

   Quando una frazione \(f_{\mathrm{cap}}\) di bulk bubbles viene catturata dalle dislocation bubbles:
   - \(m_b \rightarrow (1-f_{\mathrm{cap}})m_b\)
   - \(m_d \rightarrow m_d+f_{\mathrm{cap}}m_b\)
   - \(n_{vb}\rightarrow(1-f_{\mathrm{cap}})n_{vb}\)
   - \(n_{vd}\rightarrow n_{vd}+f_{\mathrm{cap}}n_{vb}\)
   - \(N_b\rightarrow(1-f_{\mathrm{cap}})N_b\)

   Diagnostica importante:
   - `f_cap_step` è una frazione per step e deve essere sempre \(\le 1\);
   - `capture_fraction_sum` può superare 1 perché è solo la somma diagnostica dei valori per step, **non una frazione fisica cumulativa**;
   - non interpretare `capture_fraction_sum > 1` come errore se `f_cap_step <= 1`.

---

## 4. Parametri principali nominali

Valori usati nel codice:

| parametro | valore nominale | nota |
|---|---:|---|
| `FISSION_RATE_NOMINAL` | \(5.0\times10^{19}\,\mathrm{fiss/(m^3s)}\) | ricavato/assunto da condizioni tipo 100–119 kW/m, da verificare |
| `GRAIN_RADIUS` | \(6.0\times10^{-6}\,\mathrm{m}\) | nominale |
| `XE_YIELD` | 0.24 | Xe/fissione |
| `F_N_NOMINAL` | \(1.0\times10^{-6}\) | nucleation factor Rizk |
| `K_D_NOMINAL` | \(5.0\times10^5\,\mathrm{bubble/m}\) | Rizk |
| `RHO_D_NOMINAL` | \(3.0\times10^{13}\,\mathrm{m^{-2}}\) | Rizk |
| `OMEGA_FG` | \(8.5\times10^{-29}\,\mathrm{m^3}\) | volume per fission gas atom |
| `LATTICE_PARAMETER` | \(4.889\times10^{-10}\,\mathrm{m}\) | UN |
| `GAMMA_B` | \(1.11\,\mathrm{J/m^2}\) | surface energy |
| `Z_d` | 5 | dislocation bias/capture |
| `r_d` | \(3.46\times10^{-10}\,\mathrm{m}\) | dislocation core radius |
| `radius_in_lattice` | \(0.21\,\mathrm{nm}\) | raggio efficace aggiunto |

---

## 5. Cosa è successo nei primi sweep

### 5.1 Sweep nominale M7

Il primo M7 nominale con:
- \(f_n=10^{-6}\)
- \(K_d=5\times10^5\)
- \(\rho_d=3\times10^{13}\)
- \(Fdot=5\times10^{19}\)
- \(D_v\) refit
- capture attiva
- coalescenza attiva

produceva qualitativamente:
- dislocation swelling troppo alto ad alta temperatura;
- bulk swelling molto significativo;
- bulk bubbles dominate da \(N_b\) enorme;
- pressure ratios con \(p/p_{eq}>1\) a bassa T e verso equilibrio a T alta;
- gas partition con bulk bubbles molto dominanti a bassa/intermedia T e dislocation bubbles dominanti ad alta T.

### 5.2 Sweep fisico parziale

Un miglior candidato del primo sweep fisico era circa:

- `f_n = 3e-7`
- `K_d = 3e5`
- `rho_d = 3e13`
- `Fdot = 7.5e19`
- `Dv_scale = 0.3`
- `capture_scale = 1.0`

Questo cambiava poco rispetto al nominale, soprattutto:
- \(f_n\) ridotto di circa fattore 3;
- \(K_d\) ridotto da \(5e5\) a \(3e5\);
- \(Fdot\) aumentato a \(7.5e19\);
- \(D_v\) ridotto a 0.3.

Il fit dello swelling vs T migliorava parecchio nella zona 1500–1650 K, ma restavano problemi:
- swelling dislocation esplode ancora sopra ~1700 K;
- bulk swelling non assomiglia bene a Rizk;
- concentrazione \(N_d\) non riproduce abbastanza la caduta sperimentale se il peso dello scoring non la forza;
- \(N_b\) rimane molto alta, spesso \(10^{21}\,\mathrm{m^{-3}}\), mentre i punti sperimentali sono per large bubbles/P2 e non necessariamente bulk;
- pressioni non coincidono con Rizk: in Rizk bulk \(p<p_{eq}\) per ampia zona, mentre nel nostro modello spesso parte con \(p>p_{eq}\) e poi tende a \(p\approx p_{eq}\).

### 5.3 Commento del tutor

Il tutor ha notato soprattutto:

- nel grafico della concentrazione, la linea \(N_d\) del modello non scende come i dati sperimentali ad alta T;
- ha chiesto “la coalescenza qui?”, cioè vuole capire se la coalescenza è davvero rappresentata e produce la caduta di \(N_d\);
- confrontando con Rizk, sembra che intorno a ~1700 K ci sia un cambio/legame importante;
- il modello riproduce abbastanza la salita dello swelling da dislocation come Rizk/dati, ma:
  - il bulk swelling non esce come Rizk;
  - in Rizk il bulk ha un picco intorno a ~1800 K;
  - le nostre bulk bubbles spesso non hanno quel comportamento;
  - le pressioni bulk/dislocation differiscono da Rizk.

Da questo è nata l’idea di forzare nello scoring anche la microstruttura/coalescenza, non solo lo swelling.

---

## 6. Problemi aperti principali

### 6.1 Coalescenza / concentrazione \(N_d\)

Problema più importante adesso.

I dati sperimentali di concentrazione P2/large bubbles a 1.3% FIMA mostrano:
- valori alti circa \(10^{19}\)–\(10^{20}\,\mathrm{m^{-3}}\) a T medio-basse;
- discesa marcata verso \(10^{18}\,\mathrm{m^{-3}}\) ad alta temperatura, circa 1650–1750 K.

Il nostro modello spesso:
- tiene \(N_d\) troppo piatto;
- oppure cala troppo tardi;
- oppure ha \(N_d\) troppo basso/alto come livello assoluto.

Serve quindi:
- aumentare il peso di \(N_d(T)\) nello scoring;
- includere esplicitamente un termine sulla pendenza/caduta ad alta T;
- fare sweep anche su `coalescence_d_scale`, `gd_scale`, `Dv_scale`, `b_scale`, `rho_d`, `K_d`, magari `Dg_scale`;
- mantenere coalescenza attiva.

### 6.2 Bulk swelling

Rizk mostra una curva bulk che:
- domina la partizione gas a T basse/intermedie;
- poi cala quando il gas passa alle dislocation bubbles;
- nello swelling bulk sembra avere un picco intorno a ~1800 K.

Il nostro modello spesso:
- produce bulk swelling troppo basso se la capture/dislocation domina;
- oppure bulk swelling monotono o non con lo stesso picco;
- oppure bulk \(N_b\) enorme e difficile da interpretare fisicamente.

Per ora l’utente dice: **dare priorità al fit sperimentale**, non al bulk swelling di Rizk, perché non sappiamo se Rizk è corretto in ogni dettaglio.

### 6.3 Pressioni

Rizk mostra:
- bulk bubble pressure spesso sotto equilibrio;
- dislocation pressure e bulk pressure con trend diversi.

Noi spesso otteniamo:
- \(p_b/p_{eq}>1\) a bassa T;
- \(p_b\to p_{eq}\) a T alta;
- \(p_d/p_{eq}\) molto alto a bassa T, poi minimo, talvolta risale.

Non è chiaro se convenga mettere pressione nello scoring.  
Per ora l’idea è:
- non ignorarla del tutto;
- ma darle peso molto basso, perché non è un dato sperimentale diretto e può dipendere da implementazione/assunzioni sulle vacancy;
- usarla soprattutto come diagnostica fisica.

### 6.4 Phi nella resolution

M7 usa:
\[
b_{\mathrm{gas}}=b\phi
\]

Ma non siamo sicuri che sia corretto o utile.  
Possibili varianti future:
- M7 con \(\phi\);
- M7 senza \(\phi\);
- \(\phi\) solo su \(N_b\) ma non sul gas;
- altri modelli più vicini a SCIANTIX/Rizk.

Per ora stiamo ancora calibrando M7 con \(\phi\), ma una nuova chat deve sapere che **potremmo abbandonarla**.

### 6.5 Fission rate

È stato fatto variare in alcuni sweep:
- \(3.5e19\)
- \(5.0e19\)
- \(7.5e19\)
- in ultimo sweep anche intorno a \(1.0e20\) in alcuni candidati.

L’utente preferirebbe non allontanarsi troppo da \(10^{19}\)–\(5e19\) perché il fission rate dovrebbe derivare dalle condizioni sperimentali 100–119 kW/m.  
Si può variare per sensitivity, ma non trasformarlo in parametro libero arbitrario.

---

## 7. Ultimo codice generato

È stato preparato un file completo autosufficiente:

`UN_M7_global_sensitivity_fullcell.py`

Pensato da copiare in una singola cella notebook.

Scopo:
- sweep globale compatto;
- coalescenza sempre attiva;
- salvataggio progressivo in CSV;
- plots salvati su disco;
- restart possibile se WSL si disconnette.

Variabili esplorate:
- `f_n`
- `K_d`
- `rho_d`
- `fission_rate`
- `Dv_scale`
- `Dg_scale`
- `b_scale`
- `gb_scale`
- `gd_scale`
- `coalescence_d_scale`
- `capture_scale`

Punti importanti del codice:
- non è necessario rieseguire celle precedenti;
- salva in cartella output:
  - `fast_results.csv`
  - final results/top candidates
  - summary `.md`
  - figure diagnostiche;
- se WSL cade, il CSV parziale permette di non perdere tutto;
- `SHOW_PLOTS=False` per alleggerire;
- salva figure in PNG.

Output folder del file più recente:
`UN_M7_global_sensitivity_fullcell`

---

## 8. Scoring usato/da usare

La direzione attuale dello scoring deve essere:

### Priorità alta

1. **Swelling dislocation/P2 vs T**
   - burnup 1.1, 1.3, 3.2% FIMA;
   - soprattutto T fino a ~1700 K, dove ci sono dati.

2. **Raggio dislocation bubbles \(R_d(T)\)** a 1.3% FIMA
   - deve seguire dati sperimentali P2/large bubbles.

3. **Concentrazione \(N_d(T)\)** a 1.3% FIMA
   - non solo livello assoluto;
   - deve scendere ad alta T;
   - importante perché il tutor ha sottolineato la coalescenza.

4. **Caduta alta-T di \(N_d\)**
   - aggiungere un termine specifico, tipo rapporto:
     \[
     N_d(1700)/N_d(1400)
     \]
     oppure penalità se:
     \[
     N_d(1700) \text{ non è abbastanza minore di } N_d(1300-1400)
     \]

### Priorità media

5. **Swelling vs burnup a 1600 K**
   - utile perché ci sono punti a 1.1, 1.3, 3.2% FIMA.

6. **Controllo high-T blow-up**
   - penalizzare se sopra 1700–1800 K lo swelling esplode in modo assurdo;
   - ma non forzare troppo se i dati sperimentali non coprono bene quella zona.

### Priorità bassa / diagnostica

7. **Pressione**
   - utile per capire se il modello è fisico;
   - non è chiaro se metterla forte nello scoring;
   - per ora peso basso.

8. **Gas partition tipo Rizk**
   - utile come confronto qualitativo;
   - non deve dominare il fit perché può dipendere da dettagli di Rizk e da grandezze non sperimentali dirette.

---

## 9. Come interpretare i risultati che l’utente incollerà

Quando l’utente incolla i risultati dell’ultima simulazione, guardare almeno:

1. migliori candidati in `fast_results.csv` o `final_results.csv`;
2. parametri del rank 1, 2, 3;
3. score componenti:
   - `score_swd`
   - `score_Nd`
   - `score_Rd`
   - eventuale `score_Nd_drop`
   - `score_pressure`
   - high-T penalty;
4. grafici:
   - swelling vs T 1.1%;
   - swelling vs T 1.3%;
   - swelling vs T 3.2%;
   - swelling vs burnup at 1600 K;
   - radius at 1.3%;
   - concentration at 1.3%;
   - pressure diagnostic;
   - pressure ratio diagnostic;
   - gas partition 1.1/3.2%;
   - capture diagnostic.
5. soprattutto:
   - \(N_d\) scende davvero a T alta?
   - \(R_d\) segue i dati?
   - lo swelling fit è buono senza blow-up eccessivo?
   - bulk swelling è assurdo o accettabile?
   - la pressione è fisicamente troppo lontana?

---

## 10. Cose da non fare nella nuova chat

Non fare queste cose senza consenso esplicito:

- non sostituire la capture lineare clippata con \(1-\exp(-x)\);
- non disattivare la coalescenza;
- non togliere termini da M7 senza dirlo;
- non cambiare parametri numerici “perché torna meglio” senza etichettarli come scaling/calibrazione;
- non dare per certo che Rizk sia internamente coerente;
- non dire che `capture_fraction_sum > 1` è errore se `f_cap_step <= 1`;
- non rilanciare sweep enormi senza salvataggio progressivo;
- non far dipendere tutto da fission rate troppo lontano dal valore sperimentale.

---

## 11. Possibile strategia prossima

Dopo l’ultimo sweep globale, la strategia consigliata è:

1. leggere i top candidati;
2. separare i candidati in famiglie:
   - buon swelling ma cattivo \(N_d\);
   - buon \(N_d\) ma cattivo swelling;
   - buon raggio ma pressione strana;
   - vicino a Rizk nominale;
   - parametri troppo artificiali;
3. capire quali parametri controllano di più la caduta di \(N_d\):
   - `coalescence_d_scale`
   - `gd_scale`
   - `Dv_scale`
   - `b_scale`
   - `rho_d`
   - `K_d`
   - eventualmente `Dg_scale`
4. fare uno sweep più mirato intorno alla famiglia migliore;
5. confrontare M7 con e senza \(\phi\) nella resolution;
6. se M7 continua a non riprodurre la coalescenza, progettare M8/M9 con modifica fisica dichiarata, ma solo dopo aver capito perché.

---

## 12. Sintesi breve per la nuova IA

Siamo nel progetto UN Thesis. Stiamo calibrando un modello M7 per fission gas swelling in UN, basato su Rizk/Barani/SCIANTIX. M7 include nucleazione bulk accoppiata alla massa gas, phi re-solution, vacancy absorption, coalescenza dislocation bubbles e capture bulk-dislocation. Non bisogna cambiare le equazioni senza dichiararlo. L’obiettivo è fit dei dati sperimentali P2: swelling, radius, concentration. Il problema maggiore è riprodurre la caduta della concentrazione \(N_d\) ad alta temperatura tramite coalescenza, senza distruggere il fit dello swelling. Il bulk swelling non segue bene Rizk, ma per ora conta meno del fit sperimentale. Le pressioni sono diagnostiche, non vanno pesate troppo nello scoring. Abbiamo appena lanciato uno sweep globale con vari scale factors; quando arrivano i risultati bisogna analizzare top candidates, score componenti e grafici, decidendo il prossimo sweep mirato o una variante senza phi nella resolution.
