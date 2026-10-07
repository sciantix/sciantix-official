# RizkUNcalibrazione

## Scopo del file

Questo file raccoglie il metodo di calibrazione sviluppato per il modello UN basato su Rizk/UNSIFGRS, insieme alle principali lezioni emerse da OAT, Optuna e calibrazione manuale.

Lo scopo non è soltanto conservare il **best candidate** attuale, ma creare una procedura riutilizzabile. Se in futuro viene cambiata una parte della fisica — per esempio passando dalle diffusività Rizk 2025 alle diffusività Schneider/Matthews — questo file deve permettere a un nuovo agente/Codex di:

1. capire quali parametri controllano quali output;
2. sapere quali direzioni di variazione sono già state osservate;
3. evitare di ripetere sweep inutili;
4. distinguere miglioramento reale da compensazione tra parametri;
5. rifare la calibrazione in modo ordinato senza modificare simultaneamente la fisica;
6. mantenere sotto controllo il grado di calibrazione rispetto alla baseline.

---

# 1. Obiettivo fisico della calibrazione

Il target principale è la popolazione sperimentale P2 / large intragranular bubbles, interpretata nel modello come **dislocation bubbles**.

Gerarchia pratica adottata:

\[
\boxed{S_d > R_d > N_d}
\]

dove:

- \(S_d\) = swelling da dislocation bubbles;
- \(R_d\) = raggio medio/rappresentativo delle dislocation bubbles;
- \(N_d\) = concentrazione numerica delle dislocation bubbles.

La gerarchia non significa ignorare \(N_d\). Significa che il modello single-size non può necessariamente riprodurre simultaneamente e perfettamente swelling, raggio medio e concentrazione sperimentali, perché:

\[
S_d=N_d\frac{4}{3}\pi\langle R^3\rangle
\]

mentre il modello single-size usa:

\[
S_d=N_d\frac{4}{3}\pi R_{\mathrm{eff}}^3.
\]

In generale:

\[
\langle R^3\rangle \neq \langle R\rangle^3.
\]

Quindi \(S_d\), \(R_d\) e \(N_d\) non devono essere trattati come tre target perfettamente indipendenti e autoconsistenti.

## Uso dei target

- **Swelling P2/dislocation:** target principale su tutti i quattro pin.
- **\(R_d\):** confronto microstrutturale importante soprattutto fino a circa 1500 K per il fit; il plot deve comunque essere mostrato fino a 1800 K.
- **\(N_d\):** confronto importante, soprattutto per evitare soluzioni che ottengono un buon raggio semplicemente riducendo troppo il numero di bolle.
- **Gas partition Rizk Fig. 9:** diagnostica/benchmark di implementazione, non target sperimentale.
- **FGR:** diagnostica nel separate-effects model; per una vera validazione serve un benchmark integrale.
- **Pressioni, \(R_{gf}\), \(N_{gf}\), mass balance:** diagnostica fisica/numerica.

---

# 2. Casi sperimentali da usare sempre

Usare sempre i quattro casi con fission-rate specifico del pin.

| Caso | Burnup [%FIMA] | Linear power [kW/m] | Diametro fuel [mm] | Fission rate [m\(^{-3}\) s\(^{-1}\)] |
|---|---:|---:|---:|---:|
| AP3.2 | 1.1 | 100 | 8.3 | \(5.77\times10^{19}\) |
| AP3.8 | 1.1 | 119 | 8.3 | \(6.86\times10^{19}\) |
| AP3.4 | 1.3 | 130 | 8.3 | \(7.50\times10^{19}\) |
| ANP6 | 3.2 | 125 | 8.0 | \(7.76\times10^{19}\) |

Non usare un unico fission rate rappresentativo per tutti i casi.

Il fission rate entra contemporaneamente in:

- produzione di Xe;
- contributo atermico \(D_3\);
- eventuali termini irradiation-dependent;
- re-solution;
- conversione burnup-tempo.

Per questo deve essere trattato come **input sperimentale case-specific**, non come parametro libero da usare per migliorare il fit.

---

# 3. Physics snapshot della calibrazione corrente

Durante la calibrazione descritta qui la fisica deve rimanere fissa.

Configurazione corrente:

- `RHO_MODE = "constant"`
- `USE_DYNAMIC_RHO_D_NUCLEATION = False`
- bulk-to-dislocation capture = ON
- nucleation mass coupling = ON
- grain-face/intergranular model = ON
- `USE_PHI_GAS_RESOLUTION = False`
- `D2_xe_scale = 0`
- WS gas-trapping switches = OFF
- `XE_DIFFUSIVITY_MODE = "rizk2025_refit_plot"`
- `VU_DIFFUSIVITY_MODE = "rizk2025_refit_full"`
- `DV_GB_MODE = "rizk_legacy_1e6_Dv1"`

Dominio di interesse:

\[
900\leq T\leq1800\ \mathrm{K}.
\]

Durante una campagna di calibrazione non modificare contemporaneamente leggi, closure e parametri. Se cambia la fisica, quella deve essere una nuova baseline.

---

# 4. Working baseline A

La baseline usata per la campagna manuale è:

| Parametro | Baseline A |
|---|---:|
| \(f_n\) | \(5.5\times10^{-4}\) |
| \(K_d\) | \(3.0\times10^5\) bubble/m |
| \(\rho_d\) | \(3.5\times10^{13}\) m\(^{-2}\) |
| \(D_{v,d}\) scale | 10 |
| \(D_{g,d}\) scale | 15 |
| \(N_{gf,0}\) | \(1.0\times10^{13}\) m\(^{-2}\) |

Densità iniziale equivalente di dislocation bubbles:

\[
N_d(0)=K_d\rho_d
      =1.05\times10^{19}\ \mathrm{m^{-3}}.
\]

Questa baseline è già una **working calibrated baseline**, non il set nominale puro del paper Rizk.

---

# 5. Best candidate attuale: L3

Il candidato visualmente preferito dopo il batch v7 è:

\[
\boxed{
f_n=4.2\times10^{-4},
\quad
K_d=4.1\times10^5,
\quad
\rho_d=3.5\times10^{13}
}
\]

\[
\boxed{
D_{v,d}\mathrm{\ scale}=10,
\quad
D_{g,d}\mathrm{\ scale}=17,
\quad
N_{gf,0}=1.0\times10^{13}
}
\]

Densità iniziale equivalente:

\[
N_d(0)=4.1\times10^5\cdot3.5\times10^{13}
      =1.435\times10^{19}\ \mathrm{m^{-3}}.
\]

## 5.1 Quanto è cambiato rispetto alla baseline A?

| Parametro | Baseline A | L3 | Variazione |
|---|---:|---:|---:|
| \(f_n\) | \(5.5\times10^{-4}\) | \(4.2\times10^{-4}\) | \(-23.6\%\) |
| \(K_d\) | \(3.0\times10^5\) | \(4.1\times10^5\) | \(+36.7\%\) |
| \(\rho_d\) | \(3.5\times10^{13}\) | \(3.5\times10^{13}\) | 0% |
| \(D_{v,d}\) scale | 10 | 10 | 0% |
| \(D_{g,d}\) scale | 15 | 17 | \(+13.3\%\) |
| \(N_{gf,0}\) | \(1.0\times10^{13}\) | \(1.0\times10^{13}\) | 0% |
| \(N_d(0)\) | \(1.05\times10^{19}\) | \(1.435\times10^{19}\) | \(+36.7\%\) |

### Interpretazione

Rispetto alla **working baseline A**, L3 non richiede uno stravolgimento:

- tre parametri restano identici;
- \(D_{g,d}\) cambia solo del 13%;
- \(f_n\) cambia di circa il 24%;
- \(K_d\) cambia di circa il 37%.

Quindi L3 è una **moderata calibrazione locale rispetto ad A**.

Tuttavia non va scritto che L3 è “quasi nominale Rizk” in senso assoluto. La baseline A era già calibrata. Per esempio:

- Rizk usa un \(f_n\) nominale dell'ordine di \(10^{-6}\), mentre L3 usa \(4.2\times10^{-4}\);
- il valore nominale comunemente associato a \(K_d\) in Rizk è circa \(5\times10^5\) bubble/m, quindi L3 è relativamente vicino su \(K_d\);
- gli scale factor di diffusione lungo dislocazione sono parametri efficaci e non devono essere presentati come valori direttamente misurati.

Conclusione corretta:

\[
\boxed{\text{L3 è poco distante dalla nostra baseline calibrata, ma la baseline stessa non è nominale pura.}}
\]

Questo è importante per l'obiettivo della tesi: ottenere un buon fit senza nascondere il livello reale di calibrazione.

---

# 6. Stato del fit di L3

Il candidato L3 mostra un buon accordo visivo sullo swelling per:

- AP3.8;
- AP3.4;
- ANP6.

Il disaccordo principale rimane AP3.2.

Questa osservazione è particolarmente interessante perché:

- AP3.2 e AP3.8 hanno entrambi 1.1% FIMA;
- differiscono soprattutto nella linear power / fission rate;
- AP3.2 è il caso a fission rate più basso.

Quindi la working hypothesis è:

\[
\boxed{\text{la dipendenza del modello da }\dot F\text{ potrebbe essere troppo debole o non corretta}}
\]

nel separare AP3.2 dagli altri casi.

Questa è **un'ipotesi diagnostica, non ancora una prova**. Va tenuto presente anche lo scatter sperimentale tra AP3.2 e AP3.8.

Al punto \(T=1600\) K, L3 fornisce indicativamente:

| Caso | \(S_d\) [%] | \(R_d\) [nm] | \(N_d\) [m\(^{-3}\)] |
|---|---:|---:|---:|
| AP3.2 | 2.72 | 79.7 | \(1.28\times10^{19}\) |
| AP3.8 | 2.65 | 79.0 | \(1.29\times10^{19}\) |
| AP3.4 | 2.68 | 79.3 | \(1.28\times10^{19}\) |
| ANP6 | 3.01 | 82.8 | \(1.27\times10^{19}\) |

Il fatto che AP3.2 e AP3.8 restino molto vicini nel modello, nonostante la differenza di power/fission rate, è esattamente il punto da investigare.

---

# 7. OAT — mappa direzionale dei parametri

La campagna OAT ha incluso circa 988 simulazioni:

- 13 configurazioni;
- 4 pin;
- 19 temperature tra 900 e 1800 K.

La tabella seguente è una **mappa locale/qualitativa**, non una legge universale.

Segno riferito a **incremento del parametro**.

| Parametro aumentato | \(S_d\) | \(R_d\) | \(N_d\) | Gas matrix | Gas bulk | Gas dislocation | \(R_{gf}\) | \(N_{gf}\) | FGR |
|---|---|---|---|---|---|---|---|---|---|
| \(f_n\uparrow\) | ↓↓↓ | ↓↓↓ | ↑ | ↓↓↓ | ↑↑ | ↓↓↓ | ↓↓ | ↑ | ↓↓↓ |
| \(K_d\uparrow\) | ↓ / non-monotono | ↓↓↓ | ↑↑↑ | ~ | ↑ lieve | ↓ | ↑ lieve | ↓ lieve | ↑ lieve |
| \(\rho_d\uparrow\) | ↑↑ | ↑ / mixed | ↑↑ | ↓ lieve | ↓↓ | ↑↑↑ | ↓ | ↑ | ↓↓ |
| \(D_{v,d}\uparrow\) | ↑ lieve | ↑ lieve | ~ / ↓ | poco | poco | poco | — | — | poco |
| \(D_{g,d}\uparrow\) | ↑↑↑ | ↑↑↑ | ↓ | ↓ lieve | ↓↓↓ | ↑↑↑ | ↓ | ↑ | ↓↓↓ |
| \(N_{gf,0}\uparrow\) | ~ intragranulare | ~ | ~ | ~ | ~ | ~ | ↓↓↓ | ↑↑↑ | ↑↑ |

Legenda qualitativa:

- `↑↑↑` = effetto forte;
- `↑↑` = medio-forte;
- `↑` = apprezzabile;
- `~` = piccolo/trascurabile nel dominio testato.

## 7.1 Leve principali

### \(f_n\)

Controlla la nucleazione delle bulk bubbles:

\[
\nu_b=8\pi f_nD_g\Omega_{fg}^{1/3}c^2.
\]

Aumentare \(f_n\):

- crea più bulk bubbles;
- aumenta il sink bulk;
- sottrae gas alle dislocation bubbles;
- riduce \(R_d\) e \(S_d\).

Ridurre \(f_n\) fa l'opposto.

**Uso pratico:** leva per recuperare swelling/raggio quando \(K_d\) è stato aumentato.

---

### \(K_d\)

Nel modello:

\[
N_d(0)=K_d\rho_d.
\]

Aumentare \(K_d\):

- aumenta fortemente \(N_d\);
- distribuisce il gas su più bolle;
- riduce il raggio medio;
- tende a ridurre lo swelling.

**Uso pratico:** leva principale quando il modello ha \(N_d\) troppo basso e \(R_d\) troppo alto.

---

### \(\rho_d\)

È una leva molto forte perché agisce simultaneamente su:

- numero/sink delle dislocation;
- trapping;
- gas partition;
- swelling;
- FGR.

Aumentare \(\rho_d\) tende a:

- aumentare \(N_d\);
- aumentare gas trattenuto sulle dislocazioni;
- aumentare lo swelling dislocation;
- ridurre FGR.

**Uso pratico:** da modificare solo dopo aver esaurito i leveraggi più locali, perché cambia molte quantità contemporaneamente.

---

### \(D_{g,d}\)

È una leva molto efficace sul trasporto/trapping associato alle dislocation.

Aumentarlo tende a:

- aumentare \(R_d\);
- aumentare \(S_d\);
- diminuire \(N_d\);
- spostare gas dal bulk verso le dislocation bubbles;
- ridurre FGR.

**Uso pratico:** forte leva di forma dello swelling, ma rischia di alterare troppo la gas partition.

---

### \(D_{v,d}\)

Nel range locale esplorato è risultato sorprendentemente poco efficace.

Lo sweep 10 → 15 → 20 → 30 ha modificato poco le curve principali.

**Decisione corrente:** tenere \(D_{v,d}=10\), salvo cambio della legge di diffusività o nuova sensitivity.

---

### \(N_{gf,0}\)

Agisce quasi esclusivamente sul comparto intergranulare/FGR.

**Decisione corrente:** non usarlo per correggere il fit P2 intragranulare. Lasciarlo alla fase finale.

---

# 8. Lezioni dai singoli batch manuali

## 8.1 Batch iniziale — variazioni \(\rho_d,K_d,D_g\)

Baseline A:

\[
f_n=5.5\times10^{-4},
\quad
K_d=3.0\times10^5,
\quad
\rho_d=3.5\times10^{13},
\quad
D_{g,d}=15.
\]

Sono stati testati:

- \(\rho_d\downarrow\);
- \(K_d\uparrow\);
- \(D_{g,d}\uparrow\).

Lezione:

- \(\rho_d\downarrow + D_g\uparrow\) può aumentare swelling ma peggiorare \(N_d\);
- \(K_d\uparrow\) migliora \(N_d\), ma abbassa troppo \(R_d\) e swelling;
- \(D_g\uparrow\) può recuperare swelling/raggio dopo \(K_d\uparrow\), ma sposta molto gas sulle dislocazioni.

Questa è la prima evidenza della compensazione:

\[
K_d\uparrow
\quad\leftrightarrow\quad
D_g\uparrow.
\]

---

## 8.2 Sweep \(D_{v,d}\)

Valori testati:

\[
10,\ 15,\ 20,\ 30.
\]

Lezione:

\[
\boxed{D_{v,d}\text{ non è una buona leva locale nella regione corrente}}
\]

e quindi non va usato come primo parametro di fine tuning.

---

## 8.3 Sweep \(f_n\) a \(K_d=4\times10^5\)

Valori:

\[
5.5,\ 4,\ 3,\ 2\times10^{-4}.
\]

Lezione:

- \(f_n\downarrow\) aumenta \(R_d\) e \(S_d\);
- \(N_d\) cambia poco;
- troppo basso porta a swelling e gas-dislocation eccessivi.

Questo ha mostrato una compensazione molto utile:

\[
\boxed{K_d\uparrow,\quad f_n\downarrow}
\]

per alzare \(N_d\) senza perdere completamente raggio e swelling.

---

## 8.4 Sweep diagonale \(K_d-f_n\)

Test principali:

- A: \(K_d=3.0e5,\ f_n=5.5e-4\)
- D1: \(K_d=3.2e5,\ f_n=5.1e-4\)
- D2: \(K_d=3.5e5,\ f_n=4.7e-4\)
- D3: \(K_d=3.7e5,\ f_n=4.3e-4\)

Lezione:

D1 è risultato un ottimo compromesso:

- swelling praticamente invariato rispetto ad A;
- \(N_d\) migliore;
- raggio ancora buono.

Questo ha dimostrato che non serve modificare un solo parametro alla volta fino all'estremo: è più utile muoversi lungo una direzione compensata fisicamente comprensibile.

---

## 8.5 Sweep \(D_{g,d}\) attorno a D1

Valori:

\[
D_{g,d}=14,\ 15,\ 16,\ 17.
\]

Risultato:

- 15 era il miglior compromesso numerico;
- 16–17 miglioravano alcuni pin e il raggio;
- \(D_g=17\) produceva un risultato **visivamente interessante**, soprattutto AP3.8 e ANP6.

Lezione fondamentale:

\[
\boxed{\text{non selezionare il candidato solo dal global score}}
\]

Un singolo score può penalizzare troppo un pin sperimentalmente disperso o un osservabile secondario.

Il contatto visivo con:

- tutti i quattro swelling plots;
- \(R_d\);
- \(N_d\);
- gas partition;

resta obbligatorio.

---

## 8.6 Sweep \(K_d\) attorno al candidato \(D_g=17\)

Con:

\[
f_n=5.1e-4,\quad D_g=17
\]

sono stati testati:

\[
K_d=3.2,\ 3.5,\ 3.8,\ 4.1\times10^5.
\]

Lezione:

- aumentando \(K_d\), \(N_d\) migliora;
- AP3.2/AP3.4 vengono ridotti;
- \(R_d\), AP3.8 e ANP6 tendono a scendere.

Il punto \(K_d=4.1e5\) è risultato un candidato molto buono, poi usato come base del v7.

---

## 8.7 Sweep \(f_n\) attorno a \(K_d=4.1e5\)

Valori:

\[
f_n=5.1,\ 4.8,\ 4.5,\ 4.2\times10^{-4}.
\]

Il candidato preferito visivamente è diventato:

\[
\boxed{L3:\ f_n=4.2\times10^{-4}}
\]

con gli altri parametri fissi.

Lezione:

ridurre moderatamente \(f_n\) ha recuperato raggio/swelling nei pin ad alta potenza senza distruggere \(N_d\).

Il residuo principale è AP3.2.

---

# 9. Lezioni da Optuna

È stata eseguita una campagna NSGA-II multiobjective di 60 trial.

Obiettivi:

- swelling sui quattro pin;
- microstruttura \(R_d,N_d\);
- FGR diagnostico.

La baseline A è rimasta molto competitiva.

Un candidato Optuna poteva migliorare leggermente lo swelling aggregato, ma a costo di:

- microstruttura peggiore;
- FGR peggiore;
- gas partition molto più lontana;
- parametri meno interpretabili.

Lezione:

\[
\boxed{\text{un optimizer non sostituisce la diagnosi fisica}}
\]

Il valore principale di Optuna è stato:

1. confermare che A era già in una regione buona;
2. evidenziare famiglie di compensazione;
3. mostrare che alcuni mismatch high-T sono strutturali;
4. suggerire dove NON vale la pena spingere i parametri.

Nei trial competitivi sono emerse correlazioni di compensazione, per esempio:

- \(K_d-\rho_d\): forte anticorrelazione;
- \(D_v-D_g\): anticorrelazione.

Queste correlazioni **non sono Sobol indices** e non devono essere interpretate come sensitività fisiche universali. Sono segnali di degenerazione/compensazione all'interno dello spazio calibrato.

---

# 10. Limite strutturale high-T osservato

In gran parte della campagna:

- \(R_d\) tende a risultare basso o a mostrare andamento non perfettamente coerente ad alta T;
- \(N_d\) tende a risultare troppo alto o comunque difficile da conciliare simultaneamente con \(R_d\);
- la relazione single-size lega fortemente swelling, raggio e numero.

Questo supporta l'idea che parte del mismatch non sia correggibile semplicemente scegliendo un valore migliore di \(K_d\) o \(f_n\).

Possibili cause/model-form uncertainty:

- single-size approximation;
- distribuzione reale delle bubble sizes;
- closure di coalescenza;
- nucleazione dimerica/effective nucleation;
- trapping/re-solution dipendenti dal raggio;
- \(\rho_d\) costante;
- diffusività effective;
- fission-rate dependence incompleta.

Non modificare queste leggi durante la stessa calibrazione. Devono essere test separati e dichiarati.

---

# 11. Metodo di calibrazione raccomandato

## Fase A — fissare la fisica

Prima di calibrare:

- fissare le diffusività;
- fissare le closure;
- fissare \(\rho_d\) constant/dynamic;
- fissare capture ON/OFF;
- fissare re-solution model;
- fissare coalescence model;
- fissare grain-boundary model.

Una calibrazione è interpretabile soltanto se durante lo sweep cambia **il parametro dichiarato**, non la struttura del modello.

---

## Fase B — definire una baseline tracciabile

Salvare sempre:

- nome/versione del codice;
- tutti i parametri;
- diffusivity modes;
- case-specific fission rates;
- timestep;
- number of modes;
- output CSV;
- contact sheet.

La baseline deve poter essere rieseguita identica.

---

## Fase C — OAT

Prima di usare optimizer:

1. scegliere range fisicamente plausibili;
2. fare low/base/high per ogni parametro;
3. osservare direzione e magnitudine degli effetti;
4. costruire una tabella come quella della Sezione 7.

Non assumere che la direzione OAT rimanga identica molto lontano dalla baseline.

---

## Fase D — identificare compensazioni

Cercare coppie del tipo:

\[
\text{parametro A migliora }N_d\text{ ma peggiora }R_d
\]

e

\[
\text{parametro B recupera }R_d\text{ senza annullare il miglioramento di }N_d.
\]

Esempio osservato:

\[
K_d\uparrow
\Rightarrow
N_d\uparrow,\ R_d\downarrow,\ S_d\downarrow
\]

compensato da:

\[
f_n\downarrow
\Rightarrow
R_d\uparrow,\ S_d\uparrow.
\]

Fare quindi sweep diagonali piccoli invece di esplorazioni casuali.

---

## Fase E — massimo 4–10 casi per iterazione

Ogni iterazione manuale deve avere una domanda precisa.

Esempi:

- “Quanto \(K_d\) posso aumentare prima di perdere AP3.8?”
- “Quanto \(f_n\) devo ridurre per recuperare il raggio?”
- “\(D_g\) modifica davvero la separazione tra pin a diverso fission rate?”
- “Il nuovo set migliora \(N_d\) senza spostare troppo la gas partition?”

Se non si riesce a formulare la domanda, lo sweep non è abbastanza mirato.

---

## Fase F — contact sheet unica

Per ogni run usare una sola figura con:

1. swelling AP3.2;
2. swelling AP3.8;
3. swelling AP3.4;
4. swelling ANP6;
5. \(R_d\) 900–1800 K;
6. \(N_d\) 900–1800 K;
7. gas partition 1.1% FIMA;
8. gas partition 3.2% FIMA.

Non creare decine di plot separati durante la calibrazione ordinaria.

---

# 12. Regola anti-overfitting / anti-overcalibrazione

Un candidato migliore non è automaticamente preferibile se richiede parametri molto più lontani dalla baseline.

Per ogni candidato calcolare almeno:

\[
r_i=\frac{p_i}{p_{i,\mathrm{base}}}
\]

oppure:

\[
\Delta_i=\log_{10}\left(\frac{p_i}{p_{i,\mathrm{base}}}\right).
\]

Classificare ogni parametro come:

- invariato;
- modifica lieve: <20%;
- modifica moderata: 20–50%;
- modifica forte: 50–100%;
- modifica molto forte: > fattore 2.

Una metrica semplice di “calibration distance” può essere:

\[
D_{cal}
=
\sqrt{
\frac{1}{n}
\sum_i
\left[
\log_{10}
\left(
\frac{p_i}{p_{i,\mathrm{base}}}
\right)
\right]^2
}.
\]

Questa metrica non sostituisce la fisica, ma aiuta a distinguere:

- best fit;
- best physically conservative fit.

Per L3 rispetto ad A la calibrazione aggiuntiva è moderata.

---

# 13. Sensitivity analysis da fare dopo la calibrazione

La sensitivity deve essere distinta dalla calibrazione.

## 13.1 Local sensitivity attorno al best candidate

Per ogni parametro \(p\), perturbare per esempio:

\[
p\times0.8,\quad p,\quad p\times1.2
\]

e calcolare per ogni output:

\[
S_{y,p}
=
\frac{\Delta y/y}{\Delta p/p}.
\]

Output consigliati:

- \(S_d(T)\) per ciascun pin;
- \(R_d(T)\);
- \(N_d(T)\);
- gas partition;
- FGR diagnostico.

La sensitivity deve essere riportata anche per temperatura, perché un parametro può essere irrilevante a 1000 K e dominante a 1700 K.

---

## 13.2 Global sensitivity

Dopo la local sensitivity:

- LHS/Morris per screening;
- Sobol solo sui parametri rimasti importanti.

Parametri prioritari:

\[
f_n,\quad K_d,\quad\rho_d,\quad D_{g,d},\quad D_{v,d}.
\]

Per il solo P2 intragranulare, \(N_{gf,0}\) è secondario e può essere escluso dalla prima SA.

Non usare \(\dot F\) come free calibration parameter. Se si vuole studiarne la sensitività, farlo come **input physics sensitivity** attorno ai valori sperimentali.

---

# 14. Procedura quando si cambia diffusività: esempio Matthews

Se si passa dalla diffusività Rizk alla diffusività Schneider/Matthews, **non trasferire automaticamente la calibrazione L3 come se nulla fosse cambiato**.

Procedura raccomandata:

## Step 1 — physics swap isolato

Eseguire:

- baseline A con diffusività Rizk;
- stessi identici parametri con diffusività Matthews.

L'unica differenza deve essere la diffusività.

Questo mostra l'effetto puro della nuova lower-length-scale physics.

## Step 2 — test del candidato precedente

Eseguire L3 con la nuova diffusività senza retuning.

Serve a capire quanto della vecchia calibrazione sopravvive.

## Step 3 — nuovo OAT locale

Rifare almeno:

- \(f_n\);
- \(K_d\);
- \(\rho_d\);
- \(D_{g,d}\);
- \(D_{v,d}\).

Le direzioni OAT precedenti possono essere usate come prior qualitativa, ma non date per scontate.

## Step 4 — minimal recalibration

Calibrare inizialmente il minor numero possibile di parametri.

Ordine consigliato:

1. \(K_d\) per \(N_d\);
2. \(f_n\) per compensare \(R_d/S_d\);
3. \(D_{g,d}\) per la forma/trasporto;
4. \(\rho_d\) solo se resta un mismatch globale;
5. \(D_{v,d}\) solo se la nuova diffusività lo rende nuovamente sensibile.

## Step 5 — confrontare “best fit” e “least calibrated”

Riportare almeno due candidati:

- **best numerical/visual fit**;
- **best conservative candidate**, cioè con minore distanza dalla baseline fisica.

Questo è particolarmente importante se Matthews riduce la necessità di scale factor artificiali.

---

# 15. Problema aperto prioritario: dipendenza dal fission rate

L3 riproduce molto bene tre casi ma sovrastima soprattutto AP3.2.

Dato che AP3.2 è il caso a power/fission rate più basso, il prossimo studio non dovrebbe essere un altro tuning casuale dei parametri.

Prima domanda:

\[
\boxed{
\text{il modello produce una separazione sufficiente tra AP3.2 e AP3.8 quando cambia }\dot F?
}
\]

Test suggeriti:

1. fissare \(T\) e burnup;
2. variare \(\dot F\) su una griglia che includa i quattro valori sperimentali;
3. osservare separatamente:
   - produzione gas;
   - \(D_3\);
   - trapping bulk;
   - trapping dislocation;
   - re-solution;
   - \(S_d\);
   - \(R_d\);
   - \(N_d\);
4. calcolare una elasticità locale:

\[
S_{S_d,\dot F}
=
\frac{\Delta S_d/S_d}{\Delta\dot F/\dot F}.
\]

Questo serve a capire se il problema è realmente nella dipendenza da fission rate o se AP3.2 è semplicemente un punto sperimentale non compatibile con gli altri all'interno del modello single-material-point.

---

# 16. Cose da non fare

1. Non modificare una legge fisica e poi chiamare il miglioramento “calibrazione del parametro”.
2. Non usare la gas partition di Rizk come dato sperimentale.
3. Non calibrare il fission rate.
4. Non scegliere il best candidate solo da un singolo score.
5. Non ottimizzare \(R_d\) ignorando \(N_d\).
6. Non ottimizzare \(N_d\) sacrificando completamente swelling/raggio.
7. Non usare \(N_{gf,0}\) per correggere un problema intragranulare.
8. Non aumentare \(D_g\) indefinitamente: può spostare artificialmente troppo gas sulle dislocation bubbles.
9. Non usare \(D_v\) come knob principale se la local sensitivity continua a mostrarlo quasi inerte.
10. Non rilanciare grandi optimizer prima di aver capito le direzioni OAT.
11. Non trasferire ciecamente la calibrazione da una legge di diffusività a un'altra.
12. Non nascondere che la baseline A stessa è già calibrata.

---

# 17. Decisione corrente

Per i plot e per il seguito della calibrazione, il candidato attuale da conservare è:

\[
\boxed{\text{L3}}
\]

con:

\[
f_n=4.2\times10^{-4},
\quad
K_d=4.1\times10^5,
\quad
\rho_d=3.5\times10^{13},
\]

\[
D_{v,d}=10,
\quad
D_{g,d}=17,
\quad
N_{gf,0}=1.0\times10^{13}.
\]

È un candidato interessante perché:

- riproduce bene tre dei quattro dataset di swelling;
- mantiene una microstruttura ragionevole;
- non richiede una grossa modifica aggiuntiva rispetto alla working baseline A;
- lascia un mismatch molto chiaro e interpretabile su AP3.2, utile per diagnosticare la dipendenza da fission rate.

Non va ancora chiamato “final calibrated model”.

La formulazione consigliata è:

> **current best / provisional calibration candidate**

fino a quando non vengono completati:

1. test della dipendenza da fission rate;
2. sensitivity analysis;
3. eventuale confronto con la diffusività Matthews;
4. verifica numerica finale;
5. scelta esplicita tra best-fit e least-calibrated candidate.

---

# 18. Riferimenti / documenti di progetto utili

- Rizk et al., *Mechanistic nuclear fuel performance modeling of uranium nitride*, JNM 606 (2025) 155604.
- Schneider et al., *Ab-initio informed cluster dynamics simulation of self- and Xe diffusivity in uranium mononitride under irradiation*, JNM 620 (2026) 156360.
- Schneider et al., *Integrating mechanistic models with separate effects and integral tests through neural network-enabled Bayesian inference: Application to uranium nitride fuel swelling*, JNM 632 (2026) 156895.
- Matthews et al., LA-UR-25-28152 (2025).
- Ronchi et al. (1978), microscopic swelling/P2 database.
- `05-10-2026_UN_bibliografia_consolidata_pronta_tesi_D3_VERIFICATO.md`
- `UN_sensitivity_analysis_summary.md`
- `UN_M7_calibration_lessons_report.md`
- notebook/results della serie `Rizk_Rho_Constant_manual_*`.

---

# 19. Sintesi operativa per Codex

Quando si riparte da questo report:

1. **Non cambiare la fisica senza dichiararlo.**
2. Usa tutti e quattro i fission rate sperimentali.
3. Parti dalla baseline o dal current best candidate.
4. Fai OAT piccolo prima di ogni optimizer.
5. Usa la mappa direzionale come prior, non come verità universale.
6. Cerca compensazioni \(K_d-f_n\), poi eventualmente \(K_d-D_g\).
7. Tieni \(D_v\) fisso finché non torna sensibile.
8. Non usare \(N_{gf,0}\) per correggere P2.
9. Giudica sempre i quattro swelling plot insieme.
10. Controlla \(R_d\) e \(N_d\) per evitare compensazioni non fisiche.
11. Mantieni gas partition/FGR come diagnostiche.
12. Riporta sempre sia qualità del fit sia distanza dalla baseline.
13. Se cambia \(D(T,\dot F)\), rifai almeno un OAT locale.
14. Il problema aperto prioritario del candidato L3 è la risposta al fission rate, soprattutto la separazione AP3.2–AP3.8.
