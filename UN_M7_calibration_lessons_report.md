# UN M7 — Report ragionato sulla calibrazione del modello

**Data:** 2026-05-01  
**Oggetto:** stato della calibrazione del modello UN/M7 per fission gas swelling, dopo:
- sweep `capture_only` fixed-\(D_v\) eseguito da Codex/PC2;
- rerun esatti dei 4 candidati Codex;
- primo run `v3` su `capture_only` con vincoli su gas partition e `q_gb`.

---

## 1. Scopo del lavoro

L’obiettivo non è semplicemente ottenere lo score più basso, ma arrivare a un modello **fisicamente difendibile** per la tesi, cioè capace di riprodurre almeno in modo qualitativo e quantitativo ragionevole:

1. swelling P2 / dislocation bubbles;
2. raggio medio delle bolle grandi;
3. concentrazione numerica delle bolle grandi;
4. pressione interna rispetto alla pressione di equilibrio;
5. partizione del gas tra matrice, bulk bubbles, dislocation bubbles e gas arrivato al grain face;
6. parametri non eccessivamente lontani da Rizk, salvo parametri già sospetti o ereditati.

Il confronto principale resta con la popolazione P2 / large intragranular bubbles, interpretata da Rizk come popolazione di **dislocation bubbles**. Bulk bubbles e grain-boundary/release sono diagnostiche fisiche importanti, ma non sono tutte direttamente confrontabili con i dati P2.

---

## 2. Modelli/famiglie considerate

### 2.1 Baseline concettuale

La struttura comune è:

- gas in soluzione \(c\);
- gas in bulk bubbles \(m_b\);
- gas in dislocation bubbles \(m_d\);
- diffusione al grain face, accumulata in `q_gb`;
- trapping verso bulk e dislocation bubbles;
- re-solution;
- vacancy absorption;
- coalescenza dislocation-dislocation;
- eventuale cattura bulk→dislocation.

### 2.2 Differenza tra `capture_only` e `M7_no_phi`

Nel codice attuale:

| famiglia | phi-resolution | nucleation mass coupling | bulk→dislocation capture |
|---|---:|---:|---:|
| `baseline` | no | no | no |
| `capture_only` | no | no | sì |
| `M7_no_phi` | no | sì | sì |
| `M7_full` | sì | sì | sì |

La differenza tra `capture_only` e `M7_no_phi` è quindi il termine di **nucleation mass coupling**:

\[
S_c = \beta - 2\nu_b,
\qquad
S_{m_b}=2\nu_b .
\]

Tuttavia questo termine è concettualmente delicato nel single-size model: la nucleazione aumenta il numero di bolle, ma interpretare rigidamente la massa come formazione di dimeri da 2 atomi può essere una forzatura, soprattutto se \(f_n\) è un parametro efficace e non un parametro misurato. Per questo motivo, allo stato attuale, `capture_only` è la famiglia che si sta privilegiando per l’esplorazione, non perché sia “semplice” in assoluto, ma perché evita questo ulteriore accoppiamento massa-nucleazione.

### 2.3 `capture_only` non è Rizk puro

`capture_only` conserva il termine bulk→dislocation capture, cioè un termine Barani-like:

\[
f_{\mathrm{cap}}
=
\min(1,\max(0, s_{\mathrm{cap}} N_d \Delta V_{\mathrm{cap}})).
\]

Quindi è meglio descriverlo come:

\[
\text{Rizk-like + bulk→dislocation capture}
\]

e non come Rizk puro.

---

## 3. Risultati Codex/PC2 su `capture_only` fixed-\(D_v\)

Codex ha eseguito uno sweep fixed-\(D_v\) su 7 valori:

\[
D_v = 0.03,\ 0.05,\ 0.10,\ 0.15,\ 0.30,\ 0.50,\ 1.00
\]

con 100 trial per valore, quindi circa 700 trial Optuna più rerun finali.

### 3.1 Conclusione principale del PC2

Il miglior candidato numerico era:

| categoria | \(D_v\) | score | problema |
|---|---:|---:|---|
| best overall | 0.05 | 1.286 | pressione dislocation troppo alta |

Il candidato più interessante fisicamente era:

| categoria | \(D_v\) | score | \(p_d/p_{eq}\) max 1200–1700 K |
|---|---:|---:|---:|
| best balanced | 1.0 | 1.337 | circa 2.34 |

Questo è stato un risultato importante: `capture_only` con \(D_v=1\) riusciva a dare un fit decente con pressione molto più ragionevole rispetto ai candidati a \(D_v\) basso.

### 3.2 Problema emerso nei rerun esatti dei candidati Codex

Il candidato `best_balanced` aveva pressione relativamente buona, ma una partizione gas non soddisfacente:

- troppo gas verso `q_gb` già a 1600 K;
- bulk gas troppo basso rispetto alla logica della Fig. 9 di Rizk;
- `pressure_friendly` migliorava pochissimo la pressione ma peggiorava fit e partizione.

Conclusione:

\[
\boxed{
\text{v2 non era sufficiente: serviva uno score con gas partition e } q_{gb}.
}
\]

---

## 4. Interpretazione di `q_gb`

Nel modello attuale `q_gb` non è un modello completo di grain-boundary bubble evolution e fission gas release. È il gas arrivato al grain face tramite il bilancio della diffusione intragranulare.

Per confronto con Rizk Fig. 9, la cosa più coerente è interpretarlo come:

\[
q_{gb}^{model}
\approx
\text{gas in grain-boundary bubbles}
+
\text{fission gas release}.
\]

Questa interpretazione è utile perché Rizk separa grain-boundary bubbles e FGR, mentre il nostro modello attuale no.

Quindi nello scoring ha senso vincolare:

- `q_gb` basso/moderato sotto circa 1700 K;
- `q_gb` non nullo ad alta T, ma non dominante;
- bulk gas dominante sotto ~1700–1800 K;
- dislocation gas dominante sopra ~2000 K.

---

## 5. Run PC1 `v3` con gas partition score

È stato creato un primo `v3` con:

- family: `capture_only`;
- \(D_v\) libero nel range 0.3–1.5;
- pressione più severa rispetto a v2;
- nuovo score su gas partition;
- nuovo score su `q_gb`;
- weak guardrail su raggio high-T.

### 5.1 Risultato positivo

Il vincolo su gas partition ha funzionato: i migliori candidati `v3` portano la partizione molto più vicino alla logica di Rizk.

Per il miglior candidato finale `v3`:

| quantità | valore circa |
|---|---:|
| bulk gas, 1.1% FIMA, 1600 K | 57.4% |
| dislocation gas, 1.1% FIMA, 1600 K | 34.3% |
| qgb, 1.1% FIMA, 1600 K | 8.0% |
| bulk gas, 1.1% FIMA, 2000 K | 1.7% |
| dislocation gas, 1.1% FIMA, 2000 K | 87.2% |
| qgb, 1.1% FIMA, 2000 K | 11.1% |

Questa è molto più coerente con la Fig. 9 di Rizk rispetto ai candidati v2.

### 5.2 Nuovo problema emerso

Il prezzo pagato è un raggio delle dislocation bubbles troppo grande ad alta temperatura.

Per i top candidati `v3`:

| candidato | \(R_d(1800\,K)\) | \(R_d(2000\,K)\) | rapporto |
|---|---:|---:|---:|
| rank1 | ~393 nm | ~2061 nm | ~5.25 |
| rank2 | ~394 nm | ~1986 nm | ~5.04 |
| rank3 | ~394 nm | ~2038 nm | ~5.17 |
| rank4 | ~388 nm | ~2028 nm | ~5.23 |

Questo conferma l’osservazione visiva: la curva \(R_d(T)\) non satura come in Rizk, ma continua a crescere rapidamente dopo 1800 K.

---

## 6. Diagnosi fisica del problema del raggio dislocation

### 6.1 Relazione tra swelling, concentrazione e raggio

Per una popolazione di bolle:

\[
\frac{\Delta V}{V} = N_d V_d
\]

e, assumendo bolle sferiche:

\[
R_d \propto \left(\frac{\Delta V/V}{N_d}\right)^{1/3}.
\]

Quindi il raggio cresce se:

1. aumenta lo swelling dislocation;
2. diminuisce \(N_d\);
3. entrambe le cose avvengono insieme.

Nei candidati v3 succede proprio questo:
- la partizione spinge molto gas verso le dislocation bubbles ad alta T;
- la coalescenza riduce \(N_d\);
- il gas resta in larga parte nelle dislocation bubbles;
- quindi il volume medio per bolla cresce troppo.

### 6.2 Ruolo della coalescenza

La coalescenza dislocation-dislocation riduce \(N_d\). A parità di gas/swelling, ridurre \(N_d\) aumenta il raggio medio.

I top v3 usano:

\[
\text{coalescence\_d\_scale} \simeq 6\text{--}6.6
\]

cioè un valore alto. Questo aiuta probabilmente a riprodurre la caduta di \(N_d\), ma porta a un raggio eccessivo.

Quindi è plausibile che lo scoring stia premiando troppo la riduzione di \(N_d\) / trasferimento gas a dislocation rispetto al controllo del raggio high-T.

### 6.3 Ruolo di \(g_d\), \(b\), \(D_v\)

I top v3 convergono verso:

| parametro | trend nei top v3 |
|---|---|
| \(D_v\) | ~0.41–0.46 |
| \(D_g\) | ~1.2–1.3 |
| \(D2_{Xe}\) | ~2.7–4.7 |
| \(b\) | ~0.22–0.25 |
| \(g_b\) | ~2–4 |
| \(g_d\) | ~5.6–6.0 |
| coalescence | ~6–6.6 |
| capture | ~0.33–0.55 |
| \(K_d\) | ~0.74–0.95e6 |
| \(\rho_d\) | ~1.25–1.51e13 |

Lettura:
- \(g_d\) alto spinge gas verso dislocations;
- \(b\) basso riduce la re-solution, quindi il gas resta nelle bolle;
- coalescenza alta riduce \(N_d\);
- \(D_v < 1\) rallenta parzialmente la crescita vacancy, ma non basta a impedire il raggio micrometrico ad alta T.

---

## 7. Confronto qualitativo con Rizk

Rizk mostra:

- \(N_d\) circa costante sotto ~1400 K;
- \(N_d\) che diminuisce ad alta T per coalescenza;
- \(R_d\) che cresce con T;
- bulk bubbles che tengono la maggior parte del gas sotto ~1800 K;
- gas che transiziona verso dislocation bubbles sopra ~1800–2000 K.

Il nostro v3 riproduce bene la **partizione gas** e qualitativamente anche la caduta di \(N_d\), ma il raggio \(R_d\) cresce troppo dopo 1800 K.

Attenzione: il raggio di Rizk a 2000 K non va preso come verità assoluta, perché:
- i raggi sperimentali sono ricavati indirettamente da swelling e concentrazione;
- alcune popolazioni sotto soglia non sono osservate;
- ad alta T entrano in gioco grain-boundary bubbles, release e coarsening non completamente rappresentati nel nostro modello.

Tuttavia \(R_d \sim 2\,\mu m\) a 2000 K è probabilmente un campanello d’allarme, non solo un dettaglio grafico.

---

## 8. Cosa significa il buon comportamento del bulk

Dai plot si vede che il bulk radius tende a saturare in modo più simile a Rizk, mentre il dislocation radius no.

Questo è coerente con la struttura del modello:

- le bulk bubbles sono numerose;
- la nucleazione bulk continua a fornire molte bolle;
- quando il gas passa alle dislocations, il bulk perde gas e si stabilizza;
- quindi il raggio medio bulk non esplode.

Le dislocation bubbles, invece:
- sono meno numerose;
- subiscono coalescenza;
- diventano il serbatoio principale del gas ad alta T;
- quindi se non esiste un ulteriore meccanismo di limitazione, il raggio cresce troppo.

---

## 9. Ipotesi sui meccanismi mancanti o mal tarati

Il problema del raggio ad alta T potrebbe indicare:

1. coalescenza dislocation troppo forte;
2. \(g_d\) troppo alto;
3. re-solution \(b_d\) troppo bassa;
4. capture bulk→dislocation troppo efficace ad alta T;
5. mancanza di un sink fisico dalle dislocation bubbles verso grain boundaries/release;
6. assenza di un modello completo di intergranular bubbles/FGR;
7. problema nel valore o nella forma di \(D_v\);
8. problema nell’interpretazione single-size della popolazione dislocation.

La soluzione non deve essere scelta “a occhio”: bisogna fare studi mirati.

---

## 10. Cosa fare nello score successivo (`v3b`)

### 10.1 Mantenere

Mantenere:
- score su P2 swelling;
- score su \(R_d\) e \(N_d\);
- pressure score;
- prior Rizk;
- gas partition score;
- qgb score.

### 10.2 Rafforzare il guardrail sul raggio high-T

Aggiungere un vincolo più serio, ma non rigido, su:

\[
\frac{R_d(2000K)}{R_d(1800K)}
\]

e/o su \(R_d(2000K)\).

Esempio:

\[
S_{R,HT}
=
\max\left(0,\log_{10}\frac{R_d(2000)}{R_d(1800)}-\log_{10}(2)\right)
\]

cioè nessuna penalità se il raggio cresce fino a un fattore ~2, penalità se cresce di un fattore 5.

Non bisogna imporre esattamente la curva di Rizk, ma bisogna impedire raggi micrometrici non plausibili.

### 10.3 Non restringere subito troppo i range

È utile esplorare anche range più ampi, perché alcuni parametri Rizk potrebbero essere:
- ereditati;
- assunti;
- trascritti con unità ambigue;
- o non direttamente verificabili.

Però l’esplorazione ampia deve essere fatta con report diagnostico, non solo con score totale.

---

## 11. Strategia consigliata per Codex / agente

### 11.1 Calibrazione notturna

Far lavorare Codex su `v3b`, senza modificare gli script originali, creando nuovi file.

Codex dovrebbe eseguire tre livelli:

#### Livello A — nominal-near
Range relativamente vicini ai valori Rizk, prior forte.

Scopo: vedere se esiste un candidato tesi-ready senza stravolgere parametri.

#### Livello B — wide exploratory
Range più ampi, prior più debole.

Scopo: scoprire se un singolo parametro o gruppo di parametri “sblocca” il modello.

#### Livello C — one-family escape studies
Aprire una famiglia di parametri alla volta:
- diffusività: \(D_v, D_g, D2_{Xe}\);
- re-solution: \(b\);
- trapping: \(g_b, g_d\);
- microstruttura: \(f_n, K_d, \rho_d\);
- coalescenza/capture.

Scopo: capire quale famiglia è responsabile del trade-off tra fit, gas partition e raggio high-T.

### 11.2 Classificazione obbligatoria dei candidati

Codex non deve riportare solo il best score. Deve classificare:

- best numerical fit;
- best physical candidate;
- best Rizk-near candidate;
- best gas-partition candidate;
- best high-T-radius candidate;
- candidati rifiutati per pressione;
- candidati rifiutati per \(R_d\) high-T;
- candidati rifiutati per parametri troppo lontani.

---

## 12. Audit documentale separato

Deep Research precedente non ha risolto davvero il problema dei parametri perché ha fatto più un report bibliografico che una ricostruzione numerica.

Per parametri come \(D_v\), non basta cercare il valore finale già pronto. Serve:

1. aprire la fonte originale;
2. identificare equazioni e unità;
3. ricostruire l’ordine di grandezza;
4. confrontare con la curva di Rizk;
5. dire se il valore è verificato, ereditato, assunto o sospetto.

Parametri prioritari da auditare:

| parametro | priorità | motivo |
|---|---:|---|
| \(D_v\) vacancy diffusivity | altissima | già sospetto, possibile mismatch tabella/figura |
| \(b_0(R)\) resolution | alta | controlla permanenza gas nelle bolle |
| \(f_n\) | alta | ereditato/closure, molto libero |
| \(K_d\) | alta | Rizk lo calibra/modifica a \(5e5\) |
| \(\rho_d\) | media/alta | microstruttura incerta |
| \(g_d\) / pipe diffusion | alta | top v3 vuole \(g_d\sim6\) |
| coalescence law | alta | determina \(N_d\) e quindi \(R_d\) |
| capture bulk→dislocation | alta | non è Rizk puro, ma sembra importante |

---

## 13. Stato decisionale attuale

### Cose ormai abbastanza chiare

1. `capture_only` è competitivo come famiglia da esplorare.
2. Lo score v2 era incompleto perché non controllava gas partition.
3. Lo score v3 migliora molto gas partition e `q_gb`.
4. Il nuovo problema v3 è il blow-up del raggio dislocation ad alta T.
5. La coalescenza alta è probabilmente parte del problema.
6. Non basta aumentare il numero di trial: bisogna correggere lo score con un guardrail high-T radius più forte.
7. Non bisogna prendere Rizk come verità perfetta, ma neppure ignorare il comportamento qualitativo di Fig. 7–9.

### Candidato finale attuale

Non c’è ancora un candidato finale tesi-ready.

Il migliore v3 è un **successo parziale**:
- buona gas partition;
- P2 accettabile;
- pressione non assurda;
- ma raggio dislocation high-T troppo grande.

---

## 14. Prossimo passo operativo consigliato

1. Creare `UN_M7_optuna_calibration_v3b.py`.
2. Partire da `v3`.
3. Rafforzare il termine su high-T radius:
   - \(R_d(2000)/R_d(1800)\);
   - eventualmente limite morbido su \(R_d(2000)\).
4. Lanciare prima 150–200 trial `capture_only`.
5. Se il problema migliora, lanciare Codex overnight con:
   - nominal-near;
   - wide exploratory;
   - one-family escape studies.
6. In parallelo, avviare audit documentale vero sui parametri.

---

## 15. Sintesi finale

La calibrazione ha mostrato un progresso reale: imponendo gas partition e `q_gb`, il modello riesce a spostarsi verso una struttura molto più simile a Rizk Fig. 9. Questo dimostra che il problema non è completamente strutturale.

Il limite attuale è che il modello paga questo miglioramento con una crescita eccessiva del raggio delle dislocation bubbles ad alta temperatura. La causa più probabile è la combinazione:

\[
g_d \uparrow,\quad b \downarrow,\quad N_d \downarrow \text{ per coalescenza},\quad gas_d \uparrow .
\]

Il prossimo score deve quindi impedire questa compensazione non fisica, senza però forzare ciecamente la curva Rizk.

La domanda chiave per il prossimo round è:

\[
\boxed{
\text{si può ottenere P2 + gas partition + pressione accettabile senza } R_d(2000K)\text{ micrometrico?}
}
\]

Se sì, `capture_only` resta un candidato serio.  
Se no, bisogna capire se manca fisica, se qualche parametro Rizk è sbagliato, o se serve un modello più completo per grain-boundary/release.
