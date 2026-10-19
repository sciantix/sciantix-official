# FASE 2A — esiti solver exchange e stato UN

Data: 18 ottobre 2026. FASE 2A completata. **FASE 2B non iniziata.**
Branch `feature/un-l3-porting`, HEAD invariato `8f5bfc7e123b0241507c5d1e587a28d20e4dcb24`.

## 1. Esito e separazione dei tre obiettivi

| Obiettivo | Esito e limite |
|---|---|
| Correttezza numerica del 3×3 | vero `Solver::SpectralDiffusion3equationsExchange`, invariato; fixture FASE 1, passi isolati e consecutivi; errore zero nei confronti eseguiti |
| Conservazione dello stato UN tra chiamate al modulo | nuovo contenitore proprietario e adattatore; modi, flag e inventari conservati secondo contratto; test separato superato |
| Integrazione nel normale ciclo `Sciantix`/`Simulation` | **rinviata**: non implementata né dichiarata verificata |

Nessun coefficiente fisico è stato prodotto da una nuova fisica C++.
Nessuna legge completa UN implementata per anticipare l'integrazione.
Il driver eseguibile è esclusivamente un harness di test del solver di produzione.

Istruzioni ripetibili, protocolli, unità e limiti sono nella
[`README FASE 2A`](../UN_model/phase2a/README.md).

## 2. Confronti numerici eseguiti

Input letti direttamente dai riferimenti Python immutati:
rate e sorgenti dall'evento `after_exchange`, raggio dal manifest, vettori iniziali
o `step_end` precedenti da `modes.npz`. Oracolo: vettori `after_exchange` e medie
`c_new,mb_new,md_new` prima della cattura. Verificati gli hash prima della lettura.
Nessun import del modello Python, nessuna rigenerazione degli oracoli.

| Caso | Passi isolati | Prefisso consecutivo senza interventi | Replay lungo |
|---|---:|---:|---:|
| AP3.2 / 900 K | 605 | 2 | 605 |
| AP3.4 / 1600 K | 550 | 3 | 550 |
| AP3.4 / 1800 K | 550 | 3 | 550 |
| ANP6 / 1600 K | 1307 | 3 | 1307 |
| **Totale per percorso** | **3012** | **11** | **3012** |

Ogni prova è eseguita sia chiamando direttamente il solver originale sia usando
l'adattatore tipizzato di stato. I due percorsi producono output identici byte per byte.
Sono confrontati **5.829.810 valori** complessivi: tre medie e 480 coefficienti per
aggiornamento, includendo entrambi i percorsi e tutti e tre gli scenari.

Nel replay lungo gli effetti intermedi non appartenenti al solver vengono rappresentati
solo dai delta dei vettori già registrati in Python. Il test li aggiunge al risultato
C++ propagato; non sostituisce lo stato con quello Python e non elimina un eventuale
errore numerico accumulato. Non viene calcolato un rate di cattura C++.
Questo replay a ingressi imposti non è una simulazione UN completa.

| Metrica | Isolati | Prefissi senza interventi | Replay lunghi |
|---|---:|---:|---:|
| Massimo errore assoluto, modi e medie [atomi/m³] | 0 | 0 | 0 |
| Massimo errore relativo, riferimenti non nulli | 0 | 0 | 0 |
| Massimo errore normalizzato | 0 | 0 | 0 |
| Valori oltre soglia | 0 | 0 | 0 |
| Primo timestep divergente | nessuno | nessuno | nessuno |

Le soglie fissate prima del confronto sono `rtol=1e-11` e
`atol=64*eps_float64*S`, dove S è il massimo modulo atteso **del singolo passo
e popolazione**, con pavimento 1 atomo/m³. Le stesse scale si applicano alle medie.
Vicino a zero si usa quindi una soglia assoluta esplicita; gli errori non vengono
divisi per un riferimento nullo. L'accordo esatto osservato non richiede la tolleranza.

Tre controlli su copie volutamente corrotte sono stati correttamente rilevati:
binding gb/gd scambiati, sorgenti c/mb scambiate e perdita dei modi md.
Ognuno produce 483 valori fuori soglia, come atteso. Non sono risultati fisici alternativi.

Dettagli per variabile, posizione del massimo e tempi:
[`errors.csv`](../UN_model/phase2a/results/release/errors.csv),
[`results.json`](../UN_model/phase2a/results/release/results.json).

## 3. Stato UN: contratto verificato

`un_l3::State` possiede vettori dinamici indipendenti per c, mb e md, con numero
modi esplicito. Contiene medie, inventari e geometria bubble, inventari GF/rilascio,
flag first-gas/saturazione, produzione cumulata e tempo.
`advanceExchange` aggiorna solo modi e medie; gli altri campi sono preservati.
Non si usano variabili globali o buffer/offset legacy a 40/720.

Il test separato verifica:

- otto chiamate intercalate tra punti materiali da 160 e 40 modi, confrontate con
  otto chiamate ininterrotte allo stesso stato;
- permanenza dei tre vettori, delle sentinelle microstrutturali/GF/cumulate e dei flag;
- conservazione di un flag modificato tra due chiamate;
- copia indipendente dello stato, senza aliasing dei vettori;
- dt zero, dimensioni incoerenti, conteggi fuori intervallo, raggio non valido,
  timestep negativo e input non finiti.

I coefficienti sintetici del solo test lifecycle non forniscono un oracolo numerico:
il confronto Python–C++ usa esclusivamente le fixture FASE 1.
Il test non chiama `Sciantix()` e non prova la futura proprietà dello stato da parte
di `Simulation`; quella verifica sarà effettuata con l'integrazione completa.
Nessuna API di checkpoint/restart esterno aggiunta.

## 4. Build e controlli memoria

GNU 13.3.0, CMake 3.31.6 privato già disponibile dalla FASE 1, Release.
Configure 2.208 s, compilazione completa più target test 21.030 s, CTest 0.035 s.
La procedura completa Release, inclusa estrazione fixture e confronti, dura 27.210 s.

CTest: `un_l3_state_contract` **PASS**, 1/1.
Build Debug con AddressSanitizer/UndefinedBehaviorSanitizer: stato e tre prefissi
salvati più tutti i 1307 passi ANP6 in replay **PASS**, nessun errore ASan/UBSan;
risultati numerici ancora esattamente uguali al riferimento Release/Python.

**Limitazione esplicita:** il primo test con LeakSanitizer attivo fallisce perché
LeakSanitizer non funziona sotto ptrace in questo ambiente. Log conservato in
[`extra_checks/sanitize_state.log`](../UN_model/phase2a/results/extra_checks/sanitize_state.log).
La ripetizione imposta solo `ASAN_OPTIONS=detect_leaks=0`; la ricerca leak non è
verificata. ASan e UBSan restano attivi. Non è stata modificata la fisica per risolvere il problema.
Esiti completi in [`extra_checks_no_lsan/results.json`](../UN_model/phase2a/results/extra_checks_no_lsan/results.json).

## 5. Non regressione dei percorsi storici

Ricompilato l'eseguibile normale con i nuovi file, test opzionali disabilitati di default,
in una nuova snapshot. Nessuna sostituzione del binario originale dell'utente.
Configure 2.267 s, build 19.186 s, suite standard 19.223 s.

- Suite standard: **109 PASS, 0 FAIL**.
- Caso supplementare Baker escluso da `--all`: **stesso FAIL preesistente**.
- Vercors: stessi tre NaN presenti anche nel gold e già registrati in FASE 1.
- Suite UN notebook-8 legacy separata: **91 confronti PASS** con tolleranza legacy 35%.
- **Tutti i 110 output storici sono identici byte per byte alla baseline FASE 1**,
  anche per il caso Baker e Vercors; identico anche il CSV legacy UN.
- Nessun gold aggiornato e nessun test storico eliminato. L'exit 1 dell'audit esteso
  segnala i problemi preesistenti e viene conservato, non mascherato.

Report: [`nonregression/report.html`](../UN_model/phase2a/results/nonregression/report.html),
[`audit.json`](../UN_model/phase2a/results/nonregression/audit.json),
[`comparison_to_phase1.json`](../UN_model/phase2a/results/nonregression/comparison_to_phase1.json).

## 6. File aggiunti o modificati

| File | Modifica |
|---|---|
| `CMakeLists.txt` | opzione test `SCIANTIX_UN_L3_TESTS`, OFF di default; target che compilano il vero Solver.C e il nuovo adattatore; CTest separato |
| `include/models/UNParameters.h` | struttura tipizzata dei soli coefficienti exchange già forniti |
| `include/models/UNState.h` | contenitore proprietario e campi dello stato UN, senza equazioni fisiche |
| `include/models/UNIntraGranular.h` | contratto del passo numerico exchange |
| `src/models/UNIntraGranular.C` | controlli degli ingressi, mapping parametri e chiamata al solver esistente |
| `UN_model/phase2a/solver_fixture.cpp` | driver di test con input binari congelati; nessuna riscrittura del 3×3 |
| `UN_model/phase2a/state_test.cpp` | test indipendente dello stato e dei flag |
| `UN_model/phase2a/run_phase2a.py` | derivazione fixture, build isolato e confronti quantitativi |
| `UN_model/phase2a/run_extra_checks.py` | controlli di rilevazione e test sanitizzati |
| `UN_model/phase2a/README.md`, `.gitignore`, manifest e `results/` | istruzioni, prove, log e risultati separati dalla reference |
| questo report | esiti e limiti FASE 2A |

**Il file `src/classes/Solver.C` non è modificato.** Nessuna modifica a `Simulation`,
dispatch, buffer pubblici, costanti globali, modelli UO2/UN legacy, equazioni del notebook,
gold, tesi o file staged dell'utente.

La protezione iniziale comprende 3853 file: riferimenti FASE 1, baseline pre-port,
gold, sorgenti tracciati e build preesistente. Il solo file preesistente autorizzato
qui a cambiare è `CMakeLists.txt`. Controllo finale in
[`preservation_check.json`](../UN_model/phase2a/preservation_check.json).

## 7. Confine per la FASE 2B

L'exchange numerico è pronto per il porting incrementale delle equazioni, ma il
modello UN completo non è implementato. La fase successiva potrà portare coefficienti
e componenti fisici e confrontarli con i rate intermedi salvati, mantenendo questo
test del solver come controllo indipendente.
L'integrazione in `Simulation` e la verifica end-to-end della persistenza restano
successive al completamento del modello. Verifica analitica e restart esterno restano rinviati.

Il risultato vale per le fixture testate, il compilatore/configurazione registrati
e le condizioni L3 approvate. Non dimostra convergenza dt/N, verifica analitica o
correttezza fisica rispetto agli esperimenti. La sorgente md non nulla non è coperta
dai riferimenti L3 disponibili.

**Arresto al termine della FASE 2A; attesa dell'approvazione per la FASE 2B.**
