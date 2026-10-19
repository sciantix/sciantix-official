# FASE 2A — solver exchange SCIANTIX e stato UN

Questa fase verifica separatamente:

1. il vero `Solver::SpectralDiffusion3equationsExchange` contro dati Python salvati;
2. proprietà e conservazione del nuovo stato UN tra chiamate al modulo exchange;
3. l'integrazione nel normale ciclo SCIANTIX, **rinviata e non verificata qui**.

Nessuna equazione di diffusività, trapping, nucleazione, crescita, cattura o grain-face
è stata implementata per produrre i coefficienti di questi confronti.
Il driver è uno strumento di test; il codice riutilizzabile vive in `include/models`
e `src/models/UNIntraGranular.C`. Il solver condiviso originale non è stato modificato.

## Ripetizione

Dalla radice del repository, con directory di output nuove:

```bash
.venv/bin/python UN_model/phase2a/run_phase2a.py \
  --output UN_model/phase2a/results/release_repeat_new
.venv/bin/python UN_model/phase2a/run_extra_checks.py \
  --release UN_model/phase2a/results/release_repeat_new \
  --output UN_model/phase2a/results/extra_repeat_new --disable-leak-detection
```

Entrambi accettano `--cmake /percorso/cmake`; il default è l'installazione privata
CMake 3.31.6 già preparata in FASE 1. Richiedono Python/numpy, g++ e make.
Non caricano il solver Python e non generano nuovi riferimenti fisici.
L'output deve essere nuovo per evitare sovrascritture. La compilazione avviene nella
snapshot del nuovo output: il post-build non scrive nel `build/obj` dell'utente.

Il flag `--disable-leak-detection` è necessario **in questo ambiente sotto ptrace**:
il primo tentativo LeakSanitizer ha segnalato un errore fatale di incompatibilità.
Disabilita soltanto la ricerca leak; AddressSanitizer e UndefinedBehaviorSanitizer
restano attivi. I log del tentativo originale restano in `results/extra_checks/`.
In un WSL senza ptrace si può omettere il flag e provare anche LeakSanitizer.

Per ricostruire la non regressione, senza aggiornare gold:

```bash
.venv/bin/python UN_model/baseline_sciantix/prepare_baseline.py \
  --cmake /tmp/sciantix-un-l3-phase1-tools/cmake/data/bin/cmake \
  --output UN_model/phase2a/results/nonregression_repeat_new --jobs 2
.venv/bin/python UN_model/baseline_sciantix/audit_baseline.py \
  UN_model/phase2a/results/nonregression_repeat_new
```

L'audit esteso termina con exit 1 per il FAIL Baker e i NaN Vercors preesistenti;
il risultato non va interpretato come nuova regressione. Gli esiti sono confrontati
con la baseline FASE 1 nel report di questa fase.

## Dati impiegati e scenari distinti

Fonte: `UN_model/regression_reference/results/full/<caso>/`.
Il runner verifica gli hash degli output di ogni caso prima di leggerli.

- Rate e sorgenti correnti: evento **`after_exchange`** del trace. Non usare
  `old_state` per i rate, perché può conservare locali del passo precedente.
- Raggio grano: manifest del caso.
- Stato modale in ingresso: `initial` per il passo zero, `step_end` precedente dopo.
- Stato atteso: `after_exchange` in `modes.npz`; medie `c_new,mb_new,md_new` dello
  stesso evento. Le medie dopo la cattura non sono l'oracolo dell'exchange.

| Scenario | Stato tra chiamate | Significato |
|---|---|---|
| `single` | ripristino dell'ingresso Python per ogni test | isola ogni singolo aggiornamento, 3012 passi |
| `prefix` | nessun ripristino o intervento tra chiamate | 2 passi AP3.2 e 3 per ciascuno degli altri tre casi; prima della prima cattura interposta |
| `replay` | propagazione del risultato C++; sole variazioni modali esterne congelate | tutte le sequenze di 605/550/550/1307 aggiornamenti |

Nel replay, prima del passo i>0, il test somma:

```text
delta_i = saved_python_input_i - saved_python_exchange_output_(i-1)
cpp_modes_i = cpp_previous_exchange_output + delta_i
```

Non si sottrae l'errore C++, non si sostituiscono i modi con quelli Python e non
si ricostruiscono profili dalle medie. Ogni errore numerico propagato resta presente.
Il delta è un dato esterno salvato/derivato dai vettori di FASE 1, non il risultato
di una legge di cattura C++. Per questo il replay lungo **non è** una simulazione
fisica UN C++ completa o un test end-to-end di `Simulation`.

Ogni scenario viene eseguito in due modi: chiamata diretta al solver originale
e chiamata tramite `un_l3::advanceExchange`. Gli output dei due percorsi devono
coincidere byte per byte. Le fixture derivate vivono solo sotto `phase2a/results`;
i riferimenti originali restano immutati.

## Contratto del nuovo stato

`un_l3::State` possiede tre `std::vector<double>` indipendenti (c, mb, md), le medie,
inventari e geometria bubble, grain-face, flag first-gas/saturazione e cumulati.
Il conteggio dei modi è esplicito: non usa gli offset legacy a 40 o il buffer pubblico 720.
`advanceExchange` modifica soltanto modi e medie. Non modifica tempo, produzione,
vacancy, geometria, flag o gas GF/rilasciato. Il futuro orchestratore ne resterà responsabile.

Il test CTest usa sentinelle di stato e chiamate intercalate tra due punti materiali
da 160 e 40 modi; confronta continuità, indipendenza delle copie e aggiornamenti dei flag.
Questo è un test di proprietà/lifecycle: i coefficienti sintetici del test non sono
un riferimento fisico né sono usati per il confronto C++–Python.
Dimensioni incoerenti, dt negativo, raggio non positivo e input non finiti generano errori;
non vengono applicate correzioni silenziose. dt zero conserva tutti i modi.

Il contenitore non è ancora un membro di `Simulation`: quella connessione,
con la persistenza end-to-end attraverso `Sciantix()`, sarà verificata durante
l'integrazione del modello completo. Nessuna API avanzata di checkpoint/restart.

## Unità, protocollo e tolleranze

Coefficienti in ingresso: dt [s], Dg [m²/s], raggio [m], sorgenti [atomi/(m³ s)],
trapping/re-solution gas [s^-1]. Le sorgenti sono già quelle effettive Python,
incluso il trasferimento di nucleazione; l'adattatore non le ricalcola.
Modi e medie sono in atomi/m³; i modi possono avere segno alternato.

Il protocollo test è float64 little-endian, C order. `config.txt` contiene
N, numero passi e lunghezza del prefisso. `coefficients.bin` ha dieci colonne,
documentate nel manifest della fixture; `input_modes.bin` e `interventions.bin`
hanno forma `(passi,3,N)`. Ogni riga output ha tre medie seguite da 3×N modi.
`expected.npz` copia solo gli oracoli salvati; il driver C++ non lo legge.

Prima dell'esecuzione è fissata la regola:

```text
abs(cpp-python) <= 1e-11*abs(python) + 64*eps_float64*S
S = max(max_modo(abs(python_mode)) per timestep e popolazione, 1 atomo/m³)
errore normalizzato = abs(cpp-python)/S
```

La scala è locale per passo e popolazione: grandi valori tardivi non possono
nascondere errori iniziali delle bulk bubbles. Sono riportati anche errore assoluto,
relativo dove il riferimento è non nullo, coordinate del massimo e primo scarto oltre soglia.
In Release e nei replay sanitizzati eseguiti gli errori sono **esattamente zero**:
nessun esito dipende dall'allargamento della tolleranza.

Tre controlli deliberatamente errati — binding trapping scambiati, sorgenti scambiate,
modi md persi — devono essere rifiutati dal comparatore. Sono solo test di rilevazione
su copie derivate; non alterano i coefficienti delle fixture canoniche né la fisica.

## Risultati conservati

- `results/release/results.json`, `errors.csv`: 5.829.810 valori confrontati, contando
  medie e modi in entrambi i percorsi e tutti gli scenari; zero errori.
- `results/release/state_ctest.log`: contratto dello stato superato.
- `results/release/fixtures/`: input e oracoli derivati, provenienza/hash.
- `results/release/outputs/`: risultati C++ integrali dei tre scenari.
- `results/extra_checks_no_lsan/results.json`: tre controlli di rilevazione passati,
  test stato ASan/UBSan e replay sanitizzati, con limitazione leak esplicita.
- `results/nonregression/`: 109 PASS standard, esito supplementare Baker e audit;
  `comparison_to_phase1.json` conferma tutti i 110 output identici alla baseline.
- `protection_before.json` e `preservation_check.json`: conservazione riferimenti,
  gold, file preesistenti e staging.

Le snapshot di compilazione e le directory di oggetti sanitizzati restano locali,
escluse da Git. Nessun gold è stato rigenerato, nessun test storico eliminato.

Non sono dimostrati in questa fase: fisica UN completa, coefficienti calcolati in C++,
sorgente md non nulla assente dalle fixture L3, convergenza dt/N, soluzione analitica,
persistenza nel normale ciclo SCIANTIX o restart esterno. La FASE 2B richiede nuova approvazione.
