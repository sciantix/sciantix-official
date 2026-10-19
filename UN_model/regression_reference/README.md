# Riferimento Python UN L3 — FASE 1

Il notebook approvato è `UN_model/notebooks/finalRizkUN_L3_solver_unificato.ipynb`,
SHA-256 `e29d798d3685edb827a43933b3dc4400087f1aff488ee4080650c1430e1c59ff`.
Il runner rifiuta hash diversi. Non riscrive le equazioni e non importa la libreria UN storica.

## Esecuzione nel WSL

Dalla radice del repository, usando directory di output nuove:

```bash
export MPLBACKEND=Agg
export MPLCONFIGDIR=/tmp/sciantix-un-l3-mpl
.venv/bin/python UN_model/regression_reference/run_reference.py \
  --suite diagnostic --output /tmp/un-l3-diagnostic-new
.venv/bin/python UN_model/regression_reference/run_reference.py \
  --suite full --output /tmp/un-l3-full-new
.venv/bin/python UN_model/regression_reference/run_reference.py \
  --suite full --plain --output /tmp/un-l3-full-original-new
.venv/bin/python UN_model/regression_reference/run_reference.py \
  --suite full --case AP3p4_1800K --output /tmp/un-l3-full-observed-repeat-new
```

Il runner rifiuta directory esistenti e non modifica il notebook.
I quattro casi sono AP3.2/900 K, AP3.4/1600 K, AP3.4/1800 K, ANP6/1600 K.
`--suite diagnostic` termina dopo tre timestep (9 h), mantenendo parametri, dt e modi L3;
il manifest distingue il tempo ridotto dal tempo finale del caso sperimentale completo.
Non usare i risultati diagnostici come simulazioni a burnup completo.

Per verificare gli artefatti già prodotti e rigenerare solo la figura:

```bash
MPLBACKEND=Agg MPLCONFIGDIR=/tmp/sciantix-un-l3-mpl \
  .venv/bin/python UN_model/regression_reference/verify_reference.py
MPLCONFIGDIR=/tmp/sciantix-un-l3-mpl \
  .venv/bin/python UN_model/regression_reference/plot_reference.py
```

Il verificatore controlla il layout nominato sotto `results/`; per un nuovo insieme
con lo stesso layout usare `--results /percorso/nuovo/results`.
Richiede numpy e le dipendenze del runner originale (pandas/matplotlib).
SciPy non serve in questa fase e non è stato installato.

## Isolamento e osservazione

Ogni invocazione usa un nuovo processo Python e un nuovo modulo registrato in `sys.modules`.
Carica solo le celle 1, 2, 6, 8 e 10, con indici zero-based. Non carica le celle che
eseguono `main()`, l'ispezione singola o la griglia opzionale. Gli ultimi factory del
notebook costruiscono i parametri case-specific: AP3.2/AP3.8 restano distinti nelle impostazioni.
I default della dataclass isolata non sostituiscono i valori del runner L3.

L'osservazione inserisce esclusivamente chiamate che leggono `locals()`:
nessuna assegnazione, equazione, ramo o sorgente fisica viene sostituita.
Togliere le chiamate deve riprodurre esattamente l'AST originale; il runner lo verifica.
`--plain` esegue direttamente l'AST originale. I confronti indipendenti plain/observed
verificano l'uguaglianza esatta float64 di tutte le storie, non una tolleranza percentuale.

Le tracce osservano inizializzazione, vecchio stato, exchange, eventuale ramo di
non negatività, seed, vacancy, crescita, coalescenza, geometria, cattura, grain-face e fine passo.
Sono osservati anche i ritorni di vacancy implicite, bisezione seed, LimitedGrowth,
grain-face 3×3 e update grain-face. I rami con `return` precoce restano invariati.

## File prodotti

- `results/full/`: riferimento canonico completo, quattro casi.
- `results/diagnostic_with_function_trace/`: diagnostici completi di tre passi con osservazione interna dei componenti.
- `results/diagnostic/`: primo controllo di tre passi con osservazione del timestep.
- `results/reproducibility_full_plain/`: seconda esecuzione dei quattro casi, solver originale senza osservazione.
- `results/reproducibility_full_observed/`: ulteriore processo, AP3.4/1800 K con osservazione.
- `results/reproducibility_diagnostic_plain/`: controllo indipendente plain dei diagnostici.
- `results/verification.json`, `full_case_summary.csv`: verifiche e sintesi quantitativa.
- `results/reference_overview.pdf` e `.png`: grafici delle storie salvate, nessuna nuova simulazione.

Ogni gruppo contiene copia byte-identica del notebook, manifest di esecuzione,
hash delle celle caricate e impostazioni complete, incluse tabelle lookup.
I gruppi canonici completi conservano anche `executed_runner.py`, copia del runner effettivamente usato.
Ogni caso contiene:

| File | Contenuto |
|---|---|
| `manifest.json` | parametri effettivi dataclass, tempo completo/ridotto, dt, modi, hash output, tempi, warning e controlli |
| `history.csv`, `history.npz` | tutte le storie originali, più bilancio, rapporti pressione e vacancy per bolla |
| `initial_state.json`, `final_state.json` | tutti i campi storici iniziali/finali |
| `last_rates.json` | ultimo dizionario diagnostico restituito dal notebook |
| `trace.jsonl.gz` | fasi del timestep e variabili intermedie locali, rate/componenti, stato GF e inventario vacancy GF totale |
| `modes.npz` | modi iniziali, post-exchange, post-cattura e a fine di ogni timestep |
| `warnings.json`, `stdout_stderr.txt` | warning e output; vuoti nei casi completati senza messaggi |

`modes.npz`: assi `(passo, popolazione, modo)`, popolazioni `[c, mb, md]`,
indice del modo `i=0..159` corrispondente a `n=i+1`; i modi sono coefficienti firmati,
non concentrazioni medie positive. Ogni stadio ha un vettore `*_time_s`.
`initial` ha una riga; gli altri stadi una riga per avanzamento.

Nel trace `step` è zero-based; nelle fasi prima di `step_end`, `t` è il tempo
all'inizio del passo, `dt` il passo effettivo. `step_end` segue l'incremento di t.
Le variabili locali grezze possono includere valori del passo precedente prima della
rispettiva riassegnazione: per i rate correnti usare post-exchange o `step_end.last_rates`.
I ritorni di componenti ereditano l'indice del timestep corrente.
La lettura dei trace non richiede di decomprimere permanentemente i file:

```python
import gzip, json
with gzip.open("trace.jsonl.gz", "rt") as f:
    for line in f:
        event = json.loads(line)
```

Il campo `solver_wall_seconds` comprende il tempo delle chiamate di osservazione e
compressione del trace. `wall_seconds_including_output` misura la chiamata al factory,
incluso l'output del trace, ma esclude il salvataggio successivo di CSV/NPZ/manifest.
`total_wall_seconds` del gruppo comprende caricamento celle e salvataggio degli artefatti.
Confrontare con i tempi `plain` per isolare il costo dell'osservazione; non sono tempi del futuro C++.

## Unità e convenzioni

| Variabili | Unità |
|---|---|
| `time`, dt nei parametri | s; la figura converte in h |
| `burnup_percent_fima` | percento FIMA, non MWd/kg |
| `rho_d`, `rho_d_next` | m^-2 |
| `c`, `mb`, `md`, `generated`, `retained*`, `q_gb`, `q_gf`, `q_rel`, residuo massa | atomi/m³ combustibile |
| `Nb`, `Nd`, `delta_Nd_rho`, `capture_bubbles_cumulative` | bolle/m³ combustibile |
| `nvb`, `nvd`, `qv_gf` nel trace | vacancy/m³ combustibile |
| `vacancies_per_bubble_b/d`, `nv_gf` | vacancy/bolla |
| `Ngf_areal`, `Ngf_vol` | bolle/m² GB, bolle/m³ combustibile |
| `ng_gf` | atomi/bolla |
| `Rb`, `Rd`, `R_gf` | m |
| `Vb`, `Vd`, `V_gf` | m³/bolla |
| `A_gf` | m²/bolla |
| `p_b/d`, `p_b/d_eq`, `p_gf`, `p_gf_eq` | Pa |
| `swelling*`, copertura, phi, lambda, fattori cattura | frazioni adimensionali |
| campi `*_gas_percent` | percentuale della produzione cumulata |
| `nu_b`, beta | bolle/(m³ s), atomi/(m³ s) rispettivamente |
| Dg/Dv e componenti | m²/s |
| re-solution e trapping | s^-1 |

`q_gb=q_gf+q_rel`: non sommare nuovamente q_gb agli altri due inventari.
Le medie della storia sono ricostruite dai modi come nel notebook.
CSV usa la rappresentazione float64 round-trip di Python; NPZ conserva float64.
I non finiti diagnostici non applicabili, per esempio alcune componenti inattive,
sono codificati in JSON come `{"nonfinite":"nan"}` e non come zero.
Nessuna variabile delle quattro storie complete salvate è non finita.

## Limiti

Questi dati dimostrano riproducibilità del riferimento Python e neutralità
dell'osservazione. Non dimostrano equivalenza C++–Python, convergenza temporale/spaziale,
verifica analitica o correttezza fisica rispetto agli esperimenti.
La FASE 2 non è iniziata.
