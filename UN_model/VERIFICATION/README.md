# UN L3 — FASE 4A

Verification preliminare del solver lineare spettrale, sul branch `development/nitride`, HEAD `97162196`. Tutti i nuovi file sono sotto questa directory. Nessuna modifica alla fisica, ai notebook, ai gold o alle reference precedenti; nessun campionamento LHS o riordino eseguito.

- [Report tecnico](REPORT_FASE4A.md).
- [Proposta studi successivi e dimensionamento LHS](STUDI_SUCCESSIVI_E_LHS.md).
- [Proposta separata di riordino](PROPOSTA_RIORDINO.md).
- [Verifiche indipendenti e preservazione](validation.json).

## Riproduzione

Da root del repository, con g++, il Python della `.venv`, NumPy e Matplotlib esistenti. SciPy 1.16.2 è stato installato **solo** in `UN_model/VERIFICATION/dependencies/`, senza alterare la `.venv`. Per riprodurre in un ambiente senza questa directory:

```bash
.venv/bin/python -m pip install --no-deps --no-cache-dir --target UN_model/VERIFICATION/dependencies -r UN_model/VERIFICATION/requirements.txt
```

Gli script antepongono questo target a `sys.path`. I risultati presenti sono quelli effettivamente eseguiti; per nuove prove scegliere nomi di output nuovi. I runner rifiutano la sovrascrittura delle directory di risultati.

```bash
.venv/bin/python UN_model/VERIFICATION/run_verification.py benchmark --output UN_model/VERIFICATION/results/NEW_speed
.venv/bin/python UN_model/VERIFICATION/check_basis.py
.venv/bin/python UN_model/VERIFICATION/run_pilots.py --benchmark UN_model/VERIFICATION/results/NEW_speed/results.json --output UN_model/VERIFICATION/results/NEW_pilots
.venv/bin/python UN_model/VERIFICATION/tabulate_timescales.py --cases UN_model/VERIFICATION/results/NEW_pilots/cases.json
.venv/bin/python UN_model/VERIFICATION/validate_results.py --pilots UN_model/VERIFICATION/results/NEW_pilots
.venv/bin/python UN_model/VERIFICATION/plot_and_summarize.py --pilots UN_model/VERIFICATION/results/NEW_pilots --output UN_model/VERIFICATION/results/NEW_figures
.venv/bin/python UN_model/VERIFICATION/inventory_legacy.py
```

`validate_results.py` ricontrolla anche il benchmark canonico `results/speed_initial/` e i file protetti all'avvio: è una verifica di questa sessione sul suo HEAD originale, non un vincolo di provenienza da riscrivere dopo futuri commit. Il file `preservation_before.json` e i risultati precedenti non vanno modificati per adattarli a un nuovo HEAD.

## File e risultati canonici

`solver_harness.cpp` collega direttamente `src/classes/Solver.C`. Il comando g++ effettivo e i tempi sono nei manifest dei risultati. Il driver non contiene un altro aggiornamento 3×3; legge solo coefficienti costanti e condizioni iniziali, poi chiama `SpectralDiffusion3equationsExchange` per ogni passo.

`analytic.py` implementa l'IVP indipendente con `scipy.linalg.expm`: matrice aumentata per sorgente costante e, nel solo esperimento spaziale molto lungo dei casi accoppiati, traslazione rispetto all'equilibrio per evitare cancellazioni. `check_basis.py` verifica per quadratura normalizzazione, proiezione e medie.

| Directory/file | Contenuto |
|---|---|
| `results/speed_initial/` | Primo benchmark: N=40, dt=3600 s, T=24 h; tempi separati e confronto |
| `results/pilots_v2/cases.json` | Cinque casi, coefficienti, provenienza, tempi caratteristici e T fissi |
| `characteristic_times.csv` | Tempi reciproci dei coefficienti, tempi modali e rapporti di stiffness |
| `results/pilots_v2/temporal_errors.csv` | Errori e ordini temporali a N=40 |
| `results/pilots_v2/temporal_errors_per_population.csv` | Errori delle tre medie separatamente |
| `results/pilots_v2/spatial_errors.csv` | Errori di troncamento, temporali residui e ordini spaziali |
| `results/pilots_v2/reference_convergence.json` | Reference a N elevato e convergenza, incluse le code esatte D=0 |
| `results/pilots_v2/spatial_dt_selection.json` | Verifica quantitativa della separazione dell'errore temporale |
| `results/pilots_v2/supported_modes.json` | Prova reale N=10,20,40,80,160 e inizializzatore di produzione |
| `results/figures/` | Sei figure PNG/PDF e proposta aritmetica dei costi LHS |
| `legacy_inventory.csv` | Inventario in sola lettura delle sei directory storiche |
| `checkpoints/complete/` | Sorgenti/script/documenti di questa fase e hash degli artefatti |

In ogni run C++: `input.txt`, `output.bin`, `timing.json`, `run.log`. Il binario contiene righe little-endian float64: tempo, tre medie, poi tre vettori modali, forma `(righe, 4+3*N)`. I campioni temporali comuni sono ogni ora, più l'ultimo passo; la simulazione C++ evolve realmente a ciascun dt richiesto. Lo studio spaziale salva lo stato iniziale/finale e valuta **solo lo stato finale lungo**, non l'accuratezza dei transitori con il suo dt grande.

`results/pilots/` conserva un tentativo di preparazione interrotto prima dei test: mancava la gestione della lista vuota di tempi propri nel caso senza alcun tasso. Corretto lo script, rieseguiti tutti i casi in `pilots_v2`. Nessun riferimento o sorgente di produzione modificato.

Il solver indicizza i modi con `unsigned short`: il driver limita N a 1–65535 e i cinque valori richiesti sono stati provati. Il limite di sicurezza dell'harness è 5 milioni di passi per invocazione. Dipendenze binarie e build locali sono ignorate da Git, ma restano sul disco; script, configurazioni, risultati e report sono tutti nella cartella richiesta. Nessun commit, cambio branch o push eseguito in questa fase.
