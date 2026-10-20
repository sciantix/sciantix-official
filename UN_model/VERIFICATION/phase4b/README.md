# FASE 4B — benchmark completati, campagna arrestata al controllo di costo

**Non sono state eseguite le 110 simulazioni principali.** Sono state completate le cinque prove cronometrate richieste e due simulazioni aggiuntive per la selezione del dt spaziale. La campagna si ferma come previsto dalla richiesta, perché l'isolamento temporale/spaziale sull'intera storia fisica con un dt uniforme richiederebbe costi stimati di mesi già per un solo caso.

- [Report scientifico e tempi](REPORT_PHASE4B_COST_GATE.md).
- [Alternative e proposta per la ripresa](PROPOSTA_RIPRESA.md).
- [Dieci set, provenienza e tempi caratteristici](parameters_10.csv).
- [Intervalli Table B.1 invariati](bounds.json).
- [Richiesta originale](REQUEST.md).
- [Checkpoint dell'arresto](checkpoints/cost_gate_stop/manifest.json).

## Organizzazione e riproduzione

Tutti i nuovi file sono sotto `UN_model/VERIFICATION/phase4b/`. Gli script importano senza modificare quelli 4A; il vero `solver_harness.cpp` 4A viene ricompilato con `src/classes/Solver.C` **in un build distinto**, `phase4b/build`. La `.venv`, le dipendenze locali SciPy, gli script, i risultati e il build 4A originari restano invariati. Non vengono generati nuovi bytecode nelle directory precedenti.

Da root del repository, con directory di output nuove:

```bash
.venv/bin/python -B UN_model/VERIFICATION/phase4b/benchmark.py --output UN_model/VERIFICATION/phase4b/results/NEW_benchmark
.venv/bin/python -B UN_model/VERIFICATION/phase4b/assess_cost.py --benchmark UN_model/VERIFICATION/phase4b/results/NEW_benchmark/results.json --output UN_model/VERIFICATION/phase4b/results/NEW_gate
.venv/bin/python -B UN_model/VERIFICATION/phase4b/summarize.py --benchmark UN_model/VERIFICATION/phase4b/results/NEW_benchmark/results.json --gate UN_model/VERIFICATION/phase4b/results/NEW_gate/decision.json --output UN_model/VERIFICATION/phase4b/results/NEW_figures
```

Il primo script salva anche `cases.json`, `parameters_10.csv` e `bounds.json` nella radice 4B; per ripetizioni successive alla consegna conservarne prima una copia o usare una copia della directory 4B, senza toccare la 4A. Non esiste un lanciatore automatico della campagna 110, per impedire di oltrepassare il controllo di costo senza una nuova decisione.

La reference usata nei cinque benchmark è quella 4A, con expm aumentata 4×4. Per il controllo E2 a g_b=0 viene risolto il sottospazio invariante c/md con expm aumentata 3×3, mantenendo bulk identicamente nullo per algebra, senza clipping. La matrice e le proiezioni sono quelle 4A. Questa riduzione evita di usare il rumore numerico dell'expm in una popolazione esattamente inattiva per stimare un rapporto di errori.

## Risultati conservati

| Percorso | Contenuto |
|---|---|
| `results/benchmark_4000h/timings.csv` | Cinque simulazioni N=40, dt=225 s, 4000 h; 64.000 passi ciascuna |
| `results/benchmark_4000h/*/comparison.npz` | Storie delle medie, reference 4A ed errori |
| `results/cost_accuracy_gate/cpp_*` | Le due esecuzioni C++ N=320, dt=225/112,5 s; 4000 h |
| `results/cost_accuracy_gate_v2/accuracy_precheck.csv` | Separazione degli errori a 1,24,100,1000,4000 h, per popolazione |
| `results/cost_accuracy_gate_v2/reference_convergence.json` | Reference controllata fino a N=5120 |
| `results/cost_accuracy_gate_v2/required_step_estimates.csv` | Stime con ordine locale osservato |
| `results/cost_accuracy_gate_v2/decision.json` | Decisione di arresto, budget ingenuo e costo con vincolo di accuratezza |
| `results/diagnostic_figures/asymptotic_p1_cost.csv` | Stime separate con ordine asintotico p=1 |
| `results/diagnostic_figures/` | Quattro figure PNG/PDF e CSV dei soli benchmark |

La prima elaborazione del controllo di costo si è fermata nel postprocessing della componente bulk inattiva; i due processi C++ erano già conclusi correttamente. La versione v2 riusa i loro output con hash registrati, senza ulteriori simulazioni. Sono quindi **5+2 esecuzioni C++**, non 5+4. [Stato del primo tentativo](results/cost_accuracy_gate/ATTEMPT_STATUS.md).

`validate_and_checkpoint.py` verifica configurazioni, reference, preservazione di tutti i file iniziali e index Git staged, quindi crea un checkpoint locale. È un verbale legato a questa sessione e richiede che `checkpoints/cost_gate_stop` non esista; non serve rilanciarlo sopra il checkpoint consegnato. I suoi hash possono essere verificati leggendo il manifest. Nessun commit, merge, cambio branch, push o LHS è stato eseguito.
