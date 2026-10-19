# Baseline SCIANTIX prima del porting UN L3

La baseline è in `runs/pre_port_2026-10-18/`. Nessun file C++ è stato modificato.
Il progetto è stato copiato in `snapshot/` prima della compilazione: anche il post-build
CMake che scrive `obj/` rimane nella copia. Il binario originale `build/sciantix.x`
non è stato usato né sostituito. La snapshot locale è esclusa da Git, ma conservata su disco.

## Ripetizione nel WSL

Il solo strumento mancante necessario era CMake. È stato installato sotto `/tmp`,
senza cambiare il `.venv` o i pacchetti di sistema:

```bash
.venv/bin/python -m pip install --disable-pip-version-check --no-cache-dir \
  --target /tmp/sciantix-un-l3-phase1-tools-new cmake==3.31.6
.venv/bin/python UN_model/baseline_sciantix/prepare_baseline.py \
  --cmake /tmp/sciantix-un-l3-phase1-tools-new/cmake/data/bin/cmake \
  --output UN_model/baseline_sciantix/runs/pre_port_repeat_new --jobs 2
.venv/bin/python UN_model/baseline_sciantix/audit_baseline.py \
  UN_model/baseline_sciantix/runs/pre_port_repeat_new
```

Le directory di output devono essere nuove. Nessun comando Allclean o reset Git.
`prepare_baseline.py` copia `src/`, `include/`, `CMakeLists.txt` e `regression/` correnti;
salva gli hash dei sorgenti e dei gold, compila Release GNU 13.3.0 e richiama il runner
storico con `--all --mode-gold 0 -j 2`. Un adapter al solo punto di reporting salva
gli stessi esiti anche come JSON, senza cambiare esecuzione o confronto.
Gli input e la snapshot usata nel run originale restano consultabili localmente.

`audit_baseline.py` esegue i casi non selezionati dal discovery storico, controlla
separatamente finitudine/colonne nominate di tutti gli output e avvia la suite UN
notebook-8 legacy con `--strict --no-plot`. Termina con exit 1 se trova problemi,
anche quando `--all` termina con exit 0. L'exit 1 è quindi atteso sulla baseline corrente.
La suite UN legacy non è un confronto con il notebook L3.

Per rianalizzare gli output già salvati senza alcuna nuova simulazione:

```bash
.venv/bin/python UN_model/baseline_sciantix/audit_baseline.py \
  UN_model/baseline_sciantix/runs/pre_port_2026-10-18 --reuse-executions
```

## File da consultare

- `manifest.json`: HEAD, branch/stato Git, compilatore, versione CMake, comandi,
  exit code/tempi, hash di 72 file sorgente/header/build e dei gold, SHA-256 del binario.
- `configure.log`, `build.log`, `regression.log`: log integrali della suite standard.
- `CMakeCache.txt`, `compile_commands.json`: configurazione e flag effettivi.
- `report.html`, `case_results.json`: **109 PASS / 0 FAIL** nella suite standard.
- `supplemental_case_results.json`: caso analytics escluso dal runner standard, FAIL.
- `audit.json`: controlli di finitudine, provenienza input/runner e risultati supplementari.
- `preexisting_issues.json`: dettaglio dei NaN Vercors e primi scarti Baker.
- `legacy_un_notebook8_summary.csv`, `.log`: 91 confronti legacy superati, tolleranza 35%.
- `ctest_inventory.log`: nessun test CTest registrato nell'HEAD corrente.
- `workspace_protection.json`, `workspace_preservation_check.json` nella directory
  superiore: hash di 2.350 file preesistenti e verifica di conservazione/staging.

## Problemi già presenti

1. `analytics/test_Baker1977_1273K_porosityevolution` non è selezionato da `--all`,
   perché il prefisso analytics del runner è `test_powerPulse`. Eseguito separatamente
   contro lo stesso gold: 1.127 celle oltre le soglie storiche, primi scarti a riga 0
   su probabilità di venting e porosità aperta. Nessuna correzione è stata applicata.
2. `vercors/test_Vercors5` passa il confronto storico ma ha tre NaN nominati anche nel gold:
   pressione parziale ossigeno/potenziale ossigeno alla riga 400 e deviazione
   stechiometrica di equilibrio alla riga 401. Il comparatore storico non li rifiuta.
3. La generazione del riferimento notebook-8 legacy punta ancora a un percorso spostato.
   Il test sui CSV esistenti funziona; non è stata tentata la rigenerazione dei gold.
4. CMake segnala la deprecazione della compatibilità con versioni precedenti alla 3.10.
   Il build termina correttamente; non è stato aggiornato il CMakeLists per eliminare il warning.

Tutti i gold, i sorgenti copiati e i file preesistenti nel workspace risultano invariati.
