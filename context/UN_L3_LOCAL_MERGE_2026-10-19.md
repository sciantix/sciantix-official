# UN L3 — merge locale in development/nitride, 19 ottobre 2026

Merge completato senza conflitti, con storia integrale e branch feature conservato. Nessun push, fetch o modifica a `origin/development/nitride`. FASE 4 non avviata.

## Commit e modalità

| Riferimento | Commit |
|---|---|
| `development/nitride` iniziale e antenato comune | `8f5bfc7e123b0241507c5d1e587a28d20e4dcb24` |
| `feature/un-l3-porting` iniziale, commit manuale | `016b4d25688354623e5d630efcc20b06967d0f59` |
| Salvataggio sorgenti/documentazione 2D–3, 19 file | `14b33628` |
| Salvataggio risultati/checkpoint e audit preliminare, 292 file | `607540b61e9c32e37388fb1763876bb904063d89` |
| Merge locale `--no-ff` | `115c93612b51826ab5f67a31dd4a52fd9cec04ec` |
| `feature/un-l3-porting` finale, backup locale | `607540b61e9c32e37388fb1763876bb904063d89` |
| `origin/development/nitride`, invariato | `8f5bfc7e123b0241507c5d1e587a28d20e4dcb24` |

Il merge è stato eseguito con `--no-ff --no-commit`, verificato prima del commit, poi committato normalmente. I due genitori sono il development iniziale e il feature finale; il tree del merge è **identico** al tree feature. Nessuno squash, rebase, reset, clean o eliminazione di branch. Un successivo commit locale su development archivia questo report e le verifiche dopo il merge; non modifica il codice integrato. L'identificativo del commit di evidenza è visibile con `git log -1` dopo il completamento.

## Controlli preliminari e file trasferiti

Working tree iniziale: nove sorgenti modificati, nessuna modifica staged, 298 file untracked tutti pertinenti alla FASE 2D/3, nessuna modifica estranea da spostare. Un solo worktree; development non aveva commit propri divergenti: conteggio destra/sinistra `0/1` prima del salvataggio. Il commit manuale `016b4d25` contiene anche la modifica di `BaseRizk2025.ipynb` e l'aggiunta di `BaseRizk2025_grid.csv`: entrambi conservati nello stesso commit originale.

Selezionati e controllati elenchi espliciti, senza `git add -A`. Trasferiti **1.205 percorsi** rispetto al development iniziale, includendo sorgenti L3, runner, notebook, reference Python, checkpoint, report FASI 0–3 e risultati già salvati. [Elenco completo](../UN_model/local_merge_2026-10-19/transferred_files.txt), [audit iniziale](../UN_model/local_merge_2026-10-19/audit_before.json), [selezione sorgenti](../UN_model/local_merge_2026-10-19/source_paths.txt), [selezione artefatti](../UN_model/local_merge_2026-10-19/artifact_paths.txt).

Segnalazione dimensioni, senza cancellazioni: il tree già committato in `016b4d25` contiene 915,60 MiB non compressi complessivi, contro 277,66 MiB del development iniziale, circa **638 MiB aggiuntivi**. I file maggiori aggiunti sono confronti NPZ fino a 15,66 MiB, input testuali circa 14,22 MiB e modi reference fino a 13,32 MiB. Gli artefatti 2D/3 da salvare occupavano circa **154 MiB**. I valori sono dimensioni dei file nel tree, non dimensioni dell'object database Git, che deduplica i blob identici. Build isolate, cache e temporanei già ignorati rimangono sul disco senza essere aggiunti ai commit.

## Verifiche dal branch development/nitride

Tutti i test sono stati rilanciati da `development/nitride` al commit di merge, con nuove build del vero `sciantix.x` in copie isolate. Riferimenti Python, gold, fisica e parametri calibrati invariati.

| Verifica | Esito |
|---|---|
| Build reale SCIANTIX Release | PASS |
| Solver 3×3 SCIANTIX: passi singoli, prefissi e replay | PASS, 5.829.810 valori, errore zero |
| Contratto dello stato CTest | 1/1 PASS |
| Predittore 3×3 grain-face | PASS, 9.036 valori, errore zero |
| Quattro storie complete Python–SCIANTIX | PASS, 3.012 timestep, 4.637.652 valori |
| Errore normalizzato massimo delle storie complete | 1,80918e-12, identico alla FASE 3 |
| Conservazione della massa globale | residuo / produzione ≤ 3,33615e-15 |
| Bilancio locale grain-face | errore normalizzato zero |
| Persistenza, contatori, flag e ultimo passo parziale | PASS |
| Suite storica, gold mode 0 | 109/109 PASS |
| UN legacy, tolleranza storica 35% | 91/91 PASS |

Per ciascun caso, `output.txt`, diagnostiche e i due file modali sono **identici byte per byte** agli output della FASE 3. Coincidono anche tutte le metriche numeriche. I 110 output storici, incluso il caso supplementare, coincidono byte per byte con quelli pre-merge; anche il CSV legacy è identico.

Problemi preesistenti, senza nuove regressioni: Baker `analytics/test_Baker1977_1273K_porosityevolution` rimane FAIL rispetto al gold (1.127 discrepanze), escluso dalla selezione standard; Vercors5 conserva i tre NaN già documentati. L'audit supplementare restituisce quindi 1, come prima del merge. Il vecchio generatore notebook8 resta assente; i riferimenti legacy esistenti sono stati usati senza rigenerarli.

I vecchi manifest continuano a riportare gli HEAD storici, inclusi `8f5bfc7e` e `016b4d25`: sono provenienza immutabile, non errori numerici. I nuovi manifest registrano `115c9361`. Eventuali differenze di hash del binario o di percorsi di build non modificano il confronto sui risultati; gli hash dei sorgenti e gli output numerici sono verificati separatamente. Nessun manifest congelato è stato riscritto. I vecchi verificatori che richiedono esplicitamente l'HEAD originario non sono stati usati per riscrivere i loro risultati.

Tempi misurati con tre build indipendenti avviate in parallelo: build integrazione 31,380 s; build solver/stato 33,403 s; build storica 31,630 s, suite storica 19,432 s. Non sono nuove misure di prestazione del modello rispetto alle precedenti esecuzioni.

## Evidenze e stato finale

[Verifica complessiva](../UN_model/local_merge_2026-10-19/results/verification.json), [errori completi](../UN_model/local_merge_2026-10-19/results/full/errors.csv), [risultati solver](../UN_model/local_merge_2026-10-19/results/solver_state/results.json), [audit storico](../UN_model/local_merge_2026-10-19/results/historical/audit.json). Log, input, output, manifest e runner della verifica sono conservati nella stessa directory. `postmerge_paths.txt` elenca esattamente i file del commit locale finale di evidenza.

Verificati **7.039 file preesistenti**, identici agli hash raccolti prima del salvataggio: inclusi tutto il porting non ancora committato, notebook, reference, gold, checkpoint, documenti e dati utente. Il branch feature resta locale come backup. Il branch finale è `development/nitride`; il commit finale di evidenza lascia il working tree pulito. Nessun push o pubblicazione del feature, nessuna modifica al remote-tracking origin, nessuna attività della FASE 4.

Per ripetere la verifica dei risultati salvati: `.venv/bin/python UN_model/local_merge_2026-10-19/verify_merge.py`. Questo script scrive soltanto il riepilogo del merge e verifica la cronologia/preservazione; non modifica i risultati congelati e non rilancia fisica. Per nuove esecuzioni usare i runner FASE 2A/3 e baseline con directory di output nuove, come nei manifest qui conservati.
