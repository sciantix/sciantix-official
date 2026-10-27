# Calibrazione L3 OAT

Tutti gli artefatti della campagna sono contenuti in questa directory. Il notebook originale non viene modificato.

Campagna conclusa: **20 candidati in tre batch (8+6+6)**, 2960 punti finali, oltre alla riproduzione L3 e alle sue storie di controllo. Raccomandazione: conservare L3; `B3_K085_rho093` è l'alternativa microstrutturale scelta per la revisione, senza sostituzione automatica.

Aprire prima il [rapporto tecnico finale](reports/RAPPORTO_TECNICO_FINALE.md), la [contact sheet finale](reports/FINAL_L3_vs_candidate.png), la [tabella dei candidati](reports/best_candidates.md) e la [mappa OAT aggiornata](reports/OAT_sensitivities.md). La [verifica conclusiva](reports/verification.json) conferma integrità degli originali, fisica/numerica e fission rate invariati, copertura completa e controlli numerici superati.

- `runs/baseline`: copie originali, configurazione, esecuzione testuale delle celle, CSV e grafici riprodotti, estrazione dello ZIP e confronto colonna per colonna.
- `scripts/cell_*.py`: estrazioni verbatim delle celle; `model.py` le carica senza cambiarne la fisica.
- `scripts/campaign.py`: esecuzione dei batch dichiarati nei JSON, con registrazione delle storie e monitoraggio degli scambi negativi prima del clipping.
- `runs/B*`: parametri completi, configurazione, risultati CSV, metriche, residui, diagnostiche e storie compresse per candidato.
- `plots`: contact sheet comparative con la curva L3 originale.
- `reports/registro_progressivo.md`: ipotesi, risultati e decisioni fra i batch.

Dalla radice del repository:

```bash
.venv/bin/python UN_model/notebooks/ARCHIVIO/calibrazione_L3_OAT/scripts/baseline.py
.venv/bin/python UN_model/notebooks/ARCHIVIO/calibrazione_L3_OAT/scripts/campaign.py UN_model/notebooks/ARCHIVIO/calibrazione_L3_OAT/scripts/batch_01.json --workers 4
```

Le simulazioni parallele sono processi indipendenti, ciascuno con le stesse celle e la medesima discretizzazione. Non sono agenti né una ricerca automatica. Dipendenze: Python 3, NumPy, pandas, Matplotlib già presenti nella `.venv` del repository.

Per riprodurre i batch successivi usare gli analoghi JSON `batch_02.json` e `batch_03.json`; sono definizioni congelate delle decisioni già prese. Per riesportare i riepiloghi e verificare la campagna completa:

```bash
.venv/bin/python UN_model/notebooks/ARCHIVIO/calibrazione_L3_OAT/scripts/audit_baseline.py
.venv/bin/python UN_model/notebooks/ARCHIVIO/calibrazione_L3_OAT/scripts/summarize.py
.venv/bin/python UN_model/notebooks/ARCHIVIO/calibrazione_L3_OAT/scripts/microstructure_consistency.py
.venv/bin/python UN_model/notebooks/ARCHIVIO/calibrazione_L3_OAT/scripts/export_review.py B3_K085_rho093 B2_K085_Dg094
.venv/bin/python UN_model/notebooks/ARCHIVIO/calibrazione_L3_OAT/scripts/verify_campaign.py
```

Le figure sono generate dai CSV senza editing delle immagini. Le celle estratte e le tre copie degli originali rendono la baseline indipendente da future modifiche ad altri notebook o moduli del repository. `scripts/.mplconfig` e `scripts/__pycache__` contengono soltanto cache locali della campagna.
