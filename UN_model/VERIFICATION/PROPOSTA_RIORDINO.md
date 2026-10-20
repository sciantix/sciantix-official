# Proposta separata di riordino — nessuno spostamento eseguito

Inventario in sola lettura di **13.467 file** nelle sei directory richieste. [Elenco completo e classificazione](legacy_inventory.csv), [riepilogo dimensioni](legacy_inventory_summary.json). Dimensioni non compresse sul filesystem, incluse copie ignorate da Git; la categoria temporanei indica rigenerabilità tecnica, **non autorizza cancellazioni**.

| Directory attuale | MiB circa | Contenuto da conservare / codice attivo |
|---|---:|---|
| phase2a | 184,6 | Harness del vero solver, test dello stato, runner e fixture; CMake punta ancora a solver_fixture.cpp e state_test.cpp |
| phase2bc | 601,1 | Test riproducibili dei componenti, driver, checkpoint ZIP, input/output e metriche storiche |
| phase2d3 | 460,7 | Runner dei quattro casi integrati, plotting/validazione, evidenze complete, checkpoint e tentativi diagnostici documentati |
| baseline_sciantix | 86,2 | Runner di build/audit ancora riutilizzati; baseline pre-porting e anomalie Baker/Vercors |
| regression_reference | 59,6 | Runner Python riproducibile e reference congelate: notebook copiato, manifest, storie, trace, modi e verifica riproducibilità |
| local_merge_2026-10-19 | 210,8 | Audit Git, elenchi di staging/trasferimento, nuove regressioni e confronto byte per byte del merge |

## Distinzioni necessarie

**Codice attivo o ancora necessario:** runner 2A/2BC/2D–3, driver C++, test di stato, build/audit baseline, script di confronto; alcuni sono richiamati da CMake e gli strumenti successivi. Non possono essere archiviati come dati inerti senza aggiornare e verificare i consumatori.

**Reference congelate:** soprattutto `regression_reference/results/full`, incluse trace JSONL e NPZ modali, più controlli di riproducibilità. Sono usate anche dalla nuova verification per estrarre coefficienti rappresentativi; i test di equivalenza non possono perderne accesso, hash o provenienza.

**Evidenze e checkpoint storici:** risultati dei confronti, CSV/JSON, manifest, log, ZIP dei sorgenti, report e tentativi falliti documentati. Anche i tentativi non canonici spiegano problemi di ambiente/preparazione; non sono classificati automaticamente come spazzatura. Non alterare gli hash o gli HEAD originali dei manifest.

**File tecnicamente rigenerabili:** build, oggetti, eseguibili, cache Python/Matplotlib. La classificazione trova circa 291 MiB in questa categoria complessiva. Sono candidati a una decisione futura, non a rimozione ora. Non tutto `snapshot/` è temporaneo: `snapshot/regression/.../output.txt` contiene output di baseline oggi usati dai controlli byte per byte, e `snapshot/src/include` può documentare sorgenti storici. I riferimenti congelati e i checkpoint ZIP non vanno confusi con build rigenerabili.

## Struttura futura proposta

Una sola radice di archivio storico, più una sede esplicita per i test ancora attivi:

```text
UN_model/
  VERIFICATION/                  # verification analitica nuova e studi futuri
  REGRESSION/                    # futura sede dei runner/test attivi UN L3
  ARCHIVE/
    UN_L3_2026-10-19/
      phase2a/
      phase2bc/
      phase2d3/
      baseline_sciantix/
      regression_reference/      # reference immutabile ancora consultata dai test
      local_merge_2026-10-19/
      path_map.json              # mapping di compatibilità, non riscrittura manifest
      inventory_sha256.json
  notebooks/, src/, ...           # altre directory utente preesistenti
```

Le directory di questa figura sono **solo una proposta**: non sono state create, spostate o rinominate, salvo `VERIFICATION` richiesta per il lavoro corrente. Le copie storiche dei runner rimarrebbero nell'archivio come provenienza; la sede REGRESSION conterrà versioni attive chiaramente distinguibili e una configurazione centralizzata degli artefatti di riferimento.

## Vincoli di percorso trovati

- `CMakeLists.txt` richiama direttamente `UN_model/phase2a/solver_fixture.cpp` e `state_test.cpp`.
- I runner 2A, 2BC e 2D–3 usano percorsi espliciti verso `UN_model/regression_reference/results/full`.
- I verificatori di preservazione e baseline usano directory delle fasi precedenti e alcuni richiedono esattamente l'HEAD originario. Questi controlli di provenienza non devono essere scambiati per una regressione numerica dopo un trasferimento.
- Diversi script ricavano ROOT con `Path(__file__).resolve().parents[2]`. **Un semplice symlink di compatibilità non basta:** `.resolve()` porta al percorso reale nell'archivio e cambia la root calcolata.
- Report e manifest possono contenere sia percorsi relativi sia assoluti del workspace; i manifest congelati non vanno riscritti per farli sembrare prodotti nella nuova sede.

## Procedura futura, dopo approvazione

1. Congelare un inventario/hash e una copia di sicurezza completa, includendo i file ignorati da Git. Un solo `git mv` dei file tracked non basta a garantire la conservazione delle copie ignorate di baseline e build.
2. Preparare nella futura sede REGRESSION i runner attivi con risoluzione della root robusta e configurazione esplicita delle reference; preservare le vecchie versioni come artefatti storici.
3. Aggiornare in un blocco verificabile i riferimenti CMake e i consumatori. Definire un mapping dei vecchi percorsi per consultazione/provenienza; usare eventuali alias solo dove siano effettivamente compatibili con `.resolve()` e con la configurazione nuova.
4. Spostare le sei directory nella singola radice ARCHIVE mantenendo tutti i file e le strutture interne. Conservare i manifest originali, con mapping esterno e nuovi verbali di migrazione.
5. Confrontare tutti gli hash prima/dopo; ricompilare, rieseguire solver/stato, quattro casi completi e suite storica senza aggiornare gold; verificare anche accesso a notebook e documenti/report.
6. Solo in un'attività distinta e dopo approvazione esplicita valutare eventuale eliminazione delle build/cache rigenerabili. Nessuna proposta di cancellare reference, notebook, risultati storici o checkpoint.

La proposta privilegia prima la continuità dei test, poi l'ordine del filesystem. **Nessuna parte della migrazione è stata eseguita nella FASE 4A.**
