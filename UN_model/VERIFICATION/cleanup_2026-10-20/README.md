# Pulizia conservativa della verification UN

Il commit completo precedente è conservato sul branch locale
`backup/un-verification-before-cleanup-20261020` (b5b098c0). La sostituzione
del solo commit non pubblicato è reversibile; non cambia il suo genitore remoto.
Nessun push, force push, reset, rebase, cancellazione di branch o garbage
collection è eseguito da questa procedura.

## Evidenza conservata

Sono conservati tutti gli script originali, tutti i parametri e gli input C++, i
seed e i campioni congelati, le storie complete estratte dalle reference, i
manifest con gli hash e gli esiti dei controlli. Restano tutte le tabelle di
errori, ordini temporali e spaziali, L2, anomalie, massimi, costi e decomposizioni,
sia nella raccolta iniziale sia nella finale. Conserviamo anche le piccole
tabelle per caso, per evitare di perdere diagnostica non ancora utilizzata.

Restano entrambi gli ZIP conclusivi e il CSV completo dei coefficienti delle
cinque varianti. In particolare le **640.160 righe** a tutte le **4001 ore**
dei 160 casi di accuratezza restano integralmente nello ZIP conclusivo, insieme
alle scale e alle definizioni necessarie per gli errori di c, mb, md. Il CSV
orario sciolto è eliminato solo dopo confronto esatto con il membro dello ZIP;
le sue 160 partizioni sono confrontate riga per riga. I 16 CSV delle singole
storie dei coefficienti sono verificati contro il CSV completo prima della
cancellazione. Gli array modali completi non restano nel nuovo commit: sono
rigenerabili dal solver originale e dagli input conservati. I loro riepiloghi,
norme L2 e ordini restano nei CSV.

Non c'erano grafici PNG/PDF nelle tre nuove campagne: i grafici delle fasi
precedenti restano intatti fuori dal perimetro. Non sono stati creati nuovi
grafici o nuovi risultati scientifici durante la pulizia.

## Audit e backup

- `inventory.csv`: ogni file precedente, dimensione, SHA-256, azione e motivo.
- `plan.json`: selezione precisa; non contiene wildcard di cancellazione.
- `predelete_validation.json`: verifica preventiva di archivi e duplicati.
- `backup_manifest.json`: backup esterno, dimensione, hash e verifica dei file.
- `deletion_result.json`: conteggi e byte effettivamente rimossi dal working tree.
- `validation_after.json`: verifiche conclusive dei dati rimasti e della build.
- `/home/destro/sciantix-verification-backup-20261020/commit_review.json`:
  commit sostitutivo e dimensione misurata del pack da inviare, registrati dopo
  il commit nel backup esterno per mantenere il working tree pulito.

Il backup esterno è `/home/destro/sciantix-verification-backup-20261020/`.
Contiene una copia verificata di **tutti** i file preesistenti conservati nelle
tre directory, non soltanto una selezione dei risultati. L'archivio originale
non viene modificato dopo la verifica. I nuovi documenti di riproduzione e il
verbale conclusivo sono aggiunti in un secondo archivio separato.

## Rigenerazione senza dipendere dai dati eliminati

I vecchi `completed.json`, manifest, budget e validazioni sono **verbali storici**,
non la dichiarazione che i binari esistano ancora. Non riprendere la campagna
nella directory conclusa: i checkpoint la salterebbero e i budget sono scaduti.
Non copiare i vecchi build manifest in un nuovo build, perché legano il binario
e il suo percorso originali. I vecchi hash dei grezzi rimangono come provenienza;
non vengono riscritti per nascondere l'eliminazione intenzionale.

`prepare_reproduction.py` prepara esclusivamente una nuova directory sorella,
copia gli script originali e gli input congelati, crea una fotografia Git
corrente e un budget nuovo di 90 minuti. Non avvia simulazioni e non rigenera
né ricalibra i 160 set. La fotografia dei file può richiedere tempo. Si parte
con working tree e index tracked puliti. La cartella di riproduzione è ignorata
da Git; i risultati finali vanno eventualmente esportati e revisionati a parte.

Controllo leggero senza avviare la campagna:

```bash
.venv/bin/python -B UN_model/VERIFICATION/cleanup_2026-10-20/prepare_reproduction.py --check-only
```

Preparazione della convergenza estesa, solo quando si vuole rigenerarla:

```bash
.venv/bin/python -B UN_model/VERIFICATION/cleanup_2026-10-20/prepare_reproduction.py --kind extended --destination reproduce_extended_NEW
.venv/bin/python -B UN_model/VERIFICATION/reproduce_extended_NEW/run_campaign.py benchmark
.venv/bin/python -B UN_model/VERIFICATION/reproduce_extended_NEW/run_campaign.py run
.venv/bin/python -B UN_model/VERIFICATION/reproduce_extended_NEW/collect_results.py
.venv/bin/python -B UN_model/VERIFICATION/reproduce_extended_NEW/targeted_extension.py
.venv/bin/python -B UN_model/VERIFICATION/reproduce_extended_NEW/collect_results.py --output UN_model/VERIFICATION/reproduce_extended_NEW/deliverables_final
.venv/bin/python -B UN_model/VERIFICATION/reproduce_extended_NEW/validate_and_package.py --input-directory UN_model/VERIFICATION/reproduce_extended_NEW/deliverables_final
```

I raffinamenti e i controlli possono arrestarsi al budget o se l'ambiente
numerico produce sequenze diverse: gli assert scientifici non vanno rimossi.
Le versioni originarie e quelle della nuova esecuzione sono entrambe registrate.
Per riprodurre i risultati originali usare l'ambiente registrato nei manifest.

Dopo aver completato la convergenza estesa, preparare l'accuratezza:

```bash
.venv/bin/python -B UN_model/VERIFICATION/cleanup_2026-10-20/prepare_reproduction.py --kind accuracy --destination reproduce_accuracy_NEW --source-extended reproduce_extended_NEW
.venv/bin/python -B UN_model/VERIFICATION/reproduce_accuracy_NEW/run_task.py baseline benchmark
.venv/bin/python -B UN_model/VERIFICATION/reproduce_accuracy_NEW/run_task.py baseline all
.venv/bin/python -B UN_model/VERIFICATION/reproduce_accuracy_NEW/run_task.py targeted benchmark
.venv/bin/python -B UN_model/VERIFICATION/reproduce_accuracy_NEW/run_task.py targeted all
.venv/bin/python -B UN_model/VERIFICATION/reproduce_accuracy_NEW/run_task.py collect
```

`run_task.py` reindirizza soltanto il percorso della campagna estesa in memoria;
equazioni, solver e file sorgenti originali sono invariati. L'analisi aggiuntiva
dei coefficienti/rapporti è quella già prevista dal collettore originale.
Confrontare i CSV rigenerati con quelli conservati, separando i metadati di
percorso, Git e tempi di esecuzione dai numeri fisici. Il vecchio
`validate_package.py` dell'accuratezza include un divieto di esecuzioni duplicate
specifico della richiesta originale: una riproduzione intenzionale incontra
gli stessi input storici conservati e non deve usare quel vincolo come test
di equivalenza numerica. La validazione originale rimane conservata; il nuovo
packaging richiede un verbale della nuova sessione. Non è stato dichiarato
di aver rieseguito qui una campagna completa dopo la pulizia.

Per rigenerare il CSV dei coefficienti, il preparatore supporta anche
`--kind coefficients --destination reproduce_coefficients_NEW`; poi eseguire
`extract.py` nella nuova directory. Le reference L3 precedenti sono mantenute.

## Spazio Git: fase successiva, solo previa conferma

L'eliminazione libera il working tree e riduce drasticamente il nuovo push.
Gli oggetti del vecchio commit sono tuttora **raggiungibili dal branch di backup**
e possono essere trattenuti anche dai reflog: non sono ancora garbage.
Una normale garbage collection ora non li eliminerebbe.

Dopo revisione del nuovo commit e del backup, si potrà decidere se preservare
anche tutta la vecchia storia in un bundle esterno, quindi rimuovere i soli
riferimenti locali che la trattengono, scadere i relativi reflog e ricomprimere/
eliminare gli oggetti divenuti irraggiungibili. Questi passaggi non sono eseguiti:
richiedono conferma esplicita e il controllo che nessun altro branch o processo
Git dipenda dagli oggetti. Nessuna modifica del remoto è necessaria.
