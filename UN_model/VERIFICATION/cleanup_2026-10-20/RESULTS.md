# Esito della pulizia conservativa

Eliminati **3360 file**, **8,003,819,491 byte**
(8,004 GB decimali; 7,454 GiB) dal working tree. Conservati byte per byte
**6478 file preesistenti**, **196,624,254 byte**
(circa 196,6 MB), oltre a questo verbale e ai nuovi documenti di riproduzione.
Riduzione delle tre directory: circa **97,6%**. L'inventario completo con nomi,
azioni, motivi e checksum è `inventory.csv`.

Eliminati: 2108 NPZ, 911 BIN, 322 esportazioni orarie duplicate, 16 CSV dei
coefficienti duplicati, due eseguibili ricompilabili e una cache font.
Conservati: tutti i 19 file Python originali (incluse copie di provenienza),
911 input C++, 160 set congelati con seed e coordinate LHS, tutte le tabelle
di errore e ordine, massimi e diagnostiche, i due ZIP finali e l'intero CSV
dei coefficienti delle varianti UN. La storia oraria completa dell'accuratezza
resta nello ZIP e non viene ridotta a un campionamento. I grafici e le reference
delle fasi precedenti restano fuori dal perimetro e non sono modificati.

Backup esterno verificato: `/home/destro/sciantix-verification-backup-20261020/verification_essential_files.tar.gz`;
**81,869,376 byte**, SHA-256 `3db43b6915ccbfc10cf20af4019e2a6e7818fc20b1a645f82e49c63862aa38b1`.
Il branch locale `backup/un-verification-before-cleanup-20261020` conserva
l'intero commit `b5b098c0`, inclusi i dati intermedi eliminati dal working tree.
La sostituzione del commit resta reversibile; parent remoto invariato.

Validazione **PASS** (`validation_after.json`): byte e ignore controllati,
seed e input presenti, 23.793 righe di ordini temporali e 52.800 spaziali
ricontrollate, entrambi gli ZIP verificati. Il vero solver è ricompilato in
/tmp e provato su due set con N40/N160, tre update ciascuno e ultimo passo
parziale di 123 s: scarto modale BE normalizzato massimo
4.031e-16.
La campagna completa non è rieseguita; i risultati scientifici conservati
non vengono alterati.

L'hash conclusivo del nuovo commit, la revisione dei file e la dimensione
**misurata** del suo pacchetto thin Git sono registrati dopo il commit in
`/home/destro/sciantix-verification-backup-20261020/commit_review.json`.
Questo evita un report autoreferenziale e mantiene pulito il working tree.
Nessun push o modifica remota è eseguito.

Lo spazio locale `.git` non è ancora recuperato: gli oggetti originali restano
raggiungibili dal branch di backup e dai reflog. Nessun GC, prune o scadenza
dei reflog è autorizzato o eseguito. Il percorso proposto per recuperarlo,
previa conferma, è documentato nel README.
