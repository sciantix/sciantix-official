# Stato dopo la pulizia conservativa del 20 ottobre 2026

Questa è una campagna conclusa. I manifest, gli hash dei grezzi, i budget e i
checkpoint precedenti sono verbali storici e restano immutati. I file `.npz`
e `.bin` rigenerabili sono stati eliminati intenzionalmente; la loro assenza
non invalida i controlli numerici già registrati. Gli script, gli input, i
parametri e i risultati finali sono conservati. Non usare i vecchi checkpoint
per tentare un resume nella directory storica.

Per inventario completo, backup e preparazione di una riproduzione in una
nuova directory vedere [il verbale di pulizia](../cleanup_2026-10-20/README.md).

Nella campagna di accuratezza, la tabella completa a 4001 ore è conservata
all'interno di `accuracy_N40_dt3600_compact.zip` (membro
`temporal_hourly_lossless.csv`), mentre le copie sciolte e gzip sono state
eliminate. Gli ZIP conclusivi non sono modificati. Nella campagna dei
coefficienti, il CSV completo sostituisce le copie per singola serie eliminate.

La storia completa pre-pulizia resta sul branch locale
`backup/un-verification-before-cleanup-20261020`; nessuna garbage collection
è stata eseguita. Il backup separato dei file conservati si trova in
`/home/destro/sciantix-verification-backup-20261020/`.
