Tentativo iniziale interrotto prima di qualunque simulazione: la serializzazione
delle impostazioni incontrava un ndarray numpy non gestito. È stata aggiunta la
conversione esplicita ndarray→lista nel solo serializzatore del runner.
La copia del notebook è conservata; questa directory non contiene risultati numerici.
I diagnostici riusciti sono nelle directory sorelle diagnostic e diagnostic_with_function_trace.
