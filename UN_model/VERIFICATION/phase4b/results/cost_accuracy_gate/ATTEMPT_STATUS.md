# Stato del primo controllo di costo

Le due esecuzioni C++ a N=320, dt=225 s e 112,5 s, T=4000 h sono terminate correttamente. Output e timing originali sono conservati in questa directory.

La prima elaborazione Python si è interrotta nella stima del passo: il caso g_b=0 ha bulk matematicamente nullo, ma l'expm aumentata 4×4 può introdurre valori bulk di arrotondamento dell'ordine di 1e7 rispetto a concentrazioni circa 1e23. La differenza spaziale bulk risultava zero, provocando una divisione nulla nella stima del costo.

Il controllo corretto `cost_accuracy_gate_v2` risolve l'esatto sottospazio invariante c/md con expm e mantiene bulk nullo per costruzione matematica, senza clipping, senza modificare la reference 4A e senza rieseguire le due simulazioni C++. I JSON `reused_cpp_*.json` registrano origine e hash degli output riutilizzati. Queste sono due simulazioni, non quattro. Nessuno dei 110 test principali è stato avviato.
