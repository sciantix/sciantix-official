## Confronto dei termini di coalescenza nei modelli

La variazione del numero di bolle nel tempo dovuta alla coalescenza viene trattata con diversi livelli di complessità nei modelli disponibili. Nei modelli recenti di Barani (2020) e Rizk (2025), la coalescenza intragranulare è descritta principalmente come impingement, cioè contatto diretto tra bolle dovuto alla loro crescita volumetrica, non come coalescenza per migrazione Browniana delle bolle.

---

### 1. Coalescenza tra bolle su dislocazioni

Questo termine descrive la diminuzione della densità numerica delle bolle su dislocazioni, $N_d$, quando le bolle crescono e si toccano.

In Barani (2020) e Rizk (2025), il termine di coalescenza è:

$$
\left(
\frac{\partial N_d}{\partial t}
\right)_{coals}
=
-4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

dove:

$$
\lambda =
\frac{2-\xi}{2(1-\xi)^3}
$$

e:

$$
\xi =
V_d N_d
=
\frac{4}{3}\pi R_d^3 N_d
$$

Qui $V_d$ è il volume medio della bolla su dislocazione e $\xi$ è la porosità intragranulare associata alla popolazione di bolle su dislocazioni.

Nel modello Rizk per UN, questo termine è incluso nella evoluzione di $N_d$:

$$
\frac{\partial N_d}{\partial t}
=
\frac{N_d}{\rho_d}
\frac{\partial \rho_d}{\partial t}
-
4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

Se si assume densità di dislocazioni costante:

$$
\frac{\partial \rho_d}{\partial t}=0
$$

rimane comunque:

$$
\frac{\partial N_d}{\partial t}
=
-
4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

quindi $N_d$ diminuisce se le bolle su dislocazioni crescono.

---

### 2. Coalescenza tra bolle su dislocazioni e bolle bulk

Barani (2020) include anche un termine di interazione tra bolle su dislocazioni e bolle bulk. L’idea è che una bolla su dislocazione, crescendo, catturi le bolle bulk contenute nel volume spazzato dalla sua espansione.

Il volume di cattura è:

$$
V_d^*
=
\frac{4}{3}\pi(R_d+R_b)^3
$$

La variazione della densità numerica delle bolle bulk dovuta a questa interazione è:

$$
\left(
\frac{\partial N_b}{\partial t}
\right)_{bulk-disloc}
=
-
N_d N_b
\frac{\partial V_d^*}{\partial t}
$$

Questo termine trasferisce gas dalle bolle bulk alle bolle su dislocazioni e riduce la densità numerica delle bolle bulk.

Nel modello Rizk (2025) per UN, questo termine non è incluso esplicitamente. Rizk include la coalescenza tra bolle su dislocazioni, ma non il termine di cattura bulk-dislocation nella equazione di $N_b$.

---

### 3. Coalescenza tra bolle bulk

Barani (2020) trascura esplicitamente la coalescenza tra bolle bulk nanometriche. Nel modello, le bolle bulk sono piccole e la loro evoluzione è dominata da nucleazione, trapping e re-solution.

Nel modello Rizk (2025), la densità numerica delle bolle bulk evolve come:

$$
\frac{\partial N_b}{\partial t}
=
\nu_b
-
b_b\phi_bN_b
$$

quindi non compare un termine di coalescenza bulk-bulk.

Nel modello SCIANTIX/Pizzocri standard per UO₂, la concentrazione di bolle intragranulari evolve nella forma:

$$
\frac{dN_{ig}}{dt}
=
\nu
-
bN_{ig}
$$

anche qui senza termine esplicito di coalescenza bulk-bulk.

---

### 4. Modelli classici con bolle mobili

Nei modelli classici, come quelli discussi nella letteratura di Olander, si possono considerare meccanismi di coalescenza legati alla migrazione delle bolle. In questo caso la coalescenza deriva dallo scontro tra bolle mobili, non dall’impingement di bolle immobili che crescono.

Questa forma rappresenta una coalescenza per migrazione/collisione, mentre Barani (2020) e Rizk (2025) usano una descrizione basata sulla crescita volumetrica e sull’impingement.

---



## Sintesi

La differenza principale è che Barani (2020) ha il modello più completo per la coalescenza intragranulare tra popolazioni diverse:

$$
N_d \rightarrow \text{coalescenza dislocation-dislocation}
$$

e:

$$
N_b \rightarrow \text{riduzione per cattura da bolle su dislocazioni}
$$

Rizk (2025), invece, mantiene solo il termine:

$$
-4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

nella evoluzione delle bolle su dislocazioni.

Quindi, per il modello UN attuale, la scelta coerente con Rizk è:

$$
\frac{\partial N_b}{\partial t}
=
\nu_b
-
b_b\phi_bN_b
$$

$$
\frac{\partial N_d}{\partial t}
=
\frac{N_d}{\rho_d}
\frac{\partial \rho_d}{\partial t}
-
4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$

e, se $\rho_d$ è costante:

$$
\frac{\partial N_d}{\partial t}
=
-
4\lambda N_d^2
\frac{\partial V_d}{\partial t}
$$