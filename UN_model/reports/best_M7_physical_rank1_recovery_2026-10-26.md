# Recupero della calibrazione `best_M7_physical_rank1`

Data dell'indagine: **26 ottobre 2026**. Repository: `/home/destro/sciantix-official`, branch `development/nitride`, HEAD `bb6cebe7`.

**La calibrazione è stata identificata con corrispondenza esatta.** È il candidato `PHYS_0045`, secondo nel FAST sweep, poi primo nella verifica FINAL con etichetta `FINAL_rank2_PHYS_0045` e score **1.7068047916474136**. Il prefisso delle figure è `best_M7_physical_rank1`; l'etichetta stampata nelle diagnostiche è `best_M7_physical_FINAL_rank2_PHYS_0045`. `rank2` conserva la posizione FAST, mentre `rank1` indica la posizione dopo il riordinamento FINAL.

La prima registrazione Git trovata, comprendente codice, output completato e CSV, è del **30 aprile 2026 alle 13:24:48, Europe/Rome**. È una data del commit, non un timestamp certificato dell'esecuzione.

## Risposte ai sei quesiti

| Quesito | Riscontro |
|---|---|
| Codice/versione | Cella autonoma `UN M7 PHYSICAL SWEEP`, funzione `solve_UN_M7`, nel notebook storico `3UNpython_tests.ipynb`, commit `3416c92a5015a68495303c16b2d72065e8a451ce`. La stessa cella è conservata nell'attuale `UN_Barani_model.ipynb`, indice cella 1. |
| Phi nella re-solution del gas | **Sì, per entrambe le popolazioni**: `b_b_gas = b_b * phi_b` e `b_d_gas = b_d * phi_d`, effettivamente passati al sistema di scambio del gas. Phi compare anche, separatamente, nella distruzione di `N_b`. |
| Opzioni fisiche | Nucleation mass coupling **attivo**; bulk-dislocation capture **attiva**, `capture_scale=1`; coalescenza delle dislocation bubbles **attiva**, `coalescence_d_scale=1`. |
| Dv | Legge a due termini dichiarata nel codice come refit di Fig. 4 di Rizk. `Dv_scale=0.3` moltiplica **l'intera somma `D1 + D2`** e agisce sull'assorbimento di vacancy in **entrambe** le popolazioni. |
| Diffusività separate | **Nessun `Dg_d` o `Dv_d` separato** nella versione identificata. La stessa `Dg` alimenta i trapping bulk/dislocazioni; la stessa `Dv` è passata ai due aggiornamenti delle vacancy. `gd_scale` scala il trapping, non costituisce una diffusività separata. |
| Dati/obiettivo | FINAL: 37 punti swelling P2/dislocazioni con T ≤ 1700 K, 28 punti `N_d`, 9 punti `R_d`; RMSE lineare dello swelling + RMSE log10 di `N_d` e `R_d` + penalità pressione e alta T. Dettagli e valori sotto. |

## 1. File originali, provenienza e versione effettiva

Tutti i percorsi relativi qui sotto si riferiscono alla radice `/home/destro/sciantix-official`.

### Originali recuperati da Git

| Revisione | Percorso originale nella revisione | Copia recuperata |
|---|---|---|
| `3416c92a` | `3UNpython_tests.ipynb` | [Notebook originale](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/original/3UNpython_tests.ipynb) |
| `3416c92a` | `UN_M7_physical_sweep_outputs/physical_fast_all.csv` | [FAST completo](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/original/UN_M7_physical_sweep_outputs/physical_fast_all.csv:1) |
| `3416c92a` | `UN_M7_physical_sweep_outputs/physical_fast_partial.csv` | [FAST progressivo](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/original/UN_M7_physical_sweep_outputs/physical_fast_partial.csv:1) |
| `3416c92a` | `UN_M7_physical_sweep_outputs/physical_final_top.csv` | [FINAL completo](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/original/UN_M7_physical_sweep_outputs/physical_final_top.csv:2) |
| `3416c92a` | `UN_M7_physical_sweep_outputs/physical_sweep_summary.md` | [Riepilogo originale](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/original/UN_M7_physical_sweep_outputs/physical_sweep_summary.md:13) |
| `303595b9` | `UN_M7_global_sensitivity_fullcell/{global_fast_all.csv,global_fast_partial.csv,global_final_top.csv,global_sensitivity_summary.md}` | [Riepilogo dello sweep globale successivo](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/original/UN_M7_global_sensitivity_fullcell/global_sensitivity_summary.md:1); tutti e quattro i file sono recuperati nella stessa directory. |

La riga **2** del CSV FINAL corrisponde a tutti i parametri richiesti:

```text
label          = FINAL_rank2_PHYS_0045
f_n            = 3e-07
K_d            = 300000.0
rho_d          = 30000000000000.0
fission_rate   = 7.5e+19
Dv_scale       = 0.3
capture_scale  = 1.0
dt_h           = 1.0
n_modes        = 40
use_full_exp   = 1
score_total    = 1.7068047916474136
```

L'output del notebook conferma i parametri, la selezione e il completamento (`DONE.`). Vedi [stdout salvato originale](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.stdout.txt:1) e [diagnostica nel notebook attuale, righe JSON 291–293](/home/destro/sciantix-official/UN_model/notebooks/ARCHIVIO/UN_Barani_model.ipynb:291).

### Catena delle versioni verificata

| Data locale del commit | Commit | Evidenza |
|---|---|---|
| 30/04/2026 13:24:48 | `3416c92a` | `3UNpython_tests.ipynb`, cella indice 1, `execution_count=8`; introduce i quattro file del physical sweep. |
| 30/04/2026 14:06:20 | `03be6f65` | Aggiunge `UN_Barani_model.ipynb` con identica sorgente della cella physical e output corrispondente. |
| 30/04/2026 17:45:50 | `1790c8c8` | Conserva la cella physical; aggiunge celle per la sensitivity mirata. |
| 30/04/2026 18:49:46 | `303595b9` | Notebook a sei celle; aggiunge lo sweep globale e i suoi quattro file di risultati. |
| 05/05/2026 23:24:21 | `0509e7ab` | Esiste una successiva rifattorizzazione che cambia la sorgente e rimuove gli output: non è la fonte usata per attribuire questa calibrazione. |
| 05/05/2026 23:41:07 | `b75cb525` | Nella relativa linea di storia sposta i risultati sotto `un_calibration/results/UN_M7_physical_sweep_outputs/`. |
| 06/05/2026 00:02:28 | `93d3670d` | Elimina quei risultati dal percorso `un_calibration/results/`. Anche `25a3fc83` dell'08/05 elimina le copie nel percorso in radice in un'altra linea disponibile della storia. |
| 08/05/2026 12:26:13 | `d77219dc` | Aggiunge il notebook conservato sotto `UN_model/notebooks/UN_Barani_model.ipynb`. |
| 16/10/2026 12:20:28 | `8f5bfc7e` | Sposta il notebook in `UN_model/notebooks/ARCHIVIO/UN_Barani_model.ipynb`. |

La sorgente della cella physical è **identica byte per byte** in `3416c92a:3UNpython_tests.ipynb`, `03be6f65:UN_Barani_model.ipynb`, `303595b9:UN_Barani_model.ipynb` e nella cella indice 1 del notebook attuale. SHA-256 della sorgente concatenata:

```text
d8fc4313ddf757d9e31dfbfaed30c525f2dce99b4f4e28850def147d7d4245d3
```

Il notebook attuale intero coincide inoltre con il blob Git `644256ce40abba19c2d0a3bb2f93be6fd9da4b79` di `303595b9:UN_Barani_model.ipynb`. I CSV FINAL e il riepilogo physical hanno blob identici nelle revisioni `3416c92a`, `03be6f65` e `303595b9`.

La versione usata è dunque il **solver Python autonomo M7 del physical sweep**, con diffusione spettrale e scambio implicito 3×3 fra matrice, bulk e dislocazioni. La cella contiene le proprie classi e funzioni; importa libreria standard e Matplotlib e non importa `UN_model/src/un_model.py` né lancia l'eseguibile SCIANTIX. La verifica FINAL usa **1 ora, 40 modi**; il FAST seleziona 8 candidati tra 80 con **24 ore, 15 modi**, seed **42**. Vedi [configurazione, righe sorgente 25–78](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:25), [sistema 3×3, 508–545](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:508) e [FINAL, 1171–1203](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:1171).

Parametri aggiuntivi: `Dg_scale=b_scale=gb_scale=gd_scale=nu_scale=coalescence_d_scale=1`, raggio del grano `6e-6 m`, resa Xe `0.24`, parametro reticolare `4.889e-10 m`, `gamma_b=1.11 J/m²`, `omega_fg=8.5e-29 m³`, stress idrostatico zero; `rho_d` è costante e `N_d(0)=K_d*rho_d=9e18 m^-3`. Gas, raggi e `N_b` iniziali sono zero. Il tempo finale è ricavato da FIMA tramite densità atomica `4/a³`, non fissato indipendentemente. Vedi [parametri, 197–288](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:197).

## 2. Phi: evidenza nei termini del gas, distinta da N_b

La funzione `phi_population(m,N)` restituisce `1/(m/N-1)` se `N>0`, `m>0` e `m/N>1`; altrimenti restituisce zero ([righe 371–377](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:371)).

Il percorso effettivo nel solver è:

```python
phi_b = phi_population(mb_old, Nb_old)
phi_d = phi_population(md_old, Nd_old)
b_b_gas = b_b * phi_b
b_d_gas = b_d * phi_d

# Distruzione del numero di bolle bulk, operazione distinta:
N_b = (Nb_old + dt * nu_b) / (1.0 + dt * b_b * phi_b)

# Scambio delle concentrazioni di gas:
c_new, mb_new, md_new = sciantix_3x3_exchange_step(
    modes_c, modes_mb, modes_md,
    Dg, p.grain_radius,
    source_c, source_mb, source_md,
    g_b, g_d,
    b_b_gas, b_d_gas,
    dt,
)
```

Evidenza: [sorgente 675–706](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:675), corrispondente al [notebook attuale, righe JSON 1096–1127](/home/destro/sciantix-official/UN_model/notebooks/ARCHIVIO/UN_Barani_model.ipynb:1096).

Nel sistema implicito i coefficienti sono effettivamente usati sia per il ritorno del gas in matrice sia per la perdita dalle due popolazioni:

```python
A = [
    [1.0 + (diffusion_rate + g_b + g_d) * dt, -b_b_gas * dt, -b_d_gas * dt],
    [-g_b * dt, 1.0 + b_b_gas * dt, 0.0],
    [-g_d * dt, 0.0, 1.0 + b_d_gas * dt],
]
```

Vedi [sorgente 535–539](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:535), [notebook, JSON 956–960](/home/destro/sciantix-official/UN_model/notebooks/ARCHIVIO/UN_Barani_model.ipynb:956). Questa è evidenza della presenza di phi **nelle equazioni del gas di entrambe le popolazioni**, ulteriore rispetto alla sola equazione di `N_b`.

Il coefficiente base è `b_i = Fdot * b0(R_i + 0.21e-9) * b_scale`, con `b0(R)=1e-25*[2.64-2.02*exp(-2.61e-9/R)]`, per `i=b,d` ([323–331](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:323)). Nel physical sweep questi termini sono incondizionati: non c'è uno switch per disattivare phi nel gas.

## 3. Opzioni fisiche effettivamente attive

**Nucleation mass coupling: attivo.** `nu_b = nu_scale * 8*pi*f_n*Dg*omega_fg^(1/3)*max(c,0)^2`. Le sorgenti sono `source_c=beta-2*nu_b`, `source_mb=2*nu_b`, `source_md=0` e vengono passate al solver del gas ([367–368](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:367), [694–706](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:694)).

**Bulk-dislocation capture: attiva, lineare e clippata.** Dopo la crescita e la coalescenza:

```text
Vcap = 4*pi/3 * (Rb + Rd)^3
delta_Vcap = max(Vcap_new - Vcap_old, 0)
f_cap = clip(capture_scale * Nd * delta_Vcap, 0, 1)
```

Il codice trasferisce la frazione `f_cap` del gas e delle vacancy bulk alle dislocation bubbles e riduce `N_b` della medesima frazione; non incrementa `N_d` per la cattura. Vedi [754–786](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:754). Il CSV originale documenta una cattura non nulla a 1600 K, 3.2% FIMA: `max_f_cap_step=3.456627451336176e-05`, `capture_fraction_sum=0.09439361668117116`. Quest'ultima è la somma diagnostica dei valori per step, non la frazione cumulativa fisica di gas catturato.

**Coalescenza delle dislocation bubbles: attiva**, `coalescence_d_scale=1`. La legge usa `xi=clip(Vd_old*Nd_old,0,0.999999)`, `lambda=(2-xi)/[2*(1-xi)^3]` e, quando il volume cresce, `Nd_new=Nd_old/[1+4*coalescence_d_scale*lambda*Nd_old*max(Vd_growth-Vd_old,0)]`. Vedi [380–382](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:380), [736–744](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:736). Non si trova una legge separata di coalescenza bulk in questo solver.

**Vacancy:** `vacancy_absorption_only=True`, `update_bulk_vacancies=True`; aggiornamento implicito per entrambe le popolazioni, senza emissione quando la pressione non supera quella di equilibrio. Nessun seed artificiale di vacancy da raggio bulk: `bulk_seed_radius_nm=0`. Vedi [257–261](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:257), [422–455](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:422).

## 4. Formula Dv e portata di Dv_scale=0.3

Con `kBT = 8.617333262e-5 * T` in eV, la funzione effettiva è:

```text
Dv1 = 1.35e-2 * exp(-5.66/kBT)
Dv2 = sqrt(Fdot) * 4.6304523933553033e-29
      * exp(-0.62/kBT - 0.04/(kBT)^2)
Dv  = 0.3 * (Dv1 + Dv2)                         [m²/s]
```

I parametri sono dichiarati come `refit previously obtained from Rizk Fig. 4` ([219–225](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:219)); la formula è in [312–320](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:312), corrispondenti al [notebook, JSON 733–741](/home/destro/sciantix-official/UN_model/notebooks/ARCHIVIO/UN_Barani_model.ipynb:733). Non è la successiva legge con diffusività atermica D3 o con scaling indipendenti di Dv1 e Dv2.

Il valore scalato `Dv` entra nel coefficiente `A=2*pi*Dv*delta*N/(kB_J*T*zeta)` del passo implicito delle vacancy, sia nella chiamata bulk sia in quella dislocazioni ([437–455](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:437), [717–731](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:717)). Agisce quindi **direttamente sull'assorbimento delle vacancy e sul contributo delle vacancy alla crescita di entrambe le bolle**; modifica indirettamente raggi, pressioni, coalescenza e capture. Non moltiplica direttamente diffusione Xe, nucleazione, trapping o re-solution. L'aggiornamento implicito è non lineare: `Dv_scale=0.3` non implica che il numero finale di vacancy sia il 30% di quello nominale.

## 5. Dg e Dv comuni alle due popolazioni

La diffusività Xe è:

```text
Dg = Dg_scale * precursor_factor
     * [1.56e-3 * exp(-4.94/kBT) + 1.85e-39 * Fdot]
```

Qui `Dg_scale=1`, `precursor_factor=1`; D2 dello Xe è intenzionalmente assente. Vedi [210–214](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:210), [303–309](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:303).

La stessa variabile `Dg` entra nella diffusione spettrale della matrice, nella nucleazione bulk, nel trapping bulk `4*pi*Dg*(Rb+r_lattice)*Nb` e nei **due** termini di trapping dislocazioni, bolle e linea libera. `gb_scale` e `gd_scale` sono fattori sui rispettivi **tassi di trapping**; entrambi sono 1 nella calibrazione. Vedi [334–350](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:334), [524–529](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:524), [670–675](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:670).

Le due chiamate di aggiornamento vacancy ricevono letteralmente la **stessa `Dv`**, senza fattori aggiuntivi per dislocazioni ([717–721](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:717)). Le geometrie e le popolazioni sono diverse, non la diffusività passata. Non vi sono variabili o parametri `Dg_d`, `Dv_d`, `Dg_dislocation_scale` o analoghi nella cella identificata.

## 6. Punti sperimentali e funzione obiettivo effettiva

### Dati incorporati nella cella

| Dataset | Contenuto incorporato | Punti FINAL | Punti FAST | Uso |
|---|---|---:|---:|---|
| `EXP_SWELLING_T` | 39 punti Fig3a/b/c: 20 a 1.1% FIMA (10 serie 100 kW/m, 10 serie 119 kW/m), 9 a 1.3%, 10 a 3.2% | 37 | 21 | Swelling **dislocation/P2**, in punti percentuali; si escludono T=1709 e 1724 K a 1.3% FIMA perché T>1700 K. |
| `EXP_ND_T_13` | 28 punti a 1.3% FIMA, 1153–1742 K, unità m^-3 | 28 | 7 | Livello `N_d`, errore log10. Conserva entrambe le misure a 1656 K. |
| `EXP_RD_T_13` | 9 punti a 1.3% FIMA, 1045–1725 K, unità nm | 9 | 6 | `R_d`, errore log10. |
| `EXP_SWELLING_BURNUP_1600` | 4 punti: `(1.12,1.64)`, `(1.11,2.90)`, `(1.31,3.51)`, `(3.18,3.72)` in (%FIMA, swelling %) | 0 | 0 | Solo confronto grafico swelling vs burnup, non entra nel punteggio. |

I valori completi e i flag `used_final`/`used_fast` sono nel [CSV dei punti estratti](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/experimental_points.csv:1), senza arrotondamenti o aggiornamenti ai dati delle versioni successive. Le costanti originali sono alle [righe sorgente 101–173](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:101); la selezione è alle [979–991](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:979).

Le etichette P2/large bubbles e Fig3a/b/c provengono dal notebook. La cella non fornisce il file sorgente della digitalizzazione, barre d'errore o un riferimento bibliografico completo associato a ogni punto: questa indagine certifica **i numeri effettivamente usati nel codice**, non la loro accuratezza rispetto all'articolo. Le serie 100 e 119 kW/m entrano come punti distinti, ma sono simulate con il medesimo `fission_rate` del candidato; non c'è conversione separata da potenza lineare per serie.

### Formula esatta del punteggio physical

Definendo `RMSE(e)=sqrt(mean(e²))`, la funzione minimizzata è:

```text
J = RMSE(Sd_pred[%] - S_exp[%])
    + RMSE(log10(Nd_pred / Nd_exp))
    + RMSE(log10(Rd_pred / Rd_exp))
    + 0.15 * RMSE(log10(p_i / p_eq_i))
    + H

H = sum over T in {1800,2000} K:
      0.02  * max(Sd_pred(T,3.2%FIMA)[%] - 40, 0)
    + 0.002 * max(Rd_pred(T,1.3%FIMA)[nm] - 800, 0)
```

Per la pressione si usano entrambe le popolazioni `i=b,d`, a **3.2% FIMA**, per T = **1200, 1400, 1600, 1700 K**. Sono accettati solo rapporti positivi finiti; il codice applica `abs(log10(max(ratio,1e-30)))` prima della RMSE. L'assoluto non cambia la RMSE. Anche la RMSE scarta valori non finiti; per le RMSE log10 di `N_d` e `R_d` sono ammessi soltanto predizioni e dati positivi finiti.

Evidenza: [helper RMSE, 943–955](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:943) e [obiettivo, 976–1040](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/physical_cell_1.source.txt:976), corrispondenti al [notebook, JSON 1397–1461](/home/destro/sciantix-official/UN_model/notebooks/ARCHIVIO/UN_Barani_model.ipynb:1397).

Non ci sono pesi basati sull'incertezza sperimentale, né una normalizzazione dell'errore swelling. Ogni punto ha lo stesso peso all'interno della propria RMSE; i tre blocchi principali sono sommati con coefficiente 1. `score_pressure` contiene **già** il fattore 0.15. `score_swig_diag` è la RMSE dello swelling totale bulk+dislocazioni, salvata **solo come diagnostica**. Non entrano in questo score una penalità esplicita sulla caduta di `N_d`, un prior su Fdot o un confronto di partizione gas con Rizk: questi appartengono allo sweep globale successivo.

### Controllo aritmetico sui risultati salvati, senza simulare

| Componente FINAL | Valore nel CSV |
|---|---:|
| `score_swd` | 0.8264825611639194 |
| `score_Nd` | 0.4877610182736616 |
| `score_Rd` | 0.10101100031512107 |
| `score_pressure` (già pesato) | 0.07207034824953623 |
| `highT_penalty` | 0.2194798636451756 |
| **Somma = `score_total`** | **1.7068047916474136** |
| `score_swig_diag` (escluso dalla somma) | 2.0466734352125155 |

La somma ricalcolata dai decimali serializzati differisce dal totale salvato di `2.22e-16`, compatibile con l'arrotondamento floating point.

Ulteriori valori utili per riconoscere le schermate, tutti dal [CSV originale FINAL, riga 2](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/original/UN_M7_physical_sweep_outputs/physical_final_top.csv:2):

| Condizione | Valori salvati |
|---|---|
| 1600 K, 1.3% FIMA | Swelling dislocazioni **2.668592103851043%**, bulk **1.254397391878499%**; `Rd=92.43879195817759 nm`, `Nd=8.065490864498744e18 m^-3`. |
| 1600 K, 3.2% FIMA | Swelling dislocazioni **5.061468420296727%**, bulk **5.172271860209517%**. |
| 1800 K, 3.2% FIMA | Swelling dislocazioni **25.389209537816537%**. |
| 2000 K, 3.2% FIMA | Swelling dislocazioni **50.97399318225878%**; genera la penalità `0.02*(50.97399318225878-40)=0.2194798636451756`. |

Il riconoscimento della calibrazione non equivale a una validazione fisica del suo andamento ad alta T: il valore a 2000 K era già presente negli output originali.

## 7. Collegamento con `UN_M7_global_sensitivity_fullcell.py`

Il nome esatto `UN_M7_global_sensitivity_fullcell.py` compare nel [documento di contesto, riga 409](/home/destro/sciantix-official/UN_model/reports/UN_Thesis_M7_context_for_new_chat.md:409), descritto come file autonomo da incollare in una singola cella. **Non è stato trovato un file con quel nome nel working tree o fra i percorsi delle revisioni Git raggiungibili.** È invece presente il codice globale autonomo nella **cella indice 5** del notebook `UN_Barani_model.ipynb`, associato agli output nella directory `UN_M7_global_sensitivity_fullcell` introdotti da `303595b9`.

La [sorgente globale estratta senza esecuzione](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/global_cell_5.source.txt:1) conserva phi nel gas per entrambe le popolazioni (689–690), ma **la sua calibrazione migliore è diversa**: `FINAL_rank2_LHS_0021`, score `1.4305424032078753`, `f_n=4.6985326242526694e-08`, `Fdot=4.763851636728949e19`, `Dv_scale=0.5911018179965486`, con ulteriori fattori variabili. Vedi [riepilogo globale originale](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/original/UN_M7_global_sensitivity_fullcell/global_sensitivity_summary.md:16).

La cella globale include un anchor `ANCHOR_prev_region_nomFdot` vicino al vecchio optimum, ma con **Fdot cambiato a 5e19**: non è la simulazione richiesta ([sorgente globale 1201–1206](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/global_cell_5.source.txt:1201)). La ricerca numerica dei sei parametri nel CSV `global_fast_all.csv` non trova una corrispondenza esatta.

Lo score globale ha pesi `W_SWELLING=1`, `W_RD=0.6`, `W_ND_LEVEL=0.65`, `W_ND_DROP=1`, `W_PRESSURE=0.03`, `W_FDOT_PRIOR=0.25`, `W_RIZK_GAS_SHAPE=0.08`, `W_HIGHT_GUARD=0.04` ([62–70](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/global_cell_5.source.txt:62), [1104–1113](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/global_cell_5.source.txt:1104)). **Non va attribuito retroattivamente a `best_M7_physical_rank1`.**

Anche lo script corrente `UN_model/optuna/M7_v14/UN_M7_optuna_calibration_v14_rhoSat_qgbStrict_NdAnchors_STANDALONE.py` usa il nome di output `UN_M7_global_sensitivity_fullcell`, ma contiene evoluzioni successive del modello: il solo nome della cartella non identifica la versione del solver.

## 8. Evidenze recuperate, controlli e incertezze residue

La directory di evidenze è:

```text
/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/
```

Contiene gli originali Git, gli estratti testuali delle due celle, lo stdout physical salvato, il CSV dei dati sperimentali e **11 figure originali incorporate nel notebook**, estratte dai payload PNG senza rigenerarle. Esempi: [swelling a 1.3% FIMA](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/embedded_figures/best_M7_physical_rank1_swelling_T_1.3FIMA.png), [raggio](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/embedded_figures/best_M7_physical_rank1_radius_T_1.3FIMA.png), [concentrazione](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/embedded_figures/best_M7_physical_rank1_concentration_T_1.3FIMA.png).

I nomi delle copie PNG sono ricavati dall'ordine delle chiamate di plotting. Sono i byte delle immagini **incorporate nel notebook**, non copie certificate dei PNG eventualmente scritti su disco con `savefig(dpi=200)`: quei file separati non sono stati trovati nella cronologia dei percorsi physical.

Il [manifest JSON](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/manifest.json:1) registra revisioni complete, percorsi originali, blob Git e SHA-256 dei file recuperati. Gli estratti `.source.txt` sono trascrizioni letterali, **non script eseguiti**. I numeri di riga del codice in questo report si riferiscono a tali estratti, partendo da 1; il [CSV di corrispondenza](/home/destro/sciantix-official/UN_model/reports/evidence/best_M7_physical_rank1_2026-04-30/source_line_map.csv:1) collega ogni riga alle righe JSON del notebook attuale. Per la cella physical, `riga_JSON = riga_sorgente + 421`.

**Livello di certezza:** elevato per identificazione, formule, opzioni, dati numerici, obiettivo e impostazioni FINAL: notebook, stdout e CSV concordano, e la sorgente è identica nelle revisioni confrontate. Come per qualunque notebook salvato, manca un log esterno che certifichi lo stato del kernel al momento del calcolo; la concordanza di sorgente e risultati è l'evidenza disponibile.

Restano non documentati l'istante esatto dell'esecuzione, le versioni complete delle dipendenze e la provenienza puntuale della digitalizzazione/refit Dv. I metadati del notebook indicano kernel `.venv`, Python **3.12.3**, senza lockfile associato all'esecuzione. Non è stata verificata numericamente la riproducibilità tramite un nuovo run, in conformità alla richiesta.

La ricerca ha coperto working tree (inclusi file non tracciati), vecchi notebook e script, risultati e documenti locali, nomi dei contenuti ZIP sotto `UN_model`, e cronologia delle branch/ref Git locali e remote già disponibili. Non sono stati interrogati servizi esterni, ref remoti non scaricati o oggetti Git irraggiungibili: un eventuale file `.py` standalone potrebbe essere rimasto fuori da questi ambiti.

**Operazioni effettuate:** sola lettura degli originali, estrazione e controllo statico/aritmetico, creazione di questo report e delle copie di evidenza in una nuova directory. Nessun codice del modello modificato, nessuna simulazione avviata, nessun checkout/reset o commit effettuato.

Per consultare manualmente gli originali storici senza ripristinare il working tree:

```bash
git show 3416c92a5015a68495303c16b2d72065e8a451ce:3UNpython_tests.ipynb
git show 3416c92a5015a68495303c16b2d72065e8a451ce:UN_M7_physical_sweep_outputs/physical_final_top.csv
git show 303595b954bf9e297757ffbdc37c24dad8525808:UN_Barani_model.ipynb
```
