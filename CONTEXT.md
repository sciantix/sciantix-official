# CONTEXT — Tesi Magistrale: Fission Gas in Uranium Nitride (UN)
## Sciantix — Estensione per UN (Uranium Mononitride)

---

## 1. Introduzione storica e dati disponibili su UN

- Nel **Ritzk** si trovano references su space propulsion test su UN per validation.
- Simulazioni Xe e cluster dynamics → sempre nel Ritzk.
- **Il 30% dei fission products sono gas nobili** (Xe, Kr) e sono loro a dare swelling. Nel modello attuale si considera solo Xe e non Kr, He.

**Ref:**
> *Unit mechanisms of fission gas release: Current understanding and future needs* — ScienceDirect
> https://www.sciencedirect.com/science/article/pii/S0022311517313405

---

## 2. Assunzione delle tre popolazioni di bolle

**Ref principale:**
> *Modeling intra-granular fission gas bubble evolution and coarsening in uranium dioxide during in-pile transients* — ScienceDirect
> https://www.sciencedirect.com/science/article/pii/S0022311520300969

Si assumono **3 popolazioni di bolle**:
1. **Bulk bubbles** — bolle piccole, distribuzione omogenea nel grano
2. **Dislocation bubbles** — bolle grandi, associate a dislocazioni; distribuzione non omogenea (a linee rette). La loro crescita aumenta al crescere della temperatura, sia in UO₂ che in UN.
3. **Intergranular bubbles** — ai bordi di grano

### Evidenze sperimentali — Bolle su dislocazioni

**UO₂:**
- Ref [19] — C. Baker, *The fission gas bubble distribution in uranium dioxide from high temperature irradiated SGHWR fuel pins*, J. Nucl. Mater., 66 (1977), pp. 283–291
- *Re-solution of fission gas – A review: Part I. Intragranular bubbles* — ScienceDirect
  https://www.sciencedirect.com/science/article/pii/S002231150600198X
  > "TEM observations such as that shown in Fig. 2(a) clearly show intragranular bubbles lying in straight line"

**MX fuel (UC e UN):**
> "The critical temperature previously defined is the least affected by changes of the environmental conditions (neutron spectrum, heat rating, fuel design), and effectively independent of the ceramographic structure of the pellet (grain size, porosity), and thus more suitable to characterize the intrinsic properties of a fuel. In most cases microscopic swelling increases sharply across the isothermal of the critical temperature, and therefore this can be readily detected from the radial swelling profiles."

**Ref:**
> C. Ronchi, I.L.F. Ray, H. Thiele, J. Van De Laar — *Swelling analysis of highly-rated MX-type LMFBR fuels: II. Microscopic swelling behaviour*, Journal of Nuclear Materials 74 (1978) 193–211, North-Holland Publishing Company, Karlsruhe Establishment, European Institute for Transuranium Elements.

### Teorie di crescita delle bolle su dislocazioni

Riferimenti teorici per pipe diffusion e crescita delle bolle su dislocazioni (usati nel modello Sciantix):

**Ref:**
> *Pipe and grain boundary diffusion of He in UO₂* — C.O.T. Galvin, M.W.D. Cooper, P.C.M. Fossati, C.R. Stanek, R.W. Grimes, D.A. Andersson — Imperial College London / Los Alamos National Laboratory

> Altre refs nel file di riferimento di Pizzocri coarsening oppure nel Ritzk (tipo pipe diffusion Xe).

---

## 3. Sciantix → Adattamento per Uranium Nitride (UN)

### 3.1 System.C — Diffusività del gas di fissione

```
setFissionGasDiffusivity(
    iFissionGasDiffusivity = 11
```

- Si usa **1 sola diffusività** (sia per Xe che per Kr); definita solo per Xe.
- Il termine **D₂** (irradiation-enhanced diffusion per Xe) è **trascurabile** secondo Ritzk:
  > "The irradiation-enhanced diffusion for Xe (D2) calculated using Centipede is neglected because improvements to the underlying atomistic defect parameters have demonstrated that D2 remains negligible compared to D1+D3 for all relevant conditions [66]. However, D2 is still a significant contributor for vacancy diffusion."
- D₂ corrisponde alla **D_g** nella formula.

**I tre contributi alla diffusività:**
- **D₁** — diffusione termica (solo T, senza irraggiamento)
- **D₂** — radiation-enhanced diffusion (dipende da T + √fission rate density); trascurata per Xe
- **D₃** — radiation-induced mixing (athermal, lineare con fission rate density; domina a bassa T)

### 3.2 Nuova funzione: `setTrappingRatesUN()`

```cpp
double trapping_rate_bulk;
double trapping_rate_dislocation;
```
- `gb` e `gd` definiti **separatamente** in `system.h`
- Su `SciantixVariable` definire anche **intragranular dislocation bubble** separata da intragranular bulk bubble

### 3.3 Setvariables.c / Updatevariables.c

- Si assume una **dislocation density costante** `rho_d`
- Il gas release tramite interconnessione bolle su dislocazioni **non viene modellato** (evoluzione del numero di dislocazioni e numero di bolle su dislocazioni non modellati)
- `rho_d` da definire dentro `matrix.h`:

```cpp
double dislocation_density;
double dislocation_core_radius;
setDislocationDensity(...)
setDislocationCoreRadius(...)
```

In `setmatrix.c` per il caso UN:
```cpp
matrix_.setDislocationDensity(1.0e14);       // MANCA VALORE
matrix_.setDislocationCoreRadius(5.0 * 3.8e-10);  // MANCA VALORE — 5.0 * burg_vector
```

### 3.4 Nuove variabili in UpdateVariables.c e SetvariablesFunctions.c

```cpp
{163, "Dislocation bubble concentration"},
{164, "Dislocation bubble radius"},
```

In `SetvariablesFunctions.c`:
```cpp
SciantixVariable("Dislocation bubble concentration", "(bub/m3)", Sciantix_variables[19], Sciantix_variables[19], 0),
SciantixVariable("Dislocation bubble radius", "(m)", Sciantix_variables[20], Sciantix_variables[20], 0),
SciantixVariable("Dislocation bubble volume", "(m3)", 0.0, 0.0, 0),
```

Costruttore `SciantixVariable` (definito in `include/classes/SciantixVariable.h` line 43):
```cpp
SciantixVariable(std::string name, std::string uom, double initial_value, double final_value, bool output)
```

Differenza rispetto a `"Intragranular bubble concentration"` (output = 1 → va in output).

### 3.5 Resolution → due `b` separati (bulk e dislocation)

- Il parametro di re-solution **b dipende da R** → ci sono **R_bulk** e **R_dislocation** → due resolution rate separati
- Rif. equazione (23) nel Ritzk

### 3.6 Trapping su dislocazioni

- Non presente su Sciantix UO₂ originale
- Presente nella **ref 39** per UO₂

**Differenze Ritzk vs ref 39 sulla formula del trapping Γ_d:**
- Differenza nelle unità di misura di `Gamma_d` (sembra un errore nel Ritzk)
- Differenza nel termine che dà la frazione di dislocazione effettivamente nuda:
  - Ritzk ha `rho_d²` a numeratore
  - Ref 39 NON ha quel termine
- **Interpretazione:** Ritzk ha scritto per sbaglio `rho_d` due volte (va tolto dal numeratore perché viene sostituito da densità disloc [m/m³] meno lunghezza occupata da bolle su disloc [m/m³])

---

## 4. Nucleazione delle bolle bulk

### 4.1 Homogeneous vs Heterogeneous nucleation

| Modello | Approccio | Fuel |
|---|---|---|
| Ritzk (UN) | Homogeneous (diffusion limited) | UN |
| Ref 39 UO₂ + Sciantix UO₂ | Heterogeneous (from fission spikes) | UO₂ |
| U₃Si₂ (Politecnico) | Homogeneous | U₃Si₂ |

**Ref U₃Si₂:**
> *Multiscale modeling of fission gas behavior in U₃Si₂ under LWR conditions* — ScienceDirect
> https://www.sciencedirect.com/science/article/pii/S0022311519301151

Nota: U₃Si₂ usa la stessa formula del trapping ma con `D_rel = 2*D`.

**Ref diffusion-controlled reaction rates:**
> Frank C. Collins & George E. Kimball — *Diffusion-Controlled Reaction Rates*, Dept. of Chemistry, Columbia University, New York, 1949.

### 4.2 Nucleazione omogenea — formula

Rate di formazione di dimeri:
```
Rate = f_N * k_N * c²

k_N = z * Ω_fg^(1/3) * D
```
- `D` = diffusività del fg in UO₂
- `Ω_fg^(1/3)` = raggio di un atomo fg
- `z ≈ 75` = numero di siti reticolari attorno a un atomo fg che, se occupati, garantiscono la formazione del dimero

Valori di `f_N` proposti: 10⁻⁷ → 10⁻² (Olander: "little more than an adjustable parameter"). Il valore usato da Ritzk per UN (ereditato da U₃Si₂): **10⁻⁶**.

**Formula nucleazione bolle bulk:**
```
ν_b = 8π f_n D_g Ω_f^(1/3) c²
```

### 4.3 Single-size model

**Assunzione:** distribuzione di dimensioni non considerata; `C_b` e `m` rappresentano valori medi.

Rate di nucleazione di bolle da m atomi:
```
Rate = (1/m) * f_N * k_N * c²
```
> "The factor 1/m … is the Achilles heel of the single-size method because it implies essentially instantaneous growth of dimers to m-atom bubbles." — Olander

**Interpretazione del fattore 1/m:**
- `f_N k_N c²` dà il numero di atomi che entrano in "stato nucleante"
- divisi per m → numero di bolle da m atomi create per unità di tempo
- Non c'è dinamica temporale dei precursori (dimeri → trimeri → ecc.)

**Il sistema reale (non implementato) sarebbe:**
```
dn₂/dt = f_N k_N c² - G_{2→3}
dn₃/dt = G_{2→3} - G_{3→4}
...
dn_m/dt = G_{m-1→m}
```

**Motivazione dell'uso di nucleazione omogenea per UN:**
1. La formula di nucleazione per trapping e formazione dimeri ha la stessa forma (guidata dalla diffusione) → tarando il parametro si riesce a tenere conto della somma dei due processi
2. Non va a togliere completamente la dipendenza dal fission rate (D₂, D₃ dipendono dal fission rate)
3. Le bolle su dislocazioni modellano in modo semplificato la nucleazione eterogenea su difetti
4. Xe in UN ha alta mobilità + scarsa stabilizzazione su difetti → clustering per collisione diffusiva → non serve sito eterogeneo iniziale (supportato da MD: *Molecular dynamics simulations of xenon in uranium carbide and nitride fuels*)

### 4.4 Heterogeneous nucleation — modello (UO₂, riferimento)

Rate di nucleazione:
```
dC_b/dt = 2F * α
```
- `2F` = flusso di ff (due per fissione)
- `α` = numero di bolle nucleate da ciascun ff
- Nel modello eterogeneo: si impone **dC_b/dt = 0** (equilibrio istantaneo tra nucleazione e distruzione da ff)

**Fisica:** i fission fragments creano displacement spikes → alta concentrazione locale di vacanze → collasso in void → nucleo per nuova bolla se c'è gas sufficiente.

**Ref:**
> J.A. Turnbull — *The distribution of intragranular fission gas bubbles in UO₂ during irradiation* — Central Electricity Generating Board, Berkeley Nuclear Laboratories

---

## 5. Re-solution — teoria e modelli

### 5.1 Heterogeneous re-solution (Turnbull — UO₂)

Parametro di re-solution eterogenea:
```
b_het = π(R_b + R_ff)² * (2F * μ_ff)
```
- `F` = densità di fissioni [m⁻³s⁻¹]
- `μ_ff` = range dei fission fragments [m]
- `2F` = numero di ff prodotti per volume e tempo
- `π(R_b + R_ff)²` = sezione d'urto geometrica bolla–traccia

**Fisica:** i ff creano bolle (displacement spike + vacanze) e le distruggono → entrambi processi velocissimi → `C_b` si stabilizza subito a valore di equilibrio.

### 5.2 Homogeneous re-solution (Nelson — non-oxide fuels, UN)

**Formula (Nelson, coulombian cross section):**
```
b_hom^ff = μ_ff * F * [2π Z⁴e⁴ / E_ff_max * E_min] * ln(E_ff_max / E_min)
```

**Rapporto homogeneous/heterogeneous:**
```
b_hom / b_het = [Z⁴e⁴ / (R_b + R_ff)²] * [E_ff_max / E_min] * ln(E_ff_max / E_min)
```

**Valori tipici:**
- `E_ff_max ≈ 67 MeV`
- `E_min ≈ 0.3 keV`
- `R_b ≈ 1 nm`
- **b_hom / b_het ∼ 10⁻³** → la resolution omogenea è molto più debole

**Fisica:** ogni urto espelle ≈ 1 atomo (ν ≈ 1). Energy threshold per uscire dalla bolla: `E_min ∼ 300 eV`. La sezione d'urto microscopica è molto più piccola della sezione d'urto geometrica.

### 5.3 Perché modello omogeneo per UN/UC e non per UO₂

**Fisica chiave:**

| Fuel | Conducibilità | Thermal spike | Meccanismo dominante | Tipo |
|---|---|---|---|---|
| UO₂ | bassa | ~2000 K | distruzione bolla (Turnbull) | heterogeneous |
| UC / UN | alta (metallica) | ~50 K | knock-out atomico (Nelson) | homogeneous |

- La cascata collisionale esiste **sempre** in entrambi i casi.
- La differenza è **quanto l'energia resta localizzata** vs **quanto si dissipa velocemente**.
- In UO₂: bassa conducibilità → thermal spike enorme → onda termomeccanica → distruzione totale della bolla.
- In UC/UN: alta conducibilità → energia si disperde → thermal spike piccolo → solo knock-out graduale di singoli atomi.

**Refs chiave:**
> H. Blank & H. Matzke — *The effect of fission spikes on fission gas re-solution*, Radiat. Eff., 17 (1973), pp. 57–64
>
> C. Matthews, D. Schwen, A.C. Klein — *Radiation re-solution of fission gas in non-oxide nuclear fuel*, J. Nucl. Mater., 457 (2015), pp. 273–278
>
> C. Ronchi & P.T. Elton — *Radiation re-solution of fission gas in uranium dioxide and carbide*, J. Nucl. Mater., 140 (1986), pp. 228–244

### 5.4 Paper Ronchi–Elton 1986 — punti chiave

1. Meccanismo collisionale (atomico) → too weak: rate calcolato ~3×10⁻⁵ s⁻¹, troppo basso
2. Rate sperimentale osservato ~10⁻⁴ s⁻¹ → manca un meccanismo
3. In UO₂ domina lo **shock termomeccanico** (thermal spike → onda d'urto → frammentazione bolla)
4. Lo shock crea pressioni ~10–30 GPa; si riflette come onda tensile alla superficie della bolla → distrugge la bolla geometricamente
5. **Punto critico:** "bubble disappearance ≠ gas re-solution" → la bolla sparisce ma il gas NON diventa atomico nel reticolo; si redistribuisce in micro-volumi/difetti
6. In UC il thermal spike è ~50 K → niente shock → basta il modello collisionale
7. La distruzione meccanica è più efficace in UO₂, ma NON è vera re-solution

### 5.5 Re-solution e nucleazione eterogenea — relazione (Olander)

> "In the heterogeneous model, fission fragments play the dual role of creator and destroyer of intragranular bubbles. If a ff re-solves bubbles and nucleates new ones in the same track, it is very likely that re-solution simply produces a very high local concentration of fg atoms from which new bubbles nucleate. The net result is disappearance and re-appearance of bubbles that contain the same fg atoms."

---

## 6. Parametro di re-solution φ (phi) — dibattito aperto

### 6.1 Definizione del parametro b

Dal paper Matthews et al. [48] (ref. di Ritzk):
> "b represents the number of escaped atoms per bubble atoms for a single fission and single bubble"

Unità: **atom/atom per fission** — cioè probabilità di espulsione per atomo per fissione.

**Eq. (6) Ritzk — formulazione generale:**
```
b = Ḟ ∫ b₀(R) m'(R) α(R) dR
```
- `b₀(R)` = resolution parameter per atomo per fissione
- `m'(R) = m(R)/N(R)` = atomi per bolla di raggio R
- `α(R)` = distribuzione di concentrazione delle bolle

**Semplificazione single-size (una sola dimensione media per popolazione):**
```
α(R) = N δ(R - R_b)  →  b = Ḟ b₀(R_b) m
```
Ma poiché b è definito **per atomo**, si arriva a:
```
b = Ḟ b₀(R_b)   →  Eq. (9) Ritzk
```

### 6.2 Confronto Ritzk vs Barani (U₃Si₂) vs Ref39

**Ritzk:**
- `b*m` → atomi rimessi in soluzione (bilancio su m)
- `b*φ*N` → bolle distrutte (bilancio su N)
- `φ = 1/(n−1)` dove n = m/N = dimensione media
- φ **non compare** nelle equazioni per m (b agisce direttamente sugli atomi)

**Barani U₃Si₂ / Ref39:**
- `α*φ*N` → bolle distrutte
- `α*φ*m` → atomi persi per distruzione di bolle intere (termine distinto dalla re-solution atomica!)
- La perdita di atomi da distruzione bolle: `(αφN) * (m/N) = αφm` → coerente
- φ compare anche in m perché quel termine rappresenta **la perdita di atomi dalla distruzione dell'intera bolla**, non la re-solution atomica singola

**Incoerenza notata nel Ritzk:**
- Ritzk fa `bφN` per le bolle ma non aggiorna coerentemente gli atomi associati alla bolla distrutta rispetto al bilancio di m
- La "grandezza media delle bolle morte" risulterebbe `bm / bφN = m/φN = (n−1)*n` → bolle che muoiono avrebbero dimensione ~n² → fisicamente sbagliato
- Il problema nasce perché `bm` include gli atomi espulsi da **tutte le bolle**, non solo da quelle distrutte

**Conclusione delle note:**
- Il parametro φ non ha senso fisico rigoroso nel single-size model (distribuzione di bolle ignota)
- È utile come parametro correttivo: resolution su bolle di dimensione media 10 ha minore probabilità di distruzione rispetto a bolle di dimensione 5, e questa differenza scala come φ = 1/(n−1)
- **Raccomandazione:** usare b come rate per bolla (come altri lavori, trattandolo come probabilità) e mettere φ ovunque per rispettare il bilancio di atomi:
  - `b φ N` = numero bolle distrutte di dimensione media n
  - `b φ m` = atomi rimessi in soluzione (con N*n = m)

---

## 7. Equazioni del modello Ritzk — sistema di ODEs

### 7.1 Sistema completo (equazioni 21a–21f)

**(21a)** — gas in soluzione:
```
∂c/∂t = D_g ∇²c − (g_b + g_d) c + b_b m_b + b_d m_d + β
```

**(21b)** — gas nelle bolle bulk / dislocation:
```
∂m_{b,d}/∂t = g_{b,d} c − b_{b,d} m_{b,d}
```

**(21c)** — numero di bolle bulk:
```
∂N_b/∂t = ν_b − b_b φ_b N_b
```

**(21d)** — numero di bolle su dislocazioni:
```
∂N_d/∂t = N_d (1/ρ_d) (∂ρ_d/∂t) − 4λ N_d² dV_d/dt
```

**(21e)** — volume delle bolle:
```
∂V_{b,d}/∂t = Ω_fg N_{b,d} (∂m_{b,d}/∂t) + Ω N_{b,d} (∂n_{b,d}/∂t)
```

**(21f)** — numero di vacanze nelle bolle:
```
∂n_{b,d}/∂t = 2π D_v δ_{b,d} N_{b,d} (kT/ζ_{b,d}) (p_{b,d} − p_{b,d}^eq)
```

### 7.2 Nomenclatura

| Simbolo | Significato | Unità |
|---|---|---|
| c | concentrazione gas in soluzione | at/m³ |
| m_{b,d} | gas totale nelle bolle bulk/disloc | at/m³ |
| N_{b,d} | concentrazione numerica bolle | bub/m³ |
| D_g | diffusività gas | m²/s |
| D_v | diffusività vacanze | m²/s |
| g_{b,d} | trapping rate (bulk/disloc) | s⁻¹ |
| b_{b,d} | re-solution rate | s⁻¹ |
| β | produzione gas da fissione | at/m³s |
| ρ_d | densità lineare dislocazioni | m⁻² |
| λ | fattore correttivo coalescenza (sfere rigide) | — |
| Ω_fg | volume reticolare atomi fg | m³ |
| Ω | volume reticolare vacanze | m³ |
| n_{b,d} | vacanze nelle bolle | — |
| p_{b,d} | pressione reale nelle bolle | Pa |
| p_{b,d}^eq | pressione di equilibrio | Pa |
| δ_{b,d} | raggio cella Wigner-Seitz | m |
| ζ_{b,d} | sink strength | — |
| φ_b | fattore correttivo re-solution | — |
| ν_b | tasso di nucleazione bolle bulk | m⁻³s⁻¹ |

### 7.3 Note sulle equazioni — confronto con Ref39 e U₃Si₂

- **Ritzk:** nell'equazione per c manca il termine che sottrae gli atomi nucleati come dimeri (→ presente in Ref39, Pizzocri 2020, Barani U₃Si₂)
- **Ref39 / Barani:** nucleazione di dimeri contribuisce 2 atomi a m e -2 atomi a c
- **Ref39 (eq.20):** per N_b c'è termine aggiuntivo di coalescenza delle bulk bubbles; Ritzk non lo ha
- **Coalescenza:**
  - Ritzk: solo disloc–disloc
  - Ref39: disloc–disloc + disloc–bulk
- **Nota (eq.20 Ref39):** non chiaro il termine che sposta bolle dal bulk alla popolazione dislocation (manca il termine corrispondente che toglie atomi da m_b e li mette in m_d)
- **Nei modelli omogenei:** si impone `dm/dt = 0` (dimensione media si adatta velocemente, bilancio trapping-resolution)
- **Nei modelli eterogenei:** si impone `dC_b/dt = 0` (numero bolle si adatta velocemente)

---

## 8. Nucleazione nel modello di Nelson (1968)

**Ref:**
> R.S. Nelson — *The stability of gas bubbles in an irradiation environment*, UKAEA, Atomic Energy Research Establishment (Solid State Div.), received October 1968

**Modello di Nelson (production-limited capture):**
- Errore del modello precedente: si assumeva che il rate di cattura dipendesse **solo** dalla diffusione termica
- Correzione di Nelson: il rate dipende anche dalla **produzione di gas da fissione**
- Se la diffusione è molto più veloce della produzione → il collo di bottiglia è la produzione

**Condizione di nucleazione (eq. 13–14):**
```
n = f ṅ₀ N₀ ε / (m* η'(E_t*))
```
- `f` = frazione di gas prodotto nei ff
- `ṅ₀` = flusso neutronico
- `N₀ ε` = densità atomi fissili effettivi
- `m*` = numero di atomi per nucleo critico (= 2, dimero)
- `η'(E_t*)` = tasso di distruzione del nucleo (re-solution)

**Risultato:** n ~ 10¹⁷ /cm³ → ottimo accordo con osservazioni sperimentali

**Conclusione fisica:** nucleazione = equilibrio dinamico tra produzione (fissione) e distruzione (re-solution da ff). NON dipende dalla temperatura né dal fission rate (se diffusione rapida è giustificata).

---

## 9. Multiscale modeling

**Definizione:**
- **Scala atomistica (Å–nm, ps–ns):** DFT, MD, BCA → energia di formazione difetti, barriere migrazione Xe, diffusività vacanze, rate re-solution, binding energies
- **Mesoscala (nm–µm, ns–s):** cluster dynamics, phase-field, kMC → nucleazione bolle, crescita, distribuzioni dimensioni
- **Scala ingegneristica (µm–cm, ore–anni):** fuel performance codes (BISON, FRAPCON, TRANSURANUS) → swelling, FGR, comportamento meccanico

Sciantix è un modello multiscala.

**Ref U₃Si₂:**
> *Multiscale modeling of fission gas behavior in U₃Si₂ under LWR conditions* — ScienceDirect
> https://www.sciencedirect.com/science/article/pii/S0022311519301151

---

## 10. Implementazione in Sciantix — codice

### 10.1 Nuove variabili (SciantixVariable)

**SetVariablesFunctions.c:**
```cpp
// AD UN URANIUMNITRIDE
SciantixVariable("Dislocation bubble concentration", "(bub/m3)", Sciantix_variables[19], Sciantix_variables[19], 0),
SciantixVariable("Dislocation bubble radius", "(m)", Sciantix_variables[20], Sciantix_variables[20], 0),
SciantixVariable("Dislocation bubble volume", "(m3)", 0.0, 0.0, 0),
```

**UpdateVariables.c:**
```cpp
{163, "Dislocation bubble concentration"},
{164, "Dislocation bubble radius"},
```

**GasDiffusion.C:**
```cpp
SciantixVariable("Xe in dislocation bubbles", "(at/m3)", 0.0, 0.0, 0),  // AD UN URANIUMNITRIDE
```

### 10.2 Matrix.h / SetMatrix.c — parametri materiale UN

```cpp
double dislocation_density;
double dislocation_core_radius;
setDislocationDensity(...)
setDislocationCoreRadius(...)
```

```cpp
// setmatrix.c — caso UN
matrix_.setDislocationDensity(1.0e14);             // MANCA VALORE DEFINITIVO
matrix_.setDislocationCoreRadius(5.0 * 3.8e-10);   // ~ 5 * vettore di Burgers — MANCA VALORE DEFINITIVO
```

### 10.3 Solver.h / Solver.C — nuovo solver spettrale 3 equazioni

```cpp
// AD UN URANIUMNITRIDE
void Solver::SpectralDiffusion3equationsExchange(
    double& c,
    double& m_b,
    double& m_d,
    double* modes_c,
    double* modes_m_b,
    double* modes_m_d,
    std::vector<double> parameter,
    double increment)
```

**Sistema risolto (mediato spazialmente su grano sferico):**
```
dc/dt   = D_g ∇²c − (g_b + g_d) c + b_b m_b + b_d m_d + β
dm_b/dt = g_b c − b_b m_b
dm_d/dt = g_d c − b_d m_d
```

**Parametri (vettore):**
```
0: N_modes
1: D_g
2: r (grain radius)
3: beta
4: g_b (capture rate bulk bubbles)
5: g_d (capture rate dislocation bubbles)
6: b_b (re-solution rate bulk)
7: b_d (re-solution rate dislocation)
```

**Metodo:** approccio spettrale in spazio, backward Euler in tempo → sistema 3×3 lineare per modo.

### 10.4 Simulation.h — modalità diffusione dislocation bubbles

```cpp
std::vector<double> modes_initial_conditions_dislocation_bubbles;
modes_initial_conditions_dislocation_bubbles.resize(200);

// AD UN URANIUMNITRIDE
double* getDiffusionModesDislocationBubbles(std::string gas_name)
{
    if (gas_name == "Xe")   return &modes_initial_conditions_dislocation_bubbles[0 * 40];
    else if (gas_name == "Kr")    return &modes_initial_conditions_dislocation_bubbles[1 * 40];
    else if (gas_name == "He")    return &modes_initial_conditions_dislocation_bubbles[2 * 40];
    else if (gas_name == "Xe133") return &modes_initial_conditions_dislocation_bubbles[3 * 40];
    else if (gas_name == "Kr85m") return &modes_initial_conditions_dislocation_bubbles[4 * 40];
    ...
}
```

I modi delle dislocation bubbles sono **interni** (non accoppiati all'array esterno `Sciantix_diffusion_modes[]`) per preservare la compatibilità backward.

### 10.5 GasDiffusion.C — nuovo ramo iDiffusionSolver = 4

**Attivazione:** `iDiffusionSolver = 4` nell'input.

**Operazioni:**
- Prende `c = "<Gas> in intragranular solution"`, `m_b = "<Gas> in intragranular bubbles"`, `m_d = "<Gas> in dislocation bubbles"`
- Chiama `solver.SpectralDiffusion3equationsExchange(...)`
- Aggiorna `"<Gas> in grain" = c + m_b + m_d`

### 10.6 Precursor factor per gas radioattivi

| Gas | Precursor factor |
|---|---|
| Xe | 1.00 |
| Xe133 | 1.25 |
| Kr | 1.00 |
| Kr85m | 1.31 |
| He | 1.00 |

Sciantix non risolve esplicitamente due equazioni (gas + precursore) ma usa un'**effective diffusivity**:
```
D_g_eff = D_g * precursor_factor
```

---

## 11. Punti aperti e TODO

- [ ] **MANCA VALORE:** `dislocation_density` per UN (attualmente 1.0e14, placeholder)
- [ ] **MANCA VALORE:** `dislocation_core_radius` per UN (attualmente 5 × Burgers vector)
- [ ] **Mancano da implementare:** parametri per equazioni 21d, 21e, 21f
- [ ] **Differenza Gamma_d:** sembra errore in Ritzk (rho_d² al numeratore invece di rho_d)
- [ ] **Coerenza phi:** valutare se usare phi anche nell'equazione per m (come Barani/Ref39) per rispettare bilancio di atomi
- [ ] **Nucleazione — termini mancanti:** in Ritzk manca il termine che sottrae da c gli atomi nucleati come dimeri (presente in Ref39 e U₃Si₂)
- [ ] **Termine coalescenza eq.20 Ref39:** non chiaro il termine che trasferisce bolle da bulk a disloc con il corrispettivo trasferimento di atomi da m_b a m_d
- [ ] **Restart/coupling:** i diffusion modes delle dislocation bubbles non vengono salvati nell'array esterno (solo in memoria durante il run)
- [ ] **D₂ per Xe:** trascurata su indicazione Ritzk, ma mancano parametri completi per calcolarla
- [ ] **Gas release da interconnessione bolle su dislocazioni:** non modellato (evoluzione ρ_d non modellata)

---

## 12. Riferimenti bibliografici chiave

| Label | Riferimento |
|---|---|
| Ritzk | Paper principale di riferimento per il modello UN in Sciantix (Rizk et al.) |
| Ref39 | Pizzocri 2020 — *Modeling intra-granular fission gas bubble evolution and coarsening in UO₂ during in-pile transients*, https://www.sciencedirect.com/science/article/pii/S0022311520300969 |
| Barani / U₃Si₂ | *Multiscale modeling of fission gas behavior in U₃Si₂ under LWR conditions*, https://www.sciencedirect.com/science/article/pii/S0022311519301151 |
| Olander review | *Re-solution of fission gas – A review: Part I. Intragranular bubbles*, https://www.sciencedirect.com/science/article/pii/S002231150600198X |
| Matthews 2015 | C. Matthews et al. — *Radiation re-solution of fission gas in non-oxide nuclear fuel*, J. Nucl. Mater., 457 (2015), pp. 273–278 |
| Ronchi & Elton 1986 | C. Ronchi, P.T. Elton — *Radiation re-solution of fission gas in uranium dioxide and carbide*, J. Nucl. Mater., 140 (1986), pp. 228–244 |
| Cooper 2023 | M.W.D. Cooper, J. Rizk et al. — *Simulations of the self- and Xe diffusivity in UN including chemistry and irradiation effects*, J. Nucl. Mater. 587 (2023) 154685 |
| Kocevski 2022 | V. Kocevski et al. — *Development and application of a UN potential: thermomechanical properties and Xe diffusion*, J. Nucl. Mater. 562 (2022) 153553 |
| Nelson 1968 | R.S. Nelson — *The stability of gas bubbles in an irradiation environment*, UKAEA |
| Turnbull 1970 | J.A. Turnbull — *The distribution of intragranular fission gas bubbles in UO₂ during irradiation*, Berkeley Nuclear Laboratories |
| Baker 1977 | C. Baker — *The fission gas bubble distribution in UO₂ from high temperature irradiated SGHWR fuel pins*, J. Nucl. Mater., 66 (1977) |
| Ronchi 1978 | C. Ronchi et al. — *Swelling analysis of highly-rated MX-type LMFBR fuels: II*, J. Nucl. Mater. 74 (1978) |
| Collins & Kimball 1949 | F.C. Collins, G.E. Kimball — *Diffusion-Controlled Reaction Rates*, Columbia University, 1949 |
| Pizzocri 2020 | D. Pizzocri, T. Barani, L. Luzzi — *SCIANTIX: a new open source multi-scale code for fission gas behaviour modelling*, J. Nucl. Mater., 532 (2020) |
| Ray & Blank 1984 | I. Ray, H. Blank — *Microstructure and fission gas bubbles in irradiated mixed carbide fuels at 2 to 11 a/o burnup*, J. Nucl. Mater. 124 (1984) |
