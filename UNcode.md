---
title: "SCIANTIX — UN (Uranium Nitride) code notes"
---

# SCIANTIX — UN (Uranium Nitride)

Questo file raccoglie, in un unico posto, **equazioni e parametri** effettivamente usati nel codice per il caso **UN** (tag nel codice: `// AD URANIUMNITRIDE` / `// AD UN URANIUMNITRIDE` / `// UN AD URANIUMNITRIDE`).

## Attivazione (input)

- **Matrice UN**: `iFuelMatrix = 2` (vedi `src/operations/SetMatrix.C`, `src/operations/SetSystem.C`)
- **Solver diffusione UN a 3 equazioni (exchange)**: `iDiffusionSolver = 4` (vedi `src/models/GasDiffusion.C`)
- **Diffusività Xe in UN (Rizk 2025)**: `iFissionGasDiffusivity = 11` (vedi `src/classes/System.C`)
- **Trapping UN (bulk + dislocazioni)**: `iTrappingRate = 2` (vedi `src/classes/System.C`)
- **Re-solution UN (bulk + dislocazioni)**: `iResolutionRate = 4` (vedi `src/classes/System.C`)

## Variabili UN aggiunte

### Gas in dislocation bubbles (concentrazione)
Inizializzate in `src/operations/SetVariablesFunctions.C`:
- `"Xe in dislocation bubbles"` `(at/m3)`
- `"Kr in dislocation bubbles"` `(at/m3)`
- `"He in dislocation bubbles"` `(at/m3)`
- `"Xe133 in dislocation bubbles"` `(at/m3)`
- `"Kr85m in dislocation bubbles"` `(at/m3)`

### Dislocation bubbles (microstruttura)
Inizializzate in `src/operations/SetVariablesFunctions.C` e mappate in `src/operations/UpdateVariables.C`:
- `"Dislocation bubble concentration"` `(bub/m3)`  (indice update 163)
- `"Dislocation bubble radius"` `(m)` (indice update 164)

Nota: nel codice sono **ri-usati** gli indici 19–20 (storicamente intragranular bubble conc/radius) come placeholder anche per le dislocation bubbles (vedi commenti in `src/operations/SetVariablesFunctions.C`).

## Parametri materiale UN (Matrix)

Definiti in `src/operations/SetMatrix.C` (funzione `UN(...)`) e accessibili via `Matrix`:

### Parametri UN-specifici (dislocazioni)
- `rho_d` = `matrix.getDislocationDensity()`  `[1/m^2]`
  - valore attuale nel codice: `1.0e14` (placeholder)
- `r_d` = `matrix.getDislocationCoreRadius()` `[m]`
  - valore attuale nel codice: `3.8e-10` (placeholder, ~ Burgers vector)

### Altri valori impostati (placeholder)
- `matrix_density = 14300.0` `(kg/m3)` (TODO nel codice)
- `lattice_parameter = 4.88e-10` `(m)` (TODO nel codice)

## Diffusività Xe in UN (iFissionGasDiffusivity = 11)

Implementata in `src/classes/System.C` (case 11).

### Formula
Sia `T` la temperatura (K) e `Ḟ` il fission rate density (1/(m³·s)).

Costanti:
- `kB = 8.617333262e-5` (eV/K)
- `D10 = 1.56e-3` (m²/s)
- `Q1  = 4.94` (eV)
- `A30 = 1.85e-39` (m⁵)

Componenti:
- `d1 = D10 * exp( -Q1 / (kB*T) )`
- `d2 = 0` (termine irradiation-enhanced presente come commento ma disattivato)
- `d3 = A30 * Ḟ`

Risultato:
- `D_g = (d1 + d2 + d3) * scaling_factors["Diffusivity"]`

## Re-solution UN: bulk + dislocazioni (iResolutionRate = 4)

Implementata in `src/classes/System.C` in `System::setResolutionRatesUN(...)`.

### Definizioni
- `Ḟ` = fission rate density (1/(m³·s)) = `history_variable["Fission rate"]`
- `r_lat` = `radius_in_lattice` (m)
- `R_b,intra = R_intra + r_lat`, con `R_intra = sciantix_variable["Intragranular bubble radius"].getInitialValue()`
- `R_b,disl  = R_disl  + r_lat`, con `R_disl  = sciantix_variable["Dislocation bubble radius"].getInitialValue()`

### Coefficiente b0 (Rizk 2025, come in codice)
Per entrambi (intra e disl):
- `b0(R) = 1.0e-25 * ( 2.64 - 2.02 * exp( -2.61e-9 / R ) )`

### Tassi di re-solution usati dal solver diffusione UN
- `b_b = resolution_rate_intra = Ḟ * b0(R_b,intra) * scaling_factors["Resolution rate"]`
- `b_d = resolution_rate_disl  = Ḟ * b0(R_b,disl)  * scaling_factors["Resolution rate"]`

## Trapping UN: bulk + dislocazioni (iTrappingRate = 2)

Implementata in `src/classes/System.C` in `System::setTrappingRatesUN(...)`.

### Trapping verso bulk bubbles (gb)
Definizioni:
- `N_b` = `"Intragranular bubble concentration"` (bub/m³)
- `R_b` = `"Intragranular bubble radius"` + `radius_in_lattice` (m)

Formula (Ham-like sink):
- se `N_b == 0`: `g_b = 0`
- altrimenti: `g_b = 4 * pi * D_g * R_b * N_b`
- `g_b *= scaling_factors["Trapping rate"]`

### Trapping verso dislocazioni (gd)
Somma di due contributi:
1) **Bolle su dislocazioni** (analogo a gb)
2) **Dislocazione “nuda” (line sink)**

Definizioni:
- `N_d` = `"Dislocation bubble concentration"` (bub/m³)
- `R_d` = `"Dislocation bubble radius"` (m)
- `rho_d` = dislocation density (1/m²) = `matrix.getDislocationDensity()`
- `r_d` = dislocation core radius (m) = `matrix.getDislocationCoreRadius()`
- `Z_d = 5.0` (costante nel codice)
- `Gamma_d = 1 / sqrt(pi * rho_d)` (m) (Wigner–Seitz radius per dislocazioni)
- `R_d,eff = R_d + radius_in_lattice` (m)

Termine bolle:
- `term_bubbles = 4 * pi * D_g * R_d,eff * N_d`

Termine line-sink:
- `den = ln( Gamma_d / (Z_d * r_d) ) - 3/5` (protetto numericamente nel codice)
- `free_dislocation = rho_d - 2 * R_d * N_d` (clamp a ≥ 0 nel codice)
- `term_dislocation = (2*pi*D_g/den) * free_dislocation`

Risultato:
- `g_d = term_bubbles + term_dislocation`
- `g_d *= scaling_factors["Trapping rate"]`

## Solver diffusione UN a 3 equazioni (iDiffusionSolver = 4)

### Variabili (spazialmente mediate sul grano)
- `c` = gas in solution (diffonde) `(at/m³)`
- `m_b` = gas in bulk bubbles `(at/m³)`
- `m_d` = gas in dislocation bubbles `(at/m³)`

### Sistema di equazioni (usato nel solver)
Implementato in `src/classes/Solver.C` (`Solver::SpectralDiffusion3equationsExchange`):

```
dc/dt   = D_g ∇²c − (g_b + g_d) c + b_b m_b + b_d m_d + β
dm_b/dt = g_b c − b_b m_b
dm_d/dt = g_d c − b_d m_d
```

### Vettore parametri passato al solver (Model: "Gas diffusion - <system>")
Costruito in `src/models/GasDiffusion.C` (`defineSpectralDiffusion3EquationsExchange`):

| idx | simbolo | descrizione | unità |
|---:|---|---|---|
| 0 | `N_modes` | numero modi spettrali | (/) |
| 1 | `D_g` | diffusività gas in soluzione (con precursor factor) | (m²/s) |
| 2 | `r` | raggio grano | (m) |
| 3 | `β` | produzione volumetrica gas | (at/m³·s) |
| 4 | `g_b` | trapping verso bulk bubbles | (1/s) |
| 5 | `g_d` | trapping verso dislocation bubbles | (1/s) |
| 6 | `b_b` | re-solution da bulk bubbles | (1/s) |
| 7 | `b_d` | re-solution da dislocation bubbles | (1/s) |

Note codice:
- `D_g` viene costruito come `system.getFissionGasDiffusivity() * system.getGas().getPrecursorFactor()`.
- Per sistemi non-UN, `g_b, g_d, b_b, b_d` sono attesi `0`.
