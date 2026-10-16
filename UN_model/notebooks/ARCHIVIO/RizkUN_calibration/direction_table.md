> **Gerarchia aggiornata:** swelling sui quattro pin prioritario; R_d ≤1600 K secondario forte; R_d >1600 K e N_d solo diagnostiche, fuori dai ranking. Gas partition Rizk come coerenza di modello. Le frecce seguenti descrivono la risposta fisica e non uno score di fit. Le variazioni degli errori sono in [evaluation_directions.md](evaluation_directions.md); il confronto separato in [pareto_comparison.md](pareto_comparison.md).

# Direzioni OAT — aumento di un solo parametro

Le direzioni confrontano low → high, con tutti gli altri parametri alla baseline. Nessuna combinazione è stata eseguita.
La percentuale indicata è 100 × (media high − media low) / |media baseline|, non una derivata locale. Le frazioni di temperature e le differenze low/base/high sono nel CSV.
↑/↓ = segno coerente sui punti sensibili; ↕ = segno diverso con T; ≈ = invariato entro tolleranza relativa 1e-6; * = inversione fra low/baseline/high ad almeno una T.
Swelling = P2/dislocation; gas = percentuale del gas generato; R in nm; N_d e N_gf in m^-3 (N_gf volume equivalente).
N_d sopra 1600 K è una diagnostica molto debole. Pressione esclusa dalla tabella e dai punteggi sperimentali.

## Temperature 900-1800 K

| Parametro | Pin | P2 swelling | R_d | N_d | matrix gas | bulk gas | dislocation gas | grain-face gas | R_gf | N_gf volume equivalent | FGR |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| f_n | AP3.2 | ↓ -3.4e+02% | ↓ -95% | ↑ +19% | ↓ -3.9e+02% | ↑ +1.2e+02% | ↓ -3.4e+02% | ↓ -1e+02% | ↓ -77% | ↑ +44% | ↓ -8.4e+02% |
| f_n | AP3.8 | ↓ -3.4e+02% | ↓ -94% | ↑ +19% | ↓ -3.9e+02% | ↑ +1.2e+02% | ↓ -3.4e+02% | ↓ -1e+02% | ↓ -78% | ↑ +44% | ↓ -9.1e+02% |
| f_n | AP3.4 | ↓ -4.1e+02% | ↓ -1.1e+02% | ↑ +22% | ↓ -4.4e+02% | ↑ +1.1e+02% | ↓ -4e+02% | ↓ -1.1e+02% | ↓ -92% | ↑ +50% | ↓ -7.3e+02% |
| f_n | ANP6 | ↓ -7.2e+02% | ↓ -2.4e+02% | ↑ +35% | ↓ -6e+02% | ↑ +81% | ↓ -6e+02% | ↓ -2.7e+02% | ↓ -3.7e+02% | ↑ +65% | ↓ -3.2e+02% |
| K_d | AP3.2 | ↕* -1.1e+02% | ↓ -1.1e+02% | ↑ +3.1e+02% | ↑* +0.27% | ↑ +4.4% | ↓ -15% | ↑ +1.2% | ↑ +1.6% | ↓ -0.83% | ↑ +16% |
| K_d | AP3.8 | ↕* -1.1e+02% | ↓ -1.1e+02% | ↑ +3.1e+02% | ↑* +0.27% | ↑ +4.4% | ↓ -15% | ↑ +1.2% | ↑ +1.5% | ↓ -0.81% | ↑ +17% |
| K_d | AP3.4 | ↕* -1.1e+02% | ↓ -1.1e+02% | ↑ +3.1e+02% | ↑* +0.23% | ↑ +3.6% | ↓ -15% | ↑ +0.9% | ↑ +1.3% | ↓ -0.7% | ↑ +9.5% |
| K_d | ANP6 | ↕* -1.1e+02% | ↓ -1.1e+02% | ↑ +3.1e+02% | ↕* +0.11% | ↑ +1.4% | ↓ -15% | ↑ +0.36% | ↑ +1.3% | ↓ -0.27% | ↑ +1.1% |
| rho_d | AP3.2 | ↑ +1.8e+02% | ↕* +9.3% | ↑ +1.5e+02% | ↓ -4.5% | ↓ -53% | ↑ +1.8e+02% | ↓ -16% | ↓ -19% | ↑ +11% | ↓ -1.8e+02% |
| rho_d | AP3.8 | ↑ +1.8e+02% | ↕* +9% | ↑ +1.5e+02% | ↓ -4.5% | ↓ -52% | ↑ +1.8e+02% | ↓ -16% | ↓ -19% | ↑ +10% | ↓ -1.8e+02% |
| rho_d | AP3.4 | ↑ +1.9e+02% | ↕* +11% | ↑ +1.5e+02% | ↓ -3.7% | ↓ -44% | ↑ +1.8e+02% | ↓ -12% | ↓ -16% | ↑ +9.2% | ↓ -1.1e+02% |
| rho_d | ANP6 | ↑ +2.7e+02% | ↕* +30% | ↕* +1.4e+02% | ↕* -1.7% | ↓ -18% | ↑ +2e+02% | ↓ -4.5% | ↓ -17% | ↑ +3.7% | ↓ -15% |
| Dv_dislocation_scale | AP3.2 | ↑ +16% | ↑ +11% | ↓ -1% | ↓ -0.022% | ↕ -0.38% | ↕ +1.3% | ↓ -0.15% | ↓ -0.071% | ↑ +0.055% | ↓ -0.068% |
| Dv_dislocation_scale | AP3.8 | ↑ +17% | ↑ +11% | ↓ -1% | ↓ -0.022% | ↕ -0.39% | ↕ +1.3% | ↓ -0.15% | ↓ -0.073% | ↑ +0.055% | ↓ -0.075% |
| Dv_dislocation_scale | AP3.4 | ↑ +17% | ↑ +11% | ↓ -1% | ↓ -0.019% | ↕ -0.31% | ↕ +1.3% | ↓ -0.13% | ↓ -0.069% | ↑ +0.056% | ↓ -0.07% |
| Dv_dislocation_scale | ANP6 | ↑ +15% | ↑ +9.5% | ↓ -1% | ↓ -0.01% | ↕ -0.1% | ↕ +1.1% | ↕* -0.059% | ↕* -0.014% | ↕* +0.03% | ↓ -0.044% |
| Dg_dislocation_scale | AP3.2 | ↑ +2.8e+02% | ↑ +1.1e+02% | ↓ -16% | ↓ -5.2% | ↓ -62% | ↑ +2.1e+02% | ↓ -18% | ↓ -22% | ↑ +12% | ↓ -2.1e+02% |
| Dg_dislocation_scale | AP3.8 | ↑ +2.8e+02% | ↑ +1.1e+02% | ↓ -15% | ↓ -5.1% | ↓ -61% | ↑ +2.1e+02% | ↓ -18% | ↓ -22% | ↑ +12% | ↓ -2.1e+02% |
| Dg_dislocation_scale | AP3.4 | ↑ +3e+02% | ↑ +1.2e+02% | ↓ -17% | ↓ -4.2% | ↓ -51% | ↑ +2.2e+02% | ↓ -14% | ↓ -19% | ↑ +11% | ↓ -1.3e+02% |
| Dg_dislocation_scale | ANP6 | ↑ +4.1e+02% | ↑ +1.7e+02% | ↓ -22% | ↕* -2% | ↓ -21% | ↑ +2.3e+02% | ↓ -5.3% | ↓ -20% | ↑ +4.3% | ↓ -17% |
| N_gf0_factor | AP3.2 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -1.8% | ↓ -2.2e+02% | ↑ +8e+02% | ↑ +38% |
| N_gf0_factor | AP3.8 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -1.7% | ↓ -2.2e+02% | ↑ +8e+02% | ↑ +41% |
| N_gf0_factor | AP3.4 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -2.4% | ↓ -2.2e+02% | ↑ +7.7e+02% | ↑ +29% |
| N_gf0_factor | ANP6 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -25% | ↓ -2.1e+02% | ↑ +5.9e+02% | ↑ +42% |

## Temperature 900-1600 K

| Parametro | Pin | P2 swelling | R_d | N_d | matrix gas | bulk gas | dislocation gas | grain-face gas | R_gf | N_gf volume equivalent | FGR |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| f_n | AP3.2 | ↓ -3.1e+02% | ↓ -77% | ↑ +10% | ↓ -4e+02% | ↑ +1.1e+02% | ↓ -4e+02% | ↓ -1.3e+02% | ↓ -86% | ↑ +38% | ↓ Δ-1.6 pp |
| f_n | AP3.8 | ↓ -3e+02% | ↓ -76% | ↑ +9.7% | ↓ -4e+02% | ↑ +1.1e+02% | ↓ -4e+02% | ↓ -1.3e+02% | ↓ -86% | ↑ +37% | ↓ Δ-1.5 pp |
| f_n | AP3.4 | ↓ -3.4e+02% | ↓ -82% | ↑ +11% | ↓ -4.5e+02% | ↑ +1.1e+02% | ↓ -4.6e+02% | ↓ -1.3e+02% | ↓ -96% | ↑ +42% | ↓ -6.6e+04% |
| f_n | ANP6 | ↓ -5.1e+02% | ↓ -1.1e+02% | ↑ +19% | ↓ -6.1e+02% | ↑ +75% | ↓ -6.3e+02% | ↓ -2.8e+02% | ↓ -2.3e+02% | ↑ +61% | ↓ -4.4e+02% |
| K_d | AP3.2 | ↕* -63% | ↓ -94% | ↑ +3e+02% | ↑ +0.27% | ↑ +3.6% | ↓ -15% | ↑ +1.4% | ↑ +1.1% | ↓ -0.53% | ≈ |
| K_d | AP3.8 | ↕* -62% | ↓ -93% | ↑ +3e+02% | ↑ +0.27% | ↑ +3.6% | ↓ -15% | ↑ +1.4% | ↑ +1.1% | ↓ -0.52% | ≈ |
| K_d | AP3.4 | ↕* -63% | ↓ -94% | ↑ +3e+02% | ↑ +0.23% | ↑ +3% | ↓ -15% | ↑ +1.1% | ↑ +0.93% | ↓ -0.48% | ↑ +1.5e+02% |
| K_d | ANP6 | ↕* -73% | ↓ -98% | ↑ +3.1e+02% | ↑ +0.11% | ↑ +1.2% | ↓ -15% | ↑ +0.4% | ↑ +0.48% | ↓ -0.23% | ↑ +0.98% |
| rho_d | AP3.2 | ↑ +1.7e+02% | ↕* +3.5% | ↑ +1.6e+02% | ↓ -4.2% | ↓ -45% | ↑ +1.9e+02% | ↓ -18% | ↓ -13% | ↑ +6.7% | ≈ |
| rho_d | AP3.8 | ↑ +1.7e+02% | ↕* +3.2% | ↑ +1.6e+02% | ↓ -4.2% | ↓ -44% | ↑ +1.9e+02% | ↓ -17% | ↓ -13% | ↑ +6.6% | ≈ |
| rho_d | AP3.4 | ↑ +1.7e+02% | ↕* +4.1% | ↑ +1.6e+02% | ↓ -3.5% | ↓ -37% | ↑ +1.9e+02% | ↓ -14% | ↓ -12% | ↑ +6.3% | ↓ -7.7e+02% |
| rho_d | ANP6 | ↑ +1.9e+02% | ↕* +7.9% | ↑ +1.6e+02% | ↓ -1.8% | ↓ -15% | ↑ +1.9e+02% | ↓ -5% | ↓ -6.1% | ↑ +3.1% | ↓ -12% |
| Dv_dislocation_scale | AP3.2 | ↑ +36% | ↑ +16% | ↓ -1.2% | ↓ -0.023% | ↓ -0.46% | ↑ +1.9% | ↓ -0.19% | ↓ -0.1% | ↑ +0.056% | ≈ |
| Dv_dislocation_scale | AP3.8 | ↑ +37% | ↑ +16% | ↓ -1.2% | ↓ -0.023% | ↓ -0.47% | ↑ +2% | ↓ -0.2% | ↓ -0.1% | ↑ +0.057% | ≈ |
| Dv_dislocation_scale | AP3.4 | ↑ +36% | ↑ +16% | ↓ -1.2% | ↓ -0.02% | ↓ -0.38% | ↑ +1.9% | ↓ -0.17% | ↓ -0.097% | ↑ +0.057% | ↓ -3.1% |
| Dv_dislocation_scale | ANP6 | ↑ +31% | ↑ +14% | ↓ -1.3% | ↓ -0.011% | ↓ -0.13% | ↑ +1.6% | ↕* -0.066% | ↕* -0.035% | ↕* +0.031% | ↓ -0.084% |
| Dg_dislocation_scale | AP3.2 | ↑ +2.2e+02% | ↑ +98% | ↓ -7.5% | ↓ -4.9% | ↓ -53% | ↑ +2.2e+02% | ↓ -20% | ↓ -16% | ↑ +7.8% | ≈ |
| Dg_dislocation_scale | AP3.8 | ↑ +2.2e+02% | ↑ +98% | ↓ -7.3% | ↓ -4.9% | ↓ -52% | ↑ +2.2e+02% | ↓ -20% | ↓ -16% | ↑ +7.7% | ≈ |
| Dg_dislocation_scale | AP3.4 | ↑ +2.3e+02% | ↑ +99% | ↓ -7.7% | ↓ -4.1% | ↓ -44% | ↑ +2.2e+02% | ↓ -17% | ↓ -14% | ↑ +7.3% | ↓ -1.1e+03% |
| Dg_dislocation_scale | ANP6 | ↑ +2.5e+02% | ↑ +1.1e+02% | ↓ -10% | ↓ -2.1% | ↓ -18% | ↑ +2.2e+02% | ↓ -5.9% | ↓ -7.2% | ↑ +3.7% | ↓ -14% |
| N_gf0_factor | AP3.2 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -1.9e+02% | ↑ +7.9e+02% | ≈ |
| N_gf0_factor | AP3.8 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -1.8e+02% | ↑ +7.9e+02% | ≈ |
| N_gf0_factor | AP3.4 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -0.8% | ↓ -1.8e+02% | ↑ +7.6e+02% | ↑ +1.2e+03% |
| N_gf0_factor | ANP6 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -26% | ↓ -1.7e+02% | ↑ +5.8e+02% | ↑ +86% |

## Temperature 1650-1800 K

| Parametro | Pin | P2 swelling | R_d | N_d | matrix gas | bulk gas | dislocation gas | grain-face gas | R_gf | N_gf volume equivalent | FGR |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| f_n | AP3.2 | ↓ -3.7e+02% | ↓ -1.3e+02% | ↑ +57% | ↓ -2.6e+02% | ↑ +1.3e+02% | ↓ -2.1e+02% | ↓ -23% | ↓ -63% | ↑ +1e+02% | ↓ -3.9e+02% |
| f_n | AP3.8 | ↓ -3.8e+02% | ↓ -1.3e+02% | ↑ +56% | ↓ -2.7e+02% | ↑ +1.3e+02% | ↓ -2.2e+02% | ↓ -25% | ↓ -65% | ↑ +1.1e+02% | ↓ -4.3e+02% |
| f_n | AP3.4 | ↓ -4.8e+02% | ↓ -1.7e+02% | ↑ +66% | ↓ -3.1e+02% | ↑ +1.3e+02% | ↓ -2.6e+02% | ↓ -14% | ↓ -86% | ↑ +1.3e+02% | ↓ -3.2e+02% |
| f_n | ANP6 | ↓ -9.2e+02% | ↓ -5.4e+02% | ↑ +1e+02% | ↓ -5.3e+02% | ↑ +1e+02% | ↓ -5.2e+02% | ↓ -1.9e+02% | ↓ -4.9e+02% | ↑ +3.7e+02% | ↓ -2.2e+02% |
| K_d | AP3.2 | ↓ -1.5e+02% | ↓ -1.4e+02% | ↑ +3.4e+02% | ↑* +0.32% | ↑ +8.5% | ↓ -15% | ↑ +0.58% | ↑ +2.3% | ↓ -3.7% | ↑ +16% |
| K_d | AP3.8 | ↓ -1.5e+02% | ↓ -1.4e+02% | ↑ +3.3e+02% | ↑* +0.31% | ↑ +8% | ↓ -15% | ↑ +0.55% | ↑ +2.1% | ↓ -3.5% | ↑ +17% |
| K_d | AP3.4 | ↓ -1.5e+02% | ↓ -1.4e+02% | ↑ +3.3e+02% | ↑* +0.21% | ↑ +6.3% | ↓ -15% | ↑ +0.054% | ↑ +1.8% | ↓ -3.2% | ↑ +8.6% |
| K_d | ANP6 | ↓ -1.5e+02% | ↓ -1.4e+02% | ↑ +3.4e+02% | ↕* +0.039% | ↑ +2.2% | ↓ -15% | ↑ +0.039% | ↑ +2% | ↓ -2.8% | ↑ +1.2% |
| rho_d | AP3.2 | ↑ +1.8e+02% | ↑ +22% | ↑ +1.3e+02% | ↓ -9% | ↓ -93% | ↑ +1.7e+02% | ↓ -9.2% | ↓ -28% | ↑ +49% | ↓ -1.8e+02% |
| rho_d | AP3.8 | ↑ +1.9e+02% | ↑ +22% | ↑ +1.3e+02% | ↓ -8.4% | ↓ -90% | ↑ +1.7e+02% | ↓ -9.9% | ↓ -27% | ↑ +47% | ↓ -1.8e+02% |
| rho_d | AP3.4 | ↑ +2.1e+02% | ↑ +27% | ↑ +1.2e+02% | ↓ -5.9% | ↓ -73% | ↑ +1.8e+02% | ↓ -1.1% | ↓ -23% | ↑ +43% | ↓ -1.1e+02% |
| rho_d | ANP6 | ↑ +3.5e+02% | ↑ +80% | ↕* +90% | ↕* -1% | ↓ -30% | ↑ +2.1e+02% | ↓ -0.53% | ↓ -27% | ↑ +40% | ↓ -17% |
| Dv_dislocation_scale | AP3.2 | ↑ +0.72% | ↑ +0.28% | ↓ -0.12% | ↓ -0.0064% | ↕ +0.0015% | ↕ +0.0012% | ↓ -0.0059% | ↓ -0.022% | ↑ +0.038% | ↓ -0.068% |
| Dv_dislocation_scale | AP3.8 | ↑ +0.7% | ↑ +0.28% | ↓ -0.11% | ↓ -0.0062% | ↕ +0.0014% | ↕ +0.0014% | ↓ -0.0061% | ↓ -0.022% | ↑ +0.039% | ↓ -0.075% |
| Dv_dislocation_scale | AP3.4 | ↑ +0.47% | ↑ +0.19% | ↓ -0.078% | ↓ -0.0054% | ↕ +0.0025% | ↕ -0.0013% | ↓ -0.00078% | ↓ -0.023% | ↑ +0.047% | ↓ -0.051% |
| Dv_dislocation_scale | ANP6 | ↑ +0.23% | ↑ +0.1% | ↓ -0.041% | ↓ -0.00079% | ↕ +0.00034% | ↕ +0.0036% | ↕ +4.8e-05% | ↕* +0.0044% | ↕* -0.0083% | ↓ -0.01% |
| Dg_dislocation_scale | AP3.2 | ↑ +3.2e+02% | ↑ +1.5e+02% | ↓ -52% | ↓ -9.9% | ↓ -1.1e+02% | ↑ +1.9e+02% | ↓ -11% | ↓ -33% | ↑ +57% | ↓ -2.1e+02% |
| Dg_dislocation_scale | AP3.8 | ↑ +3.2e+02% | ↑ +1.5e+02% | ↓ -49% | ↓ -9.3% | ↓ -1e+02% | ↑ +2e+02% | ↓ -11% | ↓ -32% | ↑ +54% | ↓ -2.1e+02% |
| Dg_dislocation_scale | AP3.4 | ↑ +3.7e+02% | ↑ +1.6e+02% | ↓ -54% | ↓ -6.5% | ↓ -86% | ↑ +2.1e+02% | ↓ -1.5% | ↓ -27% | ↑ +49% | ↓ -1.2e+02% |
| Dg_dislocation_scale | ANP6 | ↑ +5.7e+02% | ↑ +3e+02% | ↓ -73% | ↕* -0.72% | ↓ -35% | ↑ +2.5e+02% | ↓ -0.61% | ↓ -30% | ↑ +44% | ↓ -19% |
| N_gf0_factor | AP3.2 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -7.7% | ↓ -2.8e+02% | ↑ +9e+02% | ↑ +38% |
| N_gf0_factor | AP3.8 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -7.4% | ↓ -2.8e+02% | ↑ +9e+02% | ↑ +41% |
| N_gf0_factor | AP3.4 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -8.7% | ↓ -2.8e+02% | ↑ +9e+02% | ↑ +21% |
| N_gf0_factor | ANP6 | ≈ | ≈ | ≈ | ≈ | ≈ | ≈ | ↓ -15% | ↓ -2.4e+02% | ↑ +8.5e+02% | ↑ +5.3% |

## Valori OAT

| Parametro | Low | Baseline | High |
| --- | --- | --- | --- |
| f_n | 1e-07 | 0.00055 | 0.01 |
| K_d | 1e+05 | 3e+05 | 1e+06 |
| rho_d | 1e+13 | 3e+13 | 6e+13 |
| Dv_dislocation_scale | 1 | 10 | 30 |
| Dg_dislocation_scale | 1 | 13 | 30 |
| N_gf0_factor | 0.1 | 1 | 10 |

## Confronti e limiti

Gli esperimenti sono interpolati linearmente sulla griglia 50 K, senza nuove simulazioni alle temperature digitalizzate. Il punto AP3.8 a 899 K è escluso (fuori griglia); nessuna extrapolazione.
I benchmark gas partition Rizk 2025 sono confrontati numericamente solo a 1.1 e 3.2% FIMA, per T ≤ 1800 K. La curva 1.1% è condivisa fra AP3.2/AP3.8 e non ha una provenienza pin-specific verificata.
A 1.3% FIMA non ci sono curve gas partition incorporate. Il CSV Fig.7/8 per R_gf e N_gf citato in _16NgfOnly_plots_with_Rizk_intergranular.ipynb non è disponibile nel workspace: nessun valore ricostruito o assunto.
Una violazione è persistente se compare in almeno 3 temperature consecutive (span ≥100 K). Gli ordinamenti confrontano i tre raggi e le tre densità volumetriche allo stesso burnup finale; non gli stati iniziali o le storie temporali. Gli zeri di popolazione restano inclusi nel controllo degli ordinamenti; i valori non finiti sono segnalati separatamente.