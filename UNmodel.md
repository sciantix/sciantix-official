# Modello UN (Uranium Nitride) - Equazioni e Parametri

Questo documento descrive le equazioni e i parametri del modello UN basato su Rizk (2025) per il comportamento del gas di fissione in Uranium Nitride.

## Diffusività del Gas di Fissione (Xe)

La diffusività $D_g$ è data dalla somma di tre contributi:

$$
D_g = D_1 + D_2 + D_3
$$

dove:
- $D_1 = D_{10} \exp\left(-\frac{Q_1}{k_B T}\right)$ (diffusione termica)
- $D_2 = A_{20} \sqrt{\dot{F}} \exp\left( -\frac{B_{21}}{k_B T} - \frac{B_{22}}{(k_B T)^2} - \frac{B_{23}}{(k_B T)^3} \right)$ (irradiation-enhanced diffusion)
- $D_3 = A_{30} \dot{F}$ (radiation-induced mixing)

Valori dei parametri:
- $D_{10} = 1.56 \times 10^{-3}$ m²/s
- $Q_1 = 4.94$ eV
- $A_{20} = 1.21 \times 10^{-67}$ m⁷/²/s¹/²
- $B_{21} = 25.87$ eV
- $B_{22} = -1.49$ eV²
- $B_{23} = 0.0$ eV³
- $A_{30} = 1.85 \times 10^{-39}$ m⁵
- $k_B = 8.617 \times 10^{-5}$ eV/K
- $\dot{F}$ è il fission rate density (fissioni/m³·s)

Nota: Nel modello attuale, $D_2$ è trascurato per Xe secondo Rizk, ma è incluso qui per completezza.

## Re-solution Rate

Il tasso di re-solution $b$ per le bolle è:

$$
b = b_0(R) \dot{F}
$$

dove:

$$
b_0(R) = 10^{-25} \left(2.64 - 2.02 \exp\left(-\frac{2.61 \times 10^{-9}}{R}\right)\right)
$$

- $R$ è il raggio della bolla (m)
- $\dot{F}$ è il fission rate density (fissioni/m³·s)

Questo vale sia per le bolle bulk che per quelle su dislocazioni.

## Trapping Rate

### Trapping verso bolle bulk ($g_b$)

$$
g_b = 4\pi D_g R_b N_b
$$

- $D_g$ diffusività (m²/s)
- $R_b$ raggio bolle bulk (m)
- $N_b$ concentrazione bolle bulk (bub/m³)

### Trapping verso dislocazioni ($g_d$)

$$
g_d = 4\pi D_g R_d N_d + \frac{2\pi D_g}{\ln\left(\frac{\Gamma_d}{Z_d r_d}\right) - \frac{3}{5}} (\rho_d - 2 R_d N_d)
$$

dove:
- $R_d$ raggio bolle su dislocazioni (m)
- $N_d$ concentrazione bolle su dislocazioni (bub/m³)
- $\Gamma_d = \frac{1}{\sqrt{\pi \rho_d}}$ (Wigner-Seitz radius per dislocazioni, m)
- $\rho_d = 3.0 \times 10^{13}$ m⁻² (densità dislocazioni)
- $r_d = 3.46 \times 10^{-10}$ m (raggio core dislocazioni)
- $Z_d = 5.0$

## Nucleazione

Il tasso di nucleazione per bolle bulk $\nu_b$:

$$
\nu_b = 8\pi f_n D_g \Omega_{fg}^{1/3} c^2
$$

- $f_n = 10^{-6}$
- $\Omega_{fg} = 8.5 \times 10^{-29}$ m³
- $c$ concentrazione gas in soluzione (at/m³)

## Sistema di Equazioni di Diffusione (3 equazioni)

Le variabili sono:
- $c$: gas in soluzione (at/m³)
- $m_b$: gas in bolle bulk (at/m³)
- $m_d$: gas in bolle su dislocazioni (at/m³)

Sistema:

$$
\frac{\partial c}{\partial t} = D_g \nabla^2 c - (g_b + g_d) c + b_b m_b + b_d m_d + \beta
$$

$$
\frac{\partial m_b}{\partial t} = g_b c - b_b m_b
$$

$$
\frac{\partial m_d}{\partial t} = g_d c - b_d m_d
$$

dove:
- $\beta$ produzione volumetrica di gas (at/m³·s)
- $b_b, b_d$ tassi di re-solution per bulk e dislocazioni

## Valori Iniziali

- Concentrazione iniziale bolle su dislocazioni: $N_d(0) = 3.6 \times 10^{19}$ bub/m³

## Riferimenti

- Rizk et al., JNM 606 (2025) 155604