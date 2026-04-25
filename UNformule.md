# Modelli di Re-solution, Trapping e Nucleazione

## Rizk (2025) – Barani (2019, 2020)

---

# 1. Rizk (2025) – Uranium Nitride (UN)

## Meccanismo

Omogeneo (collisioni balistiche)

---

## Re-solution rate

$$
b = b_0(R)\dot{F} \quad [s^{-1}]
$$

$$
b_0(R)=10^{-25}\left(2.64 - 2.02 \exp\left[-\frac{2.61\times10^{-9}}{R}\right]\right)
$$

* $R$ in metri
* $\dot{F}\ \text{in fissioni}\ \mathrm{m}^{-3}\ \mathrm{s}^{-1}$

---

## Trapping rate

### Bulk

$$
g_b = 4\pi D_g R_b N_b  
\quad [\mathrm{s}^{-1}]
$$

### Dislocazioni

$$
g_d = 4\pi D_g R_d N_d
+\frac{2\pi D_g}{\ln\left(\frac{\Gamma_d}{ Z_d r_d }\right)-\frac{5}{3}}
  (\rho_d - 2R_d N_d)
  $$

$$
\Gamma_d = \frac{1}{\sqrt{\pi \rho_d}}
$$

---

## Nucleazione

$$
\nu_b = 8\pi f_n D_g \Omega_{fg}^{1/3} c^2
\quad [m^{-3}s^{-1}]
$$

---

## Costanti (UN)

* $f_n = 10^{-6}$
* $\Omega_{fg} = 8.5\times10^{-29}, m^3$
* $\rho_d = 3.0\times10^{13}, m^{-2}$
* $r_d = 3.46\times10^{-10}, m$
* $Z_d = 5.0$
* $k_B = 8.617\times10^{-5}, eV/K$

$$
D_g = \left(1.56\times10^{-3} e^{-4.94/(k_BT)} + 1.85\times10^{-39}\dot{F}\right) SF_D
$$

---

# 2. Barani (2019) – Uranium Silicide (U₃Si₂)

## Meccanismo

Omogeneo (metallo)

---

## Re-solution rate

$$
\alpha = \alpha_0(R)\dot{F}
\quad [\mathrm{s}^{-1}]
$$

$$
\alpha_0(R)=2.80\times10^{-25}\left(\frac{5\times10^{-10}}{R}\right)^{0.23}
$$

---

## Trapping rate

$$
b_n = 4\pi D R_n c_1
\quad [\mathrm{s}^{-1}]
$$

---

## Nucleazione

$$
n = 8\pi D R_{sg} f_n c_1^2
\quad [\mathrm{at}\cdot \mathrm{m}^{-3}\ \mathrm{s}^{-1}]
$$

---

## Costanti (U₃Si₂)

* $f_n = 10^{-4}$
* $R_{sg} = 2.41\times10^{-10}, m$
* $D = 5.91\times10^{-6} \exp\left[-\frac{4.41\times10^{-19}}{k_BT}\right]$
* Nota: qui l'energia è in Joule, quindi

$$
k_B = 1.38 \cdot 10^{-23}\ \mathrm{J/K}
$$
---

# 3. Barani (2020) – Uranium Dioxide (UO₂)

## Meccanismo

Eterogeneo / thermal spikes

---

## Re-solution rate

$$
\alpha =
\left[
a e^{-b_1 R}

+ \frac{b_0 - a}{1 + cR^2} e^{-dR^2}
  \right]\dot{F}
\quad [\mathrm{s}^{-1}]
$$

---

## Trapping rate

### Bulk

$$
b_b = 4\pi D R_b N_b
$$

### Dislocazioni

$$
b_d =
\frac{2\pi D_d}{\ln\left(\frac{r_{ws,d}}{r_d}\right) - \frac{3}{5}}
\;\rho_d
$$

## Nucleazione

### Bulk

$$
h \cdot 2\dot{F}
$$

### Dislocazioni

$$
N_{d,initial} = K\rho_d
$$

---

## Costanti (UO₂)

### Re-solution fit

* $a = 9.49\times10^{-24}, m^3$
* $b_0 = 9.18\times10^{-23}, m^3$
* $b_1 = 7.07\times10^7, m^{-1}$
* $c = 7.982\times10^{18}, m^{-2}$
* $d = 3.71\times10^{16}, m^{-2}$

### Microstruttura

* $h = 25 \quad \mathrm{bolle/fission\ fragment}$ 
* $K = 10^6, m^{-1}$
* $\rho_d = 4.0\times10^{13}, m^{-2}$
* $r_d = 1.925\times10^{-9}, m$

### Diffusività

$$
D_1 = 7.6\times10^{-10} e^{-4.86\times10^{-19}/(k_BT)}
$$

$$
D_2 = 5.64 \cdot 10^{-25}\ \sqrt{\dot{F}}\ e^{-\frac{1.91 \cdot 10^{-19}}{k_B T}}
$$

$$
D_3 = 2.0\times10^{-40}\dot{F}
$$
