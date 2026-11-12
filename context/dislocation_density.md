# Dislocation-density fit and scripts

dd_fit.py fits model curves from Veshchunov and Shestak (2009),
[JNM 384, 12-18](https://doi.org/10.1016/j.jnucmat.2008.09.024): MFPR calculations,
not measurements. Fig. 4 (base parameter set, 900-1400 K) is "rather insensitive to
temperature" up to 1300 K and drops to a flat ~8e12 m^-2 at 1400 K (a drop the authors
call "somewhat overestimated"). Fig. 5 (modified parameter set, 900-1500 K) decreases
smoothly. No calculations exist above 1500 K.

The curves were extracted from the vector paths of the PDF (local, untracked
validation/VS2009_dislocation_fit/) and sampled every 5 GWd/tHM from 5 to 100:
[VS2009_fig4_grid.csv](VS2009_fig4_grid.csv), [VS2009_fig5_grid.csv](VS2009_fig5_grid.csv).

```text
rho(B,T) = A*B^n*[A_inf+(1-A_inf)/(1+exp((T-Tc)/dT))]
B: GWd/tHM (= MWd/kgHM numerically); T: K; rho: m^-2
Fig. 5, all T (used by the C++ code, T held at 1500 K above it):
A = 3.437e13; n = 0.8572; A_inf = 0.03347; Tc = 1019.0 K; dT = 255.6 K
within -30%/+36% of the curves at 900-1500 K
Fig. 4, T <= 1300 K (reference): A = 6.596e12; n = 1.217; A_inf = 0.442; Tc = 918 K; dT = 111 K
within 8%; the A*B^n*f(T) form cannot represent the flat 1400 K curve
```

The previous fit (A = 6.545e12, n = 1.151, A_inf = 0.608, Tc = 1109 K, dT = 25.8 K,
[fit_parameters.json](fit_parameters.json)) used 19 hand-digitized points whose labels did
not match Fig. 4: the "1100 K" points were the 1000 K curve and the "1400 K" points nearly
duplicated 1200 K. It stays within 35% of Fig. 4 up to 1300 K and is 3-100 times above it at
1400 K; its step at 1109 K is an artefact of the labels.

Convert SCIANTIX burnup per oxide mass to heavy-metal mass using
(238.02891+2*15.9994)/238.02891 for UO2. Measured densities on the White rods
(White et al. 2006, Sec. 5.5, Fig. 36: about 1-4e13 m^-2 in the controls) are 2-5 times
below the Fig. 5 fit.

- [dd_fit.py](dd_fit.py): reads the grid files, fits Fig. 5 and Fig. 4 (<= 1300 K), plots
  both with the previous fit (dislocation_density_fit.png next to the script).
- [kjma_fit_comparison.py](kjma_fit_comparison.py): HBS KJMA comparisons with rounded
  density coefficients of the previous fit. The rho-driven KJMA fit is disabled. Its
  max(B,1e-12) differs from the coarsening model's exact rho=0 at B=0.

Both scripts require NumPy, SciPy and Matplotlib.

The current C++ default evaluates the Fig. 5 fit directly and enables dose-driven,
gas-limited births. SCIANTIX_NUCLEATION=control restores constant rho=4e13 m^-2.
