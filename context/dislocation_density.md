# Dislocation-density fit and scripts

dd_fit.py fits digitized model curves from Veshchunov and Shestak (2009),
Fig. 4, [JNM 384, 12-18](https://doi.org/10.1016/j.jnucmat.2008.09.024).
This represents published model curves, not independent experimental validation
or a recovery kinetic law.

```text
rho(B,T) = A*B^n*[A_inf+(1-A_inf)/(1+exp((T-Tc)/dT))]
B: GWd/tHM (= MWd/kgHM numerically); T: K; rho: m^-2
A = 6544878893406.698; n = 1.1513567102867877
A_inf = 0.6076180337519009
Tc = 1109.0189319357949 K; dT = 25.80556262160369 K
RMSE log10 = 0.024693489987843015
```

[fit_parameters.json](fit_parameters.json) includes covariance. Digitized data
span approximately 3-93 GWd/tHM and 900-1400 K; analytic tails are unvalidated.
Convert SCIANTIX burnup per oxide mass to heavy-metal mass using
(238.02891+2*15.9994)/238.02891 for UO2.

- [dd_fit.py](dd_fit.py): density data, fitting and plots. Importing also executes
  fitting/plotting.
- [kjma_fit_comparison.py](kjma_fit_comparison.py): HBS KJMA comparisons with rounded
  density coefficients. The rho-driven KJMA fit is disabled. Its max(B,1e-12)
  differs from the coarsening model's exact rho=0 at B=0.

Both scripts are unchanged and require NumPy, SciPy and Matplotlib. Run from
context/generated/ to keep plots separate; that directory is ignored by Git.
Active savefig calls use the current working directory.

The current C++ default evaluates this fit directly and enables dose-driven,
gas-limited births. SCIANTIX_NUCLEATION=control restores constant rho=4e13 m^-2.
