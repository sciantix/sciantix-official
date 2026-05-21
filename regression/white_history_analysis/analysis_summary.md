# White History Analysis

COARSENING exploratory analysis for calibrating `K_eff` and future bubble-family splitting.

Cases analyzed: 38

## Strongest Absolute Correlations

- `Inferred K_eff = N_exp/rho_d (bub/m)` vs `Inferred K_eff = N_exp/rho_d (bub/m)`: r = +1.000
- `Inferred spacing 1/K_eff (nm)` vs `Calculated / experimental concentration`: r = +0.974
- `Final burnup (MWd/kgUO2)` vs `Calculated coarsened bubble concentration (bub/m3)`: r = +0.920
- `Dislocation density (m/m3)` vs `Calculated coarsened bubble concentration (bub/m3)`: r = +0.919
- `Integral fission-rate proxy (fiss/m3)` vs `Calculated coarsened bubble concentration (bub/m3)`: r = +0.916
- `Peak-temperature holding time (history units)` vs `Calculated / experimental concentration`: r = +0.864
- `Maximum heating rate (K/history unit)` vs `Calculated coarsened swelling (%)`: r = +0.780
- `Transient integral fission-rate proxy (fiss/m3)` vs `Experimental swelling (%)`: r = -0.755
- `Final grain radius (um)` vs `Experimental bubble radius (m)`: r = +0.750
- `Heating-ramp duration (history units)` vs `Experimental swelling (%)`: r = -0.740
- `Inferred spacing 1/K_eff (nm)` vs `Calculated coarsened bubble concentration (bub/m3)`: r = +0.739
- `Maximum cooling rate (K/history unit)` vs `Experimental swelling (%)`: r = -0.729
- `Maximum cooling rate (K/history unit)` vs `Calculated / experimental radius`: r = +0.726
- `Transient integral fission-rate proxy (fiss/m3)` vs `Calculated / experimental swelling`: r = +0.702
- `Maximum heating rate (K/history unit)` vs `Calculated / experimental swelling`: r = +0.690
- `Final burnup (MWd/kgUO2)` vs `Calculated coarsened swelling (%)`: r = +0.674
- `Integral fission-rate proxy (fiss/m3)` vs `Calculated coarsened swelling (%)`: r = +0.671
- `Dislocation density (m/m3)` vs `Experimental swelling (%)`: r = +0.665
- `Peak fission rate (fiss/m3/s)` vs `Calculated / experimental swelling`: r = -0.660
- `Time above 1500 K (history units)` vs `Experimental swelling (%)`: r = -0.660
- `Inferred K_eff = N_exp/rho_d (bub/m)` vs `Experimental bubble concentration (bub/m3)`: r = +0.639
- `Thermal dose above 1800 K (K history units)` vs `Experimental bubble concentration (bub/m3)`: r = -0.619
- `Time-weighted fission rate (fiss/m3/s)` vs `Calculated coarsened bubble concentration (bub/m3)`: r = -0.617
- `Inferred K_eff = N_exp/rho_d (bub/m)` vs `Calculated / experimental radius`: r = +0.616
- `Base temperature (K)` vs `Calculated coarsened bubble concentration (bub/m3)`: r = -0.615
- `Maximum cooling rate (K/history unit)` vs `Calculated coarsened bubble radius (m)`: r = +0.614
- `Transient integral fission-rate proxy (fiss/m3)` vs `Calculated / experimental radius`: r = +0.607
- `Final grain radius (um)` vs `Calculated / experimental radius`: r = -0.606
- `Thermal dose above 1500 K (K history units)` vs `Experimental bubble concentration (bub/m3)`: r = -0.600
- `Final burnup (MWd/kgUO2)` vs `Experimental swelling (%)`: r = +0.599

## Practical Readout

- If `N_exp/rho_d` correlates with a history feature, that feature is a good candidate for `K_eff`.
- If radius correlates with thermal dose or peak-hold time while concentration does not, a family-splitting or growth/vacancy parameter is more appropriate than only changing `K_eff`.
- If model/experiment ratios correlate with a feature, that feature is a useful calibration axis for reducing the horizontal parity-plot alignment.

Figures are grouped in `figures/history_overlays`, `figures/feature_scatter`, `figures/candidate_keff`, and `figures/correlations`.
