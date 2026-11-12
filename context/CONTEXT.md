# Selected analytic model and local Barani 2020 reference

Checked against local sources on 12 November 2026.
Reference: [Barani et al., JNM 538 (2020) 152195](https://doi.org/10.1016/j.jnucmat.2020.152195).

## Configuration

The executable defaults to the selected analytic_rho development: direct fitted
rho(T,B), gas-limited dose-driven births and dynamic Hart/Murphy weighting.
Four source/header overrides are copied unchanged from the local campaign.
SCIANTIX_NUCLEATION=control recovers the frozen constant-density Hart reference.
The table below describes that reference; the selected extensions are documented
in [Subsequent developments](coarsening_barani_extensions.md).

| Item | Frozen reference setting |
|---|---|
| Intragranular model / resolution / diffusion | 5 / 98 / SDA2 (2) |
| Dislocation density / K | rho_d(B,T) Fig. 5 fit (4e13 m^-2 reference) / 1e5 m^-1 (calibrated on White 2006) |
| Initialization | One-off minimum seed K*rho=4e19 m^-3 at first irradiation |
| Re-solution correction | phi=1/(n-1) in dislocation-bubble number loss only |
| Transport radius / capture radius | a=1.5b / 5b, b=0.385 nm |
| White time grid | 100 steps per history interval |

For the selected model, rho in the transport equations below is rho(T,B).
The one-off seed remains independent of that density. Vacancy uptake, EOS,
coalescence and native bulk transport retain the reference implementation.

```text
Dcore = 2e-7 exp[-1.62 eV/(kB*T)] m^2/s
f = pi*a^2*rho
Dmean = (1-f)*Dbulk_native + f*Dcore
beta_prime = 2*pi*rho*Dmean / [ln((pi*rho)^(-1/2)/(5b))-0.6]
```

The mean applies only to line capture. Native bulk transport and vacancy
diffusivity are unchanged. Weighting assumes equal mobile-Xe density in bulk
and core; a=1.5b and the application to beta-prime are local closures.
Sources: [Murphy 2014](https://doi.org/10.1016/j.jnucmat.2013.12.029), Eq. 5;
[Murphy 2015](https://doi.org/10.1016/j.jnucmat.2015.09.007), Table 1.

## Reproduction and limitations

Experimental data, published Table 4 predictions and local calculations are
distinct. White comparisons cover 38 of 40 published locations; histories for
4136-E and 4140-C are unavailable. Reproducing a local reference does not
establish exact reproduction of the published model.

Retained approximations: algebraic gas partitioning followed by SDA2,
Carnahan-Starling with operational volume V=n*omega(T)+nv*Omega,
absorption-only vacancy uptake above 1873 K, up to mechanical equilibrium, 250 nm guard.
Three independent gas balances are not solved; coalescence does not conserve
vacancies as pure fusion would. Structural and post-SDA inventories must not
be treated as simultaneous states. Full timestep convergence is unproven.

The bbbf85dc Hart reference had RMSE log10 R/Nd/IG/GB of
0.234452 / 0.557682 / 0.493686 / 0.323223 and the analytic model
0.225432 / 0.636547 / 0.594226 / 0.375000. After the White (2006) corrections
(12 November 2026) the analytic model has 0.298805 / 0.665966 / 0.504330 / 0.305647. The new White runner checks all
114 outputs against hashes from the original local campaigns; its result is
recorded in regression/White_COARSENING/reproduction_validation.json.

Sources: [kernels](../include/models/IntraGranularBubbleBehavior.h),
[model](../src/models/IntraGranularBubbleBehavior.C) (IntraGranularBubbleCoarsening),
[dislocation density](../src/models/DislocationDensity.C),
[White regression](../regression/White_COARSENING/README.md),
[reproduction check](../regression/White_COARSENING/verify_reference.py).
