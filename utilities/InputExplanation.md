# Input explanation

This folder contains the essential input files required by SCIANTIX:

- **`input_settings.txt`**: Specifies the models and numerical solvers used in the simulation.
- **`input_history.txt`**: Includes the time (h), temperature (K), fission rate (fiss/m³-s), and hydrostatic stress (MPa) as a function of time.
- **`input_initial_conditions.txt`**: Sets initial conditions for the simulation.
- **`input_scaling_factors.txt`**: (Optional) File for scaling factors.

Below is a detailed description of each file.

`input_settings.txt`, `input_initial_conditions.txt` and `input_scaling_factors.txt` share one
format: every entry is a single line

```plaintext
<value(s)>    #    <Key> (<description>)
```

and SCIANTIX looks each entry up by its key, the first word after the `#`.

Blank lines and lines holding only a comment are ignored. 
A line with a value but no key, a key given twice, or a key the code does not know stops the run with an error.

---

## Input settings

The following settings define the models and methods used for the simulation. A setting left out defaults to 0.

```plaintext
1    #    iGrainGrowth (0= no grain growth, 1= Ainscough et al. (1973), 2= Van Uffelen et al. (2013))
1    #    iFissionProductDiffusivity (0= constant value, 1= Turnbull et al. (1988))
1    #    iDiffusionSolver (1= SDA with quasi-stationary hypothesis, 2= SDA without quasi-stationary hypothesis)
1    #    iIntraGranularBubbleBehavior (1= Pizzocri et al. (2018))
1    #    iResolutionRate (0= constant value, 1= Turnbull (1971), 2= Losonen (2000), 3= thermal resolution, Cognini et al. (2021))
1    #    iTrappingRate (0= constant value, 1= Ham (1958))
1    #    iNucleationRate (0= constant value, 1= Olander, Wongsawaeng (2006))
1    #    iOutput (1= default output files)
1    #    iGrainBoundaryVacancyDiffusivity (0= constant value, 1= Reynolds and Burton (1979), 2= White (2004))
1    #    iGrainBoundaryBehaviour (0= no grain boundary bubbles, 1= Pastore et al (2013))
1    #    iGrainBoundaryMicroCracking (0= no model considered, 1= Barani et al. (2017))
0    #    iFuelMatrix (0= UO2, 1= UO2 + HBS)
0    #    iGrainBoundaryVenting (0= no model considered, 1= Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE)
0    #    iRadioactiveFissionGas (0= not considered)
0    #    iHelium (0= not considered)
0    #    iHeDiffusivity (0= null value, 1= limited lattice damage, Luzzi et al. (2018), 2= significant lattice damage, Luzzi et al. (2018))
0    #    iGrainBoundarySweeping (0= no model considered, 1= TRANSURANUS swept volume model)
0    #    iHighBurnupStructureFormation (0= no model considered, 1= fraction of HBS-restructured volume from Barani et al. (2020))
0    #    iHighBurnupStructurePorosity (0= no evolution of HBS porosity, 1= HBS porosity evolution based on Spino et al. (2006) data)
0    #    iHeliumProductionRate (0= zero production rate, 1= helium from ternary fissions, 2= linear with burnup (FR))
0    #    iStoichiometryDeviation (0= not considered, 1= Cox et al. 1986, 2= Bittel et al. 1969, 3= Abrefah et al. 1994, 4= Imamura et al. 1997, 5= Langmuir-based approach)
0    #    iBubbleDiffusivity (0= not considered, 1= volume diffusivity)
```

---

## Input history

Input history defines the conditions imposed to the simulation in terms of duration of the simulated history (in hours, first column), local temperature (in K, second column), local fission rate density (in fission per cubic meter per second, third column), and local hydrostatic stress (in MPa, fourth column).

```plaintext
0	1273	1e19	0
5500	1273	1e19	0
```

Between the given rows, the code linearly interpolates with a fixed number of time steps. A
case can choose that number by adding the optional entry

```plaintext
7500    #    Number_of_time_steps_per_interval
```

to `input_settings.txt`. Absent, the default in `src/MainVariables.C` applies.

---

## Input initial conditions

This file sets the initial conditions for various parameters within the simulation. An entry left out takes the value 0.

```plaintext
5.0e-06                         # Grain_radius[0] (initial grain radius (m))
0.0 0.0 0.0 0.0 0.0 0.0         # Initial_composition_Xe (initial Xe (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released)
0.0 0.0 0.0 0.0 0.0 0.0         # Initial_composition_Kr (initial Kr (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released)
0.0 0.0 0.0 0.0 0.0 0.0         # Initial_composition_He (initial He (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released)
0.0 0.0                         # Initial_intragranular_bubbles (initial intragranular bubble concentration (bub/m3), radius (m))
0.0                             # Burn_up[0] (initial fuel burn-up (MWd/kgUO2))
0.0                             # Effective_burn_up[0] (initial fuel effective burn-up (MWd/kgUO2))
0.0                             # Irradiation_time[0] (initial irradiation time (h))
10641.0                         # Fuel_density[0] (initial fuel density (kg/m3))
0.0 3.0 0.0 0.0 97.0            # Initial_composition_U (initial U234 U235 U236 U237 U238 content (% of heavy atoms))
0.0 0.0 0.0 0.0 0.0 0.0 0.0     # Initial_composition_Xe133 (initial Xe133 (at/m3): produced, intragranular, in solution, in bubbles, decayed, grain boundary, released)
0.0 0.0 0.0 0.0 0.0 0.0 0.0     # Initial_composition_Kr85m (initial Kr85m (at/m3): produced, intragranular, in solution, in bubbles, decayed, grain boundary, released)
0.0                             # Initial_stoichiometry_deviation[0] (initial fuel stoichiometry deviation (/))
```

| Key | Values |
|---|---|
| `Grain_radius[0]` | 1 |
| `Initial_composition_Xe`, `Initial_composition_Kr`, `Initial_composition_He` | 6 each |
| `Initial_intragranular_bubbles` | 2 |
| `Burn_up[0]`, `Effective_burn_up[0]`, `Irradiation_time[0]`, `Fuel_density[0]` | 1 each |
| `Initial_composition_U` | 5 |
| `Initial_composition_Xe133`, `Initial_composition_Kr85m` | 7 each |
| `Initial_stoichiometry_deviation[0]`, `Chromium_content` | 1 each |
| `Initial_composition_Pu` (MOX only) | 5 |
| `q` (MOX only) | 1 |

An entry must hold exactly its number of values, otherwise the run stops with an error.

### Optional model parameters

Two model parameters may also be set per case:

```plaintext
2.5e13                          # Intergranular_bubble_concentration[0]
0.6                             # Surface_tension
```

| Key | Meaning | Value used when the entry is absent |
|---|---|---|
| `Intergranular_bubble_concentration[0]` | initial intergranular bubble concentration (bub/m²) | 2.0e13 |
| `Surface_tension` | surface tension of the fuel matrix (N/m) | 0.7 for UO2 and UO2+HBS, 0.626 for MOX |


---

## Input scaling factors

This file contains the scaling factors applied to various model parameters during the
simulation. It is optional; a factor left out defaults to 1.0.

```plaintext
1.0    # sf_resolution_rate (scaling factor - resolution rate)
1.0    # sf_trapping_rate (scaling factor - trapping rate)
1.0    # sf_nucleation_rate (scaling factor - nucleation rate)
1.0    # sf_diffusivity (scaling factor - diffusivity)
1.0    # sf_temperature (scaling factor - temperature)
1.0    # sf_fission_rate (scaling factor - fission rate)
1.0    # sf_diffusion_based_release (scaling factor - diffusion-based release)
1.0    # sf_helium_production_rate (scaling factor - helium production rate)
1.0    # sf_grain_boundary_energy (scaling factor - grain-boundary energy)
1.0    # sf_fabricated_porosity (scaling factor - fabricated porosity)
1.0    # sf_cs_production (scaling factor - Cs production)
```

---

## Converting older inputs

Earlier versions wrote `input_initial_conditions.txt` and `input_scaling_factors.txt` with each
value on its own line and the `#` description below it, and labelled some settings with names
that are no longer accepted (e.g. `iFissionGasDiffusivity`). SCIANTIX stops with an error on
such files. They can be rewritten in the keyed format, with unchanged results:

```bash
python3 utilities/inputExample/convert_to_named_inputs.py <case_dir> [<case_dir> ...]
```

---

If you experience any issues with these files, please contact the main developers (D. Pizzocri, G. Zullo) for support.