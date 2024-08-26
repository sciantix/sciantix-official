# Input explanation

This folder contains the essential input files required by Sciantix:

- **`input_settings.txt`**: Specifies the models and numerical solvers used in the simulation.
- **`input_history.txt`**: Includes the time (h), temperature (K), fission rate (fiss/m³-s), and hydrostatic stress (MPa) as a function of time.
- **`input_initial_conditions.txt`**: Sets initial conditions for the simulation.
- **`input_scaling_factors.txt`**: (Optional) File for scaling factors.

Below is a detailed description of each file.

---

## Input Settings

The following settings define the models and methods used for the simulation.

```plaintext
1  # iGrainGrowth (0 = no grain growth, 1 = Ainscough et al. (1973), 2 = Van Uffelen et al. (2013))
1  # iFissionGasDiffusivity (0 = constant value, 1 = Turnbull et al. (1988))
1  # iDiffusionSolver (1 = SDA with quasi-stationary hypothesis, 2 = SDA without quasi-stationary hypothesis)
1  # iIntraGranularBubbleBehavior (1 = Pizzocri et al. (2018))
1  # iResolutionRate (0 = constant value, 1 = Turnbull (1971), 2 = Losonen (2000), 3 = thermal resolution, Cognini et al. (2021))
1  # iTrappingRate (0 = constant value, 1 = Ham (1958))
1  # iNucleationRate (0 = constant value, 1 = Olander, Wongsawaeng (2006))
1  # iOutput (1 = default output files)
1  # iGrainBoundaryVacancyDiffusivity (0 = constant value, 1 = Reynolds and Burton (1979), 2 = Pastore et al. (2015))
1  # iGrainBoundaryBehaviour (0 = no grain boundary bubbles, 1 = Pastore et al. (2013))
1  # iGrainBoundaryMicroCracking (0 = no model considered, 1 = Barani et al. (2017))
0  # iFuelMatrix (0 = UO2, 1 = UO2 + HBS)
0  # iGrainBoundaryVenting (0 = no model considered, 1 = Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE)
0  # iRadioactiveFissionGas (0 = not considered)
0  # iHelium (0 = not considered)
0  # iHeDiffusivity (0 = null value, 1 = limited lattice damage, Luzzi et al. (2018), 2 = significant lattice damage, Luzzi et al. (2018))
0  # iGrainBoundarySweeping (0 = no model considered, 1 = TRANSURANUS swept volume model)
0  # iHighBurnupStructureFormation (0 = no model considered, 1 = fraction of HBS-restructured volume from Barani et al. (2020))
0  # iHighBurnupStructurePorosity (0 = no evolution of HBS porosity, 1 = HBS porosity evolution based on Spino et al. (2006) data)
0  # iHeliumProductionRate (0 = zero production rate, 1 = helium from ternary fissions, 2 = linear with burnup (FR))
0  # iStoichiometryDeviation (0 = not considered, 1 = Cox et al. (1986), 2 = Bittel et al. (1969), 3 = Abrefah et al. (1994), 4 = Imamura et al. (1997), 5 = Langmuir-based approach)
0  # iBubbleDiffusivity (0 = not considered, 1 = volume diffusivity)
```

---

## Input Initial Conditions

This file sets the initial conditions for various parameters within the simulation.

```plaintext
5.0e-06
# initial grain radius (m)
0.0 0.0 0.0 0.0 0.0 0.0
# initial Xe (at/m³) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released
0.0 0.0 0.0 0.0 0.0 0.0
# initial Kr (at/m³) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released
0.0 0.0
# initial He (at/m³) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released
0.0 0.0
# initial intragranular bubble concentration (at/m³), radius (m)
0.0
# initial fuel burn-up (MWd/kgUO2)
0.0
# initial fuel effective burn-up (MWd/kgUO2)
0.0
# initial irradiation time (h)
10641.0
# initial fuel density (kg/m³)
0.0 3.0 0.0 0.0 97.0
# initial U234 U235 U236 U237 U238 (% of heavy atoms) content
0.0 0.0 0.0 0.0 0.0 0.0 0.0
# initial Xe133 (at/m³) produced, intragranular, intragranular in solution, intragranular in bubbles, decayed, grain boundary, released
0.0 0.0 0.0 0.0 0.0 0.0 0.0
# initial Kr85m (at/m³) produced, intragranular, intragranular in solution, intragranular in bubbles, decayed, grain boundary, released
0.0
# initial fuel stoichiometry deviation (\)
```

**Note**: Ensure that the comment symbol (`#`) is placed **below** each data series. This is mandatory.

---

## Input Scaling Factors

This file contains the scaling factors applied to various model parameters during the simulation.

```plaintext
1.0
# scaling factor - resolution rate
1.0
# scaling factor - trapping rate
1.0
# scaling factor - nucleation rate
1.0
# scaling factor - diffusivity
1.0
# scaling factor - temperature
1.0
# scaling factor - fission rate
1.0
# scaling factor - cent parameter
1.0
# scaling factor - helium production rate
1.0
# scaling factor - dummy
```

---

If you experience any issues with these files, please contact the main developers (D. Pizzocri, G. Zullo) for support.