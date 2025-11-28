---
title: "SCIANTIX: an open-source multi-scale code for fission gas behaviour modelling designed for nuclear fuel performance codes"
tags:
  - C++
  - nuclear engineering
  - fuel performance
  - fission gas
  - mesoscale modelling

authors:
  - name: Giovanni Zullo
    affiliation: 1
  - name: Elisa Cappellari
    affiliation: 1
  - name: Giovanni Nicodemo
    affiliation: 1
  - name: Aya Zayat
    affiliation: 1
  - name: Davide Pizzocri
    affiliation: 1
  - name: Lelio Luzzi
    affiliation: 1

affiliations:
  - name: Politecnico di Milano, Department of Energy, Nuclear Engineering Division, Milano, Italy
    index: 1

date: 2025-11-28
bibliography: paper.bib
---

# Summary

SCIANTIX is an open-source meso-scale code for the behaviour of gaseous fission products in nuclear fuel. It models the evolution of fission gas atoms and bubbles from the intra-granular scale to the grain boundaries, as well as helium and high-burnup structure (HBS) porosity. The code is written in modern C++ and is designed as a stand-alone 0D grain-scale solver that can be coupled to thermo-mechanical fuel performance codes.

The code implements physics-based models for intra-granular diffusion, trapping and resolution, grain-boundary saturation and venting, bubble growth and coalescence, and HBS porosity formation. These models are validated against dedicated separate-effect experiments and integral rod tests. SCIANTIX has been applied in multiple research and industrial projects, and has been coupled with the fuel performance codes, applied in light water reactor fuels and fast reactor conditions [@Pizzocri2020; @Zullo2023; @Zullo2024].

# Statement of need

Fuel performance codes used by industry, research laboratories and safety authorities require models for fission gas release, swelling, helium behaviour and porosity that are numerically verified and validated against experimental data. Conventional industrial tools rely on semi-empirical correlations applicable to standard datasets, limiting their predictive capability when exploring new conditions such as high burnup, different microstructures or alternative fuel and cladding materials.

SCIANTIX addresses this gap by providing:

- a dedicated, modular meso-scale solver independent of the specific fuel performance code;
- physics-based models for fission gas, helium and HBS behaviour;
- a clear C++ API suitable for integration into external thermo-mechanical solvers;
- a regression test suite and a separate-effect validation database ensuring reproducibility.

The primary users of SCIANTIX are developers of fuel performance tools and researchers working on nuclear fuel behaviour who require a transparent and physically based meso-scale description of fission gas evolution.

# Software description

## Implementation

SCIANTIX is implemented in modern C++ with an object-oriented architecture. The code is organised into core model classes, numerical routines, input/output utilities and a stand-alone driver. The build system uses CMake. The repository includes:

- the SCIANTIX library and stand-alone executable (`sciantix.x`);
- a complete regression test suite with reference outputs;
- documentation generated via Doxygen;
- examples and utilities for generating input and analysing output.

## Functionality

SCIANTIX adopts at 0D rate-theory approach, advancing internal state variables at each timestep, describing:

- intra-granular fission gas atoms, trapping and resolution;
- inter-granular saturation and release;
- bubble growth, coalescence and swelling;
- HBS porosity evolution;
- helium behaviour under irradiation and annealing.

SCIANTIX can run as a stand-alone solver or be embedded in an external code through its C++ API.

# Validation and use

The code has been validated against a dedicated database of experiments and integral tests, covering:

- intra- and inter-granular swelling;
- fission gas release in steady and transient conditions;
- helium annealing tests;
- HBS porosity and bubble size distribution measurements.

The repository provides a full regression suite that reproduces the published validation database, allowing users to verify correctness and reproducibility. SCIANTIX has been used in multiple European research projects and coupled to fuel performance codes [@Pizzocri2020; @Zullo2023; @Zullo2024].

# Acknowledgements

The authors acknowledge contributions from collaborators providing experimental data, validation and feedback during development.

# References
