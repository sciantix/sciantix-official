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
  - name: Lelio Luzzi
    affiliation: 1
  - name: Davide Pizzocri
    affiliation: 1

affiliations:
  - name: Politecnico di Milano, Department of Energy, Nuclear Engineering Division, Milano, Italy
    index: 1

date: 2025-11-28
bibliography: paper.bib
---

# Summary
SCIANTIX is an open-source multi-scale code for modelling fission gas behaviour in oxide nuclear fuel, together with the relevant microstructural phenomena. The code behaves as a 0D meso-scale solver implementing physics-based rate-theory models, designed to be either a stand-alone tool for separate-effect calculations or a module embedded in engineering thermo-mechanical fuel performance codes (FPCs). Compared with empirical approaches used in FPCs, SCIANTIX solves evolutionary equations for stable and radioactive intra-granular gas diffusion, trapping, re-solution, grain-boundary bubble evolution, diffusional and burst release, helium behaviour and high-burnup structure (HBS) porosity. The code adopts verified numerical solvers and includes a regression suite to ensure reproducibility.

# Statement of need
Engineering FPCs typically rely on empirical correlations for modelling fission gas release and gaseous swelling. The underlying physical processes are commonly described using classical rate-theory formulations [@Forsberg1985a; @Forsberg1985b], which are often simplified in engineering codes for robustness and computational speed. These implementations are usually calibrated for specific datasets or reactor conditions and are rarely available as open-source software, limiting reproducibility and research efforts for new fuel designs or irradiation scenarios.

SCIANTIX addresses these limitations by providing:
1. An open-source physics-based meso-scale module, independent of any specific FPC [@Pizzocri2020].
2. A modular C++ architecture enabling extensions and direct coupling to external multi-physics solvers [@Zullo2023].
3. Numerical verification through the Method of Manufactured Solutions (MMS) for all employed solvers.
4. A regression testing suite, covering intra-/inter-granular swelling, HBS porosity, helium and radioactive gas release.
5. A clear API, already used for online coupling with FPCs such as TRANSURANUS, FRAPCON/FRAPTRAN and OFFBEAT [@Zullo2024].

# Software description

## Implementation
The SCIANTIX 2.0 architecture adopts:
* Object-oriented structure (matrix, gas, systems, models and solvers as independent classes).
* Solver–model separation, enabling independent MMS verification and separate-effect model validations.
* Spectral diffusion solver, providing a meshless method with controlled numerical error for Booth-type diffusion problems.
* First-order L-stable implicit time integrators.
* Segregated operator-splitting scheme, ensuring CPU-time compatibility with online coupling to engineering-scale codes.

## Functionality
SCIANTIX models:
* intra-granular diffusion, trapping and irradiation-induced re-solution;
* nucleation and growth of intra-granular bubbles;
* grain-boundary bubble growth, coalescence, saturation and burst release;
* HBS formation and porosity evolution;
* helium diffusion, solubility, trapping and thermal re-solution;
* release of short-lived radioactive fission gases (diffusion–decay with first-precursor enhancement).

# Verification and Validation
The numerical solvers are verified using the Method of Manufactured Solutions [@Oberkampf2004; @Zullo2022], with verification tests available in the repository. Separate-effect validation reproduces the datasets used in the SCIANTIX publications [@Pizzocri2020; @Zullo2023], without parameter tuning and using published models and parameters.

# Research efforts and ongoing developments
SCIANTIX is continuously developed within international research projects. Current development directions include:
* digital-twin workflows, where SCIANTIX provides fast physics-based evaluations of He behaviour for real-time monitoring;
* reduced-order models (ROMs) enabling fast surrogate modelling of complex multi-scale phenomena (e.g., diffusion in non-spherical geometries);
* machine-learning assisted developments, including Gaussian Process regression for automatic correlation update;
* generalisation to volatile fission products, including thermo-chemical evaluations.

# Acknowledgements
The authors acknowledge contributions from collaborators providing feedback and support during development.

# References
