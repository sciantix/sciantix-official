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

SCIANTIX addresses these limitations by providing an open-source physics-based meso-scale module [@Pizzocri2020], together with a modular C++ architecture that enables extensions and direct coupling to external multi-physics solvers [@Zullo2023]. The code is supported by numerical verification through the Method of Manufactured Solutions (MMS) for all employed solvers, and by a regression testing suite covering intra- and inter-granular swelling, HBS porosity, helium behaviour and radioactive gas release. A stable API facilitates its integration into engineering-scale codes, and is already used for online coupling with TRANSURANUS, FRAPCON/FRAPTRAN and OFFBEAT [@Zullo2024].

# Software description

## Implementation
SCIANTIX (>2.0) adopts an object-oriented structure, in which matrices, gas, systems, models and solvers are implemented as independent classes. Separation between solvers and models supports independent MMS verification and separate-effect model validation. The spectral diffusion solver provides a meshless approach with controlled numerical error for Booth-type diffusion problems. First-order L-stable implicit time integrators ensure numerical stability. A segregated operator-splitting scheme maintains CPU-time compatibility in online coupling with engineering-scale thermo-mechanical codes.

## Functionality
SCIANTIX models intra-granular diffusion, trapping and irradiation-induced re-solution; nucleation and growth of intra-granular bubbles; grain-boundary bubble growth, coalescence, saturation and fission gas release; HBS formation and porosity evolution; helium diffusion, solubility and thermal re-solution; the release of short-lived radioactive fission gases through diffusion–decay with first-precursor enhancement.

## Verification and Validation
The numerical solvers are verified using the Method of Manufactured Solutions [@Oberkampf2004; @Zullo2022], with verification tests available in the repository. Separate-effect validation reproduces the published results [@Pizzocri2020; @Zullo2023] without parameter tuning and using published models and parameters.

# Research efforts and ongoing developments
SCIANTIX is continuously developed within international research projects (e.g., R2CA, PATRICIA, OperaHPC, TRANSPARANT). Current development covers digital-twin workflows, in which SCIANTIX provides fast calculations of helium behaviour for real-time monitoring; reduced-order models enabling accelerated surrogate evaluations of complex multi-scale phenomena; machine-learning assisted developments such as Gaussian Process regression for automatic correlation updates; and extensions towards volatile fission products including thermo-chemical evaluations.

# Acknowledgements
The authors acknowledge contributions from collaborators providing feedback and support during development.

# References
