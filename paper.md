---
title: "SCIANTIX: an open-source multi-scale code for fission gas behaviour modelling designed for nuclear fuel performance codes"
tags:
  - C++
  - nuclear engineering
  - fuel performance
  - fission gas
  - mesoscale modelling

authors:
  - name: Davide Pizzocri
    affiliation: 1
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

affiliations:
  - name: Politecnico di Milano, Department of Energy, Nuclear Engineering Division, Milano, Italy
    index: 1

date: 2025-11-28
bibliography: paper.bib
---

# Summary

Fission gases produced during nuclear reactor operation strongly influence fuel swelling, pressure build-up, and overall fuel performance. Predictive modelling of these processes is therefore essential for safe and efficient reactor operation. SCIANTIX is an open-source multi-scale code for modelling fission gas behaviour in oxide nuclear fuel, together with the relevant microstructural phenomena. The code behaves as a 0D meso-scale solver implementing physics-based rate-theory models, designed to be either a stand-alone tool for separate-effect calculations or a module embedded in engineering thermo-mechanical fuel performance codes (FPCs). Compared with empirical approaches used in FPCs, SCIANTIX solves evolutionary equations for stable and radioactive intra-granular gas diffusion, trapping, re-solution, grain-boundary bubble evolution, diffusional and burst release, helium behaviour and high-burnup structure (HBS) porosity. The code adopts verified numerical solvers and includes a regression suite to ensure reproducibility.

# Statement of need

Engineering FPCs typically rely on empirical correlations for modelling fission gas release and gaseous swelling. The underlying physical processes are commonly described using classical rate-theory formulations [@Forsberg1985a; @Forsberg1985b], which are often simplified in engineering codes for robustness and computational speed. These implementations are usually calibrated for specific datasets or reactor conditions and are rarely available as open-source software, limiting reproducibility and research efforts for new fuel designs or irradiation scenarios.

SCIANTIX is intended for nuclear fuel performance researchers, code developers, and advanced students working on mechanistic modelling of fission gas behaviour.

SCIANTIX addresses these limitations by providing an open-source physics-based meso-scale module [@Pizzocri2020], together with a modular C++ architecture that enables extensions and direct coupling to external multi-physics solvers [@Zullo2023]. The code is supported by numerical verification through the Method of Manufactured Solutions (MMS) for all employed solvers, and by a regression testing suite covering intra- and inter-granular swelling, HBS porosity, helium behaviour and radioactive gas release. A stable API facilitates its integration into engineering-scale codes, and is already used for online coupling with TRANSURANUS, FRAPCON/FRAPTRAN and OFFBEAT [@Zullo2024].

# State of the field                                                                                                           

Fission gas behaviour in oxide fuels is traditionally modelled within engineering-scale FPCs, where gas release and swelling are represented through empirical correlations calibrated to selected datasets. While effective for industrial and licensing applications, these tools are typically closed-source and offer limited flexibility for systematic methodological development. At the opposite end of the modelling spectrum, microstructural approaches such as phase-field and atomistic simulations provide detailed spatial resolution but remain computationally intensive and unsuitable for routine integration within engineering-scale thermo-mechanical solvers. 

SCIANTIX bridges this methodological gap by providing a computationally efficient meso-scale framework that retains mechanistic transparency while remaining compatible with multi-physics coupling. Rather than modifying proprietary engineering codes, SCIANTIX was developed as an independent open-source platform to provide a verified and extensible meso-scale module that can operate both stand-alone and within established fuel performance frameworks.


# Software design

The current release of SCIANTIX (version 2.0 and above) adopts a fully object-oriented C++ structure, in which core simulation entities like matrices, gas, systems, models and solvers are implemented as independent classes. This modular abstraction improves maintainability, clarity and extensibility.

A fundamental design decision is the separation between solvers and models. This supports the independent verification and separate-effect model validation. The spectral diffusion solver provides a meshless approach with controlled numerical error for Booth-type diffusion problems. First-order L-stable implicit time integrators ensure numerical stability. A segregated operator-splitting scheme maintains CPU-time compatibility in online coupling with engineering-scale thermo-mechanical codes. These choices balance the physical fidelity, numerical robustness, and overall efficiency.

SCIANTIX models intra-granular diffusion, trapping and irradiation-induced re-solution; nucleation and growth of intra-granular bubbles; grain-boundary bubble growth, coalescence, saturation and fission gas release; HBS formation and porosity evolution; helium diffusion, solubility and thermal re-solution; the release of short-lived radioactive fission gases through diffusion–decay with first-precursor enhancement.

The numerical solvers are verified using the Method of Manufactured Solutions [@Oberkampf2004; @Zullo2022], with verification tests available in the repository. Separate-effect validation reproduces the published results [@Pizzocri2020; @Zullo2023] without parameter tuning and using published models and parameters.

# Research impact statement

Since its first release, SCIANTIX has contributed to a growing and internationally collaborative research effort in multi-scale modelling of fission gas behaviour. Between 2020 and 2024, SCIANTIX-related work resulted in 21 peer-reviewed publications (90% of which are all Open Access) involvling 42 authors, including 13 internationally co-authored contributions. These publications have accumulated 173 citations.

Beyond bibliometric indicators, SCIANTIX has been integrated as meso-scale module within established fuel performance frameworks including coupling with TRANSURANUS and OFFBEAT. Its adoption within these engineering-scale environments demonstrates practical use beyond stand-alone research applications. SCIANTIX has also been applied in European research projects addressing advanced fuel concepts, helium behaviour and burnup structure evolution. Its verified numerical solvers and regression-testing infrastructure support reproducible research and independent validation, evidencing realized and continuing impact within the nuclear fuel modelling community.

SCIANTIX has also contributed to European research projects addressing advanced fuel concepts, helium behaviour, and high-burnup structure evolution. Its continued development within collaborative initiatives such as R2CA, PATRICIA, OperaHPC, and TRANSPARANT further demonstrates sustained community engagement and ongoing scientific relevance.

# AI usage disclosure

Generative AI tools were used in the preparation and refinement of parts of the documentation (e.g., language polishing, structural suggestions, and formatting assistance). 

No generative AI tools were used in the development of the SCIANTIX software, in the implementation of its physical models or numerical solvers, or in the preparation of validation results.

# Acknowledgements

The authors acknowledge contributions from collaborators providing feedback and support during development.

# References
