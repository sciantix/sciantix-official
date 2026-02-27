SCIANTIX documentation
======================

**SCIANTIX** is a physics-based meso-scale simulation code for modeling the behavior of nuclear fuel grains. It bridges the gap between atomistic simulations and engineering-scale fuel performance codes.

.. grid:: 1 1 2 2
    :gutter: 3

    .. grid-item-card::  Mechanistic Modeling
        :icon: microscope
        
        Focuses on physics-based rate-theory models rather than empirical correlations for fission gas behavior.

    .. grid-item-card::  Numerical Robustness
        :icon: gear
        
        Employs L-stable implicit integrators and spectral diffusion solvers for stable and efficient simulations.

    .. grid-item-card::  Highly Integrated
        :icon: link
        
        Designed to be embedded within industrial fuel performance codes like TRANSURANUS and OFFBEAT.

    .. grid-item-card::  Open Science
        :icon: eye
        
        Open-source development with comprehensive regression testing and physical verification.

Quick Start
-----------

To get started with SCIANTIX, follow these steps:

1. **Install Dependencies**: Ensure you have a C++17 compiler and CMake installed.
2. **Clone and Build**:
   
   .. code-block:: bash

      git clone https://github.com/sciantix/sciantix-official.git
      cd sciantix-official
      ./Allmake.sh

3. **Run a Test Case**:
   
   .. code-block:: bash

      cd regression
      python3 runner.py --baker

Next Steps
----------

.. toctree::
   :maxdepth: 2
   :caption: User Guide

   overview
   installation
   examples
   regression
   references

.. toctree::
   :maxdepth: 2
   :caption: Physics & Numerics

   models
   solvers

.. toctree::
   :maxdepth: 1
   :caption: API Reference

   api/index_api

Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
