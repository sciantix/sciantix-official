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
        
        Open-source development with comprehensive testing and physical verification.

Quick Start
-----------

To get started with SCIANTIX, follow these steps:

1. **Install Dependencies**: Ensure you have a C++17 compiler and CMake installed. Running the test case in step 3 also requires Python ≥ 3.8 with the packages in ``testing/requirements.txt`` (``python3 -m pip install -r testing/requirements.txt``).
2. **Clone and Build**:
   
   .. code-block:: bash

      git clone https://github.com/sciantix/sciantix-official.git
      cd sciantix-official
      mkdir -p build && cd build
      cmake ..
      make -j
      ./sciantix.x --version
      cd ..

   The ``--version`` command checks that the build succeeded (``--help`` lists the expected input files).

3. **Run a Test Case**:

   .. code-block:: bash

      python3 -m testing.runner --baker

   This is a quick check (a few seconds) that the build works. It runs the nine Baker (1977) validation cases, isothermal irradiations of UO\ :sub:`2` between 1273 K and 2073 K, and compares each result with its stored reference output. The Baker cases were chosen because they run in a few seconds, do not require OpenCalphad, and exercise the core intragranular fission gas models. A working installation prints ``PASS`` for every case and ends with ``9 passed, 0 failed, 0 skipped``. The complete test suite is described in :doc:`testing`.

Next Steps
----------

.. toctree::
   :maxdepth: 2
   :caption: User Guide

   overview
   installation
   examples
   testing
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
