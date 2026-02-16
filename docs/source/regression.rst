Regression
==========

SCIANTIX includes a comprehensive regression testing suite to ensure code stability and physics verification. The regression tests are located in the ``regression/`` directory.

Running Regression Tests
------------------------

The regression suite is controlled by the Python script ``runner.py`` located in ``regression/``.

To run **all** regression tests:

.. code-block:: bash

    python3 -m regression.runner --all

To run a **specific suite** (e.g., Baker benchmarks):

.. code-block:: bash

    python3 -m regression.runner --baker

Available flags include:
- ``--white``
- ``--cornell``
- ``--kashibe``
- ``--talip``
- ``--oxidation``
- ``--chromium``
- ``--contact``
- ``--hbs``
- ``--vercors``
- ``--pulse``
- ``--gpr``

Comparison Modes
----------------

You can control how the results are verified using the ``--mode-gold`` argument:

- ``0``: Run simulation + Compare with gold standard (Default)
- ``1``: Run simulation + Update gold standard (Use with caution!)
- ``2``: Compare existing output with gold standard
- ``3``: Gold standard check only

Test Case Structure
-------------------

Each regression test case is a directory containing:

- **input_settings.txt**: configuration flags for models.
- **input_history.txt**: time-dependent boundary conditions (Time, Temperature, Fission Rate, Stress).
- **input_initial_conditions.txt**: initial values for state variables.
- **output_gold.txt**: reference results for verification.

Available Test Suites
---------------------

- **Baker**: Isothermal irradiation/annealing cases (Baker 1977).
- **White**: Intragranular bubble evolution benchmarks (White 1983).
- **Cornell**: Re-solution/nucleation rate tests.
- **Kashibe**: Ramp tests (Kashibe et al.).
- **Talip**: Power ramp experiments.
- **Oxidation**: Stoichiometry deviation and oxidation tests.
- **HBS**: High-Burnup Structure formation and porosity.
- **Chromium**: Chromium-doped fuel tests.
- **Contact**: Fuel-cladding mechanical contact tests.
- **Vercors**: Fission gas release validation against VERCORS experiments.
- **GPR**: Gaussian Process Regression integration tests.
