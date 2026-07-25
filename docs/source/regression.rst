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
- ``--all``
- ``--baker``
- ``--white``
- ``--cornell``
- ``--kashibe``
- ``--talip``
- ``--oxidation``
- ``--chromium``
- ``--contact``
- ``--hbs``
- ``--vercors``
- ``--analytics`` (``--pulse`` is an alias)
- ``--gpr``

Several flags can be combined, and a single case can be selected with
``--<group>.<case>``, for instance ``--baker.1273K``. Runs can be parallelised with
``-j``/``--jobs``.

Comparison Modes
----------------

You can control how the results are verified using the ``--mode-gold`` argument:

- ``0``: Run simulation + Compare with gold standard (Default)
- ``1``: Run simulation + Update gold standard (Use with caution!)
- ``2``: Compare existing output with gold standard
- ``3``: Update gold standard from the existing output, without running the simulation

The comparison is element-wise, with an absolute tolerance of 1e-8 and a relative
tolerance of 1e-6; the column headers must match as well. Regenerate the gold standard
deliberately, never to paper over an unexplained difference.

Test Case Structure
-------------------

Each regression test case is a directory containing:

- **input_settings.txt**: configuration flags for models.
- **input_history.txt**: time-dependent boundary conditions (Time, Temperature, Fission Rate, Stress).
- **input_initial_conditions.txt**: initial values for state variables.
- **output_gold.txt**: reference results for verification.

Available Test Suites
---------------------

- **Baker**: Intragranular gaseous swelling, bubble radius and density (Baker 1977).
- **White**: Intergranular gaseous swelling and grain-face porosity (White 2004).
- **Cornell**: Intragranular bubble radius and density (Cornell 1969).
- **Kashibe**: Fission gas release and swelling in annealing and ramp tests (Kashibe et al. 1990, 1991, 1993).
- **Talip**: Helium fractional release and release rate under annealing (Talip et al. 2014).
- **Oxidation**: Stoichiometry deviation under oxidising conditions.
- **HBS**: High-burnup structure formation, porosity and pore statistics.
- **Chromium**: Chromium-doped fuel: solubility and fission gas release.
- **Contact**: Release-to-birth ratio of Xe133 and Kr85m (CONTACT irradiation experiment), compared with ANS-5.4.
- **Vercors**: Fission gas release validation against VERCORS experiments.
- **Analytics**: Analytic checks (power pulse, open porosity); ``--pulse`` is an alias.
- **GPR**: Gaussian Process Regression integration tests.
