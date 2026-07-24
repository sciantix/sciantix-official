Testing SCIANTIX
================

SCIANTIX includes a comprehensive testing suite to ensure code stability, physics verification, and model validation. Test cases live in two top-level directories:

- ``verification/`` — does SCIANTIX reproduce its own model?
- ``validation/``   — does SCIANTIX match real experimental data?

Running Tests
-------------

The testing suite is controlled by the Python script ``runner.py`` located in ``testing/``.

To run **all** tests:

.. code-block:: bash

    python3 -m testing.runner

To run a **whole suite**:

.. code-block:: bash

    python3 -m testing.runner --verification
    python3 -m testing.runner --validation

To run a **specific group** (e.g., Baker benchmarks):

.. code-block:: bash

    python3 -m testing.runner --baker

Available group flags include:

- ``--openPorosity`` / ``--powerPulse`` (also reachable together via ``--pulse``/``--analytics``)
- ``--oxidation``
- ``--vercors``
- ``--gpr``
- ``--mox-po2``
- ``--baker``
- ``--cornell``
- ``--white``
- ``--kashibe``
- ``--talip``
- ``--chromium``
- ``--contact``
- ``--hbs``
- ``--jog``
- ``--oxygenpotential-freshfuel`` / ``--oxygenpotential-burnup`` (also reachable together via ``--oxygenpotential``)

OpenCalphad-dependent groups
----------------------------

``jog``, ``oxygenpotential-freshfuel``/``oxygenpotential-burnup``, and ``mox-po2`` all use the OpenCalphad (OC) coupling for part of their checks. Every one of them is attempted on every run, whether or not OC is linked:

- ``mox-po2`` and the ``oxygenpotential-*`` groups degrade gracefully: the OC-independent (Kato-path) part of the check always runs, and the OC-dependent part is skipped with a warning if OpenCalphad isn't linked or ``upuo-v21.TDB`` isn't found next to it.
- ``jog`` has no OC-independent analog for what it measures, so the whole group is skipped with a warning instead.

Pass ``--oc`` to assert that OpenCalphad is expected to be available: if it turns out not to be (e.g. a forgotten ``Allmake.sh --oc`` build), these groups fail loudly instead of degrading/skipping, so the run doesn't go green by accident.

.. code-block:: bash

    python3 -m testing.runner --oc

Comparison Modes
----------------

You can control how the results are verified using the ``--mode-gold`` argument:

- ``0``: Run simulation + Compare with gold standard (Default)
- ``1``: Run simulation + Update gold standard (Use with caution!)
- ``2``: Compare existing output with gold standard
- ``3``: Gold standard check only

For OpenCalphad-dependent groups, modes ``1``/``3`` (gold rewrite) are refused rather than executed if OC is unavailable, so a non-OC build can't silently overwrite real CALPHAD gold values.

Test Case Structure
-------------------

Each test case is a directory containing:

- **input_settings.txt**: configuration flags for models.
- **input_history.txt**: time-dependent boundary conditions (Time, Temperature, Fission Rate, Stress).
- **input_initial_conditions.txt**: initial values for state variables.
- **output_gold.txt**: reference results for verification.

Available Test Suites
---------------------

Verification
~~~~~~~~~~~~

- **openPorosity / powerPulse**: analytic self-consistency checks.
- **Oxidation**: stoichiometry deviation and oxidation tests.
- **Vercors**: fission gas release vs. the VERCORS-5 severe-accident campaign.
- **GPR**: Gaussian Process Regression integration tests.
- **MOX pO2**: MOX oxygen-potential verification against the explicit Kato equation (always) and independent Thermo-Calc tables (only if OpenCalphad/``upuo-v21.TDB`` is available).

Validation
~~~~~~~~~~

- **Baker**: Isothermal irradiation/annealing cases (Baker 1977).
- **White**: Intragranular bubble evolution benchmarks (White 1983).
- **Cornell**: Re-solution/nucleation rate tests.
- **Kashibe**: Ramp tests (Kashibe et al.).
- **Talip**: Power ramp experiments.
- **HBS**: High-Burnup Structure formation and porosity.
- **Chromium**: Chromium-doped fuel tests.
- **Contact**: Fuel-cladding mechanical contact tests.
- **JOG**: OpenCalphad-coupled PHENIX pin fission-product speciation (skipped without OC).
- **Oxygen potential (fresh fuel / burnup)**: MOX oxygen-potential validation against digitized experimental datasets, split by fresh vs. irradiated fuel.
