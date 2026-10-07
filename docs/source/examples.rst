Examples
========

This page walks through a complete SCIANTIX calculation. It uses the isothermal annealing benchmark of Baker (1977), which runs in a few seconds and needs no external libraries. Short additional examples are given at the end.

Tutorial: intragranular bubbles in UO\ :sub:`2` at constant temperature (Baker, 1977)
-----------------------------------------------------------------------------------------

Background
~~~~~~~~~~

:ref:`Baker (1977) <ref-baker1977>` examined UO\ :sub:`2` fuel pins irradiated at constant temperature and measured, in the fuel grains, the concentration and radius of the intragranular fission gas bubbles and the resulting gaseous swelling, for temperatures between about 1273 K and 2073 K. SCIANTIX reproduces this experiment as a 0D calculation on a single spherical grain of 5 µm radius, with a constant temperature and fission rate (1e19 fiss/m\ :sup:`3` s) for 5500 h. The validation set is made of nine cases, one per temperature, in ``validation/baker/``; this tutorial uses the 1273 K case ``test_Baker1977__1273K``.

In this case the gas produced by fission (Xe and Kr) diffuses inside the grain, is trapped in intragranular bubbles and is re-dissolved by irradiation, while the bubbles nucleate and grow. The grain size is allowed to evolve, and the gas reaching the grain boundary feeds the intergranular bubbles. The models involved are, in the order of the flags in the settings file:

- grain growth (:doc:`models/grain_growth`);
- diffusion of the fission products and the spectral diffusion solver (:doc:`models/gas_diffusion`, :doc:`solvers`);
- intragranular bubbles: nucleation, trapping and re-solution (:doc:`models/intragranular_bubble_behavior`);
- intergranular bubbles and micro-cracking (:doc:`models/intergranular_bubble_behavior`, :doc:`models/grain_boundary_microcracking`).

Prerequisites
~~~~~~~~~~~~~

SCIANTIX must be built as described in :doc:`installation`. In the following, ``sciantix.x`` is the executable (``build/sciantix.x`` from the repository root), and the commands are run from the repository root.

Step 1: prepare the input files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A case is a folder. Copy the Baker case to a new folder to experiment without touching the reference:

.. code-block:: bash

    cp -r validation/baker/test_Baker1977__1273K my_baker_case

It contains three input files (``input_scaling_factors.txt`` is optional and not needed here, since all the scaling factors default to 1) and the reference result ``output_gold.txt``. The complete description of the files is in :doc:`input_files`; the conventions that matter for this case are given below.

**input_settings.txt** selects the models. Each line is ``<value> # <flag> (<description>)``: SCIANTIX reads the value and the flag name, and the order of the lines is irrelevant. The lines that are active (non-zero) in this case are:

.. code-block:: text

    1    #    iGrainGrowth (0= no grain growth, 1= Ainscough et al. (1973), 2= Van Uffelen et al. (2013))
    1    #    iFissionProductDiffusivity (0= constant value, 1= Turnbull et al. (1988))
    2    #    iDiffusionSolver (1= SDA with quasi-stationary hypothesis, 2= SDA without quasi-stationary hypothesis)
    1    #    iIntraGranularBubbleBehavior (1= Pizzocri et al. (2018))
    1    #    iResolutionRate (0= constant value, 1= Turnbull (1971), 2= Losonen (2000), 3= thermal resolution, Cognini et al. (2021))
    1    #    iTrappingRate (0= constant value, 1= Ham (1958))
    1    #    iNucleationRate (0= constant value, 1= Olander, Wongsawaeng (2006))
    1    #    iOutput (1= default output files)
    1    #    iGrainBoundaryVacancyDiffusivity (0= constant value, 1= Reynolds and Burton (1979), 2= White (2004))
    1    #    iGrainBoundaryBehaviour (0= no grain boundary bubbles, 1= Pastore et al (2013))
    1    #    iGrainBoundaryMicroCracking (0= no model considered, 1= Barani et al. (2017), 2= Cappellari et al. (2025))

**input_history.txt** has four columns without header: time (h), temperature (K), fission rate density (fiss/m\ :sup:`3` s) and hydrostatic stress (MPa). The conditions are interpolated linearly between rows.

.. code-block:: text

    0       1273    1e19    0
    5500    1273    1e19    0

The simulation therefore lasts 5500 h at a constant 1273 K, a constant fission rate of 1e19 fiss/m\ :sup:`3` s and zero hydrostatic stress. By default each interval between two rows is divided into 100 time steps.

**input_initial_conditions.txt** defines the state of the grain at time zero. Here the fuel is fresh: no gas, no bubbles, zero burnup.

.. code-block:: text

    5.0e-06                        # Grain_radius[0] (initial grain radius (m))
    0.0 0.0 0.0 0.0 0.0 0.0        # Initial_composition_Xe (initial Xe (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released)
    0.0 0.0 0.0 0.0 0.0 0.0        # Initial_composition_Kr (...)
    0.0 0.0 0.0 0.0 0.0 0.0        # Initial_composition_He (...)
    0.0 0.0                        # Initial_intragranular_bubbles (initial intragranular bubble concentration (bub/m3), radius (m))
    0.0                            # Burn_up[0] (initial fuel burn-up (MWd/kgUO2))
    0.0                            # Effective_burn_up[0] (initial fuel effective burn-up (MWd/kgUO2))
    0.0                            # Irradiation_time[0] (initial irradiation time (h))
    10641.0                        # Fuel_density[0] (initial fuel density (kg/m3))
    0.0 3.0 0.0 0.0 97.0           # Initial_composition_U (initial U234 U235 U236 U237 U238 content (% of heavy atoms))
    ...                            # (further entries, all zero here, are in the file)


Step 2: run SCIANTIX
~~~~~~~~~~~~~~~~~~~~

Pass the case folder to the executable:

.. code-block:: bash

    ./build/sciantix.x my_baker_case/

The run takes a few seconds. The results and a record of the run are written in the same folder: ``output.txt`` (the results), ``input_check.txt`` (the input flags as read by the code), ``overview.txt`` (the model and reference selected for each phenomenon) and ``execution.txt``. A first check is to open ``overview.txt`` and ``input_check.txt`` to verify that the intended models were selected.

.. _tutorial-output:

Step 3: check the output
~~~~~~~~~~~~~~~~~~~~~~~~

``output.txt`` is a tab-separated table. The first line is a header with the name and unit of each column and every following line is one time step (here 101 rows: the initial state and the 100 steps). The columns used most often are:

.. list-table::
   :header-rows: 1
   :widths: 8 52 25

   * - Column
     - Quantity
     - Notes
   * - 1
     - Time
     - From ``input_history.txt``
   * - 2-4
     - Temperature, fission rate, hydrostatic stress
     - Interpolated history
   * - 5
     - Grain radius
     - Constant if ``iGrainGrowth = 0``
   * - 6-17
     - Xe and Kr produced / in grain / in intragranular solution / in intragranular bubbles / at grain boundary / released
     - 
   * - 18
     - Fission gas release
     - Released gas over produced gas
   * - 19-21
     - *Intragranular bubble:* concentration, radius and gas swelling
     - 
   * - 22-31
     - *Intergranular bubbles:* concentration, atoms and vacancies per bubble, radius, area, volume, fractional coverage, saturation coverage, swelling, fractional intactness
     - Grain-boundary variables
   * - 32
     - Burnup
     - Calculated from the fission rate

Reading the last row, at 5500 h, gives the quantities that Baker measured:

.. code-block:: python

    import numpy as np

    with open("my_baker_case/output.txt") as f:
        header = f.readline().rstrip("\n").split("\t")
    data = np.loadtxt("my_baker_case/output.txt", skiprows=1)
    last = dict(zip(header, data[-1]))

    print("bubble concentration (bub/m3):", last["Intragranular bubble concentration (bub/m3)"])
    print("bubble radius (m):            ", last["Intragranular bubble radius (m)"])
    print("gas swelling (%):             ", 100 * last["Intragranular gas bubble swelling (/)"])
    print("fission gas release (/):      ", last["Fission gas release (/)"])

For 1273 K the result is a bubble concentration of 5.9e23 bub/m\ :sup:`3`, a bubble radius of 0.50 nm, an intragranular swelling of 0.031 % and a fission gas release of 13%. The experimental values of Baker, in ``validation/baker/data/``, are 8.7e23 bub/m\ :sup:`3`, 0.55 nm and 0.06 %.

**Regression.** ``output_gold.txt`` is the reference output of the case, with the same layout. If the code is unchanged, ``output.txt`` agrees with it; this is what ``python3 -m testing.runner --baker`` checks for all nine temperatures. ``validation/baker/parity_plot.py`` collects the last row of every case and plots it against the experimental data.

Things to try
~~~~~~~~~~~~~

- Change the temperature in ``input_history.txt`` (both rows) and compare with the corresponding Baker case.
- Switch off grain growth (``iGrainGrowth = 0``) and see the effect on the gas release.
- Add an ``input_scaling_factors.txt`` containing ``2.0 # sf_resolution_rate`` and observe the change of the bubble concentration and radius.

Additional short examples
-------------------------

**A power transient.** More rows in ``input_history.txt`` describe ramps and holds. For instance, the history below holds 1000 h at 1273 K, ramps to 1773 K in 10 h and holds for 100 h, at constant fission rate:

.. code-block:: text

    0       1273    1e19    0
    1000    1273    1e19    0
    1010    1773    1e19    0
    1110    1773    1e19    0

**Helium annealing.** Set ``iHelium = 1`` and ``iHeDiffusivity = 1`` in ``input_settings.txt``, give the initial helium inventory in ``Initial_composition_He`` and use a history without fission (rate 0). The cases in ``validation/talip`` are complete examples; see :doc:`testing`.

**Other cases.** Every case of ``verification/`` and ``validation/`` is a ready-to-run example, with its own input files and reference output (see :doc:`testing`).
