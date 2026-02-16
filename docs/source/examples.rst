Examples
========

This section presents typical usage examples of SCIANTIX.

Baker (1977) Benchmark
----------------------

This example corresponds to the regression test ``test_Baker1977__1273K`` found in ``regression/baker/``. It represents an isothermal annealing experiment.

Input Settings
~~~~~~~~~~~~~~

The ``input_settings.txt`` file defines the active physical models.

.. code-block:: text

    1    #    iGrainGrowth (1= Ainscough et al. (1973))
    1    #    iFissionGasDiffusivity (1= Turnbull et al. (1988))
    2    #    iDiffusionSolver (2= SDA without quasi-stationary hypothesis)
    1    #    iIntraGranularBubbleBehavior (1= Pizzocri et al. (2018))
    1    #    iResolutionRate (1= Turnbull (1971))
    1    #    iTrappingRate (1= Ham (1958))
    1    #    iNucleationRate (1= Olander, Wongsawaeng (2006))
    1    #    iOutput (1= default output files)
    1    #    iGrainBoundaryVacancyDiffusivity (1= Reynolds and Burton (1979))
    1    #    iGrainBoundaryBehaviour (1= Pastore et al (2013))
    1    #    iGrainBoundaryMicroCracking (1= Barani et al. (2017))
    0    #    iFuelMatrix (0= UO2)
    0    #    iGrainBoundaryVenting (0= no model considered)
    0    #    iRadioactiveFissionGas (0= not considered)

Input History
~~~~~~~~~~~~~

The ``input_history.txt`` file defines the irradiation history. The columns are:
1. Time (h)
2. Temperature (K)
3. Fission Rate (fissions/m3/s)
4. Hydrostatic Stress (MPa)

.. code-block:: text

    0	    1273	1e19	0
    5500	1273	1e19	0

In this example:
- The simulation runs for **5500 hours**.
- The temperature is constant at **1273 K**.
- The fission rate is constant at **1e19 fis/m3/s**.
- The hydrostatic stress is **0 MPa**.

Expected Output
~~~~~~~~~~~~~~~

Key output variables for this case include:
- **Intragranular bubble swelling**
- **Grain boundary coverage**
- **Fission gas release**

You can find the reference results in ``output_gold.txt`` within the regression folder.
