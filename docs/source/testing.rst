Testing SCIANTIX
================

SCIANTIX includes a comprehensive testing suite to ensure code stability, model verification and validation.
SCIANTIX is tested at four levels, using the terminology of :ref:`Oberkampf and Roy (2010) <ref-oberkampf2010>`: 

.. list-table::
   :header-rows: 1
   :widths: 16 34 30 20

   * - Level
     - Aim
     - Reference
     - Location
   * - **Unit tests**
     - Individual numerical building blocks return the expected values, with tolerances down to 1e-12.
     - Hand-computed values
     - ``tests/`` (``unit_tests.C``, CTest label ``unit``)
   * - **Regression tests**
     - Code stability, i.e., the results of a full simulation have not changed unintentionally.
     - ``output_gold.txt``, which is a previously SCIANTIX output, which has been consolidated by code developers.
     - every case under ``verification/`` and ``validation/``, compared by the engine in ``testing/``
   * - **Verification**
     - The equations are solved correctly.
     - Analytical or manufactured solutions, or another code or table results
     - ``verification/`` (registered groups ``test_*`` and the standalone MMS suite ``Operahpc_5.1``)
   * - **Validation**
     - The models agree with experiments.
     - Experimental data (``data/`` folders, digitized from the cited papers)
     - ``validation/`` (cases, data and plotting scripts; ``parity_by_topic.py`` gives one parity plot per phenomenon)

Please note that:

- **Gold files are not experimental data.** Every ``output_gold.txt`` is an output of SCIANTIX itself, produced on a given version of the code and accepted by the developers after review. A regression test passes if the current code reproduces it, which shows that the code behaves as before. It does not show that the result is correct. Correctness is established separately, by comparing against analytical solutions (verification) or experiments (validation).
- **Validation cases have two comparisons.** The automated pass/fail check compares against the gold file (regression). The comparison with experimental data is made by the plotting scripts in each folder (``parity_plot.py``, ``plot.py``, ...), which produce parity plots and bias figures that are inspected by the developers. They do not decide whether the test passes.

The reference behind each suite is listed in `Available Test Suites`_.

The folder ``testing/`` is the Python engine that runs and checks all the cases of ``verification/`` and ``validation/``: ``runner.py`` holds the list of groups (``REGISTRY``) and the command line, ``core/generic_runner.py`` runs ``sciantix.x`` on every case of a group, ``core/compare.py`` compares ``output.txt`` with ``output_gold.txt``, ``core/mox_po2_runner.py`` adds the oxygen-potential accuracy criteria, ``core/oc_status.py`` detects OpenCalphad, ``core/report.py`` writes the HTML report, and ``core/plot.py`` and ``core/parser.py`` are shared by the plotting scripts. Its Python dependencies are in ``testing/requirements.txt``. A new group needs an entry in ``REGISTRY`` and in the group lists of ``CMakeLists.txt``.

Running Tests
-------------

All tests are registered with CTest, so after building they can be run from the ``build/`` directory with:

.. code-block:: bash

    ctest --output-on-failure -j $(nproc)

or, equivalently, ``make check``. Each test group is a separate CTest test, labelled ``unit``, ``verification`` or ``validation``:

.. code-block:: bash

    ctest -L verification          # a whole suite
    ctest -R validation_baker      # a single group

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

- ``--openPorosity``
- ``--powerPulse``
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
- ``--oxygenpotential-freshfuel``
- ``--oxygenpotential-burnup``

A single case, or the cases whose name contains a given string, can be selected with ``--<group>.<case-substring>``, e.g. ``--baker.1273K``. The runner writes an HTML summary to ``testing/report.html``.

OpenCalphad-dependent groups
----------------------------

``jog``, ``oxygenpotential-freshfuel``/``oxygenpotential-burnup``, and ``mox-po2`` all use the OpenCalphad (OC) coupling for part of their checks. Every one of them is attempted on every run, whether or not OC is linked:

- ``mox-po2`` and the ``oxygenpotential-*`` groups degrade gracefully: the OC-independent (Kato-path) part of the check always runs, and the OC-dependent part is skipped with a warning if OpenCalphad isn't linked or ``upuo-v21.TDB`` isn't found next to it.
- ``jog`` has no OC-independent analog for what it measures, so the whole group is skipped with a warning instead.

Pass ``--oc`` to assert that OpenCalphad is expected to be available: if it turns out not to be (e.g. a forgotten ``Allmake.sh --oc`` build), these groups fail loudly instead of degrading/skipping, so the run doesn't go green by accident.

.. code-block:: bash

    python3 -m testing.runner --oc

Comparison Modes and Updating the Gold Files
--------------------------------------------

A case passes the regression comparison if every value of ``output.txt`` agrees with ``output_gold.txt`` within an absolute tolerance of 1e-8 or a relative tolerance of 1e-6, the column headers and table shapes are identical.

The ``--mode-gold`` argument selects what the runner does:

- ``0``: run the simulation and compare with the gold file (default)
- ``1``: run the simulation and overwrite the gold file with the new output (Use with caution!)
- ``2``: compare the existing ``output.txt`` with the gold file (no run)
- ``3``: overwrite the gold file with the existing ``output.txt`` (no run)

Updating the gold file means accepting the current output of SCIANTIX as the new baseline. It is only appropriate when a change in the results is intended (e.g. a model has been improved or corrected). In that case the change in each affected case should be understood and explained in the pull request, for validation cases by checking that the comparison with the experimental data has not worsened.

For OpenCalphad-dependent groups, modes ``1``/``3`` are refused if OC is unavailable, so a non-OC build can't overwrite gold values.

Test Case Structure
-------------------

A case is a directory containing the SCIANTIX input files and the gold file:

- **input_settings.txt**: model options (see the comments in the file).
- **input_history.txt**: time-dependent boundary conditions: time (h), temperature (K), fission rate (fiss/m3/s), hydrostatic stress (MPa). Some cases (oxidation, VERCORS) add a fifth column, the oxidising gas partial pressure used by the stoichiometry-deviation model.
- **input_initial_conditions.txt**: initial values of the state variables.
- **input_scaling_factors.txt**: scaling factors on model parameters, if needed.
- **output_gold.txt**: regression baseline (see above).

Validation cases also hold the experimental data (in the case folder, or in the ``data/`` folder of the group). OpenCalphad-coupled cases add ``input_thermochemistry*.txt`` files. The ``mox-po2`` group is driven by its own script rather than by scanning folders.

Available Test Suites
---------------------

Each suite is described by the **problem** it represents, how it is **modelled** in SCIANTIX, **what the test checks**, and the **reference** it is compared with. Every group also runs the regression comparison with its gold file.

Verification
~~~~~~~~~~~~

openPorosity (``--openPorosity``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** Evolution of the fuel porosity under the Baker (1977) 1273 K conditions: densification of the as-fabricated porosity and venting of grain-boundary gas through interconnected porosity.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* One case: constant 1273 K and 1e19 fiss/m3/s for 5500 h, with 5 um grains and 3% enrichment.
- *Models and options under test:* (i) fuel densification (``iDensification = 1``) and (ii) athermal grain-boundary venting (``iGrainBoundaryVenting = 3``), both from :ref:`Pagani et al. (2026) <ref-pagani2026>`.

**Test purpose.** ``sciantix_plot.py`` draws the densification factor, the fission gas release and the venting probability.

**Reference.** Gold only.

powerPulse (``--powerPulse``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** A fast power transient: a short pulse in which temperature and fission rate rise over several orders of magnitude.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* One case: 30000 h at 1000 K and 1e19 fiss/m3/s, then 40 s of pulse, in which the temperature reaches 2500 K and the fission rate 1.2e23 fiss/m3/s.
- *Models and options under test:* standard intragranular and grain-boundary models (including grain-boundary micro-cracking, :ref:`Barani et al. (2017) <ref-barani2017>`) under a fast transient.

**Test purpose.** The solver and the time-stepping remain stable and reproducible when the conditions change by orders of magnitude in a few time steps.

**Reference.** Gold only.

oxidation (``--oxidation``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** Deviation from stoichiometry (O/U ratio, ``x`` in UO\ :sub:`2+x`) of UO2 oxidised in steam/air at constant temperature.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* Four cases without irradiation (zero fission rate): Cox et al. at 1273 K and 1473 K, Imamura and Une, and PS-623C at 1623 K. The oxidising gas partial pressure is the fifth column of the history.
- *Models and options under test:* Langmuir-based approach of :ref:`Massih (2018) <ref-massih2018>` (``iStoichiometryDeviation = 5``), in which the deviation approaches its equilibrium value at a rate that depends on temperature and on the oxidising gas partial pressure. The fission gas models are not exercised, since there are no fissions.

**Test purpose.** The kinetics of the stoichiometry deviation. Each case folder holds the measured time/deviation curve (``data.txt``), compared with the calculation by ``plot.py``.

**Reference.** (i) Gold; (ii) Experimental curves from :ref:`Cox et al. (1986) <ref-cox1986>` and :ref:`Imamura and Une (1997) <ref-imamura1997>`. The correlations of :ref:`Bittel et al. (1969) <ref-bittel1969>` and :ref:`Abrefah et al. (1994) <ref-abrefah1994>` are available as alternative model options. These cases sit under ``verification/`` because they test one single model implementation and behaviour, but the data are experimental: they could also be read as validation.

vercors (``--vercors``)
^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** Fission gas release in a severe-accident test: base irradiation followed by heating to very high temperature, as in the VERCORS-5 campaign.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* One case with a 16-point history from 673 K to 2573 K.
- *Models and options under test:* stoichiometry deviation (``iStoichiometryDeviation = 6``, :ref:`Massih (2018) <ref-massih2018>`), fission product diffusivity that depends on the stoichiometry deviation (``iFissionProductDiffusivity = 6``), bubble diffusivity (``iBubbleDiffusivity = 1``), grain-boundary micro-cracking, and the diffusion solver without the quasi-stationary hypothesis (``iDiffusionSolver = 2``).

**Test purpose.** The coupled release and stoichiometry models run through a long, multi-stage, high-temperature history.

**Reference.** Gold only since the experimental VERCORS-5 data are not in the repository.

gpr (``--gpr``)
^^^^^^^^^^^^^^^

**Original problem.** SCIANTIX can use correlations that have been *updated with Gaussian Process Regression* (GPR) against experimental data, together with their uncertainty. The xenon diffusion coefficient is the example. The GPR is run outside of SCIANTIX (``GPregression/MainRegression.py``) and writes two tables, the updated ``log10 D`` vs temperature and its standard deviation, in ``GPregression/Results/``.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* Nine cases from 1273 K to 2073 K with the same history as the Baker cases (constant temperature, 1e19 fiss/m3/s, 5500 h).
- *Models and options under test:* the only difference from Baker is ``iFissionProductDiffusivity = 90``. Instead of the :ref:`Turnbull et al. (1988) <ref-turnbull1988>` correlation (option 1), SCIANTIX reads the GPR tables (``src/operations/SetGPVariables.C``) and uses them at the case temperature.

**Test purpose.** The *integration* of the GPR output into the code: the tables are found and read, the updated diffusivity and its uncertainty reach the intragranular model, and the resulting bubble density, radius and swelling do not change unintentionally. ``parity_plot.py`` plots the calculation against the Baker data with and without the GPR update (``ig_swelling_no_gpr.txt``), to judge whether the update improves the prediction.

**Reference.** (i) Gold for pass/fail; (ii) Experimental data from :ref:`Baker (1977) <ref-baker1977>` for the parity plots.

mox-po2 (``--mox-po2``)
^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** Oxygen potential of MOX fuel as a function of the O/M ratio, temperature and Pu content.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* 24 cases, ``T_<T>K_q_<Pu>``, one for each pair of temperature (1000-2400 K) and Pu content (q = 10, 20, 30 %). Each case sweeps O/M linearly in time (``iStoichiometryDeviation = 8``) at fixed temperature, in an MOX matrix (``iFuelMatrix = 2``).
- Comparison domain: 753-2550 K, Pu/M = 0.10-0.32, O/M = 1.92-2.08.
- *Models and options under test:* the Kato analytic correlation (:ref:`Kato et al. (2017) <ref-kato2017>`, as implemented from :ref:`NEA/NSC/R(2024)1 <ref-nea2025>`, Eqs. 8.4-8.5) and the OpenCalphad coupling with the database ``upuo-v21.TDB`` (``iThermochemistry = 2``).

**Test purpose.** Numerical accuracy of the two paths against independent references:
- Kato path: maximum error below 0.05 kJ/mol (pure numerics: bisection and interpolation).
- OpenCalphad path: mean error below 2 kJ/mol per (q, T) group, for T >= 1000 K. Skipped, with a warning, if OpenCalphad or the database is not available.

Details in ``verification/test_MOX_po2/README.md``.

**Reference.** Two independent, non-SCIANTIX references: the explicit Kato equation (code-to-analytical) and Thermo-Calc tables (code-to-code).

Method of manufactured solutions suite (``verification/Operahpc_5.1``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** Show that the numerical schemes in ``Solver.C`` and the models built on them solve their equations correctly, including the aspects that matter when SCIANTIX is coupled to a fuel performance code (OFFBEAT).

**Modelling in SCIANTIX.**

- *Setup and simplifications:* Method of manufactured solutions (MMS): for each case a solution ``c_M`` is chosen, the source term that makes it an exact solution is derived analytically, and the numerical scheme (a Python transcription of the C++ discretisation) is run with that source.
- *Models and options under test:* 13 cases covering the spectral diffusion solver (01-03, 05), grain-boundary release (04), coupled precursor/daughter diffusion (06), fission gas release (07), restart (08), outer-iteration relaxation (09), stress-coupled grain-boundary bubble growth (10), grain growth (11), bubble coalescence (12) and the oxygen-to-metal ratio (13).

**Test purpose.** The L1, L2, Linf and final-value errors between the numerical solution ``c_N`` and ``c_M``, the observed order of convergence in time and space, and the cost scaling. Case 11 exposed a real defect in the shipped solver (see ``README.md``). ``VERIFICATION_TAXONOMY.md`` classifies what kind of defect each case can detect.

**Reference.** Manufactured (exact) solutions: this is the true verification of the code. The suite follows the verification approach of :ref:`OperaHPC D5.1 <ref-operahpc2025>`, Section 4.3. 

**Status:** it runs with its own ``run_case.py`` scripts and is part of ``testing.runner`` or CTest, so it does not run automatically. See ``README.md`` for the case table and ``METHODOLOGY.md`` for how a case is built.

Validation
~~~~~~~~~~

In every validation group the pass/fail check is the comparison with the gold file. The experimental data are compared by the plotting script of the group, and ``parity_by_topic.py`` merges the groups into one parity plot per phenomenon.

baker (``--baker``)
^^^^^^^^^^^^^^^^^^^

**Original problem.** Intragranular fission gas bubbles in UO2 irradiated at constant temperature. Baker (1977) measured the bubble concentration, radius and the resulting swelling in fuel pins at temperatures from about 1273 K to 2073 K.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* Nine cases, one per temperature: constant temperature and 1e19 fiss/m3/s for 5500 h, 5 um grain radius, 3% enrichment.
- *Models and options under test:* the standard intragranular model (:ref:`Pizzocri et al. (2018) <ref-pizzocri2018>`) with the Turnbull diffusivity, the Ham trapping rate, the Turnbull re-solution and the Olander-Wongsawaeng nucleation; grain growth of :ref:`Ainscough et al. (1973) <ref-ainscough1973>`.

**Test purpose.** The intragranular bubble model (nucleation, trapping, re-solution) against the three measured quantities over the whole temperature range (``parity_plot.py``).

**Reference.** Experimental: :ref:`Baker (1977) <ref-baker1977>`.

cornell (``--cornell``)
^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** Intragranular bubbles: :ref:`Cornell (1969) <ref-cornell1969>` measured bubble concentration and radius in irradiated UO2 as a function of temperature (1133 K to 1853 K).

**Modelling in SCIANTIX.**

- *Setup and simplifications:* Nine cases with a constant temperature and fission rate (9.26e18 fiss/m3/s) for 960 h. The measured values are compared at the end of the irradiation.
- *Models and options under test:* the same standard intragranular model as Baker, with ``iDiffusionSolver = 1``.

**Test purpose.** The nucleation and re-solution models against bubbles of about 1 nm radius, a second, independent data set for the same model as Baker.

**Reference.** Experimental: :ref:`Cornell (1969) <ref-cornell1969>`, data in ``cornell/data/ig_density.txt`` and ``ig_radius.txt``.

white (``--white``)
^^^^^^^^^^^^^^^^^^^

**Original problem.** Intergranular (grain-face) swelling of UO2 during fast thermal ramps. White (2004) reports the swelling of grain-face bubbles in about 40 irradiated and then ramped specimens.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* 43 cases (``test_White2004_<id>-<n>``), each with the thermal history of one specimen (temperature from 573 K up to 1976-2048 K, 7-8 history points).
- White tabulates the swelling as ``(3/2a) sum(V_i)/A_gf`` while SCIANTIX computes ``(3/a) N V``: the measured values are therefore doubled in the comparison. The raw values are kept in ``data/ig_swelling.txt``, as published.
- *Models and options under test:* grain-boundary bubbles and micro-cracking (``iGrainBoundaryMicroCracking = 2``, ``iReleaseMode = 1``) with the vacancy diffusivity of :ref:`White (2004) <ref-white2004>`.

**Test purpose.** The grain-boundary bubble growth and coalescence model for fast ramps. ``parity_plot.py`` gives the parity plot, ``figure_ramp_groups.py`` splits it by ramp type (fast vs slow, as in :ref:`Cappellari et al. (2025) <ref-cappellari2025>`), and ``bias.py`` runs a grid sweep of scaling factors and reports bias, RMSE and MAD (a sensitivity analysis, which re-runs the cases and restores them afterwards).

**Reference.** (i) Experimental: :ref:`White (2004) <ref-white2004>`. The swelling data come from the AGR/Halden ramp test programme (:ref:`White et al. (2006) <ref-white2006>`, OECD/NEA IFPE/CAGR-UOX-SWELL, NEA-1705). (ii) ``data/ig_swelling_sciantix20.txt`` holds the swelling calculated by SCIANTIX 2.0, for comparison.

kashibe (``--kashibe``)
^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** Post-irradiation annealing of UO2: fuel irradiated to a given burnup is cooled down and then heated to 1673-2073 K, and the fission gas release during the anneal and the bubble populations are measured (:ref:`Une and Kashibe, 1990 <ref-une1990>`; :ref:`Kashibe and Une, 1991 <ref-kashibe1991>`; :ref:`Kashibe et al., 1993 <ref-kashibe1993>`).

**Modelling in SCIANTIX.**

- *Setup and simplifications:* 23 cases. The 1990 and 1991 histories have three stages: base irradiation (1e19 fiss/m3/s), cooldown and anneal; the "Multiple" cases of 1991 have a cyclic anneal. The two 1993 cases are irradiation only (1073 K, about 7e18 fiss/m3/s).
- The measured release is the *increase* during the anneal, so for the annealed cases the comparison is made on the difference between the end of the anneal and its start.
- *Models and options under test:* intragranular and intergranular bubble models with micro-cracking (``iGrainBoundaryMicroCracking = 2``), as for White.

**Test purpose.** The bubble models and the release during anneals, against the fission gas release (``data/fgr.txt``), the intergranular swelling and the intragranular bubble density and radius (``data/``).

**Reference.** Experimental: :ref:`Une and Kashibe (1990) <ref-une1990>`, :ref:`Kashibe and Une (1991) <ref-kashibe1991>` and :ref:`Kashibe et al. (1993) <ref-kashibe1993>`, as processed in :ref:`Cappellari et al. (2025) <ref-cappellari2025>`.

talip (``--talip``)
^^^^^^^^^^^^^^^^^^^

**Original problem.** Thermal release of helium from 238Pu-doped UO2: samples with built-in helium are heated on a temperature ramp and the helium release and release rate are measured (:ref:`Talip et al. (2014) <ref-talip2014>`).

**Modelling in SCIANTIX.**

- *Setup and simplifications:* Five cases named after the annealing temperature (1320 K, 1400 K twice, 1600 K, 1800 K); the history of each is the measured heating ramp. No irradiation (zero fission rate), and the initial helium content is given in ``input_initial_conditions.txt``.
- *Models and options under test:* helium behaviour (``iHelium = 1``) with ``iHeDiffusivity = 1`` (limited lattice damage, :ref:`Luzzi et al. (2018) <ref-luzzi2018>`), thermal re-solution of :ref:`Cognini et al. (2021) <ref-cognini2021>`, grain boundary sweeping, and grain growth of :ref:`Van Uffelen et al. (2013) <ref-vanuffelen2013>`.

**Test purpose.** Helium diffusion, trapping in bubbles and thermal re-solution during annealing, against the release (``Talip2014_release_data.txt``) and the release rate (``Talip2014_rrate_data.txt``). ``output_Cognini.txt`` holds the calculation of the previous model, for comparison.

**Reference.** Experimental: :ref:`Talip et al. (2014) <ref-talip2014>`.

hbs (``--hbs``)
^^^^^^^^^^^^^^^

**Original problem.** The high-burnup structure (HBS): at high burnup the grains subdivide into sub-micron grains and a population of large pores forms.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* One case at 723 K and 2e19 fiss/m3/s, long enough to reach high burnup.
- *Models and options under test:* HBS formation and porosity evolution (``iFuelMatrix = 1``, ``iHighBurnupStructureFormation = 1``, ``iHighBurnupStructurePorosity = 1``; :ref:`Barani et al. (2020) <ref-barani2020>`, :ref:`(2022) <ref-barani2022>`; the porosity model is based on the data of :ref:`Spino et al. (2006) <ref-spino2006>`).

**Test purpose.** The restructured fraction, pore density, pore radius and porosity as a function of burnup, against measurements (``exp_pore_density.txt``, ``exp_pore_radius.txt``, ``exp_porosity.txt``), plus the swelling data of :ref:`Spino et al. (2006) <ref-spino2006>` and the Xe retention data of :ref:`Walker (1999) <ref-walker1999>`.

**Reference.** Experimental: :ref:`Spino et al. (2006) <ref-spino2006>` (pore data and swelling data, which are also the basis of the porosity model) and :ref:`Walker (1999) <ref-walker1999>` (Xe retention), in the case folder.

chromium (``--chromium``)
^^^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** Chromia-doped UO2, which has larger grains and a different gas release than undoped fuel because chromium changes the diffusivity and is only soluble up to a temperature-dependent limit.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* Three cases. ``Killeen``: 1700 K, 9.2e18 fiss/m3/s, 3500 h, no grain growth. ``solubility1`` and ``solubility2``: temperature histories (600-2500 K).
- *Models and options under test:* the diffusivity for Cr-doped fuel (``iFissionProductDiffusivity = 8``, :ref:`Nicodemo et al. (2024) <ref-nicodemo2024>`) in ``Killeen``, compared with the measured fission gas release as a function of FIMA; the chromium solubility model (``iChromiumSolubility``) in the solubility cases, compared with the chromium content in solution of :ref:`Riglet-Martial et al. (2014) <ref-rigletmartial2014>`, which is also the source of the solubility model.

**Test purpose.** The two chromium-related models: enhanced diffusion and solubility limit.

**Reference.** Experimental: :ref:`Killeen (1980) <ref-killeen1980>` (``Killeen_exp.txt``) and :ref:`Riglet-Martial et al. (2014) <ref-rigletmartial2014>` (``Riglet-Martial_data_exp*.txt``).

contact (``--contact``)
^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** Release of short-lived radioactive gases (Kr-85m, Xe-133) from a LWR rod during base irradiation with power changes, as measured in the CONTACT 1 experiment, expressed as release-to-birth ratio.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* One case with a 128-point history (324-1342 K, 3.6e17-2.5e19 fiss/m3/s over about 7600 h). The history is that of one location of the rod, not of the whole rod.
- *Models and options under test:* radioactive gas behaviour (``iRadioactiveFissionGas = 1``), so that Kr-85m and Xe-133 are tracked separately with their decay (model of :ref:`Zullo et al. (2022) Part I <ref-zullo2022a>`; the coupling with TRANSURANUS is described in :ref:`Part II <ref-zullo2022b>`).

**Test purpose.** The release of Kr-85m and Xe-133 and the total release against burnup (``experimental_RB_Kr85m.txt``, ``experimental_RB_Xe133.txt``, ``experimental_fgr.txt``). The ANS 5.4-2010 results of the same case (``ANS54-2010-*.txt``) are plotted as a comparison with a standard model (:ref:`Beyer and Turnbull (2010) <ref-turnbull2010>`).

**Reference.** Experimental: CONTACT 1 measurements in the case folder, the same data used to assess the model in :ref:`Zullo et al. (2022) Part I <ref-zullo2022a>` and :ref:`Part II <ref-zullo2022b>`.

jog (``--jog``)
^^^^^^^^^^^^^^^

**Original problem.** In fast-reactor MOX fuel, fission products accumulate in the fuel-cladding gap as a fission-product-rich layer, the *joint oxyde-gaine* (JOG). Its composition depends on the thermochemistry of the fuel, which is why the group needs OpenCalphad.

**Modelling in SCIANTIX.**

- *Setup and simplifications:* Four cases, ``test_PHENIXpins_point_01..04``, at four radial positions of a PHENIX pin (r = 1.04 to 2.48 mm). The input histories and the caesium production are generated beforehand with the OXIRED and CSRED preprocessing packages (``PHENIXpins/generate_inputs.py``, run by hand, not by the suite).
- *Models and options under test:* the OpenCalphad coupling, which computes the phases during the run (the same approach as :ref:`Samuelsson et al. (2020) <ref-samuelsson2020b>`, who couple GERMINAL V2 with Calphad calculations). These cases carry extra thermochemistry inputs and a second gold file, ``thermochemistry_output_gold.txt``.

**Test purpose.** The predicted JOG phases and composition (``PHENIXpins/plot_JOG.py``) against the experimental data in ``PHENIXpins/exp_data``. The whole group is skipped, with a warning, when OpenCalphad is not available, because there is no OC-independent check of what it measures.

**Reference.** (i) Experimental data in ``exp_data``: JOG thickness measurements of :ref:`Tourasse et al. (1992) <ref-tourasse1992>` and :ref:`Melis et al. (1993) <ref-melis1993>` (compiled in Fig. 1 of :ref:`Samuelsson et al. (2020) <ref-samuelsson2020b>`) and of the CABRI JOG tests of :ref:`Melis et al. (1993b) <ref-melis1993b>`; the measured composition of the metallic precipitate (Table 3 of :ref:`Samuelsson et al. (2020) <ref-samuelsson2020a>`); the composition of the simulated JOG studied by :ref:`Oulfarsi et al. (2024) <ref-oulfarsi2024>`; the metallic precipitates measured by :ref:`Fayette et al. (2026) <ref-fayette2026>`; and the oxygen potentials of :ref:`Matzke et al. (1988) <ref-matzke1988>`. (ii) Calculated results kept for comparison (not experimental): the GERMINAL and OpenCalphad + TAF-ID JOG thicknesses of :ref:`Samuelsson et al. (2020) <ref-samuelsson2020b>` (``Samuellson2020_simulation.txt``).

oxygenpotential-freshfuel / -burnup (``--oxygenpotential-freshfuel``, ``--oxygenpotential-burnup``)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Original problem.** The oxygen potential measured in MOX as a function of O/M, temperature and Pu content, both in unirradiated fuel (23 sources) and in irradiated or simulated high-burnup fuel (8 sources).

**Modelling in SCIANTIX.**

- *Setup and simplifications:* One case per data set (``test_<Source>``). The two groups differ only in the data set.
- *Models and options under test:* the same two paths as ``mox-po2`` (Kato correlation and OpenCalphad). Each ``output.txt`` carries both sets of columns. Without OpenCalphad the CALPHAD columns are zero and excluded from the comparison, with a warning.

**Test purpose.** The oxygen potential of the code against the measurements (``plot.py`` in each group, ``combined_parity_plot.py`` for the fresh/irradiated and Kato/OC parity figure, 323 cases in total). In contrast to ``mox-po2``, the reference is experimental, not the correlation itself.

**Reference.** Experimental data digitized from the original sources compiled in the NEA/NSC/R(2024)1 review (:ref:`NEA 2025 <ref-nea2025>`). Details in ``validation/oxygenpotential/README.md``.
