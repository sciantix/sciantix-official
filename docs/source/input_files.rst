Input Files
===========

A SCIANTIX case is a folder holding up to four plain-text input files. The code is run on the folder (``./sciantix.x <case_dir>/``) and writes its results into the same folder. A complete worked case is given in the :doc:`examples` tutorial; every case of ``verification/`` and ``validation/`` is a further template (see :doc:`testing`).

.. list-table::
   :header-rows: 1
   :widths: 30 15 55

   * - File
     - Required
     - Content
   * - ``input_settings.txt``
     - yes
     - Specifies the models and numerical solvers used in the simulation.
   * - ``input_history.txt``
     - yes
     - Includes the time (h), temperature (K), fission rate (fiss/m³-s), and hydrostatic stress (MPa) as a function of time.
   * - ``input_initial_conditions.txt``
     - yes
     - Sets initial conditions for the simulation.
   * - ``input_scaling_factors.txt``
     - optional
     - Multiplicative factors applied to selected model parameters

``input_settings.txt``, ``input_initial_conditions.txt`` and ``input_scaling_factors.txt`` share one format. Every entry is a single line:

.. code-block:: text

    <value(s)>    #    <Key> (<description>)

SCIANTIX looks each entry up by its **key**, the first word after the ``#``. Therefore:

- the order of the lines does not matter, and the description in brackets is free text for the code user (it is not parsed);
- blank lines and lines holding only a comment are ignored;
- a line with a value but no key, a key given twice, or a key the code does not know stops the run with an error;
- an entry that is left out takes its default: 0 in ``input_settings.txt`` and ``input_initial_conditions.txt``, 1.0 in ``input_scaling_factors.txt``.

.. note::

   Earlier versions wrote the value and the ``#`` description on separate lines and used some setting names that are no longer accepted (e.g. ``iFissionGasDiffusivity``). Such files can be rewritten in the keyed format, with unchanged results, with
   ``python3 utilities/inputExample/convert_to_named_inputs.py <case_dir> [<case_dir> ...]``.

.. _input-settings:

input_settings.txt
------------------

The file contains one integer flag per model. Each flag selects a model option: ``0`` generally switches the model off (or uses a constant value), higher integers select a correlation or a variant. The table lists the flags, their options and the page of the documentation that describes the selected model.

.. list-table::
   :header-rows: 1
   :widths: 27 43 30

   * - Flag
     - Options
     - Model documentation
   * - ``iGrainGrowth``
     - 0 = no grain growth; 1 = :ref:`Ainscough et al. (1973) <ref-ainscough1973>`; 2 = :ref:`Van Uffelen et al. (2013) <ref-vanuffelen2013>`
     - :doc:`models/grain_growth`
   * - ``iFissionProductDiffusivity``
     - 0 = constant value; 1 = :ref:`Turnbull et al. (1988) <ref-turnbull1988>`
     - :doc:`models/gas_diffusion`
   * - ``iDiffusionSolver``
     - 1 = spectral diffusion solver (SDA) with quasi-stationary hypothesis; 2 = SDA without quasi-stationary hypothesis
     - :doc:`models/gas_diffusion`, :doc:`solvers`
   * - ``iIntraGranularBubbleBehavior``
     - 1 = :ref:`Pizzocri et al. (2018) <ref-pizzocri2018>`
     - :doc:`models/intragranular_bubble_behavior`
   * - ``iResolutionRate``
     - 0 = constant value; 1 = :ref:`Turnbull (1971) <ref-turnbull1971>`; 2 = Losonen (2000); 3 = thermal resolution, :ref:`Cognini et al. (2021) <ref-cognini2021>`
     - :doc:`models/intragranular_bubble_behavior`
   * - ``iTrappingRate``
     - 0 = constant value; 1 = :ref:`Ham (1958) <ref-ham1958>`
     - :doc:`models/intragranular_bubble_behavior`
   * - ``iNucleationRate``
     - 0 = constant value; 1 = :ref:`Olander and Wongsawaeng (2006) <ref-olander2006>`
     - :doc:`models/intragranular_bubble_behavior`
   * - ``iOutput``
     - 1 = default output files
     - :ref:`output-files`
   * - ``iGrainBoundaryVacancyDiffusivity``
     - 0 = constant value; 1 = :ref:`Reynolds and Burton (1979) <ref-reynolds1979>`; 2 = :ref:`White (2004) <ref-white2004>`
     - :doc:`models/intergranular_bubble_behavior`
   * - ``iGrainBoundaryBehaviour``
     - 0 = no grain-boundary bubbles; 1 = :ref:`Pastore et al. (2013) <ref-pastore2013>`
     - :doc:`models/intergranular_bubble_behavior`
   * - ``iReleaseMode``
     - 0 = coalescence by :ref:`White (2004) <ref-white2004>`, saturation threshold of the fractional coverage by :ref:`Pastore et al. (2013) <ref-pastore2013>`; 1 = coalescence by :ref:`Pastore et al. (2013) <ref-pastore2013>`, :ref:`Cappellari et al. (2025) <ref-cappellari2025>`
     - :doc:`models/intergranular_bubble_behavior`, :doc:`models/grain_boundary_microcracking`
   * - ``iGrainBoundaryMicroCracking``
     - 0 = no model; 1 = :ref:`Barani et al. (2017) <ref-barani2017>`; 2 = :ref:`Cappellari et al. (2025) <ref-cappellari2025>`
     - :doc:`models/grain_boundary_microcracking`
   * - ``iGrainBoundaryVenting``
     - 0 = no model; 1 = Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE; 2 = :ref:`Claisse and Van Uffelen (2015) <ref-claisse2015>`; 3 = :ref:`Pagani et al. (2026) <ref-pagani2026>`
     - :doc:`models/grain_boundary_venting`
   * - ``iGrainBoundarySweeping``
     - 0 = no model; 1 = TRANSURANUS swept-volume model
     - :doc:`models/grain_boundary_sweeping`
   * - ``iFuelMatrix``
     - 0 = UO\ :sub:`2`; 1 = UO\ :sub:`2` with HBS; 2 = MOX
     - :doc:`models/microstructure`
   * - ``iHighBurnupStructureFormation``
     - 0 = no model; 1 = fraction of HBS-restructured volume, :ref:`Barani et al. (2020) <ref-barani2020>`
     - :doc:`models/high_burnup_structure_formation`
   * - ``iHighBurnupStructurePorosity``
     - 0 = no HBS porosity evolution; 1 = HBS porosity evolution based on :ref:`Spino et al. (2006) <ref-spino2006>`
     - :doc:`models/high_burnup_structure_porosity`
   * - ``iRadioactiveFissionGas``
     - 0 = not considered; 1 = radioactive Xe-133 and Kr-85m tracked with decay
     - :doc:`models/gas_decay`
   * - ``iHelium``
     - 0 = not considered; non-zero = helium behaviour (also activates the helium outputs)
     - :doc:`models/gas_production`, :doc:`models/gas_diffusion`
   * - ``iHeDiffusivity``
     - 0 = null value; 1 = limited lattice damage, :ref:`Luzzi et al. (2018) <ref-luzzi2018>`; 2 = significant lattice damage, :ref:`Luzzi et al. (2018) <ref-luzzi2018>`
     - :doc:`models/gas_diffusion`
   * - ``iHeliumProductionRate``
     - 0 = zero production rate; 1 = helium from ternary fissions; 2 = linear with burnup
     - :doc:`models/gas_production`
   * - ``iBubbleDiffusivity``
     - 0 = not considered; 1 = volume diffusivity
     - :doc:`models/intragranular_bubble_behavior`
   * - ``iStoichiometryDeviation``
     - 0 = not considered; 1 = :ref:`Cox et al. (1986) <ref-cox1986>`; 2 = :ref:`Bittel et al. (1969) <ref-bittel1969>`; 3 = :ref:`Abrefah et al. (1994) <ref-abrefah1994>`; 4 = :ref:`Imamura et al. (1997) <ref-imamura1997>`; 5 = Langmuir-based approach; 8 = prescribed O/M history. Further options are described in the model page.
     - :doc:`models/stoichiometry_deviation`, :doc:`models/uo2_thermochemistry`, :doc:`models/gap_partial_pressure`
   * - ``iChromiumSolubility``
     - 0 = parameter set of :ref:`Riglet-Martial et al. (2014) <ref-rigletmartial2014>`; 1 = optimised coefficients. Values above 0 also activate the chromium outputs
     - :doc:`models/chromium_solubility`
   * - ``iDensification``
     - 0 = not considered; 1 = fit from :ref:`Van Uffelen (2002) <ref-vanuffelen2002>`
     - :doc:`models/densification`

Remarks:

- Some validation cases use options that are not in the table (for instance ``iFissionProductDiffusivity = 6`` and ``90``, ``iStoichiometryDeviation = 6``). They are explained, together with the case that uses them, in :doc:`testing`.
- ``Number_of_time_steps_per_interval`` is an optional integer entry of ``input_settings.txt`` that sets the number of time steps between two rows of ``input_history.txt`` (see below). Absent, the default in ``src/MainVariables.C`` (100) applies.

input_history.txt
-----------------

Input history defines the conditions imposed to the simulation in terms of duration of the simulated history (in hours, first column), local temperature (in K, second column), local fission rate density (in fission per cubic meter per second, third column), and local hydrostatic stress (in MPa, fourth column).
It is a table without header, one row per history point, with four tab- or space-separated columns:

.. code-block:: text

    0       1273    1e19    0
    5500    1273    1e19    0

Between two rows the conditions vary linearly and the interval is divided into a fixed number of time steps (``Number_of_time_steps_per_interval``, defined in `input_settings.txt`).

input_initial_conditions.txt
----------------------------

The file sets the initial state of the grain. An entry left out takes the value 0. Each entry if written must hold exactly the number of values in the table below, otherwise the run stops with an error.

.. code-block:: text

    5.0e-06                         # Grain_radius[0] (initial grain radius (m))
    0.0 0.0 0.0 0.0 0.0 0.0         # Initial_composition_Xe (initial Xe (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released)
    0.0 0.0 0.0 0.0 0.0 0.0         # Initial_composition_Kr (initial Kr (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released)
    0.0 0.0 0.0 0.0 0.0 0.0         # Initial_composition_He (initial He (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released)
    0.0 0.0                         # Initial_intragranular_bubbles (initial intragranular bubble concentration (bub/m3), radius (m))
    0.0                             # Burn_up[0] (initial fuel burn-up (MWd/kgUO2))
    0.0                             # Effective_burn_up[0] (initial fuel effective burn-up (MWd/kgUO2))
    0.0                             # Irradiation_time[0] (initial irradiation time (h))
    10641.0                         # Fuel_density[0] (initial fuel density (kg/m3))
    0.0 3.0 0.0 0.0 97.0            # Initial_composition_U (initial U234 U235 U236 U237 U238 content (% of heavy atoms))
    0.0 0.0 0.0 0.0 0.0 0.0 0.0     # Initial_composition_Xe133 (initial Xe133 (at/m3): produced, intragranular, in solution, in bubbles, decayed, grain boundary, released)
    0.0 0.0 0.0 0.0 0.0 0.0 0.0     # Initial_composition_Kr85m (initial Kr85m (at/m3): produced, intragranular, in solution, in bubbles, decayed, grain boundary, released)
    0.0                             # Initial_stoichiometry_deviation[0] (initial fuel stoichiometry deviation (/))


Optional model parameters
~~~~~~~~~~~~~~~~~~~~~~~~~

Two model parameters can also be set per case in this file; when the entry is absent the value in round brackets is used.

.. code-block:: text

    2.5e13                          # Intergranular_bubble_concentration[0] (default = 2.0e13)
    0.6                             # Surface_tension (default UO2 and HBS = 0.7, MOX = 0.626 )


input_scaling_factors.txt
-------------------------

This optional file multiplies selected model parameters, for sensitivity or uncertainty studies without editing the code. A factor left out defaults to 1.0.

.. code-block:: text

    1.0    # sf_resolution_rate (scaling factor - resolution rate)
    1.0    # sf_trapping_rate (scaling factor - trapping rate)
    1.0    # sf_nucleation_rate (scaling factor - nucleation rate)
    1.0    # sf_diffusivity (scaling factor - diffusivity)
    1.0    # sf_temperature (scaling factor - temperature)
    1.0    # sf_fission_rate (scaling factor - fission rate)
    1.0    # sf_diffusion_based_release (scaling factor - diffusion-based release)
    1.0    # sf_helium_production_rate (scaling factor - helium production rate)
    1.0    # sf_grain_boundary_energy (scaling factor - grain-boundary energy)
    1.0    # sf_fabricated_porosity (scaling factor - fabricated porosity)
    1.0    # sf_cs_production (scaling factor - Cs production)

.. _output-files:

Output files
------------

Besides ``output.txt``, a run writes into the case folder ``input_check.txt`` (the flags and the values actually read, to check that the inputs were interpreted as intended), ``overview.txt`` (the model and reference selected for each phenomenon) and ``execution.txt`` (run information).

If you experience any issues with these files, please contact the main developers (D. Pizzocri, G. Zullo) for support.
