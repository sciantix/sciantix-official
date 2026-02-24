Grain Boundary Sweeping
=======================

This model represents the “sweeping” of intragranular gas towards the grain
boundary caused by grain growth. When the grain radius increases, a thin shell
of material is incorporated into the grain boundary region; the gas contained in
that swept volume is removed from the intragranular inventory.

The implementation follows ``Simulation::GrainBoundarySweeping()``.

Activation and options
----------------------

The model is enabled by:

- ``iGrainBoundarySweeping`` (input option)

If ``iGrainBoundarySweeping = 0``, the model returns immediately and no sweeping
is applied.

Inputs
------

- ``iGrainBoundarySweeping`` (input option)
- ``Grain radius`` (sciantix variable)
- ``iDiffusionSolver`` (to determine which modes are affected)
- modal coefficients / intragranular concentration representation (``modes_initial_conditions``)

Outputs
-------

- Updated intragranular modal coefficients or concentrations after sweeping
- ``Swept fraction`` or ``f_sweep`` stored as a model parameter

Swept volume fraction
---------------------

For ``iGrainBoundarySweeping = 1``, the model uses the classical relation for the
increment of grain volume (spherical approximation):

.. math::

   \frac{\Delta V}{V} \approx 3\,\frac{\Delta r}{r}

In the code, the swept fraction is stored as a single parameter:

.. math::

   f_{\mathrm{sweep}} = 3 \frac{\Delta r}{r^{n+1}}

where:

- :math:`\Delta r` is ``Grain radius`` increment,
- :math:`r^{n+1}` is ``Grain radius`` final value.

Reference: “TRANSURANUS model”.

Effect on intragranular gas representation
------------------------------------------

SCIANTIX represents the intragranular gas field using modal coefficients
(``modes_initial_conditions``). Sweeping acts as a proportional removal of the
intragranular concentration:

.. math::

   \frac{\Delta C}{C} = - f_{\mathrm{sweep}}

which corresponds to an exponential decay update of each affected mode:

.. math::

   C^{n+1} = C^{n}\,\exp\!\left(-f_{\mathrm{sweep}}\right)

In the implementation this is applied via:

- ``solver.Decay(value, 1.0, 0.0, f_sweep)``

and the set of modes updated depends on the diffusion solver selection:

- ``iDiffusionSolver = 1``: updates the mode block starting at ``6*40`` (one equation set).
- ``iDiffusionSolver = 2``: updates the mode blocks starting at ``7*40`` and ``8*40`` (two equation sets).
- ``iDiffusionSolver = 3``: no sweeping update is applied in this routine.
