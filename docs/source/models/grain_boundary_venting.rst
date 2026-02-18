Grain Boundary Venting
======================

This model represents venting of gas from grain-boundary bubbles to the free
volume (gap) once the grain boundary becomes sufficiently “open” (e.g. due to
microcracking). The venting strength is expressed through a **venting
probability**, which is then applied as a sink term to the grain-boundary gas
inventories.

The implementation follows ``Simulation::GrainBoundaryVenting()``.

Activation and option
---------------------

The model is enabled by:

- ``iGrainBoundaryVenting`` (input option)

If ``iGrainBoundaryVenting = 0``, the model returns immediately and no venting is
applied.

Inputs
------

- ``iGrainBoundaryVenting`` (input option)
- ``Intergranular fractional coverage``
- ``Intergranular fractional intactness``
- ``<Gas> at grain boundary`` for each gas system
- ``Time step`` (physics variable)

Outputs
-------

- ``P_vent`` (venting probability stored as a model parameter)
- Updated grain-boundary inventories (after applying the venting sink)

Key variables
-------------

The model uses:

- ``Intergranular fractional coverage``: initial fractional coverage at the grain boundary.
- ``Intergranular fractional intactness``: measure of how “intact” the grain boundary is
  (its increment and final value are used).
- ``Intergranular vented fraction``: sigmoid-based fraction of vented boundary.
- ``Intergranular venting probability``: effective venting probability used in the sink term.
- ``<Gas> at grain boundary`` for each gas system (Xe, Kr, He, radioactive gases, …).

Sigmoid-based vented fraction
-----------------------------

For ``iGrainBoundaryVenting = 1``, a sigmoid function is used to compute the
vented fraction. First, an internal sigmoid variable is defined as:

.. math::

   s = F^{n}\,\exp\!\left(-\Delta I\right)

where:

- :math:`F^{n}` is ``Intergranular fractional coverage`` (initial value),
- :math:`\Delta I` is the increment of ``Intergranular fractional intactness``.

Then the vented fraction is computed as:

.. math::

   f_{\mathrm{vent}} =
   \left(1 + a\,\exp\!\left(-b\,(s-c)\right)\right)^{-1/a}

with constants (as in the code):

- :math:`a = 0.1` (``screw_parameter``),
- :math:`b = 10.0` (``span_parameter``),
- :math:`c = 0.43` (``cent_parameter``).

Venting probability
-------------------

The effective venting probability is defined by mixing the intact and non-intact
fractions:

.. math::

   P_{\mathrm{vent}} =
   (1 - I^{n+1}) + I^{n+1}\,f_{\mathrm{vent}}

where :math:`I^{n+1}` is the final value of ``Intergranular fractional intactness``.

Reference: Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE.

Application to grain-boundary gas inventories
---------------------------------------------

The model stores:

- ``P_vent`` as the single model parameter.

Then, for **each** gas system, the grain-boundary inventory is updated by applying
a sink term proportional to the current amount at the grain boundary:

.. math::

   C_{\mathrm{gb}}^{n+1} =
   C_{\mathrm{gb}}^{n} - P_{\mathrm{vent}}\,C_{\mathrm{gb}}^{n}\,\Delta t

In the implementation this is done through:

- ``solver.Integrator( <Gas> at grain boundary , -P_vent , <Gas> at grain boundary increment )``

Finally, the grain-boundary variable is reset for the next step using
``resetValue()``.
