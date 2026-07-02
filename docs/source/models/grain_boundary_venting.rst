Grain Boundary Venting
======================

This model represents venting of gas from grain-boundary to the free
volume (gap). 
This is modeled either via a fractional coverage-driven sigmoid representing the interconnection of bubbles or via correlations linked to the fuel's open porosity. The venting strength is expressed through a **venting probability**, applied as a sink term to the grain-boundary gas inventories.

The implementation follows ``Simulation::GrainBoundaryVenting()``.

Activation and options
----------------------

The model is enabled by:

- ``iGrainBoundaryVenting`` (input option)

Options available:
- ``0``: No venting, the model returns immediately.
- ``1``: Sigmoid-based model, it represents bubble interconnection.
- ``2``: Open porosity-based model (Claisse and Van Uffelen correlation).
- ``3``: Open porosity-based model (under development).

Inputs
------

- ``iGrainBoundaryVenting`` (input option)
- ``Intergranular fractional coverage`` for option 1
- ``Intergranular fractional intactness`` for option 1
- ``Fabrication porosity`` for options 2, 3
- ``<Gas> at grain boundary`` for each gas system
- ``Time step`` (physics variable)

Outputs
-------

- ``Intergranular venting probability, P_vent``
- ``Open porosity`` for options 2, 3
- Updated grain-boundary inventories (after applying the venting sink)

Key variables
-------------

The model uses:
- ``Intergranular fractional coverage``: initial fractional coverage at the grain boundary.
- ``Intergranular fractional intactness``: measure of boundary integrity (increment and final value).
- ``Intergranular vented fraction``: sigmoid-based fraction of vented boundary.
- ``Open porosity``: fraction of interconnected porosity reaching the exterior.
- ``Intergranular venting probability``: effective venting probability used in the sink term.
- ``<Gas> at grain boundary`` for each gas system (Xe, Kr, He, radioactive gases, …).

Model Option 1: Sigmoid-based vented fraction
---------------------------------------------

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

The effective venting probability is defined by mixing the intact and non-intact
fractions:

.. math::

   P_{\mathrm{vent}} =
   (1 - I^{n+1}) + I^{n+1}\,f_{\mathrm{vent}}

where :math:`I^{n+1}` is the final value of ``Intergranular fractional intactness``.

Reference: Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE.

Model Option 2: Open porosity-based (Claisse & Van Uffelen)
----------------------------------------------------------

For ``iGrainBoundaryVenting = 2``, the probability depends on the open porosity $p_{\mathrm{open}}$:

.. math::

    P_{\mathrm{vent}} = 1.54 \sqrt{p_{\mathrm{open}}}

The open porosity is calculated from ``Fabrication porosity`` ($p_{\mathrm{fab}}$) as:

- If $p_{\mathrm{fab}} < 0.050$: $p_{\mathrm{open}} = p_{\mathrm{fab}} / 20$
- If $0.050 \leq p_{\mathrm{fab}} \leq 0.058$: $p_{\mathrm{open}} = 3.10 \, p_{\mathrm{fab}} - 0.1525$
- If $p_{\mathrm{fab}} > 0.058$: $p_{\mathrm{open}} = p_{\mathrm{fab}} / 2.1 - 3.2 \cdot 10^{-4}$

Reference: Claisse and Van Uffelen, JNM, 466 (2015).

Application to grain-boundary gas inventories
---------------------------------------------

For **each** gas system, the inventory is updated by a sink term:

.. math::

    C_{\mathrm{gb}}^{n+1} = C_{\mathrm{gb}}^{n} - P_{\mathrm{vent}}\,C_{\mathrm{gb}}^{n}\,\Delta t

Implemented as:
``solver.Integrator( <Gas> at grain boundary, -P_vent, <Gas> at grain boundary increment )``
