High-burnup Structure Formation
===============================

This model describes the formation of the high-burnup structure (HBS) through a
restructuring (phase transformation) law expressed in terms of **effective
burnup**. The model updates the ``Restructured volume fraction`` and is enabled
through the input option ``iHighBurnupStructureFormation``.

The implementation follows ``Simulation::HighBurnupStructureFormation()``.

Activation
----------

The model is executed only if:

- ``iHighBurnupStructureFormation`` > 0

Option summary
--------------

- **0**: not considered (no restructuring).
- **1**: HBS formation model from Barani et al. (2020).

Inputs
------

The model uses:

- ``iHighBurnupStructureFormation`` (input option)
- ``Effective burnup`` (sciantix variable): used as the independent variable for
  restructuring
- ``Effective burnup`` increment: used as the integration increment

Main output:

- ``Restructured volume fraction`` (sciantix variable)

Model formulation (case 1)
--------------------------

For option **1**, the model defines two constants:

- Avrami constant: :math:`A = 3.54`
- Transformation rate: :math:`k = 2.77 \times 10^{-7}`

Two additional parameters are defined in the code:

- ``resolution_layer_thickness`` = :math:`1.0\times 10^{-9}` m
- ``resolution_critical_distance`` = :math:`1.0\times 10^{-9}` m

.. note::
   In the current implementation of ``Simulation::HighBurnupStructureFormation()``,
   the two “resolution” parameters are stored in the model parameter vector but are
   not used in the restructuring update.

Restructuring rate
~~~~~~~~~~~~~~~~~~

A burnup-dependent coefficient is computed as:

.. math::

   c = A\,k\,B_{\mathrm{eff}}^{2.54}

where :math:`B_{\mathrm{eff}}` is the effective burnup (``Effective burnup``).

Update equation
~~~~~~~~~~~~~~~

The restructured volume fraction :math:`\alpha_r` is updated using
``solver.Decay(...)`` with effective burnup increment :math:`\Delta B_{\mathrm{eff}}`:

.. math::

   \alpha_r^{n+1} =
   \mathrm{Decay}\left(\alpha_r^{n},\; c,\; c,\; \Delta B_{\mathrm{eff}}\right)

In other words, the evolution is written in a “decay + source” form with equal
coefficients for the decay and source terms, as implemented in the solver.

Reference
---------

- Barani et al., *Journal of Nuclear Materials* 539 (2020) 152296.
  https://doi.org/10.1016/j.jnucmat.2020.152296