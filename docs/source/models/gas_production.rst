Gas Production
==============

This model computes the production of gaseous species (stable or radioactive)
from fission and tracks the cumulative amount produced over time. The calculation
is performed separately for each gas ``System`` defined in ``sciantix_system``
(e.g., Xe, Kr, He, and selected isotopes), and it supports both the standard fuel
matrix and the high-burnup structure (HBS) region.

The implementation follows ``Simulation::GasProduction()``.

Inputs
------

For each gas system, the model uses:

- ``system.getProductionRate()``: production rate of the gas (source term).
- ``Time step`` (physics variable): current integration step size.

Model resolution
----------------

For each gas system, SCIANTIX creates a model entry named:

- ``Gas production - <system name>``

and stores as parameters:

- the production rate,
- the time step.

The cumulative produced amount is updated by time integration using
``solver.Integrator``:

.. math::

   P^{n+1} = P^{n} + \dot{P}\,\Delta t

where:

- :math:`\dot{P}` is the gas production rate for the system,
- :math:`\Delta t` is the current time step,
- :math:`P` is the cumulative produced inventory.

Depending on whether the system belongs to the standard matrix or to the HBS
(restructured matrix flag), the result is written to different variables:

- If ``system.getRestructuredMatrix() == 0``:

  - ``<GasName> produced`` is updated.

- If ``system.getRestructuredMatrix() == 1``:

  - ``<GasName> produced in HBS`` is updated.

Implementation note
-------------------

The routine loops over all systems in ``sciantix_system``, so gas production is
handled consistently for all modeled gaseous species using the same integration
scheme.
