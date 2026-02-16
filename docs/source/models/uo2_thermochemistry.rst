UO₂ Thermochemistry
===================

Purpose
-------
This model computes the equilibrium stoichiometry deviation of UO₂ under the
current thermo-chemical conditions, based on the Blackburn (1973) formulation.
It is activated only when stoichiometry deviation modelling is enabled.



Activation
----------

The model is executed only if the input option ``iStoichiometryDeviation`` is
enabled (non-zero). If stoichiometry deviation modelling is disabled, the model
returns immediately and no variables are updated.

Inputs
------

The model uses:

- ``iStoichiometryDeviation`` (input option)
- ``Stoichiometry deviation`` (sciantix variable, initial value)
- ``Temperature`` (history variable)
- ``Gap oxygen partial pressure`` (sciantix variable, in atm)

Model formulation
-----------------

The model solves for the **equilibrium stoichiometry deviation** using the
Blackburn formulation through a dedicated nonlinear solver
``solver.NewtonBlackburn(...)``. The parameter vector passed to the solver is:

- initial stoichiometry deviation
- temperature
- gap oxygen partial pressure

Cut-off conditions
------------------

For numerical robustness and physical consistency, the equilibrium stoichiometry
deviation is set to zero under either of the following conditions:

- :math:`T < 1000~\mathrm{K}`, or
- the gap oxygen partial pressure is zero.

Otherwise, the equilibrium value is obtained by solving the Blackburn equation
via Newton iteration:

.. math::

   \Delta x_{\mathrm{eq}} = \mathrm{NewtonBlackburn}\left(\Delta x_0,\,T,\,p_{\mathrm{O}_2}\right)

Outputs
-------

The model updates:

- ``Equilibrium stoichiometry deviation``

Reference
---------

Blackburn (1973), *Journal of Nuclear Materials*, 46, 244–252.
https://doi.org/10.1016/0022-3115(73)90038-X
