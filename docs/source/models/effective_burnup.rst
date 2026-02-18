Effective Burnup
================

This model computes an *effective burnup* that accumulates only when the fuel
temperature is below a threshold. The intent is to represent burnup-driven
effects that are assumed to be active primarily at relatively low temperature.

Reference
---------

G. Khvostov et al., WRFPM-2005, Kyoto, Japan, 2005.

Inputs
------

The model uses:

- ``Temperature`` (history variable)
- ``Specific power`` (sciantix variable)
- ``Time step`` (physics variable)

Activation
----------

The effective burnup model is executed at every time step; it computes an
accumulated burnup that depends on the temperature threshold. There is no
dedicated input flag to disable the routine.

Model formulation
-----------------

A temperature threshold is defined as:

.. math::

   T_{\mathrm{th}} = 1273.15~\mathrm{K}

The effective burnup rate is taken equal to the burnup rate when either:

- the current temperature is below the threshold, or
- the temperature crosses the threshold within the current step (from below to above).

Otherwise, the effective burnup rate is set to zero:

.. math::

   \dot{B}_{\mathrm{eff}} =
   \begin{cases}
   \dfrac{P_{\mathrm{spec}}}{86400} & \text{if } T \le T_{\mathrm{th}}
   \text{ or } (T > T_{\mathrm{th}} \ \wedge \ T^{n} < T_{\mathrm{th}}) \\
   0 & \text{otherwise}
   \end{cases}

where :math:`P_{\mathrm{spec}}` is the specific power.

Time integration
----------------

The effective burnup is updated by time integration:

.. math::

   B_{\mathrm{eff}}^{n+1} = B_{\mathrm{eff}}^{n} + \dot{B}_{\mathrm{eff}}\,\Delta t

In the implementation this is performed using ``solver.Integrator`` with the
current ``Time step``.

Outputs
-------

- ``Effective burnup`` (updated accumulated burnup stored in sciantix variables)
