Gap Partial Pressure
===========================

This model computes the *partial pressure in the fuel–cladding gap*
based on thermodynamic equilibrium relations. The intent is to represent
oxidizing conditions in the gap atmosphere when stoichiometry deviation
effects are activated.

Reference
---------

Lewis et al., *Journal of Nuclear Materials* 227 (1995) 83–109.

D.R. Olander, *Nuclear Technology* 74 (1986) 215.

Inputs
------

The model uses:

- ``Temperature`` (history variable)
- ``Steam pressure`` (history variable)
- ``iStoichiometryDeviation`` (input flag)

Outputs
-------

- ``Gap oxygen partial pressure`` (sciantix variable, atm)

Model activation
----------------

The model is executed only if:

::

   iStoichiometryDeviation = 1

If the flag is not active, the calculation is skipped.

Model formulation
-----------------

An equilibrium constant is defined as:

.. math::

   K =
   \exp\left(
   -\frac{25300}{T}
   + 4.64
   + 1.04\left(0.0007\,T - 0.2\right)
   \right)

where :math:`T` is the temperature (K).

The oxygen partial pressure in the gap is computed as:

.. math::

   p_{\mathrm{O}_2}^{\mathrm{gap}} =
   \left(
   \frac{K^2 \, p_{\mathrm{H_2O}}^2}{4}
   \right)^{1/3}

where :math:`p_{\mathrm{H_2O}}` is the steam pressure (atm).

Implementation
--------------

The computed value is assigned to the variable
``Gap oxygen partial pressure`` (atm) at each time step.
