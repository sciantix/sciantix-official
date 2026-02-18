Gas Decay
=========

This model computes the radioactive decay of gaseous species present in the
fuel matrix. The decay process follows first-order kinetics and accounts for
continuous production of the radioactive gas during irradiation.

Reference
---------

Standard radioactive decay law.

Inputs
------

The model uses:

- ``<Gas name> decayed`` (sciantix variable)
- ``<Gas name> produced`` (sciantix variable)
- ``Decay rate`` (gas property)
- ``Time step`` (physics variable)

Model activation
----------------

The model is applied to each gas system satisfying:

- the decay rate is greater than zero,
- the gas is not located in a restructured matrix.

Model formulation
-----------------

For each radioactive gas species, the evolution of the decayed amount
follows:

.. math::

   \frac{dN_{\mathrm{dec}}}{dt}
   =
   \lambda \, N_{\mathrm{prod}}
   - \lambda \, N_{\mathrm{dec}}

where:

- :math:`\lambda` is the decay constant,
- :math:`N_{\mathrm{prod}}` is the amount of produced gas,
- :math:`N_{\mathrm{dec}}` is the accumulated decayed amount.

Time integration
----------------

The solution over a time step :math:`\Delta t` is obtained using the internal
``solver.Decay`` routine:

.. math::

   N_{\mathrm{dec}}^{n+1}
   =
   \texttt{Decay}
   \left(
   N_{\mathrm{dec}}^{n},
   \lambda,
   \lambda N_{\mathrm{prod}}^{n+1},
   \Delta t
   \right)

The updated value is stored in:

::

   <Gas name> decayed

Outputs
-------

- ``<Gas name> decayed`` (updated decayed inventory stored in sciantix variables)
