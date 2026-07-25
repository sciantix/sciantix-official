Stoichiometry Deviation
=======================

This model computes the time evolution of the fuel stoichiometry deviation
(:math:`x` in :math:`\mathrm{UO_{2+x}}`) under oxidising conditions. The model is
activated via the input option ``iStoichiometryDeviation`` and supports both
semi-empirical oxidation kinetics (options 1–4) and a mechanistic Langmuir-based
approach (options 5–6). The model also updates the fuel oxygen partial pressure
and oxygen potential using the Blackburn thermochemical relation.

Activation
----------

The model is executed only if ``iStoichiometryDeviation`` is enabled (non-zero).
If the option is disabled, the function returns and no variables are updated.

Inputs
------

The model uses:

- ``iStoichiometryDeviation`` (input option)
- ``Grain radius`` (sciantix variable) to compute :math:`S/V`
- ``Temperature`` (history variable)
- ``Steam pressure`` (history variable)
- ``Gap oxygen partial pressure`` (sciantix variable)
- ``Equilibrium stoichiometry deviation`` (sciantix variable, computed by the
  :doc:`uo2_thermochemistry` model)
- ``Time step`` (physics variable)

Key definitions
---------------

Surface-to-volume ratio
~~~~~~~~~~~~~~~~~~~~~~~

For the default grain-scale case, the surface-to-volume ratio is computed as:

.. math::

   \frac{S}{V} = \frac{3}{a}

where :math:`a` is the grain radius (``Grain radius``). In option
``iStoichiometryDeviation = 6`` the value is overridden and a fixed
:math:`S/V = 225~\mathrm{m^{-1}}` is used.

Temperature cut-off
~~~~~~~~~~~~~~~~~~~

If :math:`T < 1000~\mathrm{K}`, the model does not evolve the deviation:

- ``Stoichiometry deviation`` is kept constant
- ``Fuel oxygen partial pressure`` is set to zero

Semi-empirical oxidation kinetics (options 1–4)
-----------------------------------------------

For ``iStoichiometryDeviation = 1..4``, the model follows a Carter-and-Lay type
oxidation law, driven by the difference between the equilibrium deviation
:math:`x_{\mathrm{eq}}` and the current deviation :math:`x` and scaled by
:math:`(S/V)\sqrt{P_{\mathrm{H_2O}}}`.

In the implementation, this is written in linear source–decay form:

.. math::

   \frac{dx}{dt} = -k\,x + s

with:

.. math::

   k = \alpha(T)\,\sqrt{P_{\mathrm{H_2O}}}\,\frac{S}{V},
   \qquad
   s = \alpha(T)\,\sqrt{P_{\mathrm{H_2O}}}\,x_{\mathrm{eq}}\,\frac{S}{V}

where :math:`\alpha(T) = A \exp(-Q/T)` is the surface exchange coefficient.
Different parameter sets (:math:`A, Q`) are used depending on the selected
option:

- **Option 1:** Cox et al. / Carter and Lay
- **Option 2:** Bittel et al. (re-analysed)
- **Option 3:** Abrefah et al.
- **Option 4:** Imamura and Une (includes a normalisation by 0.12 in the steam term)

Time integration is performed using the solver decay form:

.. math::

   x^{n+1} = \mathrm{Decay}\left(x^{n},\,k,\,s,\,\Delta t\right)

via ``solver.Decay(...)``.

Mechanistic Langmuir-based model (options 5–6)
----------------------------------------------

For ``iStoichiometryDeviation = 5`` and ``6``, the oxidation kinetics are based
on a mechanistic Langmuir-type formulation (Massih, 2018). In SCIANTIX the
evolution equation is rewritten in the form:

.. math::

   \frac{dx}{dt} = K\left(1 - \beta\,e^{\alpha x}\right)

where:

- :math:`K` depends on temperature, steam pressure, and surface-to-volume ratio
- :math:`\beta` depends on the gap oxygen partial pressure
- :math:`\alpha` is treated as a constant (in the implementation :math:`\alpha = 57/2`)

The new value is computed using Newton iteration:

.. math::

   x^{n+1} = \mathrm{NewtonLangmuirBasedModel}\left(x^{n},\,\{K,\beta,\alpha\},\,\Delta t\right)

via ``solver.NewtonLangmuirBasedModel(...)``.

Thermochemical update: fuel oxygen partial pressure
---------------------------------------------------

After updating ``Stoichiometry deviation``, the model computes the fuel oxygen
partial pressure using the Blackburn thermochemical relation:

.. math::

   \ln p = 2\ln\left(\frac{x(2+x)}{1-x}\right) + 108\,x^2 - \frac{32700}{T} + 9.92

and returns:

.. math::

   p_{\mathrm{O_2}} = e^{\ln p}

This is implemented by ``BlackburnThermochemicalModel(...)`` and stored in:

- ``Fuel oxygen partial pressure``

Fuel oxygen potential
---------------------

Finally, the fuel oxygen potential is computed from the oxygen partial pressure:

- if :math:`p_{\mathrm{O_2}} = 0`, then the oxygen potential is set to 0
- otherwise:

.. math::

   \mu_{\mathrm{O_2}} = RT \ln\left(\frac{p_{\mathrm{O_2}}}{0.1013}\right)

In the implementation this is stored as:

- ``Fuel oxygen potential``

Outputs
-------

The model updates:

- ``Stoichiometry deviation``
- ``Fuel oxygen partial pressure``
- ``Fuel oxygen potential``

References
----------

- Carter and Lay, *J. Nucl. Mater.*, 36, 77–86 (1970).
  https://doi.org/10.1016/0022-3115(70)90063-2

- Cox et al., NUREG/CP-0078 (1986), U.S. NRC.
- Bittel et al., *J. Amer. Ceram. Soc.*, 52, 446–451 (1969).
  https://doi.org/10.1111/j.1151-2916.1969.tb11976.x

- Abrefah et al., *J. Nucl. Mater.*, 208, 98–110 (1994).
  https://doi.org/10.1016/0022-3115(94)90201-1

- Imamura and Une, *J. Nucl. Mater.*, 247, 131–137 (1997).
  https://doi.org/10.1016/S0022-3115(97)00082-2

- Massih (2018), Swedish Radiation Safety Authority (SSM), Report 2018:25.
  https://www.stralsakerhetsmyndigheten.se/contentassets/f52c9deecaf4441194fa8220d829b040/201825-uo2-fuel-oxidation-and-fission-gas-release.pdf

- Blackburn (1973), *J. Nucl. Mater.*, 46, 244–252.
  https://doi.org/10.1016/0022-3115(73)90038-X

