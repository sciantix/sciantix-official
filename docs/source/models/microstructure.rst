Microstructure
==============

This model updates microstructural material properties of the UO₂ matrix that
are needed by other models, namely the **lattice parameter** and the
**theoretical density**. The calculation accounts for chromium content through
an empirical correlation for the lattice parameter and propagates this change
to the density.

Reference
---------

T. Cardinaels et al., *Journal of Nuclear Materials*, 424 (2012) 252–260.

Purpose and outputs
-------------------

The model computes and updates:

- ``Lattice parameter`` (sciantix variable)
- ``Theoretical density`` (sciantix variable)

The same values are also assigned to the UO₂ matrix object:

- ``matrices["UO2"].setLatticeParameter(...)``
- ``matrices["UO2"].setTheoreticalDensity(...)``

Inputs
------

The model uses:

- ``Chromium content`` (sciantix variable)
- ``Fuel density`` (sciantix variable)
- Uranium isotopic inventories (sciantix variables): ``U234``, ``U235``,
  ``U236``, ``U237``, ``U238``

and the constants:

- ``avogadro_number``
- ``molar_mass_Oxygen``
- ``molar_mass_Chromium``

Lattice parameter correlation
-----------------------------

The lattice parameter is computed from a baseline UO₂ lattice parameter and an
empirical chromium-dependent correction:

.. math::

   a = \left(a_0 - 1.2\times 10^{-6}\,C_{\mathrm{Cr}}\right)\times 10^{-10}

where:

- :math:`a_0 = 5.47109` (baseline value used in the code),
- :math:`C_{\mathrm{Cr}}` is the chromium content from ``Chromium content``.

The result is stored in ``Lattice parameter``.

Uranium molar mass estimate
---------------------------

A molar-mass-like quantity for uranium is computed from the isotopic inventory
variables. In the implementation this is obtained via a conversion factor:

.. math::

   \mathrm{conv\_fact} =
   \rho_f\,N_A\,10\,0.8815\,100

where :math:`\rho_f` is the fuel density and :math:`N_A` is Avogadro’s number.
The code then combines the isotopic inventories to produce ``molar_mass_Uranium``.
(This follows the current implementation and is kept here for traceability.)

Chromium molar fraction term
----------------------------

The chromium contribution is converted into a molar fraction-like term
(:math:`C_{\mathrm{Cr, mol}}`) used in the density expression:

.. math::

   C_{\mathrm{Cr, mol}} =
   C_{\mathrm{Cr}}^{\ast}
   \frac{M_U + 2M_O}{C_{\mathrm{Cr}}^{\ast}(M_U - M_{\mathrm{Cr}}) + M_U}

where:

- :math:`C_{\mathrm{Cr}}^{\ast} = (\text{Chromium content})\times 10^{-6}`,
- :math:`M_U` is the computed uranium molar mass term,
- :math:`M_{\mathrm{Cr}}` is the chromium molar mass,
- :math:`M_O` is the oxygen molar mass.

Theoretical density
-------------------

The theoretical density is computed using the lattice parameter and the
composition-dependent mass per unit cell:

.. math::

   \rho_{\mathrm{th}} =
   4\,
   \frac{(1-C_{\mathrm{Cr, mol}})M_U + C_{\mathrm{Cr, mol}}M_{\mathrm{Cr}} + 2M_O}
        {N_A\,(a\cdot 10^{2})^{3}}
   \times 10^{3}

where:

- the factor 4 corresponds to the number of formula units per fluorite unit cell,
- the unit conversions follow the implementation (including the :math:`10^2` and
  :math:`10^3` factors).

The result is stored in ``Theoretical density``.

Implementation notes
--------------------

This model is a property-update routine and does not solve an evolution
equation. It is typically called to keep matrix properties consistent with the
current fuel composition (e.g., chromium doping).
