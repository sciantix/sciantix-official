Chromium Solubility
===================

This model computes the solubility of chromium in UO₂ as a function of
temperature and oxygen potential. It predicts chromium partitioning between
a metallic phase (Cr) and an oxide phase (Cr₂O₃) and determines how much
chromium is in solution or precipitated in each phase.

The default correlation is based on Riglet-Martial et al. (2014).

Reference
---------

Riglet-Martial et al., J. Nucl. Mater. 447 (2014) 63–72. https://doi.org/10.1016/j.jnucmat.2013.12.021

Inputs
------

The model uses:

- ``Temperature`` (history variable)
- ``iChromiumSolubility`` (input option selecting the parameter set)
- Fuel composition (uranium isotopes) and ``Fuel density`` (sciantix variables)
- ``Chromium content`` (sciantix variable)
- ``Burnup`` (sciantix variable), used in the Cr₂O₃/Cr partitioning rule

Model outline
-------------

1. **Oxygen chemical potential and oxygen pressure**

   The oxygen chemical potential is evaluated for two reference equilibria
   (Cr₂O₃ and Cr) and converted into an effective oxygen pressure, expressed
   as :math:`\log_{10}(p_{\mathrm{O}_2})`.

2. **Chromium solubility in metal and oxide phases**

   The chromium solubility correlations are evaluated in two temperature
   regimes (below/above a threshold temperature). The correlations are of the
   general form:

   .. math::

      x = 10^{\,p\,\log_{10}(p_{\mathrm{O}_2}) + V + \frac{U}{T}}

   where :math:`T` is the temperature and :math:`(p, V, U)` are model
   coefficients. The result is converted to chromium **weight percent** in UO₂.

3. **Molar mass of uranium and conversion between wt% and atoms/m³**

   A temperature-dependent uranium molar mass is computed from the isotopic
   inventory. The model converts between weight fractions and atomic
   concentrations to determine chromium atoms in the fuel.

4. **Phase partitioning (Cr₂O₃ fraction)**

   The fraction of chromium in the oxide phase is computed from an
   exponential function of temperature, with an additional burnup-dependent
   correction applied for :math:`B \le 30`:

   - :math:`f_{\mathrm{Cr_2O_3}} = 1 - \exp(C_1(T - C_2))`
   - A burnup-dependent shift is applied for low burnup.

5. **Solution vs precipitate**

   For each phase (metallic Cr and oxide Cr₂O₃), the model compares the
   available chromium inventory to the solubility limit:

   - if inventory ≤ solubility → all chromium is in solution
   - otherwise → excess chromium is assigned to a precipitate reservoir

Outputs
-------

The model updates the following sciantix variables:

- ``Chromium solubility`` (wt% Cr in UO₂)
- ``Chromia solubility`` (wt% Cr in UO₂ corresponding to Cr₂O₃ equilibrium)
- ``Chromium solution`` / ``Chromium precipitate`` (atoms/m³)
- ``Chromia solution`` / ``Chromia precipitate`` (atoms/m³)

The same quantities are also stored in the UO₂ material object (matrix
properties) for later use by other models.