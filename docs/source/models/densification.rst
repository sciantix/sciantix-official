Densification
=============

This model accounts for fuel densification during irradiation, represented as a
progressive reduction of the fabrication porosity towards a residual porosity.
Densification is enabled through the input option ``iDensification`` and is
driven by burnup.

Reference
---------

Van Uffelen, P. (2002), PhD thesis, SCK•CEN Reports No. BLG-907. https://www.oecd-nea.org/science/wprs/fuel/Thesis_Van_Uffelen_BLG.pdf

Activation
----------

If ``iDensification = 0``, the model is not considered and no updates are
performed. If ``iDensification = 1``, densification is calculated according to a
fit based on temperature.

Inputs
------

The model uses:

- ``iDensification`` (input option)
- ``Temperature`` (history variable)
- ``Burnup`` increment (sciantix variable, used as the evolution variable)
- ``Residual porosity`` (sciantix variable)
- ``Fabrication porosity`` (sciantix variable)

Model formulation
-----------------

A *densification factor* :math:`f_{\mathrm{dens}}` is evolved with burnup using
an exponential decay law (implemented through ``solver.Decay``). For the default
correlation (``iDensification = 1``), the parameters are:

- :math:`a = 2.0`
- :math:`b = 0.006 \exp\!\left(0.002\,T\right)`

where :math:`T` is the fuel temperature.

The densification factor is bounded by:

.. math::

   f_{\mathrm{dens}} \le 1

Porosity update
---------------

The fabrication porosity is updated as a transition from its current value
towards the residual porosity:

.. math::

   \varepsilon_{\mathrm{fab}} =
   \varepsilon_{\mathrm{res}} +
   \left(\varepsilon_{\mathrm{fab}} - \varepsilon_{\mathrm{res}}\right)
   \left(1 - f_{\mathrm{dens}}\right)

The total porosity is then incremented using the fabrication-porosity increment.

Outputs
-------

The model updates:

- ``Densification factor``
- ``Fabrication porosity``
- ``Porosity`` (incremented by the fabrication porosity increment)
