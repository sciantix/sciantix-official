Burnup
======

This model computes the local fuel burnup from the fission rate density. The
specific power is derived from the fission rate and fuel density and is then
time-integrated to obtain the burnup expressed in MWd/kg. The model also updates
related irradiation quantities, including the irradiation time and the fraction
of initial metal atoms (FIMA).

The implementation follows the steps used in ``Simulation::Burnup()``.

Inputs
~~~~~~

The model uses the following variables:

- ``Fission rate`` (history variable): fission rate density.
- ``Fuel density`` (sciantix variable): fuel density used to convert to specific power.
- ``Time step`` (physics variable): integration time step.

Specific power and burnup rate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The specific power is computed from the fission rate density using a constant
conversion factor:

.. math::

   P_{\mathrm{spec}} = \frac{\dot{F}\,C}{\rho_f}

where:

- :math:`\dot{F}` is the fission rate density (``Fission rate``),
- :math:`\rho_f` is the fuel density (``Fuel density``),
- :math:`C = 3.12 \times 10^{-17}` is the conversion constant used in the code.

The burnup rate is then defined as:

.. math::

   \dot{B} = \frac{P_{\mathrm{spec}}}{86400}

so that burnup is expressed in MWd/kg.

Time integration
~~~~~~~~~~~~~~~~

The burnup value is updated by time integration:

.. math::

   B^{n+1} = B^{n} + \dot{B}\,\Delta t

where :math:`\Delta t` is the current time step (``Time step``). In the code, this
operation is performed using ``solver.Integrator``.

Irradiation time
~~~~~~~~~~~~~~~~

When the fission rate density is positive, the irradiation time is updated
consistently with the burnup increment:

.. math::

   t_{\mathrm{irr}}^{n+1} = t_{\mathrm{irr}}^{n}
   + \frac{24}{P_{\mathrm{spec}}}\,\Delta B

where :math:`\Delta B` is the burnup increment over the current step. If the
fission rate is zero, the irradiation time is kept constant.

FIMA
~~~~

The Fraction of Initial Metal Atoms (FIMA) is updated as:

.. math::

   \mathrm{FIMA}^{n+1} = \mathrm{FIMA}^{n}
   + \left(\frac{\dot{F}\,3.6 \times 10^{5}}{U}\right)\Delta t_{\mathrm{irr}}

where:

- :math:`U` is the uranium inventory variable (``U``),
- :math:`\Delta t_{\mathrm{irr}}` is the irradiation time increment.

Implementation note
~~~~~~~~~~~~~~~~~~~

In the source code, the model reference is reported as:

"The local burnup is calculated from the fission rate density."
