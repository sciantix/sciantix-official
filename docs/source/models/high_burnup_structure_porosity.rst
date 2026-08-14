High-burnup Structure Porosity
==============================

This model describes the evolution of the high-burnup structure (HBS) porosity
and related pore statistics (pore density, pore volume, pore radius, and xenon
inventory in pores). The model is activated through the input option
``iHighBurnupStructurePorosity`` and follows the implementation in
``Simulation::HighBurnupStructurePorosity()``.

Activation and option flag
--------------------------

The model is executed only if:

- ``iHighBurnupStructurePorosity`` > 0

Option summary
--------------

- **0**: not considered (HBS porosity forced to zero).
- **1**: empirical porosity increase after a burnup threshold, capped at a
  maximum porosity.

Inputs
------

The model uses:

- ``iHighBurnupStructurePorosity`` (input option)
- ``Burnup`` (sciantix variable, used to trigger porosity increase and as
  integration increment)
- ``Time step`` (physics variable, used for time integration of pore density and
  xenon inventory)

It also uses UO₂HBS matrix properties (via ``matrices["UO2HBS"]``):

- pore nucleation rate
- pore resolution rate
- pore trapping rate

State variables updated include:

- ``HBS porosity``
- ``HBS pore density``
- ``HBS pore volume``
- ``HBS pore radius``
- ``Xe in HBS pores``
- ``Xe atoms per HBS pore``
- ``Xe in HBS pores - variance``
- ``Xe atoms per HBS pore - variance``

Model formulation (case 1)
--------------------------

Porosity evolution
~~~~~~~~~~~~~~~~~~

For option **1**, the porosity evolution is controlled by:

- a burnup threshold :math:`B_{th} = 50` (MWd/kg),
- a constant porosity increment rate :math:`k = 1.3\times 10^{-3}`,
- an upper porosity cap :math:`\phi_{max} = 0.15`.

If the current porosity is below :math:`\phi_{max}` and burnup is below
:math:`B_{th}`, the porosity increment is zero. Otherwise the increment is set
to :math:`k`. If porosity exceeds the cap, it is limited to :math:`\phi_{max}`.

In the code, porosity is integrated with burnup increment:

.. math::

   \phi^{n+1} = \phi^n + \dot{\phi}\,\Delta B

where :math:`\Delta B` is the burnup increment in the current step
(``Burnup`` increment).

The porosity is finally limited to:

.. math::

   \phi^{n+1} \le \phi_{max} = 0.15

Pore density (nucleation and resolution)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If :math:`\phi > 0`, the pore number density is evolved with a decay-type
equation (nucleation as source, resolution as decay):

.. math::

   N_p^{n+1} = \mathrm{Decay}\left(N_p^n,\; k_{res},\; k_{nuc},\; \Delta t\right)

where:

- :math:`k_{res}` is ``matrices["UO2HBS"].getPoreResolutionRate()``
- :math:`k_{nuc}` is ``matrices["UO2HBS"].getPoreNucleationRate()``
- :math:`\Delta t` is the current ``Time step``

If :math:`\phi = 0`, then ``HBS pore density`` is set to 0.

Pore volume and radius
~~~~~~~~~~~~~~~~~~~~~~

When :math:`N_p > 0`, pore volume is obtained directly from porosity:

.. math::

   V_p = \frac{\phi}{N_p}

The pore radius is then computed from volume as:

.. math::

   R_p = 0.620350491\,V_p^{1/3}

Pore interconnection by impingement
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The pore number density is further updated to account for pore interconnection
by impingement. A limiting factor is computed as:

.. math::

   L(\phi) = \frac{2 - \phi}{2(1-\phi)^3}

and the interconnection rate is defined as:

.. math::

   k_{int} = 4\,L(\phi)

Pore density is then updated with a binary interaction law driven by the pore
volume increment:

- ``solver.BinaryInteraction(Np, k_int, ΔV_p)``

After this update, pore volume and radius are recomputed consistently from the
updated pore density.

Xenon in HBS pores
------------------

The xenon inventory in HBS pores (average concentration, at/m³) is integrated as:

.. math::

   C_{Xe,p}^{n+1} = C_{Xe,p}^{n} +
   \left( 2k_{nuc} + N_p\,(k_{trap}-k_{res}) \right)\Delta t

implemented via ``solver.Integrator(...)`` with:

- :math:`k_{nuc}` = ``UO2HBS.getPoreNucleationRate()``
- :math:`k_{trap}` = ``UO2HBS.getPoreTrappingRate()``
- :math:`k_{res}` = ``UO2HBS.getPoreResolutionRate()``

If :math:`N_p > 0`, atoms per pore is:

.. math::

   n_{Xe,p} = \frac{C_{Xe,p}}{N_p}

Variance of xenon in pores
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A variance-like quantity is also evolved and converted into a per-pore variance:

.. math::

   \sigma_{Xe,p}^{2} = \frac{C_{Xe,p}^{var}}{N_p}

The source term used in the code includes trapping/resolution contributions and
a nucleation contribution proportional to
:math:`(n_{Xe,p} - 2)^2`. The evolution is integrated with the time step using
``solver.Integrator(...)``.

References
----------

- Spino et al. (2006), data basis for the empirical porosity evolution.
