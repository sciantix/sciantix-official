Intergranular Bubble Behaviour
==============================

This model describes the evolution of grain-boundary (intergranular) bubbles and 
computes the associated quantities needed for fission gas release and swelling. 
The implementation follows ``Simulation::InterGranularBubbleBehavior()``.

Activation and option flag
--------------------------

The model is controlled by two main input options:

- ``iGrainBoundaryBehaviour``: Selects the bubble evolution model.
- ``iReleaseMode``: Selects the coalescence and release mechanism.

Inputs
------

The model uses:

- ``iGrainBoundaryBehaviour``, ``iReleaseMode`` (input options)
- ``Temperature``, ``Hydrostatic stress``, ``Time step``  (history variables)
- ``Intergranular fractional intactness``
- ``Grain radius`` (sciantix variable)

Intergranular bubble state variables (sciantix variables), including:
- ``Intergranular bubble concentration`` (bubbles/m²)
- ``Intergranular vacancies per bubble``
- ``Intergranular bubble volume``
- ``Intergranular bubble radius``
- ``Intergranular bubble area``
- ``Intergranular fractional coverage``
- ``Intergranular saturation fractional coverage``

Gas inventories at the grain boundary (per gas system), e.g.:

- ``Xe at grain boundary``, ``Kr at grain boundary``, ``He at grain boundary``, ...
- produced / decayed / in grain / released inventories used for mass balance

It also uses matrix properties from the UO₂ ``Matrix`` object (``fuel_``):

- grain-boundary thickness and vacancy diffusivity
- Schottky volume
- surface tension
- lenticular shape factor
- semidihedral angle

and gas properties from each ``system``:

- Van der Waals volume (per atom)
- decay rate (to skip radioactive gases)
- restructured-matrix flag (to skip restructured systems)

Intergranular Bubble Evolution (iGrainBoundaryBehaviour = 1)
------------------------------------------------------------
Bubbles are assumed to be lenticular. The number of atoms per bubble ($n_{at}$) is 
derived from the grain-boundary inventory and the bubble concentration. The atoms per bubble are computed from the grain-boundary inventory:

.. math::

   n_i = \frac{C_i^{gb}}{N_{gb}\,(S/V)}

where:

- :math:`C_i^{gb}` is ``<gas> at grain boundary`` (atoms/m³),
- :math:`N_{gb}` is ``Intergranular bubble concentration`` (bubbles/m²),
- :math:`S/V = 3/a` with :math:`a` the grain radius.

The total atoms per bubble is:

.. math::

   n_{tot} = \sum_i n_i

The initial bubble volume includes gas (Van der Waals) and vacancies
(Schottky volume):

.. math::

   V_b = \sum_i n_i\,v_i^{vdW} + n_v\,\Omega_S

The initial radius is computed assuming a lenticular correction via the
shape factor :math:`f_L`:

.. math::

   R_b = 0.620350491\left(\frac{V_b}{f_L}\right)^{1/3}

The bubble projected area is:

.. math::

   A_b = \pi \left(R_b \sin\theta\right)^2

where :math:`\theta` is the semidihedral angle.

The fractional coverage is then:

.. math::

   F = N_{gb}\,A_b

Vacancy-driven growth term and equilibrium term
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A sink-strength correlation is used as a function of fractional coverage
(:math:`F`), and a vacancy-driven volume flow rate is computed:

.. math::

   \dot{V} \propto 2\pi\,t_{gb}\,D_v\,S(F)

Using this, a “growth rate” term is formed proportional to the atoms per bubble
and normalised by Schottky volume, and an equilibrium term accounts for surface
tension and hydrostatic stress through an equilibrium pressure:

.. math::

   p_{eq} = \frac{2\gamma}{R_b} - \sigma_h

These two terms are stored as the model parameters and are later used to evolve
the vacancy content per bubble with a limited-growth solver.

Vacancies per bubble are advanced with:

- ``solver.LimitedGrowth(nv0, parameters, Δt)``

where the parameter vector contains the growth and equilibrium terms computed
above.

Coalescence Modes (iReleaseMode)
---------------------------------

Option 0 - 2: White (2004)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Coalescence is modelled following White (2004).
Coalescence is applied by updating the bubble concentration using a binary
interaction law driven by the increment in bubble area:

- ``solver.BinaryInteraction(N0, 2.0, ΔA_b)``


Option 1: Pastore et al. (2013)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A 4x4 state matrix is defined to evolve Volume, Area, Concentration, and 
Coverage simultaneously. The system is solved using ``solver.Laplace``. 
Coalescence follows Pastore et al. (2013) for Option 1.

Release Modes (iReleaseMode)
---------------------------------

Option 0
~~~~~~~~~~

If the computed fractional coverage exceeds a saturation constraint, a similarity
ratio is computed:

.. math::

   s = \sqrt{\frac{F_{sat}}{F}}

If :math:`s < 1`, several bubble variables are rescaled consistently (area,
concentration, volume, radius, vacancies, and atoms-per-bubble), and the
grain-boundary gas inventories are rescaled accordingly to maintain consistent
state variables.

Option 1 - 2
~~~~~~~~~~~~~~
For these modes, the vented fraction ($f_{\mathrm{vent}}$) is calculated using 
a Gaussian Process Regression (GPR) model:

.. math::
    f_{\mathrm{vent}} = 0.210911 \cdot \left(\mathrm{erf}(0.0937 \cdot 100 \cdot F - 3.7250) + 1\right)

The total release fraction ($R$) combines diffusion-based release and burst 
release from micro-cracking (via ``Intergranular fractional intactness``, $I$):

.. math::
    R = f_{\mathrm{vent}} \cdot I + (1 - I)

The inventories are updated via ``solver.Integrator`` applying the increment 
$\Delta R$ to the grain-boundary gas.


Bubble pressure
~~~~~~~~~~~~~~~

The grain-boundary bubble pressure (MPa) is computed as:

.. math::

   p = 10^{-6}\frac{k_B T\,n_{tot}}{n_v\,\Omega_S}

If ``Intergranular vacancies per bubble`` is zero, the pressure is set to zero.

References
----------

- Pastore et al., *Nucl. Eng. Des.*, 256 (2013) 75–86. 
  https://doi.org/10.1016/j.nucengdes.2012.12.002
- White, *J. Nucl. Mater.*, 325 (2004), 61–77.
- Cappellari et al., *J. Nucl. Mater.*, 617 (2025), 156116.
