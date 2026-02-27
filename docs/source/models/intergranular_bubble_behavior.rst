Intergranular Bubble Behaviour
==============================

This model describes the evolution of grain-boundary (intergranular) bubbles and
computes the associated quantities needed for fission gas release and swelling:
bubble volume/radius/area, fractional coverage, bubble pressure, intergranular
gas swelling, and released inventories (by mass balance). The governing
equations are implemented following Pastore et al. (2013) for grain-boundary
bubble growth and coalescence, with additional saturation handling when the
fractional coverage exceeds a prescribed limit.

Activation and option flag
--------------------------

The behaviour is selected by the input option:

- ``iGrainBoundaryBehaviour``

Option summary
--------------

- **0**: no grain-boundary bubble evolution model (growth terms set to zero).
- **1**: Pastore et al. (2013) model for vacancy-driven growth, with coalescence
  and saturation scaling.

Inputs
------

The model uses:

- ``iGrainBoundaryBehaviour`` (input option)
- ``Grain radius`` (sciantix variable)
- ``Temperature`` (history variable)
- ``Hydrostatic stress`` (history variable, MPa in input; converted in code)
- ``Time step`` (physics variable)

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

Model formulation (case 1: Pastore et al. 2013)
-----------------------------------------------

Gas distribution among grain-boundary bubbles
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For each non-radioactive system in a non-restructured matrix, the atoms per
bubble are computed from the grain-boundary inventory:

.. math::

   n_i = \frac{C_i^{gb}}{N_{gb}\,(S/V)}

where:

- :math:`C_i^{gb}` is ``<gas> at grain boundary`` (atoms/m³),
- :math:`N_{gb}` is ``Intergranular bubble concentration`` (bubbles/m²),
- :math:`S/V = 3/a` with :math:`a` the grain radius.

The total atoms per bubble is:

.. math::

   n_{tot} = \sum_i n_i

Initial bubble geometry and fractional coverage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Model resolution
----------------

Vacancies per bubble
~~~~~~~~~~~~~~~~~~~~

Vacancies per bubble are advanced with:

- ``solver.LimitedGrowth(nv0, parameters, Δt)``

where the parameter vector contains the growth and equilibrium terms computed
above.

Bubble volume, radius, area
~~~~~~~~~~~~~~~~~~~~~~~~~~~

After updating vacancies, the bubble volume is recomputed:

.. math::

   V_b^{n+1} = \sum_i n_i^{n+1} v_i^{vdW} + n_v^{n+1}\Omega_S

Radius and area are updated with the same relations as in the initialization
step.

Coalescence and conservation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Coalescence is applied by updating the bubble concentration using a binary
interaction law driven by the increment in bubble area:

- ``solver.BinaryInteraction(N0, 2.0, ΔA_b)``

After coalescence, the model enforces conservation by rescaling:

- ``Intergranular <gas> atoms per bubble`` (and vacancies per bubble)
  by :math:`N_0/N_1`

and then recomputes total atoms per bubble, volume, radius, and area again.

Saturation scaling (fractional coverage limit)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the computed fractional coverage exceeds a saturation constraint, a similarity
ratio is computed:

.. math::

   s = \sqrt{\frac{F_{sat}}{F}}

If :math:`s < 1`, several bubble variables are rescaled consistently (area,
concentration, volume, radius, vacancies, and atoms-per-bubble), and the
grain-boundary gas inventories are rescaled accordingly to maintain consistent
state variables.

Bubble pressure
~~~~~~~~~~~~~~~

The grain-boundary bubble pressure (MPa) is computed as:

.. math::

   p = 10^{-6}\frac{k_B T\,n_{tot}}{n_v\,\Omega_S}

If ``Intergranular vacancies per bubble`` is zero, the pressure is set to zero.

Released inventories and release metrics
----------------------------------------

Released concentration (mass balance)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For each non-restructured system, released inventory is computed by mass balance:

.. math::

   C^{rel} = C^{prod} - C^{dec} - C^{grain} - C^{gb}

Negative values are clipped to zero.

Intergranular gaseous swelling
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The intergranular gas swelling is computed as:

.. math::

   S_{ig} = \frac{3}{a}\,N_{gb}\,V_b

Fission gas release (Xe+Kr)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The fission gas release fraction is:

.. math::

   \mathrm{FGR} =
   \frac{C_{Xe}^{rel} + C_{Kr}^{rel}}
        {C_{Xe}^{prod} + C_{Kr}^{prod}}

If no Xe and Kr are produced, FGR is set to zero.

Release-to-birth ratios (radioactive gases)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Xe133 R/B and Kr85m R/B are computed as released divided by (produced − decayed),
  with zero used when the denominator is not positive.

Helium release quantities
~~~~~~~~~~~~~~~~~~~~~~~~~

- ``He fractional release`` = ``He released`` / ``He produced`` (if produced > 0)
- ``He release rate`` = increment of ``He released`` / ``Time step`` (if Δt > 0)

Outputs
-------

Depending on the option and available systems, the model updates:

- intergranular bubble state:
  ``Intergranular bubble concentration``, ``... vacancies per bubble``,
  ``... volume``, ``... radius``, ``... area``, ``... fractional coverage``,
  ``Intergranular bubble pressure``, ``Intergranular atoms per bubble``
- per-gas atoms-per-bubble variables:
  ``Intergranular <gas> atoms per bubble``
- released inventories and release metrics:
  ``<gas> released``, ``Fission gas release``, ``Xe133 R/B``, ``Kr85m R/B``,
  ``He fractional release``, ``He release rate``
- swelling metric:
  ``Intergranular gas swelling``

References
----------

- Pastore et al., *Nucl. Eng. Des.*, 256 (2013) 75–86. 
  https://doi.org/10.1016/j.nucengdes.2012.12.002