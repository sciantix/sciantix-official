Grain-Boundary Micro-Cracking
=============================

This model describes the degradation (micro-cracking) of grain-boundary integrity
during transients and a progressive healing with burnup. The main state variable
is the **intergranular fractional intactness** :math:`f` (1 = intact, 0 = fully
degraded). When the release mode relies on grain-boundary storage
(``iReleaseMode == 0``), micro-cracking also reduces:

- the **intergranular fractional coverage** :math:`F_c`
- the **intergranular saturation fractional coverage** :math:`F_{c,\mathrm{sat}}`

The implementation is in ``Simulation::GrainBoundaryMicroCracking()``.

Model switch
------------

Activated if ``iGrainBoundaryMicroCracking > 0``. The available options are:

- ``0``: not considered
- ``1``: transient-driven micro-cracking (Barani et al., 2017)
- ``2``: mechanistic micro-cracking criterion based on bubble pressure vs. critical pressure
  (Cappellari et al., 2025), with healing from Barani et al. (2017)

State variables affected
------------------------

- ``Intergranular fractional intactness``  (:math:`f`)
- (only if ``iReleaseMode == 0``)
  ``Intergranular fractional coverage`` (:math:`F_c`) and
  ``Intergranular saturation fractional coverage`` (:math:`F_{c,\mathrm{sat}}`)

For ``iReleaseMode == 0`` the model also rescales grain-boundary bubble geometry
and inventories to remain consistent after a coverage/intactness change.

Model formulation
-----------------

General degradation law
^^^^^^^^^^^^^^^^^^^^^^^

Micro-cracking is applied through a first-order ODE written with respect to a
generic *driving increment* :math:`\Delta x`:

.. math::

   \frac{\mathrm{d}f}{\mathrm{d}x} = -m_x \, f

so that over one step:

.. math::

   f^{n+1} = f^n \exp(-m_x \, \Delta x)

In the code this is solved via ``solver.Decay(f^n, m_x, 0, Δx)`` where:

- ``parameter[0] = m_x``  (microcracking parameter)
- ``parameter[1] = Δx``   (increment of interest)

Case 1: temperature-transient micro-cracking
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Driving increment: :math:`\Delta x = \Delta T`
- Microcracking parameter: :math:`m_x = \mathrm{d}m/\mathrm{d}T`

The model uses a sigmoid-like function centered at an inflection temperature that
depends on burnup:

.. math::

   T_{\mathrm{infl}} = 1773 + 520 \exp\!\left(-\frac{B}{10\cdot 0.8814}\right)

The sign of the transient is accounted for by using a *transient type*
:math:`s = +1` for heating (:math:`\Delta T>0`) and :math:`s=-1` otherwise,
with a span parameter (10.0) and exponent (33.0), producing
:math:`\mathrm{d}m/\mathrm{d}T` as implemented in the source.

Case 2: mechanistic cracking driven by bubble pressure
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Driving increment: :math:`\Delta x = \Delta q`
  where :math:`\Delta q` is an opening increment (from 0 to the bubble minor-axis
  opening term) stored as:

  ``Δq = (1 - cos(semidihedral_angle)) * R_b``

A microcracking parameter is computed only when the bubble gas pressure exceeds a
critical pressure:

1) **Equilibrium pressure** (capillarity – hydrostatic stress):

.. math::

   P_{\mathrm{eq}} =
   \frac{2\gamma(1-\cos\theta)}{R_b} - \sigma_h

2) **Fracture stress contribution** based on a toughness-like term
(using :math:`E,\nu,G` and a stress intensification factor, plus a geometric
factor :math:`h(F_c,Y)`):

.. math::

   P_{\mathrm{crit}} = P_{\mathrm{eq}} + \sigma_{\mathrm{fracture}}

3) **Bubble pressure** is evaluated from an atom-per-vacancy ratio (capped) and a
Schottky volume, as in the code.

If:

.. math::

   P_b > P_{\mathrm{crit}} \quad \text{and} \quad P_b > P_{\mathrm{eq}}

then:

.. math::

   m_q = \frac{(P_b - P_{\mathrm{eq}})\,F_c}{G}

and is used as ``parameter[0]``.

Healing with burnup
-------------------

A burnup-driven healing term (from Barani et al., 2017) is always applied after
the transient degradation:

.. math::

   \frac{\mathrm{d}f}{\mathrm{d}B} = -h f + h,
   \qquad h = \frac{1}{0.8814}

implemented via ``solver.Decay(f^{n+1}, h, h, ΔB)``.

Effect on coverage and storage capability (only for iReleaseMode == 0)
----------------------------------------------------------------------

When ``iReleaseMode == 0`` the code updates coverage and saturation coverage using
the **increment of intactness**:

.. math::

   F_c^{n+1} = F_c^n \exp\!\left(+\Delta f\right),
   \qquad
   F_{c,\mathrm{sat}}^{n+1} = F_{c,\mathrm{sat}}^n \exp\!\left(+\Delta f\right)

which in the implementation corresponds to:

- ``solver.Decay(Fc^n, 1, 0, -Δf)``
- ``solver.Decay(Fcsat^n, 1, 0, -Δf)``

Then, during burnup healing, only the saturation coverage is healed as:

.. math::

   \frac{\mathrm{d}F_{c,\mathrm{sat}}}{\mathrm{d}B}
   = -h(1-f)\,F_{c,\mathrm{sat}}

Rescaling step (only for iReleaseMode == 0)
-------------------------------------------

A similarity ratio is computed from the fractional coverage change:

.. math::

   \lambda = \sqrt{\frac{F_c^{n+1}}{F_c^n}}

If :math:`\lambda < 1`, a consistent rescaling is applied to grain-boundary bubble
state variables (area, concentration, volume, radius, vacancies per bubble) and
to stable-gas atoms-per-bubble, and grain-boundary inventories are rescaled
accordingly.

References
----------

- Barani et al. (2017), Journal of Nuclear Materials.
  https://doi.org/10.1016/j.jnucmat.2016.10.051
- Cappellari et al. (2025), Journal of Nuclear Materials.
  https://doi.org/10.1016/j.jnucmat.2025.156116
