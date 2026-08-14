Intragranular Bubble Behaviour
==============================

This model updates the intragranular bubble population and derived quantities
such as atoms per bubble, bubble radius, and intragranular gaseous swelling.
The bubble concentration is evolved with a simple source–decay law, while the
bubble radius is inferred from the total intragranular bubble volume accumulated
from gas inventories stored in intragranular bubbles.

Activation and option flag
--------------------------

The model behaviour is selected by the input option:

- ``iIntraGranularBubbleBehavior``

Option summary
--------------

The following cases are implemented:

- **0**: constant bubble concentration and radius (fixed values)
- **1**: Pizzocri et al. (2018) model rates (uses system resolution/nucleation rates)
- **2**: White and Tucker (1983) correlation for initial bubble concentration
- **3**: special case for annealing / helium behaviour using the intragranular similarity ratio
- **99**: no intragranular bubbles (all related variables set to zero)

Inputs
------

The model uses:

- ``iIntraGranularBubbleBehavior`` (input option)
- ``Time step`` (physics variable)
- Gas inventories in intragranular bubbles, e.g. ``Xe in intragranular bubbles``,
  ``Kr in intragranular bubbles``, ``He in intragranular bubbles`` (sciantix variables)
- ``Intragranular similarity ratio`` (sciantix variable, used in option 3 and updated at the end)

It also uses system-level quantities from ``sciantix_system``:

- ``getResolutionRate()``
- ``getNucleationRate()``
- gas decay rate and restructured matrix flag (to filter systems)

Model formulation
-----------------

Bubble concentration evolution
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The intragranular bubble concentration :math:`N` is evolved as:

.. math::

   \frac{dN}{dt} = -k\,N + s

where the coefficients :math:`k` and :math:`s` depend on the selected option
(e.g., resolution and nucleation rates for option 1).

In the code, the update is performed using:

- ``solver.Decay(N0, k, s, Δt)``

so that:

.. math::

   N^{n+1} = \mathrm{Decay}\left(N^{n}, k, s, \Delta t\right)

Atoms per bubble and bubble volume
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For each gas system (e.g., Xe, Kr, He), the model computes the number of atoms
per bubble when the system is not radioactive (decay rate = 0) and the matrix is
not restructured:

.. math::

   n_i = \frac{C_i}{N}

where:

- :math:`C_i` is the inventory of gas :math:`i` in intragranular bubbles
  (e.g., ``Xe in intragranular bubbles``),
- :math:`N` is the intragranular bubble concentration.

If :math:`N = 0`, then :math:`n_i = 0`.

The intragranular bubble volume is then accumulated by summing the contribution
from each gas species:

.. math::

   V_b = \sum_i v_i\,n_i

where :math:`v_i` is the per-atom volume in the lattice returned by
``system.getVolumeInLattice()``.

Bubble radius
~~~~~~~~~~~~~

The intragranular bubble radius is computed from the total bubble volume via a
spherical assumption:

.. math::

   R_b = 0.620350491 \, V_b^{1/3}

(The numeric factor is the implementation constant used in the code.)

Intragranular gaseous swelling
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The intragranular gas bubble swelling is computed as the total bubble volume
fraction:

.. math::

   S_g = \frac{4}{3}\pi N R_b^3

In the implementation:

- :math:`\frac{4}{3}\pi` is represented by ``4.188790205``.

Similarity ratio (helium annealing case)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At the end of the routine, an intragranular similarity ratio is updated from the
helium inventory in intragranular bubbles:

.. math::

   \mathrm{SR} =
   \sqrt{\frac{C_{\mathrm{He}}^{n+1}}{C_{\mathrm{He}}^{n}}}

If the initial helium inventory in intragranular bubbles is zero, the similarity
ratio is set to zero.

Outputs
-------

The model updates (depending on option and available systems):

- ``Intragranular bubble concentration``
- ``Intragranular bubble radius``
- ``Intragranular <gas> atoms per bubble`` (for each relevant gas)
- ``Intragranular bubble volume``
- ``Intragranular gas bubble swelling``
- ``Intragranular similarity ratio``

References
----------

- Pizzocri et al., *J. Nucl. Mater.*, 502 (2018) 323–330.
  https://doi.org/10.1016/j.jnucmat.2018.02.024

- White and Tucker, *J. Nucl. Mater.*, 118 (1983) 1–38.
  https://doi.org/10.1016/0022-3115(83)90176-9
