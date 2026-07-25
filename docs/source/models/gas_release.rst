Gas Release
===========

Purpose
-------

This routine computes released gas inventories and standard release metrics
from the current state of the fuel microstructure.

It does not solve transport equations; instead, it post-processes the gas
inventories after diffusion, trapping, and grain-boundary models have been
applied.

Physical meaning
----------------

At each time step, fission gases are partitioned into:

- produced
- decayed (radioactive gases)
- retained in the grain
- retained at grain boundaries
- released to the free volume

The released fraction is obtained by mass conservation.

Algorithm
---------

For each gas species *i*:

1. Compute released inventory by mass balance:

   .. math::

      N_i^{rel} =
      N_i^{prod} - N_i^{dec} - N_i^{grain} - N_i^{GB}

2. Enforce non-negativity:

   .. math::

      N_i^{rel} \ge 0

3. Compute intergranular gaseous swelling:

   .. math::

      S_{IG} =
      \frac{3}{R_g} N_b V_b

4. Compute integral release metrics (FGR, R/B).

Outputs
-------

- ``Xe released``, ``Kr released``
- ``Fission gas release``
- ``Xe133 R/B``, ``Kr85m R/B``
- ``Helium fractional release``
- ``Helium release rate``

Numerical notes
---------------

- Release is computed only for non-restructured matrix systems.
- Division-by-zero conditions are explicitly guarded.
- Negative released inventories are clipped to zero.

Relation to other models
------------------------

This routine depends on:

- Intragranular diffusion
- Grain-boundary bubble evolution
