Gas Diffusion
=============

This model computes the intra-granular diffusion of fission gases using a
spectral (Booth-type) formulation. Three solver configurations are available,
allowing increasing physical detail: a single-equation model, a coupled
solution–bubble model, and an extended formulation including high-burnup
structure (HBS) effects.

The model also performs mass balance to determine the gas concentration at the
grain boundary and accounts for optional immediate release.

Activation
----------

The diffusion model is executed when a diffusion solver option is selected via
the ``iDiffusionSolver`` input flag (values 1, 2 or 3). If the flag is not set
or set to a disabling value, the routine returns without updating diffusion
variables.

Inputs
------

The model uses:

- ``iDiffusionSolver`` (input flag: 1, 2, or 3)
- ``iGrainBoundaryBehaviour`` (input flag)
- ``Time step`` (physics variable)
- Gas production rate
- Gas decay rate
- Grain radius
- Diffusivities (solution and bubble)
- Resolution and trapping rates
- Restructured volume fraction (for HBS case)

Solver options
--------------

The solver is selected through:

::

   iDiffusionSolver = 1, 2, or 3

**Case 1 — One equation (effective diffusion)**

A single spectral diffusion equation is solved for the total intragranular gas:

.. math::

   \frac{\partial C}{\partial t}
   =
   D_{\mathrm{eff}} \nabla^2 C
   + S
   - \lambda C

where the effective diffusivity is:

.. math::

   D_{\mathrm{eff}}
   =
   \frac{R}{R+T} D_{\mathrm{fg}}
   +
   \frac{T}{R+T} D_{\mathrm{b}}

with:

- :math:`R` resolution rate
- :math:`T` trapping rate
- :math:`D_{\mathrm{fg}}` lattice diffusivity
- :math:`D_{\mathrm{b}}` bubble diffusivity

The solution is obtained via:

::

   solver.SpectralDiffusion(...)

The total gas is partitioned into solution and bubbles according to the
equilibrium fraction:

.. math::

   f_{\mathrm{eq}} = \frac{R}{R+T}

**Case 2 — Two coupled equations**

Separate spectral equations are solved for:

- Gas in intragranular solution
- Gas in intragranular bubbles

The solver:

::

   solver.SpectralDiffusion2equations(...)

updates both populations simultaneously.

The total intragranular gas is:

.. math::

   C_{\mathrm{grain}} =
   C_{\mathrm{solution}} + C_{\mathrm{bubbles}}

**Case 3 — Three coupled equations (Xe with HBS)**

This configuration is available for xenon in UO₂ with high-burnup structure.

Three populations are solved:

- Intragranular solution
- Intragranular bubbles
- HBS region

The solver:

::

   solver.SpectralDiffusion3equations(...)

includes:

- Separate diffusivities in UO₂ and HBS
- Production rates in both regions
- Resolution and trapping
- Radioactive decay
- A restructuring sweeping term

The sweeping term is defined as:

.. math::

   S_{\mathrm{sweep}} =
   \frac{1}{1 - f_r}
   \frac{\Delta f_r}{\Delta t}

where :math:`f_r` is the restructured volume fraction.

Grain boundary mass balance
---------------------------

For each gas species (non-HBS systems), the grain-boundary concentration is
computed by mass conservation:

.. math::

   C_{\mathrm{gb}} =
   C_{\mathrm{prod}}
   - C_{\mathrm{dec}}
   - C_{\mathrm{grain}}
   - C_{\mathrm{released}}

Negative values are set to zero.

Immediate release option
------------------------

If:

::

   iGrainBoundaryBehaviour = 0

all gas at the grain boundary is instantaneously released:

.. math::

   C_{\mathrm{released}} =
   C_{\mathrm{prod}}
   - C_{\mathrm{dec}}
   - C_{\mathrm{grain}}

and the grain-boundary concentration is reset to zero.

Numerical method
----------------

All diffusion equations are solved using a spectral (Booth-type) formulation
with a finite number of modes:

.. math::

   C(r,t) =
   \sum_{k=1}^{N}
   a_k(t) \frac{\sin(k \pi r / a)}{r}

where:

- :math:`N` is the number of modes
- :math:`a` is the grain radius

The spectral formulation provides a meshless and computationally efficient
solution compatible with online coupling to fuel performance codes.

Outputs
-------

- ``<Gas> in intragranular solution``
- ``<Gas> in intragranular bubbles``
- ``<Gas> in HBS`` (when Case 3 is active)
- ``Grain-boundary concentration``
- ``Released inventory`` (when applicable)
