Solvers
=======

SCIANTIX relies on a set of numerical solvers capable of handling Ordinary Differential Equations (ODEs), Partial Differential Equations (PDEs), and non-linear equations. These solvers are implemented in the ``Solver`` class.

.. toctree::
   :maxdepth: 1

ODE Solvers
-----------

The code includes specific solvers for first-order ODEs of the form:

.. math::

    \frac{dy}{dt} = S - L y

where :math:`S` is a source term and :math:`L` is a loss (decay) rate.

Integrator
~~~~~~~~~~

Solves the simple integration :math:`y' = S` (i.e., :math:`L=0`) using a forward Euler scheme:

.. math::

    y^{n+1} = y^n + S \Delta t

**Usage**: Used for cumulative quantities like burnup, gas production, and effective burnup.

Decay
~~~~~

Solves the decay equation :math:`y' = S - L y` using an analytical solution over the time step (assuming constant coefficients):

.. math::

    y^{n+1} = \frac{y^n + S \Delta t}{1 + L \Delta t}

**Usage**: Used for radioactive decay, grain boundary micro-cracking retention, and intragranular bubble evolution.

Limited Growth
~~~~~~~~~~~~~~

Solves a logistic-type growth equation :math:`y' = k/y + S`.

Binary Interaction
~~~~~~~~~~~~~~~~~~

Solves equations of the form :math:`y' = -k y^2` representing binary interactions (e.g., bubble coalescence).

PDE Solvers
-----------

SCIANTIX solves diffusion-reaction equations using a **Spectral Method**.

Spectral Diffusion
~~~~~~~~~~~~~~~~~~

Solves the diffusion equation in spherical symmetry:

.. math::

    \frac{\partial C}{\partial t} = D \frac{1}{r^2} \frac{\partial}{\partial r} \left( r^2 \frac{\partial C}{\partial r} \right) - \beta C + S

The spatial dependence is handled by projecting the concentration :math:`C(r,t)` onto a basis of eigenfunctions of the Laplacian operator (diffusion modes). This transforms the PDE into a system of decoupled ODEs for the mode amplitudes, which are then solved analytically over the time step.

Coupled Diffusion
~~~~~~~~~~~~~~~~~

- **SpectralDiffusion2equations**: Solves a system of 2 coupled diffusion equations.
- **SpectralDiffusion3equations**: Solves a system of 3 coupled diffusion equations.

Non-linear Solvers
------------------

Newton-Blackburn
~~~~~~~~~~~~~~~~

Solves the Blackburn thermochemical model equation using the Newton-Raphson method to find the equilibrium stoichiometry deviation :math:`x` (in :math:`UO_{2+x}`) given temperature and oxygen partial pressure.

Newton-Langmuir
~~~~~~~~~~~~~~~

Solves the Langmuir-based stoichiometry deviation model using the Newton-Raphson method.

Quartic Equation
~~~~~~~~~~~~~~~~

Solves a quartic equation :math:`ax^4 + bx^3 + cx^2 + dx + e = 0` using Newton's method. Used in the grain growth model.

Linear Algebra
--------------

The ``Solver`` class provides utility methods for linear algebra, such as:

- **Laplace**: Solves systems of linear equations of size NxN using Cramer's rule / Laplace expansion (optimized for N=2 and N=3).
- **Dot Product**: Computes dot products for 1D vectors and 2D matrices.