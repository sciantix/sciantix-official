Grain Growth
============

This model updates the fuel grain radius during the simulation. Grain growth is
controlled by the input option ``iGrainGrowth`` and is solved by building a
parameter vector and passing it to ``solver.QuarticEquation(...)`` (see
``Simulation::GrainGrowth()``).

At the end of the update, the matrix grain radius is synchronised through:

- ``matrices["UO2"].setGrainRadius( Grain radius )``

Activation and options
----------------------

The model behaviour is selected with:

- ``iGrainGrowth`` (input option)

Implemented options:

- **0**: constant grain radius
- **1**: Ainscough et al. (1973), temperature-driven growth with burnup retardation
- **2**: Van Uffelen et al. (2013), temperature-driven growth up to a limiting size

Main variables
--------------

Inputs used by the model (depending on the option):

- ``Grain radius`` (sciantix variable): initial/final grain radius
- ``Temperature`` (history variable)
- ``Burnup`` (sciantix variable) (option 1)
- ``Time step`` (physics variable) (used to build the solver parameters)
- ``matrices["UO2"].getGrainBoundaryMobility()`` (material property used as a rate constant)

Output:

- ``Grain radius`` updated at the end of the step

Option 0: constant grain radius
-------------------------------

For ``iGrainGrowth = 0``, the grain radius is kept constant. The parameter vector
is set so that the quartic solver returns the initial grain radius.

Reference: constant grain radius.

Option 1: Ainscough et al. (1973)
---------------------------------

For ``iGrainGrowth = 1``, grain growth depends on temperature and is progressively
retarded by burnup (fission product accumulation).

Limiting radius and burnup factor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A temperature-dependent limiting grain radius is computed:

.. math::

   R_{\mathrm{lim}} = 2.23\times 10^{-3}\,\frac{1.56}{2}\,
   \exp\!\left(-\frac{7620}{T}\right)

A burnup factor is defined as:

.. math::

   f_B = 1 + 2\,\frac{B}{0.8815}

where:

- :math:`T` is ``Temperature`` (K)
- :math:`B` is ``Burnup``

Growth condition
~~~~~~~~~~~~~~~~

Growth is applied only if:

.. math::

   R^{n} < \frac{R_{\mathrm{lim}}}{f_B}

If this condition is not satisfied, the parameters are set to keep the grain
radius constant over the step.

Rate constant used in the step
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When growth is active, a rate constant is built from the grain boundary mobility
and a correction term:

.. math::

   k = M_{\mathrm{gb}}\left(1 - f_B\,\frac{R^{n+1}}{R_{\mathrm{lim}}}\right)

where :math:`M_{\mathrm{gb}} =` ``matrices["UO2"].getGrainBoundaryMobility()``.

The time step ``Time step`` is included in the parameter vector through the term
``-k * Time step``.

Reference
~~~~~~~~~

Ainscough et al., *J. Nucl. Mater.* 49 (1973) 117–128.
https://www.osti.gov/biblio/4322065

Option 2: Van Uffelen et al. (2013)
-----------------------------------

For ``iGrainGrowth = 2``, grain growth is applied up to a temperature-dependent
limiting radius.

Limiting radius
~~~~~~~~~~~~~~~

.. math::

   R_{\mathrm{lim}} = \frac{3.345\times 10^{-3}}{2}\,
   \exp\!\left(-\frac{7620}{T}\right)

Growth condition
~~~~~~~~~~~~~~~~

If:

.. math::

   R^{n} < R_{\mathrm{lim}}

then a growth step is applied using a rate constant derived from the grain
boundary mobility:

- :math:`k =` ``matrices["UO2"].getGrainBoundaryMobility()``

Otherwise, the grain radius is kept constant.

Reference
~~~~~~~~~

Van Uffelen et al., *J. Nucl. Mater.* 434 (2013) 287–290.
https://doi.org/10.1016/j.jnucmat.2012.11.053

Numerical note
--------------

In all cases, the update is performed by calling:

.. code-block:: cpp

   sciantix_variable["Grain radius"].setFinalValue(
       solver.QuarticEquation(model["Grain growth"].getParameter())
   );

So the model builds coefficients/constraints in the parameter vector and delegates
the actual step update to the quartic-equation solver.
