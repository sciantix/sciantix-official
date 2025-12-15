Models
======

This section describes the physical and empirical models implemented in SCIANTIX. 
Each model represents a specific phenomenon relevant to nuclear fuel behaviour and is documented individually in the following sections.

Burnup
------

This model computes the local fuel burnup from the fission rate density. The
specific power is derived from the fission rate and fuel density and is then
time-integrated to obtain the burnup expressed in MWd/kg. The model also updates
related irradiation quantities, including the irradiation time and the fraction
of initial metal atoms (FIMA).

Chromium Solubility
-------------------

This model describes the solubility of chromium in the fuel matrix and its
influence on fuel thermochemistry.

Densification
-------------

This model accounts for fuel densification during irradiation as a result
of pore shrinkage and microstructural evolution.

Effective Burnup
----------------

This model defines an effective burnup used to account for local or
phenomenological effects beyond the nominal burnup.

Gap Partial Pressure
--------------------

This model computes the gas pressure in the fuel–cladding gap.

Gas Decay
---------

This model describes the radioactive decay of gaseous species.

Gas Diffusion
-------------

This model describes intragranular fission gas diffusion in the fuel matrix.

Gas Production
--------------

This model accounts for the production rate of fission gases during
irradiation.

Gas Release
-----------

This model describes the release of fission gases from the fuel to the gap.

Grain Boundary Microcracking
----------------------------

This model accounts for microcrack formation at grain boundaries and its
effect on gas transport.

Grain Boundary Sweeping
-----------------------

This model describes the sweeping of intragranular gas towards grain
boundaries during grain growth.

Grain Boundary Venting
----------------------

This model accounts for gas venting along grain boundaries.

Grain Growth
------------

This model describes the evolution of grain size during irradiation.

High Burnup Structure Formation
--------------------------------

This model accounts for the formation of the high burnup structure (HBS).

High Burnup Structure Porosity
-------------------------------

This model describes porosity evolution within the high burnup structure.

Intergranular Bubble Behavior
-----------------------------

This model describes the behaviour of gas bubbles at grain boundaries.

Intragranular Bubble Behavior
-----------------------------

This model describes the behaviour of gas bubbles within fuel grains.

Microstructure
--------------

This model represents the evolution of the fuel microstructure.

Stoichiometry Deviation
-----------------------

This model accounts for deviations from stoichiometric UO₂.

UO₂ Thermochemistry
-------------------

This model describes the thermochemical behaviour of uranium dioxide.