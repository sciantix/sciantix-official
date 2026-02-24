Overview
========

What is SCIANTIX?
-----------------

SCIANTIX is an open-source 0D simulation code developed at Politecnico di Milano, designed to model the behavior of a single grain of nuclear fuel, with a particular focus on fission gas behavior.
Currently, SCIANTIX is validated against experimental data for the following phenomena:

- **Intragranular gaseous swelling**: Swelling caused by gas bubbles within the fuel grain
- **Intergranular gaseous swelling**: Swelling at grain boundaries due to bubble growth and coalescence
- **Fission gas release**: Diffusion and release of gaseous fission products (Xe, Kr) from the fuel
- **Helium behavior**: Diffusion, solubility, and thermal re-solution of helium under annealing conditions
- **High-burnup structure (HBS) formation**: Evolution of porosity at high burnups

SCIANTIX employs **physics-based rate-theory models** rather than empirical correlations, enabling better integration with lower-length scale calculations and improved predictive capability. The engineering-compatible design allows SCIANTIX to operate both as an independent tool for separate-effect calculations and as an embedded module within industrial fuel performance codes (FPCs) such as TRANSURANUS, FRAPCON/FRAPTRAN, and OFFBEAT.

Conceptual Introduction
-----------------------

**Physics-Based Modeling Approach**

Traditional fuel performance codes rely on empirical correlations for modeling fission gas release and swelling, which are often calibrated for specific datasets and reactor conditions. SCIANTIX addresses this limitation by implementing mechanistic rate-theory models based on fundamental physical principles:

- **Intragranular diffusion**: Models the migration of fission gas atoms within the fuel grain matrix
- **Trapping and re-solution**: Accounts for gas atoms being trapped at defects and subsequently released through irradiation
- **Bubble nucleation and growth**: Describes the formation and expansion of gas-filled bubbles
- **Grain-boundary bubble evolution**: Models coalescence, saturation, and rupture at grain boundaries
- **Decay chains**: Includes first-precursor enhancement for short-lived radioactive fission gases

**Meso-Scale Framework**

SCIANTIX bridges the gap between atomistic simulations (which are computationally intensive) and engineering-scale fuel performance codes. As a meso-scale solver, it provides:

- **Mechanistic transparency**: Clear physical models with verifiable parameters
- **Computational efficiency**: Suitable for routine integration within multi-physics simulations
- **Numerical robustness**: First-order L-stable implicit time integrators and spectral diffusion solvers
- **Flexibility**: Modular design supporting alternative models and solver approaches

**Helium Behavior**

Beyond fission gases, SCIANTIX includes dedicated models for helium behavior, which is critical in advanced fuel concepts and under annealing conditions:

- **Helium diffusion**: Models temperature-dependent migration of helium atoms within the fuel matrix
- **Solubility**: Accounts for helium solubility in the fuel and its variation with burnup and temperature
- **Trapping and de-trapping**: Describes interaction between helium and fuel defects
- **Thermal re-solution**: Models the release of trapped helium through thermal annealing, important for transient conditions
- **Bubble formation**: Tracks helium bubble nucleation and growth at grain boundaries
- **Experimental validation**: Validated against experimental data from helium release behavior under annealing conditions

This mechanism is particularly important for understanding fuel behavior in advanced designs and during reactor transients where thermal annealing plays a significant role.

Architecture
------------

**Object-Oriented Design**

SCIANTIX is implemented in modern C++ (C++17 standard) with a fully object-oriented architecture. Key design features include:

- **Modularity**: Core simulation entities (matrices, gases, systems, models, solvers) are implemented as independent classes
- **Separation of concerns**: Solvers and models are kept separate, supporting independent verification and validation
- **Extensibility**: Clear class hierarchies and interfaces enable the addition of new models and solvers

**Core Components**

The SCIANTIX codebase is organized into several primary modules:

1. **Classes** (`classes/`): Fundamental simulation objects
   
   - Matrix: Properties of the fuel material
   - Gas species: Physical and chemical properties of fission gases
   - System: Collections of gas species and their interactions
   - Models: Physics-based implementations of gas behavior
   - Solvers: Numerical algorithms for solving differential equations

2. **Solvers** (`solvers.h`): Numerical solution methods
   
   - **Spectral diffusion solver**: A meshless approach for solving Booth-type diffusion problems with controlled numerical error
   - **Time integrators**: L-stable implicit schemes ensuring stability across wide time scales
   - **Operator splitting**: Segregated schemes for efficient coupling with external codes

3. **Models**: Physics-based rate-theory formulations
   
   - Diffusion models at different burnup regimes
   - Bubble nucleation and growth kinetics
   - Grain-boundary phenomena and fission gas release mechanisms
   - HBS porosity evolution

4. **File Management** (`file_manager/`): Input/output handling
   
   - Parsing simulation parameters and initial conditions
   - Writing output files and results

5. **Coupling Interface** (`coupling/`): External code integration
   
   - API for embedding SCIANTIX as a static library within fuel performance codes
   - Controlled data exchange with thermo-mechanical solvers

6. **Operations** (`operations/`): Utility functions and computational routines

Workflow
--------

**Simulation Stages**

A typical SCIANTIX simulation follows these stages:

1. **Initialization**
   
   - Read input files containing:
     - Simulation settings (time stepping, convergence criteria)
     - Scaling factors for model parameters
     - Initial conditions (temperature history, burnup rate, fuel properties)
   - Create and initialize simulation objects (matrix, gas species, systems)
   - Set up numerical solvers and time integrators

2. **Gas Behavior Calculations**
   
   - Solve diffusion equations for intragranular gas distribution
   - Compute trapping and re-solution rates
   - Model intra- and inter-granular bubble evolution
   - Calculate fission gas release rates

3. **Microstructural Updates**
   
   - Update fuel matrix properties (defect densities, porosity)
   - Track swelling from gaseous and non-gaseous sources
   - Model high-burnup structure formation and porosity evolution

4. **Environmental Interactions**
   
   - Apply temperature changes
   - Update irradiation conditions (fission density, neutron flux)
   - Handle thermal or mechanical annealing effects

5. **System Evolution**
   
   - Advance all dependent variables in time
   - Update gas release fractions and bubble populations
   - Compute cumulative swelling and pressure build-up

6. **Output Generation**
   
   - Write results at specified time steps
   - Generate output files with fuel properties and gas behavior predictions
   - Store history information for subsequent time steps

**Input Files Structure**

SCIANTIX requires three main input files that define the simulation setup, history, and initial conditions:

1. **input_settings.txt**: Model selection and solver configuration
   
   A single-column text file with integer flags selecting which physics models and numerical methods to use. Example structure:
   
   .. code-block:: plaintext
   
       1    #    iGrainGrowth (0= no, 1= Ainscough, 2= Van Uffelen)
       1    #    iFissionGasDiffusivity (0= constant, 1= Turnbull et al.)
       1    #    iDiffusionSolver (1= SDA with quasi-stationarity, 2= without)
       1    #    iIntraGranularBubbleBehavior (1= Pizzocri et al.)
       ...
   
   Each line contains an integer flag and a comment explaining the available options.

2. **input_history.txt**: Time-dependent conditions
   
   A four-column text file defining the simulation history with linear interpolation between specified points:
   
   .. code-block:: plaintext
   
       Time(h)    Temperature(K)    FissionRate(fiss/m³-s)    HydrostaticStress(MPa)
       0          1273              1e19                      0
       5500       1273              1e19                      0
   
   The code automatically interpolates between these rows using a fixed number of time steps.

3. **input_initial_conditions.txt**: Physical parameters and initial state
   
   A structured text file with values followed by comments:
   
   .. code-block:: plaintext
   
       5.0e-06
       # initial grain radius (m)
       0.0 0.0 0.0 0.0 0.0 0.0
       # initial Xe (at/m³): produced, intragranular, in solution, in bubbles, grain boundary, released
       0.0 0.0 0.0 0.0 0.0 0.0
       # initial Kr (at/m³): produced, intragranular, in solution, in bubbles, grain boundary, released
       0.0 0.0
       # initial He (at/m³): produced, intragranular [+ in solution, in bubbles, grain boundary, released]
       ...
   
   Each data line is followed by a comment line (starting with `#`) describing the parameters.

4. **input_scaling_factors.txt** (Optional): Parameter adjustment
   
   A single-column text file with scaling factors applied to model parameters:
   
   .. code-block:: plaintext
   
       1.0
       # scaling factor - resolution rate
       1.0
       # scaling factor - trapping rate
       1.0
       # scaling factor - nucleation rate
       ...

**Input and Output**

- **Inputs**: Temperature history, irradiation conditions, fuel properties, initial conditions, simulation settings, and optional parameter scaling factors
- **Outputs**: Fission gas release fractions, swelling estimates, temperature-dependent accelerations, bubble densities, burnup-dependent phenomena

For detailed input-file templates and examples, see the :doc:`examples` page and the utilities documentation at ``utilities/InputExplanation.md``.

**Numerical Stability and Efficiency**

SCIANTIX employs several strategies to balance accuracy and computational efficiency:

- **Implicit time integration**: Ensures stability for stiff equations governing diffusion at varying temperatures
- **Adaptive time stepping**: Automatically adjusts step sizes to maintain accuracy
- **Operator splitting**: Decouples independent physical processes for efficient coupling with external codes
- **Verification**: All solvers are verified using the Method of Manufactured Solutions (MMS)

**Validation**

The code is validated against experimental data through a comprehensive regression testing suite covering:

- Intragranular and intergranular swelling experiments
- Helium behavior under annealing
- Fission gas release measurements
- High-burnup structure characterization

Further Reading
---------------

For detailed information on specific models, solvers, and usage, refer to:

- :doc:`models`: Physics-based models implemented in SCIANTIX
- :doc:`solvers`: Numerical solution methods and verification
- :doc:`examples`: Practical examples and use cases
- :doc:`regression`: Validation against experimental data
- :doc:`api/index_api`: Application Programming Interface documentation
