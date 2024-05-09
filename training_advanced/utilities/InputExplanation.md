# **Input file example**

In this folder are reported the three essential input files required from SCIANTIX

 - `input_settings.txt` contains the choice of the models and numerical solvers employed in the simulation
 - `input_history.txt` containes the time, temperature (K), fission rate (fiss / m3-s)$, and hydrostatic stress (MPa) as a function of time (hr)
 - `input_initial_conditions.txt` provides the code with several initial conditions
 - `input_scaling_factors.txt`optional file with scaling factors

Below we detail each file content.

# Input settings

The provided input settings are the **recommended** choice for standard simulations of uranium dioxide.

1 # grain growth -- The default model for grain growth according to *Ainscough et al., J. Nucl. Mater. 49 (1978) 117* is selected.

1 # fission gas (xenon and krypton) diffusivity -- The selected correlation for single gas atom intra-granular diffusion coefficient is the one by *J.A.Turnbull et al., IWGFPT--32, 1988*. It considers three terms contributing to gas diffusivity, representing thermally activated, irradiation-enhanced, and a-thermal mechanism.

1 #  solver for intragranular diffusion -- Intra-granular diffusion equation is solved through a Spectral Diffusion Algoritm. This formulation entails the solution of a single equation for gas populations in the grain, enforcing the so-called effective diffusion coefficient (cfr. *M.V. Speight, Nucl. Sci. Eng. 37 (1969) 180*).

1 #  intragranular bubble evolution -- Selection of mechanistic intra-granular inert gas behaviour model, featuring a direct description of intra-granular bubble nucleation, re-solution, gas trapping into intra-granular bubbles and diffusion towards grain boundaries. The model is taken from *Pizzocri et al., J. Nucl. Mater. 502 (2018) 323*.

1 #  re-solution -- Heterogeneous bubble re-solution mechanism, irradiaion-induced for fission gases, i.e. *en-bloc* destruction of bubbles  interacting with fission fragments. The re-solution rate is calculated according to *J.A. Turnbull, J. Nucl. Mater. 38 (1971), 203*.

1 #  trapping -- Trapping rate of single gas atoms into intra-granular bubbles according to the formulation of *F.S. Ham, J. Phys. Chem. Sol., 6 (1958) 335*.

1 #  nucleation -- Heterogeneous nucleation of intra-granular bubbles, i.e. a number of bubbles are created in the wake of fission fragments trails. The rate is calculated according to *Olander and Wongsawaeng, J. Nucl. Mater. 354 (2006), 94*.

1 #  output -- The output of the simulation is provided in a text file (`output.txt`), in which values are separated by **tabs**.

1 #  grain boundary vacancy diffusivity -- Grain-boundary diffusion coefficient of vacancies evaluated according to *G.L.Reynolds and B.Burton, J. Nucl. Mater. 82 (1979) 22*.

1 #  grain boundary behaviour of bubbles -- Inert gas behaviour is **considered**.


1 #  intra bubble_radius -- Mechanistic model of inert gas behaviour at grain boundaries. The model is taken from *Pastore et al., Nucl. Eng. Des. 256 (2013) 75* and *Barani et al., J. Nucl. Mater. 486 (2017) 96*. A direct description of lenticular-shaped grain-boundary bubbles is enforced, encompassing their growth due to gas and vacancies inflow, interconnection, and describing as inherently coupled gaseous swelling and fission gas release.

1 #  grain boundary micro-cracking -- Burst release of fission gas during temperature transients is considered, according to the model by *Barani et al., J. Nucl. Mater. 486 (2017) 96*. The model features a semi-empirical description of grain-boundary micro-cracking  and is nested on the general grain-boundary gas behaviour model.

0	#	fuel matrix -- UO2 matrix and properties are considered

0 #  grain boundary venting -- if 1, the gas release consider the contribution from the venting probability calculated according to *Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE*.

0 # hbs formation -- no HBS forming in the UO2 fuel matrix.

0 # radioactive fission gases behaviour 

0	#	iHelium (0= no model considered, 1= Cognini et al. (2021))

0	#	iHeDiffusivity (0= constant value, 1= Luzzi et al. (2018))

0	#	iGrainBoundarySweeping (0= no model considered, 1= TRANSURANUS swept volume model)

0	#	iHBS_FGDiffusionCoefficient (0= constant value)

0	#	iHighBurnupStructurePorosity (0= no evolution of HBS porosity, 1= HBS porosity evolution based on Spino et al. (2006) data)

# Input initial conditions

In this file, the user can provide initial conditions to some of the SCIANTIX state variables.

5.0e-06
\#	initial grain radius (m)
0.0	0.0	0.0	0.0	0.0	0.0
\#	initial Xe (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released
0.0	0.0	0.0	0.0	0.0	0.0
\#	initial Kr (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released
0.0	0.0	0.0	0.0	0.0	0.0
\#	initial He (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released
0.0 0.0
\# initial intragranular bubble concentration (at/m3), radius (m)
0.0
\#	initial fuel burn-up (MWd/kgUO2)
0.0
\#	initial fuel effective burn-up (MWd/kgUO2)
10960.0
\#	initial fuel density (kg/m3)
0.0	3.0	0.0	0.0	97.0
\#	initial U234 U235 U236 U237 U238 (% of heavy atoms) content
0.0	0.0	0.0	0.0	0.0	0.0
\#	initial Xe133 (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released
0.0	0.0	0.0	0.0	0.0	0.0
\#	initial Kr85m (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released

***
**PLEASE NOTE** that from the quantities below, it is **mandatory** to put the comment symbol (#) **below** each data series.
***

# Input history
In this file, a simplified temperature, fission rate (power), and hydrostatic stress history is contained. SCIANTIX interpolates the quantities among two consecutive values of each type, if differents. Every time step is internally subdivided in 100 intervals, at which the calculations are executed and outputs are provided. To change this value, the user must modify the source code.
**TIP**: this list of quantities **should not** terminate with a blank line. It can cause troubles to the input reading especially on UNIX OS. In case you will obtain a blank output file, this is most probably the issue.
***

# Input scaling factors

1.0
\# scaling factor - resolution rate
1.0
\# scaling factor - trapping rate
1.0
\# scaling factor - nucleation rate
1.0
\# scaling factor - diffusivity
1.0
\# scaling factor - screw parameter
1.0
\# scaling factor - span parameter
1.0
\# scaling factor - cent parameter
1.0
\# scaling factor - helium production rate

In case of any trouble with those files, please contact the main developers (D. Pizzocri, T. Barani and G. Zullo).
