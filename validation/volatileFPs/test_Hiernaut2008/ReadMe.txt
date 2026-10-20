Volatile fission product behaviour during thermal annealing of irradiated UO2 fuel oxidised up to U3O8
Authors: J.-P. Hiernaut, T. Wiss, D. Papaioannou, R.J.M. Konings, V.V. Rondinella
Journal of Nuclear Materials 372 (2008) 215-225
doi:10.1016/j.jnucmat.2007.03.174

SAMPLE
 - "as-stored" sample (not oxidised, O/U developed during irradiation and storage)
 - small pieces (10-20 mg) of BWR UO2 with 65 GWd/t taken from a region close to the cladding, same pellet as
   Colle et al. (2006), irradiated at temperatures below 800 K
 
KNUDSEN CELL
 - tungsten cell, described in Colle et al. (2006) (reference [4] of the paper): aperture variable between 0.3 and
   1 mm; the aperture of this experiment is not given, the maximum (1 mm) is taken, as for test_Colle2005
 - samples of 10-20 mg 
 - input_thermochemistry_settings.txt: matrix.vaporisation_orifice_diameter = 1.0e-3 (m),
   matrix.vaporisation_sample_mass = 15.0e-6 (kg), matrix.vaporisation_clausing_factor = 1.0

THERMAL ANNEALING
 - Knudsen cell (tungsten cell) in vacuum, from 300 K up to 2800 K, complete vaporisation of the sample
 - heating rate 30 K/min = 1800 K/h ("A heating rate of 30 K/min was used in all experiments")
 - in input_history.txt: base irradiation 53400 h at 800 K, 1e19 fiss/(m3 s); linear ramp 300 -> 2800 K
   between 53400.100 h and 53401.489 h (2500 K / 1800 K/h)
 - the system pressure used by the grain-boundary thermochemistry during annealing is 1e3 Pa, as in the Colle
   case 

RESULTS
Fig. 4. (a) Change of the oxidation level of a non pre-oxidised irradiated UO2 sample, O/U, as a function of absolute
temperature during the annealing in the Knudsen cell in vacuum up to its total vaporisation. (b) Corresponding mass
spectrometry signals: release rate of Cs, I, Te, BaO, Sr, NdO as a function of the absolute temperature.
exp_data/{Cs137,I129,Te130}.txt: release rate (kg/s) vs temperature (K), from Fig. 4(b)

NB. The ionisation electron energy was 40 eV, sufficient to ionise all the species released, but too high to avoid
dissociation of complex molecules present in the vapour (such as CsOx, CsTe, Cs2MoO4).

REFS
[3] J.-P. Hiernaut, J.-Y. Colle, R. Pflieger-Cuvellier, J. Jonnet, J. Somers, C. Ronchi, J. Nucl. Mater. 344 (2005) 246.