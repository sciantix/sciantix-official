Comparative study of fission product release of homogeneous and heterogeneous high-burn up MOX fuel by Knudsen
Effusion Mass Spectrometry supported by EPMA, SEM/TEM and thermal diffusivity investigations
Authors: J.-Y. Colle, T. Wiss, O. Dieste, P. Poeml, D. Staicu, T. Tverberg, S. Bremier, R.J.M. Konings,
         V.V. Rondinella, T. Sonoda, A. Sasahara, S. Kitajima
Journal of Nuclear Materials 578 (2023) 154340
doi:10.1016/j.jnucmat.2023.154340

SAMPLE (Table 1, MOX2)
 - heterogeneous MOX (direct powder mixing, PuO2-rich agglomerates of 32-63 um in a UO2 matrix with ~10 um grains),
   IFA-655 rod 12, Halden reactor (2001-2006)
 - Pu 16 wt.%HM; Pu vector 238/239/240/241/242 = 0.059/85.85/13.25/0.57/0.27 %; U vector 234/235/236/238 =
   0.036/0.26/0.007/99.73 %; density 96 %TD; O/M 1.998
 - target irradiation temperature 973 K, burnup 118 MWd/kgHM, discharge linear power 51.7 kW/m
 - fission gas release during irradiation 47 % (in-pile pressure of the sibling rodlet), 60 % (puncture)
 - KEMS sample: one fragment of 8.6 mg, heated at 10 K/min up to complete vaporisation (March 2012)

KNUDSEN CELL
 - "A tungsten Knudsen cell with an internal height of 12 mm and an internal diameter of 7 mm was used. The effusion
   aperture had a diameter of 0.5 mm and a length of less than 0.2 mm." Sample of 8.6 mg (MOX1: 16.8 mg).
 - input_thermochemistry_settings.txt: matrix.vaporisation_orifice_diameter = 0.5e-3 (m),
   matrix.vaporisation_sample_mass = 8.6e-6 (kg), matrix.vaporisation_clausing_factor = 1.0. The aperture is
   less than 0.2 mm long: for length / radius = 0.8 the Clausing factor would be ~0.7, which moves the calculated
   temperatures below by +30 K (peak of the rate 2600 K, complete vaporisation 2615 K).
 - this is the only case of the group where the geometry of the cell and the mass of the sample are both given

THERMAL ANNEALING
 - heating rate 10 K/min = 600 K/h up to complete vaporisation of the matrix at around 2600 K
 - in input_history.txt: base irradiation 28245 h at 973 K, 3e19 fiss/(m3 s) (104 MWd/kg of oxide = 118 MWd/kgHM),
   1e6 Pa; linear ramp 300 -> 2800 K at 600 K/h from 28245.001 h (one history point every 250 K, 2.5 K per step)
 - the system pressure used by the grain-boundary thermochemistry during annealing is 1e3 Pa, as in the other
   cases of the group (the experiment is in vacuum)

EXPERIMENTAL DATA
 - exp_data/Nd.txt: release intensity of Nd(Ox) of MOX2 against temperature, normalised to its maximum, from Fig. 6. Nd is immobile and non-volatile, its release follows the vaporisation of the matrix.
 - exp_data/{Kr,Xe,I,Cs,Te}.txt: normalised fractional release (NFR, Eq. (5): release normalised to the value at the end of the release) of MOX2 against temperature, from Figs. 9 (Kr), 10 (Xe), 12 (I), 13 (Cs), 14 (Te), 

