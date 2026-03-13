Minimal regression case for comparing oxygen thermodynamics in the U-O system.

Focus:
- Blackburn oxygen partial pressure / potential
- CALPHAD oxygen partial pressure / potential from OpenCalphad
- direct comparison along a temperature ramp for selected initial stoichiometry deviations

Inputs are intentionally reduced to the matrix U-O system only.
The thermodynamic database reference is `UO.TDB`.

Useful outputs:
- `output.txt`: SCIANTIX main variables, including Blackburn and CALPHAD pO2 / oxygen potentials
- `thermochemistry_output.txt`: selected U-O phase amounts
- `thermochemistry_matrix.py`: post-processing script dedicated to the pO2 and oxygen-potential comparison
