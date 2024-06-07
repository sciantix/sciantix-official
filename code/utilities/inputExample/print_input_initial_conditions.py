"""
@author (G. Zullo)
This is a python script to write the SCIANTIX default input file "input_initial_conditions.txt".

"""

with open('input_initial_conditions.txt', 'w') as file:
    file.write('5.0e-06\n')
    file.write('#	initial grain radius (m)\n')
    file.write('0.0	0.0	0.0	0.0	0.0	0.0\n')
    file.write('#	initial Xe (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released\n')
    file.write('0.0	0.0	0.0	0.0	0.0	0.0\n')
    file.write('#	initial Kr (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released\n')
    file.write('0.0	0.0	0.0	0.0	0.0	0.0\n')
    file.write('#	initial He (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released\n')
    file.write('0.0 0.0\n')
    file.write('# initial intragranular bubble concentration (at/m3), radius (m)\n')
    file.write('0.0\n')
    file.write('#	initial fuel burn-up (MWd/kgUO2)\n')
    file.write('0.0\n')
    file.write('#	initial fuel effective burn-up (MWd/kgUO2)\n')
    file.write('0.0\n')
    file.write('#	initial irradiation time (h)\n')
    file.write('10641.0\n')
    file.write('#	initial fuel density (kg/m3)\n')
    file.write('0.0	3.0	0.0	0.0	97.0\n')
    file.write('#	initial U234 U235 U236 U237 U238 (% of heavy atoms) content\n')
    file.write('0.0	0.0	0.0	0.0	0.0	0.0 0.0\n')
    file.write('#	initial Xe133 (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released\n')
    file.write('0.0	0.0	0.0	0.0	0.0	0.0 0.0\n')
    file.write('#	initial Kr85m (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released\n')
    file.write('0.0\n')
    file.write('#   initial fuel stoichiometry deviation (\)')