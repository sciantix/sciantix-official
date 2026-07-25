"""
This is a python script to write the SCIANTIX default input file "input_initial_conditions.txt".

@author G. Zullo

The blocks are positional: they are written in the order in which InputReading.C consumes
them, each value line followed by its comment line. Adding, removing or reordering a block
here without doing the same there shifts every following block.

Note that InputReading.C tolerates a file that stops early: the trailing blocks are then
left at zero, because a failed extraction at end of file is not an error. That tolerance
is why a missing block produces no diagnostic, so the count is asserted below instead.
"""

# Number of blocks consumed by InputReading.C, from "Grain radius[0]" to "Chromium content".
EXPECTED_BLOCKS = 14

# (values, description) — one entry per block read by InputReading.C, in order
INITIAL_CONDITIONS = [
    ('5.0e-06',
     'initial grain radius (m)'),
    ('0.0\t0.0\t0.0\t0.0\t0.0\t0.0',
     'initial Xe (at/m3) produced, intragranular, intragranular in solution, '
     'intragranular in bubbles, grain boundary, released'),
    ('0.0\t0.0\t0.0\t0.0\t0.0\t0.0',
     'initial Kr (at/m3) produced, intragranular, intragranular in solution, '
     'intragranular in bubbles, grain boundary, released'),
    ('0.0\t0.0\t0.0\t0.0\t0.0\t0.0',
     'initial He (at/m3) produced, intragranular, intragranular in solution, '
     'intragranular in bubbles, grain boundary, released'),
    ('0.0\t0.0',
     'initial intragranular bubble concentration (bub/m3), radius (m)'),
    ('0.0',
     'initial fuel burn-up (MWd/kgUO2)'),
    ('0.0',
     'initial fuel effective burn-up (MWd/kgUO2)'),
    ('0.0',
     'initial irradiation time (h)'),
    ('10641.0',
     'initial fuel density (kg/m3)'),
    ('0.0\t3.0\t0.0\t0.0\t97.0',
     'initial U234 U235 U236 U237 U238 (% of heavy atoms) content'),
    ('0.0\t0.0\t0.0\t0.0\t0.0\t0.0\t0.0',
     'initial Xe133 (at/m3) produced, intragranular, intragranular in solution, '
     'intragranular in bubbles, decayed, grain boundary, released'),
    ('0.0\t0.0\t0.0\t0.0\t0.0\t0.0\t0.0',
     'initial Kr85m (at/m3) produced, intragranular, intragranular in solution, '
     'intragranular in bubbles, decayed, grain boundary, released'),
    ('0.0',
     'initial fuel stoichiometry deviation (/)'),
    ('0.0',
     'initial chromium content (ppm)'),
]

if len(INITIAL_CONDITIONS) != EXPECTED_BLOCKS:
    raise ValueError(
        f'{len(INITIAL_CONDITIONS)} blocks defined, but InputReading.C reads {EXPECTED_BLOCKS}: '
        'a missing block is silently left at zero rather than reported'
    )

with open('input_initial_conditions.txt', 'w') as file:
    for values, description in INITIAL_CONDITIONS:
        file.write(f'{values}\n')
        file.write(f'#\t{description}\n')
