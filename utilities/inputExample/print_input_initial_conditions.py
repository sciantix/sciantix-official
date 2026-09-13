"""
This is a python script to write the SCIANTIX default input file "input_initial_conditions.txt".

Every entry is written as "<value(s)>  # <Key> (<description>)" and SCIANTIX looks it up by
<Key>, so the order of the lines is irrelevant and an entry left out takes its default (0).
See utilities/InputExplanation.md.

@author G. Zullo

"""

entries = [
    ("5.0e-06",                      "Grain_radius[0] (initial grain radius (m))"),
    ("0.0 0.0 0.0 0.0 0.0 0.0",      "Initial_composition_Xe (initial Xe (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released)"),
    ("0.0 0.0 0.0 0.0 0.0 0.0",      "Initial_composition_Kr (initial Kr (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released)"),
    ("0.0 0.0 0.0 0.0 0.0 0.0",      "Initial_composition_He (initial He (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released)"),
    ("0.0 0.0",                      "Initial_intragranular_bubbles (initial intragranular bubble concentration (bub/m3), radius (m))"),
    ("0.0",                          "Burn_up[0] (initial fuel burn-up (MWd/kgUO2))"),
    ("0.0",                          "Effective_burn_up[0] (initial fuel effective burn-up (MWd/kgUO2))"),
    ("0.0",                          "Irradiation_time[0] (initial irradiation time (h))"),
    ("10641.0",                      "Fuel_density[0] (initial fuel density (kg/m3))"),
    ("0.0 3.0 0.0 0.0 97.0",         "Initial_composition_U (initial U234 U235 U236 U237 U238 content (% of heavy atoms))"),
    ("0.0 0.0 0.0 0.0 0.0 0.0 0.0",  "Initial_composition_Xe133 (initial Xe133 (at/m3): produced, intragranular, in solution, in bubbles, decayed, grain boundary, released)"),
    ("0.0 0.0 0.0 0.0 0.0 0.0 0.0",  "Initial_composition_Kr85m (initial Kr85m (at/m3): produced, intragranular, in solution, in bubbles, decayed, grain boundary, released)"),
    ("0.0",                          "Initial_stoichiometry_deviation[0] (initial fuel stoichiometry deviation (/))"),
]

with open('input_initial_conditions.txt', 'w') as file:
    for values, comment in entries:
        file.write(f"{values:<32}# {comment}\n")
