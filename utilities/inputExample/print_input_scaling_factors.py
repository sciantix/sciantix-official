"""
This is a python script to write the SCIANTIX optional input file "input_scaling_factors.txt".

Every entry is written as "<value>  # <Key> (<description>)" and SCIANTIX looks it up by <Key>,
so the order of the lines is irrelevant and a factor left out defaults to 1.0.
See utilities/InputExplanation.md.

@author G. Zullo

"""

factors = [
    ("sf_resolution_rate", "resolution rate"),
    ("sf_trapping_rate", "trapping rate"),
    ("sf_nucleation_rate", "nucleation rate"),
    ("sf_diffusivity", "diffusivity"),
    ("sf_temperature", "temperature"),
    ("sf_fission_rate", "fission rate"),
    ("sf_diffusion_based_release", "diffusion-based release"),
    ("sf_helium_production_rate", "helium production rate"),
    ("sf_grain_boundary_energy", "grain-boundary energy"),
    ("sf_fabricated_porosity", "fabricated porosity"),
    ("sf_cs_production", "Cs production"),
]

with open('input_scaling_factors.txt', 'w') as file:
    for key, description in factors:
        file.write(f"1.0    # {key} (scaling factor - {description})\n")
