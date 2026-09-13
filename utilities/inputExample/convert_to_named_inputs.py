"""
Migrates SCIANTIX input files written in the retired formats to the keyed format:

    <value(s)>    # <Key> (<description>)

- input_initial_conditions.txt and input_scaling_factors.txt: rewrites the old two-line
  layout (a bare value line followed by a "# description" line, in a fixed order);
- input_settings.txt: renames settings still labelled with names the code no longer
  accepts (e.g. iFissionGasDiffusivity -> iFissionProductDiffusivity).

SCIANTIX only reads the keyed format (src/file_manager/InputReading.C) and stops with an
error on an unconverted file. Converting a case does not change its results. Files already
in the keyed format are left as they are.

Usage:
    python3 utilities/inputExample/convert_to_named_inputs.py <case_dir> [<case_dir> ...]

author: Elisa Cappellari
"""
import os
import sys

# (key, number of values, default when an old file stops before it, description), in the
# order the old two-line layout lists them. The keys must match src/file_manager/InputReading.C.
INITIAL_CONDITIONS = [
    ("Grain_radius[0]", 1, "0.0", "initial grain radius (m)"),
    ("Initial_composition_Xe", 6, "0.0",
     "initial Xe (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released"),
    ("Initial_composition_Kr", 6, "0.0",
     "initial Kr (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released"),
    ("Initial_composition_He", 6, "0.0",
     "initial He (at/m3): produced, intragranular, in solution, in bubbles, grain boundary, released"),
    ("Initial_intragranular_bubbles", 2, "0.0",
     "initial intragranular bubble concentration (bub/m3), radius (m)"),
    ("Burn_up[0]", 1, "0.0", "initial fuel burn-up (MWd/kgUO2)"),
    ("Effective_burn_up[0]", 1, "0.0", "initial fuel effective burn-up (MWd/kgUO2)"),
    ("Irradiation_time[0]", 1, "0.0", "initial irradiation time (h)"),
    ("Fuel_density[0]", 1, "0.0", "initial fuel density (kg/m3)"),
    ("Initial_composition_U", 5, "0.0", "initial U234 U235 U236 U237 U238 content (% of heavy atoms)"),
    ("Initial_composition_Xe133", 7, "0.0",
     "initial Xe133 (at/m3): produced, intragranular, in solution, in bubbles, decayed, grain boundary, released"),
    ("Initial_composition_Kr85m", 7, "0.0",
     "initial Kr85m (at/m3): produced, intragranular, in solution, in bubbles, decayed, grain boundary, released"),
    ("Initial_stoichiometry_deviation[0]", 1, "0.0", "initial fuel stoichiometry deviation (/)"),
    ("Chromium_content", 1, "0.0", "chromium content"),
]
MOX_ONLY = [
    ("Initial_composition_Pu", 5, "0.0", "initial Pu238 Pu239 Pu240 Pu241 Pu242 content (% of heavy atoms)"),
    ("q", 1, "0.0", "Pu fraction of heavy metal (/)"),
]
SCALING_FACTORS = [
    ("sf_resolution_rate", 1, "1.0", "scaling factor - resolution rate"),
    ("sf_trapping_rate", 1, "1.0", "scaling factor - trapping rate"),
    ("sf_nucleation_rate", 1, "1.0", "scaling factor - nucleation rate"),
    ("sf_diffusivity", 1, "1.0", "scaling factor - diffusivity"),
    ("sf_temperature", 1, "1.0", "scaling factor - temperature"),
    ("sf_fission_rate", 1, "1.0", "scaling factor - fission rate"),
    ("sf_diffusion_based_release", 1, "1.0", "scaling factor - diffusion-based release"),
    ("sf_helium_production_rate", 1, "1.0", "scaling factor - helium production rate"),
    ("sf_grain_boundary_energy", 1, "1.0", "scaling factor - grain-boundary energy"),
    ("sf_fabricated_porosity", 1, "1.0", "scaling factor - fabricated porosity"),
    ("sf_cs_production", 1, "1.0", "scaling factor - Cs production"),
]


# Setting labels the code no longer accepts -> the key it reads.
RENAMED_SETTINGS = {
    "iFissionGasDiffusivity": "iFissionProductDiffusivity",
    "iFGDiffusionCoefficient": "iFissionProductDiffusivity",
    "iIntraGranularBubbleEvolution": "iIntraGranularBubbleBehavior",
}


def rename_settings(path):
    """Replace retired setting keys in input_settings.txt, keeping the rest of each line."""
    lines = open(path).read().splitlines(keepends=True)
    changed = False
    for i, line in enumerate(lines):
        value, hash_, comment = line.partition("#")
        key = comment.split()[:1]
        if key and key[0] in RENAMED_SETTINGS:
            lines[i] = value + hash_ + comment.replace(key[0], RENAMED_SETTINGS[key[0]], 1)
            changed = True
    if changed:
        open(path, "w").write("".join(lines))
    return changed


def split(path):
    """Old-layout value lines (in order) and lines that are already keyed entries."""
    old_lines, named = [], []
    for line in open(path).read().splitlines():
        before = line.split("#", 1)[0]
        if not before.strip():
            continue                      # blank, or an old "# description" line
        (named if "#" in line else old_lines).append(line)
    return old_lines, named


def is_mox(case_dir):
    """iFuelMatrix == 2, looked up by name as the code does."""
    path = os.path.join(case_dir, "input_settings.txt")
    for line in open(path):
        value, _, comment = line.partition("#")
        if comment.split()[:1] == ["iFuelMatrix"]:
            return value.strip() == "2"
    return False


def convert(path, table):
    old_lines, named = split(path)
    if not old_lines:
        return False                      # already keyed
    if len(old_lines) > len(table):
        raise ValueError(f"{path}: {len(old_lines)} value lines, but only {len(table)} entries are known")

    rows = []
    for k, (key, count, default, description) in enumerate(table):
        tokens = old_lines[k].split() if k < len(old_lines) else []
        if len(tokens) > count:
            raise ValueError(f"{path}: line {k + 1} ({key}) has {len(tokens)} values, expected {count}")
        tokens += [default] * (count - len(tokens))
        rows.append((" ".join(tokens), f"# {key} ({description})"))
    for line in named:
        values, _, comment = line.partition("#")
        rows.append((" ".join(values.split()), "# " + comment.strip()))
    width = max(len(values) for values, _ in rows)

    with open(path, "w") as f:
        for values, comment in rows:
            f.write(f"{values:<{width}}    {comment}\n")
    return True


def main(case_dirs):
    for case in case_dirs:
        st = os.path.join(case, "input_settings.txt")
        if os.path.isfile(st):
            print(("converted  " if rename_settings(st) else "unchanged  ") + st)
        ic = os.path.join(case, "input_initial_conditions.txt")
        if os.path.isfile(ic):
            table = INITIAL_CONDITIONS + (MOX_ONLY if is_mox(case) else [])
            print(("converted  " if convert(ic, table) else "unchanged  ") + ic)
        sf = os.path.join(case, "input_scaling_factors.txt")
        if os.path.isfile(sf):
            print(("converted  " if convert(sf, SCALING_FACTORS) else "unchanged  ") + sf)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    main(sys.argv[1:])
