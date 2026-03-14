Minimal regression case for comparing oxygen thermodynamics in the U-O system.

Focus:
- Blackburn oxygen partial pressure / potential
- CALPHAD oxygen partial pressure / potential from OpenCalphad
- internal verification of the SCIANTIX-OpenCalphad coupling

Inputs are intentionally reduced to the matrix U-O system only.
The thermodynamic database reference is `UO.TDB`.

Single entrypoint:
- `thermochemistry_matrix.py`

What the script does:
1. runs all configured stoichiometry cases if results are missing
2. generates per-case Blackburn vs CALPHAD comparison plots
3. parses `oc_debug.txt` and verifies the CALPHAD values reconstructed from `activity(O)`
4. runs a standalone OpenCalphad sweep for each case:
   same initial stoichiometry deviation, same temperature grid, fixed pressure
5. compares SCIANTIX-stored CALPHAD values against standalone OpenCalphad values
6. writes a consolidated verification package under `verification/`

Useful outputs:
- `<case>/output_chemistry.txt`
- `<case>/thermochemistry_output_chemistry.txt`
- `<case>/oc_debug.txt`
- `<case>/oxygen_comparison_summary.txt`
- `<case>/verification_table.tsv`
- `verification/verification_report.md`
- `verification/verification_summary.tsv`
- `verification/standalone_oc/<case>/standalone_oc_table.tsv`
- `verification/standalone_oc/<case>/standalone_oc_debug.txt`
- `verification/plots/*.png`
