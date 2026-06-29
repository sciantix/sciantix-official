### NEO4MAT - SCIANTIX-DIVA: White semantic export (JSON and JSON-LD)

This folder contains the machine-readable metadata layer for the White 2004 SCIANTIX regression cases. The original SCIANTIX text files remain unchanged and continue to drive the regression workflow; the JSON and JSON-LD files add structured inputs, structured outputs, provenance, source metadata, validation targets, physical-model links, and an RDF-mappable SCIANTIX variable catalog.

## Vocabulary

JSON-LD use established vocabularies whenever a suitable term is available:
- Dublin Core Terms (`dcterms`) for identifiers, creation time, and source references.
- W3C CSVW (`csvw`) for tabular output concepts such as columns and rows.
- QUDT (`qudt`) for units.
- W3C SOSA/SSN (`sosa`, `ssn`) for observations and simulated/observed properties.
- W3C DCAT (`dcat`) for datasets and file distributions.
- W3C PROV (`prov`) for simulation provenance, associated software, used inputs, and generated outputs.
- W3C SKOS (`skos`) and Schema.org (`schema`) for labels, names, model concepts, positions, and content URLs.
- XML Schema datatypes (`xsd`) for machine-readable date-time typing.

A project namespace placeholder (`nmkos`) only for SCIANTIX/project-specific fields that do not yet have a mapping has been added. The following identifiers are preliminary:
- `https://w3id.org/nm-kos/terms#`: preliminary project vocabulary for nuclear-materials.
- `https://w3id.org/nm-kos/sciantix/model#`: preliminary SCIANTIX model identifier namespace.

Project-specific `nmkos` terms used in the catalog: `nmkos:sourceFile`, `nmkos:sourceFunction`, `nmkos:sourceClass`, `nmkos:sourceArray`, `nmkos:sourceArrayIndex`, `nmkos:sourceArrayReferences`, `nmkos:outputFlagExpression`, `nmkos:cppType`, `nmkos:sha256`, `nmkos:valueSource`.

## Case specific data

For each case folder (for example `test_White2004_4000-1/`), the workflow can generate:
- `input.json`: structured input extracted from `input_settings.txt`, `input_history.txt`, and `input_initial_conditions.txt`. It points to the local schema `metadata/schema/input.schema.json`. The `history` section uses a fixed four-column schema (Time, Temperature, Fission rate, Hydrostatic stress); `initial_conditions` entries pair raw values with the inline comment from the source file.
- `output.json`: structured tabular copy of the native `output.txt`. It points to the local schema `metadata/schema/output.schema.json`.
- `output.jsonld`: RDF-mappable table metadata for the same output data, typed as `csvw:Table`, using CSVW/QUDT/SKOS/Schema.org terms where applicable. Each column entry carries:
  - `unitURI`: a QUDT vocab IRI (e.g., `https://qudt.org/vocab/unit/M2-PER-SEC`) for standard SI/physical units, or a preliminary `https://w3id.org/nm-kos/unit#` IRI for SCIANTIX domain-specific units (e.g., `AtomPerM3`, `BubblePerM2`); mapped to `qudt:unit` with `@type: @id`.
  - `catalogVariable`: the `@id` of the corresponding state or history variable in `sciantix_variable_catalog.jsonld`; mapped to `dcterms:references`.
- `case_metadata.jsonld`: typed as both `prov:Activity` and `dcat:Dataset`. It links native inputs (`prov:used`), generated outputs (`prov:generated`), SCIANTIX software provenance, source metadata, model catalogs, the variable catalog, and the White validation target. The `software` block includes repository branch, commit hash, and a dirty repository flag which is relevant for code developers.
- `metadata/experimental/white_experimental_measurements.jsonld`: JSON-LD dataset of the White intergranular swelling measurements (validation targets), sourced from `data/ig_swelling.txt`.

## SCIANTIX state variables

- `metadata/variables/sciantix_variable_catalog.jsonld`: catalog of SCIANTIX input settings, scaling factors, history variables, state variables, and Material/Matrix/Gas/System properties derived from the source code.
    - `src/operations/SetVariablesFunctions.C` for input settings, scaling factors, history variables, and state variables.
    - `include/classes/Material.h` for common material fields.
    - `include/classes/Matrix.h` for fuel-matrix property fields.
    - `include/classes/Gas.h` for gas property fields.
    - `include/classes/System.h` for gas-matrix system property fields.

For state and history variables, the catalog records the declared label, unit, `unitURI` (QUDT or nmkos IRI, absent for `unspecified` units), source function, output-flag expression, and source-array indexes where available. For scaling factors and input settings, the catalog records the name and array index from `getScalingFactorsNames` / `getInputVariableNames` defined in the SCIANTIX code; input settings that control documented physical models also carry `dcterms:references` links to the local model catalog.

For Material/Matrix/Gas/System class properties, the catalog records the C++ type, source class, and source file (header). Units are extracted from inline comments in `SetMatrix.C`, `SetGas.C`, and `SetSystem.C`; a fallback table in `variable_metadata_export.py` supplements properties whose source comments omit a unit. Constant values are also parsed from functions in `Set*.C` and stored in the `value` field (a scalar when all instances share the same value, a per-instance dict otherwise, e.g the fission yield of fission gases is a property which is defined as a dictionary because it has a different value for each gas); `nmkos:valueSource` records the Set*.C file from which the value was extracted.

Dublin Core Terms are used whenever possible:
- `dcterms:identifier` for variable identifiers.
- `dcterms:title` for human-readable variable titles.
- `dcterms:description` for short textual descriptions.
- `dcterms:source` for the source-code file from which the variable metadata were generated.
- `dcterms:references` for links from variables or settings to physical-model identifiers.
- `dcterms:created` for the catalog generation timestamp.

Project-specific terms remain under the preliminary `nmkos` namespace: source function, source array index, source array references, C++ type, output-flag expression, SHA-256 string fields, and `nmkos:valueSource` for the Set*.C origin of extracted property values.

## Other metadata: authoritative sources, models and schema

The following files are not regenerated from the case outputs and can be updated manually if needed:
- `metadata/sources/white2004.jsonld`
- `metadata/sources/ifpe_cagr_uox_swell.jsonld`
- `metadata/sources/sciantix_model_references.jsonld`
- `metadata/sources/sciantix_software_sources.jsonld`

--> These files use Dublin Core Terms (`dcterms`) to describe the scientific article, the IFPE dataset, the model references, and the SCIANTIX GitHub/documentation sources supporting the case study.


- `metadata/models/sciantix_physical_models.jsonld`

--> The physical-model catalog represents SCIANTIX models as SKOS concepts and links each model to its category, target description, main assumptions, and supporting reference. 


- `metadata/schema/input.schema.json`
- `metadata/schema/output.schema.json`

--> local JSON Schema files for the SCIANTIX input/output JSON structures. These local schema identifiers can later be replaced by published schema.

## Generating scripts

Two Python modules in `regression/white/` drive the export:
- `semantic_export.py`: generates `input.json`, `output.json`, `output.jsonld`, `case_metadata.jsonld` for each case, plus `metadata/experimental/white_experimental_measurements.jsonld`. It is called per-case inside `regression/core/generic_runner.py` when running the `white` group.
- `variable_metadata_export.py`: generates `metadata/variables/sciantix_variable_catalog.jsonld` by parsing the C++ source tree. It is called once per group run (before the parallel case loop) and can also be invoked directly.

Both modules are invoked automatically by `runRegression.sh` via `regression.core.generic_runner`. 