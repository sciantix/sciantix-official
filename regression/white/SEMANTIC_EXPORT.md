### NEO4MAT - SCIANTIX-DIVA: White semantic export (JSON and JSON-LD)

This folder contains the machine-readable metadata layer for the White 2004
SCIANTIX regression cases. The original SCIANTIX text files remain unchanged
and continue to drive the regression workflow; the JSON and JSON-LD files add
structured inputs, structured outputs, provenance, source metadata, validation
targets, physical-model links, and an RDF-mappable SCIANTIX variable catalog.

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

A project namespace placeholder (`nmkos`) only for SCIANTIX/project-specific technical fields that do not yet have a clean external mapping has been added. The following identifiers are preliminary:
- `https://w3id.org/nm-kos/terms#`: preliminary project vocabulary for nuclear-materials.
- `https://w3id.org/nm-kos/sciantix/model#`: preliminary SCIANTIX model identifier namespace.

## Case specific data

For each case folder (for example `test_White2004_4000-1/`), the workflow can generate:
- `input.json`: structured input extracted from `input_settings.txt`, `input_history.txt`, and `input_initial_conditions.txt`. It points to the local schema `metadata/schema/input.schema.json`.
- `output.json`: structured tabular copy of the native `output.txt`. It points to the local schema `metadata/schema/output.schema.json`.
- `output.jsonld`: RDF-mappable table metadata for the same output data, using CSVW/QUDT/SKOS/Schema.org terms where applicable.
- `case_metadata.jsonld`: case-level metadata linking native files, generated files, SCIANTIX software provenance, source metadata, model catalogs, variable catalogs, and validation targets.
- `metadata/experimental/white_experimental_measurements.jsonld`: JSON-LD dataset of the White dataset (validation target).

## SCIANTIX state variables

- `metadata/variables/sciantix_variable_catalog.jsonld`: catalog of SCIANTIX input settings, scaling factors, history variables, state variables, and Material/Matrix/Gas/System properties derived from the source code.
    - `src/operations/SetVariablesFunctions.C` for input settings, scaling factors, history variables, and state variables.
    - `include/classes/Material.h` for common material fields.
    - `include/classes/Matrix.h` for fuel-matrix property fields.
    - `include/classes/Gas.h` for gas property fields.
    - `include/classes/System.h` for gas-matrix system property fields.

For state and history variables, the catalog records the declared label, unit, source function, output-flag expression, and source-array indexes where available. For Material/Matrix/Gas/System class properties, the catalog records the C++ type and source class; detailed physical units for these internal properties can be added later from authoritative documentation where they are not explicit in the header fields.

Dublin Core Terms are used whenever possible:
- `dcterms:identifier` for variable identifiers.
- `dcterms:title` for human-readable variable titles.
- `dcterms:description` for short textual descriptions.
- `dcterms:source` for the source-code file from which the variable metadata were generated.
- `dcterms:references` for links from variables or settings to physical-model identifiers.
- `dcterms:created` for the catalog generation timestamp.

Project-specific terms remain under the preliminary `nmkos` namespace, for example source function, source array index, source array references, C++ type, output-flag expression, and direct SHA-256 string fields.

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