# White semantic export (JSON and JSON-LD)

This folder now supports machine-readable exports for White regression cases.

## What is generated

For each case folder (for example `test_White2004_4000-1/`), the workflow can generate:
- `input.json`: structured machine-readable input. It includes a `schema` field pointing to: `metadata/schema/input.schema.json`
- `output.json`: structured machine-readable output. It includes a `schema` field pointing to: `metadata/schema/output.schema.json`
- `output.jsonld`: JSON-LD export for semantic/RDF mapping.
- `case_metadata.jsonld`: JSON-LD case-level metadata linking inputs, outputs, software, and sources.
- `metadata/experimental/white_experimental_measurements.jsonld`: JSON-LD dataset of case-level experimental validation targets.
- `metadata/variables/sciantix_variable_catalog.jsonld`: JSON-LD catalog of SCIANTIX input settings, scaling factors, history variables, state variables, and Material/Matrix/Gas/System properties.

Case-level model and software source information is stored in:
- `metadata/models/sciantix_physical_models.jsonld`
- `metadata/sources/sciantix_model_references.jsonld`
- `metadata/sources/sciantix_software_sources.jsonld`
- `metadata/variables/sciantix_variable_catalog.jsonld`

Model-related entries in `input.json` include links to this model catalog and to the model-reference source metadata.
Each `case_metadata.jsonld` records SCIANTIX software provenance, including the declared SCIANTIX version, current Git branch, current Git commit, and whether the repository had uncommitted changes when the export was generated.
Each `case_metadata.jsonld` also links the simulation case to the corresponding experimental intergranular swelling target used in the White parity plot.
Each `case_metadata.jsonld` links to the variable catalog, which provides source-code traceability for the variables defined by SCIANTIX. The catalog is global to the code, while each case-level `output.json` records the variables actually written in the simulation output table.

Original files are unchanged and still used by regression comparison:
- `input_settings.txt`
- `input_history.txt`
- `input_initial_conditions.txt`
- `output.txt`
- `output_gold.txt`

## Provenance sources (dcterms)

Authoritative source metadata are stored in:
- `metadata/sources/white2004.jsonld`
- `metadata/sources/ifpe_cagr_uox_swell.jsonld`
- `metadata/sources/sciantix_model_references.jsonld`
- `metadata/sources/sciantix_software_sources.jsonld`
- `metadata/experimental/white_experimental_measurements.jsonld`
- `metadata/variables/sciantix_variable_catalog.jsonld`

## How it runs
When running White regression through `regression.runner`, machine-readable exports are generated automatically for White cases after `output.txt` is produced. 

# Compliance with authoritative sources

The current semantic export is RDF-mappable at an early project stage. Each `output.jsonld` file declares a JSON-LD context using:
- Dublin Core Terms (`dcterms`) for identifiers, creation time, and source references.
- W3C CSVW (`csvw`) for tabular output concepts such as columns and rows.
- QUDT (`qudt`) for units.
- W3C SOSA/SSN (`sosa`, `ssn`) for observations and simulated/observed properties.
- W3C DCAT (`dcat`) for datasets and file distributions.
- W3C PROV (`prov`) for simulation provenance, software association, used inputs, and generated outputs.
- W3C SKOS (`skos`) and Schema.org (`schema`) for labels, names, model concepts, positions, and content URLs.
- EMMO was verified as a relevant external ontology for future materials-modelling alignment, but no EMMO term is emitted until a stable class IRI is selected.
- A project namespace placeholder (`nmkos`) only for SCIANTIX/project-specific technical fields that do not yet have a clean external mapping.
- XML Schema datatypes (`xsd`) for machine-readable date-time typing.

The repository includes machine-readable metadata for authoritative sources under:
- `metadata/sources/white2004.jsonld`
- `metadata/sources/ifpe_cagr_uox_swell.jsonld`
- `metadata/sources/sciantix_model_references.jsonld`
- `metadata/sources/sciantix_software_sources.jsonld`
- `metadata/models/sciantix_physical_models.jsonld`

These files use Dublin Core Terms (`dcterms`) to describe the scientific article, the IFPE dataset, the model references, and the SCIANTIX GitHub/documentation sources supporting the case study. The physical-model catalog links each SCIANTIX model to its category, target description, main assumptions, and supporting reference. Individual input settings are linked to the relevant physical-model entries where such a mapping is available. 

The variable catalog is generated from SCIANTIX source definitions:
- `src/operations/SetVariablesFunctions.C` for input settings, scaling factors, history variables, and state variables.
- `include/classes/Material.h` for common material fields.
- `include/classes/Matrix.h` for fuel-matrix property fields.
- `include/classes/Gas.h` for gas property fields.
- `include/classes/System.h` for gas-matrix system property fields.

For state and history variables, the catalog records the declared label, unit, source function, output-flag expression, and source-array indexes where available. For Material/Matrix/Gas/System class properties, the catalog records the C++ type and source class; detailed physical units for these internal properties can be added later from authoritative documentation where they are not explicit in the header fields.

The variable catalog uses Dublin Core Terms whenever the metadata meaning is generic and stable:
- `dcterms:identifier` for variable identifiers.
- `dcterms:title` for human-readable variable titles.
- `dcterms:description` for short textual descriptions.
- `dcterms:source` for the source-code file from which the variable metadata were generated.
- `dcterms:references` for links from variables or settings to physical-model identifiers.
- `dcterms:created` for the catalog generation timestamp.

Project-specific terms remain under the preliminary `nmkos` namespace only where the metadata are code-specific or still provisional, for example source function, source array index, source array references, C++ type, output-flag expression, and direct SHA-256 string fields.

The following namespaces are placeholders in this demonstrator:
- `https://w3id.org/nm-kos/terms#`: preliminary project vocabulary for nuclear-materials and simulation-data terms not covered by Dublin Core.
- `https://w3id.org/nm-kos/sciantix/model#`: preliminary SCIANTIX model identifier namespace used to connect variables and input settings to the local physical-model catalog.
- `https://sciantix.org/schema/white/input.schema.json` and `https://sciantix.org/schema/white/output.schema.json`: provisional schema identifiers for the White input/output JSON structures.

These placeholders can later be replaced by project ontology terms and published schema URIs.
