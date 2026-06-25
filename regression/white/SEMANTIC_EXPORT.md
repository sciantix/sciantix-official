# White semantic export (JSON and JSON-LD)

This folder now supports machine-readable exports for White regression cases.

## What is generated

For each case folder (for example `test_White2004_4000-1/`), the workflow can generate:
- `input.json`: structured machine-readable input. It includes a `schema` field pointing to: `metadata/schema/input.schema.json`
- `output.json`: structured machine-readable output. It includes a `schema` field pointing to: `metadata/schema/output.schema.json`
- `output.jsonld`: JSON-LD export for semantic/RDF mapping.
- `case_metadata.jsonld`: JSON-LD case-level metadata linking inputs, outputs, software, and sources.
- `metadata/experimental/white_experimental_measurements.jsonld`: JSON-LD dataset of case-level experimental validation targets.

Case-level model and software source information is stored in:
- `metadata/models/sciantix_physical_models.jsonld`
- `metadata/sources/sciantix_model_references.jsonld`
- `metadata/sources/sciantix_software_sources.jsonld`

Model-related entries in `input.json` include links to this model catalog and to the model-reference source metadata.
Each `case_metadata.jsonld` records SCIANTIX software provenance, including the declared SCIANTIX version, current Git branch, current Git commit, and whether the repository had uncommitted changes when the export was generated.
Each `case_metadata.jsonld` also links the simulation case to the corresponding experimental intergranular swelling target used in the White parity plot.

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

## How it runs
When running White regression through `regression.runner`, machine-readable exports are generated automatically for White cases after `output.txt` is produced. 

# Compliance with authoritative sources

The current semantic export is RDF-mappable at an early project stage. Each `output.jsonld` file declares a JSON-LD context using:
- Dublin Core Terms (`dcterms`) for identifiers, creation time, and source references.
- A project namespace placeholder (`nmkos`) for simulation-output concepts such as columns, rows, labels, units, names, and indexes.
- XML Schema datatypes (`xsd`) for machine-readable date-time typing.

The repository includes machine-readable metadata for authoritative sources under:
- `metadata/sources/white2004.jsonld`
- `metadata/sources/ifpe_cagr_uox_swell.jsonld`
- `metadata/sources/sciantix_model_references.jsonld`
- `metadata/sources/sciantix_software_sources.jsonld`
- `metadata/models/sciantix_physical_models.jsonld`

These files use Dublin Core Terms (`dcterms`) to describe the scientific article, the IFPE dataset, the model references, and the SCIANTIX GitHub/documentation sources supporting the case study. The physical-model catalog links each SCIANTIX model to its category, target description, main assumptions, and supporting reference. Individual input settings are linked to the relevant physical-model entries where such a mapping is available. 
