#!/usr/bin/env python3
"""### NEO4MAT - SCIANTIX-DIVA: White case semantic export utilities.

This module generates machine-readable JSON/JSON-LD files for the White 2004 SCIANTIX
regression cases.
"""

import hashlib
import json
import os
import re
import subprocess
from datetime import datetime, timezone
from typing import Dict, List, Tuple

from regression.white.variable_metadata_export import export_variable_catalog, unit_uri


_SCIANTIX_VERSION = "2.2.1"
_DCTERMS_SOURCES = [
    "../metadata/sources/white2004.jsonld",
    "../metadata/sources/ifpe_cagr_uox_swell.jsonld",
]
_MODEL_CATALOG = "../metadata/models/sciantix_physical_models.jsonld"
_MODEL_REFERENCES = "../metadata/sources/sciantix_model_references.jsonld"
_SOFTWARE_SOURCES = "../metadata/sources/sciantix_software_sources.jsonld"
_EXPERIMENTAL_MEASUREMENTS = "../metadata/experimental/white_experimental_measurements.jsonld"
_VARIABLE_CATALOG = "../metadata/variables/sciantix_variable_catalog.jsonld"

_SCHEMA_RELATIVE_PATH = "../metadata/schema/output.schema.json"
_INPUT_SCHEMA_RELATIVE_PATH = "../metadata/schema/input.schema.json"
_EXPERIMENTAL_SWELLING_FILE = "data/ig_swelling.txt"
_GENERATED_SIDECAR_FILENAMES = {
    "input.json",
    "output.json",
    "output.jsonld",
    "case_metadata.jsonld",
}
_GENERATED_METADATA_PATHS = {
    "regression/white/metadata/experimental/white_experimental_measurements.jsonld",
    "regression/white/metadata/variables/sciantix_variable_catalog.jsonld",
}
_INPUT_FILES = {
    "settings": "input_settings.txt",
    "history": "input_history.txt",
    "initial_conditions": "input_initial_conditions.txt",
}
_HISTORY_COLUMNS = [
    {"index": 0, "name": "Time (h)", "label": "Time", "unit": "h"},
    {"index": 1, "name": "Temperature (K)", "label": "Temperature", "unit": "K"},
    {
        "index": 2,
        "name": "Fission rate (fiss / m3 s)",
        "label": "Fission rate",
        "unit": "fiss / m3 s",
    },
    {
        "index": 3,
        "name": "Hydrostatic stress (MPa)",
        "label": "Hydrostatic stress",
        "unit": "MPa",
    },
]
_SETTING_MODEL_MAP = {
    "iDensification": ["model:densification"],
    "iGrainGrowth": ["model:grain-growth"],
    "iFissionGasDiffusivity": ["model:gas-diffusion"],
    "iDiffusionSolver": ["model:gas-diffusion"],
    "iIntraGranularBubbleBehavior": ["model:intragranular-bubble-behaviour"],
    "iResolutionRate": ["model:intragranular-bubble-behaviour"],
    "iTrappingRate": ["model:intragranular-bubble-behaviour"],
    "iNucleationRate": ["model:intragranular-bubble-behaviour"],
    "iGrainBoundaryVacancyDiffusivity": ["model:intergranular-bubble-behavior"],
    "iGrainBoundaryBehaviour": ["model:intergranular-bubble-behavior"],
    "iReleaseMode": ["model:intergranular-bubble-behavior"],
    "iGrainBoundaryMicroCracking": ["model:grain-boundary-micro-cracking"],
    "iGrainBoundaryVenting": ["model:grain-boundary-venting"],
}


def _split_tsv_line(line: str) -> List[str]:
    """Split a legacy tab-separated row and remove trailing empty cells.

    SCIANTIX output files may end rows with a final tab. Trimming only the
    trailing empty fields keeps the table rectangular without altering values.
    """
    parts = [p.strip() for p in line.rstrip("\n").split("\t")]
    while parts and parts[-1] == "":
        parts.pop()
    return parts


def _parse_label_and_unit(column_name: str) -> Tuple[str, str]:
    """Extract a display label and unit from headers like ``Time (h)``.

    If no parenthesized unit is present, the column is treated as
    dimensionless. This keeps the output schema simple and predictable.
    """
    match = re.match(r"^(.*?)\s*\((.*?)\)\s*$", column_name)
    if match:
        return match.group(1).strip(), match.group(2).strip()
    return column_name.strip(), "dimensionless"


def _to_number_or_text(token: str):
    """Cast numeric-looking tokens to Python numbers.

    Textual tokens are preserved as strings so the export does not silently
    rewrite non-numeric metadata or comments.
    """
    if token == "":
        return None
    if re.match(r"^[+-]?\d+$", token):
        return int(token)
    try:
        value = float(token)
        return value
    except ValueError:
        return token


def _sha256(path: str) -> str:
    """Return the SHA-256 digest of a native or generated case file."""
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _git_value(args: List[str], cwd: str) -> str:
    """Run a Git metadata query and return ``unknown`` if it is unavailable."""
    try:
        result = subprocess.run(
            ["git"] + args,
            cwd=cwd,
            check=True,
            capture_output=True,
            text=True,
        )
    except Exception:
        return "unknown"
    return result.stdout.strip() or "unknown"


def _timestamp_from_git(git_cwd: str) -> str:
    """Use the current commit timestamp as a reproducible generation time."""
    value = _git_value(["show", "-s", "--format=%cI", "HEAD"], git_cwd)
    if value == "unknown":
        return datetime.now(timezone.utc).isoformat()
    try:
        return datetime.fromisoformat(value).astimezone(timezone.utc).isoformat()
    except ValueError:
        return datetime.now(timezone.utc).isoformat()


def _exported_at_utc(path: str) -> str:
    """Return the export timestamp used in generated metadata.

    The optional ``SCIANTIX_SEMANTIC_GENERATED_AT_UTC`` environment variable is
    useful when regenerating files without introducing timestamp-only diffs.
    """
    override = os.environ.get("SCIANTIX_SEMANTIC_GENERATED_AT_UTC")
    if override:
        return datetime.fromisoformat(override).astimezone(timezone.utc).isoformat()

    repo_root = _git_value(["rev-parse", "--show-toplevel"], path)
    git_cwd = repo_root if repo_root != "unknown" else path
    return _timestamp_from_git(git_cwd)


def _is_generated_semantic_path(path: str) -> bool:
    """Identify files generated by this semantic-export workflow.

    Generated files are ignored when deciding whether the source repository was
    dirty at export time; otherwise a fresh export would always mark itself as
    dirty simply because it just rewrote its own outputs.
    """
    normalized = path.strip().replace("\\", "/")
    if normalized in _GENERATED_METADATA_PATHS:
        return True
    parts = normalized.split("/")
    return (
        len(parts) == 4
        and parts[0] == "regression"
        and parts[1] == "white"
        and parts[2].startswith("test_White")
        and parts[3] in _GENERATED_SIDECAR_FILENAMES
    )


def _repository_is_dirty_for_provenance(git_cwd: str) -> bool:
    """Report whether non-generated files were modified during export."""
    status = _git_value(["status", "--porcelain"], git_cwd)
    if status == "unknown":
        return True

    for line in status.splitlines():
        if not line:
            continue
        path = line[3:] if len(line) > 3 else ""
        if " -> " in path:
            path = path.rsplit(" -> ", 1)[1]
        if not _is_generated_semantic_path(path):
            return True
    return False


def _software_provenance(case_dir: str) -> dict:
    """Build the SCIANTIX software provenance block for one simulation case."""
    repo_root = _git_value(["rev-parse", "--show-toplevel"], case_dir)
    git_cwd = repo_root if repo_root != "unknown" else case_dir
    dirty_override = os.environ.get("SCIANTIX_SEMANTIC_REPOSITORY_IS_DIRTY")
    if dirty_override is None:
        repository_is_dirty = _repository_is_dirty_for_provenance(git_cwd)
    else:
        repository_is_dirty = dirty_override.strip().lower() in {"1", "true", "yes"}

    return {
        "@type": ["prov:SoftwareAgent", "schema:SoftwareApplication"],
        "name": "SCIANTIX",
        "version": _SCIANTIX_VERSION,
        "role": "simulation code",
        "repository": "https://github.com/sciantix/sciantix-official",
        "project_branch_url": "https://github.com/sciantix/sciantix-official/tree/project/NEO4MAT_DIVA",
        "documentation": "https://sciantix.github.io/sciantix-official/models.html",
        "repository_branch": _git_value(["rev-parse", "--abbrev-ref", "HEAD"], git_cwd),
        "repository_commit": os.environ.get("SCIANTIX_SEMANTIC_REPOSITORY_COMMIT")
        or _git_value(["rev-parse", "HEAD"], git_cwd),
        "repository_is_dirty": repository_is_dirty,
    }


def _file_record(case_dir: str, filename: str, role: str) -> dict:
    """Describe one native or generated file with role, path, hash, and size."""
    path = os.path.join(case_dir, filename)
    return {
        "role": role,
        "path": filename,
        "sha256": _sha256(path),
        "size_bytes": os.path.getsize(path),
    }


def _case_measurement_id(case_id: str) -> str:
    """Return a stable local identifier for a White validation measurement."""
    return f"white-measurement:{case_id}"


def _load_experimental_swelling(white_root: str) -> Dict[str, float]:
    """Load case-level White intergranular swelling validation targets."""
    path = os.path.join(white_root, _EXPERIMENTAL_SWELLING_FILE)
    measurements = {}
    with open(path, "r", encoding="utf-8") as handle:
        for line in handle:
            raw = line.strip()
            if not raw:
                continue
            case_id, value = raw.split()[:2]
            measurements[case_id] = float(value)
    return measurements


def export_white_experimental_measurements(white_root: str) -> str:
    """Export White validation measurements as a compact JSON-LD dataset.

    The source values come from ``data/ig_swelling.txt`` and are represented as
    SOSA observations inside a DCAT dataset. The export is regenerated together
    with the White semantic sidecars.
    """
    measurements = _load_experimental_swelling(white_root)
    output_dir = os.path.join(white_root, "metadata", "experimental")
    os.makedirs(output_dir, exist_ok=True)
    output_path = os.path.join(output_dir, "white_experimental_measurements.jsonld")
    exported_at = _exported_at_utc(white_root)

    payload = {
        "@context": {
            "dcat": "http://www.w3.org/ns/dcat#",
            "dcterms": "http://purl.org/dc/terms/",
            "qudt": "https://qudt.org/schema/qudt/",
            "sosa": "http://www.w3.org/ns/sosa/",
            "xsd": "http://www.w3.org/2001/XMLSchema#",
            "caseId": "dcterms:identifier",
            "source": {
                "@id": "dcterms:source",
                "@type": "@id",
            },
            "generatedAt": {
                "@id": "dcterms:created",
                "@type": "xsd:dateTime",
            },
            "quantity": "sosa:observedProperty",
            "value": "sosa:hasSimpleResult",
            "unit": "qudt:unit",
            "measurement": "dcterms:hasPart",
        },
        "@type": "dcat:Dataset",
        "dcterms:identifier": "white-2004-intergranular-swelling-validation-targets",
        "dcterms:title": "White intergranular swelling validation targets for SCIANTIX regression cases",
        "dcterms:description": "Case-level intergranular gas swelling values used by the SCIANTIX White regression parity workflow.",
        "generatedAt": exported_at,
        "source": _DCTERMS_SOURCES,
        "dcterms:relation": _EXPERIMENTAL_SWELLING_FILE,
        "measurement": [
            {
                "@id": _case_measurement_id(case_id),
                "@type": "sosa:Observation",
                "caseId": case_id,
                "quantity": "Intergranular gas swelling",
                "value": value,
                "unit": "%",
            }
            for case_id, value in sorted(measurements.items())
        ],
    }

    with open(output_path, "w", encoding="utf-8") as handle:
        json.dump(payload, handle, indent=2)
        handle.write("\n")

    return output_path


def _load_output_tsv(output_path: str) -> Tuple[List[str], List[List[object]]]:
    """Load legacy ``output.txt`` as ``(header, rows)``.

    The function preserves the original table shape while converting numeric
    cells to numbers for easier machine processing.
    """
    with open(output_path, "r", encoding="utf-8") as handle:
        lines = [line for line in handle if line.strip()]

    if not lines:
        raise ValueError(f"Empty output file: {output_path}")

    header = _split_tsv_line(lines[0])
    rows: List[List[object]] = []

    for line in lines[1:]:
        fields = _split_tsv_line(line)
        if len(fields) < len(header):
            fields.extend([""] * (len(header) - len(fields)))
        elif len(fields) > len(header):
            fields = fields[: len(header)]

        rows.append([_to_number_or_text(token) for token in fields])

    return header, rows


def _parse_settings(settings_path: str) -> List[dict]:
    """Parse ``input_settings.txt`` into structured model-setting records.

    SCIANTIX stores the setting name in the comment after ``#``. When a setting
    controls a documented physical model, the exported record links to the
    local model catalog and model-reference metadata.
    """
    settings = []
    with open(settings_path, "r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle):
            raw = line.strip()
            if not raw:
                continue
            value_part, _, comment_part = raw.partition("#")
            tokens = value_part.split()
            if not tokens:
                continue
            description = comment_part.strip()
            name = description.split(" ", 1)[0] if description else f"setting_{idx}"
            setting = {
                "index": len(settings),
                "name": name,
                "value": _to_number_or_text(tokens[0]),
                "description": description,
                "source_line": idx + 1,
            }
            related_models = _SETTING_MODEL_MAP.get(name)
            if related_models:
                setting["related_models"] = related_models
                setting["model_catalog"] = _MODEL_CATALOG
                setting["model_reference_source"] = _MODEL_REFERENCES
            settings.append(setting)
    return settings


def _parse_history(history_path: str) -> List[List[object]]:
    """Parse ``input_history.txt`` rows into numeric time-history records."""
    rows = []
    with open(history_path, "r", encoding="utf-8") as handle:
        for line in handle:
            raw = line.strip()
            if not raw or raw.startswith("#"):
                continue
            rows.append([_to_number_or_text(token) for token in raw.split()])
    return rows


def _parse_initial_conditions(initial_conditions_path: str) -> List[dict]:
    """Parse value/comment pairs from ``input_initial_conditions.txt``.

    The native format stores values on one line and their description on the
    following comment line. The export keeps both pieces and records the source
    line of the value row for traceability.
    """
    entries = []
    pending_values = None
    pending_line = None

    with open(initial_conditions_path, "r", encoding="utf-8") as handle:
        for idx, line in enumerate(handle):
            raw = line.strip()
            if not raw:
                continue

            if raw.startswith("#"):
                description = raw.lstrip("#").strip()
                if pending_values is not None:
                    entries.append(
                        {
                            "index": len(entries),
                            "values": pending_values,
                            "description": description,
                            "source_line": pending_line,
                        }
                    )
                    pending_values = None
                    pending_line = None
                continue

            pending_values = [_to_number_or_text(token) for token in raw.split()]
            pending_line = idx + 1

    if pending_values is not None:
        entries.append(
            {
                "index": len(entries),
                "values": pending_values,
                "description": "",
                "source_line": pending_line,
            }
        )

    return entries


def _load_input_payload(case_dir: str) -> dict:
    """Load all native White input files required for one case export."""
    missing = [
        filename
        for filename in _INPUT_FILES.values()
        if not os.path.isfile(os.path.join(case_dir, filename))
    ]
    if missing:
        raise FileNotFoundError(f"Missing input file(s) in {case_dir}: {', '.join(missing)}")

    return {
        "files": {
            role: _file_record(case_dir, filename, role)
            for role, filename in _INPUT_FILES.items()
        },
        "settings": _parse_settings(os.path.join(case_dir, _INPUT_FILES["settings"])),
        "history": {
            "columns": _HISTORY_COLUMNS,
            "rows": _parse_history(os.path.join(case_dir, _INPUT_FILES["history"])),
        },
        "initial_conditions": _parse_initial_conditions(
            os.path.join(case_dir, _INPUT_FILES["initial_conditions"])
        ),
    }


def _build_columns(header: List[str], label_map: Dict[str, str] = None):
    """Build column metadata from the native SCIANTIX output header.

    When *label_map* is provided, each column entry is enriched with:
    - ``unitURI``: a QUDT or nmkos unit IRI for the column unit;
    - ``catalogVariable``: the variable catalog ``@id`` for the corresponding
      state or history variable.
    """
    if label_map is None:
        label_map = {}
    columns = []
    for idx, raw_name in enumerate(header):
        label, unit = _parse_label_and_unit(raw_name)
        col: dict = {
            "index": idx,
            "name": raw_name,
            "label": label,
            "unit": unit,
        }
        _uri = unit_uri(unit)
        if _uri:
            col["unitURI"] = _uri
        var_id = label_map.get(label)
        if var_id:
            col["catalogVariable"] = var_id
        columns.append(col)
    return columns


def _load_variable_label_map(white_root: str) -> Dict[str, str]:
    """Return a ``{label: @id}`` map for state and history variables from the catalog.

    Used by ``_build_columns`` to attach ``catalogVariable`` links to each
    output column, closing the gap between simulation output and the variable
    metadata layer.
    """
    catalog_path = os.path.join(
        white_root, "metadata", "variables", "sciantix_variable_catalog.jsonld"
    )
    if not os.path.isfile(catalog_path):
        return {}
    with open(catalog_path, "r", encoding="utf-8") as handle:
        catalog = json.load(handle)
    return {
        v["label"]: v["@id"]
        for v in catalog.get("variable", [])
        if v.get("category") in ("state_variable", "history_variable")
    }


def _validate_export_payload(payload_json: dict) -> None:
    """Perform minimal structural checks on ``output.json``.

    These checks are intentionally lightweight. They catch broken exports
    without replacing the JSON Schema files stored in ``metadata/schema``.
    """
    required_top = ["format_version", "case_id", "generated_at_utc", "dcterms_sources", "table"]
    for key in required_top:
        if key not in payload_json:
            raise ValueError(f"Missing top-level key in output.json: {key}")

    schema = payload_json.get("schema")
    if schema and not schema.startswith(("http://", "https://", "../")):
        raise ValueError(f"Schema path must be a URI or relative to the case directory: {schema}")

    table = payload_json["table"]
    if not isinstance(table, dict):
        raise ValueError("'table' must be an object")

    if "columns" not in table or "rows" not in table:
        raise ValueError("'table' must contain 'columns' and 'rows'")

    columns = table["columns"]
    rows = table["rows"]

    if not isinstance(columns, list) or len(columns) == 0:
        raise ValueError("'columns' must be a non-empty array")
    if not isinstance(rows, list):
        raise ValueError("'rows' must be an array")

    for col in columns:
        for key in ("index", "name", "label", "unit"):
            if key not in col:
                raise ValueError(f"Column metadata missing key: {key}")

    ncols = len(columns)
    for idx, row in enumerate(rows):
        if not isinstance(row, list):
            raise ValueError(f"Row {idx} is not an array")
        if len(row) != ncols:
            raise ValueError(
                f"Row {idx} length ({len(row)}) does not match columns ({ncols})"
            )


def _validate_input_payload(payload_json: dict) -> None:
    """Perform minimal structural checks on ``input.json``."""
    required_top = [
        "format_version",
        "case_id",
        "generated_at_utc",
        "dcterms_sources",
        "input_files",
        "settings",
        "history",
        "initial_conditions",
    ]
    for key in required_top:
        if key not in payload_json:
            raise ValueError(f"Missing top-level key in input.json: {key}")

    schema = payload_json.get("schema")
    if schema and not schema.startswith(("http://", "https://", "../")):
        raise ValueError(f"Schema path must be a URI or relative to the case directory: {schema}")

    history = payload_json["history"]
    if "columns" not in history or "rows" not in history:
        raise ValueError("'history' must contain 'columns' and 'rows'")

    ncols = len(history["columns"])
    for idx, row in enumerate(history["rows"]):
        if len(row) != ncols:
            raise ValueError(
                f"History row {idx} length ({len(row)}) does not match columns ({ncols})"
            )


def _build_case_metadata(
    case_id: str,
    exported_at: str,
    input_files: Dict[str, dict],
    output_files: Dict[str, dict],
    software: dict,
    experimental_measurement: dict,
) -> dict:
    """Build the case-level JSON-LD metadata record.

    The result connects native inputs, generated outputs, SCIANTIX software
    provenance, model/source catalogs, the variable catalog, and the White
    validation target used by the parity workflow.
    """
    return {
        "@context": {
            "dcat": "http://www.w3.org/ns/dcat#",
            "dcterms": "http://purl.org/dc/terms/",
            "nmkos": "https://w3id.org/nm-kos/terms#",
            "prov": "http://www.w3.org/ns/prov#",
            "schema": "https://schema.org/",
            "sosa": "http://www.w3.org/ns/sosa/",
            "xsd": "http://www.w3.org/2001/XMLSchema#",
            "caseId": "dcterms:identifier",
            "generatedAt": {
                "@id": "dcterms:created",
                "@type": "xsd:dateTime",
            },
            "source": {
                "@id": "dcterms:source",
                "@type": "@id",
            },
            "software": "prov:wasAssociatedWith",
            "modelCatalog": {
                "@id": "dcterms:references",
                "@type": "@id",
            },
            "modelReferenceSource": {
                "@id": "dcterms:source",
                "@type": "@id",
            },
            "softwareSource": {
                "@id": "dcterms:source",
                "@type": "@id",
            },
            "variableCatalog": {
                "@id": "dcterms:references",
                "@type": "@id",
            },
            "experimentalDataset": {
                "@id": "dcterms:relation",
                "@type": "@id",
            },
            "validationTarget": "sosa:hasResult",
            "inputFile": "prov:used",
            "outputFile": "prov:generated",
            "role": "dcterms:type",
            "path": "schema:contentUrl",
            "sha256": "nmkos:sha256",
            "sizeBytes": "dcat:byteSize",
            "size_bytes": "dcat:byteSize",
        },
        "@type": ["prov:Activity", "dcat:Dataset"],
        "caseId": case_id,
        "generatedAt": exported_at,
        "source": _DCTERMS_SOURCES,
        "modelCatalog": _MODEL_CATALOG,
        "modelReferenceSource": _MODEL_REFERENCES,
        "softwareSource": _SOFTWARE_SOURCES,
        "variableCatalog": _VARIABLE_CATALOG,
        "experimentalDataset": _EXPERIMENTAL_MEASUREMENTS,
        "validationTarget": experimental_measurement,
        "software": software,
        "inputFile": list(input_files.values()),
        "outputFile": list(output_files.values()),
    }


def export_white_case_semantic_outputs(case_dir: str) -> Tuple[str, str, str, str]:
    """Export all machine-readable sidecars for one White regression case.

    The function writes:
    - ``input.json`` for structured native input data;
    - ``output.json`` for the tabular SCIANTIX output;
    - ``output.jsonld`` for RDF-mappable table metadata;
    - ``case_metadata.jsonld`` for case-level provenance and source links.
    """
    output_txt = os.path.join(case_dir, "output.txt")
    input_json = os.path.join(case_dir, "input.json")
    output_json = os.path.join(case_dir, "output.json")
    output_jsonld = os.path.join(case_dir, "output.jsonld")
    case_metadata_jsonld = os.path.join(case_dir, "case_metadata.jsonld")

    if not os.path.isfile(output_txt):
        raise FileNotFoundError(f"output.txt not found in case: {case_dir}")

    input_payload = _load_input_payload(case_dir)
    header, rows = _load_output_tsv(output_txt)
    white_root = os.path.dirname(__file__)
    label_map = _load_variable_label_map(white_root)
    columns = _build_columns(header, label_map)
    exported_at = _exported_at_utc(case_dir)
    case_id = os.path.basename(os.path.normpath(case_dir))
    white_root = os.path.dirname(__file__)
    experimental_swelling = _load_experimental_swelling(white_root).get(case_id)
    if experimental_swelling is None:
        raise ValueError(f"Missing experimental swelling value for case: {case_id}")

    payload_input_json = {
        "format_version": "0.1.0",
        "schema": _INPUT_SCHEMA_RELATIVE_PATH,
        "case_id": case_id,
        "generated_at_utc": exported_at,
        "dcterms_sources": _DCTERMS_SOURCES,
        "input_files": input_payload["files"],
        "settings": input_payload["settings"],
        "history": input_payload["history"],
        "initial_conditions": input_payload["initial_conditions"],
    }

    _validate_input_payload(payload_input_json)

    with open(input_json, "w", encoding="utf-8") as handle:
        json.dump(payload_input_json, handle, indent=2)
        handle.write("\n")

    payload_json = {
        "format_version": "0.1.0",
        "schema": _SCHEMA_RELATIVE_PATH,
        "case_id": case_id,
        "generated_at_utc": exported_at,
        "dcterms_sources": _DCTERMS_SOURCES,
        "table": {
            "columns": columns,
            "rows": rows,
        },
    }

    _validate_export_payload(payload_json)

    with open(output_json, "w", encoding="utf-8") as handle:
        json.dump(payload_json, handle, indent=2)
        handle.write("\n")

    payload_jsonld = {
        "@context": {
            "csvw": "http://www.w3.org/ns/csvw#",
            "dcterms": "http://purl.org/dc/terms/",
            "qudt": "https://qudt.org/schema/qudt/",
            "schema": "https://schema.org/",
            "skos": "http://www.w3.org/2004/02/skos/core#",
            "xsd": "http://www.w3.org/2001/XMLSchema#",
            "caseId": "dcterms:identifier",
            "generatedAt": {
                "@id": "dcterms:created",
                "@type": "xsd:dateTime",
            },
            "source": {
                "@id": "dcterms:source",
                "@type": "@id",
            },
            "columns": "csvw:column",
            "rows": "csvw:row",
            "label": "skos:prefLabel",
            "unit": "qudt:unit",
            "unitURI": {
                "@id": "qudt:unit",
                "@type": "@id",
            },
            "catalogVariable": {
                "@id": "dcterms:references",
                "@type": "@id",
            },
            "index": "schema:position",
            "name": "schema:name",
        },
        "@type": "csvw:Table",
        "caseId": case_id,
        "generatedAt": exported_at,
        "source": _DCTERMS_SOURCES,
        "columns": columns,
        "rows": rows,
    }

    with open(output_jsonld, "w", encoding="utf-8") as handle:
        json.dump(payload_jsonld, handle, indent=2)
        handle.write("\n")

    metadata_input_files = dict(input_payload["files"])
    metadata_input_files["structured_input"] = _file_record(
        case_dir, "input.json", "structured_input"
    )
    output_files = {
        "native_output": _file_record(case_dir, "output.txt", "native_output"),
        "structured_output": _file_record(case_dir, "output.json", "structured_output"),
        "semantic_output": _file_record(case_dir, "output.jsonld", "semantic_output"),
    }
    payload_case_metadata = _build_case_metadata(
        case_id,
        exported_at,
        metadata_input_files,
        output_files,
        _software_provenance(case_dir),
        {
            "@id": _case_measurement_id(case_id),
            "quantity": "Intergranular gas swelling",
            "value": experimental_swelling,
            "unit": "%",
            "source": _EXPERIMENTAL_MEASUREMENTS,
        },
    )

    with open(case_metadata_jsonld, "w", encoding="utf-8") as handle:
        json.dump(payload_case_metadata, handle, indent=2)
        handle.write("\n")

    return input_json, output_json, output_jsonld, case_metadata_jsonld


def _discover_white_cases(white_root: str) -> List[str]:
    """Return sorted White case directories under ``regression/white``."""
    cases = []
    for name in sorted(os.listdir(white_root)):
        if not name.startswith("test_White"):
            continue
        case_dir = os.path.join(white_root, name)
        if os.path.isdir(case_dir):
            cases.append(case_dir)
    return cases


def main() -> int:
    """Regenerate all White semantic exports and print the case count."""
    white_root = os.path.dirname(__file__)
    exported = 0
    export_white_experimental_measurements(white_root)
    export_variable_catalog(white_root)

    for case_dir in _discover_white_cases(white_root):
        output_txt = os.path.join(case_dir, "output.txt")
        if not os.path.isfile(output_txt):
            continue
        export_white_case_semantic_outputs(case_dir)
        exported += 1

    print(f"Exported machine-readable files for {exported} White case(s).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
