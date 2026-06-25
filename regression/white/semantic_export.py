#!/usr/bin/env python3
"""White case semantic export utilities.

This module adds machine-readable exports to original SCIANTIX white cases
without changing the existing txt-based regression workflow.
"""

import hashlib
import json
import os
import re
import subprocess
from datetime import datetime, timezone
from typing import Dict, List, Tuple


_SCIANTIX_VERSION = "2.2.1"
_DCTERMS_SOURCES = [
    "../metadata/sources/white2004.jsonld",
    "../metadata/sources/ifpe_cagr_uox_swell.jsonld",
]
_MODEL_CATALOG = "../metadata/models/sciantix_physical_models.jsonld"
_MODEL_REFERENCES = "../metadata/sources/sciantix_model_references.jsonld"
_SOFTWARE_SOURCES = "../metadata/sources/sciantix_software_sources.jsonld"
_EXPERIMENTAL_MEASUREMENTS = "../metadata/experimental/white_experimental_measurements.jsonld"

_SCHEMA_RELATIVE_PATH = "metadata/schema/output.schema.json"
_INPUT_SCHEMA_RELATIVE_PATH = "metadata/schema/input.schema.json"
_EXPERIMENTAL_SWELLING_FILE = "data/ig_swelling.txt"
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
    """Split a TSV line and trim an optional trailing empty field."""
    parts = [p.strip() for p in line.rstrip("\n").split("\t")]
    while parts and parts[-1] == "":
        parts.pop()
    return parts


def _parse_label_and_unit(column_name: str) -> Tuple[str, str]:
    """Extract plain label and unit from headers like 'Time (h)'."""
    match = re.match(r"^(.*?)\s*\((.*?)\)\s*$", column_name)
    if match:
        return match.group(1).strip(), match.group(2).strip()
    return column_name.strip(), "dimensionless"


def _to_number_or_text(token: str):
    """Cast to number when possible, otherwise keep text."""
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
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _git_value(args: List[str], cwd: str) -> str:
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


def _software_provenance(case_dir: str) -> dict:
    repo_root = _git_value(["rev-parse", "--show-toplevel"], case_dir)
    git_cwd = repo_root if repo_root != "unknown" else case_dir
    dirty = _git_value(["status", "--porcelain"], git_cwd)

    return {
        "name": "SCIANTIX",
        "version": _SCIANTIX_VERSION,
        "role": "simulation code",
        "repository": "https://github.com/sciantix/sciantix-official",
        "project_branch_url": "https://github.com/sciantix/sciantix-official/tree/project/NEO4MAT_DIVA",
        "documentation": "https://sciantix.github.io/sciantix-official/models.html",
        "repository_branch": _git_value(["rev-parse", "--abbrev-ref", "HEAD"], git_cwd),
        "repository_commit": _git_value(["rev-parse", "HEAD"], git_cwd),
        "repository_is_dirty": dirty != "",
    }


def _file_record(case_dir: str, filename: str, role: str) -> dict:
    path = os.path.join(case_dir, filename)
    return {
        "role": role,
        "path": filename,
        "sha256": _sha256(path),
        "size_bytes": os.path.getsize(path),
    }


def _case_measurement_id(case_id: str) -> str:
    return f"white-measurement:{case_id}"


def _load_experimental_swelling(white_root: str) -> Dict[str, float]:
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
    """Export White validation measurements used by the regression workflow."""
    measurements = _load_experimental_swelling(white_root)
    output_dir = os.path.join(white_root, "metadata", "experimental")
    os.makedirs(output_dir, exist_ok=True)
    output_path = os.path.join(output_dir, "white_experimental_measurements.jsonld")
    exported_at = datetime.now(timezone.utc).isoformat()

    payload = {
        "@context": {
            "dcterms": "http://purl.org/dc/terms/",
            "nmkos": "https://w3id.org/nm-kos/terms#",
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
            "quantity": "nmkos:quantity",
            "value": "nmkos:value",
            "unit": "nmkos:unit",
            "measurement": "nmkos:measurement",
        },
        "@type": "nmkos:ExperimentalDataset",
        "dcterms:identifier": "white-2004-intergranular-swelling-validation-targets",
        "dcterms:title": "White intergranular swelling validation targets for SCIANTIX regression cases",
        "dcterms:description": "Case-level intergranular gas swelling values used by the SCIANTIX White regression parity workflow.",
        "generatedAt": exported_at,
        "source": _DCTERMS_SOURCES,
        "dcterms:relation": _EXPERIMENTAL_SWELLING_FILE,
        "measurement": [
            {
                "@id": _case_measurement_id(case_id),
                "@type": "nmkos:ExperimentalMeasurement",
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
    """Load legacy output.txt as (header, rows)."""
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
    rows = []
    with open(history_path, "r", encoding="utf-8") as handle:
        for line in handle:
            raw = line.strip()
            if not raw or raw.startswith("#"):
                continue
            rows.append([_to_number_or_text(token) for token in raw.split()])
    return rows


def _parse_initial_conditions(initial_conditions_path: str) -> List[dict]:
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


def _build_columns(header: List[str]):
    columns = []
    for idx, raw_name in enumerate(header):
        label, unit = _parse_label_and_unit(raw_name)
        columns.append(
            {
                "index": idx,
                "name": raw_name,
                "label": label,
                "unit": unit,
            }
        )
    return columns


def _validate_export_payload(payload_json: dict) -> None:
    """Perform minimal structural checks on exported JSON payload.
    """
    required_top = ["format_version", "case_id", "generated_at_utc", "dcterms_sources", "table"]
    for key in required_top:
        if key not in payload_json:
            raise ValueError(f"Missing top-level key in output.json: {key}")

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
    return {
        "@context": {
            "dcterms": "http://purl.org/dc/terms/",
            "nmkos": "https://w3id.org/nm-kos/terms#",
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
            "software": "nmkos:software",
            "modelCatalog": {
                "@id": "nmkos:modelCatalog",
                "@type": "@id",
            },
            "modelReferenceSource": {
                "@id": "nmkos:modelReferenceSource",
                "@type": "@id",
            },
            "softwareSource": {
                "@id": "nmkos:softwareSource",
                "@type": "@id",
            },
            "experimentalDataset": {
                "@id": "nmkos:experimentalDataset",
                "@type": "@id",
            },
            "validationTarget": "nmkos:validationTarget",
            "inputFile": "nmkos:inputFile",
            "outputFile": "nmkos:outputFile",
            "role": "nmkos:role",
            "path": "nmkos:path",
            "sha256": "nmkos:sha256",
            "sizeBytes": "nmkos:sizeBytes",
        },
        "@type": "nmkos:SimulationCase",
        "caseId": case_id,
        "generatedAt": exported_at,
        "source": _DCTERMS_SOURCES,
        "modelCatalog": _MODEL_CATALOG,
        "modelReferenceSource": _MODEL_REFERENCES,
        "softwareSource": _SOFTWARE_SOURCES,
        "experimentalDataset": _EXPERIMENTAL_MEASUREMENTS,
        "validationTarget": experimental_measurement,
        "software": software,
        "inputFile": list(input_files.values()),
        "outputFile": list(output_files.values()),
    }


def export_white_case_semantic_outputs(case_dir: str) -> Tuple[str, str, str, str]:
    """Export machine-readable files for a White regression case.
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
    columns = _build_columns(header)
    exported_at = datetime.now(timezone.utc).isoformat()
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
            "dcterms": "http://purl.org/dc/terms/",
            "nmkos": "https://w3id.org/nm-kos/terms#",
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
            "columns": "nmkos:columns",
            "rows": "nmkos:rows",
            "label": "nmkos:label",
            "unit": "nmkos:unit",
            "index": "nmkos:index",
            "name": "nmkos:name",
        },
        "@type": "nmkos:SimulationOutput",
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
    cases = []
    for name in sorted(os.listdir(white_root)):
        if not name.startswith("test_White"):
            continue
        case_dir = os.path.join(white_root, name)
        if os.path.isdir(case_dir):
            cases.append(case_dir)
    return cases


def main() -> int:
    white_root = os.path.dirname(__file__)
    exported = 0
    export_white_experimental_measurements(white_root)

    for case_dir in _discover_white_cases(white_root):
        output_txt = os.path.join(case_dir, "output.txt")
        if not os.path.isfile(output_txt):
            continue
        try:
            export_white_case_semantic_outputs(case_dir)
            exported += 1
        except Exception as err:  # pragma: no cover - utility path
            print(f"[WARNING] Semantic export failed for {case_dir}: {err}")

    print(f"Exported machine-readable files for {exported} White case(s).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
