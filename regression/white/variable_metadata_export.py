#!/usr/bin/env python3
"""### NEO4MAT - SCIANTIX-DIVA: SCIANTIX variable catalog export.

This module generates a global JSON-LD catalog of SCIANTIX variables and
material/system properties used by the White case-study metadata. The catalog
is derived from source-code declarations so it can stay aligned with SCIANTIX
without manual intervention.
"""

import json
import os
import re
import subprocess
from datetime import datetime, timezone
from typing import Dict, List


_CONTEXT = {
    "dcterms": "http://purl.org/dc/terms/",
    "dcat": "http://www.w3.org/ns/dcat#",
    "nmkos": "https://w3id.org/nm-kos/terms#",
    "model": "https://w3id.org/nm-kos/sciantix/model#",
    "prov": "http://www.w3.org/ns/prov#",
    "qudt": "https://qudt.org/schema/qudt/",
    "schema": "https://schema.org/",
    "skos": "http://www.w3.org/2004/02/skos/core#",
    "sosa": "http://www.w3.org/ns/sosa/",
    "ssn": "http://www.w3.org/ns/ssn/",
    "xsd": "http://www.w3.org/2001/XMLSchema#",
    "identifier": "dcterms:identifier",
    "title": "dcterms:title",
    "description": "dcterms:description",
    "source": "dcterms:source",
    "references": {
        "@id": "dcterms:references",
        "@type": "@id",
    },
    "variable": "ssn:hasProperty",
    "category": "dcterms:type",
    "name": "schema:name",
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
    "sourceFile": "nmkos:sourceFile",
    "sourceFunction": "nmkos:sourceFunction",
    "sourceClass": "nmkos:sourceClass",
    "sourceArray": "nmkos:sourceArray",
    "sourceArrayIndex": "nmkos:sourceArrayIndex",
    "sourceArrayReferences": "nmkos:sourceArrayReferences",
    "outputFlagExpression": "nmkos:outputFlagExpression",
    "cppType": "nmkos:cppType",
    "value": "schema:value",
    "valueSource": "nmkos:valueSource",
    "generatedAt": {
        "@id": "dcterms:created",
        "@type": "xsd:dateTime",
    },
}

# ---------------------------------------------------------------------------
# Unit URI map: QUDT vocabulary for standard SI/physical units; nmkos/unit#
# for SCIANTIX domain-specific units that have no QUDT equivalent yet.
# ---------------------------------------------------------------------------
_NMKOS_UNIT_BASE = "https://w3id.org/nm-kos/unit#"

UNIT_URI_MAP: Dict[str, str] = {
    # Standard SI / physical units  →  QUDT vocab unit IRI
    "h":             "https://qudt.org/vocab/unit/HR",
    "K":             "https://qudt.org/vocab/unit/K",
    "m":             "https://qudt.org/vocab/unit/M",
    "m2":            "https://qudt.org/vocab/unit/M2",
    "m3":            "https://qudt.org/vocab/unit/M3",
    "m2/s":          "https://qudt.org/vocab/unit/M2-PER-SEC",
    "kg/m3":         "https://qudt.org/vocab/unit/KiloGM-PER-M3",
    "MPa":           "https://qudt.org/vocab/unit/MegaPA",
    "atm":           "https://qudt.org/vocab/unit/ATM",
    "J/m2":          "https://qudt.org/vocab/unit/J-PER-M2",
    "N/m":           "https://qudt.org/vocab/unit/N-PER-M",
    "1/s":           "https://qudt.org/vocab/unit/PER-SEC",
    "dimensionless": "https://qudt.org/vocab/unit/UNITLESS",
    "/":             "https://qudt.org/vocab/unit/UNITLESS",
    "%":             "https://qudt.org/vocab/unit/PERCENT",
    "u":             "https://qudt.org/vocab/unit/AMU",
    "ug/g":          "https://qudt.org/vocab/unit/MicroGM-PER-GM",
    "µg/g":          "https://qudt.org/vocab/unit/MicroGM-PER-GM",
    "rad":           "https://qudt.org/vocab/unit/RAD",
    "KJ/mol":        "https://qudt.org/vocab/unit/KiloJ-PER-MOL",
    "MW/kg":         "https://qudt.org/vocab/unit/MegaW-PER-KiloGM",
    # SCIANTIX domain-specific units  →  nmkos/unit# (preliminary)
    "at/m3":         _NMKOS_UNIT_BASE + "AtomPerM3",
    "at/m3 s":       _NMKOS_UNIT_BASE + "AtomPerM3PerSec",
    "at/bub":        _NMKOS_UNIT_BASE + "AtomPerBubble",
    "at/pore":       _NMKOS_UNIT_BASE + "AtomPerPore",
    "at^2/m3":       _NMKOS_UNIT_BASE + "Atom2PerM3",
    "at^2/pore":     _NMKOS_UNIT_BASE + "Atom2PerPore",
    "bub/m2":        _NMKOS_UNIT_BASE + "BubblePerM2",
    "bub/m3":        _NMKOS_UNIT_BASE + "BubblePerM3",
    "pores/m3":      _NMKOS_UNIT_BASE + "PorePerM3",
    "vac/bub":       _NMKOS_UNIT_BASE + "VacancyPerBubble",
    "fiss / m3 s":   _NMKOS_UNIT_BASE + "FissionPerM3PerSec",
    "at/fission":    _NMKOS_UNIT_BASE + "AtomPerFission",
    "MWd/kgUO2":     _NMKOS_UNIT_BASE + "MWdPerKgUO2",
    "% weight/UO2":  _NMKOS_UNIT_BASE + "WeightPercentPerUO2",
    "weight%/UO2":   _NMKOS_UNIT_BASE + "WeightPercentPerUO2",
}


def unit_uri(unit_label: str) -> str:
    """Return a QUDT or nmkos unit URI for *unit_label*, or empty string if unknown."""
    return UNIT_URI_MAP.get(unit_label, "")


_SET_VARIABLES_SOURCE = "src/operations/SetVariablesFunctions.C"
_CLASS_SOURCES = {
    "Material": ("include/classes/Material.h", "material_property"),
    "Matrix": ("include/classes/Matrix.h", "matrix_property"),
    "Gas": ("include/classes/Gas.h", "gas_property"),
    "System": ("include/classes/System.h", "system_property"),
}
_MODEL_CATALOG = "../models/sciantix_physical_models.jsonld"
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

_SET_SOURCES = {
    "Matrix": "src/operations/SetMatrix.C",
    "Gas":    "src/operations/SetGas.C",
    "System": "src/operations/SetSystem.C",
}

# Units not annotated with an inline comment in Set*.C source files.
_UNIT_FALLBACK: Dict[str, str] = {
    "Matrix.lattice_parameter":            "m",
    "Matrix.lenticular_shape_factor":      "/",
    "Matrix.grain_boundary_mobility":      "unspecified",
    "Matrix.grain_boundary_diffusivity":   "m2/s",
    "Matrix.grain_radius":                 "m",
    "Matrix.nucleation_rate":              "1/s",
    "Matrix.pore_nucleation_rate":         "1/s",
    "Matrix.pore_resolution_rate":         "1/s",
    "Matrix.pore_trapping_rate":           "1/s",
    "Matrix.chromium_content":             "ug/g",
    "Matrix.chromium_solubility":          "% weight/UO2",
    "Matrix.Cr2O3_solubility":             "% weight/UO2",
    "Matrix.chromium_solution":            "at/m3",
    "Matrix.chromium_precipitate":         "at/m3",
    "Matrix.chromia_solution":             "at/m3",
    "Matrix.chromia_precipitate":          "at/m3",
    "Matrix.elastic_modulus":              "MPa",
    "Matrix.shear_modulus":                "MPa",
    "Gas.atomic_number":                   "/",
    "Gas.mass_number":                     "u",
    "Gas.van_der_waals_volume":            "m3",
    "Gas.decay_rate":                      "1/s",
    "Gas.precursor_factor":                "/",
    "System.yield":                        "at/fission",
    "System.radius_in_lattice":            "m",
    "System.volume_in_lattice":            "m3",
    "System.henry_constant":               "unspecified",
    "System.diffusivity":                  "m2/s",
    "System.bubble_diffusivity":           "m2/s",
    "System.resolution_rate":              "1/s",
    "System.trapping_rate":                "1/s",
    "System.nucleation_rate":              "1/s",
    "System.pore_nucleation_rate":         "1/s",
    "System.production_rate":              "unspecified",
    "System.modes":                        "/",
}


def _setter_to_label(setter_name: str) -> str:
    """Convert 'setFissionFragmentRange' → 'Fission fragment range'."""
    without_set = re.sub(r'^set', '', setter_name)
    words = re.sub(r'([A-Z])', r' \1', without_set).strip().split()
    if not words:
        return setter_name
    return words[0] + (' ' + ' '.join(w.lower() for w in words[1:]) if len(words) > 1 else '')


def _unit_from_comment(comment: str) -> str:
    """Extract unit string from a C++ inline comment (text after //).

    Handles two common patterns used in SCIANTIX:
    - Parenthesized unit: ``// (kg/m3) optional description``
    - Bare unit symbol:   ``// K``
    """
    comment = comment.strip()
    m = re.match(r'\(([^)]+)\)', comment)
    if m:
        return m.group(1).strip()
    m = re.match(r'^([A-Za-z][A-Za-z0-9/*^.%]*)(?:\s|$)', comment)
    if m:
        candidate = m.group(1)
        if (len(candidate) <= 8
                and candidate.lower() not in {
                    'from', 'the', 'this', 'note', 'see', 'ref', 'value', 'number'
                }):
            return candidate
    return ""


def _parse_literal_setter_calls(body: str, obj_var: str):
    """Yield (setter_name, numeric_value, comment) for ``obj_var.setXxx(literal)`` calls."""
    pattern = re.compile(
        r'\b' + re.escape(obj_var) + r'\.(set[A-Za-z0-9_]+)\s*\(\s*'
        r'([+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?)\s*\)\s*;'
        r'[ \t]*(?://\s*(.*))?',
    )
    for m in pattern.finditer(body):
        setter  = m.group(1)
        val_str = m.group(2)
        comment = (m.group(3) or '').strip()
        val = float(val_str) if ('.' in val_str or 'e' in val_str.lower()) else int(val_str)
        yield setter, val, comment


def _parse_setter_to_field_map(source: str) -> Dict[str, str]:
    """Map setter method name → member field name from setter implementations.

    Works for both inline header setters and ``ClassName::setter`` definitions.
    """
    result: Dict[str, str] = {}
    sig_re = re.compile(
        r'void\s+(?:\w+::)?(set[A-Z][A-Za-z0-9_]*)\s*\(\s*\w+\s+(\w+)\s*\)'
    )
    for m in sig_re.finditer(source):
        setter = m.group(1)
        param  = m.group(2)
        try:
            body = _find_function_body(source, setter)
        except ValueError:
            continue
        assign = re.search(
            r'\b([a-z_][a-z0-9_]*)\s*=\s*' + re.escape(param) + r'\b',
            body,
        )
        if assign:
            result[setter] = assign.group(1)
    return result


def _parse_factory_constants(
    source: str,
    obj_var: str,
    factory_re: re.Pattern,
    setter_map: Dict[str, str],
    source_path: str,
) -> Dict[str, Dict]:
    """Parse per-instance constant values from named factory functions in Set*.C.

    Each factory function (e.g. ``UO2(...)``, ``Xe_in_UO2(...)``) creates one
    named instance; the instance name comes from ``obj_var.setName(\"...\")``.
    """
    constants: Dict[str, Dict] = {}
    for func_match in factory_re.finditer(source):
        func_name = func_match.group(1)
        try:
            # Slice the source from the definition site so _find_function_body
            # cannot accidentally match an earlier call-site occurrence.
            body = _find_function_body(source[func_match.start():], func_name)
        except ValueError:
            continue
        name_m = re.search(
            r'\b' + re.escape(obj_var) + r'\.setName\s*\(\s*"([^"]+)"', body
        )
        instance = name_m.group(1) if name_m else func_name
        for setter, value, comment in _parse_literal_setter_calls(body, obj_var):
            field = setter_map.get(setter)
            if field is None:
                continue
            label = _setter_to_label(setter)
            unit  = _unit_from_comment(comment)
            if field not in constants:
                constants[field] = {
                    "label": label,
                    "instances": {},
                    "unit": unit,
                    "valueSource": source_path,
                }
            constants[field]["instances"][instance] = value
            if unit and not constants[field]["unit"]:
                constants[field]["unit"] = unit
    return constants


def _parse_gas_constants(
    source: str,
    setter_map: Dict[str, str],
    source_path: str,
) -> Dict[str, Dict]:
    """Parse gas species constant values from SetGas.C.

    The source is split at every ``gas.push(gas_)`` call; each preceding
    segment corresponds to one named gas species.
    """
    constants: Dict[str, Dict] = {}
    segments = re.split(r'gas\.push\s*\(\s*gas_\s*\)', source)
    for seg in segments[:-1]:
        name_m   = re.search(r'gas_\.setName\s*\(\s*"([^"]+)"', seg)
        instance = name_m.group(1) if name_m else "unknown"
        for setter, value, comment in _parse_literal_setter_calls(seg, "gas_"):
            field = setter_map.get(setter)
            if field is None:
                continue
            label = _setter_to_label(setter)
            unit  = _unit_from_comment(comment)
            if field not in constants:
                constants[field] = {
                    "label": label,
                    "instances": {},
                    "unit": unit,
                    "valueSource": source_path,
                }
            constants[field]["instances"][instance] = value
            if unit and not constants[field]["unit"]:
                constants[field]["unit"] = unit
    return constants


def _parse_all_class_constants(repo_root: str) -> Dict[str, Dict]:
    """Parse constant property values from Set*.C and class implementation files.

    Returns a dict ``{class_name: {field_name: {label, instances, unit, valueSource}}}``
    derived entirely from the C++ source without hardcoding.
    """
    header_sources = {
        cn: _read_repo_file(repo_root, path)
        for cn, (path, _) in _CLASS_SOURCES.items()
    }
    try:
        system_c = _read_repo_file(repo_root, "src/classes/System.C")
    except OSError:
        system_c = ""

    setter_maps = {
        "Matrix":   _parse_setter_to_field_map(header_sources["Matrix"]),
        "Gas":      _parse_setter_to_field_map(header_sources["Gas"]),
        "System":   _parse_setter_to_field_map(system_c),
        "Material": _parse_setter_to_field_map(header_sources["Material"]),
    }

    result: Dict[str, Dict] = {}

    try:
        matrix_src = _read_repo_file(repo_root, _SET_SOURCES["Matrix"])
        result["Matrix"] = _parse_factory_constants(
            matrix_src, "matrix_",
            re.compile(r'\bMatrix\s+(\w+)\s*\('),
            setter_maps["Matrix"],
            _SET_SOURCES["Matrix"],
        )
    except OSError:
        result["Matrix"] = {}

    try:
        gas_src = _read_repo_file(repo_root, _SET_SOURCES["Gas"])
        result["Gas"] = _parse_gas_constants(
            gas_src, setter_maps["Gas"], _SET_SOURCES["Gas"]
        )
    except OSError:
        result["Gas"] = {}

    try:
        system_src = _read_repo_file(repo_root, _SET_SOURCES["System"])
        result["System"] = _parse_factory_constants(
            system_src, "system_",
            re.compile(r'\bSystem\s+(\w+)\s*\('),
            setter_maps["System"],
            _SET_SOURCES["System"],
        )
    except OSError:
        result["System"] = {}

    return result


def _repo_root_from_white_root(white_root: str) -> str:
    """Return the SCIANTIX repository root from ``regression/white``."""
    return os.path.abspath(os.path.join(white_root, "..", ".."))


def _read_repo_file(repo_root: str, relative_path: str) -> str:
    """Read a repository file using a path relative to the repo root."""
    with open(os.path.join(repo_root, relative_path), "r", encoding="utf-8") as handle:
        return handle.read()


def _git_value(args: List[str], cwd: str) -> str:
    """Run a Git query and return ``unknown`` when provenance is unavailable."""
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


def _generated_at_utc(repo_root: str) -> str:
    """Return the catalog generation timestamp.

    By default the timestamp is the current Git commit time, which avoids
    unnecessary diffs when regenerating identical metadata. The environment
    variable ``SCIANTIX_SEMANTIC_GENERATED_AT_UTC`` can override it.
    """
    override = os.environ.get("SCIANTIX_SEMANTIC_GENERATED_AT_UTC")
    if override:
        return datetime.fromisoformat(override).astimezone(timezone.utc).isoformat()

    value = _git_value(["show", "-s", "--format=%cI", "HEAD"], repo_root)
    if value == "unknown":
        return datetime.now(timezone.utc).isoformat()
    try:
        return datetime.fromisoformat(value).astimezone(timezone.utc).isoformat()
    except ValueError:
        return datetime.now(timezone.utc).isoformat()


def _slug(value: str) -> str:
    """Create a stable lowercase identifier fragment from a variable name."""
    slug = re.sub(r"[^a-z0-9]+", "-", value.lower()).strip("-")
    return slug or "unnamed"


def _strip_cpp_string(value: str) -> str:
    """Remove surrounding C++ string quotes when present."""
    value = value.strip()
    if len(value) >= 2 and value[0] == '"' and value[-1] == '"':
        return value[1:-1]
    return value


def _normalize_unit(unit: str) -> str:
    """Normalize SCIANTIX constructor units such as ``(m)`` to ``m``."""
    unit = _strip_cpp_string(unit).strip()
    if unit.startswith("(") and unit.endswith(")"):
        unit = unit[1:-1].strip()
    return unit or "unspecified"


def _compact_expression(value: str) -> str:
    """Collapse whitespace in C++ expressions recorded as metadata."""
    return re.sub(r"\s+", " ", value.strip())


def _find_function_body(source: str, function_name: str) -> str:
    """Extract a C++ function body with balanced-brace parsing.

    Regex alone is fragile for function bodies. This scanner handles strings
    and nested braces well enough for the SCIANTIX initializer functions used
    by the catalog.
    """
    match = re.search(r"\b" + re.escape(function_name) + r"\s*\([^)]*\)", source)
    if not match:
        raise ValueError(f"Cannot find function: {function_name}")

    start = source.find("{", match.end())
    if start < 0:
        raise ValueError(f"Cannot find function body: {function_name}")

    depth = 0
    in_string = False
    escaped = False
    for idx in range(start, len(source)):
        char = source[idx]
        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            continue
        if char == '"':
            in_string = True
        elif char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if depth == 0:
                return source[start + 1 : idx]

    raise ValueError(f"Unbalanced function body: {function_name}")


def _split_top_level_commas(value: str) -> List[str]:
    """Split constructor arguments while respecting nested expressions."""
    parts = []
    start = 0
    depth_round = 0
    depth_square = 0
    depth_brace = 0
    in_string = False
    escaped = False

    for idx, char in enumerate(value):
        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            continue
        if char == '"':
            in_string = True
        elif char == "(":
            depth_round += 1
        elif char == ")":
            depth_round -= 1
        elif char == "[":
            depth_square += 1
        elif char == "]":
            depth_square -= 1
        elif char == "{":
            depth_brace += 1
        elif char == "}":
            depth_brace -= 1
        elif (
            char == ","
            and depth_round == 0
            and depth_square == 0
            and depth_brace == 0
        ):
            parts.append(value[start:idx].strip())
            start = idx + 1

    parts.append(value[start:].strip())
    return parts


def _extract_constructor_args(body: str, constructor_name: str) -> List[List[str]]:
    """Extract argument lists for constructor calls inside a C++ body."""
    args = []
    search = constructor_name + "("
    idx = 0
    while True:
        start = body.find(search, idx)
        if start < 0:
            return args
        pos = start + len(search)
        depth = 1
        in_string = False
        escaped = False
        while pos < len(body):
            char = body[pos]
            if in_string:
                if escaped:
                    escaped = False
                elif char == "\\":
                    escaped = True
                elif char == '"':
                    in_string = False
            elif char == '"':
                in_string = True
            elif char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
                if depth == 0:
                    args.append(_split_top_level_commas(body[start + len(search) : pos]))
                    idx = pos + 1
                    break
            pos += 1


def _extract_string_list(function_body: str) -> List[str]:
    """Extract quoted names from simple C++ string-vector initializers."""
    return re.findall(r'"([^"]+)"', function_body)


def _source_array_refs(args: List[str]) -> List[Dict[str, int]]:
    """Find ``Sciantix_variables[...]`` and ``Sciantix_history[...]`` refs."""
    refs = []
    seen = set()
    for arg in args:
        for array_name, index in re.findall(r"(Sciantix_(?:variables|history))\[(\d+)\]", arg):
            key = (array_name, int(index))
            if key in seen:
                continue
            seen.add(key)
            refs.append({"sourceArray": array_name, "sourceArrayIndex": int(index)})
    return refs


def _variable_id(category: str, name: str) -> str:
    """Build a stable local variable IRI from category and name."""
    return f"sciantix-variable:{category}:{_slug(name)}"


def _source_code_description(category: str, name: str, source: str) -> str:
    """Create a short human-readable description for generated entries."""
    return (
        f"SCIANTIX {category.replace('_', ' ')} '{name}', generated from the "
        f"source-code definition in {source}."
    )


def _state_variable_models(name: str) -> List[str]:
    """Infer conservative physical-model links from state-variable names.

    These links support navigation from variables to the local model catalog.
    They are intentionally pattern-based and conservative; ambiguous variables
    are left unlinked rather than over-interpreted.
    """
    lower = name.lower()
    models = []

    if "grain radius" in lower:
        models.append("model:grain-growth")
    if "produced" in lower and any(gas in lower for gas in ("xe", "kr", "he")):
        models.append("model:gas-production")
    if "diffusion coefficient" in lower:
        models.append("model:gas-diffusion")
    if "intragranular" in lower or "in grain" in lower:
        models.append("model:intragranular-bubble-behaviour")
    if "intergranular" in lower or "grain boundary" in lower or "released" in lower:
        models.append("model:intergranular-bubble-behavior")
    if "intactness" in lower:
        models.append("model:grain-boundary-micro-cracking")
    if "vent" in lower:
        models.append("model:grain-boundary-venting")
    if "porosity" in lower or "densification" in lower or "fuel density" in lower:
        models.append("model:densification")

    return list(dict.fromkeys(models))


def _add_model_links(variable: dict, model_ids: List[str]) -> None:
    """Attach model references to a variable entry when available."""
    if not model_ids:
        return
    variable["references"] = model_ids


def _input_setting_variables(body: str) -> List[dict]:
    """Generate metadata entries for SCIANTIX input setting names."""
    variables = []
    for index, name in enumerate(_extract_string_list(body)):
        variables.append(
            {
                "@id": _variable_id("input_setting", name),
                "@type": "prov:Entity",
                "identifier": name,
                "title": name,
                "description": _source_code_description(
                    "input_setting", name, _SET_VARIABLES_SOURCE
                ),
                "category": "input_setting",
                "name": name,
                "label": name,
                "unit": "dimensionless",
                "unitURI": unit_uri("dimensionless"),
                "source": _SET_VARIABLES_SOURCE,
                "sourceFile": _SET_VARIABLES_SOURCE,
                "sourceFunction": "getInputVariableNames",
                "sourceArrayIndex": index,
            }
        )
        _add_model_links(variables[-1], _SETTING_MODEL_MAP.get(name, []))
    return variables


def _scaling_factor_variables(body: str) -> List[dict]:
    """Generate metadata entries for SCIANTIX scaling factor names."""
    variables = []
    for index, name in enumerate(_extract_string_list(body)):
        variables.append(
            {
                "@id": _variable_id("scaling_factor", name),
                "@type": "prov:Entity",
                "identifier": name,
                "title": name,
                "description": _source_code_description(
                    "scaling_factor", name, _SET_VARIABLES_SOURCE
                ),
                "category": "scaling_factor",
                "name": name,
                "label": name,
                "unit": "dimensionless",
                "unitURI": unit_uri("dimensionless"),
                "source": _SET_VARIABLES_SOURCE,
                "sourceFile": _SET_VARIABLES_SOURCE,
                "sourceFunction": "getScalingFactorsNames",
                "sourceArrayIndex": index,
            }
        )
    return variables


def _sciantix_variables(body: str, category: str, function_name: str) -> List[dict]:
    """Generate metadata for history or state ``SciantixVariable`` entries.

    The parser records the declared label, unit, output flag expression, and
    array references used by each constructor call.
    """
    variables = []
    for ordinal, args in enumerate(_extract_constructor_args(body, "SciantixVariable")):
        if len(args) < 5:
            continue
        name = _strip_cpp_string(args[0])
        unit_label = _normalize_unit(args[1])
        variable = {
            "@id": _variable_id(category, name),
            "@type": ["ssn:Property", "prov:Entity"],
            "identifier": name,
            "title": name,
            "description": _source_code_description(category, name, _SET_VARIABLES_SOURCE),
            "category": category,
            "name": name,
            "label": name,
            "unit": unit_label,
            "source": _SET_VARIABLES_SOURCE,
            "sourceFile": _SET_VARIABLES_SOURCE,
            "sourceFunction": function_name,
            "sourceArrayIndex": ordinal,
            "outputFlagExpression": _compact_expression(args[4]),
        }
        _uri = unit_uri(unit_label)
        if _uri:
            variable["unitURI"] = _uri
        refs = _source_array_refs(args[2:4])
        if refs:
            variable["sourceArrayReferences"] = refs
        if category == "state_variable":
            _add_model_links(variable, _state_variable_models(name))
        variables.append(variable)
    return variables


def _class_member_variables(
    repo_root: str,
    class_name: str,
    relative_path: str,
    category: str,
    parsed_constants: Dict[str, Dict],
) -> List[dict]:
    """Generate metadata for Material/Matrix/Gas/System class fields.

    Unit, label and constant value are derived automatically from the C++
    Set*.C source files; a fallback table covers properties whose source
    comments do not include a unit annotation.
    """
    source = _read_repo_file(repo_root, relative_path)
    members = []
    for match in re.finditer(
        r"^\s*(?P<type>std::string|std::vector<double>|double|int|bool|Gas|Matrix)\s+"
        r"(?P<name>[A-Za-z_][A-Za-z0-9_]*)\s*;",
        source,
        flags=re.MULTILINE,
    ):
        cpp_type     = match.group("type")
        name         = match.group("name")
        property_key = f"{class_name}.{name}"
        field_data   = parsed_constants.get(name, {})

        unit  = field_data.get("unit") or _UNIT_FALLBACK.get(property_key, "unspecified")
        label = field_data.get("label") or name.replace("_", " ")

        member: dict = {
            "@id":         _variable_id(category, property_key),
            "@type":       ["ssn:Property", "prov:Entity"],
            "identifier":  property_key,
            "title":       label,
            "description": _source_code_description(category, name, relative_path),
            "category":    category,
            "name":        name,
            "label":       label,
            "unit":        unit,
            "cppType":     cpp_type,
            "source":      relative_path,
            "sourceFile":  relative_path,
            "sourceClass": class_name,
        }
        _uri = unit_uri(unit)
        if _uri:
            member["unitURI"] = _uri

        instances = field_data.get("instances", {})
        if instances:
            vals = list(instances.values())
            unique = {round(v, 15) if isinstance(v, float) else v for v in vals}
            member["value"] = vals[0] if len(unique) == 1 else dict(instances)
            if field_data.get("valueSource"):
                member["valueSource"] = field_data["valueSource"]

        members.append(member)
    return members


def build_variable_catalog(white_root: str) -> dict:
    """Build the complete SCIANTIX variable catalog JSON-LD payload."""
    repo_root = _repo_root_from_white_root(white_root)
    source = _read_repo_file(repo_root, _SET_VARIABLES_SOURCE)
    variables = []

    variables.extend(_input_setting_variables(_find_function_body(source, "getInputVariableNames")))
    variables.extend(
        _sciantix_variables(
            _find_function_body(source, "initializeHistoryVariable"),
            "history_variable",
            "initializeHistoryVariable",
        )
    )
    variables.extend(
        _sciantix_variables(
            _find_function_body(source, "initializeSciantixVariable"),
            "state_variable",
            "initializeSciantixVariable",
        )
    )
    variables.extend(_scaling_factor_variables(_find_function_body(source, "getScalingFactorsNames")))

    class_constants = _parse_all_class_constants(repo_root)
    for class_name, (relative_path, category) in _CLASS_SOURCES.items():
        variables.extend(
            _class_member_variables(
                repo_root, class_name, relative_path, category,
                class_constants.get(class_name, {}),
            )
        )

    return {
        "@context": _CONTEXT,
        "@type": "dcat:Dataset",
        "dcterms:identifier": "sciantix-variable-catalog",
        "dcterms:title": "SCIANTIX variable metadata catalog",
        "dcterms:description": (
            "Machine-readable catalog of SCIANTIX input settings, scaling factors, "
            "history variables, state variables, and Material/Matrix/Gas/System class "
            "properties used to support RDF-mappable case-study metadata."
        ),
        "dcterms:references": _MODEL_CATALOG,
        "generatedAt": _generated_at_utc(repo_root),
        "variable": variables,
    }


def export_variable_catalog(white_root: str) -> str:
    """Write the SCIANTIX variable catalog under ``metadata/variables``."""
    output_dir = os.path.join(white_root, "metadata", "variables")
    os.makedirs(output_dir, exist_ok=True)
    output_path = os.path.join(output_dir, "sciantix_variable_catalog.jsonld")

    with open(output_path, "w", encoding="utf-8") as handle:
        json.dump(build_variable_catalog(white_root), handle, indent=2)
        handle.write("\n")

    return output_path


def main() -> int:
    """CLI entry point for regenerating only the variable catalog."""
    white_root = os.path.dirname(__file__)
    output_path = export_variable_catalog(white_root)
    print(f"Exported SCIANTIX variable catalog: {output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
