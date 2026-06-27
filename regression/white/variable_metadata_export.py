#!/usr/bin/env python3
"""Generate a SCIANTIX variable metadata catalog for the White case study."""

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
    "sourceFile": "nmkos:sourceFile",
    "sourceFunction": "nmkos:sourceFunction",
    "sourceClass": "nmkos:sourceClass",
    "sourceArray": "nmkos:sourceArray",
    "sourceArrayIndex": "nmkos:sourceArrayIndex",
    "sourceArrayReferences": "nmkos:sourceArrayReferences",
    "outputFlagExpression": "nmkos:outputFlagExpression",
    "cppType": "nmkos:cppType",
    "generatedAt": {
        "@id": "dcterms:created",
        "@type": "xsd:dateTime",
    },
}

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


def _repo_root_from_white_root(white_root: str) -> str:
    return os.path.abspath(os.path.join(white_root, "..", ".."))


def _read_repo_file(repo_root: str, relative_path: str) -> str:
    with open(os.path.join(repo_root, relative_path), "r", encoding="utf-8") as handle:
        return handle.read()


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


def _generated_at_utc(repo_root: str) -> str:
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
    slug = re.sub(r"[^a-z0-9]+", "-", value.lower()).strip("-")
    return slug or "unnamed"


def _strip_cpp_string(value: str) -> str:
    value = value.strip()
    if len(value) >= 2 and value[0] == '"' and value[-1] == '"':
        return value[1:-1]
    return value


def _normalize_unit(unit: str) -> str:
    unit = _strip_cpp_string(unit).strip()
    if unit.startswith("(") and unit.endswith(")"):
        unit = unit[1:-1].strip()
    return unit or "unspecified"


def _compact_expression(value: str) -> str:
    return re.sub(r"\s+", " ", value.strip())


def _find_function_body(source: str, function_name: str) -> str:
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
    return re.findall(r'"([^"]+)"', function_body)


def _source_array_refs(args: List[str]) -> List[Dict[str, int]]:
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
    return f"sciantix-variable:{category}:{_slug(name)}"


def _source_code_description(category: str, name: str, source: str) -> str:
    return (
        f"SCIANTIX {category.replace('_', ' ')} '{name}', generated from the "
        f"source-code definition in {source}."
    )


def _state_variable_models(name: str) -> List[str]:
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
    if not model_ids:
        return
    variable["references"] = model_ids


def _input_setting_variables(body: str) -> List[dict]:
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
                "source": _SET_VARIABLES_SOURCE,
                "sourceFile": _SET_VARIABLES_SOURCE,
                "sourceFunction": "getInputVariableNames",
                "sourceArrayIndex": index,
            }
        )
        _add_model_links(variables[-1], _SETTING_MODEL_MAP.get(name, []))
    return variables


def _scaling_factor_variables(body: str) -> List[dict]:
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
                "source": _SET_VARIABLES_SOURCE,
                "sourceFile": _SET_VARIABLES_SOURCE,
                "sourceFunction": "getScalingFactorsNames",
                "sourceArrayIndex": index,
            }
        )
    return variables


def _sciantix_variables(body: str, category: str, function_name: str) -> List[dict]:
    variables = []
    for ordinal, args in enumerate(_extract_constructor_args(body, "SciantixVariable")):
        if len(args) < 5:
            continue
        name = _strip_cpp_string(args[0])
        variable = {
            "@id": _variable_id(category, name),
            "@type": ["ssn:Property", "prov:Entity"],
            "identifier": name,
            "title": name,
            "description": _source_code_description(category, name, _SET_VARIABLES_SOURCE),
            "category": category,
            "name": name,
            "label": name,
            "unit": _normalize_unit(args[1]),
            "source": _SET_VARIABLES_SOURCE,
            "sourceFile": _SET_VARIABLES_SOURCE,
            "sourceFunction": function_name,
            "sourceArrayIndex": ordinal,
            "outputFlagExpression": _compact_expression(args[4]),
        }
        refs = _source_array_refs(args[2:4])
        if refs:
            variable["sourceArrayReferences"] = refs
        if category == "state_variable":
            _add_model_links(variable, _state_variable_models(name))
        variables.append(variable)
    return variables


def _class_member_variables(repo_root: str, class_name: str, relative_path: str, category: str) -> List[dict]:
    source = _read_repo_file(repo_root, relative_path)
    members = []
    for match in re.finditer(
        r"^\s*(?P<type>std::string|std::vector<double>|double|int|bool|Gas|Matrix)\s+"
        r"(?P<name>[A-Za-z_][A-Za-z0-9_]*)\s*;",
        source,
        flags=re.MULTILINE,
    ):
        cpp_type = match.group("type")
        name = match.group("name")
        members.append(
            {
                "@id": _variable_id(category, f"{class_name}.{name}"),
                "@type": ["ssn:Property", "prov:Entity"],
                "identifier": f"{class_name}.{name}",
                "title": name.replace("_", " "),
                "description": _source_code_description(category, name, relative_path),
                "category": category,
                "name": name,
                "label": name.replace("_", " "),
                "unit": "unspecified",
                "cppType": cpp_type,
                "source": relative_path,
                "sourceFile": relative_path,
                "sourceClass": class_name,
            }
        )
    return members


def build_variable_catalog(white_root: str) -> dict:
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

    for class_name, (relative_path, category) in _CLASS_SOURCES.items():
        variables.extend(_class_member_variables(repo_root, class_name, relative_path, category))

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
    output_dir = os.path.join(white_root, "metadata", "variables")
    os.makedirs(output_dir, exist_ok=True)
    output_path = os.path.join(output_dir, "sciantix_variable_catalog.jsonld")

    with open(output_path, "w", encoding="utf-8") as handle:
        json.dump(build_variable_catalog(white_root), handle, indent=2)
        handle.write("\n")

    return output_path


def main() -> int:
    white_root = os.path.dirname(__file__)
    output_path = export_variable_catalog(white_root)
    print(f"Exported SCIANTIX variable catalog: {output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
