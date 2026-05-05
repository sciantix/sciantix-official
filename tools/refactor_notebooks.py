"""One-shot refactor: remove duplicated un_model definitions from notebooks.

For each .ipynb cell:
  - parse with ast
  - find top-level FunctionDef / AsyncFunctionDef / ClassDef whose name exists
    in un_model and whose body is bit-identical (after whitespace normalisation)
  - remove those definitions
  - if any were removed, inject `from un_model import *` and a fast-overrides
    import after the cell's existing imports
  - clear the cell's cached outputs (they no longer match the source)

Conservative by design: any cell-defined function whose body differs from
un_model is kept untouched. Cell-specific helpers (run_temperature_sweep etc.)
are preserved because their names aren't in un_model.

Usage:
    python3 refactor_notebooks.py
"""

from __future__ import annotations

import ast
import inspect
import json
import sys
from pathlib import Path

import un_model

REPO = Path(__file__).resolve().parent

# Build {name: source} for every public function/class in un_model.
UN_MODEL_SOURCES: dict[str, str] = {}
for _name, _obj in vars(un_model).items():
    if _name.startswith("_"):
        continue
    if inspect.isfunction(_obj) or inspect.isclass(_obj):
        try:
            UN_MODEL_SOURCES[_name] = inspect.getsource(_obj)
        except (OSError, TypeError):
            pass


def normalise(src: str) -> str:
    return "\n".join(line.rstrip() for line in src.splitlines() if line.strip())


def get_node_source(node: ast.AST, source_lines: list[str]) -> str:
    start = node.lineno - 1
    end = node.end_lineno
    return "\n".join(source_lines[start:end])


def refactor_cell_source(source: str) -> tuple[str, int, list[str]]:
    """Returns (new_source, n_removed, removed_names)."""
    try:
        tree = ast.parse(source)
    except SyntaxError:
        return source, 0, []

    source_lines = source.splitlines()
    nodes_to_remove: list[ast.AST] = []
    removed_names: list[str] = []

    for node in tree.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            name = node.name
            if name not in UN_MODEL_SOURCES:
                continue
            cell_src = get_node_source(node, source_lines)
            if normalise(cell_src) == normalise(UN_MODEL_SOURCES[name]):
                nodes_to_remove.append(node)
                removed_names.append(name)

    if not nodes_to_remove:
        return source, 0, []

    # Lines to drop (1-indexed, inclusive on both ends).
    drop = set()
    for node in nodes_to_remove:
        for ln in range(node.lineno, node.end_lineno + 1):
            drop.add(ln)

    kept = [line for i, line in enumerate(source_lines, start=1) if i not in drop]
    new_src = "\n".join(kept)

    # Find the position right after the last existing top-level import.
    # Use AST again on the new source so line numbers are correct.
    try:
        new_tree = ast.parse(new_src)
    except SyntaxError:
        # Cell was somehow broken by removal; bail out and return original.
        return source, 0, []

    last_import_lineno = 0
    for node in new_tree.body:
        if isinstance(node, (ast.Import, ast.ImportFrom)):
            last_import_lineno = max(last_import_lineno, node.end_lineno or node.lineno)

    new_lines = new_src.splitlines()
    insert_at = last_import_lineno  # 0 means top of cell

    inject = [
        "",
        "# --- refactored: model definitions imported from un_model / un_model_fast ---",
        "from un_model import *  # full namespace of the reference scalar model",
        "from un_model_fast import run_model_point, solve_UN_fast, clear_run_cache  # vectorised overrides",
    ]

    new_lines = new_lines[:insert_at] + inject + new_lines[insert_at:]

    # Collapse runs of >2 blank lines to keep the cell readable.
    cleaned: list[str] = []
    blank_run = 0
    for line in new_lines:
        if line.strip() == "":
            blank_run += 1
            if blank_run > 2:
                continue
        else:
            blank_run = 0
        cleaned.append(line)

    return "\n".join(cleaned), len(nodes_to_remove), removed_names


def refactor_notebook(path: Path) -> tuple[int, int, dict[str, list[str]]]:
    nb = json.loads(path.read_text())
    cells_modified = 0
    defs_removed = 0
    by_cell: dict[str, list[str]] = {}
    for idx, cell in enumerate(nb.get("cells", [])):
        if cell.get("cell_type") != "code":
            continue
        original = "".join(cell.get("source", []))
        new_src, n_removed, removed_names = refactor_cell_source(original)
        if n_removed == 0:
            continue
        # ipynb cell source is a list of strings; preserve newlines correctly.
        new_lines_keep = new_src.splitlines(keepends=True)
        # If the new source ended without a trailing newline, that's fine for ipynb.
        cell["source"] = new_lines_keep if new_lines_keep else [""]
        cell["outputs"] = []
        cell["execution_count"] = None
        cells_modified += 1
        defs_removed += n_removed
        by_cell[f"cell{idx}"] = removed_names

    if cells_modified > 0:
        path.write_text(json.dumps(nb, indent=1) + "\n")
    return cells_modified, defs_removed, by_cell


def main() -> int:
    targets = [
        "UNmodel.ipynb",
        "UNpython_tests.ipynb",
        "2UNpython_tests.ipynb",
        "UN_Barani_model.ipynb",
        "b_g_nu_comparison.ipynb",
    ]
    print(f"un_model has {len(UN_MODEL_SOURCES)} public function/class definitions")
    print()
    grand_cells = 0
    grand_defs = 0
    for nb in targets:
        path = REPO / nb
        if not path.exists():
            print(f"{nb}: missing, skipping")
            continue
        cells, defs, by_cell = refactor_notebook(path)
        grand_cells += cells
        grand_defs += defs
        print(f"{nb}: {cells} cells modified, {defs} definitions removed")
        for cell_id, names in by_cell.items():
            print(f"  {cell_id}: {', '.join(names)}")
    print()
    print(f"TOTAL: {grand_cells} cells modified, {grand_defs} definitions removed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
