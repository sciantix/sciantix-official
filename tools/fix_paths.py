"""One-shot patcher: make scripts/notebooks/shell-runners work after un_calibration/ reorg.

What this does (idempotent):

1. For every .py file under un_calibration/<subfolder>/ that has cross-imports
   or hardcoded result-folder strings, insert a 4-line header that adds
   un_calibration/ to sys.path and imports `_pathsetup` (which puts the
   sibling subfolders on sys.path and chdirs to un_calibration/).

2. Rewrite hardcoded result-folder string literals like
       "UN_M7_optuna_v14_..._results"
   to
       "results/UN_M7_optuna_v14_..._results"
   so they resolve correctly after the chdir into un_calibration/.

3. Patch the run_v*.sh shell wrappers in un_calibration/runners/ to:
   - chdir into un_calibration/ at start
   - call python on optuna/<script>.py
   - target --output-dir results/...
   - tee logs into logs/...

4. Patch the refactored notebook cells (those that contain
   `from un_model_fast import` or `from un_model import *`) to
   prepend a sys.path injection so the imports resolve when the
   notebook is run from un_calibration/notebooks/.

Run from the repo root:
    python3 tools/fix_paths.py
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
UN = REPO / "un_calibration"

PATH_HEADER = """\
# --- path setup (added by tools/fix_paths.py after un_calibration/ reorg) ---
import sys as _sys
from pathlib import Path as _Path
_sys.path.insert(0, str(_Path(__file__).resolve().parent.parent))
import _pathsetup  # noqa: F401
# --- end path setup ---
"""

PATH_HEADER_MARKER = "import _pathsetup  # noqa: F401"

# Hardcoded result-folder identifiers we expect at root level inside string
# literals. Order matters: longer/more specific first so we don't
# double-prefix ("results/results/UN_M7...").
RESULT_FOLDERS = [
    "UN_M7_optuna_v14_rhoSat_qgbStrict_NdAnchors_results",
    "UN_M7_optuna_v13b_rhoSat_qgbStrict_results",
    "UN_M7_optuna_v13_rhoSat_partition_results",
    "UN_M7_optuna_v12_rhoFT_slope_partition_results",
    "UN_M7_optuna_v11_rhoFT2_partition_results",
    "UN_M7_optuna_v10c_rhoFT2_partition_results",
    "UN_M7_optuna_v10_rhoFT_burnup_results",
    "UN_M7_optuna_v8_rhoFT_results",
    "UN_M7_optuna_v9_results",
    "UN_M7_optuna_v8_results",
    "UN_M7_optuna_v7_results",
    "UN_M7_optuna_v6_results",
    "UN_M7_optuna_v5_results",
    "UN_M7_optuna_v4_results",
    "UN_M7_optuna_v3_results",
    "UN_M7_optuna_v2_results",
    "UN_M7_optuna_results",
    "UN_M7_codex_results",
    "UN_M7_v5_codex_results",
    "UN_M7_v5_rizk_base_nocodex_results",
    "UN_M7_v6_B_sensitivity_results",
    "UN_M7_global_sensitivity_fullcell",
    "UN_M7_targeted_sensitivity_fullcell",
    "UN_M7_physical_sweep_outputs",
    "UN_M7_physics_diagnostics",
    "UN_M7_dislocation_vacancy_relaxation_diagnostics",
    "UN_M7_boundary_sink_diagnostics",
    "results_M7_sweep",
    "results_model_variants",
    "results_un_calibration",
]


def insert_path_header(src: str) -> tuple[str, bool]:
    """Insert PATH_HEADER after the docstring/shebang. Return (new_src, changed)."""
    if PATH_HEADER_MARKER in src:
        return src, False
    lines = src.splitlines(keepends=True)
    insert_at = 0
    # Skip shebang
    if lines and lines[0].startswith("#!"):
        insert_at = 1
    # Skip leading comments / blank lines
    while insert_at < len(lines) and (lines[insert_at].lstrip().startswith("#") or lines[insert_at].strip() == ""):
        insert_at += 1
    # Skip module docstring
    if insert_at < len(lines):
        first = lines[insert_at].lstrip()
        for q in ('"""', "'''"):
            if first.startswith(q):
                # find matching close
                j = insert_at
                if first.count(q) >= 2:
                    insert_at += 1
                    break
                j += 1
                while j < len(lines):
                    if q in lines[j]:
                        insert_at = j + 1
                        break
                    j += 1
                else:
                    break
                break
    # Skip blank lines after docstring
    while insert_at < len(lines) and lines[insert_at].strip() == "":
        insert_at += 1
    new_lines = lines[:insert_at] + [PATH_HEADER, "\n"] + lines[insert_at:]
    return "".join(new_lines), True


def rewrite_result_strings(src: str) -> tuple[str, int]:
    """Replace bare "X_results" / "results_M7_..." string literals with
    "results/X_results" so they resolve under un_calibration/ after chdir.
    Skip if the string already starts with "results/".
    """
    n = 0
    for folder in RESULT_FOLDERS:
        # match "FOLDER" or "FOLDER/anything" or 'FOLDER' or 'FOLDER/anything'
        pattern = re.compile(rf'(["\'])({re.escape(folder)})(/[^"\']*)?\1')

        def repl(m: re.Match) -> str:
            nonlocal n
            quote = m.group(1)
            tail = m.group(3) or ""
            n += 1
            return f"{quote}results/{folder}{tail}{quote}"

        src = pattern.sub(repl, src)
    return src, n


def process_python_file(path: Path) -> dict:
    src = path.read_text()
    new_src, header_added = insert_path_header(src)
    new_src, n_paths = rewrite_result_strings(new_src)
    if new_src != src:
        path.write_text(new_src)
    return {"path": str(path.relative_to(REPO)), "header": header_added, "paths": n_paths}


def process_shell_script(path: Path) -> dict:
    src = path.read_text()
    if "# --- path setup added by tools/fix_paths.py" in src:
        return {"path": str(path.relative_to(REPO)), "shell": False}

    lines = src.splitlines(keepends=True)
    insert_at = 0
    if lines and lines[0].startswith("#!"):
        insert_at = 1
    while insert_at < len(lines) and (
        lines[insert_at].startswith("#")
        or lines[insert_at].strip() == ""
        or lines[insert_at].startswith("set ")
    ):
        insert_at += 1

    chdir_block = (
        "# --- path setup added by tools/fix_paths.py after un_calibration/ reorg ---\n"
        'cd "$(dirname "$0")/.."   # cd into un_calibration/\n'
        "# --- end path setup ---\n\n"
    )
    new_lines = lines[:insert_at] + [chdir_block] + lines[insert_at:]
    new_src = "".join(new_lines)

    # python <bare>.py -> python optuna/<bare>.py
    new_src = re.sub(
        r"(?P<pre>python\d?\s+)(UN_M7_optuna_calibration[\w]*\.py)",
        r"\g<pre>optuna/\2",
        new_src,
    )
    # --output-dir UN_M7_..._results -> --output-dir results/UN_M7_..._results
    for folder in RESULT_FOLDERS:
        new_src = re.sub(
            rf"(--output-dir\s+){re.escape(folder)}",
            rf"\1results/{folder}",
            new_src,
        )
    # tee -a vN_*.log -> tee -a logs/vN_*.log
    new_src = re.sub(
        r"(tee\s+-a\s+)(v[\w\.]+\.log)",
        r"\1logs/\2",
        new_src,
    )

    if new_src != src:
        path.write_text(new_src)
    return {"path": str(path.relative_to(REPO)), "shell": True}


NB_HEADER_MARKER = "# path-setup added by tools/fix_paths.py"
NB_HEADER = (
    "# path-setup added by tools/fix_paths.py after un_calibration/ reorg\n"
    "import sys as _sys, pathlib as _pl\n"
    "_p = _pl.Path.cwd().resolve()\n"
    'while _p.name != "un_calibration" and _p.parent != _p:\n'
    "    _p = _p.parent\n"
    'for _sub in ("model", "optuna", "codex", "diagnostics"):\n'
    "    _q = str(_p / _sub)\n"
    "    if _q not in _sys.path:\n"
    "        _sys.path.insert(0, _q)\n"
    "del _sys, _pl, _p, _sub, _q\n"
    "\n"
)


def process_notebook(path: Path) -> dict:
    nb = json.loads(path.read_text())
    cells_patched = 0
    for cell in nb.get("cells", []):
        if cell.get("cell_type") != "code":
            continue
        src = "".join(cell.get("source", []))
        if "from un_model_fast import" not in src and "from un_model import" not in src:
            continue
        if NB_HEADER_MARKER in src:
            continue
        new_src = NB_HEADER + src
        new_lines = new_src.splitlines(keepends=True)
        cell["source"] = new_lines if new_lines else [""]
        cells_patched += 1
    if cells_patched > 0:
        path.write_text(json.dumps(nb, indent=1) + "\n")
    return {"path": str(path.relative_to(REPO)), "cells": cells_patched}


def main() -> int:
    print(f"Repo: {REPO}")
    print(f"un_calibration/: {UN}")
    if not UN.is_dir():
        print("ERROR: un_calibration/ not found", file=sys.stderr)
        return 2

    print("\n[1/3] Patching .py files...")
    py_results = []
    for py in sorted(UN.rglob("*.py")):
        if py.name == "_pathsetup.py":
            continue
        if "/results/" in str(py):
            continue
        py_results.append(process_python_file(py))
    n_header = sum(r["header"] for r in py_results)
    n_paths = sum(r["paths"] for r in py_results)
    print(f"  scanned {len(py_results)} files, header inserted in {n_header}, "
          f"{n_paths} hardcoded result strings rewritten")
    for r in py_results:
        if r["header"] or r["paths"]:
            print(f"    {r['path']:60s} header={r['header']} paths={r['paths']}")

    print("\n[2/3] Patching shell runners...")
    sh_results = []
    for sh in sorted((UN / "runners").glob("*.sh")):
        sh_results.append(process_shell_script(sh))
    print(f"  patched {sum(r['shell'] for r in sh_results)} of {len(sh_results)} shell files")
    for r in sh_results:
        print(f"    {r['path']:60s} patched={r['shell']}")

    print("\n[3/3] Patching notebooks...")
    nb_results = []
    for nb in sorted((UN / "notebooks").glob("*.ipynb")):
        nb_results.append(process_notebook(nb))
    print(f"  patched cells in {sum(1 for r in nb_results if r['cells']>0)} notebooks "
          f"({sum(r['cells'] for r in nb_results)} cells total)")
    for r in nb_results:
        print(f"    {r['path']:60s} cells={r['cells']}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
