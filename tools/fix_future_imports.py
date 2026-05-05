"""Fixup pass: move my path-setup block to after any `from __future__` imports.

Python requires `from __future__ import ...` to be at the top of the file
(only docstring / shebang / encoding may precede it). My fix_paths.py header
got inserted before existing future imports in some files, breaking them.

This pass detects that case and swaps the order so the future imports come first.
Idempotent: a file that's already correct passes through unchanged.
"""

from __future__ import annotations

import re
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
UN = REPO / "un_calibration"

HEADER_START = "# --- path setup (added by tools/fix_paths.py after un_calibration/ reorg) ---"
HEADER_END = "# --- end path setup ---"

FUTURE_RE = re.compile(r"^\s*from\s+__future__\s+import\b.*$")


def fix_file(path: Path) -> bool:
    src = path.read_text()
    if HEADER_START not in src:
        return False
    lines = src.splitlines(keepends=True)

    # Locate path-setup block
    h_start = next((i for i, l in enumerate(lines) if HEADER_START in l), None)
    h_end = next((i for i, l in enumerate(lines) if HEADER_END in l), None)
    if h_start is None or h_end is None or h_end <= h_start:
        return False
    header_block = lines[h_start : h_end + 1]

    # Find all future-import lines AFTER the header (they're misplaced)
    future_idx = [i for i, l in enumerate(lines) if FUTURE_RE.match(l) and i > h_end]
    if not future_idx:
        return False  # already correct

    # Strategy: lift the future imports out, place them BEFORE the header,
    # leave everything else as is (including the header and trailing code).
    future_lines = [lines[i] for i in future_idx]
    # Indices to drop (the future import lines)
    drop = set(future_idx)
    rebuilt = [l for i, l in enumerate(lines) if i not in drop]

    # Rebuild: re-find header_start in rebuilt list
    h_start = next(i for i, l in enumerate(rebuilt) if HEADER_START in l)

    # Insert future imports immediately before header, separated by a blank line
    insert = list(future_lines)
    if not insert[-1].endswith("\n"):
        insert[-1] = insert[-1] + "\n"
    insert.append("\n")
    new_lines = rebuilt[:h_start] + insert + rebuilt[h_start:]
    new_src = "".join(new_lines)
    if new_src != src:
        path.write_text(new_src)
        return True
    return False


def main() -> int:
    fixed = 0
    scanned = 0
    for py in sorted(UN.rglob("*.py")):
        if "/results/" in str(py):
            continue
        if py.name == "_pathsetup.py":
            continue
        scanned += 1
        if fix_file(py):
            fixed += 1
            print(f"  fixed: {py.relative_to(REPO)}")
    print(f"\n{fixed}/{scanned} files reordered (future imports moved before path setup)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
