#!/usr/bin/env python3
"""Compare SCIANTIX output.txt against output_new.txt by column name."""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np


def read_tabular_output(path: Path) -> tuple[list[str], np.ndarray]:
    raw = np.genfromtxt(
        path,
        delimiter="\t",
        dtype=str,
        filling_values="nan",
        autostrip=True,
    )

    if raw.ndim == 1:
        raw = np.array([raw])

    rows = [row for row in raw if any(cell.strip() for cell in row)]
    if not rows:
        raise ValueError(f"{path} is empty")

    header = [cell.strip() for cell in rows[0] if cell.strip()]
    data_rows = []
    for row in rows[1:]:
        values = []
        for cell in row[: len(header)]:
            try:
                values.append(float(cell))
            except ValueError:
                values.append(np.nan)
        data_rows.append(values)

    return header, np.asarray(data_rows, dtype=float)


def relative_difference(diff: np.ndarray, reference: np.ndarray, abs_tol: float) -> np.ndarray:
    return diff / np.maximum(abs_tol, np.abs(reference))


def compare_files(
    old_path: Path,
    new_path: Path,
    abs_tol: float,
    rel_tol: float,
    top: int,
) -> bool:
    old_header, old_data = read_tabular_output(old_path)
    new_header, new_data = read_tabular_output(new_path)

    old_cols = {name: i for i, name in enumerate(old_header)}
    new_cols = {name: i for i, name in enumerate(new_header)}

    removed = [name for name in old_header if name not in new_cols]
    added = [name for name in new_header if name not in old_cols]
    common = [name for name in old_header if name in new_cols]

    print(f"\nComparing {old_path.name} -> {new_path.name}")
    print(f"Rows: {old_data.shape[0]} -> {new_data.shape[0]}")
    print(f"Columns: {len(old_header)} -> {len(new_header)}")

    if removed:
        print("\nColumns only in old output:")
        for name in removed:
            print(f"  - {name}")

    if added:
        print("\nColumns only in new output:")
        for name in added:
            print(f"  + {name}")

    if old_data.shape[0] != new_data.shape[0]:
        print("\nCannot compare common columns: row counts differ.")
        return False

    differences = []
    failing = []
    for name in common:
        old_values = old_data[:, old_cols[name]]
        new_values = new_data[:, new_cols[name]]
        diff = np.abs(new_values - old_values)
        rel = relative_difference(diff, old_values, abs_tol)
        finite = np.isfinite(diff) & np.isfinite(rel)

        if not np.any(finite):
            max_abs = np.nan
            max_rel = np.nan
            row = -1
        else:
            scored = np.where(finite, diff, -np.inf)
            row = int(np.argmax(scored))
            max_abs = float(diff[row])
            max_rel = float(rel[row])

        bad = (diff > abs_tol) & (rel > rel_tol)
        n_bad = int(np.count_nonzero(bad))
        if n_bad:
            failing.append(name)

        differences.append(
            {
                "name": name,
                "row": row,
                "max_abs": max_abs,
                "max_rel": max_rel,
                "n_bad": n_bad,
                "old": float(old_values[row]) if row >= 0 else np.nan,
                "new": float(new_values[row]) if row >= 0 else np.nan,
            }
        )

    differences.sort(
        key=lambda item: (
            item["n_bad"] > 0,
            np.nan_to_num(item["max_abs"], nan=-1.0),
        ),
        reverse=True,
    )

    print(f"\nCommon columns compared: {len(common)}")
    print(f"Columns outside tolerance: {len(failing)}")

    print(f"\nTop {min(top, len(differences))} column differences:")
    for item in differences[:top]:
        print(
            f"  {item['name']}: row={item['row']}, "
            f"old={item['old']:.8e}, new={item['new']:.8e}, "
            f"abs={item['max_abs']:.8e}, rel={item['max_rel']:.8e}, "
            f"bad_rows={item['n_bad']}"
        )

    return not removed and not added and not failing


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Compare output.txt and output_new.txt by matching column names."
    )
    parser.add_argument("old", nargs="?", default="thermochemistry_output.txt", type=Path)
    parser.add_argument("new", nargs="?", default="thermochemistry_output_new.txt", type=Path)
    parser.add_argument("--abs-tol", default=1e-8, type=float)
    parser.add_argument("--rel-tol", default=1e-6, type=float)
    parser.add_argument("--top", default=20, type=int)
    args = parser.parse_args()

    ok = compare_files(
        args.old,
        args.new,
        abs_tol=args.abs_tol,
        rel_tol=args.rel_tol,
        top=args.top,
    )
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
