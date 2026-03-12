#!/usr/bin/env python3
from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Iterable


BASE_DIR = Path(__file__).resolve().parent
CASES = (
    ("output.txt", "output_gold.txt", "output_diff_report.csv"),
    (
        "thermochemistry_output.txt",
        "thermochemistry_output_gold.txt",
        "thermochemistry_diff_report.csv",
    ),
)


def read_table(path: Path) -> tuple[list[str], list[list[str]]]:
    lines = path.read_text().splitlines()
    if not lines:
        raise ValueError(f"Empty file: {path}")

    header = lines[0].split("\t")
    if header and header[-1] == "":
        header = header[:-1]

    rows: list[list[str]] = []
    for line in lines[1:]:
        parts = line.split("\t")
        if parts and parts[-1] == "":
            parts = parts[:-1]
        if parts:
            rows.append(parts)

    return header, rows


def numeric_diff_report(
    current_rows: list[list[str]],
    gold_rows: list[list[str]],
    current_header: list[str],
    gold_header: list[str],
    common_columns: list[str],
) -> list[dict[str, object]]:
    diffs: list[dict[str, object]] = []

    current_index = {name: index for index, name in enumerate(current_header)}
    gold_index = {name: index for index, name in enumerate(gold_header)}

    for name in common_columns:
        current_col = current_index[name]
        gold_col = gold_index[name]
        differing_rows = 0
        first_row = None
        first_current = None
        first_gold = None
        max_abs_diff = 0.0
        max_rel_diff = 0.0

        for row_index, (current_row, gold_row) in enumerate(
            zip(current_rows, gold_rows), start=1
        ):
            current_value = float(current_row[current_col])
            gold_value = float(gold_row[gold_col])

            same = False
            if math.isnan(current_value) and math.isnan(gold_value):
                same = True
            elif current_value == gold_value:
                same = True

            if same:
                continue

            abs_diff = abs(current_value - gold_value)
            if abs_diff == 0.0:
                continue

            rel_diff = abs_diff / max(abs(gold_value), 1e-300)
            differing_rows += 1

            if first_row is None:
                first_row = row_index
                first_current = current_row[current_col]
                first_gold = gold_row[gold_col]

            max_abs_diff = max(max_abs_diff, abs_diff)
            max_rel_diff = max(max_rel_diff, rel_diff)

        if differing_rows:
            diffs.append(
                {
                    "column_index": current_col,
                    "column_name": name,
                    "differing_rows": differing_rows,
                    "first_differing_data_row": first_row,
                    "current_at_first_diff": first_current,
                    "gold_at_first_diff": first_gold,
                    "max_abs_diff": f"{max_abs_diff:.16g}",
                    "max_rel_diff": f"{max_rel_diff:.16g}",
                }
            )

    return diffs


def write_csv(path: Path, rows: Iterable[dict[str, object]]) -> None:
    fieldnames = [
        "column_index",
        "column_name",
        "differing_rows",
        "first_differing_data_row",
        "current_at_first_diff",
        "gold_at_first_diff",
        "max_abs_diff",
        "max_rel_diff",
    ]
    with path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def compare_case(current_name: str, gold_name: str, report_name: str) -> bool:
    current_path = BASE_DIR / current_name
    gold_path = BASE_DIR / gold_name
    report_path = BASE_DIR / report_name

    current_header, current_rows = read_table(current_path)
    gold_header, gold_rows = read_table(gold_path)

    ok = True
    current_set = set(current_header)
    gold_set = set(gold_header)
    common_columns = [name for name in current_header if name in gold_set]

    print(f"\n== {current_name} ==")
    print(f"current rows: {len(current_rows)}")
    print(f"gold rows:    {len(gold_rows)}")

    if current_header != gold_header:
        ok = False
        current_only = [name for name in current_header if name not in gold_set]
        gold_only = [name for name in gold_header if name not in current_set]
        print("header: MISMATCH")
        if current_only:
            print("only in current:")
            for name in current_only:
                print(f"  + {name}")
        if gold_only:
            print("only in gold:")
            for name in gold_only:
                print(f"  - {name}")
    else:
        print(f"columns:      {len(current_header)}")
        print("header:       OK")

    print(f"common cols:  {len(common_columns)}")

    if len(current_rows) != len(gold_rows):
        ok = False
        print("numeric:      SKIPPED (row count mismatch)")
        return ok

    diffs = numeric_diff_report(
        current_rows,
        gold_rows,
        current_header,
        gold_header,
        common_columns,
    )
    write_csv(report_path, diffs)

    if not diffs:
        print("numeric:      OK")
        print(f"report:       {report_name} (header only, no diffs)")
        return ok

    ok = False
    print(f"numeric:      MISMATCH ({len(diffs)} differing columns)")
    print(f"report:       {report_name}")
    for diff in diffs[:10]:
        print(
            f"  [{diff['column_index']}] {diff['column_name']}: "
            f"{diff['differing_rows']} differing rows, "
            f"first row {diff['first_differing_data_row']} "
            f"(current={diff['current_at_first_diff']}, "
            f"gold={diff['gold_at_first_diff']})"
        )
    if len(diffs) > 10:
        print(f"  ... {len(diffs) - 10} more differing columns")

    return ok


def main() -> int:
    all_ok = True
    for current_name, gold_name, report_name in CASES:
        all_ok = compare_case(current_name, gold_name, report_name) and all_ok

    print("\nSummary:")
    print("PASS" if all_ok else "FAIL")
    return 0 if all_ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
