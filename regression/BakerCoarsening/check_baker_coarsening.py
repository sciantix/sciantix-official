#!/usr/bin/env python3
"""COARSENING: check BakerCoarsening model-4 cases against their suite gold swelling."""

from __future__ import annotations

import math
from pathlib import Path


def load_output(path: Path) -> tuple[list[str], list[float]]:
    lines = [line.strip() for line in path.read_text().splitlines() if line.strip()]
    header = [item.strip() for item in lines[0].split("\t") if item.strip()]
    values = [float(item) for item in lines[-1].split("\t") if item.strip()]
    return header, values


def main() -> int:
    root = Path(__file__).resolve().parent
    column = "Intragranular gas bubble swelling (/)"

    errors: list[float] = []
    rows: list[tuple[str, float, float, float]] = []

    for case in sorted(root.glob("test_Baker*")):
        suite_gold = case / "output_gold.txt"
        if not suite_gold.exists():
            continue

        header, values = load_output(case / "output.txt")
        gold_header, gold_values = load_output(suite_gold)
        calculated = values[header.index(column)] * 100.0
        gold = gold_values[gold_header.index(column)] * 100.0
        error = calculated - gold
        errors.append(error)
        rows.append((case.name, gold, calculated, error))

    rmse = math.sqrt(sum(error * error for error in errors) / len(errors)) if errors else 0.0
    max_abs = max((abs(error) for error in errors), default=0.0)

    print("COARSENING Baker suite intragranular swelling gold check")
    print(f"Cases analyzed: {len(rows)}")
    print(f"RMSE vs BakerCoarsening gold (%): {rmse:.6g}")
    print(f"Max abs error vs BakerCoarsening gold (%): {max_abs:.6g}")
    for name, gold, calculated, error in rows:
        print(f"{name}: gold={gold:.8g} calc={calculated:.8g} error={error:.3e}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
