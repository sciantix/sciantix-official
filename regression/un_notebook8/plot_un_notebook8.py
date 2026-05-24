#!/usr/bin/env python3
"""Plot standalone UN notebook-8 regression results."""

from __future__ import annotations

import argparse
import csv
import math
import re
import struct
import zlib
from collections import defaultdict
from pathlib import Path


SUITE_DIR = Path(__file__).resolve().parent
SUMMARY_CSV = SUITE_DIR / "results" / "un_notebook8_summary.csv"
REFERENCE_CSV = SUITE_DIR / "reference" / "python8_reference_points.csv"
HISTORY_CASE = SUITE_DIR / "cases" / "test_UN_history_T1600"
FIGURES_DIR = SUITE_DIR / "figures"


def slug(text: str) -> str:
    return re.sub(r"[^A-Za-z0-9]+", "_", text).strip("_").lower()


def write_png(path: Path, width: int, height: int, pixels: bytearray) -> None:
    def chunk(kind: bytes, data: bytes) -> bytes:
        payload = kind + data
        return struct.pack(">I", len(data)) + payload + struct.pack(">I", zlib.crc32(payload) & 0xFFFFFFFF)

    scanlines = bytearray()
    stride = width * 3
    for y in range(height):
        scanlines.append(0)
        scanlines.extend(pixels[y * stride : (y + 1) * stride])
    payload = zlib.compress(bytes(scanlines), 9)
    png = b"\x89PNG\r\n\x1a\n"
    png += chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
    png += chunk(b"IDAT", payload)
    png += chunk(b"IEND", b"")
    path.write_bytes(png)


def set_pixel(pixels: bytearray, width: int, height: int, x: int, y: int, color: tuple[int, int, int]) -> None:
    if 0 <= x < width and 0 <= y < height:
        index = (y * width + x) * 3
        pixels[index : index + 3] = bytes(color)


def draw_line(
    pixels: bytearray,
    width: int,
    height: int,
    x0: int,
    y0: int,
    x1: int,
    y1: int,
    color: tuple[int, int, int],
) -> None:
    dx = abs(x1 - x0)
    dy = -abs(y1 - y0)
    sx = 1 if x0 < x1 else -1
    sy = 1 if y0 < y1 else -1
    err = dx + dy
    while True:
        set_pixel(pixels, width, height, x0, y0, color)
        if x0 == x1 and y0 == y1:
            break
        e2 = 2 * err
        if e2 >= dy:
            err += dy
            x0 += sx
        if e2 <= dx:
            err += dx
            y0 += sy


def draw_point(
    pixels: bytearray,
    width: int,
    height: int,
    x: int,
    y: int,
    color: tuple[int, int, int],
    radius: int = 3,
) -> None:
    for yy in range(y - radius, y + radius + 1):
        for xx in range(x - radius, x + radius + 1):
            if (xx - x) * (xx - x) + (yy - y) * (yy - y) <= radius * radius:
                set_pixel(pixels, width, height, xx, yy, color)


def scaled(values: list[float]) -> tuple[list[float], str]:
    finite = [value for value in values if math.isfinite(value)]
    if finite and min(finite) > 0 and max(finite) / min(finite) > 1.0e3:
        return [math.log10(value) if value > 0 else math.nan for value in values], "log10"
    return values, "linear"


def make_canvas(width: int = 720, height: int = 520) -> tuple[bytearray, int, int, tuple[int, int, int, int]]:
    pixels = bytearray([255] * width * height * 3)
    box = (70, 35, width - 35, height - 65)
    left, top, right, bottom = box
    draw_line(pixels, width, height, left, bottom, right, bottom, (30, 30, 30))
    draw_line(pixels, width, height, left, bottom, left, top, (30, 30, 30))
    return pixels, width, height, box


def project(value: float, low: float, high: float, start: int, end: int, flip: bool = False) -> int:
    if high == low:
        fraction = 0.5
    else:
        fraction = (value - low) / (high - low)
    if flip:
        fraction = 1.0 - fraction
    return int(round(start + fraction * (end - start)))


def fallback_plot_xy(path: Path, xs: list[float], ys: list[float], diagonal: bool = False) -> None:
    x_scaled, _ = scaled(xs)
    y_scaled, _ = scaled(ys)
    pairs = [(x, y) for x, y in zip(x_scaled, y_scaled) if math.isfinite(x) and math.isfinite(y)]
    if not pairs:
        return
    x_values = [pair[0] for pair in pairs]
    y_values = [pair[1] for pair in pairs]
    x_low, x_high = min(x_values), max(x_values)
    y_low, y_high = min(y_values), max(y_values)
    if diagonal:
        x_low = y_low = min(x_low, y_low)
        x_high = y_high = max(x_high, y_high)
    pad_x = (x_high - x_low) * 0.05 if x_high != x_low else 1.0
    pad_y = (y_high - y_low) * 0.05 if y_high != y_low else 1.0
    x_low -= pad_x
    x_high += pad_x
    y_low -= pad_y
    y_high += pad_y
    pixels, width, height, box = make_canvas()
    left, top, right, bottom = box
    if diagonal:
        draw_line(pixels, width, height, left, bottom, right, top, (115, 115, 115))
    last = None
    for x, y in pairs:
        px = project(x, x_low, x_high, left, right)
        py = project(y, y_low, y_high, top, bottom, flip=True)
        draw_point(pixels, width, height, px, py, (34, 97, 165))
        if last and not diagonal:
            draw_line(pixels, width, height, last[0], last[1], px, py, (34, 97, 165))
        last = (px, py)
    write_png(path, width, height, pixels)


def read_summary(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def read_output(path: Path) -> tuple[list[str], list[dict[str, float]]]:
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle, delimiter="\t")
        rows = []
        for row in reader:
            parsed = {}
            for key, value in row.items():
                try:
                    parsed[key] = float(value)
                except (TypeError, ValueError):
                    parsed[key] = math.nan
            rows.append(parsed)
        return list(reader.fieldnames or []), rows


def read_reference(path: Path) -> list[dict[str, float | str]]:
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        rows = []
        for row in reader:
            parsed: dict[str, float | str] = {"case": row["case"]}
            for key, value in row.items():
                if key == "case":
                    continue
                try:
                    parsed[key] = float(value)
                except (TypeError, ValueError):
                    parsed[key] = math.nan
            rows.append(parsed)
        return rows


def plot_parity(summary_rows: list[dict[str, str]]) -> list[Path]:
    grouped: dict[str, list[tuple[float, float, str]]] = defaultdict(list)
    for row in summary_rows:
        if row["status"] not in {"PASS", "FAIL"}:
            continue
        try:
            value = float(row["sciantix"])
            reference = float(row["reference"])
        except ValueError:
            continue
        if math.isfinite(value) and math.isfinite(reference):
            grouped[row["quantity"]].append((reference, value, row["case"]))

    paths = []
    FIGURES_DIR.mkdir(parents=True, exist_ok=True)
    try:
        import matplotlib.pyplot as plt
    except ModuleNotFoundError:
        for quantity, data in grouped.items():
            refs = [item[0] for item in data]
            vals = [item[1] for item in data]
            path = FIGURES_DIR / f"parity_{slug(quantity)}.png"
            fallback_plot_xy(path, refs, vals, diagonal=True)
            paths.append(path)
        return paths

    for quantity, data in grouped.items():
        if not data:
            continue
        refs = [item[0] for item in data]
        vals = [item[1] for item in data]
        low = min(refs + vals)
        high = max(refs + vals)
        if low == high:
            low *= 0.9
            high *= 1.1 if high else 1.0
        fig, ax = plt.subplots(figsize=(5.5, 4.5), constrained_layout=True)
        ax.scatter(refs, vals, s=36)
        ax.plot([low, high], [low, high], color="0.35", linewidth=1)
        ax.set_xlabel("Notebook-8 reference")
        ax.set_ylabel("SCIANTIX")
        ax.set_title(quantity)
        ax.grid(True, alpha=0.25)
        path = FIGURES_DIR / f"parity_{slug(quantity)}.png"
        fig.savefig(path, dpi=180)
        plt.close(fig)
        paths.append(path)
    return paths


def plot_history(reference_rows: list[dict[str, float | str]]) -> list[Path]:
    output_path = HISTORY_CASE / "output.txt"
    if not output_path.is_file():
        print(f"WARNING: missing history output: {output_path}")
        return []
    headers, rows = read_output(output_path)
    if "FIMA (%)" not in headers:
        print("WARNING: history output lacks FIMA (%); history plots skipped")
        return []
    quantities = [
        "Intragranular gas bubble swelling (/)",
        "UN fission gas release (/)",
        "Dislocation gas bubble swelling (/)",
        "UN bulk nucleation rate (bub/m3/s)",
    ]
    t1600_refs = [row for row in reference_rows if float(row.get("T_K", math.nan)) == 1600.0]
    paths = []
    try:
        import matplotlib.pyplot as plt
    except ModuleNotFoundError:
        for quantity in quantities:
            if quantity not in headers:
                print(f"WARNING: history output lacks {quantity}; plot skipped")
                continue
            xs = [row["FIMA (%)"] for row in rows]
            ys = [row[quantity] for row in rows]
            path = FIGURES_DIR / f"history_T1600_{slug(quantity)}.png"
            fallback_plot_xy(path, xs, ys, diagonal=False)
            paths.append(path)
        return paths

    for quantity in quantities:
        if quantity not in headers:
            print(f"WARNING: history output lacks {quantity}; plot skipped")
            continue
        xs = [row["FIMA (%)"] for row in rows]
        ys = [row[quantity] for row in rows]
        fig, ax = plt.subplots(figsize=(6.0, 4.2), constrained_layout=True)
        ax.plot(xs, ys, linewidth=1.6, label="SCIANTIX history")
        ref_x = []
        ref_y = []
        for ref in t1600_refs:
            if quantity in ref and math.isfinite(float(ref[quantity])):
                ref_x.append(float(ref["FIMA (%)"]))
                ref_y.append(float(ref[quantity]))
        if ref_x:
            ax.scatter(ref_x, ref_y, s=34, label="Notebook-8 points")
        ax.set_xlabel("FIMA (%)")
        ax.set_ylabel(quantity)
        ax.set_title(f"T = 1600 K: {quantity}")
        ax.grid(True, alpha=0.25)
        ax.legend()
        path = FIGURES_DIR / f"history_T1600_{slug(quantity)}.png"
        fig.savefig(path, dpi=180)
        plt.close(fig)
        paths.append(path)
    return paths


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--summary", default=str(SUMMARY_CSV))
    args = parser.parse_args(argv)

    summary_path = Path(args.summary)
    if not summary_path.is_file():
        raise FileNotFoundError(f"summary not found: {summary_path}")

    summary_rows = read_summary(summary_path)
    reference_rows = read_reference(REFERENCE_CSV)
    paths = []
    paths.extend(plot_parity(summary_rows))
    paths.extend(plot_history(reference_rows))
    print(f"Wrote {len(paths)} figures in {FIGURES_DIR}")
    print("Storms FGR optional reference not plotted: no Storms reference CSV configured.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
