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
REL_TOL = 0.35


FONT_5X7 = {
    "A": ("01110", "10001", "10001", "11111", "10001", "10001", "10001"),
    "B": ("11110", "10001", "10001", "11110", "10001", "10001", "11110"),
    "C": ("01111", "10000", "10000", "10000", "10000", "10000", "01111"),
    "D": ("11110", "10001", "10001", "10001", "10001", "10001", "11110"),
    "E": ("11111", "10000", "10000", "11110", "10000", "10000", "11111"),
    "F": ("11111", "10000", "10000", "11110", "10000", "10000", "10000"),
    "G": ("01111", "10000", "10000", "10011", "10001", "10001", "01111"),
    "H": ("10001", "10001", "10001", "11111", "10001", "10001", "10001"),
    "I": ("11111", "00100", "00100", "00100", "00100", "00100", "11111"),
    "J": ("00111", "00010", "00010", "00010", "10010", "10010", "01100"),
    "K": ("10001", "10010", "10100", "11000", "10100", "10010", "10001"),
    "L": ("10000", "10000", "10000", "10000", "10000", "10000", "11111"),
    "M": ("10001", "11011", "10101", "10101", "10001", "10001", "10001"),
    "N": ("10001", "11001", "10101", "10011", "10001", "10001", "10001"),
    "O": ("01110", "10001", "10001", "10001", "10001", "10001", "01110"),
    "P": ("11110", "10001", "10001", "11110", "10000", "10000", "10000"),
    "Q": ("01110", "10001", "10001", "10001", "10101", "10010", "01101"),
    "R": ("11110", "10001", "10001", "11110", "10100", "10010", "10001"),
    "S": ("01111", "10000", "10000", "01110", "00001", "00001", "11110"),
    "T": ("11111", "00100", "00100", "00100", "00100", "00100", "00100"),
    "U": ("10001", "10001", "10001", "10001", "10001", "10001", "01110"),
    "V": ("10001", "10001", "10001", "10001", "10001", "01010", "00100"),
    "W": ("10001", "10001", "10001", "10101", "10101", "10101", "01010"),
    "X": ("10001", "10001", "01010", "00100", "01010", "10001", "10001"),
    "Y": ("10001", "10001", "01010", "00100", "00100", "00100", "00100"),
    "Z": ("11111", "00001", "00010", "00100", "01000", "10000", "11111"),
    "0": ("01110", "10001", "10011", "10101", "11001", "10001", "01110"),
    "1": ("00100", "01100", "00100", "00100", "00100", "00100", "01110"),
    "2": ("01110", "10001", "00001", "00010", "00100", "01000", "11111"),
    "3": ("11110", "00001", "00001", "01110", "00001", "00001", "11110"),
    "4": ("00010", "00110", "01010", "10010", "11111", "00010", "00010"),
    "5": ("11111", "10000", "10000", "11110", "00001", "00001", "11110"),
    "6": ("01110", "10000", "10000", "11110", "10001", "10001", "01110"),
    "7": ("11111", "00001", "00010", "00100", "01000", "01000", "01000"),
    "8": ("01110", "10001", "10001", "01110", "10001", "10001", "01110"),
    "9": ("01110", "10001", "10001", "01111", "00001", "00001", "01110"),
    "-": ("00000", "00000", "00000", "11111", "00000", "00000", "00000"),
    "+": ("00000", "00100", "00100", "11111", "00100", "00100", "00000"),
    "=": ("00000", "00000", "11111", "00000", "11111", "00000", "00000"),
    ".": ("00000", "00000", "00000", "00000", "00000", "01100", "01100"),
    ",": ("00000", "00000", "00000", "00000", "00000", "01100", "01000"),
    ":": ("00000", "01100", "01100", "00000", "01100", "01100", "00000"),
    "/": ("00001", "00010", "00010", "00100", "01000", "01000", "10000"),
    "%": ("11001", "11010", "00010", "00100", "01000", "01011", "10011"),
    "(": ("00010", "00100", "01000", "01000", "01000", "00100", "00010"),
    ")": ("01000", "00100", "00010", "00010", "00010", "00100", "01000"),
    "'": ("00100", "00100", "01000", "00000", "00000", "00000", "00000"),
    " ": ("00000", "00000", "00000", "00000", "00000", "00000", "00000"),
}


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


def draw_rect(
    pixels: bytearray,
    width: int,
    height: int,
    x0: int,
    y0: int,
    x1: int,
    y1: int,
    color: tuple[int, int, int],
) -> None:
    for y in range(max(0, y0), min(height, y1 + 1)):
        for x in range(max(0, x0), min(width, x1 + 1)):
            set_pixel(pixels, width, height, x, y, color)


def draw_text(
    pixels: bytearray,
    width: int,
    height: int,
    x: int,
    y: int,
    text: str,
    color: tuple[int, int, int] = (30, 30, 30),
    scale: int = 1,
) -> None:
    cursor = x
    for raw_char in text.upper().replace("³", "3").replace("²", "2").replace("–", "-"):
        pattern = FONT_5X7.get(raw_char, FONT_5X7[" "])
        for row_index, row in enumerate(pattern):
            for col_index, bit in enumerate(row):
                if bit == "1":
                    draw_rect(
                        pixels,
                        width,
                        height,
                        cursor + col_index * scale,
                        y + row_index * scale,
                        cursor + (col_index + 1) * scale - 1,
                        y + (row_index + 1) * scale - 1,
                        color,
                    )
        cursor += 6 * scale


def draw_polyline(
    pixels: bytearray,
    width: int,
    height: int,
    points: list[tuple[int, int]],
    color: tuple[int, int, int],
) -> None:
    for first, second in zip(points, points[1:]):
        draw_line(pixels, width, height, first[0], first[1], second[0], second[1], color)


def scaled(values: list[float]) -> tuple[list[float], str]:
    finite = [value for value in values if math.isfinite(value)]
    if finite and min(finite) > 0 and max(finite) / min(finite) > 1.0e3:
        return [math.log10(value) if value > 0 else math.nan for value in values], "log10"
    return values, "linear"


def make_canvas(width: int = 980, height: int = 680) -> tuple[bytearray, int, int, tuple[int, int, int, int]]:
    pixels = bytearray([255] * width * height * 3)
    box = (120, 80, width - 55, height - 110)
    left, top, right, bottom = box
    for i in range(1, 5):
        x = left + i * (right - left) // 5
        y = top + i * (bottom - top) // 5
        draw_line(pixels, width, height, x, top, x, bottom, (230, 230, 230))
        draw_line(pixels, width, height, left, y, right, y, (230, 230, 230))
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


def fallback_plot_xy(
    path: Path,
    xs: list[float],
    ys: list[float],
    title: str,
    xlabel: str,
    ylabel: str,
    *,
    diagonal: bool = False,
    tolerance: bool = False,
    series_label: str = "SCIANTIX",
    reference_label: str = "Python notebook-8 reference",
    statuses: list[str] | None = None,
    log_scale: bool = False,
    connect: bool = False,
    annotation: str = "",
) -> None:
    if log_scale:
        pairs = [
            (math.log10(x), math.log10(y), status)
            for x, y, status in zip(xs, ys, statuses or ["PASS"] * len(xs))
            if x > 0.0 and y > 0.0 and math.isfinite(x) and math.isfinite(y)
        ]
    else:
        pairs = [
            (x, y, status)
            for x, y, status in zip(xs, ys, statuses or ["PASS"] * len(xs))
            if math.isfinite(x) and math.isfinite(y)
        ]
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
        if tolerance:
            offset = math.log10(1.0 + REL_TOL) if log_scale else 0.0
            if log_scale:
                upper_points = []
                lower_points = []
                for x in (x_low, x_high):
                    upper_points.append(
                        (project(x, x_low, x_high, left, right), project(x + offset, y_low, y_high, top, bottom, True))
                    )
                    lower_points.append(
                        (project(x, x_low, x_high, left, right), project(x + math.log10(1.0 - REL_TOL), y_low, y_high, top, bottom, True))
                    )
                draw_polyline(pixels, width, height, upper_points, (180, 180, 180))
                draw_polyline(pixels, width, height, lower_points, (180, 180, 180))
    last = None
    for x, y, status in pairs:
        px = project(x, x_low, x_high, left, right)
        py = project(y, y_low, y_high, top, bottom, flip=True)
        color = (34, 139, 84) if status == "PASS" else (190, 54, 45)
        draw_point(pixels, width, height, px, py, color)
        if last and connect:
            draw_line(pixels, width, height, last[0], last[1], px, py, (34, 97, 165))
        last = (px, py)
    draw_text(pixels, width, height, 22, 18, title[:90], scale=2)
    draw_text(pixels, width, height, (left + right) // 2 - min(len(xlabel) * 3, 280), height - 42, xlabel[:95])
    draw_text(pixels, width, height, 12, top - 25, ylabel[:95])
    if log_scale:
        draw_text(pixels, width, height, right - 210, top + 12, "LOG-LOG SCALE")
    draw_point(pixels, width, height, right - 250, top + 40, (34, 139, 84), radius=4)
    draw_text(pixels, width, height, right - 235, top + 36, series_label[:32])
    if reference_label:
        draw_line(pixels, width, height, right - 250, top + 62, right - 220, top + 62, (115, 115, 115))
        draw_text(pixels, width, height, right - 215, top + 56, reference_label[:32])
    if tolerance:
        draw_line(pixels, width, height, right - 250, top + 84, right - 220, top + 84, (180, 180, 180))
        draw_text(pixels, width, height, right - 215, top + 78, "+/-35% TOLERANCE")
    if annotation:
        draw_text(pixels, width, height, left, bottom + 20, annotation[:100])
    write_png(path, width, height, pixels)


def fallback_bar_chart(
    path: Path,
    labels: list[str],
    values: list[float],
    title: str,
    xlabel: str,
    threshold: float | None = None,
) -> None:
    pixels, width, height, box = make_canvas(1100, 760)
    left, top, right, bottom = box
    finite_values = [value for value in values if math.isfinite(value)]
    high = max(finite_values + ([threshold] if threshold is not None else [0.0]) + [1.0e-12])
    row_height = max(18, (bottom - top) // max(len(labels), 1))
    for index, (label, value) in enumerate(zip(labels, values)):
        y = top + index * row_height + row_height // 2
        bar_right = project(value, 0.0, high * 1.05, left, right)
        draw_rect(pixels, width, height, left, y - 5, bar_right, y + 5, (34, 97, 165))
        draw_text(pixels, width, height, 10, y - 8, label[:24])
        draw_text(pixels, width, height, bar_right + 6, y - 8, f"{value:.2G}")
    if threshold is not None:
        x = project(threshold, 0.0, high * 1.05, left, right)
        draw_line(pixels, width, height, x, top, x, bottom, (190, 54, 45))
        draw_text(pixels, width, height, x + 5, top - 22, f"REL_TOL={threshold:.2G}")
    draw_text(pixels, width, height, 22, 18, title[:90], scale=2)
    draw_text(pixels, width, height, (left + right) // 2 - min(len(xlabel) * 3, 280), height - 42, xlabel[:95])
    write_png(path, width, height, pixels)


def fallback_history_plot(
    path: Path,
    xs: list[float],
    ys: list[float],
    ref_xs: list[float],
    ref_ys: list[float],
    title: str,
    ylabel: str,
) -> None:
    pairs = [(x, y) for x, y in zip(xs, ys) if math.isfinite(x) and math.isfinite(y)]
    ref_pairs = [(x, y) for x, y in zip(ref_xs, ref_ys) if math.isfinite(x) and math.isfinite(y)]
    all_pairs = pairs + ref_pairs
    if not all_pairs:
        return
    x_values = [pair[0] for pair in all_pairs]
    y_values = [pair[1] for pair in all_pairs]
    x_low, x_high = min(x_values), max(x_values)
    y_low, y_high = min(y_values), max(y_values)
    pad_x = (x_high - x_low) * 0.05 if x_high != x_low else 1.0
    pad_y = (y_high - y_low) * 0.05 if y_high != y_low else 1.0
    x_low -= pad_x
    x_high += pad_x
    y_low -= pad_y
    y_high += pad_y

    pixels, width, height, box = make_canvas()
    left, top, right, bottom = box
    projected = [
        (project(x, x_low, x_high, left, right), project(y, y_low, y_high, top, bottom, flip=True))
        for x, y in pairs
    ]
    draw_polyline(pixels, width, height, projected, (34, 97, 165))
    for point in projected:
        draw_point(pixels, width, height, point[0], point[1], (34, 97, 165), radius=2)
    for x, y in ref_pairs:
        px = project(x, x_low, x_high, left, right)
        py = project(y, y_low, y_high, top, bottom, flip=True)
        draw_point(pixels, width, height, px, py, (190, 54, 45), radius=4)

    draw_text(pixels, width, height, 22, 18, title[:90], scale=2)
    draw_text(pixels, width, height, (left + right) // 2 - 35, height - 42, "FIMA (%)")
    draw_text(pixels, width, height, 12, top - 25, ylabel[:95])
    draw_line(pixels, width, height, right - 250, top + 42, right - 220, top + 42, (34, 97, 165))
    draw_text(pixels, width, height, right - 215, top + 36, "SCIANTIX")
    draw_point(pixels, width, height, right - 235, top + 64, (190, 54, 45), radius=4)
    draw_text(pixels, width, height, right - 215, top + 58, "PYTHON NOTEBOOK-8 REFERENCE")
    write_png(path, width, height, pixels)


def fallback_summary(path: Path, compared: int, passes: int, failures: int, missing: int) -> None:
    pixels = bytearray([255] * 980 * 520 * 3)
    width, height = 980, 520
    draw_text(pixels, width, height, 60, 40, "UN NOTEBOOK-8 REGRESSION SUMMARY", scale=2)
    lines = [
        f"COMPARED VALUES: {compared}",
        f"PASS: {passes}",
        f"FAIL: {failures}",
        f"MISSING: {missing}",
        f"OVERALL STATUS: {'PASS' if failures == 0 and missing == 0 else 'FAIL'}",
    ]
    colors = [(30, 30, 30), (34, 139, 84), (190, 54, 45), (190, 54, 45), (34, 139, 84)]
    for index, line in enumerate(lines):
        draw_text(pixels, width, height, 120, 135 + index * 65, line, colors[index], scale=2)
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
    all_rows: list[tuple[float, float, str]] = []
    compared = 0
    failures = 0
    missing = 0
    for row in summary_rows:
        if row["status"].startswith("MISSING"):
            missing += 1
        if row["status"] not in {"PASS", "FAIL"}:
            continue
        compared += 1
        if row["status"] == "FAIL":
            failures += 1
        try:
            value = float(row["sciantix"])
            reference = float(row["reference"])
        except ValueError:
            continue
        if math.isfinite(value) and math.isfinite(reference):
            grouped[row["quantity"]].append((reference, value, row["case"]))
            all_rows.append((reference, value, row["status"]))

    paths = []
    FIGURES_DIR.mkdir(parents=True, exist_ok=True)
    passes = compared - failures
    positive = [(ref, val, status) for ref, val, status in all_rows if ref > 0.0 and val > 0.0]
    excluded = len(all_rows) - len(positive)
    title = f"UN notebook-8 regression parity - {passes} PASS / {failures} FAIL / {missing} missing"
    try:
        import matplotlib.pyplot as plt
    except ModuleNotFoundError:
        if positive:
            refs = [item[0] for item in positive]
            vals = [item[1] for item in positive]
            statuses = [item[2] for item in positive]
            annotation = "zero-valued quantities excluded from log parity" if excluded else ""
            for name in ("parity_all_quantities.png", "parity_positive_loglog.png"):
                path = FIGURES_DIR / name
                fallback_plot_xy(
                    path,
                    refs,
                    vals,
                    title,
                    "Python notebook-8 reference",
                    "SCIANTIX",
                    diagonal=True,
                    tolerance=True,
                    series_label="Compared quantities",
                    reference_label="y = x",
                    statuses=statuses,
                    log_scale=True,
                    annotation=annotation,
                )
                paths.append(path)
        if excluded:
            zero_rows = [(ref, val, status) for ref, val, status in all_rows if ref <= 0.0 or val <= 0.0]
            path = FIGURES_DIR / "parity_zero_or_linear.png"
            fallback_plot_xy(
                path,
                [item[0] for item in zero_rows],
                [item[1] for item in zero_rows],
                "UN notebook-8 regression parity - zero/linear values",
                "Python notebook-8 reference",
                "SCIANTIX",
                diagonal=True,
                series_label="Compared quantities",
                reference_label="y = x",
                statuses=[item[2] for item in zero_rows],
            )
            paths.append(path)
        return paths

    if positive:
        refs = [item[0] for item in positive]
        vals = [item[1] for item in positive]
        statuses = [item[2] for item in positive]
        low = min(refs + vals)
        high = max(refs + vals)
        fig, ax = plt.subplots(figsize=(7.2, 5.8), constrained_layout=True)
        pass_refs = [ref for ref, _, status in positive if status == "PASS"]
        pass_vals = [val for _, val, status in positive if status == "PASS"]
        fail_refs = [ref for ref, _, status in positive if status == "FAIL"]
        fail_vals = [val for _, val, status in positive if status == "FAIL"]
        ax.scatter(pass_refs, pass_vals, s=38, label="Compared quantities", color="#238b53", alpha=0.82)
        if fail_refs:
            ax.scatter(fail_refs, fail_vals, s=48, label="FAIL", color="#be362d", marker="x")
        ax.plot([low, high], [low, high], color="0.2", linewidth=1.5, label="y = x")
        ax.plot([low, high], [(1.0 + REL_TOL) * low, (1.0 + REL_TOL) * high],
                color="0.55", linestyle="--", linewidth=1.1, label="+35% tolerance")
        ax.plot([low, high], [(1.0 - REL_TOL) * low, (1.0 - REL_TOL) * high],
                color="0.55", linestyle="--", linewidth=1.1, label="-35% tolerance")
        ax.set_xscale("log")
        ax.set_yscale("log")
        ax.set_xlabel("Python notebook-8 reference")
        ax.set_ylabel("SCIANTIX")
        ax.set_title(title)
        if excluded:
            ax.text(0.02, 0.02, "zero-valued quantities excluded from log parity",
                    transform=ax.transAxes, fontsize=9)
        ax.grid(True, which="both", alpha=0.25)
        ax.legend()
        for name in ("parity_all_quantities.png", "parity_positive_loglog.png"):
            path = FIGURES_DIR / name
            fig.savefig(path, dpi=220)
            paths.append(path)
        plt.close(fig)
    if excluded:
        zero_rows = [(ref, val, status) for ref, val, status in all_rows if ref <= 0.0 or val <= 0.0]
        fig, ax = plt.subplots(figsize=(7.0, 5.0), constrained_layout=True)
        ax.scatter([item[0] for item in zero_rows], [item[1] for item in zero_rows],
                   s=42, label="Compared quantities")
        low = min([item[0] for item in zero_rows] + [item[1] for item in zero_rows] + [0.0])
        high = max([item[0] for item in zero_rows] + [item[1] for item in zero_rows] + [1.0])
        ax.plot([low, high], [low, high], color="0.2", linewidth=1.4, label="y = x")
        ax.set_xlabel("Python notebook-8 reference")
        ax.set_ylabel("SCIANTIX")
        ax.set_title("UN notebook-8 regression parity - zero/linear values")
        ax.grid(True, alpha=0.25)
        ax.legend()
        path = FIGURES_DIR / "parity_zero_or_linear.png"
        fig.savefig(path, dpi=220)
        plt.close(fig)
        paths.append(path)
    return paths


def plot_relative_errors(summary_rows: list[dict[str, str]]) -> list[Path]:
    max_by_quantity: dict[str, float] = {}
    for row in summary_rows:
        if row["status"] not in {"PASS", "FAIL"}:
            continue
        try:
            error = float(row["rel_diff"])
        except ValueError:
            continue
        if math.isfinite(error):
            max_by_quantity[row["quantity"]] = max(error, max_by_quantity.get(row["quantity"], 0.0))
    items = sorted(max_by_quantity.items(), key=lambda item: item[1])
    path = FIGURES_DIR / "relative_error_by_quantity.png"
    try:
        import matplotlib.pyplot as plt
    except ModuleNotFoundError:
        fallback_bar_chart(
            path,
            [item[0] for item in items],
            [item[1] for item in items],
            "UN notebook-8 regression - maximum relative error by quantity",
            "Maximum relative error (-)",
            threshold=REL_TOL,
        )
        return [path]

    fig, ax = plt.subplots(figsize=(8.5, 5.8), constrained_layout=True)
    labels = [item[0] for item in items]
    values = [item[1] for item in items]
    ax.barh(labels, values, color="#2b6cb0")
    ax.axvline(REL_TOL, color="#be362d", linestyle="--", linewidth=1.4, label="rel_tol = 0.35")
    ax.set_xlabel("Maximum relative error (-)")
    ax.set_ylabel("Quantity")
    ax.set_title("UN notebook-8 regression - maximum relative error by quantity")
    ax.grid(True, axis="x", alpha=0.25)
    ax.legend()
    fig.savefig(path, dpi=220)
    plt.close(fig)
    return [path]


def plot_pass_fail_summary(summary_rows: list[dict[str, str]]) -> list[Path]:
    compared = sum(1 for row in summary_rows if row["status"] in {"PASS", "FAIL"})
    failures = sum(1 for row in summary_rows if row["status"] == "FAIL")
    missing = sum(1 for row in summary_rows if row["status"].startswith("MISSING"))
    passes = compared - failures
    path = FIGURES_DIR / "regression_pass_fail_summary.png"
    try:
        import matplotlib.pyplot as plt
    except ModuleNotFoundError:
        fallback_summary(path, compared, passes, failures, missing)
        return [path]

    fig, ax = plt.subplots(figsize=(6.2, 4.2), constrained_layout=True)
    ax.axis("off")
    status = "PASS" if failures == 0 and missing == 0 else "FAIL"
    lines = [
        f"Compared values: {compared}",
        f"PASS: {passes}",
        f"FAIL: {failures}",
        f"Missing: {missing}",
        f"Overall status: {status}",
    ]
    ax.text(0.05, 0.82, "UN notebook-8 regression summary", fontsize=16, weight="bold")
    for index, line in enumerate(lines):
        ax.text(0.08, 0.64 - index * 0.13, line, fontsize=14)
    fig.savefig(path, dpi=220)
    plt.close(fig)
    return [path]


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
        "Dislocation bubble radius (m)",
        "Dislocation bubble concentration (bub/m3)",
        "Dislocation bubble pressure (MPa)",
        "UN grain-face gas (at/m3)",
        "UN released gas (at/m3)",
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
            ref_x = []
            ref_y = []
            for ref in t1600_refs:
                if quantity in ref and math.isfinite(float(ref[quantity])):
                    ref_x.append(float(ref["FIMA (%)"]))
                    ref_y.append(float(ref[quantity]))
            fallback_history_plot(
                path,
                xs,
                ys,
                ref_x,
                ref_y,
                f"UN notebook-8 regression - T = 1600 K - {quantity}",
                quantity,
            )
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
            ax.scatter(ref_x, ref_y, s=34, label="Python notebook-8 reference")
        ax.set_xlabel("FIMA (%)")
        ax.set_ylabel(quantity)
        ax.set_title(f"UN notebook-8 regression - T = 1600 K - {quantity}")
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
    paths.extend(plot_relative_errors(summary_rows))
    paths.extend(plot_pass_fail_summary(summary_rows))
    paths.extend(plot_history(reference_rows))
    print(f"Wrote {len(paths)} figures in {FIGURES_DIR}")
    print("Storms FGR optional reference not plotted: no Storms reference CSV configured.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
