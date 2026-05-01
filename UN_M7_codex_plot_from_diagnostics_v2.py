#!/usr/bin/env python3
"""
UN_M7_codex_plot_from_diagnostics_v2.py

Robust plotter for Codex final capture_only diagnostic .dat files.

Why v2?
-------
The first version could fail on newer/older pandas because pandas.to_numeric(...,
errors="ignore") may raise "invalid error value specified". This version avoids
that and parses the gnuplot-style .dat files more defensively.

Run from repository root:

  cd ~/sciantix-official
  source .venv/bin/activate
  pip install matplotlib pandas numpy
  python UN_M7_codex_plot_from_diagnostics_v2.py

Outputs:

  UN_M7_codex_results/final_full_plots_from_dat_v2/

This script does not modify the model, does not run Optuna, and does not change
any existing result files. It only reads codex_final_*_diagnostics.dat and writes
PNG/CSV/MD outputs.
"""

from __future__ import annotations

import argparse
import re
from io import StringIO
from pathlib import Path
from typing import Iterable, Optional

import numpy as np
import pandas as pd

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

DEFAULT_INPUT_DIR = Path("UN_M7_codex_results")
DEFAULT_OUTPUT_DIR = Path("UN_M7_codex_results/final_full_plots_from_dat_v2")


def slugify(text: str) -> str:
    text = text.strip().replace(" ", "_")
    text = re.sub(r"[^A-Za-z0-9_.-]+", "_", text)
    text = re.sub(r"_+", "_", text)
    return text.strip("_") or "candidate"


def _split_fields(line: str) -> list[str]:
    line = line.strip()
    if "," in line:
        return [x.strip() for x in line.split(",") if x.strip()]
    return [x.strip() for x in re.split(r"\s+", line) if x.strip()]


def read_dat_file(path: Path) -> pd.DataFrame:
    """Read a Codex diagnostic .dat file robustly.

    Handles common gnuplot style files:
      # T_K swD ...
      900 0.1 ...

    Also handles normal whitespace or comma-separated tables.
    """
    lines = path.read_text(errors="replace").splitlines()
    if not any(ln.strip() for ln in lines):
        raise ValueError(f"empty file: {path}")

    # Find the most likely header: first comment/non-comment line with letters.
    header: Optional[list[str]] = None
    header_idx: Optional[int] = None
    for i, ln in enumerate(lines):
        s = ln.strip()
        if not s:
            continue
        candidate = s.lstrip("#").strip() if s.startswith("#") else s
        if any(ch.isalpha() for ch in candidate):
            fields = _split_fields(candidate)
            # avoid treating titles like "set terminal" as headers: require at least 2 fields
            if len(fields) >= 2:
                header = fields
                header_idx = i
                break
        # If first non-empty line is numeric, no header exists.
        if not s.startswith("#"):
            break

    data_lines: list[str] = []
    start = (header_idx + 1) if header_idx is not None else 0
    for ln in lines[start:]:
        s = ln.strip()
        if not s or s.startswith("#"):
            continue
        # Skip gnuplot command lines if accidentally present.
        lower = s.lower()
        if lower.startswith(("set ", "plot ", "unset ", "pause ")):
            continue
        # Keep only rows that start like numeric data.
        if not re.match(r"^[+\-]?(?:\d|\.\d)", s):
            continue
        data_lines.append(s)

    if not data_lines:
        raise ValueError(f"no numeric data rows found in {path}")

    sep = "," if any("," in ln for ln in data_lines[:3]) else r"\s+"

    if header is not None:
        # If row width differs from header, trim/pad safely.
        rows = []
        for ln in data_lines:
            fields = _split_fields(ln)
            rows.append(fields)
        maxw = max(len(r) for r in rows)
        if len(header) != maxw:
            if len(header) < maxw:
                header = header + [f"col_{j+1}" for j in range(len(header), maxw)]
            else:
                header = header[:maxw]
        csv_text = "\n".join(" ".join(r) for r in rows)
        df = pd.read_csv(StringIO(csv_text), sep=r"\s+", names=header, engine="python")
    else:
        df = pd.read_csv(StringIO("\n".join(data_lines)), sep=sep, engine="python")

    df.columns = [str(c).strip().strip("#") for c in df.columns]

    # Convert columns to numeric only when conversion actually gives numbers.
    for c in list(df.columns):
        converted = pd.to_numeric(df[c], errors="coerce")
        if converted.notna().any():
            df[c] = converted

    # Remove entirely empty columns if any.
    df = df.dropna(axis=1, how="all")
    return df


def find_col(df: pd.DataFrame, candidates: Iterable[str]) -> Optional[str]:
    cols = list(df.columns)
    low = {str(c).lower(): c for c in cols}
    norm = {re.sub(r"[^a-z0-9]+", "", str(c).lower()): c for c in cols}
    for cand in candidates:
        if cand in cols:
            return cand
        cl = cand.lower()
        if cl in low:
            return low[cl]
        cn = re.sub(r"[^a-z0-9]+", "", cl)
        if cn in norm:
            return norm[cn]
    return None


def find_first_contains(df: pd.DataFrame, must: Iterable[str], avoid: Iterable[str] = ()) -> Optional[str]:
    must_l = [m.lower() for m in must]
    avoid_l = [a.lower() for a in avoid]
    for c in df.columns:
        cl = str(c).lower()
        if all(m in cl for m in must_l) and not any(a in cl for a in avoid_l):
            return c
    return None


def x_temperature_col(df: pd.DataFrame) -> Optional[str]:
    return find_col(df, ["T", "T_K", "temperature", "temperature_K", "temp", "temp_K"])


def x_burnup_col(df: pd.DataFrame) -> Optional[str]:
    return find_col(df, ["burnup", "burnup_fima", "burnup_percent_fima", "FIMA", "fima", "bu", "burnup_percent"])


def ensure_dir(p: Path) -> None:
    p.mkdir(parents=True, exist_ok=True)


def numeric_col(df: pd.DataFrame, col: str) -> bool:
    return col in df.columns and pd.api.types.is_numeric_dtype(df[col]) and df[col].notna().any()


def plot_series(df: pd.DataFrame, xcol: str, ycols: list[str], title: str, ylabel: str, out: Path, logy: bool = False) -> bool:
    ycols = [c for c in ycols if c and numeric_col(df, c)]
    if not ycols or not numeric_col(df, xcol):
        return False
    sub = df[[xcol] + ycols].dropna(how="all")
    if sub.empty:
        return False
    plt.figure(figsize=(9, 6))
    for y in ycols:
        valid = sub[[xcol, y]].dropna()
        if valid.empty:
            continue
        plt.plot(valid[xcol], valid[y], marker="o", linewidth=1.5, markersize=3, label=str(y))
    plt.xlabel(str(xcol))
    plt.ylabel(ylabel)
    plt.title(title)
    if logy:
        plt.yscale("log")
    plt.grid(True, alpha=0.3)
    plt.legend()
    plt.tight_layout()
    plt.savefig(out, dpi=180)
    plt.close()
    return out.exists()


def extract_candidate_name(path: Path) -> str:
    stem = path.stem.replace("codex_final_", "").replace("_diagnostics", "")
    return slugify(stem)


def choose_columns(df: pd.DataFrame) -> dict[str, list[str]]:
    cols: dict[str, list[str]] = {}

    cols["swelling"] = [c for c in [
        find_col(df, ["swelling_d_percent", "swD_percent", "swD", "sw_d", "swD_1p3_1600K"]),
        find_col(df, ["swelling_b_percent", "swB_percent", "swB", "sw_b", "swB_1p3_1600K"]),
        find_col(df, ["swelling_total_percent", "sw_total", "total_swelling"]),
        find_first_contains(df, ["swelling", "d"], ["score"]),
        find_first_contains(df, ["swelling", "b"], ["score"]),
    ] if c]

    cols["radius"] = [c for c in [
        find_col(df, ["Rd_nm", "R_d_nm", "Rd", "R_d", "radius_d_nm", "dislocation_radius_nm"]),
        find_col(df, ["Rb_nm", "R_b_nm", "Rb", "R_b", "radius_b_nm", "bulk_radius_nm"]),
        find_first_contains(df, ["radius", "d"]),
        find_first_contains(df, ["radius", "b"]),
    ] if c]

    cols["concentration"] = [c for c in [
        find_col(df, ["Nd", "N_d", "Nd_m3", "N_d_m3", "dislocation_bubble_concentration"]),
        find_col(df, ["Nb", "N_b", "Nb_m3", "N_b_m3", "bulk_bubble_concentration"]),
        find_first_contains(df, ["concentration", "d"]),
        find_first_contains(df, ["concentration", "b"]),
    ] if c]

    cols["pressure"] = [c for c in [
        find_col(df, ["p_d", "pd", "pressure_d", "dislocation_pressure"]),
        find_col(df, ["p_d_eq", "pd_eq", "pressure_d_eq", "dislocation_equilibrium_pressure"]),
        find_col(df, ["p_b", "pb", "pressure_b", "bulk_pressure"]),
        find_col(df, ["p_b_eq", "pb_eq", "pressure_b_eq", "bulk_equilibrium_pressure"]),
    ] if c]

    cols["pressure_ratio"] = [c for c in [
        find_col(df, ["p_d_over_eq", "pd_over_eq", "p_d_over_p_eq", "p_d_over_p_d_eq", "p_d/p_d_eq"]),
        find_col(df, ["p_b_over_eq", "pb_over_eq", "p_b_over_p_eq", "p_b_over_p_b_eq", "p_b/p_b_eq"]),
        find_first_contains(df, ["p_d", "over"]),
        find_first_contains(df, ["p_b", "over"]),
    ] if c]

    cols["gas_partition"] = [c for c in [
        find_col(df, ["matrix_percent", "gas_matrix_percent", "matrix_gas_percent", "matrix"]),
        find_col(df, ["bulk_percent", "gas_bulk_percent", "bulk_gas_percent", "bulk_bubbles_percent"]),
        find_col(df, ["dislocation_percent", "gas_dislocation_percent", "dislocation_gas_percent", "dislocation_bubbles_percent"]),
        find_col(df, ["qgb_percent", "q_gb_percent", "grain_face_percent", "gas_to_grain_face_percent", "q_gb"]),
    ] if c]

    for k, v in list(cols.items()):
        seen = set()
        vv = []
        for c in v:
            if c not in seen:
                vv.append(c)
                seen.add(c)
        cols[k] = vv
    return cols


def split_by_burnup(df: pd.DataFrame, burnup_col: Optional[str]) -> list[tuple[str, pd.DataFrame]]:
    if burnup_col is None or burnup_col not in df.columns:
        return [("all", df)]
    vals = pd.to_numeric(df[burnup_col], errors="coerce")
    unique = sorted([float(v) for v in vals.dropna().unique()])
    if len(unique) <= 1 or len(unique) > 20:
        return [("all", df)]
    return [(f"burnup_{v:g}", df[np.isclose(vals, v, rtol=0, atol=1e-8)].copy()) for v in unique]


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--input-dir", default=str(DEFAULT_INPUT_DIR))
    ap.add_argument("--output-dir", default=str(DEFAULT_OUTPUT_DIR))
    args = ap.parse_args()

    input_dir = Path(args.input_dir)
    output_dir = Path(args.output_dir)
    ensure_dir(output_dir)

    dat_files = sorted(input_dir.glob("codex_final_*_diagnostics.dat"))
    if not dat_files:
        print(f"No codex_final_*_diagnostics.dat files found in {input_dir}")
        return 2

    report_lines: list[str] = [
        "# Missing/available plot report v2\n",
        f"Input directory: `{input_dir}`\n",
        f"Output directory: `{output_dir}`\n",
    ]
    made_any = False

    for dat in dat_files:
        name = extract_candidate_name(dat)
        cand_dir = output_dir / name
        ensure_dir(cand_dir)
        report_lines.append(f"\n## {name}\n")
        report_lines.append(f"Source: `{dat}`\n")

        try:
            df = read_dat_file(dat)
        except Exception as e:
            report_lines.append(f"ERROR reading file: `{type(e).__name__}: {e}`\n")
            continue

        parsed_csv = cand_dir / f"{name}_parsed_diagnostics.csv"
        df.to_csv(parsed_csv, index=False)
        report_lines.append(f"Rows: `{len(df)}`\n")
        report_lines.append(f"Parsed columns ({len(df.columns)}): `{', '.join(map(str, df.columns))}`\n")

        tcol = x_temperature_col(df)
        bcol = x_burnup_col(df)
        columns = choose_columns(df)
        report_lines.append(f"Temperature column: `{tcol}`\n")
        report_lines.append(f"Burnup column: `{bcol}`\n")

        if tcol is None:
            report_lines.append("No temperature column found; cannot make T-plots. Parsed CSV was still saved.\n")
            continue

        for group_name, sub in split_by_burnup(df, bcol):
            prefix = slugify(group_name)
            title_suffix = name if group_name == "all" else f"{name} - {group_name}"

            specs = [
                ("swelling", "Swelling [%]", False),
                ("radius", "Radius [as column units]", False),
                ("concentration", "Concentration [m$^{-3}$]", True),
                ("pressure", "Pressure [Pa]", True),
                ("pressure_ratio", "p / p_eq [-]", True),
                ("gas_partition", "Amount of generated gas [%]", False),
            ]
            for key, ylabel, logy in specs:
                ycols = columns.get(key, [])
                if ycols:
                    ok = plot_series(
                        sub, tcol, ycols,
                        f"{key.replace('_', ' ').title()} vs T - {title_suffix}",
                        ylabel,
                        cand_dir / f"{prefix}_{key}_T.png",
                        logy=logy,
                    )
                    made_any = made_any or ok
                    if ok:
                        report_lines.append(f"- {group_name}: created `{prefix}_{key}_T.png` using `{', '.join(map(str, ycols))}`\n")
                    else:
                        report_lines.append(f"- {group_name}: failed to plot {key}; columns found but not numeric/usable: `{', '.join(map(str, ycols))}`\n")
                else:
                    report_lines.append(f"- {group_name}: no {key} columns found.\n")

        report_lines.append(f"Created candidate folder: `{cand_dir}`\n")

    report = output_dir / "missing_columns_report_v2.md"
    report.write_text("\n".join(report_lines), encoding="utf-8")

    print("Done.")
    print(f"Plots/report written under: {output_dir}")
    print(f"Report: {report}")
    if not made_any:
        print("WARNING: no plots were created. Send missing_columns_report_v2.md back to ChatGPT.")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
