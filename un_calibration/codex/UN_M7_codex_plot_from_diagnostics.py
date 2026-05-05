#!/usr/bin/env python3
"""
UN_M7_codex_plot_from_diagnostics.py

Purpose
-------
Create a richer set of PNG plots for Codex-selected capture_only candidates,
using the final diagnostic .dat files already produced by UN_M7_codex_analysis.py.

This script DOES NOT change the model equations and DOES NOT re-run Optuna.
It only reads final diagnostic data files such as:

  UN_M7_codex_results/codex_final_*_diagnostics.dat

and creates plots under:

  UN_M7_codex_results/final_full_plots_from_dat/

If a requested plot cannot be produced because the required columns are not in the
.dat file, the script writes this explicitly in missing_columns_report.md.

Run from repository root:

  cd ~/sciantix-official
  source .venv/bin/activate
  pip install matplotlib pandas
  python UN_M7_codex_plot_from_diagnostics.py

"""

from __future__ import annotations

import argparse
import re
from pathlib import Path
from typing import Iterable, Optional

import numpy as np
import pandas as pd

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


DEFAULT_INPUT_DIR = Path("UN_M7_codex_results")
DEFAULT_OUTPUT_DIR = Path("UN_M7_codex_results/final_full_plots_from_dat")


def slugify(text: str) -> str:
    text = text.strip().replace(" ", "_")
    text = re.sub(r"[^A-Za-z0-9_.-]+", "_", text)
    text = re.sub(r"_+", "_", text)
    return text.strip("_") or "candidate"


def read_dat_file(path: Path) -> pd.DataFrame:
    """Read a Codex diagnostic .dat file robustly.

    Supports:
    - whitespace-separated columns with header beginning with '#'
    - whitespace-separated columns with normal header
    - comma-separated files
    """
    text = path.read_text(errors="replace").splitlines()
    nonempty = [ln for ln in text if ln.strip()]
    if not nonempty:
        raise ValueError(f"empty file: {path}")

    header_line: Optional[str] = None
    data_start_idx = 0

    # Prefer a comment header that contains alphabetic column names.
    for i, ln in enumerate(text):
        s = ln.strip()
        if not s:
            continue
        if s.startswith("#"):
            candidate = s.lstrip("#").strip()
            if any(ch.isalpha() for ch in candidate):
                header_line = candidate
                data_start_idx = i + 1
        else:
            # If first non-comment line looks like a header, use it.
            if header_line is None and any(ch.isalpha() for ch in s):
                header_line = s
                data_start_idx = i + 1
            break

    if header_line is not None:
        sep = "," if "," in header_line else r"\s+"
        names = [c.strip() for c in re.split(r",|\s+", header_line) if c.strip()]
        data_lines = []
        for ln in text[data_start_idx:]:
            s = ln.strip()
            if not s or s.startswith("#"):
                continue
            data_lines.append(s)
        if not data_lines:
            raise ValueError(f"no data rows after header in {path}")
        from io import StringIO
        df = pd.read_csv(StringIO("\n".join(data_lines)), sep=sep, names=names, engine="python")
    else:
        sep = "," if "," in nonempty[0] else r"\s+"
        df = pd.read_csv(path, sep=sep, comment="#", engine="python")

    # Clean column names.
    df.columns = [str(c).strip().strip("#") for c in df.columns]

    # Convert possible numeric columns.
    for c in df.columns:
        df[c] = pd.to_numeric(df[c], errors="ignore")

    return df


def find_col(df: pd.DataFrame, candidates: Iterable[str]) -> Optional[str]:
    """Find a column by exact or normalized name."""
    cols = list(df.columns)
    low = {c.lower(): c for c in cols}
    norm = {re.sub(r"[^a-z0-9]+", "", c.lower()): c for c in cols}
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
        cl = c.lower()
        if all(m in cl for m in must_l) and not any(a in cl for a in avoid_l):
            return c
    return None


def x_temperature_col(df: pd.DataFrame) -> Optional[str]:
    return find_col(df, ["T", "T_K", "temperature", "temperature_K", "temp", "temp_K"])


def x_burnup_col(df: pd.DataFrame) -> Optional[str]:
    return find_col(df, ["burnup", "burnup_fima", "burnup_percent_fima", "FIMA", "fima", "bu"])


def ensure_dir(p: Path) -> None:
    p.mkdir(parents=True, exist_ok=True)


def plot_series(df: pd.DataFrame, xcol: str, ycols: list[str], title: str, ylabel: str, out: Path, logy: bool = False) -> bool:
    ycols = [c for c in ycols if c and c in df.columns]
    if not ycols or xcol not in df.columns:
        return False
    plt.figure(figsize=(9, 6))
    for y in ycols:
        try:
            plt.plot(df[xcol], df[y], marker="o", linewidth=1.5, markersize=3, label=y)
        except Exception:
            continue
    plt.xlabel(xcol)
    plt.ylabel(ylabel)
    plt.title(title)
    if logy:
        plt.yscale("log")
    plt.grid(True, alpha=0.3)
    plt.legend()
    plt.tight_layout()
    plt.savefig(out, dpi=180)
    plt.close()
    return True


def extract_candidate_name(path: Path) -> str:
    stem = path.stem
    stem = stem.replace("codex_final_", "")
    stem = stem.replace("_diagnostics", "")
    return slugify(stem)


def choose_columns(df: pd.DataFrame) -> dict[str, list[str]]:
    """Choose likely columns for standard plots."""
    cols: dict[str, list[str]] = {}

    cols["swelling"] = [c for c in [
        find_first_contains(df, ["sw", "d"], ["score"]),
        find_first_contains(df, ["sw", "b"], ["score"]),
        find_first_contains(df, ["swelling", "d"], ["score"]),
        find_first_contains(df, ["swelling", "b"], ["score"]),
        find_first_contains(df, ["swelling", "total"], ["score"]),
    ] if c]

    cols["radius"] = [c for c in [
        find_col(df, ["Rd_nm", "R_d_nm", "Rd", "R_d", "radius_d_nm", "dislocation_radius_nm"]),
        find_col(df, ["Rb_nm", "R_b_nm", "Rb", "R_b", "radius_b_nm", "bulk_radius_nm"]),
    ] if c]

    cols["concentration"] = [c for c in [
        find_col(df, ["Nd", "N_d", "Nd_m3", "N_d_m3", "dislocation_bubble_concentration"]),
        find_col(df, ["Nb", "N_b", "Nb_m3", "N_b_m3", "bulk_bubble_concentration"]),
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
    ] if c]

    cols["gas_partition"] = [c for c in [
        find_col(df, ["matrix_percent", "gas_matrix_percent", "matrix_gas_percent", "matrix"]),
        find_col(df, ["bulk_percent", "gas_bulk_percent", "bulk_gas_percent", "bulk_bubbles_percent"]),
        find_col(df, ["dislocation_percent", "gas_dislocation_percent", "dislocation_gas_percent", "dislocation_bubbles_percent"]),
        find_col(df, ["qgb_percent", "q_gb_percent", "grain_face_percent", "gas_to_grain_face_percent"]),
    ] if c]

    # Remove duplicates while preserving order.
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
    # Group only if reasonable number of unique values.
    vals = pd.to_numeric(df[burnup_col], errors="coerce")
    unique = sorted([v for v in vals.dropna().unique()])
    if len(unique) <= 1 or len(unique) > 10:
        return [("all", df)]
    groups = []
    for v in unique:
        sub = df[np.isclose(vals, v, rtol=0, atol=1e-8)].copy()
        groups.append((f"burnup_{v:g}", sub))
    return groups


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--input-dir", default=str(DEFAULT_INPUT_DIR), help="Directory containing codex_final_*_diagnostics.dat")
    ap.add_argument("--output-dir", default=str(DEFAULT_OUTPUT_DIR), help="Directory to write PNG plots")
    args = ap.parse_args()

    input_dir = Path(args.input_dir)
    output_dir = Path(args.output_dir)
    ensure_dir(output_dir)

    dat_files = sorted(input_dir.glob("codex_final_*_diagnostics.dat"))
    if not dat_files:
        print(f"No codex_final_*_diagnostics.dat files found in {input_dir}")
        print("Expected files like UN_M7_codex_results/codex_final_1_best_overall_score_dv0.05_diagnostics.dat")
        return 2

    report_lines: list[str] = []
    report_lines.append("# Missing/available plot report\n")
    report_lines.append(f"Input directory: `{input_dir}`\n")
    report_lines.append(f"Output directory: `{output_dir}`\n")

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
            report_lines.append(f"ERROR reading file: `{e}`\n")
            continue

        # Save parsed CSV for inspection.
        parsed_csv = cand_dir / f"{name}_parsed_diagnostics.csv"
        df.to_csv(parsed_csv, index=False)
        report_lines.append(f"Parsed columns ({len(df.columns)}): `{', '.join(map(str, df.columns))}`\n")

        tcol = x_temperature_col(df)
        bcol = x_burnup_col(df)
        columns = choose_columns(df)

        if tcol is None:
            report_lines.append("No temperature column found; cannot make T-plots.\n")
            continue

        for group_name, sub in split_by_burnup(df, bcol):
            prefix = slugify(group_name)
            title_suffix = name if group_name == "all" else f"{name} - {group_name}"

            if columns["swelling"]:
                ok = plot_series(sub, tcol, columns["swelling"], f"Swelling vs T - {title_suffix}", "Swelling [%]", cand_dir / f"{prefix}_swelling_T.png")
                made_any = made_any or ok
            else:
                report_lines.append(f"- {group_name}: no swelling columns found.\n")

            if columns["radius"]:
                ok = plot_series(sub, tcol, columns["radius"], f"Bubble radius vs T - {title_suffix}", "Radius [as column units]", cand_dir / f"{prefix}_radius_T.png")
                made_any = made_any or ok
            else:
                report_lines.append(f"- {group_name}: no radius columns found.\n")

            if columns["concentration"]:
                ok = plot_series(sub, tcol, columns["concentration"], f"Bubble concentration vs T - {title_suffix}", "Concentration [m$^{-3}$]", cand_dir / f"{prefix}_concentration_T.png", logy=True)
                made_any = made_any or ok
            else:
                report_lines.append(f"- {group_name}: no concentration columns found.\n")

            if columns["pressure"]:
                ok = plot_series(sub, tcol, columns["pressure"], f"Pressure diagnostic vs T - {title_suffix}", "Pressure [Pa]", cand_dir / f"{prefix}_pressure_T.png", logy=True)
                made_any = made_any or ok
            else:
                report_lines.append(f"- {group_name}: no pressure columns found.\n")

            if columns["pressure_ratio"]:
                ok = plot_series(sub, tcol, columns["pressure_ratio"], f"Pressure ratio vs T - {title_suffix}", "p / p_eq [-]", cand_dir / f"{prefix}_pressure_ratio_T.png", logy=True)
                made_any = made_any or ok
            else:
                report_lines.append(f"- {group_name}: no pressure-ratio columns found.\n")

            if columns["gas_partition"]:
                ok = plot_series(sub, tcol, columns["gas_partition"], f"Gas partition vs T - {title_suffix}", "Amount of generated gas [%]", cand_dir / f"{prefix}_gas_partition_T.png")
                made_any = made_any or ok
            else:
                report_lines.append(f"- {group_name}: no gas-partition columns found.\n")

        report_lines.append(f"Created candidate folder: `{cand_dir}`\n")

    report = output_dir / "missing_columns_report.md"
    report.write_text("\n".join(report_lines), encoding="utf-8")

    print("Done.")
    print(f"Plots/report written under: {output_dir}")
    print(f"Missing/available columns report: {report}")
    if not made_any:
        print("WARNING: no plots were created. Open missing_columns_report.md to see which columns were found.")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
