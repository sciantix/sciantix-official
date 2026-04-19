#!/usr/bin/env python3
"""Generate Thermo-Calc .tcm: sweep T repeatedly for a series of ACR(O) values.

No LOGARITHMIC step in Thermo-Calc is used.
We generate log-spaced ACR values in Python, then for each ACR we do a normal T sweep.

Run:
    python3 generate_tcm_macro_acr.py
"""

from __future__ import annotations

import math
from pathlib import Path, PureWindowsPath
from typing import List


# =========================
# USER SETTINGS (edit here)
# =========================
DATABASE_PATH = r"C:\Users\elisa\OneDrive - Politecnico di Milano\THERMOCALC\Cs-Mo-O\CsMoO_TAFID_2021_Anna.TDB"
THERMOCALC_OUT_DIR = r"C:\Users\elisa\OneDrive - Politecnico di Milano\THERMOCALC\Cs-Mo-O"
MACRO_PATH = Path(__file__).resolve().parent / "MAP_T_sweep_for_multiple_ACR.tcm"

# T sweep [K]
T_MIN = 500.0
T_MAX = 3000.0
T_STEP = 50.0

# ACR(O) values generated in Python (log-spaced)
ACR_MIN = 1.0e-16
ACR_MAX = 1.0e-2
POINTS_PER_DECADE = 1

# Fixed conditions
P_FIXED = 1.0e5
N_CS = 0.17
N_MO = 0.22
P_REF_FOR_GAS = 1.0e5


def sci(v: float) -> str:
    return f"{v:.6E}".replace("E+0", "E+").replace("E-0", "E-")


def compact_tag(v: float) -> str:
    s = f"{v:.1e}".replace("+", "")
    return s.replace("-", "m").replace(".", "p")


def log_spaced(min_val: float, max_val: float, points_per_decade: int) -> List[float]:
    if min_val <= 0 or max_val <= 0:
        raise ValueError("ACR min/max must be > 0 for log spacing")
    if min_val > max_val:
        raise ValueError("ACR_MIN must be <= ACR_MAX")
    if points_per_decade <= 0:
        raise ValueError("POINTS_PER_DECADE must be > 0")

    lo = math.log10(min_val)
    hi = math.log10(max_val)
    n_steps = int(round((hi - lo) * points_per_decade))
    vals = [10.0 ** (lo + i / points_per_decade) for i in range(n_steps + 1)]

    # Ensure exact endpoints are present
    vals[0] = min_val
    vals[-1] = max_val
    return vals


def build_tcm_text(acr_values: List[float], tabulate_output_path: str) -> str:
    lines: List[str] = []
    lines.append("@@ Auto-generated Thermo-Calc Console Mode macro")
    lines.append("@@ Strategy: for each ACR(O), do a NORMAL sweep on T")
    lines.append("GOTO_MODULE DATABASE_RETRIEVAL")
    lines.append("")
    lines.append("SWITCH_DATABASE")
    lines.append(f'USER "{DATABASE_PATH}"')
    lines.append("")
    lines.append("DEFINE_SYSTEM CS MO O")
    lines.append("GET_DATA")
    lines.append("")
    lines.append("GOTO_MODULE POLY-3")
    lines.append("")
    lines.append("SET_REFERENCE_STATE")
    lines.append("O")
    lines.append("GAS")
    lines.append("*")
    lines.append(sci(P_REF_FOR_GAS))
    lines.append("")
    lines.append("@@ Starting state")
    lines.append(f"SET_CONDITION P={sci(P_FIXED)}")
    lines.append(f"SET_CONDITION T={sci(T_MIN)}")
    lines.append(f"SET_CONDITION N(CS)={sci(N_CS)}")
    lines.append(f"SET_CONDITION N(MO)={sci(N_MO)}")
    lines.append(f"SET_CONDITION ACR(O)={sci(acr_values[0])}")
    lines.append("")
    lines.append("COMPUTE_EQUILIBRIUM")
    lines.append("")
    lines.append("ENTER_SYMBOL")
    lines.append("TABLE")
    lines.append("GRID=T,P,N(*),AC(O),ACR(O),NP(*),X(GAS,*),Y(GAS,*)")
    lines.append("")

    for acr in acr_values:
        lines.append(f"@@ ACR(O) = {acr:.6E}")
        lines.append(f"SET_CONDITION ACR(O)={sci(acr)}")
        lines.append("COMPUTE_EQUILIBRIUM")
        lines.append("SET_AXIS_VARIABLE")
        lines.append("1")
        lines.append("T")
        lines.append(sci(T_MIN))
        lines.append(sci(T_MAX))
        lines.append(sci(T_STEP))
        lines.append("STEP")
        lines.append("NORMAL")
        lines.append("")

    lines.append("@@ Write the calculated grid to a text file")
    lines.append("TABULATE")
    lines.append("GRID")
    lines.append(f'"{tabulate_output_path}"')
    lines.append("")
    return "\n".join(lines)


def main() -> None:
    if T_STEP <= 0:
        raise ValueError("T_STEP must be > 0")
    if T_MIN > T_MAX:
        raise ValueError("T_MIN must be <= T_MAX")

    acr_values = log_spaced(ACR_MIN, ACR_MAX, POINTS_PER_DECADE)

    out_name = (
        f"results_T{int(T_MIN)}-{int(T_MAX)}K_dT{int(T_STEP)}_"
        f"ACRseries_{compact_tag(ACR_MIN)}_to_{compact_tag(ACR_MAX)}_"
        f"ppd{POINTS_PER_DECADE}_P_{compact_tag(P_FIXED)}_"
        f"NCS_{N_CS:.4f}_NMO_{N_MO:.4f}.txt"
    )
    tabulate_output_path = str(PureWindowsPath(THERMOCALC_OUT_DIR) / out_name)

    text = build_tcm_text(acr_values, tabulate_output_path)
    MACRO_PATH.write_text(text, encoding="utf-8")

    print(f"Macro written: {MACRO_PATH}")
    print(f"TABULATE output file (inside Thermo-Calc): {tabulate_output_path}")
    print(f"ACR points: {len(acr_values)} from {ACR_MIN:.3E} to {ACR_MAX:.3E}")
    print(f"T points per sweep: {int(round((T_MAX - T_MIN) / T_STEP)) + 1}")


if __name__ == "__main__":
    main()
