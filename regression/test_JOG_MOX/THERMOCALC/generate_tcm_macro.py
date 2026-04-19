#!/usr/bin/env python3
"""Generate a Thermo-Calc Console Mode .tcm macro (no CLI parser).

Edit values in USER SETTINGS and run:
    python3 generate_tcm_macro.py
"""

from __future__ import annotations

from pathlib import Path, PureWindowsPath
from typing import List


# =========================
# USER SETTINGS (edit here)
# =========================
DATABASE_PATH = r"C:\Users\elisa\OneDrive - Politecnico di Milano\THERMOCALC\Cs-Mo-O\CsMoO_TAFID_2021_Anna.TDB"
THERMOCALC_OUT_DIR = r"C:\Users\elisa\OneDrive - Politecnico di Milano\THERMOCALC\Cs-Mo-O"
MACRO_PATH = Path(__file__).resolve().parent / "MAP_TP_ACRO_1EMINUS12.tcm"

# Temperature sweep [K]
T_MIN = 500.0
T_MAX = 3000.0
T_STEP = 50.0

# Pressure sweep [Pa]
P_MIN = 1.0e5
P_MAX = 1.0e7
P_STEP = 2.0e4

# Fixed composition/oxygen activity
N_CS = 0.17
N_MO = 0.22
ACR_O = 1.0e-12

# Reference pressure used in SET_REFERENCE_STATE (Pa)
P_REF_FOR_GAS = 1.0e5


def sci(v: float) -> str:
    return f"{v:.6E}".replace("E+0", "E+").replace("E-0", "E-")


def compact_tag(v: float) -> str:
    s = f"{v:.1e}".replace("+", "")
    return s.replace("-", "m").replace(".", "p")


def frange_inclusive(start: float, stop: float, step: float) -> List[float]:
    vals: List[float] = []
    x = start
    while x <= stop + 0.5 * abs(step):
        vals.append(round(x, 10))
        x += step
    return vals


def build_tcm_text(temperatures_k: List[float], tabulate_output_path: str) -> str:
    lines: List[str] = []
    lines.append("@@ Auto-generated Thermo-Calc Console Mode macro")
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
    lines.append("@@ Set oxygen reference to GAS")
    lines.append("SET_REFERENCE_STATE")
    lines.append("O")
    lines.append("GAS")
    lines.append("*")
    lines.append(sci(P_REF_FOR_GAS))
    lines.append("")
    lines.append("@@ Starting state")
    lines.append(f"SET_CONDITION P={sci(P_MIN)}")
    lines.append(f"SET_CONDITION T={sci(temperatures_k[0])}")
    lines.append(f"SET_CONDITION N(CS)={sci(N_CS)}")
    lines.append(f"SET_CONDITION N(MO)={sci(N_MO)}")
    lines.append(f"SET_CONDITION ACR(O)={sci(ACR_O)}")
    lines.append("")
    lines.append("COMPUTE_EQUILIBRIUM")
    lines.append("")
    lines.append("ENTER_SYMBOL")
    lines.append("TABLE")
    lines.append("GRID=T,P,N(*),AC(O),ACR(O),NP(*),X(GAS,*),Y(GAS,*)")
    lines.append("")

    for t in temperatures_k:
        lines.append(f"@@ Isothermal sweep at T = {t:g} K")
        lines.append(f"SET_CONDITION T={sci(t)}")
        lines.append("COMPUTE_EQUILIBRIUM")
        lines.append("SET_AXIS_VARIABLE")
        lines.append("1")
        lines.append("P")
        lines.append(sci(P_MIN))
        lines.append(sci(P_MAX))
        lines.append(sci(P_STEP))
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
    if T_STEP <= 0 or P_STEP <= 0:
        raise ValueError("T_STEP and P_STEP must be > 0")
    if T_MIN > T_MAX or P_MIN > P_MAX:
        raise ValueError("Range min must be <= max")

    temperatures = frange_inclusive(T_MIN, T_MAX, T_STEP)

    out_name = (
        f"results_T{int(T_MIN)}-{int(T_MAX)}K_dT{int(T_STEP)}_"
        f"P{compact_tag(P_MIN)}_to_{compact_tag(P_MAX)}_d{compact_tag(P_STEP)}_"
        f"ACRO_{compact_tag(ACR_O)}_NCS_{N_CS:.4f}_NMO_{N_MO:.4f}.txt"
    )
    tabulate_output_path = str(PureWindowsPath(THERMOCALC_OUT_DIR) / out_name)

    text = build_tcm_text(temperatures, tabulate_output_path)
    MACRO_PATH.write_text(text, encoding="utf-8")

    print(f"Macro written: {MACRO_PATH}")
    print(f"TABULATE output file (inside Thermo-Calc): {tabulate_output_path}")
    print(f"Temperatures: {len(temperatures)} points from {T_MIN:g} to {T_MAX:g} K")


if __name__ == "__main__":
    main()
