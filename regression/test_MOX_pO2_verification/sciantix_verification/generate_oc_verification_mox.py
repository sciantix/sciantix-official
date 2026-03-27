#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent
ROOT_DIR = SCRIPT_DIR.parent
SUMMARY_PATH = ROOT_DIR / "temperature_sweep_summary.tsv"
OUTPUT_OCM_PATH = SCRIPT_DIR / "oc_verification.OCM"

O_LOW_START = 1.90
O_LOW_END = 1.98
O_HIGH_START = 2.02
O_HIGH_END = 2.31
O_STEP = 0.01

def q_tag(q_value: float) -> str:
    return f"{q_value:.2f}".replace(".", "p")


def required_cases_from_summary(summary_path: Path) -> list[tuple[int, float]]:
    if not summary_path.exists():
        temperatures = list(range(800, 2800, 100))
        q_values = [value / 100 for value in range(10, 40, 5)]
        return [(t, q) for t in temperatures for q in q_values]

    frame = pd.read_csv(summary_path, sep="\t")

    temperature_col = "Temperature (K)" if "Temperature (K)" in frame.columns else "Temperature target (K)"
    q_col = "q (-)" if "q (-)" in frame.columns else "q target (-)"

    frame[temperature_col] = pd.to_numeric(frame[temperature_col], errors="coerce")
    frame[q_col] = pd.to_numeric(frame[q_col], errors="coerce")

    if "Case" in frame.columns:
        required = frame[["Case", temperature_col, q_col]].dropna().drop_duplicates("Case")
    else:
        required = frame[[temperature_col, q_col]].dropna().drop_duplicates()

    pairs = [
        (int(round(float(row[temperature_col]))), float(row[q_col]))
        for _, row in required.iterrows()
    ]
    pairs = sorted(set(pairs), key=lambda item: (item[0], item[1]))
    return pairs


def build_ocm_text(cases: list[tuple[int, float]]) -> str:
    lines: list[str] = []
    lines.append("@$ Initialize variables:")
    lines.append("r t ./O-Pu-UPaper")
    lines.append("O Pu U")
    lines.append("")
    lines.append("set ref o gas * 1e5")
    lines.append("")

    for temperature_k, q_value in cases:
        n_pu = q_value
        n_u = 1.0 - q_value
        tag = q_tag(q_value)

        lines.append(f"@$ {temperature_k}K q={q_value:.6f}")
        lines.append("")
        lines.append(
            f"set c t={temperature_k} p=1e5"
        )
        lines.append("")
        lines.append(
            f"set c n(u)={n_u:.8f} n(pu)={n_pu:.8f} n(o)={O_LOW_START:.2f}"
        )
        lines.append("")
        lines.append("c e")
        lines.append("")
        lines.append("c c")
        lines.append("")
        lines.append(f"set axis 1 n(o) {O_LOW_START:.2f} {O_LOW_END:.2f} {O_STEP:.2f}")
        lines.append("")
        lines.append("step n")
        lines.append("")
        lines.append(f"l ex n(o) ac(o) ./test_{temperature_k}K_q_{tag}_ipo")
        lines.append("")
        lines.append(
            f"set c t={temperature_k} p=1e5 n(u)={n_u:.8f} n(pu)={n_pu:.8f} n(o)={O_HIGH_START:.2f}"
        )
        lines.append("")
        lines.append("c e")
        lines.append("")
        lines.append("c c")
        lines.append("")
        lines.append(f"set axis 1 n(o) {O_HIGH_START:.2f} {O_HIGH_END:.2f} {O_STEP:.2f}")
        lines.append("")
        lines.append("step n")
        lines.append("")
        lines.append(f"l ex n(o) ac(o) ./test_{temperature_k}K_q_{tag}_iper")
        lines.append("")
    
    lines.append("fin")

    return "\n".join(lines).rstrip() + "\n"


def main() -> None:
    cases = required_cases_from_summary(SUMMARY_PATH)
    ocm_text = build_ocm_text(cases)
    OUTPUT_OCM_PATH.write_text(ocm_text)
    print(f"Wrote {OUTPUT_OCM_PATH} with {len(cases)} (T,q) cases")


if __name__ == "__main__":
    main()
