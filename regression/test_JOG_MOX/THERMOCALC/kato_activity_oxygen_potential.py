#!/usr/bin/env python3
"""Kato-equation solver to relate oxygen activity and oxygen potential.

This script mirrors the SCIANTIX Kato implementation and generates two tables:
1) Given T, q, O/M: solve pO2 and compute oxygen activity and oxygen potential
2) Given T, q, pO2: compute O/M and oxygen activity and oxygen potential

Definitions used:
- pressure ratio r = pO2 / p_ref, with p_ref = 0.1013 MPa
- oxygen potential (for O2 basis): Delta mu_O2 = R*T*ln(r) [kJ/mol]
- oxygen activity (atomic O proxy): a_O = sqrt(r)
  hence Delta mu_O2 = 2*R*T*ln(a_O)
"""

from __future__ import annotations

import math
from pathlib import Path


# User-defined sampling points
TEMPERATURES_K = [1200.0, 1400.0, 1600.0, 1800.0]
Q_VALUES = [0.10, 0.20, 0.30, 0.40]
OM_VALUES = [1.94, 1.96, 1.98, 2.00]
PRESSURES_MPA = [1.0e-15, 1.0e-12, 1.0e-9, 1.0e-6, 1.0e-3]

REFERENCE_PRESSURE_MPA = 0.1013
GAS_CONSTANT = 8.314  # J/(mol K)


def adjusted_temperature(temperature_actual: float, q_eff: float) -> float:
    """NEA-based temperature adjustment used with the Kato correlation."""
    exponent = -300.0 * (0.335 - q_eff)
    sigmoid_s = 1.0 / (1.0 + math.exp(exponent))
    return temperature_actual + (0.16 * sigmoid_s) * (1773.0 - temperature_actual)


def om_from_pressure_ratio(temperature_actual: float, q_eff: float, pressure_ratio: float) -> float:
    """Explicit Kato equation: O/M = f(T, q, pO2/p_ref)."""
    if pressure_ratio <= 0.0:
        return math.nan
    if q_eff <= 0.0:
        raise ValueError("q must be > 0 for the Kato equation term log(0.5*q).")

    temperature_adjusted = adjusted_temperature(temperature_actual, q_eff)
    ln_po2 = math.log(pressure_ratio)

    log_v1 = -5.0 * (
        (44.0 + 55.8 * q_eff) / GAS_CONSTANT
        - 376000.0 / (GAS_CONSTANT * temperature_adjusted)
        - 0.5 * ln_po2
    )
    log_v2 = -5.0 * (
        0.5 * (68.8 + 131.3 * q_eff) / GAS_CONSTANT
        - 0.5 * 515000.0 / (GAS_CONSTANT * temperature_adjusted)
        - 0.25 * ln_po2
    )
    log_v3 = -5.0 * (
        (
            math.log(2.0)
            + (153.5 - 96.5 * q_eff + 331.0 * q_eff * q_eff) / GAS_CONSTANT
            - 891000.0 / (GAS_CONSTANT * temperature_adjusted)
        ) / 3.0
        - ln_po2 / 3.0
    )
    log_v4 = -5.0 * math.log(0.5 * q_eff)

    max_log = max(log_v1, log_v2, log_v3, log_v4)
    sum_exp = (
        math.exp(log_v1 - max_log)
        + math.exp(log_v2 - max_log)
        + math.exp(log_v3 - max_log)
        + math.exp(log_v4 - max_log)
    )
    s_term = math.exp(-0.2 * (max_log + math.log(sum_exp)))

    term5 = math.exp(
        (-22.8 - 84.5 * q_eff) / GAS_CONSTANT
        + 105000.0 / (GAS_CONSTANT * temperature_adjusted)
        + 0.5 * ln_po2
    )

    return 2.0 - s_term + term5


def solve_pressure_ratio_from_om(
    temperature_actual: float,
    q_eff: float,
    om_target: float,
    log_low: float = -80.0,
    log_high: float = 10.0,
    max_iter: int = 120,
    tol_om: float = 1.0e-12,
) -> float:
    """Bisection solver for pO2/p_ref from a target O/M using Kato equation."""
    if q_eff <= 0.0:
        raise ValueError("q must be > 0 for the Kato equation term log(0.5*q).")

    low = log_low
    high = log_high

    om_low = om_from_pressure_ratio(temperature_actual, q_eff, 10.0 ** low)
    om_high = om_from_pressure_ratio(temperature_actual, q_eff, 10.0 ** high)
    if not (om_low <= om_target <= om_high):
        raise ValueError(
            f"O/M={om_target} outside bracketed range [{om_low:.6g}, {om_high:.6g}] "
            f"for T={temperature_actual}, q={q_eff}."
        )

    log_mid = 0.5 * (low + high)
    for _ in range(max_iter):
        log_mid = 0.5 * (low + high)
        ratio_mid = 10.0 ** log_mid
        om_mid = om_from_pressure_ratio(temperature_actual, q_eff, ratio_mid)

        if abs(om_mid - om_target) < tol_om:
            break

        if om_mid > om_target:
            high = log_mid
        else:
            low = log_mid

    return 10.0 ** log_mid


def oxygen_potential_kj_per_mol(temperature_k: float, pressure_ratio: float) -> float:
    """Delta mu_O2 = R*T*ln(pO2/p_ref), in kJ/mol."""
    return GAS_CONSTANT * temperature_k * math.log(pressure_ratio) * 1.0e-3


def oxygen_activity_from_pressure_ratio(pressure_ratio: float) -> float:
    """Atomic oxygen activity proxy from oxygen pressure ratio: a_O = sqrt(pO2/p_ref)."""
    return math.sqrt(pressure_ratio)


def build_rows_from_om() -> list[dict[str, float | str]]:
    rows: list[dict[str, float | str]] = []
    for temperature_k in TEMPERATURES_K:
        for q_value in Q_VALUES:
            for om_value in OM_VALUES:
                row: dict[str, float | str] = {
                    "T (K)": temperature_k,
                    "q (-)": q_value,
                    "O/M (-)": om_value,
                    "pO2/p_ref (-)": math.nan,
                    "pO2 (MPa)": math.nan,
                    "a_O (-)": math.nan,
                    "oxygen potential (kJ/mol O2)": math.nan,
                    "status": "ok",
                }
                try:
                    pressure_ratio = solve_pressure_ratio_from_om(temperature_k, q_value, om_value)
                    p_o2_mpa = REFERENCE_PRESSURE_MPA * pressure_ratio
                    a_o = oxygen_activity_from_pressure_ratio(pressure_ratio)
                    potential_kj_mol = oxygen_potential_kj_per_mol(temperature_k, pressure_ratio)

                    row.update(
                        {
                            "pO2/p_ref (-)": pressure_ratio,
                            "pO2 (MPa)": p_o2_mpa,
                            "a_O (-)": a_o,
                            "oxygen potential (kJ/mol O2)": potential_kj_mol,
                        }
                    )
                except ValueError as exc:
                    row["status"] = str(exc)
                rows.append(row)
    return rows


def build_rows_from_pressure() -> list[dict[str, float | str]]:
    rows: list[dict[str, float | str]] = []
    for temperature_k in TEMPERATURES_K:
        for q_value in Q_VALUES:
            for p_o2_mpa in PRESSURES_MPA:
                pressure_ratio = p_o2_mpa / REFERENCE_PRESSURE_MPA
                if pressure_ratio <= 0.0:
                    continue
                om_value = om_from_pressure_ratio(temperature_k, q_value, pressure_ratio)
                a_o = oxygen_activity_from_pressure_ratio(pressure_ratio)
                potential_kj_mol = oxygen_potential_kj_per_mol(temperature_k, pressure_ratio)

                rows.append(
                    {
                        "T (K)": temperature_k,
                        "q (-)": q_value,
                        "pO2 (MPa)": p_o2_mpa,
                        "pO2/p_ref (-)": pressure_ratio,
                        "a_O (-)": a_o,
                        "O/M (-)": om_value,
                        "oxygen potential (kJ/mol O2)": potential_kj_mol,
                        "status": "ok",
                    }
                )
    return rows


def write_tsv(path: Path, rows: list[dict[str, float | str]], columns: list[str]) -> None:
    with path.open("w", encoding="utf-8") as handle:
        handle.write("\t".join(columns) + "\n")
        for row in rows:
            values = []
            for col in columns:
                value = row[col]
                if isinstance(value, float):
                    values.append(f"{value:.10e}")
                else:
                    values.append(str(value))
            handle.write("\t".join(values) + "\n")


def main() -> None:
    out_dir = Path(__file__).resolve().parent

    rows_from_om = build_rows_from_om()
    rows_from_pressure = build_rows_from_pressure()

    file_from_om = out_dir / "kato_from_om.tsv"
    file_from_pressure = out_dir / "kato_from_pressure.tsv"

    write_tsv(
        file_from_om,
        rows_from_om,
        [
            "T (K)",
            "q (-)",
            "O/M (-)",
            "pO2/p_ref (-)",
            "pO2 (MPa)",
            "a_O (-)",
            "oxygen potential (kJ/mol O2)",
            "status",
        ],
    )

    write_tsv(
        file_from_pressure,
        rows_from_pressure,
        [
            "T (K)",
            "q (-)",
            "pO2 (MPa)",
            "pO2/p_ref (-)",
            "a_O (-)",
            "O/M (-)",
            "oxygen potential (kJ/mol O2)",
            "status",
        ],
    )

    failed = sum(1 for row in rows_from_om if row["status"] != "ok")
    print(f"Wrote {len(rows_from_om)} rows: {file_from_om} ({failed} out-of-range points)")
    print(f"Wrote {len(rows_from_pressure)} rows: {file_from_pressure}")


if __name__ == "__main__":
    main()
