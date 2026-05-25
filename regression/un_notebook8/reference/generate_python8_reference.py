#!/usr/bin/env python3
"""Generate notebook-8 reference points with SCIANTIX numerical settings."""

from __future__ import annotations

import csv
import json
import math
import sys
import types
from pathlib import Path


REFERENCE_DIR = Path(__file__).resolve().parent
SUITE_DIR = REFERENCE_DIR.parent
REPO_ROOT = SUITE_DIR.parents[1]
NOTEBOOK_PATH = REPO_ROOT / "UN_model" / "notebooks" / "8test_UN_intergranular.ipynb"
OUTPUT_CSV = REFERENCE_DIR / "python8_reference_points.csv"

sys.path.insert(0, str(SUITE_DIR))
from run_un_notebook8 import FISSION_RATE, GRAIN_RADIUS, LATTICE_PARAMETER, POINT_CASES, U_ATOM_DENSITY  # noqa: E402


FIELDS = [
    "case",
    "T_K",
    "target_FIMA_percent",
    "FIMA (%)",
    "Intragranular bulk gas bubble swelling (/)",
    "Dislocation gas bubble swelling (/)",
    "Intragranular gas bubble swelling (/)",
    "Dislocation bubble radius (m)",
    "Dislocation bubble concentration (bub/m3)",
    "Dislocation bubble pressure (MPa)",
    "Dislocation bubble equilibrium pressure (MPa)",
    "UN grain-face gas (at/m3)",
    "UN released gas (at/m3)",
    "UN fission gas release (/)",
    "Grain-face fractional coverage (/)",
    "UN bulk nucleation rate (bub/m3/s)",
]


def load_notebook8_namespace() -> dict[str, object]:
    notebook = json.loads(NOTEBOOK_PATH.read_text(encoding="utf-8"))
    module_name = "notebook8_reference"
    sys.modules[module_name] = types.ModuleType(module_name)
    namespace = sys.modules[module_name].__dict__
    for cell_index in (1, 7):
        source = "".join(notebook["cells"][cell_index]["source"])
        exec(compile(source, str(NOTEBOOK_PATH), "exec"), namespace)
    return namespace


def smooth_rho_d(namespace: dict[str, object], temperature: float, burnup_percent: float) -> float:
    fima = max(float(burnup_percent), 0.0)
    temperature = float(temperature)
    rho_fab = float(namespace["RHO_RIZK2023_SMOOTH_RHO_FAB"])
    rho_amp = float(namespace["RHO_RIZK2023_SMOOTH_RHO_AMP"])
    rho_scale = float(namespace["RHO_RIZK2023_SMOOTH_SCALE"])
    fima_scale = max(float(namespace["RHO_RIZK2023_SMOOTH_FC_PERCENT"]), 1.0e-12)
    temperature_half = float(namespace["RHO_RIZK2023_SMOOTH_T_HALF"])
    width = max(float(namespace["RHO_RIZK2023_SMOOTH_WIDTH"]), 1.0e-12)
    high_temperature_floor = min(max(float(namespace["RHO_RIZK2023_SMOOTH_F_MIN"]), 0.0), 1.0)
    rho_cap = float(namespace["RHO_RIZK2023_SMOOTH_CAP"])

    logistic_argument = (temperature - temperature_half) / width
    if logistic_argument > 80.0:
        logistic_decrease = 0.0
    elif logistic_argument < -80.0:
        logistic_decrease = 1.0
    else:
        logistic_decrease = 1.0 / (1.0 + math.exp(logistic_argument))

    burnup_part = 1.0 - math.exp(-fima / fima_scale)
    temperature_part = high_temperature_floor + (1.0 - high_temperature_floor) * logistic_decrease
    rho = rho_fab + rho_scale * rho_amp * burnup_part * temperature_part
    return min(max(rho, 1.0e10), rho_cap)


def final_time_h(fima_percent: float) -> float:
    return fima_percent * U_ATOM_DENSITY / (FISSION_RATE * 3.6e5)


def run_reference_point(namespace: dict[str, object], case: str, temperature: float, fima_percent: float) -> dict[str, str]:
    candidate = namespace["Candidate"](label=case, **dict(namespace["MANUAL_PARAMS"]))
    parameters = namespace["UNParameters"](
        temperature=float(temperature),
        fission_rate=candidate.fission_rate,
        grain_radius=GRAIN_RADIUS,
        target_burnup_percent_fima=float(fima_percent),
        dt=final_time_h(fima_percent) * 3600.0 / 100.0,
        n_modes=40,
        xe_yield=namespace["XE_YIELD"],
        f_n=candidate.f_n,
        K_d=candidate.K_d,
        rho_d=smooth_rho_d(namespace, temperature, fima_percent),
        Dv_scale=candidate.Dv_scale,
        Dv_D1_scale=candidate.Dv_D1_scale,
        Dv_D2_scale=candidate.Dv_D2_scale,
        Dv_dislocation_scale=candidate.Dv_dislocation_scale,
        vacancy_diffusivity_mode=namespace["VU_DIFFUSIVITY_MODE"],
        A20_vU=namespace["A20_VU_DEFAULT"],
        B21_vU=namespace["B21_VU"],
        B22_vU=namespace["B22_VU"],
        Dg_scale=candidate.Dg_scale,
        Dg_D1_scale=candidate.Dg_D1_scale,
        Dg_D3_scale=candidate.Dg_D3_scale,
        Dg_dislocation_scale=candidate.Dg_dislocation_scale,
        D2_xe_scale=candidate.D2_xe_scale,
        xe_diffusivity_mode=namespace["XE_DIFFUSIVITY_MODE"],
        b_scale=candidate.b_scale,
        b_bulk_scale=candidate.b_bulk_scale,
        b_dislocation_scale=candidate.b_dislocation_scale,
        gb_scale=candidate.gb_scale,
        gd_scale=candidate.gd_scale,
        gd_bubble_scale=candidate.gd_bubble_scale,
        gd_line_scale=candidate.gd_line_scale,
        gd_line_alpha=candidate.gd_line_alpha,
        coalescence_d_scale=candidate.coalescence_d_scale,
        capture_scale=candidate.capture_scale,
    )
    history, _ = namespace["solve_UN"](parameters, keep_history=False)
    generated = history["generated"][-1]
    released = history.get("q_rel", [0.0])[-1]

    values = {
        "case": case,
        "T_K": f"{temperature:g}",
        "target_FIMA_percent": f"{fima_percent:g}",
        "FIMA (%)": f"{fima_percent:.16g}",
        "Intragranular bulk gas bubble swelling (/)": history["swelling_b"][-1],
        "Dislocation gas bubble swelling (/)": history["swelling_d"][-1],
        "Intragranular gas bubble swelling (/)": history["swelling_ig"][-1],
        "Dislocation bubble radius (m)": history["Rd"][-1],
        "Dislocation bubble concentration (bub/m3)": history["Nd"][-1],
        "Dislocation bubble pressure (MPa)": history["p_d"][-1] / 1.0e6,
        "Dislocation bubble equilibrium pressure (MPa)": history["p_d_eq"][-1] / 1.0e6,
        "UN grain-face gas (at/m3)": history.get("q_gf", [0.0])[-1],
        "UN released gas (at/m3)": released,
        "UN fission gas release (/)": released / generated if generated > 0.0 else 0.0,
        "Grain-face fractional coverage (/)": history.get("Fc_gf", [0.0])[-1],
        "UN bulk nucleation rate (bub/m3/s)": history["nu_b"][-1],
    }
    return {key: values[key] if isinstance(values[key], str) else f"{values[key]:.16e}" for key in FIELDS}


def main() -> int:
    namespace = load_notebook8_namespace()
    if int(namespace["N_MODES"]) != 22:
        raise RuntimeError("Unexpected notebook-8 default N_MODES; update generator assumptions.")
    if float(namespace["DT_H"]) != 12.0:
        raise RuntimeError("Unexpected notebook-8 default DT_H; update generator assumptions.")
    if float(namespace["LATTICE_PARAMETER"]) != LATTICE_PARAMETER:
        raise RuntimeError("Notebook and regression lattice parameters differ.")

    rows = [run_reference_point(namespace, case, temperature, fima) for case, temperature, fima in POINT_CASES]
    with OUTPUT_CSV.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {OUTPUT_CSV.relative_to(REPO_ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
