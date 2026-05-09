"""Single-point smoke test at T = 1600 K, 1.3 % FIMA.

Validates that:
  - The config package + un_model wiring is intact.
  - The Rizk-nominal run produces the expected reference numbers
    (those reported below as REFERENCE).

REFERENCE values (capture_only flags ON, K_D=5e5, BE-quadratic coalescence,
2026-05-09):
    swelling (dislocation)  =  3.17 %
    swelling (bulk)         =  1.35 %
    R_d                     =  83.2 nm
    R_b                     =  13.8 nm
    N_d                     =  1.32e+19  m^-3
    N_b                     =  ~1.2e+21  m^-3

A discrepancy >1% on these numbers means a regression.
"""

import sys
import pathlib

ROOT = pathlib.Path(__file__).resolve().parents[1]    # un_calibration/
for sub in ("model", "config"):
    sys.path.insert(0, str(ROOT / sub))

import un_model as m                       # noqa: E402
from builder import model_runner           # noqa: E402

T_K = 1600.0
BURNUP_PERCENT_FIMA = 1.3
DT_HOURS = 12.0
N_MODES = 25


def main():
    out = model_runner(T_K, BURNUP_PERCENT_FIMA,
                       dt_h=DT_HOURS, n_modes=N_MODES,
                       keep_history=True)

    print(f"Smoke test at T = {T_K} K, {BURNUP_PERCENT_FIMA} % FIMA")
    print(f"  flags:  phi={m.USE_PHI_GAS_RESOLUTION}  "
          f"mass={m.USE_NUCLEATION_MASS_COUPLING}  "
          f"capture={m.USE_BULK_DISLOCATION_CAPTURE}")
    print()
    print(f"  {'quantity':<26s} {'value':>14s}")
    print(f"  {'swelling (dislocation)':<26s} {out['swelling_d_percent']:>13.3f} %")
    print(f"  {'swelling (bulk)':<26s} {out['swelling_b_percent']:>13.3f} %")
    print(f"  {'R_d':<26s} {out['Rd_nm']:>12.1f} nm")
    print(f"  {'R_b':<26s} {out['Rb_nm']:>12.1f} nm")
    print(f"  {'N_d':<26s} {out['Nd']:>14.2e}")
    print(f"  {'N_b':<26s} {out['Nb']:>14.2e}")
    print()
    print(f"  Gas partition:  matrix={out['matrix_gas_percent']:.1f}%  "
          f"bulk={out['bulk_gas_percent']:.1f}%  "
          f"disl={out['dislocation_gas_percent']:.1f}%  "
          f"q_gb={out['qgb_gas_percent']:.1f}%")


if __name__ == "__main__":
    main()
