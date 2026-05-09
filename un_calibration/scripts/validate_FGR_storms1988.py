"""Validate model FGR predictions against the Storms 1988 dataset.

Source: Storms 1988 Table 1 — 134 numbered rows of UN and U-Pu-N pin
irradiation tests with measured fission gas release (FGR), end-of-life
burnup and three model-derived fuel temperatures (Ross / Baars / Thomas).
We use T_Thomas because it is the only column populated for all rows.

Runs the UN-calibrated model at each row's (T, burnup) and compares the
predicted FGR with the measured value. Produces:

  - scatter plot predicted vs measured (log-log) per material
  - distribution of residuals (predicted − measured)
  - RMSE and bias per material, and split by Storms's "excluded from fit"
    flag (asterisk marker in the original table)

Caveats encoded in the script:
  - Fission rate is not in the Storms dataset. We assume the model default
    F = 5×10¹⁹ fiss/m³/s (from MANUAL_PARAMS).
  - Theoretical-density variability (TD = 80-98 % across the dataset) is
    NOT propagated into the model — the run uses Rizk's nominal density.
  - Linear power, cladding, gap conditions are unknown from Storms Table 1.
  - U-Pu-N rows are run but the model was calibrated for UN; the (U,Pu)N
    comparison should be read as a sanity check, not a pass/fail criterion.

Output:
    un_calibration/reports/storms1988/validate_FGR_storms1988.csv
    un_calibration/reports/storms1988/validate_FGR_storms1988.png
"""

import sys
import pathlib
import math

ROOT = pathlib.Path(__file__).resolve().parents[1]
for sub in ("model", "config"):
    sys.path.insert(0, str(ROOT / sub))

import csv                                              # noqa: E402
import numpy as np                                      # noqa: E402
import matplotlib.pyplot as plt                         # noqa: E402

import un_data_storms as storms                         # noqa: E402
from builder import model_runner                        # noqa: E402

DT_HOURS = 12.0
N_MODES = 25

OUT_DIR = ROOT / "reports" / "storms1988"


def _rmse(residuals):
    if not residuals:
        return float("nan")
    return math.sqrt(sum(r * r for r in residuals) / len(residuals))


def _mean(xs):
    return sum(xs) / len(xs) if xs else float("nan")


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    df = storms.build_dataframe()

    # Run model at each row using T_Thomas (always populated).
    rows_out = []
    n = len(df)
    for i, row in df.iterrows():
        T = float(row["T_Thomas"])
        bu = float(row["burnup"])      # paper says % FIMA
        out = model_runner(T, bu, dt_h=DT_HOURS, n_modes=N_MODES)
        fgr_model = out["fgr_percent"]
        rows_out.append({
            "id":           int(row["id"]),
            "designation":  row["designation"],
            "material":     row["material"],
            "T_Thomas":     T,
            "burnup":       bu,
            "TD":           float(row["TD"]),
            "fgr_meas":     float(row["release"]),
            "fgr_model":    float(fgr_model),
            "residual":     float(fgr_model - row["release"]),
            "log_ratio":    (math.log10(max(fgr_model, 1.0e-12) /
                                        max(row["release"], 1.0e-12))),
            "excluded":     bool(row["excluded_from_fit"]),
        })
        if (i + 1) % 25 == 0 or (i + 1) == n:
            print(f"  ... {i + 1}/{n}")

    # --- CSV ---
    csv_path = OUT_DIR / "validate_FGR_storms1988.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows_out[0].keys()))
        w.writeheader()
        for r in rows_out:
            w.writerow(r)
    print(f"  wrote {csv_path}")

    # --- Metrics ---
    def _metrics(rs, label):
        if not rs:
            return
        residuals = [r["residual"] for r in rs]
        log_ratios = [r["log_ratio"] for r in rs if not math.isinf(r["log_ratio"])]
        rmse = _rmse(residuals)
        bias = _mean(residuals)
        median_log_ratio = sorted(log_ratios)[len(log_ratios) // 2] if log_ratios else float("nan")
        print(f"  {label:32s} n={len(rs):3d}  RMSE={rmse:6.3f}%  "
              f"bias={bias:+6.3f}%  median log10(model/exp)={median_log_ratio:+5.2f}")

    print()
    print("--- Validation metrics (FGR % vs Storms 1988) ---")
    _metrics(rows_out, "all")
    _metrics([r for r in rows_out if r["material"] == "UN"], "UN")
    _metrics([r for r in rows_out if r["material"] == "U-Pu-N"], "U-Pu-N")
    _metrics([r for r in rows_out if r["material"] == "UN" and not r["excluded"]],
             "UN (used in Storms eq. 7)")
    _metrics([r for r in rows_out if r["material"] == "UN" and r["excluded"]],
             "UN (excluded by Storms)")

    # --- Figure ---
    fig, axes = plt.subplots(1, 2, figsize=(14, 6))

    # Panel 1: predicted vs measured (log-log)
    ax = axes[0]
    floor = 1.0e-3   # for log-log plotting
    ax.plot([floor, 100], [floor, 100], "--", color="grey", linewidth=1,
            label="1:1")
    for label, color, marker in [("UN", "tab:blue", "o"),
                                  ("U-Pu-N", "tab:red", "s")]:
        rs_in   = [r for r in rows_out if r["material"] == label and not r["excluded"]]
        rs_out  = [r for r in rows_out if r["material"] == label and r["excluded"]]
        if rs_in:
            ax.scatter([max(r["fgr_meas"], floor) for r in rs_in],
                       [max(r["fgr_model"], floor) for r in rs_in],
                       marker=marker, s=22, color=color, alpha=0.8,
                       edgecolor="black", linewidth=0.4,
                       label=f"{label} (kept)")
        if rs_out:
            ax.scatter([max(r["fgr_meas"], floor) for r in rs_out],
                       [max(r["fgr_model"], floor) for r in rs_out],
                       marker=marker, s=22, facecolor="white",
                       edgecolor=color, linewidth=0.8,
                       label=f"{label} (excluded by Storms)")
    ax.set_xscale("log"); ax.set_yscale("log")
    ax.set_xlim(floor, 100); ax.set_ylim(floor, 100)
    ax.set_xlabel("Measured FGR (%) — Storms 1988 Tab. 1")
    ax.set_ylabel("Model FGR (%)")
    ax.set_title("Predicted vs measured FGR (133 points)")
    ax.grid(alpha=0.3, which="both")
    ax.legend(fontsize=9, loc="upper left")

    # Panel 2: residuals vs T_Thomas
    ax = axes[1]
    for label, color, marker in [("UN", "tab:blue", "o"),
                                  ("U-Pu-N", "tab:red", "s")]:
        rs_lab = [r for r in rows_out if r["material"] == label]
        if rs_lab:
            ax.scatter([r["T_Thomas"] for r in rs_lab],
                       [r["residual"] for r in rs_lab],
                       marker=marker, s=22, color=color, alpha=0.7,
                       edgecolor="black", linewidth=0.4, label=label)
    ax.axhline(0.0, color="grey", linestyle="--", linewidth=1)
    ax.set_xlabel("T_Thomas (K)")
    ax.set_ylabel("Residual: model − measured (% FGR)")
    ax.set_title("Residuals vs temperature")
    ax.grid(alpha=0.3)
    ax.legend(fontsize=9, loc="upper left")

    fig.suptitle(
        "FGR validation against Storms 1988 (UN + U-Pu-N pin tests)\n"
        f"Model: f_n=3e-6, K_d=5e5, ρ_d=3e13, F=5e19, T from Storms T_Thomas",
        fontsize=11,
    )
    plt.tight_layout(rect=[0, 0, 1, 0.93])

    png_path = OUT_DIR / "validate_FGR_storms1988.png"
    plt.savefig(png_path, dpi=140, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)


if __name__ == "__main__":
    run()
