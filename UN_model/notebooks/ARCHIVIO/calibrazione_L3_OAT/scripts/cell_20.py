# ============================================================
# 12test — Monte Resegone timestep-convergence helper
# ============================================================

from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

MONTE_RESEGONE_CASE_ID = "AP3.4"
MONTE_RESEGONE_BURNUP = EXPERIMENT_CASES[MONTE_RESEGONE_CASE_ID]["burnup"]
MONTE_RESEGONE_T_MIN = 900.0
MONTE_RESEGONE_T_MAX = 1800.0
MONTE_RESEGONE_T_STEP = 25.0
MONTE_RESEGONE_DT_H = [10.0, 5.0, 1.0]
MONTE_RESEGONE_N_MODES = 40

def monte_resegone_temperature_grid():
    n = int(round((MONTE_RESEGONE_T_MAX - MONTE_RESEGONE_T_MIN) / MONTE_RESEGONE_T_STEP))
    return [MONTE_RESEGONE_T_MIN + i * MONTE_RESEGONE_T_STEP for i in range(n + 1)]

def run_monte_resegone_dt_sweep():
    """
    Keep the physical model fixed and vary only dt.
    Returns a dataframe and saves:
      - CSV with all final-state diagnostics;
      - FGR(T) comparison;
      - grain-face coverage Fc(T) comparison;
      - grain-face inventory(T) comparison.
    """
    ensure_output_dir()
    cand = candidate_for_experiment(make_candidate(), MONTE_RESEGONE_CASE_ID)
    Ts = monte_resegone_temperature_grid()
    records = []

    # Avoid carrying cached results from a different numerical setup.
    RUN_CACHE.clear()

    for dt_h in MONTE_RESEGONE_DT_H:
        print(
            f"Monte Resegone sweep: case={MONTE_RESEGONE_CASE_ID}, burnup={MONTE_RESEGONE_BURNUP:g}% FIMA, "
            f"dt={dt_h:g} h, n_modes={MONTE_RESEGONE_N_MODES}, "
            f"{len(Ts)} temperature points"
        )
        for T in Ts:
            r = run_model_point(
                T,
                MONTE_RESEGONE_BURNUP,
                cand,
                dt_h=dt_h,
                n_modes=MONTE_RESEGONE_N_MODES,
                keep_history=False,
            )
            records.append({
                "burnup_percent_FIMA": MONTE_RESEGONE_BURNUP,
                "T_K": float(T),
                "dt_h": float(dt_h),
                "n_modes": int(MONTE_RESEGONE_N_MODES),
                "release_gas_percent": r.get("release_gas_percent", np.nan),
                "grainface_gas_percent": r.get("grainface_gas_percent", np.nan),
                "qgb_gas_percent": r.get("qgb_gas_percent", np.nan),
                "Fc_gf": r.get("Fc_gf", np.nan),
                "Ngf_areal": r.get("Ngf_areal", np.nan),
                "Rgf_nm": r.get("Rgf_nm", np.nan),
                "p_gf_over_eq": r.get("p_gf_over_eq", np.nan),
                "matrix_gas_percent": r.get("matrix_gas_percent", np.nan),
                "bulk_gas_percent": r.get("bulk_gas_percent", np.nan),
                "dislocation_gas_percent": r.get("dislocation_gas_percent", np.nan),
            })

    conv_df = pd.DataFrame(records)
    ensure_output_dir()
    outdir = Path(OUTPUT_DIR) / "diagnostic"
    outdir.mkdir(parents=True, exist_ok=True)

    bu_tag = str(MONTE_RESEGONE_BURNUP).replace(".", "p")
    csv_path = outdir / f"{CASE_LABEL}_monte_resegone_dt_sweep_BU{bu_tag}.csv"
    conv_df.to_csv(csv_path, index=False)

    # ---- FGR(T) ----
    plt.figure(figsize=(9.5, 6.0))
    for dt_h in MONTE_RESEGONE_DT_H:
        d = conv_df[conv_df["dt_h"] == dt_h].sort_values("T_K")
        plt.plot(
            d["T_K"],
            d["release_gas_percent"],
            marker="o",
            markersize=3,
            linewidth=1.2,
            label=f"dt = {dt_h:g} h",
        )
    plt.xlabel("Temperature [K]")
    plt.ylabel("Released gas / FGR [% of generated gas]")
    plt.grid(True, alpha=0.25)
    plt.legend()
    fgr_path = outdir / f"{CASE_LABEL}_monte_resegone_FGR_dt_convergence_BU{bu_tag}.png"
    plt.savefig(fgr_path, dpi=180, bbox_inches="tight")
    if SHOW_PLOTS:
        plt.show()
    else:
        plt.close()

    # ---- grain-face coverage(T) ----
    plt.figure(figsize=(9.5, 6.0))
    for dt_h in MONTE_RESEGONE_DT_H:
        d = conv_df[conv_df["dt_h"] == dt_h].sort_values("T_K")
        plt.plot(
            d["T_K"],
            d["Fc_gf"],
            marker="o",
            markersize=3,
            linewidth=1.2,
            label=f"dt = {dt_h:g} h",
        )
    plt.axhline(0.5, linestyle="--", linewidth=1.0, label="Fc,sat = 0.5")
    plt.xlabel("Temperature [K]")
    plt.ylabel("Grain-face coverage $F_c$ [-]")
    plt.grid(True, alpha=0.25)
    plt.legend()
    fc_path = outdir / f"{CASE_LABEL}_monte_resegone_Fc_dt_convergence_BU{bu_tag}.png"
    plt.savefig(fc_path, dpi=180, bbox_inches="tight")
    if SHOW_PLOTS:
        plt.show()
    else:
        plt.close()

    # ---- grain-face retained gas(T) ----
    plt.figure(figsize=(9.5, 6.0))
    for dt_h in MONTE_RESEGONE_DT_H:
        d = conv_df[conv_df["dt_h"] == dt_h].sort_values("T_K")
        plt.plot(
            d["T_K"],
            d["grainface_gas_percent"],
            marker="o",
            markersize=3,
            linewidth=1.2,
            label=f"dt = {dt_h:g} h",
        )
    plt.xlabel("Temperature [K]")
    plt.ylabel("Gas retained in grain-face bubbles [%]")
    plt.grid(True, alpha=0.25)
    plt.legend()
    qgf_path = outdir / f"{CASE_LABEL}_monte_resegone_grainface_dt_convergence_BU{bu_tag}.png"
    plt.savefig(qgf_path, dpi=180, bbox_inches="tight")
    if SHOW_PLOTS:
        plt.show()
    else:
        plt.close()

    print(f"\nMonte Resegone CSV: {csv_path}")
    print(f"FGR convergence plot: {fgr_path}")
    print(f"Coverage convergence plot: {fc_path}")
    print(f"Grain-face gas plot: {qgf_path}")

    return conv_df, [fgr_path, fc_path, qgf_path]
