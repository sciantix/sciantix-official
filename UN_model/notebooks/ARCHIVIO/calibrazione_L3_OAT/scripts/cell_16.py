
# ============================================================
# POST-PROCESSING GUARD DIAGNOSTICS
# This cell does NOT modify the solver. It only analyses rows/hist.
# ============================================================

import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path

def _finite_positive(x):
    try:
        return math.isfinite(float(x)) and float(x) > 0.0
    except Exception:
        return False

def _safe_float(x, default=math.nan):
    try:
        x = float(x)
        return x if math.isfinite(x) else default
    except Exception:
        return default

def _xi_from_row(r, pop):
    # In this notebook porosity_b/d is exactly V*N for each population.
    key = "porosity_b" if pop == "b" else "porosity_d"
    return _safe_float(r.get(key, math.nan))

def _psi_from_row(r, pop):
    key = "psi_b" if pop == "b" else "psi_d"
    return _safe_float(r.get(key, math.nan))

def _lambda_clip_value():
    # coalescence_lambda clips xi to 0.999999.
    xi = 0.999999
    return (2.0 - xi) / (2.0 * (1.0 - xi) ** 3)

def _zeta_from_psi_for_plot(psi):
    # Original analytic expression, with NaN outside the physical domain psi < 1.
    # This is only for plotting; it does not touch solver behavior.
    psi = np.asarray(psi, dtype=float)
    z = np.full_like(psi, np.nan, dtype=float)
    mask = np.isfinite(psi) & (psi > 0.0) & (psi < 1.0)
    den = -psi[mask]**6 + 5.0 * psi[mask]**2 - 9.0 * psi[mask] + 5.0
    good = den > 0.0
    vals = np.full(mask.sum(), np.nan)
    vals[good] = 10.0 * psi[mask][good] * (1.0 + psi[mask][good]**3) / den[good]
    z[mask] = vals
    return z

def guard_dataframe_from_rows(rows):
    """Build a dataframe with final-state guard indicators for each T/burnup point."""
    records = []
    lam_clip = _lambda_clip_value()
    for r in rows:
        psi_b = _psi_from_row(r, "b")
        psi_d = _psi_from_row(r, "d")
        xi_b = _xi_from_row(r, "b")
        xi_d = _xi_from_row(r, "d")
        lam = _safe_float(r.get("lambda_d", math.nan))

        fcap_max = _safe_float(r.get("max_f_cap_step", math.nan))
        cap_raw_sum = _safe_float(r.get("capture_raw_sum", math.nan))
        cap_clip_sum = _safe_float(r.get("capture_fraction_sum", math.nan))

        p_b_eq = _safe_float(r.get("p_b_eq", math.nan))
        p_d_eq = _safe_float(r.get("p_d_eq", math.nan))
        rb_nm = _safe_float(r.get("Rb_nm", math.nan))
        rd_nm = _safe_float(r.get("Rd_nm", math.nan))
        nb = _safe_float(r.get("Nb", math.nan))
        nd = _safe_float(r.get("Nd", math.nan))

        rec = dict(r)
        rec.update({
            "xi_b": xi_b,
            "xi_d": xi_d,
            "zeta_b_final": float(_zeta_from_psi_for_plot([psi_b])[0]) if math.isfinite(psi_b) else math.nan,
            "zeta_d_final": float(_zeta_from_psi_for_plot([psi_d])[0]) if math.isfinite(psi_d) else math.nan,

            # Guard/invalid indicators inferred from final values.
            "guard_fcap_clipped": bool(math.isfinite(fcap_max) and fcap_max >= 1.0 - 1e-12),
            "guard_capture_raw_exceeded_clipped_sum": bool(
                math.isfinite(cap_raw_sum) and math.isfinite(cap_clip_sum) and cap_raw_sum > cap_clip_sum + 1e-12
            ),
            "guard_psi_b_ge_1": bool(math.isfinite(psi_b) and psi_b >= 1.0),
            "guard_psi_d_ge_1": bool(math.isfinite(psi_d) and psi_d >= 1.0),
            "guard_xi_b_ge_1": bool(math.isfinite(xi_b) and xi_b >= 1.0),
            "guard_xi_d_ge_1": bool(math.isfinite(xi_d) and xi_d >= 1.0),
            "near_xi_b_08": bool(math.isfinite(xi_b) and xi_b >= 0.8),
            "near_xi_d_08": bool(math.isfinite(xi_d) and xi_d >= 0.8),
            "near_psi_b_08": bool(math.isfinite(psi_b) and psi_b >= 0.8),
            "near_psi_d_08": bool(math.isfinite(psi_d) and psi_d >= 0.8),
            "guard_lambda_near_clip": bool(math.isfinite(lam) and lam >= 0.01 * lam_clip),
            "guard_pressure_floor_b": bool(math.isfinite(p_b_eq) and p_b_eq > 1e14),
            "guard_pressure_floor_d": bool(math.isfinite(p_d_eq) and p_d_eq > 1e14),
            "guard_nonfinite_R_or_N": bool(
                not math.isfinite(rb_nm) or not math.isfinite(rd_nm) or not math.isfinite(nb) or not math.isfinite(nd)
            ),
            "invalid_single_size_final": bool(not r.get("valid_single_size", False)),
        })
        records.append(rec)
    return pd.DataFrame(records)

def _save_guard_plot(name):
    path = output_path_for(name)
    plt.savefig(path, dpi=180, bbox_inches="tight")
    if not SHOW_PLOTS:
        plt.close()
    return path

def plot_guard_geometry_summary(rows, cand=None):
    """For each burnup, plot psi, xi, lambda and zeta final values vs T."""
    gdf = guard_dataframe_from_rows(rows)
    label = cand.label if cand is not None else CASE_LABEL
    saved = []

    for bu in BURNUPS:
        sub = gdf[np.isclose(gdf["burnup"], float(bu))].sort_values("T")
        if sub.empty:
            continue
        Ts = sub["T"].to_numpy()

        fig, axes = plt.subplots(2, 2, figsize=(13, 8.5))

        ax = axes[0, 0]
        ax.plot(Ts, sub["psi_b"], "--", label=r"$\psi_b=R_b/\delta_b$")
        ax.plot(Ts, sub["psi_d"], label=r"$\psi_d=R_d/\delta_d$")
        ax.axhline(0.8, linestyle=":", linewidth=1, label=r"$\psi=0.8$")
        ax.axhline(1.0, linestyle="--", linewidth=1, label=r"$\psi=1$ guard")
        ax.set_ylabel(r"$\psi$ [-]")
        ax.set_title(f"{label}: psi at {bu:.1f}% FIMA")
        ax.grid(True, alpha=0.3)
        ax.legend(fontsize=8)

        ax = axes[0, 1]
        ax.plot(Ts, sub["xi_b"], "--", label=r"$\xi_b=V_bN_b$")
        ax.plot(Ts, sub["xi_d"], label=r"$\xi_d=V_dN_d$")
        ax.axhline(0.5, linestyle=":", linewidth=1, label=r"$\xi=0.5$")
        ax.axhline(1.0, linestyle="--", linewidth=1, label=r"$\xi=1$ invalid")
        ax.set_ylabel(r"$\xi$ [-]")
        ax.set_title(f"{label}: porosity/volume fraction at {bu:.1f}% FIMA")
        ax.grid(True, alpha=0.3)
        ax.legend(fontsize=8)

        ax = axes[1, 0]
        ax.semilogy(Ts, np.maximum(sub["lambda_d"].to_numpy(dtype=float), 1e-300), label=r"$\lambda_d$")
        ax.axhline(_lambda_clip_value(), linestyle="--", linewidth=1, label=r"$\lambda(\xi=0.999999)$")
        ax.set_xlabel("Temperature [K]")
        ax.set_ylabel(r"$\lambda_d$ [-]")
        ax.set_title(f"{label}: coalescence factor at {bu:.1f}% FIMA")
        ax.grid(True, which="both", alpha=0.3)
        ax.legend(fontsize=8)

        ax = axes[1, 1]
        ax.semilogy(Ts, np.maximum(sub["zeta_b_final"].to_numpy(dtype=float), 1e-300), "--", label=r"$\zeta_b$ final, only if $\psi<1$")
        ax.semilogy(Ts, np.maximum(sub["zeta_d_final"].to_numpy(dtype=float), 1e-300), label=r"$\zeta_d$ final, only if $\psi<1$")
        ax.set_xlabel("Temperature [K]")
        ax.set_ylabel(r"$\zeta$ [-]")
        ax.set_title(f"{label}: zeta geometry factor at {bu:.1f}% FIMA")
        ax.grid(True, which="both", alpha=0.3)
        ax.legend(fontsize=8)

        plt.tight_layout()
        saved.append(_save_guard_plot(f"{label}_guard_geometry_{bu:.1f}FIMA.png"))

    return saved

def plot_guard_activation_summary(rows, cand=None):
    """For each burnup, plot guard indicators vs T."""
    gdf = guard_dataframe_from_rows(rows)
    label = cand.label if cand is not None else CASE_LABEL
    saved = []

    flag_cols = [
        ("guard_fcap_clipped", "f_cap clipped to 1"),
        ("guard_capture_raw_exceeded_clipped_sum", "raw capture > clipped sum"),
        ("near_psi_d_08", "psi_d >= 0.8"),
        ("guard_psi_d_ge_1", "psi_d >= 1"),
        ("near_xi_d_08", "xi_d >= 0.8"),
        ("guard_xi_d_ge_1", "xi_d >= 1"),
        ("guard_lambda_near_clip", "lambda near xi clip"),
        ("guard_pressure_floor_b", "bulk p_eq floor"),
        ("guard_pressure_floor_d", "disl p_eq floor"),
        ("guard_nonfinite_R_or_N", "nonfinite R/N"),
        ("invalid_single_size_final", "invalid single-size flag"),
    ]

    for bu in BURNUPS:
        sub = gdf[np.isclose(gdf["burnup"], float(bu))].sort_values("T")
        if sub.empty:
            continue
        Ts = sub["T"].to_numpy()

        plt.figure(figsize=(11, 6.5))
        offset = 0
        for col, lab in flag_cols:
            y = sub[col].astype(float).to_numpy()
            # Offset each flag vertically so all activations are visible.
            plt.step(Ts, y + offset, where="mid", label=lab)
            offset += 1.25

        plt.yticks([])
        plt.xlabel("Temperature [K]")
        plt.grid(True, axis="x", alpha=0.3)
        plt.legend(fontsize=8, loc="center left", bbox_to_anchor=(1.02, 0.5))
        saved.append(_save_guard_plot(f"{label}_guard_activation_flags_{bu:.1f}FIMA.png"))

    return saved

def plot_vacancy_absorption_summary(rows, cand=None):
    """For each burnup, plot final vacancy absorption diagnostics vs T."""
    gdf = guard_dataframe_from_rows(rows)
    label = cand.label if cand is not None else CASE_LABEL
    saved = []

    for bu in BURNUPS:
        sub = gdf[np.isclose(gdf["burnup"], float(bu))].sort_values("T")
        if sub.empty:
            continue
        Ts = sub["T"].to_numpy()

        fig, axes = plt.subplots(2, 2, figsize=(13, 8.5))

        ax = axes[0, 0]
        ax.semilogy(Ts, np.maximum(sub.get("Dv", pd.Series(np.nan, index=sub.index)).to_numpy(dtype=float), 1e-300), label=r"$D_v$ bulk total")
        if "Dv1" in sub.columns:
            ax.semilogy(Ts, np.maximum(sub["Dv1"].to_numpy(dtype=float), 1e-300), "--", label=(r"$D_{v,1}$ thermal diagnostic" if VU_DIFFUSIVITY_MODE == "matthews2025_DU_fig42_lookup" else r"$D_{v,1}$ bulk thermal (Rizk baseline)"))
        if "Dv2" in sub.columns:
            ax.semilogy(Ts, np.maximum(sub["Dv2"].to_numpy(dtype=float), 1e-300), "--", label=r"$D_{v,2}$ bulk irradiation")
        dv_matthews_thermal = np.array([3.25e-4 * math.exp(-5.95 / (8.617333262145e-5 * float(T))) for T in Ts], dtype=float)
        ax.semilogy(Ts, np.maximum(dv_matthews_thermal, 1e-300), label=r"$D_v$ thermal bulk (Matthews 2025)")
        if "Dv_gb" in sub.columns:
            ax.semilogy(Ts, np.maximum(sub["Dv_gb"].to_numpy(dtype=float), 1e-300), "-.", label=r"$D_v^{gb}=10^6D_{v,1}$")
        elif "Dv1" in sub.columns:
            ax.semilogy(Ts, np.maximum((float(DV_GB_MULTIPLIER) * sub["Dv1"]).to_numpy(dtype=float), 1e-300), "-.", label=r"$D_v^{gb}=10^6D_{v,1}$")
        ax.set_ylabel(r"$D_v$ [m$^2$/s]")
        ax.set_title(f"{label}: vacancy diffusivity at {bu:.1f}% FIMA")
        ax.grid(True, which="both", alpha=0.3)
        ax.legend(fontsize=8)

        ax = axes[0, 1]
        if "dnvb_dt" in sub.columns:
            ax.semilogy(Ts, np.maximum(np.abs(sub["dnvb_dt"].to_numpy(dtype=float)), 1e-300), "--", label=r"$|dn_{v,b}/dt|$")
        if "dnvd_dt" in sub.columns:
            ax.semilogy(Ts, np.maximum(np.abs(sub["dnvd_dt"].to_numpy(dtype=float)), 1e-300), label=r"$|dn_{v,d}/dt|$")
        ax.set_ylabel(r"Vacancy absorption rate [vac m$^{-3}$ s$^{-1}$]")
        ax.set_title(f"{label}: final vacancy absorption rate at {bu:.1f}% FIMA")
        ax.grid(True, which="both", alpha=0.3)
        ax.legend(fontsize=8)

        ax = axes[1, 0]
        ax.semilogy(Ts, np.maximum(sub.get("p_b_over_eq", pd.Series(np.nan, index=sub.index)).to_numpy(dtype=float), 1e-300), "--", label=r"$p_b/p_{b,eq}$")
        ax.semilogy(Ts, np.maximum(sub.get("p_d_over_eq", pd.Series(np.nan, index=sub.index)).to_numpy(dtype=float), 1e-300), label=r"$p_d/p_{d,eq}$")
        ax.axhline(1.0, linestyle="--", linewidth=1)
        ax.set_xlabel("Temperature [K]")
        ax.set_ylabel("Pressure ratio [-]")
        ax.set_title(f"{label}: pressure driving force at {bu:.1f}% FIMA")
        ax.grid(True, which="both", alpha=0.3)
        ax.legend(fontsize=8)

        ax = axes[1, 1]
        ax.semilogy(Ts, np.maximum(sub.get("max_f_cap_step", pd.Series(np.nan, index=sub.index)).to_numpy(dtype=float), 1e-300), label="max f_cap step")
        ax.semilogy(Ts, np.maximum(sub.get("capture_raw_sum", pd.Series(np.nan, index=sub.index)).to_numpy(dtype=float), 1e-300), "--", label="sum raw capture hazard")
        ax.axhline(1.0, linestyle="--", linewidth=1)
        ax.set_xlabel("Temperature [K]")
        ax.set_ylabel("Capture diagnostic")
        ax.set_title(f"{label}: capture vs vacancy-growth consequences at {bu:.1f}% FIMA")
        ax.grid(True, which="both", alpha=0.3)
        ax.legend(fontsize=8)

        plt.tight_layout()
        saved.append(_save_guard_plot(f"{label}_vacancy_absorption_guard_summary_{bu:.1f}FIMA.png"))

    return saved

def make_guard_diagnostic_plots(rows, cand=None):
    """Create all final-state guard diagnostics."""
    if cand is None:
        cand = make_candidate()
    saved = []
    saved += plot_guard_geometry_summary(rows, cand)
    saved += plot_guard_activation_summary(rows, cand)
    saved += plot_vacancy_absorption_summary(rows, cand)

    gdf = guard_dataframe_from_rows(rows)
    out_csv = output_path_for(f"{cand.label}_guard_diagnostics_final_state.csv")
    gdf.to_csv(out_csv, index=False)
    saved.append(out_csv)
    print("Guard diagnostic outputs:")
    for pth in saved:
        print(f"  - {pth}")
    return gdf, saved

def history_dataframe_for_point(T, burnup, cand=None):
    """Rerun one point with keep_history=True and build internal time-history diagnostics."""
    if cand is None:
        cand = make_candidate()
    out = run_model_point(float(T), float(burnup), cand, DT_H, N_MODES, keep_history=True)
    hist = out["hist"]
    hdf = pd.DataFrame({k: v for k, v in hist.items() if isinstance(v, list)})
    if hdf.empty:
        return hdf, out

    # Geometry from internal history.
    hdf["psi_b"] = [
        (R / wigner_seitz_delta(N) if _finite_positive(N) and math.isfinite(R) else math.nan)
        for R, N in zip(hdf["Rb"], hdf["Nb"])
    ]
    hdf["psi_d"] = [
        (R / wigner_seitz_delta(N) if _finite_positive(N) and math.isfinite(R) else math.nan)
        for R, N in zip(hdf["Rd"], hdf["Nd"])
    ]
    hdf["xi_b"] = hdf["swelling_b"]
    hdf["xi_d"] = hdf["swelling_d"]
    hdf["zeta_b"] = _zeta_from_psi_for_plot(hdf["psi_b"].to_numpy(dtype=float))
    hdf["zeta_d"] = _zeta_from_psi_for_plot(hdf["psi_d"].to_numpy(dtype=float))

    # Finite-difference vacancy absorption rates from history.
    time = hdf["time"].to_numpy(dtype=float)
    for key in ["nvb", "nvd"]:
        vals = hdf[key].to_numpy(dtype=float)
        rate = np.full_like(vals, np.nan, dtype=float)
        if len(vals) > 1:
            dt = np.diff(time)
            dv = np.diff(vals)
            ok = dt > 0
            rate[1:][ok] = dv[ok] / dt[ok]
        hdf[f"d{key}_dt_fd"] = rate

    # Inferred guard flags.
    hdf["guard_fcap_clipped"] = hdf["f_cap_step"] >= 1.0 - 1e-12
    hdf["guard_psi_b_ge_1"] = hdf["psi_b"] >= 1.0
    hdf["guard_psi_d_ge_1"] = hdf["psi_d"] >= 1.0
    hdf["near_psi_d_08"] = hdf["psi_d"] >= 0.8
    hdf["near_xi_d_08"] = hdf["xi_d"] >= 0.8
    hdf["guard_xi_d_ge_1"] = hdf["xi_d"] >= 1.0
    hdf["guard_pressure_floor_b"] = hdf["p_b_eq"] > 1e14
    hdf["guard_pressure_floor_d"] = hdf["p_d_eq"] > 1e14

    return hdf, out

def plot_guard_history(T, burnup, cand=None):
    """Detailed internal history plots for one temperature/burnup point."""
    if cand is None:
        cand = make_candidate()
    hdf, out = history_dataframe_for_point(T, burnup, cand)
    if hdf.empty:
        print("Empty history")
        return hdf, []

    label = cand.label
    suffix = f"{label}_history_guards_{float(burnup):.1f}FIMA_{float(T):.0f}K"
    saved = []
    x = hdf["burnup_percent_fima"].to_numpy(dtype=float)

    # Plot geometry and coalescence.
    fig, axes = plt.subplots(2, 2, figsize=(13, 8.5))
    ax = axes[0, 0]
    ax.plot(x, hdf["psi_b"], "--", label=r"$\psi_b$")
    ax.plot(x, hdf["psi_d"], label=r"$\psi_d$")
    ax.axhline(0.8, linestyle=":", linewidth=1)
    ax.axhline(1.0, linestyle="--", linewidth=1)
    ax.set_ylabel(r"$\psi$ [-]")
    ax.set_title(f"{label}: psi history, T={T:.0f} K, burnup={burnup:.1f}%")
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=8)

    ax = axes[0, 1]
    ax.plot(x, hdf["xi_b"], "--", label=r"$\xi_b$")
    ax.plot(x, hdf["xi_d"], label=r"$\xi_d$")
    ax.axhline(0.8, linestyle=":", linewidth=1)
    ax.axhline(1.0, linestyle="--", linewidth=1)
    ax.set_ylabel(r"$\xi=VN$ [-]")
    ax.set_title("Porosity / volume fraction")
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=8)

    ax = axes[1, 0]
    ax.semilogy(x, np.maximum(hdf["lambda_d"].to_numpy(dtype=float), 1e-300), label=r"$\lambda_d$")
    ax.axhline(_lambda_clip_value(), linestyle="--", linewidth=1, label=r"$\lambda(\xi=0.999999)$")
    ax.set_xlabel("Burnup [% FIMA]")
    ax.set_ylabel(r"$\lambda_d$ [-]")
    ax.grid(True, which="both", alpha=0.3)
    ax.legend(fontsize=8)

    ax = axes[1, 1]
    ax.semilogy(x, np.maximum(hdf["zeta_b"].to_numpy(dtype=float), 1e-300), "--", label=r"$\zeta_b$")
    ax.semilogy(x, np.maximum(hdf["zeta_d"].to_numpy(dtype=float), 1e-300), label=r"$\zeta_d$")
    ax.set_xlabel("Burnup [% FIMA]")
    ax.set_ylabel(r"$\zeta$ [-]")
    ax.grid(True, which="both", alpha=0.3)
    ax.legend(fontsize=8)

    plt.tight_layout()
    saved.append(_save_guard_plot(f"{suffix}_geometry.png"))

    # Plot capture and vacancy absorption.
    fig, axes = plt.subplots(2, 2, figsize=(13, 8.5))
    ax = axes[0, 0]
    ax.semilogy(x, np.maximum(hdf["f_cap_step"].to_numpy(dtype=float), 1e-300), label="f_cap step")
    ax.semilogy(x, np.maximum(hdf["cap_raw_step"].to_numpy(dtype=float), 1e-300), "--", label="cap raw step")
    ax.axhline(1.0, linestyle="--", linewidth=1)
    ax.set_ylabel("Capture")
    ax.set_title("Capture clipping history")
    ax.grid(True, which="both", alpha=0.3)
    ax.legend(fontsize=8)

    ax = axes[0, 1]
    ax.semilogy(x, np.maximum(np.abs(hdf["dnvb_dt_fd"].to_numpy(dtype=float)), 1e-300), "--", label=r"$|dn_{v,b}/dt|$ finite diff")
    ax.semilogy(x, np.maximum(np.abs(hdf["dnvd_dt_fd"].to_numpy(dtype=float)), 1e-300), label=r"$|dn_{v,d}/dt|$ finite diff")
    ax.set_ylabel(r"Vacancy absorption rate [vac m$^{-3}$ s$^{-1}$]")
    ax.set_title("Vacancy absorption history")
    ax.grid(True, which="both", alpha=0.3)
    ax.legend(fontsize=8)

    ax = axes[1, 0]
    pb_ratio = hdf["p_b"] / hdf["p_b_eq"].replace(0, np.nan)
    pd_ratio = hdf["p_d"] / hdf["p_d_eq"].replace(0, np.nan)
    ax.semilogy(x, np.maximum(pb_ratio.to_numpy(dtype=float), 1e-300), "--", label=r"$p_b/p_{b,eq}$")
    ax.semilogy(x, np.maximum(pd_ratio.to_numpy(dtype=float), 1e-300), label=r"$p_d/p_{d,eq}$")
    ax.axhline(1.0, linestyle="--", linewidth=1)
    ax.set_xlabel("Burnup [% FIMA]")
    ax.set_ylabel("Pressure ratio [-]")
    ax.grid(True, which="both", alpha=0.3)
    ax.legend(fontsize=8)

    ax = axes[1, 1]
    flag_cols = [
        ("guard_fcap_clipped", "f_cap clipped"),
        ("near_psi_d_08", "psi_d >= 0.8"),
        ("guard_psi_d_ge_1", "psi_d >= 1"),
        ("near_xi_d_08", "xi_d >= 0.8"),
        ("guard_xi_d_ge_1", "xi_d >= 1"),
        ("guard_pressure_floor_b", "bulk p_eq floor"),
    ]
    off = 0
    for col, lab in flag_cols:
        ax.step(x, hdf[col].astype(float) + off, where="post", label=lab)
        off += 1.25
    ax.set_yticks([])
    ax.set_xlabel("Burnup [% FIMA]")
    ax.set_title("Guard/invalidity flags")
    ax.grid(True, axis="x", alpha=0.3)
    ax.legend(fontsize=7, loc="center left", bbox_to_anchor=(1.02, 0.5))

    plt.tight_layout()
    saved.append(_save_guard_plot(f"{suffix}_capture_vacancy_flags.png"))

    # Save history CSV.
    csv_path = output_path_for(f"{suffix}.csv")
    hdf.to_csv(csv_path, index=False)
    saved.append(csv_path)

    print(f"History guard outputs for T={T:g} K, burnup={burnup:g}% FIMA:")
    for pth in saved:
        print(f"  - {pth}")
    return hdf, saved
