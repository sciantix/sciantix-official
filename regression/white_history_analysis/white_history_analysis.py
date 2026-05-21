#!/usr/bin/env python3
"""COARSENING: exploratory White history-feature analysis for K_eff and bubble-family calibration."""

from __future__ import annotations

import csv
import math
from pathlib import Path


ROOT = Path(__file__).resolve().parent
REPO = ROOT.parent.parent
WHITE = REPO / "regression" / "white"
COARSENING = REPO / "regression" / "whiteCOARSENING_dev"
DATA = COARSENING / "data"
FIGURES = ROOT / "figures"


EXPECTED = {
    "exp_radius_m": ("ig_coarsening_radius.txt", 1.0e-9),
    "exp_density_bub_m3": ("ig_coarsening_density.txt", 1.0e19),
    "exp_swelling_percent": ("ig_coarsening_swelling.txt", 1.0),
}


FEATURE_LABELS = {
    "final_burnup": "Final burnup (MWd/kgUO2)",
    "grain_radius_um": "Final grain radius (um)",
    "max_temperature": "Maximum temperature (K)",
    "base_temperature": "Base temperature (K)",
    "temperature_excursion": "Temperature excursion (K)",
    "time_at_peak": "Peak-temperature holding time (history units)",
    "time_above_1200": "Time above 1200 K (history units)",
    "time_above_1500": "Time above 1500 K (history units)",
    "time_above_1800": "Time above 1800 K (history units)",
    "thermal_dose_1500": "Thermal dose above 1500 K (K history units)",
    "thermal_dose_1800": "Thermal dose above 1800 K (K history units)",
    "ramp_up_duration": "Heating-ramp duration (history units)",
    "ramp_down_duration": "Cooling-ramp duration (history units)",
    "max_heating_rate": "Maximum heating rate (K/history unit)",
    "max_cooling_rate": "Maximum cooling rate (K/history unit)",
    "peak_fission_rate": "Peak fission rate (fiss/m3/s)",
    "mean_fission_rate": "Time-weighted fission rate (fiss/m3/s)",
    "fluence_proxy": "Integral fission-rate proxy (fiss/m3)",
    "transient_fluence_proxy": "Transient integral fission-rate proxy (fiss/m3)",
    "min_hydrostatic_stress": "Minimum hydrostatic stress (MPa)",
    "dislocation_density": "Dislocation density (m/m3)",
    "inferred_k_exp": "Inferred K_eff = N_exp/rho_d (bub/m)",
    "inferred_spacing_nm": "Inferred spacing 1/K_eff (nm)",
}


TARGET_LABELS = {
    "exp_density_bub_m3": "Experimental bubble concentration (bub/m3)",
    "exp_radius_m": "Experimental bubble radius (m)",
    "exp_swelling_percent": "Experimental swelling (%)",
    "calc_density_bub_m3": "Calculated coarsened bubble concentration (bub/m3)",
    "calc_radius_m": "Calculated coarsened bubble radius (m)",
    "calc_swelling_percent": "Calculated coarsened swelling (%)",
    "density_ratio_calc_exp": "Calculated / experimental concentration",
    "radius_ratio_calc_exp": "Calculated / experimental radius",
    "swelling_ratio_calc_exp": "Calculated / experimental swelling",
    "inferred_k_exp": "Inferred K_eff = N_exp/rho_d (bub/m)",
}


def load_expected() -> dict[str, dict[str, float]]:
    # COARSENING: White experimental coarsened-bubble metrics from Barani/White tables.
    out: dict[str, dict[str, float]] = {}
    for key, (filename, factor) in EXPECTED.items():
        with (DATA / filename).open() as stream:
            for line in stream:
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                name, value = line.split()[:2]
                out.setdefault(name, {})[key] = float(value) * factor
    return out


def load_history(path: Path) -> list[dict[str, float]]:
    # COARSENING: input_history columns are time, temperature, fission rate, hydrostatic stress.
    rows: list[dict[str, float]] = []
    with path.open() as stream:
        for line in stream:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            values = [float(item) for item in line.split()[:4]]
            rows.append({
                "time": values[0],
                "temperature": values[1],
                "fission_rate": values[2],
                "stress": values[3],
            })
    return rows


def load_output(path: Path) -> tuple[list[str], list[float]]:
    lines = [line.strip() for line in path.read_text().splitlines() if line.strip()]
    header = [item.strip() for item in lines[0].split("\t") if item.strip()]
    values = [float(item) for item in lines[-1].split("\t") if item.strip()]
    return header, values


def integrate_piecewise(rows: list[dict[str, float]], key: str) -> float:
    value = 0.0
    for left, right in zip(rows, rows[1:]):
        dt = max(right["time"] - left["time"], 0.0)
        value += 0.5 * (left[key] + right[key]) * dt
    return value


def time_above(rows: list[dict[str, float]], threshold: float) -> float:
    value = 0.0
    for left, right in zip(rows, rows[1:]):
        dt = max(right["time"] - left["time"], 0.0)
        if left["temperature"] >= threshold and right["temperature"] >= threshold:
            value += dt
        elif left["temperature"] < threshold and right["temperature"] < threshold:
            continue
        else:
            # COARSENING: linear-crossing approximation for ramp segments.
            delta = right["temperature"] - left["temperature"]
            if delta == 0.0:
                continue
            frac = (threshold - left["temperature"]) / delta
            frac = min(max(frac, 0.0), 1.0)
            value += dt * (1.0 - frac if right["temperature"] >= threshold else frac)
    return value


def thermal_dose_above(rows: list[dict[str, float]], threshold: float) -> float:
    value = 0.0
    for left, right in zip(rows, rows[1:]):
        dt = max(right["time"] - left["time"], 0.0)
        excess_left = max(left["temperature"] - threshold, 0.0)
        excess_right = max(right["temperature"] - threshold, 0.0)
        value += 0.5 * (excess_left + excess_right) * dt
    return value


def history_features(rows: list[dict[str, float]]) -> dict[str, float]:
    max_temperature = max(row["temperature"] for row in rows)
    base_temperature = rows[0]["temperature"]
    total_time = max(rows[-1]["time"] - rows[0]["time"], 0.0)
    ramp_up = 0.0
    ramp_down = 0.0
    max_heat = 0.0
    max_cool = 0.0
    time_at_peak = 0.0
    transient_fluence = 0.0
    transient_started = False
    for left, right in zip(rows, rows[1:]):
        dt = max(right["time"] - left["time"], 0.0)
        dT = right["temperature"] - left["temperature"]
        if dT > 0.0:
            ramp_up += dt
            if dt > 0.0:
                max_heat = max(max_heat, dT / dt)
            transient_started = True
        elif dT < 0.0:
            ramp_down += dt
            if dt > 0.0:
                max_cool = max(max_cool, -dT / dt)
            transient_started = True
        if left["temperature"] >= max_temperature - 1.0 and right["temperature"] >= max_temperature - 1.0:
            time_at_peak += dt
        if transient_started:
            transient_fluence += 0.5 * (left["fission_rate"] + right["fission_rate"]) * dt

    fluence = integrate_piecewise(rows, "fission_rate")
    return {
        "history_total_time": total_time,
        "base_temperature": base_temperature,
        "max_temperature": max_temperature,
        "temperature_excursion": max_temperature - base_temperature,
        "time_at_peak": time_at_peak,
        "time_above_1200": time_above(rows, 1200.0),
        "time_above_1500": time_above(rows, 1500.0),
        "time_above_1800": time_above(rows, 1800.0),
        "thermal_dose_1500": thermal_dose_above(rows, 1500.0),
        "thermal_dose_1800": thermal_dose_above(rows, 1800.0),
        "ramp_up_duration": ramp_up,
        "ramp_down_duration": ramp_down,
        "max_heating_rate": max_heat,
        "max_cooling_rate": max_cool,
        "peak_fission_rate": max(row["fission_rate"] for row in rows),
        "mean_fission_rate": fluence / total_time if total_time > 0.0 else 0.0,
        "fluence_proxy": fluence,
        "transient_fluence_proxy": transient_fluence,
        "min_hydrostatic_stress": min(row["stress"] for row in rows),
    }


def collect_rows() -> list[dict[str, float | str]]:
    expected = load_expected()
    rows: list[dict[str, float | str]] = []
    for case_name, values in sorted(expected.items()):
        case = COARSENING / case_name
        if not case.exists():
            continue
        history = load_history(case / "input_history.txt")
        header, output = load_output(case / "output.txt")
        row: dict[str, float | str] = {"case": case_name, "case_group": case_name.split("_")[-1].split("-")[0]}
        row.update(values)
        row.update(history_features(history))
        row["final_burnup"] = output[header.index("Burnup (MWd/kgUO2)")]
        row["grain_radius_um"] = output[header.index("Grain radius (m)")] * 1.0e6
        row["calc_density_bub_m3"] = output[header.index("Intragranular coarsened bubble concentration (bub/m3)")]
        row["calc_radius_m"] = output[header.index("Intragranular coarsened bubble radius (m)")]
        row["calc_swelling_percent"] = 100.0 * output[header.index("Intragranular coarsened gas bubble swelling (/)")]
        row["dislocation_density"] = output[header.index("Dislocation density (m/m3)")]
        row["density_ratio_calc_exp"] = safe_ratio(row["calc_density_bub_m3"], row["exp_density_bub_m3"])
        row["radius_ratio_calc_exp"] = safe_ratio(row["calc_radius_m"], row["exp_radius_m"])
        row["swelling_ratio_calc_exp"] = safe_ratio(row["calc_swelling_percent"], row["exp_swelling_percent"])
        rho = float(row["dislocation_density"])
        density = float(row["exp_density_bub_m3"])
        row["inferred_k_exp"] = density / rho if rho > 0.0 else 0.0
        row["inferred_spacing_nm"] = 1.0e9 / float(row["inferred_k_exp"]) if float(row["inferred_k_exp"]) > 0.0 else 0.0
        rows.append(row)
    return rows


def safe_ratio(numerator: object, denominator: object) -> float:
    den = float(denominator)
    return float(numerator) / den if den != 0.0 else 0.0


def pearson(x: list[float], y: list[float]) -> float:
    if len(x) < 3:
        return 0.0
    mx = sum(x) / len(x)
    my = sum(y) / len(y)
    sx = math.sqrt(sum((v - mx) ** 2 for v in x))
    sy = math.sqrt(sum((v - my) ** 2 for v in y))
    if sx == 0.0 or sy == 0.0:
        return 0.0
    return sum((a - mx) * (b - my) for a, b in zip(x, y)) / (sx * sy)


def write_csv(rows: list[dict[str, float | str]]) -> None:
    keys = ["case", "case_group"] + [key for key in FEATURE_LABELS if key in rows[0]] + list(TARGET_LABELS)
    with (ROOT / "white_history_features.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=keys)
        writer.writeheader()
        for row in rows:
            writer.writerow({key: row.get(key, "") for key in keys})


def plot_history_overlays(rows: list[dict[str, float | str]]) -> None:
    # COARSENING: overview of actual White thermal and fission-rate histories.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    history_dir = FIGURES / "history_overlays"
    history_dir.mkdir(parents=True, exist_ok=True)

    for color_key, label in [
        ("exp_radius_m", "Experimental radius (m)"),
        ("exp_density_bub_m3", "Experimental bubble concentration (bub/m3)"),
        ("exp_swelling_percent", "Experimental swelling (%)"),
    ]:
        fig, axes = plt.subplots(2, 1, figsize=(8.0, 6.4), sharex=True, constrained_layout=True)
        values = [float(row[color_key]) for row in rows]
        vmin, vmax = min(values), max(values)
        for row in rows:
            hist = load_history(COARSENING / str(row["case"]) / "input_history.txt")
            t0 = hist[0]["time"]
            time = [point["time"] - t0 for point in hist]
            color_value = (float(row[color_key]) - vmin) / (vmax - vmin) if vmax > vmin else 0.5
            color = plt.cm.viridis(color_value)
            axes[0].plot(time, [point["temperature"] for point in hist], color=color, alpha=0.55, linewidth=1.1)
            axes[1].plot(time, [point["fission_rate"] for point in hist], color=color, alpha=0.55, linewidth=1.1)
        axes[0].set_ylabel("Temperature (K)")
        axes[1].set_ylabel("Fission rate (fiss/m3/s)")
        axes[1].set_xlabel("Time since history start (history units)")
        axes[0].set_title(f"White histories colored by {label}")
        sm = plt.cm.ScalarMappable(cmap="viridis", norm=plt.Normalize(vmin=vmin, vmax=vmax))
        fig.colorbar(sm, ax=axes, label=label)
        fig.savefig(history_dir / f"histories_colored_by_{color_key}.png", dpi=250)
        plt.close(fig)


def scatter_feature_grid(rows: list[dict[str, float | str]]) -> None:
    # COARSENING: many direct feature-vs-observable plots to expose useful K_eff dependencies.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    scatter_dir = FIGURES / "feature_scatter"
    scatter_dir.mkdir(parents=True, exist_ok=True)
    features = [
        "final_burnup",
        "grain_radius_um",
        "max_temperature",
        "temperature_excursion",
        "time_at_peak",
        "time_above_1500",
        "time_above_1800",
        "thermal_dose_1500",
        "thermal_dose_1800",
        "ramp_up_duration",
        "ramp_down_duration",
        "max_heating_rate",
        "peak_fission_rate",
        "mean_fission_rate",
        "fluence_proxy",
        "transient_fluence_proxy",
        "min_hydrostatic_stress",
        "dislocation_density",
    ]
    targets = [
        "exp_density_bub_m3",
        "exp_radius_m",
        "exp_swelling_percent",
        "inferred_k_exp",
        "density_ratio_calc_exp",
        "radius_ratio_calc_exp",
        "swelling_ratio_calc_exp",
    ]
    for target in targets:
        for feature in features:
            x = [float(row[feature]) for row in rows]
            y = [float(row[target]) for row in rows]
            if max(x) == min(x) or max(y) == min(y):
                continue
            fig, ax = plt.subplots(figsize=(5.2, 4.1), constrained_layout=True)
            colors = [float(row["exp_radius_m"]) for row in rows]
            ax.scatter(x, y, c=colors, cmap="plasma", s=28, edgecolors="black", linewidths=0.25)
            if min(x) > 0.0 and max(x) / min(x) > 100.0:
                ax.set_xscale("log")
            if min(y) > 0.0 and max(y) / min(y) > 100.0:
                ax.set_yscale("log")
            ax.set_xlabel(FEATURE_LABELS.get(feature, feature))
            ax.set_ylabel(TARGET_LABELS.get(target, target))
            ax.grid(True, color="0.88", linewidth=0.8)
            ax.set_title(f"r = {pearson(x, y):+.2f}")
            fig.savefig(scatter_dir / f"{target}_vs_{feature}.png", dpi=220)
            plt.close(fig)


def plot_correlation_heatmap(rows: list[dict[str, float | str]]) -> list[tuple[str, str, float]]:
    # COARSENING: correlation heatmap ranks which history features may calibrate K_eff or family splitting.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    corr_dir = FIGURES / "correlations"
    corr_dir.mkdir(parents=True, exist_ok=True)
    features = [key for key in FEATURE_LABELS if key in rows[0]]
    targets = list(TARGET_LABELS)
    matrix: list[list[float]] = []
    ranked: list[tuple[str, str, float]] = []
    for target in targets:
        target_values = [float(row[target]) for row in rows]
        line = []
        for feature in features:
            feature_values = [float(row[feature]) for row in rows]
            corr = pearson(feature_values, target_values)
            line.append(corr)
            ranked.append((feature, target, corr))
        matrix.append(line)

    fig, ax = plt.subplots(figsize=(11.5, 5.5), constrained_layout=True)
    image = ax.imshow(matrix, cmap="coolwarm", vmin=-1.0, vmax=1.0, aspect="auto")
    ax.set_xticks(range(len(features)), [FEATURE_LABELS.get(feature, feature) for feature in features], rotation=70, ha="right")
    ax.set_yticks(range(len(targets)), [TARGET_LABELS.get(target, target) for target in targets])
    fig.colorbar(image, ax=ax, label="Pearson correlation")
    ax.set_title("White history-feature correlations")
    fig.savefig(corr_dir / "history_feature_correlation_heatmap.png", dpi=260)
    plt.close(fig)
    return sorted(ranked, key=lambda item: abs(item[2]), reverse=True)


def plot_candidate_keff(rows: list[dict[str, float | str]]) -> None:
    # COARSENING: candidate calibration plots for K_eff from experimental N_exp/rho_d.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    out_dir = FIGURES / "candidate_keff"
    out_dir.mkdir(parents=True, exist_ok=True)
    candidate_features = [
        "dislocation_density",
        "max_temperature",
        "final_burnup",
        "time_at_peak",
        "thermal_dose_1500",
        "peak_fission_rate",
        "fluence_proxy",
        "grain_radius_um",
    ]
    for feature in candidate_features:
        fig, ax = plt.subplots(figsize=(5.2, 4.1), constrained_layout=True)
        x = [float(row[feature]) for row in rows if float(row["inferred_k_exp"]) > 0.0]
        y = [float(row["inferred_k_exp"]) for row in rows if float(row["inferred_k_exp"]) > 0.0]
        color = [float(row["exp_radius_m"]) * 1.0e9 for row in rows if float(row["inferred_k_exp"]) > 0.0]
        ax.scatter(x, y, c=color, cmap="magma", s=32, edgecolors="black", linewidths=0.25)
        if min(x) > 0.0 and max(x) / min(x) > 100.0:
            ax.set_xscale("log")
        ax.set_yscale("log")
        ax.set_xlabel(FEATURE_LABELS.get(feature, feature))
        ax.set_ylabel("Inferred K_eff = N_exp / rho_d (bub/m)")
        ax.grid(True, color="0.88", linewidth=0.8)
        ax.set_title(f"K_eff candidate, r = {pearson(x, y):+.2f}")
        cbar = fig.colorbar(plt.cm.ScalarMappable(cmap="magma", norm=plt.Normalize(vmin=min(color), vmax=max(color))), ax=ax)
        cbar.set_label("Experimental radius (nm)")
        fig.savefig(out_dir / f"inferred_keff_vs_{feature}.png", dpi=230)
        plt.close(fig)


def write_summary(rows: list[dict[str, float | str]], ranked: list[tuple[str, str, float]]) -> None:
    # COARSENING: compact written readout so the plots have an engineering interpretation.
    lines = [
        "# White History Analysis",
        "",
        "COARSENING exploratory analysis for calibrating `K_eff` and future bubble-family splitting.",
        "",
        f"Cases analyzed: {len(rows)}",
        "",
        "## Strongest Absolute Correlations",
        "",
    ]
    for feature, target, corr in ranked[:30]:
        lines.append(f"- `{FEATURE_LABELS.get(feature, feature)}` vs `{TARGET_LABELS.get(target, target)}`: r = {corr:+.3f}")
    lines.extend([
        "",
        "## Practical Readout",
        "",
        "- If `N_exp/rho_d` correlates with a history feature, that feature is a good candidate for `K_eff`.",
        "- If radius correlates with thermal dose or peak-hold time while concentration does not, a family-splitting or growth/vacancy parameter is more appropriate than only changing `K_eff`.",
        "- If model/experiment ratios correlate with a feature, that feature is a useful calibration axis for reducing the horizontal parity-plot alignment.",
        "",
        "Figures are grouped in `figures/history_overlays`, `figures/feature_scatter`, `figures/candidate_keff`, and `figures/correlations`.",
    ])
    (ROOT / "analysis_summary.md").write_text("\n".join(lines) + "\n")


def main() -> int:
    FIGURES.mkdir(parents=True, exist_ok=True)
    rows = collect_rows()
    write_csv(rows)
    plot_history_overlays(rows)
    scatter_feature_grid(rows)
    plot_candidate_keff(rows)
    ranked = plot_correlation_heatmap(rows)
    write_summary(rows, ranked)
    print(f"COARSENING White history analysis written to {ROOT}")
    print(f"Cases analyzed: {len(rows)}")
    print("Top correlations:")
    for feature, target, corr in ranked[:12]:
        print(f"  {FEATURE_LABELS.get(feature, feature)} -> {TARGET_LABELS.get(target, target)}: r={corr:+.3f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
