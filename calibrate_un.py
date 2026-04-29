"""Automatic calibration pipeline for the UN/Rizk intragranular model."""

import csv
import math
import struct
import zlib
from pathlib import Path

from un_data import EXP_ND_T_13, EXP_RD_T_13, EXP_SWELLING_BURNUP_1600, EXP_SWELLING_T
from un_model import GRAIN_RADIUS, run_model_point

try:
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
except ModuleNotFoundError:
    plt = None

RESULTS_DIR = Path("results_un_calibration")

FN_GRID = [1.0e-10, 3.0e-10, 1.0e-9, 3.0e-9, 1.0e-8, 3.0e-8, 1.0e-7, 3.0e-7, 1.0e-6]
K_D_GRID = [1.0e5, 2.0e5, 3.0e5, 5.0e5, 8.0e5]
G_D_SCALE_GRID = [0.5, 1.0, 2.0, 3.0, 5.0, 10.0]
SEED_GRID_NM = [0.0, 1.0, 2.0, 3.0]

DEFAULT_SEED_NM = 0.0
DEFAULT_XE_YIELD = 0.24
DEFAULT_DT_H = 48.0
DEFAULT_N_MODES = 8


def rmse(values):
    values = [v for v in values if math.isfinite(v)]
    if not values:
        return math.nan
    return math.sqrt(sum(v * v for v in values) / len(values))


def log10_rmse(pairs):
    errs = []
    for pred, exp in pairs:
        if pred > 0.0 and exp > 0.0 and math.isfinite(pred) and math.isfinite(exp):
            errs.append(math.log10(pred / exp))
    return rmse(errs)


def model_point(T, burnup, f_n, K_d, g_d_scale, seed_nm=DEFAULT_SEED_NM):
    return run_model_point(
        T,
        burnup,
        f_n,
        K_d=K_d,
        g_d_scale=g_d_scale,
        bulk_seed_radius_nm=seed_nm,
        dt_h=DEFAULT_DT_H,
        n_modes=DEFAULT_N_MODES,
        grain_radius=GRAIN_RADIUS,
        xe_yield=DEFAULT_XE_YIELD,
    )


def pressure_penalty_for_row(row):
    penalty = 0.0
    for key in ("p_b_over_eq", "p_d_over_eq"):
        value = row.get(key, math.nan)
        if not (value > 0.0 and math.isfinite(value)):
            penalty += 2.0
            continue
        logv = abs(math.log10(max(value, 1.0e-30)))
        if value < 0.3 or value > 3.0:
            penalty += max(0.0, logv - math.log10(3.0))
    return penalty


def high_temperature_penalty(f_n, K_d, g_d_scale, seed_nm):
    penalties = []
    for T in (1800.0, 1900.0, 2000.0):
        row = model_point(T, 3.2, f_n, K_d, g_d_scale, seed_nm)
        sw_excess = max(0.0, (row["swelling_d_percent"] - 15.0) / 15.0)
        rd_excess = max(0.0, (row["Rd_nm"] - 500.0) / 500.0)
        penalties.append(sw_excess + rd_excess)
    return rmse(penalties)


def score_candidate(f_n, K_d, g_d_scale, seed_nm=DEFAULT_SEED_NM, stage="coarse"):
    swelling_errors_d = []
    swelling_errors_ig = []
    for exp in EXP_SWELLING_T:
        out = model_point(exp["T"], exp["burnup"], f_n, K_d, g_d_scale, seed_nm)
        swelling_errors_d.append(out["swelling_d_percent"] - exp["swelling"])
        swelling_errors_ig.append(out["swelling_ig_percent"] - exp["swelling"])

    burnup_errors_d = []
    burnup_errors_ig = []
    for exp in EXP_SWELLING_BURNUP_1600:
        out = model_point(1600.0, exp["burnup"], f_n, K_d, g_d_scale, seed_nm)
        burnup_errors_d.append(out["swelling_d_percent"] - exp["swelling"])
        burnup_errors_ig.append(out["swelling_ig_percent"] - exp["swelling"])

    n_pairs = []
    for exp in EXP_ND_T_13:
        out = model_point(exp["T"], 1.3, f_n, K_d, g_d_scale, seed_nm)
        n_pairs.append((out["Nd"], exp["N"]))

    r_pairs = []
    for exp in EXP_RD_T_13:
        out = model_point(exp["T"], 1.3, f_n, K_d, g_d_scale, seed_nm)
        r_pairs.append((out["Rd_nm"], exp["R_nm"]))

    pressure_rows = [model_point(T, 3.2, f_n, K_d, g_d_scale, seed_nm) for T in (1200.0, 1400.0, 1600.0, 1800.0)]
    score_pressure = rmse([pressure_penalty_for_row(row) for row in pressure_rows])
    score_highT_penalty = high_temperature_penalty(f_n, K_d, g_d_scale, seed_nm)
    score_swd = rmse(swelling_errors_d)
    score_swig = rmse(swelling_errors_ig)
    score_burnup_d = rmse(burnup_errors_d)
    score_burnup_ig = rmse(burnup_errors_ig)
    score_Nd = log10_rmse(n_pairs)
    score_Rd = log10_rmse(r_pairs)
    score_main = score_swd + 0.25 * score_burnup_d + 0.7 * score_Nd + 0.7 * score_Rd + 0.2 * score_pressure + score_highT_penalty
    score_alt_total_ig = score_swig + 0.25 * score_burnup_ig + 0.7 * score_Nd + 0.7 * score_Rd + 0.2 * score_pressure + score_highT_penalty
    probe = model_point(1600.0, 1.3, f_n, K_d, g_d_scale, seed_nm)
    return {
        "stage": stage,
        "f_n": f_n,
        "K_d": K_d,
        "g_d_scale": g_d_scale,
        "seed_nm": seed_nm,
        "score_main": score_main,
        "score_alt_total_ig": score_alt_total_ig,
        "score_swd": score_swd,
        "score_swig": score_swig,
        "score_burnup_d": score_burnup_d,
        "score_burnup_ig": score_burnup_ig,
        "score_Nd": score_Nd,
        "score_Rd": score_Rd,
        "score_pressure": score_pressure,
        "score_highT_penalty": score_highT_penalty,
        "probe_swelling_d_1600_13": probe["swelling_d_percent"],
        "probe_swelling_ig_1600_13": probe["swelling_ig_percent"],
        "probe_Nd_1600_13": probe["Nd"],
        "probe_Rd_nm_1600_13": probe["Rd_nm"],
        "probe_p_b_over_eq_1600_13": probe["p_b_over_eq"],
        "probe_p_d_over_eq_1600_13": probe["p_d_over_eq"],
    }


def unique_candidate_key(row):
    return (
        f"{row['f_n']:.12e}",
        f"{row['K_d']:.12e}",
        f"{row['g_d_scale']:.12e}",
        f"{row['seed_nm']:.12e}",
    )


def run_sweep():
    rows = []
    seen = set()
    baseline = score_candidate(1.0e-6, 5.0e5, 1.0, DEFAULT_SEED_NM, stage="baseline")
    rows.append(baseline)
    seen.add(unique_candidate_key(baseline))

    for f_n in FN_GRID:
        for K_d in K_D_GRID:
            for g_d_scale in G_D_SCALE_GRID:
                candidate = {"f_n": f_n, "K_d": K_d, "g_d_scale": g_d_scale, "seed_nm": DEFAULT_SEED_NM}
                key = unique_candidate_key(candidate)
                if key in seen:
                    continue
                rows.append(score_candidate(f_n, K_d, g_d_scale, DEFAULT_SEED_NM, stage="coarse"))
                if len(rows) % 25 == 0:
                    print(f"scored {len(rows)} candidates", flush=True)
                seen.add(key)

    best_for_refinement = sorted(rows, key=lambda r: r["score_main"])[:5]
    for base in best_for_refinement:
        for f_mult in (10.0 ** -0.5, 10.0 ** 0.5):
            for k_mult in (0.5, 1.5):
                for g_mult in (0.5, 1.5):
                    f_n = min(max(base["f_n"] * f_mult, 1.0e-11), 3.0e-6)
                    K_d = min(max(base["K_d"] * k_mult, 5.0e4), 1.2e6)
                    g_d_scale = min(max(base["g_d_scale"] * g_mult, 0.1), 20.0)
                    candidate = {"f_n": f_n, "K_d": K_d, "g_d_scale": g_d_scale, "seed_nm": DEFAULT_SEED_NM}
                    key = unique_candidate_key(candidate)
                    if key in seen:
                        continue
                    rows.append(score_candidate(f_n, K_d, g_d_scale, DEFAULT_SEED_NM, stage="refine"))
                    if len(rows) % 25 == 0:
                        print(f"scored {len(rows)} candidates", flush=True)
                    seen.add(key)

    best_for_seed = sorted(rows, key=lambda r: r["score_main"])[:5]
    for base in best_for_seed:
        for seed_nm in SEED_GRID_NM:
            candidate = {"f_n": base["f_n"], "K_d": base["K_d"], "g_d_scale": base["g_d_scale"], "seed_nm": seed_nm}
            key = unique_candidate_key(candidate)
            if key in seen:
                continue
            rows.append(score_candidate(base["f_n"], base["K_d"], base["g_d_scale"], seed_nm, stage="seed"))
            seen.add(key)
    return rows


def write_csv(path, rows):
    if not rows:
        return
    fields = list(rows[0].keys())
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def experimental_swelling_for_burnup(ax, burnup):
    series_names = sorted({p["series"] for p in EXP_SWELLING_T if abs(p["burnup"] - burnup) < 1.0e-9})
    for series in series_names:
        pts = [p for p in EXP_SWELLING_T if abs(p["burnup"] - burnup) < 1.0e-9 and p["series"] == series]
        marker = "x" if "119" in series else "^"
        ax.scatter([p["T"] for p in pts], [p["swelling"] for p in pts], marker=marker, s=35, label=f"exp {series}")


def write_png_rgb(path, width, height, pixels):
    raw = bytearray()
    for y in range(height):
        raw.append(0)
        start = y * width * 3
        raw.extend(pixels[start:start + width * 3])
    def chunk(kind, data):
        return (
            struct.pack(">I", len(data))
            + kind
            + data
            + struct.pack(">I", zlib.crc32(kind + data) & 0xFFFFFFFF)
        )
    png = b"\x89PNG\r\n\x1a\n"
    png += chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
    png += chunk(b"IDAT", zlib.compress(bytes(raw), 9))
    png += chunk(b"IEND", b"")
    path.write_bytes(png)


def make_canvas(width=900, height=560):
    return bytearray([255] * width * height * 3), width, height


def set_pixel(pixels, width, height, x, y, color):
    if 0 <= x < width and 0 <= y < height:
        i = (y * width + x) * 3
        pixels[i:i + 3] = bytes(color)


def draw_line(pixels, width, height, x0, y0, x1, y1, color):
    x0, y0, x1, y1 = int(round(x0)), int(round(y0)), int(round(x1)), int(round(y1))
    dx = abs(x1 - x0)
    dy = -abs(y1 - y0)
    sx = 1 if x0 < x1 else -1
    sy = 1 if y0 < y1 else -1
    err = dx + dy
    while True:
        for ox in (-1, 0, 1):
            for oy in (-1, 0, 1):
                set_pixel(pixels, width, height, x0 + ox, y0 + oy, color)
        if x0 == x1 and y0 == y1:
            break
        e2 = 2 * err
        if e2 >= dy:
            err += dy
            x0 += sx
        if e2 <= dx:
            err += dx
            y0 += sy


def draw_marker(pixels, width, height, x, y, color, size=4):
    x, y = int(round(x)), int(round(y))
    for yy in range(y - size, y + size + 1):
        for xx in range(x - size, x + size + 1):
            if abs(xx - x) + abs(yy - y) <= size:
                set_pixel(pixels, width, height, xx, yy, color)


def simple_chart(path, series, xlim=None, ylim=None, logy=False):
    pixels, width, height = make_canvas()
    left, right, top, bottom = 72, width - 28, 28, height - 58
    values_x = []
    values_y = []
    for item in series:
        values_x.extend(item["x"])
        values_y.extend(item["y"])
    if logy:
        values_y = [v for v in values_y if v > 0.0 and math.isfinite(v)]
    else:
        values_y = [v for v in values_y if math.isfinite(v)]
    values_x = [v for v in values_x if math.isfinite(v)]
    xmin, xmax = xlim if xlim else (min(values_x), max(values_x))
    ymin, ymax = ylim if ylim else (min(values_y), max(values_y))
    if ymin == ymax:
        ymax = ymin + 1.0
    if logy:
        ymin = max(ymin, 1.0e-30)
        ymin_l, ymax_l = math.log10(ymin), math.log10(ymax)
    for frac in (0.0, 0.25, 0.5, 0.75, 1.0):
        x = left + frac * (right - left)
        y = bottom - frac * (bottom - top)
        draw_line(pixels, width, height, x, top, x, bottom, (235, 235, 235))
        draw_line(pixels, width, height, left, y, right, y, (235, 235, 235))
    draw_line(pixels, width, height, left, bottom, right, bottom, (0, 0, 0))
    draw_line(pixels, width, height, left, top, left, bottom, (0, 0, 0))

    def map_point(x, y):
        px = left + (x - xmin) / (xmax - xmin) * (right - left)
        if logy:
            y = max(y, 1.0e-30)
            py = bottom - (math.log10(y) - ymin_l) / (ymax_l - ymin_l) * (bottom - top)
        else:
            py = bottom - (y - ymin) / (ymax - ymin) * (bottom - top)
        return px, py

    for item in series:
        color = item["color"]
        points = [
            map_point(x, y)
            for x, y in zip(item["x"], item["y"])
            if math.isfinite(x) and math.isfinite(y) and (not logy or y > 0.0)
        ]
        if item.get("kind") == "scatter":
            for x, y in points:
                draw_marker(pixels, width, height, x, y, color)
        else:
            for (x0, y0), (x1, y1) in zip(points, points[1:]):
                draw_line(pixels, width, height, x0, y0, x1, y1, color)
    write_png_rgb(path, width, height, pixels)


def plot_outputs_basic(best):
    f_n, K_d, g_d_scale, seed_nm = best["f_n"], best["K_d"], best["g_d_scale"], best["seed_nm"]
    temperatures = [float(T) for T in range(900, 2001, 50)]
    colors = [(25, 93, 170), (210, 88, 45), (45, 145, 85), (120, 80, 160), (20, 20, 20)]

    swelling_series = []
    for idx, burnup in enumerate((1.1, 1.3, 3.2)):
        rows = [model_point(T, burnup, f_n, K_d, g_d_scale, seed_nm) for T in temperatures]
        swelling_series.append({"x": temperatures, "y": [r["swelling_d_percent"] for r in rows], "color": colors[idx]})
        swelling_series.append({"x": temperatures, "y": [r["swelling_ig_percent"] for r in rows], "color": colors[idx + 1]})
    swelling_series.append({"x": [p["T"] for p in EXP_SWELLING_T], "y": [p["swelling"] for p in EXP_SWELLING_T], "color": colors[-1], "kind": "scatter"})
    simple_chart(RESULTS_DIR / "swelling_vs_T_best.png", swelling_series, xlim=(850.0, 2050.0), ylim=(0.0, 6.0))

    rows_13 = [model_point(T, 1.3, f_n, K_d, g_d_scale, seed_nm) for T in temperatures]
    simple_chart(
        RESULTS_DIR / "radius_vs_T_best.png",
        [
            {"x": temperatures, "y": [r["Rd_nm"] for r in rows_13], "color": colors[0]},
            {"x": temperatures, "y": [r["Rb_nm"] for r in rows_13], "color": colors[1]},
            {"x": [p["T"] for p in EXP_RD_T_13], "y": [p["R_nm"] for p in EXP_RD_T_13], "color": colors[-1], "kind": "scatter"},
        ],
        xlim=(850.0, 2050.0),
        logy=True,
    )
    simple_chart(
        RESULTS_DIR / "number_density_vs_T_best.png",
        [
            {"x": temperatures, "y": [r["Nd"] for r in rows_13], "color": colors[0]},
            {"x": temperatures, "y": [r["Nb"] for r in rows_13], "color": colors[1]},
            {"x": [p["T"] for p in EXP_ND_T_13], "y": [p["N"] for p in EXP_ND_T_13], "color": colors[-1], "kind": "scatter"},
        ],
        xlim=(850.0, 2050.0),
        logy=True,
    )
    rows_32 = [model_point(T, 3.2, f_n, K_d, g_d_scale, seed_nm) for T in temperatures]
    simple_chart(
        RESULTS_DIR / "pressure_vs_T_best.png",
        [
            {"x": temperatures, "y": [r["p_b_over_eq"] for r in rows_32], "color": colors[0]},
            {"x": temperatures, "y": [r["p_d_over_eq"] for r in rows_32], "color": colors[1]},
        ],
        xlim=(850.0, 2050.0),
        logy=True,
    )
    temperatures_gas = [float(T) for T in range(900, 2601, 100)]
    rows_gas = [model_point(T, 3.2, f_n, K_d, g_d_scale, seed_nm) for T in temperatures_gas]
    simple_chart(
        RESULTS_DIR / "gas_partition_best.png",
        [
            {"x": temperatures_gas, "y": [r["matrix_gas_percent"] for r in rows_gas], "color": colors[0]},
            {"x": temperatures_gas, "y": [r["bulk_gas_percent"] for r in rows_gas], "color": colors[1]},
            {"x": temperatures_gas, "y": [r["dislocation_gas_percent"] for r in rows_gas], "color": colors[2]},
            {"x": temperatures_gas, "y": [r["qgb_gas_percent"] for r in rows_gas], "color": colors[3]},
        ],
        xlim=(850.0, 2650.0),
        ylim=(0.0, 100.0),
    )
    return True


def plot_outputs(best):
    if plt is None:
        return plot_outputs_basic(best)
    f_n, K_d, g_d_scale, seed_nm = best["f_n"], best["K_d"], best["g_d_scale"], best["seed_nm"]
    temperatures = [float(T) for T in range(900, 2001, 50)]

    fig, axes = plt.subplots(1, 3, figsize=(15, 4), sharey=True)
    for ax, burnup in zip(axes, (1.1, 1.3, 3.2)):
        rows = [model_point(T, burnup, f_n, K_d, g_d_scale, seed_nm) for T in temperatures]
        ax.plot(temperatures, [r["swelling_d_percent"] for r in rows], label="model dislocation")
        ax.plot(temperatures, [r["swelling_ig_percent"] for r in rows], linestyle="--", label="model total IG")
        experimental_swelling_for_burnup(ax, burnup)
        ax.set_title(f"{burnup:.1f}% FIMA")
        ax.set_xlabel("T [K]")
        ax.grid(True, alpha=0.3)
    axes[0].set_ylabel("Swelling [%]")
    axes[0].legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(RESULTS_DIR / "swelling_vs_T_best.png", dpi=180)
    plt.close(fig)

    rows_13 = [model_point(T, 1.3, f_n, K_d, g_d_scale, seed_nm) for T in temperatures]
    fig, ax = plt.subplots(figsize=(7, 5))
    ax.plot(temperatures, [r["Rd_nm"] for r in rows_13], label="model R_d")
    ax.plot(temperatures, [r["Rb_nm"] for r in rows_13], linestyle=":", label="model R_b")
    ax.scatter([p["T"] for p in EXP_RD_T_13], [p["R_nm"] for p in EXP_RD_T_13], marker="^", label="exp")
    ax.set_xlabel("T [K]")
    ax.set_ylabel("Radius [nm]")
    ax.grid(True, alpha=0.3)
    ax.legend()
    fig.tight_layout()
    fig.savefig(RESULTS_DIR / "radius_vs_T_best.png", dpi=180)
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(7, 5))
    ax.plot(temperatures, [r["Nd"] for r in rows_13], label="model N_d")
    ax.plot(temperatures, [r["Nb"] for r in rows_13], linestyle=":", label="model N_b")
    ax.scatter([p["T"] for p in EXP_ND_T_13], [p["N"] for p in EXP_ND_T_13], marker="^", label="exp")
    ax.set_yscale("log")
    ax.set_xlabel("T [K]")
    ax.set_ylabel("Number density [m^-3]")
    ax.grid(True, which="both", alpha=0.3)
    ax.legend()
    fig.tight_layout()
    fig.savefig(RESULTS_DIR / "number_density_vs_T_best.png", dpi=180)
    plt.close(fig)

    rows_32 = [model_point(T, 3.2, f_n, K_d, g_d_scale, seed_nm) for T in temperatures]
    fig, ax = plt.subplots(figsize=(7, 5))
    ax.plot(temperatures, [r["p_b_over_eq"] for r in rows_32], label="p_b/p_eq")
    ax.plot(temperatures, [r["p_d_over_eq"] for r in rows_32], label="p_d/p_eq")
    ax.axhspan(0.3, 3.0, color="0.9", zorder=-1)
    ax.set_yscale("log")
    ax.set_xlabel("T [K]")
    ax.set_ylabel("Pressure ratio [-]")
    ax.grid(True, which="both", alpha=0.3)
    ax.legend()
    fig.tight_layout()
    fig.savefig(RESULTS_DIR / "pressure_vs_T_best.png", dpi=180)
    plt.close(fig)

    temperatures_gas = [float(T) for T in range(900, 2601, 100)]
    fig, axes = plt.subplots(1, 2, figsize=(12, 4), sharey=True)
    for ax, burnup in zip(axes, (1.1, 3.2)):
        rows = [model_point(T, burnup, f_n, K_d, g_d_scale, seed_nm) for T in temperatures_gas]
        ax.plot(temperatures_gas, [r["matrix_gas_percent"] for r in rows], label="matrix")
        ax.plot(temperatures_gas, [r["bulk_gas_percent"] for r in rows], label="bulk")
        ax.plot(temperatures_gas, [r["dislocation_gas_percent"] for r in rows], label="dislocation")
        ax.plot(temperatures_gas, [r["qgb_gas_percent"] for r in rows], label="q_gb")
        ax.set_title(f"{burnup:.1f}% FIMA")
        ax.set_xlabel("T [K]")
        ax.grid(True, alpha=0.3)
    axes[0].set_ylabel("Generated gas [%]")
    axes[0].legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(RESULTS_DIR / "gas_partition_best.png", dpi=180)
    plt.close(fig)
    return True


def fmt(value):
    if isinstance(value, str):
        return value
    if value == 0:
        return "0"
    if isinstance(value, float):
        return f"{value:.6g}"
    return str(value)


def candidate_markdown(title, row, score_key):
    keys = [
        "f_n",
        "K_d",
        "g_d_scale",
        "seed_nm",
        score_key,
        "score_swd",
        "score_swig",
        "score_Nd",
        "score_Rd",
        "score_pressure",
        "score_highT_penalty",
        "probe_swelling_d_1600_13",
        "probe_swelling_ig_1600_13",
        "probe_Nd_1600_13",
        "probe_Rd_nm_1600_13",
    ]
    lines = [f"## {title}", ""]
    for key in keys:
        lines.append(f"- `{key}`: {fmt(row[key])}")
    return "\n".join(lines)


def write_summary(rows, best_main, best_alt, plots_written):
    baseline = next(r for r in rows if r["stage"] == "baseline")
    lines = [
        "# UN calibration summary",
        "",
        "Pipeline generated from the long `# CONFIGURAZIONE GENERALE` notebook cell and the AI calibration instructions.",
        "",
        "Fixed/default choices: `GRAIN_RADIUS = 6.0e-6 m`, `FISSION_RATE = 5.0e19`, `bulk_seed_radius_nm = 0.0` for the main sweep, `xe_yield = 0.24`, and SCIANTIX-like old-step gas-solver coefficients. `g_d_scale` multiplies only the dislocation trapping rate `g_d`, not `g_b`.",
        "",
        "Scores use Fig. 3 against both `swelling_d_percent` and `swelling_ig_percent`, Fig. 7 against `Nd`, Fig. 8 against `Rd_nm`, a medium-temperature pressure penalty on `p_b/p_eq` and `p_d/p_eq`, and a high-temperature blow-up penalty.",
        "",
        candidate_markdown("Baseline nominal", baseline, "score_main"),
        "",
        candidate_markdown("Best candidate if Fig. 3 is P2/dislocation swelling", best_main, "score_main"),
        "",
        candidate_markdown("Best candidate if Fig. 3 is total intragranular swelling", best_alt, "score_alt_total_ig"),
        "",
        "## Notes",
        "",
        "- `g_d_scale != 1` is an empirical trapping correction, not an original Rizk parameter.",
        "- `q_gb` is only a grain-face balance term here; the model still lacks explicit grain-boundary bubbles, interconnection, and release.",
        "- Results above about 1800 K should be read cautiously because missing release/intergranular physics can make dislocation bubbles grow too aggressively.",
        f"- PNG plots written: `{plots_written}`.",
    ]
    (RESULTS_DIR / "summary.md").write_text("\n".join(lines) + "\n")


def main():
    RESULTS_DIR.mkdir(exist_ok=True)
    rows = run_sweep()
    rows = sorted(rows, key=lambda r: r["score_main"])
    best_main = rows[0]
    best_alt = min(rows, key=lambda r: r["score_alt_total_ig"])
    best_candidates = sorted({unique_candidate_key(r): r for r in rows}.values(), key=lambda r: r["score_main"])[:50]
    write_csv(RESULTS_DIR / "all_runs.csv", rows)
    write_csv(RESULTS_DIR / "best_candidates.csv", best_candidates)
    plots_written = plot_outputs(best_main)
    write_summary(rows, best_main, best_alt, plots_written)
    print(f"Wrote {RESULTS_DIR / 'summary.md'}")
    print(f"Best dislocation score: f_n={best_main['f_n']:.3e}, K_d={best_main['K_d']:.3e}, g_d_scale={best_main['g_d_scale']:.3g}")
    print(f"Best total-IG score: f_n={best_alt['f_n']:.3e}, K_d={best_alt['K_d']:.3e}, g_d_scale={best_alt['g_d_scale']:.3g}")


if __name__ == "__main__":
    main()
