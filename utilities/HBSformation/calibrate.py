"""Calibration of the Landau HBS-formation model on the JSON datasets.

@author  E. Cappellari
@date    2026-09-15
---------------------------------------------------------------------------------
Fits k, beta and rho_c of `hbs_formation_landau.py` (n fixed) by minimising

    J = <(Theta_pred - Theta_obs)^2>_w / var(Theta)
      + w_r <(r_pred - r_obs)^2>_w / var(r)
      + w_X <(X_pred - X_obs)^2>_w / var(X)

with `differential_evolution` over (k, beta, log10 rho_c) from several seeds.

MODES
    python3 calibrate.py [--scenario A|B|C] [--weight W_R] [--fraction-weight W_X] ...
        defaults: data set C, w_r = 0.2, w_X = 1, 6 seeds (the choice made on the calibration front)
        one fit; prints the parameters ready to paste into hbs_formation_landau.py and into the
        case 4 parameter push of src/models/HighBurnupStructureFormation.C

    python3 calibrate.py --front [--scenarios ABC] [--jobs N]
        calibration front: one fit per (data set, w_r, w_X) on a grid, scored on every observable
        on ALL the data. Shows what each weight buys and costs, marks the Pareto-optimal weights.
        -> figures/calibration/front.csv, front.png

    python3 calibrate.py --study [--weight W_R] [--fraction-weight W_X] [--jobs N]
        the three data sets at the chosen weights: weight of each point (leverage, Cook's
        distance), scores on all the data, leave one paper out.
        -> figures/calibration/decision.csv, paste_blocks.txt, metrics.csv, points.csv,
           weights_landau.png, influence_landau.png, curves.png

DATA  (hbs_dataset.load_points, data/*.json; local burnup and temperature of each point)
  theta     Theta from f1, f10, AMis2Mean (theta_measured)       ZAC2022 std + Cr, ONO2025 (Theta = 0)
  fraction  X = f10/100                                          ZAC2022 std + Cr
            Xe-depleted area fraction (Fig. 6 markers)           NOI2015
            HBS area fraction, Barani 2020 image analysis (U)    GER2018
  radius    r_n = ECD50/2                                        ZAC2022 std + Cr
            dA/2 in the HBS layer (r/ro >= 0.97, Fig. 8)         GER2018
  NOI2015 and GER2018 fractions enter the fit only when w_X > 0.

DATA SETS
  A  Zacharie-Aubrun 2022 + Onofri 2025, standard UO2, equal weights
  B  all four papers (Cr-doped included), equal weights
  C  as B, point weights = rank factor x relevance / 3 (hbs_dataset.study_weight)

WEIGHT OF EACH POINT
  leverage h_ii    diagonal of J (J^T J)^-1 J^T of the weighted residual Jacobian at the optimum
  Cook's distance  D_i = e_i^2 / (p s^2) h_ii / (1 - h_ii)^2: parameter change if the point is removed

Needs numpy and scipy (matplotlib for --front and --study).  `hbs_formation_landau.py` stays stdlib-only.
"""

from __future__ import annotations

import argparse
import csv
import math
import os
import sys
from concurrent.futures import ProcessPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)

from hbs_formation_landau import (  # noqa: E402
    DATA_FILE,
    FABRICATION_POROSITY,
    GRAIN_RADIUS,
    N_FAMILIES,
    ModelParameters,
    hbs_state,
    load_ebsd,
    measured_radius,
    theta_measured,
    validate,
)

try:
    import numpy as np
    from scipy.optimize import differential_evolution
except ImportError as error:  # pragma: no cover
    sys.exit("calibrate.py needs numpy and scipy (%s).\n"
             "Install them, or use hbs_formation_landau.py alone, which does not." % error)

# Search box: (k, beta, log10 rho_c).
BOUNDS_K = (0.001, 50.0)
BOUNDS_BETA = (2.0, 300.0)
BOUNDS_LOG10_RHO_C = (8.0, 20.0)

WEIGHT_DEFAULT = 0.2           # w_r, size term (chosen on the calibration front)
FRACTION_WEIGHT_DEFAULT = 1.0  # w_X, fraction term (chosen on the calibration front)
SEEDS_DEFAULT = 6
SCENARIO_DEFAULT = "C"         # all four papers, weighted by quality rank x relevance

FRONT_W_R = (0.0, 0.01, 0.05, 0.2, 1.0)
FRONT_W_X = (0.0, 0.1, 0.3, 1.0, 3.0)

OUT = os.path.join(HERE, "figures", "calibration")
OBS = ("theta", "radius", "fraction")
OBS_LABEL = {"theta": "mean misorientation Θ (°)", "radius": "sub-grain radius r_n (µm)",
             "fraction": "restructured fraction X (–)"}
GROUP_STYLE = {   # group -> (marker, edge colour, label)
    "ZAC": ("o", "#1f5fbf", "Zacharie-Aubrun 2022 · standard UO₂"),
    "Cr": ("D", "#7a3fb0", "Zacharie-Aubrun 2022 · Cr-doped UO₂"),
    "ONO": ("s", "#d0621c", "Onofri 2025 · standard UO₂"),
    "GER": ("*", "#b3263b", "Gerczak 2018 · H.B. Robinson rim"),
    "NOI": ("^", "#138a5a", "Noirot 2015 · Halden discs"),
}
SCENARIO_TITLE = {"A": "Fit A · Zacharie-Aubrun + Onofri (standard UO₂), equal weights",
                  "B": "Fit B · all four papers, equal weights",
                  "C": "Fit C · all four papers, weighted by quality rank × relevance"}
SCENARIO_SHORT = {"A": "fit A (Zacharie+Onofri)", "B": "fit B (all papers)", "C": "fit C (all papers, weighted)"}
SCENARIO_COLOUR = {"A": "#1f5fbf", "B": "#d0621c", "C": "#138a5a"}


# ---------------------------------------------------------------------------
# DATA
# ---------------------------------------------------------------------------

def all_points(path=None):
    from hbs_dataset import dataset_dir, load_points
    folder = dataset_dir(path)
    if folder is None:
        raise FileNotFoundError("JSON datasets not found (%s)" % (path or "data/"))
    points, notes = load_points(folder, fabrication_porosity=FABRICATION_POROSITY, grain_radius=GRAIN_RADIUS,
                                theta_measured=theta_measured, measured_radius=measured_radius)
    return folder, points, notes


def scenario_points(points, name, ranks=None, rank_weights=False):
    """Copies of the targets of data set A, B or C, with their weight `w`."""
    sel = [dict(p) for p in points if name != "A" or p["group"] in ("ZAC", "ONO")]
    if ranks:
        sel = [p for p in sel if p["rank"] in ranks]
    for p in sel:
        p["w"] = p["w_study"] if (name == "C" or rank_weights) else 1.0
    return sel


def pack(sel, obs):
    s = [p for p in sel if p["obs"] == obs]
    return {"burnup": np.array([p["bu"] for p in s]), "temperature": np.array([p["T"] for p in s]),
            "porosity": np.array([p["porosity"] for p in s]),
            "grain_radius": np.array([p["grain_radius"] for p in s]),
            "label": [p["label"] for p in s], "y": np.array([p["y"] for p in s]),
            "w": np.array([p["w"] for p in s]), "points": s}


def load_targets(path=None, ranks=None, rank_weights=False, scenario="A"):
    """(theta, size, fraction) target dicts. A .csv path uses the legacy spreadsheet copy (data set A only)."""
    path = path or DATA_FILE
    if os.path.isfile(path) and path.endswith(".csv"):
        rows = [r for r in load_ebsd(path) if r["burnup"] > 0.0]

        def legacy(subset, values):
            return {"w": np.ones(len(subset)), "burnup": np.array([r["burnup"] for r in subset]),
                    "temperature": np.array([r["temperature"] for r in subset]),
                    "porosity": np.array([r["porosity"] for r in subset]),
                    "grain_radius": np.array([r["grain_radius"] for r in subset]),
                    "label": [r["label"] for r in subset], "y": np.array(values), "points": []}
        with_size = [r for r in rows if not math.isnan(measured_radius(r))]
        with_fraction = [r for r in rows if not math.isnan(r["f10"])]
        return (legacy(rows, [theta_measured(r) for r in rows]),
                legacy(with_size, [measured_radius(r) for r in with_size]),
                legacy(with_fraction, [r["f10"] / 100.0 for r in with_fraction]))
    _, points, _ = all_points(path)
    sel = scenario_points(points, scenario, ranks, rank_weights)
    return pack(sel, "theta"), pack(sel, "radius"), pack(sel, "fraction")


# ---------------------------------------------------------------------------
# THE FIT
# ---------------------------------------------------------------------------

def predict(parameters, targets):
    """(Theta [deg], r_n [m], X [-]) of the model on a target set."""
    theta, radius, fraction = [], [], []
    for burnup, temperature, porosity, grain_radius in zip(
            targets["burnup"], targets["temperature"], targets["porosity"], targets["grain_radius"]):
        state = hbs_state(float(burnup), float(temperature), porosity=float(porosity),
                          grain_radius_m=float(grain_radius), parameters=parameters)
        theta.append(state.theta_deg)
        radius.append(state.subgrain_radius_m)
        fraction.append(state.restructured_fraction)
    return np.array(theta), np.array(radius), np.array(fraction)


def build_parameters(vector, n_families, fixed_rho_c=None):
    """A `ModelParameters` from the optimizer's vector, cast to plain floats (for the paste block)."""
    if fixed_rho_c is None:
        k_sweep, beta, rho_c = vector[0], vector[1], 10.0 ** vector[2]
    else:
        k_sweep, beta, rho_c = vector[0], vector[1], fixed_rho_c
    return ModelParameters(n_families=float(n_families), beta=float(beta),
                           k_sweep=float(k_sweep), rho_c=float(rho_c))


def _variance(t):
    v = float(t["y"].var()) if len(t["y"]) else 1.0
    return v if v > 0 else 1.0


def objective(vector, theta, size, fraction, weight, fraction_weight,
              variance_theta, variance_radius, variance_fraction, n_families, fixed_rho_c):
    parameters = build_parameters(vector, n_families, fixed_rho_c)
    cost = 0.0
    if len(theta["y"]):
        theta_model, _, _ = predict(parameters, theta)
        if not np.all(np.isfinite(theta_model)):
            return 1.0e6
        cost += float(np.average((theta_model - theta["y"]) ** 2, weights=theta["w"]) / variance_theta)
    if weight > 0.0 and len(size["y"]):
        _, radius_model, _ = predict(parameters, size)
        if not np.all(np.isfinite(radius_model)):
            return 1.0e6
        cost += weight * float(np.average((radius_model - size["y"]) ** 2, weights=size["w"]) / variance_radius)
    if fraction_weight > 0.0 and len(fraction["y"]):
        _, _, fraction_model = predict(parameters, fraction)
        cost += fraction_weight * float(np.average((fraction_model - fraction["y"]) ** 2, weights=fraction["w"])
                                        / variance_fraction)
    return cost


def fit(theta, size, fraction, weight=WEIGHT_DEFAULT, fraction_weight=FRACTION_WEIGHT_DEFAULT,
        seeds=SEEDS_DEFAULT, n_families=N_FAMILIES, fixed_rho_c=None, maxiter=300, popsize=20, verbose=True):
    """Repeated global search. Returns (best ModelParameters, best cost, all runs)."""
    variances = (_variance(theta), _variance(size), _variance(fraction))
    bounds = [BOUNDS_K, BOUNDS_BETA] + ([BOUNDS_LOG10_RHO_C] if fixed_rho_c is None else [])
    runs = []
    for seed in range(seeds):
        result = differential_evolution(
            objective, bounds,
            args=(theta, size, fraction, weight, fraction_weight) + variances + (n_families, fixed_rho_c),
            seed=seed, tol=1e-12, maxiter=maxiter, popsize=popsize)
        parameters = build_parameters(result.x, n_families, fixed_rho_c)
        runs.append((float(result.fun), parameters))
        if verbose:
            print("    seed %d   J = %.6f   k = %8.5f   beta = %7.3f   rho_c = %.4e"
                  % (seed, result.fun, parameters.k_sweep, parameters.beta, parameters.rho_c))
    runs.sort(key=lambda item: item[0])
    return runs[0][1], runs[0][0], runs


def _metrics(observed, model):
    ok = np.isfinite(model)
    observed, model = observed[ok], model[ok]
    if not len(observed):
        return math.nan, math.nan
    residual = observed - model
    ss = np.sum((observed - observed.mean()) ** 2)
    return float(np.sqrt(np.mean(residual ** 2))), (float(1.0 - np.sum(residual ** 2) / ss) if ss > 0 else math.nan)


def scores(parameters, theta, size, fraction=None):
    """RMSE and R2 of the fitted observables."""
    result = {}
    result["rmse_theta"], result["r2_theta"] = _metrics(theta["y"], predict(parameters, theta)[0])
    result["rmse_radius"], result["r2_radius"] = _metrics(size["y"], predict(parameters, size)[1])
    if fraction is not None:
        result["rmse_fraction"], result["r2_fraction"] = _metrics(fraction["y"], predict(parameters, fraction)[2])
    return result


def paste_block(parameters, label=""):
    return "\n".join([
        "# Landau model %s" % label,
        "#   utilities/HBSformation/hbs_formation_landau.py",
        "N_FAMILIES = %r   # -" % parameters.n_families,
        "BETA       = %r   # -" % parameters.beta,
        "K_SWEEP    = %r   # -" % parameters.k_sweep,
        "RHO_C      = %r   # m^-2" % parameters.rho_c,
        "#   src/models/HighBurnupStructureFormation.C, case 4 parameter push (offsets 0-3)",
        "parameter.push_back(%r);  // n, dislocation families in a wall" % parameters.n_families,
        "parameter.push_back(%r);  // beta, wall geometry" % parameters.beta,
        "parameter.push_back(%r);  // k, sweeping" % parameters.k_sweep,
        "parameter.push_back(%r);  // rho_c, strain-field cut-off (m^-2)" % parameters.rho_c,
        "#   rho_c^(-1/2) = %.4f um" % (parameters.rho_c ** -0.5 * 1e6),
    ])


def predict_points(parameters, pts):
    out = []
    for p in pts:
        s = hbs_state(p["bu"], p["T"], porosity=p["porosity"], grain_radius_m=p["grain_radius"], parameters=parameters)
        out.append({"theta": s.theta_deg, "radius": s.subgrain_radius_m, "fraction": s.restructured_fraction}[p["obs"]])
    return np.array(out, float)


def rmse_by_obs(pts, pred):
    res = {}
    for obs in OBS:
        idx = [i for i, p in enumerate(pts) if p["obs"] == obs]
        res[obs] = _metrics(np.array([pts[i]["y"] for i in idx]), pred[idx])[0] if idx else math.nan
    return res


def fit_points(sel, weight, fraction_weight, seeds, maxiter, popsize):
    """Fit on a list of points (worker-friendly: plain arguments, returns plain values)."""
    targets = {o: pack(sel, o) for o in OBS}
    params, cost, _ = fit(targets["theta"], targets["radius"], targets["fraction"], weight, fraction_weight,
                          seeds, N_FAMILIES, None, maxiter, popsize, verbose=False)
    return params, cost


def _fit_job(job):
    key, sel, weight, fraction_weight, seeds, maxiter, popsize = job
    params, cost = fit_points(sel, weight, fraction_weight, seeds, maxiter, popsize)
    return key, params, cost


def run_jobs(jobs, n_jobs):
    if n_jobs <= 1:
        return [_fit_job(j) for j in jobs]
    with ProcessPoolExecutor(max_workers=n_jobs) as pool:
        return list(pool.map(_fit_job, jobs))


def fmt(v, obs=None):
    if v is None or (isinstance(v, float) and math.isnan(v)):
        return ""
    return float("%.4g" % (v * 1e6 if obs == "radius" else v))


# ---------------------------------------------------------------------------
# INFLUENCE OF EACH POINT
# ---------------------------------------------------------------------------

def influence(residual_fn, vector, rel_step=1e-3):
    """(leverage h_ii, Cook's distance D_i) of every weighted residual at the optimum."""
    vector = np.asarray(vector, float)
    e = residual_fn(vector)
    J = np.empty((e.size, vector.size))
    for j in range(vector.size):
        h = rel_step * max(abs(vector[j]), 1e-3)
        vp, vm = vector.copy(), vector.copy()
        vp[j] += h
        vm[j] -= h
        J[:, j] = (residual_fn(vp) - residual_fn(vm)) / (2 * h)
    J = np.nan_to_num(J, nan=0.0, posinf=0.0, neginf=0.0)
    hat = np.clip(np.diag(J @ np.linalg.pinv(J.T @ J) @ J.T), 0.0, 0.999)
    p = vector.size
    s2 = float(np.sum(e ** 2) / max(e.size - p, 1))
    cook = e ** 2 / (p * s2) * hat / (1.0 - hat) ** 2 if s2 > 0 else np.zeros_like(e)
    return hat, cook


def annotate(sel, params, weight, fraction_weight):
    targets = {o: pack(sel, o) for o in OBS}
    variances = {o: _variance(targets[o]) for o in OBS}
    lam = {"theta": 1.0, "radius": weight, "fraction": fraction_weight}

    def residuals(vector):
        pr = build_parameters(vector, N_FAMILIES)
        out = []
        for i, obs in enumerate(OBS):
            t = targets[obs]
            if len(t["y"]):
                model = np.nan_to_num(predict(pr, t)[i], nan=0.0)
                out.append(np.sqrt(lam[obs] * t["w"] / t["w"].sum() / variances[obs]) * (model - t["y"]))
        return np.concatenate(out)

    lev, cook = influence(residuals, [params.k_sweep, params.beta, math.log10(params.rho_c)])
    k = 0
    for i, obs in enumerate(OBS):
        t = targets[obs]
        n = len(t["y"])
        if not n:
            continue
        for p, m, h, c in zip(t["points"], predict(params, t)[i], lev[k:k + n], cook[k:k + n]):
            p["pred"], p["lev"], p["cook"] = float(m), float(h), float(c)
        k += n


# ---------------------------------------------------------------------------
# FRONT
# ---------------------------------------------------------------------------

def pareto_mask(values):
    """True for the rows not dominated (<= in every column, < in one) by any other row."""
    values = np.asarray(values, float)
    mask = np.ones(len(values), bool)
    for i in range(len(values)):
        if not np.all(np.isfinite(values[i])):
            mask[i] = False
            continue
        others = np.delete(values, i, axis=0)
        others = others[np.all(np.isfinite(others), axis=1)]
        if np.any(np.all(others <= values[i], axis=1) & np.any(others < values[i], axis=1)):
            mask[i] = False
    return mask


def read_front():
    rows = []
    with open(os.path.join(OUT, "front.csv"), newline="") as f:
        for r in csv.DictReader(f):
            rows.append({k: (v == "True" if k == "pareto_all_data" else v if k == "data_set" else float(v)) for k, v in r.items()})
    return rows


def draw_front(rows, scenarios, best):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.colors import LogNorm, Normalize

    wx_norm = LogNorm(0.05, max(FRONT_W_X))
    cmap = plt.get_cmap("viridis")
    lim = {"theta": (1.6, 2.7), "fraction": (0.16, 0.32), "radius": (0.07, 0.26)}   # radius in um
    sums = [r["all_error_sum"] for r in rows]
    sum_norm = Normalize(min(sums), min(max(sums), min(sums) + 1.0), clip=True)
    fig, axes = plt.subplots(len(scenarios), 4, figsize=(21, 4.9 * len(scenarios)), squeeze=False)
    for i, s in enumerate(scenarios):
        rs = [r for r in rows if r["data_set"] == s]
        size = lambda r: 30 + 160 * (FRONT_W_R.index(r["w_r"]) / (len(FRONT_W_R) - 1))  # noqa: E731
        unit = lambda o, v: v * 1e6 if o == "radius" else v  # noqa: E731
        off = [r for r in rs if unit("radius", r["all_rmse_radius"]) > lim["radius"][1] or r["all_rmse_theta"] > lim["theta"][1]]
        for j, (xo, yo) in enumerate((("theta", "fraction"), ("theta", "radius"), ("fraction", "radius"))):
            ax = axes[i, j]
            shown = [r for r in rs if r not in off]
            sc = ax.scatter([unit(xo, r["all_rmse_" + xo]) for r in shown], [unit(yo, r["all_rmse_" + yo]) for r in shown],
                            s=[size(r) for r in shown], c=[max(r["w_X"], 0.05) for r in shown], cmap=cmap, norm=wx_norm,
                            edgecolors=["#000" if r["pareto_all_data"] else "#bbb" for r in shown],
                            linewidths=[1.8 if r["pareto_all_data"] else 0.6 for r in shown], zorder=3)
            if best in shown:
                ax.annotate("best Σ: w_r=%g, w_X=%g" % (best["w_r"], best["w_X"]),
                            (unit(xo, best["all_rmse_" + xo]), unit(yo, best["all_rmse_" + yo])),
                            fontsize=7, xytext=(6, -12), textcoords="offset points")
            ax.set_xlim(*lim[xo])
            ax.set_ylim(*lim[yo])
            ax.set_xlabel("RMSE %s, all data" % OBS_LABEL[xo])
            ax.set_ylabel("RMSE %s, all data" % OBS_LABEL[yo])
            ax.set_title("%s\n%s" % (SCENARIO_TITLE[s], "%d degenerate fits off scale (w_r = 0: radius not constrained)" % len(off) if off else ""),
                         fontsize=8.5)
            ax.grid(alpha=0.25)
            if j == 2:
                fig.colorbar(sc, ax=ax, fraction=0.046, pad=0.02).set_label("w_X, fraction weight (0 drawn at 0.05)", fontsize=8)
        ax = axes[i, 3]
        grid = np.array([[next(r["all_error_sum"] for r in rs if r["w_r"] == wr and r["w_X"] == wx)
                          for wx in FRONT_W_X] for wr in FRONT_W_R])
        im = ax.imshow(grid, origin="lower", cmap="magma_r", aspect="auto", norm=sum_norm)
        for a_ in range(len(FRONT_W_R)):
            for b_ in range(len(FRONT_W_X)):
                r = next(r for r in rs if r["w_r"] == FRONT_W_R[a_] and r["w_X"] == FRONT_W_X[b_])
                dark = sum_norm(grid[a_, b_]) > 0.55
                ax.text(b_, a_, "%.2f%s\nk=%.2g\nℓ=%.0f nm" % (grid[a_, b_], "*" if r["pareto_all_data"] else "",
                                                               r["k"], r["cutoff_um"] * 1e3),
                        ha="center", va="center", fontsize=6.5, color="#fff" if dark else "#000")
        ax.set_xticks(range(len(FRONT_W_X)), [str(v) for v in FRONT_W_X])
        ax.set_yticks(range(len(FRONT_W_R)), [str(v) for v in FRONT_W_R])
        ax.set_xlabel("w_X, fraction weight")
        ax.set_ylabel("w_r, radius weight")
        ax.set_title("%s · Σ RMSE/σ on all data (lower is better)\nk = fitted k, ℓ = ρc^-½ cut-off length, * = Pareto-optimal" % SCENARIO_SHORT[s],
                     fontsize=8.5)
        fig.colorbar(im, ax=ax, fraction=0.046, pad=0.02).set_label("Σ RMSE/σ over Θ, X, r_n (clipped)", fontsize=8)
    fig.suptitle("Calibration front of the Landau model: one fit per (data set, w_r, w_X), errors on ALL the data\n"
                 "marker colour = w_X · marker size = w_r (0, 0.01, 0.05, 0.2, 1) · black ring = not dominated by any other fit "
                 "(all data sets pooled)", fontsize=11)
    fig.tight_layout(rect=(0, 0, 1, 0.95))
    fig.savefig(os.path.join(OUT, "front.png"), dpi=120)
    plt.close(fig)


def run_front(args):
    os.makedirs(OUT, exist_ok=True)
    if args.replot:
        rows = read_front()
        scenarios = [s for s in "ABC" if any(r["data_set"] == s for r in rows)]
        draw_front(rows, scenarios, min(rows, key=lambda r: r["all_error_sum"]))
        print("redrawn: %s/front.png" % OUT)
        return 0
    folder, points, _ = all_points(args.data if os.path.isdir(str(args.data)) else None)
    everything = scenario_points(points, "B")
    scale = {o: float(np.std([p["y"] for p in everything if p["obs"] == o])) for o in OBS}
    scenarios = [s for s in args.scenarios if s in "ABC"]
    jobs = [((s, wr, wx), scenario_points(points, s), wr, wx, args.seeds, args.maxiter, args.popsize)
            for s in scenarios for wr in FRONT_W_R for wx in FRONT_W_X]
    print("calibration front: %d fits (%s x w_r %s x w_X %s), %d workers, dataset %s"
          % (len(jobs), "".join(scenarios), FRONT_W_R, FRONT_W_X, args.jobs, folder))
    rows = []
    for (s, wr, wx), params, cost in run_jobs(jobs, args.jobs):
        own = scenario_points(points, s)
        r_own = rmse_by_obs(own, predict_points(params, own))
        r_all = rmse_by_obs(everything, predict_points(params, everything))
        rows.append(dict(data_set=s, w_r=wr, w_X=wx, k=params.k_sweep, beta=params.beta, rho_c=params.rho_c,
                         cutoff_um=params.rho_c ** -0.5 * 1e6, J=cost,
                         **{"rmse_%s" % o: r_own[o] for o in OBS},
                         **{"all_rmse_%s" % o: r_all[o] for o in OBS},
                         all_error_sum=sum(r_all[o] / scale[o] for o in OBS)))
    vals = [[r["all_rmse_%s" % o] for o in OBS] for r in rows]
    for r, m in zip(rows, pareto_mask(vals)):
        r["pareto_all_data"] = bool(m)
    rows.sort(key=lambda r: (r["data_set"], r["w_r"], r["w_X"]))

    with open(os.path.join(OUT, "front.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        for r in rows:
            w.writerow({k: (float("%.5g" % v) if isinstance(v, float) else v) for k, v in r.items()})

    print("\n  set   w_r    w_X     k       beta    rho_c      cut-off   RMSE on all data: Θ(°)  X(–)   r_n(µm)   Σ/σ   Pareto")
    for r in rows:
        print("  %s  %5.2f  %5.2f  %7.4g  %7.4g  %9.3e  %7.4f um        %6.3f  %6.3f  %6.3f  %6.3f   %s"
              % (r["data_set"], r["w_r"], r["w_X"], r["k"], r["beta"], r["rho_c"], r["cutoff_um"],
                 r["all_rmse_theta"], r["all_rmse_fraction"], r["all_rmse_radius"] * 1e6, r["all_error_sum"],
                 "*" if r["pareto_all_data"] else ""))
    best = min(rows, key=lambda r: r["all_error_sum"])
    print("\n  smallest normalised error sum on all data: data set %s, w_r = %g, w_X = %g (Σ RMSE/σ = %.3f)"
          % (best["data_set"], best["w_r"], best["w_X"], best["all_error_sum"]))

    draw_front(rows, scenarios, best)
    print("\nwritten: %s/front.csv, front.png" % OUT)
    return 0


# ---------------------------------------------------------------------------
# STUDY AT THE CHOSEN WEIGHTS
# ---------------------------------------------------------------------------

def run_study(args):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.colors import LogNorm, Normalize

    os.makedirs(OUT, exist_ok=True)
    folder, points, notes = all_points(args.data if os.path.isdir(str(args.data)) else None)
    print("dataset: %s   w_r = %g   w_X = %g" % (folder, args.weight, args.fraction_weight))
    for n in notes:
        print("  " + n)
    everything = scenario_points(points, "B")
    groups = [g for g in GROUP_STYLE if any(p["group"] == g for p in everything)]

    jobs = [(("fit", s), scenario_points(points, s), args.weight, args.fraction_weight, args.seeds, args.maxiter,
             args.popsize) for s in "ABC"]
    jobs += [(("loo", g), [dict(p) for p in everything if p["group"] != g], args.weight, args.fraction_weight,
              args.seeds, args.maxiter, args.popsize) for g in groups]
    done = {key: (params, cost) for key, params, cost in run_jobs(jobs, args.jobs)}

    results, rows_out, decision, paste = {}, [], [], []
    for s in "ABC":
        params, cost = done[("fit", s)]
        sel = scenario_points(points, s)
        annotate(sel, params, args.weight, args.fraction_weight)
        results[s] = dict(sel=sel, params=params, cost=cost)
        paste.append(paste_block(params, "— %s, w_r = %g, w_X = %g" % (SCENARIO_TITLE[s], args.weight, args.fraction_weight)))
        print("\n%s: %s" % (SCENARIO_TITLE[s], ", ".join("%s N=%d" % (o, sum(p["obs"] == o for p in sel)) for o in OBS)))
        print("  k = %.4g  beta = %.4g  rho_c = %.4g (cut-off %.3g um)   J = %.4f"
              % (params.k_sweep, params.beta, params.rho_c, params.rho_c ** -0.5 * 1e6, cost))
        for obs in OBS:
            pts = [p for p in sel if p["obs"] == obs]
            for g in ["all"] + [x for x in GROUP_STYLE if any(p["group"] == x for p in pts)]:
                gp = pts if g == "all" else [p for p in pts if p["group"] == g]
                rmse, r2 = _metrics(np.array([p["y"] for p in gp]), np.array([p["pred"] for p in gp]))
                rows_out.append(dict(data_set=s, observable=obs, paper=g, n=len(gp), rmse=fmt(rmse, obs),
                                     r2=round(r2, 4) if (g == "all" or len(gp) > 2) and not math.isnan(r2) else "",
                                     sum_leverage=round(sum(p["lev"] for p in gp), 3),
                                     sum_cook=round(sum(p["cook"] for p in gp), 3),
                                     weight_share=round(sum(p["w"] for p in gp) / sum(p["w"] for p in pts), 3)))
                if g == "all":
                    print("    %-8s N=%2d  RMSE %.4g%s  R2 %+.3f" % (obs, len(gp), fmt(rmse, obs), " um" if obs == "radius" else "", r2))

    print("\nleave one paper out (all data, equal weights):")
    loo_pred = np.full(len(everything), math.nan)
    for g in groups:
        params, _ = done[("loo", g)]
        idx = [i for i, p in enumerate(everything) if p["group"] == g]
        test = [everything[i] for i in idx]
        loo_pred[idx] = predict_points(params, test)
        r = rmse_by_obs(test, loo_pred[idx])
        print("  without %-4s k=%.3g beta=%.3g rho_c=%.3g   held-out RMSE  Θ %s   X %s   r_n %s um"
              % (g, params.k_sweep, params.beta, params.rho_c, fmt(r["theta"]), fmt(r["fraction"]), fmt(r["radius"], "radius")))
    loo = rmse_by_obs(everything, loo_pred)

    for s in "ABC":
        res = results[s]
        ins = rmse_by_obs(res["sel"], np.array([p["pred"] for p in res["sel"]]))
        alld = rmse_by_obs(everything, predict_points(res["params"], everything))
        pr = res["params"]
        row = dict(data_set=s, w_r=args.weight, w_X=args.fraction_weight,
                   k=float("%.6g" % pr.k_sweep), beta=float("%.6g" % pr.beta), rho_c=float("%.6g" % pr.rho_c),
                   cutoff_um=float("%.4g" % (pr.rho_c ** -0.5 * 1e6)))
        for obs in OBS:
            row["in_sample_rmse_" + obs] = fmt(ins[obs], obs)
            row["all_data_rmse_" + obs] = fmt(alld[obs], obs)
            row["leave_one_paper_out_rmse_" + obs] = fmt(loo[obs], obs) if s == "B" else ""
        decision.append(row)
    print("\n  set  in sample Θ | X | r_n       all data Θ | X | r_n       leave one paper out (fit B)")
    for r in decision:
        trip = lambda pre: " | ".join("%6s" % r[pre + o] for o in ("theta", "fraction", "radius"))  # noqa: E731
        print("  %s    %-24s  %-24s  %s" % (r["data_set"], trip("in_sample_rmse_"), trip("all_data_rmse_"),
                                          trip("leave_one_paper_out_rmse_")))

    def write_csv(name, rows):
        with open(os.path.join(OUT, name), "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=list(rows[0]))
            w.writeheader()
            w.writerows(rows)
    write_csv("decision.csv", decision)
    write_csv("metrics.csv", rows_out)
    with open(os.path.join(OUT, "points.csv"), "w", newline="") as f:
        keys = ["data_set", "obs", "group", "label", "rank", "relevance", "conditions", "bu", "T", "y", "w", "pred", "lev", "cook"]
        wr = csv.DictWriter(f, fieldnames=keys, extrasaction="ignore")
        wr.writeheader()
        for s, res in results.items():
            for p in res["sel"]:
                wr.writerow(dict(p, data_set=s))
    with open(os.path.join(OUT, "paste_blocks.txt"), "w") as f:
        f.write("\n\n".join(paste) + "\n")
    draw_study_figures(plt, LogNorm, Normalize, results, args)
    print("\nwritten: %s/{decision.csv, paste_blocks.txt, metrics.csv, points.csv, weights_landau.png, "
          "influence_landau.png, curves.png}" % OUT)
    return 0


def legend_handles():
    from matplotlib.lines import Line2D
    return [Line2D([], [], marker=m, linestyle="", markerfacecolor="white", markeredgecolor=e,
                   markeredgewidth=1.2, markersize=7, label=l) for m, e, l in GROUP_STYLE.values()]


def draw_study_figures(plt, LogNorm, Normalize, results, args):
    idx = {"theta": "theta_deg", "radius": "subgrain_radius_m", "fraction": "restructured_fraction"}
    weights_text = "w_r = %g, w_X = %g" % (args.weight, args.fraction_weight)

    def field(params, obs):
        return lambda B, TT: np.vectorize(lambda b, t: getattr(hbs_state(float(b), float(t), parameters=params), idx[obs]))(B, TT)

    def point_map(ax, pts, key, fn, obs, title, norm, cmap):
        B, TT = np.meshgrid(np.linspace(0, 190, 96), np.linspace(573, 1373, 41))
        Z = fn(B, TT) * (1e6 if obs == "radius" else 1.0)
        levels = {"radius": [0.1, 0.2, 0.3, 0.5, 0.8, 1.2, 2, 3], "theta": [0.5, 1, 2, 4, 6, 8, 9.5],
                  "fraction": [0.05, 0.2, 0.5, 0.8, 0.95]}[obs]
        cs = ax.contour(B, TT - 273.15, Z, levels=levels, colors="#8a8f98", linewidths=0.8)
        ax.clabel(cs, fontsize=6.5, fmt="%g")
        wsum = sum(p["w"] for p in pts) or 1.0
        sc = None
        for g, (marker, edge, _) in GROUP_STYLE.items():
            gp = [p for p in pts if p["group"] == g]
            if gp:
                sc = ax.scatter([p["bu"] for p in gp], [p["T"] - 273.15 for p in gp],
                                s=[14 + 900 * p["w"] / wsum for p in gp], c=[max(p[key], 1e-6) for p in gp],
                                cmap=cmap, norm=norm, marker=marker, edgecolors=edge, linewidths=1.2, zorder=3)
        ax.set_title(title, fontsize=9)
        ax.set_xlim(0, 190)
        ax.set_ylim(300, 1000)
        ax.grid(alpha=0.2)
        return sc

    def map_figure(key, cbar, norm_fn, path, text):
        fig, axes = plt.subplots(3, 3, figsize=(15, 13), sharex=True, sharey=True)
        for i, s in enumerate("ABC"):
            for j, obs in enumerate(OBS):
                pts = [p for p in results[s]["sel"] if p["obs"] == obs]
                allv = [p[key] for n in "ABC" for p in results[n]["sel"] if p["obs"] == obs]
                sc = point_map(axes[i, j], pts, key, field(results[s]["params"], obs), obs,
                               "%s · %s" % (SCENARIO_SHORT[s], OBS_LABEL[obs]), norm_fn(allv), plt.get_cmap("viridis"))
                if sc is not None:
                    fig.colorbar(sc, ax=axes[i, j], fraction=0.046, pad=0.02).set_label(cbar, fontsize=8)
            axes[i, 0].set_ylabel("local temperature (°C)")
        for ax in axes[-1]:
            ax.set_xlabel("local burnup (GWd/tU)")
        fig.legend(handles=legend_handles(), loc="upper center", bbox_to_anchor=(0.5, 0.97), ncol=5, frameon=False, fontsize=9)
        fig.suptitle("Landau model (k, β, ρc), %s · grey lines: fitted model · marker size: weight given to the point\n%s"
                     % (weights_text, text), y=0.99, fontsize=11)
        fig.text(0.5, 0.915, "   ".join(SCENARIO_TITLE[n] for n in "ABC"), ha="center", va="top", fontsize=8.5, color="#444")
        fig.tight_layout(rect=(0, 0, 1, 0.89))
        fig.savefig(path, dpi=130)
        plt.close(fig)

    map_figure("lev", "leverage (0–1)", lambda v: Normalize(0, max(v)), os.path.join(OUT, "weights_landau.png"),
               "colour: leverage = how strongly the point pulls the fitted value at its own position")
    map_figure("cook", "Cook's distance", lambda v: LogNorm(max(min(x for x in v if x > 0), 1e-4), max(v), clip=True),
               os.path.join(OUT, "influence_landau.png"),
               "colour: Cook's distance = how much the fitted parameters change if the point is removed")

    T_REF, T_LINES = 450.0, (400.0, 650.0, 850.0)
    scale = {"fraction": 1.0, "theta": 1.0, "radius": 1e6}
    bu = np.linspace(0, 190, 191)
    all_pts = results["B"]["sel"]
    tnorm, tcmap = Normalize(300, 1000), plt.get_cmap("plasma")

    def curve(params, obs, TC):
        return np.array([getattr(hbs_state(b, TC + 273.15, parameters=params), idx[obs]) for b in bu]) * scale[obs]

    fig, axes = plt.subplots(3, 3, figsize=(16, 14))
    for i, obs in enumerate(("fraction", "theta", "radius")):
        pts = [p for p in all_pts if p["obs"] == obs]
        y = lambda gp: [p["y"] * scale[obs] for p in gp]  # noqa: E731
        ax = axes[i, 0]
        for s in "ABC":
            ax.plot(bu, curve(results[s]["params"], obs, T_REF), "-", color=SCENARIO_COLOUR[s], lw=1.6,
                    label="Landau model, %s" % SCENARIO_SHORT[s])
        for g, (marker, edge, _) in GROUP_STYLE.items():
            gp = [p for p in pts if p["group"] == g]
            ax.scatter([p["bu"] for p in gp], y(gp), marker=marker, facecolors="none", edgecolors=edge, s=40, zorder=3)
        ax.set_title("%s vs local burnup\nlines: fits drawn at %d °C" % (OBS_LABEL[obs], T_REF), fontsize=9)
        ax.set_xlabel("local burnup (GWd/tU)")
        ax.set_ylabel(OBS_LABEL[obs])
        ax = axes[i, 1]
        for g, (marker, edge, _) in GROUP_STYLE.items():
            gp = [p for p in pts if p["group"] == g]
            ax.scatter([p["T"] - 273.15 for p in gp], y(gp), marker=marker, facecolors="none", edgecolors=edge, s=40, zorder=3)
        ax.set_title("%s vs local temperature\ndata only" % OBS_LABEL[obs], fontsize=9)
        ax.set_xlabel("local temperature (°C)")
        ax.set_xlim(300, 1000)
        ax = axes[i, 2]
        for TC in T_LINES:
            ax.plot(bu, curve(results["B"]["params"], obs, TC), "-", color=tcmap(tnorm(TC)), lw=1.5,
                    label="Landau model, fit B, %d °C" % TC)
        sc = None
        for g, (marker, edge, _) in GROUP_STYLE.items():
            gp = [p for p in pts if p["group"] == g]
            if gp:
                sc = ax.scatter([p["bu"] for p in gp], y(gp), marker=marker, c=[p["T"] - 273.15 for p in gp],
                                cmap=tcmap, norm=tnorm, edgecolors="#222", linewidths=0.5, s=45, zorder=3)
        fig.colorbar(sc, ax=ax, fraction=0.046, pad=0.02).set_label("local temperature (°C)", fontsize=8)
        ax.set_title("%s vs local burnup · colour = local temperature\nlines: fit B at 400, 650, 850 °C (they overlap)"
                     % OBS_LABEL[obs], fontsize=9)
        ax.set_xlabel("local burnup (GWd/tU)")
        for a in axes[i]:
            a.grid(alpha=0.2)
            if obs == "radius" and a is not axes[i, 1]:
                a.set_ylim(0, 1.0)
    fig.legend(handles=legend_handles(), loc="upper center", ncol=5, frameon=False, fontsize=8.5)
    h1, l1 = axes[0, 0].get_legend_handles_labels()
    h3, l3 = axes[0, 2].get_legend_handles_labels()
    fig.legend(h1, l1, loc="lower left", bbox_to_anchor=(0.02, 0.0), ncol=3, frameon=False, fontsize=8, title="left column (%s)" % weights_text, title_fontsize=8)
    fig.legend(h3, l3, loc="lower right", bbox_to_anchor=(0.98, 0.0), ncol=3, frameon=False, fontsize=8, title="right column", title_fontsize=8)
    fig.tight_layout(rect=(0, 0.05, 1, 0.965))
    fig.savefig(os.path.join(OUT, "curves.png"), dpi=130)
    plt.close(fig)


# ---------------------------------------------------------------------------
# SINGLE FIT
# ---------------------------------------------------------------------------

def run_fit(args):
    ranks = args.ranks.split(",") if args.ranks else None
    theta, size, fraction = load_targets(args.data, ranks, args.rank_weights, args.scenario)
    print("Calibration of the Landau HBS-formation model")
    print("  data   %s   %s%s%s" % (args.data, SCENARIO_TITLE[args.scenario], "   ranks " + args.ranks if ranks else "",
                                     "   rank x relevance weights" if args.rank_weights else ""))
    print("  Theta  N = %2d   sizes  N = %2d   fractions  N = %2d" % (len(theta["y"]), len(size["y"]), len(fraction["y"])))
    print("  w_r = %g (sizes)   w_X = %g (fraction)   seeds = %d   n = %g" % (args.weight, args.fraction_weight, args.seeds, args.n))
    fixed_rho_c = args.fix_rho_c ** -2.0 if args.fix_rho_c is not None else None
    if fixed_rho_c is not None:
        print("  rho_c fixed at %.4e m^-2  (%.3f um)" % (fixed_rho_c, args.fix_rho_c * 1e6))
    print()
    parameters, cost, _ = fit(theta, size, fraction, args.weight, args.fraction_weight, args.seeds, args.n,
                              fixed_rho_c, args.maxiter, args.popsize)
    score = scores(parameters, theta, size, fraction)
    print()
    print("  best   J = %.6f" % cost)
    print("    Theta   N = %2d   RMSE = %.4f deg   R2 = %.4f" % (len(theta["y"]), score["rmse_theta"], score["r2_theta"]))
    print("    r_n     N = %2d   RMSE = %.4f um    R2 = %.4f" % (len(size["y"]), score["rmse_radius"] * 1e6, score["r2_radius"]))
    print("    X       N = %2d   RMSE = %.4f       R2 = %.4f" % (len(fraction["y"]), score["rmse_fraction"], score["r2_fraction"]))
    print()
    print("  Zacharie-Aubrun + Onofri EBSD rows with the fitted parameters:")
    validate(path=args.data, parameters=parameters)
    print()
    print(paste_block(parameters, "— %s, w_r = %g, w_X = %g" % (SCENARIO_TITLE[args.scenario], args.weight, args.fraction_weight)))
    return 0


def main(argv=None):
    parser = argparse.ArgumentParser(
        description="Calibration of the Landau HBS-formation model (single fit, --front, or --study).",
        formatter_class=argparse.RawDescriptionHelpFormatter)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--front", action="store_true",
                      help="calibration front over (w_r, w_X) for each data set -> figures/calibration/front.*")
    mode.add_argument("--study", action="store_true",
                      help="data sets A, B, C at the chosen weights: point influence, all-data scores, "
                           "leave one paper out -> figures/calibration/decision.csv")
    parser.add_argument("--scenario", choices=("A", "B", "C"), default=SCENARIO_DEFAULT,
                        help="single fit: data set (A Zacharie+Onofri; B all papers; C all papers, weighted)")
    parser.add_argument("--scenarios", default="ABC", help="front: data sets to scan (default ABC)")
    parser.add_argument("--replot", action="store_true", help="front: redraw front.png from front.csv without fitting")
    parser.add_argument("--weight", type=float, default=WEIGHT_DEFAULT, metavar="W_R",
                        help="weight of the size term (default %g)" % WEIGHT_DEFAULT)
    parser.add_argument("--fraction-weight", dest="fraction_weight", type=float, default=FRACTION_WEIGHT_DEFAULT,
                        metavar="W_X", help="weight of the fraction term (default %g)" % FRACTION_WEIGHT_DEFAULT)
    parser.add_argument("--seeds", type=int, default=SEEDS_DEFAULT, metavar="N", help="global searches per fit")
    parser.add_argument("--n", type=float, default=N_FAMILIES, metavar="N", help="dislocation families, fixed")
    parser.add_argument("--fix-rho-c", type=float, default=None, metavar="R",
                        help="single fit: fix rho_c to R^-2 with R a length in metres")
    parser.add_argument("--data", default=DATA_FILE, metavar="PATH",
                        help="JSON dataset folder (default %(default)s) or a legacy .csv (data set A only)")
    parser.add_argument("--ranks", default=None, metavar="A,B,...", help="single fit: keep values of these Rose ranks")
    parser.add_argument("--rank-weights", dest="rank_weights", action="store_true",
                        help="single fit: weight the targets by rank x relevance (data set C does this already)")
    parser.add_argument("--jobs", type=int, default=max(1, (os.cpu_count() or 2) - 2), metavar="N",
                        help="parallel fits for --front and --study (default: cores - 2)")
    parser.add_argument("--maxiter", type=int, default=300)
    parser.add_argument("--popsize", type=int, default=20)
    args = parser.parse_args(argv)
    if args.front:
        return run_front(args)
    if args.study:
        return run_study(args)
    return run_fit(args)


if __name__ == "__main__":
    sys.exit(main())
