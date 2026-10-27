"""
Size of the RVE of the bubble cubes of test_rve.py and fission gas release (FGR) of every cube as in OperaHPC D5.2 (Section 2.2, Fig. 7).

Folders of ROOT:
    side<L>_seed<NN>             one cube per seed; several sides L are allowed 
    side<L>_seed01_<variant>     the pores of side<L>_seed01 with another mesh (meshdouble, wall<h>_bulk<H>); a variant
                                 ending in _refinement (or _refinement<k>, _r<k>) is the mesh of its summary refined k
                                 times in MMM (test-polybubble -r k, k = 1 if not written), e.g. meshdouble_refinement
each with bubbles_<run>.txt and summary_<run>.txt of Mérope, bubbles_and_stresses_selected.txt and sig_hydro.txt of MMM.

Stress factor of a pore (MMM runs with the same pressure p in every pore):
    sigma  sigma_I_max / p, the largest first principal stress near the pore, at zero macroscopic strain (MMM)
    s_hat  (sigma + h) / (1 - h), the same at zero macroscopic stress, with h p = xi p - (1 - xi) <sigma_h>, <sigma_h> the
           mean hydrostatic stress of the matrix (sig_hydro.txt) and xi the porosity: the rescaling of D5.2, exact with
           one pressure in all the pores. It is the quantity analysed when every cube has sig_hydro.txt.
A pore breaks when s_hat p > sigma_R, i.e. above its critical pressure p_c = sigma_R / s_hat, and (D5.2, Section 2.2.2)
    FGR(p) = volume of the broken pores / volume of all the pores
depends on x = p / sigma_R only: one MMM run gives the whole curve, for any sigma_R.

The analysis answers four questions, in this order:
 1. Is the mesh fine enough?                    side<L>_seed01 with all its meshes          mesh_test.txt, mesh_test.png
 2. Does a cube hold the whole population?      classes expected and placed by Mérope       top of rve_size.txt
 3. How much does the FGR change between seeds? the curves of D5.2 Fig. 7 with their spread rve_fgr.txt, rve_fgr.png
 4. Which side gives the precision asked?       Kanit, on the FGR and on the class means    rve_fgr.txt, rve_size.txt/png
A mesh refined in MMM has smaller elements but the pore surfaces of its coarse mesh: mesh_test.txt (1b) tells how much of
the change from the coarse mesh to the mesh of gmsh of the same size the refinement recovers.

Kanit et al., Int. J. Solids Struct. 40 (2003) 3647, in short. A property Z computed on a cube of volume V changes from
one random cube (seed) to the other: D^2(V), its variance between cubes, decreases with the volume as D^2 ~ V^-alpha.
alpha = 1 when Z averages independent contributions (integral range A: D^2 = S^2 A / V, with S^2 the variance of one
contribution). With n cubes the mean of Z is known within 2 D / sqrt(n) (95 %), so an RVE is a pair (volume, number of
cubes) giving the mean within the precision asked; "one cube is an RVE" means n = 1, i.e. eps = 2 D / mean <= EPS_R.
With a single side, the side for eps = EPS_R is extrapolated with alpha = 1: (eps / EPS_R)^2 cubes of side L hold as many
independent contributions as one cube of side L (eps / EPS_R)^(2/3). A small cube may also bias the mean, which more seeds
do not correct: only cubes of another side can show it.
Z is here
  - the critical pressure x_c of a cube at a given level of FGR: 5 % on x_c is 5 % on the sigma_R calibrated on the
    measured release. The onset (5 %, one or two large pores in a small cube) is a tail and stays out of the verdict;
  - the mean of the stress factor over the pores of one class of radius in a cube. A x pore density of the class is 1 if
    its pores are independent. Only the classes holding at least MIN_VOLUME_SHARE of the pore volume enter the verdict,
    since the FGR weighs the pores by their volume.

Usage
    python3 plot_bubbles.py [ROOT]     default ROOT below
writes in ROOT: mesh_test.txt/png, rve_fgr.txt/png, rve_size.txt/png and rve_scaling.png (several sides).
Exit 1 if one cube of the main side is not an RVE, or if the mesh of the runs is more than EPS_MESH from the reference.

@author E. Cappellari
@date   October 2026
"""

import glob
import os
import re
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

ROOT = os.path.join("test_rve_side10", "run_NFIR")
EPS_R = 0.05             # precision asked on a mean (relative, 95 %)
EPS_MESH = 0.02          # tolerance on the class means between the mesh of the runs and the reference mesh
PREF = 1.0               # --pref of the MMM runs: the pressure of a pore is PREF x its pressure in the bubble file
FGR_LEVELS = (0.05, 0.10, 0.25, 0.50, 0.75, 0.90)  # levels of FGR at which the critical pressures of the cubes are compared
VERDICT_LEVELS = (0.10, 0.90)                      # levels of FGR used in the verdict: the onset (5 %) is a tail
MIN_VOLUME_SHARE = 0.05  # classes holding less of the pore volume barely move the FGR: not used in the verdict
SIGMA_R_MPA = 275.0      # MPa, rupture stress of D5.2 (monodisperse pores): only for the pressure axis of rve_fgr.png
D52_SIDE = 30.0          # um, side of the RVE of D5.2 (10 seeds, about 1200 pores of one size)
EXPERIMENT = None        # optional csv with a header and the columns p_MPa,FGR_percent, drawn on rve_fgr.png

STRESSES, HYDRO = "bubbles_and_stresses_selected.txt", "sig_hydro.txt"
CASE = re.compile(r"side(?P<side>\d+(?:\.\d+)?)_seed(?P<seed>\d+)(?:_(?P<variant>.+))?$")
REFINED = re.compile(r"(?:^|_)(?:refinement|refined|refine|r)(?P<levels>\d*)$")  # end of a variant refined in MMM
LABEL = {"sigma": r"$\sigma_I / p$", "s_hat": r"$\hat s$"}
SEQ_BLUE, GRID = ("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281"), "#e1e0d9"
REFERENCE_LINE, D52_COLOR, EXPERIMENT_COLOR = "#8a8a85", "#c2410c", "#d62728"
REFINED_COLOR = "#e0a030"  # meshes refined in MMM, hatched


# ---- Reading a solved cube --------------------------------------------------------------------------------------------

def read_csv(path):
    """Columns of a csv with a header, as float arrays ("nan" allowed); None if the file is missing or has no rows."""
    if path is None or not os.path.isfile(path) or os.path.getsize(path) == 0:
        return None
    with open(path) as f:
        rows = [line.strip().split(",") for line in f if line.strip()]
    if len(rows) < 2:
        return None
    data = np.array([[float(x) for x in row] for row in rows[1:]])
    return {name.strip(): data[:, k] for k, name in enumerate(rows[0])}


def nearest_gaps(centres, radii, side):
    """Distance from the wall of every pore to the wall of its nearest neighbour, in the periodic cube."""
    d = centres[:, None, :] - centres[None, :, :]
    d -= side * np.round(d / side)
    gap = np.linalg.norm(d, axis=2) - radii[:, None] - radii[None, :]
    np.fill_diagonal(gap, np.inf)
    return gap.min(axis=1)


def read_summary(folder):
    """(wall, bulk) mesh sizes (um) of a cube and its classes, from the summary of Mérope. The classes are the rows of six
    numbers of the summary: radius, pressure, porosity asked, porosity placed, pores expected, pores placed (None for a
    summary without them)."""
    summaries = glob.glob(os.path.join(folder, "summary_*.txt"))
    text = open(summaries[0]).read() if summaries else ""
    m = re.search(r"wall ([\d.eE+-]+) um, bulk ([\d.eE+-]+) um", text)
    if m:
        mesh = float(m.group(1)), float(m.group(2))
    else:
        m = re.search(r"mesh size ([\d.eE+-]+) um", text)
        mesh = (float(m.group(1)),) * 2 if m else (np.nan, np.nan)
    rows = []
    for line in text.splitlines():
        words = line.split()
        if line.lstrip().startswith("#") or len(words) != 6:
            continue
        try:
            rows.append([float(w) for w in words])
        except ValueError:
            continue
    if not rows:
        return mesh, None
    a = np.array(rows)
    return mesh, {"R": np.round(a[:, 0], 6), "asked": a[:, 2], "placed": a[:, 3], "expected": a[:, 4], "pores": a[:, 5]}


def read_cube(folder):
    """Pores of a solved cube: dict with the folder, side, seed, variant, mesh (wall and bulk sizes, divided by 2^k for a
    variant refined k times in MMM, whose geometry keeps the sizes of the summary), classes of the summary, and per pore id,
    radius R, volume V, gap to the nearest pore, sigma and s_hat (None without sig_hydro.txt); None if MMM has not run."""
    case = CASE.match(os.path.basename(folder))
    side = float(case["side"])

    # bubbles_and_stresses_selected.txt in the folder or in its shallowest subfolder
    hits = sorted(glob.glob(os.path.join(folder, "**", STRESSES), recursive=True), key=lambda p: (p.count(os.sep), p))
    if len(hits) > 1:
        print(f"{folder}: {len(hits)} {STRESSES}, reading {os.path.relpath(hits[0], folder)}")
    stresses = hits[0] if hits else None

    table = read_csv(stresses)
    if table is None:
        return None
    for column in ("bubble_id", "radius", "pressure", "sigma_I_max"):
        if column not in table:
            raise SystemExit(f"{stresses}: no column {column} (columns: {', '.join(table)})")

    pores = [p for p in glob.glob(os.path.join(folder, "bubbles_*.txt")) if os.path.basename(p) != STRESSES]
    if len(pores) != 1:
        raise SystemExit(f"{folder}: one bubbles_<run>.txt of Mérope expected, found {len(pores)}")

    bubbles = np.atleast_2d(np.loadtxt(pores[0]))  # id, x, y, z, radius, pressure
    row = {int(i): k for k, i in enumerate(bubbles[:, 0])}
    ids = table["bubble_id"].astype(int)
    if set(ids) != set(row):
        raise SystemExit(f"{folder}: the pores of {STRESSES} ({len(ids)}) are not those of {os.path.basename(pores[0])} "
                         f"({len(row)})")

    (wall, bulk), classes = read_summary(folder)
    refined = REFINED.search(case["variant"] or "")
    levels = int(refined["levels"] or 1) if refined else 0
    geometry = (wall, bulk)  # sizes of the mesh of gmsh, which also sets the pore surfaces
    wall, bulk = wall / 2**levels, bulk / 2**levels  # a uniform refinement of MFEM halves every edge
    p = PREF * table["pressure"]
    sigma = table["sigma_I_max"] / p
    R = np.round(table["radius"], 6)
    gap = nearest_gaps(bubbles[:, 1:4], bubbles[:, 4], side)[[row[i] for i in ids]]
    cube = {"folder": os.path.basename(folder), "side": side, "seed": int(case["seed"]), "variant": case["variant"],
            "wall": wall, "bulk": bulk, "refine": levels, "geometry": geometry, "classes": classes, "id": ids, "R": R, "V": 4.0 / 3.0 * np.pi * R**3,
            "gap": gap, "gap_R": gap / R, "sigma": sigma, "s_hat": None}

    if not np.isfinite(sigma).all():
        print(f"{folder}: {np.count_nonzero(~np.isfinite(sigma))} pores of {len(sigma)} without a stress (nan), left out")

    hydro = read_csv(os.path.join(os.path.dirname(stresses), HYDRO))
    if hydro is not None and {"Volume", "Average"} <= set(hydro):
        if np.ptp(p) > 0.0:
            print(f"{folder}: pores at different pressures, s_hat rescales with their mean pressure")
        xi = 1.0 - hydro["Volume"][0] / side**3
        xi_pores = np.sum(4.0 / 3.0 * np.pi * bubbles[:, 4] ** 3) / side**3
        if abs(xi - xi_pores) > 0.01:
            print(f"{folder}: porosity {xi:.4f} from the volume of {HYDRO}, {xi_pores:.4f} from the pores: wrong side or "
                  "output of another run? s_hat of this cube is suspect")
        h = xi - (1.0 - xi) * hydro["Average"][0] / p.mean()
        cube.update(xi=xi, h=h, s_hat=(sigma + h) / (1.0 - h))
    return cube


# ---- Statistics of Kanit ---------------------------------------------------------------------------------------------

def chi2_quantile(p, k):
    """Quantile p (0.025 or 0.975) of the chi-square law with k degrees of freedom (Wilson-Hilferty, within 1 % for k >= 3)."""
    z = {0.025: -1.959964, 0.975: 1.959964}[p]
    return k * (1.0 - 2.0 / (9.0 * k) + z * np.sqrt(2.0 / (9.0 * k))) ** 3


def kanit(Z, side):
    """Statistics of Kanit of a quantity measured once per cube on cubes of one side: mean, variance D^2 between the
    cubes, half-width 2 D / sqrt(n) of the mean, error eps = 2 D / mean of one cube, cubes of this side needed for EPS_R,
    side of one cube with the same precision (alpha = 1) and its 95 % interval from the uncertainty of D^2 on n cubes
    (chi-square with n - 1 degrees of freedom: with 10 cubes D^2 is known within a factor of about 2). nan below 3 cubes."""
    Z = np.asarray(Z, dtype=float)
    Z = Z[np.isfinite(Z)]
    n = len(Z)
    r = {"n": n, "side": side, "mean": Z.mean() if n else np.nan, "D2": np.nan, "half": np.nan, "eps": np.nan,
         "cubes": np.nan, "rve": np.nan, "rve_low": np.nan, "rve_high": np.nan}
    if n < 3:
        return r
    D2 = Z.var(ddof=1)
    eps = 2.0 * np.sqrt(D2) / abs(r["mean"])
    cubes = (eps / EPS_R) ** 2
    low, high = (n - 1) / chi2_quantile(0.975, n - 1), (n - 1) / chi2_quantile(0.025, n - 1)
    r.update(D2=D2, half=2.0 * np.sqrt(D2 / n), eps=eps, cubes=cubes, rve=side * np.cbrt(cubes),
             rve_low=side * np.cbrt(cubes * low), rve_high=side * np.cbrt(cubes * high))
    return r


def interval(r):
    """The 95 % interval of the side of the RVE, as text."""
    return f"{r['rve_low']:.0f}-{r['rve_high']:.0f}"


def class_values(cubes, q, R):
    """Values of the quantity q for the pores of class R, one array per cube where the class is."""
    groups = [c[q][(c["R"] == R) & np.isfinite(c[q])] for c in cubes]
    return [g for g in groups if len(g)]


def by_seed(cubes):
    """The cubes sorted by seed."""
    return sorted(cubes, key=lambda c: c["seed"])


def volume_shares(cube, radii):
    """Share of the pore volume of a cube held by every class of radii."""
    return np.array([cube["V"][cube["R"] == R].sum() for R in radii]) / cube["V"].sum()


def percent(share):
    """A share as a percentage, with one decimal below 10 %."""
    return f"{100 * share:.1f}" if share < 0.1 else f"{100 * share:.0f}"


def class_ticks(radii, shares):
    """Tick labels of the classes: radius and share of the pore volume."""
    return [f"{R:.3g}\n{percent(s)} % vol" for R, s in zip(radii, shares)]


def symlog_percent(ax):
    """Symmetric log scale for deviations in %, linear within +/- 5 %, with readable ticks."""
    ax.set_yscale("symlog", linthresh=5.0)
    low, high = ax.get_ylim()
    ticks = [t for t in (-200, -100, -50, -20, -10, -5, -2, 0, 2, 5, 10, 20, 50, 100, 200) if low <= t <= high]
    ax.set_yticks(ticks, [f"{t:g}" for t in ticks])


def class_color(k, n):
    """Colour of the class k of n (viridis)."""
    return plt.get_cmap("viridis")(k / max(n - 1, 1))


# ---- 1. Is the mesh fine enough? -------------------------------------------------------------------------------------

def richardson(h, s):
    """Observed order p and extrapolated value s0 of s(h) = s0 + C h^p from the three finest sizes (h ascending); None if s
    is not monotone in h or no order between 0.05 and 8 fits."""
    (h1, h2, h3), (s1, s2, s3) = h[:3], s[:3]
    if (s2 - s1) * (s3 - s2) <= 0.0:
        return None
    target = (s3 - s2) / (s2 - s1)
    f = lambda p: (h3**p - h2**p) / (h2**p - h1**p) - target
    low, high = 0.05, 8.0
    if f(low) * f(high) > 0.0:
        return None
    for _ in range(80):
        mid = 0.5 * (low + high)
        low, high = (low, mid) if f(low) * f(mid) <= 0.0 else (mid, high)
    p = 0.5 * (low + high)
    return p, s1 - (s2 - s1) * h1**p / (h2**p - h1**p)


def same_mesh(a, b):
    """True if the (wall, bulk) sizes a and b are the same mesh size."""
    return bool(np.isclose(a[0], b[0], rtol=1e-3) and np.isclose(a[1], b[1], rtol=1e-3))


def mesh_name(c):
    """How the mesh of a cube was made: by gmsh, or refined k times in MMM from the mesh of its summary."""
    return f"MMM -r {c['refine']}" if c["refine"] else "gmsh"


def refinement_lines(refined, gmsh, means, value, ids, used, names, width):
    """Each mesh refined in MMM against the mesh of gmsh of the same size (target) and its coarse mesh (parent): class
    means against the target, the share of the change between parent and target that the refinement recovers, and a
    verdict line per refined mesh."""
    lines, summaries = [], []
    for r in refined:
        what = (f"{r['folder']} (the {r['geometry'][0]:g} um mesh refined {r['refine']}x in MMM, -r {r['refine']}, "
                f"elements of about {r['wall']:g} um)")
        parent = next((c for c in gmsh if same_mesh((c["wall"], c["bulk"]), r["geometry"])), None)
        target = next((c for c in gmsh if same_mesh((c["wall"], c["bulk"]), (r["wall"], r["bulk"]))), None)
        if target is None:
            text = f"{what}: no mesh of gmsh at {r['wall']:g} um solved to compare with, only the table above"
            lines += ["#", f"# {text}"]
            summaries.append(text)
            continue
        m_r, m_t = means[r["folder"]], means[target["folder"]]
        left = 100.0 * (m_r / m_t - 1.0)
        pore = np.array([100.0 * (value[r["folder"]][i] / value[target["folder"]][i] - 1.0) for i in ids])
        lines += ["#", f"# 1b. Refinement in MMM against a finer mesh of gmsh: {what}",
                  f"#     against {target['folder']} ({target['wall']:g} um from gmsh)",
                  f"# {'class means against ' + target['folder'] + ' (%)':<{width}s}" + "".join(f"{n:>8s}" for n in names)]
        recovered = None
        if parent is not None:
            m_p = means[parent["folder"]]
            off = 100.0 * (m_p / m_t - 1.0)
            recovered = np.full(len(off), np.nan)
            ok = np.abs(off) > 100 * EPS_MESH  # elsewhere the coarse mesh is already good and the ratio is noise
            recovered[ok] = 100.0 * (m_r[ok] - m_p[ok]) / (m_t[ok] - m_p[ok])
            lines.append(f"# {'  ' + parent['folder'] + ' (coarse, gmsh)':<{width}s}" + "".join(f"{x:8.2f}" for x in off))
        lines.append(f"# {'  ' + r['folder'] + ' (' + mesh_name(r) + ')':<{width}s}" + "".join(f"{x:8.2f}" for x in left))
        if recovered is not None:
            lines.append(f"# {'  change recovered by the refinement (%)':<{width}s}"
                         + "".join(f"{x:8.0f}" if np.isfinite(x) else f"{'-':>8s}" for x in recovered))
            lines.append(f"#   100: as good as the mesh of gmsh at {target['wall']:g} um, 0: no gain on the coarse mesh "
                         f"(- : the coarse mesh is already within {100 * EPS_MESH:g} % there)")
        sel = used if used.any() else np.arange(len(left)) == len(left) - 1  # the classes of the verdict, or all pores
        worst = np.abs(left[sel]).max()
        verdict = "equivalent" if worst <= 100 * EPS_MESH else "NOT equivalent"
        before = (f"the {parent['wall']:g} um mesh of gmsh is off by up to {np.abs(off[sel]).max():.2f} %, "
                  if parent is not None else "")
        text = (f"refinement in MMM ({r['folder']}): on the classes of the verdict {before}refined {r['refine']}x in MMM "
                f"by up to {worst:.2f} % from the {target['wall']:g} um mesh of gmsh (per pore: median |d| "
                f"{np.median(np.abs(pore)):.2f} %, largest {np.abs(pore).max():.2f} %): {verdict} at {100 * EPS_MESH:g} %")
        lines.append(f"# {text}")
        summaries.append(text)
    return lines, summaries


def analyse_mesh(root, cubes, q):
    """Mesh test on side<L>_seed01 and its variants: class means and pores against the reference, the Richardson value
    when there are three wall sizes of gmsh at one bulk size (or three uniform meshes), otherwise the finest mesh of gmsh
    (then the error of the finest mesh is not measured). A mesh refined in MMM keeps the pore surfaces of its coarse mesh,
    so it stays out of the Richardson fit: it is compared with its coarse mesh and with the mesh of gmsh of the same size
    (refinement_lines).
    Writes mesh_test.txt and mesh_test.png. Returns (status, one line for the verdict); status 1 if the mesh of the runs is
    more than EPS_MESH from a reference that tells it, on a class used in the verdict."""
    families = {}
    for c in cubes:
        if c["seed"] == 1:
            families.setdefault(c["side"], []).append(c)
    families = {L: f for L, f in families.items() if len(f) > 1 and any(c["variant"] for c in f)}
    if not families:
        print(f"{root}: no mesh variant of a seed01 solved, no mesh test")
        return 0, "# - mesh: not tested (no variant of side<L>_seed01 solved)"
    side = max(families, key=lambda L: len(families[L]))
    family = sorted(families[side], key=lambda c: (c["refine"] > 0, c["wall"], c["bulk"]))
    gmsh, refined = [c for c in family if not c["refine"]], [c for c in family if c["refine"]]
    if not gmsh:
        print(f"{root}: only meshes refined in MMM for side{side:g}_seed01, no mesh test")
        return 0, "# - mesh: not tested (only meshes refined in MMM)"
    finest, runs = gmsh[0], next((c for c in gmsh if c["variant"] is None), None)
    ids = sorted(set.intersection(*(set(c["id"][np.isfinite(c[q])]) for c in family)))
    value = {c["folder"]: dict(zip(c["id"], c[q])) for c in family}
    R = dict(zip(finest["id"], finest["R"]))
    gap_R = dict(zip(finest["id"], finest["gap_R"]))
    radii = sorted({R[i] for i in ids})
    shares = volume_shares(finest, radii)
    used = np.append(shares >= MIN_VOLUME_SHARE, False)  # the classes of the verdict, not "all"
    means = {c["folder"]: np.array([np.mean([value[c["folder"]][i] for i in ids if R[i] == Rk]) for Rk in radii] +
                                   [np.mean([value[c["folder"]][i] for i in ids])]) for c in family}

    # reference: Richardson on three wall sizes of gmsh or more, per class and for all the pores. The meshes of a group
    # share the bulk size (graded meshes), or are all uniform (wall = bulk, "mesh size h" in the summary)
    reference, how, extrapolated = means[finest["folder"]], f"the finest mesh of gmsh, {finest['folder']}", False
    failed = None  # (group, sizes, classes) of the last Richardson fit not possible
    uniform = lambda c: bool(np.isclose(c["wall"], c["bulk"], rtol=1e-3))
    groups = [("uniform meshes", [c for c in gmsh if uniform(c)])]
    groups += [(f"bulk {bulk:g} um", [c for c in gmsh if not uniform(c) and c["bulk"] == bulk])
               for bulk in sorted({c["bulk"] for c in gmsh if not uniform(c)})]
    for where, group in groups:
        if len({c["wall"] for c in group}) >= 3:
            h = np.array([c["wall"] for c in group])
            fits = [richardson(h, np.array([means[c["folder"]][k] for c in group])) for k in range(len(radii) + 1)]
            if all(fit is not None for fit in fits):
                reference, extrapolated = np.array([fit[1] for fit in fits]), True
                how = (f"Richardson on the wall sizes {', '.join(f'{x:g}' for x in h[:3])} um ({where}), observed "
                       f"order per class {', '.join(f'{fit[0]:.2f}' for fit in fits[:-1])}, all pores {fits[-1][0]:.2f}")
                break
            bad = [n for n, fit in zip([f"{Rk:.3f}" for Rk in radii] + ["all"], fits) if fit is None]
            failed = (where, len(group), bad)
            print(f"{root}: Richardson on the {where} not possible for the classes {', '.join(bad)} (class means not "
                  "monotone in the size, or no order between 0.05 and 8)")
    coarse = gmsh if extrapolated else gmsh[1:]  # the meshes of gmsh against the reference
    compared = coarse + refined
    deviation = {c["folder"]: 100.0 * (means[c["folder"]] / reference - 1.0) for c in family}

    names = [f"{Rk:.3f}" for Rk in radii] + ["all"]
    width = 69  # label column of the tables, as the folder, wall, bulk, mesh and w/Rmin columns
    lines = [f"# 1. Is the mesh fine enough? {q} of the {len(ids)} pores of side{side:g}_seed01 with {len(family)} meshes",
             f"# - reference: {how}",
             f"# - tolerance on the class means: {100 * EPS_MESH:g} %",
             f"# - smallest radius {min(radii):.3f} um, smallest gap between two pores "
             f"{min(gap_R[i] * R[i] for i in ids):.3f} um"]
    if refined:
        lines.append("# - mesh MMM -r k: the mesh of the summary refined k times in MMM (every edge halved each time), "
                     "wall and bulk are its sizes / 2^k; its pore surfaces are those of the coarse mesh")
    lines += ["# deviation of the class means from the reference (%), then per pore from the finest mesh of gmsh: median "
              "and largest |d| (%)",
              f"# {'folder':<38s}{'wall':>7s}{'bulk':>7s}{'mesh':>10s}{'w/Rmin':>7s}" + "".join(f"{n:>8s}" for n in names)
              + f"{'med|d|':>8s}{'max|d|':>8s}",
              f"# {'share of the pore volume (%)':<{width}s}" + "".join(f"{100 * s:8.1f}" for s in shares) + f"{100.0:8.1f}"]
    for c in family:
        pore = np.array([100.0 * (value[c["folder"]][i] / value[finest["folder"]][i] - 1.0) for i in ids])
        lines.append(f"  {c['folder']:<38s}{c['wall']:7g}{c['bulk']:7g}{mesh_name(c):>10s}{c['wall'] / min(radii):7.2f}"
                     + "".join(f"{x:8.2f}" for x in deviation[c["folder"]])
                     + f"{np.median(np.abs(pore)):8.2f}{np.abs(pore).max():8.2f}")

    # verdict on the meshes of gmsh: against an extrapolated value, or (two wall sizes) the difference between the meshes
    status = 0
    small = [f"R = {radii[k]:.3g} um ({100 * shares[k]:.1f} % vol) changes by "
             f"{max(abs(deviation[c['folder']][k]) for c in coarse):.0f} %"
             for k in range(len(radii)) if not used[k]] if coarse else []
    worst = lambda c: np.abs(deviation[c["folder"]][used]).max() if used.any() else 0.0
    if extrapolated:
        if runs is None:
            summary = "extrapolated value available, but no folder without a variant (the mesh of the runs) to compare"
        else:
            status = int(worst(runs) > 100 * EPS_MESH)
            summary = (f"the mesh of the runs ({runs['wall']:g} um) is within {worst(runs):.2f} % of the extrapolated "
                       f"value on the classes of the verdict: {'converged' if not status else 'NOT converged'} at "
                       f"{100 * EPS_MESH:g} %")
    elif coarse:
        following = coarse[0]  # the mesh of gmsh just coarser than the finest
        ratio = following["wall"] / finest["wall"]
        order2 = (f" (it would be {worst(following) / (ratio**2 - 1.0):.2f} % with an order 2)" if ratio > 1.0 else "")
        summary = (f"the classes of the verdict change by at most {worst(following):.2f} % from {following['wall']:g} to "
                   f"{finest['wall']:g} um; the error of the finest mesh is not measured{order2}: "
                   + (f"Richardson not possible on the {failed[1]} {failed[0]} (means of the classes {', '.join(failed[2])} "
                      "not monotone in the size)" if failed else "add a third mesh size of gmsh"))
        if runs is not None and runs is not finest:
            status = int(worst(runs) > 100 * EPS_MESH)
    else:
        summary = f"one mesh of gmsh only ({finest['folder']}), not tested: add another mesh size of gmsh"
    lines += ["#", f"# {summary}"] + (["# not in the verdict: " + "; ".join(small)] if small else [])
    extra, refined_summaries = refinement_lines(refined, gmsh, means, value, ids, used, names, width)
    lines += extra
    with open(os.path.join(root, "mesh_test.txt"), "w") as f:
        f.write("\n".join(lines) + "\n")

    # figure: class means against the reference, then every pore against the finest mesh of gmsh
    fig, ax = plt.subplots(1, 2, figsize=(14.0, 5.0))
    width_bar = 0.8 / max(len(compared), 1)
    big = max(abs(deviation[c["folder"]]).max() for c in compared) > 20.0
    shaded = [c for c in compared if not c["refine"]]  # meshes of gmsh, finest first: darkest to lightest blue
    shade = {c["folder"]: SEQ_BLUE[len(SEQ_BLUE) - 1 - round(k * (len(SEQ_BLUE) - 1) / max(len(shaded) - 1, 1))]
             for k, c in enumerate(shaded)}
    for j, c in enumerate(compared):
        x = np.arange(len(names)) - 0.4 + width_bar * (j + 0.5)
        d = deviation[c["folder"]]
        how_made = (f", the {c['geometry'][0]:g} um mesh refined {c['refine']}x in MMM" if c["refine"] else "")
        ax[0].bar(x, d, width=0.9 * width_bar, zorder=2, hatch="//" if c["refine"] else None,
                  color=REFINED_COLOR if c["refine"] else shade[c["folder"]],
                  edgecolor="white" if c["refine"] else None,
                  label=f"{c['folder']} (wall {c['wall']:g}, bulk {c['bulk']:g} um{how_made})")
        for xk, dk in zip(x, d):
            ax[0].annotate(f"{dk:.1f}", (xk, dk), ha="center", va="bottom" if dk >= 0 else "top", fontsize=7,
                           xytext=(0, 2 if dk >= 0 else -2), textcoords="offset points")
    ax[0].axhspan(-100 * EPS_MESH, 100 * EPS_MESH, color=GRID, lw=0, zorder=0, label=rf"$\pm${100 * EPS_MESH:g} %")
    ax[0].axhline(0.0, color=REFERENCE_LINE, lw=0.8)
    ax[0].set_xticks(range(len(names)), class_ticks(radii, shares) + ["all\npores"])
    ax[0].set(xlabel="radius of the class (um) and its share of the pore volume",
              ylabel="class mean against the reference (%)" + (" (symlog)" if big else ""),
              title="Class means against " + ("the extrapolated value (Richardson)" if extrapolated else
                                              f"the finest mesh of gmsh ({finest['wall']:g} um)\n(its own error is not "
                                              "measured)"))
    ax[0].margins(y=0.1)
    others = [c for c in compared if c is not finest]
    x_pore = np.array([gap_R[i] for i in ids])
    for k, c in enumerate(others):
        pore = np.array([100.0 * (value[c["folder"]][i] / value[finest["folder"]][i] - 1.0) for i in ids])
        marker = "x" if c["refine"] else "osd^v"[k % 5]
        for kr, Rk in enumerate(radii):
            sel = np.array([R[i] == Rk for i in ids])
            ax[1].scatter(x_pore[sel], pore[sel], s=20 if c["refine"] else 16, marker=marker, zorder=3 if c["refine"] else 2,
                          color=class_color(kr, len(radii)), label=f"R = {Rk:.3g} um" if k == 0 else None)
        if len(others) > 1:
            ax[1].scatter([], [], marker=marker, color=REFERENCE_LINE,
                          label=f"{c['folder']} (wall {c['wall']:g} um, {mesh_name(c)})")
    ax[1].axhspan(-100 * EPS_MESH, 100 * EPS_MESH, color=GRID, lw=0, zorder=0)
    ax[1].axhline(0.0, color=REFERENCE_LINE, lw=0.8)
    ax[1].set(xscale="log", xlabel="gap to the nearest pore / radius of the pore",
              ylabel=f"{LABEL[q]} against the finest mesh of gmsh (%)" + (" (symlog)" if big else ""),
              title=f"Every pore of side{side:g}_seed01:\nother meshes against the finest mesh of gmsh ({finest['wall']:g} um)")
    for a, columns in zip(ax, (1, 2)):  # legends under the axes: long labels on the left, many short ones on the right
        if big:
            symlog_percent(a)
        a.grid(True, color=GRID)
        a.legend(frameon=False, fontsize=7, loc="upper center", bbox_to_anchor=(0.5, -0.21), ncol=columns)
    fig.tight_layout()
    fig.savefig(os.path.join(root, "mesh_test.png"), dpi=150, bbox_inches="tight")
    plt.close(fig)
    print("\n".join(lines))
    print(f"wrote mesh_test.txt and mesh_test.png in {root}")
    return status, f"# - mesh: {summary}" + "".join(f"\n# - {s}" for s in refined_summaries)


# ---- 2. Does a cube hold the whole population? -----------------------------------------------------------------------

def completeness(cubes):
    """Classes of the measured distribution against the pores Mérope placed in the cubes of one side: radius, share of
    the pore volume in the distribution (real) and in the cubes, pores expected and placed per cube, porosity asked and
    placed. None without the classes in the summaries."""
    cubes = [c for c in cubes if c["classes"] is not None]
    if not cubes:
        return None
    ref = cubes[0]["classes"]
    placed = np.array([c["classes"]["pores"] for c in cubes])
    porosity = np.array([c["classes"]["placed"] for c in cubes])
    return {"side": cubes[0]["side"], "n": len(cubes), "R": ref["R"], "real": ref["asked"] / ref["asked"].sum(),
            "cube": (porosity / porosity.sum(axis=1, keepdims=True)).mean(axis=0), "expected": ref["expected"],
            "placed_min": placed.min(axis=0), "placed_max": placed.max(axis=0), "porosity_asked": ref["asked"].sum(),
            "porosity": porosity.sum(axis=1).mean()}


def side_for(info, k, pores):
    """Side (um) of a cube expected to hold the given number of pores of class k."""
    return info["side"] * np.cbrt(pores / info["expected"][k]) if info["expected"][k] > 0.0 else np.inf


def missing_classes(info):
    """Indices of the classes of the distribution that no cube holds."""
    if info is None:
        return []
    return [k for k in range(len(info["R"])) if info["placed_max"][k] == 0 and info["real"][k] > 0.0]


def real_share(info, R):
    """Share of the pore volume of class R in the measured distribution, nan if unknown."""
    if info is None:
        return np.nan
    k = np.flatnonzero(np.isclose(info["R"], R, atol=1e-6))
    return info["real"][k[0]] if len(k) else np.nan


def completeness_lines(info):
    """Table of the classes of one side."""
    L = info["side"]
    lines = [f"# 2. Does a cube of {L:g} um hold the whole population? (summary_*.txt of Mérope, {info['n']} cubes)",
             f"#    porosity {info['porosity']:.4f} in the cubes against {info['porosity_asked']:.4f} asked: Mérope places "
             "the integer part of the pores expected in every class",
             f"#  {'R_um':>7s}{'vol_real_%':>11s}{'vol_cube_%':>11s}{'expected':>9s}{'placed':>8s}{'side_1_pore_um':>15s}"]
    for k, R in enumerate(info["R"]):
        lo, hi = info["placed_min"][k], info["placed_max"][k]
        placed = f"{lo:.0f}" if lo == hi else f"{lo:.0f}-{hi:.0f}"
        mark = "   <- not in the cubes" if k in missing_classes(info) else ""
        lines.append(f"   {R:7.4f}{100 * info['real'][k]:11.2f}{100 * info['cube'][k]:11.2f}{info['expected'][k]:9.1f}"
                     f"{placed:>8s}{side_for(info, k, 1.0):15.1f}{mark}")
    missing = missing_classes(info)
    if missing:
        lines.append(f"#    {100 * sum(info['real'][k] for k in missing):.1f} % of the pore volume of the distribution is "
                     f"in classes that no cube of {L:g} um holds: their FGR leaves it out, whatever the number of seeds")
    return lines


# ---- 3. FGR of every cube (D5.2 Fig. 7) ------------------------------------------------------------------------------

def critical_pressures(cube, q):
    """x_c = p_c / sigma_R = 1 / q of the pores of a cube with a value (infinite for q <= 0: never breaks), and their
    volumes."""
    ok = np.isfinite(cube[q])
    s, V = cube[q][ok], cube["V"][ok]
    return np.where(s > 0.0, 1.0 / np.where(s > 0.0, s, 1.0), np.inf), V


def fgr_curve(cube, q, x):
    """FGR of a cube at the normalised pressures x = p / sigma_R: volume of the pores with x_c < x over all the pores."""
    xc, V = critical_pressures(cube, q)
    return (xc[None, :] < x[:, None]) @ V / V.sum()


def fgr_level(cube, q, level):
    """Normalised pressure at which the FGR of a cube reaches the level: the x_c of the pore whose rupture brings the
    released volume to the level."""
    xc, V = critical_pressures(cube, q)
    order = np.argsort(xc)
    released = np.cumsum(V[order]) / V.sum()
    return xc[order][min(np.searchsorted(released, level - 1e-12), len(xc) - 1)]


def fgr_stats(cubes, q):
    """Statistics of Kanit of the critical pressure at every level of FGR_LEVELS, for the cubes of one side."""
    side = cubes[0]["side"]
    return {level: kanit([fgr_level(c, q, level) for c in cubes], side) for level in FGR_LEVELS}


def in_verdict(level):
    """True if the level of FGR enters the verdict."""
    return VERDICT_LEVELS[0] - 1e-9 <= level <= VERDICT_LEVELS[1] + 1e-9


def fgr_lines(side, n, stats):
    """Table of the critical pressures of one side."""
    lines = [f"# side {side:g} um, {n} cubes",
             f"#  {'FGR_%':>5s}{'mean_x_c':>10s}{'+/-':>8s}{'D':>10s}{'eps_cube_%':>11s}{'cubes':>7s}{'RVE_um':>8s}"
             f"{'[95 %]':>10s}{'p_MPa':>8s}"]
    for level, r in stats.items():
        note = "" if in_verdict(level) else "   (onset: a tail, not in the verdict)"
        if np.isnan(r["cubes"]):
            lines.append(f"   {100 * level:5.0f}{r['mean']:10.4f}   fewer than 3 cubes{note}")
            continue
        lines.append(f"   {100 * level:5.0f}{r['mean']:10.4f}{r['half']:8.4f}{np.sqrt(r['D2']):10.2e}{100 * r['eps']:11.2f}"
                     f"{r['cubes']:7.1f}{r['rve']:8.1f}{interval(r):>10s}{SIGMA_R_MPA * r['mean']:8.1f}{note}")
    return lines


def read_experiment():
    """(p_MPa, FGR_percent) of EXPERIMENT, None if not given or missing."""
    if not EXPERIMENT or not os.path.isfile(EXPERIMENT):
        if EXPERIMENT:
            print(f"EXPERIMENT = {EXPERIMENT}: no such file, not drawn")
        return None
    data = np.atleast_2d(np.loadtxt(EXPERIMENT, delimiter=",", skiprows=1))
    return data[:, 0], data[:, 1]


def draw_fgr(ax, by_side, q, x, single, legend=True):
    """The FGR curves of every cube (thin) and their mean with the band +/- 2 D of one cube, in %."""
    for k, L in enumerate(sorted(by_side)):
        cubes = by_seed(by_side[L])
        curves = np.array([fgr_curve(c, q, x) for c in cubes])
        side_color = plt.get_cmap("tab10")(k % 10)
        for j, (c, f) in enumerate(zip(cubes, curves)):
            color = plt.get_cmap("tab10")(j % 10) if single else side_color
            ax.plot(x, 100 * f, lw=0.9, color=color, alpha=0.85 if single else 0.35,
                    label=(f"seed {c['seed']:02d}" if single else None) if legend else None)
        mean = curves.mean(axis=0)
        if len(curves) > 1:
            D = curves.std(axis=0, ddof=1)
            ax.fill_between(x, 100 * np.clip(mean - 2 * D, 0, 1), 100 * np.clip(mean + 2 * D, 0, 1),
                            color="black" if single else side_color, alpha=0.10, lw=0,
                            label=(r"mean $\pm\,2D$ (spread of one cube)" if single else None) if legend else None)
        ax.plot(x, 100 * mean, color="black" if single else side_color, lw=2.2,
                label=(f"mean of {len(cubes)} cubes" if single else f"side {L:g} um, mean of {len(cubes)} cubes")
                if legend else None)


def analyse_fgr(root, by_side, q):
    """FGR of every cube against the normalised pressure, as D5.2 Fig. 7, and the statistics of Kanit of the critical
    pressure at every level of FGR_LEVELS, for every side. Writes rve_fgr.txt and rve_fgr.png. Returns {side: stats}."""
    stats = {L: fgr_stats(by_side[L], q) for L in sorted(by_side)}
    lines = [f"# 3. FGR of every cube, as OperaHPC D5.2 Fig. 7 ({q}; python3 plot_bubbles.py {root})",
             f"# - a pore breaks when {q} p > sigma_R; FGR(p) = volume of the broken pores / volume of all the pores",
             "# - x_c = p / sigma_R at which the FGR of a cube reaches the level (mean over the cubes +/- 2 D / sqrt(n))",
             f"# - p_MPa = mean_x_c x {SIGMA_R_MPA:g} MPa, the sigma_R of D5.2 (monodisperse pores: to recalibrate)",
             f"# - eps_cube = 2 D / mean: error of x_c, i.e. of a sigma_R calibrated on the release, from one cube",
             f"# - cubes = (eps_cube / {100 * EPS_R:g} %)^2 cubes of this side; RVE_um = side x cubes^(1/3) (alpha = 1), "
             "with its 95 % interval"]
    for L in sorted(by_side):
        lines += ["#"] + fgr_lines(L, len(by_side[L]), stats[L])
    rows = scaling({L: stats[L] for L in stats})
    lines += scaling_lines(rows, "FGR level", lambda level: f"{100 * level:5.0f} %")
    with open(os.path.join(root, "rve_fgr.txt"), "w") as f:
        f.write("\n".join(lines) + "\n")

    # figure: the curves of D5.2 Fig. 7 (left, with a zoom on the onset), the error of one cube per level (right)
    single = len(by_side) == 1
    xc = np.concatenate([critical_pressures(c, q)[0] for cubes in by_side.values() for c in cubes])
    xc = xc[np.isfinite(xc)]
    x = np.linspace(0.97 * xc.min(), 1.02 * xc.max(), 800)
    fig, ax = plt.subplots(1, 2, figsize=(15.0, 5.6), gridspec_kw={"width_ratios": [1.6, 1.0]})
    draw_fgr(ax[0], by_side, q, x, single)
    experiment = read_experiment()
    if experiment is not None:
        ax[0].plot(experiment[0] / SIGMA_R_MPA, experiment[1], "*", color=EXPERIMENT_COLOR, ms=9,
                   label=f"experiment (p / {SIGMA_R_MPA:g} MPa)")
    ax[0].set(xlim=(x[0], x[-1]), ylim=(0.0, 100.0), xlabel=r"$p / \sigma_R$ (pore pressure / rupture stress)",
              ylabel="FGR (% of the pore volume)",
              title="FGR of every cube (as D5.2 Fig. 7)" + ("" if q == "s_hat" else ", zero macroscopic strain"))
    top = ax[0].secondary_xaxis("top", functions=(lambda v: v * SIGMA_R_MPA, lambda v: v / SIGMA_R_MPA))
    top.set_xlabel(rf"pore pressure (MPa) for $\sigma_R$ = {SIGMA_R_MPA:g} MPa (value of D5.2, to recalibrate)", fontsize=8)
    ax[0].grid(True, color=GRID)
    ax[0].legend(loc="upper left", frameon=False, fontsize=7, ncol=2)
    zoom = ax[0].inset_axes([0.60, 0.08, 0.37, 0.40])
    first = np.mean([fgr_curve(c, q, x) for L in by_side for c in by_side[L]], axis=0)
    end = x[min(np.searchsorted(first, 0.30), len(x) - 1)]
    draw_fgr(zoom, by_side, q, x, single, legend=False)
    if experiment is not None:
        zoom.plot(experiment[0] / SIGMA_R_MPA, experiment[1], "*", color=EXPERIMENT_COLOR, ms=7)
    zoom.set(xlim=(x[0], end), ylim=(0.0, 30.0), title="zoom on the onset")
    zoom.title.set_fontsize(8)
    zoom.tick_params(labelsize=7)
    zoom.grid(True, color=GRID)

    ax[1].axhspan(0.0, 100 * EPS_R, color=GRID, alpha=0.8, lw=0, label=f"one cube is an RVE ({100 * EPS_R:g} %)")
    for k, L in enumerate(sorted(by_side)):
        levels = [lv for lv in FGR_LEVELS if np.isfinite(stats[L][lv]["eps"])]
        eps = np.array([100 * stats[L][lv]["eps"] for lv in levels])
        color = SEQ_BLUE[3] if single else plt.get_cmap("tab10")(k % 10)
        ax[1].plot([100 * lv for lv in levels], eps, "-", color=color, lw=1.2,
                   label=f"side {L:g} um ({len(by_side[L])} cubes)")
        for lv, e in zip(levels, eps):
            r = stats[L][lv]
            ax[1].plot(100 * lv, e, "o", ms=6, color=color, mfc=color if in_verdict(lv) else "white")
            ax[1].annotate(f"{r['rve']:.0f} um", (100 * lv, e), xytext=(0, 6), textcoords="offset points", ha="center",
                           fontsize=7, color=color)
    ax[1].set(xlabel="level of FGR (%)", ylabel=r"error of one cube on $x_c$, $2D$ / mean (%)", ylim=(0.0, None),
              title="Error of one cube; labels: side of the RVE (alpha = 1)")
    ax[1].plot([], [], "o", color=REFERENCE_LINE, mfc="white", label="onset: not in the verdict")
    ax[1].grid(True, color=GRID)
    ax[1].legend(frameon=False, fontsize=8)
    fig.tight_layout()
    fig.savefig(os.path.join(root, "rve_fgr.png"), dpi=150)
    plt.close(fig)
    print("\n".join(lines))
    print(f"wrote rve_fgr.txt and rve_fgr.png in {root}")
    return stats


# ---- 4. Size of the RVE: classes, scaling with the side, verdict -----------------------------------------------------

def class_stats(cubes, q, info):
    """Statistics of Kanit of the class means for the cubes of one side, one entry per class of radius."""
    side = cubes[0]["side"]
    total = sum(c["V"].sum() for c in cubes)
    results = []
    for R in np.unique(np.concatenate([c["R"] for c in cubes])):
        groups = class_values(cubes, q, R)
        pooled = np.concatenate(groups)
        r = kanit([g.mean() for g in groups], side)
        S = pooled.std(ddof=1) if len(pooled) > 1 else np.nan
        pores = np.mean([len(g) for g in groups])
        r.update(R=R, n_all=len(cubes), pores=pores, S=S,
                 A_x_dens=r["D2"] * pores / S**2 if S > 0.0 else np.nan,
                 vol_cube=sum(c["V"][c["R"] == R].sum() for c in cubes) / total, vol_real=real_share(info, R))
        r["share"] = r["vol_real"] if np.isfinite(r["vol_real"]) else r["vol_cube"]
        results.append(r)
    return results


def class_lines(side, cubes, q, results):
    """Table of the class means of one side."""
    lines = [f"# {q}: {len(cubes)} cubes of {side:g} um",
             f"# {'class':>5s}{'R_um':>8s}{'n':>4s}{'pores':>7s}{'vol_cube':>9s}{'vol_real':>9s}{'mean':>8s}{'+/-':>8s}"
             f"{'D':>9s}{'S':>8s}{'A_x_dens':>9s}{'eps_cube':>9s}{'cubes':>7s}{'RVE_um':>8s}{'[95 %]':>10s}"]
    for k, r in enumerate(results, 1):
        n = f"{r['n']}{'*' if r['n'] < r['n_all'] else ''}"
        head = f"{k:7d}{r['R']:8.4f}{n:>4s}{r['pores']:7.1f}{100 * r['vol_cube']:8.1f}%{100 * r['vol_real']:8.1f}%"
        if np.isnan(r["cubes"]):
            lines.append(f"{head}   the class is in fewer than 3 cubes")
            continue
        lines.append(f"{head}{r['mean']:8.4f}{r['half']:8.4f}{np.sqrt(r['D2']):9.2e}{r['S']:8.4f}{r['A_x_dens']:9.2f}"
                     f"{100 * r['eps']:8.1f}%{r['cubes']:7.1f}{r['rve']:8.1f}{interval(r):>10s}")
    return lines


def scaling(stats_by_side):
    """Keys (classes or levels of FGR) measured at every side: sides, D^2, means, half-widths 2 D / sqrt(n), and alpha of
    D^2 ~ V^-alpha. Empty with one side."""
    sides = sorted(stats_by_side)
    if len(sides) < 2:
        return []
    keys = set.intersection(*({k for k, r in stats_by_side[L].items() if np.isfinite(r["D2"]) and r["D2"] > 0.0}
                              for L in sides))
    out = []
    for key in sorted(keys):
        rows = [stats_by_side[L][key] for L in sides]
        D2 = np.array([r["D2"] for r in rows])
        alpha = -np.polyfit(np.log(np.array(sides) ** 3), np.log(D2), 1)[0]
        out.append({"key": key, "sides": np.array(sides), "D2": D2, "alpha": alpha,
                    "mean": np.array([r["mean"] for r in rows]), "half": np.array([r["half"] for r in rows])})
    return out


def scaling_lines(rows, what, name):
    """Table of the scaling with the side: alpha, and the mean per side, which must not drift beyond its half-widths
    (otherwise the small cubes are biased)."""
    if not rows:
        return []
    lines = ["#", f"# {what}: scaling with the side, D^2 ~ V^-alpha (alpha = 1 for independent contributions); the mean "
                  "must not drift with the side beyond 2 D / sqrt(n)",
             f"# {what:>10s}{'alpha':>7s}  mean +/- 2 D / sqrt(n) per side"]
    for r in rows:
        means = "  ".join(f"{L:g}: {m:.4f} +/- {h:.4f}" for L, m, h in zip(r["sides"], r["mean"], r["half"]))
        lines.append(f"  {name(r['key']):>10s}{r['alpha']:7.2f}  {means}")
    return lines


def scaling_figure(class_rows, fgr_rows, q, name):
    """D^2 against the volume of the cube, for the classes and for the levels of FGR, with the slope -1 of independent
    contributions."""
    fig, ax = plt.subplots(1, 2, figsize=(13.0, 4.8))
    panels = ((ax[0], class_rows, lambda r: f"R = {r['key']:.3g} um", f"$D^2$ of the class mean of {LABEL[q]}",
               "Class means"),
              (ax[1], fgr_rows, lambda r: f"FGR {100 * r['key']:.0f} %", r"$D^2$ of the critical pressure $x_c$", "FGR"))
    for a, rows, label, ylabel, what in panels:
        for k, r in enumerate(rows):
            V = r["sides"] ** 3
            color = class_color(k, len(rows))
            a.loglog(V, r["D2"], "o-", color=color, label=f"{label(r)}, alpha = {r['alpha']:.2f}")
            a.loglog(V, r["D2"][0] * V[0] / V, ":", color=color, lw=0.8)
        a.set(xlabel="volume of the cube (um$^3$)", ylabel=ylabel, title=what + r"; dotted: $D^2 \sim 1/V$ (alpha = 1)")
        a.grid(True, color=GRID, which="both")
        a.legend(frameon=False, fontsize=8)
    fig.tight_layout()
    fig.savefig(name, dpi=150)
    plt.close(fig)


def verdict(cubes, info, fgr, classes, class_rows, fgr_rows, mesh_summary):
    """Side of the RVE of the main side: the largest side over the levels of FGR of VERDICT_LEVELS and the classes with at
    least MIN_VOLUME_SHARE of the pore volume, then the reserves. Returns (lines, status), status 1 if one cube is not an
    RVE."""
    L, n = cubes[0]["side"], len(cubes)
    lines = ["#", f"# 4. Verdict for one cube of {L:g} um, precision {100 * EPS_R:g} % on the mean, {n} cubes"]
    on_fgr = [(r["rve"], r, f"FGR {100 * lv:.0f} %") for lv, r in fgr.items() if in_verdict(lv) and np.isfinite(r["rve"])]
    on_classes = [(r["rve"], r, f"class R = {r['R']:.3g} um ({100 * r['share']:.0f} % of the pore volume)")
                  for r in classes if r["share"] >= MIN_VOLUME_SHARE and np.isfinite(r["rve"])]
    if not on_fgr and not on_classes:
        return lines + ["# not determined: fewer than 3 cubes"], 0
    side, worst, why = max(on_fgr + on_classes, key=lambda t: t[0])
    status = int(side > L)
    if not status:
        lines.append(f"# one cube of {L:g} um is an RVE: the largest side needed is {side:.1f} um ({why})")
    else:
        lines += [f"# one cube of {L:g} um is NOT an RVE",
                  f"# side of the RVE: {side:.0f} um (95 %: {interval(worst)} um), set by {why}; or the mean of "
                  f"{np.ceil(worst['cubes']):.0f} cubes of {L:g} um"]
    for what, cands in (("on the FGR (levels 10-90 %)", on_fgr), ("on the classes of the verdict", on_classes)):
        if cands:
            s, r, w = max(cands, key=lambda t: t[0])
            lines.append(f"#   {what}: {s:.0f} um ({interval(r)}), {w}")
    small = [f"R = {r['R']:.3g} um ({100 * r['share']:.1f} %, {r['rve']:.0f} um)" for r in classes
             if r["share"] < MIN_VOLUME_SHARE and np.isfinite(r["rve"])]
    if small:
        lines.append(f"#   classes below {100 * MIN_VOLUME_SHARE:g} % of the pore volume, not in the verdict: "
                     + ", ".join(small))

    lines.append("# Reserves")
    A = [r["A_x_dens"] for r in classes if np.isfinite(r["A_x_dens"])]
    if not class_rows and not fgr_rows:
        lines.append("# - one side only: alpha = 1 assumed (D^2 ~ 1/V)")
        if A:
            independent = 0.5 <= min(A) and max(A) <= 2.0
            lines.append(f"#   A x pore density of the classes is {min(A):.2f}-{max(A):.2f} (1 for independent pores): "
                         + ("the pores are close to independent, which supports alpha = 1" if independent else
                            "the pores are not independent, alpha may differ from 1"))
        lines.append("#   cubes of a second side (EXTRA_SIDES of test_rve.py) would measure alpha and the drift of the mean")
    else:
        alphas = [r["alpha"] for r in class_rows + fgr_rows]
        lines.append(f"# - several sides: alpha from {min(alphas):.2f} to {max(alphas):.2f} (1 expected; with few cubes "
                     "per side D^2 is uncertain by a factor of 2 or more, and so is alpha); the drift of the mean with "
                     "the side is in the scaling tables")
    if info is not None:
        needed = [r["cubes"] * r["pores"] for r in classes if np.isfinite(r["cubes"])]
        typical = np.median(needed) if needed else np.nan
        missing = missing_classes(info)
        for k in [k for k in missing if info["real"][k] >= 0.001]:
            more = (f"; the {typical:.0f} pores that give the other classes {100 * EPS_R:g} % on their mean need "
                    f"{side_for(info, k, typical):.0f} um (more than the FGR needs, where the class weighs only its share)"
                    if typical >= 1.0 else "")
            lines.append(f"# - class R = {info['R'][k]:.2f} um ({percent(info['real'][k])} % of the pore volume) is in no "
                         f"cube: {info['expected'][k]:.1f} pores expected per cube; one pore needs a side of "
                         f"{side_for(info, k, 1.0):.1f} um{more}")
        negligible = [k for k in missing if info["real"][k] < 0.001]
        if negligible:
            lines.append("# - also in no cube, negligible: " + ", ".join(
                f"R = {info['R'][k]:.2f} um ({100 * info['real'][k]:.2f} % of the pore volume)" for k in negligible))
        lines.append(f"# - porosity {info['porosity']:.4f} in the cubes against {info['porosity_asked']:.4f} asked")
    lines.append(mesh_summary)
    return lines, status


def analyse_size(root, by_side, main, q, infos, fgr_by_side, mesh_summary):
    """Class means of every side, scaling with the side, verdict on the main side (rve_size.txt), and the figure of the
    class means of the main side (rve_size.png). Returns 1 if one cube of the main side is not an RVE, 0 otherwise."""
    stats = {L: class_stats(by_side[L], q, infos[L]) for L in sorted(by_side)}
    quantities = ["s_hat", "sigma"] if q == "s_hat" else ["sigma"]
    lines = [f"# Size of the RVE (python3 plot_bubbles.py {root}): {', '.join(f'{len(by_side[L])} cubes of {L:g} um' for L in sorted(by_side))}",
             f"# precision asked on a mean: {100 * EPS_R:g} %, quantity of the verdict: {q}"]
    for L in sorted(by_side):
        if infos[L] is not None:
            lines += ["#"] + completeness_lines(infos[L])
    lines += ["#", "# 4. Class means (the FGR is in rve_fgr.txt)",
              "# - Z = mean of the quantity over the pores of one class in one cube, n = cubes holding the class "
              "(* if some cubes miss it), pores = pores of the class per cube",
              "# - vol_cube / vol_real = share of the pore volume of the class in the cubes / in the measured distribution",
              "# - mean +/- 2 D / sqrt(n) = mean of Z over the cubes; D = standard deviation of Z between the cubes; "
              "S = standard deviation between single pores",
              "# - A_x_dens = D^2 x pores / S^2: 1 if the pores of a class are independent (then D^2 ~ 1/V, alpha = 1)",
              f"# - eps_cube = 2 D / mean: error of Z from one cube; cubes = (eps_cube / {100 * EPS_R:g} %)^2 cubes of this "
              "side; RVE_um = side x cubes^(1/3) (alpha = 1), with its 95 % interval"]
    class_rows = []
    for quantity in quantities:
        results = stats if quantity == q else {L: class_stats(by_side[L], quantity, infos[L]) for L in sorted(by_side)}
        for L in sorted(by_side):
            lines += ["#"] + class_lines(L, by_side[L], quantity, results[L])
        rows = scaling({L: {r["R"]: r for r in results[L]} for L in sorted(by_side)})
        lines += scaling_lines(rows, f"{quantity} R_um", lambda R: f"{R:.4f}")
        if quantity == q:
            class_rows = rows
    fgr_rows = scaling(fgr_by_side)
    if class_rows or fgr_rows:
        scaling_figure(class_rows, fgr_rows, q, os.path.join(root, "rve_scaling.png"))
    end, status = verdict(by_side[main], infos[main], fgr_by_side[main], stats[main], class_rows, fgr_rows, mesh_summary)
    lines += end
    with open(os.path.join(root, "rve_size.txt"), "w") as f:
        f.write("\n".join(lines) + "\n")

    # figure: Z of Kanit per class, side of the RVE per class, stress factor against the nearest ligament
    cubes, results, info = by_seed(by_side[main]), stats[main], infos[main]
    side = cubes[0]["side"]
    radii = [r["R"] for r in results]
    shares = [r["share"] for r in results]
    fig, ax = plt.subplots(1, 3, figsize=(18.0, 5.4))
    spread = np.linspace(-0.32, 0.12, max(len(cubes), 2))
    for i, r in enumerate(results):
        for j, c in enumerate(cubes):
            v = c[q][(c["R"] == r["R"]) & np.isfinite(c[q])]
            if len(v):
                ax[0].plot(i + spread[j], v.mean(), "o", ms=4.5, color=plt.get_cmap("tab10")(j % 10), alpha=0.85)
        if np.isfinite(r["D2"]):
            ax[0].errorbar(i + 0.28, r["mean"], yerr=2.0 * np.sqrt(r["D2"]), color=SEQ_BLUE[1], marker="s", ms=5, lw=0,
                           elinewidth=1.2, capsize=3, label=r"one cube, mean $\pm\,2D$" if i == 0 else None)
            ax[0].errorbar(i + 0.28, r["mean"], yerr=r["half"], color=SEQ_BLUE[4], marker="s", ms=5, lw=0,
                           elinewidth=2.6, capsize=0, label=rf"mean of the cubes $\pm\,2D/\sqrt{{n}}$" if i == 0 else None)
    ax[0].plot([], [], "o", color=REFERENCE_LINE, label="mean of the class in one cube (colour: seed)")
    ax[0].set_xticks(range(len(radii)), class_ticks(radii, shares))
    ax[0].set(xlabel="radius of the class (um) and its share of the pore volume", ylabel=f"class mean of {LABEL[q]}",
              title="Z of Kanit: the class mean in every cube")

    for i, r in enumerate(results):
        if not np.isfinite(r["rve"]):
            continue
        used = r["share"] >= MIN_VOLUME_SHARE
        color = SEQ_BLUE[3] if used else REFERENCE_LINE
        ax[1].errorbar(i, r["rve"], yerr=[[r["rve"] - r["rve_low"]], [r["rve_high"] - r["rve"]]], color=color, marker="o",
                       ms=7, mfc=color if used else "white", lw=0, elinewidth=1.4, capsize=4)
        ax[1].annotate(f"{r['rve']:.0f}", (i, r["rve"]), xytext=(8, 0), textcoords="offset points", va="center",
                       fontsize=8, color=color)
    ax[1].axhline(side, color=REFERENCE_LINE, ls="--", lw=1.2, label=f"these cubes, {side:g} um")
    ax[1].axhline(D52_SIDE, color=D52_COLOR, ls=":", lw=1.4, label=f"RVE of D5.2, {D52_SIDE:g} um")
    ax[1].plot([], [], "o", color=REFERENCE_LINE, mfc="white",
               label=f"below {100 * MIN_VOLUME_SHARE:g} % of the pore volume: not in the verdict")
    missing = [k for k in missing_classes(info) if info["real"][k] >= 0.001]
    if missing:
        text = "not in the cubes: " + "; ".join(f"R = {info['R'][k]:.2f} um ({percent(info['real'][k])} % of the "
                                               f"pore volume)" for k in missing)
        ax[1].text(0.02, 0.02, text, transform=ax[1].transAxes, fontsize=8, color=D52_COLOR)
    highest = max([D52_SIDE] + [r["rve_high"] for r in results if np.isfinite(r["rve_high"])])
    ax[1].set_xticks(range(len(radii)), class_ticks(radii, shares))
    ax[1].set(xlabel="radius of the class (um) and its share of the pore volume", ylim=(0.0, 1.35 * highest),
              ylabel="side of one cube that is an RVE (um)",
              title=f"Side for {100 * EPS_R:g} % on the class mean (alpha = 1, 95 % interval)")

    for k, r in enumerate(results):
        gap_R = np.concatenate([c["gap_R"][c["R"] == r["R"]] for c in cubes])
        v = np.concatenate([c[q][c["R"] == r["R"]] for c in cubes])
        ax[2].scatter(gap_R, v, s=10, color=class_color(k, len(results)), label=f"R = {r['R']:.3g} um")
    ax[2].set(xscale="log", xlabel="gap to the nearest pore / radius of the pore", ylabel=LABEL[q],
              title="The stress factor follows the nearest ligament")
    for a, where in zip(ax, ("upper left", "upper left", "upper right")):
        a.grid(True, color=GRID)
        a.legend(frameon=False, fontsize=8, loc=where)
    fig.tight_layout()
    fig.savefig(os.path.join(root, "rve_size.png"), dpi=150)
    plt.close(fig)
    print("\n".join(lines))
    print(f"wrote rve_size.txt, rve_size.png{', rve_scaling.png' if class_rows or fgr_rows else ''} in {root}")
    return status


# ---- Main ---------------------------------------------------------------------------------------------------------

def main():
    """The four questions on the folders of ROOT (argument or default). Returns the worst status."""
    root = sys.argv[1] if len(sys.argv) > 1 else ROOT
    folders = sorted(f for f in glob.glob(os.path.join(root, "side*_seed*"))
                     if os.path.isdir(f) and CASE.match(os.path.basename(f)))
    if not folders:
        print(f"no side<L>_seed<NN> folder in {root}: nothing to analyse (python3 test_rve.py writes them)")
        return 0
    cubes, unsolved = [], []
    for folder in folders:
        cube = read_cube(folder)
        (cubes if cube else unsolved).append(cube or os.path.basename(folder))
    print(f"{root}: {len(cubes)} folders solved" + (f", not solved yet: {', '.join(unsolved)}" if unsolved else ""))
    if not cubes:
        return 0
    q = "s_hat" if all(c["s_hat"] is not None for c in cubes) else "sigma"
    if q == "sigma":
        print(f"{HYDRO} missing in some folders: the analysis is on sigma = sigma_I / p, at zero macroscopic strain")

    print("\n== 1. mesh ==")
    mesh_status, mesh_summary = analyse_mesh(root, cubes, q)
    by_side = {}
    for c in cubes:
        if c["variant"] is None:
            by_side.setdefault(c["side"], []).append(c)
    if not by_side:
        print(f"{root}: no cube of the size test solved")
        return mesh_status
    main_side = max(by_side, key=lambda L: (len(by_side[L]), -L))
    infos = {L: completeness(by_side[L]) for L in by_side}
    print("\n== 3. FGR of every cube ==")
    fgr_by_side = analyse_fgr(root, by_side, q)
    print("\n== 2. population and 4. size of the RVE ==")
    status = analyse_size(root, by_side, main_side, q, infos, fgr_by_side, mesh_summary)
    return max(mesh_status, status)


if __name__ == "__main__":
    sys.exit(main())
