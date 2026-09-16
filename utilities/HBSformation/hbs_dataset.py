"""Targets read from the HBS experimental datasets (data/*.json, one JSON file per paper).

Every radial point of the JSON files carries its measured values and the local conditions of
that point (`temperature.value_C`, `burnup.value_GWd_tU`, with `basis` = reported | model |
literature | average).  No TRANSURANUS run is used: the conditions are the ones of the JSON
files (Halden-conductivity model on the published centreline histories for ZAC2022 and GER2018,
Alcyone radial profile for ONO2025, FTEMP disc temperature for NOI2015; rim burnup profile for
ZAC2022/ONO2025, Barani 2020 for GER2018 and the NOI2015 76 GWd/tHM disc).

    load_rows(...)    EBSD rows of ZAC2022 / ONO2025 with the keys `load_ebsd` has always
                      returned, so that `validate` and `calibrate` work unchanged
    load_points(...)  one target per (point, observable) for all four papers, with its
                      quality weight, for calibrate.py and comparison.py

Row keys added to the historical ones:
    sample_id, r_over_R, group               position and paper group (ZAC, ONO, GER, NOI; Cr = doped ZAC)
    rank_theta, rank_fraction, rank_radius   Rose rank of the values entering each observable (worst)
    relevance_*                              relevance 1-3 of those values (lowest)
    weight_theta, weight_fraction,           study weight = rank factor x relevance / 3, with rank
    weight_radius                            factor A, B = 1; U* = 0.5; U = 0.25 (a choice of this
                                             study, not part of Rose)
    conditions                               "<temperature basis>/<burnup basis>"
    flags                                    measurement flags, ';'-separated

Location of the JSON folder: the argument, else $HBS_DATASET, else data/ next to this file.
The files in data/ are copies of HBS_2026/data/json (the curated source): copy them again after
any change there.

@author  E. Cappellari
@date    2026-09-15
"""

from __future__ import annotations

import json
import math
import os

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_DATASET = os.path.join(HERE, "data")
FILES = ("ZAC2022", "ONO2025", "GER2018", "NOI2015")

QUANTITY_KEY = {
    "restructured_fraction_1deg": "f1",
    "restructured_fraction_10deg": "f10",
    "AMis2Mean": "amis",
    "subgrain_ECD50": "ecd_sub",
    "newgrain_ECD50": "ecd_new",
}
DATASET_LABEL = {"ZAC2022": "Zacharie", "ONO2025": "Onofri", "GER2018": "Gerczak", "NOI2015": "Noirot"}
RANK_ORDER = {"A": 0, "B": 1, "U*": 2, "U": 3, "C": 4, "": 5}
ALL_RANKS = ("A", "B", "U*", "U")
RANK_FACTOR = {"A": 1.0, "B": 1.0, "U*": 0.5, "U": 0.25}


def dataset_dir(path=None):
    """The JSON folder, or None when it cannot be found."""
    for candidate in (path, os.environ.get("HBS_DATASET"), DEFAULT_DATASET):
        if candidate and os.path.isfile(os.path.join(candidate, "ZAC2022.json")):
            return candidate
    return None


def read_all(path=None):
    folder = dataset_dir(path)
    if folder is None:
        raise FileNotFoundError("HBS JSON datasets not found (argument, $HBS_DATASET, %s)" % DEFAULT_DATASET)
    docs = {}
    for name in FILES:
        with open(os.path.join(folder, name + ".json"), encoding="utf-8") as handle:
            docs[name] = json.load(handle)
    return folder, docs


def study_weight(rank, relevance):
    if rank not in RANK_FACTOR or relevance is None:
        return math.nan
    return RANK_FACTOR[rank] * relevance / 3.0


def _doped(sample):
    return (sample.get("dopant") or "none").strip().lower() not in ("", "none", "-")


def _group(source, sample):
    return "Cr" if source == "ZAC2022" and _doped(sample) else source[:3]


def _grain_radius(sample, default):
    g = sample.get("grain_size_um")
    return g / 2.0 * 1e-6 if isinstance(g, (int, float)) else default


def _conditions(point):
    t, b = point["temperature"], point["burnup"]
    return t["value_C"], b["value_GWd_tU"], "%s/%s" % (t.get("basis"), b.get("basis"))


def load_rows(path=None, sources=("ZAC2022", "ONO2025"), ranks=ALL_RANKS, dopant="none",
              fabrication_porosity=None, grain_radius=None):
    """Row dicts as `load_ebsd`, built from the JSON datasets (EBSD papers only).

    sources  papers to take the EBSD maps from
    ranks    Rose ranks accepted for a value
    dopant   "none" keeps the undoped samples only, None keeps every sample
    fabrication_porosity, grain_radius
             defaults where the JSON sample gives no value (the JSON files carry the grain size
             but not the fabrication porosity, so the porosity is always the module default)
    """
    folder, docs = read_all(path)
    rows, skipped = [], []
    for source in sources:
        for sample in docs[source]["samples"]:
            for point in sample["points"]:
                if point["r_over_R"] is None:
                    continue
                p = {}
                for m in point["measurements"]:
                    key = QUANTITY_KEY.get(m["quantity"])
                    if key is None or m["value"] is None or m["rose_rank"] not in ranks:
                        continue
                    p[key] = (float(m["value"]), m["rose_rank"], m["relevance"], ";".join(m["flags"]))
                if not p:
                    continue
                if dopant is not None and _doped(sample):
                    skipped.append((sample["sample_id"], point["r_over_R"], "dopant %s" % sample["dopant"]))
                    continue
                t_c, bu, how = _conditions(point)
                if t_c is None or bu is None:
                    skipped.append((sample["sample_id"], point["r_over_R"], "no local conditions"))
                    continue
                rows.append(_row(source, sample, point, p, t_c, bu, how, fabrication_porosity, grain_radius))
    load_rows.skipped = skipped
    load_rows.folder = folder
    return rows


def _row(source, sample, point, p, t_c, bu, how, fabrication_porosity, grain_radius):
    def value(k):
        return p[k][0] if k in p else math.nan

    def worst(keys):
        got = [p[k] for k in keys if k in p]
        if not got:
            return "", None, math.nan
        rank = max((g[1] for g in got), key=RANK_ORDER.get)
        rels = [g[2] for g in got if g[2] is not None]
        rel = min(rels) if rels else None
        return rank, rel, study_weight(rank, rel)

    ecd_key = "ecd_new" if "ecd_new" in p else "ecd_sub"
    rank_theta, rel_theta, w_theta = worst(["f1", "f10", "amis"])
    rank_fraction, rel_fraction, w_fraction = worst(["f10"])
    rank_radius, rel_radius, w_radius = worst([ecd_key])
    flags = set()
    for v in p.values():
        flags.update(filter(None, v[3].split(";")))
    return {
        "label": "%s/%s" % (DATASET_LABEL[source], sample["label_in_paper"].split()[0]),
        "Dataset": DATASET_LABEL[source],
        "source_id": source,
        "group": _group(source, sample),
        "sample_id": sample["sample_id"],
        "r_over_R": point["r_over_R"],
        "burnup": float(bu),
        # no effective-burnup history in the JSON files; all points are below the 1000 C
        # threshold on average, so the local burnup is carried in its place
        "burnup_effective": float(bu),
        "temperature": float(t_c) + 273.15,
        "f1": value("f1"), "f10": value("f10"), "amis": value("amis"),
        "ecd_sub": value("ecd_sub"), "ecd_new": value("ecd_new"),
        "porosity": fabrication_porosity,
        "grain_radius": _grain_radius(sample, grain_radius),
        "rank_theta": rank_theta, "rank_fraction": rank_fraction, "rank_radius": rank_radius,
        "relevance_theta": rel_theta, "relevance_fraction": rel_fraction, "relevance_radius": rel_radius,
        "weight_theta": w_theta, "weight_fraction": w_fraction, "weight_radius": w_radius,
        "meas": dict(p),
        "conditions": how,
        "flags": ";".join(sorted(flags)),
    }


def load_points(path=None, fabrication_porosity=None, grain_radius=None, theta_measured=None,
                measured_radius=None, ger_rim_min=0.97):
    """One target per (point, observable) for the four papers.

    theta     ZAC2022 (std and Cr) and ONO2025 EBSD rows, Theta from `theta_measured`
    fraction  ZAC2022 f10 / 100; NOI2015 Xe-depleted area fraction (Fig. 6 markers, the points
              with a radial position); GER2018 HBS area fraction of Barani 2020 (image analysis
              of Gerczak Fig. 10, rank U)
    radius    ZAC2022 ECD50 / 2; GER2018 area-weighted grain size dA / 2 in the HBS layer
              (r/ro >= ger_rim_min, Fig. 8 markers)

    Returns (points, notes).  Each point: obs, y, bu, T [K], porosity, grain_radius, group,
    source, sample_id, r, label, rank, relevance, w_study, conditions.
    """
    folder, docs = read_all(path)
    points, notes = [], []

    def add(obs, y, source, sample, point, rank, relevance, what):
        t_c, bu, how = _conditions(point)
        if t_c is None or bu is None:
            notes.append("no conditions: %s %s" % (sample["sample_id"], point["r_over_R"]))
            return
        points.append(dict(obs=obs, y=float(y), bu=float(bu), T=float(t_c) + 273.15,
                           porosity=fabrication_porosity, grain_radius=_grain_radius(sample, grain_radius),
                           group=_group(source, sample), source=source, sample_id=sample["sample_id"],
                           r=point["r_over_R"], label="%s %g %s" % (sample["sample_id"], point["r_over_R"], what),
                           rank=rank, relevance=relevance, w_study=study_weight(rank, relevance),
                           conditions=how))

    rows = load_rows(folder, sources=("ZAC2022", "ONO2025"), dopant=None,
                     fabrication_porosity=fabrication_porosity, grain_radius=grain_radius)
    samples = {s["sample_id"]: s for d in docs.values() for s in d["samples"]}
    point_of = {}
    for d in docs.values():
        for s in d["samples"]:
            for pt in s["points"]:
                point_of.setdefault((s["sample_id"], pt["r_over_R"]), pt)
    for row in rows:
        if not row["burnup"] > 0.0:
            continue
        sample, point = samples[row["sample_id"]], point_of[(row["sample_id"], row["r_over_R"])]
        # Theta needs f1: ONO2025 reports no restructuring (f1 = 0, Theta = 0 as in validate());
        # points with AMis2Mean only and no f1 elsewhere (Cr-63 0.05-0.25R) are not Theta targets
        has_f1 = not math.isnan(row["f1"]) or row["source_id"] == "ONO2025"
        if theta_measured is not None and has_f1 and not math.isnan(theta_measured(row)):
            add("theta", theta_measured(row), row["source_id"], sample, point,
                row["rank_theta"], row["relevance_theta"], "Theta")
        if not math.isnan(row["f10"]):
            add("fraction", row["f10"] / 100.0, row["source_id"], sample, point,
                row["rank_fraction"], row["relevance_fraction"], "f10")
        if measured_radius is not None and not math.isnan(measured_radius(row)):
            add("radius", measured_radius(row), row["source_id"], sample, point,
                row["rank_radius"], row["relevance_radius"], "ECD50/2")

    for sample in docs["NOI2015"]["samples"]:
        for point in sample["points"]:
            if point["r_over_R"] is None:
                continue
            for m in point["measurements"]:
                if m["quantity"] == "HBS_area_fraction_Xe" and m["value"] is not None:
                    add("fraction", m["value"] / 100.0, "NOI2015", sample, point, m["rose_rank"], m["relevance"], "Xe area")
    ger = docs["GER2018"]["samples"][0]
    for point in ger["points"]:
        r = point["r_over_R"]
        if r is None:
            continue
        for m in point["measurements"]:
            if m["quantity"] == "HBS_area_fraction_EBSD_image":
                add("fraction", m["value"], "GER2018", ger, point, m["rose_rank"], m["relevance"], "Barani area")
        # rim dA: the paper gives the same position both in the text and as a Fig. 8 marker, so
        # keep ONE value per radius -- the best-ranked, and the text one when the ranks tie
        # (a stated number is exact, a marker is read off the raster).
        best = _best_rim_da(point, r, ger_rim_min)
        if best is not None:
            add("radius", best["value"] / 2.0 * 1e-6, "GER2018", ger, point,
                best["rose_rank"], best["relevance"], "dA/2")
    return points, notes


def _best_rim_da(point, r, ger_rim_min):
    """The GER2018 rim grain size to use at this radius, or None."""
    if r is None or r < ger_rim_min:
        return None
    candidates = [m for m in point["measurements"]
                  if m["quantity"] == "grain_size_dA" and m["value"] is not None]
    if not candidates:
        return None
    return min(candidates, key=lambda m: (RANK_ORDER.get(m["rose_rank"], 9),
                                          (m["provenance"] or "").startswith("Fig.")))


def summary(rows):
    """One line per sample: points, conditions, ranks."""
    by = {}
    for r in rows:
        by.setdefault(r["sample_id"], []).append(r)
    lines = []
    for sid, g in by.items():
        ranks = {}
        for r in g:
            ranks[r["rank_theta"]] = ranks.get(r["rank_theta"], 0) + 1
        conds = sorted({r["conditions"] for r in g})
        lines.append("  %-12s N = %2d   conditions %s   Theta ranks %s"
                     % (sid, len(g), ",".join(conds),
                        " ".join("%s=%d" % kv for kv in sorted(ranks.items(), key=lambda kv: RANK_ORDER[kv[0]]))))
    return "\n".join(lines)


if __name__ == "__main__":
    rows = load_rows()
    print("dataset: %s" % load_rows.folder)
    print(summary(rows))
    for sid, r, why in load_rows.skipped:
        print("  skipped %s r/R=%g: %s" % (sid, r, why))
