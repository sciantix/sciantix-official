"""#COARSENING: self-contained 38-case regression using published ramp data.

Barani2020 is read only as published Table4 data. There is no Barani solver.
"""

from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import argparse
import csv
import hashlib
import json
import os
import shutil
import subprocess
import tempfile
import numpy as np

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
POOLS = ("solution", "bulk", "dislocations", "gb", "released")


def read_csv(path):
    with path.open() as f:
        return list(csv.DictReader(f))


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def save(path, value):
    path.write_text(json.dumps(value, indent=2, allow_nan=False) + "\n")


def output(path):
    with path.open() as f:
        names = f.readline().strip().split("\t")
    values = np.loadtxt(path, skiprows=1, ndmin=2)
    assert values.shape[1] == len(names), (path, values.shape, len(names))
    return dict(zip(names, values.T))


def setting(text, name, value):
    import re

    text, count = re.subn(
        r"^\d+(\s*#\s*" + name + r"\b)", str(value) + r"\1", text, flags=re.M
    )
    assert count == 1, name
    return text


def environment(steps, tolerance):
    env = {k: v for k, v in os.environ.items() if not k.startswith("SCIANTIX_")}
    env.update(
        SCIANTIX_TIME_STEPS=str(steps),
        SCIANTIX_COARSENING_TOLERANCE=str(tolerance),
        SCIANTIX_STABLE_HISTORY="1",
    )
    return env


def prepare(folder, sample, model, scales=None, density=1):
    folder.mkdir(parents=True, exist_ok=True)
    for p in (HERE / "inputs" / sample).iterdir():
        shutil.copyfile(p, folder / p.name)
    settings = setting(
        (HERE / "input_settings.txt").read_text(),
        "iIntraGranularBubbleBehavior",
        4 if model == "coarsening" else 1,
    )
    settings = setting(settings, "iDislocationDensity", density)
    (folder / "input_settings.txt").write_text(settings)
    lines = (HERE / "input_scaling_factors.txt").read_text().splitlines()
    if scales is not None:
        for i, value in enumerate(scales, 9):
            lines[i] = str(value) + " # " + lines[i].split("#", 1)[1].strip()
    (folder / "input_scaling_factors.txt").write_text("\n".join(lines) + "\n")


def summarize(path, history, model):
    t = output(path)
    pair = lambda label: sum(t[g + " " + label + " (at/m3)"] for g in ("Xe", "Kr"))
    produced = pair("produced")
    md = (
        pair("in dislocation bubbles")
        if model == "coarsening"
        else np.zeros_like(produced)
    )
    pools = np.array(
        [
            pair("in intragranular solution"),
            pair("in intragranular bubbles") - md,
            md,
            pair("at grain boundary"),
            pair("released"),
        ]
    )
    valid = produced > 0
    balance = float(np.max(abs(pools[:, valid].sum(axis=0) / produced[valid] - 1)))
    assert balance < (2e-12 if model == "coarsening" else 1e-6), (path, balance)
    assert np.min(pools[:, valid]) > -1e-7 * np.max(produced)
    share = np.divide(
        100 * pools, produced, out=np.zeros_like(pools), where=produced > 0
    )
    h = np.loadtxt(history)
    ramps = np.flatnonzero(np.diff(h[:, 1]) > 1)
    phases = {"end": len(produced) - 1}
    onset = None
    if len(ramps):
        j = ramps[0]
        onset = float(h[j, 0])
        cold = h[j, 1]
        phases["pre"] = int(
            np.flatnonzero(
                (t["Time (h)"] <= onset + 1e-7) & (t["Temperature (K)"] <= cold + 1e-8)
            )[-1]
        )
        phases["hot"] = int(
            np.flatnonzero(t["Temperature (K)"] >= max(t["Temperature (K)"]) - 1e-6)[-1]
        )
    else:
        phases["pre"] = phases["end"]

    def state(i):
        def get(k):
            return float(t[k][i]) if k in t else 0.0

        return dict(
            time_h=get("Time (h)"),
            T=get("Temperature (K)"),
            produced=float(produced[i]),
            R=get(
                "Coarsening dislocation radius (m)"
                if model == "coarsening"
                else "Intragranular bubble radius (m)"
            ),
            N=get(
                "Coarsening dislocation concentration (bub/m3)"
                if model == "coarsening"
                else "Intragranular bubble concentration (bub/m3)"
            ),
            S=100 * get("Intragranular gas bubble swelling (/)"),
            Sgb=100 * get("Intergranular gas swelling (/)"),
            Rb=get("Intragranular bubble radius (m)"),
            Nb=get("Intragranular bubble concentration (bub/m3)"),
            P=get("Coarsening pressure (Pa)"),
            nv=get("Coarsening vacancies per bubble (vac/bub)"),
            **{name: float(share[k, i]) for k, name in enumerate(POOLS)}
        )

    states = {name: state(i) for name, i in phases.items()}
    assert abs(t["Time (h)"][-1] - h[-1, 0]) < 1e-6
    return dict(states=states, onset_h=onset, max_balance=balance)


def run(binary, sample, model, steps, tolerance, scales=None, density=1):
    identifier = hashlib.sha256(
        json.dumps(
            [
                sha(binary),
                sample,
                model,
                steps,
                tolerance,
                scales,
                density,
                sha(HERE / "input_settings.txt"),
                sha(HERE / "input_scaling_factors.txt"),
                *[sha(p) for p in sorted((HERE / "inputs" / sample).iterdir())],
            ],
            sort_keys=True,
        ).encode()
    ).hexdigest()[:16]
    folder = HERE / "runs" / model / sample / identifier
    folder.mkdir(parents=True, exist_ok=True)
    target = folder / "result.json"
    if target.exists():
        result = json.loads(target.read_text())
        assert sha(HERE / result["trace"]) == result["output_sha256"]
        return result
    prepare(folder, sample, model, scales, density)
    with tempfile.TemporaryDirectory(prefix="white-coarsening-") as temporary:
        work = Path(temporary)
        for p in folder.glob("input*"):
            shutil.copyfile(p, work / p.name)
        p = subprocess.run(
            [str(binary), str(work)],
            cwd=work,
            env=environment(steps, tolerance),
            capture_output=True,
            text=True,
            timeout=240,
        )
        (folder / "run.log").write_text(p.stdout + p.stderr)
        assert p.returncode == 0, (sample, model, p.stdout[-1000:], p.stderr[-1000:])
        shutil.copyfile(work / "output.txt", folder / "output.txt")
    result = dict(
        sample=sample,
        model=model,
        steps=steps,
        tolerance=tolerance,
        scales=scales,
        density=density,
        trace=str((folder / "output.txt").relative_to(HERE)),
        binary_sha256=sha(binary),
        output_sha256=sha(folder / "output.txt"),
        input_sha256={p.name: sha(p) for p in folder.glob("input*")},
        **summarize(folder / "output.txt", folder / "input_history.txt", model)
    )
    save(target, result)
    return result


def endpoint_rows(results):
    return [
        dict(
            sample=r["sample"],
            model=r["model"],
            R=r["states"]["end"]["R"],
            N=r["states"]["end"]["N"],
            S=r["states"]["end"]["S"],
            Sgb=r["states"]["end"]["Sgb"],
            **{p: r["states"]["end"][p] for p in POOLS}
        )
        for r in results
    ]


def write_csv(path, rows):
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def check(rows):
    expected = read_csv(HERE / "expected_endpoints.csv")
    assert len(rows) == len(expected) == 76
    reference = {(r["sample"], r["model"]): r for r in expected}
    errors = []
    for r in rows:
        old = reference[r["sample"], r["model"]]
        for k in ("R", "N", "S", "Sgb"):
            a, b = r[k], float(old[k])
            err = abs(a - b) / max(abs(b), 1e-30)
            assert err < 5e-4 or abs(a - b) < 1e-15, (r["sample"], r["model"], k, err)
            errors.append(err)
        for k in POOLS:
            assert abs(r[k] - float(old[k])) < 0.025, (r["sample"], r["model"], k)
    return float(max(errors))


def verify(binary, jobs):
    from verify import verify_model

    return verify_model(binary, jobs)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path, default=ROOT / "build/sciantix.x")
    parser.add_argument("--jobs", type=int, default=4)
    parser.add_argument("--steps", type=int, default=200)
    parser.add_argument("--tolerance", type=float, default=2e-5)
    parser.add_argument("--samples", nargs="+")
    parser.add_argument("--no-plots", action="store_true")
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--write-reference", action="store_true")
    parser.add_argument("--verify", action="store_true")
    args = parser.parse_args()
    binary = args.binary.resolve()
    assert binary.is_file(), binary
    assert not (args.check and args.write_reference)
    measured = read_csv(HERE / "data/white_2006.csv")
    samples = args.samples or [r["sample"] for r in measured]
    jobs = [(s, m) for s in samples for m in ("coarsening", "legacy")]
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        results = []
        for i, result in enumerate(
            pool.map(lambda j: run(binary, *j, args.steps, args.tolerance), jobs), 1
        ):
            results.append(result)
            if i % 10 == 0:
                print("White", i, "/", len(jobs), flush=True)
    if args.samples:
        print(json.dumps(results, indent=2))
        return
    save(HERE / "results.json", results)
    rows = endpoint_rows(results)
    write_csv(HERE / "endpoints.csv", rows)
    report = dict(
        ramp_cases=38,
        models=["coarsening", "legacy"],
        max_balance={
            m: max(r["max_balance"] for r in results if r["model"] == m)
            for m in ("coarsening", "legacy")
        },
    )
    if args.write_reference:
        write_csv(HERE / "expected_endpoints.csv", rows)
    if args.check:
        report["max_endpoint_relative_difference"] = check(rows)
    if args.verify:
        report["verification"] = verify(binary, args.jobs)
    save(HERE / "verification.json", report)
    if not args.no_plots:
        from plot import plot_all

        plot_all(results, measured)
    print(json.dumps(report, indent=2), flush=True)


if __name__ == "__main__":
    main()
