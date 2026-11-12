"""Run 38 White locations for legacy, frozen Hart and selected analytic rho."""
from pathlib import Path
import argparse
import concurrent.futures
import csv
import json
import os
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parent


def run_case(job):
    executable, mode, case = job
    folder = ROOT / mode / case
    environment = {key: value for key, value in os.environ.items()
                   if not key.startswith('SCIANTIX_')}
    if mode != 'analytic_rho':
        environment['SCIANTIX_NUCLEATION'] = 'control'
    else:
        # Exercise the executable's selected default, without a mode override.
        environment.update(SCIANTIX_NUCLEATION_TRACE=str(folder / 'nucleation.csv'),
                           SCIANTIX_HART_TRACE=str(folder / 'hart.csv'),
                           SCIANTIX_NU_RATE_TRACE=str(folder / 'nu_rate.csv'))
    with (folder / 'run.log').open('w') as log:
        result = subprocess.run([str(executable), str(folder)], stdout=log,
                                stderr=subprocess.STDOUT, timeout=300, env=environment)
    return dict(case=case, model=mode, returncode=result.returncode)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--executable', type=Path, default=ROOT.parents[1] / 'build/sciantix.x')
    parser.add_argument('--jobs', type=int, default=8)
    args = parser.parse_args()
    cases = list(csv.DictReader((ROOT / 'cases.csv').open()))
    assert len(cases) == 38
    for row in cases:
        source = ROOT / 'barani' / row['case']
        folder = ROOT / 'analytic_rho' / row['case']
        folder.mkdir(parents=True, exist_ok=True)
        for path in source.glob('input*'):
            if path.is_file():
                target = folder / path.name
                if not target.exists():
                    shutil.copy2(path, target)
    # Reject altered sources or inputs before running; validate all outputs afterwards.
    subprocess.run([sys.executable, str(ROOT / 'verify_reference.py'), '--inputs-only'], check=True)
    jobs = [(args.executable.resolve(), mode, row['case'])
            for mode in ['legacy', 'barani', 'analytic_rho'] for row in cases]
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        results = []
        for result in pool.map(run_case, jobs):
            results.append(result)
            if len(results) % 19 == 0:
                print(f'{len(results)}/{len(jobs)} completed', flush=True)
    (ROOT / 'run_manifest.json').write_text(json.dumps(results, indent=2))
    if any(row['returncode'] != 0 for row in results):
        raise SystemExit('A White simulation failed; inspect its run.log')
    subprocess.run([sys.executable, str(ROOT / 'verify_reference.py')], check=True)
    subprocess.run([sys.executable, str(ROOT / 'plot.py')], check=True)
