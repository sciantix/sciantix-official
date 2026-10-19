#!/usr/bin/env python3
"""Immutable local source archive plus hashes of actually passed checks."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import zipfile

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('name')
    p.add_argument('results', nargs='+', type=Path)
    a = p.parse_args()
    out = HERE / 'checkpoints' / a.name
    out.mkdir(parents=True, exist_ok=False)
    checks = []
    for f in a.results:
        assert json.loads(f.read_text())['status'] == 'pass', f
        checks.append({'file': str(f), 'sha256': hashlib.sha256(f.read_bytes()).hexdigest()})
    files = [ROOT / 'CMakeLists.txt']
    files += [f for d in ['src', 'include'] for f in (ROOT / d).rglob('*') if f.is_file()]
    files += [f for f in HERE.iterdir() if f.is_file()]
    files += list((ROOT / 'context').glob('UN_L3_PHASE2D3*'))
    hashes = {str(f.relative_to(ROOT)): hashlib.sha256(f.read_bytes()).hexdigest() for f in sorted(files)}
    with zipfile.ZipFile(out / 'source.zip', 'x', zipfile.ZIP_DEFLATED) as z:
        for f in sorted(files):
            z.write(f, f.relative_to(ROOT))
    manifest = {'status': 'pass', 'name': a.name, 'passed_checks': checks, 'source_sha256': hashes,
                'head': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                'note': 'Local checkpoint; no Git/index changes; test scope is limited to listed checks.'}
    (out / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
    print(out)


if __name__ == '__main__':
    main()
