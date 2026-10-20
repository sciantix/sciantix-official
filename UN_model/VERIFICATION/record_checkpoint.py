#!/usr/bin/env python3
"""Record a local Phase 4A checkpoint without Git changes or external writes."""
import hashlib
import json
import os
from pathlib import Path
import platform
import subprocess
import sys
import zipfile

from run_verification import HERE, ROOT, save, sha
import numpy as np
import scipy


def main():
    for name in ['validation.json', 'basis_checks.json', 'results/pilots_v2/results.json', 'results/figures/summary.json']:
        assert json.loads((HERE / name).read_text())['status'] == 'pass', name
    out = HERE / 'checkpoints/complete'
    out.mkdir(parents=True, exist_ok=False)
    files = []
    for f in HERE.rglob('*'):
        relative = f.relative_to(HERE)
        if not f.is_file() or any(part in {'dependencies', 'build', '__pycache__', 'mplconfig', 'checkpoints'} for part in relative.parts):
            continue
        files.append(f)
    source = [f for f in files if f.parent == HERE and f.suffix in {'.py', '.cpp', '.md', '.txt', '.json'}]
    with zipfile.ZipFile(out / 'source.zip', 'x', zipfile.ZIP_DEFLATED) as z:
        for f in source:
            z.write(f, f.relative_to(HERE))
    manifest = {'status': 'pass', 'scope': 'Phase 4A complete; no LHS or reorganization',
                'head': subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
                'branch': subprocess.check_output(['git','branch','--show-current'],cwd=ROOT,text=True).strip(),
                'python': sys.version, 'numpy': np.__version__, 'scipy': scipy.__version__,
                'platform': platform.platform(), 'compiler': subprocess.check_output(['g++','--version'],text=True),
                'thread_environment': {k: os.environ.get(k) for k in ['OMP_NUM_THREADS','OPENBLAS_NUM_THREADS','MKL_NUM_THREADS']},
                'production_solver_sha256': sha(ROOT / 'src/classes/Solver.C'),
                'production_solver_header_sha256': sha(ROOT / 'include/classes/Solver.h'),
                'artifact_sha256': {str(f.relative_to(HERE)): sha(f) for f in sorted(files)},
                'source_zip_sha256': sha(out / 'source.zip'),
                'note': 'Local archive only; all original files retained. Build/dependencies/cache excluded from archive; pinned requirements retained.'}
    save(out / 'manifest.json', manifest)
    print('Checkpoint complete:',len(files),'artifacts,',len(source),'source/document files')


if __name__ == '__main__':
    main()
