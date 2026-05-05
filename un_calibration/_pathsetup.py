"""Path setup for the un_calibration sandbox after the folder reorganisation.

Importing this module:
  - puts un_calibration/{model,optuna,codex,diagnostics,runners} on sys.path
    so cross-folder imports (e.g. `import un_model`,
    `import UN_M7_optuna_calibration_v8`) work from anywhere
  - chdirs to un_calibration/ so relative paths like
    "results/UN_M7_optuna_v14_..." resolve correctly

Use it from any script under un_calibration/<subfolder>/<script>.py with:

    import sys as _sys
    from pathlib import Path as _Path
    _sys.path.insert(0, str(_Path(__file__).resolve().parent.parent))
    import _pathsetup  # noqa: F401
"""

import os as _os
import sys as _sys
from pathlib import Path as _Path

_HERE = _Path(__file__).resolve().parent  # un_calibration/

for _sub in ("model", "optuna", "codex", "diagnostics", "runners"):
    _p = str(_HERE / _sub)
    if _p not in _sys.path:
        _sys.path.insert(0, _p)

# Make relative paths in scripts resolve against un_calibration/.
_os.chdir(str(_HERE))

UN_CALIBRATION_ROOT = _HERE
RESULTS_DIR = _HERE / "results"
LOGS_DIR = _HERE / "logs"
MODEL_DIR = _HERE / "model"
OPTUNA_DIR = _HERE / "optuna"
CODEX_DIR = _HERE / "codex"
DIAGNOSTICS_DIR = _HERE / "diagnostics"
NOTEBOOKS_DIR = _HERE / "notebooks"
REPORTS_DIR = _HERE / "reports"
