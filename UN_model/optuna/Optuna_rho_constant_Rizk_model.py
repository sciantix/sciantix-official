"""Load the frozen notebook; only add candidate-specific initial grain-face density and I/O."""
import json
from pathlib import Path
ROOT = Path(__file__).resolve().parent / 'Optuna_rho_constant_Rizk_results'
notebook = json.loads((ROOT/'source_notebook.ipynb').read_text())
ADAPTATIONS = []

def change(source, old, new, name):
    assert source.count(old) == 1, (name, source.count(old))
    ADAPTATIONS.append(name)
    return source.replace(old, new)

for index in (1, 2, 6, 8, 10):
    source = ''.join(notebook['cells'][index]['source'])
    if index == 1:
        source = change(source, 'OUTPUT_DIR = "RizkUN"', 'OUTPUT_DIR = str(ROOT / "plots")', 'redirect output directory')
    if index == 8:
        # The same Dv field appears in both dataclasses. Anchor each declaration separately.
        source = change(source, '    fission_rate: float\n\n    Dv_scale:', '    fission_rate: float\n    ngf_areal_0: float = NGF_AREAL_0\n\n    Dv_scale:', 'Candidate.ngf_areal_0')
        source = change(source, 'class UNParameters:\n    temperature:', 'class UNParameters:\n    ngf_areal_0: float = NGF_AREAL_0\n    temperature:', 'UNParameters.ngf_areal_0')
        source = change(source, '    NgfA = float(NGF_AREAL_0)', '    NgfA = float(p.ngf_areal_0)', 'pass initial density into grain-face initialization')
    if index == 10:
        source = change(source, '    p = UNParameters(\n        temperature=', '    p = UNParameters(\n        ngf_areal_0=cand.ngf_areal_0,\n        temperature=', 'runner parameter forwarding')
        source = change(source, '    # Add rate diagnostics.', '    # Save every final scalar for offline diagnostics; do not overwrite public units.\n    for key, values in hist.items():\n        if values:\n            out.setdefault(key, values[-1])\n\n    # Add rate diagnostics.', 'export final-state diagnostics')
        source = change(source, 'def ensure_output_dir(path=OUTPUT_DIR):', 'def ensure_output_dir(path=None):\n    path = OUTPUT_DIR if path is None else path', 'dynamic plotting directory')
        source = change(source, 'T_grid = np.arange(900, 2101, 50)', 'T_grid = np.arange(900, 1801, 50)', 'limit rho visualization to 1800 K')
    exec(compile(source, f'frozen_finalRizkUN_cell_{index}', 'exec'), globals())
assert RHO_MODE == 'constant' and USE_DYNAMIC_RHO_D_NUCLEATION is False
assert (DT_H, N_MODES, T_STEP) == (1.0, 40, 25.0)
assert (XE_DIFFUSIVITY_MODE, VU_DIFFUSIVITY_MODE, DV_GB_MODE) == ('rizk2025_refit_plot', 'rizk2025_refit_full', 'rizk_legacy_1e6_Dv1')


def calibration_candidate(parameters, label):
    params = dict(MANUAL_PARAMS)
    params.update({k: v for k, v in parameters.items() if k != 'NGF_AREAL_0'})
    params['ngf_areal_0'] = parameters['NGF_AREAL_0']
    return Candidate(label=label, **params)
