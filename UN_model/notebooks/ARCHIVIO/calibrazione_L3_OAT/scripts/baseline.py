"""Reproduce the original notebook's main(), then compare every CSV column."""
import os
import io
import json
import time
import zipfile
from dataclasses import asdict
import model as m
import numpy as np
import pandas as pd

root = m.ROOT
out = root / 'runs' / 'baseline'
os.chdir(out)
config = {k: v for k, v in vars(m).items()
          if k.isupper() and isinstance(v, (str, int, float, bool, list, dict))
          and k not in ('RUN_CACHE',)}
(out / 'configuration.json').write_text(json.dumps(config, indent=2, default=str))
(out / 'parameters.json').write_text(json.dumps(asdict(m.make_candidate()), indent=2))
start = time.perf_counter()
rows, df, plots = m.main()
with zipfile.ZipFile(out / 'originals' / 'RizkUN_L3_unificato.zip') as z:
    z.extractall(out / 'zip_reference')
    ref = pd.read_csv(io.BytesIO(z.read('RizkUN_L3_unificato/L3_unificato_grid.csv')))
keys = ['experiment_case', 'T']
a = df.set_index(keys).sort_index()
b = ref.set_index(keys).sort_index()
assert a.index.equals(b.index)
comparisons = []
for col in b.columns:
    if pd.api.types.is_numeric_dtype(b[col]) and b[col].dtype != bool:
        x = a[col].to_numpy(float)
        y = b[col].to_numpy(float)
        finite = np.isfinite(x) & np.isfinite(y)
        ok = bool(np.allclose(x, y, rtol=1e-12, atol=0, equal_nan=True))
        absdiff = np.abs(x[finite] - y[finite])
        reldiff = absdiff / np.maximum(np.abs(y[finite]), 1e-300)
        comparisons.append(dict(column=col, match=ok, max_abs=float(absdiff.max()) if len(absdiff) else 0,
                                max_rel=float(reldiff.max()) if len(reldiff) else 0))
    else:
        comparisons.append(dict(column=col, match=bool(a[col].fillna('').equals(b[col].fillna('')))))
pd.DataFrame(comparisons).to_csv(out / 'zip_comparison.csv', index=False)
report = dict(rows=len(df), columns=len(df.columns), seconds=time.perf_counter()-start,
              all_columns_match=all(c['match'] for c in comparisons),
              failed_columns=[c for c in comparisons if not c['match']])
(out / 'reproduction.json').write_text(json.dumps(report, indent=2))
print(json.dumps(report, indent=2), flush=True)
assert report['all_columns_match'], 'Baseline mismatch: diagnose before calibration.'
