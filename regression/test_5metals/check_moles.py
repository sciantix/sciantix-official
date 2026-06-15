
import pandas as pd
import numpy as np

df = pd.read_csv('thermochemistry_output.txt', sep='\t', low_memory=False)
df.columns = df.columns.str.strip()
res = {}

for c in df.columns:
    if any(m in c.upper() for m in ['RU', 'MO']) and 'mol/m3' in c:
        v = pd.to_numeric(df[c], errors='coerce').dropna()
        if not v.empty:
            max_val = v.max()
            if max_val > 0:
                res[c] = max_val

print("\n=== VALORI MASSIMI RILEVATI ===")
for k, v in sorted(res.items()):
    print(f"{k}: {v:.4f}")