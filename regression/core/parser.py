"""
sciantix regression suite
author: Giovanni Zullo
"""

import numpy as np

class SciantixOutput:
    def __init__(self, path):
        raw = np.genfromtxt(
            path,
            delimiter='\t',
            dtype=str,
            filling_values="nan",
            autostrip=True
        )

        if raw.ndim == 1:
            raw = np.array([raw])

        cleaned = []
        for row in raw:
            if any(cell.strip() != "" for cell in row):
                cleaned.append(row)

        cleaned = np.array(cleaned, dtype=str)

        self.header = cleaned[0]

        data = []
        for row in cleaned[1:]:
            vals = []
            for cell in row:
                try:
                    vals.append(float(cell))
                except:
                    vals.append(np.nan)
            data.append(vals)

        self.data = np.array(data, dtype=float)
        self.colmap = {name: i for i, name in enumerate(self.header)}

    def get_last(self, var: str) -> float:
        return self.data[-1, self.colmap[var]]

    def get_all(self, var: str):
        return self.data[:, self.colmap[var]]
