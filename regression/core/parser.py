"""
sciantix regression suite
author: Giovanni Zullo
"""

import numpy as np

class SciantixOutput:
    def __init__(self, path):
        # The header is read on its own and the body straight into floats: going through a
        # string array first needs several GB for a long output (rows x columns cells).
        with open(path, encoding="utf-8") as f:
            self.header = np.array([c.strip() for c in f.readline().rstrip("\r\n").split("\t")], dtype=str)

        data = np.genfromtxt(
            path,
            delimiter='\t',
            dtype=float,
            skip_header=1,
            filling_values=np.nan,
            autostrip=True
        )

        if data.ndim == 1:
            data = data.reshape(1, -1)

        # blank rows carry no information
        self.data = data[~np.all(np.isnan(data), axis=1)]
        self.colmap = {name: i for i, name in enumerate(self.header)}

    def get_last(self, var: str) -> float:
        return self.data[-1, self.colmap[var]]

    def get_all(self, var: str):
        return self.data[:, self.colmap[var]]
