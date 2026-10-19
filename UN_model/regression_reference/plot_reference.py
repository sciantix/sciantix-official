#!/usr/bin/env python3
"""Plot saved Python reference histories without running the solver."""
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

HERE = Path(__file__).resolve().parent
fig, axes = plt.subplots(4, 3, figsize=(14, 14), constrained_layout=True)
for row, folder in enumerate(sorted((HERE / "results/full").iterdir())):
    if not folder.is_dir():
        continue
    with np.load(folder / "history.npz") as h:
        hours = h["time"] / 3600
        for name in ("c", "mb", "md"):
            axes[row, 0].plot(hours, h[name], label=name)
        axes[row, 0].set_ylabel(folder.name + "\ngas [atoms/m³]")
        for name in ("swelling_b", "swelling_d", "swelling_gf", "swelling_total_gas"):
            axes[row, 1].plot(hours, h[name] * 100, label=name)
        axes[row, 1].set_ylabel("swelling [%]")
        axes[row, 2].plot(hours, h["release_gas_percent"], label="released gas [% produced]")
        axes[row, 2].plot(hours, h["Fc_gf"], label="grain-face coverage [fraction]")
        for ax in axes[row]:
            ax.set_xlabel("time [h]")
            ax.grid(alpha=0.25)
            ax.legend(fontsize=8)
fig.suptitle("Approved UN L3 Python reference — 3 h / 160 modes")
fig.savefig(HERE / "results/reference_overview.png", dpi=160)
fig.savefig(HERE / "results/reference_overview.pdf")
plt.close(fig)
