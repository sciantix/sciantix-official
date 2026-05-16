#!/usr/bin/env python3
import csv
import os
import shutil
import subprocess
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
import numpy as np


TEST_DIR = Path(__file__).resolve().parent
PROJECT_DIR = TEST_DIR.parents[1]
BUILD_EXECUTABLE = PROJECT_DIR / "build" / "sciantix.x"
LOCAL_EXECUTABLE = TEST_DIR / "sciantix.x"
OUTPUT_FILE = TEST_DIR / "output.txt"
PLOT_FILE = TEST_DIR / "metallic_fission_products.png"


def run_command(command: list[str], cwd: Path) -> None:
    print(f"Running in {cwd}: {' '.join(command)}", flush=True)
    subprocess.run(command, cwd=cwd, check=True)


def find_allmake_script() -> Path:
    for name in ("allmake", "Allmake", "Allmake.sh"):
        candidate = PROJECT_DIR / name
        if candidate.is_file():
            return candidate

    raise FileNotFoundError(f"No allmake script found in {PROJECT_DIR}")


def run_allmake() -> None:
    allmake_script = find_allmake_script()
    run_command([f"./{allmake_script.name}"], PROJECT_DIR)


def copy_sciantix_executable() -> Path:
    if not BUILD_EXECUTABLE.is_file():
        raise FileNotFoundError(f"SCIANTIX executable not found: {BUILD_EXECUTABLE}")

    shutil.copy2(BUILD_EXECUTABLE, LOCAL_EXECUTABLE)
    LOCAL_EXECUTABLE.chmod(LOCAL_EXECUTABLE.stat().st_mode | 0o111)
    print(f"Copied: {BUILD_EXECUTABLE} -> {LOCAL_EXECUTABLE}", flush=True)
    return LOCAL_EXECUTABLE


def run_sciantix() -> None:
    if not LOCAL_EXECUTABLE.is_file():
        raise FileNotFoundError(f"Local SCIANTIX executable not found: {LOCAL_EXECUTABLE}")

    run_command([f"./{LOCAL_EXECUTABLE.name}"], TEST_DIR)


def load_columns(output_file: Path, x_label: str, y_label: str) -> tuple[np.ndarray, np.ndarray]:
    with output_file.open(newline="") as handle:
        reader = csv.reader(handle, delimiter="\t")
        rows = [[cell.strip() for cell in row if cell.strip()] for row in reader]

    if len(rows) < 2:
        raise ValueError(f"Not enough data in {output_file}")

    headers = rows[0]
    try:
        x_index = headers.index(x_label)
        y_index = headers.index(y_label)
    except ValueError as exc:
        raise ValueError(f"Required column not found in {output_file}: {exc}") from exc

    values = np.array(rows[1:], dtype=float)
    return values[:, x_index], values[:, y_index]


def plot_metallic_fission_products() -> Path:
    time_h, cm       = load_columns(OUTPUT_FILE, "Time (h)", "Cm (at/m3)")
    time_h, cm_mat   = load_columns(OUTPUT_FILE, "Time (h)", "Cm matrix (at/m3)")
    time_h, cm_intra = load_columns(OUTPUT_FILE, "Time (h)",
                                    "Cm precipitated intragranular (at/m3)")
    time_h, cm_gb    = load_columns(OUTPUT_FILE, "Time (h)",
                                    "Cm precipitated grain boundary (at/m3)")

    fig, ax = plt.subplots(figsize=(9, 6))
    ax.plot(time_h, cm,       label="Cm total",      color="black",  linewidth=2)
    ax.plot(time_h, cm_mat,   label="Cm matrix",     color="blue",   linewidth=1.5)
    ax.plot(time_h, cm_intra, label="Cm prec. intra",color="green",  linewidth=1.5)
    ax.plot(time_h, cm_gb,    label="Cm prec. GB",   color="red",    linewidth=1.5)
    ax.set_xlabel("Time (h)")
    ax.set_ylabel("Concentration (at/m3)")
    ax.set_title("Metallic fission products evolution")
    ax.legend()
    ax.grid(True, which="both", linestyle=":", linewidth=0.8)

    fig.tight_layout()
    fig.savefig(PLOT_FILE, dpi=180)
    plt.show()
    return PLOT_FILE

def check_mass_conservation() -> None:
    time_h, cm       = load_columns(OUTPUT_FILE, "Time (h)", "Cm (at/m3)")
    time_h, cm_mat   = load_columns(OUTPUT_FILE, "Time (h)", "Cm matrix (at/m3)")
    time_h, cm_intra = load_columns(OUTPUT_FILE, "Time (h)",
                                    "Cm precipitated intragranular (at/m3)")
    time_h, cm_gb    = load_columns(OUTPUT_FILE, "Time (h)",
                                    "Cm precipitated grain boundary (at/m3)")

    # Calcola l'errore relativo di massa ad ogni passo
    cm_sum = cm_mat + cm_intra + cm_gb

    # Evita divisione per zero al primo passo (cm = 0)
    with np.errstate(invalid='ignore', divide='ignore'):
        epsilon = np.where(cm > 0, np.abs(cm - cm_sum) / cm, 0.0)

    max_epsilon = np.max(epsilon)
    print(f"Mass conservation check: max relative error = {max_epsilon:.2e}", flush=True)

    # Il test fallisce se l'errore supera l'1%
    assert max_epsilon < 0.01, (
        f"Mass conservation violated: max error = {max_epsilon:.2e} (threshold = 0.01)"
    )
    print("Mass conservation: OK", flush=True)


if __name__ == "__main__":
    run_allmake()
    copy_sciantix_executable()
    run_sciantix()
    saved_plot = plot_metallic_fission_products()   
    print(f"Saved: {saved_plot}", flush=True)
    check_mass_conservation()                       

